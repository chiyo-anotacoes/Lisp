/* This is the parser of the following grammar:
 *
 * e  ::= λx.e                   -- Lambda abstraction
 *      | (x: e) → e             -- Pi type
 *      | e e                    -- Application
 *      | x                      -- Var
 *      | C                      -- Constructor
 *      | U                      -- Type universe
 *      | x : x                  -- Annotation
 *   
 * p ::= x                       -- Pattern variable
 *     | _                       -- Wildcard
 *     | C p*                    -- Constructor
 *  
 * d  ::= let x : e (| p+ = e )* -- Definition
 * 
 * b  ::= (x : e)                -- Binder
 *
 * dc ::= | C b* = e
 *
 * t  ::= type C b* dc*       -- Type family definition
 *
 */

open Lexer
open Tree
open Location


type error = {
    location: point
}

type syntax_error = {
    expected: option<string>,
    got: string,
    on: range
}

exception SyntaxError(syntax_error)

type parser_state = {
    mutable lexer_state: lexer_state,
    mutable current_pos: range,
    mutable current: token,
    mutable next_pos: range,
    mutable next: token
}

let new_parser_state = (lexer_state) => {
    let (next_pos, next) = lex(lexer_state);
    { lexer_state,
      current_pos: {start: {line: 1, column: 1, index: 0}, end: {line: 1, column: 1, index: 0}},
      current: Eof,
      next_pos,
      next
    }
}

let advance = (parser_state) => {
    let (next_pos, next)     = lex(parser_state.lexer_state);
    parser_state.current     = parser_state.next;
    parser_state.current_pos = parser_state.next_pos;
    parser_state.next        = next;
    parser_state.next_pos    = next_pos;
    (parser_state.current_pos, parser_state.current)
}

let eat = (parser_state, expected) => {
    let (pos, got) = advance(parser_state);
    if equal_token(expected, got) {
        (pos, got)
    } else {
        raise(SyntaxError({expected: Some(print_token(expected)), got: print_token(got), on: pos}))
    }
}

let eat_id = parser_state => {
    let (pos, got) = advance(parser_state);
    switch got {
    | Id(x) => (pos, x)
    | got => raise(SyntaxError({expected: None, got: print_token(got), on: pos}))
    }
}

let peek = (parser_state) => {
    parser_state.next
}

let rec parse_ident = parser_state => {
    let (iPos, iVal) = eat_id(parser_state);
    {iVal, iPos}
}

and parse_lambda = (parser_state) => {
    let _       = eat(parser_state, Lambda);
    let n       = parse_ident(parser_state);
    let _       = eat(parser_state, Dot);
    let e       = parse_expr(parser_state);
    Lam(n, e)
}

and parse_atom = parser_state => {
    switch peek(parser_state) {
    | Star => {
        let _ = eat(parser_state, Star);
        Type
    }
    | Id(_) => Var(parse_ident(parser_state))
    | LPar => {
        let _ = eat(parser_state, LPar);
        let t = parse_expr(parser_state);
        let _ = eat(parser_state, RPar);
        t
    }
    | got => raise(SyntaxError({expected: None, got: print_token(got), on: parser_state.next_pos}))
    }
}

and parse_partial_call = (left, parser_state) => 
    switch peek(parser_state) {
        | Id(_) | LPar | Star => {
            let right = parse_atom(parser_state)
            parse_partial_call(App(left, right), parser_state)
        }
        | _ => left
    }

and parse_call = parser_state => 
    parse_partial_call(parse_atom(parser_state), parser_state)


and parse_arrow = parser_state => {
    let atom = parse_call(parser_state)
    switch peek(parser_state) {
    | Arrow => 
        let _ = eat(parser_state, Arrow);
        Pi({iVal: "_", iPos: parser_state.current_pos}, atom, parse_arrow(parser_state))
    | _ => atom
    }
}

and parse_pi = parser_state => {
    let _  = eat(parser_state, PiT);
    let n  = parse_ident(parser_state);
    let _  = eat(parser_state, Colon);
    let t  = parse_expr(parser_state);
    let _  = eat(parser_state, Dot);
    let b  = parse_expr(parser_state);
    Pi(n, t, b)
}

and parse_let = parser_state => {
    let _  = eat(parser_state, KwLet);
    let n  = parse_ident(parser_state);
    let _  = eat(parser_state, Colon);
    let t  = parse_expr(parser_state);
    let _  = eat(parser_state, Eq);
    let v  = parse_expr(parser_state);
    let _  = eat(parser_state, KwIn);
    let b  = parse_expr(parser_state);
    Let(n, t, v, b)
}

and parse_expr = parser_state => {
    switch peek(parser_state) {
    | Lambda    => parse_lambda(parser_state)
    | PiT       => parse_pi(parser_state)
    | KwLet     => parse_let(parser_state)
    | _         => parse_arrow(parser_state)
    }
}