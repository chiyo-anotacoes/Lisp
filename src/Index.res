exception NoRoot

open Parser
open Location
open Lexer
open Context

open Check
open Term
open Eval

let input = `
    let t : ★ = Π x : ★. x → x in
    let x : t = λ_. λz. z in
    x
`

let lex_state = {pos: {line: 1, column: 1, index: 0}, input}

let nf = s => quote(0, eval(empty_ctx, s))

let s = parse_expr(new_parser_state(lex_state))

let err : string => unit =  %raw(`console.error`)

try {
    let (elab, t) = infer(empty_ctx, s)
    Js.log(print_term(list{}, elab))
} catch {
    | Unify.UnifyMismatch(_, l, r) => err("Mismatch between:\n  " ++ print_term(list{}, quote(0, l)) ++ "\n and\n  " ++ print_term(list{}, quote(0, r)))
    | Parser.SyntaxError(x) => err("Syntax error: unexpected '" ++ x.got ++ "' on " ++ show_range(x.on))
    | Check.CannotFindVariable(x, r) => err("Type error: cannot find variable '" ++ x ++ "' on " ++ show_range(r))
    | Check.CannotInferLambda => err("Type error: cannot infer lambda")
}