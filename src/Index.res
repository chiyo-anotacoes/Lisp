exception NoRoot

open Tree;
open Parser;
open Location;
open Lexer;

open Check;
open Term;
open Eval;

let rec t = (ctx) => {
    switch lex(ctx) {
    | (_, Eof) => print_endline("eof")
    | (_, other) => {
        print_endline(print_token(other))
        t(ctx)
    }
    }
}

let input = `
    let x : (★ → ★) → ★ → ★ = λz. λx. z x in
    x
`

let lex_state = {pos: {line: 1, column: 1, index: 0}, input}

let s = parse_expr(new_parser_state(lex_state))

Js.log(print_expr(s))

let (elab, t) = infer(empty_ctx, s)

Js.log(print_term(list{}, elab))

//Js.log(print_term(list{}, quote(0, eval(list{}, elab))))

switch ReactDOM.querySelector("#root_react_element") {
| Some(root) => ReactDOM.render(<Demo />, root)
| None => raise(NoRoot)
}