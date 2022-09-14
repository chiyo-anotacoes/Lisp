exception NoRoot

open Tree
open Parser
open Location
open Lexer
open Context

open Check
open Term
open Eval

let input = `
    let x : (★ → ★) → ★ → ★ = λz. λu. z u in
    x (λr. ★) ★ 
`

let lex_state = {pos: {line: 1, column: 1, index: 0}, input}

let nf = s => quote(0, eval(empty_ctx, s))

let s = parse_expr(new_parser_state(lex_state))

Js.log(print_expr(s))

let (elab, t) = infer(empty_ctx, s)

Js.log(print_term(list{}, nf(elab)) ++ "\n\n" ++ print_term(list{}, quote(0, t)))

switch ReactDOM.querySelector("#root_react_element") {
| Some(root) => ReactDOM.render(<Demo />, root)
| None => raise(NoRoot)
}