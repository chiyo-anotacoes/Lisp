exception NoRoot

open Tree;
open Lexer;
open Parser;
open Location;

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
    let x : Π x:x. x = x in 
    let y : Π x:Type. x → x = y in
    (+ x y)
`

let lex_state = {pos: {line: 1, column: 1, index: 0}, input}

let s = parse_expr(new_parser_state(lex_state))

Js.log(print_expr(s))

switch ReactDOM.querySelector("#root_react_element") {
| Some(root) => ReactDOM.render(<Demo />, root)
| None => raise(NoRoot)
}