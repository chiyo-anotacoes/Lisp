exception NoRoot

open Parser
open Location
open Lexer
open Context

open Check
open Term
open Eval

@module("../../../src/Editor.js") external setOnChange: (string => unit) => unit = "setOnChange"
@module("../../../src/Editor.js") external setRes: string => unit = "setRes"
@module("../../../src/Editor.js") external mark: array<range> => unit = "mark"

let change = input => {
    let lex_state = {pos: {line: 1, column: 1, index: 0}, input}

    try {
        let s = parse_expr(new_parser_state(lex_state))
        let (_, t) = infer(empty_ctx, s)
        setRes(print_term(list{}, quote(0, t)))
    } catch {
        | Unify.UnifyMismatch(ctx, ra, l, r) => 
            mark([ra, Value.get_val_range(r)])
            setRes("Mismatch between:\n  " ++ print_term(list{}, quote(ctx.level, l)) ++ "\n and\n  " ++ print_term(list{}, quote(ctx.level, r)))
        | Parser.SyntaxError(x) => 
            mark([x.on])
            setRes("Syntax error: unexpected '" ++ x.got ++ "' on " ++ show_range(x.on))
        | Check.CannotApply(r, l) => 
            mark([r])
            setRes("Type error: Cannot apply '" ++  print_term(list{}, l) ++ "'")
        | Check.CannotFindVariable(x, r) => 
            mark([r])
            setRes("Type error: cannot find variable '" ++ x ++ "' on " ++ show_range(r))
        | Check.CannotInferLambda(r) => 
            mark([r])
            setRes("Type error: cannot infer lambda")
    }
}

setOnChange(change)

let input = `
let t : ★ = Π x : ★. x → x in
let x : t  = λ_. λz. z in
x
`

change(input)