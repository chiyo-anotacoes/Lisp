open Location

type ident = {
    val: string,
    pos: range
}

type rec tree =
    | Var(ident)
    | Lam(ident, tree)
    | App(tree, tree)
    | Pi(ident, tree, tree)
    | Let(ident, tree, tree, tree)
    | Type

and pat =
    | PVar(ident)
    | PWild
    | PCons(ident, list<pat>)

and equation = {
    pats: list<pat>,
    val: tree
}

and constructor = {
    cons_name: ident,
    defs: list<(ident, tree)>
}

and def = {
    def_name: ident,
    equations: list<equation>
}

and data = {
    data_name: ident,
    cons: list<constructor>
}

let rec print_expr = (expr) => 
    switch expr {
    | Var(ident) => ident.val
    | Lam(ident, expr) => `(λ` ++ ident.val ++ ". " ++ print_expr(expr) ++ `)`
    | App(a, b) => "(" ++ print_expr(a) ++ " " ++ print_expr(b) ++ ")"
    | Pi(ident, a, b) => "((" ++ ident.val ++ ": " ++ print_expr(a) ++ ") -> " ++ print_expr(b) ++ ")"
    | Let(ident, t, v, b) => "(let " ++ ident.val ++ ": " ++ print_expr(t) ++ " = " ++ print_expr(v) ++ " in " ++ print_expr(b) ++ ")"
    | Type => `U`
    }