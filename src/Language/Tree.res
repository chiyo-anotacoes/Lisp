open Location

type ident = {
    iVal: string,
    iPos: range
}

type rec tree =
    | Var(ident)
    | Lam(range, ident, tree)
    | App(range, tree, tree)
    | Pi(range, ident, tree, tree)
    | Ann(range, tree, tree)
    | Let(range, ident, tree, tree, tree)
    | Type(range)

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

let get_range = (expr) => 
    switch expr {
    | Var(i) => i.iPos
    | Lam(r, _, _) => r
    | App(r, _, _) => r
    | Ann(r, _, _) => r
    | Pi(r, _, _, _) => r
    | Let(r, _, _, _, _) => r
    | Type(r) => r
    }

let rec print_expr = (expr) => 
    switch expr {
    | Var(ident) => ident.iVal
    | Lam(_, ident, expr) => `(λ` ++ ident.iVal ++ ". " ++ print_expr(expr) ++ `)`
    | App(_, a, b) => "(" ++ print_expr(a) ++ " " ++ print_expr(b) ++ ")"
    | Ann(_, a, b) => "(" ++ print_expr(a) ++ " : " ++ print_expr(b) ++ ")"
    | Pi(_, {iVal: "_"}, a, b) => "(" ++ print_expr(a) ++ " -> " ++ print_expr(b) ++ ")"
    | Pi(_, ident, a, b) => "((" ++ ident.iVal ++ ": " ++ print_expr(a) ++ ") -> " ++ print_expr(b) ++ ")"
    | Let(_, ident, t, v, b) => "(let " ++ ident.iVal ++ ": " ++ print_expr(t) ++ " = " ++ print_expr(v) ++ " in " ++ print_expr(b) ++ ")"
    | Type(_) => `★`
    }