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