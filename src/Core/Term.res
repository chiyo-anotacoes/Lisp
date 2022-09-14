type rec term =
    | Var(int)
    | Lam(string, term)
    | App(term, term)
    | Pi(string, term, term)
    | Let(string, term, term, term)
    | Type

let rec print_term = (ctx, term) => 
    switch term {
    | Var(ident) => 
        switch (Belt.List.get(ctx, ident)) {
        | Some(x) => x
        | None => "?" ++ string_of_int(ident)
        }
    | Lam(ident, term) => `(λ` ++ ident ++ ". " ++ print_term(list{ident, ...ctx}, term) ++ `)`
    | App(a, b) => "(" ++ print_term(ctx, a) ++ " " ++ print_term(ctx, b) ++ ")"
    | Pi("_", a, b) => "(" ++ print_term(ctx, a) ++ " -> " ++ print_term(list{"_", ...ctx}, b) ++ ")"
    | Pi(ident, a, b) => "((" ++ ident ++ ": " ++ print_term(ctx, a) ++ ") -> " ++ print_term(list{ident, ...ctx}, b) ++ ")"
    | Let(ident, t, v, b) => "(let " ++ ident ++ ": " ++ print_term(ctx, t) ++ " = " ++ print_term(ctx, v) ++ " in " ++ print_term(list{ident, ...ctx}, b) ++ ")"
    | Type => `★`
    }