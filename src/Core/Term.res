
open Location

type rec term =
    | Var(range, int)
    | Lam(range, string, term)
    | App(range, term, term)
    | Pi(range, string, term, term)
    | Let(range, string, term, term, term)
    | Ann(range, term, term)
    | Type(range)

let get_range = (term) => 
    switch term {
    | Var(range, _) => range
    | Lam(range, _, _) => range
    | App(range, _, _) => range
    | Ann(range, _, _) => range
    | Pi(range, _, _, _) => range
    | Let(range, _, _, _, _) => range
    | Type(range) => range
    }

let rec print_term = (ctx, term) => 
    switch term {
    | Var(_, ident) => 
        switch (Belt.List.get(ctx, ident)) {
        | Some(x) => x
        | None => "?" ++ string_of_int(ident)
        }
    | Lam(_, ident, term) => `(λ` ++ ident ++ ". " ++ print_term(list{ident, ...ctx}, term) ++ `)`
    | App(_, a, b) => "(" ++ print_term(ctx, a) ++ " " ++ print_term(ctx, b) ++ ")"
    | Ann(_, a, b) => "(" ++ print_term(ctx, a) ++ " : " ++ print_term(ctx, b) ++ ")"
    | Pi(_, "_", a, b) => "(" ++ print_term(ctx, a) ++ " -> " ++ print_term(list{"_", ...ctx}, b) ++ ")"
    | Pi(_, ident, a, b) => "((" ++ ident ++ ": " ++ print_term(ctx, a) ++ ") -> " ++ print_term(list{ident, ...ctx}, b) ++ ")"
    | Let(_, ident, t, v, b) => "(let " ++ ident ++ ": " ++ print_term(ctx, t) ++ " = " ++ print_term(ctx, v) ++ " in " ++ print_term(list{ident, ...ctx}, b) ++ ")"
    | Type(_) => `★`
    }