
open Location

type rec term =
    | Var(range, int)
    | Num(range, int)
    | Top(range, string)
    | Lam(range, string, term)
    | App(range, term, term)
    | Pi(range, string, term, term)
    | Let(range, string, term, term, term)
    | Ann(range, term, term)
    | Type(range)

let get_range = (term) => 
    switch term {
    | Num(range, _) => range
    | Top(range, _) => range
    | Var(range, _) => range
    | Lam(range, _, _) => range
    | App(range, _, _) => range
    | Ann(range, _, _) => range
    | Pi(range, _, _, _) => range
    | Let(range, _, _, _, _) => range
    | Type(range) => range
    }

let set_range = (new_range, term) => 
    switch term {
    | Num(_, t) => Num(new_range, t)
    | Var(_, ident) => Var(new_range, ident)
    | Top(_, n) => Top(new_range, n)
    | Lam(_, ident, term) => Lam(new_range, ident, term)
    | App(_, a, b) => App(new_range, a, b)
    | Ann(_, a, b) => Ann(new_range, a, b)
    | Pi(_, ident, a, b) => Pi(new_range, ident, a, b)
    | Let(_, ident, t, v, b) => Let(new_range, ident, t, v, b)
    | Type(_) => Type(new_range)
    }

let rec print_term = (ctx, term) => 
    switch term {
    | Num(_, t) => string_of_int(t)
    | Var(_, ident) => 
        switch (Belt.List.get(ctx, ident)) {
        | Some(x) => x
        | None => "?" ++ string_of_int(ident)
        }
    | Top(_, n) => n
    | Lam(_, ident, term) => `(λ` ++ ident ++ ". " ++ print_term(list{ident, ...ctx}, term) ++ `)`
    | App(_, a, b) => "(" ++ print_term(ctx, a) ++ " " ++ print_term(ctx, b) ++ ")"
    | Ann(_, a, b) => "(" ++ print_term(ctx, a) ++ " : " ++ print_term(ctx, b) ++ ")"
    | Pi(_, "_", a, b) => "(" ++ print_term(ctx, a) ++ " -> " ++ print_term(list{"_", ...ctx}, b) ++ ")"
    | Pi(_, ident, a, b) => "((" ++ ident ++ ": " ++ print_term(ctx, a) ++ ") -> " ++ print_term(list{ident, ...ctx}, b) ++ ")"
    | Let(_, ident, t, v, b) => "(let " ++ ident ++ ": " ++ print_term(ctx, t) ++ " = " ++ print_term(ctx, v) ++ " in " ++ print_term(list{ident, ...ctx}, b) ++ ")"
    | Type(_) => `★`
    }