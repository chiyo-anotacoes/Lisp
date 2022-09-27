open Location

type stuck =
    | Rigid(int)

type rec value =
    | VLam(range, string, value => value)
    | VStuck(range, stuck, list<value>)
    | VPi(range, string, value, value => value)
    | VLet(range, string, value, value, value => value)
    | VType(range)

let get_val_range = value =>
    switch value {
    | VLam(r, _, _)        => r
    | VPi(r, _, _, _)      => r
    | VLet(r, _, _, _, _)  => r
    | VStuck(r, _, _)      => r
    | VType(r)             => r
    }