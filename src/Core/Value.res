type stuck =
    | Rigid(int)

type rec value =
    | VLam(string, value => value)
    | VStuck(stuck, list<value>)
    | VPi(string, value, value => value)
    | VLet(string, value, value, value => value)
    | VType