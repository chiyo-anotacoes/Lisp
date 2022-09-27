open Location

type lexer_state = {
    mutable pos: point,
    input: string,
}

type token =
    | Id(string)  // x
    | Num(int)    // 123
    | Lambda      // λ
    | Arrow       // →
    | Dot         // .
    | LPar        // (
    | RPar        // )
    | Colon       // :
    | Pipe        // |
    | Eq          // =
    | PiT         // Π
    | Star        // ★
    | KwIn          // In
    | KwLet         // Let
    | Eof

let print_token = tkn => 
    switch tkn {
    | Id(s) => "id(" ++ s ++ ")"
    | Lambda => `λ`
    | Arrow => `→`
    | Star => `★`
    | Dot => "."
    | PiT => "Π"
    | LPar => "("
    | Num(s) => string_of_int(s)
    | RPar => ")"
    | Colon => ":"
    | Eq => "="
    | KwIn => "'in'"
    | KwLet => "'let'"
    | Pipe => "|"
    | Eof => "eof"
    }

let equal_token = (a, b) => 
    switch (a,b) {
    | (KwIn, KwIn) => true
    | (Id(a), Id(b)) => a == b
    | (Lambda,Lambda) => true
    | (Arrow,Arrow) => true
    | (Dot,Dot) => true
    | (LPar,LPar) => true
    | (RPar,RPar) => true
    | (Colon, Colon) => true
    | (Pipe, Pipe) => true
    | (Eof, Eof) => true
    | (PiT, PiT) => true
    | (KwLet, KwLet) => true
    | (Eq, Eq) => true
    | (Star, Star) => true
    | _ => false
    }

let is_special = (char) => 
    char == `λ` || char == `Π` || char == `→`|| char == "."  || char == "("  
 || char == ")" || char == ":" || char == "|" || char == "="

let is_useless = char => 
    char == "\n" || char == "\t" || char == "\r" || char == " "

let is_num = char => String.get(char, 0) >= '0' && String.get(char, 0) <= '9'

let is_id_letter = char => !is_useless(char) && !is_special(char)

let get_char = (inp: string, on) => Js.String.get(inp, on.index)

let accumulate_while = (state: lexer_state, fun: string => bool) => {
    let start = state.pos
    let pos = ref(state.pos)
    while pos.contents.index < String.length(state.input) && fun(get_char(state.input, pos.contents)) {
        pos := next_point(pos.contents, get_char(state.input, pos.contents))
    }
    state.pos = pos.contents
    ({start, end: pos.contents}, Js.String.substring(~from=start.index, ~to_=pos.contents.index, state.input))
}

let one_char_st = (state, chr) => {
    let pos = one_col(state.pos)
    state.pos = next_point(state.pos, chr)
    pos
}

let t_lam  = `λ`;
let t_arr  = `→`;
let t_pi   = `Π`;
let t_star = `★`;

let rec lex = (state: lexer_state) =>
    if state.pos.index >= String.length(state.input) {
        ({start: state.pos, end: state.pos}, Eof)
    } else {
        let chr = get_char(state.input, state.pos)
        switch chr {
        | c if t_lam  == c => (one_char_st(state, chr), Lambda)
        | c if t_arr  == c => (one_char_st(state, chr), Arrow)
        | c if t_pi   == c => (one_char_st(state, chr), PiT)
        | c if t_star == c => (one_char_st(state, chr), Star)
        | "." => (one_char_st(state, chr), Dot)
        | "=" => (one_char_st(state, chr), Eq)
        | "(" => (one_char_st(state, chr), LPar)
        | ")" => (one_char_st(state, chr), RPar)
        | ":" => (one_char_st(state, chr), Colon)
        | "|" => (one_char_st(state, chr), Pipe)
        | c if is_useless(c) => 
            let _ = accumulate_while(state, is_useless)
            lex(state)
        | c if is_num(c)   => 
            let (range, str) = accumulate_while(state, is_num)
            (range, Num(int_of_string(str)))
        | _   => 
            let (range, str) = accumulate_while(state, is_id_letter)
            switch str {
            | "in"  => (range, KwIn)
            | "let" => (range, KwLet)
            | str   => (range, Id(str))
            }
        }
    }