// Module that describes source code location

type point = {
    line: int,
    column: int,
    index: int
}

type range = {
    start: point,
    end: point
}

let next_point = (point, char) => 
    switch char {
    | "\n" => {line: point.line + 1, column: 1, index: point.index + 1}
    |   _  => {...point, column: point.column + 1, index: point.index + 1}
    }

let empty_point = {line: 1, column: 1, index: 0}

let one_col = (pos) => {start:pos, end: next_point(pos, "_")}