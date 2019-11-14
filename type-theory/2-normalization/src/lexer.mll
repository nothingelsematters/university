{
open Parser
}

let variable = ['a' - 'z'] ['a' - 'z' '0' - '9' '\'']*
let space = [' ' '\r' '\n' '\t']

rule main = parse
  | space         { main lexbuf }
  | variable as v { VARIABLE(v) }
  | "."           { DOT }
  | "\\"          { LAMBDA }
  | "("           { OPENP }
  | ")"           { CLOSEP }
  | eof           { EOF }
