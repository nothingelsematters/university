{
open Parser
}

let variable = ['a' - 'z'] ['a' - 'z' '0' - '9' '\'']*

rule main = parse
        | variable as v { VARIABLE(v) }
        | "."           { DOT }
        | "\\"          { LAMBDA }
        | "("           { OPENP }
        | ")"           { CLOSEP }
        | eof           { EOF }
