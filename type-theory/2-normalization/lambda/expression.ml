type expression =
    | Variable of string
    | Application of expression * expression
    | Lambda of string * expression;;
