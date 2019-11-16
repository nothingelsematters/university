open Buffer

type lambda =
  | Variable of string
  | Application of lambda * lambda
  | Lambda of string * lambda

let string_of_lambda lambda =
  let buf = create 1000 in
  let add_buf = add_string buf in

  let rec s_e expr = match expr with
    | Variable v -> add_buf v
    | Application (l, r) -> add_buf "("; s_e l; add_buf " "; s_e r; add_buf ")"
    | Lambda (v, l) -> add_buf "("; add_buf "\\"; add_buf v; add_buf "."; s_e l; add_buf ")"

  in s_e lambda;
  contents buf;;
