open Lambda

let (amount, step) =
  let ints_list = read_line () |> String.split_on_char ' ' |> List.map int_of_string in
  (List.hd ints_list, ints_list |> List.tl |> List.hd)
let expression = Lexing.from_channel stdin |> Parser.main Lexer.main |> ref;;

let i = ref 0 in
let condition = ref true in
while !condition && !i <= amount do
  if !condition && (!i mod step == 0 || !i == amount) then !expression |> string_of_lambda |> print_endline else ();
  let (reduced, flag) = reduce !expression in
  expression := reduced;
  condition := flag;
  i := !i + 1
done
