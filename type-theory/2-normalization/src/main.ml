open Lambda

let (>>) x f = f x;;
let (amount, step) =
  let ints_list = read_line () >> String.split_on_char ' ' >> List.map int_of_string in
  (List.hd ints_list, ints_list >> List.tl >> List.hd);;
let expression = Lexing.from_channel stdin >> Parser.main Lexer.main >> ref;;

for i = 0 to amount do
  expression := reduce expression;
  if (i mod step == 0) || (i == amount) then !expression >> string_of_lambda >> print_endline else ()
done
