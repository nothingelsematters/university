open Lambda

let (>>) x f = f x;;
Lexing.from_channel stdin >> Parser.main Lexer.main >> string_of_lambda >> print_endline;;
