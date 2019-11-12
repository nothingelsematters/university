%{
  open Expression;;
%}
%token <string> VARIABLE
%token LAMBDA DOT
%token OPEN CLOSE
%token EOF
%start main
%type <Expression.expression> main
%%
main:
        application              { $1 }
        |application lambda      { Application ($1, $2) }
        |lambda                  { $1 }
variable:
        VARIABLE                 { Variable ($1) }
atom:
        OPENP main CLOSEP        { $2 }
        |variable                { $1 }
application:
        atom application         { Application ($1, $2) }
        |application             { $1 }
        |atom                    { $1 }
lambda:
        LAMBDA VARIABLE DOT main { Lambda ($2, $4) }
