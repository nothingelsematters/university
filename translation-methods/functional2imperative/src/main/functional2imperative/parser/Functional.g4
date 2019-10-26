grammar Functional;

@header {
package functional2imperative.parser;

import java.util.LinkedList;
import functional2imperative.*;
}

/*
    program -> List of (function | functionType)
    functionType -> NAME (fuction type | atomic type)
    function -> name body

    function body -> values, variables, (boolean cases | expression)
    boolean cases -> condition & expression
*/

program returns [Lines fns] : list { $fns = new Lines($list.lines); };

list returns [List<Line> lines]: line      { $lines = new ArrayList<Line>(); $lines.add($line.l); }
                               | line list { $lines = $list.lines; $lines.add($line.l); }
                               ;

line returns [Line l]: functionType { $l = $functionType.dt; }
                     | function     { $l = $function.f; }
                     | NEWLINE
                     ;

functionType returns [DeclaredType dt]: NAME TYPE type { $dt = new DeclaredType($NAME.text, $type.t); };

type returns [Type t]: leftType IMPLIES type  { $t = new FunctionType($leftType.t, $type.t); }
                     | leftType               { $t = $leftType.t; }
                     ;

leftType returns [Type t] : NAME              { $t = new AtomicType($NAME.text); }
                          | OPENP type CLOSEP { $t = $type.t; }
                          ;

function returns [FunctionDefinition f] : NAME values body NEWLINE { $f = new FunctionDefinition($NAME.text, $values.vs, $body.fb); };

body returns [FunctionBody fb] : EQUALS expression NEWLINE { $fb = new InsecureFunction($expression.expr); }
                               | booleanCases              { $fb = new GuardedFunction($booleanCases.lb); }
                               ;

booleanCases returns [List<BooleanCase> lb]: bc=booleanCase NEWLINE             { $lb = new ArrayList<BooleanCase>(); $lb.add($bc.bc); }
                                           | bc=booleanCase bclist=booleanCases { $lb = $bclist.lb; $lb.add($bc.bc); }
                                           | NEWLINE
                                           ;

booleanCase returns [BooleanCase bc]: CASE cond=expression EQUALS expr=expression { $bc = new BooleanCase($cond.expr, $expr.expr); };

expression returns [Expression expr]: NAME emptyExpression      { $expr = new Expression($NAME.text + " " + $emptyExpression.expr.toString()); }
                                    | OPERATION emptyExpression { $expr = new Expression($OPERATION.text + " " + $emptyExpression.expr.toString()); }
                                    ;

emptyExpression returns [Expression expr]: expression    { $expr = $expression.expr; }
                                         | /* epsilon */ { $expr = new Expression(""); }
                                         ;

values returns [LinkedList<Argument> vs]: value values   { $vs = $values.vs; $vs.addFirst($value.arg); }
                                        | /* epsilon */ { $vs = new LinkedList<Argument>(); }
                                        ;

value returns [Argument arg]: NAME    { $arg = new Name($NAME.text); }
                            | LITERAL { $arg = new Literal($LITERAL.text); }
                            ;

TYPE : '::';
IMPLIES : '->';
OPENP : '(';
CLOSEP : ')';
LITERAL : [0-9]+;
NAME : [a-zA-Z_]([a-zA-Z0-9_'])*;
OPERATION : [!%^&*()\-+<>'"/~];
CASE : '|';
EQUALS : '=';
NEWLINE : '\n';
SPACES : [ \t\r]+ -> channel(HIDDEN);
