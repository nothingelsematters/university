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

list returns [LinkedList<Line> lines]: line      { $lines = new LinkedList<Line>(); $lines.add($line.l); }
                                     | line list { $lines = $list.lines; $lines.addFirst($line.l); }
                                     ;

line returns [Line l]: functionType NEWLINE { $l = $functionType.dt; }
                     | function             { $l = $function.f; }
                     | NEWLINE
                     ;

functionType returns [DeclaredType dt]: NAME TYPE type { $dt = new DeclaredType($NAME.text, $type.t); };

type returns [FunctionalType t]: leftType IMPLIES type  { $t = new FunctionType($leftType.t, $type.t); }
                               | leftType               { $t = $leftType.t; }
                               ;

leftType returns [FunctionalType t] : NAME              { $t = new AtomicType($NAME.text); }
                                    | OPENP type CLOSEP { $t = $type.t; }
                                    ;

function returns [FunctionDefinition f] : NAME values body { $f = new FunctionDefinition($NAME.text, $values.vs, $body.fb); };

body returns [FunctionBody fb] : EQUALS expression NEWLINE { $fb = new InsecureFunction($expression.expr); }
                               | mbnewline booleanCases    { $fb = new GuardedFunction($booleanCases.lb); }
                               ;

booleanCases returns [LinkedList<BooleanCase> lb]: bc=booleanCase NEWLINE                          { $lb = new LinkedList<BooleanCase>(); $lb.add($bc.bc); }
                                                 | bc=booleanCase NEWLINE bclist=emptyBooleanCases { $lb = $bclist.lb; $lb.addFirst($bc.bc); }
                                                 ;

emptyBooleanCases returns [LinkedList<BooleanCase> lb]: bc=booleanCases { $lb = $bc.lb; }
                                                      | NEWLINE         { $lb = new LinkedList<BooleanCase>(); }
                                                      ;

booleanCase returns [BooleanCase bc]: CASE cond=expression EQUALS expr=expression { $bc = new BooleanCase($cond.expr, $expr.expr); };

expression returns [Expression expr]: s=expressionString { $expr = new StringExpression($s.str); };

expressionString returns [String str]: OPERATION emptyExpression { $str = $OPERATION.text + " " + $emptyExpression.str; }
                                     | OPENP     emptyExpression { $str = $OPENP.text + $emptyExpression.str; }
                                     | CLOSEP    emptyExpression { $str = $CLOSEP.text + " " + $emptyExpression.str; }
                                     | LITERAL   emptyExpression { if ($emptyExpression.str.matches("[\\),].*"))
                                                                        $str = $LITERAL.text + $emptyExpression.str;
                                                                    else
                                                                        $str = $LITERAL.text + " " + $emptyExpression.str; }
                                     | NAME      emptyExpression { if ($emptyExpression.str.matches("[\\(\\),].*"))
                                                                        $str = $NAME.text + $emptyExpression.str;
                                                                    else
                                                                        $str = $NAME.text + " " + $emptyExpression.str; }
                                     ;

emptyExpression returns [String str]: expressionString    { $str = $expressionString.str; }
                                    | /* epsilon */       { $str = ""; }
                                    ;

values returns [LinkedList<Argument> vs]: value values   { $vs = $values.vs; $vs.addFirst($value.arg); }
                                        | /* epsilon */ { $vs = new LinkedList<Argument>(); }
                                        ;

value returns [Argument arg]: NAME    { $arg = new Name($NAME.text); }
                            | LITERAL { $arg = new Literal($LITERAL.text); }
                            ;

mbnewline : NEWLINE
          |
          ;


TYPE : '::';
IMPLIES : '->';
OPENP : '(';
CLOSEP : ')';
LITERAL : [0-9]+;
NAME : [a-zA-Z_]([a-zA-Z0-9_'])*;
OPERATION : ([!%^&*,\-+<>'"/~]+|'=='|'!=');
CASE : '|';
EQUALS : '=';
NEWLINE : [\n]+;
SPACES : [ \t\r]+ -> channel(HIDDEN);
