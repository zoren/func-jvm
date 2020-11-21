grammar csl;

source_text
    : top_level_decl* EOF
;

expression_eof : expression EOF;
type_eof : type EOF;

top_level_decl
    : 'val' pattern '=' expression
    | 'type' UPPER_IDENTIFIER type_kind
;

val_decl : 'val' pattern '=' expression;

type_decl_field : LOWER_IDENTIFIER ':' type;

type_kind
    : '|' UPPER_IDENTIFIER type*
    | (':' qualified_upper)? '{' (type_decl_field (',' type_decl_field)*)? '}'
;

integer : INTEGER;
float : FLOAT;

constant
    : 'true'
    | 'false'
    | integer
    | float
    | STRING
    | POUND_CONSTANT
;

type
    : java_qualified type*
    | LOWER_IDENTIFIER
    | '(' type ')'
    | type '->' type
    ;

wildcard : '_';

pattern_identifier : LOWER_IDENTIFIER;

pattern
    : wildcard
    | pattern_identifier
    | pattern ':' type
    | constant
    | qualified_upper pattern*
    | '(' (pattern (',' pattern)*)? ')'
    | '[' (pattern (',' pattern)*)? ']'
;

variable : qualified_lower;

if : 'if' '(' expression ')' expression 'else' expression;

lambda_case : pattern '->' expression;

lambda : '\\' lambda_case ('|' lambda_case)*;

expression
    : constant
    | qualified_upper
    | variable
    | if
    | lambda
    | 'let' val_decl+ 'in' expression
    | '(' (expression (',' expression)*)? ')'
    | '[' (expression (',' expression)*)? ']'
    | '-' expression
    | expression ('*' | '/') expression
    | expression ('+' | '-') expression
    | expression ('<=' | '>=' | '<' | '>' | '=') expression
    | expression ('&&' | '||') expression // remember r-assoc
    | expression expression
    | expression '.' LOWER_IDENTIFIER
    | expression ':>' t=type
;

qualified_lower : (UPPER_IDENTIFIER '::')* LOWER_IDENTIFIER;
qualified_upper : (UPPER_IDENTIFIER '::')* UPPER_IDENTIFIER;
java_qualified : JAVA_QUALIFIED;
JAVA_QUALIFIED : ([a-z]+.)+[A-Z][a-z]*;

POUND_CONSTANT : '#' ~('#')+ '#';

LOWER_IDENTIFIER : [a-z][a-zA-Z0-9]*;
UPPER_IDENTIFIER : [A-Z][a-zA-Z0-9]*;

LineComment
    :   '//' ~[\r\n]*
        -> skip
    ;

WS
  : ( ' '
  | '\t'
  | '\n'
  | '\r'
  ) -> channel(HIDDEN)
;

INTEGER: '-'? [0-9]+;

FLOAT
    : '-'? [0-9]+ FLOAT_TAIL
    ;

fragment
FLOAT_TAIL
    : FLOAT_DECIMAL FLOAT_EXP
    | FLOAT_DECIMAL
    | FLOAT_EXP
    ;

fragment
FLOAT_DECIMAL
    : '.' [0-9]+
    ;

fragment
FLOAT_EXP
    : [eE] '-'? [0-9]+
    ;

STRING
  : '"' ( ESC_SEQ | ~('\\'|'"') )* '"'
;

fragment
ESC_SEQ
  : '\\' ('"'|'\\'|'/'|'b'|'f'|'n'|'r'|'t')
  | UNICODE_ESC
;

fragment
UNICODE_ESC
  : '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
;

fragment
HEX_DIGIT
  : ('0'..'9'|'a'..'f'|'A'..'F')
;
