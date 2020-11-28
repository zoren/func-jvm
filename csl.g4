grammar csl;

source_text
    : top_level_decl* EOF
;

expression_eof : expression EOF;
type_eof : type EOF;

top_level_decl
    : val_decl
    | 'type' IDENTIFIER type_kind
;

val_decl : 'val' pattern '=' expression;

type_decl_field : IDENTIFIER ':' type;

type_kind
    : '|' IDENTIFIER type*
    | (':' qualified_name)? '{' (type_decl_field (',' type_decl_field)*)? '}'
;

integer : INTEGER;
lang_float : FLOAT;

constant
    : 'true'
    | 'false'
    | integer
    | lang_float
    | STRING
    | POUND_CONSTANT
;

type
    : qualified_name type*
    | '(' type ')'
    | type '->' type
;

wildcard : '_';

pattern_identifier : IDENTIFIER;

pattern
    : wildcard
    | pattern_identifier
    | pattern ':' type
    | constant
    | qualified_name pattern*
    | '(' (pattern (',' pattern)*)? ')'
    | '[' (pattern (',' pattern)*)? ']'
;

if_exp : 'if' '(' expression ')' expression 'else' expression;

lambda_case : pattern '->' expression;

lambda : '\\' lambda_case ('|' lambda_case)*;

tuple_or_paren : '(' (expression (',' expression)*)? ')';

expression
    : qualified_name
    | constant
    | if_exp
    | lambda
    | 'let' val_decl+ 'in' expression
    | tuple_or_paren
    | '[' (expression (',' expression)*)? ']'
    | '-' expression
    | expression ('*' | '/') expression
    | expression ('+' | '-') expression
    | expression ('<=' | '>=' | '<' | '>' | '=') expression
    | expression ('&&' | '||') expression // todo remember r-assoc
    | expression expression
    | expression ('.' IDENTIFIER)+
    | expression ':>' type
;

qualified_name : IDENTIFIER ('::' IDENTIFIER)*;

POUND_CONSTANT : '#' ~('#')+ '#';

IDENTIFIER : [a-zA-Z][a-zA-Z0-9]*;

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
