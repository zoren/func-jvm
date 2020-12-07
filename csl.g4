grammar csl;

source_text
    : top_level_decl* EOF
;

pattern_eof : pattern EOF;
expression_eof : expression EOF;
type_eof : type EOF;

top_level_decl
    : val_decl
    | 'type' IDENTIFIER type_kind
;

val_decl : 'val' pattern '=' expression;
val_decls : val_decl+;

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

pattern_tuple_or_paren : '(' (pattern (',' pattern)*)? ')';

pattern
    : wildcard
    | pattern_identifier
    | pattern ':' type
    | constant
    | qualified_name pattern*
    | pattern_tuple_or_paren
    | '[' (pattern (',' pattern)*)? ']'
;

lambda_case : pattern '->' expression;

expression
    : expression ('*' | '/') expression # binary_expression
    | expression ('+' | '-') expression # binary_expression
    | expression ('<=' | '>=' | '<' | '>' | '=') expression # binary_expression
    | <assoc=right> expression ('&&' | '||') expression # binary_expression
    | expression expression # apply
    | qualified_name # var_or_const
    | constant # constant_exp
    | 'if' '(' expression ')' expression 'else' expression # if_exp
    | '\\' lambda_case ('|' lambda_case)* # lambda
    | 'let' val_decls 'in' expression # let
    | '(' (expression (',' expression)*)? ')' # tuple_or_paren
    | '[' (expression (',' expression)*)? ']' # list_exp
    | '-' expression # unary_minus
    | expression '.' IDENTIFIER # field_access_exp
    | expression ':>' type # upcast_annotation_exp
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
