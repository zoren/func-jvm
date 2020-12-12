grammar lang;

source_text
    : top_level_decl* EOF
;

pattern_eof : pattern EOF;
expression_eof : expression EOF;
type_eof : type EOF;

type_params : IDENTIFIER*;
top_level_decl
    : 'val' pattern '=' expression # top_level_val_decl
    | 'type' IDENTIFIER type_params type_kind # type_decl
;

val_decl : 'val' pattern '=' expression;
val_decls : val_decl+;

type_decl_field : IDENTIFIER ':' type;
union_constructor : '|' IDENTIFIER type_atom*;
type_kind
    : union_constructor+ # union_type_kind
    | (':' qualified_name)? '{' (type_decl_field (',' type_decl_field)*)? '}' # record_type_kind
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

type_atom
    : qualified_name # type_var
    | '(' type ')' # paren_type
;

type
    : qualified_name type_atom* # type_var_apply
    | type ('->' type)+ # function_type
;

pattern
    : '_' # wildcard
    | IDENTIFIER # pattern_identifier
    | pattern ':' type # type_annotation_pattern
    | constant # constant_pattern
//    | qualified_name pattern*
    | '(' (pattern (',' pattern)*)? ')' # tuple_or_paren_pattern
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
    | '(' (expression (',' expression)*)? ')' # tuple_or_paren_exp
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
