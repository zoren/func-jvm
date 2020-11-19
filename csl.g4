grammar csl;

source_text
    : top_level_decl* EOF
;

top_level_decl
    : decl
    | 'type' UPPER_IDENTIFIER type_kind
;

type_decl_field : LOWER_IDENTIFIER ':' type;

type_kind
    : '|' UPPER_IDENTIFIER type*
    | (':' qualified_upper)? '{' (type_decl_field (',' type_decl_field)*)? '}'
;

decl
    : 'val' pattern '=' expression
;

constant
    : NUMBER
    | STRING
    | POUND_CONSTANT
;

type
    : qualified_upper type*
    | LOWER_IDENTIFIER
    | '(' type ')'
    | type '->' type
    ;

pattern
    : '_'
    | constant
    | LOWER_IDENTIFIER
    | qualified_upper
    | pattern pattern
    | '(' (pattern (',' pattern)*)? ')'
    | '[' (pattern (',' pattern)*)? ']'
    | pattern ':' type
;

expression
    : 'if' '(' expression ')' expression 'else' expression
    | '\\' pattern '->' expression ('|' pattern '->' expression)*
    | 'let' decl+ 'in' expression
    | constant
    | qualified_upper
    | qualified_lower
    | '(' (expression (',' expression)*)? ')'
    | '[' (expression (',' expression)*)? ']'
    | '-' expression
    | expression ('*' | '/') expression
    | expression ('+' | '-') expression
    | expression ('<=' | '>=' | '<' | '>' | '=') expression
    | expression ('&&' | '||') expression
    | expression expression
    | expression '.' LOWER_IDENTIFIER
;

qualified_lower : (UPPER_IDENTIFIER '::')* LOWER_IDENTIFIER;
qualified_upper : (UPPER_IDENTIFIER '::')* UPPER_IDENTIFIER;

POUND_CONSTANT : '#' ~('#')* '#';

LOWER_IDENTIFIER : [a-z][a-zA-Z0-9]*;
UPPER_IDENTIFIER : [A-Z][a-zA-Z0-9]*;

fragment
INT
  : '0'..'9'+
;

NUMBER
  : '-'? ('0' | ( '1'..'9' INT* )) ('.' INT+)? EXPONENT?
;

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

STRING
  : '"' ( ESC_SEQ | ~('\\'|'"') )* '"'
;

fragment
EXPONENT
  : ('e'|'E') ('+'|'-')? ('0'..'9')+
;

fragment
HEX_DIGIT
  : ('0'..'9'|'a'..'f'|'A'..'F')
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
