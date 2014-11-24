tokens = (
    'DEF',
    'LPAREN',
    'RPAREN',
    'EQUALS',
    'AND',
    'OR',
    'NOT',
    'LBRACKET',
    'RBRACKET',
    'ENDSENTENCE',
    'NAME',
    'ALLNAME',
)
reserved = {
    'def' : 'DEF'
}

t_DEF = r'def'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQUALS = r'='
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'/'
t_LBRACKET = r'{'
t_RBRACKET = r'}'
t_ENDSENTENCE = r'\.'

t_ignore = ' \t'

def t_NAME(t):
    r'[a-zA-Z0-9_]+'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_ALLNAME(t):
    r'\$[a-zA-Z][a-zA-Z0-9_]*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

import ply.lex as lex
lex.lex()

def p_program(t):
    '''program : expression
               | program expression'''

def p_expression(t):
    '''expression : definition
                  | declaration'''

def p_definition(t):
    '''definition : DEF NAME argument_list LBRACKET program RBRACKET'''

def p_definition_blank(t):
    '''definition : DEF NAME argument_list ENDSENTENCE'''

def p_declaration(t):
    '''declaration : selbri_list ENDSENTENCE'''

def p_selbri_list(t):
    '''selbri_list : selbri
                   | selbri_list selbri'''

def p_selbri(t):
    'selbri : NAME'

def p_selbri_special(t):
    'selbri : LPAREN operator RPAREN'

def p_selbri_declaration(t):
    'selbri : LPAREN selbri_list RPAREN'

def p_operator(t):
    '''operator : EQUALS
                | AND
                | OR
                | NOT'''

def p_declaration_list(t):
    '''declaration_list : declaration
                        | declaration_list declaration'''

def p_argument(t):
    '''argument : selbri
                | ALLNAME'''

def p_argument_list(t):
    '''argument_list : argument
                     | argument_list argument'''

import ply.yacc as yacc
yacc.yacc()

while 1:
    try:
        s = input('munje > ')
    except EOFError:
        break
    yacc.parse(s)
