import ply.lex as lex
import ply.yacc as yacc

tokens = [  # terminals
    'ID',
    'DEF',
    'VAR',
    'INT',
    'IF',
    'ELSE',
    'NUM',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'BECOMES',
    'EQ',
    'NE',
    'LT',
    'GT',
    'LE',
    'GE',
    'PLUS',
    'MINUS',
    'STAR',
    'SLASH',
    'PCT',
    'COMMA',
    'SEMI',
    'COLON',
    'ARROW',
    'COMMENT',
    'WHITESPACE'
]

# match keywords to their tokens, reduces # of regex rules
reserved = {
    'def': 'DEF',
    'var': 'VAR',
    'Int': 'INT',
    'if': 'IF',
    'else': 'ELSE'
}

# Regular expression rules for symbol/simple tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_STAR = r'\*'
t_SLASH = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_BECOMES = r'='
t_EQ = r'=='
t_NE = r'!='
t_LT = r'<'
t_GT = r'>'
t_LE = r'<='
t_GE = r'>='
t_PCT = r'%'
t_COMMA = r','
t_SEMI = r';'
t_COLON = r':'
t_ARROW = r'=>'
# ignored chars
t_ignore_WHITESPACE = r'\s'
t_ignore_COMMENT = r'\/\/.*'


# WARNING: PLY uses docstrings to build the parser

def t_ID(t):  # consisting of a letter followed by 0 or more letters or digits.
    r"""[a-zA-Z][a-zA-Z0-9]*"""
    t.type = reserved.get(t.value, 'ID')  # checks for reserved words, if none found, sets the default type to 'ID'
    return t


def t_NUM(t):  # string of one or more digits
    r"""\d+"""
    t.value = int(t.value)
    return t


def t_error(t):  # error handling, warns when unexpected tokens are found and skips them
    print(f"Invalid token: {t.value[0]}")
    t.lexer.skip(1)


def t_newline(t):  # count rows
    r"""\n+"""
    t.lexer.lineno += len(t.value)


# Establishes parsing precedence (prevents ambiguity)
precedence = [
    ('left', 'PLUS', 'MINUS'),
    ('left', 'STAR', 'SLASH'),
]


# non terminals
def p_declaration(p):  # start symbol/rule
    """defdefs : defdef defdefs
               | defdef"""
    pass


def p_define_expression(p):
    """defdef : DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE"""
    pass


def p_empty(p):
    """empty :"""
    pass


def p_opt(p):
    """parmsopt : parms
                | empty"""
    pass


def p_parms(p):
    """parms : vardef COMMA parms
             | vardef"""
    pass


def p_var_declaration(p):
    """vardef : ID COLON type"""
    pass


def p_type(p):
    """type : INT
            | LPAREN typesopt RPAREN ARROW type"""
    pass


def p_type_opt(p):
    """typesopt : types
                | empty"""
    pass


def p_types(p):
    """types : type COMMA types
             | type"""
    pass


def p_var_opt(p):
    """vardefsopt : VAR vardef SEMI vardefsopt
                  | empty"""
    pass


def p_def_opt(p):
    """defdefsopt : defdefs
                  | empty"""
    pass


def p_expras(p):
    """expras : expra SEMI expras
              | expra"""
    pass


def p_expra(p):
    """expra : ID BECOMES expr
             | expr"""
    pass


def p_expr(p):
    """expr : IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
            | term
            | expr PLUS term
            | expr MINUS term"""
    pass


def p_term(p):
    """term : factor
            | term STAR factor
            | term SLASH factor
            | term PCT factor"""
    pass


def p_factor(p):
    """factor : ID
              | NUM
              | LPAREN expr RPAREN
              | factor LPAREN argsopt RPAREN"""
    pass


def p_test(p):
    """test : expr NE expr
            | expr LT expr
            | expr LE expr
            | expr GE expr
            | expr GT expr
            | expr EQ expr"""
    pass


def p_argsopt(p):
    """argsopt : args
               | empty"""
    pass


def p_args(p):
    """args : expr COMMA args
            | expr"""
    pass


def p_error(p):
    print(f"Syntax error occurred at: '{p.value}' on line #{p.lineno}")


if __name__ == '__main__':
    lexer = lex.lex()
    parser = yacc.yacc()
    # open data source and parse it
    with open('TestData.txt', 'r') as data_source:
        data = data_source.read()
        lexer.input(data)
        parser.parse(data)
