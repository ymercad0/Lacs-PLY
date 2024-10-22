import ply.lex as lex
import ply.yacc as yacc


def print_token_array(p):
    print([token for token in p])

# name -> Procedure
found_procedures = dict()

# name -> Variable
found_variables = dict()

class Procedure:
    def __init__(self, name:str, return_type:str, parameters:list, local_variables:list):
        self.name = name
        self.return_type = return_type
        self.parameters = parameters
        self.local_variables = local_variables

    def __str__(self):
        return f"Procedure \n" + \
                f"\t * name: {self.name} \n" + \
                f"\t * return_type: {self.return_type} \n" + \
                f"\t * parameters: {self.parameters} \n" + \
                f"\t * local_variables: {self.local_variables} \n"


    def __repr__(self):
        return self.__str__()

class Variable:
    def __init__(self, name:str, type:str):
        self.name = name
        self.type = type

    def __str__(self):
        return f"Variable {self.name} -> {self.type}"

    def __repr__(self):
        return self.__str__()

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

    p[0] = [p[1]] + p[2] if len(p) > 2 else [p[1]]
    
    # Returns a list of Procedure objects
    return p


def p_define_expression(p):
    """defdef : DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE"""
    # ?                                                                                 ^ We can define procedures inside procedures?
    
    # ! o The type of the child expras must be the same as the child type.
    child_expras = p[12]
    child_type = p[7]

    if child_expras != child_type:
        print(f"Error in defdef: expras is of type {child_expras}, but type is of type {child_type}")
        return
    
    procedure_name = p[2]
    procedure_return_type = p[7]
    procedure_parameters = p[4]
    procedure_local_variables = p[10]

    procedure = Procedure(procedure_name, \
        procedure_return_type, \
        procedure_parameters, \
        procedure_local_variables)

    if procedure_name in found_procedures:
        print(f"Procedure {procedure_name} already defined")
        return

    print(f"Found procedure: {procedure}")
    found_procedures[procedure_name] = procedure

    # Returns a Procedure object
    p[0] = procedure
    return p


def p_empty(p):
    """empty :"""
    # Returns None. Equivalent to pass
    p[0] = None
    return p


def p_opt(p):
    """parmsopt : parms
                | empty"""

    # Returns a list of Variable objects, or an empty list
    p[0] = p[1] if p[1] else []
    return p


def p_parms(p):
    """parms : vardef COMMA parms
             | vardef"""
    
    # Returns a list of Variable objects
    p[0] = [p[1]] + p[3] if len(p) > 2 else [p[1]]
    return p


def p_var_declaration(p):
    """vardef : ID COLON type"""
    variable_name = p[1]
    variable_type = p[3]
    variable = Variable(variable_name, variable_type)

    # TODO: Check if variable is already defined
    # ? Maybe we should check if the variable is already defined in the current scope
    # ? i.e: if the procedure parameters have a variable with the same name as a higher scope variable

    print(f"Found variable: {variable}")
    found_variables[variable_name] = variable

    # Returns a Variable object
    p[0] = variable
    return p


def p_type(p):
    """type : INT
            | LPAREN typesopt RPAREN ARROW type"""

    # ? Maybe ARROW type means the return type of the parameter procedure?
    # Returns a string representing the type of the variable or procedure
    p[0] = p[1] if len(p) == 2 else p[5]
    return p


def p_type_opt(p):
    """typesopt : types
                | empty"""

    # Returns a list of strings representing the types, or an empty list
    # ! UNUSED: Typesopt is used in type, but type ignores it
    
    p[0] = p[1] if p[1] else []
    return p


def p_types(p):
    """types : type COMMA types
             | type"""

    # Returns a list of strings representing the types, or an empty list
    # ! UNUSED: Types is used in typesopt only. See above comment for more info

    p[0] = [p[1]] + (p[3] if len(p) == 4 else [])
    return p


def p_var_opt(p):
    """vardefsopt : VAR vardef SEMI vardefsopt
                  | empty"""
    
    # Returns a list of Variable objects, or an empty list
    p[0] = ([p[2]] + p[4]) if len(p) == 5 else []
    return p


def p_def_opt(p):
    """defdefsopt : defdefs
                  | empty"""
    

    # Returns a list of Procedure objects, or an empty list
    p[0] = p[1] if p[1] else []
    return p


def p_expras(p):
    """expras : expra SEMI expras
              | expra"""    
    # • expras
    # * o The type of an expras that derives expra SEMI expras is the type of the child expras.
    # * o The type of an expras that derives expra is the type of the expra.

    # Returns a string representing the type of the expression
    p[0] = p[3] if len(p) == 4 else p[1]
    return p


def p_expra(p):
    """expra : ID BECOMES expr
             | expr"""

    # ! o When an expra derives ID BECOMES expr, the type of the ID and
    # !     the expr must be the same, and the ID must denote a variable, not a
    # !     procedure. An expra always derives either expr or ID BECOMES expr; in
    # !     both cases, the type of the expra is the type of the child expr.

    child_expr = p[3] if len(p) == 4 else p[1]

    if len(p) == 4:
        child_id = p[1]
        var = found_variables.get(child_id)

        if var:
            if var.type != child_expr:
                print(f"Error in expra: Variable {var.name} is of type {var.type}, but expr is of type {child_expr}")
                return

            
        else:
            print(f"Error in expra: Variable {child_id} not found")
            return

    # Returns a string representing the type of the expression
    p[0] = child_expr

    return p


def p_expr(p):
    """expr : IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
            | term
            | expr PLUS term
            | expr MINUS term"""

    if len(p) == 2:
        # * o The type of an expr that derives a term is the type of the term.
        p[0] = p[1]

    elif len(p) == 4:
        # ! o When an expr derives expr PLUS term, or expr MINUS term, the types of
        # !     the child expr and term must be Int, and the type of the
        # !     resulting expr is Int.
        child_expr = p[1]
        child_term = p[3]

        if child_expr != "Int":
            print(f"Error in expr: Expected Int, got {child_expr} as child expr")
            return

        if child_term != "Int":
            print(f"Error in expr: Expected Int, got {child_term} as child term")
            return


        p[0] = "Int"

    else:
        # ! o When an expr derives IF LPAREN test RPAREN LBRACE expras
        # !     RBRACE ELSE LBRACE expras RBRACE, the type of the two exprs
        # !     derived from test must be Int, the types of the two child exprasmust be
        # !     the same, and the type of the resulting expr is the type of the two
        # !     child expras.
        p[0] = p[6]


    
    # Returns a string representing the type of the expression
    return p


def p_term(p):
    """term : factor
            | term STAR factor
            | term SLASH factor
            | term PCT factor"""

    if len(p) == 2:
        # * o The type of a term that derives a factor is the type of the factor.
        p[0] = p[1]

    else:
        # ! o When a term derives term STAR factor, term SLASH factor, or term PCT
        # !     factor, the types of the child term and factor must be Int, and the
        # !     type of the resulting term is Int.
        p[0] = "Int"

    return p


def p_factor(p):
    """factor : ID
              | NUM
              | LPAREN expr RPAREN
              | factor LPAREN argsopt RPAREN"""

    if len(p) == 2:
        # * o The type of a factor deriving an ID is the type of that ID.
        # * o The type of a factor deriving NUM is Int.
    
        var = found_variables.get(p[1])

        if p[1].isdigit():
            p[0] = "Int"

        elif var:
            p[0] = var.type

        else:
            print(f"Error in factor: Variable {p[1]} not found")
            return


    elif len(p) == 4:
        # * o The type of a factor deriving LPAREN expr RPAREN is the type of the expr.
        p[0] = p[2]

    else:
        # ! o When a factor derives factor LPAREN argsopt RPAREN, the type of the
        # !      child factor must be a procedure type whose parameter types are the
        # !      same as the types of the exprs occurring as children of the args derived
        # !      from the argsopt. The type of a factor deriving factor LPAREN argsopt
        # !      RPAREN is the return type of the child factor.

        pass # TODO

    return p


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

    # Returns a list of strings representing the types, or an empty list
    p[0] = p[1] if p[1] else []
    return p


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
