# Programs

```ebnf
program : functions? data? transfomed_data? parameters? transfomed_parameters? model? generated_quantities?;
functions : 'functions' function_decls;
data : 'data' var_decls;
transformed_data : 'transformed data' var_decls_statements;
parameters : 'parameters' var_decls;
transformed_parameters : 'transformed parameters' var_decls_statements;
model : 'model' statement;
generated : 'generated quantities' var_decls_statements;
function_decls : '{' function_decl* '}';
var_decls : '{' var_decl* '}';
var_decls_statements : '{' var_decl* statement* '}';
```

# Function Declarations

```ebnf
function_declaration : unsized_return_type identifier '(' unsized_types ')' statement?;

unsized_return_type : 'void' | unsized_type;
unsized_type : basic_type unsized_dims?;
unsized_types : unsized_type (',' unsized_type)*;
basic_type : 'int' | 'real' | 'vector' | 'row_vector' | 'matrix';
unsized_dims : '[' ','* ']';
```

# Variable Declarations

```
variable_declaration : variable_type identifier dims? ('=' expression)? ';';

variable_type : 'int' range_constraint;
| 'real' range_constraint;
| 'vector' range_constraint '[' expression ']';
| 'ordered' '[' expression ']';
| 'positive_ordered' '[' expression ']';
| 'simplex' '[' expression ']';
| 'unit_vector' '[' expression ']';
| 'row_vector' range_constraint '[' expression ']';
| 'matrix' range_constraint '[' expression ',' expression ']';
| 'cholesky_factor_corr' '[' expression ']';
| 'cholesky_factor_cov' '[' expression (',' expression) ']'?;
| 'corr_matrix' '[' expression ']';
| 'cov_matrix' '[' expression ']';

range_constraint : ?('<' range '>')

range : 'lower' '=' range_expression ',' 'upper' = expression
  | 'lower' '=' range_expression
  | 'upper' '=' range_expression
dims : '[' expressions ']'
variable : identifier
identifier : [a-zA-Z] [a-zA-Z0-9_]*
```

# Expressions

```
expressions : expression % ','

expression : range_expression
  | expression infixLogicalOp expression
  | in

range_expression : numeric_literal
  | variable
  | '{' expressions '}'
  | expression `?` expression `:` expression
  | expression infixMathOp expression
  | expression infixLogicalOp expression
  | prefixOp expression
  | expression postfixOp
  | expression '[' indexes ']'
  | function_literal '(' ?expressions ')'
  | function_literal '(' expression ?('|' expression % ',') ')'
  | integrate_ode '(' function_literal (',' expression){6} ')'
  | integrate_ode_rk45 '(' function_literal (',' expression){6|9} ')'
  | integrate_ode_bdf '(' function_literal (',' expression){6|9} ')'
  | algebra_solver '('function_literal (',' expression){4|7} ')'
  | '(' expression ')' ;

LOGICAL_INFIX_OP : '&&' | '||' | '<' | '<=' | '>' | '>='
ARITHMETIC_INFIX_OP : '+' | '-' | '*' | '/' | '\' | '.*' | './'
PREFIX_OP =

index : expression
  | expression ':'
  | ':' expression
  | expression ':' expression;
indexes : index (',' index)*;
numeric_literal : INTEGER_LITERAL | real_literal
INTEGER_LITERAL : [0-9]+;
real_literal : integer_literal '.' [0-9]* exp_literal?
  | '.' [0-9]+ exp_literal?
  | integer_literal exp_literal?;
exponent_literal : ('e' | 'E') ('+' | '-')? integer_literal;
function_literal : identifier;
```

# Statements

```
statement : atomic_statement | nested_statement
atomic_statement : lhs assignment_op expression
  | expression '~' identifier '(' expressions ')' ?truncation ';' // sampling statement
  | function_literal '(' expressions ')' ';' // function call
  | 'increment_log_prob' '(' expression ')' ';' // increment_log_prob statement
  | 'target' '+=' expression ';' // target statement
  | 'break' ';' //
  | 'continue' ';'
  | 'print' '(' (expression | string_literal) % ',' ')' ';'
  | 'reject' '(' (expression | string_literal) % ',' ')' ';'
  | 'return' expression ';'
  | ';' ;

assignment_op : '<-' | '=' | '+=' | '-=' | '*=' | '/=' | '.*=' | '/*='
string_literal : '"' char* '"'

truncation : 'T' '[' expression? ',' expression? ']';

lhs : identifier ('[' indexes ']')*;

nested_statement :
  | 'if' '(' expression ')' statement
    ('else' 'if' '(' expression ')' statement)*
    ('else' statement)?
  | 'while' '(' expression ')' statement
  | 'for' '(' identifier 'in' expression ':' expression ')' statement
  | '{' var_decl* statement+ '}' ;
```

# Edits

-  added +/- to exp_literal
-  update integer_literal and real_literal
-  fixed range expression
-  Could if statement be written as

    ```
    'if' '(' expression ')' statement
    ?('else' statement)
    ```

-   Add comma separator to print() and reject() statements
-   Allow multiple indexes in LHS of assignments


Questions

- function declaration - optional
- fix exp
- cholesky factor cov (allow unbalanced) ? 
