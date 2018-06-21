const PREC = {
  ASSIGN: -2,
  CONDITIONAL: -1,
  DEFAULT: 0,
  LOR: 2,
  LAND: 3,
  EQ: 4,
  NEQ: 4,
  LT: 5,
  LEQ: 5,
  GT: 5,
  GEQ: 5,
  SUB: 6,
  ADD: 6,
  MOD: 7,
  DIV: 7,
  MULT: 7,
  LEFT_DIV: 8,
  EL_DIV: 9,
  EL_MULT: 9,
  PLUS: 10,
  MINUS: 10,
  LNEG: 10,
  EXPON: 11,
  BRACKET: 12,
  INDEX: 12,
  PAREN: 12,
  FUNCTION: 12,
  TRANSPOSE: 12,
  RANGE: 13
};

module.exports = grammar({
  name: 'stan',

  word: $ => $.identifier,

  extras: $ => [
    /\s/,
    $.comment,
    $.include
  ],

  inline: $ => [
    $._statement
  ],

  conflicts: $ => [
    [$.array_expression, $.block_statement],
    [$._range_expression, $.block_statement],
    [$._range_expression, $.transformed_data],
    [$._range_expression, $.if_statement],
    [$._range_expression, $.while_statement],
    [$._range_expression, $.for_statement],
    [$._expression, $._range_expression],
  ],

  rules: {
    // The production rules of the context-free grammar
    program: $ => seq(
      optional($.functions),
      optional($.data),
      optional($.transformed_data),
      optional($.params),
      optional($.transformed_params),
      $.model,
      optional($.generated_quantities)
    ),

    functions: $ => seq(
      'functions',
      '{',
      repeat($.function_definition),
      '}'
    ),

    data: $ => seq(
      'data',
      '{',
      repeat($.variable_declaration),
      '}'
    ),

    transformed_data: $ => seq(
      'transformed',
      'data',
      '{',
      repeat($.variable_declaration),
      repeat($._statement),
      '}'
    ),

    params: $ => seq(
      'parameters',
      '{',
      repeat($.variable_declaration),
      '}'
    ),

    transformed_params: $ => seq(
      'transformed',
      'parameters',
      '{',
      repeat($.variable_declaration),
      repeat($._statement),
      '}'
    ),

    model: $ => seq(
      'model',
      // model statement; is valid. {} are not necessary.
      $._statement
    ),

    generated_quantities: $ => seq(
      'generated',
      'quantities',
      '{',
      repeat($.variable_declaration),
      repeat($._statement),
      '}'
    ),

    // Function declaration
    function_definition: $ => seq(
      $.return_type,
      $.function_declarator,
      $._statement
    ),

    function_declarator: $ => prec(1, seq(
      $.identifier,
      $.parameter_list
    )),

    parameter_list: $ => prec.dynamic(1, seq(
      '(',
      commaSep($.parameter_declaration),
      ')'
    )),

    parameter_declaration: $ => seq(
      $.unsized_type,
      $.identifier
    ),

    return_type: $ => choice(
      'void',
      $.unsized_type
    ),

    unsized_type: $ => seq(
      $.basic_type,
      optional(seq(
        '[',
        repeat(','),
        ']'
      ))
    ),

    basic_type: $ => prec.dynamic(1, choice(
      'int',
      'real',
      'vector',
      'row_vector',
      'matrix'
    )),

    // Variable Declarations
    variable_declaration: $ => prec(1, seq(
      $._var_type,
      $.identifier,
      optional($.dims),
      optional(seq('=', $._expression)),
      ';'
    )),

    _var_type: $ => choice(
      $.int_type,
      $.real_type,
      $.vector_type,
      $.ordered_type,
      $.positive_ordered_type,
      $.simplex_type,
      $.unit_vector_type,
      $.row_vector_type,
      $.matrix_type,
      $.cholesky_factor_cov_type,
      $.cholesky_factor_corr_type,
      $.cov_matrix_type,
      $.corr_matrix_type
    ),

    int_type: $ => seq(
      'int',
      optional($.range_constraint)
    ),

    real_type: $ => seq(
      'real',
      optional($.range_constraint)
    ),

    vector_type: $ => seq(
      'vector',
      optional($.range_constraint),
      '[',
      $._expression,
      ']'
    ),

    ordered_type: $ => seq(
      'ordered',
      '[',
      $._expression,
      ']'
    ),

    positive_ordered_type: $ => seq(
      'positive_ordered',
      '[',
      $._expression,
      ']'
    ),

    simplex_type: $ => seq(
      'simplex',
      '[',
      $._expression,
      ']'
    ),

    unit_vector_type: $ => seq(
      'unit_vector',
      '[',
      $._expression,
      ']'
    ),

    row_vector_type: $ => seq(
      'row_vector',
      optional($.range_constraint),
      '[',
      $._expression,
      ']'
    ),

    matrix_type: $ => seq(
      'matrix',
      optional($.range_constraint),
      '[',
      $._expression,
      ',',
      $._expression,
      ']'
    ),

    cholesky_factor_corr_type: $ => seq(
      'cholesky_factor_corr',
      '[',
      $._expression,
      ']'
    ),

    cholesky_factor_cov_type: $ => seq(
      'cholesky_factor_cov',
      '[',
      $._expression,
      ']'
    ),

    corr_matrix_type: $ => seq(
      'corr_matrix',
      '[',
      $._expression,
      ']'
    ),

    cov_matrix_type: $ => seq(
      'cov_matrix',
      '[',
      $._expression,
      ']'
    ),

    // this parses differently than Stan. Stan does not allow
    // ANY <> expressions inside the
    range_constraint: $ => prec(PREC.RANGE, choice(
      $.range_empty,
      $.range_lower,
      $.range_upper,
      $.range_lower_upper
    )),

    range_empty: $ => prec(PREC.RANGE, seq('<', '>')),

    range_lower_upper: $ => prec(PREC.RANGE, seq(
      '<',
      'lower',
      '=',
      $._range_expression,
      ',',
      'upper',
      '=',
      $._range_expression,
      '>'
    )),

    range_lower: $ => prec(PREC.RANGE, seq(
      '<',
      'lower',
      '=',
      $._range_expression,
      '>'
    )),

    range_upper: $ => prec(PREC.RANGE, seq(
      '<',
      'upper',
      '=',
      $._range_expression,
      '>'
    )),

    dims: $ => seq(
      '[',
      commaSep($._expression),
      ']'
    ),

    identifier: $ => /[A-Za-z][A-Za-z0-9_]*/,

    // Expressions
    _expression: $ => choice(
      $._range_expression,
      $.conditional_expression,
      $.infix_logical_expression
    ),

    // range constraints only allow a subset of expressions
    _range_expression: $ => choice(
      $.integer_literal,
      $.real_literal,
      $.identifier,
      $.array_expression,
      $.infix_math_expression,
      $.prefix_op_expression,
      $.postfix_op_expression,
      $.indexed_expression,
      $.function_expression,
      $.distr_expression,
      $.parenthized_expression,
    ),

    conditional_expression: $ => prec.right(PREC.CONDITIONAL, seq(
      $._expression,
      '?',
      $._expression,
      ':',
      $._expression
    )),

    array_expression: $ => seq(
      '{',
      commaSep($._expression),
      '}'
    ),

    infix_math_expression: $ => choice(
      ...[
        ['^', PREC.EXPON, prec.right],
        ['.*', PREC.EL_MULT, prec.left],
        ['./', PREC.EL_DIV, prec.left],
        ['\\', PREC.LEFT_DIV, prec.left],
        ['*', PREC.MULT, prec.left],
        ['%', PREC.MOD, prec.left],
        ['/', PREC.DIV, prec.left],
        ['+', PREC.ADD, prec.left],
        ['-', PREC.SUB, prec.left],
      ].map(([operator, precedence, fun]) =>
        fun(precedence, seq($._expression, operator, $._expression))
      )
    ),

    // these are separated because they cannot be used inside of
    // range constraints <>
    infix_logical_expression: $ => choice(
      ...[
        ['>=', PREC.GEQ, prec.left],
        ['>', PREC.GT, prec.left],
        ['<=', PREC.LEQ, prec.left],
        ['<', PREC.LT, prec.left],
        ['!=', PREC.NEQ, prec.left],
        ['==', PREC.EQ, prec.left],
        ['&&', PREC.LAND, prec.left],
        ['||', PREC.LOR, prec.left]
      ].map(([operator, precedence, fun]) =>
        fun(precedence, seq($._expression, operator, $._expression))
      )
    ),

    prefix_op_expression: $ => choice(
      ...[
        ['+', PREC.PLUS],
        ['-', PREC.MINUS],
        ['!', PREC.LNEG]
      ].map(([operator, precedence]) =>
             prec.left(precedence, seq(operator, $._expression)))
    ),

    postfix_op_expression: $ => choice(
      prec.right(PREC.TRANSPOSE, seq($._expression, '\''))
    ),

    // trick used for call expression in c grammar
    indexed_expression: $ => prec.left(PREC.INDEX, seq(
      $._expression,
      '[',
      commaSep1($.index),
      ']'
    )),

    function_expression: $ => prec(PREC.FUNCTION, seq(
      $.identifier,
      $.argument_list
    )),

    argument_list: $ => prec.dynamic(1, seq(
      '(',
      commaSep($._expression),
      ')'
    )),

    distr_expression: $ => prec(PREC.FUNCTION, seq(
      $.identifier,
      $.distr_argument_list
    )),

    distr_argument_list: $ => prec.dynamic(1, seq(
      '(',
      $._expression,
      '|',
      commaSep($._expression),
      ')'
    )),

    integrate_ode: $ => seq(
      'integrate_ode',
      '(',
      $.identifier,
      repRule($._expression, 6),
      ')'
    ),

    integrate_ode_r45: $ => seq(
      'integrate_ode_rk45',
      '(',
      $.identifier,
      choice(
        repRule($._expression, 6),
        repRule($._expression, 9)
      ),
      ')'
    ),

    algebra_solver: $ => seq(
      'algebra_solver',
      '(',
      $.identifier,
      choice(
        repRule($._expression, 4),
        repRule($._expression, 7)
      ),
      ')'
    ),

    integrate_ode_bdf: $ => seq(
      'integrate_ode_bdf',
      '(',
      $.identifier,
      choice(
        repRule($._expression, 6),
        repRule($._expression, 9)
      ),
      ')'
    ),

    parenthized_expression: $ => seq(
      '(',
      $._expression,
      ')'
    ),

    index: $ => choice(
      $._expression,
      seq($._expression, ":"),
      seq(":", $._expression),
      $._expression, ":", $._expression
    ),

    integer_literal: $ => /[0-9]+/,

    real_literal: $ => token(choice(
      /[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?/,
      /[0-9]+\.([eE][+-]?[0-9]+)?/,
      /[0-9]+[eE][+-]?[0-9]+/
    )),

    identifier: $ => /[A-Za-z][A-Za-z0-9_]*/,

    // Statements
    _statement: $ => choice(
      $.empty_statement,
      $.assignment_statement,
      $.sampling_statement,
      $.function_expression,
      $.log_prob_statement,
      $.target_statement,
      $.break_statement,
      $.continue_statement,
      $.print_statement,
      $.reject_statement,
      $.return_statement,
      $.if_statement,
      $.while_statement,
      $.for_statement,
      $.block_statement
    ),

    empty_statement: $ => ';',

    assignment_statement: $ => prec(PREC.ASSIGN, seq(
      $.identifier,
      optional(seq('[', commaSep1($.index), ']')),
      $.assignment_op,
      $._expression,
      ';'
    )),

    assignment_op: $ => choice(
      "<-",
      "=",
      "+=",
      "-=",
      "/=",
      ".*=",
      "./="
    ),

    sampling_statement: $ => seq(
      $._expression,
      '~',
      $.identifier,
      '(',
      commaSep($._expression),
      ')',
      optional($.truncation),
      ';'
    ),

    truncation: $ => seq(
      'T',
      '[',
      optional($._expression),
      ',',
      optional($._expression),
      ']'
    ),

    log_prob_statement: $ => seq(
      'increment_log_prob',
      '(',
      $._expression,
      ')',
      ';'
    ),

    target_statement: $ => seq(
      'target',
      '+=',
      $._expression,
      ';'
    ),

    break_statement: $ => seq('break', ';'),

    continue_statement: $ => seq('continue', ';'),

    print_statement: $ => seq(
      'print',
      '(',
      optional(choice($._expression, $.string_literal)),
      ')',
      ';'
    ),

    // currently nested quotes not allowed
    string_literal: $ => /"[A-Za-z0-9 ~@#$%^&*_'`+{}[\]()<>|/!?.,;:-]*"/,

    reject_statement: $ => seq(
      'reject',
      '(',
      optional(choice($._expression, $.string_literal)),
      ')',
      ';'
    ),

    return_statement: $ => seq(
      'return',
      $._expression,
      ';'
    ),

    // right precedence needed to resolve ambiguity
    if_statement: $ => prec.right(seq(
      'if',
      '(', $._expression, ')',
      $._statement,
      // implicit else if
      optional(seq('else', $._statement))
    )),

    while_statement: $ => seq(
      'while',
      '(',
      $._expression,
      ')',
      $._statement
    ),

    for_statement: $ => seq(
      'for',
      '(',
      $.identifier,
      'in',
      $._expression,
      ':',
      $._expression,
      ')',
      $._statement
    ),

    block_statement: $ => seq(
      '{',
      repeat($.variable_declaration),
      repeat($._statement),
      '}'
    ),

    include: $ => token(seq(
      // does include have to be at the start of the line?
      '#include',
      '/.*/'
    )),

    // OTHER
    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: $ => token(choice(
       seq('//', /.*/),
       seq('#', /.*/),
       seq(
         '/*',
         /[^*]*\*+([^/*][^*]*\*+)*/,
         '/'
       )
    )),

  }
});

function commaSep1(rule) {
  return seq(
    rule,
    repeat(seq(',', rule))
  )
}

function commaSep(rule) {
  return optional(commaSep1(rule))
}

// repeat rule n times
function repRule(rule, n) {
  return seq(...Array(n).fill(rule))
}
