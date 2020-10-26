CLASS zcl_aoc_check_100 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_check_flag,
        inline_declarations   TYPE flag,
        constructor_operators TYPE flag,
        iteration_expressions TYPE flag,
        builtin_functions     TYPE flag,
        string_templates      TYPE flag,
        table_expressions     TYPE flag,
        opensql               TYPE flag,
        abap_constants        TYPE flag,
        chained_statements    TYPE flag,
        pragmas               TYPE flag,
        assignment_operators  TYPE flag,
      END OF ty_check_flag .

    CONSTANTS:
      BEGIN OF ci_error_code,
        inline_declarations   TYPE scimessage-code VALUE 'InlineDecl', "#EC NOTEXT
        constructor_operators TYPE scimessage-code VALUE 'ConstrOper', "#EC NOTEXT
        iteration_expressions TYPE scimessage-code VALUE 'IteratExp', "#EC NOTEXT
        builtin_functions     TYPE scimessage-code VALUE 'BuiltInFun', "#EC NOTEXT
        string_templates      TYPE scimessage-code VALUE 'StrTemplat', "#EC NOTEXT
        table_expressions     TYPE scimessage-code VALUE 'TableExpr', "#EC NOTEXT
        opensql               TYPE scimessage-code VALUE 'OpenSQL', "#EC NOTEXT
        abap_constants        TYPE scimessage-code VALUE 'ABAPConst', "#EC NOTEXT
        chained_statements    TYPE scimessage-code VALUE 'ChainedSta', "#EC NOTEXT
        pragmas               TYPE scimessage-code VALUE 'Pragma', "#EC NOTEXT
        assignment_operators  TYPE scimessage-code VALUE 'AssignOper', "#EC NOTEXT
      END OF ci_error_code .

    METHODS constructor .

    METHODS check
         REDEFINITION .
    METHODS get_attributes
         REDEFINITION .
    METHODS if_ci_test~query_attributes
         REDEFINITION .
    METHODS put_attributes
         REDEFINITION .
  PROTECTED SECTION.

    DATA ms_check_flag TYPE ty_check_flag.
  PRIVATE SECTION.

    METHODS check_abap_constants
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_builtin_functions
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_opensql
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_string_templates
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_table_expressions
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_iteration_expressions
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_assignment_operators
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_constructor_operators
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_pragmas
      IMPORTING
        !is_statement        TYPE sstmnt
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_chained_statements
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
    METHODS check_inline_declarations
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc .
ENDCLASS.



CLASS ZCL_AOC_CHECK_100 IMPLEMENTATION.


  METHOD check.
* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA:
      lt_code      TYPE STANDARD TABLE OF sci_errc,
      lv_position  TYPE i,
      lv_include   TYPE sobj_name,
      lv_token     TYPE string,
      lv_statement TYPE string,
      lv_row       TYPE token_row,
      lv_code      TYPE sci_errc.

    FIELD-SYMBOLS:
      <ls_token>     LIKE LINE OF io_scan->tokens,
      <ls_statement> LIKE LINE OF io_scan->statements,
      <ls_code>      LIKE LINE OF lt_code.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>.
      lv_position = sy-tabix.
      CLEAR: lt_code, lv_statement.

      LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
        IF <ls_token>-type <> io_scan->gc_token-identifier.
          lv_token = 'SOMETHING'.
        ELSE.
          lv_token = <ls_token>-str.
        ENDIF.

        IF lv_statement IS INITIAL.
          lv_statement = lv_token.
        ELSE.
          CONCATENATE lv_statement lv_token INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF <ls_token> IS ASSIGNED.
        lv_row = <ls_token>-row.
      ENDIF.

      IF ms_check_flag-inline_declarations = abap_true.
        APPEND check_inline_declarations( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-constructor_operators = abap_true.
        APPEND check_constructor_operators( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-iteration_expressions = abap_true.
        APPEND check_iteration_expressions( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-string_templates = abap_true.
        APPEND check_string_templates( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-builtin_functions = abap_true.
        APPEND check_builtin_functions( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-table_expressions = abap_true.
        APPEND check_table_expressions( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-opensql = abap_true.
        APPEND check_opensql( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-abap_constants = abap_true.
        APPEND check_abap_constants( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-chained_statements = abap_true.
        APPEND check_chained_statements( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-assignment_operators = abap_true.
        APPEND check_assignment_operators( lv_statement ) TO lt_code.
      ENDIF.

      IF ms_check_flag-pragmas = abap_true.
        lv_code = check_pragmas( <ls_statement> ).
        IF lv_code IS NOT INITIAL.
          APPEND lv_code TO lt_code.
          CLEAR lv_position.
          lv_row = <ls_statement>-trow.
        ENDIF.
      ENDIF.

      DELETE lt_code WHERE table_line = space.

      IF lt_code IS NOT INITIAL.
        lv_include = io_scan->get_include( <ls_statement>-level ).

        LOOP AT lt_code ASSIGNING <ls_code>.
          inform( p_sub_obj_name = lv_include
                  p_position     = lv_position
                  p_line         = lv_row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = <ls_code> ).
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_abap_constants.
    IF iv_statement CP '* abap_bool*'
        OR iv_statement CP '* abap_true*'
        OR iv_statement CP '* abap_false*'
        OR iv_statement CP '* abap_undefined*'.
      rv_error_code = ci_error_code-abap_constants.
    ENDIF.
  ENDMETHOD.


  METHOD check_assignment_operators.
    FIND FIRST OCCURRENCE OF REGEX '\b\s+(\+|-|\*|\/|&&)=\s+\b' IN iv_statement IGNORING CASE ##NO_TEXT.
    IF sy-subrc = 0.
      rv_error_code = ci_error_code-assignment_operators.
    ENDIF.
  ENDMETHOD.


  METHOD check_builtin_functions.
    IF iv_statement CP '* line_index( * )*'
        OR iv_statement CP '* line_exists( * )*'.
      rv_error_code = ci_error_code-builtin_functions.
    ENDIF.
  ENDMETHOD.


  METHOD check_chained_statements.
    FIND FIRST OCCURRENCE OF REGEX '(=|-)>\w+\(.+\)(=|-)>\w+\(.+\)' IN iv_statement ##NO_TEXT.
    IF sy-subrc = 0.
      rv_error_code = ci_error_code-chained_statements.
    ENDIF.
  ENDMETHOD.


  METHOD check_constructor_operators.
    IF iv_statement CP '* NEW *( * )*'
        OR iv_statement CP '* VALUE *( * )*'
        OR iv_statement CP '* CONV *( * )*'
        OR iv_statement CP '* CAST *( * )*'
        OR iv_statement CP '* REF *( * )*'
        OR iv_statement CP '* CORRESPONDING *( * )*'
        OR iv_statement CP '* EXACT *( * )*'
        OR iv_statement CP '* REDUCE *( * )*'
        OR iv_statement CP '* FILTER *( * )*'
        OR iv_statement CP '* COND *( * )*'
        OR iv_statement CP '* SWITCH *( * )*'.
      rv_error_code = ci_error_code-constructor_operators.
    ENDIF.
  ENDMETHOD.


  METHOD check_inline_declarations.
    FIND FIRST OCCURRENCE OF REGEX '\bDATA\(.+\)' IN iv_statement IGNORING CASE ##NO_TEXT.
    IF sy-subrc = 0.
      rv_error_code = ci_error_code-inline_declarations.
    ENDIF.

    FIND FIRST OCCURRENCE OF REGEX '\bFIELD-SYMBOL\(.+\)' IN iv_statement IGNORING CASE ##NO_TEXT.
    IF sy-subrc = 0.
      rv_error_code = ci_error_code-inline_declarations.
    ENDIF.
  ENDMETHOD.


  METHOD check_iteration_expressions.
    IF iv_statement CP '* (*FOR * )*'
        OR iv_statement CP 'LOOP AT * GROUP BY *'.
      rv_error_code = ci_error_code-iteration_expressions.
    ENDIF.
  ENDMETHOD.


  METHOD check_opensql.
    RETURN.
  ENDMETHOD.


  METHOD check_pragmas.
    IF is_statement-type = 'G' AND
        ( is_statement-terminator = '.' OR is_statement-terminator = ',' OR is_statement-terminator = ':' ).
      rv_error_code = ci_error_code-pragmas.
    ENDIF.
  ENDMETHOD.


  METHOD check_string_templates.
    IF iv_statement CP '*|*|*'
        OR iv_statement CP '* && *'.
      rv_error_code = ci_error_code-string_templates.
    ENDIF.
  ENDMETHOD.


  METHOD check_table_expressions.
    IF iv_statement CP '*[ * ]*'.
      rv_error_code = ci_error_code-table_expressions.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    version        = '001'.
    position       = '100'.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    insert_scimessage( iv_code = ci_error_code-inline_declarations iv_text = 'Inline Declarations detected'(m01) ).
    insert_scimessage( iv_code = ci_error_code-constructor_operators iv_text = 'Constructor Operators detected'(m02) ).
    insert_scimessage( iv_code = ci_error_code-iteration_expressions iv_text = 'Iteration Expressions detected'(m03) ).
    insert_scimessage( iv_code = ci_error_code-string_templates iv_text = 'String Templates detected'(m04) ).
    insert_scimessage( iv_code = ci_error_code-builtin_functions iv_text = 'Built-in Functions detected'(m05) ).
    insert_scimessage( iv_code = ci_error_code-table_expressions iv_text = 'Table Expressions detected'(m06) ).
    insert_scimessage( iv_code = ci_error_code-opensql iv_text = 'New syntax for OpenSQL detected'(m07) ).
    insert_scimessage( iv_code = ci_error_code-abap_constants iv_text = 'New syntax for ABAP Constants detected'(m08) ).
    insert_scimessage( iv_code = ci_error_code-chained_statements iv_text = 'Chained Statements detected'(m09) ).
    insert_scimessage( iv_code = ci_error_code-pragmas iv_text = 'Pragmas detected'(m10) ).
    insert_scimessage( iv_code = ci_error_code-assignment_operators iv_text = 'Assignment Operators detected'(m11) ).

    ms_check_flag-inline_declarations   = abap_true.
    ms_check_flag-constructor_operators = abap_true.
    ms_check_flag-iteration_expressions = abap_true.
    ms_check_flag-builtin_functions     = abap_true.
    ms_check_flag-string_templates      = abap_true.
    ms_check_flag-table_expressions     = abap_true.
    ms_check_flag-opensql               = abap_true.
    ms_check_flag-abap_constants        = abap_true.
    ms_check_flag-chained_statements    = abap_true.
    ms_check_flag-pragmas               = abap_true.
    ms_check_flag-assignment_operators  = abap_true.
  ENDMETHOD.


  METHOD get_attributes.
    EXPORT
      mv_errty = mv_errty
      ms_check_flag = ms_check_flag
      TO DATA BUFFER p_attributes.
  ENDMETHOD.


  METHOD if_ci_test~query_attributes.
    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type'(t00) ''.
    zzaoc_fill_att ms_check_flag-inline_declarations 'Inline Declarations'(t01) ''.
    zzaoc_fill_att ms_check_flag-constructor_operators 'Constructor Operators'(t02) ''.
    zzaoc_fill_att ms_check_flag-iteration_expressions 'Iteration Expressions'(t03) ''.
    zzaoc_fill_att ms_check_flag-string_templates 'String Templates'(t04) ''.
    zzaoc_fill_att ms_check_flag-builtin_functions 'Built-in Functions'(t05) ''.
    zzaoc_fill_att ms_check_flag-table_expressions 'Table Expressions'(t06) ''.
    zzaoc_fill_att ms_check_flag-opensql 'OpenSQL'(t07) ''.
    zzaoc_fill_att ms_check_flag-abap_constants 'ABAP Constants'(t08) ''.
    zzaoc_fill_att ms_check_flag-chained_statements 'Chained Statements'(t09) ''.
    zzaoc_fill_att ms_check_flag-pragmas 'Pragmas'(t10) ''.
    zzaoc_fill_att ms_check_flag-assignment_operators 'Assignment Operators'(t11) ''.

    zzaoc_popup.
  ENDMETHOD.


  METHOD put_attributes.
    IMPORT
      mv_errty = mv_errty
      ms_check_flag = ms_check_flag
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.
  ENDMETHOD.
ENDCLASS.
