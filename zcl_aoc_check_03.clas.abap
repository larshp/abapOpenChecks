class ZCL_AOC_CHECK_03 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_03
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.

  methods CHECK_NESTED
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
      !IT_STRUCTURES type Ty_STRUCTURES_tt .
  methods CHECK_NO_CATCH
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
      !IT_STRUCTURES type Ty_STRUCTURES_tt .
*"* protected components of class ZCL_AOC_CHECK_03
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_03 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  check_no_catch( it_tokens     = it_tokens
                  it_statements = it_statements
                  it_levels     = it_levels
                  it_structures = it_structures ).

  check_nested( it_tokens     = it_tokens
                it_statements = it_statements
                it_levels     = it_levels
                it_structures = it_structures ).

ENDMETHOD.


METHOD check_nested.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_index TYPE i,
        lv_include TYPE program,
        lv_error TYPE abap_bool,
        lv_exception TYPE string.

  FIELD-SYMBOLS: <ls_token> LIKE LINE OF it_tokens,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type <> scan_stmnt_type-comment
      AND type <> scan_stmnt_type-empty
      AND type <> scan_stmnt_type-comment_in_stmnt
      AND type <> scan_stmnt_type-pragma.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    IF sy-subrc <> 0.
      CLEAR lv_exception.
      CONTINUE.
    ENDIF.

    IF <ls_token>-str = 'CATCH'.
      lv_index = <ls_statement>-from + 1.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX lv_index.
      IF sy-subrc <> 0.
        CLEAR lv_exception.
        CONTINUE.
      ENDIF.

      IF lv_exception = <ls_token>-str.
        lv_error = abap_true.
      ELSE.
        lv_error = abap_false.
      ENDIF.
      lv_exception = <ls_token>-str.
    ELSEIF <ls_token>-str = 'ENDTRY'.
      IF lv_error = abap_true AND NOT lv_exception IS INITIAL.
        lv_include = get_include( p_level = <ls_statement>-level ).
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line = <ls_token>-row
                p_kind = mv_errty
                p_test = myname
                p_code = '002' ).
      ENDIF.
      lv_error = abap_false.
      CONTINUE.
    ELSE.
      CLEAR lv_exception.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD check_no_catch.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include TYPE program,
        lv_found   TYPE abap_bool,
        lv_index   LIKE sy-tabix.

  FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures,
                 <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_structures ASSIGNING <ls_structure>
      WHERE stmnt_type = scan_struc_stmnt_type-try.
    lv_index = sy-tabix.

    lv_found = abap_false.

    READ TABLE it_structures
      WITH KEY stmnt_type = scan_struc_stmnt_type-catch back = lv_index
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_found = abap_true.
    ENDIF.

    READ TABLE it_structures
      WITH KEY stmnt_type = scan_struc_stmnt_type-cleanup back = lv_index
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_found = abap_true.
    ENDIF.

    IF lv_found = abap_false.

      READ TABLE it_statements ASSIGNING <ls_statement> INDEX <ls_structure>-stmnt_from.
      ASSERT sy-subrc = 0.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      ASSERT sy-subrc = 0.

      lv_include = get_include( p_level = <ls_statement>-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line = <ls_token>-row
              p_kind = mv_errty
              p_test = myname
              p_code = '001' ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Wrong use of TRY-CATCH'.                "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '003'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'TRY without CATCH'.                         "#EC NOTEXT
    WHEN '002'.
      p_text = 'Nesting with same exception'.               "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.