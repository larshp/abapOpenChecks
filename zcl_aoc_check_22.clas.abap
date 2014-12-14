class ZCL_AOC_CHECK_22 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_22
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_22
*"* do not include other source files here!!!

  types:
    BEGIN OF ty_condition,
           statements TYPE string_table,
           level type STMNT_LEVL,
           row type TOKEN_ROW,
           STMNT_TYPE TYPE STRU_TYPE,
           cond type string,
         END OF ty_condition .
  types:
    tt_conditions TYPE STANDARD TABLE OF ty_condition WITH NON-UNIQUE DEFAULT KEY .

  methods ANALYZE
    importing
      !IT_CONDITIONS type TT_CONDITIONS .
  methods CONDITIONS
    importing
      !IS_ALTERNATION type SSTRUC
      !IT_STRUCTURES type TT_STRUCTURES
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_TOKENS type STOKESX_TAB
    changing
      !CT_CONDITIONS type TT_CONDITIONS .
private section.
*"* private components of class ZCL_AOC_CHECK_22
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_22'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_22 IMPLEMENTATION.


METHOD analyze.

  DATA: lv_inform    TYPE abap_bool,
        lv_search    TYPE string,
        lv_statement TYPE string.

  FIELD-SYMBOLS: <ls_condition> LIKE LINE OF it_conditions.


  LOOP AT it_conditions ASSIGNING <ls_condition>.
    IF lines( <ls_condition>-statements ) = 0.
      RETURN.
    ENDIF.
  ENDLOOP.

  IF lines( it_conditions ) <= 1.
    RETURN.
  ENDIF.


* first statements in each condition
  lv_inform = abap_true.
  LOOP AT it_conditions ASSIGNING <ls_condition>.
    READ TABLE <ls_condition>-statements INDEX 1 INTO lv_statement.
    IF lv_search IS INITIAL.
      lv_search = lv_statement.
    ENDIF.
    IF lv_search <> lv_statement.
      lv_inform = abap_false.
    ENDIF.
  ENDLOOP.

* last statements
  IF lv_inform = abap_false.
    CLEAR lv_search.
    lv_inform = abap_true.
    LOOP AT it_conditions ASSIGNING <ls_condition>.
      READ TABLE <ls_condition>-statements
        INDEX lines( <ls_condition>-statements )
        INTO lv_statement.
      IF lv_search IS INITIAL.
        lv_search = lv_statement.
      ENDIF.
      IF lv_search <> lv_statement.
        lv_inform = abap_false.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lv_inform = abap_true.
    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = get_include( p_level = <ls_condition>-level )
            p_line         = <ls_condition>-row
            p_kind         = mv_errty
            p_test         = c_my_name
            p_code         = '001' ).
  ENDIF.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_conditions TYPE tt_conditions.

  FIELD-SYMBOLS: <ls_alternation> LIKE LINE OF it_structures.


  LOOP AT it_structures ASSIGNING <ls_alternation>
      WHERE type = scan_struc_type-alternation
      AND key_start = abap_true
      AND key_end = abap_true.

    conditions(
      EXPORTING
        is_alternation = <ls_alternation>
        it_structures  = it_structures
        it_statements  = it_statements
        it_tokens      = it_tokens
      CHANGING
        ct_conditions  = lt_conditions ).

    analyze( lt_conditions ).

  ENDLOOP.

ENDMETHOD.


METHOD conditions.

  DATA: lv_statement TYPE string.

  FIELD-SYMBOLS: <ls_token>     LIKE LINE OF it_tokens,
                 <ls_statement> LIKE LINE OF it_statements,
                 <ls_condition> LIKE LINE OF ct_conditions,
                 <lv_statement> LIKE LINE OF <ls_condition>-statements,
                 <ls_structure> LIKE LINE OF it_structures.


  CLEAR ct_conditions.

  LOOP AT it_structures ASSIGNING <ls_structure>
      FROM is_alternation-struc_from
      TO is_alternation-struc_to.

    APPEND INITIAL LINE TO ct_conditions ASSIGNING <ls_condition>.
    <ls_condition>-stmnt_type = <ls_structure>-stmnt_type.

    LOOP AT it_statements ASSIGNING <ls_statement>
        FROM <ls_structure>-stmnt_from
        TO <ls_structure>-stmnt_to
        WHERE type <> scan_stmnt_type-trmac_call
        AND type <> scan_stmnt_type-macro_call.

      IF <ls_condition>-level IS INITIAL.
        <ls_condition>-level = <ls_statement>-level.
      ENDIF.

      CLEAR lv_statement.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from
          TO <ls_statement>-to
          WHERE type <> scan_token_type-comment.

        IF <ls_condition>-row IS INITIAL.
          <ls_condition>-row = <ls_token>-row.
        ENDIF.

* todo, statement type = C?
        IF <ls_token>-str = 'ENDIF' OR <ls_token>-str = 'ENDLOOP'.
          READ TABLE <ls_condition>-statements
            INDEX lines( <ls_condition>-statements )
            ASSIGNING <lv_statement>.
          ASSERT sy-subrc = 0.
          CONCATENATE <lv_statement> <ls_token>-str
            INTO <lv_statement> SEPARATED BY space.
          CONTINUE. " current loop
        ENDIF.

        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF NOT lv_statement IS INITIAL.
        APPEND lv_statement TO <ls_condition>-statements.
      ENDIF.
    ENDLOOP.

    IF <ls_structure>-key_start = abap_true.
      READ TABLE <ls_condition>-statements INDEX 1 INTO <ls_condition>-cond.
      DELETE <ls_condition>-statements INDEX 1.
    ENDIF.

  ENDLOOP.


* IFs must contain ELSE
  IF is_alternation-stmnt_type = scan_struc_stmnt_type-if.
    READ TABLE ct_conditions WITH KEY stmnt_type = scan_struc_stmnt_type-else
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CLEAR ct_conditions.
    ENDIF.
  ENDIF.

* CASE must contain OTHERS
  IF is_alternation-stmnt_type = scan_struc_stmnt_type-case.
    READ TABLE ct_conditions WITH KEY cond = 'WHEN OTHERS'
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CLEAR ct_conditions.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Conditions contain identical code'.     "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty       = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Conditions contain identical code'.         "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.