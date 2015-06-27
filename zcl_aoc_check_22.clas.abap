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

  methods ANALYZE_CONDITION
    importing
      !IO_STRUCTURE type ref to ZCL_AOC_STRUCTURE .
  class ZCL_AOC_STRUCTURE definition load .
  type-pools ABAP .
  methods COMPARE
    importing
      !IT_STRUCTURE type ZCL_AOC_STRUCTURE=>TY_STRUCTURE_TT
      !IV_FIRST_LAST type ABAP_BOOL .
  methods LOOP
    importing
      !IO_STRUCTURE type ref to ZCL_AOC_STRUCTURE .
private section.
*"* private components of class ZCL_AOC_CHECK_22
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_22'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_22 IMPLEMENTATION.


METHOD analyze_condition.

  DATA: lo_structure TYPE REF TO zcl_aoc_structure,
        lv_found     TYPE abap_bool.


* IFs must contain ELSE, CASE must contain OTHERS
  LOOP AT io_structure->mt_structure INTO lo_structure.
    IF ( io_structure->mv_stmnt_type = scan_struc_stmnt_type-if
        AND lo_structure->ms_statement-statement = 'ELSE' )
        OR ( io_structure->mv_stmnt_type = scan_struc_stmnt_type-case
        AND lo_structure->ms_statement-statement = 'WHEN OTHERS' ).
      lv_found = abap_true.
      EXIT. " current loop.
    ENDIF.
  ENDLOOP.

  IF lv_found = abap_false.
    RETURN.
  ENDIF.

  compare( it_structure  = io_structure->mt_structure
           iv_first_last = abap_true ).
  compare( it_structure  = io_structure->mt_structure
           iv_first_last = abap_false ).

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  loop( zcl_aoc_structure=>build( it_tokens     = it_tokens
                                  it_statements = it_statements
                                  it_levels     = it_levels
                                  it_structures = it_structures ) ).

ENDMETHOD.


METHOD compare.

  DATA: lo_stru    TYPE REF TO zcl_aoc_structure,
        lo_first   TYPE REF TO zcl_aoc_structure,
        lo_compare TYPE REF TO zcl_aoc_structure,
        lv_str1    TYPE string,
        lv_str2    TYPE string,
        lv_index   TYPE i.


* compare first or last statement in each branch
  LOOP AT it_structure INTO lo_stru.

    IF iv_first_last = abap_true.
      lv_index = 1.
    ELSE.
      lv_index = lines( lo_stru->mt_structure ).
    ENDIF.

    IF NOT lo_first IS BOUND.
      READ TABLE lo_stru->mt_structure INDEX lv_index INTO lo_first.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      lv_str1 = zcl_aoc_structure=>to_string_simple( lo_first ).
      CONTINUE. " current loop
    ENDIF.
    READ TABLE lo_stru->mt_structure INDEX lv_index INTO lo_compare.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_str2 = zcl_aoc_structure=>to_string_simple( lo_compare ).
    IF lv_str1 <> lv_str2.
      RETURN.
    ENDIF.

  ENDLOOP.
  IF sy-subrc <> 0.
    RETURN. " list is empty
  ENDIF.

  IF lv_str1 IS INITIAL.
    RETURN.
  ENDIF.

  inform( p_sub_obj_type = c_type_include
          p_sub_obj_name = get_include( p_level = lo_first->ms_statement-level )
          p_line         = lo_first->ms_statement-row
          p_kind         = mv_errty
          p_test         = c_my_name
          p_code         = '001'
          p_param_1      = lv_str1 ).

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
      p_text = 'Conditions contain identical code, &1'.      "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD loop.

  DATA: lo_structure TYPE REF TO zcl_aoc_structure.


  CASE io_structure->mv_stmnt_type.
    WHEN scan_struc_stmnt_type-if OR scan_struc_stmnt_type-case.
      analyze_condition( io_structure ).
  ENDCASE.

  LOOP AT io_structure->mt_structure INTO lo_structure.
    loop( lo_structure ).
  ENDLOOP.

ENDMETHOD.
ENDCLASS.