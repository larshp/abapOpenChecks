class ZCL_AOC_CHECK_01 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_01
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_01
*"* do not include other source files here!!!

  data MV_ERRTY type SCI_ERRTY .
private section.
*"* private components of class ZCL_AOC_CHECK_01
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_01'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_01 IMPLEMENTATION.


METHOD check.

  DATA: lv_after_start TYPE string,
        lv_line        TYPE token_row,
        lv_count       TYPE i,
        lv_before_end  TYPE string.

  FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures,
                 <ls_level>     LIKE LINE OF it_levels.


  LOOP AT it_levels ASSIGNING <ls_level>.

* only run for lowest level
    READ TABLE it_levels WITH KEY level = sy-tabix TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE. " current loop
    ENDIF.

    LOOP AT it_structures ASSIGNING <ls_structure>
        WHERE stmnt_type = scan_struc_stmnt_type-if
        AND stmnt_from >= <ls_level>-from
        AND stmnt_to <= <ls_level>-to.

* continue if it contains ELSEIF or ELSE
      LOOP AT it_structures TRANSPORTING NO FIELDS
          WHERE stmnt_from >= <ls_structure>-stmnt_from
          AND stmnt_to <= <ls_structure>-stmnt_to
          AND ( stmnt_type = scan_struc_stmnt_type-elseif OR stmnt_type = scan_struc_stmnt_type-else ).
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        CONTINUE. " current loop
      ENDIF.

* check that it only contains one IF
      lv_count = 0.
      LOOP AT it_structures TRANSPORTING NO FIELDS
          WHERE stmnt_from > <ls_structure>-stmnt_from
          AND stmnt_to < <ls_structure>-stmnt_to
          AND stmnt_type = scan_struc_stmnt_type-if.
        lv_count = lv_count + 1.
      ENDLOOP.
      IF lv_count <> 1.
        CONTINUE. " current loop
      ENDIF.

      lv_after_start = statement_keyword(
          iv_number     = <ls_structure>-stmnt_from + 1
          it_statements = it_statements
          it_tokens     = it_tokens ).

      lv_before_end = statement_keyword(
          iv_number     = <ls_structure>-stmnt_to - 1
          it_statements = it_statements
          it_tokens     = it_tokens ).

      IF lv_after_start = 'IF' AND lv_before_end = 'ENDIF'.
        lv_line = statement_row(
            iv_number     = <ls_structure>-stmnt_from
            it_statements = it_statements
            it_tokens     = it_tokens ).

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_level>-name
                p_line = lv_line
                p_kind = mv_errty
                p_test = c_my_name
                p_code = '001' ).
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = 'IF in IF'.                              "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT mv_errty = mv_errty TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'IF in IF, can easily be reduced'.           "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~display_documentation.

  documentation( c_my_name ).

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  DATA: lv_ok         TYPE abap_bool,
        lv_message    TYPE c LENGTH 72,
        lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = c_my_name
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD put_attributes.

  IMPORT mv_errty = mv_errty FROM DATA BUFFER p_attributes. "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.