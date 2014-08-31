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
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_01
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_01
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_01'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_01 IMPLEMENTATION.


METHOD CHECK.

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

      lv_after_start = zcl_aoc_super=>statement_keyword(
          iv_number     = <ls_structure>-stmnt_from + 1
          it_statements = it_statements
          it_tokens     = it_tokens ).

      lv_before_end = zcl_aoc_super=>statement_keyword(
          iv_number     = <ls_structure>-stmnt_to - 1
          it_statements = it_statements
          it_tokens     = it_tokens ).

      IF lv_after_start = 'IF' AND lv_before_end = 'ENDIF'.
        lv_line = zcl_aoc_super=>statement_row(
            iv_number     = <ls_structure>-stmnt_from
            it_statements = it_statements
            it_tokens     = it_tokens ).

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_level>-name
                p_line = lv_line
                p_kind = c_error
                p_test = c_my_name
                p_code = '001' ).
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD CONSTRUCTOR .

  super->constructor( ).

  description    = 'IF in IF'.                              "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

* todo, attributes, error/warning/info

*  HAS_ATTRIBUTES = 'X'.                        "optional
*  ATTRIBUTES_OK  = 'X' or ' '.                 "optional

ENDMETHOD.                    "CONSTRUCTOR


METHOD GET_MESSAGE_TEXT.

  CASE p_code.
    WHEN '001'.
      p_text = 'IF in IF, can easily be reduced'.           "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 2.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~display_documentation.

  documentation( c_my_name ).

ENDMETHOD.
ENDCLASS.