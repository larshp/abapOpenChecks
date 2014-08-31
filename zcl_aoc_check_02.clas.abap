class ZCL_AOC_CHECK_02 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_02
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_02
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_02
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_02'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_02 IMPLEMENTATION.


METHOD check.

  DATA: lv_keyword TYPE string,
        lv_line    TYPE token_row,
        lv_index   LIKE sy-tabix.

  FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_levels ASSIGNING <ls_level>.

* only run for lowest level
    READ TABLE it_levels WITH KEY level = sy-tabix TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE. " current loop
    ENDIF.

    LOOP AT it_statements ASSIGNING <ls_statement> FROM <ls_level>-from TO <ls_level>-to.
      lv_index = sy-tabix.

      lv_keyword = statement_keyword(
          iv_number     = lv_index
          it_statements = it_statements
          it_tokens     = it_tokens ).

      IF lv_keyword <> 'EXIT'.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT it_structures TRANSPORTING NO FIELDS
          WHERE ( stmnt_type = scan_struc_stmnt_type-loop
          OR stmnt_type = scan_struc_stmnt_type-while
          OR stmnt_type = scan_struc_stmnt_type-do )
          AND stmnt_from <= lv_index
          AND stmnt_to >= lv_index.
      ENDLOOP.
      IF sy-subrc <> 0.
        lv_line = statement_row(
          iv_number     = lv_index
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


METHOD constructor .

  super->constructor( ).

  description    = 'EXIT outside of LOOP'.                  "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

* todo, attributes, error/warning/info

*  HAS_ATTRIBUTES = 'X'.                        "optional
*  ATTRIBUTES_OK  = 'X' or ' '.                 "optional

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'EXIT outside loop, use RETURN instead'.     "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 2.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~display_documentation.

  documentation( c_my_name ).

ENDMETHOD.
ENDCLASS.