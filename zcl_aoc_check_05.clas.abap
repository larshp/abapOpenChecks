class ZCL_AOC_CHECK_05 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_05
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.

  class-data GO_CONV type ref to CL_ABAP_CONV_OUT_CE .
*"* protected components of class ZCL_AOC_CHECK_05
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_05
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_05'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_05 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_code    TYPE string_table,
        lv_line    TYPE token_row,
        lv_bad     TYPE c.

  FIELD-SYMBOLS: <ls_level> LIKE LINE OF it_levels,
                 <lv_code>  LIKE LINE OF lt_code.


  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lt_code = get_source( <ls_level> ).
    LOOP AT lt_code ASSIGNING <lv_code>.
      lv_line = sy-tabix.

      cl_abap_file_utilities=>check_string_7bit_ascii(
        EXPORTING
          string    = <lv_code>
        IMPORTING
          bad_chars = lv_bad ).
      IF lv_bad <> space.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_level>-name
                p_line         = lv_line
                p_kind         = mv_errty
                p_test         = c_my_name
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = '7 bit ASCII'.                           "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Conatins non 7 bit ASCII'.                  "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.