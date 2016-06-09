CLASS zcl_aoc_check_09 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_09 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_code TYPE string_table,
          lv_line TYPE token_row.

    FIELD-SYMBOLS: <ls_level> LIKE LINE OF it_levels,
                   <lv_code>  LIKE LINE OF lt_code.


    LOOP AT it_levels ASSIGNING <ls_level>.

      lt_code = get_source( <ls_level> ).

      LOOP AT lt_code ASSIGNING <lv_code>.
        lv_line = sy-tabix.
        IF <lv_code> CA cl_abap_char_utilities=>horizontal_tab.
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = <ls_level>-name
                  p_line         = lv_line
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Tab instead of spaces'.               "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '009'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Tab instead of spaces'.                   "#EC NOTEXT
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.