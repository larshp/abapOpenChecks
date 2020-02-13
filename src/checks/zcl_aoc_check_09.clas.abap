CLASS zcl_aoc_check_09 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
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

    FIELD-SYMBOLS: <ls_level> LIKE LINE OF io_scan->levels,
                   <lv_code>  LIKE LINE OF lt_code.


    LOOP AT io_scan->levels ASSIGNING <ls_level>.

      lt_code = get_source( <ls_level> ).

      LOOP AT lt_code ASSIGNING <lv_code>.
        lv_line = sy-tabix.
        IF <lv_code> CA cl_abap_char_utilities=>horizontal_tab.
          inform( p_sub_obj_name = <ls_level>-name
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

    version  = '001'.
    position = '009'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
      iv_code  = '001'
      iv_text  = 'Tab instead of spaces'(m01) ).

  ENDMETHOD.
ENDCLASS.
