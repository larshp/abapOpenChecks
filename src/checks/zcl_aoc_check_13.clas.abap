CLASS zcl_aoc_check_13 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
  PROTECTED SECTION.
    DATA mv_lines TYPE linestotal.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_13 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_code TYPE string_table,
          lv_seq  TYPE i,
          lv_line TYPE token_row.

    FIELD-SYMBOLS: <ls_level> LIKE LINE OF io_scan->levels,
                   <lv_code>  LIKE LINE OF lt_code.


    LOOP AT io_scan->levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
      lt_code = get_source( <ls_level> ).

      LOOP AT lt_code ASSIGNING <lv_code>.
        lv_line = sy-tabix.

        IF strlen( <lv_code> ) = 0.
          lv_seq = lv_seq + 1.
        ELSE.
          lv_seq = 0.
        ENDIF.

        IF lv_seq >= mv_lines.
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = <ls_level>-name
                  p_line         = lv_line
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
* only report one error per include
          EXIT. " current loop
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '013'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.
    mv_lines = 4.

    enable_rfc( ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT mv_errty = mv_errty mv_maxlength = mv_lines TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Sequential blank lines'.                  "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_lines 'Lines' ''.                     "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_maxlength = mv_lines
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
