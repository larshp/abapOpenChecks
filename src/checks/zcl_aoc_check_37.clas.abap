CLASS zcl_aoc_check_37 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS report
      IMPORTING
        !iv_program TYPE program
        !is_token   TYPE stokesx
        !iv_code    TYPE sci_errc .
ENDCLASS.



CLASS ZCL_AOC_CHECK_37 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_number    TYPE t100-msgnr,
          lv_class     TYPE t100-arbgb,
          lv_text      TYPE t100-text,
          lv_trash     TYPE string ##NEEDED,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>.

      CLEAR lv_statement.
      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_statement CP 'MESSAGE ''*'
          OR lv_statement CP 'MESSAGE text-+++ *'.
        report( iv_program = io_scan->get_include( <ls_statement>-level )
                is_token   = <ls_token>
                iv_code    = '001' ).
      ENDIF.

      IF lv_statement CP 'MESSAGE *->GET_TEXT( ) *'
          OR lv_statement CP 'MESSAGE *->IF_MESSAGE~GET_TEXT( ) *'.
        report( iv_program = io_scan->get_include( <ls_statement>-level )
                is_token     = <ls_token>
                iv_code      = '002' ).
      ENDIF.

      IF lv_statement CP 'MESSAGE ++++(*) *'.
        lv_number = lv_statement+9(3).
        lv_class = lv_statement+13.
        SPLIT lv_class AT ')' INTO lv_class lv_trash.
        SELECT SINGLE text FROM t100
          INTO lv_text
          WHERE sprsl = sy-langu
          AND arbgb = lv_class
          AND msgnr = lv_number.
        IF sy-subrc = 0 AND lv_text CO '&12345678 '.
          report( iv_program = io_scan->get_include( <ls_statement>-level )
                  is_token   = <ls_token>
                  iv_code    = '003' ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '037'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Define message texts in SE91'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Remove ''->get_text( )'''(m02) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Use of generic SE91 message'(m03) ).

  ENDMETHOD.


  METHOD report.

    inform( p_sub_obj_name = iv_program
            p_line         = is_token-row
            p_kind         = mv_errty
            p_test         = myname
            p_code         = iv_code ).

  ENDMETHOD.
ENDCLASS.
