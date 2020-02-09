CLASS zcl_aoc_check_48 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.

    METHODS check_table_body_access
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan .
    METHODS check_table_key
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan .
    METHODS support_empty_key
      RETURNING
        VALUE(rv_supported) TYPE abap_bool .
  PRIVATE SECTION.

    CLASS-DATA gv_checked TYPE abap_bool .
    CLASS-DATA gv_supported TYPE abap_bool .
ENDCLASS.



CLASS ZCL_AOC_CHECK_48 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    check_table_key( io_scan ).

    check_table_body_access( io_scan ).

  ENDMETHOD.


  METHOD check_table_body_access.

    DATA: lv_level LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_level>     LIKE LINE OF io_scan->levels.


    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    LOOP AT io_scan->levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.
      LOOP AT io_scan->statements ASSIGNING <ls_statement> WHERE level = lv_level.
        LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to
            WHERE type <> io_scan->gc_token-literal
            AND type <> io_scan->gc_token-comment.

          IF <ls_token>-str CP '*+[]*'.
            inform( p_sub_obj_name = io_scan->get_include( lv_level )
                    p_line         = <ls_token>-row
                    p_kind         = mv_errty
                    p_test         = myname
                    p_code         = '002' ).
          ENDIF.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_table_key.

    DATA: lt_statements TYPE zcl_aoc_scan=>ty_statements,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    lt_statements = io_scan->build_statements( ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF ( <ls_statement>-str CP 'DATA* WITH DEFAULT KEY*'
          OR <ls_statement>-str CP 'TYPE* WITH DEFAULT KEY*' )
          AND support_empty_key( ) = abap_true
          AND <ls_statement>-include(8) <> '/1BCWDY/'.
        lv_code = '001'.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_position     = <ls_statement>-index
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '002'.
    position = '048'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'DEFAULT KEY, add table key or EMPTY KEY'(m01)
        iv_pcom = '"#EC CI_DEFAULT_KEY' ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Access table body is obsolete, no headers'(m02) ).

  ENDMETHOD.


  METHOD support_empty_key.

    DATA: lt_itab  TYPE STANDARD TABLE OF string,
          lv_mess  TYPE string,
          lv_lin   TYPE i,
          ls_trdir TYPE trdir,
          lv_code  TYPE string,
          lv_wrd   TYPE string.


    IF gv_checked = abap_true.
      rv_supported = gv_supported.
      RETURN.
    ENDIF.

    lv_code = 'REPORT zfoobar.' ##NO_TEXT.
    APPEND lv_code TO lt_itab.
    lv_code = 'TYPES: ty_table TYPE STANDARD TABLE OF usr02 WITH EMPTY KEY.' ##NO_TEXT.
    APPEND lv_code TO lt_itab.

    ls_trdir-uccheck = abap_true.

    SYNTAX-CHECK FOR lt_itab
      MESSAGE lv_mess
      LINE lv_lin
      WORD lv_wrd
      DIRECTORY ENTRY ls_trdir.
    IF sy-subrc = 0.
      rv_supported = abap_true.
    ELSE.
      rv_supported = abap_false.
    ENDIF.

    gv_supported = rv_supported.
    gv_checked = abap_true.

  ENDMETHOD.
ENDCLASS.
