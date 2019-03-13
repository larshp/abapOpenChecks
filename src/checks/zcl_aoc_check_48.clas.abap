CLASS zcl_aoc_check_48 DEFINITION
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

    METHODS check_table_body_access
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab .
    METHODS check_table_key
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab .
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

    check_table_key(
      it_tokens     = it_tokens
      it_statements = it_statements ).

    check_table_body_access(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

  ENDMETHOD.


  METHOD check_table_body_access.

    DATA: lv_level LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF it_tokens,
                   <ls_statement> LIKE LINE OF it_statements,
                   <ls_level>     LIKE LINE OF it_levels.


    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    LOOP AT it_levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.
      LOOP AT it_statements ASSIGNING <ls_statement> WHERE level = lv_level.
        LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to
            WHERE type <> scan_token_type-literal
            AND type <> scan_token_type-comment.

          IF <ls_token>-str CP '*+[]*'.
            inform( p_sub_obj_type = c_type_include
                    p_sub_obj_name = get_include( p_level = lv_level )
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

    DATA: lt_statements TYPE ty_statements,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    lt_statements = build_statements(
        it_tokens     = it_tokens
        it_statements = it_statements ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF ( <ls_statement>-str CP 'DATA* WITH DEFAULT KEY*'
          OR <ls_statement>-str CP 'TYPE* WITH DEFAULT KEY*' )
          AND support_empty_key( ) = abap_true
          AND <ls_statement>-include(8) <> '/1BCWDY/'.
        lv_code = '001'.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_position     = <ls_statement>-index
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA: ls_message LIKE LINE OF scimessages.

    super->constructor( ).

    version        = '002'.
    position       = '048'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    ls_message-test = myname.
    ls_message-code = '001'.
    ls_message-kind = c_error.
    ls_message-pcom = '"#EC CI_DEFAULT_KEY'.
    INSERT ls_message INTO TABLE scimessages.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'DEFAULT KEY, add table key or EMPTY KEY'. "#EC NOTEXT
      WHEN '002'.
        p_text = 'Access table body is obsolete, no headers'. "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

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
