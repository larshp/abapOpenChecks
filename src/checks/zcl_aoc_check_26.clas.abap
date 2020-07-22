CLASS zcl_aoc_check_26 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mt_tables TYPE scit_tabl .

    METHODS find_user
      IMPORTING
        !iv_tabname       TYPE clike
      RETURNING
        VALUE(rv_as4user) TYPE dd02l-as4user .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_26 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_keyword1  TYPE string,
          lv_keyword2  TYPE string,
          lt_code      TYPE string_table,
          lv_as4user   TYPE dd02l-as4user,
          ls_result    TYPE zcl_aoc_parser=>ty_result,
          lv_include   TYPE program,
          lv_statement TYPE string,
          lv_index     TYPE sy-tabix.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_rt>        LIKE LINE OF ls_result-tokens,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement> WHERE type = io_scan->gc_statement-standard.
      lv_index = sy-tabix.

      CLEAR lv_keyword1.
      CLEAR lv_keyword2.
      CLEAR lv_statement.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from
          TO <ls_statement>-to
          WHERE type <> io_scan->gc_token-comment.

        IF lv_keyword1 IS INITIAL.
          lv_keyword1 = <ls_token>-str.
        ELSEIF lv_keyword2 IS INITIAL.
          lv_keyword2 = <ls_token>-str.
        ENDIF.

        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_keyword1 <> 'UPDATE'
          AND lv_keyword1 <> 'MODIFY'
          AND lv_keyword1 <> 'DELETE'
          AND lv_keyword1 <> 'INSERT'.
        CONTINUE. " current loop
      ENDIF.

      IF lv_keyword2 = 'SCREEN'
          OR lv_keyword2 = 'CURRENT'.
        CONTINUE. " current loop
      ENDIF.

      CLEAR lt_code.
      APPEND lv_statement TO lt_code.
      ls_result = zcl_aoc_parser=>run( it_code           = lt_code
                                       iv_rule           = lv_keyword1
                                       iv_allow_obsolete = abap_false ).

      IF ls_result-match = abap_false.
        CONTINUE.
      ENDIF.

* the parser sometimes mixes up the itab and dbtab updates, so look for the first role
      READ TABLE ls_result-tokens ASSIGNING <ls_rt>
        WITH KEY type = zcl_aoc_parser=>c_type-role.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lv_as4user = find_user( <ls_rt>-code ).
      IF ( lv_as4user = 'SAP' OR lv_as4user = 'DDIC' )
          AND <ls_rt>-code IN mt_tables.
        lv_include = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_position     = lv_index
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001'
                p_param_1      = <ls_rt>-code ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '026'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    CLEAR mt_tables.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'No direct changes to standard tables, &1'(m01)
        iv_pcom = '"#EC AOC_STD_TABLE' ).

  ENDMETHOD.


  METHOD find_user.

    DATA: lv_tabname     TYPE dd02l-tabname,
          ls_dd02v       TYPE dd02v,
          lv_destination TYPE rfcdest.


    lv_tabname = iv_tabname.

    lv_destination = get_destination( ).

    CALL FUNCTION 'DD_TABL_GET'
      DESTINATION lv_destination
      EXPORTING
        tabl_name      = lv_tabname
        add_typeinfo   = abap_false
      IMPORTING
        dd02v_wa_a     = ls_dd02v
      EXCEPTIONS
        access_failure = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_dd02v-tabclass = 'TRANSP'.
      rv_as4user = ls_dd02v-as4user.
    ENDIF.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mt_tables = mt_tables
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mt_tables 'Tables' 'S'.                  "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mt_tables = mt_tables
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
