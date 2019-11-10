CLASS zcl_aoc_check_52 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_52 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_code       TYPE string_table,
          lt_tokens     TYPE stokesx_tab,
          lv_next       TYPE i,
          lt_statements TYPE sstmnt_tab.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements,
                   <ls_token>     LIKE LINE OF lt_tokens,
                   <ls_next>      LIKE LINE OF lt_tokens,
                   <ls_level>     LIKE LINE OF io_scan->levels.


    LOOP AT io_scan->levels ASSIGNING <ls_level> WHERE type = io_scan->gc_level-program.

      lt_code = get_source( <ls_level> ).

* by default Pragmas are ignored in SAP standard code inspector
* so scan the source again,
      SCAN ABAP-SOURCE lt_code
        TOKENS INTO lt_tokens
        STATEMENTS INTO lt_statements
        WITH ANALYSIS
        WITH PRAGMAS '*'.

      LOOP AT lt_statements ASSIGNING <ls_statement> WHERE type <> io_scan->gc_statement-pragma.
        APPEND INITIAL LINE TO lt_tokens ASSIGNING <ls_token>.
        <ls_token>-row = <ls_statement>-trow.
        <ls_token>-col = <ls_statement>-tcol.
        <ls_token>-str = <ls_statement>-terminator.
      ENDLOOP.

      SORT lt_tokens BY row ASCENDING col ASCENDING.

      LOOP AT lt_tokens ASSIGNING <ls_token> WHERE type = io_scan->gc_token-pragma.
        lv_next = sy-tabix + 1.
        READ TABLE lt_tokens INDEX lv_next ASSIGNING <ls_next>.
        IF sy-subrc = 0 AND <ls_next>-type <> io_scan->gc_token-pragma
            AND <ls_next>-str <> '.'
            AND <ls_next>-str <> ','.
          inform( p_sub_obj_name = <ls_level>-name
                  p_line         = <ls_token>-row
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
    position = '052'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Place pragma at end of line before , or .'(m01) ).

  ENDMETHOD.
ENDCLASS.
