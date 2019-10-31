CLASS zcl_aoc_check_23 DEFINITION
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



CLASS ZCL_AOC_CHECK_23 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_code       TYPE sci_errc,
          lt_statements LIKE io_scan->statements,
          lv_include    TYPE program.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    lt_statements = io_scan->statements.

    LOOP AT lt_statements ASSIGNING <ls_statement>
        WHERE coloncol <> 0
        AND type <> io_scan->gc_statement-pragma.

      CLEAR lv_code.

      READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_token>-str <> 'TYPES'
          AND <ls_token>-str <> 'DATA'
          AND <ls_token>-str <> 'CLASS-DATA'
          AND <ls_token>-str <> 'STATICS'
          AND <ls_token>-str <> 'WRITE'
          AND <ls_token>-str <> 'MOVE'  " anyhow obsolete
          AND <ls_token>-str <> 'RANGES' " anyhow obsolete
          AND <ls_token>-str <> 'METHODS'
          AND <ls_token>-str <> 'CLEAR'
          AND <ls_token>-str <> 'PERFORM'
          AND <ls_token>-str <> 'REFRESH'
          AND <ls_token>-str <> 'UNASSIGN'
          AND <ls_token>-str <> 'FREE'
          AND <ls_token>-str <> 'CONSTANTS'
          AND <ls_token>-str <> 'TABLES'
          AND <ls_token>-str <> 'PARAMETERS'
          AND <ls_token>-str <> 'PARAMETER'
          AND <ls_token>-str <> 'INTERFACES'
          AND <ls_token>-str <> 'SELECT-OPTIONS'
          AND <ls_token>-str <> 'SELECTION-SCREEN'
          AND <ls_token>-str <> 'ALIASES'
          AND <ls_token>-str <> 'INCLUDE'
          AND <ls_token>-str <> 'TYPE-POOLS'
          AND <ls_token>-str <> 'CLASS-METHODS'
          AND <ls_token>-str <> 'FIELD-SYMBOLS'.
        lv_code = '001'.
      ENDIF.

      IF lv_code IS INITIAL
            AND strlen( <ls_token>-str ) + <ls_token>-col <> <ls_statement>-coloncol
          AND <ls_token>-str <> 'PERFORM'.
        lv_code = '002'.
      ENDIF.

      IF lv_code IS INITIAL.
        LOOP AT io_scan->tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to
            WHERE row = <ls_statement>-colonrow.
          IF <ls_token>-col = <ls_statement>-coloncol + 1.
            lv_code = '003'.
            EXIT. " current loop
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        lv_include = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_line = <ls_token>-row
                p_kind = mv_errty
                p_test = myname
                p_code = lv_code ).

* do not report multiple errors for the same chanined statement
        DELETE lt_statements
          WHERE level = <ls_statement>-level
          AND colonrow = <ls_statement>-colonrow
          AND coloncol = <ls_statement>-coloncol.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '023'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Use chained statements mainly for declarations'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Space before colon'(m02) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Missing space after colon'(m03) ).

  ENDMETHOD.
ENDCLASS.
