CLASS zcl_aoc_check_27 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_stmnt,
        statement TYPE string,
        row       TYPE token_row,
        row_to    TYPE token_row,
        level     TYPE stmnt_levl,
        col       TYPE token_col,
        col_to    TYPE token_col,
      END OF ty_stmnt .
    TYPES:
      ty_stmnt_tt TYPE STANDARD TABLE OF ty_stmnt WITH DEFAULT KEY .

    DATA mt_statements TYPE ty_stmnt_tt .

    METHODS is_local
      IMPORTING
        !it_statements TYPE ty_stmnt_tt
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    METHODS analyze
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan .
    METHODS build
      IMPORTING
        !is_structure  TYPE sstruc
        !it_statements TYPE sstmnt_tab
        !it_tokens     TYPE stokesx_tab .
  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_AOC_CHECK_27 IMPLEMENTATION.


  METHOD analyze.

    DATA: lv_index     TYPE i,
          lv_include   TYPE program,
          lv_code      TYPE sci_errc,
          ls_statement LIKE LINE OF mt_statements.


    WHILE lines( mt_statements ) > 0.

      lv_index = lines( mt_statements ).
      READ TABLE mt_statements INDEX lv_index INTO ls_statement.
      ASSERT sy-subrc = 0.

      IF ls_statement-statement = 'ENDIF'
          OR ls_statement-statement = 'ENDTRY'
          OR ls_statement-statement = 'ENDFORM'
          OR ls_statement-statement = 'ENDMETHOD'.
        DELETE mt_statements INDEX lv_index.
      ELSEIF ls_statement-statement = 'RETURN'.
        lv_code = '001'.
      ELSEIF ( ls_statement-statement CP 'CLEAR *'
          OR ls_statement-statement CP 'FREE *' )
          AND is_local( mt_statements ) = abap_true
          AND NOT ls_statement-statement CP 'CLEAR <*'.
        lv_code = '002'.
      ELSEIF ls_statement-statement = 'EXIT'
          OR ls_statement-statement CP 'CHECK *'.
        lv_code = '003'.
      ELSE.
        RETURN.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        lv_include = io_scan->get_include( ls_statement-level ).

        inform( p_sub_obj_name = lv_include
                p_line         = ls_statement-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
        RETURN.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD build.

    DATA: ls_statement LIKE LINE OF mt_statements.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    CLEAR mt_statements.

    LOOP AT it_statements ASSIGNING <ls_statement>
        FROM is_structure-stmnt_from TO is_structure-stmnt_to
        WHERE type <> zcl_aoc_scan=>gc_statement-comment
        AND type <> zcl_aoc_scan=>gc_statement-comment_in_stmnt
        AND type <> zcl_aoc_scan=>gc_statement-macro_call
        AND trow <> 0. " skip macro calls

      CLEAR ls_statement.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF ls_statement-statement IS INITIAL.
          ls_statement-statement = <ls_token>-str.
          ls_statement-row = <ls_token>-row.
          ls_statement-col = <ls_token>-col.
          ls_statement-row_to = <ls_token>-row.
          ls_statement-col_to = <ls_token>-col.
          ls_statement-level = <ls_statement>-level.
        ELSE.
          CONCATENATE ls_statement-statement <ls_token>-str
            INTO ls_statement-statement SEPARATED BY space.
          ls_statement-row_to = <ls_token>-row.
          ls_statement-col_to = <ls_token>-col.
        ENDIF.
      ENDLOOP.
      APPEND ls_statement TO mt_statements.
    ENDLOOP.

    IF lines( mt_statements ) = 3.
      CLEAR mt_statements.
    ENDIF.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF io_scan->structures.


    LOOP AT io_scan->structures ASSIGNING <ls_structure>
        WHERE stmnt_type = zcl_aoc_scan=>gc_structure_statement-module
        OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-function
        OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-form
        OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-method.

      build( is_structure  = <ls_structure>
             it_statements = io_scan->statements
             it_tokens     = io_scan->tokens ).

      analyze( io_scan ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '027'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Last statement is RETURN'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Last statement is CLEAR or FREE'(m02) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Last statement is CHECK or EXIT'(m03) ).

  ENDMETHOD.


  METHOD is_local.

    DATA: ls_statement LIKE LINE OF it_statements,
          lv_index     TYPE i,
          lv_trash     TYPE string ##NEEDED,
          lv_var       TYPE string.


    lv_index = lines( it_statements ) - 1.

    READ TABLE it_statements INDEX lv_index INTO ls_statement.
    ASSERT sy-subrc = 0.
    IF ls_statement-statement CP |DATA(*) *|.
      SPLIT ls_statement-statement AT '(' INTO lv_trash lv_var.
      SPLIT lv_var AT ')' INTO lv_var lv_trash.
    ELSE.
      SPLIT ls_statement-statement AT space INTO lv_trash lv_var.
    ENDIF.

    WHILE lv_index > 0.

      READ TABLE it_statements INDEX lv_index INTO ls_statement.
      ASSERT sy-subrc = 0.

      IF ls_statement-statement CP 'FORM *' OR ls_statement-statement CP 'METHOD *'.
* not a local
        RETURN.
      ENDIF.

      IF ls_statement-statement CP |DATA { lv_var } *|
          OR ls_statement-statement = |DATA { lv_var }|
          OR ls_statement-statement CP |DATA({ lv_var }) *|.
        rv_bool = abap_true.
        RETURN.
      ENDIF.

      lv_index = lv_index - 1.

    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
