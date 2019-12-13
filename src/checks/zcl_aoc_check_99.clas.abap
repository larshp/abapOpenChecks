CLASS zcl_aoc_check_99 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_stmnt,
        statement TYPE string,
        row       TYPE token_row,
        row_to    TYPE token_row,
        level     TYPE stmnt_levl,
        col       TYPE token_col,
        col_to    TYPE token_col,
      END OF ty_stmnt.
    TYPES:
      ty_stmnt_tt TYPE STANDARD TABLE OF ty_stmnt WITH DEFAULT KEY.

    DATA mt_statements TYPE ty_stmnt_tt.

    DATA mv_min_branches TYPE i.

    METHODS analyze
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan.
    METHODS build
      IMPORTING
        !is_structure  TYPE sstruc
        !it_statements TYPE sstmnt_tab
        !it_tokens     TYPE stokesx_tab.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_99 IMPLEMENTATION.


  METHOD analyze.

    DATA: lv_include_name                TYPE program,
          lv_no_of_whens                 TYPE i,
          lv_start_row_of_case_statement TYPE token_row,
          lv_is_in_when_clause           TYPE abap_bool VALUE abap_false,
          lv_code_rows_in_when_clause    TYPE i,
          ls_statement                   LIKE LINE OF mt_statements.

    LOOP AT mt_statements INTO ls_statement.
      IF ls_statement-statement CP 'CASE*'.
        lv_no_of_whens = 0.
        lv_code_rows_in_when_clause = 0.
        lv_start_row_of_case_statement = ls_statement-row.
      ELSEIF ls_statement-statement CP 'WHEN*'.
        " empty WHEN branches don't count
        IF lv_is_in_when_clause = abap_true AND lv_code_rows_in_when_clause > 0.
          lv_no_of_whens = lv_no_of_whens + 1.
        ENDIF.

        lv_is_in_when_clause = abap_true.
        lv_code_rows_in_when_clause = 0.
      ELSEIF ls_statement-statement = 'ENDCASE'.
        " empty WHEN branches don't count
        IF lv_is_in_when_clause = abap_true AND lv_code_rows_in_when_clause > 0.
          lv_no_of_whens = lv_no_of_whens + 1.
        ENDIF.

        lv_is_in_when_clause = abap_false.

        IF lv_no_of_whens < mv_min_branches.
          lv_include_name = io_scan->get_include( ls_statement-level ).

          inform( p_sub_obj_name = lv_include_name
                  p_line         = lv_start_row_of_case_statement
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
          RETURN.
        ENDIF.
      ELSEIF lv_is_in_when_clause = abap_true.
        lv_code_rows_in_when_clause = lv_code_rows_in_when_clause + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build.

    DATA ls_statement LIKE LINE OF mt_statements.

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
    position = '099'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_min_branches = 2.

    insert_scimessage(
        iv_code = '001'
        iv_text = TEXT-m01 ).

  ENDMETHOD.


  METHOD get_attributes.
    EXPORT
      mv_errty = mv_errty
      mv_min_branches = mv_min_branches
      TO DATA BUFFER p_attributes.
  ENDMETHOD.


  METHOD if_ci_test~query_attributes.
    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_min_branches 'Minimum no. of branches' ''. "#EC NOTEXT

    zzaoc_popup.
  ENDMETHOD.


  METHOD put_attributes.
    IMPORT
      mv_errty = mv_errty
      mv_min_branches = mv_min_branches
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.
  ENDMETHOD.
ENDCLASS.
