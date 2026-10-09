CLASS zcl_aoc_check_02 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.

  PROTECTED SECTION.
    DATA mv_check TYPE flag.
    DATA mv_exit TYPE flag.

    METHODS _is_check_allow
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan
        !iv_statement_index TYPE i
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_02 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_keyword TYPE string,
          lv_line    TYPE token_row,
          lv_include TYPE program,
          lv_error   TYPE sci_errc,
          lv_index   LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>.
      lv_index = sy-tabix.

      lv_keyword = io_scan->statement_keyword( lv_index ).

      IF lv_keyword = 'EXIT' AND mv_exit = abap_true.
        lv_error = '001'.
      ELSEIF lv_keyword = 'CHECK' AND mv_check = abap_true.
        lv_error = '002'.
      ELSE.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT io_scan->structures TRANSPORTING NO FIELDS
          WHERE ( stmnt_type = zcl_aoc_scan=>gc_structure_statement-loop
          OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-while
          OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-do
          OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-select )
          AND stmnt_from <= lv_index
          AND stmnt_to >= lv_index.
        EXIT. " current loop
      ENDLOOP.
      IF sy-subrc <> 0.
        IF lv_error = '002' AND _is_check_allow(
                                  io_scan            = io_scan
                                  iv_statement_index = lv_index
                                ) IS NOT INITIAL.
          CONTINUE.
        ENDIF.
        lv_line = io_scan->statement_row( lv_index ).

        lv_include = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_position     = lv_index
                p_line         = lv_line
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_error ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '002'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    mv_check = abap_true.
    mv_exit  = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'EXIT outside loop, use RETURN instead'(m01) ).
    insert_scimessage(
        iv_code = '002'
        iv_text = 'CHECK outside of loop'(m02) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_check = mv_check
      mv_exit = mv_exit
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_check 'CHECK' 'C'.                    "#EC NOTEXT
    zzaoc_fill_att mv_exit 'EXIT' 'C'.                      "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_check = mv_check
      mv_exit = mv_exit
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD _is_check_allow.
************************************************************************
* Copyright (c) 2023 by Alexandr Zhuravlev
* MIT License
* https://github.com/alezhu/abapOpenChecks/

*Rule
*
*Only use  RETURN to exit procedures

*Exception !!!!!!
*
*An exception to the rule to only use RETURN to exit procedures are
*CHECK statements that are located at the beginning of a procedure and
*that check the prerequisites for the execution of the procedure there.
*
*Using the CHECK statement in such a way does not impair the legibility
*and is thus allowed.
************************************************************************

    DATA(lp_s_check) = REF #( io_scan->statements[ iv_statement_index ] ).
    DATA(lv_struct_index) = lp_s_check->struc.
    "Search Routine parent
    WHILE lv_struct_index > 0.
      DATA(lp_s_struct) = REF #( io_scan->structures[ lv_struct_index ] ).
      IF lp_s_struct->type = scan_struc_type-routine.
        EXIT.
      ENDIF.
      lv_struct_index = lp_s_struct->back.
    ENDWHILE.
    IF lp_s_struct IS NOT BOUND.
      RETURN.
    ENDIF.

    "Check all statements from routine start to current CHECK
    "and skip available statements
    DATA(lv_from) = SWITCH i( lp_s_struct->type
      WHEN scan_struc_type-routine THEN lp_s_struct->stmnt_from + 1 " +1 skips METHOD/FORM/FUNCTION etc
      ELSE lp_s_struct->stmnt_from ).
    LOOP AT io_scan->statements REFERENCE INTO DATA(lp_s_statement)
        FROM lv_from
        TO iv_statement_index - 1. " -1 skips current CHECK

      CASE lp_s_statement->type.
        WHEN scan_stmnt_type-include
            OR scan_stmnt_type-include_miss
            OR scan_stmnt_type-type_pools
            OR scan_stmnt_type-type_pools_miss
            OR scan_stmnt_type-comment
            OR scan_stmnt_type-comment_in_stmnt
            OR scan_stmnt_type-pragma
            OR scan_stmnt_type-abap_doc
            OR scan_stmnt_type-macro_definition
            OR scan_stmnt_type-empty.
          "Skip allow
          CONTINUE.
        WHEN scan_stmnt_type-standard
            OR scan_stmnt_type-unknown.
          DATA(lv_keyword) = io_scan->statement_keyword( sy-tabix ).
          CASE lv_keyword.
            WHEN 'TYPES'
                OR 'DATA'
                OR 'STATICS'
                OR 'TABLES'
                OR 'CONSTANTS'
                OR 'FIELD-SYMBOLS'.
            WHEN 'CLEAR'
                OR 'FREE'
                OR 'REFRESH'.
            WHEN 'ASSERT'
                OR 'CHECK'
                OR 'LEAVE'
                OR 'RAISE'
                OR 'RETURN'
                OR 'EXIT'.
            WHEN 'BREAK-POINT'
                OR 'LOG-POINT'.
            WHEN 'DESCRIBE'
                OR 'GET'
                OR 'INCLUDE'
                OR 'ASSIGN'.
            WHEN 'IF'
                OR 'ENDIF'.
            WHEN 'DEFINE'
                OR 'END-OF-DEFINITION'.
            WHEN OTHERS.
              "CHECK not allow after others
              RETURN.
          ENDCASE.
        WHEN OTHERS.
          "CHECK not allow after such statement type
          RETURN.
      ENDCASE.
    ENDLOOP.

    rv_result = abap_true.
  ENDMETHOD.
ENDCLASS.