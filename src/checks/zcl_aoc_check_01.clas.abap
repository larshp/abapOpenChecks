CLASS zcl_aoc_check_01 DEFINITION
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

    METHODS remove_comments
      IMPORTING
        !it_structures       TYPE ty_structures_tt
        !it_statements       TYPE sstmnt_tab
      RETURNING
        VALUE(rt_structures) TYPE ty_structures_tt.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_01 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include    TYPE program,
          lv_sub_else   TYPE abap_bool,
          lt_structures LIKE it_structures,
          lv_top_else   TYPE abap_bool.

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_statement> LIKE LINE OF it_statements,
                   <ls_sub>       LIKE LINE OF it_structures.


    lt_structures = remove_comments( it_structures = it_structures
                                     it_statements = it_statements ).

    LOOP AT lt_structures ASSIGNING <ls_structure>
        WHERE stmnt_type = scan_struc_stmnt_type-then
        OR stmnt_type = scan_struc_stmnt_type-else.
      IF <ls_structure>-struc_from <> <ls_structure>-struc_to.
        CONTINUE.
      ENDIF.
      READ TABLE lt_structures ASSIGNING <ls_sub> INDEX <ls_structure>-struc_to.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_sub>-stmnt_type <> scan_struc_stmnt_type-if.
        CONTINUE.
      ENDIF.

      lv_sub_else = abap_false.
      LOOP AT lt_structures TRANSPORTING NO FIELDS
          WHERE stmnt_from >= <ls_sub>-stmnt_from
          AND stmnt_to <= <ls_sub>-stmnt_to
          AND ( stmnt_type = scan_struc_stmnt_type-elseif
          OR stmnt_type = scan_struc_stmnt_type-else ).
        lv_sub_else = abap_true.
        EXIT.
      ENDLOOP.

      lv_top_else = abap_false.
      LOOP AT lt_structures TRANSPORTING NO FIELDS
          WHERE back = <ls_structure>-back
          AND ( stmnt_type = scan_struc_stmnt_type-elseif
          OR stmnt_type = scan_struc_stmnt_type-else ).
        lv_top_else = abap_true.
        EXIT.
      ENDLOOP.

      IF ( <ls_structure>-stmnt_type = scan_struc_stmnt_type-then
          AND <ls_sub>-stmnt_from = <ls_structure>-stmnt_from
          AND <ls_sub>-stmnt_to = <ls_structure>-stmnt_to
          AND lv_sub_else = abap_false
          AND lv_top_else = abap_false )
          OR ( <ls_structure>-stmnt_type = scan_struc_stmnt_type-else
          AND <ls_sub>-stmnt_from = <ls_structure>-stmnt_from + 1
          AND <ls_sub>-stmnt_to = <ls_structure>-stmnt_to ).
        READ TABLE it_statements ASSIGNING <ls_statement> INDEX <ls_structure>-stmnt_from.
        ASSERT sy-subrc = 0.

        READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
        ASSERT sy-subrc = 0.

        lv_include = get_include( p_level = <ls_statement>-level ).

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line = <ls_token>-row
                p_kind = mv_errty
                p_test = myname
                p_code = '001' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'IF in IF'.                            "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '001'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'IF in IF, can easily be reduced'.         "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD remove_comments.

    DATA: lv_only  TYPE abap_bool,
          lv_index TYPE i.

    FIELD-SYMBOLS: <ls_statement>  LIKE LINE OF it_statements,
                   <ls_structure1> LIKE LINE OF rt_structures,
                   <ls_structure2> LIKE LINE OF rt_structures.


    rt_structures = it_structures.

    LOOP AT rt_structures ASSIGNING <ls_structure1>.
      lv_index = sy-tabix.

      lv_only = abap_true.
      LOOP AT it_statements ASSIGNING <ls_statement>
          FROM <ls_structure1>-stmnt_from TO <ls_structure1>-stmnt_to.
        IF <ls_statement>-type <> scan_stmnt_type-comment.
          lv_only = abap_false.
        ENDIF.
      ENDLOOP.
      IF sy-subrc <> 0.
        lv_only = abap_false.
      ENDIF.

      IF lv_only = abap_true.
        LOOP AT rt_structures ASSIGNING <ls_structure2>.
          IF <ls_structure2>-struc_from > lv_index.
            <ls_structure2>-struc_from = <ls_structure2>-struc_from - 1.
          ENDIF.
          IF <ls_structure2>-struc_to > lv_index.
            <ls_structure2>-struc_to = <ls_structure2>-struc_to - 1.
          ENDIF.
          IF <ls_structure1>-stmnt_from = <ls_structure1>-stmnt_from
              AND <ls_structure1>-stmnt_from = <ls_structure1>-stmnt_to.
            <ls_structure2>-stmnt_from = <ls_structure2>-stmnt_from + 1.
          ENDIF.
        ENDLOOP.

        DELETE rt_structures INDEX lv_index.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
