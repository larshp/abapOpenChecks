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
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_01 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include  TYPE program,
          lv_sub_else TYPE abap_bool,
          lv_top_else TYPE abap_bool.

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_statement> LIKE LINE OF it_statements,
                   <ls_sub>       LIKE LINE OF it_structures.


    LOOP AT it_structures ASSIGNING <ls_structure>
        WHERE stmnt_type = scan_struc_stmnt_type-then
        OR stmnt_type = scan_struc_stmnt_type-else.
      IF <ls_structure>-struc_from <> <ls_structure>-struc_to.
        CONTINUE.
      ENDIF.
      READ TABLE it_structures ASSIGNING <ls_sub> INDEX <ls_structure>-struc_to.
      ASSERT sy-subrc = 0.

      IF <ls_sub>-stmnt_type <> scan_struc_stmnt_type-if.
        CONTINUE.
      ENDIF.

      lv_sub_else = abap_false.
      LOOP AT it_structures TRANSPORTING NO FIELDS
          WHERE stmnt_from >= <ls_sub>-stmnt_from
          AND stmnt_to <= <ls_sub>-stmnt_to
          AND ( stmnt_type = scan_struc_stmnt_type-elseif
          OR stmnt_type = scan_struc_stmnt_type-else ).
        lv_sub_else = abap_true.
        EXIT.
      ENDLOOP.

      lv_top_else = abap_false.
      LOOP AT it_structures TRANSPORTING NO FIELDS
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

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'IF in IF, can easily be reduced'.         "#EC NOTEXT
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.