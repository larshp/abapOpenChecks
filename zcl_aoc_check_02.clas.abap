CLASS zcl_aoc_check_02 DEFINITION
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



CLASS ZCL_AOC_CHECK_02 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_keyword TYPE string,
          lv_line    TYPE token_row,
          lv_include TYPE program,
          lv_error   TYPE sci_errc,
          lv_index   LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements.


    LOOP AT it_statements ASSIGNING <ls_statement>.
      lv_index = sy-tabix.

      lv_keyword = statement_keyword(
          iv_number     = lv_index
          it_statements = it_statements
          it_tokens     = it_tokens ).

      CASE lv_keyword.
        WHEN 'EXIT'.
          lv_error = '001'.
        WHEN 'CHECK'.
          lv_error = '002'.
        WHEN OTHERS.
          CONTINUE. " current loop
      ENDCASE.

      LOOP AT it_structures TRANSPORTING NO FIELDS
          WHERE ( stmnt_type = scan_struc_stmnt_type-loop
          OR stmnt_type = scan_struc_stmnt_type-while
          OR stmnt_type = scan_struc_stmnt_type-do
          OR stmnt_type = scan_struc_stmnt_type-select )
          AND stmnt_from <= lv_index
          AND stmnt_to >= lv_index.
        EXIT. " current loop
      ENDLOOP.
      IF sy-subrc <> 0.
        lv_line = statement_row(
          iv_number     = lv_index
          it_statements = it_statements
          it_tokens     = it_tokens ).

        lv_include = get_include( p_level = <ls_statement>-level ).

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line = lv_line
                p_kind = mv_errty
                p_test = myname
                p_code = lv_error ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'EXIT or CHECK outside of LOOP'.       "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '002'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'EXIT outside loop, use RETURN instead'.   "#EC NOTEXT
      WHEN '002'.
        p_text = 'CHECK outside of loop'.                   "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.