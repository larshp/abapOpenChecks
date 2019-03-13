CLASS zcl_aoc_check_81 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_AOC_CHECK_81 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_level TYPE i,
          lv_first TYPE abap_bool,
          lv_code  TYPE sci_errc,
          ls_next  LIKE LINE OF it_tokens.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_statement> LIKE LINE OF it_statements.


    LOOP AT it_levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.

      LOOP AT it_statements ASSIGNING <ls_statement> WHERE level = lv_level
          AND type <> scan_stmnt_type-empty
          AND type <> scan_stmnt_type-macro_definition
          AND type <> scan_stmnt_type-comment
          AND type <> scan_stmnt_type-native_sql
          AND type <> scan_stmnt_type-pragma
          AND type <> scan_stmnt_type-comment_in_stmnt.

        lv_first = abap_true.
        LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to - 1.
          IF lv_first = abap_true
              AND ( <ls_token>-str = 'SELECT'
              OR <ls_token>-str = 'UPDATE'
              OR <ls_token>-str = 'INSERT' ).
            EXIT.
          ENDIF.
          lv_first = abap_false.

          READ TABLE it_tokens INTO ls_next INDEX sy-tabix + 1.
          ASSERT sy-subrc = 0.

          CLEAR lv_code.
          IF <ls_token>-str(1) = '''' AND <ls_token>-col + <ls_token>-len1 = ls_next-col.
            lv_code = '001'.
          ELSEIF ls_next-str(1) = '''' AND <ls_token>-col + <ls_token>-len1 = ls_next-col.
            lv_code = '002'.
          ENDIF.

          IF NOT lv_code IS INITIAL.
            inform( p_sub_obj_type = c_type_include
                    p_sub_obj_name = <ls_level>-name
                    p_line         = <ls_token>-row
                    p_column       = <ls_token>-col + <ls_token>-len1
                    p_kind         = mv_errty
                    p_test         = myname
                    p_code         = lv_code ).
          ENDIF.

        ENDLOOP.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    category = 'ZCL_AOC_CATEGORY'.
    version  = '001'.
    position = '081'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Missing space after '''.                  "#EC NOTEXT
      WHEN '002'.
        p_text = 'Missing space before '''.                 "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
