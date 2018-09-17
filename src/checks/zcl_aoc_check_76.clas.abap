CLASS zcl_aoc_check_76 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
         REDEFINITION .
    METHODS get_attributes
         REDEFINITION .
    METHODS get_message_text
         REDEFINITION .
    METHODS if_ci_test~query_attributes
         REDEFINITION .
    METHODS put_attributes
         REDEFINITION .
  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS get_tokens_for_statement
      IMPORTING
        is_statement     TYPE sstmnt
        it_tokens        TYPE stokesx_tab
      RETURNING
        VALUE(rt_tokens) TYPE stokesx_tab.
    METHODS is_text_table
      IMPORTING
        iv_tablename           TYPE tabname
      RETURNING
        VALUE(rf_is_texttable) TYPE abap_bool.
ENDCLASS.



CLASS zcl_aoc_check_76 IMPLEMENTATION.

  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include             TYPE sobj_name,
          ln_prev_token          TYPE i,
          ln_next_token          TYPE i,
          lf_relevant_join_found TYPE abap_bool,
          lv_tabname             TYPE tabname,
          ln_line                TYPE i,
          ln_column              TYPE token_col.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    LOOP AT it_statements ASSIGNING <ls_statement>.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_token>-str = 'SELECT'.

        lf_relevant_join_found = abap_false.

        DATA(lt_statement_tokens) = get_tokens_for_statement(
          is_statement = <ls_statement>
          it_tokens    = it_tokens
        ).

        CHECK line_exists( lt_statement_tokens[ str = 'JOIN' ] ).

        LOOP AT lt_statement_tokens ASSIGNING FIELD-SYMBOL(<ls_statement_token>)
        WHERE str = 'JOIN'.
          ln_prev_token = sy-tabix - 1.
          ln_next_token = sy-tabix + 1.
          IF lt_statement_tokens[ ln_prev_token ]-str <> 'OUTER'.
            " check if the join contains a text table
            lv_tabname = lt_statement_tokens[ ln_next_token ]-str.
            IF is_text_table( lv_tabname ) = abap_true.
              ln_line = lt_statement_tokens[ ln_next_token ]-row.
              ln_column = lt_statement_tokens[ ln_next_token ]-col.
              lf_relevant_join_found = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

        CHECK lf_relevant_join_found = abap_true.

        lv_include = get_include( p_level = <ls_statement>-level ).

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line         = ln_line
                p_column       = ln_column
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '076'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty   = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'INNER JOIN on text table'.                "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD get_tokens_for_statement.
    CLEAR rt_tokens.
    LOOP AT it_tokens FROM is_statement-from TO is_statement-to ASSIGNING FIELD-SYMBOL(<ls_token>).
      APPEND <ls_token> TO rt_tokens.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_text_table.
    DATA:
      lt_dfies TYPE STANDARD TABLE OF dfies.

    rf_is_texttable = abap_false.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_tablename
      TABLES
        dfies_tab = lt_dfies
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    CHECK sy-subrc = 0.

    CHECK line_exists( lt_dfies[ keyflag = abap_true datatype = 'LANG' ] ).

    rf_is_texttable = abap_true.

  ENDMETHOD.

ENDCLASS.