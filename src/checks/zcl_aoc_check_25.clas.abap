CLASS zcl_aoc_check_25 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_field,
        name  TYPE string,
        level TYPE i,
        row   TYPE token_row,
      END OF ty_field.
    TYPES:
      ty_fields_tt TYPE STANDARD TABLE OF ty_field WITH NON-UNIQUE DEFAULT KEY.

    DATA mv_skip_radio TYPE sychar01.

    METHODS strip
      IMPORTING
        !iv_input        TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.
    METHODS analyze
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_fields     TYPE ty_fields_tt
        !it_statements TYPE sstmnt_tab.
    METHODS find_fields
      IMPORTING
        !it_tokens       TYPE stokesx_tab
        !it_statements   TYPE sstmnt_tab
      RETURNING
        VALUE(rt_fields) TYPE ty_fields_tt.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_25 IMPLEMENTATION.


  METHOD analyze.

* this might not be 100% correct but will work for most cases

    DATA: lv_include TYPE program,
          lv_name    TYPE string,
          lt_fields  LIKE it_fields.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_field>     LIKE LINE OF it_fields,
                   <ls_token>     LIKE LINE OF it_tokens.


    lt_fields = it_fields.

    LOOP AT it_statements ASSIGNING <ls_statement>.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from
          TO <ls_statement>-to
          WHERE type <> scan_token_type-comment
          AND type <> scan_token_type-literal.
        lv_name = strip( <ls_token>-str ).
        DELETE lt_fields
          WHERE name = lv_name
          AND ( row <> <ls_token>-row
          OR level <> <ls_statement>-level ).
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_fields ASSIGNING <ls_field>.

      lv_include = get_include( p_level = <ls_field>-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_field>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = <ls_field>-name ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_fields TYPE ty_fields_tt.


    lt_fields = find_fields( it_tokens     = it_tokens
                             it_statements = it_statements ).

    analyze( it_tokens     = it_tokens
             it_fields     = lt_fields
             it_statements = it_statements ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '025'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.
    mv_skip_radio = abap_true.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD find_fields.

    DATA: lv_keyword   TYPE string,
          lt_code      TYPE string_table,
          ls_result    TYPE zcl_aoc_parser=>ty_result,
          ls_field     LIKE LINE OF rt_fields,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_rt>        LIKE LINE OF ls_result-tokens,
                   <ls_token>     LIKE LINE OF it_tokens.


    LOOP AT it_statements ASSIGNING <ls_statement> WHERE type = scan_stmnt_type-standard.

      CLEAR lv_keyword.
      CLEAR lv_statement.

      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from
          TO <ls_statement>-to
          WHERE type <> scan_token_type-comment.

        IF lv_keyword IS INITIAL.
          lv_keyword = <ls_token>-str.
        ENDIF.

        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_keyword <> 'PARAMETERS'
          AND lv_keyword <> 'PARAMETER'
          AND lv_keyword <> 'SELECT-OPTIONS'.
        CONTINUE. " current loop
      ENDIF.

      CLEAR lt_code.
      APPEND lv_statement TO lt_code.
      ls_result = zcl_aoc_parser=>run( it_code = lt_code
                                       iv_rule = lv_keyword ).

      IF ls_result-match = abap_false.
        CONTINUE.
      ENDIF.

      LOOP AT ls_result-tokens ASSIGNING <ls_rt>
          WHERE value = zcl_aoc_parser=>c_role-fielddefid
          AND type = zcl_aoc_parser=>c_type-role.

        IF mv_skip_radio = abap_true.
          READ TABLE ls_result-tokens
            WITH KEY type = zcl_aoc_parser=>c_type-terminal code = 'RADIOBUTTON'
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR ls_field.
        ls_field-name  = strip( <ls_rt>-code ).
        ls_field-level = <ls_statement>-level.
        ls_field-row   = <ls_token>-row.
        APPEND ls_field TO rt_fields.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_skip_radio = mv_skip_radio
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = '&1 not referenced statically'.            "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_skip_radio 'Skip radio buttons' 'C'.  "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_skip_radio = mv_skip_radio
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD strip.

    DATA: lv_offset TYPE i,
          lv_length TYPE i.


    rv_output = iv_input.

    FIND FIRST OCCURRENCE OF '[' IN rv_output MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      rv_output = rv_output(lv_offset).
    ENDIF.

    FIND FIRST OCCURRENCE OF '+' IN rv_output MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      rv_output = rv_output(lv_offset).
    ENDIF.

    lv_length = strlen( rv_output ) - 1.
    IF strlen( rv_output ) > 1 AND rv_output(1) = '(' AND rv_output+lv_length(1) = ')'.
      lv_length = lv_length - 1.
      rv_output = rv_output+1(lv_length).
    ELSE.
      FIND FIRST OCCURRENCE OF '(' IN rv_output MATCH OFFSET lv_offset.
      IF sy-subrc = 0.
        rv_output = rv_output(lv_offset).
      ENDIF.
    ENDIF.

    IF strlen( rv_output ) > 1 AND rv_output(1) = '@'.
      rv_output = rv_output+1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
