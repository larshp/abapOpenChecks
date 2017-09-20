CLASS zcl_aoc_check_04 DEFINITION
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

    DATA mv_skipc TYPE flag.
    DATA mv_maxlength TYPE maxflength.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_04 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_len   LIKE mv_maxlength,
          lv_level TYPE stmnt_levl.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                   <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
      lv_level = sy-tabix.

      IF mv_skipc = abap_true
          AND is_class_definition( <ls_level>-name ) = abap_true.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT it_statements ASSIGNING <ls_statement>
          WHERE level = lv_level.
        LOOP AT it_tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to.

          lv_len = <ls_token>-col + <ls_token>-len1.
          IF lv_len > mv_maxlength.
            inform( p_sub_obj_type = c_type_include
                    p_sub_obj_name = <ls_level>-name
                    p_line         = <ls_token>-row
                    p_kind         = mv_errty
                    p_test         = myname
                    p_code         = '001' ).
            EXIT. " only one error per statement
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    FIELD-SYMBOLS: <lv_rfc> TYPE abap_bool.

    super->constructor( ).

    description    = 'Line length'.                         "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '002'.
    position       = '004'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    ASSIGN ('REMOTE_RFC_ENABLED') TO <lv_rfc>.
    IF sy-subrc = 0.
      <lv_rfc> = abap_true.
    ENDIF.

    mv_errty     = c_error.
    mv_maxlength = 90.
    mv_skipc     = abap_true.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_maxlength = mv_maxlength
      mv_skipc = mv_skipc
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Reduce line length'.                      "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_maxlength 'Line Length' ''.           "#EC NOTEXT
    zzaoc_fill_att mv_skipc 'Skip global class definitions' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_maxlength = mv_maxlength
      mv_skipc = mv_skipc
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
