CLASS zcl_aoc_check_32 DEFINITION
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
    METHODS put_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_devclass TYPE packrange.
    DATA mv_ignore_ltcl TYPE boolean.
ENDCLASS.



CLASS ZCL_AOC_CHECK_32 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_statement TYPE string,
          lv_devclass  TYPE tadir-devclass,
          lv_include   TYPE sobj_name.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    IF mt_devclass IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE type = scan_stmnt_type-standard
        OR type = scan_stmnt_type-method_direct.

      CLEAR lv_statement.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type = scan_token_type-identifier.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_statement CP 'SELECT * FROM *'.
        IF NOT object_type IS INITIAL.
          SELECT SINGLE devclass FROM tadir INTO lv_devclass
            WHERE pgmid = 'R3TR'
            AND object = object_type
            AND obj_name = object_name.
          IF sy-subrc <> 0 OR lv_devclass NOT IN mt_devclass.
            RETURN.
          ENDIF.
        ENDIF.

        lv_include = get_include( p_level = <ls_statement>-level ).

        IF mv_ignore_ltcl = abap_true
            AND object_type = 'CLAS'
            AND strlen( lv_include ) = 34
            AND lv_include+30(4) = 'CCAU'.
          CONTINUE.
        ENDIF.

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

* todo, database operations, configurable?:
* UPDATE
* MODIFY
* INSERT

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '032'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.
    mv_ignore_ltcl = abap_true.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mt_devclass = mt_devclass
      mv_ignore_ltcl = mv_ignore_ltcl
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Database access'.                         "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mt_devclass 'Package' 'S'.               "#EC NOTEXT
    zzaoc_fill_att mv_ignore_ltcl 'Ignore local test classes' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mt_devclass = mt_devclass
      mv_ignore_ltcl = mv_ignore_ltcl
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
