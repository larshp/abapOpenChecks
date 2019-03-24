CLASS zcl_aoc_check_86 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_86 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '086'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( c_type_program ).
    add_obj_type( 'DOMA' ).
    add_obj_type( 'DTEL' ).
    add_obj_type( 'TABL' ).
    add_obj_type( 'VIEW' ).
    add_obj_type( 'TTYP' ).
    add_obj_type( 'SHLP' ).
    add_obj_type( 'WAPA' ).

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Uses &1 &2, see note &3'.                 "#EC NOTEXT
      WHEN '002'.
        p_text = 'Load database via report ZAOC_UPLOAD_SIDB'. "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_obj_type    TYPE euobj-id,
          lt_result      TYPE STANDARD TABLE OF zaoc_sidb,
          ls_result      LIKE LINE OF lt_result,
          lv_note        TYPE string,
          ls_sidb        TYPE zaoc_sidb,
          lt_environment TYPE senvi_tab.


* check zip is loaded
    SELECT SINGLE * FROM zaoc_sidb INTO ls_sidb.
    IF sy-subrc <> 0.
      inform( p_test    = myname
              p_kind    = 'S'
              p_code    = '002' ).
    ENDIF.

    lv_obj_type = object_type.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
      EXPORTING
        obj_type       = lv_obj_type
        object_name    = object_name
      TABLES
        environment    = lt_environment
      EXCEPTIONS
        batch          = 1
        batchjob_error = 2
        not_executed   = 3
        OTHERS         = 4.
    IF sy-subrc <> 0 OR lines( lt_environment ) = 0.
      RETURN.
    ENDIF.

    SELECT * FROM zaoc_sidb
      INTO TABLE lt_result
      FOR ALL ENTRIES IN lt_environment
      WHERE object_type = lt_environment-type(4)
      AND object_name = lt_environment-object(40).

    LOOP AT lt_result INTO ls_result.
      lv_note = ls_result-note.
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = ls_result-object_type
              p_param_2 = ls_result-object_name
              p_param_3 = lv_note ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
