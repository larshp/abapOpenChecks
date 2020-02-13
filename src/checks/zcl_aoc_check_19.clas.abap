CLASS zcl_aoc_check_19 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_obj TYPE sap_bool .
    DATA mv_simple TYPE sap_bool .

    METHODS init_range .
  PRIVATE SECTION.

    DATA:
      mt_range     TYPE RANGE OF string.
ENDCLASS.



CLASS ZCL_AOC_CHECK_19 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    TYPES: BEGIN OF ty_name,
             name TYPE string,
           END OF ty_name.

    DATA: lt_not       TYPE SORTED TABLE OF ty_name WITH UNIQUE KEY name,
          lv_statement TYPE string,
          ls_name      TYPE ty_name,
          lv_source    TYPE string,
          lv_name      TYPE string.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    init_range( ).

    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type <> io_scan->gc_statement-comment
        AND type <> io_scan->gc_statement-comment_in_stmnt
        AND type <> io_scan->gc_statement-empty.

      CLEAR lv_statement.
      LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      IF sy-subrc <> 0.
        CONTINUE. " current loop
      ENDIF.

      IF <ls_token>-str = 'DATA' OR <ls_token>-str = 'FIELD-SYMBOLS'.
        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from + 1.
        ASSERT sy-subrc = 0.
        lv_name = <ls_token>-str.

        IF lv_statement IN mt_range.
          DELETE TABLE lt_not WITH TABLE KEY name = lv_name.
        ELSE.
          CLEAR ls_name.
          ls_name-name = lv_name.
          INSERT ls_name INTO TABLE lt_not.
        ENDIF.
      ENDIF.

      CLEAR lv_source.

      FIND REGEX 'READ TABLE .* INTO ([^ .]*).*'
        IN lv_statement SUBMATCHES lv_name.
      IF sy-subrc <> 0.
        FIND REGEX 'LOOP AT (.*) INTO ([^ .]*).*'
          IN lv_statement SUBMATCHES lv_source lv_name.
      ENDIF.
      IF sy-subrc <> 0.
        FIND REGEX 'READ TABLE .* ASSIGNING ([^ .]*).*'
          IN lv_statement SUBMATCHES lv_name.
      ENDIF.
      IF sy-subrc <> 0.
        FIND REGEX 'LOOP AT .* ASSIGNING ([^ .]*).*'
          IN lv_statement SUBMATCHES lv_name.
      ENDIF.
      IF sy-subrc <> 0.
        FIND REGEX 'APPEND INITIAL LINE TO .* ASSIGNING ([^ .]*).*'
          IN lv_statement SUBMATCHES lv_name.
      ENDIF.

* todo, report error where the variable is defined instead of used
      IF sy-subrc = 0.

        IF lv_source CA '()'.
          CONTINUE.
        ENDIF.

* dont report for class member variables
        IF object_type = 'CLAS'.
          SELECT COUNT(*) FROM seocompodf
            WHERE clsname = object_name
            AND typtype <> ''
            AND cmpname = lv_name.
          IF sy-subrc = 0.
            CONTINUE. " current loop
          ENDIF.
        ENDIF.

        READ TABLE lt_not WITH KEY name = lv_name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          inform( p_sub_obj_name = io_scan->get_include( <ls_statement>-level )
                  p_line         = <ls_token>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '002'.
    position       = '019'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_obj    = abap_true.
    mv_simple = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Use LINE OF'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_obj = mv_obj
      mv_simple = mv_simple
      mv_errty = mv_errty
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_obj 'Allow objects' 'C'.              "#EC NOTEXT
    zzaoc_fill_att mv_simple 'Allow simple types' 'C'.      "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD init_range.

    DATA: ls_range LIKE LINE OF mt_range.


    IF NOT mt_range IS INITIAL.
      RETURN.
    ENDIF.

    ls_range-sign = 'I'.
    ls_range-option = 'CP'.

    ls_range-low = '* LINE OF *'.
    APPEND ls_range TO mt_range.

    ls_range-low = '* TYPE ANY'.
    APPEND ls_range TO mt_range.
    IF mv_simple = abap_true.
      ls_range-low = '* TYPE I'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE F'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE D'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE T'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE C *'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE X *'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE N *'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE STRING'.
      APPEND ls_range TO mt_range.
      ls_range-low = '* TYPE XSTRING'.
      APPEND ls_range TO mt_range.
    ENDIF.

    IF mv_obj = abap_true.
      ls_range-low = '* TYPE REF TO *'.
      APPEND ls_range TO mt_range.
    ENDIF.

* generic types
    ls_range-low = '* TYPE DATA'.
    APPEND ls_range TO mt_range.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_obj = mv_obj
      mv_simple = mv_simple
      mv_errty = mv_errty
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
