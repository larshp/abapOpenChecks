CLASS zcl_aoc_check_66 DEFINITION
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

    DATA mv_simplification TYPE sap_bool .
    DATA mv_exists TYPE sap_bool .

    METHODS check_exists
      IMPORTING
        !is_statement  TYPE ty_statement
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
    METHODS check_simplification
      IMPORTING
        !is_statement  TYPE ty_statement
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
ENDCLASS.



CLASS ZCL_AOC_CHECK_66 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE ty_statements,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    lt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements ).

    LOOP AT lt_statements ASSIGNING <ls_statement>
        WHERE str CP 'RAISE EXCEPTION TYPE *'.

      CLEAR lv_code.

      IF lv_code IS INITIAL.
        lv_code = check_simplification( <ls_statement> ).
      ENDIF.

      IF lv_code IS INITIAL.
        lv_code = check_exists( <ls_statement> ).
      ENDIF.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_exists.

    DATA: lv_msgnr TYPE t100-msgnr,
          lv_arbgb TYPE t100-arbgb.


    FIND REGEX ' MESSAGE \w(\d+)\((\w+)\)' IN is_statement-str
      SUBMATCHES lv_msgnr lv_arbgb ##NO_TEXT.
    IF sy-subrc = 0.
      SELECT SINGLE arbgb FROM t100 INTO lv_arbgb
        WHERE arbgb = lv_arbgb
        AND msgnr = lv_msgnr ##WARN_OK.                 "#EC CI_GENBUFF
      IF sy-subrc <> 0.
        rv_code = '002'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_simplification.

    IF is_statement-str CP '* MESSAGE ID ''*'
        AND is_statement-str CP '* TYPE ''*'.
      IF is_statement-str CP '* NUMBER ''*'.
        rv_code = '001'.
        RETURN.
      ENDIF.
      FIND REGEX ' NUMBER \d' IN is_statement-str ##NO_TEXT.
      IF sy-subrc = 0.
        rv_code = '001'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '066'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.
    mv_exists = abap_true.
    mv_simplification = abap_true.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_exists = mv_exists
      mv_simplification = mv_simplification
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Statement can be simiplified, MESSAGE tn(id)'. "#EC NOTEXT
      WHEN '002'.
        p_text = 'Message does not exist'.                  "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_exists 'Check message exists' ''.     "#EC NOTEXT
    zzaoc_fill_att mv_simplification 'Check for simplifications' ''. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_exists = mv_exists
      mv_simplification = mv_simplification
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
