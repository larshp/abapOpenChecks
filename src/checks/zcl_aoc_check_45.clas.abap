CLASS zcl_aoc_check_45 DEFINITION
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

    METHODS check_loop
      IMPORTING
        !it_statements TYPE zcl_aoc_scan=>ty_statements
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    METHODS support_740sp02
      RETURNING
        VALUE(rv_supported) TYPE abap_bool .
  PRIVATE SECTION.

    DATA mv_lines TYPE flag .
    DATA mv_new TYPE flag .
    DATA mv_inline_decl TYPE flag .
    DATA mv_line_exists TYPE flag .
    DATA mv_ref TYPE flag .
    DATA mv_condense TYPE flag .
    DATA mv_concat_lines TYPE flag .
    DATA mv_shift TYPE flag .
    DATA mv_translate_to TYPE flag .
    DATA mv_translate_using TYPE flag .
    DATA mv_templates TYPE flag .
    DATA mv_corresponding TYPE flag .
ENDCLASS.



CLASS zcl_aoc_check_45 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE zcl_aoc_scan=>ty_statements,
          lt_procedure  TYPE zcl_aoc_scan=>ty_statements,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    lt_statements = io_scan->build_statements( ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      APPEND <ls_statement> TO lt_procedure.
      IF <ls_statement>-str CP 'METHOD *'
          OR <ls_statement>-str = 'ENDMETHOD'
          OR <ls_statement>-str CP 'FORM *'
          OR <ls_statement>-str = 'ENDFORM'.
        CLEAR lt_procedure.
      ENDIF.

      IF ( <ls_statement>-str CP 'DESCRIBE TABLE *'
          AND <ls_statement>-count = 3
          AND mv_lines = abap_true )
          OR ( <ls_statement>-str CP 'DESCRIBE TABLE * LINES *'
          AND mv_lines = abap_true ).
        lv_code = '001'.
      ELSEIF <ls_statement>-str CP 'CREATE OBJECT *'
          AND NOT <ls_statement>-str CP '* TYPE (*'
          AND NOT <ls_statement>-str CP '* EXCEPTIONS *'
          AND NOT <ls_statement>-str CP '* AREA HANDLE *'
          AND mv_new = abap_true
          AND support_740sp02( ) = abap_true.
        lv_code = '002'.
      ELSEIF ( ( <ls_statement>-str CP 'LOOP AT * INTO *'
          AND NOT <ls_statement>-str CP 'LOOP AT * INTO DATA(*' )
          OR ( <ls_statement>-str CP 'LOOP AT * ASSIGNING *'
          AND NOT <ls_statement>-str CP 'LOOP AT * ASSIGNING FIELD-SYMBOL(*' )
          OR ( <ls_statement>-str CP 'CATCH * INTO *'
          AND NOT <ls_statement>-str CP 'CATCH * INTO DATA(*' ) )
          AND mv_inline_decl = abap_true
          AND support_740sp02( ) = abap_true
          AND check_loop( lt_procedure ) = abap_true.
        lv_code = '003'.
      ELSEIF mv_condense = abap_true
          AND <ls_statement>-str CP 'CONDENSE *'.
        lv_code = '004'.
      ELSEIF mv_concat_lines = abap_true
          AND <ls_statement>-str CP 'CONCATENATE LINES OF *'
          AND NOT <ls_statement>-str CP '* IN BYTE MODE*'
          AND NOT <ls_statement>-str CP '* RESPECTING BLANKS*'.
        lv_code = '005'.
      ELSEIF mv_shift = abap_true
          AND <ls_statement>-str CP 'SHIFT *'.
        lv_code = '006'.
      ELSEIF mv_translate_to = abap_true
          AND <ls_statement>-str CP 'TRANSLATE * TO *'.
        lv_code = '007'.
      ELSEIF mv_translate_using = abap_true
          AND <ls_statement>-str CP 'TRANSLATE * USING *'.
        lv_code = '008'.
      ELSEIF mv_templates = abap_true
          AND <ls_statement>-str CP 'CONCATENATE *'.
        lv_code = '009'.
      ELSEIF mv_ref = abap_true
          AND <ls_statement>-str CP 'GET REFERENCE OF *'
          AND support_740sp02( ) = abap_true.
        lv_code = '010'.
      ELSEIF mv_corresponding = abap_true
          AND <ls_statement>-str CP 'MOVE-CORRESPONDING * TO *'
          AND support_740sp02( ) = abap_true.
        lv_code = '011'.
      ELSEIF mv_line_exists = abap_true
          AND <ls_statement>-str CP 'READ TABLE * TRANSPORTING NO FIELDS*'
          AND NOT <ls_statement>-str CP '* BINARY SEARCH*'
          AND support_740sp02( ) = abap_true.
        lv_code = '012'.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_loop.

* this functionality is not completely correct, but will work in most cases

    CONSTANTS: lc_into      TYPE string VALUE 'INTO',
               lc_assigning TYPE string VALUE 'ASSIGNING'.

    DATA: lt_str       TYPE TABLE OF string,
          ls_statement LIKE LINE OF it_statements,
          lv_declare   TYPE i,
          lv_var       TYPE string,
          lv_tabix     TYPE i.


    READ TABLE it_statements INDEX lines( it_statements ) INTO ls_statement.
    ASSERT sy-subrc = 0.

    SPLIT ls_statement-str AT space INTO TABLE lt_str.
    READ TABLE lt_str FROM lc_into TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      READ TABLE lt_str FROM lc_assigning TRANSPORTING NO FIELDS.
    ENDIF.
    IF sy-subrc <> 0.
      RETURN. " sometimes the keyword is not found, something with macros
    ENDIF.
    lv_tabix = sy-tabix + 1.
    READ TABLE lt_str INDEX lv_tabix INTO lv_var.
    ASSERT sy-subrc = 0.

* make sure it is a local variable
    LOOP AT it_statements TRANSPORTING NO FIELDS WHERE str CP |DATA { lv_var }*|
        OR str CP |FIELD-SYMBOLS { lv_var }*|
        OR str CP |FIELD-SYMBOL { lv_var }*|.
      lv_declare = sy-tabix + 1.
    ENDLOOP.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* find the first use of the variable, this should be the LOOP
    LOOP AT it_statements INTO ls_statement FROM lv_declare
        WHERE str CP |*{ lv_var } *| OR str CP |* { lv_var }*|.
      IF sy-tabix <> lines( it_statements ).
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '004'.
    position       = '045'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_lines           = abap_true.
    mv_new             = abap_true.
    mv_inline_decl     = abap_true.
    mv_condense        = abap_true.
    mv_concat_lines    = abap_true.
    mv_shift           = abap_true.
    mv_translate_to    = abap_true.
    mv_translate_using = abap_true.
    mv_ref             = abap_true.
    mv_corresponding   = abap_true.
    mv_line_exists     = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Use lines( ) expression'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Use NEW abc( ) expression'(m02) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Declare variable inline'(m03) ).

    insert_scimessage(
        iv_code = '004'
        iv_text = 'Use condense( )'(m04) ).

    insert_scimessage(
        iv_code = '005'
        iv_text = 'Use concat_lines_of( )'(m05) ).

    insert_scimessage(
        iv_code = '006'
        iv_text = 'Use shift_left( ) or shift_right( )'(m06) ).

    insert_scimessage(
        iv_code = '007'
        iv_text = 'Use to_upper( ) or to_lower( )'(m07) ).

    insert_scimessage(
        iv_code = '008'
        iv_text = 'Use translate( )'(m08) ).

    insert_scimessage(
        iv_code = '009'
        iv_text = 'Use string templates'(m09) ).

    insert_scimessage(
        iv_code = '010'
        iv_text = 'Use REF expression'(m10) ).

    insert_scimessage(
        iv_code = '011'
        iv_text = 'Use corresponding #( )'(m11) ).

    insert_scimessage(
        iv_code = '012'
        iv_text = 'Use line_exists( )'(m12) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_lines           = mv_lines
      mv_new             = mv_new
      mv_inline_decl     = mv_inline_decl
      mv_condense        = mv_condense
      mv_concat_lines    = mv_concat_lines
      mv_shift           = mv_shift
      mv_translate_to    = mv_translate_to
      mv_translate_using = mv_translate_using
      mv_templates       = mv_templates
      mv_ref             = mv_ref
      mv_corresponding   = mv_corresponding
      mv_errty           = mv_errty
      mv_line_exists     = mv_line_exists
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_lines 'lines( )' ''.                  "#EC NOTEXT
    zzaoc_fill_att mv_new 'NEW #( )' ''.                    "#EC NOTEXT
    zzaoc_fill_att mv_inline_decl 'Inline declarations' ''. "#EC NOTEXT
    zzaoc_fill_att mv_condense 'condense( )' ''.            "#EC NOTEXT
    zzaoc_fill_att mv_concat_lines 'concat_lines_of( )' ''. "#EC NOTEXT
    zzaoc_fill_att mv_shift 'shift_left( ) or shift_right( )' ''. "#EC NOTEXT
    zzaoc_fill_att mv_translate_to 'to_upper( ) or to_lower( )' ''. "#EC NOTEXT
    zzaoc_fill_att mv_translate_using 'translate( )' ''.    "#EC NOTEXT
    zzaoc_fill_att mv_templates 'CONCATENATE -> String templates' ''. "#EC NOTEXT
    zzaoc_fill_att mv_ref 'REF #( )' ''.                    "#EC NOTEXT
    zzaoc_fill_att mv_corresponding 'corresponding #( )' ''. "#EC NOTEXT
    zzaoc_fill_att mv_line_exists 'line_exists( )' ''.      "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_lines           = mv_lines
      mv_new             = mv_new
      mv_inline_decl     = mv_inline_decl
      mv_condense        = mv_condense
      mv_concat_lines    = mv_concat_lines
      mv_shift           = mv_shift
      mv_translate_to    = mv_translate_to
      mv_translate_using = mv_translate_using
      mv_templates       = mv_templates
      mv_ref             = mv_ref
      mv_corresponding   = mv_corresponding
      mv_errty           = mv_errty
      mv_line_exists     = mv_line_exists
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD support_740sp02.

    rv_supported = lcl_supported=>support_740sp02( ).

  ENDMETHOD.
ENDCLASS.