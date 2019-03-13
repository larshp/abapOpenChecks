CLASS zcl_aoc_check_45 DEFINITION
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

    METHODS check_loop
      IMPORTING
        !is_statement  TYPE ty_statement
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



CLASS ZCL_AOC_CHECK_45 IMPLEMENTATION.


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

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF ( <ls_statement>-str CP 'DESCRIBE TABLE *'
          AND <ls_statement>-count = 3
          AND mv_lines = abap_true )
          OR ( <ls_statement>-str CP 'DESCRIBE TABLE * LINES *'
          AND mv_lines = abap_true ).
        lv_code = '001'.
      ELSEIF <ls_statement>-str CP 'CREATE OBJECT *'
          AND NOT <ls_statement>-str CP '* TYPE (*'
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
          AND check_loop( <ls_statement> ) = abap_true.
        lv_code = '003'.
      ELSEIF mv_condense = abap_true
          AND <ls_statement>-str CP 'CONDENSE *'.
        lv_code = '004'.
      ELSEIF mv_concat_lines = abap_true
          AND <ls_statement>-str CP 'CONCATENATE LINES OF *'
          AND NOT <ls_statement>-str CP '* IN BYTE MODE*'.
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

* todo, add READ TABLE?
*           IF can be changed to: boolc()

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


  METHOD check_loop.

    CONSTANTS: lc_into      TYPE string VALUE 'INTO',
               lc_assigning TYPE string VALUE 'ASSIGNING'.

    DATA: lt_result TYPE scr_refs,
          lt_str    TYPE TABLE OF string,
          lv_var    TYPE string.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result.


    lt_result = zcl_aoc_compiler=>get_instance(
      iv_object_type = object_type
      iv_object_name = object_name )->get_result( ).
    DELETE lt_result WHERE tag <> cl_abap_compiler=>tag_data.
    DELETE lt_result WHERE name = ''.

    SPLIT is_statement-str AT space INTO TABLE lt_str.
    READ TABLE lt_str FROM lc_into TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      READ TABLE lt_str FROM lc_assigning TRANSPORTING NO FIELDS.
    ENDIF.
    ASSERT sy-subrc = 0.
    sy-tabix = sy-tabix + 1.
    READ TABLE lt_str INDEX sy-tabix INTO lv_var.
    ASSERT sy-subrc = 0.

* this will make sure it is a local variable
    READ TABLE lt_result WITH KEY
      name = lv_var
      grade = cl_abap_compiler=>grade_definition
      mode2 = '2'             " downport, cl_abap_compiler=>mode2_def
      statement->source_info->name = is_statement-include
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* find the first use of the variable, this should be the LOOP
    READ TABLE lt_result ASSIGNING <ls_result> WITH KEY
      name = lv_var
      grade = cl_abap_compiler=>grade_direct
      statement->source_info->name = is_statement-include.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF <ls_result>-statement->start_line = is_statement-start-row.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '004'.
    position       = '045'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.

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


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Use lines( ) expression'.                 "#EC NOTEXT
      WHEN '002'.
        p_text = 'Use NEW abc( ) expression'.               "#EC NOTEXT
      WHEN '003'.
        p_text = 'Declare variable inline'.                 "#EC NOTEXT
      WHEN '004'.
        p_text = 'Use condense( )'.                         "#EC NOTEXT
      WHEN '005'.
        p_text = 'Use concat_lines_of( )'.                  "#EC NOTEXT
      WHEN '006'.
        p_text = 'Use shift_left( ) or shift_right( )'.     "#EC NOTEXT
      WHEN '007'.
        p_text = 'Use to_upper( ) or to_lower( )' .         "#EC NOTEXT
      WHEN '008'.
        p_text = 'Use translate( )' .                       "#EC NOTEXT
      WHEN '009'.
        p_text = 'Use string templates' .                   "#EC NOTEXT
      WHEN '010'.
        p_text = 'Use REF expression' .                     "#EC NOTEXT
      WHEN '011'.
        p_text = 'Use corresponding #( )' .                 "#EC NOTEXT
      WHEN '012'.
        p_text = 'Use line_exists( )' .                     "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_lines 'lines( )' ''.                  "#EC NOTEXT
    zzaoc_fill_att mv_new 'NEW #( )' ''.                    "#EC NOTEXT
    zzaoc_fill_att mv_inline_decl 'Inline declarations' ''. "#EC NOTEXT
    zzaoc_fill_att mv_condense 'condense( )' ''.            "#EC NOTEXT
    zzaoc_fill_att mv_concat_lines 'concate_lines_of( )' ''. "#EC NOTEXT
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
