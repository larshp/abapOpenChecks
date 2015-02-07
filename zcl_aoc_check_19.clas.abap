class ZCL_AOC_CHECK_19 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_19
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_19
*"* do not include other source files here!!!

  data MV_OBJ type SAP_BOOL .
  data MV_SIMPLE type SAP_BOOL .

  methods INIT_RANGE .
private section.
*"* private components of class ZCL_AOC_CHECK_19
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_19'. "#EC NOTEXT
  data:
    mt_range     TYPE RANGE OF string .
ENDCLASS.



CLASS ZCL_AOC_CHECK_19 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  TYPES: BEGIN OF st_name,
           name TYPE string,
         END OF st_name.

  DATA: lt_not       TYPE SORTED TABLE OF st_name WITH UNIQUE KEY name,
        lv_statement TYPE string,
        ls_name      TYPE st_name,
        lv_name      TYPE string.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  init_range( ).

  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type <> scan_stmnt_type-comment
      AND type <> scan_stmnt_type-comment_in_stmnt
      AND type <> scan_stmnt_type-empty.

    CLEAR lv_statement.
    LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    IF sy-subrc <> 0.
      CONTINUE. " current loop
    ENDIF.

    IF <ls_token>-str = 'DATA' OR <ls_token>-str = 'FIELD-SYMBOLS'.
      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from + 1.
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

    FIND REGEX 'READ TABLE .* INTO ([^ .]*).*'
      IN lv_statement SUBMATCHES lv_name.
    IF sy-subrc <> 0.
      FIND REGEX 'LOOP AT .* INTO ([^ .]*).*'
        IN lv_statement SUBMATCHES lv_name.
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

* todo, also show error if looping at table with simple types
* todo, report error where the variable is defined instead of used
    IF sy-subrc = 0.
      READ TABLE lt_not WITH KEY name = lv_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = get_include( p_level = <ls_statement>-level )
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = c_my_name
                p_code         = '001' ).
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Use LINE OF'.                           "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty  = c_error.
  mv_obj    = abap_true.
  mv_simple = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT mv_obj = mv_obj mv_simple = mv_simple TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use LINE OF'.                               "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~query_attributes.

  DATA: lv_ok         TYPE abap_bool,
        lv_message    TYPE c LENGTH 72,
        lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT
  fill_att mv_obj 'Allow objects' 'C'.                      "#EC NOTEXT
  fill_att mv_simple 'Allow simple types' 'C'.              "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = c_my_name
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF ( mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note ).
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

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

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_obj = mv_obj
    mv_simple = mv_simple
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.