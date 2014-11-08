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
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_19
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_19
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_19'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_19 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_not       TYPE TABLE OF string,
        lv_statement TYPE string,
        lv_name      TYPE string.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


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

      IF lv_statement CP '* LINE OF *'.
        DELETE TABLE lt_not FROM lv_name.
      ELSE.
        APPEND lv_name TO lt_not.
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
      READ TABLE lt_not FROM lv_name TRANSPORTING NO FIELDS.
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

  description    = 'Use LIKE LINE OF'.                      "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use LINE OF'.                               "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.