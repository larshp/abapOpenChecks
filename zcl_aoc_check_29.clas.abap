class ZCL_AOC_CHECK_29 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_29
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
private section.

  data MV_NAME type SEOCLSNAME .
*"* private components of class ZCL_AOC_CHECK_29
*"* do not include other source files here!!!
  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_29' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_AOC_CHECK_29 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_name  TYPE string,
        lv_str   TYPE string,
        lv_level TYPE stmnt_levl.

  FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                 <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lv_level = sy-tabix.


    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE type <> scan_stmnt_type-comment
        AND type <> scan_stmnt_type-comment_in_stmnt
        AND level = lv_level.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      IF sy-subrc <> 0 OR <ls_token>-str <> 'CLASS'.
        CONTINUE.
      ENDIF.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from + 1.
      ASSERT sy-subrc = 0.
      lv_name = <ls_token>-str.
      TRANSLATE lv_name TO UPPER CASE.

      CLEAR lv_str.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from + 2
          TO <ls_statement>-to.
        IF lv_str IS INITIAL.
          lv_str = <ls_token>-str.
        ELSE.
          CONCATENATE lv_str <ls_token>-str INTO lv_str SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF NOT lv_str CP '*FOR TESTING*'.
        CONTINUE.
      ENDIF.

      IF NOT lv_name CP mv_name.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_level>-name
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = c_my_name
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Naming, Local test classes'.            "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_name = 'LTCL_*'.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mv_name = mv_name
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Naming, Local test classes'.                "#EC NOTEXT
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
  fill_att mv_name 'Name' ''.                               "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = myname
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mv_name  = mv_name
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.