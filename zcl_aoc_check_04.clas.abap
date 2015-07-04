class ZCL_AOC_CHECK_04 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_04
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_04
*"* do not include other source files here!!!

  data MV_MAXLENGTH type MAXFLENGTH .
private section.
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

* skip class definitions, they are auto generated(in most cases)
    IF object_type = 'CLAS'
        AND strlen( <ls_level>-name ) = 32
        AND ( <ls_level>-name+30(2) = 'CU'
        OR <ls_level>-name+30(2) = 'CO'
        OR <ls_level>-name+30(2) = 'CI'
        OR <ls_level>-name+30(2) = 'CP' ).
      CONTINUE. " current loop
    ENDIF.
    IF <ls_level>-name(8) = '/1BCWDY/'.
* todo, web dynpro
      RETURN.
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

  super->constructor( ).

  description    = 'Line length'.                           "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_maxlength = 90.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT mv_errty = mv_errty mv_maxlength = mv_maxlength TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Reduce line length'.                        "#EC NOTEXT
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
  fill_att mv_maxlength 'Line Length' ''.                   "#EC NOTEXT

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
    mv_maxlength = mv_maxlength
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.