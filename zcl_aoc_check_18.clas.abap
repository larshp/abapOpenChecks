class ZCL_AOC_CHECK_18 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_18
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_18
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_18
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_18'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_18 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include TYPE program,
        lv_found   TYPE abap_bool.

  FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures,
                 <ls_token>     LIKE LINE OF it_tokens,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_structures ASSIGNING <ls_structure>
      WHERE type = scan_struc_type-condition.

    lv_found = abap_false.

    LOOP AT it_statements ASSIGNING <ls_statement>
        FROM <ls_structure>-stmnt_from TO <ls_structure>-stmnt_to.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      CASE <ls_token>-str.
        WHEN 'WHEN' OR 'ELSEIF' OR 'ELSE'.
          CONTINUE.
        WHEN OTHERS.
          lv_found = abap_true.
          EXIT.
      ENDCASE.
    ENDLOOP.

    IF lv_found = abap_false.
      READ TABLE it_statements ASSIGNING <ls_statement> INDEX <ls_structure>-stmnt_from.
      CHECK sy-subrc = 0.
      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      CHECK sy-subrc = 0.

      lv_include = get_include( p_level = <ls_statement>-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line = <ls_token>-row
              p_kind = mv_errty
              p_test = c_my_name
              p_code = '001' ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = 'Empty branch'.                          "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Empty branch'.                              "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.