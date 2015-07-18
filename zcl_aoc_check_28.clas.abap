class ZCL_AOC_CHECK_28 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_28
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_28 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_len   TYPE i,
        lt_code  TYPE string_table,
        lv_code  LIKE LINE OF lt_code,
        lv_level TYPE stmnt_levl.

  FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                 <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lv_level = sy-tabix.

* skip class definitions, they are auto generated(in most cases)
    IF object_type = 'INTF'.
      RETURN.
    ENDIF.
    IF object_type = 'CLAS'
        AND strlen( <ls_level>-name ) = 32
        AND ( <ls_level>-name+30(2) = 'CU'
        OR <ls_level>-name+30(2) = 'CO'
        OR <ls_level>-name+30(2) = 'CI'
        OR <ls_level>-name+30(2) = 'CP' ).
      CONTINUE. " current loop
    ENDIF.

    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE type <> scan_stmnt_type-comment
        AND type <> scan_stmnt_type-comment_in_stmnt
        AND terminator <> ''
        AND level = lv_level.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-to.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lv_len = <ls_token>-col + strlen( <ls_token>-str ).

      IF <ls_statement>-tcol > lv_len.
        lt_code = get_source( <ls_level> ).
        READ TABLE lt_code INDEX <ls_token>-row INTO lv_code.
* pragmas are not part of IT_TOKENS or IT_STATEMENTS, so check:
        IF sy-subrc = 0 AND lv_code CP '*##*'.
          CONTINUE. " current loop
        ENDIF.

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_level>-name
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Space before . or ,'.                        "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.
  position       = '028'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Space before . or ,'.                            "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.