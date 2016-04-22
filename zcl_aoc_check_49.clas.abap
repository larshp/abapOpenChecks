class ZCL_AOC_CHECK_49 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_49
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.

  class-data GO_CONV type ref to CL_ABAP_CONV_OUT_CE .
*"* protected components of class ZCL_AOC_CHECK_49
*"* do not include other source files here!!!
private section.

  types:
    BEGIN OF ty_code,
           text TYPE c LENGTH 255,
           row  TYPE token_row,
           name TYPE level_name,
         END OF ty_code .
  types:
    ty_code_tt TYPE STANDARD TABLE OF ty_code WITH DEFAULT KEY .

  methods BUILD
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
    returning
      value(RT_CODE) type TY_CODE_TT .
  methods CHECK_CODE
    importing
      !IT_CODE type TY_CODE_TT .
ENDCLASS.



CLASS ZCL_AOC_CHECK_49 IMPLEMENTATION.


METHOD build.

  DATA: lv_prev   TYPE token_row,
        lv_offset TYPE i,
        lv_level  LIKE sy-tabix.

  FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                 <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens,
                 <ls_code>      LIKE LINE OF rt_code.


  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lv_level = sy-tabix.

    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE level = lv_level
        AND coloncol = 0.

      CLEAR lv_prev.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type <> scan_token_type-comment.
        IF lv_prev <> <ls_token>-row.
          APPEND INITIAL LINE TO rt_code ASSIGNING <ls_code>.
          <ls_code>-name = <ls_level>-name.
          <ls_code>-row = <ls_token>-row.

          lv_prev = <ls_token>-row.
        ENDIF.

        lv_offset = <ls_token>-col.
        <ls_code>-text+lv_offset = <ls_token>-str.
      ENDLOOP.

    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_code TYPE ty_code_tt.


  lt_code = build( it_tokens     = it_tokens
                   it_statements = it_statements
                   it_levels     = it_levels ).

  check_code( lt_code ).

ENDMETHOD.


METHOD check_code.

  DATA: lv_error TYPE sci_errc,
        lv_code  TYPE c LENGTH 255.

  FIELD-SYMBOLS: <ls_code> LIKE LINE OF it_code.


  LOOP AT it_code ASSIGNING <ls_code>.

    CLEAR lv_error.

    lv_code = <ls_code>-text.
    WHILE lv_code(1) = space.
      SHIFT lv_code LEFT DELETING LEADING space IN CHARACTER MODE.
    ENDWHILE.

    IF lv_code CP 'IF  *'.
      lv_error = '001'.
    ELSEIF lv_code CP 'SHIFT  *'.
      lv_error = '002'.
    ELSEIF lv_code CP 'WHEN  *'.
      lv_error = '003'.
    ELSEIF lv_code CP 'READ TABLE  *'.
      lv_error = '004'.
    ELSEIF lv_code CP 'MODIFY  *'.
      lv_error = '005'.
    ELSEIF lv_code CP 'DELETE  *'.
      lv_error = '006'.
    ELSEIF lv_code CP 'COLLECT  *'.
      lv_error = '007'.
    ELSEIF lv_code CP 'CHECK  *'.
      lv_error = '008'.
    ELSEIF lv_code CP 'SORT  *'.
      lv_error = '009'.
    ENDIF.

    IF NOT lv_error IS INITIAL.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = <ls_code>-name
              p_line         = <ls_code>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = lv_error ).
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Double space'.                          "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '049'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Double space after IF'.                     "#EC NOTEXT
    WHEN '002'.
      p_text = 'Double space after SHIFT'.                  "#EC NOTEXT
    WHEN '003'.
      p_text = 'Double space after WHEN'.                   "#EC NOTEXT
    WHEN '004'.
      p_text = 'Double space after READ TABLE'.             "#EC NOTEXT
    WHEN '005'.
      p_text = 'Double space after MODIFY'.                 "#EC NOTEXT
    WHEN '006'.
      p_text = 'Double space after DELETE'.                 "#EC NOTEXT
    WHEN '007'.
      p_text = 'Double space after COLLECT'.                "#EC NOTEXT
    WHEN '008'.
      p_text = 'Double space after CHECK'.                  "#EC NOTEXT
    WHEN '009'.
      p_text = 'Double space after SORT'.                   "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.