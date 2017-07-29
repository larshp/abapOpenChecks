CLASS zcl_aoc_check_59 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
  PROTECTED SECTION.

    METHODS analyze
      IMPORTING
        !it_tokens     TYPE stokesx_tab
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_59 IMPLEMENTATION.


  METHOD analyze.

    DATA: lt_tokens LIKE it_tokens,
          lo_node   TYPE REF TO zcl_aoc_boolean_node.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF it_tokens.


    READ TABLE it_tokens INDEX 1 ASSIGNING <ls_token>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE <ls_token>-str.
      WHEN 'IF' OR 'ELSEIF' OR 'WHILE'.
* nothing
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    lt_tokens = it_tokens.
    DELETE lt_tokens INDEX 1.

    lo_node = zcl_aoc_boolean=>parse( lt_tokens ).
    IF lo_node IS INITIAL.
      rv_code = '001'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_tokens  LIKE it_tokens,
          lv_code    TYPE sci_errc,
          lv_include TYPE sobj_name.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE type = scan_stmnt_type-standard.

      CLEAR lt_tokens.

      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        APPEND <ls_token> TO lt_tokens.
      ENDLOOP.

      lv_code = analyze( lt_tokens ).

      IF NOT lv_code IS INITIAL.
        lv_include = get_include( p_level = <ls_statement>-level ).
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_kind         = mv_errty
                p_line         = <ls_token>-row
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Boolean expression structure'.        "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '059'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'abapOpenChecks boolean parser error'.     "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
