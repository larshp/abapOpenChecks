class ZCL_AOC_CHECK_14 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_14
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_14
*"* do not include other source files here!!!
private section.

  methods PARSE
    importing
      !IT_COMMENTED type STRING_TABLE
      !IS_LEVEL type SLEVEL
      !IV_LINE type TOKEN_ROW .
ENDCLASS.



CLASS ZCL_AOC_CHECK_14 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_code      TYPE string_table,
        lt_commented TYPE string_table,
        lv_line      TYPE token_row.

  FIELD-SYMBOLS: <ls_level> LIKE LINE OF it_levels,
                 <lv_code>  LIKE LINE OF lt_code.


  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lt_code = get_source( <ls_level> ).

    LOOP AT lt_code ASSIGNING <lv_code>.

      IF strlen( <lv_code> ) > 1 AND <lv_code>(1) = '*'.
        IF lines( lt_commented ) = 0.
          lv_line = sy-tabix.
        ENDIF.
        APPEND <lv_code>+1 TO lt_commented.
      ELSE.
        parse( it_commented = lt_commented
               is_level     = <ls_level>
               iv_line      = lv_line ).
        CLEAR lt_commented.
      ENDIF.

    ENDLOOP.

    parse( it_commented = lt_commented
           is_level     = <ls_level>
           iv_line      = lv_line ).
    CLEAR lt_commented.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Commented code'.                        "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '014'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Commented code'.                            "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD parse.

  DATA: lv_code LIKE LINE OF it_commented.

  FIELD-SYMBOLS: <lv_commented> LIKE LINE OF it_commented.


  IF lines( it_commented ) = 0.
    RETURN.
  ENDIF.

* skip auto generated INCLUDEs in function group top include
  IF ( is_level-name CP 'LY*TOP' OR is_level-name CP 'LZ*TOP' )
      AND lines( it_commented ) = 1.
    READ TABLE it_commented INDEX 1 INTO lv_code.
    ASSERT sy-subrc = 0.

    IF lv_code CS 'INCLUDE L'.
      RETURN.
    ENDIF.
  ENDIF.

  LOOP AT it_commented ASSIGNING <lv_commented>.
    IF strlen( <lv_commented> ) > 1 AND <lv_commented>(2) = '"*'.
      RETURN.
    ENDIF.
    IF strlen( <lv_commented> ) > 0 AND <lv_commented>(1) = '*'.
      RETURN.
    ENDIF.
    IF strlen( <lv_commented> ) > 4 AND <lv_commented>(5) = '-----'.
      RETURN.
    ENDIF.
    IF strlen( <lv_commented> ) > 4 AND <lv_commented>(5) = ' ===='.
      RETURN.
    ENDIF.
    IF strlen( <lv_commented> ) > 4 AND <lv_commented>(5) = '*****'.
      RETURN.
    ENDIF.
    IF lines( it_commented ) = 1 AND <lv_commented> CO '. '.
      RETURN.
    ENDIF.
  ENDLOOP.

  IF zcl_aoc_parser=>run( it_commented )-match = abap_true.
    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = is_level-name
            p_line         = iv_line
            p_kind         = mv_errty
            p_test         = myname
            p_code         = '001' ).
  ENDIF.

ENDMETHOD.
ENDCLASS.