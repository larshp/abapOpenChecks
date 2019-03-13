CLASS zcl_aoc_boolean_tokens DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !it_tokens TYPE stokesx_tab .
    METHODS eat
      IMPORTING
        !iv_count        TYPE i
      RETURNING
        VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens .
    METHODS find_end_paren
      IMPORTING
        !iv_start     TYPE i
      RETURNING
        VALUE(rv_end) TYPE i .
    METHODS find_end_square
      IMPORTING
        !iv_start     TYPE i
      RETURNING
        VALUE(rv_end) TYPE i .
    METHODS get_length
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS get_token
      IMPORTING
        !iv_index       TYPE i
      RETURNING
        VALUE(rs_token) TYPE stokesx .
    METHODS get_tokens
      RETURNING
        VALUE(rt_tokens) TYPE stokesx_tab .
    METHODS remove
      IMPORTING
        !iv_index        TYPE i
      RETURNING
        VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens .
    METHODS replace
      IMPORTING
        !iv_str          TYPE string
        !iv_start        TYPE i
        !iv_end          TYPE i OPTIONAL
      RETURNING
        VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens .
    METHODS set_tokens
      IMPORTING
        !it_tokens TYPE stokesx_tab .
    METHODS split
      IMPORTING
        !iv_start        TYPE i
        !iv_end          TYPE i DEFAULT 0
      RETURNING
        VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens .
    METHODS to_string
      RETURNING
        VALUE(rv_string) TYPE string .
  PROTECTED SECTION.

    DATA mt_tokens TYPE stokesx_tab .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN_TOKENS IMPLEMENTATION.


  METHOD constructor.

    mt_tokens = it_tokens.

  ENDMETHOD.


  METHOD eat.

    DATA: lt_tokens LIKE mt_tokens.

    lt_tokens = mt_tokens.
    DELETE lt_tokens FROM iv_count + 1.

    DELETE mt_tokens TO iv_count.

    CREATE OBJECT ro_tokens
      EXPORTING
        it_tokens = lt_tokens.

  ENDMETHOD.


  METHOD find_end_paren.

    DATA: ls_token LIKE LINE OF mt_tokens,
          lv_open  TYPE i,
          lv_close TYPE i,
          lv_index TYPE i,
          lv_count TYPE i.


    LOOP AT mt_tokens FROM iv_start INTO ls_token.
      lv_index = sy-tabix.

      FIND ALL OCCURRENCES OF '(' IN ls_token-str MATCH COUNT lv_open.
      lv_count = lv_count + lv_open.
      FIND ALL OCCURRENCES OF ')' IN ls_token-str MATCH COUNT lv_close.
      lv_count = lv_count - lv_close.

      IF lv_count = 0.
        rv_end = lv_index.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_end_square.

    DATA: ls_token LIKE LINE OF mt_tokens,
          lv_open  TYPE i,
          lv_close TYPE i,
          lv_index TYPE i,
          lv_count TYPE i.


    LOOP AT mt_tokens FROM iv_start INTO ls_token.
      lv_index = sy-tabix.

      FIND ALL OCCURRENCES OF '[' IN ls_token-str MATCH COUNT lv_open.
      lv_count = lv_count + lv_open.
      FIND ALL OCCURRENCES OF ']' IN ls_token-str MATCH COUNT lv_close.
      lv_count = lv_count - lv_close.

      IF lv_count = 0.
        rv_end = lv_index.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_length.

    rv_length = lines( mt_tokens ).

  ENDMETHOD.


  METHOD get_token.

    READ TABLE mt_tokens INDEX iv_index INTO rs_token.    "#EC CI_SUBRC

  ENDMETHOD.


  METHOD get_tokens.

    rt_tokens = mt_tokens.

  ENDMETHOD.


  METHOD remove.

    DELETE mt_tokens INDEX iv_index.
    ASSERT sy-subrc = 0.

    ro_tokens = me.

  ENDMETHOD.


  METHOD replace.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF mt_tokens.


    READ TABLE mt_tokens INDEX iv_start ASSIGNING <ls_token>.
    ASSERT sy-subrc = 0.

    <ls_token>-str = to_upper( iv_str ).

    IF iv_end > iv_start.
      DELETE mt_tokens FROM iv_start + 1 TO iv_end.
    ENDIF.

    ro_tokens = me.

  ENDMETHOD.


  METHOD set_tokens.

    mt_tokens = it_tokens.

  ENDMETHOD.


  METHOD split.

    DATA: lt_tokens LIKE mt_tokens.

    lt_tokens = mt_tokens.

    DELETE lt_tokens TO iv_start.

    IF iv_end > 0.
      DELETE lt_tokens FROM iv_end - iv_start + 1.
    ENDIF.

    CREATE OBJECT ro_tokens
      EXPORTING
        it_tokens = lt_tokens.

  ENDMETHOD.


  METHOD to_string.

    DATA: lv_row   TYPE i,
          ls_token LIKE LINE OF mt_tokens.


    LOOP AT mt_tokens INTO ls_token.
      IF lv_row IS INITIAL.
        lv_row = ls_token-row.
      ENDIF.
      IF lv_row <> ls_token-row.
        CONCATENATE rv_string ls_token-str INTO rv_string SEPARATED BY space.
        lv_row = ls_token-row.
      ELSEIF rv_string IS INITIAL.
        rv_string = ls_token-str.
      ELSE.
        CONCATENATE rv_string ls_token-str INTO rv_string SEPARATED BY space.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
