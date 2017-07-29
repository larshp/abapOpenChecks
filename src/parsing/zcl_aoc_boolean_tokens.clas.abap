class ZCL_AOC_BOOLEAN_TOKENS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_TOKENS type STOKESX_TAB .
  methods FIND_END_PAREN
    importing
      !IV_START type I
    returning
      value(RV_END) type I .
  methods GET_TOKENS
    returning
      value(RT_TOKENS) type STOKESX_TAB .
  methods REMOVE
    importing
      !IV_INDEX type I
    returning
      value(RO_TOKENS) type ref to ZCL_AOC_BOOLEAN_TOKENS .
  methods REPLACE
    importing
      !IV_STR type STRING
      !IV_START type I
      !IV_END type I optional
    returning
      value(RO_TOKENS) type ref to ZCL_AOC_BOOLEAN_TOKENS .
  methods TO_STRING
    returning
      value(RV_STRING) type STRING .
protected section.

  data MT_TOKENS type STOKESX_TAB .
private section.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN_TOKENS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    mt_tokens = it_tokens.

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


  METHOD to_string.

    DATA: lv_row      TYPE i,
          ls_token    LIKE LINE OF mt_tokens.


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
