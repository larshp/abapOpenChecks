class ZCL_AOC_BOOLEAN_TOKENS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_TOKENS type STOKESX_TAB .
  methods GET .
  methods REMOVE
    importing
      !IV_INDEX type I .
  methods REPLACE
    importing
      !IV_STR type STRING
      !IV_INDEX type I .
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


  method GET.
  endmethod.


  METHOD REMOVE.

* todo

  ENDMETHOD.


  METHOD replace.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF mt_tokens.


    READ TABLE mt_tokens INDEX iv_index ASSIGNING <ls_token>.
    ASSERT sy-subrc = 0.

    <ls_token>-str = to_upper( iv_str ).

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
