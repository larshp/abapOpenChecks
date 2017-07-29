class ZCL_AOC_BOOLEAN definition
  public
  create public .

public section.

  class-methods PARSE
    importing
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RO_NODE) type ref to ZCL_AOC_BOOLEAN_NODE .
protected section.

  class-methods REMOVE_METHOD_CALLS
    importing
      !IO_TOKENS type ref to ZCL_AOC_BOOLEAN_TOKENS .
  class-methods REMOVE_STRINGS
    importing
      !IO_TOKENS type ref to ZCL_AOC_BOOLEAN_TOKENS .
private section.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN IMPLEMENTATION.


  METHOD parse.

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    CREATE OBJECT lo_tokens EXPORTING it_tokens = it_tokens.

    remove_strings( lo_tokens ).
    remove_method_calls( lo_tokens ).

* charcter strings
* string templates
* remove arrows
* remove dashes

* todo:
* 2: real paren
* 3: AND / OR
* 4: compares

  ENDMETHOD.


  METHOD remove_method_calls.

    DATA: ls_token   TYPE stokesx,
          lv_end     TYPE i,
          lv_restart TYPE abap_bool,
          lv_index   TYPE i.


    DO.
      lv_restart = abap_false.
      LOOP AT io_tokens->get_tokens( ) INTO ls_token.
        lv_index = sy-tabix.

        FIND REGEX '^[\w>\-=]+\($' IN ls_token-str.
        IF sy-subrc = 0.
          lv_end = io_tokens->find_end_paren( lv_index ).

          io_tokens->replace(
            iv_str   = 'METHOD'
            iv_start = lv_index
            iv_end   = lv_end ).

          lv_restart = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_restart = abap_false.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD remove_strings.

    DATA: ls_token TYPE stokesx.


    LOOP AT io_tokens->get_tokens( ) INTO ls_token WHERE type = scan_token_type-literal.
      io_tokens->replace(
        iv_str   = 'str'
        iv_start = sy-tabix ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
