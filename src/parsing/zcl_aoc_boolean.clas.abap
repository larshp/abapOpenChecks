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
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RT_TOKENS) type STOKESX_TAB .
private section.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN IMPLEMENTATION.


  METHOD parse.

    DATA: lt_tokens TYPE stokesx_tab.

    lt_tokens = remove_method_calls( it_tokens ).

* todo:
* 2: real paren
* 3: AND / OR
* 4: compares

  ENDMETHOD.


  method REMOVE_METHOD_CALLS.



  endmethod.
ENDCLASS.
