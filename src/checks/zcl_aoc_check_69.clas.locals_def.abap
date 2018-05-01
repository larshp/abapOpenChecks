CLASS lcl_stack DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      push
        IMPORTING iv_string TYPE string,
      set
        IMPORTING iv_string TYPE string,
      concatenate
        IMPORTING iv_string        TYPE string OPTIONAL
        RETURNING VALUE(rv_string) TYPE string,
      clear,
      pop.

  PRIVATE SECTION.
    DATA: mt_data TYPE string_table.

ENDCLASS.
