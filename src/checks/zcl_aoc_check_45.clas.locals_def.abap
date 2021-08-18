CLASS lcl_supported DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS is_740sp02_supported
      RETURNING
        VALUE(rv_supported) TYPE abap_bool.

    CLASS-METHODS is_740sp08_supported
      RETURNING
        VALUE(rv_supported) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA gv_740sp02 TYPE abap_bool VALUE abap_undefined.
    CLASS-DATA gv_740sp08 TYPE abap_bool VALUE abap_undefined.

ENDCLASS.
