CLASS zcl_aoc_util_reg_atc_namespace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACE if_satc_namespace_access LOAD .

    CLASS-METHODS is_registered_fugr_uxx
      IMPORTING
        !iv_sub_obj_name TYPE sobj_name
      RETURNING
        VALUE(rv_bool)   TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_UTIL_REG_ATC_NAMESPACE IMPLEMENTATION.


  METHOD is_registered_fugr_uxx.

    DATA: lt_namespaces TYPE STANDARD TABLE OF satc_ac_namespcs.

    FIELD-SYMBOLS: <ls_namespace> LIKE LINE OF lt_namespaces.


    rv_bool = abap_false.

    SELECT * FROM satc_ac_namespcs INTO TABLE lt_namespaces.

    LOOP AT lt_namespaces ASSIGNING <ls_namespace>.
      IF iv_sub_obj_name CP |{ <ls_namespace>-namespace }L*UXX|.
        rv_bool = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
