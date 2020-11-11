CLASS zcl_aoc_util_reg_atc_namespace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACE if_satc_namespace_access LOAD .

    TYPES:
      BEGIN OF ty_ns_object,
             namespace TYPE string,
             object    TYPE string,
           END OF ty_ns_object .

    TYPE-POOLS abap .
    CLASS-METHODS is_registered_fugr_uxx
      IMPORTING
      !iv_sub_obj_name TYPE sobj_name
      RETURNING
      VALUE(rv_bool) TYPE abap_bool .
    CLASS-METHODS is_in_namespace
      IMPORTING
      !iv_pgmid TYPE pgmid
      !iv_object TYPE trobjtype
      !iv_obj_name TYPE csequence
      RETURNING
      VALUE(rv_bool) TYPE abap_bool .
    CLASS-METHODS split_ns_object
      IMPORTING
      !iv_pgmid TYPE pgmid
      !iv_object TYPE trobjtype
      !iv_obj_name TYPE csequence
      RETURNING
      VALUE(rs_ns_object) TYPE ty_ns_object .
    CLASS-METHODS is_append_obj_in_ns
      IMPORTING
      !iv_sub_obj_name TYPE sobj_name
      RETURNING
      VALUE(rv_bool) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_util_reg_atc_namespace IMPLEMENTATION.


  METHOD is_append_obj_in_ns.

    DATA: lt_namespaces TYPE STANDARD TABLE OF satc_ac_namespcs.

    FIELD-SYMBOLS: <ls_namespace> LIKE LINE OF lt_namespaces.


    rv_bool = abap_false.

    SELECT * FROM satc_ac_namespcs INTO TABLE lt_namespaces.

    LOOP AT lt_namespaces ASSIGNING <ls_namespace>.
      IF iv_sub_obj_name CP |{ <ls_namespace>-namespace }*|.
        rv_bool = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_in_namespace.

    DATA: lv_obj_name  TYPE sobj_name,
          lv_namespace TYPE namespace.

    lv_obj_name = iv_obj_name.

    CALL FUNCTION 'TRINT_GET_NAMESPACE'
      EXPORTING
        iv_pgmid            = iv_pgmid
        iv_object           = iv_object
        iv_obj_name         = lv_obj_name
      IMPORTING
        ev_namespace        = lv_namespace
      EXCEPTIONS
        invalid_prefix      = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lv_namespace IS INITIAL OR lv_namespace = '/0CUST/' OR lv_namespace = '/0SAP/'.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.


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


  METHOD split_ns_object.

    DATA: lv_obj_name  TYPE sobj_name,
          lv_namespace TYPE namespace,
          lv_len_ns    TYPE i.

    lv_obj_name = iv_obj_name.

    CALL FUNCTION 'TRINT_GET_NAMESPACE'
      EXPORTING
        iv_pgmid            = iv_pgmid
        iv_object           = iv_object
        iv_obj_name         = lv_obj_name
      IMPORTING
        ev_namespace        = lv_namespace
      EXCEPTIONS
        invalid_prefix      = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0 OR lv_namespace = '/0CUST/' OR lv_namespace = '/0SAP/'.
      RETURN.
    ENDIF.

    lv_len_ns = strlen( lv_namespace ).

    rs_ns_object-namespace = lv_namespace.
    rs_ns_object-object    = iv_obj_name+lv_len_ns.

  ENDMETHOD.
ENDCLASS.