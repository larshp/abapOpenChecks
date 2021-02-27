CLASS zcl_aoc_compiler DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      IMPORTING
        !iv_object_type    TYPE trobjtype
        !iv_object_name    TYPE sobj_name
      RETURNING
        VALUE(ro_compiler) TYPE REF TO zcl_aoc_compiler .
    METHODS get_result
      RETURNING
        VALUE(rt_result) TYPE scr_refs .
    METHODS has_error
      RETURNING
        VALUE(rv_error) TYPE abap_bool .
    METHODS constructor
      IMPORTING
        !it_result TYPE scr_refs
        !iv_error  TYPE abap_bool .
  PROTECTED SECTION.

    CLASS-DATA gt_cache_result TYPE scr_refs .
    CLASS-DATA gv_cache_program TYPE program .
    CLASS-DATA gv_cache_error TYPE abap_bool .
    DATA mt_result TYPE scr_refs .
    DATA mv_error TYPE abap_bool .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_COMPILER IMPLEMENTATION.


  METHOD constructor.
    mt_result = it_result.
    mv_error = iv_error.
  ENDMETHOD.


  METHOD get_instance.

    DATA: lv_class    TYPE seoclsname,
          lo_compiler TYPE REF TO cl_abap_compiler,
          lv_subrc    TYPE sy-subrc,
          lv_name     TYPE program.


    CASE iv_object_type.
      WHEN 'PROG'.
        lv_name = iv_object_name.
      WHEN 'CLAS'.
        lv_class = iv_object_name.
        lv_name = cl_oo_classname_service=>get_classpool_name( lv_class ).
      WHEN 'FUGR'.
        CONCATENATE 'SAPL' iv_object_name INTO lv_name.
      WHEN OTHERS.
        CONCATENATE iv_object_type iv_object_name INTO lv_name.
    ENDCASE.

    IF lv_name <> gv_cache_program.
      lo_compiler = cl_abap_compiler=>create(
        p_name             = lv_name
        p_no_package_check = abap_true ).
      CLEAR gt_cache_result.
      IF lo_compiler IS INITIAL.
        gv_cache_error = abap_true.
      ELSE.
        lo_compiler->get_all(
          IMPORTING
            p_result = gt_cache_result ).
        lo_compiler->get_check_infos( IMPORTING p_subrc = lv_subrc ).
        gv_cache_error = boolc( lv_subrc <> 0 ).
      ENDIF.
      gv_cache_program = lv_name.
    ENDIF.

    CREATE OBJECT ro_compiler
      EXPORTING
        it_result = gt_cache_result
        iv_error  = gv_cache_error.

  ENDMETHOD.


  METHOD get_result.
    rt_result = mt_result.
  ENDMETHOD.


  METHOD has_error.
    rv_error = mv_error.
  ENDMETHOD.
ENDCLASS.
