CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aoc_super DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PRIVATE SECTION.

    METHODS:
      setup,
      export_import FOR TESTING,
      inform FOR TESTING,
      check_class_neg FOR TESTING,
      check_class_proxy FOR TESTING.

    DATA: mo_super TYPE REF TO zcl_aoc_super.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_super TYPE zcl_aoc_check_01.
* clear static attributes
    CLEAR mo_super->object_name.
    CLEAR mo_super->object_type.
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_super ).
  ENDMETHOD.

  METHOD check_class_neg.

    DATA: lv_skip TYPE abap_bool.

    lv_skip = mo_super->check_class( '' ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_skip
        exp = abap_false ).

  ENDMETHOD.

  METHOD check_class_proxy.

    DATA: lv_skip TYPE abap_bool.

    mo_super->object_type = 'CLAS'.
    mo_super->object_name = 'CL_SPRXT_RFC_PING'.
    lv_skip = mo_super->check_class( '' ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_skip
        exp = abap_true ).

  ENDMETHOD.

  METHOD inform.

    mo_super->object_type = 'FUGR'.
    mo_super->object_name = 'ZSOMETHING_AOC'.
    mo_super->inform(
      p_test         = 'ABC'
      p_code         = 'ABC' ).

    mo_super->inform(
      p_sub_obj_type = 'PROG'
      p_sub_obj_name = 'SAPLSETB'
      p_test         = 'ABC'
      p_code         = 'ABC' ).

  ENDMETHOD.

ENDCLASS.
