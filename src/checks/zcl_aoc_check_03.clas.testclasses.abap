*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
* ================

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_03.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'TRY.             '.
    _code '  WRITE ''foo''. '.
    _code 'ENDTRY.          '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'TRY.               '.
    _code '    WRITE ''foo''. '.
    _code '  CATCH cx_root.   '.
    _code '    WRITE ''bar''. '.
    _code 'ENDTRY.            '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'TRY.                 '.
    _code '  WRITE ''foo''.     '.
    _code '  TRY.               '.
    _code '      WRITE ''foo''. '.
    _code '    CATCH cx_root.   '.
    _code '      WRITE ''bar''. '.
    _code '  ENDTRY.            '.
    _code 'ENDTRY.              '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_04.
* ===========

    _code 'TRY.               '.
    _code '    WRITE ''foo''. '.
    _code '  CLEANUP.         '.
    _code '    WRITE ''bar''. '.
    _code 'ENDTRY.            '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test002_01.
* ===========

    _code 'TRY.'.
    _code '    WRITE: ''foo''.'.
    _code '    TRY.'.
    _code '        WRITE: ''bar''.'.
    _code '      CATCH zcx_error.'.
    _code '    ENDTRY.'.
    _code '  CATCH zcx_error.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_02.
* ===========

    _code 'TRY.'.
    _code '  CATCH cx_ai_system_fault cx_ai_application_fault INTO cl_exc.'.
    _code '  CATCH cx_soap_core INTO cl_exc..'.
    _code '  CATCH   zcx_fileinfo INTO cl_exc.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test002_03.
* ===========

    _code 'TRY.'.
    _code '    TRY.'.
    _code '      CATCH ycx_root.'.
    _code '    ENDTRY.'.
    _code '  CATCH ycx_root.'.
    _code '    WRITE: ''asdf''.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
