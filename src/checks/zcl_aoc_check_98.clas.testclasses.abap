CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aoc_check_98 DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_98.

    METHODS:
      setup,
      export_import FOR TESTING.

    METHODS:
      test000_01 FOR TESTING,
      test000_02 FOR TESTING,
      test000_03 FOR TESTING,
      test000_04 FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test002_01 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

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

  METHOD test000_01.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH ZCX_ABAPGIT_2FA_ERROR.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test000_02.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND '.
    _code '      ZCX_ABAPGIT_2FA_ERROR '.
    _code '      CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test000_03.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND '.
    _code '    ZCX_ABAPGIT_2FA_ERROR.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test000_04.
    " more then one try-catch
    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND '.
    _code '      ZCX_ABAPGIT_2FA_ERROR.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND '.
    _code '      ZCX_ABAPGIT_2FA_ERROR.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_01.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND.'.
    _code '  CATCH ZCX_ABAPGIT_2FA_ERROR.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_02.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH ZCX_ABAPGIT_2FA_ERROR.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_03.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND '.
    _code '    ZCX_ABAPGIT_2FA_ERROR.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_04.
    " more then one try-catch
    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND '.
    _code '    ZCX_ABAPGIT_2FA_ERROR.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    _code 'TRY.'.
    _code '    CHECK 1 = 2.'.
    _code '  CATCH CX_SY_ITAB_LINE_NOT_FOUND.'.
    _code '  CATCH ZCX_ABAPGIT_2FA_ERROR.'.
    _code '  CATCH CX_ROOT.'.
    _code 'ENDTRY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.


  METHOD test002_01.
    " no try/catch
    _code 'IF sy-datum = ''200720''.'.
    _code '  WRITE 1.'.
    _code 'ELSE.'.
    _code '  WRITE 2.'.
    _code 'ENDIF'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.
ENDCLASS.
