CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_08.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING,
      test003_01 FOR TESTING,
      test003_02 FOR TESTING,
      test004_01 FOR TESTING,
      test005_01 FOR TESTING,
      test005_02 FOR TESTING,
      test005_03 FOR TESTING,
      test006_01 FOR TESTING,
      test006_02 FOR TESTING,
      test007_01 FOR TESTING,
      test007_02 FOR TESTING,
      test007_03 FOR TESTING,
      test007_04 FOR TESTING,
      test008_01 FOR TESTING,
      test009_01 FOR TESTING,
      test010_01 FOR TESTING,
      test011_01 FOR TESTING,
      test012_01 FOR TESTING,
      test013_01 FOR TESTING,
      test014_01 FOR TESTING,
      test015_01 FOR TESTING,
      test016_01 FOR TESTING,
      test016_02 FOR TESTING,
      test017_01 FOR TESTING,
      test018_01 FOR TESTING,
      test019_01 FOR TESTING,
      test020_01 FOR TESTING,
      test021_01 FOR TESTING,
      test022_01 FOR TESTING,
      test022_02 FOR TESTING,
      test023_01 FOR TESTING,
      test024_01 FOR TESTING.

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

  METHOD test001_01.

    _code 'REFRESH lt_foobar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.

    _code 'CLEAR lt_foobar[].'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.

    _code 'REFRESH CONTROL something.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test3

  METHOD test002_01.

    _code 'IF iv_input IS REQUESTED. '.
    _code '  WRITE ''foo''.          '.
    _code 'ENDIF.                    '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test002_02.

    _code 'IF iv_input IS SUPPLIED.  '.
    _code '  WRITE ''foo''.          '.
    _code 'ENDIF.                    '.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test002_03.

    _code 'WRITE: / ''foo IS REQUESTED bar'''.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test003_01.

    _code 'LEAVE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test003_02.

    _code 'LEAVE LIST-PROCESSING.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test004_01.

    _code 'COMPUTE lv_foo = lv_bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '004'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test005_01.

    _code 'MOVE lv_foo TO lv_bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '005'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test005_02.

    _code 'MOVE-CORRESPONDING lv_foo TO lv_bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test005_03.

    _code 'MOVE EXACT is_status-installed_release TO lv_number.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test006_01.

    _code 'IF lv_foo >< lv_bar.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '006'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test006_02.

    _code 'result = SELECT field1'.
    _code 'FROM schema._SYS_BIC.view_name'.
    _code 'PLACEHOLDER.$$parameter$$ => :input;'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test007_01.

    _code 'IF lv_foo EQ lv_bar.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '007'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test007_02.

    _code 'IF lv_foo NE ''asf''.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '007'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test007_03.

    _code 'DATA eq TYPE i.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test007_04.

    _code 'DATA moo TYPE c LENGTH 2 VALUE ''EQ''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test008_01.

    _code 'DEMAND something.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '008'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test009_01.

    _code 'SUPPLY something.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '009'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test010_01.

    _code 'CONTEXTS something.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '010'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test011_01.

    _code 'ADD 2 TO lv_foo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '011'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test012_01.

    _code 'SUBTRACT 2 FROM lv_foo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '012'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test013_01.

    _code 'MULTIPLY lv_foo BY 2.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '013'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test014_01.

    _code 'DIVIDE lv_foo BY 4.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '014'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test015_01.

    _code 'CALL DIALOG something.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '015'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test016_01.

    _code 'DATA: lt_foo TYPE c OCCURS 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '016'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test016_02.

    _code 'DATA occurs TYPE c.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test017_01.

    _code 'DATA lt_foo TYPE TABLE OF c WITH HEADER LINE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '017'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test018_01.

    _code 'RANGES: lt_foo TYPE RANGE OF c.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '018'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test019_01.

    _code 'ADD-CORRESPONDING lt_foo TO lt_bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '019'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test020_01.

    _code 'SET EXTENDED CHECK OFF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '020'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test021_01.

    _code 'LOCAL lv_foo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '021'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test022_01.

    _code 'DO 1 TIMES.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '022'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test022_02.

    _code 'DO lv_count TIMES.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test023_01.

    _code 'DO 5 TIMES VARYING l_meinh FROM alt-uom1 NEXT alt-uom2.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '023'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test024_01.

    _code 'CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 4.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '024'
                                        act = ms_result-code ).

  ENDMETHOD.

ENDCLASS.
