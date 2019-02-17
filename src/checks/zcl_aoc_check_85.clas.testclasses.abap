CLASS ltcl_temporary_program DEFINITION DEFERRED.

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_check   TYPE REF TO zcl_aoc_check_85.

    METHODS:
      setup,
      export_import FOR TESTING.

ENDCLASS.

CLASS ltcl_test_code DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS
  FINAL.

  PRIVATE SECTION.
    DATA:
      mt_code    TYPE string_table,
      ms_result  TYPE scirest_ad,
      mv_text    TYPE string,
      mo_program TYPE REF TO ltcl_temporary_program,
      mo_check   TYPE REF TO zcl_aoc_check_85.

    METHODS:
      setup,
      teardown,
      message_handler FOR EVENT message OF zcl_aoc_check_85 IMPORTING p_param_1,
      test_1 FOR TESTING,
      test_1_with_fixpt FOR TESTING,
      test_2 FOR TESTING,
      test_3 FOR TESTING,
      test_4 FOR TESTING,
      test_5 FOR TESTING,
      test_6 FOR TESTING,
      test_7 FOR TESTING,
      test_8 FOR TESTING.

ENDCLASS.

CLASS ltcl_temporary_program DEFINITION FOR TESTING FINAL.
  PUBLIC SECTION.
    TYPES:
      t_program_name TYPE c LENGTH 30.
    METHODS:
      constructor,
      cleanup,
      get_program_name RETURNING VALUE(rv_program_name) TYPE t_program_name,
      generate_programm IMPORTING it_source_code TYPE STANDARD TABLE iv_fixpt TYPE fixpt DEFAULT abap_false.

  PRIVATE SECTION.
    DATA:
      mv_program_generated TYPE abap_bool,
      mv_program_name      TYPE t_program_name.
ENDCLASS.

CLASS ltcl_temporary_program IMPLEMENTATION.
  METHOD constructor.
    DATA:
      lt_data TYPE STANDARD TABLE OF rbase.
    mv_program_name = 'YAOC_CHECK_85_TEST'.

    "find a free program name
    LOAD REPORT mv_program_name PART 'BASE' INTO lt_data.
    WHILE sy-subrc <> 4.
      IF sy-index > 10.
        CLEAR mv_program_name.
        cl_abap_unit_assert=>abort( 'Can not find a free program name to generate report for testing'  ).
        RETURN.
      ENDIF.
      mv_program_name = 'YAOC_CHECK_85_TEST' && '_' && sy-index.
      LOAD REPORT mv_program_name PART 'BASE' INTO lt_data.
    ENDWHILE.

  ENDMETHOD.

  METHOD get_program_name.
    rv_program_name = mv_program_name.
  ENDMETHOD.

  METHOD cleanup.
    IF mv_program_name IS NOT INITIAL AND mv_program_generated = abap_true.
      DELETE REPORT mv_program_name.
      mv_program_generated = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD generate_programm.
    IF mv_program_name IS NOT INITIAL.
      INSERT REPORT mv_program_name
        FROM it_source_code
        FIXED-POINT ARITHMETIC iv_fixpt.
      cl_abap_unit_assert=>assert_subrc( msg = 'Invalid test source code' ).
      mv_program_generated = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test_code IMPLEMENTATION.

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_program.
    CREATE OBJECT mo_check.
    SET HANDLER message_handler FOR mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR mv_text.
    mo_program->cleanup( ).
  ENDMETHOD.

  METHOD message_handler.
    mv_text = p_param_1.
  ENDMETHOD.

  METHOD test_1.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'gv_p = 12345.'.

    mo_program->generate_programm( mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'GV_P = 12345'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test_1_with_fixpt.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'gv_p = 12345.'.

    mo_program->generate_programm( it_source_code = mt_code iv_fixpt = abap_true ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test_2.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'FORM assignment.'.
    _code '  DATA gv_r TYPE p LENGTH 10 DECIMALS 3.'.
    _code '  gv_p = gv_r.'.
    _code 'ENDFORM.'.

    mo_program->generate_programm( mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test_3.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'FORM assignment.'.
    _code '  DATA gv_r TYPE p LENGTH 10 DECIMALS 4.'.
    _code '  gv_p = gv_r.'.
    _code 'ENDFORM.'.

    mo_program->generate_programm( mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'GV_P = GV_R'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test_4.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'gv_p = ''500.12''.'.

    mo_program->generate_programm( it_source_code = mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'GV_P = ''500.12'''
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test_5.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'gv_p = `500.12`.'.

    mo_program->generate_programm( it_source_code = mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'GV_P = `500.12`'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test_6.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'gv_p = |500.12|.'.

    mo_program->generate_programm( it_source_code = mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'GV_P = | `500.12` |'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test_7.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'DATA gv_i TYPE i.'.
    _code 'gv_p = gv_i.'.

    mo_program->generate_programm( mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'GV_P = GV_I'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test_8.
    _code 'REPORT.'.
    _code 'DATA gv_p TYPE p LENGTH 10 DECIMALS 3.'.
    _code 'DATA gv_i TYPE i.'.
    _code 'gv_i = gv_p.'.

    mo_program->generate_programm( mt_code ).

    ms_result = zcl_aoc_unit_test=>check_program( mo_program->get_program_name( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.
ENDCLASS.
