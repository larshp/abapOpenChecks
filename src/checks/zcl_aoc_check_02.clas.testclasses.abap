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
          mo_check  TYPE REF TO zcl_aoc_check_02.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING,
      test002_04 FOR TESTING.
    METHODS test002_05 FOR TESTING.

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

    _code 'EXIT.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'LOOP AT lt_table INTO lv_structure.'.
    _code '  EXIT.                            '.
    _code 'ENDLOOP.                           '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'DO.      '.
    _code '  EXIT.  '.
    _code 'ENDDO.   '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'WHILE 1 = 2'.
    _code '  EXIT.    '.
    _code 'ENDWHILE.  '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

  METHOD test002_01.
* ===========

    _code 'cl_class=>method( ).'.
    _code 'CHECK 1 = 2.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
  ENDMETHOD.                    "test1

  METHOD test002_02.
* ===========

    _code 'LOOP AT lt_table INTO lv_structure.'.
    _code '  CHECK 1 = 2.                     '.
    _code 'ENDLOOP.                           '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test002_03.
* ===========

    _code 'DO.      '.
    _code '  CHECK 1 = 2.  '.
    _code 'ENDDO.   '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test002_04.
* ===========

    _code 'WHILE 1 = 2.'.
    _code '  CHECK 1 = 2.    '.
    _code 'ENDWHILE.  '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

  METHOD test002_05.
* ===========

	_code 'CLASS LCL_FOO DEFINITION.'.
	_code '  PUBLIC SECTION.'.
	_code '  METHODS test.'.
	_code 'ENDCLASS.'.
	_code 'CLASS LCL_FOO IMPLEMENTATION.'.
	_code 'METHOD test.'.
	_code '  TYPE-POOLS SCAN.'.
	_code '  TYPES:'.
	_code '  BEGIN OF ts_struct,'.
	_code '    date TYPE d,'.
	_code '  END OF ts_struct.'.
	_code '  CONSTANTS cv_value type string value `test`.'.
	_code '  STATICS lv_index type i.'.
	_code '  DATA ls_struct TYPE ts_struct'.
	_code '  DATA lt_table LIKE STANDARD TABLE OF LS_STRUCT'.
	_code '  FIELD-SYMBOLS <ls_data> TYPE any.'.
	_code '  TABLES t100.'.
	_code '  DEFINE macro.'.
	_code '    READ TABLE lt_table INTO &1 INDEX &2.'.
	_code '  END-OF-DEFINITION.'.
	_code '*******************************'.
	_code '" Test comment'.
	_code '  CLEAR lv_index.'.
	_code '  FREE lt_table.'.
	_code '  REFRESH lt_table.'.
	_code '  ASSERT 1 = 2.'.
	_code '  BREAK-POINT.'.
	_code '  BREAK-POINT ID Z.'.
	_code '  LOG-POINT ID Z.'.
	_code '  DESCRIBE FIELD lv_index LENGTH DATA(lv_length) IN BYTE MODE.'.
	_code '  GET TIME.'.
	_code '  INCLUDE zdummy IF FOUND.'.
	_code '  ASSIGN ls_struct TO <ls_data>.'.
	_code '  IF <ls_data> IS NOT ASSIGNED.'.
	_code '    RAISE EXCEPTION TYPE CX_STATIC_CHECK.'.
	_code '    LEAVE PROGRAM.'.
	_code '    RETURN.'.
	_code '  ENDIF.'.
	_code ''.
	_code '  CHECK <ls_data> IS ASSIGNED.'.
	_code ''.
	_code 'ENDMETHOD.'.
	_code 'ENDCLASS.'.
	
	ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.                    "test002_05

ENDCLASS.       "lcl_Test
