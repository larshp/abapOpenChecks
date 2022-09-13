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
          mt_result TYPE STANDARD TABLE OF scirest_ad WITH DEFAULT KEY,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_40.

    METHODS:
      setup,
      message_handler FOR EVENT message OF zcl_aoc_check_40 IMPORTING p_code p_line,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test001_05 FOR TESTING,
      test001_06 FOR TESTING,
      test001_07 FOR TESTING,
      test001_08 FOR TESTING,
      test001_09 FOR TESTING,
      test001_10 FOR TESTING,
      test001_11 FOR TESTING,
      test001_12 FOR TESTING,
      test001_13 FOR TESTING,
      test001_14 FOR TESTING,
      test001_15 FOR TESTING,
      test001_16 FOR TESTING.

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
    SET HANDLER message_handler FOR mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD message_handler.
    DATA:
      ls_result LIKE LINE OF mt_result.

    ls_result-line     = p_line.
    ls_result-code     = p_code.
    APPEND ls_result TO mt_result.
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'WRITE: / ''Hello''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_02.
* ===========

    _code 'READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_03.
* ===========

    _code 'READ TABLE lt_table INDEX 2 INTO ls_table.'.
    _code 'IF sy-subrc <> 0.'.
    _code '  READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'ENDIF.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_04.
* ===========

    _code 'ASSIGN (''FOO-BAR'') TO <lg_data>.'.
    _code 'WRITE: / ''Hello''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_05.
* ===========

    _code 'ASSIGN COMPONENT lv_string OF STRUCTURE ls_foo TO <lg_data>.'.
    _code 'WRITE: / ''Hello''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_06.
* ===========

* first line should fail
    _code 'ASSIGN COMPONENT lv_string OF STRUCTURE ls_foo TO <lg_data>.'.
    _code 'ASSIGN COMPONENT lv_string OF STRUCTURE ls_foo TO <lg_data>.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_07.
* ===========

    _code 'IF sy-subrc <> 0.'.
    _code '  READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'ELSE.'.
    _code '  READ TABLE lt_table INDEX 2 INTO ls_table.'.
    _code 'ENDIF.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_08.
* ===========

    _code 'IF sy-subrc = 8.'.
    _code '  READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'ELSEIF sy-subrc = 4.'.
    _code '  READ TABLE lt_table INDEX 2 INTO ls_table.'.
    _code 'ELSE.'.
    _code '  READ TABLE lt_table INDEX 3 INTO ls_table.'.
    _code 'ENDIF.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_09.
* ===========

    _code 'IF sy-subrc = 8.'.
    _code '  READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code '  WRITE: / ''Hello''.'.
    _code 'ELSEIF sy-subrc = 4.'.
    _code '  READ TABLE lt_table INDEX 2 INTO ls_table.'.
    _code 'ELSE.'.
    _code '  READ TABLE lt_table INDEX 3 INTO ls_table.'.
    _code 'ENDIF.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_10.
* ===========

    _code 'IF sy-subrc = 8.'.
    _code '  READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'ELSEIF sy-subrc = 4.'.
    _code '  READ TABLE lt_table INDEX 2 INTO ls_table.'.
    _code '  WRITE: / ''Hello''.'.
    _code 'ELSE.'.
    _code '  READ TABLE lt_table INDEX 3 INTO ls_table.'.
    _code 'ENDIF.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_11.
* ===========

    _code 'IF sy-subrc = 8.'.
    _code '  READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'ELSEIF sy-subrc = 4.'.
    _code '  READ TABLE lt_table INDEX 2 INTO ls_table.'.
    _code 'ELSE.'.
    _code '  READ TABLE lt_table INDEX 3 INTO ls_table.'.
    _code '  WRITE: / ''Hello''.'.
    _code 'ENDIF.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_12.
* ===========

    _code 'CASE sy-subrc.'.
    _code '  WHEN 0.'.
    _code '    READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code '  WHEN OTHERS.'.
    _code '    ASSERT 0 = 1.'.
    _code 'ENDCASE.'.
    _code 'ASSERT sy-subrc = 0.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_13.
* ===========

    _code 'CASE sy-subrc.'.
    _code '  WHEN 0.'.
    _code '    READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code '  WHEN OTHERS.'.
    _code '    ASSERT sy-subrc = 0.'.
    _code 'ENDCASE.'.
    _code 'WRITE: / ''Hello''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_14.

* first two lines should fail
    _code 'ASSIGN COMPONENT lv_string OF STRUCTURE ls_foo TO <lg_data>.'.
    _code 'ASSIGN COMPONENT lv_string OF STRUCTURE ls_foo TO <lg_data>.'.
    _code 'WRITE: / ''Dummy statement''.'.

    zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( mt_result ) ).

    READ TABLE mt_result INTO ms_result INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = ms_result-line ).

    READ TABLE mt_result INTO ms_result INDEX 2.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = ms_result-line ).
  ENDMETHOD.

  METHOD test001_15.
    _code 'IF sy-subrc = 8.'.
    _code '  READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'ELSEIF sy-subrc = 4.'.
    _code '  READ TABLE lt_table INDEX 2 INTO ls_table.'.
    _code 'ELSE.'.
    _code '  READ TABLE lt_table INDEX 3 INTO ls_table.'.
    _code 'ENDIF.'.
    _code 'IF sy-subrc = 0.'.
    _code '  WRITE: / ''success''.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test001_16.
    _code 'READ TABLE lt_table INDEX 1 INTO ls_table.'.
    _code 'IF 0 = sy-subrc.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

ENDCLASS.       "lcl_Test
