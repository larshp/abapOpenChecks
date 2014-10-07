*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

* todo, colon separator

  PRIVATE SECTION.
* ================

    METHODS: back            FOR TESTING,
             suppress_dialog FOR TESTING,
             suppress_foobar FOR TESTING,
             tables          FOR TESTING,
             tables_neg      FOR TESTING,
             divide          FOR TESTING,
             divide_neg      FOR TESTING,
             condense1       FOR TESTING,
             condense2       FOR TESTING,
             newline1        FOR TESTING,
             newline2        FOR TESTING,
             newline3        FOR TESTING,
             sort1           FOR TESTING,
             sort2           FOR TESTING,
             sort3           FOR TESTING,
             sort4           FOR TESTING,
             sort5           FOR TESTING,
             sort6           FOR TESTING,
             sort7           FOR TESTING,
             sort8           FOR TESTING,
             loop1           ,"FOR TESTING, " broken
             collect1        ,"FOR TESTING, " FIELD_OR_MCALL
             call_screen     ,"FOR TESTING, " broken
             data1           FOR TESTING,
             write1          FOR TESTING,
             write2          FOR TESTING,
             replace1        ,"FOR TESTING, " broken
             replace2        ."FOR TESTING. " broken

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
* ==============================

  METHOD back.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'BACK.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.       "parse_Str

  METHOD suppress_dialog.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SUPPRESS DIALOG.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "suppress_dialog

  METHOD suppress_foobar.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SUPPRESS FOOBAR.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = lv_match ).

  ENDMETHOD.                    "suppress_foobar

  METHOD tables.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'TABLES usr02.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "tables

  METHOD tables_neg.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

* colon and commas missing,
    _code 'TABLES usr02 usr01.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = lv_match ).

  ENDMETHOD.                    "tables_neg

  METHOD divide.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'DIVIDE lv_foo BY 2.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "divide

  METHOD divide_neg.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'DIVIDE lv_foo BAR 2.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = lv_match ).

  ENDMETHOD.                    "divide_neg

  METHOD condense1.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'CONDENSE lv_bar.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "condense1

  METHOD condense2.
* =================

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'CONDENSE lv_bar NO-GAPS.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "condense2

  METHOD newline1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'NEW-LINE.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "newline1

  METHOD newline2.

    DATA: lt_code  TYPE string_table,
        lv_match TYPE abap_bool.


    _code 'NEW-LINE SCROLLING.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "newline2

  METHOD newline3.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'NEW-LINE NO-SCROLLING.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "newline3

  METHOD sort1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "newline3

  METHOD sort2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo DESCENDING.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort2

  METHOD sort3.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo AS TEXT.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort3

  METHOD sort4.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo ASCENDING AS TEXT.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort4

  METHOD sort5.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo BY bar.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort5

  METHOD sort6.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo BY bar DESCENDING.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort6

  METHOD sort7.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo AS TEXT BY bar DESCENDING.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort7

  METHOD sort8.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo BY foo bar.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort8

  METHOD loop1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'LOOP AT lt_data.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "loop1

  METHOD collect1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'COLLECT ls_wa INTO lt_tab.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "collect1

  METHOD call_screen.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'CALL SCREEN 2000.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "call_screen

  METHOD data1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'DATA lv_bar TYPE c.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "data1

  METHOD write1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'WRITE ''fobar''.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "write1

  METHOD write2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'WRITE ''fobar'' TO lv_var.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "write2

  METHOD replace1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'REPLACE SECTION OFFSET off LENGTH len OF dobj WITH new IN BYTE MODE.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "replace1

  METHOD replace2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'REPLACE ALL OCCURRENCES OF pattern IN var WITH new.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "replace2

ENDCLASS.       "lcl_Test