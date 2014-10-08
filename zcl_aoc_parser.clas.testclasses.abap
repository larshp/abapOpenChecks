*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

* todo, colon test, separator

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
             sort9           FOR TESTING,
             sort10          FOR TESTING,
             loop1           FOR TESTING,
             collect1        FOR TESTING,
             call_screen     FOR TESTING,
             call_method1    FOR TESTING,
             call_method2    FOR TESTING,
             call_method3    FOR TESTING,
             call_method4    FOR TESTING,
             call_method5    FOR TESTING,
             data1           ,"FOR TESTING, todo: IterationOption
             write1          FOR TESTING,
             write2          FOR TESTING,
             write3          FOR TESTING,
             replace1        FOR TESTING,
             replace2        FOR TESTING,
             compute1        FOR TESTING,
             compute2        FOR TESTING,
             if1             FOR TESTING,
             if2             FOR TESTING,
             wait1           FOR TESTING,
             wait2           FOR TESTING,
             code1           FOR TESTING,
             methods1        FOR TESTING,
             read_table1     FOR TESTING,
             call_function1  FOR TESTING,
             concatenate1    FOR TESTING,
             create_object1  FOR TESTING,
             create_object2  FOR TESTING.

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

  METHOD sort9.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo BY foo AS TEXT DESCENDING.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort9

  METHOD sort10.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'SORT lt_foo BY foo DESCENDING AS TEXT.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "sort10

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

  METHOD call_method1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'CALL METHOD cl_foo=>bar( ).'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "call_method

  METHOD call_method2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'CALL METHOD lo_foo->bar( ).'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "call_method2

  METHOD call_method3.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'CALL METHOD lo_foo->bar( 2 ).'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "call_method3

  METHOD call_method4.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'CALL METHOD lo_foo->bar( 2 + 2 ).'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "call_method4

  METHOD call_method5.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'io_xml->table_add( it_table = lt_men                '.
    _code '                   iv_name = ''RSMPE_MEN_TABLE'' ). '.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "call_method5

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

  METHOD write3.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'WRITE lv_var.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "write3

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


    _code 'REPLACE ALL OCCURRENCES OF SUBSTRING var IN var WITH new.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "replace2

  METHOD compute1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'COMPUTE lv_foo = lv_bar.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "compute1

  METHOD compute2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'lv_foo = lv_bar.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "compute2

  METHOD if1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'IF lv_foo = lv_bar.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "if1

  METHOD if2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.


    _code 'IF lv_foo = lv_bar AND lv_moo = lv_boo.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "if2

  METHOD wait1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'WAIT UNTIL lv_foo = lv_bar AND lv_moo = lv_boo.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "wait1

  METHOD wait2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'WAIT UP TO 1 SECONDS.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "wait2

  METHOD code1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'DO li_children->get_length( ) TIMES.'.
    _code 'DO get_length( ) TIMES.'.
    _code '  li_child = li_children->get_item( sy-index - 1 ).'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "code1

  METHOD methods1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'METHODS constructor IMPORTING iv_text TYPE string.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "methods1

  METHOD read_table1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'READ TABLE lt_lines INTO ls_line INDEX 1.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "read_table1

  METHOD call_function1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'CALL FUNCTION ''SAVE_TEXT'' '.
    _code '  EXPORTING                 '.
    _code '    header   = ls_header    '.
    _code '  TABLES                    '.
    _code '    lines    = lt_lines     '.
    _code '  EXCEPTIONS                '.
    _code '    id       = 1            '.
    _code '    language = 2            '.
    _code '    name     = 3            '.
    _code '    object   = 4            '.
    _code '    OTHERS   = 5.           '.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "call_function1

  METHOD concatenate1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'CONCATENATE ''FOOBAR'' sy-uname INTO lv_name.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "concatenate1

  METHOD create_object1.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'CREATE OBJECT lo_xml.'.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "create_object1

  METHOD create_object2.

    DATA: lt_code  TYPE string_table,
          lv_match TYPE abap_bool.

    _code 'CREATE OBJECT lo_source             '.
    _code '  EXPORTING                         '.
    _code '    clskey             = is_clskey  '.
    _code '  EXCEPTIONS                        '.
    _code '    class_not_existing = 1          '.
    _code '    OTHERS             = 2.         '.

    lv_match = zcl_aoc_parser=>parse_str( lt_code ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = lv_match ).

  ENDMETHOD.                    "create_object2

ENDCLASS.       "lcl_Test