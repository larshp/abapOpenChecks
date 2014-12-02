*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
    DURATION LONG
    RISK LEVEL HARMLESS
    FINAL.

* todo, colon test, separator
* todo, chained method call

  PRIVATE SECTION.
* ================

    DATA: mv_debug TYPE abap_bool VALUE abap_false,
          mt_code  TYPE string_table,
          ms_result TYPE zcl_aoc_parser=>st_result.

    METHODS: setup.

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
             call_method6    FOR TESTING,
             data1           FOR TESTING,
             write1          FOR TESTING,
             write2          FOR TESTING,
             write3          FOR TESTING,
             replace1        FOR TESTING,
             replace2        FOR TESTING,
             compute1        FOR TESTING,
             compute2        FOR TESTING,
             compute3        FOR TESTING,
             compute4        FOR TESTING,
             compute5        FOR TESTING,
             compute6        FOR TESTING,
             if1             FOR TESTING,
             if2             FOR TESTING,
             if3             FOR TESTING,
             if4             FOR TESTING,
             wait1           FOR TESTING,
             wait2           FOR TESTING,
             code1           FOR TESTING,
             code2           FOR TESTING,
             code3           FOR TESTING,
             methods1        FOR TESTING,
             read_table1     FOR TESTING,
             call_function1  FOR TESTING,
             concatenate1    FOR TESTING,
             create_object1  FOR TESTING,
             create_object2  FOR TESTING,
             non_code1       FOR TESTING,
             non_code2       FOR TESTING,
             non_code3       FOR TESTING,
             non_code4       FOR TESTING,
             non_code5       FOR TESTING,
             non_code6       FOR TESTING,
             non_code7       FOR TESTING,
             non_code8       FOR TESTING,
             non_code9       FOR TESTING,
             perform1        FOR TESTING,
             perform2        FOR TESTING,
             try1            FOR TESTING,
             try2            FOR TESTING,
             try3            FOR TESTING,
             select1         FOR TESTING,
             uline1          FOR TESTING,
             uline2          FOR TESTING,
             uline3          FOR TESTING,
             uline4          FOR TESTING,
             uline5          FOR TESTING,
             uline6          FOR TESTING,
             uline7          FOR TESTING,
             uline8          FOR TESTING,
             selection_scr1  FOR TESTING,
             selection_scr2  FOR TESTING,
             constant1       FOR TESTING,
             check1          FOR TESTING,
             check2          FOR TESTING,
             get1            FOR TESTING,
             field_symbol1   FOR TESTING,
             raise1          FOR TESTING,
             message1        FOR TESTING,
             message2        FOR TESTING,
             form1           FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
* ==============================

  METHOD setup.
    CLEAR mt_code.
  ENDMETHOD.                    "setup

  METHOD back.

    _code 'BACK.'.

    _test abap_true.

  ENDMETHOD.       "parse_Str

  METHOD suppress_dialog.

    _code 'SUPPRESS DIALOG.'.

    _test abap_true.

  ENDMETHOD.                    "suppress_dialog

  METHOD suppress_foobar.

    _code 'SUPPRESS FOOBAR.'.

    _test abap_false.

  ENDMETHOD.                    "suppress_foobar

  METHOD tables.

    _code 'TABLES usr02.'.

    _test abap_true.

  ENDMETHOD.                    "tables

  METHOD tables_neg.

* colon and commas missing,
    _code 'TABLES usr02 usr01.'.

    _test abap_false.

  ENDMETHOD.                    "tables_neg

  METHOD divide.

    _code 'DIVIDE lv_foo BY 2.'.

    _test abap_true.

  ENDMETHOD.                    "divide

  METHOD divide_neg.

    _code 'DIVIDE lv_foo BAR 2.'.

    _test abap_false.

  ENDMETHOD.                    "divide_neg

  METHOD condense1.

    _code 'CONDENSE lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "condense1

  METHOD condense2.

    _code 'CONDENSE lv_bar NO-GAPS.'.

    _test abap_true.

  ENDMETHOD.                    "condense2

  METHOD newline1.

    _code 'NEW-LINE.'.

    _test abap_true.

  ENDMETHOD.                    "newline1

  METHOD newline2.

    _code 'NEW-LINE SCROLLING.'.

    _test abap_true.

  ENDMETHOD.                    "newline2

  METHOD newline3.

    _code 'NEW-LINE NO-SCROLLING.'.

    _test abap_true.

  ENDMETHOD.                    "newline3

  METHOD sort1.

    _code 'SORT lt_foo.'.

    _test abap_true.

  ENDMETHOD.                    "newline3

  METHOD sort2.

    _code 'SORT lt_foo DESCENDING.'.

    _test abap_true.

  ENDMETHOD.                    "sort2

  METHOD sort3.

    _code 'SORT lt_foo AS TEXT.'.

    _test abap_true.

  ENDMETHOD.                    "sort3

  METHOD sort4.

    _code 'SORT lt_foo ASCENDING AS TEXT.'.

    _test abap_true.

  ENDMETHOD.                    "sort4

  METHOD sort5.

    _code 'SORT lt_foo BY bar.'.

    _test abap_true.

  ENDMETHOD.                    "sort5

  METHOD sort6.

    _code 'SORT lt_foo BY bar DESCENDING.'.

    _test abap_true.

  ENDMETHOD.                    "sort6

  METHOD sort7.

    _code 'SORT lt_foo AS TEXT BY bar DESCENDING.'.

    _test abap_true.

  ENDMETHOD.                    "sort7

  METHOD sort8.

    _code 'SORT lt_foo BY foo bar.'.

    _test abap_true.

  ENDMETHOD.                    "sort8

  METHOD sort9.

    _code 'SORT lt_foo BY foo AS TEXT DESCENDING.'.

    _test abap_true.

  ENDMETHOD.                    "sort9

  METHOD sort10.

    _code 'SORT lt_foo BY foo DESCENDING AS TEXT.'.

    _test abap_true.

  ENDMETHOD.                    "sort10

  METHOD loop1.

    _code 'LOOP AT lt_data.'.

    _test abap_true.

  ENDMETHOD.                    "loop1

  METHOD collect1.

    _code 'COLLECT ls_wa INTO lt_tab.'.

    _test abap_true.

  ENDMETHOD.                    "collect1

  METHOD call_screen.

    _code 'CALL SCREEN 2000.'.

    _test abap_true.

  ENDMETHOD.                    "call_screen

  METHOD call_method1.

    _code 'CALL METHOD cl_foo=>bar( ).'.

    _test abap_true.

  ENDMETHOD.                    "call_method

  METHOD call_method2.

    _code 'CALL METHOD lo_foo->bar( ).'.

    _test abap_true.

  ENDMETHOD.                    "call_method2

  METHOD call_method3.

    _code 'CALL METHOD lo_foo->bar( 2 ).'.

    _test abap_true.

  ENDMETHOD.                    "call_method3

  METHOD call_method4.

    _code 'CALL METHOD lo_foo->bar( 2 + 2 ).'.

    _test abap_true.

  ENDMETHOD.                    "call_method4

  METHOD call_method5.

    _code 'io_xml->table_add( it_table = lt_men                '.
    _code '                   iv_name = ''RSMPE_MEN_TABLE'' ). '.

    _test abap_true.

  ENDMETHOD.                    "call_method5

  METHOD call_method6.

    _code 'go_grid->check_changed_data( ).'.

    _test abap_true.

  ENDMETHOD.                    "call_method6

  METHOD data1.

    _code 'DATA lv_bar TYPE c.'.

    _test abap_true.

  ENDMETHOD.                    "data1

  METHOD write1.

    _code 'WRITE ''fobar''.'.

    _test abap_true.

  ENDMETHOD.                    "write1

  METHOD write2.

    _code 'WRITE ''fobar'' TO lv_var.'.

    _test abap_true.

  ENDMETHOD.                    "write2

  METHOD write3.

    _code 'WRITE lv_var.'.

    _test abap_true.

  ENDMETHOD.                    "write3

  METHOD replace1.

    _code 'REPLACE SECTION OFFSET off LENGTH len OF dobj WITH new IN BYTE MODE.'.

    _test abap_true.

  ENDMETHOD.                    "replace1

  METHOD replace2.

    _code 'REPLACE ALL OCCURRENCES OF SUBSTRING var IN var WITH new.'.

    _test abap_true.

  ENDMETHOD.                    "replace2

  METHOD compute1.

    _code 'COMPUTE lv_foo = lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "compute1

  METHOD compute2.

    _code 'lv_foo = lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "compute2

  METHOD compute3.

    _code '<lv_foo> = lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "compute3

  METHOD compute4.

    _code 'lv_bar = <lv_foo>.'.

    _test abap_true.

  ENDMETHOD.                    "compute4

  METHOD compute5.

    _code '<lv_foo>-moo = lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "compute5

  METHOD compute6.

    _code 'lv_bar = <lv_foo>-moo.'.

    _test abap_true.

  ENDMETHOD.                    "compute6

  METHOD if1.

    _code 'IF lv_foo = lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "if1

  METHOD if2.

    _code 'IF lv_foo = lv_bar AND lv_moo = lv_boo.'.

    _test abap_true.

  ENDMETHOD.                    "if2

  METHOD if3.

    _code 'IF gt_table[] IS NOT INITIAL.  '.
    _code '  PERFORM something.           '.
    _code 'ENDIF.                         '.

    _test abap_true.

  ENDMETHOD.                    "if3

  METHOD if4.

    _code 'IF sy-subrc <> 0.'.

    _test abap_true.

  ENDMETHOD.                    "if4

  METHOD wait1.

    _code 'WAIT UNTIL lv_foo = lv_bar AND lv_moo = lv_boo.'.

    _test abap_true.

  ENDMETHOD.                    "wait1

  METHOD wait2.

    _code 'WAIT UP TO 1 SECONDS.'.

    _test abap_true.

  ENDMETHOD.                    "wait2

  METHOD code1.

    _code 'DO li_children->get_length( ) TIMES.'.
    _code 'DO get_length( ) TIMES.'.
    _code '  li_child = li_children->get_item( sy-index - 1 ).'.

    _test abap_true.

  ENDMETHOD.                    "code1

  METHOD code2.

    _code 'CLASS lcl_node IMPLEMENTATION.'.
    _code '  METHOD constructor.'.
    _code '    ASSERT NOT iv_value IS INITIAL.'.
    _code '    mv_type  = iv_type.'.
    _code '    mv_value = iv_value.'.
    _code '    mv_key   = gv_key.'.
    _code '    gv_key   = gv_key + 1.'.
    _code '  ENDMETHOD.'.
    _code 'ENDCLASS.'.

    _test abap_true.

  ENDMETHOD.                    "code2

  METHOD code3.

    _code 'DO iv_count TIMES.'.
    _code 'lt_new = lt_perm.'.
    _code 'lv_value = sy-index.'.
    _code 'READ TABLE lt_new FROM lv_value TRANSPORTING NO FIELDS.'.
    _code 'APPEND lv_value TO lt_new.'.
    _code 'APPEND lt_new TO et_perm.'.
    _code 'ENDDO.'.

    _test abap_true.

  ENDMETHOD.                    "code3

  METHOD methods1.

    _code 'METHODS constructor IMPORTING iv_text TYPE string.'.

    _test abap_true.

  ENDMETHOD.                    "methods1

  METHOD read_table1.

    _code 'READ TABLE lt_lines INTO ls_line INDEX 1.'.

    _test abap_true.

  ENDMETHOD.                    "read_table1

  METHOD call_function1.

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

    _test abap_true.

  ENDMETHOD.                    "call_function1

  METHOD concatenate1.

    _code 'CONCATENATE ''FOOBAR'' sy-uname INTO lv_name.'.

    _test abap_true.

  ENDMETHOD.                    "concatenate1

  METHOD create_object1.

    _code 'CREATE OBJECT lo_xml.'.

    _test abap_true.

  ENDMETHOD.                    "create_object1

  METHOD create_object2.

    _code 'CREATE OBJECT lo_source             '.
    _code '  EXPORTING                         '.
    _code '    clskey             = is_clskey  '.
    _code '  EXCEPTIONS                        '.
    _code '    class_not_existing = 1          '.
    _code '    OTHERS             = 2.         '.

    _test abap_true.

  ENDMETHOD.                    "create_object2

  METHOD non_code1.

    _code 'update loop foobar.'.

    _test abap_false.

  ENDMETHOD.                    "non_code1

  METHOD non_code2.

    _code 'missing.'.

    _test abap_false.

  ENDMETHOD.                    "non_code2

  METHOD non_code3.

    _code 'item.'.

    _test abap_false.

  ENDMETHOD.                    "non_code3

  METHOD non_code4.

    _code '11.111.'.

    _test abap_false.

  ENDMETHOD.                    "non_code4

  METHOD non_code5.

    _code 'expected.'.

    _test abap_false.

  ENDMETHOD.                    "non_code5

  METHOD non_code6.

    _code 'IF something is something then do.'.

    _test abap_false.

  ENDMETHOD.                    "non_code6

  METHOD non_code7.

    _code 'attachment.'.

    _test abap_false.

  ENDMETHOD.                    "non_code7

  METHOD non_code8.

    _code '12345679.'.

    _test abap_false.

  ENDMETHOD.                    "non_code8

  METHOD non_code9.

    _code 'Call 911.'.

    _test abap_false.

  ENDMETHOD.                    "non_code9

  METHOD perform1.

    _code 'PERFORM something.'.

    _test abap_true.

  ENDMETHOD.                    "perform1

  METHOD perform2.

    _code 'PERFORM something USING lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "perform2

  METHOD try1.

    _code 'TRY.'.
    _code '  lv_foo = lv_bar.'.
    _code 'ENDTRY.'.

    _test abap_true.

  ENDMETHOD.                    "try1

  METHOD try2.

    _code 'TRY.'.
    _code '    lv_foo = lv_bar.'.
    _code '  CATCH cx_root.'.
    _code 'ENDTRY.'.

    _test abap_true.

  ENDMETHOD.                    "try2

  METHOD try3.

    _code 'TRY.'.
    _code '    lv_foo = lv_bar.'.
    _code '  CATCH cx_root INTO lx_root.'.
    _code 'ENDTRY.'.

    _test abap_true.

  ENDMETHOD.                    "try3

  METHOD select1.

    _code 'SELECT * FROM ssyntaxstructure INTO TABLE gt_syntax.'.

    _test abap_true.

  ENDMETHOD.                    "try1

  METHOD uline1.

    _code 'ULINE.'.

    _test abap_true.

  ENDMETHOD.                    "uline1

  METHOD uline2.

    _code 'ULINE AT /10(10).'.

    _test abap_true.

  ENDMETHOD.                    "uline1

  METHOD uline3.

    _code 'ULINE AT 10(10).'.

    _test abap_true.

  ENDMETHOD.                    "uline1

  METHOD uline4.

    _code 'ULINE NO-GAP.'.

    _test abap_true.

  ENDMETHOD.                    "uline1

  METHOD uline5.

    _code 'ULINE AT /10(10) NO-GAP.'.

    _test abap_true.

  ENDMETHOD.                    "uline1

  METHOD uline6.

    _code 'ULINE AT 10(10) NO-GAP.'.

    _test abap_true.

  ENDMETHOD.                    "uline1

  METHOD uline7.

    _code 'ULINE AT 10 NO-GAP.'.

    _test abap_true.

  ENDMETHOD.                    "uline7

  METHOD uline8.

    _code 'ULINE AT (10) NO-GAP.'.

    _test abap_true.

  ENDMETHOD.                    "uline8

  METHOD selection_scr1.

    _code 'SELECTION-SCREEN FUNCTION KEY 1.'.

    _test abap_true.

  ENDMETHOD.                    "selection1

  METHOD selection_scr2.

    _code 'SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.'.

    _test abap_true.

  ENDMETHOD.                    "selection_scr2

  METHOD constant1.

    _code 'CONSTANTS gc_var TYPE i VALUE 7.'.

    _test abap_true.

  ENDMETHOD.                    "constant1

  METHOD check1.

    _code 'CHECK lv_a = lv_b.'.

    _test abap_true.

  ENDMETHOD.                    "check1

  METHOD check2.

    _code 'CHECK SELECT-OPTIONS. '.

    _test abap_true.

  ENDMETHOD.                    "check2

  METHOD get1.

    _code 'GET BIT 1 OF lv_var INTO lv_res.'.

    _test abap_true.

  ENDMETHOD.                    "get1

  METHOD field_symbol1.

    _code 'FIELD-SYMBOLS: <lv_var> TYPE abap_bool.'.

    _test abap_true.

  ENDMETHOD.                    "field_symbol1

  METHOD raise1.

    _code 'RAISE EXCEPTION TYPE cx_root.'.

    _test abap_true.

  ENDMETHOD.                    "raise1

  METHOD message1.

    _code 'MESSAGE A500 WITH ''foobar''.'.

    _test abap_true.

  ENDMETHOD.                    "message1

  METHOD message2.

    _code 'MESSAGE s111(zzz) WITH <ls_foo>-progname.'.

    _test abap_true.

  ENDMETHOD.                    "message2

  METHOD form1.

    _code 'FORM foobar USING pv_char type c.'.

    _test abap_true.

  ENDMETHOD.                    "form1

ENDCLASS.       "lcl_Test