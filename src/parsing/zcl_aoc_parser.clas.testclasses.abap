*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test_statements DEFINITION FOR TESTING
    DURATION LONG
    RISK LEVEL HARMLESS
    FINAL.

* todo, colon test, separator

  PRIVATE SECTION.
* ================

    DATA: mv_debug  TYPE abap_bool VALUE abap_false,
          mt_code   TYPE string_table,
          ms_result TYPE zcl_aoc_parser=>ty_result.

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
      sort11          FOR TESTING,
      loop1           FOR TESTING,
      loop2           FOR TESTING,
      collect1        FOR TESTING,
      call_screen     FOR TESTING,
      call_method1    FOR TESTING,
      call_method2    FOR TESTING,
      call_method3    FOR TESTING,
      call_method4    FOR TESTING,
      call_method5    FOR TESTING,
      call_method6    FOR TESTING,
      call_method7    FOR TESTING,
      call_method8    FOR TESTING,
      data1           FOR TESTING,
      data2           FOR TESTING,
      data3           FOR TESTING,
      data4           FOR TESTING,
      data5           FOR TESTING,
      data6           FOR TESTING,
      data7           FOR TESTING,
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
      compute7        FOR TESTING,
      compute8        FOR TESTING,
      compute9        FOR TESTING,
      compute10       FOR TESTING,
      compute11       FOR TESTING,
      compute12       FOR TESTING,
      compute13       FOR TESTING,
      compute14       FOR TESTING,
      compute15       FOR TESTING,
      compute16       FOR TESTING,
      compute17       FOR TESTING,
      compute18       FOR TESTING,
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
      methods2        FOR TESTING,
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
      non_code10      FOR TESTING,
      perform1        FOR TESTING,
      perform2        FOR TESTING,
      try1            FOR TESTING,
      try2            FOR TESTING,
      try3            FOR TESTING,
      select1         FOR TESTING,
      select2         FOR TESTING,
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
      field_symbol2   FOR TESTING,
      raise1          FOR TESTING,
      message1        FOR TESTING,
      message2        FOR TESTING,
      message3        FOR TESTING,
      message4        FOR TESTING,
      form1           FOR TESTING,
      case1           FOR TESTING,
      comment1        FOR TESTING,
      comment2        FOR TESTING,
      set1            FOR TESTING,
      set2            FOR TESTING,
      set3            FOR TESTING,
      set4            FOR TESTING,
      clear1          FOR TESTING,
      append1         FOR TESTING,
      append2         FOR TESTING,
      start1          FOR TESTING,
      modify1         FOR TESTING,
      modify2         FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test_statements IMPLEMENTATION.
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

  METHOD sort11.

    _code 'SORT lt_tab BY (lv_name).'.

    _test abap_true.

  ENDMETHOD.

  METHOD loop1.

    _code 'LOOP AT lt_data.'.

    _test abap_true.

  ENDMETHOD.                    "loop1

  METHOD loop2.

    _code 'LOOP AT li_foo~mt_tab ASSIGNING <ls_var>.'.

    _test abap_true.

  ENDMETHOD.                    "loop2

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

  METHOD call_method7.

    _code 'CALL METHOD CL_GUI_CFW=>DISPATCH.'.

    _test abap_true.

  ENDMETHOD.                    "call_method7

  METHOD call_method8.

    _code 'CALL METHOD cl_abap_conv_in_ce=>uccp'.
    _code '  EXPORTING'.
    _code '    uccp = ''0000'''.
    _code '  RECEIVING'.
    _code '    char = lt_table(1).'.

    _test abap_true.

  ENDMETHOD.                    "call_method8

  METHOD data1.

    _code 'DATA lv_bar TYPE c.'.

    _test abap_true.

  ENDMETHOD.                    "data1

  METHOD data2.

    _code 'DATA: foo(4).'.

    _test abap_true.

  ENDMETHOD.                    "data2

  METHOD data3.

    _code 'DATA object LIKE tadir-object.'.

    _test abap_true.

  ENDMETHOD.                    "data3

  METHOD data4.

    _code 'DATA: foobar.'.

    _test abap_true.

  ENDMETHOD.                    "data4

  METHOD data5.

    _code 'DATA BEGIN OF foo OCCURS 0.'.

    _test abap_true.

  ENDMETHOD.                    "data5

  METHOD data6.

    _code 'DATA ls_bar TYPE zcl_foo=>st_type.'.

    _test abap_true.

  ENDMETHOD.                    "data6

  METHOD data7.

    _code 'DATA: ls_moo TYPE zcl_foo=>st_type,'.
    _code '      ls_boo TYPE zcl_foo=>st_type.'.

    _test abap_true.

  ENDMETHOD.

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

  METHOD compute7.

    _code 'lv_foo = zcl_bar=>c_value.'.

    _test abap_true.

  ENDMETHOD.                    "compute7

  METHOD compute8.

    _code 'lv_foo = ''text symbol''(001).'.

    _test abap_true.

  ENDMETHOD.                    "compute8

  METHOD compute9.

    _code 'lv_foo = lo_ref->if_erface~var.'.

    _test abap_true.

  ENDMETHOD.                    "compute9

  METHOD compute10.

    _code 'lv_var = zcl_class=>method( )-field.'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute11.

    _code 'lv_var = zcl_class=>method( )->method( ).'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute12.

    _code 'lv_var = zcl_class=>method( )->method( )-field.'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute13.

    _code 'lv_var = zcl_class=>method( )->method( )-field=>method( ).'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute14.

    _code 'lv_var = zcl_class=>method( lv_foo )-field.'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute15.

    _code 'lv_var = zcl_class=>method( )->method( lv_foo ).'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute16.

    _code 'lv_var = zcl_class=>method( )->method( lv_foo )-field.'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute17.

    _code 'lv_var = zcl_class=>method( )->method( )-field=>method( lv_foo ).'.

    _test abap_true.

  ENDMETHOD.

  METHOD compute18.

    _code 'lv_var = <ls_foo>-ref->get_all( ).'.

    _test abap_true.

  ENDMETHOD.

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

  METHOD methods2.

    _code 'METHOD foo~bar.'.

    _test abap_true.

  ENDMETHOD.                    "methods2

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

    _code 'Call 911 for something.'.

    _test abap_false.

  ENDMETHOD.                    "non_code9

  METHOD non_code10.

    _code 'Call 11.111 as foo'.

    _test abap_false.

  ENDMETHOD.

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

  METHOD select2.

    _code 'SELECT SINGLE * FROM trdir WHERE name = lv_name.'.

    _test abap_true.

  ENDMETHOD.

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

  METHOD field_symbol2.

    _code 'FIELD-SYMBOLS: <lv_var> LIKE LINE OF li_foo~mt_bar.'.

    _test abap_true.

  ENDMETHOD.                    "field_symbol2

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

  METHOD message3.

    _code 'MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno'.
    _code '  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.'.

    _test abap_true.

  ENDMETHOD.                    "message3

  METHOD message4.

    _code 'MESSAGE I001.'.

    _test abap_true.

  ENDMETHOD.                    "message4

  METHOD form1.

    _code 'FORM foobar USING pv_char type c.'.

    _test abap_true.

  ENDMETHOD.                    "form1

  METHOD case1.

    _code 'CASE lv_foobar.'.
    _code '  WHEN ''moo''.'.
    _code 'ENDCASE.'.

    _test abap_true.

  ENDMETHOD.                    "case1

  METHOD comment1.

    _code '* lorem ipsum foo'.
    _code 'lv_foo = lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "comment1

  METHOD comment2.

    _code '" lorem ipsum foo'.
    _code 'lv_foo = lv_bar.'.

    _test abap_true.

  ENDMETHOD.                    "comment2

  METHOD set1.

    _code 'SET SCREEN 2000.'.

    _test abap_true.

  ENDMETHOD.                    "set1

  METHOD set2.

    _code 'SET PF-STATUS ''TEST''.'.

    _test abap_true.

  ENDMETHOD.                    "set2

  METHOD set3.

    _code 'SET TITLEBAR ''TITLE''.'.

    _test abap_true.

  ENDMETHOD.                    "set3

  METHOD set4.

    _code 'SET SCREEN SY-DYNNR.'.

    _test abap_true.

  ENDMETHOD.                    "set4

  METHOD clear1.

    _code 'CLEAR lv_foo.'.

    _test abap_true.

  ENDMETHOD.                    "clear1

  METHOD append1.

    _code 'APPEND ls_foo TO lt_foo.'.

    _test abap_true.

  ENDMETHOD.                    "append1

  METHOD append2.

    _code 'APPEND ls_foo TO lo_bar->gt_tab.'.

    _test abap_true.

  ENDMETHOD.                    "append2

  METHOD start1.

    _code 'START-OF-SELECTION.'.

    _test abap_true.

  ENDMETHOD.                    "start1

  METHOD modify1.

    _code 'MODIFY CURRENT LINE.'.

    _test abap_true.

  ENDMETHOD.

  METHOD modify2.

    _code 'MODIFY CURRENT LINE FIELD FORMAT lv_foo COLOR 1 LINE FORMAT COLOR 5.'.

    _test abap_true.

  ENDMETHOD.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Standard_Programs DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test_programs DEFINITION FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
* ================

    METHODS: read  IMPORTING iv_program     TYPE program
                   RETURNING VALUE(rt_code) TYPE string_table,
      check IMPORTING iv_program TYPE program.

* note: the following programs should not contain macro calls
*       macro calls doesnt work in the parser
    METHODS:
      lsuid_maintenancep01 FOR TESTING,
      lslvc_fullscreenf03  FOR TESTING,
      saphtml_demo1        FOR TESTING,
      sap_picture_demo     FOR TESTING,
      rsdemo_table_control FOR TESTING,
      saprdemoviewing      FOR TESTING.

ENDCLASS.       "lcl_Standard_Programs

*----------------------------------------------------------------------*
*       CLASS lcl_Standard_Programs IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test_programs IMPLEMENTATION.
* ===========================================

  METHOD read.

    DATA: lt_source     TYPE TABLE OF abaptxt255,
          lt_tokens     TYPE stokesx_tab,
          lv_statement  TYPE string,
          lt_statements TYPE sstmnt_tab.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements,
                   <ls_token>     LIKE LINE OF lt_tokens.


    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = iv_program
        only_source      = abap_true
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>assert_initial( sy-subrc ).
    ENDIF.

* split the source so there is one statement per line in table
    SCAN ABAP-SOURCE lt_source
         TOKENS          INTO lt_tokens
         STATEMENTS      INTO lt_statements
         WITH ANALYSIS.

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_statement.
      LOOP AT lt_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.
      CONCATENATE lv_statement '.' INTO lv_statement.
      APPEND lv_statement TO rt_code.
    ENDLOOP.

  ENDMETHOD.                    "read

  METHOD check.

    DATA: lt_code     TYPE string_table,
          lt_original TYPE string_table,
          ls_result   TYPE zcl_aoc_parser=>ty_result,
          lv_string   LIKE LINE OF lt_code.


    lt_original = read( iv_program ).

    LOOP AT lt_original INTO lv_string.
      CLEAR lt_code.
      APPEND lv_string TO lt_code.
      ls_result = zcl_aoc_parser=>run( lt_code ).
      IF ls_result-match = abap_false.
        cl_abap_unit_assert=>fail( lv_string ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "check

  METHOD lsuid_maintenancep01.

    check( 'LSUID_MAINTENANCEP01' ).

  ENDMETHOD.                    "LSUID_MAINTENANCEP01

  METHOD lslvc_fullscreenf03.

    check( 'LSLVC_FULLSCREENF03' ).

  ENDMETHOD.                    "LSLVC_FULLSCREENF03

  METHOD saphtml_demo1.

    check( 'SAPHTML_DEMO1' ).

  ENDMETHOD.                    "SAPHTML_DEMO1

  METHOD sap_picture_demo.

    check( 'SAP_PICTURE_DEMO' ).

  ENDMETHOD.                    "SAP_PICTURE_DEMO

  METHOD rsdemo_table_control.

    check( 'RSDEMO_TABLE_CONTROL' ).

  ENDMETHOD.                    "RSDEMO_TABLE_CONTROL

  METHOD saprdemoviewing.

    check( 'SAPRDEMOVIEWING' ).

  ENDMETHOD.                    "SAPRDEMOVIEWING

ENDCLASS.       "lcl_Standard_Programs
