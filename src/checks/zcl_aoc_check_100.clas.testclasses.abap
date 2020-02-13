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

    DATA:
      mt_code   TYPE string_table,
      ms_result TYPE scirest_ad,
      mo_check  TYPE REF TO zcl_aoc_check_100.

    METHODS:
      setup,
      export_import FOR TESTING,
      inline_declarations_01 FOR TESTING,
      inline_declarations_02 FOR TESTING,
      inline_declarations_03 FOR TESTING,
      inline_declarations_04 FOR TESTING,
      inline_declarations_05 FOR TESTING,
      inline_declarations_06 FOR TESTING,
      constructor_operators_01 FOR TESTING,
      constructor_operators_02 FOR TESTING,
      constructor_operators_03 FOR TESTING,
      constructor_operators_04 FOR TESTING,
      constructor_operators_05 FOR TESTING,
      constructor_operators_06 FOR TESTING,
      constructor_operators_07 FOR TESTING,
      constructor_operators_08 FOR TESTING,
      constructor_operators_09 FOR TESTING,
      constructor_operators_10 FOR TESTING,
      constructor_operators_11 FOR TESTING,
      iteration_expressions_01 FOR TESTING,
      iteration_expressions_02 FOR TESTING,
      string_templates_01 FOR TESTING,
      string_templates_02 FOR TESTING,
      string_templates_03 FOR TESTING,
      builtin_functions_01 FOR TESTING,
      builtin_functions_02 FOR TESTING,
      table_expressions_01 FOR TESTING,
      table_expressions_02 FOR TESTING,
      abap_constants_01 FOR TESTING,
      abap_constants_02 FOR TESTING,
      abap_constants_03 FOR TESTING,
      abap_constants_04 FOR TESTING,
      pragmas_01 FOR TESTING,
      pragmas_02 FOR TESTING,
      pragmas_03 FOR TESTING,
      pragmas_04 FOR TESTING,
      chained_statements_01 FOR TESTING,
      chained_statements_02 FOR TESTING,
      assignment_operators_01 FOR TESTING,
      assignment_operators_02 FOR TESTING,
      assignment_operators_03 FOR TESTING,
      assignment_operators_04 FOR TESTING,
      assignment_operators_05 FOR TESTING.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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

  METHOD inline_declarations_01.
    _code 'DATA(lv_date) = sy-datum.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-inline_declarations
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD inline_declarations_02.
    _code 'SELECT * FROM tcurc INTO TABLE @DATA(lt_tcurc) WHERE waers <> 0.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-inline_declarations
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD inline_declarations_03.
    _code 'lv_data = client->response->get_cdata( ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD inline_declarations_04.
    _code 'READ TABLE lt_tcurc ASSIGNING FIELD-SYMBOL(<ls_xxx1>) WITH KEY waers = 1 BINARY SEARCH.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-inline_declarations
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD inline_declarations_05.
    _code 'ASSIGN sy-datum TO FIELD-SYMBOL(<lv_date>).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-inline_declarations
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD inline_declarations_06.
    _code 'LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<ls_xxx>).'.
    _code 'ENDLOOP.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-inline_declarations
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_01.
    _code 'lv_new = NEW i( 555 ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_02.
    _code 'lv_value = VALUE syst( datum = sy-datum uname = 12 ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_03.
    _code 'lv_conv = CONV char10( 123456 ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_04.
    _code 'lv_ref = REF #( sy-datum ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_05.
    _code 'lv_corresponding = CORRESPONDING tcurt( ls_tcurc ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_06.
    _code 'lv_exact = EXACT #( 3 *  3 ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_07.
    _code 'lv_data = REDUCE i( INIT s = 0'.
    _code 'i = 1 UNTIL i > 10'.
    _code 'NEXT s = s + i ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_08.
    _code 'lt_filter = FILTER #( messages EXCEPT WHERE sprsl = 12 )'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_09.
    _code 'lv_cond = COND string('.
    _code 'WHEN 1 < 2'.
    _code 'THEN 333'.
    _code 'ELSE 444 ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_10.
    _code 'lv_switch = SWITCH #( sy-langu'.
    _code 'WHEN 1 THEN 2'.
    _code 'ELSE 3 ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD constructor_operators_11.
    _code 'oref2 =  CAST #( oref1 ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-constructor_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD iteration_expressions_01.
    _code 'lv_data = ( FOR ls_emp_info IN lt_tcurc( ls_en_info-waers ) ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-iteration_expressions
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD iteration_expressions_02.
    _code 'LOOP AT spfli_tab INTO DATA(wa) GROUP BY wa-carrid.'.
    _code 'ENDLOOP.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-iteration_expressions
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD string_templates_01.
    _code 'lv_string_template = |xxxxx xxxxxx|.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-string_templates
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD string_templates_02.
    _code 'lv_string_template = |Date: { sy-datum }|.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-string_templates
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD string_templates_03.
    _code 'lv_string_template = ''x1'' && ''x2''.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-string_templates
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD builtin_functions_01.
    _code 'lv_itab = line_index( itab ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-builtin_functions
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD builtin_functions_02.
    _code 'lv_itab = line_exists( itab ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-builtin_functions
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD table_expressions_01.
    _code 'ls_itab = lt_tcurc[ waers = 123 ].'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-table_expressions
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD table_expressions_02.
    _code 'ls_itab = lt_tcurc[ 1 ].'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-table_expressions
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD abap_constants_01.
    _code 'DATA lv_bool TYPE abap_bool VALUE IS INITIAL.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-abap_constants
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD abap_constants_02.
    _code 'DATA lv_true TYPE c VALUE abap_true.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-abap_constants
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD abap_constants_03.
    _code 'DATA lv_false TYPE c VALUE abap_false.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-abap_constants
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD abap_constants_04.
    _code 'DATA lv_undefined TYPE c VALUE abap_undefined.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-abap_constants
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD pragmas_01.
    _code 'text  = ''Hello Pragmas'' ##no_text.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-pragmas
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD pragmas_02.
    _code 'DATA: text1 TYPE string   ##needed,'.
    _code 'text2 TYPE string   ##needed.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-pragmas
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD pragmas_03.
    _code 'DATA ##needed: text3 TYPE string.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-pragmas
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD pragmas_04.
    _code 'text  = ''Hello Pragmas''. ##no_text'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD chained_statements_01.
    _code 'cl_demo_output=>new('.
    _code ')->write_data(: `Text1` ), `Text2` ), num'.
    _code ')->display( ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-chained_statements
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD chained_statements_02.
    _code 'cl_demo_output->new(  )->write_data2321312(: `Text1` ), `Text2` ), num  ).'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-chained_statements
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD assignment_operators_01.
    _code 'lv_data += 1.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-assignment_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD assignment_operators_02.
    _code 'lv_data -= 1.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-assignment_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD assignment_operators_03.
    _code 'lv_data *= 1.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-assignment_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD assignment_operators_04.
    _code 'lv_data /= 1.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-assignment_operators
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD assignment_operators_05.
    _code 'lv_data &&= `hello`.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_equals( exp = mo_check->ci_error_code-assignment_operators
                                        act = ms_result-code ).
  ENDMETHOD.
ENDCLASS.
