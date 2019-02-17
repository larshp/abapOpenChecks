CLASS zcl_aoc_unit_test DEFINITION
  PUBLIC
  CREATE PUBLIC
  FOR TESTING.

  PUBLIC SECTION.

    CLASS-METHODS handler
          FOR EVENT message OF cl_ci_test_root
      IMPORTING
          !p_checksum_1
          !p_code
          !p_column
          !p_errcnt
          !p_kind
          !p_line
          !p_param_1
          !p_param_2
          !p_param_3
          !p_param_4
          !p_sub_obj_name
          !p_sub_obj_type
          !p_suppress
          !p_test
          !p_inclspec.
    CLASS-METHODS check
      IMPORTING
        !it_code         TYPE string_table
      RETURNING
        VALUE(rs_result) TYPE scirest_ad.
    CLASS-METHODS set_check
      IMPORTING
        !io_check TYPE REF TO zcl_aoc_super.
    CLASS-METHODS export_import
      IMPORTING
        !io_check TYPE REF TO cl_ci_test_root.
    CLASS-METHODS set_object_type
      IMPORTING
        !iv_object_type TYPE trobjtype.
  PROTECTED SECTION.

    CLASS-METHODS initialize
      IMPORTING
        !it_code       TYPE string_table
      EXPORTING
        !et_tokens     TYPE stokesx_tab
        !et_statements TYPE sstmnt_tab
        !et_levels     TYPE slevel_tab
        !et_structures TYPE zcl_aoc_super=>ty_structures_tt .
  PRIVATE SECTION.

    CLASS-DATA gs_result TYPE scirest_ad.
    CLASS-DATA go_check TYPE REF TO zcl_aoc_super.
ENDCLASS.



CLASS ZCL_AOC_UNIT_TEST IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_tokens     TYPE stokesx_tab,
          lt_statements TYPE sstmnt_tab,
          lt_levels     TYPE slevel_tab,
          lt_structures TYPE zcl_aoc_super=>ty_structures_tt.


    initialize(
      EXPORTING
        it_code       = it_code
      IMPORTING
        et_tokens     = lt_tokens
        et_statements = lt_statements
        et_levels     = lt_levels
        et_structures = lt_structures ).

    go_check->check(
      it_tokens     = lt_tokens
      it_statements = lt_statements
      it_levels     = lt_levels
      it_structures = lt_structures ).

    IF NOT gs_result-code IS INITIAL.
      go_check->get_message_text(
          p_test = ''
          p_code = gs_result-code ).
    ENDIF.

    rs_result = gs_result.

  ENDMETHOD.


  METHOD export_import.

    DATA: lv_xstr TYPE xstring.

* following code will check that the get and put
* methods does not fail when run

    lv_xstr = io_check->get_attributes( ).

    cl_abap_unit_assert=>assert_not_initial( lv_xstr ).

    io_check->put_attributes( lv_xstr ).

  ENDMETHOD.


  METHOD handler.

* assume only one result
    gs_result-sobjname = p_sub_obj_name.
    gs_result-sobjtype = p_sub_obj_type.
    gs_result-line     = p_line.
    gs_result-col      = p_column.
    gs_result-kind     = p_kind.
    gs_result-code     = p_code.

  ENDMETHOD.


  METHOD initialize.

    SCAN ABAP-SOURCE it_code
         TOKENS          INTO et_tokens
         STATEMENTS      INTO et_statements
         LEVELS          INTO et_levels
         STRUCTURES      INTO et_structures
         WITH ANALYSIS
         WITH COMMENTS
         WITH PRAGMAS    abap_true.
    cl_abap_unit_assert=>assert_subrc( msg = 'Error while parsing'(001) ).

    CLEAR gs_result.
    SET HANDLER handler FOR go_check.

    go_check->set_source( iv_name = '----------------------------------------'
                          it_code = it_code ).

  ENDMETHOD.


  METHOD set_check.

    go_check = io_check.

  ENDMETHOD.


  METHOD set_object_type.

    go_check->object_type = iv_object_type.

  ENDMETHOD.
ENDCLASS.
