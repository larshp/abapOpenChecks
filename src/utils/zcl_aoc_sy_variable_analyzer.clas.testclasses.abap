CLASS ltcl_test DEFINITION
  FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    CONSTANTS gc_given_sy_variable TYPE string VALUE `SYSID`.

    DATA mt_given_code TYPE string_table.

    DATA mt_result TYPE zcl_aoc_sy_variable_analyzer=>ty_t_result.

    METHODS analyze_variables.

    METHODS assert_single_result
      IMPORTING
        is_expected_result TYPE zcl_aoc_sy_variable_analyzer=>ty_s_result.

    METHODS first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.
  DEFINE _code.
    APPEND &1 TO mt_given_code.
  END-OF-DEFINITION.

  METHOD first_test.
    " Given
    _code `IF sy-sysid = 'PRD'.`.
    _code `ENDIF.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 2
                                   usage_kind      = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD analyze_variables.
    DATA(lo_scan) = zcl_aoc_unit_test=>create_scan( mt_given_code ).
    DATA(lo_cut) = NEW zcl_aoc_sy_variable_analyzer( lo_scan ).
    mt_result = lo_cut->analyze_variable_usage( gc_given_sy_variable ).
  ENDMETHOD.

  METHOD assert_single_result.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mt_result ) ).

    cl_abap_unit_assert=>assert_equals( exp = is_expected_result
                                        act = mt_result[ 1 ] ).
  ENDMETHOD.
ENDCLASS.
