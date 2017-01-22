
CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aoc_check_35 DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PRIVATE SECTION.

    METHODS:
      setup,
      export_import FOR TESTING,
      analyze FOR TESTING.

    DATA: mo_check TYPE REF TO zcl_aoc_check_35.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_check.
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD analyze.
* it is difficult to do both positive and negative test for this check

    DATA: lv_code TYPE sci_errc,
          lt_t100 TYPE zcl_aoc_check_35=>ty_t100_tt.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.


    APPEND INITIAL LINE TO lt_t100 ASSIGNING <ls_t100>.
    <ls_t100>-arbgb = 'ZAOC_TEST'.
    <ls_t100>-msgnr = '123'.

    lv_code = mo_check->analyze( lt_t100 ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_code
        exp = '001' ).

    mo_check->get_message_text(
        p_test = ''
        p_code = lv_code ).

  ENDMETHOD.

ENDCLASS.
