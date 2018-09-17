
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.

    DATA:
      mt_code   TYPE string_table,
      ms_result TYPE scirest_ad,
      mo_check  TYPE REF TO zcl_aoc_check_76.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01    FOR TESTING,
      test001_02    FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
    zcl_aoc_unit_test=>set_object_type( 'CLAS' ).
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.

    mt_code = VALUE #( ( `SELECT * INTO TABLE @DATA(lt_data) FROM e070 INNER JOIN e07t ON e070~trkorr = e07t~trkorr WHERE e07t~langu = @sy-langu.` ) ).

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_02.

    mt_code = VALUE #( ( `SELECT * INTO TABLE @DATA(lt_data) FROM e070 INNER JOIN e071 ON e070~trkorr = e071~trkorr.` ) ).

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = space
                                        act = ms_result-code ).

  ENDMETHOD.
ENDCLASS.