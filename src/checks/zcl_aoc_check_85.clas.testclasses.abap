CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_check  TYPE REF TO zcl_aoc_check_85.

    METHODS:
      setup,
      export_import FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

ENDCLASS.
