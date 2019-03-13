
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
* ================

    DATA: mo_check TYPE REF TO zcl_aoc_check_77.

    METHODS:
      setup,
      export_import FOR TESTING.

ENDCLASS.       "lcl_Test

CLASS ltcl_test IMPLEMENTATION.
* ==============================

  METHOD setup.
    CREATE OBJECT mo_check.
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

ENDCLASS.
