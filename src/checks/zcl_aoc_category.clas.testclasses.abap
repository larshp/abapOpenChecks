
CLASS ltcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PRIVATE SECTION.

    METHODS: test FOR TESTING.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

    DATA: lo_category TYPE REF TO zcl_aoc_category.

* just test that it does not dump

    CREATE OBJECT lo_category.

  ENDMETHOD.

ENDCLASS.
