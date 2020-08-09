*"* use this source file for your ABAP unit test classes
CLASS ltcl_test DEFINITION
                FOR TESTING
                RISK LEVEL HARMLESS
                DURATION SHORT.

  PRIVATE SECTION.

    DATA mo_check TYPE REF TO zcl_aoc_check_92.
    DATA mv_text  TYPE string.

    METHODS: setup,
      message_handler FOR EVENT message OF zcl_aoc_check_92 IMPORTING p_param_1,
      export_import FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_check.
    SET HANDLER message_handler FOR mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD message_handler.
    mv_text = p_param_1.
  ENDMETHOD.

ENDCLASS.
