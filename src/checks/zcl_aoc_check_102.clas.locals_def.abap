CONSTANTS:
  BEGIN OF gc_code,
    usage_uncategorized  TYPE sci_errc VALUE '001',
    in_condition         TYPE sci_errc VALUE '002',
    first_letter_used    TYPE sci_errc VALUE '003',
    as_default_value     TYPE sci_errc VALUE '004',
    in_concatenate       TYPE sci_errc VALUE '005',
    overridden           TYPE sci_errc VALUE '006',
    assigned_to_variable TYPE sci_errc VALUE '007',
    in_database_select   TYPE sci_errc VALUE '008',
    in_write             TYPE sci_errc VALUE '009',
    in_message           TYPE sci_errc VALUE '010',
    within_macro         TYPE sci_errc VALUE '011',
  END OF gc_code.

CLASS lcl_check_helper DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_scan TYPE REF TO zcl_aoc_scan.

    METHODS determine_error_code
      IMPORTING
        is_token             TYPE stokesx
        iv_index_token       TYPE sy-tabix
        is_statement         TYPE sstmnt
      RETURNING
        VALUE(rv_error_code) TYPE sci_errc.

  PRIVATE SECTION.
    DATA mo_scan TYPE REF TO zcl_aoc_scan.

    METHODS is_using_only_first_letter
      IMPORTING
        is_token         TYPE stokesx
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_used_in_macro
      IMPORTING
        is_statement     TYPE sstmnt
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDCLASS.
