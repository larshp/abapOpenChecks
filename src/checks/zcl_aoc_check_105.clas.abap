"! <p class="shorttext synchronized">105 - RFC blocklist</p>
CLASS zcl_aoc_check_105 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_system TYPE REF TO zif_aoc_system OPTIONAL.

    METHODS check REDEFINITION.

  PRIVATE SECTION.
    DATA mo_system TYPE REF TO zif_aoc_system.
ENDCLASS.


CLASS zcl_aoc_check_105 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    enable_rfc( ).

    DATA(lv_destination) = zcl_aoc_super=>get_destination( ).

    mo_system = COND #( WHEN io_system IS BOUND
                        THEN io_system
                        ELSE NEW zcl_aoc_system( lv_destination ) ).

    version = '001'.
    position = '105'.
  ENDMETHOD.


  METHOD check.
  ENDMETHOD.
ENDCLASS.
