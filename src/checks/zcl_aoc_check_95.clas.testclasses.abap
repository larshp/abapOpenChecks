CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aoc_check_95 DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_95.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001 FOR TESTING,
      test002 FOR TESTING,
      test002b FOR TESTING,
      test003 FOR TESTING,
      test003b FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
    zcl_aoc_unit_test=>set_object_type( 'CLAS' ).
    zcl_aoc_unit_test=>set_object_name( 'ZCL_CLASS' ).

    mo_check->mv_class_in_front_not_okay = abap_true.
    mo_check->mv_object_in_front_not_okay = abap_true.
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001.
    _code 'CLASS-POOL.'.
    _code '*"* class pool for class ZCL_CLASS'.
    _code '*"* local type definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCDEF.'.
    _code '*"* use this source file for any type of declarations (class'.
    _code '*"* definitions, interfaces or type declarations) you need for'.
    _code '*"* components in the private section'.
    _code '*"* class ZCL_CLASS definition'.
    _code '*"* public declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CU.'.
    _code 'CLASS ZCL_CLASS DEFINITION PUBLIC FINAL CREATE PUBLIC.'.
    _code 'PUBLIC SECTION.'.
    _code 'TYPES TY_TYPE TYPE STRING.'.
    _code '*"* protected declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CO.'.
    _code 'PROTECTED SECTION.'.
    _code '*"* private declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CI.'.
    _code 'PRIVATE SECTION.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS definition'.
    _code '*"* macro definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCMAC.'.
    _code '*"* use this source file for any macro definitions you need'.
    _code '*"* in the implementation part of the class'.
    _code '*"* local class implementation'.
    _code 'INCLUDE ZCL_CLASS=====================CCIMP.'.
    _code '*"* use this source file for the definition and implementation of'.
    _code '*"* local helper classes, interface definitions and type'.
    _code '*"* declarations'.
    _code 'CLASS LCL_ERROR DEFINITION FINAL.'.
    _code 'PRIVATE SECTION.'.

    "Suspicious section, but valid
    _code 'DATA MV_DATA TYPE ZCL_CLASS=>TY_TYPE.'.

    _code 'ENDCLASS.'.
    _code '*"* test class'.
    _code 'INCLUDE ZCL_CLASS=====================CCAU.'.
    _code '*"* use this source file for your ABAP unit test classes'.
    _code 'CLASS ZCL_CLASS IMPLEMENTATION.'.
    _code '*"* method''s implementations'.
    _code 'INCLUDE METHODS.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS implementation'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test002.
    _code 'CLASS-POOL.'.
    _code '*"* class pool for class ZCL_CLASS'.
    _code '*"* local type definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCDEF.'.
    _code '*"* use this source file for any type of declarations (class'.
    _code '*"* definitions, interfaces or type declarations) you need for'.
    _code '*"* components in the private section'.
    _code '*"* class ZCL_CLASS definition'.
    _code '*"* public declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CU.'.
    _code 'CLASS ZCL_CLASS DEFINITION PUBLIC FINAL CREATE PUBLIC.'.
    _code 'PUBLIC SECTION.'.
    _code 'CLASS-METHODS run.'.
    _code '*"* protected declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CO.'.
    _code 'PROTECTED SECTION.'.
    _code '*"* private declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CI.'.
    _code 'PRIVATE SECTION.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS definition'.
    _code '*"* macro definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCMAC.'.
    _code '*"* use this source file for any macro definitions you need'.
    _code '*"* in the implementation part of the class'.
    _code '*"* local class implementation'.
    _code 'INCLUDE ZCL_CLASS=====================CCIMP.'.
    _code '*"* use this source file for the definition and implementation of'.
    _code '*"* local helper classes, interface definitions and type'.
    _code '*"* declarations'.
    _code '*"* test class'.
    _code 'INCLUDE ZCL_CLASS=====================CCAU.'.
    _code '*"* use this source file for your ABAP unit test classes'.
    _code 'CLASS ZCL_CLASS IMPLEMENTATION.'.
    _code '*"* method''s implementations'.
    _code 'INCLUDE METHODS.'.
    _code 'METHOD run.'.

    "Suspicious section, should ommit "ZCL_CLASS=>"
    _code 'ZCL_CLASS=>run( ).'.

    _code 'ENDMETHOD.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS implementation'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test002b.
    _code 'CLASS-POOL.'.
    _code '*"* class pool for class ZCL_CLASS'.
    _code '*"* local type definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCDEF.'.
    _code '*"* use this source file for any type of declarations (class'.
    _code '*"* definitions, interfaces or type declarations) you need for'.
    _code '*"* components in the private section'.
    _code '*"* class ZCL_CLASS definition'.
    _code '*"* public declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CU.'.
    _code 'CLASS ZCL_CLASS DEFINITION PUBLIC FINAL CREATE PUBLIC.'.
    _code 'PUBLIC SECTION.'.
    _code 'CLASS-METHODS run.'.
    _code '*"* protected declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CO.'.
    _code 'PROTECTED SECTION.'.
    _code '*"* private declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CI.'.
    _code 'PRIVATE SECTION.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS definition'.
    _code '*"* macro definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCMAC.'.
    _code '*"* use this source file for any macro definitions you need'.
    _code '*"* in the implementation part of the class'.
    _code '*"* local class implementation'.
    _code 'INCLUDE ZCL_CLASS=====================CCIMP.'.
    _code '*"* use this source file for the definition and implementation of'.
    _code '*"* local helper classes, interface definitions and type'.
    _code '*"* declarations'.
    _code '*"* test class'.
    _code 'INCLUDE ZCL_CLASS=====================CCAU.'.
    _code '*"* use this source file for your ABAP unit test classes'.
    _code 'CLASS ZCL_CLASS IMPLEMENTATION.'.
    _code '*"* method''s implementations'.
    _code 'INCLUDE METHODS.'.
    _code 'METHOD run.'.

    "Suspicious section, should ommit "ZCL_CLASS=>", but check is deactivated
    _code 'ZCL_CLASS=>run( ).'.

    _code 'ENDMETHOD.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS implementation'.

    mo_check->mv_class_in_front_not_okay = abap_false.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test003.
    _code 'CLASS-POOL.'.
    _code '*"* class pool for class ZCL_CLASS'.
    _code '*"* local type definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCDEF.'.
    _code '*"* use this source file for any type of declarations (class'.
    _code '*"* definitions, interfaces or type declarations) you need for'.
    _code '*"* components in the private section'.
    _code '*"* class ZCL_CLASS definition'.
    _code '*"* public declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CU.'.
    _code 'CLASS ZCL_CLASS DEFINITION PUBLIC FINAL CREATE PUBLIC.'.
    _code 'PUBLIC SECTION.'.
    _code 'METHODS run.'.
    _code '*"* protected declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CO.'.
    _code 'PROTECTED SECTION.'.
    _code '*"* private declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CI.'.
    _code 'PRIVATE SECTION.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS definition'.
    _code '*"* macro definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCMAC.'.
    _code '*"* use this source file for any macro definitions you need'.
    _code '*"* in the implementation part of the class'.
    _code '*"* local class implementation'.
    _code 'INCLUDE ZCL_CLASS=====================CCIMP.'.
    _code '*"* use this source file for the definition and implementation of'.
    _code '*"* local helper classes, interface definitions and type'.
    _code '*"* declarations'.
    _code '*"* test class'.
    _code 'INCLUDE ZCL_CLASS=====================CCAU.'.
    _code '*"* use this source file for your ABAP unit test classes'.
    _code 'CLASS ZCL_CLASS IMPLEMENTATION.'.
    _code '*"* method''s implementations'.
    _code 'INCLUDE METHODS.'.
    _code 'METHOD run.'.

    "Suspicious section, should ommit "me->"
    _code 'me->run( ).'.

    _code 'ENDMETHOD.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS implementation'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test003b.
    _code 'CLASS-POOL.'.
    _code '*"* class pool for class ZCL_CLASS'.
    _code '*"* local type definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCDEF.'.
    _code '*"* use this source file for any type of declarations (class'.
    _code '*"* definitions, interfaces or type declarations) you need for'.
    _code '*"* components in the private section'.
    _code '*"* class ZCL_CLASS definition'.
    _code '*"* public declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CU.'.
    _code 'CLASS ZCL_CLASS DEFINITION PUBLIC FINAL CREATE PUBLIC.'.
    _code 'PUBLIC SECTION.'.
    _code 'METHODS run.'.
    _code '*"* protected declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CO.'.
    _code 'PROTECTED SECTION.'.
    _code '*"* private declarations'.
    _code 'INCLUDE ZCL_CLASS=====================CI.'.
    _code 'PRIVATE SECTION.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS definition'.
    _code '*"* macro definitions'.
    _code 'INCLUDE ZCL_CLASS=====================CCMAC.'.
    _code '*"* use this source file for any macro definitions you need'.
    _code '*"* in the implementation part of the class'.
    _code '*"* local class implementation'.
    _code 'INCLUDE ZCL_CLASS=====================CCIMP.'.
    _code '*"* use this source file for the definition and implementation of'.
    _code '*"* local helper classes, interface definitions and type'.
    _code '*"* declarations'.
    _code '*"* test class'.
    _code 'INCLUDE ZCL_CLASS=====================CCAU.'.
    _code '*"* use this source file for your ABAP unit test classes'.
    _code 'CLASS ZCL_CLASS IMPLEMENTATION.'.
    _code '*"* method''s implementations'.
    _code 'INCLUDE METHODS.'.
    _code 'METHOD run.'.

    "Suspicious section, should ommit "me->", but check is deactivated
    _code 'me->run( ).'.

    _code 'ENDMETHOD.'.
    _code 'ENDCLASS.'.
    _code '"ZCL_CLASS implementation'.

    mo_check->mv_object_in_front_not_okay = abap_false.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.
ENDCLASS.
