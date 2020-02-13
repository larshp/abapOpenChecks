CLASS zcl_aoc_check_50 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_50 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE zcl_aoc_scan=>ty_statements,
          lv_code       TYPE sci_errc,
          lv_category   TYPE seoclassdf-category.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    SELECT SINGLE category FROM seoclassdf
      INTO lv_category
      WHERE clsname = object_name
      AND version = '1'.                                  "#EC CI_SUBRC

    lt_statements = io_scan->build_statements( ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF ( <ls_statement>-str CP 'ASSERT *'
          AND ( <ls_statement>-include CP '*CCAU'
          OR lv_category = seoc_category_test_class ) )
          OR ( <ls_statement>-str CP 'CL_ABAP_UNIT_ASSERT=>ASSERT*'
          AND NOT ( <ls_statement>-include CP '*CCAU'
          OR lv_category = seoc_category_test_class ) ).
        lv_code = '001'.
      ELSEIF <ls_statement>-str CP '*CL_AUNIT_ASSERT*'.
        lv_code = '002'.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '050'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'CLAS' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Use only unit test asserts in unit tests'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'CL_AUNIT_ASSERT is obsolete'(m02) ).

  ENDMETHOD.
ENDCLASS.
