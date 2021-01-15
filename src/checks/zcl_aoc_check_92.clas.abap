CLASS zcl_aoc_check_92 DEFINITION
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



CLASS zcl_aoc_check_92 IMPLEMENTATION.


  METHOD check.

    DATA lt_unit_tests TYPE if_aunit_prog_info_types=>ty_t_testclasses.

    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    IF NOT is_class_pool( program_name ) = abap_true.
      RETURN.
    ENDIF.

    lt_unit_tests = cl_aunit_prog_info=>get_tests_of_program( program_name ).

    IF lines( lt_unit_tests ) = 0.

      inform( p_sub_obj_name = program_name
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '092'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'No unit tests'(m01) ).                   "#EC NOTEX

  ENDMETHOD.
ENDCLASS.
