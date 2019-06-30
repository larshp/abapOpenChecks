CLASS zcl_aoc_check_92 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
         REDEFINITION .
    METHODS get_message_text
         REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_92 IMPLEMENTATION.


  METHOD check.

    DATA lt_unit_tests TYPE if_aunit_prog_info_types=>ty_t_testclasses.

    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    IF NOT me->is_class_pool( me->program_name ) = abap_true.
      RETURN.
    ENDIF.

    lt_unit_tests = cl_aunit_prog_info=>get_tests_of_program( me->program_name ).

    IF lines( lt_unit_tests ) = 0.

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = me->program_name
              p_kind         = mv_errty
              p_test         = me->myname
              p_code         = '001' ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    category = 'ZCL_AOC_CATEGORY'.
    version  = '001'.
    position = '092'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = |No unit tests|.                           "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
