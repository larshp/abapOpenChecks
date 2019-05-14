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



CLASS zcl_aoc_check_92 IMPLEMENTATION.


  METHOD check.

    DATA unit_tests TYPE if_aunit_prog_info_types=>ty_t_testclasses.

    CHECK me->is_class_pool( me->program_name ) EQ abap_true.

    unit_tests = cl_aunit_prog_info=>get_tests_of_program( me->program_name ).

    IF lines( unit_tests ) = 0.

      inform(
          p_sub_obj_type = c_type_include    " Objekttyp
          p_sub_obj_name = me->program_name    " Objektname im Objektkatalog
          p_kind         = mv_errty
          p_test         = me->myname    " Name der Klassse
          p_code         = '001'    " CHAR04
      ).

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
        p_text = |No Unittest for class { me->my_progname }. |. "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
