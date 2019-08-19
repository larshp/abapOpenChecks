CLASS zcl_aoc_check_93 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_class_static TYPE flag.
    DATA mv_me_instance TYPE flag.

ENDCLASS.



CLASS ZCL_AOC_CHECK_93 IMPLEMENTATION.


  METHOD check.

    LOOP AT io_scan->statements INTO DATA(ls_statement).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '093'.

    enable_rfc( ).

    has_attributes = abap_true.
    attributes_ok  = abap_true.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = ||.                                        "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.
    " copied from zcl_aoc_check_39

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_class_static 'Classname in front of Static Methods' ''. "#EC NOTEXT
    zzaoc_fill_att mv_me_instance 'ME in front of Instance Methods' ''. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.
ENDCLASS.
