CLASS zcl_aoc_check_94 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS if_ci_test~query_attributes REDEFINITION .
    METHODS get_attributes REDEFINITION.
    METHODS put_attributes REDEFINITION.
    METHODS check REDEFINITION.
    METHODS get_message_text REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS check_static_access IMPORTING io_scan TYPE REF TO zcl_aoc_scan.
    METHODS check_instance_access IMPORTING io_scan TYPE REF TO zcl_aoc_scan.

    DATA mv_object_in_front_not_okay TYPE flag.
    DATA mv_class_in_front_not_okay TYPE flag.

    DATA mv_impl_start_position TYPE syst_tabix.

ENDCLASS.



CLASS ZCL_AOC_CHECK_94 IMPLEMENTATION.


  METHOD check.

    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    LOOP AT io_scan->tokens INTO DATA(token).
      DATA(token_string_in_char) = CONV char35( token-str ).
      IF token_string_in_char+30(5) = 'CCIMP'.
        mv_impl_start_position = sy-tabix.
      ENDIF.
    ENDLOOP.

    " check for static calls
    IF mv_class_in_front_not_okay = abap_true.
      check_static_access( io_scan ).
    ENDIF.

    " check for instance calls
    IF mv_object_in_front_not_okay = abap_true.
      check_instance_access( io_scan ).
    ENDIF.

  ENDMETHOD.


  METHOD check_instance_access.
    LOOP AT io_scan->tokens INTO DATA(token) FROM mv_impl_start_position.
      TRY.
          IF token-str+0(4) = 'ME->'.
            inform( p_test = me->myname                 " Name der Klassse
                    p_code = '002' ).
          ENDIF.
        CATCH cx_root.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_static_access.
    LOOP AT io_scan->tokens INTO DATA(token) FROM mv_impl_start_position.
      DATA(class_length) = strlen( me->object_name ) + 2.
      TRY.
          IF token-str+0(class_length) = me->object_name && '=>'.
            inform( p_test = me->myname                 " Name der Klassse
                    p_code = '001' ).
          ENDIF.
        CATCH cx_root. " offset
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    version = '001'.
    position = '094'.

    has_attributes = abap_true.
    attributes_ok = abap_true.

    mv_object_in_front_not_okay = abap_false.
    mv_class_in_front_not_okay = abap_false.

    enable_rfc( ).

  ENDMETHOD.


  METHOD get_attributes.
    EXPORT
      mv_errty = mv_errty
      mv_class_in_front_okay = mv_class_in_front_not_okay
      mv_object_in_front_okay = mv_object_in_front_not_okay
      TO DATA BUFFER p_attributes.
  ENDMETHOD.


  METHOD get_message_text.
    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Do not access static elements with "<clasname>=>"'(001).
      WHEN '002'.
        p_text = 'Do not access instance methods / attributes with "me->"'(002).
      WHEN OTHERS.
        CLEAR p_text.
    ENDCASE.
  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.

    zzaoc_fill_att mv_class_in_front_not_okay '"<CLASS>=>" Access not OK'(011) ''.
    zzaoc_fill_att mv_object_in_front_not_okay '"ME->" Access not OK'(021) ''.

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.
    IMPORT
      mv_errty = mv_errty
      mv_class_in_front_okay = mv_class_in_front_not_okay
      mv_object_in_front_okay = mv_object_in_front_not_okay
      FROM DATA BUFFER p_attributes.
    ASSERT sy-subrc = 0.
  ENDMETHOD.
ENDCLASS.