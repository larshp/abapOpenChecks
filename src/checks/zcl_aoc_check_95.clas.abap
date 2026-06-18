CLASS zcl_aoc_check_95 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
         REDEFINITION .
    METHODS get_attributes
         REDEFINITION .
    METHODS if_ci_test~query_attributes
         REDEFINITION .
    METHODS put_attributes
         REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS check_static_access IMPORTING io_scan TYPE REF TO zcl_aoc_scan.
    METHODS check_instance_access IMPORTING io_scan TYPE REF TO zcl_aoc_scan.

    DATA mv_object_in_front_not_okay TYPE flag.
    DATA mv_class_in_front_not_okay TYPE flag.

    DATA mv_impl_start_position TYPE i.

ENDCLASS.



CLASS ZCL_AOC_CHECK_95 IMPLEMENTATION.


  METHOD check.

    DATA lt_statements TYPE zcl_aoc_scan=>ty_statements.

    FIELD-SYMBOLS <ls_statement> LIKE LINE OF lt_statements.
    FIELD-SYMBOLS <ls_statement_index> LIKE LINE OF io_scan->statements.

    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    lt_statements = io_scan->build_statements( ).

    READ TABLE lt_statements ASSIGNING <ls_statement> WITH KEY str = 'INCLUDE METHODS'.
    IF sy-subrc = 0.
      READ TABLE io_scan->statements ASSIGNING <ls_statement_index> INDEX <ls_statement>-index.
      IF sy-subrc = 0.
        mv_impl_start_position = <ls_statement_index>-to + 1.
      ENDIF.
    ENDIF.

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
    DATA ls_token TYPE stokesx.
    LOOP AT io_scan->tokens INTO ls_token FROM mv_impl_start_position.
      TRY.
          IF ls_token-str+0(4) = 'ME->'.
            inform( p_kind = mv_errty
                    p_test = myname
                    p_code = '002' ).
          ENDIF.
        CATCH cx_root.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_static_access.
    DATA ls_token TYPE stokesx.
    DATA lv_class_length TYPE i.
    LOOP AT io_scan->tokens INTO ls_token FROM mv_impl_start_position.
      lv_class_length = strlen( object_name ) + 2.
      TRY.
          IF ls_token-str+0(lv_class_length) = object_name && '=>'.
            inform( p_kind = mv_errty
                    p_test = myname
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
    position = '095'.

    has_attributes = abap_true.
    attributes_ok = abap_true.

    mv_object_in_front_not_okay = abap_false.
    mv_class_in_front_not_okay = abap_false.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Do not access static elements with "<clasname>=>"'(001) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Do not access instance methods / attributes with "me->"'(002) ).

  ENDMETHOD.


  METHOD get_attributes.
    EXPORT
      mv_errty = mv_errty
      mv_class_in_front_okay = mv_class_in_front_not_okay
      mv_object_in_front_okay = mv_object_in_front_not_okay
      TO DATA BUFFER p_attributes.
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
