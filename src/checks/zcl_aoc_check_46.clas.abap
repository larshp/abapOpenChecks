CLASS zcl_aoc_check_46 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS is_sec
      IMPORTING
        !iv_include   TYPE string
      RETURNING
        VALUE(rv_sec) TYPE abap_bool.
    METHODS method_include
      IMPORTING
        !iv_name          TYPE string
      RETURNING
        VALUE(rv_include) TYPE programm.
ENDCLASS.



CLASS ZCL_AOC_CHECK_46 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_result  TYPE scr_refs,
          lv_include TYPE programm,
          lv_name    TYPE string,
          lv_line    TYPE i,
          lv_code    TYPE sci_errc.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result.


    lt_result = zcl_aoc_compiler=>get_instance(
      iv_object_type = object_type
      iv_object_name = object_name )->get_result( ).
    DELETE lt_result WHERE tag <> 'DA'.

    LOOP AT lt_result ASSIGNING <ls_result> WHERE mode2 = '2' OR mode2 = '3' OR mode2 = '4'.
      lv_name = <ls_result>-full_name.
      lv_code = '001'.
      lv_include = <ls_result>-statement->source_info->name.
      lv_line = <ls_result>-statement->start_line.

      IF <ls_result>-full_name CP '*\ME:*'.

        IF is_sec( <ls_result>-statement->source_info->name ) = abap_true.
          lv_include = method_include( lv_name ).
          IF lv_include IS INITIAL.
            CONTINUE. " current loop
          ENDIF.
          lv_line = 1.
          lv_code = '002'.
        ENDIF.

        REPLACE FIRST OCCURRENCE OF REGEX '\\ME:\w+\\' IN lv_name WITH '\'.
      ELSEIF <ls_result>-full_name CP '*\FO:*'.
        REPLACE FIRST OCCURRENCE OF REGEX '\\FO:\w+\\' IN lv_name WITH '\'.
      ELSE.
        CONTINUE. " current loop
      ENDIF.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_result WITH KEY full_name = lv_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line         = lv_line
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code
                p_param_1      = <ls_result>-name ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '046'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Shadowing variable &1'.                   "#EC NOTEXT
      WHEN '002'.
        p_text = 'Parameter shadowing variable &1'.         "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD is_sec.

    DATA: lv_cls TYPE seoclsname.


    rv_sec = abap_false.

    lv_cls = object_name.

    IF object_type = 'CLAS'
        AND ( iv_include = cl_oo_classname_service=>get_pubsec_name( lv_cls )
        OR iv_include = cl_oo_classname_service=>get_prisec_name( lv_cls )
        OR iv_include = cl_oo_classname_service=>get_prosec_name( lv_cls ) ).
      rv_sec = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD method_include.

    DATA: ls_mtdkey TYPE seocpdkey,
          lv_off    TYPE i,
          lv_len    TYPE i.


    FIND REGEX '\\ME:.*\\' IN iv_name
      MATCH OFFSET lv_off
      MATCH LENGTH lv_len.
    lv_off = lv_off + 4.
    lv_len = lv_len - 5.
    ls_mtdkey-clsname = object_name.
    ls_mtdkey-cpdname = iv_name+lv_off(lv_len).
    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey              = ls_mtdkey
      RECEIVING
        result              = rv_include
      EXCEPTIONS
        class_not_existing  = 1
        method_not_existing = 2
        OTHERS              = 3 ).                        "#EC CI_SUBRC

  ENDMETHOD.
ENDCLASS.
