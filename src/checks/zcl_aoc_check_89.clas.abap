CLASS zcl_aoc_check_89 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.

  PROTECTED SECTION.
    DATA mv_minlength TYPE i.

  PRIVATE SECTION.
    METHODS get_lines_of_documentation
      IMPORTING
        iv_obj_type      TYPE trobjtype
        iv_obj_name      TYPE sobj_name
      RETURNING
        VALUE(rv_result) TYPE i.
    METHODS get_content
      IMPORTING
        iv_obj_type      TYPE trobjtype
        iv_obj_name      TYPE sobj_name
      RETURNING
        VALUE(rv_result) TYPE xstring.
ENDCLASS.



CLASS zcl_aoc_check_89 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA lv_actual_length TYPE i.
    FIELD-SYMBOLS <ls_level> TYPE slevel.

    lv_actual_length = get_lines_of_documentation(
                      iv_obj_type = object_type
                      iv_obj_name = object_name ).

    IF lv_actual_length < mv_minlength.

      READ TABLE io_scan->levels
        WITH KEY level = 0
        ASSIGNING <ls_level>.

      IF sy-subrc = 0.
        inform( p_sub_obj_name = <ls_level>-name
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001'
                p_param_1  = |{ lv_actual_length } < { mv_minlength }| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '089'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Minimum lines of documentation not reached: &1'(m01) ).

    mv_minlength = 10.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_minlength = mv_minlength
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_content.

    DATA lv_destination TYPE rfcdest.
    DATA lv_objname TYPE lxeobjname.
    DATA lv_objtype TYPE lxeobjtype.

    lv_destination = get_destination( ).

    lv_objname = iv_obj_name.
    lv_objtype = iv_obj_type(2).

    IF lv_objtype = 'PR'.        "PROG -> REPO
      lv_objtype = 'RE'.
    ENDIF.

    "function module already exists in 7.02 -> should work on every satellite system
    CALL FUNCTION 'ZAOC_OBJ_DOKU_GET_XSTRING_RFC'
      DESTINATION lv_destination
      EXPORTING
        lang    = sy-langu
        objtype = lv_objtype
        objname = lv_objname
      IMPORTING
        content = rv_result.

    "not found in logon language try with English and German
    IF rv_result IS INITIAL
        AND sy-langu <> 'E'.
      CALL FUNCTION 'ZAOC_OBJ_DOKU_GET_XSTRING_RFC'
        DESTINATION lv_destination
        EXPORTING
          lang    = 'E'
          objtype = lv_objtype
          objname = lv_objname
        IMPORTING
          content = rv_result.
    ENDIF.

    IF rv_result IS INITIAL
        AND sy-langu <> 'D'.
      CALL FUNCTION 'ZAOC_OBJ_DOKU_GET_XSTRING_RFC'
        DESTINATION lv_destination
        EXPORTING
          lang    = 'D'
          objtype = lv_objtype
          objname = lv_objname
        IMPORTING
          content = rv_result.

    ENDIF.

  ENDMETHOD.


  METHOD get_lines_of_documentation.

    DATA lo_conv TYPE REF TO cl_abap_conv_in_ce.
    DATA lv_str TYPE string.
    DATA lv_count_lines TYPE i.
    DATA lv_count_chapters_u1 TYPE i.
    DATA lv_count_chapters_u2 TYPE i.
    DATA lv_count_empty TYPE i.
    DATA lv_content TYPE xstring.


    lv_content = get_content(
        iv_obj_type = iv_obj_type
        iv_obj_name = iv_obj_name ).

    IF lv_content IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        lo_conv = cl_abap_conv_in_ce=>create(
                     input = lv_content ).

        lo_conv->read(
          IMPORTING
            data = lv_str ).

      CATCH cx_sy_conversion_codepage.
      CATCH cx_sy_codepage_converter_init.
      CATCH cx_parameter_invalid_type.
      CATCH cx_parameter_invalid_range.
    ENDTRY.

    FIND ALL OCCURRENCES OF '<itf:p ' IN lv_str MATCH COUNT lv_count_lines.
    FIND ALL OCCURRENCES OF '<itf:p name="U1">' IN lv_str MATCH COUNT lv_count_chapters_u1.  "ignore chapter headers
    FIND ALL OCCURRENCES OF '<itf:p name="U2">' IN lv_str MATCH COUNT lv_count_chapters_u2.  "ignore chapter headers

    "ignore empty lines (first line in chapter)
    FIND ALL OCCURRENCES OF '<itf:p name="AS"/>' IN lv_str MATCH COUNT lv_count_empty.

    rv_result = lv_count_lines - lv_count_chapters_u1 - lv_count_chapters_u2 - lv_count_empty.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_minlength 'Minimum lines' ''.         "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_minlength = mv_minlength
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED

    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
