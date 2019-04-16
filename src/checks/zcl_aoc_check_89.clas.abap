CLASS zcl_aoc_check_89 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
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
        i_obj_type      TYPE trobjtype
        i_obj_name      TYPE sobj_name
      RETURNING
        VALUE(r_result) TYPE i.
    METHODS get_content
      IMPORTING
        i_obj_type      TYPE trobjtype
        i_obj_name      TYPE sobj_name
      RETURNING
        VALUE(r_result) TYPE xstring.
ENDCLASS.



CLASS zcl_aoc_check_89 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA actual_length TYPE i.

    actual_length = get_lines_of_documentation(
                      i_obj_type = object_type
                      i_obj_name = object_name
                    ).

    IF actual_length < mv_minlength.

      FIELD-SYMBOLS <level> TYPE slevel.
      READ TABLE it_levels
        WITH KEY level = 0
        ASSIGNING <level>.

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = <level>-name
              p_kind         = mv_errty
              p_test         = myname
              p_code         = |{ actual_length } < { mv_minlength }| ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '089'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.
    mv_minlength = 10.

  ENDMETHOD.                    "CONSTRUCTOR





  METHOD get_message_text.

    CLEAR p_text.

    IF p_code IS NOT INITIAL.
      p_text = |{ 'Minimum lines of documentation not reached:'(m01) } { p_code }|.
    ENDIF.

  ENDMETHOD.                    "GET_MESSAGE_TEXT



  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_minlength = mv_minlength
      TO DATA BUFFER p_attributes.

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


  METHOD get_lines_of_documentation.

    DATA destination TYPE rfcdest.
    IF srcid IS NOT INITIAL.
      destination = cl_abap_source_id=>get_destination( srcid ).
    ELSE.
      destination = 'NONE'.
    ENDIF.

    DATA objname TYPE lxeobjname.
    objname = i_obj_name.
    DATA objtype TYPE lxeobjtype.
    objtype = i_obj_type(2).

    DATA content TYPE xstring.
    content = get_content(
        i_obj_type = i_obj_type
        i_obj_name = i_obj_name ).

    IF content IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(ce) = cl_abap_conv_in_ce=>create(
                     input = content
                   ).

        DATA(len) = xstrlen( content ).
        DATA str TYPE string.

        ce->read(
          IMPORTING
            data = str                  " Einzulesendes Datenobjekt
        ).

      CATCH cx_sy_conversion_codepage.     " System-Exception bei Zeichensatzkonvertierung
      CATCH cx_sy_codepage_converter_init. " System-Exception für Initialisierung Code Page Converter
      CATCH cx_parameter_invalid_type.     " Parameter mit ungültigem Typ
      CATCH cx_parameter_invalid_range.    " Parameter mit ungültigem Wertebereich
    ENDTRY.

    FIND ALL OCCURRENCES OF '<itf:p ' IN str MATCH COUNT DATA(count_lines).
    FIND ALL OCCURRENCES OF '<itf:p name="U1">' IN str MATCH COUNT DATA(count_chapters).  "ignore chapter headers

    r_result = count_lines - count_chapters.

  ENDMETHOD.


  METHOD get_content.

    DATA destination TYPE rfcdest.
    IF srcid IS NOT INITIAL.
      destination = cl_abap_source_id=>get_destination( srcid ).
    ELSE.
      destination = 'NONE'.
    ENDIF.

    DATA objname TYPE lxeobjname.
    objname = i_obj_name.
    DATA objtype TYPE lxeobjtype.
    objtype = i_obj_type(2).

    "function module already exists in 7.02 -> should work on every satellite system
    CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING_RFC'
      DESTINATION destination
      EXPORTING
        lang    = sy-langu
        objtype = objtype
        objname = objname
      IMPORTING
        content = r_result.

    "not found in logon language try with English and German
    IF r_result IS INITIAL
    AND sy-langu <> 'E'.

      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING_RFC'
        DESTINATION destination
        EXPORTING
          lang    = 'E'
          objtype = objtype
          objname = objname
        IMPORTING
          content = r_result.
    ENDIF.

    IF r_result IS INITIAL
    AND sy-langu <> 'D'.

      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING_RFC'
        DESTINATION destination
        EXPORTING
          lang    = 'D'
          objtype = objtype
          objname = objname
        IMPORTING
          content = r_result.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
