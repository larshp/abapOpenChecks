class ZCL_AOC_CHECK_36 definition
  public
  inheriting from ZCL_AOC_SUPER_ROOT
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_36
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods GET_MESSAGE_TEXT
    redefinition .
  methods RUN
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_36 IMPLEMENTATION.


METHOD constructor.

  super->constructor( ).

  description    = 'Exception text not in use'.             "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_documentation = c_true.
  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

  add_obj_type( 'CLAS' ).

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Exception text not in use, &1'.             "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.


METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DEFINE _check_field.
    READ TABLE lt_comp WITH KEY name = &1 TRANSPORTING NO FIELDS.
    if sy-subrc <> 0.
    continue.
    endif.
  END-OF-DEFINITION.

  DATA: lo_object   TYPE REF TO cl_abap_objectdescr,
        lo_struct   TYPE REF TO cl_abap_structdescr,
        lv_name     TYPE wbcrossgt-name,
        lv_category TYPE seoclassdf-category,
        lt_comp     TYPE cl_abap_structdescr=>component_table.

  FIELD-SYMBOLS: <ls_attr> LIKE LINE OF lo_object->attributes.


  IF object_type <> 'CLAS'.
    RETURN.
  ENDIF.

  SELECT SINGLE category FROM seoclassdf
    INTO lv_category
    WHERE clsname = object_name
    AND version = '1'.
  IF lv_category <> '40'.
    RETURN.
  ENDIF.

  lo_object ?= cl_abap_objectdescr=>describe_by_name( object_name ).

  LOOP AT lo_object->attributes ASSIGNING <ls_attr>
      WHERE type_kind = cl_abap_classdescr=>typekind_struct2
      AND visibility = cl_abap_classdescr=>public
      AND is_constant = abap_true
      AND is_inherited = abap_false
      AND is_interface = abap_false.

    IF <ls_attr>-name = object_name.
* ignore default text name
      CONTINUE.
    ENDIF.

    lo_struct ?= lo_object->get_attribute_type( <ls_attr>-name ).
    lt_comp = lo_struct->get_components( ).

    IF lines( lt_comp ) <> 6.
      CONTINUE.
    ENDIF.
    _check_field 'MSGID'.
    _check_field 'MSGNO'.
    _check_field 'ATTR1'.
    _check_field 'ATTR2'.
    _check_field 'ATTR3'.
    _check_field 'ATTR4'.

    CONCATENATE object_name '\DA:' <ls_attr>-name INTO lv_name.

    SELECT SINGLE name FROM wbcrossgt INTO lv_name
      WHERE otype = 'DA'
      AND name = lv_name ##WARN_OK.
    IF sy-subrc <> 0.
      inform( p_sub_obj_type = object_type
              p_sub_obj_name = object_name
              p_test         = myname
              p_kind         = mv_errty
              p_code         = '001'
              p_param_1      = <ls_attr>-name ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.