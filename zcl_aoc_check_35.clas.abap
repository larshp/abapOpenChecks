class ZCL_AOC_CHECK_35 definition
  public
  inheriting from ZCL_AOC_SUPER_ROOT
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_35
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods GET_MESSAGE_TEXT
    redefinition .
  methods RUN
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_35 IMPLEMENTATION.


METHOD constructor.

  super->constructor( ).

  description    = 'Message not in use'.                    "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '035'.

  has_documentation = c_true.
  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

  add_obj_type( 'MSAG' ).

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Message not in use, &1'.                    "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.


METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  TYPES: BEGIN OF ty_cross,
           name TYPE cross-name,
         END OF ty_cross.

  DATA: lt_t100  TYPE TABLE OF t100 WITH DEFAULT KEY,
        lt_cross TYPE TABLE OF ty_cross,
        lt_name  TYPE TABLE OF cross-name,
        lv_name  LIKE LINE OF lt_name.

  FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.


  IF object_type <> 'MSAG'.
    RETURN.
  ENDIF.

  SELECT * FROM t100 INTO TABLE lt_t100
    WHERE sprsl = sy-langu
    AND arbgb = object_name.                            "#EC CI_GENBUFF
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  LOOP AT lt_t100 ASSIGNING <ls_t100>.
    CONCATENATE <ls_t100>-arbgb <ls_t100>-msgnr INTO lv_name RESPECTING BLANKS.
    APPEND lv_name TO lt_name.
  ENDLOOP.

  ASSERT lines( lt_name ) > 0.
  SELECT name FROM cross INTO TABLE lt_cross
    FOR ALL ENTRIES IN lt_name
    WHERE ( type = '3' OR type = 'N' )
    AND name = lt_name-table_line.                        "#EC CI_SUBRC
  SORT lt_cross BY name ASCENDING.

  LOOP AT lt_t100 ASSIGNING <ls_t100>.

    CONCATENATE <ls_t100>-arbgb <ls_t100>-msgnr
      INTO lv_name RESPECTING BLANKS.
    READ TABLE lt_cross WITH KEY name = lv_name
      BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    SELECT COUNT( * ) FROM dd08l
      WHERE arbgb = <ls_t100>-arbgb
      AND msgnr = <ls_t100>-msgnr ##WARN_OK.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    inform( p_sub_obj_type = object_type
            p_sub_obj_name = object_name
            p_test         = myname
            p_kind         = mv_errty
            p_code         = '001'
            p_param_1      = <ls_t100>-msgnr ).
  ENDLOOP.

ENDMETHOD.
ENDCLASS.