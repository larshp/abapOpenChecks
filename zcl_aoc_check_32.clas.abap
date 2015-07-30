class ZCL_AOC_CHECK_32 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_32
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
protected section.
private section.

  data MT_DEVCLASS type SVER_R_DEVCLASS .
  data MV_IGNORE_LTCL type BOOLEAN .
ENDCLASS.



CLASS ZCL_AOC_CHECK_32 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_statement TYPE string,
        lv_devclass TYPE tadir-devclass,
        lv_include TYPE sobj_name.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type = scan_stmnt_type-standard
      OR type = scan_stmnt_type-method_direct.

    CLEAR lv_statement.
    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to
        WHERE type = scan_token_type-identifier.
      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str
          INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lv_statement CP 'SELECT * FROM *'.
      IF NOT object_type IS INITIAL.
        SELECT SINGLE devclass FROM tadir INTO lv_devclass
          WHERE pgmid = 'R3TR'
          AND object = object_type
          AND obj_name = object_name.
        IF sy-subrc <> 0 OR lv_devclass NOT IN mt_devclass.
          RETURN.
        ENDIF.
      ENDIF.

      lv_include = get_include( p_level = <ls_statement>-level ).

      IF mv_ignore_ltcl = abap_true
          AND object_type = 'CLAS'
          AND strlen( lv_include ) = 34
          AND lv_include+30(4) = 'CCAU'.
        CONTINUE.
      ENDIF.

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDIF.

* todo, database operations, configurable?:
* UPDATE
* MODIFY
* INSERT

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Database access'.                       "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '032'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_ignore_ltcl = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mt_devclass = mt_devclass
    mv_ignore_ltcl = mv_ignore_ltcl
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Database access'.                           "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~query_attributes.

  DATA: lv_ok         TYPE abap_bool,
        lv_message    TYPE c LENGTH 72,
        lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT
  fill_att mt_devclass 'Package' 'S'.                       "#EC NOTEXT
  fill_att mv_ignore_ltcl 'Ignore local test classes' 'C'.  "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = myname
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mt_devclass = mt_devclass
    mv_ignore_ltcl = mv_ignore_ltcl
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.