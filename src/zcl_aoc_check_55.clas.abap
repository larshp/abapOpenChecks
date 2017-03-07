class ZCL_AOC_CHECK_55 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

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

  data MV_SKIPC type FLAG .

  methods WORD
    importing
      !IS_STATEMENT type TY_STATEMENT
    returning
      value(RV_WORD) type STRING .
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_55 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE ty_statements,
          lv_current    TYPE string,
          lv_previous   TYPE string,
          lv_last       TYPE string,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements,
                   <ls_previous>  LIKE LINE OF lt_statements.


    lt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      IF sy-tabix = 1.
        CONTINUE.
      ENDIF.

      READ TABLE lt_statements INDEX sy-tabix - 1 ASSIGNING <ls_previous>.
      ASSERT sy-subrc = 0.

      IF <ls_statement>-include <> <ls_previous>-include.
        CLEAR lv_last.
        CONTINUE.
      ENDIF.

      lv_current = word( <ls_statement> ).
      lv_previous = word( <ls_previous> ).

      IF lv_current IS INITIAL OR lv_previous IS INITIAL.
        CLEAR lv_last.
        CONTINUE.
      ENDIF.

      IF lv_current = lv_previous
          AND <ls_previous>-terminator = '.'
          AND ( lv_current = 'DATA'
          OR lv_current = 'FIELD-SYMBOL'
          OR lv_current = 'FIELD-SYMBOLS'
          OR lv_current = 'CONSTANT'
          OR lv_current = 'CONSTANTS' )
          AND lv_last <> lv_current.

        IF mv_skipc = abap_true
            AND is_class_definition( <ls_statement>-include ) = abap_true.
          CONTINUE.
        ENDIF.

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
* make sure only 1 finding is reported per block of chainable
* statements
        lv_last = lv_current.
      ELSE.
        CLEAR lv_last.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Statements can be chained'.           "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '055'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.
    mv_skipc = abap_true.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Statements can be chained'.               "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_skipc 'Skip global class definitions' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD word.

    DATA: lt_tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.


    SPLIT is_statement-str AT space INTO TABLE lt_tab.

    READ TABLE lt_tab INDEX 1 INTO rv_word.

  ENDMETHOD.
ENDCLASS.
