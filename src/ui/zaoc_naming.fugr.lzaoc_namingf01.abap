*----------------------------------------------------------------------*
***INCLUDE LZAOC_NAMINGF01.
*----------------------------------------------------------------------*

CLASS lcl_screen2000 IMPLEMENTATION.

  METHOD handle_command.
    IF sy-dynnr = '2000'.
      CASE sy-ucomm.
        WHEN 'MY_OKAY'.
          gv_cancel = abap_false.
          LEAVE TO SCREEN 0.
        WHEN 'MY_CANCEL'.
          gv_cancel = abap_true.
          LEAVE TO SCREEN 0.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD at_output.

    CASE sy-dynnr.
      WHEN '2000'.
        CALL FUNCTION 'RS_EXTERNAL_SELSCREEN_STATUS'
          EXPORTING
            p_fb = 'SCI_TEST_ABAP_NAMING_STATUS'.
      WHEN '3000'.
        set_texts( 'PREFIX_' ).
        modify_screen( ).
      WHEN '4000'.
        set_texts( 'GLOBALS_' ).
        modify_screen( ).
      WHEN '5000'.
        set_texts( 'LOCALS_' ).
        modify_screen( ).
      WHEN '6000'.
        set_texts( 'PROC_' ).
        modify_screen( ).
      WHEN '7000'.
        set_texts( 'OO_' ).
        modify_screen( ).
      WHEN '8000'.
        set_texts( 'SET_' ).
        modify_screen( ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD modify_screen.
    LOOP AT SCREEN.
      IF screen-group3 = 'PAR'.
        CASE gv_read_only.
          WHEN abap_true.
            screen-input = 0.
          WHEN abap_false.
            screen-input = 1.
        ENDCASE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_structure.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = 'ZAOC_NAMING'
        langu         = 'E'
      TABLES
        dd03p_tab     = rt_data
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD set_texts.

    DATA: lt_descriptions TYPE dd03ptab,
          ls_description  LIKE LINE OF lt_descriptions,
          lt_text         TYPE STANDARD TABLE OF rsseltexts,
          lv_text         TYPE string,
          lv_name         TYPE fieldname.


    lt_descriptions = read_structure( ).

    LOOP AT SCREEN.
      IF screen-group3 = 'PAR'.
        lv_name = iv_prefix && screen-name+2.
        READ TABLE lt_descriptions INTO ls_description WITH KEY fieldname = lv_name.
        IF sy-subrc = 0.
          lv_text = ls_description-scrtext_l.
          IF lv_text IS INITIAL.
            lv_text = ls_description-ddtext.
          ENDIF.

          APPEND VALUE #(
            name = screen-name
            kind = 'P'
            text = lv_text ) TO lt_text.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SELECTION_TEXTS_MODIFY'
      EXPORTING
        program  = 'SAPLZAOC_NAMING'
      TABLES
        seltexts = lt_text.

  ENDMETHOD.

  METHOD set_read_only.
    gv_read_only = iv_only.
  ENDMETHOD.

  METHOD set_data.

    DATA: lt_fields TYPE dd03ptab,
          lv_target TYPE fieldname,
          ls_field  LIKE LINE OF lt_fields.

    FIELD-SYMBOLS: <lv_source> TYPE any,
                   <lv_target> TYPE any.


    lt_fields = read_structure( ).

    LOOP AT lt_fields INTO ls_field WHERE datatype <> ''.
      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE is_data TO <lv_source>.
      ASSERT sy-subrc = 0.

      lv_target = ls_field-fieldname.
      REPLACE FIRST OCCURRENCE OF REGEX '^\w+_' IN lv_target WITH 'P_'.
      ASSIGN (lv_target) TO <lv_target>.
      ASSERT sy-subrc = 0.

      <lv_target> = <lv_source>.
    ENDLOOP.

  ENDMETHOD.

  METHOD initialize.

    set_read_only( iv_read_only ).
    set_data( is_data ).

  ENDMETHOD.

  METHOD get_data.

    DATA: lt_fields TYPE dd03ptab,
          lv_source TYPE fieldname,
          ls_field  LIKE LINE OF lt_fields.

    FIELD-SYMBOLS: <lv_source> TYPE any,
                   <lv_target> TYPE any.


    lt_fields = read_structure( ).

    LOOP AT lt_fields INTO ls_field WHERE datatype <> ''.
      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE rs_data TO <lv_target>.
      ASSERT sy-subrc = 0.

      lv_source = ls_field-fieldname.
      REPLACE FIRST OCCURRENCE OF REGEX '^\w+_' IN lv_source WITH 'P_'.
      ASSIGN (lv_source) TO <lv_source>.
      ASSERT sy-subrc = 0.

      <lv_target> = <lv_source>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
