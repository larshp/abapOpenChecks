*----------------------------------------------------------------------*
***INCLUDE LZAOC_NAMINGF01.
*----------------------------------------------------------------------*

CLASS lcl_screen2000 IMPLEMENTATION.

  METHOD at_selection_screen.
    CASE sy-ucomm.
      WHEN 'MY_OKAY'.
        BREAK-POINT.
    ENDCASE.
  ENDMETHOD.

  METHOD at_output.

    CASE sy-dynnr.
      WHEN '2000'.
        CALL FUNCTION 'RS_EXTERNAL_SELSCREEN_STATUS'
          EXPORTING
            p_fb = 'SCI_TEST_ABAP_NAMING_STATUS'.
      WHEN '3000'.
        set_texts( ).
      WHEN '4000'.
        set_texts( ).
      WHEN '5000'.
        set_texts( ).
      WHEN '6000'.
        set_texts( ).
      WHEN '7000'.
        set_texts( ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD set_texts.

    DATA: lt_descriptions TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY,
          lt_text         TYPE STANDARD TABLE OF rsseltexts.


    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = 'ZAOC_NAMING'
        langu         = 'E'
      TABLES
        dd03p_tab     = lt_descriptions
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    ASSERT sy-subrc = 0.

    LOOP AT SCREEN.
      IF screen-group3 = 'PAR'.
        APPEND VALUE #(
          name = screen-name
          kind = 'P'
          text = 'sdf' ) TO lt_text.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SELECTION_TEXTS_MODIFY'
      EXPORTING
        program  = 'SAPLZAOC_NAMING'
      TABLES
        seltexts = lt_text.

  ENDMETHOD.

ENDCLASS.
