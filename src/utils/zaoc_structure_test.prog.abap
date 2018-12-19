REPORT zaoc_structure_test.

DATA: gv_ok_code LIKE sy-ucomm.

START-OF-SELECTION.
  PERFORM run.

FORM run.
  CALL SCREEN 2000.
ENDFORM.

CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      initialize,
      run.

  PRIVATE SECTION.
    CLASS-METHODS:
      build_result
        IMPORTING io_structure     TYPE REF TO zcl_aoc_structure
        RETURNING VALUE(rv_result) TYPE string.

    CLASS-DATA:
      go_splitter TYPE REF TO cl_gui_easy_splitter_container,
      go_left     TYPE REF TO cl_gui_textedit,
      go_right    TYPE REF TO cl_gui_textedit.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD initialize.

    IF go_splitter IS BOUND.
      RETURN.
    ENDIF.

    CREATE OBJECT go_splitter
      EXPORTING
        parent      = cl_gui_container=>screen0
        orientation = 1.

    CREATE OBJECT go_left
      EXPORTING
        parent = go_splitter->top_left_container.
    go_left->set_toolbar_mode( ).
    go_left->set_font_fixed( ).
    go_left->set_statusbar_mode( cl_gui_textedit=>false ).

    CREATE OBJECT go_right
      EXPORTING
        parent = go_splitter->bottom_right_container.
    go_right->set_toolbar_mode( ).
    go_right->set_readonly_mode( ).
    go_right->set_font_fixed( ).
    go_right->set_statusbar_mode( cl_gui_textedit=>false ).

  ENDMETHOD.

  METHOD run.

    DATA: lo_structure  TYPE REF TO zcl_aoc_structure,
          lv_text       TYPE string,
          lt_code       TYPE TABLE OF abaptxt255,
          lt_tokens     TYPE stokesx_tab,
          lt_statements TYPE sstmnt_tab,
          lt_structures TYPE zcl_aoc_super=>ty_structures_tt.


    go_left->get_textstream( IMPORTING text = lv_text ).
    cl_gui_cfw=>flush( ).

    SPLIT lv_text AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_code.

    SCAN ABAP-SOURCE lt_code
         TOKENS          INTO lt_tokens
         STATEMENTS      INTO lt_statements
         STRUCTURES      INTO lt_structures
         WITH ANALYSIS
         WITH COMMENTS
         WITH PRAGMAS    abap_true.
    ASSERT sy-subrc = 0.

    lo_structure = zcl_aoc_structure=>build(
      it_tokens     = lt_tokens
      it_statements = lt_statements
      it_structures = lt_structures ).

    go_right->set_textstream( build_result( lo_structure ) ).

  ENDMETHOD.

  METHOD build_result.

    CONCATENATE LINES OF zcl_aoc_structure=>to_string( io_structure )
      INTO rv_result SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  lcl_gui=>initialize( ).

  SET PF-STATUS 'STATUS_2000'.
  SET TITLEBAR 'TITLE_2000'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  CASE gv_ok_code.
    WHEN 'BACK'.
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
    WHEN 'RUN'.
      CLEAR gv_ok_code.
      lcl_gui=>run( ).
  ENDCASE.
ENDMODULE.
