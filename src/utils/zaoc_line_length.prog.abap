REPORT zaoc_line_length.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: tdevc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_devc FOR tdevc-devclass OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_split TYPE i DEFAULT 10,
            p_mview TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

******************************************************************

TYPES: BEGIN OF ty_result,
         from  TYPE string,
         count TYPE i,
       END OF ty_result.

TYPES: ty_result_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run RETURNING VALUE(rt_result) TYPE ty_result_tt.

  PRIVATE SECTION.
    CLASS-DATA:
      gt_result TYPE ty_result_tt.

    CLASS-METHODS:
      init_result,
      run_package IMPORTING iv_devclass TYPE devclass,
      run_program IMPORTING iv_program TYPE program.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    DATA: lt_packages TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY,
          lv_package  LIKE LINE OF lt_packages.


    init_result( ).

    SELECT devclass FROM tdevc INTO TABLE lt_packages
      WHERE devclass IN s_devc
      ORDER BY devclass ASCENDING.        "#EC CI_GENBUFF "#EC CI_SUBRC

    LOOP AT lt_packages INTO lv_package.
      run_package( lv_package ).
    ENDLOOP.

    rt_result = gt_result.

  ENDMETHOD.

  METHOD init_result.

    DATA: lv_length TYPE i.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF gt_result.


    CLEAR gt_result.

    WHILE lv_length < 255.
      APPEND INITIAL LINE TO gt_result ASSIGNING <ls_result>.
      <ls_result>-from = |{ lv_length } to { lv_length + p_split - 1 } characters|.

      lv_length = lv_length + p_split.
    ENDWHILE.

  ENDMETHOD.

  METHOD run_package.

    DATA: lt_programs TYPE scit_program,
          lv_program  LIKE LINE OF lt_programs.


    lt_programs = zcl_aoc_util_programs=>get_programs_in_package(
      iv_devclass          = iv_devclass
      iv_ignore_mview_fugr = p_mview ).

    LOOP AT lt_programs INTO lv_program.
      IF sy-tabix MOD 100 = 0.
        cl_progress_indicator=>progress_indicate(
          i_text               = iv_devclass
          i_processed          = sy-tabix
          i_total              = lines( lt_programs )
          i_output_immediately = abap_true ).
      ENDIF.

      run_program( lv_program ).
    ENDLOOP.

  ENDMETHOD.

  METHOD run_program.

    DATA: lt_source TYPE TABLE OF abaptxt255,
          lv_index  TYPE i,
          ls_source LIKE LINE OF lt_source.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF gt_result.


    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = iv_program
        with_includelist = abap_false
        only_source      = abap_true
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_source INTO ls_source.
      lv_index = ( strlen( ls_source-line ) DIV p_split ) + 1.
      READ TABLE gt_result INDEX lv_index ASSIGNING <ls_result>. "#EC CI_SUBRC
      <ls_result>-count = <ls_result>-count + 1.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      show
        IMPORTING VALUE(it_result) TYPE ty_result_tt
        RAISING   cx_salv_msg.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD show.

    DATA: lo_salv TYPE REF TO cl_salv_table.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_salv
      CHANGING
        t_table      = it_result ).

    lo_salv->get_columns( )->set_optimize( ).
    lo_salv->get_functions( )->set_all( ).
    lo_salv->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_gui=>show( lcl_app=>run( ) ).
