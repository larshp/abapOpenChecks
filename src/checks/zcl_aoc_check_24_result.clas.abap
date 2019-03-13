CLASS zcl_aoc_check_24_result DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_result_program
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_kind TYPE sychar01.

    METHODS if_ci_test~navigate
        REDEFINITION.
    METHODS set_info
        REDEFINITION.
  PROTECTED SECTION.

    DATA mt_list TYPE zcl_aoc_check_24=>ty_list_tt.

    METHODS unpack
      IMPORTING
        !iv_string     TYPE string
      RETURNING
        VALUE(rt_list) TYPE zcl_aoc_check_24=>ty_list_tt.
  PRIVATE SECTION.

    METHODS double_click
          FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
          !row
          !column.
ENDCLASS.



CLASS ZCL_AOC_CHECK_24_RESULT IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_kind ).

  ENDMETHOD.


  METHOD double_click.

    DATA: lv_include TYPE programm,
          lv_line    TYPE sci_proc_line.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF mt_list.


    READ TABLE mt_list INDEX row ASSIGNING <ls_list>.
    ASSERT sy-subrc = 0.

    CASE column.
      WHEN 'PROC_NAME1' OR 'CODE1' OR 'LINE1'.
        lv_include = <ls_list>-proc_name1.
        lv_line    = <ls_list>-line1.
      WHEN 'PROC_NAME2' OR 'CODE2' OR 'LINE2'.
        lv_include = <ls_list>-proc_name2.
        lv_line    = <ls_list>-line2.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = lv_include
        object_type = 'PROG'
        position    = lv_line
      EXCEPTIONS
        OTHERS      = 3.
    IF sy-subrc <> 0.
      MESSAGE w000(zabapopenchecks).
    ENDIF.

  ENDMETHOD.


  METHOD if_ci_test~navigate.

    DATA: lo_table     TYPE REF TO cl_salv_table,
          lo_events    TYPE REF TO cl_salv_events_table,
          lo_columns   TYPE REF TO cl_salv_columns_table,
          lo_functions TYPE REF TO cl_salv_functions_list.


    IF result-param1 IS INITIAL.
      RETURN.
    ENDIF.

    mt_list = unpack( result-param1 ).

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_true
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = mt_list ).

        lo_columns = lo_table->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        lo_events = lo_table->get_event( ).
        SET HANDLER double_click FOR lo_events.

        lo_functions = lo_table->get_functions( ).
        lo_functions->set_all( ).

        lo_table->display( ).
      CATCH cx_root ##CATCH_ALL.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD set_info.

    p_result = super->set_info( p_info ).

  ENDMETHOD.


  METHOD unpack.

    DATA: lv_xstring TYPE xstring.


    lv_xstring = iv_string.
    IMPORT list = rt_list FROM DATA BUFFER lv_xstring.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
