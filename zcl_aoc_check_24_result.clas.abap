class ZCL_AOC_CHECK_24_RESULT definition
  public
  inheriting from CL_CI_RESULT_PROGRAM
  create public .

public section.

  constants:
*"* public components of class ZCL_AOC_CHECK_24_RESULT
*"* do not include other source files here!!!
    begin of C_MESSAGE_CODE,
      Summary   type Sci_ErrC  value 'Summary',                "#EC NOTEXT
      Obsolete_Tolerant type Sci_ErrC  value 'Tolerant',       "#EC NOTEXT
      Tolerable  type Sci_ErrC  value 'Tolerable',             "#EC NOTEXT
      Fatal      type Sci_ErrC  value 'Fatal',                 "#EC NOTEXT
      Critical   type Sci_ErrC  value 'Critical',              "#EC NOTEXT
      Info       type Sci_ErrC  value 'Info',                  "#EC NOTEXT
      Timeout    type Sci_ErrC  value 'Timeout',               "#EC NOTEXT
      Statistics type Sci_ErrC  value 'Statistics',            "#EC NOTEXT
    end of c_MEssage_Code .

  methods CONSTRUCTOR
    importing
      !P_KIND type SYCHAR01 .

  methods IF_CI_TEST~NAVIGATE
    redefinition .
  methods SET_INFO
    redefinition .
*"* protected components of class ZCL_AOC_CHECK_24_RESULT
*"* do not include other source files here!!!
protected section.

  class ZCL_AOC_CHECK_24 definition load .
  data MT_LIST type ZCL_AOC_CHECK_24=>TY_LIST_TT .

  methods UNPACK
    importing
      !IV_STRING type STRING
    exporting
      !ET_LIST type ZCL_AOC_CHECK_24=>TY_LIST_TT .
private section.

  methods DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
ENDCLASS.



CLASS ZCL_AOC_CHECK_24_RESULT IMPLEMENTATION.


METHOD constructor.

  super->constructor( p_kind ).

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
    RETURN.
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

  unpack( EXPORTING iv_string = result-param1
          IMPORTING et_list = mt_list ).

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
      ASSERT 1 = 1 + 1.
  ENDTRY.

ENDMETHOD.


METHOD set_info.

  p_result = super->set_info( p_info ).

ENDMETHOD.


METHOD unpack.

  DATA: lv_xstring TYPE xstring.


  lv_xstring = iv_string.
  IMPORT list = et_list FROM DATA BUFFER lv_xstring.

ENDMETHOD.
ENDCLASS.