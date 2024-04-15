*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

DEFINE add_replacement_proposal.
  CLEAR lv_proposal.
  lv_proposal-from = &1.
  lv_proposal-sequence = &2.
  lv_proposal-to = &3.
  lv_proposal-oss_note = &4.
  INSERT lv_proposal INTO TABLE rt_proposals.
END-OF-DEFINITION.

CLASS lcl_quickfix DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_quick_fixes
      IMPORTING
        iv_current_tab_name  TYPE tabname
        iv_new_tab_name      TYPE tabname
        iv_include           TYPE program
        iv_source            TYPE string_table
        iv_line              TYPE i
        iv_col               TYPE token_col
        iv_table_as_used     TYPE abap_bool
      RETURNING
        VALUE(rv_qf_xstring) TYPE xstring.

    CLASS-METHODS build_proposals_list RETURNING VALUE(rt_proposals) TYPE zcl_aoc_check_103=>tty_proposals.
  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS add_new_quickfix
      IMPORTING
        iv_current_tab_name TYPE tabname
        iv_quickfixes       TYPE REF TO object
        iv_context          TYPE any
        iv_proposal         TYPE zcl_aoc_check_103=>ty_replace_proposal
        iv_table_as_used    TYPE abap_bool.

ENDCLASS.

CLASS lcl_quickfix IMPLEMENTATION.


  METHOD get_quick_fixes.
    DATA: lv_quickfixes TYPE REF TO object.
    DATA: lo_context TYPE REF TO object.
    DATA: lo_context_interface TYPE REF TO data.
    DATA: lv_quickfix TYPE REF TO object.
    DATA: lv_quickfix_pragma TYPE REF TO object.
    DATA: lv_column TYPE i.
    FIELD-SYMBOLS: <lv_context> TYPE any.
    FIELD-SYMBOLS: <lv_proposal> TYPE zcl_aoc_check_103=>ty_replace_proposal.

    TRY.
        CALL METHOD ('CL_CI_QUICKFIX_CREATION')=>('CREATE_QUICKFIX_ALTERNATIVES')
          RECEIVING
            p_quickfix_alternatives = lv_quickfixes.


        lv_column = iv_col.
        CALL METHOD ('CL_CI_QUICKFIX_ABAP_CONTEXT')=>('CREATE_FROM_INCLUDE')
          EXPORTING
            p_include = iv_include
            p_source  = iv_source
            p_line    = iv_line
            p_col     = lv_column
          RECEIVING
            p_context = lo_context.

        CREATE DATA lo_context_interface TYPE REF TO ('IF_CI_QUICKFIX_ABAP_CONTEXT').
        ASSIGN lo_context_interface->* TO <lv_context>.
        <lv_context> ?= lo_context.

        LOOP AT zcl_aoc_check_103=>gt_replace_proposals ASSIGNING <lv_proposal> WHERE from = iv_current_tab_name.
          add_new_quickfix( iv_current_tab_name = iv_current_tab_name
                            iv_quickfixes       = lv_quickfixes
                            iv_context          = <lv_context>
                            iv_proposal         = <lv_proposal>
                            iv_table_as_used    = iv_table_as_used ).
        ENDLOOP.
        CALL METHOD lv_quickfixes->('CREATE_QUICKFIX')
          RECEIVING
            p_quickfix = lv_quickfix_pragma.

        CALL METHOD lv_quickfix_pragma->('IF_CI_QUICKFIX_ABAP_ACTIONS~ADD_PSEUDO_COMMENT')
          EXPORTING
            p_pseudo_comment = zcl_aoc_check_103=>gc_pseudo_comment
            p_context        = <lv_context>.

        TRY.
            CALL METHOD lv_quickfixes->('EXPORT_TO_XSTRING')
              RECEIVING
                p_detail = rv_qf_xstring.
          CATCH cx_root.
            RETURN.
        ENDTRY.

      CATCH cx_sy_dyn_call_illegal_class.
        "Quick fixes not allowed on this version of NW
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD add_new_quickfix.

    DATA lv_quickfix TYPE REF TO object.
    DATA lv_quickfix_code TYPE c LENGTH 10.
    DATA lv_current_tab_for_msg TYPE sy-msgv1.
    DATA lv_new_tab_for_msg TYPE sy-msgv2.
    DATA lv_proposal TYPE c LENGTH 255.

    IF iv_table_as_used = abap_false.
      CONCATENATE iv_proposal-to ' AS '  iv_proposal-from INTO lv_proposal RESPECTING BLANKS.
    ELSE.
      lv_proposal = iv_proposal-to.
    ENDIF.

    CONDENSE lv_proposal.

    "Each quick-fix should have a unique code
    CONCATENATE '001_' iv_proposal-sequence  INTO lv_quickfix_code.

    CALL METHOD iv_quickfixes->('CREATE_QUICKFIX')
      EXPORTING
        p_quickfix_code = lv_quickfix_code
      RECEIVING
        p_quickfix      = lv_quickfix.
    CALL METHOD lv_quickfix->('IF_CI_QUICKFIX_ABAP_ACTIONS~REPLACE_BY')
      EXPORTING
        p_new_code = lv_proposal
        p_context  = iv_context.

    CALL METHOD lv_quickfix->('IF_CI_QUICKFIX_SINGLE~ENABLE_AUTOMATIC_EXECUTION').
    CONCATENATE ' ' iv_current_tab_name INTO lv_current_tab_for_msg RESPECTING BLANKS.
    CONCATENATE ' ' iv_proposal-to INTO lv_new_tab_for_msg RESPECTING BLANKS.
    CALL METHOD lv_quickfix->('IF_CI_QUICKFIX_SINGLE~ADD_DOCU_FROM_MSGCLASS')
      EXPORTING
        p_msg_class      = '00'
        p_msg_number     = '001'
        p_msg_parameter1 = 'Replace'(m03)
        p_msg_parameter2 = lv_current_tab_for_msg
        p_msg_parameter3 = ' with'(m04)
        p_msg_parameter4 = lv_new_tab_for_msg.

  ENDMETHOD.



  METHOD build_proposals_list.
    DATA: lv_proposal TYPE zcl_aoc_check_103=>ty_replace_proposal.
    add_replacement_proposal 'MARC' 1 'V_MARC_MD' '2206980'.
    add_replacement_proposal 'MARC' 2 'NSDM_MIG_MARC' '2206980'.
    add_replacement_proposal 'MARD' 1 'V_MARD_MD' '2206980'.
    add_replacement_proposal 'MARD' 2 'NSDM_MIG_MARD' '2206980'.
    add_replacement_proposal 'MKPF' 1 'NSDM_MIG_MKPF' '2206980'.
    add_replacement_proposal 'MSEG' 1 'NSDM_MIG_MSEG' '2206980'.
    add_replacement_proposal 'MCHB' 1 'V_MCHB_MD' '2206980'.
    add_replacement_proposal 'MCHB' 2 'NSDM_MIG_MCHB' '2206980'.
    add_replacement_proposal 'MKOL' 1 'V_MKOL_MD' '2206980'.
    add_replacement_proposal 'MKOL' 2 'NSDM_MIG_MKOL' '2206980'.
    add_replacement_proposal 'MSLB' 1 'V_MSLB_MD' '2206980'.
    add_replacement_proposal 'MSLB' 2 'NSDM_MIG_MSLB' '2206980'.
    add_replacement_proposal 'MSKA' 1 'V_MSKA_MD' '2206980'.
    add_replacement_proposal 'MSKA' 2 'NSDM_MIG_MSKA' '2206980'.
    add_replacement_proposal 'MSSA' 1 'NSDM_MIG_MSSA' '2206980'.
    add_replacement_proposal 'MSPR' 1 'V_MSPR_MD' '2206980'.
    add_replacement_proposal 'MSPR' 2 'NSDM_MIG_MSPR' '2206980'.
    add_replacement_proposal 'MSSL' 1 'NSDM_MIG_MSSL' '2206980'.
    add_replacement_proposal 'MSSQ' 1 'NSDM_MIG_MSSQ' '2206980'.
    add_replacement_proposal 'MSKU' 1 'V_MSKU_MD' '2206980'.
    add_replacement_proposal 'MSKU' 2 'NSDM_MIG_MSKU' '2206980'.
    add_replacement_proposal 'MSTB' 1 'NSDM_MIG_MSTB' '2206980'.
    add_replacement_proposal 'MSTE' 1 'NSDM_MIG_MSTE' '2206980'.
    add_replacement_proposal 'MSTQ' 1 'NSDM_MIG_MSTQ' '2206980'.
    add_replacement_proposal 'MCSD' 1 'MCSD_MD' '2206980'.
    add_replacement_proposal 'MCSD' 2 'NSDM_MIG_MCSD' '2206980'.
    add_replacement_proposal 'MCSS' 1 'MCSS_MD' '2206980'.
    add_replacement_proposal 'MCSS' 2 'NSDM_MIG_MCSS' '2206980'.
    add_replacement_proposal 'MSCD' 1 'MSCD_MD' '2206980'.
    add_replacement_proposal 'MSCD' 2 'NSDM_MIG_MSCD' '2206980'.
    add_replacement_proposal 'MSCS' 1 'MSCS_MD' '2206980'.
    add_replacement_proposal 'MSCS' 2 'NSDM_MIG_MSCS' '2206980'.
    add_replacement_proposal 'MSFD' 1 'MSFD_MD' '2206980'.
    add_replacement_proposal 'MSFD' 2 'NSDM_MIG_MSFD' '2206980'.
    add_replacement_proposal 'MSFS' 1 'MSFS_MD' '2206980'.
    add_replacement_proposal 'MSFS' 2 'NSDM_MIG_MSFS' '2206980'.
    add_replacement_proposal 'MSID' 1 'MSID_MD' '2206980'.
    add_replacement_proposal 'MSID' 2 'NSDM_MIG_MSID' '2206980'.
    add_replacement_proposal 'MSIS' 1 'MSIS_MD' '2206980'.
    add_replacement_proposal 'MSIS' 2 'NSDM_MIG_MSIS' '2206980'.
    add_replacement_proposal 'MSRD' 1 'MSRD_MD' '2206980'.
    add_replacement_proposal 'MSRD' 2 'NSDM_MIG_MSRD' '2206980'.
    add_replacement_proposal 'MSRS' 1 'MSRS_MD' '2206980'.
    add_replacement_proposal 'MSRS' 2 'NSDM_MIG_MSRS' '2206980'.
    add_replacement_proposal 'MARCH' 1 'NSDM_MIG_MARCH' '2206980'.
    add_replacement_proposal 'MARDH' 1 'NSDM_MIG_MARDH' '2206980'.
    add_replacement_proposal 'MCHBH' 1 'NSDM_MIG_MCHBH' '2206980'.
    add_replacement_proposal 'MKOLH' 1 'NSDM_MIG_MKOLH' '2206980'.
    add_replacement_proposal 'MSLBH' 1 'NSDM_MIG_MSLBH' '2206980'.
    add_replacement_proposal 'MSKAH' 1 'NSDM_MIG_MSKAH' '2206980'.
    add_replacement_proposal 'MSSAH' 1 'NSDM_MIG_MSSAH' '2206980'.
    add_replacement_proposal 'MSPRH' 1 'NSDM_MIG_MSPRH' '2206980'.
    add_replacement_proposal 'MSSQH' 1 'NSDM_MIG_MSSQH' '2206980'.
    add_replacement_proposal 'MSKUH' 1 'NSDM_MIG_MSKUH' '2206980'.
    add_replacement_proposal 'MSTBH' 1 'NSDM_MIG_MSTBH' '2206980'.
    add_replacement_proposal 'MSTEH' 1 'NSDM_MIG_MSTEH' '2206980'.
    add_replacement_proposal 'MSTQH' 1 'NSDM_MIG_MSTQH' '2206980'.
    add_replacement_proposal 'MCSDH' 1 'NSDM_MIG_MCSDH' '2206980'.
    add_replacement_proposal 'MCSSH' 1 'NSDM_MIG_MCSSH' '2206980'.
    add_replacement_proposal 'MSCDH' 1 'NSDM_MIG_MSCDH' '2206980'.
    add_replacement_proposal 'MSFDH' 1 'NSDM_MIG_MSFDH' '2206980'.
    add_replacement_proposal 'MSIDH' 1 'NSDM_MIG_MSIDH' '2206980'.
    add_replacement_proposal 'MSRDH' 1 'NSDM_MIG_MSRDH' '2206980'.
    add_replacement_proposal 'EBEW' 1 'V_EBEW_MD' '2217299'.
    add_replacement_proposal 'EBEW' 2 'MBVEBEWOLD' '2217299'.
    add_replacement_proposal 'EBEWH' 1 'V_EBEWH_MD' '2217299'.
    add_replacement_proposal 'EBEWH' 2 'MBVEBEWHOLD' '2217299'.
    add_replacement_proposal 'MBEW' 1 'V_MBEW_MD' '2217299'.
    add_replacement_proposal 'MBEW' 2 'MBVMBEWOLD' '2217299'.
    add_replacement_proposal 'MBEWH' 1 'V_MBEWH_MD' '2217299'.
    add_replacement_proposal 'MBEWH' 2 'MBVMBEWHOLD' '2217299'.
    add_replacement_proposal 'OBEW' 1 'V_OBEW_MD' '2217299'.
    add_replacement_proposal 'OBEW' 2 'MBVOBEWOLD' '2217299'.
    add_replacement_proposal 'OBEWH' 1 'V_OBEWH_MD' '2217299'.
    add_replacement_proposal 'OBEWH' 2 'MBVOBEWHOLD' '2217299'.
    add_replacement_proposal 'QBEW' 1 'V_QBEW_MD' '2217299'.
    add_replacement_proposal 'QBEW' 2 'MBVQBEWOLD' '2217299'.
    add_replacement_proposal 'QBEWH' 1 'V_QBEWH_MD' '2217299'.
    add_replacement_proposal 'QBEWH' 2 'MBVQBEWHOLD' '2217299'.

  ENDMETHOD.

ENDCLASS.
