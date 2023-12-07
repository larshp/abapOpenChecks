CLASS zcl_aoc_check_08 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.

  PROTECTED SECTION.

    DATA mv_001 TYPE flag .
    DATA mv_002 TYPE flag .
    DATA mv_003 TYPE flag .
    DATA mv_004 TYPE flag .
    DATA mv_005 TYPE flag .
    DATA mv_006 TYPE flag .
    DATA mv_007 TYPE flag .
    DATA mv_008 TYPE flag .
    DATA mv_009 TYPE flag .
    DATA mv_010 TYPE flag .
    DATA mv_011 TYPE flag .
    DATA mv_012 TYPE flag .
    DATA mv_013 TYPE flag .
    DATA mv_014 TYPE flag .
    DATA mv_015 TYPE flag .
    DATA mv_016 TYPE flag .
    DATA mv_017 TYPE flag .
    DATA mv_018 TYPE flag .
    DATA mv_019 TYPE flag .
    DATA mv_020 TYPE flag .
    DATA mv_021 TYPE flag .
    DATA mv_022 TYPE flag .
    DATA mv_023 TYPE flag .
    DATA mv_024 TYPE flag .

    METHODS is_old
      IMPORTING
        !iv_statement TYPE string
      RETURNING
        VALUE(rv_old) TYPE abap_bool .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_08 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_position  TYPE i,
          lv_include   TYPE sobj_name,
          lv_code      TYPE sci_errc,
          lv_token     TYPE string,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>.

      lv_position = sy-tabix.
      CLEAR lv_statement.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF <ls_token>-type <> io_scan->gc_token-identifier.
          lv_token = 'SOMETHING'.
        ELSE.
          lv_token = <ls_token>-str.
        ENDIF.

        IF lv_statement IS INITIAL.
          lv_statement = lv_token.
        ELSE.
          CONCATENATE lv_statement lv_token INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      CLEAR lv_code.

      IF mv_001 = abap_true AND lv_statement CP 'REFRESH *' AND NOT lv_statement CP 'REFRESH CONTROL *'.
        lv_code = '001'.
      ELSEIF mv_002 = abap_true AND lv_statement CP '* IS REQUESTED*'.
        lv_code = '002'.
      ELSEIF mv_003 = abap_true AND lv_statement = 'LEAVE'.
        lv_code = '003'.
      ELSEIF mv_004 = abap_true AND lv_statement CP 'COMPUTE *'.
        lv_code = '004'.
      ELSEIF mv_005 = abap_true
          AND lv_statement CP 'MOVE *'
          AND lv_statement NP 'MOVE EXACT *'.
        lv_code = '005'.
      ELSEIF mv_006 = abap_true
          AND ( lv_statement CP '* >< *'
          OR lv_statement CP '* =< *'
          OR lv_statement CP '* => *' )
          AND NOT lv_statement CP '*$$*$$*'. " Allow for SDA HANA query parameters
        lv_code = '006'.
      ELSEIF mv_007 = abap_true AND is_old( lv_statement ) = abap_true.
        lv_code = '007'.
      ELSEIF mv_008 = abap_true AND lv_statement CP 'DEMAND *'.
        lv_code = '008'.
      ELSEIF mv_009 = abap_true AND lv_statement CP 'SUPPLY *'.
        lv_code = '009'.
      ELSEIF mv_010 = abap_true AND lv_statement CP 'CONTEXTS *'.
        lv_code = '010'.
      ELSEIF mv_011 = abap_true AND lv_statement CP 'ADD *'.
        lv_code = '011'.
      ELSEIF mv_012 = abap_true AND lv_statement CP 'SUBTRACT *'.
        lv_code = '012'.
      ELSEIF mv_013 = abap_true AND lv_statement CP 'MULTIPLY *'.
        lv_code = '013'.
      ELSEIF mv_014 = abap_true AND lv_statement CP 'DIVIDE *'.
        lv_code = '014'.
      ELSEIF mv_015 = abap_true AND lv_statement CP 'CALL DIALOG *'.
        lv_code = '015'.
      ELSEIF mv_016 = abap_true AND lv_statement CP '* OCCURS *'
          AND NOT lv_statement CP '* OCCURS TYPE *'.
        lv_code = '016'.
      ELSEIF mv_017 = abap_true AND lv_statement CP '* WITH HEADER LINE*'.
        lv_code = '017'.
      ELSEIF mv_018 = abap_true AND lv_statement CP 'RANGES *'.
        lv_code = '018'.
      ELSEIF mv_019 = abap_true
          AND ( lv_statement CP 'ADD-CORRESPONDING *'
          OR lv_statement CP 'SUBTRACT-CORRESPONDING *'
          OR lv_statement CP 'MULTIPLY-CORRESPONDING *'
          OR lv_statement CP 'DIVIDE-CORRESPONDING *' ).
        lv_code = '019'.
      ELSEIF mv_020 = abap_true AND lv_statement CP 'SET EXTENDED CHECK *'.
        lv_code = '020'.
      ELSEIF mv_021 = abap_true AND lv_statement CP 'LOCAL *'.
        lv_code = '021'.
      ELSEIF mv_022 = abap_true AND lv_statement CP 'DO 1 TIMES*'.
        lv_code = '022'.
      ELSEIF mv_023 = abap_true AND lv_statement CP 'DO * VARYING *'.
        lv_code = '023'.
      ELSEIF mv_024 = abap_true AND lv_statement CP 'CATCH SYSTEM-EXCEPTIONS *'.
        lv_code = '024'.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_position     = lv_position
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '004'.
    position       = '008'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'REFRESH is obsolete'(m01) ).
    insert_scimessage(
        iv_code = '002'
        iv_text = 'IS REQUESTED is obsolete'(m02) ).
    insert_scimessage(
        iv_code = '003'
        iv_text = 'LEAVE is obsolete'(m03) ).
    insert_scimessage(
        iv_code = '004'
        iv_text = 'COMPUTE is obsolete'(m04) ).
    insert_scimessage(
        iv_code = '005'
        iv_text = 'MOVE is obsolete'(m05) ).
    insert_scimessage(
        iv_code = '006'
        iv_text = 'Obsolete operator'(m06) ).
    insert_scimessage(
        iv_code = '007'
        iv_text = 'Use new operator'(m07) ).
    insert_scimessage(
        iv_code = '008'
        iv_text = 'DEMAND is obsolete'(m08) ).
    insert_scimessage(
        iv_code = '009'
        iv_text = 'SUPPLY is obsolete'(m09) ).
    insert_scimessage(
        iv_code = '010'
        iv_text = 'CONTEXTS is obsolete'(m10) ).
    insert_scimessage(
        iv_code = '011'
        iv_text = 'ADD is obsolete'(m11) ).
    insert_scimessage(
        iv_code = '012'
        iv_text = 'SUBTRACT is obsolete'(m12) ).
    insert_scimessage(
        iv_code = '013'
        iv_text = 'MULTIPLY is obsolete'(m13) ).
    insert_scimessage(
        iv_code = '014'
        iv_text = 'DIVIDE is obsolete'(m14) ).
    insert_scimessage(
        iv_code = '015'
        iv_text = 'CALL DIALOG is obsolete'(m15) ).
    insert_scimessage(
        iv_code = '016'
        iv_text = 'OCCURS is obsolete'(m16) ).
    insert_scimessage(
        iv_code = '017'
        iv_text = 'WITH HEADER LINE is obsolete'(m17) ).
    insert_scimessage(
        iv_code = '018'
        iv_text = 'RANGES declarations is obsolete'(m18) ).
    insert_scimessage(
        iv_code = '019'
        iv_text = 'Arithmetic CORRESPONDING is obsolete'(m19) ).
    insert_scimessage(
        iv_code = '020'
        iv_text = 'Do not use SET EXTENDED CHECK'(m20) ).
    insert_scimessage(
        iv_code = '021'
        iv_text = 'LOCAL is obsolete'(m21) ).
    insert_scimessage(
        iv_code = '022'
        iv_text = 'DO 1 TIMES'(m22) ).
    insert_scimessage(
        iv_code = '023'
        iv_text = 'DO ... VARYING ...'(m23) ).
    insert_scimessage(
        iv_code = '024'
        iv_text = 'CATCH SYSTEM-EXCEPTIONS is obsolete'(m24) ).

    mv_001 = abap_true.
    mv_002 = abap_true.
    mv_003 = abap_true.
    mv_004 = abap_true.
    mv_005 = abap_true.
    mv_006 = abap_true.
    mv_007 = abap_true.
    mv_008 = abap_true.
    mv_009 = abap_true.
    mv_010 = abap_true.
    mv_011 = abap_true.
    mv_012 = abap_true.
    mv_013 = abap_true.
    mv_014 = abap_true.
    mv_015 = abap_true.
    mv_016 = abap_true.
    mv_017 = abap_true.
    mv_018 = abap_true.
    mv_019 = abap_true.
    mv_020 = abap_true.
    mv_021 = abap_true.
    mv_022 = abap_true.
    mv_023 = abap_true.
    mv_024 = abap_true.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_001 = mv_001
      mv_002 = mv_002
      mv_003 = mv_003
      mv_004 = mv_004
      mv_005 = mv_005
      mv_006 = mv_006
      mv_007 = mv_007
      mv_008 = mv_008
      mv_009 = mv_009
      mv_010 = mv_010
      mv_011 = mv_011
      mv_012 = mv_012
      mv_013 = mv_013
      mv_014 = mv_014
      mv_015 = mv_015
      mv_016 = mv_016
      mv_017 = mv_017
      mv_018 = mv_018
      mv_019 = mv_019
      mv_020 = mv_020
      mv_021 = mv_021
      mv_022 = mv_022
      mv_023 = mv_023
      mv_024 = mv_024
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_001 'REFRESH' ''.                     "#EC NOTEXT
    zzaoc_fill_att mv_002 'IS REQUESTED' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_003 'LEAVE' ''.                       "#EC NOTEXT
    zzaoc_fill_att mv_004 'COMPUTE' ''.                     "#EC NOTEXT
    zzaoc_fill_att mv_005 'MOVE' ''.                        "#EC NOTEXT
    zzaoc_fill_att mv_006 '>< =< =>' ''.                    "#EC NOTEXT
    zzaoc_fill_att mv_007 'EQ NE LT GT LE GE' ''.           "#EC NOTEXT
    zzaoc_fill_att mv_008 'DEMAND' ''.                      "#EC NOTEXT
    zzaoc_fill_att mv_009 'SUPPLY' ''.                      "#EC NOTEXT
    zzaoc_fill_att mv_010 'CONTEXTS' ''.                    "#EC NOTEXT
    zzaoc_fill_att mv_011 'ADD' ''.                         "#EC NOTEXT
    zzaoc_fill_att mv_012 'SUBTRACT' ''.                    "#EC NOTEXT
    zzaoc_fill_att mv_013 'MULTIPLY' ''.                    "#EC NOTEXT
    zzaoc_fill_att mv_014 'DIVIDE' ''.                      "#EC NOTEXT
    zzaoc_fill_att mv_015 'CALL DIALOG' ''.                 "#EC NOTEXT
    zzaoc_fill_att mv_016 'OCCURS' ''.                      "#EC NOTEXT
    zzaoc_fill_att mv_017 'WITH HEADER LINE' ''.            "#EC NOTEXT
    zzaoc_fill_att mv_018 'RANGES' ''.                      "#EC NOTEXT
    zzaoc_fill_att mv_019 'Arithmetic CORRESPONDING' ''.    "#EC NOTEXT
    zzaoc_fill_att mv_020 'SET EXTENDED CHECK' ''.          "#EC NOTEXT
    zzaoc_fill_att mv_021 'LOCAL' ''.                       "#EC NOTEXT
    zzaoc_fill_att mv_022 'DO 1 TIMES' ''.                  "#EC NOTEXT
    zzaoc_fill_att mv_023 'DO ... VARYING' ''.              "#EC NOTEXT
    zzaoc_fill_att mv_024 'CATCH SYSTEM-EXCEPTIONS' ''.     "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD is_old.

    rv_old = abap_false.

    IF iv_statement CP '* EQ *' AND NOT iv_statement CP '* EQ TYPE *'.
      rv_old = abap_true.
    ENDIF.

    IF iv_statement CP '* NE *' AND NOT iv_statement CP '* NE TYPE *'.
      rv_old = abap_true.
    ENDIF.

    IF iv_statement CP '* LT *' AND NOT iv_statement CP '* LT TYPE *'.
      rv_old = abap_true.
    ENDIF.

    IF iv_statement CP '* GT *' AND NOT iv_statement CP '* GT TYPE *'.
      rv_old = abap_true.
    ENDIF.

    IF iv_statement CP '* LE *' AND NOT iv_statement CP '* LE TYPE *'.
      rv_old = abap_true.
    ENDIF.

    IF iv_statement CP '* GE *' AND NOT iv_statement CP '* GE TYPE *'.
      rv_old = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_001 = mv_001
      mv_002 = mv_002
      mv_003 = mv_003
      mv_004 = mv_004
      mv_005 = mv_005
      mv_006 = mv_006
      mv_007 = mv_007
      mv_008 = mv_008
      mv_009 = mv_009
      mv_010 = mv_010
      mv_011 = mv_011
      mv_012 = mv_012
      mv_013 = mv_013
      mv_014 = mv_014
      mv_015 = mv_015
      mv_016 = mv_016
      mv_017 = mv_017
      mv_018 = mv_018
      mv_019 = mv_019
      mv_020 = mv_020
      mv_021 = mv_021
      mv_022 = mv_022
      mv_023 = mv_023
      mv_024 = mv_024
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
