CLASS zcl_aoc_check_43 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_call,
        class        TYPE seoclsname,
        method       TYPE string,
        start_line   TYPE i,
        start_column TYPE token_col,
        end_line     TYPE i,
        end_column   TYPE token_col,
        program      TYPE programm,
        level        TYPE stmnt_levl,
      END OF ty_call.
    TYPES:
      ty_call_tt TYPE SORTED TABLE OF ty_call WITH NON-UNIQUE KEY level start_line.
    TYPES:
      BEGIN OF ty_seosubcodf,
        clsname    TYPE seosubcodf-clsname,
        cmpname    TYPE seosubcodf-cmpname,
        sconame    TYPE seosubcodf-sconame,
        version    TYPE seosubcodf-version,
        paroptionl TYPE seosubcodf-paroptionl,
        parvalue   TYPE seosubcodf-parvalue,
        parpreferd TYPE seosubcodf-parpreferd,
      END OF ty_seosubcodf.
    TYPES:
      ty_seosubcodf_tt TYPE STANDARD TABLE OF ty_seosubcodf WITH DEFAULT KEY.

    METHODS read_parameters
      IMPORTING
        !iv_class            TYPE seoclsname
        !iv_method           TYPE string
      RETURNING
        VALUE(rt_parameters) TYPE ty_seosubcodf_tt.
    METHODS check_parameters
      IMPORTING
        !is_call TYPE ty_call
        !iv_code TYPE string.
    METHODS get_calls
      IMPORTING
        !it_levels      TYPE slevel_tab
      RETURNING
        VALUE(rt_calls) TYPE ty_call_tt.
ENDCLASS.



CLASS zcl_aoc_check_43 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_calls  TYPE ty_call_tt,
          lv_index  TYPE i,
          lv_foobar TYPE string,                            "#EC NEEDED
          lv_str    TYPE string.

    FIELD-SYMBOLS: <ls_top>       LIKE LINE OF io_scan->tokens,
                   <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_call>      LIKE LINE OF lt_calls,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    lt_calls = get_calls( io_scan->levels ).
    IF lt_calls IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT io_scan->statements ASSIGNING <ls_statement>.
      LOOP AT io_scan->tokens ASSIGNING <ls_top> FROM <ls_statement>-from TO <ls_statement>-to.
        lv_index = sy-tabix.

        LOOP AT lt_calls ASSIGNING <ls_call>
            WHERE level = <ls_statement>-level
            AND start_line = <ls_top>-row
            AND start_column = <ls_top>-col.

          CLEAR lv_str.
          LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM lv_index TO <ls_statement>-to.
            IF <ls_token>-row > <ls_call>-end_line
                OR ( <ls_token>-row = <ls_call>-end_line
                AND <ls_token>-col + strlen( <ls_token>-str ) >= <ls_call>-end_column ).
              EXIT.
            ENDIF.
            IF lv_str IS INITIAL.
              lv_str = <ls_token>-str.
            ELSE.
              CONCATENATE lv_str <ls_token>-str INTO lv_str SEPARATED BY space.
            ENDIF.
          ENDLOOP.

          SPLIT lv_str AT '(' INTO lv_foobar lv_str.
          IF lv_str <> ''.
            check_parameters(
              is_call = <ls_call>
              iv_code = lv_str ).
          ENDIF.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_parameters.

    TYPES: BEGIN OF ty_seosubcodf,
             clsname    TYPE seosubcodf-clsname,
             cmpname    TYPE seosubcodf-cmpname,
             sconame    TYPE seosubcodf-sconame,
             version    TYPE seosubcodf-version,
             paroptionl TYPE seosubcodf-paroptionl,
             parvalue   TYPE seosubcodf-parvalue,
             parpreferd TYPE seosubcodf-parpreferd,
           END OF ty_seosubcodf.

    DATA: lv_parameter  TYPE seosubcodf-sconame,
          lt_parameters TYPE TABLE OF ty_seosubcodf,
          lv_foobar     TYPE string,                        "#EC NEEDED
          lv_post       TYPE string.


    SPLIT iv_code AT '=' INTO lv_parameter lv_post.
    WHILE lv_post CA '('.
      REPLACE FIRST OCCURRENCE OF '(' IN lv_post WITH space.
      REPLACE FIRST OCCURRENCE OF ')' IN lv_post WITH space.
    ENDWHILE.
    SPLIT lv_post AT ')' INTO lv_post lv_foobar.
    IF lv_post IS INITIAL OR lv_post CA '='.
      RETURN.
    ENDIF.
    CONDENSE lv_parameter.

    lt_parameters = read_parameters(
      iv_class  = is_call-class
      iv_method = is_call-method ).

* in case there are multiple parameters filter out the optional
* if there is one importing parameter it is okay to be optional
    IF lines( lt_parameters ) > 1.
      DELETE lt_parameters WHERE ( paroptionl = abap_true OR parvalue <> '' )
        AND parpreferd = abap_false.
    ENDIF.

    IF lines( lt_parameters ) <> 1.
      RETURN.
    ENDIF.

    READ TABLE lt_parameters WITH KEY sconame = lv_parameter
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      inform( p_sub_obj_name = is_call-program
              p_line         = is_call-start_line
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = lv_parameter ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '043'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    insert_scimessage(
      iv_code = '001'
      iv_text = 'Parameter name &1 can be omitted'(m01) ).

  ENDMETHOD.


  METHOD get_calls.

    DATA: lt_result TYPE scr_refs,
          lv_foobar TYPE string,                            "#EC NEEDED
          ls_call   LIKE LINE OF rt_calls.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result.


    lt_result = zcl_aoc_compiler=>get_instance(
      iv_object_type = object_type
      iv_object_name = object_name )->get_result( ).
    DELETE lt_result WHERE tag <> cl_abap_compiler=>tag_method.

    LOOP AT lt_result ASSIGNING <ls_result>.
      CLEAR ls_call.
      <ls_result>-full_name = <ls_result>-full_name+4.
      SPLIT <ls_result>-full_name AT '\ME:' INTO <ls_result>-full_name lv_foobar.
      IF <ls_result>-full_name CP '*\IN:*'.
        SPLIT <ls_result>-full_name AT '\IN:' INTO lv_foobar ls_call-class.
      ELSE.
        ls_call-class = <ls_result>-full_name.
      ENDIF.
      ls_call-method = <ls_result>-name.

      ls_call-start_line   = <ls_result>-statement->start_line.
      ls_call-start_column = <ls_result>-statement->start_column.
      ls_call-end_line     = <ls_result>-statement->end_line.
      ls_call-end_column   = <ls_result>-statement->end_column.

      ls_call-program = <ls_result>-statement->source_info->name.

      READ TABLE it_levels WITH KEY name = ls_call-program TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ls_call-level = sy-tabix.

      INSERT ls_call INTO TABLE rt_calls.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_parameters.

    DATA: lv_super TYPE seometarel-refclsname.


    SELECT * FROM seosubcodf
      INTO CORRESPONDING FIELDS OF TABLE rt_parameters
      WHERE clsname = iv_class
      AND cmpname = iv_method
      AND pardecltyp = '0'
      AND type <> ''
      ORDER BY PRIMARY KEY.                   "#EC CI_ALL_FIELDS_NEEDED
    IF sy-subrc <> 0.
      SELECT SINGLE refclsname
        FROM seometarel
        INTO lv_super
        WHERE clsname = iv_class
        AND version <> seoc_version_inactive
        AND reltype = seor_reltype_inheritance ##WARN_OK. "#EC CI_NOORDER
      IF sy-subrc = 0 AND NOT lv_super IS INITIAL.
* try looking in super class
        rt_parameters = read_parameters(
          iv_class  = lv_super
          iv_method = iv_method ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
