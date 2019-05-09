CLASS zcl_aoc_check_91 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.

  PROTECTED SECTION.
    DATA mv_maxlength TYPE i.

  PRIVATE SECTION.


ENDCLASS.

CLASS zcl_aoc_check_91 IMPLEMENTATION.

  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures.
    DATA ls_statement TYPE sstmnt.
    DATA ls_token TYPE stokesx.
    DATA lv_row TYPE token_row.
    DATA lv_count TYPE i.
    DATA lv_include TYPE program.

    LOOP AT it_structures ASSIGNING <ls_structure>
        WHERE type = scan_struc_type-routine.

      LOOP AT it_statements INTO ls_statement
          FROM <ls_structure>-stmnt_from + 1
          TO <ls_structure>-stmnt_to - 1
          WHERE type <> scan_stmnt_type-macro_call.

        READ TABLE it_tokens INTO ls_token INDEX ls_statement-from.
        IF sy-subrc <> 0
        OR ls_token-type = scan_token_type-comment
        OR ls_token-type = scan_token_type-pragma.
          CONTINUE. " current loop
        ENDIF.

        "line of first statement in block
        IF lv_row IS INITIAL.
          lv_row = ls_token-row.
        ENDIF.

        lv_count = lv_count + 1.
      ENDLOOP.

      IF lv_count > mv_maxlength.
        lv_include = get_include( p_level = ls_statement-level ).

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line         = lv_row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001'
                p_param_1      = |{ lv_count } > { mv_maxlength }| ).

        RETURN.
      ENDIF.

      CLEAR lv_count.
      CLEAR lv_row.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    DATA ls_scimessage TYPE scimessage.

    super->constructor( ).

    version        = '001'.
    position       = '091'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.
    mv_maxlength = 50.

    ls_scimessage-test = myname.
    ls_scimessage-code = '001'.
    ls_scimessage-kind = c_error.
    ls_scimessage-text = 'Maximum statements per processing block exceeded: &1'(m01).
    ls_scimessage-pcom = ''.
    ls_scimessage-pcom_alt = ''.

    INSERT ls_scimessage INTO TABLE scimessages.

  ENDMETHOD.                    "CONSTRUCTOR

  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_maxlength = mv_maxlength
      TO DATA BUFFER p_attributes.

  ENDMETHOD.

  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_maxlength 'Max. number of statements' ''. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.

  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_maxlength = mv_maxlength
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED

    ASSERT sy-subrc = 0.

  ENDMETHOD.





ENDCLASS.
