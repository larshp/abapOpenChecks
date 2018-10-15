CLASS zcl_aoc_check_68 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS inform_from_sy .
ENDCLASS.



CLASS ZCL_AOC_CHECK_68 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '068'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'IDOC' ).
    add_obj_type( 'IEXT' ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = '&1'.                                      "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD inform_from_sy.

    DATA: lv_str TYPE string.


    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_str.

    inform( p_sub_obj_type = object_type
            p_sub_obj_name = object_name
            p_test         = myname
            p_kind         = mv_errty
            p_code         = '001'
            p_param_1      = lv_str ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA:
      lv_idoctyp    TYPE edi_iapi00-idoctyp,
      ls_attributes TYPE edi_iapi01,
      lv_cimtyp     TYPE edi_iapi00-cimtyp.


    CASE object_type.
      WHEN 'IDOC'.
        lv_idoctyp = object_name.

        CALL FUNCTION 'IDOCTYPE_EXISTENCE_CHECK'
          EXPORTING
            pi_idoctyp       = lv_idoctyp
          EXCEPTIONS
            object_not_found = 1
            db_error         = 2
            OTHERS           = 3.
        IF sy-subrc = 1.
          RETURN.
        ELSEIF sy-subrc <> 0.
          inform_from_sy( ).
          RETURN.
        ENDIF.

      WHEN 'IEXT'.
        lv_cimtyp = object_name.

        CALL FUNCTION 'EXTTYPE_EXISTENCE_CHECK'
          EXPORTING
            pi_cimtyp        = lv_cimtyp
            pi_read_devc     = abap_false
          IMPORTING
            pe_attributes    = ls_attributes
          EXCEPTIONS
            object_not_found = 1
            db_error         = 2
            OTHERS           = 3.
        IF sy-subrc = 1.
          RETURN.
        ELSEIF sy-subrc <> 0.
          inform_from_sy( ).
          RETURN.
        ENDIF.
        lv_idoctyp = ls_attributes-idoctyp.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CALL FUNCTION 'IDOCTYPE_READ_COMPLETE'
      EXPORTING
        pi_idoctyp         = lv_idoctyp
        pi_cimtyp          = lv_cimtyp
      EXCEPTIONS
        object_unknown     = 1
        segment_unknown    = 2
        relation_not_found = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      inform_from_sy( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
