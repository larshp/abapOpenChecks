CLASS zcl_aoc_check_17 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.

  PROTECTED SECTION.
    DATA mv_types TYPE i.
    DATA mv_define TYPE i.
    DATA mv_constants TYPE i.
    DATA mv_data TYPE i.
    DATA mv_fs TYPE i.
    DATA mv_statics TYPE i.

    TYPE-POOLS abap.
    METHODS check_mode
      IMPORTING
        !iv_type       TYPE i
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

  PRIVATE SECTION.
    DATA ms_statement TYPE sstmnt.
    DATA ms_token TYPE stokesx.
    DATA mv_mode TYPE i.
ENDCLASS.



CLASS ZCL_AOC_CHECK_17 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_exit   TYPE abap_bool,
          lv_define TYPE abap_bool,
          lv_others TYPE i.

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures.


    lv_others = mv_constants + mv_data + mv_fs + mv_statics + mv_types + mv_define.

    LOOP AT it_structures ASSIGNING <ls_structure>
        WHERE type = scan_struc_type-routine.

      mv_mode = 0.

      LOOP AT it_statements INTO ms_statement
          FROM <ls_structure>-stmnt_from + 1
          TO <ls_structure>-stmnt_to - 1
          WHERE type <> scan_stmnt_type-macro_call.

        READ TABLE it_tokens INTO ms_token INDEX ms_statement-from.
        IF sy-subrc <> 0
            OR ms_token-type = scan_token_type-comment
            OR ms_token-type = scan_token_type-pragma.
          CONTINUE. " current loop
        ENDIF.

* skip INCLUDE if it is part of TYPE definition
        IF mv_mode = mv_types AND ms_token-str = 'INCLUDE'.
          CONTINUE.
        ENDIF.

        IF lv_define = abap_true AND ms_token-str = 'END-OF-DEFINITION'.
          lv_define = abap_false.
          CONTINUE.
        ELSEIF lv_define = abap_true.
          CONTINUE. " current loop.
        ENDIF.

        CASE ms_token-str.
          WHEN 'TYPE' OR 'TYPES'.
            lv_exit = check_mode( mv_types ).
          WHEN 'CONSTANT' OR 'CONSTANTS'.
            lv_exit = check_mode( mv_constants ).
          WHEN 'DATA'.
            lv_exit = check_mode( mv_data ).
          WHEN 'FIELD-SYMBOLS'.
            lv_exit = check_mode( mv_fs ).
          WHEN 'STATICS'.
            lv_exit = check_mode( mv_statics ).
          WHEN 'DEFINE'.
            lv_exit = check_mode( mv_define ).
            lv_define = abap_true.
          WHEN OTHERS.
            lv_exit = check_mode( lv_others ).
        ENDCASE.

        IF lv_exit = abap_true.
          EXIT. " current loop
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_mode.

    DATA: lv_include TYPE program.


    IF mv_mode > iv_type.
      rv_exit = abap_true.

      lv_include = get_include( p_level = ms_statement-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line = ms_token-row
              p_kind = mv_errty
              p_test = myname
              p_code = '001' ).
    ENDIF.

    mv_mode = iv_type.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Definitions in top of routine'.       "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '017'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_types     = 1.
    mv_define    = 2.
    mv_constants = 2.
    mv_statics   = 2.
    mv_data      = 2.
    mv_fs        = 3.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty     = mv_errty
      mv_constants = mv_constants
      mv_data      = mv_data
      mv_fs        = mv_fs
      mv_statics   = mv_statics
      mv_types     = mv_types
      mv_define    = mv_define
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Reorder definitions to top of routine'.   "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_types 'TYPES' ''.                     "#EC NOTEXT
    zzaoc_fill_att mv_define 'DEFINE' ''.                   "#EC NOTEXT
    zzaoc_fill_att mv_constants 'CONSTANTS' ''.             "#EC NOTEXT
    zzaoc_fill_att mv_data 'DATA' ''.                       "#EC NOTEXT
    zzaoc_fill_att mv_statics 'STATICS' ''.                 "#EC NOTEXT
    zzaoc_fill_att mv_fs 'FIELD-SYMBOLS' ''.                "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty     = mv_errty
      mv_constants = mv_constants
      mv_data      = mv_data
      mv_fs        = mv_fs
      mv_statics   = mv_statics
      mv_types     = mv_types
      mv_define    = mv_define
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
