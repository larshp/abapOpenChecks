CLASS zcl_aoc_check_97 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_97 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '097'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Text of Field &1 doesn''t refer to the dictionary.'(m01) ).

  ENDMETHOD.


  METHOD run.
    DATA: lv_masterlang TYPE tadir-masterlang,
          lt_selpars    TYPE rsel_paras_t,
          lt_texts      TYPE STANDARD TABLE OF textpool.
    FIELD-SYMBOLS: <ls_selpar> TYPE rsel_paras,
                   <ls_text>   TYPE textpool.
    CALL FUNCTION 'SELOPTS_AND_PARAMS'
      EXPORTING
        program              = program_name
      TABLES
        selpars              = lt_selpars
      EXCEPTIONS
        program_non_existent = 1
        subroutine_pool      = 2
        load_problems        = 3
        OTHERS               = 4.
    IF sy-subrc = 0 AND lt_selpars[] IS NOT INITIAL.
      SELECT SINGLE masterlang
        FROM tadir
        INTO lv_masterlang
        WHERE obj_name = program_name
        AND object = object_type.

      READ TEXTPOOL program_name LANGUAGE lv_masterlang INTO lt_texts.

      LOOP AT lt_selpars ASSIGNING <ls_selpar>
                         WHERE dbfield <> ''.
        READ TABLE lt_texts ASSIGNING <ls_text>
                            WITH KEY key = <ls_selpar>-name.
        IF sy-subrc = 0 AND <ls_text>-entry(1) <> 'D'.
          inform( p_kind    = mv_errty
                  p_test    = myname
                  p_code    = '001'
                  p_param_1 = <ls_selpar>-name ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
