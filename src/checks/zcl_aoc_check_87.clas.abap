CLASS zcl_aoc_check_87 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.

    METHODS check_screen
      IMPORTING
        !is_d020s TYPE d020s .
    METHODS determine_program
      RETURNING
        VALUE(rv_progname) TYPE program .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_87 IMPLEMENTATION.


  METHOD check_screen.

    DATA: ls_h TYPE d020s,
          lt_f TYPE STANDARD TABLE OF d021s WITH DEFAULT KEY,
          lt_e TYPE STANDARD TABLE OF d022s WITH DEFAULT KEY ##NEEDED,
          lt_m TYPE STANDARD TABLE OF d023s WITH DEFAULT KEY ##NEEDED,
          BEGIN OF ls_id,
            p TYPE progname,
            d TYPE sydynnr,
          END OF ls_id.


    ls_id-p = is_d020s-prog.
    ls_id-d = is_d020s-dnum.

    IMPORT DYNPRO ls_h lt_f lt_e lt_m ID ls_id.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_SCRP_CHECK_FIELD_POSITIONS'
      EXPORTING
        maxlines           = ls_h-noli
        maxcolns           = ls_h-noco
      TABLES
        field_list         = lt_f
      EXCEPTIONS
        extends_boundaries = 1
        overlapped         = 2
        overlapped_in_tc   = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = is_d020s-dnum ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '087'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Overlaps, errros, screen &1'(m01) ).

  ENDMETHOD.


  METHOD determine_program.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_subc      TYPE progdir-subc,
          lv_group     TYPE rs38l-area.


    IF object_type = 'PROG'.
      rv_progname = object_name.
      RETURN.
    ENDIF.

* else it is a function group

    lv_area = object_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_progname.

    SELECT SINGLE subc FROM progdir INTO lv_subc
      WHERE name = rv_progname AND state = 'A' AND subc = 'F'.
    IF sy-subrc <> 0.
      CLEAR rv_progname.
    ENDIF.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_d020s   TYPE TABLE OF d020s,
          lv_program TYPE program.

    FIELD-SYMBOLS: <ls_d020s> LIKE LINE OF lt_d020s.


    lv_program = determine_program( ).
    IF lv_program IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = lv_program
      TABLES
        dynpros   = lt_d020s
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_d020s ASSIGNING <ls_d020s>.
      check_screen( <ls_d020s> ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
