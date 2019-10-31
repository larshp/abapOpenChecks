CLASS zcl_aoc_check_14 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_one_per_include TYPE sap_bool .
  PRIVATE SECTION.

    METHODS parse
      IMPORTING
        !it_commented      TYPE string_table
        !is_level          TYPE slevel
        !iv_line           TYPE token_row
      RETURNING
        VALUE(rv_informed) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_AOC_CHECK_14 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_code      TYPE string_table,
          lt_commented TYPE string_table,
          lv_informed  TYPE abap_bool,
          lv_line      TYPE token_row.

    FIELD-SYMBOLS: <ls_level> LIKE LINE OF io_scan->levels,
                   <lv_code>  LIKE LINE OF lt_code.


    LOOP AT io_scan->levels ASSIGNING <ls_level> WHERE type = io_scan->gc_level-program.
      lt_code = get_source( <ls_level> ).

      LOOP AT lt_code ASSIGNING <lv_code>.

        IF strlen( <lv_code> ) > 1 AND <lv_code>(1) = '*'.
          IF lines( lt_commented ) = 0.
            lv_line = sy-tabix.
          ENDIF.
          APPEND <lv_code>+1 TO lt_commented.
        ELSE.
          lv_informed = parse( it_commented = lt_commented
                               is_level     = <ls_level>
                               iv_line      = lv_line ).
          CLEAR lt_commented.

          IF lv_informed = abap_true AND mv_one_per_include = abap_true.
            EXIT. " current loop
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF lv_informed = abap_true AND mv_one_per_include = abap_true.
        CONTINUE.
      ENDIF.

      parse( it_commented = lt_commented
             is_level     = <ls_level>
             iv_line      = lv_line ).
      CLEAR lt_commented.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '014'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_one_per_include = abap_false.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Commented code'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty           = mv_errty
      mv_one_per_include = mv_one_per_include
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_one_per_include 'One finding per include' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD parse.

    DATA: lv_code LIKE LINE OF it_commented.

    FIELD-SYMBOLS: <lv_commented> LIKE LINE OF it_commented.


    IF lines( it_commented ) = 0.
      RETURN.
    ENDIF.

* skip auto generated INCLUDEs in function group top include
    IF ( is_level-name CP 'LY*TOP' OR is_level-name CP 'LZ*TOP' )
        AND lines( it_commented ) = 1.
      READ TABLE it_commented INDEX 1 INTO lv_code.
      ASSERT sy-subrc = 0.

      IF lv_code CS 'INCLUDE L'.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT it_commented ASSIGNING <lv_commented>.
      IF strlen( <lv_commented> ) > 1 AND <lv_commented>(2) = '"*'.
        RETURN.
      ENDIF.
      IF strlen( <lv_commented> ) > 0 AND <lv_commented>(1) = '*'.
        RETURN.
      ENDIF.
      IF strlen( <lv_commented> ) > 4 AND <lv_commented>(5) = '-----'.
        RETURN.
      ENDIF.
      IF strlen( <lv_commented> ) > 4 AND <lv_commented>(5) = ' ===='.
        RETURN.
      ENDIF.
      IF strlen( <lv_commented> ) > 4 AND <lv_commented>(5) = '*****'.
        RETURN.
      ENDIF.
      IF lines( it_commented ) = 1 AND <lv_commented> CO '. '.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF zcl_aoc_parser=>run( it_commented )-match = abap_true.
      inform( p_sub_obj_name = is_level-name
              p_line         = iv_line
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
      rv_informed = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty           = mv_errty
      mv_one_per_include = mv_one_per_include
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
