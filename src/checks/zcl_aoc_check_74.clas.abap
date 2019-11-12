CLASS zcl_aoc_check_74 DEFINITION
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

    DATA mv_depth TYPE int4 .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_74 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE zcl_aoc_scan=>ty_statements,
          lv_max        TYPE i,
          ls_max        LIKE LINE OF lt_statements,
          lv_level      TYPE i,
          lv_char10     TYPE c LENGTH 10,
          lv_depth      TYPE i.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF io_scan->levels,
                   <ls_statement> LIKE LINE OF lt_statements.

* todo, test how this works with macros

    lt_statements = io_scan->build_statements( ).

    LOOP AT io_scan->levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.
      lv_depth = 0.
      lv_max = 0.

      LOOP AT lt_statements ASSIGNING <ls_statement> WHERE level = lv_level.

        IF <ls_statement>-str CP 'IF *'
            OR <ls_statement>-str CP 'CASE *'
            OR <ls_statement>-str CP 'WHILE *'
            OR <ls_statement>-str CP 'LOOP *'
            OR <ls_statement>-str CP 'DO *'
            OR <ls_statement>-str CP 'TRY *'.
          lv_depth = lv_depth + 1.
        ENDIF.

        CASE <ls_statement>-str.
          WHEN 'ENDIF' OR 'ENDCASE' OR 'ENDWHILE' OR 'ENDLOOP' OR 'ENDDO' OR 'ENDTRY'.
            lv_depth = lv_depth - 1.
        ENDCASE.

        IF lv_depth > lv_max.
          lv_max = lv_depth.
          ls_max = <ls_statement>.
        ENDIF.

      ENDLOOP.

      IF lv_max >= mv_depth.
        lv_char10 = lv_max.
        inform( p_sub_obj_name = ls_max-include
                p_line         = ls_max-start-row
                p_kind         = mv_errty
                p_test         = myname
                p_param_1      = condense( lv_char10 )
                p_code         = '001' ).
* one finding per include
        EXIT. " current loop
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '074'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_depth = 4.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Deep nesting, depth = &1'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_depth = mv_depth
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_depth 'Depth' ''.                     "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_depth = mv_depth
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
