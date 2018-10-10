CLASS zcl_aoc_check_79 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_method,
        clsname TYPE seoclsname,
        cpdname TYPE seocpdname,
        include TYPE programm,
      END OF ty_method .
    TYPES:
      ty_methods_tt TYPE STANDARD TABLE OF ty_method WITH DEFAULT KEY .

    DATA mt_compiler TYPE scr_refs .
    DATA mt_statements TYPE ty_statements .

    METHODS check_local
      IMPORTING
        !is_method      TYPE ty_method
        !is_local       TYPE scr_ref
      RETURNING
        VALUE(rv_error) TYPE abap_bool .
    METHODS find_writes
      IMPORTING
        !is_method       TYPE ty_method
        !is_local        TYPE scr_ref
      RETURNING
        VALUE(rt_writes) TYPE scr_refs .
    METHODS initialize
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab .
    METHODS find_locals
      IMPORTING
        !is_method       TYPE ty_method
      RETURNING
        VALUE(rt_locals) TYPE scr_refs .
    METHODS find_methods
      IMPORTING
        !it_levels        TYPE slevel_tab
      RETURNING
        VALUE(rt_methods) TYPE ty_methods_tt .
ENDCLASS.



CLASS ZCL_AOC_CHECK_79 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: ls_level   LIKE LINE OF it_levels,
          lt_methods TYPE ty_methods_tt,
          lt_locals  TYPE scr_refs,
          lv_error   TYPE abap_bool,
          ls_local   LIKE LINE OF lt_locals,
          ls_method  LIKE LINE OF lt_methods.


* Only consider local variables in global methods
    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    initialize(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

    lt_methods = find_methods( it_levels ).

    LOOP AT lt_methods INTO ls_method.
      lt_locals = find_locals( ls_method ).

      LOOP AT lt_locals INTO ls_local.

        IF ls_local-name = 'FOO'.
          BREAK-POINT.
        ENDIF.

        lv_error = check_local(
          is_method = ls_method
          is_local  = ls_local ).
        IF lv_error = abap_true.
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = ls_method-include
                  p_line         = ls_local-line
                  p_kind         = mv_errty
                  p_test         = myname
                  p_param_1      = ls_local-name
                  p_code         = '001' ).
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_local.

    DATA: lt_writes    TYPE scr_refs,
          ls_statement LIKE LINE OF mt_statements,
          ls_first     LIKE LINE OF lt_writes.


    lt_writes = find_writes( is_method = is_method
                             is_local  = is_local ).

    READ TABLE lt_writes INDEX 1 INTO ls_first.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* assumption: max one statement per line
    READ TABLE mt_statements INTO ls_statement WITH KEY
      include = is_method-include
      start-row = ls_first-statement->start_line.
* note that for changed statements it might not find the correct STR, but this is okay
    IF sy-subrc <> 0 OR ( ls_statement-str NP 'CLEAR *'
        AND ls_statement-str NP 'REFRESH *'
        AND ls_statement-str NP 'FREE *' ).
      RETURN.
    ENDIF.

    LOOP AT mt_statements INTO ls_statement WHERE include = is_method-include
        AND start-row < ls_first-line.
      IF ls_statement-str CP 'SELECT SINGLE *'
          OR ls_statement-str CP 'SELECT * INTO TABLE *'
          OR ls_statement-str CP 'SELECT * INTO CORRESPONDING FIELDS OF TABLE *'
          OR ls_statement-str CP 'SELECT * APPENDING TABLE *'
          OR ls_statement-str CP 'SELECT * APPENDING CORRESPONDING FIELDS OF TABLE *'.
        CONTINUE.
      ENDIF.
      IF ls_statement-str CP 'LOOP AT *'
          OR ls_statement-str CP 'WHILE *'
          OR ls_statement-str CP 'SELECT *'
          OR ls_statement-str CP 'DO *'.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_error = abap_true.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    category    = 'ZCL_AOC_CATEGORY'.
    version     = '001'.
    position    = '079'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.


  METHOD find_locals.

    rt_locals = mt_compiler.

    DELETE rt_locals WHERE grade <> cl_abap_compiler=>grade_definition
      OR mode2 <> '2'
      OR statement->source_info->name <> is_method-include.

  ENDMETHOD.


  METHOD find_methods.

    DATA: ls_mtdkey TYPE seocpdkey,
          ls_method LIKE LINE OF rt_methods,
          ls_level  LIKE LINE OF it_levels.


    LOOP AT it_levels INTO ls_level.

      cl_oo_classname_service=>get_method_by_include(
        EXPORTING
          incname             = ls_level-name
        RECEIVING
          mtdkey              = ls_mtdkey
        EXCEPTIONS
          class_not_existing  = 1
          method_not_existing = 2
          OTHERS              = 3 ).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR ls_method.
      MOVE-CORRESPONDING ls_mtdkey TO ls_method.
      ls_method-include = ls_level-name.

      APPEND ls_method TO rt_methods.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_writes.

    rt_writes = mt_compiler.

    DELETE rt_writes WHERE full_name <> is_local-full_name
      OR ( mode2 <> '6' AND mode2 <> '9' )
      OR statement->source_info->name <> is_method-include.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'CLEAR as first of variable, &1'.          "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD initialize.

    mt_compiler = get_compiler( ).

    mt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

  ENDMETHOD.
ENDCLASS.
