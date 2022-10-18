CLASS zcl_aoc_util_programs DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_programs_in_package
      IMPORTING
        !iv_devclass           TYPE devclass
        !iv_ignore_mview_fugr  TYPE abap_bool DEFAULT abap_false
        !iv_ignore_local_tests TYPE abap_bool DEFAULT abap_false
        !iv_ignore_gateway     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_programs)     TYPE scit_program .
  PROTECTED SECTION.

    CLASS-METHODS class_includes
      IMPORTING
        !iv_class              TYPE tadir-obj_name
        !iv_ignore_local_tests TYPE abap_bool
      RETURNING
        VALUE(rt_programs)     TYPE scit_program .
    CLASS-METHODS function_group_includes
      IMPORTING
        !iv_group          TYPE tadir-obj_name
      RETURNING
        VALUE(rt_programs) TYPE scit_program .
    CLASS-METHODS function_group_main
      IMPORTING
        !iv_group         TYPE tadir-obj_name
      RETURNING
        VALUE(rv_program) TYPE program .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_util_programs IMPLEMENTATION.


  METHOD class_includes.

    DATA: lv_class   TYPE seoclsname,
          lv_program TYPE program.


    lv_class = iv_class.

    cl_oo_classname_service=>get_all_class_includes(
      EXPORTING
        class_name    = lv_class
      RECEIVING
        result        = rt_programs
      EXCEPTIONS
        no_such_class = 1
        OTHERS        = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF iv_ignore_local_tests = abap_true.
      lv_program = cl_oo_classname_service=>get_ccau_name( lv_class ).
      DELETE rt_programs WHERE table_line = lv_program.
    ENDIF.

  ENDMETHOD.


  METHOD function_group_includes.

    DATA: lv_main TYPE program.


    lv_main = function_group_main( iv_group ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_main
      TABLES
        includetab   = rt_programs
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3. "#EC CI_SUBRC

  ENDMETHOD.


  METHOD function_group_main.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_group     TYPE rs38l-area.


    lv_area = iv_group.

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

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.

  ENDMETHOD.


  METHOD get_programs_in_package.

    TYPES: BEGIN OF ty_tadir,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
           END OF ty_tadir.

    DATA: lv_area  TYPE tvdir-area,
          lt_tadir TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    SELECT object obj_name FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      WHERE pgmid = 'R3TR'
      AND ( object = 'CLAS' OR object = 'PROG' OR object = 'FUGR' )
      AND devclass = iv_devclass
      ORDER BY object ASCENDING
               obj_name ASCENDING.        "#EC CI_GENBUFF "#EC CI_SUBRC

    IF iv_ignore_gateway = abap_true.
      DELETE lt_tadir WHERE obj_name CP '*_DPC' AND object = 'CLAS'.
      DELETE lt_tadir WHERE obj_name CP '*_MPC' AND object = 'CLAS'.
    ENDIF.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'CLAS'.
          APPEND LINES OF class_includes(
            iv_class = <ls_tadir>-obj_name
            iv_ignore_local_tests = iv_ignore_local_tests ) TO rt_programs.
        WHEN 'PROG'.
          APPEND <ls_tadir>-obj_name TO rt_programs.
        WHEN 'FUGR'.
          IF iv_ignore_mview_fugr = abap_true.
            SELECT SINGLE area FROM tvdir INTO lv_area
              WHERE area = <ls_tadir>-obj_name.         "#EC CI_GENBUFF
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          APPEND LINES OF function_group_includes( <ls_tadir>-obj_name ) TO rt_programs.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
