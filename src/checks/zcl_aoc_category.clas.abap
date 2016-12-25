CLASS zcl_aoc_category DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_category_root
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor,
      if_ci_test~display_documentation REDEFINITION.

ENDCLASS.

CLASS zcl_aoc_category IMPLEMENTATION.

  METHOD constructor.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    super->constructor( ).
    description = 'abapOpenChecks'.                         "#EC NOTEXT
    category    = 'CL_CI_CATEGORY_TOP'.
    position    = '999'.

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    DATA: lv_object TYPE dokhl-object.


    lv_object = myname.

    CALL FUNCTION 'DOCU_CALL'
      EXPORTING
        displ      = abap_true
        displ_mode = 2
        id         = 'CL'
        langu      = 'E'
        object     = lv_object.

  ENDMETHOD.
ENDCLASS.