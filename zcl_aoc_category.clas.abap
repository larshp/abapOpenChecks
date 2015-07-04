class ZCL_AOC_CATEGORY definition
  public
  inheriting from CL_CI_CATEGORY_ROOT
  final
  create public .

public section.
*"* public components of class ZCL_AOC_CATEGORY
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CATEGORY
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_AOC_CATEGORY IMPLEMENTATION.


METHOD constructor.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  super->constructor( ).
  description = 'abapOpenChecks'.                           "#EC NOTEXT
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