CLASS zcl_aoc_check_61 DEFINITION
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

    TYPES:
      ty_seosubcodf_tt TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY .
    TYPES:
      ty_vseosubcdf_tt TYPE STANDARD TABLE OF vseosubcdf WITH DEFAULT KEY .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_61 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_compiler TYPE scr_refs.

* _Sinks_
* method propagates exception
* TRY-CATCH block
*
* _Sources_
* method calls
* RAISE

    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    lt_compiler = get_compiler( ).

    DELETE lt_compiler WHERE tag <> 'ME' OR mode2 <> '5'.

    BREAK-POINT.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'WIP, Exception caught but not raised'. "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '061'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty      = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Parameter &1 not supplied anywhere'.      "#EC NOTEXT
      WHEN '002'.
        p_text = 'Parameter &1 not supplied anywhere, method &2'. "#EC NOTEXT
      WHEN '003'.
        p_text = 'Parameter &1 not referenced in method'.   "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
