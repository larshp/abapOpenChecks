class ZCL_AOC_CHECK_59 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_59 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

* todo

*        inform( p_sub_obj_type = c_type_include
*                p_sub_obj_name = lv_include
*                p_kind         = mv_errty
*                p_test         = myname
*                p_code         = '001' ).


  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Boolean expression structure'.        "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '059'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'todo'.                                    "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
