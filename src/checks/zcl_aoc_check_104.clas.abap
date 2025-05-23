"! <p class="shorttext synchronized">104 - Local existence of function modules called via RFC</p>
CLASS zcl_aoc_check_104 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_system TYPE REF TO zif_aoc_system OPTIONAL.

    METHODS check REDEFINITION.

  PRIVATE SECTION.
    DATA mo_system TYPE REF TO zif_aoc_system.
ENDCLASS.


CLASS zcl_aoc_check_104 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    mo_system = COND #( WHEN io_system IS BOUND
                        THEN io_system
                        ELSE NEW zcl_aoc_local_system( ) ).

    version = '001'.
    position = '104'.

    insert_scimessage( iv_code = gc_code-function_module_does_not_exist
                       iv_text = TEXT-001 ).
  ENDMETHOD.


  METHOD check.
    LOOP AT io_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statement>).
      LOOP AT io_scan->tokens
           TRANSPORTING NO FIELDS
           FROM <ls_statement>-from TO <ls_statement>-to
           WHERE str = zcl_aoc_scan=>gc_keyword-destination.

        TRY.
            IF io_scan->tokens[ <ls_statement>-from ]-str = zcl_aoc_scan=>gc_keyword-call
                AND io_scan->tokens[ <ls_statement>-from + 1 ]-str = zcl_aoc_scan=>gc_keyword-function.

              ASSIGN io_scan->tokens[ <ls_statement>-from + 2 ] TO FIELD-SYMBOL(<ls_token_function_name>).

              DATA(lv_function_module_name_length) = strlen( <ls_token_function_name>-str ) - 2.
              DATA(lv_function_module_name) = <ls_token_function_name>-str+1(lv_function_module_name_length).

              IF mo_system->is_function_module_existing( CONV #( lv_function_module_name ) ) = abap_false.
                DATA(lv_include) = io_scan->get_include( <ls_statement>-level ).

                inform( p_sub_obj_name = lv_include
                        p_line         = <ls_token_function_name>-row
                        p_kind         = mv_errty
                        p_test         = myname
                        p_code         = gc_code-function_module_does_not_exist ).
              ENDIF.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            " Does not match function module call syntax, ignore
            CONTINUE.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
