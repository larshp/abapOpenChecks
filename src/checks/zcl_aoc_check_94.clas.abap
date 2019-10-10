CLASS zcl_aoc_check_94 DEFINITION
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
ENDCLASS.



CLASS ZCL_AOC_CHECK_94 IMPLEMENTATION.


  METHOD check.
    DATA lv_idx TYPE i.
    FIELD-SYMBOLS: <ls_token>      LIKE LINE OF io_scan->tokens,
                   <ls_token_next> LIKE LINE OF io_scan->tokens.

    LOOP AT io_scan->tokens ASSIGNING <ls_token> WHERE str = 'LIKE'.
      lv_idx = sy-tabix + 1.
      READ TABLE io_scan->tokens ASSIGNING <ls_token_next> INDEX lv_idx.
      IF <ls_token_next>-str CA '*' AND
         <ls_token_next>-str NA '%'.
        inform( p_sub_obj_name = object_name
                p_line         = <ls_token_next>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.

    LOOP AT io_scan->tokens ASSIGNING <ls_token> WHERE str = 'CP' OR str = 'NP'.
      lv_idx = sy-tabix + 1.
      READ TABLE io_scan->tokens ASSIGNING <ls_token_next> INDEX lv_idx.
      IF <ls_token_next>-str CA '%' AND
         <ls_token_next>-str NA '*'.
        inform( p_sub_obj_name = object_name
                p_line         = <ls_token_next>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '002' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '094'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

  ENDMETHOD.


  METHOD get_message_text.
    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Use of LIKE with wrong wildcard'.         "#EC NOTEXT
      WHEN '002'.
        p_text = 'Use of CP/NP with wrong wildcard'.        "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
