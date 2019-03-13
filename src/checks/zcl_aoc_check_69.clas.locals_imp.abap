CLASS lcl_stack IMPLEMENTATION.

  METHOD push.
    APPEND iv_string TO mt_data.
  ENDMETHOD.

  METHOD concatenate.
    rv_string = concat_lines_of( mt_data ) && iv_string.
  ENDMETHOD.

  METHOD set.
    clear( ).
    push( iv_string ).
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_data.
  ENDMETHOD.

  METHOD pop.
    IF lines( mt_data ) > 0.
      DELETE mt_data INDEX lines( mt_data ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
