CLASS zcl_aoc_check_15 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_15 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_position  TYPE i,
          lv_include   TYPE sobj_name,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type <> io_scan->gc_statement-empty
        AND type <> io_scan->gc_statement-comment.

      lv_position = sy-tabix.
      CLEAR lv_statement.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type <> io_scan->gc_token-comment.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_statement CP 'CALL FUNCTION *'
          OR lv_statement CP 'CALL METHOD *'
          OR lv_statement CP 'CALL CUSTOMER-FUNCTION *'
          OR lv_statement CP 'CALL SCREEN *'
          OR lv_statement CP 'CALL SELECTION-SCREEN *'
          OR lv_statement CP 'CALL TRANSACTION *'
          OR lv_statement CP 'CALL TRANSFORMATION *'
          OR lv_statement CP 'CALL BADI *'.
        CONTINUE.
      ELSEIF lv_statement CP 'CALL *'
          OR lv_statement CP 'SYSTEM-CALL *'.
        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_position     = lv_position
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '015'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Kernel CALL'(m01) ).

  ENDMETHOD.
ENDCLASS.
