CLASS zcl_aoc_check_35 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_t100_tt TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.

    METHODS analyze
      IMPORTING
        !it_t100       TYPE ty_t100_tt
      RETURNING
        VALUE(rv_code) TYPE sci_errc.
ENDCLASS.



CLASS zcl_aoc_check_35 IMPLEMENTATION.


  METHOD analyze.

    TYPES: BEGIN OF ty_cross,
             name TYPE cross-name,
           END OF ty_cross.

    DATA: lt_cross TYPE TABLE OF ty_cross,
          lt_name  TYPE TABLE OF cross-name,
          lv_name  LIKE LINE OF lt_name.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF it_t100.


    LOOP AT it_t100 ASSIGNING <ls_t100>.
      CONCATENATE <ls_t100>-arbgb <ls_t100>-msgnr INTO lv_name RESPECTING BLANKS.
      APPEND lv_name TO lt_name.
    ENDLOOP.

    ASSERT lines( lt_name ) > 0.
    SELECT name FROM cross INTO TABLE lt_cross
       FOR ALL ENTRIES IN lt_name
      WHERE ( type = '3' OR type = 'N' )
      AND name = lt_name-table_line.                      "#EC CI_SUBRC
    SORT lt_cross BY name ASCENDING.

    LOOP AT it_t100 ASSIGNING <ls_t100>.

      CONCATENATE <ls_t100>-arbgb <ls_t100>-msgnr
        INTO lv_name RESPECTING BLANKS.
      READ TABLE lt_cross WITH KEY name = lv_name
        BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      SELECT COUNT( * ) FROM dd08l
        WHERE arbgb = <ls_t100>-arbgb
        AND msgnr = <ls_t100>-msgnr ##WARN_OK.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      rv_code = '001'.
      inform( p_sub_obj_type = object_type
              p_sub_obj_name = object_name
              p_test         = myname
              p_kind         = mv_errty
              p_code         = rv_code
              p_param_1      = <ls_t100>-msgnr ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '035'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'MSAG' ).

    insert_scimessage(
      iv_code = '001'
      iv_text = 'Message not in use, &1'(m01) ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_t100 TYPE ty_t100_tt.


    IF object_type <> 'MSAG'.
      RETURN.
    ENDIF.

    SELECT * FROM t100 INTO TABLE lt_t100
      WHERE sprsl = sy-langu
      AND arbgb = object_name
      ORDER BY PRIMARY KEY.                             "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    analyze( lt_t100 ).

  ENDMETHOD.
ENDCLASS.
