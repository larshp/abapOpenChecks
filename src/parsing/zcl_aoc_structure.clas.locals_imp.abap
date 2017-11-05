*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_build DEFINITION DEFERRED.
CLASS zcl_aoc_structure DEFINITION LOCAL FRIENDS lcl_build.

*----------------------------------------------------------------------*
*       CLASS lcl_build DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_build DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_tokens     TYPE stokesx_tab
        it_statements TYPE sstmnt_tab
        it_structures TYPE zcl_aoc_super=>ty_structures_tt.

    METHODS: build
      RETURNING
        VALUE(ro_structure) TYPE REF TO zcl_aoc_structure.

  PRIVATE SECTION.
    DATA: mt_tokens     TYPE stokesx_tab,
          mt_statements TYPE sstmnt_tab,
          mt_structures TYPE zcl_aoc_super=>ty_structures_tt.

    DATA: mt_sstr TYPE TABLE OF zcl_aoc_structure=>ty_statement.

    METHODS: build_sstr.

    METHODS: structure_loop
      IMPORTING
        is_structure        TYPE sstruc
      RETURNING
        VALUE(ro_structure) TYPE REF TO zcl_aoc_structure.

ENDCLASS.                    "lcl_build DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_build IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_build IMPLEMENTATION.

  METHOD constructor.

    mt_tokens     = it_tokens.
    mt_statements = it_statements.
    mt_structures = it_structures.

  ENDMETHOD.                    "constructor

  METHOD build.

    DATA: ls_structure LIKE LINE OF mt_structures.


    build_sstr( ).

    READ TABLE mt_structures INDEX 1 INTO ls_structure.
    IF sy-subrc <> 0.
      CREATE OBJECT ro_structure.
      RETURN.
    ENDIF.

    ro_structure = structure_loop( ls_structure ).

  ENDMETHOD.                    "build

  METHOD structure_loop.

    DATA: lo_structure TYPE REF TO zcl_aoc_structure,
          lv_index     TYPE i.

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF mt_structures,
                   <ls_sstr>      LIKE LINE OF mt_sstr.


    CREATE OBJECT ro_structure.

    LOOP AT mt_structures ASSIGNING <ls_structure>
        FROM is_structure-struc_from TO is_structure-struc_to.
      lo_structure = structure_loop( <ls_structure> ).
      lo_structure->mv_stmnt_type = <ls_structure>-stmnt_type.
      lo_structure->mv_type = <ls_structure>-type.
      IF <ls_structure>-key_start = abap_true.
        READ TABLE mt_sstr ASSIGNING <ls_sstr> INDEX <ls_structure>-stmnt_from.
        IF sy-subrc = 0.
          lo_structure->ms_statement = <ls_sstr>.
        ENDIF.
      ENDIF.
      APPEND lo_structure TO ro_structure->mt_structure.
    ENDLOOP.
    IF sy-subrc <> 0.
      lv_index = is_structure-stmnt_from.
      IF is_structure-key_start = abap_true.
        lv_index = lv_index + 1.
      ENDIF.
      LOOP AT mt_sstr ASSIGNING <ls_sstr>
          FROM lv_index TO is_structure-stmnt_to.
        CREATE OBJECT lo_structure.
        lo_structure->ms_statement = <ls_sstr>.
        APPEND lo_structure TO ro_structure->mt_structure.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "construct

  METHOD build_sstr.

    DATA: lv_string    TYPE string,
          ls_statement TYPE zcl_aoc_structure=>ty_statement.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF mt_tokens,
                   <ls_statement> LIKE LINE OF mt_statements.


    LOOP AT mt_statements ASSIGNING <ls_statement>.
      CLEAR lv_string.
      LOOP AT mt_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from
          TO <ls_statement>-to
          WHERE type <> scan_token_type-comment
          AND type <> scan_token_type-pragma.
        IF lv_string IS INITIAL.
          lv_string = <ls_token>-str.
        ELSE.
          CONCATENATE lv_string <ls_token>-str INTO lv_string SEPARATED BY space.
        ENDIF.
      ENDLOOP.
      IF sy-subrc <> 0.
        CONTINUE. " continue current loop, loop above was not executed
      ENDIF.

      CLEAR ls_statement.
      ls_statement-statement = lv_string.
      ls_statement-level = <ls_statement>-level.
      ls_statement-row = <ls_token>-row.
      APPEND ls_statement TO mt_sstr.
    ENDLOOP.

  ENDMETHOD.                    "build_str

ENDCLASS.                    "lcl_build IMPLEMENTATION
