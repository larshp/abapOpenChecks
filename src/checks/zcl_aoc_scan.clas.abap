CLASS zcl_aoc_scan DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_position,
        row TYPE token_row,
        col TYPE token_col,
      END OF ty_position .
    TYPES:
      BEGIN OF ty_statement,
        str        TYPE string,
        start      TYPE ty_position,
        end        TYPE ty_position,
        include    TYPE programm,
        level      TYPE stmnt_levl,
        count      TYPE i,
        terminator TYPE stmnt_term,
        index      TYPE i,
      END OF ty_statement .
    TYPES:
      ty_statements TYPE STANDARD TABLE OF ty_statement WITH DEFAULT KEY .
    TYPES:
      ty_structures_tt TYPE STANDARD TABLE OF sstruc WITH NON-UNIQUE DEFAULT KEY .

    DATA tokens TYPE stokesx_tab READ-ONLY .
    DATA statements TYPE sstmnt_tab READ-ONLY .
    DATA levels TYPE slevel_tab READ-ONLY .
    DATA structures TYPE ty_structures_tt READ-ONLY .

    METHODS constructor
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab
        !it_structures TYPE ty_structures_tt .
    METHODS statement_row
      IMPORTING
        !iv_number       TYPE stmnt_nr
      RETURNING
        VALUE(rv_result) TYPE token_row .
    METHODS build_statements
      IMPORTING
        !iv_literals         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_statements) TYPE ty_statements .
    METHODS statement_keyword
      IMPORTING
        !iv_number       TYPE stmnt_nr
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS create_from_ref
      IMPORTING
        !io_ref        TYPE REF TO object
      RETURNING
        VALUE(ro_scan) TYPE REF TO zcl_aoc_scan .
    METHODS get_include
      IMPORTING
        !iv_level         TYPE i
      RETURNING
        VALUE(rv_program) TYPE program .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS token_position
      IMPORTING
        !is_token          TYPE stokesx
      RETURNING
        VALUE(rs_position) TYPE ty_position .
ENDCLASS.



CLASS ZCL_AOC_SCAN IMPLEMENTATION.


  METHOD build_statements.

    DATA: lv_str    TYPE string,
          ls_start  TYPE ty_position,
          ls_end    TYPE ty_position,
          lt_tokens LIKE tokens,
          lv_index  TYPE i,
          lv_count  TYPE i.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF statements,
                   <ls_token>     LIKE LINE OF tokens,
                   <ls_add>       LIKE LINE OF rt_statements.


    lt_tokens = tokens.

    IF iv_literals = abap_true.
      LOOP AT lt_tokens ASSIGNING <ls_token> WHERE type = scan_token_type-literal.
        <ls_token>-str = 'STR'.
      ENDLOOP.
    ENDIF.

    LOOP AT statements ASSIGNING <ls_statement>
        WHERE type <> scan_stmnt_type-empty
        AND type <> scan_stmnt_type-comment
        AND type <> scan_stmnt_type-comment_in_stmnt
        AND type <> scan_stmnt_type-pragma.
      lv_index = sy-tabix.

      CLEAR lv_str.
      lv_count = 0.

      LOOP AT lt_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF lv_str IS INITIAL.
          lv_str = <ls_token>-str.
          ls_start = token_position( <ls_token> ).
        ELSE.
          CONCATENATE lv_str <ls_token>-str INTO lv_str SEPARATED BY space.
        ENDIF.
        lv_count = lv_count + 1.
        ls_end = token_position( <ls_token> ).
      ENDLOOP.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO rt_statements ASSIGNING <ls_add>.
        <ls_add>-str        = lv_str.
        <ls_add>-include    = get_include( <ls_statement>-level ).
        <ls_add>-level      = <ls_statement>-level.
        <ls_add>-start      = ls_start.
        <ls_add>-end        = ls_end.
        <ls_add>-count      = lv_count.
        <ls_add>-index      = lv_index.
        <ls_add>-terminator = <ls_statement>-terminator.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    tokens = it_tokens.
    statements = it_statements.
    levels = it_levels.
    structures = it_structures.

  ENDMETHOD.


  METHOD create_from_ref.

    FIELD-SYMBOLS: <lt_tokens>     TYPE stokesx_tab,
                   <lt_statements> TYPE sstmnt_tab,
                   <lt_levels>     TYPE slevel_tab,
                   <lt_structures> TYPE ty_structures_tt.


    ASSIGN io_ref->('TOKENS') TO <lt_tokens>.
    ASSERT sy-subrc = 0.

    ASSIGN io_ref->('STATEMENTS') TO <lt_statements>.
    ASSERT sy-subrc = 0.

    ASSIGN io_ref->('LEVELS') TO <lt_levels>.
    ASSERT sy-subrc = 0.

    ASSIGN io_ref->('STRUCTURES') TO <lt_structures>.
    ASSERT sy-subrc = 0.

    CREATE OBJECT ro_scan
      EXPORTING
        it_tokens     = <lt_tokens>
        it_statements = <lt_statements>
        it_levels     = <lt_levels>
        it_structures = <lt_structures>.

  ENDMETHOD.


  METHOD get_include.

    DATA: lv_level TYPE i,
          ls_level LIKE LINE OF levels.

    CONSTANTS lc_program TYPE c LENGTH 1 VALUE 'P'.


    IF iv_level = 0.
* in case INCLUDE doesnt exist in the system
      RETURN.
    ENDIF.

    lv_level = iv_level.

    DO.
      READ TABLE levels INDEX lv_level INTO ls_level.
      IF ls_level-type = lc_program.
        rv_program = ls_level-name.
        RETURN.
      ENDIF.
      lv_level = ls_level-level.
    ENDDO.

  ENDMETHOD.


  METHOD statement_keyword.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF statements,
                   <ls_token>     LIKE LINE OF tokens.


    READ TABLE statements ASSIGNING <ls_statement> INDEX iv_number.
    ASSERT sy-subrc = 0.

    IF <ls_statement>-from <= <ls_statement>-to.
      READ TABLE tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      ASSERT sy-subrc = 0.

      rv_result = <ls_token>-str.
    ENDIF.

  ENDMETHOD.


  METHOD statement_row.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF statements,
                   <ls_token>     LIKE LINE OF tokens.


    READ TABLE statements ASSIGNING <ls_statement> INDEX iv_number.
    ASSERT sy-subrc = 0.

    READ TABLE tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    ASSERT sy-subrc = 0.

    rv_result = <ls_token>-row.

  ENDMETHOD.


  METHOD token_position.

    rs_position-col = is_token-col.
    rs_position-row = is_token-row.

  ENDMETHOD.
ENDCLASS.
