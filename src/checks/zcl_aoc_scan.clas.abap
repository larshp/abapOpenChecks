CLASS zcl_aoc_scan DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF gc_token,
                 comment    TYPE c LENGTH 1 VALUE 'C',
                 identifier TYPE c LENGTH 1 VALUE 'I',
                 list       TYPE c LENGTH 1 VALUE 'L',
                 literal    TYPE c LENGTH 1 VALUE 'S',
                 pragma     TYPE c LENGTH 1 VALUE 'P',
               END OF gc_token.

    CONSTANTS: BEGIN OF gc_statement,
                 comment          TYPE c LENGTH 1 VALUE 'P',
                 comment_in_stmnt TYPE c LENGTH 1 VALUE 'S',
                 compute_direct   TYPE c LENGTH 1 VALUE 'C',
                 empty            TYPE c LENGTH 1 VALUE 'N',
                 macro_call       TYPE c LENGTH 1 VALUE 'D',
                 macro_definition TYPE c LENGTH 1 VALUE 'M',
                 method_direct    TYPE c LENGTH 1 VALUE 'A',
                 native_sql       TYPE c LENGTH 1 VALUE 'E',
                 pragma           TYPE c LENGTH 1 VALUE 'G',
                 standard         TYPE c LENGTH 1 VALUE 'K',
               END OF gc_statement.

    CONSTANTS: BEGIN OF gc_structure,
                 condition TYPE c LENGTH 1 VALUE  'C',
                 routine   TYPE c LENGTH 1 VALUE  'R',
                 sequence  TYPE c LENGTH 1 VALUE  'S',
               END OF gc_structure.

    CONSTANTS: BEGIN OF gc_structure_statement,
                 case     TYPE c LENGTH 1 VALUE 'c',
                 catch    TYPE c LENGTH 1 VALUE '+',
                 cleanup  TYPE c LENGTH 1 VALUE '-',
                 do       TYPE c LENGTH 1 VALUE 'D',
                 else     TYPE c LENGTH 1 VALUE 'e',
                 elseif   TYPE c LENGTH 1 VALUE 'f',
                 form     TYPE c LENGTH 1 VALUE 'O',
                 function TYPE c LENGTH 1 VALUE 'U',
                 if       TYPE c LENGTH 1 VALUE 'i',
                 loop     TYPE c LENGTH 1 VALUE 'L',
                 method   TYPE c LENGTH 1 VALUE 'H',
                 module   TYPE c LENGTH 1 VALUE 'M',
                 select   TYPE c LENGTH 1 VALUE 'S',
                 try      TYPE c LENGTH 1 VALUE '_',
                 when     TYPE c LENGTH 1 VALUE 'w',
                 while    TYPE c LENGTH 1 VALUE 'W',
               END OF gc_structure_statement.

    CONSTANTS: BEGIN OF gc_level,
                 macro_define TYPE c LENGTH 1 VALUE 'D',
                 macro_trmac  TYPE c LENGTH 1 VALUE 'R',
                 program      TYPE c LENGTH 1 VALUE 'P',
               END OF gc_level.

    CONSTANTS: BEGIN OF gc_keyword,
                 if                TYPE string VALUE 'IF',
                 elseif            TYPE string VALUE 'ELSEIF',
                 case              TYPE string VALUE 'CASE',
                 when              TYPE string VALUE 'WHEN',
                 check             TYPE string VALUE 'CHECK',
                 assert            TYPE string VALUE 'ASSERT',
                 types             TYPE string VALUE 'TYPES',
                 ranges            TYPE string VALUE 'RANGES',
                 methods           TYPE string VALUE 'METHODS',
                 class_methods     TYPE string VALUE 'CLASS-METHODS',
                 data              TYPE string VALUE 'DATA',
                 class_data        TYPE string VALUE 'CLASS-DATA',
                 end_of_definition TYPE string VALUE 'END-OF-DEFINITION',
                 concatenate       TYPE string VALUE 'CONCATENATE',
                 write             TYPE string VALUE 'WRITE',
                 message           TYPE string VALUE 'MESSAGE',
                 select            TYPE string VALUE 'SELECT',
                 default           TYPE string VALUE 'DEFAULT',
                 type              TYPE string VALUE 'TYPE',
                 like              TYPE string VALUE 'LIKE',
                 call              TYPE string VALUE 'CALL',
                 function          TYPE string VALUE 'FUNCTION',
                 move              TYPE string VALUE 'MOVE',
                 to                TYPE string VALUE 'TO',
               END OF gc_keyword.

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

    CLASS-METHODS create_from_ref
      IMPORTING
        !io_ref        TYPE REF TO object
      RETURNING
        VALUE(ro_scan) TYPE REF TO zcl_aoc_scan .
    METHODS build_statements
      IMPORTING
        !iv_literals         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_statements) TYPE ty_statements .
    METHODS constructor
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab
        !it_structures TYPE ty_structures_tt .
    METHODS get_include
      IMPORTING
        !iv_level         TYPE i
      RETURNING
        VALUE(rv_program) TYPE program .
    METHODS statement_keyword
      IMPORTING
        !iv_number       TYPE stmnt_nr
      RETURNING
        VALUE(rv_result) TYPE string .
    METHODS statement_row
      IMPORTING
        !iv_number       TYPE stmnt_nr
      RETURNING
        VALUE(rv_result) TYPE token_row .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS token_position
      IMPORTING
        !is_token          TYPE stokesx
      RETURNING
        VALUE(rs_position) TYPE ty_position .
ENDCLASS.



CLASS zcl_aoc_scan IMPLEMENTATION.


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
      LOOP AT lt_tokens ASSIGNING <ls_token> WHERE type = gc_token-literal.
        <ls_token>-str = 'STR'.
      ENDLOOP.
    ENDIF.

    LOOP AT statements ASSIGNING <ls_statement>
        WHERE type <> gc_statement-empty
        AND type <> gc_statement-comment
        AND type <> gc_statement-comment_in_stmnt
        AND type <> gc_statement-pragma.
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

    IF <ls_statement>-type = gc_statement-compute_direct.
      rv_result = 'COMPUTE'.
      RETURN.
    ENDIF.

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
