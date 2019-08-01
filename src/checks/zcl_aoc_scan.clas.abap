class ZCL_AOC_SCAN definition
  public
  create public .

public section.

  types:
    ty_structures_tt TYPE STANDARD TABLE OF sstruc WITH NON-UNIQUE DEFAULT KEY .

  data TOKENS type STOKESX_TAB read-only .
  data STATEMENTS type SSTMNT_TAB read-only .
  data LEVELS type SLEVEL_TAB read-only .
  data STRUCTURES type TY_STRUCTURES_TT read-only .

  methods CONSTRUCTOR
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
      !IT_STRUCTURES type TY_STRUCTURES_TT .
  methods STATEMENT_ROW
    importing
      !IV_NUMBER type STMNT_NR
    returning
      value(RV_RESULT) type TOKEN_ROW .
  methods STATEMENT_KEYWORD
    importing
      !IV_NUMBER type STMNT_NR
    returning
      value(RV_RESULT) type STRING .
  class-methods CREATE_FROM_REF
    importing
      !IO_REF type ref to OBJECT
    returning
      value(RO_SCAN) type ref to ZCL_AOC_SCAN .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_SCAN IMPLEMENTATION.


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
ENDCLASS.
