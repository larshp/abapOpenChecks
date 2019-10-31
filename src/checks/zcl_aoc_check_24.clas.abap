CLASS zcl_aoc_check_24 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_list,
        proc_name1 TYPE sci_proc_name,
        code1      TYPE string,
        line1      TYPE sci_proc_line,
        proc_name2 TYPE sci_proc_name,
        code2      TYPE string,
        line2      TYPE sci_proc_line,
      END OF ty_list .
    TYPES:
      ty_list_tt TYPE STANDARD TABLE OF ty_list WITH DEFAULT KEY .

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS get_result_node
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_code,
        statement TYPE string,
        level     TYPE i,
        row       TYPE token_row,
      END OF ty_code .
    TYPES:
      ty_code_tt TYPE STANDARD TABLE OF ty_code WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ty_block,
        statements     TYPE string,
        statement_list TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
        level          TYPE i,
        row            TYPE token_row,
      END OF ty_block .
    TYPES:
      ty_block_tt TYPE STANDARD TABLE OF ty_block WITH NON-UNIQUE DEFAULT KEY .

    DATA mv_statements TYPE i .

    METHODS pack
      IMPORTING
        !it_list         TYPE ty_list_tt
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS analyze
      IMPORTING
        !io_scan   TYPE REF TO zcl_aoc_scan
      CHANGING
        !ct_blocks TYPE ty_block_tt .
    METHODS build_blocks
      IMPORTING
        !it_code         TYPE ty_code_tt
      RETURNING
        VALUE(rt_blocks) TYPE ty_block_tt .
    METHODS build_code
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab
      RETURNING
        VALUE(rt_code) TYPE ty_code_tt .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_24 IMPLEMENTATION.


  METHOD analyze.

    DATA: ls_prev     LIKE LINE OF ct_blocks,
          lt_list     TYPE ty_list_tt,
          ls_list     LIKE LINE OF lt_list,
          lv_add      TYPE i,
          lv_include1 TYPE program,
          lv_include2 TYPE program.

    FIELD-SYMBOLS: <ls_block>     LIKE LINE OF ct_blocks,
                   <lv_statement> TYPE string,
                   <ls_list>      LIKE LINE OF lt_list.


    SORT ct_blocks BY statements ASCENDING.

    LOOP AT ct_blocks ASSIGNING <ls_block>.

      IF <ls_block>-statements CP '* ENDMETHOD*'
          AND <ls_block>-statements CP '* METHOD *'.
* ignore if it is just short methods
        CONTINUE.
      ENDIF.

      IF <ls_block>-statements = ls_prev-statements.
        lv_include1 = io_scan->get_include( ls_prev-level ).
        lv_include2 = io_scan->get_include( <ls_block>-level ).

        ls_list-proc_name1 = lv_include1.
        ls_list-line1      = ls_prev-row.
        ls_list-proc_name2 = lv_include2.
        ls_list-line2      = <ls_block>-row.

        CLEAR lt_list.
        LOOP AT <ls_block>-statement_list ASSIGNING <lv_statement>.
          lv_add = sy-tabix - 1.

          APPEND INITIAL LINE TO lt_list ASSIGNING <ls_list>.
          <ls_list> = ls_list.
          <ls_list>-code1 = <lv_statement>.
          <ls_list>-code2 = <lv_statement>.
          <ls_list>-line1 = <ls_list>-line1 + lv_add.
          <ls_list>-line2 = <ls_list>-line2 + lv_add.
        ENDLOOP.

        inform( p_sub_obj_name = lv_include1
                p_line         = ls_prev-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001'
                p_param_1      = pack( lt_list ) ).

* dont report too may findings if there are identical blocks larger than mv_statements
        DELETE ct_blocks WHERE level = ls_prev-level
          AND row >= ls_prev-row
          AND row <= ls_prev-row + mv_statements.
        CLEAR ls_prev.
        CONTINUE.
      ENDIF.

      ls_prev = <ls_block>.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_blocks.

    DATA: lv_index TYPE i,
          lv_add   TYPE abap_bool,
          ls_block LIKE LINE OF rt_blocks,
          lv_level TYPE i.

    FIELD-SYMBOLS: <ls_code>  LIKE LINE OF it_code,
                   <ls_level> LIKE LINE OF it_code.


* todo, look at memory usage
    LOOP AT it_code ASSIGNING <ls_level>.
      lv_index = sy-tabix.
      lv_level = <ls_level>-level.

      lv_add = abap_true.
      CLEAR ls_block.

      DO mv_statements TIMES.
        READ TABLE it_code ASSIGNING <ls_code> INDEX lv_index.
        IF sy-subrc <> 0 OR <ls_code>-level <> lv_level.
          lv_add = abap_false.
          EXIT.
        ENDIF.
        IF ls_block IS INITIAL.
          ls_block-level = <ls_code>-level.
          ls_block-row = <ls_code>-row.
        ENDIF.
        CONCATENATE ls_block-statements <ls_code>-statement
          INTO ls_block-statements SEPARATED BY space.
        APPEND <ls_code>-statement TO ls_block-statement_list.
        lv_index = lv_index + 1.
      ENDDO.

      IF lv_add = abap_true.
        APPEND ls_block TO rt_blocks.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD build_code.

    DATA: lv_level     TYPE i,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_code>      LIKE LINE OF rt_code,
                   <ls_statement> LIKE LINE OF it_statements.


    LOOP AT it_levels ASSIGNING <ls_level>
        WHERE type <> zcl_aoc_scan=>gc_level-macro_define.
      lv_level = sy-tabix.

      LOOP AT it_statements ASSIGNING <ls_statement> WHERE level = lv_level.

        CLEAR lv_statement.
        LOOP AT it_tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from
            TO <ls_statement>-to
            WHERE type <> zcl_aoc_scan=>gc_token-comment.
          IF lv_statement IS INITIAL.
            lv_statement = <ls_token>-str.
          ELSE.
            CONCATENATE lv_statement <ls_token>-str
              INTO lv_statement SEPARATED BY space.
          ENDIF.
        ENDLOOP.

        IF lv_statement IS INITIAL.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO rt_code ASSIGNING <ls_code>.
        <ls_code>-statement = lv_statement.
        <ls_code>-level = lv_level.
        <ls_code>-row = <ls_token>-row.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_code   TYPE ty_code_tt,
          lt_blocks TYPE ty_block_tt.


    lt_code = build_code(
        it_tokens     = io_scan->tokens
        it_statements = io_scan->statements
        it_levels     = io_scan->levels ).

    lt_blocks = build_blocks( lt_code ).

    analyze(
      EXPORTING io_scan = io_scan
      CHANGING ct_blocks = lt_blocks ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '024'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_statements = 10.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Identical code blocks, dbl click for details'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT mv_errty = mv_errty mv_statements = mv_statements TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_result_node.

    CREATE OBJECT p_result TYPE zcl_aoc_check_24_result
      EXPORTING
        iv_kind = p_kind.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_statements 'Statements' ''.           "#EC NOTEXT

    zzaoc_popup.

    IF mv_statements <= 0.
      attributes_ok = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD pack.

    DATA: lv_xstring TYPE xstring.


    EXPORT list = it_list TO DATA BUFFER lv_xstring COMPRESSION ON.
    rv_string = lv_xstring.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_statements = mv_statements
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
