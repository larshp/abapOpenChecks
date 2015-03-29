class ZCL_AOC_CHECK_27 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_27
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_27
*"* do not include other source files here!!!

  data MT_TABLES type SCIT_TABL .

  methods ANALYZE .
  methods BUILD
    importing
      !IS_STRUCTURE type SSTRUC
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_TOKENS type STOKESX_TAB .
PRIVATE SECTION.
*"* private components of class ZCL_AOC_CHECK_27
*"* do not include other source files here!!!

  CONSTANTS c_my_name TYPE seoclsname VALUE 'ZCL_AOC_CHECK_27'. "#EC NOTEXT

  TYPES: BEGIN OF st_statement,
           statement TYPE string,
           row TYPE stmnt_levl,
           level TYPE token_row,
         END OF st_statement.

  DATA: mt_statements TYPE TABLE OF st_statement.
ENDCLASS.



CLASS ZCL_AOC_CHECK_27 IMPLEMENTATION.


METHOD analyze.

  DATA: lv_index     TYPE i,
        lv_include   TYPE program,
        ls_statement LIKE LINE OF mt_statements.


  WHILE lines( mt_statements ) > 0.

    lv_index = lines( mt_statements ).
    READ TABLE mt_statements INDEX lv_index INTO ls_statement.

    IF ls_statement-statement = 'ENDIF'
        OR ls_statement-statement = 'ENDTRY'
        OR ls_statement-statement = 'ENDFORM'
        OR ls_statement-statement = 'ENDMETHOD'.
      DELETE mt_statements INDEX lv_index.
    ELSEIF ls_statement-statement = 'RETURN'.
      lv_include = get_include( p_level = ls_statement-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = ls_statement-row
              p_kind         = mv_errty
              p_test         = c_my_name
              p_code         = '001' ).
      RETURN.
    ELSE.
      RETURN.
    ENDIF.

  ENDWHILE.

ENDMETHOD.


METHOD build.

  DATA: ls_statement LIKE LINE OF mt_statements.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  CLEAR mt_statements.
  LOOP AT it_statements ASSIGNING <ls_statement>
      FROM is_structure-stmnt_from TO is_structure-stmnt_to.
    CLEAR ls_statement.
    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to.
      IF ls_statement-statement IS INITIAL.
        ls_statement-statement = <ls_token>-str.
        ls_statement-row = <ls_token>-row.
        ls_statement-level = <ls_statement>-level.
      ELSE.
        CONCATENATE ls_statement-statement <ls_token>-str
          INTO ls_statement-statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    APPEND ls_statement TO mt_statements.
  ENDLOOP.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures.


  LOOP AT it_structures ASSIGNING <ls_structure>
      WHERE stmnt_type = scan_struc_stmnt_type-module
      OR stmnt_type = scan_struc_stmnt_type-function
      OR stmnt_type = scan_struc_stmnt_type-form
      OR stmnt_type = scan_struc_stmnt_type-method.

    build( is_structure  = <ls_structure>
           it_statements = it_statements
           it_tokens     = it_tokens ).

    analyze( ).

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Last statement is RETURN'.              "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  CLEAR mt_tables.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Last statement is RETURN'.                  "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.