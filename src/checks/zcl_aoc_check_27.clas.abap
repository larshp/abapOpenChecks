CLASS zcl_aoc_check_27 DEFINITION
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

    TYPES:
      BEGIN OF ty_stmnt,
        statement TYPE string,
        row       TYPE token_row,
        row_to    TYPE token_row,
        level     TYPE stmnt_levl,
        col       TYPE token_col,
        col_to    TYPE token_col,
      END OF ty_stmnt.

    DATA mt_tables TYPE scit_tabl.

    TYPE-POOLS abap.
    METHODS is_local
      IMPORTING
        !is_statement  TYPE ty_stmnt
      RETURNING
        VALUE(rv_bool) TYPE abap_bool.
    METHODS analyze.
    METHODS build
      IMPORTING
        !is_structure  TYPE sstruc
        !it_statements TYPE sstmnt_tab
        !it_tokens     TYPE stokesx_tab.
  PRIVATE SECTION.

    DATA:
      mt_statements TYPE TABLE OF ty_stmnt.
ENDCLASS.



CLASS ZCL_AOC_CHECK_27 IMPLEMENTATION.


  METHOD analyze.

    DATA: lv_index     TYPE i,
          lv_include   TYPE program,
          lv_code      TYPE sci_errc,
          ls_statement LIKE LINE OF mt_statements.


    WHILE lines( mt_statements ) > 0.

      lv_index = lines( mt_statements ).
      READ TABLE mt_statements INDEX lv_index INTO ls_statement.
      ASSERT sy-subrc = 0.

      IF ls_statement-statement = 'ENDIF'
          OR ls_statement-statement = 'ENDTRY'
          OR ls_statement-statement = 'ENDFORM'
          OR ls_statement-statement = 'ENDMETHOD'.
        DELETE mt_statements INDEX lv_index.
      ELSEIF ls_statement-statement = 'RETURN'.
        lv_code = '001'.
      ELSEIF ( ls_statement-statement CP 'CLEAR *'
          OR ls_statement-statement CP 'FREE *' )
          AND is_local( ls_statement ) = abap_true
          AND NOT ls_statement-statement CP 'CLEAR <*'.
        lv_code = '002'.
      ELSEIF ls_statement-statement CP 'EXIT*'
          OR ls_statement-statement CP 'CHECK *'.
        lv_code = '003'.
      ELSE.
        RETURN.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        lv_include = get_include( p_level = ls_statement-level ).

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line         = ls_statement-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
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
        FROM is_structure-stmnt_from TO is_structure-stmnt_to
        WHERE type <> scan_stmnt_type-comment
        AND type <> scan_stmnt_type-comment_in_stmnt
        AND type <> scan_stmnt_type-macro_call
        AND trow <> 0. " skip macro calls

      CLEAR ls_statement.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF ls_statement-statement IS INITIAL.
          ls_statement-statement = <ls_token>-str.
          ls_statement-row = <ls_token>-row.
          ls_statement-col = <ls_token>-col.
          ls_statement-row_to = <ls_token>-row.
          ls_statement-col_to = <ls_token>-col.
          ls_statement-level = <ls_statement>-level.
        ELSE.
          CONCATENATE ls_statement-statement <ls_token>-str
            INTO ls_statement-statement SEPARATED BY space.
          ls_statement-row_to = <ls_token>-row.
          ls_statement-col_to = <ls_token>-col.
        ENDIF.
      ENDLOOP.
      APPEND ls_statement TO mt_statements.
    ENDLOOP.

    IF lines( mt_statements ) = 3.
      CLEAR mt_statements.
    ENDIF.

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

    version        = '001'.
    position       = '027'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.
    CLEAR mt_tables.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Last statement is RETURN'.                "#EC NOTEXT
      WHEN '002'.
        p_text = 'Last statement is CLEAR or FREE'.         "#EC NOTEXT
      WHEN '003'.
        p_text = 'Last statement is CHECK or EXIT'.         "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD is_local.

    DATA: lv_sconame TYPE seosubcodf-sconame,
          lv_cmpname TYPE seocompodf-cmpname,
          lt_result  TYPE scr_refs,
          ls_mtd     TYPE seocpdkey,
          lv_before  TYPE string ##NEEDED,
          lv_include TYPE program,
          lv_var     TYPE string.

    FIELD-SYMBOLS: <ls_result>    LIKE LINE OF lt_result,
                   <ls_statement> LIKE LINE OF mt_statements.


    lt_result = zcl_aoc_compiler=>get_instance(
      iv_object_type = object_type
      iv_object_name = object_name )->get_result( ).
    DELETE lt_result WHERE tag <> cl_abap_compiler=>tag_data.
    DELETE lt_result WHERE name = ''.

    SPLIT is_statement-statement AT space INTO lv_before lv_var.

    lv_include = get_include( p_level = is_statement-level ).

* todo: handle local classes?

* make sure it is a local variable
    READ TABLE lt_result
      ASSIGNING <ls_result> WITH KEY
      name = lv_var
      line = is_statement-row
      statement->source_info->name = lv_include.
    IF sy-subrc = 0
        AND ( <ls_result>-full_name CP '*\FO:*'
        OR <ls_result>-full_name CP '*\ME:*' ).

      IF <ls_result>-full_name CP '*\ME:*'.
        cl_oo_classname_service=>get_method_by_include(
          EXPORTING
            incname             = lv_include
          RECEIVING
            mtdkey              = ls_mtd
          EXCEPTIONS
            class_not_existing  = 1
            method_not_existing = 2
            OTHERS              = 3 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        IF ls_mtd-cpdname CA '~'.
* handle methods from  interfaces
          SPLIT ls_mtd-cpdname AT '~' INTO ls_mtd-clsname ls_mtd-cpdname.
        ENDIF.

        SELECT SINGLE sconame FROM seosubcodf
          INTO lv_sconame
          WHERE clsname = ls_mtd-clsname
          AND cmpname = ls_mtd-cpdname
          AND sconame = lv_var
          AND version = '1'.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        SELECT SINGLE cmpname FROM seocompodf
          INTO lv_cmpname
          WHERE clsname = ls_mtd-clsname
          AND cmpname = lv_var
          AND version = '1'.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      ELSE.
        READ TABLE lt_result
          ASSIGNING <ls_result> WITH KEY
          full_name = <ls_result>-full_name
          mode2 = cl_abap_compiler=>mode2_def.
        IF sy-subrc = 0.
          LOOP AT mt_statements ASSIGNING <ls_statement>
              WHERE ( row < <ls_result>-line AND row_to > <ls_result>-line )
              OR ( row = <ls_result>-line AND col <= <ls_result>-column AND row_to > <ls_result>-line )
              OR ( row = <ls_result>-line AND col <= <ls_result>-column
              AND row_to = <ls_result>-line AND col_to >= <ls_result>-column )
              OR ( row < <ls_result>-line AND row_to = <ls_result>-line AND col_to >= <ls_result>-column ).
            IF <ls_statement>-statement CP 'FORM *'.
              RETURN.
            ENDIF.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.

      rv_bool = abap_true.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
