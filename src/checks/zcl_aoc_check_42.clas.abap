CLASS zcl_aoc_check_42 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_42 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    TYPES: BEGIN OF ty_when,
             back    TYPE stru_back,
             code    TYPE string,
             include TYPE sobj_name,
             row     TYPE token_row,
           END OF ty_when.

    DATA: lv_index TYPE i,
          lv_from  TYPE i,
          lt_when  TYPE STANDARD TABLE OF ty_when WITH DEFAULT KEY,
          ls_when  LIKE LINE OF lt_when.

    FIELD-SYMBOLS: <ls_when>      LIKE LINE OF lt_when,
                   <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_structure> LIKE LINE OF io_scan->structures.


    LOOP AT io_scan->structures ASSIGNING <ls_structure>
        WHERE stmnt_type = scan_struc_stmnt_type-when.

      APPEND INITIAL LINE TO lt_when ASSIGNING <ls_when>.
      <ls_when>-back = <ls_structure>-back.

      lv_from = <ls_structure>-stmnt_from + 1.
      LOOP AT io_scan->statements ASSIGNING <ls_statement>
          FROM lv_from
          TO <ls_structure>-stmnt_to
          WHERE type <> scan_stmnt_type-empty
          AND type <> scan_stmnt_type-comment
          AND type <> scan_stmnt_type-comment_in_stmnt
          AND type <> scan_stmnt_type-macro_definition
          AND type <> scan_stmnt_type-pragma.

        IF <ls_when>-include IS INITIAL.
          <ls_when>-include = io_scan->get_include( <ls_statement>-level ).
        ENDIF.

        LOOP AT io_scan->tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to.

          IF <ls_when>-row IS INITIAL.
            <ls_when>-row = <ls_token>-row.
          ENDIF.

          IF <ls_when>-code IS INITIAL.
            <ls_when>-code = <ls_token>-str.
          ELSE.
            CONCATENATE <ls_when>-code <ls_token>-str
              INTO <ls_when>-code SEPARATED BY space.
          ENDIF.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    DELETE lt_when WHERE code IS INITIAL.
    SORT lt_when BY back ASCENDING code ASCENDING.

    LOOP AT lt_when ASSIGNING <ls_when>.
      lv_index = sy-tabix + 1.
      READ TABLE lt_when INDEX lv_index INTO ls_when.
      IF sy-subrc = 0
          AND <ls_when>-back = ls_when-back
          AND <ls_when>-code = ls_when-code.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_when>-include
                p_line         = <ls_when>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '042'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Identical WHEN code'.                     "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
