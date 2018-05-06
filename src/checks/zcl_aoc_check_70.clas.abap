CLASS zcl_aoc_check_70 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
         REDEFINITION .
    METHODS get_attributes
         REDEFINITION .
    METHODS get_message_text
         REDEFINITION .
    METHODS if_ci_test~query_attributes
         REDEFINITION .
    METHODS put_attributes
         REDEFINITION .
  PROTECTED SECTION.

    DATA mt_pattern_info TYPE char20_t .
    DATA mt_pattern_warning TYPE char20_t .
    DATA mt_pattern_error TYPE char20_t .
    DATA mv_multiline TYPE sap_bool .
    CLASS-DATA st_todo_texts TYPE stringtab .
  PRIVATE SECTION.

    METHODS get_code_from_text
      IMPORTING
        !iv_text       TYPE string
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
    METHODS get_comment_tokens
      IMPORTING
        !it_tokens         TYPE stokesx_tab
      RETURNING
        VALUE(rt_comments) TYPE stokesx_tab .
    METHODS get_plain_text_comment
      IMPORTING
        !it_comments            TYPE stokesx_tab
      RETURNING
        VALUE(rt_comment_texts) TYPE stringtab .
    METHODS get_tokens
      IMPORTING
        !it_tokens       TYPE stokesx_tab
        !it_statements   TYPE sstmnt_tab
        !iv_level        TYPE i
      RETURNING
        VALUE(rt_tokens) TYPE stokesx_tab .
    METHODS parse
      IMPORTING
        !it_comments      TYPE stokesx_tab
        !it_comment_texts TYPE stringtab
        !is_level         TYPE slevel
        !iv_error_type    TYPE sci_errty
        !iv_pattern       TYPE char20 .
ENDCLASS.



CLASS ZCL_AOC_CHECK_70 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA:
      lt_comment_texts TYPE stringtab,
      lt_comments      TYPE stokesx_tab,
      lt_tokens        TYPE stokesx_tab.

    FIELD-SYMBOLS:
      <ls_level>   LIKE LINE OF it_levels,
      <lv_pattern> TYPE char20.

    IF mt_pattern_info IS INITIAL AND mt_pattern_warning IS INITIAL AND mt_pattern_error IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
      CLEAR:
        lt_comment_texts,
        lt_comments,
        lt_tokens.

      lt_tokens = get_tokens( it_tokens     = it_tokens
                              it_statements = it_statements
                              iv_level      = sy-tabix ).

      lt_comments = get_comment_tokens( lt_tokens ).

      IF lt_comments IS INITIAL.
        CONTINUE.
      ENDIF.

      "get plain text from comments
      lt_comment_texts = get_plain_text_comment( lt_comments ).

      LOOP AT mt_pattern_info ASSIGNING <lv_pattern>.
        parse( it_comments      = lt_comments
               it_comment_texts = lt_comment_texts
               is_level         = <ls_level>
               iv_pattern       = <lv_pattern>
               iv_error_type    = c_note ).
      ENDLOOP.
      LOOP AT mt_pattern_warning ASSIGNING <lv_pattern>.
        parse( it_comments      = lt_comments
               it_comment_texts = lt_comment_texts
               is_level         = <ls_level>
               iv_pattern       = <lv_pattern>
               iv_error_type    = c_warning ).
      ENDLOOP.
      LOOP AT mt_pattern_error ASSIGNING <lv_pattern>.
        parse( it_comments      = lt_comments
               it_comment_texts = lt_comment_texts
               is_level         = <ls_level>
               iv_pattern       = <lv_pattern>
               iv_error_type    = c_error ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '070'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    APPEND 'TODO:' TO mt_pattern_info.
    APPEND 'HACK:' TO mt_pattern_warning.
    APPEND 'FIXME:' TO mt_pattern_error.
    mv_multiline = abap_false.
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_multiline        = mv_multiline
      mt_pattern_info     = mt_pattern_info
      mt_pattern_warning  = mt_pattern_warning
      mt_pattern_error    = mt_pattern_error
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_code_from_text.

    READ TABLE st_todo_texts TRANSPORTING NO FIELDS WITH TABLE KEY table_line = iv_text.
    IF sy-subrc <> 0.
      APPEND iv_text TO st_todo_texts.
    ENDIF.

    rv_code = sy-tabix.

  ENDMETHOD.


  METHOD get_comment_tokens.
    FIELD-SYMBOLS:
      <fs_token> TYPE stokesx.

    "only meaningful comments
    LOOP AT it_tokens ASSIGNING <fs_token> WHERE type = scan_token_type-comment AND str CN '*"-&'.
      APPEND <fs_token> TO rt_comments.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_message_text.
    DATA:
      lv_textcount TYPE i,
      lv_todo_text TYPE string.

    CLEAR p_text.

    lv_textcount = lines( st_todo_texts ).
    IF p_code CO ' 012345789' AND p_code <= lv_textcount.
      READ TABLE st_todo_texts INTO lv_todo_text INDEX p_code.
      p_text = lv_todo_text.
    ENDIF.

    IF p_text IS INITIAL.
      p_text = 'other comment marker'(005).
    ENDIF.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD get_plain_text_comment.
    FIELD-SYMBOLS:
      <lv_comment_text> TYPE string,
      <ls_comment>      TYPE stokesx.

    "get plain text from comments
    LOOP AT it_comments ASSIGNING <ls_comment>.
      APPEND INITIAL LINE TO rt_comment_texts ASSIGNING <lv_comment_text>.
      <lv_comment_text> = <ls_comment>-str.
      SHIFT <lv_comment_text> LEFT DELETING LEADING '*"-&'.
      CONDENSE <lv_comment_text>.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_tokens.
    FIELD-SYMBOLS:
      <ls_token>     TYPE stokesx,
      <ls_statement> TYPE sstmnt.

    LOOP AT it_statements ASSIGNING <ls_statement> WHERE level = iv_level.
      LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
        APPEND <ls_token> TO rt_tokens.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.
    zzaoc_top.

    zzaoc_fill_att mt_pattern_info    'Info pattern'(001)        ''.
    zzaoc_fill_att mt_pattern_warning 'Warning pattern'(002)     ''.
    zzaoc_fill_att mt_pattern_error   'Error pattern'(003)       ''.
    zzaoc_fill_att mv_multiline       'Multiline comments'(004)  'C'.

    zzaoc_popup.

    IF mt_pattern_info IS NOT INITIAL OR mt_pattern_warning IS NOT INITIAL OR mt_pattern_error IS NOT INITIAL.
      attributes_ok = abap_true.
    ELSE.
      attributes_ok = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD parse.
    DATA:
      lv_complete_text TYPE string,
      lt_result        TYPE match_result_tab.

    FIELD-SYMBOLS:
      <lv_comment_text> TYPE string,
      <ls_comment>      TYPE stokesx,
      <ls_result>       TYPE match_result.

    IF iv_pattern IS INITIAL.
      RETURN.
    ENDIF.

    FIND ALL OCCURRENCES OF SUBSTRING iv_pattern IN TABLE it_comment_texts
      IGNORING CASE
      RESULTS lt_result.

    IF lt_result IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_result ASSIGNING <ls_result>.
      READ TABLE it_comments ASSIGNING <ls_comment> INDEX <ls_result>-line.
      IF sy-subrc = 0.
        READ TABLE it_comment_texts ASSIGNING <lv_comment_text> INDEX sy-tabix.
        IF sy-subrc = 0.
          lv_complete_text = <lv_comment_text>.

          IF mv_multiline = abap_true.
            "Append consecutive comment lines
            WHILE sy-subrc = 0.
              READ TABLE it_comments ASSIGNING <ls_comment> WITH KEY row = <ls_comment>-row + 1.
              IF sy-subrc = 0.
                READ TABLE it_comment_texts ASSIGNING <lv_comment_text> INDEX sy-tabix.
                IF sy-subrc = 0.
                  CONCATENATE lv_complete_text <lv_comment_text> INTO lv_complete_text SEPARATED BY space.
                ENDIF.
              ENDIF.
            ENDWHILE.
          ENDIF.


          READ TABLE st_todo_texts TRANSPORTING NO FIELDS WITH TABLE KEY table_line = lv_complete_text.
          IF sy-subrc <> 0.
            APPEND lv_complete_text TO st_todo_texts.
          ENDIF.

          inform(
            p_sub_obj_type = c_type_include
            p_sub_obj_name = is_level-name
            p_line         = <ls_comment>-row
            p_column       = <ls_comment>-col
            p_kind         = iv_error_type
            p_test         = myname
            p_code         = get_code_from_text( lv_complete_text ) ).
          UNASSIGN <lv_comment_text>.
        ENDIF.
        UNASSIGN <ls_comment>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_multiline        = mv_multiline
      mt_pattern_info     = mt_pattern_info
      mt_pattern_warning  = mt_pattern_warning
      mt_pattern_error    = mt_pattern_error
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
