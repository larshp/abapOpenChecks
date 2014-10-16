class ZCL_AOC_PARSER definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!

  type-pools ABAP .
  class-methods MATCH
    importing
      !IT_CODE type STRING_TABLE
      !IV_DEBUG type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_MATCH) type ABAP_BOOL .
protected section.
*"* protected components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!

  class-data GT_TOKENS type STRING_TABLE .
  class-data GV_END_RULE type STRING .
  type-pools ABAP .
  class-data GV_DEBUG type ABAP_BOOL .

  class-methods WALK
    importing
      !IO_NODE type ref to LCL_NODE
      !IV_INDEX type I
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods PERMUTE
    importing
      !IV_COUNT type I
    exporting
      !ET_PERM type TT_PERM .
  class-methods GRAPH_TO_TEXT
    importing
      !IO_NODE type ref to LCL_NODE
    returning
      value(RV_TEXT) type STRING .
  class-methods GRAPH_DOWNLOAD
    importing
      !IV_RULENAME type STRING
      !IO_START type ref to LCL_NODE .
  class-methods GRAPH_BUILD
    exporting
      !EO_START type ref to LCL_NODE
      !EO_END type ref to LCL_NODE
    changing
      !CV_RULENAME type STRING .
  class-methods BUILD_PERMUTATION
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_ROLE
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_SEQUENCE
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_TERMINAL
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_OPTIONLIST
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_OPTION
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_NONTERMINAL
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_ITERATION
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD_ALTERNATIVE
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods BUILD
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IO_BEFORE type ref to LCL_NODE
      !IO_AFTER type ref to LCL_NODE .
  class-methods WALK_END
    importing
      !IO_NODE type ref to LCL_NODE
      !IV_INDEX type I
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods WALK_NODE
    importing
      !IO_NODE type ref to LCL_NODE
      !IV_INDEX type I
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods WALK_NONTERMINAL
    importing
      !IO_NODE type ref to LCL_NODE
      !IV_INDEX type I
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods WALK_ROLE
    importing
      !IO_NODE type ref to LCL_NODE
      !IV_INDEX type I
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods WALK_TERMINAL
    importing
      !IO_NODE type ref to LCL_NODE
      !IV_INDEX type I
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods XML_GET
    exporting
      !EI_RULE type ref to IF_IXML_NODE
    changing
      !CV_RULENAME type STRING .
  class-methods XML_PARSE
    importing
      !IV_XML type STRING
    returning
      value(RI_RULE) type ref to IF_IXML_NODE .
  class-methods PARSE
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
    returning
      value(RV_MATCH) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AOC_PARSER IMPLEMENTATION.


METHOD build.

  DATA: lv_name TYPE string.


  IF NOT ii_rule IS BOUND.
    io_before->edge( io_after ).
    RETURN.
  ENDIF.

  lv_name = ii_rule->get_name( ).

  CASE lv_name.
    WHEN 'Sequence'.
      build_sequence(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Alternative'.
      build_alternative(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Nonterminal'.
      build_nonterminal(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Terminal'.
      build_terminal(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Role'.
      build_role(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Option'.
      build_option(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Permutation'.
      build_permutation(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Iteration'.
      build_iteration(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN 'Optionlist'.
      build_optionlist(
          ii_rule   = ii_rule
          io_before = io_before
          io_after  = io_after ).
    WHEN OTHERS.
      BREAK-POINT.
  ENDCASE.

ENDMETHOD.


METHOD build_alternative.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node.


  li_children = ii_rule->get_children( ).

  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_child = li_child->get_first_child( ). " get rid of <Alt> tag
    build( ii_rule   = li_child
           io_before = io_before
           io_after  = io_after ).
  ENDDO.

ENDMETHOD.


METHOD build_iteration.

  DATA: li_child  TYPE REF TO if_ixml_node,
        lo_dummy1 TYPE REF TO lcl_node,
        lo_dummy2 TYPE REF TO lcl_node.


  li_child = ii_rule->get_first_child( ).

  CREATE OBJECT lo_dummy1
    EXPORTING
      iv_type  = gc_dummy
      iv_value = 'Iteration'.                               "#EC NOTEXT

  CREATE OBJECT lo_dummy2
    EXPORTING
      iv_type  = gc_dummy
      iv_value = 'Iteration'.                               "#EC NOTEXT

  io_before->edge( lo_dummy1 ).
  lo_dummy2->edge( io_after ).
  lo_dummy2->edge( lo_dummy1 ).

  zcl_aoc_parser=>build(
      ii_rule   = li_child
      io_before = lo_dummy1
      io_after  = lo_dummy2 ).

ENDMETHOD.


METHOD build_nonterminal.

  DATA: lv_rulename TYPE string,
        lo_node     TYPE REF TO lcl_node.


  lv_rulename = ii_rule->get_value( ).

  CREATE OBJECT lo_node
    EXPORTING
      iv_type  = gc_nonterminal
      iv_value = lv_rulename.

  io_before->edge( lo_node ).
  lo_node->edge( io_after ).

ENDMETHOD.


METHOD build_option.

  DATA: li_child TYPE REF TO if_ixml_node.


  li_child = ii_rule->get_first_child( ).

  io_before->edge( io_after ).

  zcl_aoc_parser=>build(
      ii_rule   = li_child
      io_before = io_before
      io_after  = io_after ).

ENDMETHOD.


METHOD build_optionlist.

  DATA: li_children     TYPE REF TO if_ixml_node_list,
        li_append       TYPE REF TO if_ixml_node_list,
        li_child        TYPE REF TO if_ixml_node,
        lv_last         TYPE abap_bool,
        lo_before       TYPE REF TO lcl_node,
        lo_after        TYPE REF TO lcl_node,
        lo_seq_before   TYPE REF TO lcl_node,
        lo_seq_after    TYPE REF TO lcl_node,
        lt_opt          TYPE TABLE OF REF TO if_ixml_node_list,
        li_opt          LIKE LINE OF lt_opt.


  li_children = ii_rule->get_children( ).
  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_append = li_child->get_children( ). " get rid of <Opt> tag
    APPEND li_append TO lt_opt.
  ENDDO.

  lo_before = io_before.

  LOOP AT lt_opt INTO li_opt.
    IF sy-tabix = lines( lt_opt ).
      lo_after = io_after.
    ELSE.
      CREATE OBJECT lo_after
        EXPORTING
          iv_type  = gc_dummy
          iv_value = 'Optionlist'.
    ENDIF.
    lo_before->edge( lo_after ).

    lv_last = abap_false.

    lo_seq_before = lo_before.

    DO li_opt->get_length( ) TIMES.
      IF li_opt->get_length( ) = sy-index.
        lv_last = abap_true.
      ENDIF.

      CREATE OBJECT lo_seq_after
        EXPORTING
          iv_type  = gc_dummy
          iv_value = 'Optionlist Seq'.

      li_child = li_opt->get_item( sy-index - 1 ).
      build( ii_rule   = li_child
             io_before = lo_seq_before
             io_after  = lo_seq_after ).

      IF lv_last = abap_true.
        lo_seq_after->edge( lo_after ).
      ELSE.
        lo_seq_before = lo_seq_after.
      ENDIF.

    ENDDO.

    lo_before = lo_after.
  ENDLOOP.

ENDMETHOD.


METHOD build_permutation.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_append   TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node,
        lt_perm     TYPE tt_perm,
        lv_index    TYPE i,
        lo_before   TYPE REF TO lcl_node,
        lo_after    TYPE REF TO lcl_node,
        lt_index    TYPE TABLE OF i,
        lt_per      TYPE TABLE OF REF TO if_ixml_node_list.


  li_children = ii_rule->get_children( ).
  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_append = li_child->get_children( ). " get rid of <Per> tag
    APPEND li_append TO lt_per.
  ENDDO.

* todo, hmm, too slow?

* only max items in permutation and then options around each?
* graph will be a lot smaller
  permute( EXPORTING iv_count = lines( lt_per )
           IMPORTING et_perm  = lt_perm ).

  LOOP AT lt_perm INTO lt_index.

    lo_before = io_before.

    LOOP AT lt_index INTO lv_index.
      IF sy-tabix = lines( lt_index ).
        lo_after = io_after.
      ELSE.
        CREATE OBJECT lo_after
          EXPORTING
            iv_type  = gc_dummy
            iv_value = 'Permutation'.
      ENDIF.

      READ TABLE lt_per INDEX lv_index INTO li_children.
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.

      DO li_children->get_length( ) TIMES.

        li_child = li_children->get_item( sy-index - 1 ).
        build( ii_rule   = li_child
               io_before = lo_before
               io_after  = lo_after ).

      ENDDO.

      lo_before = lo_after.

    ENDLOOP.
  ENDLOOP.

* old stuff, doesnt work

*  LOOP AT lt_per INTO li_children.
*    DO li_children->get_length( ) TIMES.
*      li_child = li_children->get_item( sy-index - 1 ).
*
*      build( ii_rule   = li_child
*             io_before = io_before
*             io_after  = io_after ).
*    ENDDO.
*  ENDLOOP.

ENDMETHOD.


METHOD build_role.

  DATA: lo_node  TYPE REF TO lcl_node,
        lv_value TYPE string.


  lv_value = ii_rule->get_value( ).


  CREATE OBJECT lo_node
    EXPORTING
      iv_type  = gc_role
      iv_value = lv_value.

  io_before->edge( lo_node ).
  lo_node->edge( io_after ).

ENDMETHOD.


METHOD build_sequence.

  DATA: lo_before   TYPE REF TO lcl_node,
        lo_after    TYPE REF TO lcl_node,
        li_children TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node.


  li_children = ii_rule->get_children( ).

  lo_before = io_before.

  DO li_children->get_length( ) TIMES.
    IF sy-index = li_children->get_length( ).
      lo_after = io_after.
    ELSE.
      CREATE OBJECT lo_after
        EXPORTING
          iv_type  = gc_dummy
          iv_value = 'Sequence'.                            "#EC NOTEXT
    ENDIF.
    li_child = li_children->get_item( sy-index - 1 ).
    build( ii_rule   = li_child
           io_before = lo_before
           io_after  = lo_after ).
    lo_before = lo_after.
  ENDDO.

ENDMETHOD.


METHOD build_terminal.

  DATA: lo_node  TYPE REF TO lcl_node,
        lv_value TYPE string.


  lv_value = ii_rule->get_value( ).


  CREATE OBJECT lo_node
    EXPORTING
      iv_type  = gc_terminal
      iv_value = lv_value.

  io_before->edge( lo_node ).
  lo_node->edge( io_after ).

ENDMETHOD.


METHOD graph_build.

  DATA: li_rule TYPE REF TO if_ixml_node.


  xml_get(
    IMPORTING
      ei_rule     = li_rule
    CHANGING
      cv_rulename = cv_rulename ).

  CREATE OBJECT eo_start
    EXPORTING
      iv_type  = gc_start
      iv_value = cv_rulename.

  CREATE OBJECT eo_end
    EXPORTING
      iv_type  = gc_end
      iv_value = cv_rulename.

  build( ii_rule   = li_rule
         io_before = eo_start
         io_after  = eo_end ).

  IF gv_debug = abap_true.
    graph_download(
        iv_rulename = cv_rulename
        io_start    = eo_start ).
  ENDIF.

ENDMETHOD.


METHOD graph_download.

  DATA: lv_text     TYPE string,
        lv_desktop  TYPE string,
        lv_filename TYPE string,
        lt_data     TYPE TABLE OF string.


  cl_gui_frontend_services=>get_desktop_directory(
    CHANGING
      desktop_directory    = lv_desktop
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4 ).
  IF sy-subrc <> 0.
    BREAK-POINT.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  cl_gui_cfw=>flush( ).

  lv_text = graph_to_text( io_start ).
  CLEAR lt_data.
  APPEND lv_text TO lt_data.

  lv_filename = lv_desktop && '\graphviz\' && iv_rulename && '.txt'. "#EC NOTEXT

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename                  = lv_filename
    CHANGING
      data_tab                  = lt_data
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24
         ).
  IF sy-subrc <> 0.
    BREAK-POINT.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD graph_to_text.

  DATA: lt_nodes TYPE TABLE OF REF TO lcl_node,
        lv_label TYPE string,
        lo_node  TYPE REF TO lcl_node,
        lv_node  TYPE string,
        lo_edge  TYPE REF TO lcl_node,
        lv_edge  TYPE string.


  DEFINE _out.
    rv_text = rv_text && &1 && cl_abap_char_utilities=>cr_lf.
  END-OF-DEFINITION.

  APPEND io_node TO lt_nodes.

  _out 'digraph {'.                                         "#EC NOTEXT
  LOOP AT lt_nodes INTO lo_node.
    lv_label = 'Type\n' && lo_node->mv_type && '|' &&
               'Value\n' && lo_node->mv_value.              "#EC NOTEXT
    lv_node = 'node' && lo_node->mv_key &&
              ' [label = "' && lv_label &&
              '" shape = "record"];'.                       "#EC NOTEXT
    _out lv_node.
    LOOP AT lo_node->mt_edges INTO lo_edge.
      lv_edge = 'node' && lo_node->mv_key &&
                ' -> node' && lo_edge->mv_key && ';'.       "#EC NOTEXT
      _out lv_edge.
      READ TABLE lt_nodes FROM lo_edge TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND lo_edge TO lt_nodes.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  _out '}'.

ENDMETHOD.


METHOD match.

  DATA: lt_tokens     TYPE stokesx_tab,
        lt_statements TYPE sstmnt_tab.


  SCAN ABAP-SOURCE it_code
       TOKENS          INTO lt_tokens
       STATEMENTS      INTO lt_statements
       WITH ANALYSIS
       WITH COMMENTS.

  gv_debug = iv_debug.

  rv_match = parse( it_tokens     = lt_tokens
                    it_statements = lt_statements ).

* todo, reduce graph, clone graph, cache, shared memory?

ENDMETHOD.


METHOD parse.

  DATA: lv_rulename TYPE string,
        lo_start TYPE REF TO lcl_node.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement> WHERE terminator = '.'.

    CLEAR gt_tokens.
    LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
      APPEND <ls_token>-str TO gt_tokens.
    ENDLOOP.

    READ TABLE gt_tokens INDEX 1 INTO lv_rulename.

* todo, always start with rule = START?

    graph_build( IMPORTING eo_start = lo_start
                 CHANGING cv_rulename = lv_rulename ).

    gv_end_rule = lv_rulename.

    rv_match = walk( io_node  = lo_start
                     iv_index = 1 ).
    IF rv_match = abap_false.
      RETURN.
    ENDIF.

  ENDLOOP.

  IF sy-subrc = 0.
    rv_match = abap_true.
  ELSE.
    rv_match = abap_false.
  ENDIF.

ENDMETHOD.


METHOD permute.

  DATA: lv_length TYPE i,
        lv_value  TYPE i,
        lt_perm   LIKE LINE OF et_perm,
        lt_new    LIKE LINE OF et_perm.


  DO iv_count TIMES.
    CLEAR lt_perm.
    APPEND sy-index TO lt_perm.
    APPEND lt_perm TO et_perm.
  ENDDO.

  DO iv_count - 1 TIMES.
    lv_length = sy-index + 1.

    LOOP AT et_perm INTO lt_perm.

      IF lines( lt_perm ) <> lv_length - 1.
        CONTINUE.
      ENDIF.

      DO iv_count TIMES.
        lt_new = lt_perm.

        lv_value = sy-index.
        READ TABLE lt_new FROM lv_value TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND lv_value TO lt_new.
          APPEND lt_new TO et_perm.
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDDO.

ENDMETHOD.


METHOD walk.

  CASE io_node->mv_type.
    WHEN gc_dummy OR gc_start.
      rv_match = walk_node( io_node = io_node
                            iv_index = iv_index ).
    WHEN gc_end.
      rv_match = walk_end( io_node = io_node
                           iv_index = iv_index ).
    WHEN gc_role.
      rv_match = walk_role( io_node = io_node
                            iv_index = iv_index ).
    WHEN gc_terminal.
      rv_match = walk_terminal( io_node = io_node
                                iv_index = iv_index ).
    WHEN gc_nonterminal.
      rv_match = walk_nonterminal( io_node = io_node
                                   iv_index = iv_index ).
  ENDCASE.

ENDMETHOD.


METHOD walk_end.

  IF iv_index = lines( gt_tokens ) + 1
      AND io_node->mv_value = gv_end_rule.
    rv_match = abap_true.
    RETURN.
  ENDIF.

  rv_match = walk_node( io_node  = io_node
                        iv_index = iv_index ).

ENDMETHOD.


METHOD walk_node.

  DATA: lo_node TYPE REF TO lcl_node.


  LOOP AT io_node->mt_edges INTO lo_node.
    rv_match = walk( io_node  = lo_node
                     iv_index = iv_index ).
    IF rv_match = abap_true.
      RETURN.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD walk_nonterminal.

  DATA: lv_rulename TYPE string,
        lo_start    TYPE REF TO lcl_node,
        lo_end      TYPE REF TO lcl_node,
        lo_node     TYPE REF TO lcl_node.


  lv_rulename = io_node->mv_value.
  graph_build(
    IMPORTING
      eo_start    = lo_start
      eo_end      = lo_end
    CHANGING
      cv_rulename = lv_rulename ).

  IF NOT lo_start IS BOUND.
    BREAK-POINT.
  ENDIF.

* add edges from io_node to end of nonterminal graph
  LOOP AT io_node->mt_edges INTO lo_node.
    lo_end->edge( lo_node ).
  ENDLOOP.

  rv_match = walk_node( io_node  = lo_start
                        iv_index = iv_index ).

ENDMETHOD.


METHOD walk_role.

  DATA: lv_stack TYPE string.


  READ TABLE gt_tokens INDEX iv_index INTO lv_stack.
  IF sy-subrc <> 0.
    rv_match = abap_false.
    RETURN.
  ENDIF.

  CASE io_node->mv_value.
    WHEN 'FieldId'.
      FIND REGEX '^[a-zA-Z0-9_\-]+$' IN lv_stack.           "#EC NOTEXT
      IF sy-subrc <> 0.
        FIND REGEX '^''.*''$' IN lv_stack.
      ENDIF.
    WHEN 'FieldIdW'
        OR 'FieldDefId'
        OR 'ItabFieldId'
        OR 'FieldListId'
        OR 'MethodDefId'
        OR 'TypeId'
        OR 'FormParamId'
        OR 'FormId'
        OR 'FieldCompId'.
      FIND REGEX '^[a-zA-Z0-9_\-]+$' IN lv_stack.           "#EC NOTEXT
    WHEN 'ClassrefFieldId'.
      FIND REGEX '^[a-zA-Z0-9_]+$' IN lv_stack.             "#EC NOTEXT
    WHEN 'FunctionId'.
      FIND REGEX '^''.*''$' IN lv_stack.
    WHEN 'ScreenId'.
      FIND REGEX '^[0-9]+$' IN lv_stack.
    WHEN 'MethodId('
        OR 'MethodId'.
* hmm, chained, todo?
      FIND REGEX '^[a-zA-Z0-9_\=\->]+\($' IN lv_stack.      "#EC NOTEXT
    WHEN 'LocationId'.
      BREAK-POINT.
    WHEN 'SwitchId'.
      BREAK-POINT.
    WHEN 'SelOptId'.
      BREAK-POINT.
    WHEN 'LdbNodeId'.
      BREAK-POINT.
    WHEN 'ClassexcrefFieldId'.
      BREAK-POINT.
    WHEN 'ClassexcTypeId'.
      BREAK-POINT.
    WHEN 'FieldSymbolDefId'.
      BREAK-POINT.
    WHEN 'MacroId'.
      BREAK-POINT.
    WHEN 'FieldGroupId'.
      BREAK-POINT.
    WHEN 'ComponentId'.
      FIND REGEX '^\*$' IN lv_stack.
    WHEN 'MessageNumber'.
      BREAK-POINT.
    WHEN 'ProgramId'.
      BREAK-POINT.
    WHEN 'MacroDefId'.
      BREAK-POINT.
    WHEN 'ProgramDefId'.
      BREAK-POINT.
    WHEN 'BlockDefId'.
      BREAK-POINT.
    WHEN OTHERS.
      BREAK-POINT.
  ENDCASE.

  IF sy-subrc = 0.
    rv_match = walk_node( io_node = io_node
                          iv_index = iv_index + 1 ).
  ELSE.
    rv_match = abap_false.
  ENDIF.

ENDMETHOD.


METHOD walk_terminal.

  DATA: lv_token TYPE string.


  READ TABLE gt_tokens INDEX iv_index INTO lv_token.
  IF sy-subrc <> 0.
    rv_match = abap_false.
    RETURN.
  ENDIF.

  IF lv_token <> io_node->mv_value.
    rv_match = abap_false.
    RETURN.
  ENDIF.

  rv_match = walk_node( io_node  = io_node
                        iv_index = iv_index + 1 ).

ENDMETHOD.


METHOD xml_get.

  STATICS: lt_syntax TYPE syntax_tt.

  DATA: lv_rulename TYPE ssyntaxstructure-rulename.

  FIELD-SYMBOLS: <ls_syntax> LIKE LINE OF lt_syntax.


  IF lt_syntax[] IS INITIAL.
    SELECT * FROM ssyntaxstructure INTO TABLE lt_syntax.    "#EC *
  ENDIF.

  lv_rulename = cv_rulename. " type conversion
  READ TABLE lt_syntax ASSIGNING <ls_syntax> WITH KEY rulename = lv_rulename.
  IF sy-subrc <> 0.
* todo, this is wrong,
    READ TABLE lt_syntax ASSIGNING <ls_syntax> WITH KEY rulename = 'START'.
    cv_rulename = <ls_syntax>-rulename.
  ENDIF.


  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>([A-Z]*)</Terminal>' &&
    '<Terminal>#NWS_MINUS_NWS#</Terminal><Terminal>([A-Z]*)</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>$1-$2</Terminal>' IGNORING CASE.


  REPLACE ALL OCCURRENCES OF REGEX
    '<Role>MethodId</Role><Terminal>#NWS_LPAREN#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Role>MethodId(</Role>' IGNORING CASE.
* todo #RPAREN_NWS# ?


  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#LPAREN#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>(</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#RPAREN#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>)</Terminal>' IGNORING CASE.


  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#PLUS#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>+</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#MINUS#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>-</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#ASTERISK#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>*</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#SLASH#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>/</Terminal>' IGNORING CASE.

* #ASTERISK_NWS# ?

  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>%_SORTMODE</Terminal>'
      IN <ls_syntax>-description
      WITH '' IGNORING CASE.

  ei_rule = xml_parse( <ls_syntax>-description ).

* todo, better caching

ENDMETHOD.


METHOD xml_parse.

  DATA: li_ixml           TYPE REF TO if_ixml,
        li_xml_doc        TYPE REF TO if_ixml_document,
        li_type           TYPE REF TO if_ixml_node,
        li_view           TYPE REF TO if_ixml_element,
        li_attr           TYPE REF TO if_ixml_named_node_map,
        lv_type           TYPE string,
        li_stream_factory TYPE REF TO if_ixml_stream_factory,
        li_istream        TYPE REF TO if_ixml_istream,
        li_parser         TYPE REF TO if_ixml_parser.


  li_ixml = cl_ixml=>create( ).
  li_xml_doc = li_ixml->create_document( ).

  li_stream_factory = li_ixml->create_stream_factory( ).
  li_istream = li_stream_factory->create_istream_string( iv_xml ).
  li_parser = li_ixml->create_parser( stream_factory = li_stream_factory
                                      istream        = li_istream
                                      document       = li_xml_doc ).
  IF li_parser->parse( ) <> 0.
    BREAK-POINT.
  ENDIF.

  li_istream->close( ).

  li_view = li_xml_doc->find_from_name( depth = 0 name = 'View' ). "#EC NOTEXT
* todo, get Obsolete view instead?

  li_attr = li_view->get_attributes( ).
  li_type = li_attr->get_named_item( 'type' ).              "#EC NOTEXT
  lv_type = li_type->get_value( ).
  IF lv_type = 'Private'.
    RETURN.
  ENDIF.

  ri_rule = li_view->get_first_child( ).

ENDMETHOD.
ENDCLASS.