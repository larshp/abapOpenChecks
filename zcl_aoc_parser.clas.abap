class ZCL_AOC_PARSER definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!

  type-pools ABAP .
  class-methods RUN
    importing
      !IT_CODE type STRING_TABLE
      !IV_DEBUG type ABAP_BOOL default ABAP_FALSE
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

  class-methods XML_DOWNLOAD
    importing
      !IV_RULENAME type STRING
      !II_XML type ref to IF_IXML
      !II_XML_DOC type ref to IF_IXML_DOCUMENT .
  class-methods WALK
    importing
      !IO_NODE type ref to LCL_NODE
      !IV_INDEX type I
    returning
      value(RV_MATCH) type ABAP_BOOL .
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
    importing
      !IV_RULENAME type STRING
    exporting
      !EO_START type ref to LCL_NODE
      !EO_END type ref to LCL_NODE .
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
    importing
      !IV_RULENAME type STRING
    returning
      value(RI_RULE) type ref to IF_IXML_NODE .
  class-methods XML_PARSE
    importing
      !IV_RULENAME type STRING
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
        lo_dummy    TYPE REF TO lcl_node,
        li_child    TYPE REF TO if_ixml_node.


  CREATE OBJECT lo_dummy
    EXPORTING
      iv_type  = gc_dummy
      iv_value = 'Alternative'.                             "#EC NOTEXT

  li_children = ii_rule->get_children( ).

  io_before->edge( lo_dummy ).

  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_child = li_child->get_first_child( ). " get rid of <Alt> tag
    build( ii_rule   = li_child
           io_before = lo_dummy
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
      iv_value = 'IterationStart'.                          "#EC NOTEXT

  CREATE OBJECT lo_dummy2
    EXPORTING
      iv_type  = gc_dummy
      iv_value = 'IterationEnd'.                            "#EC NOTEXT

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

  DATA: li_child TYPE REF TO if_ixml_node,
        lo_dummy TYPE REF TO lcl_node.


  CREATE OBJECT lo_dummy
    EXPORTING
      iv_type  = gc_dummy
      iv_value = 'Option'.                                  "#EC NOTEXT

  li_child = ii_rule->get_first_child( ).

  io_before->edge( lo_dummy ).
  lo_dummy->edge( io_after ).

  zcl_aoc_parser=>build(
      ii_rule   = li_child
      io_before = lo_dummy
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
          iv_value = 'Optionlist'.                          "#EC NOTEXT
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
          iv_value = 'Optionlist Seq'.                      "#EC NOTEXT

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

  TYPES: BEGIN OF st_pair,
           before TYPE REF TO lcl_node,
           after TYPE REF TO lcl_node,
         END OF st_pair.

  DATA: li_children   TYPE REF TO if_ixml_node_list,
        li_append     TYPE REF TO if_ixml_node_list,
        li_child      TYPE REF TO if_ixml_node,
        lv_index      TYPE i,
        lo_before     TYPE REF TO lcl_node,
        lv_name       TYPE string,
        lo_after      TYPE REF TO lcl_node,
        lo_seq_before TYPE REF TO lcl_node,
        lo_seq_after  TYPE REF TO lcl_node,
        lt_pair       TYPE TABLE OF st_pair,
        lt_per        TYPE TABLE OF REF TO if_ixml_node_list.

  FIELD-SYMBOLS: <ls_pair> LIKE LINE OF lt_pair,
                 <ls_to>   LIKE LINE OF lt_pair.


* good enough, but allows statements like ASCENDING ASCENDING

  li_children = ii_rule->get_children( ).
  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_append = li_child->get_children( ). " get rid of <Per> tag
    APPEND li_append TO lt_per.
  ENDDO.

  io_before->edge( io_after ).

  LOOP AT lt_per INTO li_children.

    CREATE OBJECT lo_before
      EXPORTING
        iv_type  = gc_dummy
        iv_value = 'PerBefore'.
    io_before->edge( lo_before ).

    CREATE OBJECT lo_after
      EXPORTING
        iv_type  = gc_dummy
        iv_value = 'PerAfter'.
    lo_after->edge( io_after ).

    APPEND INITIAL LINE TO lt_pair ASSIGNING <ls_pair>.
    <ls_pair>-before = lo_before.
    <ls_pair>-after  = lo_after.

    DO li_children->get_length( ) TIMES.
      lv_index = sy-index.

      li_child = li_children->get_item( sy-index - 1 ).

* permutations are always treated at optional, so make sure its not possible
* to cycle in the graph without meeting terminals
      IF li_children->get_length( ) = 1.
        lv_name = li_child->get_name( ).
        IF lv_name = 'Option'.
          li_child = li_child->get_first_child( ).
        ENDIF.
      ENDIF.

* handle un<Sequence>d permutations
      IF lv_index = 1.
        lo_seq_before = lo_before.
      ELSE.
        lo_seq_before = lo_seq_after.
      ENDIF.

      CREATE OBJECT lo_seq_after
        EXPORTING
          iv_type  = gc_dummy
          iv_value = 'PerSeq'.

      build( ii_rule   = li_child
             io_before = lo_seq_before
             io_after  = lo_seq_after ).

      IF lv_index = li_children->get_length( ).
        lo_seq_after->edge( lo_after ).
      ENDIF.

    ENDDO.

  ENDLOOP.

  LOOP AT lt_pair ASSIGNING <ls_pair>.
    LOOP AT lt_pair ASSIGNING <ls_to> WHERE before <> <ls_pair>-before.
      <ls_pair>-after->edge( <ls_to>-before ).
    ENDLOOP.
  ENDLOOP.

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


  li_rule = xml_get( iv_rulename ).

  CREATE OBJECT eo_start
    EXPORTING
      iv_type  = gc_start
      iv_value = iv_rulename.

  CREATE OBJECT eo_end
    EXPORTING
      iv_type  = gc_end
      iv_value = iv_rulename.

  build( ii_rule   = li_rule
         io_before = eo_start
         io_after  = eo_end ).

  IF gv_debug = abap_true.
    graph_download(
        iv_rulename = iv_rulename
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
      OTHERS                    = 24 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD graph_to_text.

* see http://www.graphviz.org/

  DATA: lt_nodes TYPE TABLE OF REF TO lcl_node,
        lv_label TYPE string,
        lo_node  TYPE REF TO lcl_node,
        lv_node  TYPE string,
        lo_edge  TYPE REF TO lcl_node,
        lv_edge  TYPE string,
        lv_value TYPE string.


  DEFINE _out.
    rv_text = rv_text && &1 && cl_abap_char_utilities=>cr_lf.
  END-OF-DEFINITION.

  APPEND io_node TO lt_nodes.

  _out 'digraph {'.                                         "#EC NOTEXT
  LOOP AT lt_nodes INTO lo_node.
    lv_value = lo_node->mv_value.

* escape some characters
    REPLACE ALL OCCURRENCES OF '>' IN lv_value WITH '\>'.
    REPLACE ALL OCCURRENCES OF '<' IN lv_value WITH '\<'.

    lv_label = 'Type\n' && lo_node->mv_type && '|' &&
               'Value\n' && lv_value.                       "#EC NOTEXT
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


METHOD parse.

  DATA: lo_start TYPE REF TO lcl_node.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement> WHERE terminator = '.'.

    CLEAR gt_tokens.
    LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
      APPEND <ls_token>-str TO gt_tokens.
    ENDLOOP.
    APPEND '.' TO gt_tokens.

* set start+end rule
    gv_end_rule = 'START'.

    graph_build( EXPORTING iv_rulename = gv_end_rule
                 IMPORTING eo_start = lo_start ).

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


METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

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

* todo, clone graph, cache, shared memory?

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

  IF lv_rulename = 'MACRO'.
* macro call makes everything valid ABAP code
    rv_match = abap_false.
    RETURN.
  ENDIF.

  graph_build(
    EXPORTING
      iv_rulename = lv_rulename
    IMPORTING
      eo_start    = lo_start
      eo_end      = lo_end ).

  ASSERT lo_start IS BOUND.
  ASSERT lo_end IS BOUND.

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
      FIND REGEX '^[a-zA-Z0-9_\-]+["\[\]"]*$' IN lv_stack.  "#EC NOTEXT
      IF sy-subrc <> 0.
        FIND REGEX '^''.*''$' IN lv_stack.
      ENDIF.
    WHEN 'FieldIdW'
        OR 'FieldDefId'
        OR 'ItabFieldId'
        OR 'FieldListId'
        OR 'MethodDefId'
        OR 'LdbNodeId'
        OR 'MacroId'
        OR 'TypeId'
        OR 'FormParamId'
        OR 'FormId'
        OR 'ClasstypeDefId'
        OR 'SwitchId'
        OR 'BlockDefId'
        OR 'ClassexcTypeId'
        OR 'ClassexcrefFieldId'
        OR 'FieldCompId'.
      FIND REGEX '^[a-zA-Z0-9_\-]+$' IN lv_stack.           "#EC NOTEXT
    WHEN 'ClassrefFieldId'.
      FIND REGEX '^[a-zA-Z0-9_]+$' IN lv_stack.             "#EC NOTEXT
    WHEN 'FunctionId'.
      FIND REGEX '^''.*''$' IN lv_stack.
    WHEN 'ScreenId'.
      FIND REGEX '^[0-9]+$' IN lv_stack.
    WHEN 'MethodId('.
      FIND REGEX '^[a-zA-Z0-9_\=\->]+\($' IN lv_stack.      "#EC NOTEXT
    WHEN 'MethodId'.
      FIND REGEX '^[a-zA-Z0-9_\=\->]+\(?$' IN lv_stack.     "#EC NOTEXT
    WHEN 'LocationId'.
      FIND REGEX '^/?[0-9]*["("0-9")"]*$' IN lv_stack.
    WHEN 'SelOptId'.
      rv_match = abap_false.
      RETURN.
    WHEN 'FieldSymbolDefId'.
      FIND REGEX '^<[a-zA-Z0-9_\-]+>$' IN lv_stack.
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


  rv_match = abap_true.

  READ TABLE gt_tokens INDEX iv_index INTO lv_token.
  IF sy-subrc <> 0.
    rv_match = abap_false.
  ENDIF.

  CASE io_node->mv_value.
    WHEN '#ILITERAL#'.
      FIND REGEX '^[0-9]+$' IN lv_token.
      IF sy-subrc <> 0.
        rv_match = abap_false.
      ENDIF.
    WHEN OTHERS.
      IF lv_token <> io_node->mv_value.
        rv_match = abap_false.
      ENDIF.
  ENDCASE.

  IF rv_match = abap_false.
    RETURN.
  ENDIF.

  rv_match = walk_node( io_node  = io_node
                        iv_index = iv_index + 1 ).

ENDMETHOD.


METHOD xml_download.

  DATA: lv_desktop       TYPE string,
        lv_filename      TYPE string,
        lv_xml           TYPE string,
        lt_data          TYPE TABLE OF string,
        li_ostream       TYPE REF TO if_ixml_ostream,
        li_renderer      TYPE REF TO if_ixml_renderer,
        li_streamfactory TYPE REF TO if_ixml_stream_factory.


  cl_gui_frontend_services=>get_desktop_directory(
    CHANGING
      desktop_directory    = lv_desktop
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  cl_gui_cfw=>flush( ).

  li_streamfactory = ii_xml->create_stream_factory( ).
  li_ostream = li_streamfactory->create_ostream_cstring( lv_xml ).
  li_renderer = ii_xml->create_renderer( ostream  = li_ostream
                                         document = ii_xml_doc ).
  li_renderer->set_normalizing( ).
  li_renderer->render( ).

* make sure newlines work in notepad
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
    IN lv_xml WITH cl_abap_char_utilities=>cr_lf.

  APPEND lv_xml TO lt_data.

  lv_filename = lv_desktop && '\graphviz\' && iv_rulename && '.xml'. "#EC NOTEXT

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
      OTHERS                    = 24 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD xml_get.

  STATICS: lt_syntax TYPE syntax_tt.

  DATA: lv_rulename TYPE ssyntaxstructure-rulename.

  FIELD-SYMBOLS: <ls_syntax> LIKE LINE OF lt_syntax.


  IF lt_syntax[] IS INITIAL.
    SELECT * FROM ssyntaxstructure INTO TABLE lt_syntax.    "#EC *
  ENDIF.

  lv_rulename = iv_rulename. " type conversion
  READ TABLE lt_syntax ASSIGNING <ls_syntax> WITH KEY rulename = lv_rulename.
  ASSERT sy-subrc = 0.

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

* comparison operators
  REPLACE ALL OCCURRENCES OF REGEX
    '<Terminal>#LT_NWS#</Terminal><Terminal>#NWS_GT#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>&lt;&gt;</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX
    '<Terminal>#GT_NWS#</Terminal><Terminal>#NWS_LT#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>&gt;&lt;</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#LT#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>&lt;</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#LT_NWS#</Terminal><Terminal>=</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>&lt;=</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>=</Terminal><Terminal>#NWS_LT#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>=&lt;</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#GT#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>&gt;</Terminal>' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>#GT_NWS#</Terminal><Terminal>=</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>&gt;=</Terminal>' IGNORING CASE.

  ri_rule = xml_parse( iv_rulename = iv_rulename
                       iv_xml      = <ls_syntax>-description ).

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
  ASSERT li_parser->parse( ) = 0.

  li_istream->close( ).

  IF gv_debug = abap_true.
    xml_download(
        iv_rulename = iv_rulename
        ii_xml      = li_ixml
        ii_xml_doc  = li_xml_doc ).
  ENDIF.

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