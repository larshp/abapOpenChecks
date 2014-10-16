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
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods CLASS_CONSTRUCTOR .
protected section.
*"* protected components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!

  class-data GT_TOKENS type STRING_TABLE .
  class-data GV_END_RULE type STRING .

  type-pools ABAP .
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
      iv_type  = c_dummy
      iv_value = 'Iteration'.

  CREATE OBJECT lo_dummy2
    EXPORTING
      iv_type  = c_dummy
      iv_value = 'Iteration'.

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
      iv_type  = c_nonterminal
      iv_value = lv_rulename.

  io_before->edge( lo_node ).
  lo_node->edge( io_after ).

ENDMETHOD.


METHOD build_option.

  DATA: li_child TYPE REF TO if_ixml_node,
        lo_dummy TYPE REF TO lcl_node.


  li_child = ii_rule->get_first_child( ).

*create object lo_dummy
*exporting
*iv_type = c_dummy.

  io_before->edge( io_after ).

  zcl_aoc_parser=>build(
      ii_rule   = li_child
      io_before = io_before
      io_after  = io_after ).

ENDMETHOD.


METHOD build_permutation.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_append   TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node,
        lt_per      TYPE TABLE OF REF TO if_ixml_node_list.


  li_children = ii_rule->get_children( ).
  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_append = li_child->get_children( ). " get rid of <Per> tag
    APPEND li_append TO lt_per.
  ENDDO.

* todo, hmm

  LOOP AT lt_per INTO li_children.
    DO li_children->get_length( ) TIMES.
      li_child = li_children->get_item( sy-index - 1 ).

      build( ii_rule   = li_child
             io_before = io_before
             io_after  = io_after ).
    ENDDO.
  ENDLOOP.

ENDMETHOD.


METHOD build_role.

  DATA: lo_node  TYPE REF TO lcl_node,
        lv_value TYPE string.


  lv_value = ii_rule->get_value( ).


  CREATE OBJECT lo_node
    EXPORTING
      iv_type  = c_role
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
          iv_type  = c_dummy
          iv_value = 'Sequence'.
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
      iv_type  = c_terminal
      iv_value = lv_value.

  io_before->edge( lo_node ).
  lo_node->edge( io_after ).

ENDMETHOD.


METHOD class_constructor.


ENDMETHOD.


METHOD graph_build.

  DATA: li_rule    TYPE REF TO if_ixml_node,
        lv_text    TYPE string,
        lv_desktop TYPE string,
        lt_data    TYPE TABLE OF string.


  li_rule = xml_get( iv_rulename ).

  CREATE OBJECT eo_start
    EXPORTING
      iv_type  = c_start
      iv_value = iv_rulename.

  CREATE OBJECT eo_end
    EXPORTING
      iv_type  = c_end
      iv_value = iv_rulename.

  build( ii_rule   = li_rule
         io_before = eo_start
         io_after  = eo_end ).

*****************************

  cl_gui_frontend_services=>get_desktop_directory(
    CHANGING
      desktop_directory    = lv_desktop
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
         ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  cl_gui_cfw=>flush( ).

  lv_text = graph_to_text( eo_start ).
  CLEAR lt_data.
  APPEND lv_text TO lt_data.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename                  = lv_desktop && '\graphviz\' && iv_rulename && '.txt'
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD GRAPH_TO_TEXT.

  DATA: lt_nodes TYPE TABLE OF REF TO lcl_node,
        lv_label TYPE string,
        lo_node TYPE REF TO lcl_node,
        lv_node TYPE string,
        lo_edge TYPE REF TO lcl_node,
        lv_edge TYPE string.

  DEFINE _out.
    rv_text = rv_text && &1 && cl_abap_char_utilities=>cr_lf.
  END-OF-DEFINITION.

  APPEND io_node TO lt_nodes.

  _out 'digraph {'.
  LOOP AT lt_nodes INTO lo_node.
    lv_label = 'Type\n' && lo_node->mv_type && '|' &&
               'Value\n' && lo_node->mv_value.
    lv_node = 'node' && lo_node->mv_key && ' [label = "' && lv_label && '" shape = "record"];'.
    _out lv_node.
    LOOP AT lo_node->mt_edges INTO lo_edge.
      lv_edge = 'node' && lo_node->mv_key && ' -> node' && lo_edge->mv_key && ';'.
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

  rv_match = parse( it_tokens     = lt_tokens
                    it_statements = lt_statements ).

* todo, reduce graph, clone graph, and cache?

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

    gv_end_rule = lv_rulename.

    GRAPH_BUILD( EXPORTING iv_rulename = lv_rulename
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


METHOD walk.

  CASE io_node->mv_type.
    WHEN c_dummy OR  c_start.
      rv_match = walk_node( io_node = io_node
                            iv_index = iv_index ).
    WHEN c_end.
      rv_match = walk_end( io_node = io_node
                           iv_index = iv_index ).
    WHEN c_role.
      rv_match = walk_role( io_node = io_node
                            iv_index = iv_index ).
    WHEN c_terminal.
      rv_match = walk_terminal( io_node = io_node
                                iv_index = iv_index ).
    WHEN c_nonterminal.
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

  DATA: lo_start TYPE REF TO lcl_node,
        lo_end   TYPE REF TO lcl_node,
        lo_node  TYPE REF TO lcl_node.


  graph_build(
    EXPORTING
      iv_rulename = io_node->mv_value
    IMPORTING
      eo_start    = lo_start
      eo_end      = lo_end ).

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

  DATA: lv_terminal TYPE string,
        lv_token    TYPE string.


  READ TABLE gt_tokens INDEX iv_index INTO lv_token.
  IF sy-subrc <> 0.
    rv_match = abap_false.
    RETURN.
  ENDIF.

  IF lv_token <> io_node->mv_value.
    rv_match = abap_false.
    RETURN.
  ENDIF.

  rv_match = walk_node( io_node = io_node
                        iv_index = iv_index + 1 ).

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
  IF sy-subrc <> 0.
    READ TABLE lt_syntax ASSIGNING <ls_syntax> WITH KEY rulename = 'COMPUTE'.
  ENDIF.


  REPLACE ALL OCCURRENCES OF REGEX '<Terminal>([A-Z]*)</Terminal>' &&
    '<Terminal>#NWS_MINUS_NWS#</Terminal><Terminal>([A-Z]*)</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>$1-$2</Terminal>' IGNORING CASE.


  REPLACE ALL OCCURRENCES OF REGEX '<Role>MethodId</Role><Terminal>#NWS_LPAREN#</Terminal>'
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

  ri_rule = xml_parse( <ls_syntax>-description ).

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
  li_type = li_attr->get_named_item( 'type' ).
  lv_type = li_type->get_value( ).
  IF lv_type = 'Private'.
    RETURN.
  ENDIF.

  ri_rule = li_view->get_first_child( ).

ENDMETHOD.
ENDCLASS.