class ZCL_AOC_PARSER definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!

  type-pools ABAP .
  class-methods PARSE_STR
    importing
      !IT_CODE type STRING_TABLE
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods PARSE
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods CLASS_CONSTRUCTOR .
protected section.
*"* protected components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_PARSER
*"* do not include other source files here!!!

  class-data GT_SYNTAX type SYNTAX_TT .
  class-data GT_STACK type STRING_TABLE .
  class-data GV_INDEX type I .

  type-pools ABAP .
  class-methods RULE
    importing
      !II_RULE type ref to IF_IXML_NODE
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods RULE_ALTERNATIVE
    importing
      !II_RULE type ref to IF_IXML_NODE
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods RULE_NONTERMINAL
    importing
      !II_RULE type ref to IF_IXML_NODE
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods RULE_SEQUENCE
    importing
      !II_RULE type ref to IF_IXML_NODE
    returning
      value(RV_MATCH) type ABAP_BOOL .
  class-methods RULE_TERMINAL
    importing
      !II_RULE type ref to IF_IXML_NODE
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
ENDCLASS.



CLASS ZCL_AOC_PARSER IMPLEMENTATION.


METHOD class_constructor.

  SELECT * FROM ssyntaxstructure INTO TABLE gt_syntax.      "#EC *

ENDMETHOD.


METHOD parse.

  DATA: lv_rulename TYPE string,
        lv_match    TYPE abap_bool.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  DATA: li_rule TYPE REF TO if_ixml_node.

  LOOP AT it_statements ASSIGNING <ls_statement>.

    CLEAR gt_stack.
    LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
      APPEND <ls_token>-str TO gt_stack.
    ENDLOOP.

    gv_index = 1.
    READ TABLE gt_stack INDEX gv_index INTO lv_rulename.

    li_rule = xml_get( lv_rulename ).

    rv_match = zcl_aoc_parser=>rule( li_rule ).
    IF rv_match = abap_false.
      RETURN.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD parse_str.

  DATA: lt_tokens     TYPE stokesx_tab,
        lt_statements TYPE sstmnt_tab.


  SCAN ABAP-SOURCE it_code
       TOKENS          INTO lt_tokens
       STATEMENTS      INTO lt_statements
       WITH ANALYSIS
       WITH COMMENTS.

  rv_match = parse( it_tokens     = lt_tokens
                    it_statements = lt_statements ).

ENDMETHOD.


METHOD rule.

  DATA lv_name TYPE string.


  lv_name = ii_rule->get_name( ).
  CASE lv_name.
    WHEN 'Sequence'.
      rv_match = rule_sequence( ii_rule ).
    WHEN 'Alternative'.
      rv_match = rule_alternative( ii_rule  ).
    WHEN 'Nonterminal'.
      rv_match = rule_nonterminal( ii_rule ).
    WHEN 'Terminal'.
      rv_match = rule_terminal( ii_rule ).
    WHEN OTHERS.
      BREAK-POINT.
  ENDCASE.

ENDMETHOD.


METHOD rule_alternative.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node.


  li_children = ii_rule->get_children( ).
  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_child = li_child->get_first_child( ). " get rid of <Alt> tag
* todo, match
    rule( li_child ).
  ENDDO.

ENDMETHOD.


METHOD rule_nonterminal.

  DATA: lv_rulename TYPE string,
        li_rule TYPE REF TO if_ixml_node.


  lv_rulename = ii_rule->get_value( ).

  li_rule = xml_get( lv_rulename ).
  rv_match = rule( li_rule ).

ENDMETHOD.


METHOD rule_sequence.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node.


  li_children = ii_rule->get_children( ).

  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
* todo, match
    rule( li_child ).
  ENDDO.

ENDMETHOD.


METHOD rule_terminal.

  DATA: lv_terminal TYPE string,
        lv_stack    TYPE string.


  lv_terminal = ii_rule->get_value( ).

  READ TABLE gt_stack INDEX gv_index INTO lv_stack.
  IF lv_stack = lv_terminal.
    rv_match = abap_true.
  ELSE.
    rv_match = abap_false.
  ENDIF.

ENDMETHOD.


METHOD xml_get.

  FIELD-SYMBOLS: <ls_syntax> LIKE LINE OF gt_syntax.


  READ TABLE gt_syntax ASSIGNING <ls_syntax> WITH KEY rulename = iv_rulename.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  ri_rule = xml_parse( <ls_syntax>-description ).

ENDMETHOD.


METHOD xml_parse.

  DATA: li_ixml           TYPE REF TO if_ixml,
        li_xml_doc        TYPE REF TO if_ixml_document,
        li_view           TYPE REF TO if_ixml_element,
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

  ri_rule = li_view->get_first_child( ).

ENDMETHOD.
ENDCLASS.