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
  class-data GT_TOKENS type STRING_TABLE .

  class-methods RULE
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_ALTERNATIVE
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_NONTERMINAL
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_ITERATION
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_OPTION
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_PERMUTATION
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_SEQUENCE
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_ROLE
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
  class-methods RULE_TERMINAL
    importing
      !II_RULE type ref to IF_IXML_NODE
      !IV_INDEX type I
    returning
      value(RS_RETURN) type ST_RETURN .
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
        ls_return   TYPE st_return.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  DATA: li_rule TYPE REF TO if_ixml_node.

  LOOP AT it_statements ASSIGNING <ls_statement>.

    CLEAR gt_tokens.
    LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
      APPEND <ls_token>-str TO gt_tokens.
    ENDLOOP.

    READ TABLE gt_tokens INDEX 1 INTO lv_rulename.

    li_rule = xml_get( lv_rulename ).

    ls_return = zcl_aoc_parser=>rule( ii_rule  = li_rule
                                      iv_index = 1 ).
    IF ls_return-match = abap_false.
      rv_match = abap_false.
      RETURN.
    ENDIF.
    IF ls_return-match = abap_true AND ls_return-index - 1 <> lines( gt_tokens ).
      rv_match = abap_false.
      RETURN.
    ENDIF.

  ENDLOOP.

  rv_match = abap_true.

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

  DATA: lv_name TYPE string.


  lv_name = ii_rule->get_name( ).

  CASE lv_name.
    WHEN 'Sequence'.
      rs_return = rule_sequence( ii_rule  = ii_rule
                                 iv_index = iv_index ).
    WHEN 'Alternative'.
      rs_return = rule_alternative( ii_rule  = ii_rule
                                    iv_index = iv_index ).
    WHEN 'Nonterminal'.
      rs_return = rule_nonterminal( ii_rule  = ii_rule
                                    iv_index = iv_index ).
    WHEN 'Terminal'.
      rs_return = rule_terminal( ii_rule  = ii_rule
                                 iv_index = iv_index ).
    WHEN 'Role'.
      rs_return = rule_role( ii_rule  = ii_rule
                             iv_index = iv_index ).
    WHEN 'Option'.
      rs_return = rule_option( ii_rule  = ii_rule
                               iv_index = iv_index ).
    WHEN 'Permutation'.
      rs_return = rule_permutation( ii_rule  = ii_rule
                                    iv_index = iv_index ).
    WHEN 'Iteration'.
      rs_return = rule_iteration( ii_rule  = ii_rule
                                  iv_index = iv_index ).
    WHEN OTHERS.
      BREAK-POINT.
  ENDCASE.

ENDMETHOD.


METHOD rule_alternative.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node,
        lv_best     TYPE i.


  li_children = ii_rule->get_children( ).

  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_child = li_child->get_first_child( ). " get rid of <Alt> tag
    rs_return = rule( ii_rule  = li_child
                      iv_index = iv_index ).
    IF rs_return-match = abap_true AND rs_return-index > lv_best.
      lv_best = rs_return-index.
    ENDIF.
  ENDDO.

  IF lv_best IS INITIAL.
    rs_return-match = abap_false.
    rs_return-index = iv_index.
  ELSE.
    rs_return-match = abap_true.
    rs_return-index = lv_best.
  ENDIF.

ENDMETHOD.


METHOD rule_iteration.

  DATA: li_child    TYPE REF TO if_ixml_node,
        lv_index    TYPE i.


  li_child = ii_rule->get_first_child( ).

  lv_index = iv_index.
  DO.
    rs_return = rule( ii_rule  = li_child
                      iv_index = lv_index ).
    IF rs_return-index = lv_index.
      EXIT. " current loop
    ENDIF.
    lv_index = rs_return-index.
  ENDDO.

  rs_return-match = abap_true.

ENDMETHOD.


METHOD rule_nonterminal.

  DATA: lv_rulename TYPE string,
        li_rule     TYPE REF TO if_ixml_node.


  lv_rulename = ii_rule->get_value( ).

  li_rule = xml_get( lv_rulename ).
  rs_return = rule( ii_rule  = li_rule
                    iv_index = iv_index ).

ENDMETHOD.


METHOD rule_option.

  DATA: li_child TYPE REF TO if_ixml_node.


  li_child = ii_rule->get_first_child( ).

  rs_return = rule( ii_rule  = li_child
                    iv_index = iv_index ).
  IF rs_return-match = abap_false.
    rs_return-match = abap_true.
    rs_return-index = iv_index.
  ENDIF.

ENDMETHOD.


METHOD rule_permutation.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_append   TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node,
        lt_per      TYPE TABLE OF REF TO if_ixml_node_list,
        lv_index    TYPE i,
        lv_loop     TYPE i,
        ls_return   TYPE st_return.


  li_children = ii_rule->get_children( ).
  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    li_append = li_child->get_children( ). " get rid of <Per> tag
    APPEND li_append TO lt_per.
  ENDDO.

  lv_index = iv_index.

  LOOP AT lt_per INTO li_children.
    lv_loop = sy-tabix.

    DO li_children->get_length( ) TIMES.
      li_child = li_children->get_item( sy-index - 1 ).
      ls_return = rule( ii_rule  = li_child
                        iv_index = lv_index ).
      IF ls_return-match = abap_false.
        lv_index = iv_index.
        EXIT. " current loop.
      ELSE.
        lv_index = ls_return-index.
      ENDIF.
    ENDDO.

    IF ls_return-match = abap_true.
      DELETE lt_per INDEX lv_loop.
    ENDIF.

  ENDLOOP.

  rs_return-match = abap_true.
  rs_return-index = lv_index.

ENDMETHOD.


METHOD rule_role.

  DATA: lv_role  TYPE string,
        lv_stack TYPE string.


  lv_role = ii_rule->get_value( ).

  READ TABLE gt_tokens INDEX iv_index INTO lv_stack.
  IF sy-subrc <> 0.
    rs_return-match = abap_false.
    rs_return-index = iv_index.
    RETURN.
  ENDIF.

  CASE lv_role.
    WHEN 'FieldId'
        OR 'FieldIdW'
        OR 'FieldDefId'
        OR 'ItabFieldId'
        OR 'FieldListId'
        OR 'MethodDefId'
        OR 'TypeId'
        OR 'FieldCompId'.
      FIND REGEX '^[a-zA-Z0-9_]+$' IN lv_stack.             "#EC NOTEXT
      IF sy-subrc <> 0.
* todo, this is only relevant in some cases?
        FIND REGEX '^''.*''$' IN lv_stack.
      ENDIF.
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
* todo
    WHEN 'FormParamId'.
* todo
    WHEN 'SwitchId'.
* todo
    WHEN OTHERS.
      BREAK-POINT.
  ENDCASE.

  IF sy-subrc = 0.
    rs_return-match = abap_true.
    rs_return-index = iv_index + 1.
  ELSE.
    rs_return-match = abap_false.
    rs_return-index = iv_index.
  ENDIF.


ENDMETHOD.


METHOD rule_sequence.

  DATA: li_children TYPE REF TO if_ixml_node_list,
        li_child    TYPE REF TO if_ixml_node.


  li_children = ii_rule->get_children( ).

  rs_return-index = iv_index.

  DO li_children->get_length( ) TIMES.
    li_child = li_children->get_item( sy-index - 1 ).
    rs_return = rule( ii_rule  = li_child
                      iv_index = rs_return-index ).
    IF rs_return-match = abap_false.
      RETURN.
    ENDIF.
  ENDDO.

ENDMETHOD.


METHOD rule_terminal.

  DATA: lv_terminal TYPE string,
        lv_stack    TYPE string.


  lv_terminal = ii_rule->get_value( ).

  READ TABLE gt_tokens INDEX iv_index INTO lv_stack.
  IF lv_stack = lv_terminal.
    rs_return-match = abap_true.
    rs_return-index = iv_index + 1.
  ELSE.
    rs_return-match = abap_false.
    rs_return-index = iv_index.
  ENDIF.

ENDMETHOD.


METHOD xml_get.

  FIELD-SYMBOLS: <ls_syntax> LIKE LINE OF gt_syntax.


  READ TABLE gt_syntax ASSIGNING <ls_syntax> WITH KEY rulename = iv_rulename.
  IF sy-subrc <> 0.
    READ TABLE gt_syntax ASSIGNING <ls_syntax> WITH KEY rulename = 'COMPUTE'.
  ENDIF.


  REPLACE REGEX '<Terminal>([A-Z]*)</Terminal><Terminal>#NWS_MINUS_NWS#</Terminal><Terminal>([A-Z]*)</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>$1-$2</Terminal>' IGNORING CASE.


  REPLACE REGEX '<Role>MethodId</Role><Terminal>#NWS_LPAREN#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Role>MethodId(</Role>' IGNORING CASE.
* todo #RPAREN_NWS# ?


  REPLACE REGEX '<Terminal>#LPAREN#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>(</Terminal>' IGNORING CASE.
  REPLACE REGEX '<Terminal>#RPAREN#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>)</Terminal>' IGNORING CASE.


  REPLACE REGEX '<Terminal>#PLUS#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>+</Terminal>' IGNORING CASE.
  REPLACE REGEX '<Terminal>#MINUS#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>-</Terminal>' IGNORING CASE.
  REPLACE REGEX '<Terminal>#ASTERISK#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>*</Terminal>' IGNORING CASE.
  REPLACE REGEX '<Terminal>#SLASH#</Terminal>'
    IN <ls_syntax>-description
    WITH '<Terminal>/</Terminal>' IGNORING CASE.

* #ASTERISK_NWS# ?

  ri_rule = xml_parse( <ls_syntax>-description ).

* todo, better caching

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