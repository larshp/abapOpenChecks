*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

*----------------------------------------------------------------------*
*       CLASS lcl_node IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_node IMPLEMENTATION.

  METHOD constructor.
    ASSERT NOT iv_value IS INITIAL.

    mv_type  = iv_type.
    mv_value = iv_value.
    mv_key   = gv_key.
    mv_rulename = iv_rulename.

    gv_key   = gv_key + 1.
  ENDMETHOD.                    "constructor

  METHOD edge.
    READ TABLE mt_edges FROM io_node TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND io_node TO mt_edges.
    ENDIF.
  ENDMETHOD.                    "edge

ENDCLASS.                    "lcl_node IMPLEMENTATION
