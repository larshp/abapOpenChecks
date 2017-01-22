TYPE-POOL zzaoc.

DEFINE zzaoc_top.
  DATA: lv_cancel     TYPE abap_bool,
        lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.
END-OF-DEFINITION.

DEFINE zzaoc_fill_att.
  GET REFERENCE OF &1 INTO ls_attribute-ref.
  ls_attribute-text = &2.
  ls_attribute-kind = &3.
  APPEND ls_attribute TO lt_attributes.
END-OF-DEFINITION.

DEFINE zzaoc_fill_att_rb.
  CLEAR ls_attribute.
  GET REFERENCE OF &1 INTO ls_attribute-ref.
  ls_attribute-text = &2.
  ls_attribute-kind = &3.
  ls_attribute-button_group = &4.
  APPEND ls_attribute TO lt_attributes.
END-OF-DEFINITION.

DEFINE zzaoc_popup.
  lv_cancel = cl_ci_query_attributes=>generic(
                        p_name       = myname
                        p_title      = 'Options'
                        p_attributes = lt_attributes
                        p_display    = p_display ).         "#EC NOTEXT
  IF lv_cancel = abap_true.
    RETURN.
  ENDIF.
  IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
    attributes_ok = abap_true.
  ELSE.
    attributes_ok = abap_false.
  ENDIF.
END-OF-DEFINITION.
