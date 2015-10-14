TYPE-POOL zzaoc.

DEFINE zzaoc_fill_att.
  get reference of &1 into ls_attribute-ref.
  ls_attribute-text = &2.
  ls_attribute-kind = &3.
  append ls_attribute to lt_attributes.
END-OF-DEFINITION.