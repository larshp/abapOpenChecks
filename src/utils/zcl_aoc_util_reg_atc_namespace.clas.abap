class ZCL_AOC_UTIL_REG_ATC_NAMESPACE definition    
  public    
  final    
  create public .    
    
public section.    
    
  types:    
    ty_range_incl    
      TYPE RANGE OF progname .    
    
  interface IF_SATC_NAMESPACE_ACCESS load .    
  class-methods GET_NAMESPACES    
    returning    
      value(RT_REG_NAMESPACES) type IF_SATC_NAMESPACE_ACCESS=>TY_NAMESPACES .    
  class-methods GET_R_FUGR_UXX_FROM_NAMESPACES    
    importing    
      !IT_REG_NAMESPACES type IF_SATC_NAMESPACE_ACCESS=>TY_NAMESPACES    
    returning    
      value(RT_R_INCLUDES) type TY_RANGE_INCL .    
protected section.    
    
  interface IF_SATC_NAMESPACE_ACCESS load .    
  class-data GT_REG_NAMESPACES type IF_SATC_NAMESPACE_ACCESS=>TY_NAMESPACES .    
private section.    
ENDCLASS.    
    
    
    
CLASS ZCL_AOC_UTIL_REG_ATC_NAMESPACE IMPLEMENTATION.    
    
    
  METHOD get_namespaces.    
    
    IF gt_reg_namespaces IS INITIAL.    
    
      DATA(namespace_access) = cl_satc_namespace_access=>get_namespace_access( ).    
      DATA(valid_namespaces) = namespace_access->get_valid_namespaces( ).    
    
      IF lines( valid_namespaces ) = 0.    
        RETURN.    
      ENDIF.    
    
      DATA(protected_namespaces) = namespace_access->get_protected_namespaces( ).    
    
      IF protected_namespaces IS INITIAL.    
        CLEAR rt_reg_namespaces.    
        RETURN.    
      ENDIF.    
    
      DATA(valid_registered_namespaces) = namespace_access->get_valid_registrd_namespaces( valid_namespaces ).    
    
*     Build result from valid unprotected namespaces    
      LOOP AT valid_namespaces ASSIGNING FIELD-SYMBOL(<valid_namespace>).    
        IF NOT line_exists( protected_namespaces[ table_line = <valid_namespace>-namespace ] ).    
          APPEND <valid_namespace> TO gt_reg_namespaces.    
        ENDIF.    
      ENDLOOP.    
    
    ENDIF.    
    
    rt_reg_namespaces = gt_reg_namespaces.    
    
  ENDMETHOD.    
    
    
 METHOD get_r_fugr_uxx_from_namespaces.    
    
   LOOP AT it_reg_namespaces ASSIGNING FIELD-SYMBOL(<ls_reg_namespace>).    
      APPEND VALUE #( sign = 'I' option = 'CP' low = |{ <ls_reg_namespace>-namespace }L*UXX| ) TO rt_r_includes.    
    ENDLOOP.    
    
  ENDMETHOD.    
ENDCLASS. 
