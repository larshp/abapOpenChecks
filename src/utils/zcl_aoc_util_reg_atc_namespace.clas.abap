CLASS zcl_aoc_util_reg_atc_namespace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: ty_range_incl TYPE RANGE OF progname.

    INTERFACE if_satc_namespace_access LOAD.
    CLASS-METHODS get_namespaces
      RETURNING
        VALUE(rt_reg_namespaces) TYPE if_satc_namespace_access=>ty_namespaces.
    CLASS-METHODS get_r_fugr_uxx_from_namespaces
      IMPORTING
        !it_reg_namespaces   TYPE if_satc_namespace_access=>ty_namespaces
      RETURNING
        VALUE(rt_r_includes) TYPE ty_range_incl.
  PROTECTED SECTION.

    INTERFACE if_satc_namespace_access LOAD.
    CLASS-DATA gt_reg_namespaces TYPE if_satc_namespace_access=>ty_namespaces.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_util_reg_atc_namespace IMPLEMENTATION.

  METHOD get_namespaces.

    DATA: lr_namespace_access            TYPE REF TO if_satc_namespace_access,
          lt_valid_namespaces            TYPE if_satc_namespace_access=>ty_namespaces,
          lt_protected_namespaces        TYPE if_satc_namespace_access=>ty_namespace_names,
          lt_valid_registered_namespaces TYPE if_satc_namespace_access=>ty_namespace_names.

    FIELD-SYMBOLS: <ls_valid_namespace> LIKE LINE OF lt_valid_namespaces.


    IF gt_reg_namespaces IS INITIAL.

      lr_namespace_access = cl_satc_namespace_access=>get_namespace_access( ).
      lt_valid_namespaces = lr_namespace_access->get_valid_namespaces( ).

      IF lines( lt_valid_namespaces ) = 0.
        RETURN.
      ENDIF.

      lt_protected_namespaces = lr_namespace_access->get_protected_namespaces( ).

      IF lt_protected_namespaces IS INITIAL.
        CLEAR rt_reg_namespaces.
        RETURN.
      ENDIF.

      lt_valid_registered_namespaces = lr_namespace_access->get_valid_registrd_namespaces( lt_valid_namespaces ).

*     Build result from valid unprotected namespaces
      LOOP AT lt_valid_namespaces ASSIGNING <ls_valid_namespace>.
        READ TABLE lt_protected_namespaces TRANSPORTING NO FIELDS
          WITH KEY table_line = <ls_valid_namespace>-namespace.

        IF sy-subrc <> 0.
          APPEND <ls_valid_namespace> TO gt_reg_namespaces.
        ENDIF.
      ENDLOOP.

    ENDIF.

    rt_reg_namespaces = gt_reg_namespaces.

  ENDMETHOD.

  METHOD get_r_fugr_uxx_from_namespaces.

    DATA: ls_r_includes LIKE LINE OF rt_r_includes.

    FIELD-SYMBOLS: <ls_reg_namespace> LIKE LINE OF it_reg_namespaces.


    LOOP AT it_reg_namespaces ASSIGNING <ls_reg_namespace>.
      CLEAR: ls_r_includes.
      ls_r_includes-sign   = 'I'.
      ls_r_includes-option = 'CP'.
      ls_r_includes-low    = |{ <ls_reg_namespace>-namespace }L*UXX|.
      APPEND ls_r_includes TO rt_r_includes.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.