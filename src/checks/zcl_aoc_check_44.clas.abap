CLASS zcl_aoc_check_44 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_44 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    TYPES: BEGIN OF ty_seosubcodf,
             clsname    TYPE seosubcodf-clsname,
             cmpname    TYPE seosubcodf-cmpname,
             pardecltyp TYPE seosubcodf-pardecltyp,
             sconame    TYPE seosubcodf-sconame,
             type       TYPE seosubcodf-type,
           END OF ty_seosubcodf.

    DATA: lt_seosubcodf TYPE TABLE OF ty_seosubcodf,
          lv_cmptype    TYPE seocompo-cmptype,
          ls_mtdkey     TYPE seocpdkey,
          lv_include    TYPE sobj_name.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_seosubcodf.


    IF object_type <> 'CLAS' AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

    SELECT * FROM seosubcodf
      INTO CORRESPONDING FIELDS OF TABLE lt_seosubcodf
      WHERE clsname = object_name
      AND ( pardecltyp = '1'
      OR pardecltyp = '2'
      OR pardecltyp = '3' )
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    LOOP AT lt_seosubcodf ASSIGNING <ls_data> WHERE pardecltyp = '1'.
      LOOP AT lt_seosubcodf TRANSPORTING NO FIELDS
          WHERE clsname = <ls_data>-clsname
          AND cmpname = <ls_data>-cmpname
          AND sconame <> <ls_data>-sconame.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

* generic types cannot be specified as returning
* todo, add more here
      <ls_data>-type = condense( <ls_data>-type ).
      IF <ls_data>-type = 'ANY'
          OR <ls_data>-type = 'DATA'
          OR <ls_data>-type = 'ANY TABLE'
          OR <ls_data>-type = 'INDEX TABLE'
          OR <ls_data>-type = 'STANDARD TABLE'.
        CONTINUE.
      ENDIF.

      SELECT SINGLE cmptype FROM seocompo INTO lv_cmptype
        WHERE clsname = <ls_data>-clsname
        AND cmpname = <ls_data>-cmpname.
      IF sy-subrc = 0 AND lv_cmptype = '2'.
* skip class events, they are always exporting
        CONTINUE.
      ENDIF.

      CLEAR lv_include.
      IF object_type = 'CLAS'.
        ls_mtdkey-clsname = <ls_data>-clsname.
        ls_mtdkey-cpdname = <ls_data>-cmpname.

        cl_oo_classname_service=>get_method_include(
          EXPORTING
            mtdkey              = ls_mtdkey
          RECEIVING
            result              = lv_include
          EXCEPTIONS
            class_not_existing  = 1
            method_not_existing = 2
            OTHERS              = 3 ).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      inform( p_sub_obj_name = lv_include
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = <ls_data>-cmpname ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '044'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    insert_scimessage(
      iv_code = '001'
      iv_text = 'EXPORTING can be changed to RETURNING, method &1'(m01) ).

  ENDMETHOD.
ENDCLASS.
