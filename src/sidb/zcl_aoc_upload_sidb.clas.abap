CLASS zcl_aoc_upload_sidb DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS upload
      IMPORTING
        !iv_xstr TYPE xstring .
  PROTECTED SECTION.

    TYPES:
      ty_key TYPE c LENGTH 32 .
    TYPES:
      BEGIN OF ty_piecelist,
        piecelist_id TYPE ty_key,
        pgmid        TYPE tadir-pgmid,
        object_type  TYPE tadir-object,
        object_name  TYPE tadir-obj_name,
      END OF ty_piecelist .
    TYPES:
      ty_piecelist_tt TYPE STANDARD TABLE OF ty_piecelist WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_sitem_plist,
        id           TYPE ty_key,
        piecelist_id TYPE ty_key,
      END OF ty_sitem_plist .
    TYPES:
      ty_sitem_plist_tt TYPE STANDARD TABLE OF ty_sitem_plist WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_sitem_stats,
        id    TYPE ty_key,
        state TYPE c LENGTH 1,
      END OF ty_sitem_stats .
    TYPES:
      ty_sitem_stats_tt TYPE STANDARD TABLE OF ty_sitem_stats WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_sitem,
        id   TYPE ty_key,
        note TYPE c LENGTH 10,
      END OF ty_sitem .
    TYPES:
      ty_sitem_tt TYPE STANDARD TABLE OF ty_sitem WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_simplification,
        sitem       TYPE ty_sitem_tt,
        sitem_plist TYPE ty_sitem_plist_tt,
        sitem_stats TYPE ty_sitem_stats_tt,
        piecelist   TYPE ty_piecelist_tt,
      END OF ty_simplification .
    TYPES:
      BEGIN OF ty_file,
        name TYPE string,
        data TYPE string,
      END OF ty_file .
    TYPES:
      ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY .

    CLASS-DATA gs_simplification TYPE ty_simplification .

    CLASS-METHODS parse
      IMPORTING
        !it_files TYPE ty_files_tt .
    CLASS-METHODS parse_piecelist
      IMPORTING
        !iv_xml TYPE string .
    CLASS-METHODS parse_sitem
      IMPORTING
        !iv_xml TYPE string .
    CLASS-METHODS parse_sitem_plist
      IMPORTING
        !iv_xml TYPE string .
    CLASS-METHODS parse_sitem_stats
      IMPORTING
        !iv_xml TYPE string .
    CLASS-METHODS unzip
      IMPORTING
        !iv_xstr        TYPE xstring
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt .
    CLASS-METHODS write_to_db .
    CLASS-METHODS xml_parse
      IMPORTING
        !iv_xml       TYPE string
      RETURNING
        VALUE(ri_doc) TYPE REF TO if_ixml_document .
    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xstring
      RETURNING
        VALUE(rv_string) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_upload_sidb IMPLEMENTATION.


  METHOD parse.

    DATA: ls_file LIKE LINE OF it_files.

    LOOP AT it_files INTO ls_file.

      IF ls_file-name = 'header_data.xml'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name = 'SIGNATURE.SMF'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name CP 'simplification/piecelist_0*'.
        parse_piecelist( ls_file-data ).
      ELSEIF ls_file-name CP 'simplification/plist_head_0*'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name CP 'simplification/plist_param_0*'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name CP 'simplification/plst_subobj_0*'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name CP 'simplification/sitem_0*'.
        parse_sitem( ls_file-data ).
      ELSEIF ls_file-name CP 'simplification/sitem_areas_0*'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name CP 'simplification/sitem_enpoi_0*'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name CP 'simplification/sitem_packs_0*'.
* do nothing
        CONTINUE.
      ELSEIF ls_file-name CP 'simplification/sitem_plist_0*'.
        parse_sitem_plist( ls_file-data ).
      ELSEIF ls_file-name CP 'simplification/sitem_stats_0*'.
        parse_sitem_stats( ls_file-data ).
      ELSEIF ls_file-name CP 'releases/rel_stack_0*'.
* do nothing
        CONTINUE.
      ELSE.
* Fail - unknown file -> may even be unexpected ZIP
        ASSERT 0 = 1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD parse_piecelist.

    DATA: lv_name      TYPE string,
          lv_value     TYPE string,
          ls_piecelist LIKE LINE OF gs_simplification-piecelist,
          li_node      TYPE REF TO if_ixml_node,
          li_field     TYPE REF TO if_ixml_node,
          li_iterator  TYPE REF TO if_ixml_node_iterator.


    li_iterator = xml_parse( iv_xml )->find_from_name_ns( 'PIECELIST' )->get_children( )->create_iterator( ).

    li_node = li_iterator->get_next( ).
    WHILE li_node IS NOT INITIAL.

      CLEAR ls_piecelist.

      DO li_node->get_children( )->get_length( ) TIMES.
        li_field = li_node->get_children( )->get_item( sy-index - 1 ).
        lv_name = li_field->get_name( ).
        lv_value = li_field->get_value( ).

        CASE lv_name.
          WHEN 'PIECELIST_ID'.
            ls_piecelist-piecelist_id = lv_value.
          WHEN 'PGMID'.
            ls_piecelist-pgmid = lv_value.
          WHEN 'OBJECT_TYPE'.
            ls_piecelist-object_type = lv_value.
          WHEN 'OBJECT_NAME'.
            ls_piecelist-object_name = lv_value.
        ENDCASE.
      ENDDO.

      APPEND ls_piecelist TO gs_simplification-piecelist.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_sitem.

    DATA: lv_name     TYPE string,
          lv_value    TYPE string,
          ls_sitem    LIKE LINE OF gs_simplification-sitem,
          li_node     TYPE REF TO if_ixml_node,
          li_field    TYPE REF TO if_ixml_node,
          li_iterator TYPE REF TO if_ixml_node_iterator.


    li_iterator = xml_parse( iv_xml )->find_from_name_ns( 'SITEM' )->get_children( )->create_iterator( ).

    li_node = li_iterator->get_next( ).
    WHILE li_node IS NOT INITIAL.

      CLEAR ls_sitem.

      DO li_node->get_children( )->get_length( ) TIMES.
        li_field = li_node->get_children( )->get_item( sy-index - 1 ).
        lv_name = li_field->get_name( ).
        lv_value = li_field->get_value( ).

        CASE lv_name.
          WHEN 'ID'.
            ls_sitem-id = lv_value.
          WHEN 'NOTE'.
            ls_sitem-note = lv_value.
        ENDCASE.
      ENDDO.

      APPEND ls_sitem TO gs_simplification-sitem.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_sitem_plist.

    DATA: lv_name        TYPE string,
          lv_value       TYPE string,
          ls_sitem_plist LIKE LINE OF gs_simplification-sitem_plist,
          li_node        TYPE REF TO if_ixml_node,
          li_field       TYPE REF TO if_ixml_node,
          li_iterator    TYPE REF TO if_ixml_node_iterator.


    li_iterator = xml_parse( iv_xml )->find_from_name_ns( 'SITEM_PLIST' )->get_children( )->create_iterator( ).

    li_node = li_iterator->get_next( ).
    WHILE li_node IS NOT INITIAL.

      CLEAR ls_sitem_plist.

      DO li_node->get_children( )->get_length( ) TIMES.
        li_field = li_node->get_children( )->get_item( sy-index - 1 ).
        lv_name = li_field->get_name( ).
        lv_value = li_field->get_value( ).

        CASE lv_name.
          WHEN 'PIECELIST_ID'.
            ls_sitem_plist-piecelist_id = lv_value.
          WHEN 'ID'.
            ls_sitem_plist-id = lv_value.
        ENDCASE.
      ENDDO.

      APPEND ls_sitem_plist TO gs_simplification-sitem_plist.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_sitem_stats.

    DATA: lv_name        TYPE string,
          lv_value       TYPE string,
          ls_sitem_stats LIKE LINE OF gs_simplification-sitem_stats,
          li_node        TYPE REF TO if_ixml_node,
          li_field       TYPE REF TO if_ixml_node,
          li_iterator    TYPE REF TO if_ixml_node_iterator.


    li_iterator = xml_parse( iv_xml )->find_from_name_ns( 'SITEM_STATS' )->get_children( )->create_iterator( ).

    li_node = li_iterator->get_next( ).
    WHILE li_node IS NOT INITIAL.

      CLEAR ls_sitem_stats.

      DO li_node->get_children( )->get_length( ) TIMES.
        li_field = li_node->get_children( )->get_item( sy-index - 1 ).
        lv_name = li_field->get_name( ).
        lv_value = li_field->get_value( ).

        CASE lv_name.
          WHEN 'STATE'.
            ls_sitem_stats-state = lv_value.
          WHEN 'ID'.
            ls_sitem_stats-id = lv_value.
        ENDCASE.
      ENDDO.

      APPEND ls_sitem_stats TO gs_simplification-sitem_stats.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD unzip.

    DATA: lo_zip  TYPE REF TO cl_abap_zip,
          ls_file LIKE LINE OF rt_files,
          lv_data TYPE xstring.

    FIELD-SYMBOLS: <ls_zipfile> LIKE LINE OF lo_zip->files.


    CREATE OBJECT lo_zip.
    lo_zip->load( EXPORTING
                    zip             = iv_xstr
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2 ).
    ASSERT sy-subrc = 0.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      ASSERT sy-subrc = 0.

      CLEAR ls_file.
      ls_file-name = <ls_zipfile>-name.
      ls_file-data = xstring_to_string_utf8( lv_data ).
      APPEND ls_file TO rt_files.

    ENDLOOP.

  ENDMETHOD.


  METHOD upload.

    DATA: lt_files TYPE ty_files_tt.


    lt_files = unzip( iv_xstr ).

    parse( lt_files ).

    write_to_db( ).

  ENDMETHOD.


  METHOD write_to_db.

    DATA: ls_sidb      TYPE zaoc_sidb,
          ls_sitem     LIKE LINE OF gs_simplification-sitem,
          ls_piecelist LIKE LINE OF gs_simplification-piecelist,
          ls_plist     LIKE LINE OF gs_simplification-sitem_plist,
          ls_stats     LIKE LINE OF gs_simplification-sitem_stats.

* okay, yeah, this is slow, but it will not be run very often

    DELETE FROM zaoc_sidb.                "#EC CI_NOWHERE "#EC CI_SUBRC

    LOOP AT gs_simplification-sitem INTO ls_sitem.

      READ TABLE gs_simplification-sitem_stats INTO ls_stats WITH KEY id = ls_sitem-id.
* see domain SYCM_SITEM_STATE
      IF sy-subrc <> 0 OR ls_stats-state NA 'AX'.
        CONTINUE.
      ENDIF.

      LOOP AT gs_simplification-sitem_plist INTO ls_plist WHERE id = ls_sitem-id.

        LOOP AT gs_simplification-piecelist INTO ls_piecelist WHERE piecelist_id = ls_plist-piecelist_id.

          CLEAR ls_sidb.
          ls_sidb-pgmid = ls_piecelist-pgmid.
          ls_sidb-object_type = ls_piecelist-object_type.
          ls_sidb-object_name = ls_piecelist-object_name.
          ls_sidb-state = ls_stats-state.
          ls_sidb-note = ls_sitem-note.
          INSERT zaoc_sidb FROM ls_sidb.                  "#EC CI_SUBRC

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD xml_parse.

    DATA: li_ixml                TYPE REF TO if_ixml,
          li_ixml_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_ixml_istream        TYPE REF TO if_ixml_istream,
          li_ixml_parser         TYPE REF TO if_ixml_parser.


    li_ixml = cl_ixml=>create( ).

    ri_doc                 = li_ixml->create_document( ).
    li_ixml_stream_factory = li_ixml->create_stream_factory( ).
    li_ixml_istream        = li_ixml_stream_factory->create_istream_string( iv_xml ).

    li_ixml_parser = li_ixml->create_parser(
      document       = ri_doc
      istream        = li_ixml_istream
      stream_factory = li_ixml_stream_factory ).

    IF li_ixml_parser->parse( ) <> 0.
      ASSERT li_ixml_parser->num_errors( ) = 0.
    ENDIF.

    li_ixml_istream->close( ).

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
