"! <p class="shorttext synchronized" lang="en">Scan object specially for class definition</p>
CLASS zcl_aoc_class_def_scan DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_source_code,
        line_total    TYPE i,
        source_code   TYPE string,
        level_total   TYPE i,
        line_section  TYPE i,
        level_section TYPE i,
      END OF ts_source_code,
      tt_source_code TYPE STANDARD TABLE OF ts_source_code WITH EMPTY KEY.

    DATA tokens TYPE stokesx_tab READ-ONLY .
    DATA statements TYPE sstmnt_tab READ-ONLY .
    DATA levels TYPE slevel_tab READ-ONLY .
    DATA structures TYPE sstruc_tab READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Get Instance by Ref Scan object</p>
    "!
    "! @parameter io_ref | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_scan_by_ref
      IMPORTING
        !io_ref       TYPE REF TO cl_ci_scan
      RETURNING
        VALUE(result) TYPE REF TO zcl_aoc_class_def_scan.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter it_tokens | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_statements | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_levels | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_structures | <p class="shorttext synchronized" lang="en"></p>
    METHODS constructor
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab
        !it_structures TYPE sstruc_tab.

    "! <p class="shorttext synchronized" lang="en">Get Class Definition Info for Search Term</p>
    "!
    "! @parameter iv_search_term | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_code_line_def
      IMPORTING
        !iv_search_term TYPE string
      RETURNING
        VALUE(result)   TYPE ts_source_code.

    "! <p class="shorttext synchronized" lang="en">Get Include Name by Level</p>
    "!
    "! @parameter iv_level | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_level_name
      IMPORTING
        !iv_level     TYPE i
      RETURNING
        VALUE(result) TYPE level_name.

  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF section_type,
        start_def TYPE i VALUE 2,  " CLASS * DEFINITION
        public    TYPE i VALUE 3,  " PUBLIC SECTION
        protected TYPE i VALUE 4,  " PROTECTED SECTION
        private   TYPE i VALUE 5,  " PRIVATE SECTION
      END OF section_type.

    CLASS-DATA o_instance TYPE REF TO zcl_aoc_class_def_scan.

    DATA mv_oo_name TYPE string.
    DATA mt_sc_definition TYPE tt_source_code.

    "! <p class="shorttext synchronized" lang="en">Determine the total line number</p>
    "!
    METHODS determine_line_total.

    "! <p class="shorttext synchronized" lang="en">Set the OO Object Name</p>
    "!
    METHODS set_oo_object_name.

    "! <p class="shorttext synchronized" lang="en">Set internal table with Source Code</p>
    "!
    METHODS set_source_code.

    "! <p class="shorttext synchronized" lang="en">Set internal tables with Source Code for sections</p>
    "!
    METHODS set_source_code_sections.


  PRIVATE SECTION.


    "! <p class="shorttext synchronized" lang="en">Get last line number of public section</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS lines_public_sec
      RETURNING VALUE(result) TYPE i.
    "! <p class="shorttext synchronized" lang="en">Get last line number of protected section</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS lines_protected_sec
      RETURNING VALUE(result) TYPE i.
    "! <p class="shorttext synchronized" lang="en">Get last line number of private section</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS lines_private_sec
      RETURNING VALUE(result) TYPE i.
    "! <p class="shorttext synchronized" lang="en">Get last line number for specific section</p>
    "!
    "! @parameter iv_section_type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS lines_for_section
      IMPORTING iv_section_type TYPE i
      RETURNING VALUE(result)   TYPE i.

ENDCLASS.



CLASS zcl_aoc_class_def_scan IMPLEMENTATION.

  METHOD get_scan_by_ref.

    result = COND #( WHEN o_instance IS NOT INITIAL THEN o_instance ELSE NEW #( it_tokens     = io_ref->tokens
                                                                                it_statements = io_ref->statements
                                                                                it_levels     = io_ref->levels
                                                                                it_structures = io_ref->structures ) ).

  ENDMETHOD.

  METHOD constructor.

    me->tokens     = it_tokens.
    me->statements = it_statements.
    me->levels     = it_levels.
    me->structures = it_structures.

    set_oo_object_name( ).

    IF me->mv_oo_name IS NOT INITIAL.
      set_source_code(  ).
      set_source_code_sections( ).
    ENDIF.

    o_instance = me.

  ENDMETHOD.

  METHOD get_code_line_def.

    DATA search_term TYPE string.
    FIELD-SYMBOLS <ls_code_line> TYPE ts_source_code.

    search_term = |*{ iv_search_term CASE = (cl_abap_format=>c_upper) }*|.

    LOOP AT mt_sc_definition ASSIGNING <ls_code_line> WHERE source_code CP search_term.
      result = <ls_code_line>.
      RETURN.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_level_name.

    result = VALUE #( me->levels[ iv_level ]-name OPTIONAL ).

  ENDMETHOD.

  METHOD determine_line_total.

    DATA(lines_in_public) = lines_public_sec( ).
    DATA(lines_in_protected) = lines_protected_sec( ).
    DATA(lines_in_private) = lines_private_sec( ).

    LOOP AT mt_sc_definition ASSIGNING FIELD-SYMBOL(<ls_code_line>).
      <ls_code_line>-line_total = SWITCH #( <ls_code_line>-level_section
                                            WHEN section_type-public    THEN <ls_code_line>-line_section
                                            WHEN section_type-protected THEN <ls_code_line>-line_section + lines_in_public
                                            WHEN section_type-private   THEN <ls_code_line>-line_section + lines_in_public + lines_in_protected ).
    ENDLOOP.

    DELETE mt_sc_definition WHERE line_total IS INITIAL.

  ENDMETHOD.

  METHOD set_oo_object_name.

    DATA identifier TYPE string.
    DATA line TYPE i.

    CASE me->tokens[ 1 ]-str.
      WHEN 'CLASS-POOL'.
        identifier = |CLASS|.
      WHEN 'INTERFACE-POOL'.
        identifier = |INTERFACE|.
    ENDCASE.

    CHECK identifier IS NOT INITIAL.

    LOOP AT me->tokens ASSIGNING FIELD-SYMBOL(<token>).
      IF <token>-str = identifier.
        line = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

    me->mv_oo_name = VALUE #( me->tokens[ line + 1 ]-str OPTIONAL ).

  ENDMETHOD.

  METHOD set_source_code.

    DATA code_line TYPE ts_source_code.

    LOOP AT me->tokens ASSIGNING FIELD-SYMBOL(<ls_token>).

      IF <ls_token>-row = 1 AND code_line-line_section = 0.
        code_line-level_section = 1.
        code_line-source_code   = <ls_token>-str.
        code_line-line_section  = <ls_token>-row.
      ELSEIF <ls_token>-row = 1 AND code_line-line_section <> 1.
        APPEND code_line TO mt_sc_definition.
        code_line-level_section = code_line-level_section + 1.
        code_line-source_code   = <ls_token>-str.
        code_line-line_section  = <ls_token>-row.
      ELSEIF <ls_token>-row  = code_line-line_section.
        code_line-source_code = |{ code_line-source_code } { <ls_token>-str }|.
      ELSEIF <ls_token>-row  > code_line-line_section.
        APPEND code_line TO mt_sc_definition.
        code_line-source_code = <ls_token>-str.
        code_line-line_section  = <ls_token>-row.
      ELSEIF <ls_token>-row  < code_line-line_section.
        " Do nothing at the moment
      ENDIF.
    ENDLOOP.

    APPEND code_line TO mt_sc_definition.

  ENDMETHOD.

  METHOD set_source_code_sections.

    DATA current_section TYPE i.
    DATA current_section_type TYPE i.
    DATA level_total TYPE i.

    LOOP AT mt_sc_definition ASSIGNING FIELD-SYMBOL(<ls_source_code>).

      IF <ls_source_code>-level_section > current_section.
        CLEAR current_section_type.
        current_section = <ls_source_code>-level_section.
      ENDIF.

      current_section_type = COND i( WHEN <ls_source_code>-source_code CP |CLASS { me->mv_oo_name } DEFINITION*|      THEN section_type-start_def
                                     WHEN <ls_source_code>-source_code CP 'PUBLIC SECTION'                            THEN section_type-public
                                     WHEN <ls_source_code>-source_code CP 'PROTECTED SECTION'                         THEN section_type-protected
                                     WHEN <ls_source_code>-source_code CP 'PRIVATE SECTION'                           THEN section_type-private
                                     ELSE current_section_type ).

      IF current_section_type IS NOT INITIAL.
        <ls_source_code>-level_total    = section_type-public.
      ENDIF.

    ENDLOOP.

    DELETE mt_sc_definition WHERE level_section > section_type-private.

    determine_line_total(  ).

  ENDMETHOD.

  METHOD lines_private_sec.

    result = lines_for_section( section_type-private ).

  ENDMETHOD.

  METHOD lines_protected_sec.

    result = lines_for_section( section_type-protected ).

  ENDMETHOD.

  METHOD lines_public_sec.

    result = lines_for_section( section_type-public ).

  ENDMETHOD.

  METHOD lines_for_section.

    DATA(temp_table) = VALUE tt_source_code( FOR line IN mt_sc_definition WHERE ( level_section = iv_section_type ) ( line ) ).

    SORT temp_table BY line_section DESCENDING.
    result = VALUE #( temp_table[ 1 ]-line_section OPTIONAL ).

  ENDMETHOD.

ENDCLASS.
