CLASS zcl_aoc_check_69 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_naming TYPE zaoc_naming .

    METHODS set_defaults .
ENDCLASS.



CLASS ZCL_AOC_CHECK_69 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

* todo

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description = 'Naming Conventions'.                     "#EC NOTEXT
    category    = 'ZCL_AOC_CATEGORY'.
    version     = '002'.
    position    = '069'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    set_defaults( ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'todo'.                                    "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    CALL FUNCTION 'Z_AOC_NAMING'
      EXPORTING
        iv_read_only = p_display
      CHANGING
        cs_data      = ms_naming.

    attributes_ok = abap_true.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD set_defaults.

    ms_naming-prefix_elemen = 'V'.
    ms_naming-prefix_generi = 'G'.
    ms_naming-prefix_struct = 'S'.
    ms_naming-prefix_tany   = 'T'.
    ms_naming-prefix_thash  = 'T'.
    ms_naming-prefix_tindex = 'T'.
    ms_naming-prefix_tstand = 'T'.
    ms_naming-prefix_tsort  = 'T'.
    ms_naming-prefix_rdata  = 'R'.
    ms_naming-prefix_rclass = 'O'.
    ms_naming-prefix_rbadi  = 'B'.
    ms_naming-prefix_rexcep = 'X'.
    ms_naming-prefix_rinter = 'I'.

    ms_naming-globals_nspace = 'Z'.
    ms_naming-globals_fugr   = '[:nspace:]'.
    ms_naming-globals_prog   = '[:nspace:]'.
    ms_naming-globals_clas   = '[:nspace:]CL_'.
    ms_naming-globals_clasx  = '[:nspace:]CX_'.
    ms_naming-globals_clast  = '[:nspace:]CL_'.
    ms_naming-globals_clasa  = '[:nspace:]CL_'.
    ms_naming-globals_intf   = '[:nspace:]IF_'.

    ms_naming-locals_data   = 'L[:type:]_'.
    ms_naming-locals_static = 'S[:type:]_'.
    ms_naming-locals_fsymbo = '<L[:type:]_'.
    ms_naming-locals_lconst = 'LC[:type:]_'.
    ms_naming-locals_ltypes = ''.

    ms_naming-proc_fimpor = 'I[:type:]_'.
    ms_naming-proc_fexpor = 'E[:type:]_'.
    ms_naming-proc_fchang = 'C[:type:]_'.
    ms_naming-proc_ftable = 'T[:type:]_'.
    ms_naming-proc_fousin = 'P[:type:]_'.
    ms_naming-proc_fochan = 'C[:type:]_'.
    ms_naming-proc_fotabl = 'T[:type:]_'.
    ms_naming-proc_pgdata = 'G[:type:]_'.
    ms_naming-proc_pgfisy = '<G[:type:]_'.
    ms_naming-proc_pgcons = 'GC_'.
    ms_naming-proc_pgtype = ''.
    ms_naming-proc_pgselo = 'S_'.
    ms_naming-proc_pgpara = 'P_'.

    ms_naming-oo_oodata = 'M[:type:]_'.
    ms_naming-oo_oocdat = 'G[:type:]_'.
    ms_naming-oo_oocons = 'C_'.
    ms_naming-oo_ootype = ''.
    ms_naming-oo_ooimpo = 'I[:type:]_'.
    ms_naming-oo_ooexpo = 'E[:type:]_'.
    ms_naming-oo_oochan = 'C[:type:]_'.
    ms_naming-oo_ooretu = 'R[:type:]_'.
    ms_naming-oo_oolcla = 'LCL_'.
    ms_naming-oo_ooltcl = 'LTCL_'.
    ms_naming-oo_oolint = 'LIF_'.

  ENDMETHOD.
ENDCLASS.
