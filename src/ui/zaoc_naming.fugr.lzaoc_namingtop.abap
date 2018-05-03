FUNCTION-POOL zaoc_naming.                  "MESSAGE-ID ..

* INCLUDE LZAOC_NAMINGD...                   " Local class definition

SELECTION-SCREEN BEGIN OF SCREEN 2000 TITLE TEXT-001 AS WINDOW.
SELECTION-SCREEN:
  BEGIN OF TABBED BLOCK main_tab FOR 28 LINES,
    TAB (30) button_1 USER-COMMAND to_3000 DEFAULT SCREEN 3000,
    TAB (30) button_2 USER-COMMAND to_4000 DEFAULT SCREEN 4000,
    TAB (30) button_3 USER-COMMAND to_5000 DEFAULT SCREEN 5000,
    TAB (30) button_4 USER-COMMAND to_6000 DEFAULT SCREEN 6000,
    TAB (30) button_5 USER-COMMAND to_7000 DEFAULT SCREEN 7000,
    TAB (30) button_6 USER-COMMAND to_8000 DEFAULT SCREEN 8000,
  END OF BLOCK main_tab.
SELECTION-SCREEN END OF SCREEN 2000.

* Prefixes
SELECTION-SCREEN BEGIN OF SCREEN 3000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK prefixes WITH FRAME TITLE TEXT-pre.
PARAMETERS:
  p_elemen TYPE text40 MODIF ID ro,
  p_generi TYPE text40 MODIF ID ro,
  p_struct TYPE text40 MODIF ID ro,
  p_tany   TYPE text40 MODIF ID ro,
  p_thash  TYPE text40 MODIF ID ro,
  p_tindex TYPE text40 MODIF ID ro,
  p_tstand TYPE text40 MODIF ID ro,
  p_tsort  TYPE text40 MODIF ID ro,
  p_rdata  TYPE text40 MODIF ID ro,
  p_rclass TYPE text40 MODIF ID ro,
  p_rbadi  TYPE text40 MODIF ID ro,
  p_rexcep TYPE text40 MODIF ID ro,
  p_rinter TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK prefixes.
SELECTION-SCREEN END OF SCREEN 3000.

* Globals
SELECTION-SCREEN BEGIN OF SCREEN 4000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK globals WITH FRAME TITLE TEXT-glo.
PARAMETERS:
  p_nspace TYPE text40 MODIF ID ro,
  p_fugr   TYPE text40 MODIF ID ro,
  p_prog   TYPE text40 MODIF ID ro,
  p_clas   TYPE text40 MODIF ID ro,
  p_clasx  TYPE text40 MODIF ID ro,
  p_clast  TYPE text40 MODIF ID ro,
  p_clasa  TYPE text40 MODIF ID ro,
  p_intf   TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK globals.
SELECTION-SCREEN END OF SCREEN 4000.

* Locals
SELECTION-SCREEN BEGIN OF SCREEN 5000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK locals WITH FRAME TITLE TEXT-loc.
PARAMETERS:
  p_data   TYPE text40 MODIF ID ro,
  p_static TYPE text40 MODIF ID ro,
  p_fsymbo TYPE text40 MODIF ID ro,
  p_lconst TYPE text40 MODIF ID ro,
  p_ltypes TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK locals.
SELECTION-SCREEN END OF SCREEN 5000.

* Procedural
SELECTION-SCREEN BEGIN OF SCREEN 6000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK fugr WITH FRAME TITLE TEXT-fug.
PARAMETERS:
  p_fimpor TYPE text40 MODIF ID ro,
  p_fexpor TYPE text40 MODIF ID ro,
  p_fchang TYPE text40 MODIF ID ro,
  p_ftable TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK fugr.
SELECTION-SCREEN BEGIN OF BLOCK form WITH FRAME TITLE TEXT-for.
PARAMETERS:
  p_fousin TYPE text40 MODIF ID ro,
  p_fochan TYPE text40 MODIF ID ro,
  p_fotabl TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK form.
SELECTION-SCREEN BEGIN OF BLOCK pglob WITH FRAME TITLE TEXT-pgl.
PARAMETERS:
  p_pgdata TYPE text40 MODIF ID ro,
  p_pgfisy TYPE text40 MODIF ID ro,
  p_pgcons TYPE text40 MODIF ID ro,
  p_pgtype TYPE text40 MODIF ID ro,
  p_pgselo TYPE text40 MODIF ID ro,
  p_pgpara TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK pglob.
SELECTION-SCREEN END OF SCREEN 6000.

* Object Oriented
SELECTION-SCREEN BEGIN OF SCREEN 7000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK ood WITH FRAME TITLE TEXT-ood.
PARAMETERS:
  p_oodata TYPE text40 MODIF ID ro,
  p_oocdat TYPE text40 MODIF ID ro,
  p_oocons TYPE text40 MODIF ID ro,
  p_ootype TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK ood.
SELECTION-SCREEN BEGIN OF BLOCK methods WITH FRAME TITLE TEXT-met.
PARAMETERS:
  p_ooimpo TYPE text40 MODIF ID ro,
  p_ooexpo TYPE text40 MODIF ID ro,
  p_oochan TYPE text40 MODIF ID ro,
  p_ooretu TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK methods.
SELECTION-SCREEN BEGIN OF BLOCK loo WITH FRAME TITLE TEXT-loo.
PARAMETERS:
  p_oolcla TYPE text40 MODIF ID ro,
  p_ooltcl TYPE text40 MODIF ID ro,
  p_oolint TYPE text40 MODIF ID ro.
SELECTION-SCREEN END OF BLOCK loo.
SELECTION-SCREEN END OF SCREEN 7000.

* Other Settings
SELECTION-SCREEN BEGIN OF SCREEN 8000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK other WITH FRAME TITLE TEXT-oth.
PARAMETERS:
  p_errty  TYPE sci_errty MODIF ID ro OBLIGATORY.
SELECTION-SCREEN END OF BLOCK other.
SELECTION-SCREEN BEGIN OF BLOCK gexcep WITH FRAME TITLE TEXT-gex.
PARAMETERS: p_excpar TYPE c AS CHECKBOX MODIF ID ro,
            p_excatt TYPE c AS CHECKBOX MODIF ID ro,
            p_exccon TYPE c AS CHECKBOX MODIF ID ro.
SELECTION-SCREEN END OF BLOCK gexcep.
SELECTION-SCREEN BEGIN OF BLOCK fmsig WITH FRAME TITLE TEXT-fsi.
PARAMETERS: p_cfunc  TYPE c AS CHECKBOX MODIF ID ro,
            p_idocfm TYPE c AS CHECKBOX MODIF ID ro,
            p_bwext  TYPE c AS CHECKBOX MODIF ID ro.
SELECTION-SCREEN END OF BLOCK fmsig.
SELECTION-SCREEN END OF SCREEN 8000.

************************

CLASS lcl_screen2000 DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-DATA:
      gv_cancel TYPE abap_bool.

    CLASS-METHODS:
      initialize
        IMPORTING
          iv_read_only TYPE abap_bool
          is_data      TYPE zaoc_naming,
      get_data
        RETURNING
          VALUE(rs_data) TYPE zaoc_naming,
      at_output,
      handle_command.

  PRIVATE SECTION.
    CLASS-DATA:
      gv_read_only TYPE abap_bool.

    CLASS-METHODS:
      modify_screen,
      set_read_only
        IMPORTING
          iv_only TYPE abap_bool,
      set_data
        IMPORTING
          is_data TYPE zaoc_naming,
      read_structure
        RETURNING
          VALUE(rt_data) TYPE dd03ptab,
      set_texts
        IMPORTING
          iv_prefix TYPE clike.

ENDCLASS.
