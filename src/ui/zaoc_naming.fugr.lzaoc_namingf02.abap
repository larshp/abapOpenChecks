*----------------------------------------------------------------------*
***INCLUDE LZAOC_NAMINGF02.
*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  lcl_screen2000=>at_selection_screen( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_screen2000=>at_output( ).

LOAD-OF-PROGRAM.
  button_1 = 'Prefixes'.
  button_2 = 'Globals'.
  button_3 = 'Locals'.
  button_4 = 'Procedural'.
  button_5 = 'Object Oriented'.
