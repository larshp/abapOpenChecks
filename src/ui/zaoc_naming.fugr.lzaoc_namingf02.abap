*----------------------------------------------------------------------*
***INCLUDE LZAOC_NAMINGF02.
*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  lcl_screen2000=>handle_command( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_screen2000=>at_output( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  lcl_screen2000=>handle_command( ).

LOAD-OF-PROGRAM.
  button_1 = 'Prefixes'(002).
  button_2 = 'Globals'(003).
  button_3 = 'Locals'(004).
  button_4 = 'Procedural'(005).
  button_5 = 'Object Oriented'(006).
  button_6 = 'Other Settings'(007).
