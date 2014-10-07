*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: syntax_tt TYPE STANDARD TABLE OF ssyntaxstructure.

TYPES: BEGIN OF st_return,
         match TYPE abap_bool,
         index TYPE i,
       END OF st_return.