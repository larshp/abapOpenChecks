*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: BEGIN OF ty_mapping_slin_sci_code,
         slin_code TYPE slin_desc_key,
         sci_code  TYPE sci_errc,
       END OF ty_mapping_slin_sci_code,
       tty_mapping_slin_sci_code TYPE SORTED TABLE OF ty_mapping_slin_sci_code WITH UNIQUE KEY slin_code.
