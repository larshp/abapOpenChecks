abapOpenChecks
==============

- downwards compatability, eg. NEW
- magic/intelligent naming standards
- IF inside IF
- DATA defintions in top of FORM/method (ordering vs field symbols)
- check for db statements
- line length
- check for use of pretty printer
- functional writing syle for calling methods, CALL METHOD for dynamic only
- use of sy-ucomm
- RECEIVEING
- use icon_* constants
- MESSAGEs should be defined in SE91
- Last statement is CLEAR local variable in method/form
- Constants only used once
- Same constants defined multiple places in program
- type C without LENGTH, no implicit typing, always specify "TYPE c"
- 7 bit chars(latin) only in source
- quantity + uom / amount + currency in same structure
- spaces that are tabs, eg when copying signature
- empty IF statement
- EXIT outside of loop
- comment spell checking
- late read of sy-tabix in LOOP
- always specify JOIN type in SELECT
- always specify SORT order
- CHECK used outside of loops
- IF can be changed to CASE
- indexes active and exists in db for Z and Y tables
- REFRESH is obsolete
- pseudo comments vs pragmas, ABAP\_SLIN\_PRAGMAS
- identical code blocks
- OpenSQL LIKE wildcards * vs %


Clearance program

Inactive objects list


Check indentation, eg. 

```
IF foo = bar
    AND moo = boo.
    MOVE ...
  
IF foo = bar
AND moo = boo.
    MOVE ...
	
IF foo = bar
		    AND moo = boo.
    MOVE ...
```
