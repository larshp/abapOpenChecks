abapOpenChecks
==============

- downwards compatability, eg. NEW
- magic/intelligent naming standards
- DATA defintions in top of FORM/method (ordering vs field symbols)
- check for db statements
- line length
- check for use of pretty printer
- functional writing syle for calling methods, CALL METHOD for dynamic only
- RECEIVEING
- use of sy-ucomm, unless fcode in selection screen
- use icon_* constants
- MESSAGEs should be defined in SE91
- Last statement is CLEAR local variable in method/form
- Last statement is RETURN in method/form
- Constants only used once
- Same constants defined multiple places in program
- type C without LENGTH, no implicit typing, always specify "TYPE c"
- 7 bit chars(latin) only in source
- quantity + uom / amount + currency in same structure
- spaces that are tabs, eg when copying signature
- empty IF statement/branch
- comment spell checking
- late read of sy-tabix in LOOP
- always specify JOIN type in SELECT(INNER, OUTER, LEFT)
- always specify SORT order(ASCENDING, DESCENDING)
- CHECK used outside of loops
- IF can easily be changed to CASE
- indexes active and exists in db for Z and Y tables
- REFRESH is obsolete
- pseudo comments vs pragmas, ABAP\_SLIN\_PRAGMAS
- identical code blocks
- OpenSQL LIKE wildcards * vs %
- max one statement per line
- all branches of IF/CASE ends with same code
- clear directly after definition of local variable
- no DATA definitions in MODULE
- comments on single lines starting with " that can be replaced with *
- CASE without OTHERS
- 'X' or '' that can be replaced with abap\_true or abap\_false
- IS INITIAL or NOT IS INITIAL that can be replaced with abap\_true or abap\_false


Common static class to ensure good performance
One class per check, grouping similar

Design goals
try and see if it works out
performance not high priority
unit test'able
only english


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

  IF foo = bar
AND moo = boo.
```


### CHECK_01 - IF in IF
```
IF condition1.
  IF condition2.
    ...
  ENDIF.
ENDIF.
```
Can be reducded to
```
IF ( condition1 ) AND ( condition2 ).
  ...
ENDIF.
```

### CHECK_02 - EXIT outside of loop
Only use EXIT inside loops, use RETURN to end current processing block

### CHECK_03 - TRY without CATCH
Remove TRY block, or implement CATCH case
