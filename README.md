abapOpenChecks
==============

- downwards compatability, eg. NEW
- magic/intelligent naming standards
- DATA defintions in top of FORM/method (ordering vs field symbols)
- check for db statements
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
- space dot, WRITE 'foo' .
- space colon, WRITE : 'foo'.
- no guard for division by zero
- show number of codelines scanned
- only show errors that are part of whats been changed in open transport
- Use new operators, =, <>, <, >, <=, >=, instead
- IS REQUESTED is obsolete
- IF lx_exception IS INITIAL
- Unit test assert not in test class, or assert in test class
- double spaces, CONCATENATE  foo bar INTO result.
- no empty newlines when using COLON operator
- macro naming standard
- check program headers exists
- commented code(Standard check doesnt work very well)
- no update of standard SAP tables
- unused parameter in class method(dynamic references? redefined method, interfaces?)
- number of sequential blank lines


Common static class to ensure good performance
One class per check, grouping similar

Design goals
try and see if it works out
performance not high priority
unit test'able
only english supported


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

### CHECK_04 - Line length
Check line length, default = max 90 characters
Only 72(?) characters shown in short dumps and in 2 column program compare

### CHECK_05 - 7 bit ASCII
Error if source code contains non 7 bit ASCII characters

### CHECK_06 - Check for use of pretty printer
check for use of pretty printer
