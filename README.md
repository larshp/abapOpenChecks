abapOpenChecks
==============

Installation
- install abapGit
- clone this repository into your SAP system using abapGit
- activate checks in SCI
- configure check variant
- run check

Design goals
- try and see if it works out
- performance not high priority
- unit test'able
- only english supported


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
