---
title: Performance
---

The runtime depends on which kind of objects are in your object set, and the way they are coded. Program ZAOC_PERFORMANCE can be run to analyze the performance of the different checks.

Below is a table where all checks has been run on a quite large code base,

Check         | Description   | Runtime
------------- | ------------- | ------------:
ZCL_AOC_CHECK_01 | IF in IF                                        | 35  s
ZCL_AOC_CHECK_02 | EXIT outside of loop                            | 21  s
ZCL_AOC_CHECK_03 | TRY without CATCH                               | 22  s
ZCL_AOC_CHECK_04 | Line length                                     | 22  s
ZCL_AOC_CHECK_05 | 7 bit ASCII                                     | 34  s
ZCL_AOC_CHECK_06 | Check pretty printer use                        | 94  s
ZCL_AOC_CHECK_07 | Functional writing style for CALL METHOD        | 25  s
ZCL_AOC_CHECK_08 | Obsolete statement                              | 24  s
ZCL_AOC_CHECK_09 | Tab instead of spaces                           | 36  s
ZCL_AOC_CHECK_10 | Use icon_ constants                             | 28  s
ZCL_AOC_CHECK_11 | Max one statement per line                      | 21  s
ZCL_AOC_CHECK_12 | Specify SORT order                              | 25  s
ZCL_AOC_CHECK_13 | Sequential blank lines                          | 30  s
ZCL_AOC_CHECK_14 | Commented code                                  | 117  s
ZCL_AOC_CHECK_17 | Definitions in top of routine                   | 20  s
ZCL_AOC_CHECK_18 | Empty branch                                    | 22  s
ZCL_AOC_CHECK_19 | Use LINE OF                                     | 25  s
ZCL_AOC_CHECK_20 | Bad indentation                                 | 22  s
ZCL_AOC_CHECK_21 | Unused FORM parameter                           | 43  s
ZCL_AOC_CHECK_22 | Conditions contain identical code               | 24  s
ZCL_AOC_CHECK_23 | CHECK outside of loop                           | 23  s
ZCL_AOC_CHECK_24 | Identical code blocks                           | 32  s
ZCL_AOC_CHECK_25 | Selection screen data not referenced statically | 36  s
ZCL_AOC_CHECK_26 | No direct changes to standard tables            | 28  s
ZCL_AOC_CHECK_27 | Last statement is RETURN                        | 24  s
ZCL_AOC_CHECK_28 | Space before . or ,                             | 22  s
ZCL_AOC_CHECK_29 | Naming, Local test classes                      | 23  s
ZCL_AOC_CHECK_30 | EXPORTING can be omitted                        | 24  s
ZCL_AOC_CHECK_31 | Extended Program Check, Filterable              | 612  s
ZCL_AOC_CHECK_32 | Database access                                 | 22  s
ZCL_AOC_CHECK_33 | Append structure field names                    | 18  s
ZCL_AOC_CHECK_34 | Large WHEN construct                            | 21  s
ZCL_AOC_CHECK_35 | Message not in use                              | 3  s
ZCL_AOC_CHECK_36 | Exception text not in use                       | 4  s
ZCL_AOC_CHECK_37 | Define message texts in SE91                    | 24  s
ZCL_AOC_CHECK_38 | Avoid use of SELECT-ENDSELECT                   | 21  s

Updated 2015-07-19