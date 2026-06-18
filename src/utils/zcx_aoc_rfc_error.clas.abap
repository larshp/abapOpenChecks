"! <p class="shorttext synchronized">Raised when RFC error occurs</p>
"! <p>
"! This exception inherits from CX_NO_CHECK because we want to cause a runtime error on purpose.
"! Runtime errors show up as "check failures" in the ATC result to indicate that the check itself was not successful.
"! It is better to explicitly cause a "check failure" than to mix this with the regular findings.
"! </p>
CLASS zcx_aoc_rfc_error DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

    METHODS constructor
      IMPORTING
        textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous                 OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_aoc_rfc_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
