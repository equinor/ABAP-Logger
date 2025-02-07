INTERFACE zif_logger_deprecated
  PUBLIC.
  "! <p class="shorttext synchronized" lang="en">DEPRECATED - renamed to abend( )</p>
  "!
  METHODS a
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(result)       TYPE REF TO zif_logger_deprecated.

  "! <p class="shorttext synchronized" lang="en">DEPRECATED - renamed to error( )</p>
  "!
  METHODS e
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(result)       TYPE REF TO zif_logger_deprecated.

  "! <p class="shorttext synchronized" lang="en">DEPRECATED - renamed to warning( )</p>
  "!
  METHODS w
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(result)       TYPE REF TO zif_logger_deprecated.

  "! <p class="shorttext synchronized" lang="en">DEPRECATED - renamed to info( )</p>
  "!
  METHODS i
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(result)       TYPE REF TO zif_logger_deprecated.

  "! <p class="shorttext synchronized" lang="en">DEPRECATED - renamed to success( )</p>
  "!
  METHODS s
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(result)       TYPE REF TO zif_logger_deprecated.
ENDINTERFACE.
