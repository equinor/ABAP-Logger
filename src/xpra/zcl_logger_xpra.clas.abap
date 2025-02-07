CLASS zcl_logger_xpra DEFINITION
  PUBLIC
  INHERITING FROM zcl_logger
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING settings TYPE REF TO zif_logger_settings OPTIONAL.

    METHODS zif_logger~add REDEFINITION.

  PROTECTED SECTION.
    METHODS add_text_string           REDEFINITION.
    METHODS add_message               REDEFINITION.
    METHODS add_exception_with_textid REDEFINITION.

    METHODS save_log                  REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Log level</p>
      BEGIN OF message_level,
        summary  TYPE sprot_u-level VALUE '1', " Statistics (e.g. summary of program results)
        error    TYPE sprot_u-level VALUE '2', " Error
        overview TYPE sprot_u-level VALUE '3', " Overview (e.g. logging all work steps)
        detail   TYPE sprot_u-level VALUE '4', " Details (e.g. additional information for analysis/investigation)
      END OF message_level.

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Message severity</p>
      BEGIN OF message_severity,
        cancelled   TYPE sprot_u-severity VALUE 'A', " Cancelled (internal error)
        fatal_error TYPE sprot_u-severity VALUE 'F', " Fatal error
        error       TYPE sprot_u-severity VALUE 'E', " Error (could not execute function)
        warning     TYPE sprot_u-severity VALUE 'W', " Warning
        information TYPE sprot_u-severity VALUE 'I', " Information
        status      TYPE sprot_u-severity VALUE 'S', " Status
        success     TYPE sprot_u-severity VALUE 'N', " Success (function executed)
        open        TYPE sprot_u-severity VALUE ' ', " Open (function not executed yet)
      END OF message_severity.

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Application log problem class </p>
      BEGIN OF problem_class,
        very_important       TYPE balprobcl VALUE '1', " Very important
        important            TYPE balprobcl VALUE '2', " Important
        medium               TYPE balprobcl VALUE '3', " Medium
        additional_informatn TYPE balprobcl VALUE '4', " Additional information
        other                TYPE balprobcl VALUE ' ', " Other
      END OF problem_class.
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Application log problem class </p>
      BEGIN OF message_type,
        abend       TYPE symsgty VALUE 'A', " Abend (Abort)
        error       TYPE symsgty VALUE 'E', " Error
        warning     TYPE symsgty VALUE 'W', " Warning
        information TYPE symsgty VALUE 'I', " Information
        success     TYPE symsgty VALUE 'S', " Success
      END OF message_type.

    DATA log_messages TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY. " Temporary storage until saving

    METHODS log_initial_message
      IMPORTING report_name TYPE csequence.

    METHODS map_from_application_log_messg
      IMPORTING sbal_message  TYPE bal_s_msg
      RETURNING VALUE(result) TYPE sprot_u.

    METHODS get_message_level
      IMPORTING sbal_message  TYPE bal_s_msg
      RETURNING VALUE(result) TYPE sprot_u-level.

    METHODS get_message_severity
      IMPORTING sbal_message  TYPE bal_s_msg
      RETURNING VALUE(result) TYPE sprot_u-severity.
ENDCLASS.


CLASS zcl_logger_xpra IMPLEMENTATION.
  METHOD constructor.
    super->constructor( settings ).
  ENDMETHOD.

  METHOD zif_logger~add.
    " TR_APPEND_LOG
  ENDMETHOD.

  METHOD log_initial_message.
    DATA initial_message TYPE STANDARD TABLE OF sprot_u.

    APPEND VALUE sprot_u( level    = message_level-summary
                          severity = message_severity-open
                          var1     = report_name )
           TO initial_message.

    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES     xmsg           = initial_message
      EXCEPTIONS file_not_found = 1
                 wrong_call     = 2.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.

  METHOD save_log.
    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES     xmsg           = log_messages
      EXCEPTIONS file_not_found = 1
                 wrong_call     = 2.
    IF sy-subrc = 0.
      CLEAR log_messages.
    ELSE.
      CASE sy-subrc.
        WHEN 1.
          RAISE EXCEPTION TYPE zcx_logger
            EXPORTING info = 'File not found'.
        WHEN 2.
          RAISE EXCEPTION TYPE zcx_logger
            EXPORTING info = 'Wrong call'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_logger.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD map_from_application_log_messg.
    " Convert application log message to transport log message
    " There is no equivalent to the new object indicator in application log
    result-level    = get_message_level( sbal_message ).
    result-severity = get_message_severity( sbal_message ).
    result-langu    = sy-langu.
    result-ag       = sbal_message-msgid.
    result-msgnr    = sbal_message-msgno.
    " result-newobj   = .
    result-var1     = sbal_message-msgv1.
    result-var2     = sbal_message-msgv2.
    result-var3     = sbal_message-msgv3.
    result-var4     = sbal_message-msgv4.
  ENDMETHOD.

  METHOD get_message_level.
    " Determine message level from problem class or message type
    result = COND #( WHEN sbal_message-probclass = problem_class-very_important
                       OR sbal_message-probclass = problem_class-important
                       OR sbal_message-msgty     = message_type-abend
                       OR sbal_message-msgty     = message_type-error                 THEN message_level-error
                     WHEN sbal_message-probclass = problem_class-medium
                       OR sbal_message-probclass = problem_class-additional_informatn
                       OR sbal_message-msgty     = message_type-warning               THEN message_level-overview
                     ELSE                                                                  message_level-detail ).
  ENDMETHOD.

  METHOD get_message_severity.
    result = SWITCH #( sbal_message-msgty
                       WHEN message_type-abend       THEN message_severity-cancelled
                       WHEN message_type-error       THEN message_severity-error
                       WHEN message_type-warning     THEN message_severity-warning
                       WHEN message_type-information THEN message_severity-information
                       WHEN message_type-success     THEN message_severity-success
                       ELSE                               message_severity-status ).
  ENDMETHOD.

  METHOD add_text_string.
    " TODO
  ENDMETHOD.

  METHOD add_message.
    " TODO
    " Not relevant for XPRAs - this was a workaround when re-using SBAL logger functionality
*    DATA log_messages    TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY.
*    DATA message_handles TYPE bal_t_msgh.
*    DATA message         TYPE bal_s_msg.
*    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.
*
*    message_handles = get_message_handles( ).
*    LOOP AT message_handles ASSIGNING <msg_handle>.
*      CALL FUNCTION 'BAL_LOG_MSG_READ'
*        EXPORTING  i_s_msg_handle = <msg_handle>
*        IMPORTING  e_s_msg        = message
*        EXCEPTIONS OTHERS         = 3.
*      IF sy-subrc IS INITIAL.
*        APPEND map_from_application_log_messg( message ) TO log_messages.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.

  METHOD add_exception_with_textid.
  ENDMETHOD.
ENDCLASS.
