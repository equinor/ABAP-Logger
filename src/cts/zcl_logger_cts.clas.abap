CLASS zcl_logger_cts DEFINITION
  PUBLIC
  INHERITING FROM zcl_logger_bal
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING settings TYPE REF TO zif_logger_settings OPTIONAL.

  PROTECTED SECTION.
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

    TYPES ty_log_messages TYPE STANDARD TABLE OF sprot_u WITH EMPTY KEY.

    data file_name type string.

    METHODS messages_from_application_log
      RETURNING VALUE(result) TYPE ty_log_messages.

    METHODS save_log REDEFINITION.

    METHODS map_from_application_log_messg
      IMPORTING sbal_message  TYPE bal_s_msg
      RETURNING VALUE(result) TYPE sprot_u.

    METHODS get_message_level
      IMPORTING sbal_message  TYPE bal_s_msg
      RETURNING VALUE(result) TYPE sprot_u-level.

    METHODS get_message_severity
      IMPORTING sbal_message  TYPE bal_s_msg
      RETURNING VALUE(result) TYPE sprot_u-severity.

  PRIVATE SECTION.
    CLASS-METHODS create_new_log
      IMPORTING activity_type     TYPE c
                directory_type    TYPE c                          DEFAULT 'T'
                system_name       TYPE csequence
                transport_request TYPE csequence
                log_name          TYPE csequence                  OPTIONAL
                check_levels      TYPE xfeld
                settings          TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result)     TYPE REF TO zcl_logger_cts.

    METHODS exception_after_scts_log_subrc
      IMPORTING VALUE(return_code)   TYPE syst_subrc
                function_module_name TYPE funcname.
ENDCLASS.


CLASS zcl_logger_cts IMPLEMENTATION.
  METHOD constructor.
    super->constructor( settings ).
    settings->set_autosave( abap_true ).
  ENDMETHOD.

  METHOD create_new_log.
    DATA log_file_name TYPE trfile.

    CREATE OBJECT result
      EXPORTING settings = settings.

    CALL FUNCTION 'TR_INITIALIZE_LOG'
      EXPORTING  acttype         = activity_type
                 dirtype         = directory_type
                 sysname         = system_name
                 trkorr          = transport_request
                 trbat_logname   = log_name
                 iv_check_levels = check_levels
      IMPORTING  file            = log_file_name
      EXCEPTIONS wrong_call      = 1
                 OTHERS          = 2.
    IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    result->file_name = log_file_name.
  ENDMETHOD.

  METHOD exception_after_scts_log_subrc.
    CASE return_code.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: Invalid input|.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: File access error|.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: DB access error|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } return code { return_code }|.
    ENDCASE.
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

  METHOD messages_from_application_log.
    DATA message_handles  TYPE bal_t_msgh.
    DATA logged_message   TYPE bal_s_msg.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA logged_exception TYPE bal_s_excr.
    FIELD-SYMBOLS <mh> TYPE balmsghndl.

    message_handles = get_message_handles( ).
    LOOP AT message_handles ASSIGNING <mh>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING  i_s_msg_handle = <mh>
        IMPORTING  e_s_msg        = logged_message
        EXCEPTIONS log_not_found  = 1
                   msg_not_found  = 2
                   OTHERS         = 3.
      IF sy-subrc = 0.
        APPEND map_from_application_log_messg( logged_message ) TO result.
      ELSEIF sy-subrc = 2.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_READ'
          EXPORTING  i_s_msg_handle = <mh>
          IMPORTING  e_s_exc        = logged_exception
*                     e_exists_on_db =
*                     e_txt_msgty    =
*                     e_txt_detlevel =
*                     e_txt_probclass =
*                     e_txt_msg      =
*                     e_ltxt_msg     =
*                     e_warning_text_not_found =
          EXCEPTIONS log_not_found  = 1
                     msg_not_found  = 2
                     OTHERS         = 3.
        IF sy-subrc <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_log.
    DATA log_messages TYPE STANDARD TABLE OF sprot_u WITH EMPTY KEY.

    log_messages = messages_from_application_log( ).
    CALL FUNCTION 'TR_WRITE_LOG'
*      EXPORTING
*                 iv_log_type       = 'FILE'
*                 iv_logname_file   =
*                 iv_logname_db     =
*                 iv_logname_memory =
*                 iv_append_mode    = ' '
*                 iv_condense       = 'X'
      TABLES     it_msgs           = log_messages
      EXCEPTIONS invalid_input     = 1
                 file_access_error = 2
                 db_access_error   = 3
                 OTHERS            = 4.
    IF sy-subrc <> 0.
      exception_after_scts_log_subrc( return_code          = sy-subrc
                                      function_module_name = 'TR_WRITE_LOG' ).
    ELSE.
      CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
        EXPORTING  i_log_handle  = me->handle
        EXCEPTIONS log_not_found = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
