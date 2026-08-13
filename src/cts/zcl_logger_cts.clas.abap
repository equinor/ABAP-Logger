CLASS zcl_logger_cts DEFINITION
  PUBLIC
  INHERITING FROM zcl_logger
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING settings TYPE REF TO zif_logger_settings OPTIONAL.

    METHODS zif_logger~free         REDEFINITION.
    METHODS zif_logger~has_errors   REDEFINITION.
    METHODS zif_logger~has_warnings REDEFINITION.
    METHODS zif_logger~is_empty     REDEFINITION.
    METHODS zif_logger~length       REDEFINITION.

  PROTECTED SECTION.
    TYPES ty_log_messages TYPE STANDARD TABLE OF sprot_u WITH EMPTY KEY.

    DATA unsaved_messages TYPE ty_log_messages.
    DATA all_messages     TYPE ty_log_messages.

    METHODS add_message               REDEFINITION.
    METHODS add_text_string           REDEFINITION.
    METHODS add_exception_with_textid REDEFINITION.
    METHODS save_log                  REDEFINITION.

    METHODS get_message_level
      IMPORTING importance    TYPE balprobcl
                messagetype   TYPE symsgty
      RETURNING VALUE(result) TYPE protlevel.

    METHODS get_message_severity
      IMPORTING messagetype   TYPE symsgty
      RETURNING VALUE(result) TYPE errortyp.

    METHODS has_message_with_severity
      IMPORTING severity      TYPE errortyp
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF message_level,
        summary  TYPE protlevel VALUE '1', " Statistics (e.g. summary of program results)
        error    TYPE protlevel VALUE '2', " Error
        overview TYPE protlevel VALUE '3', " Overview (e.g. logging all work steps)
        detail   TYPE protlevel VALUE '4', " Details (e.g. additional information for analysis/investigation)
      END OF message_level.
    CONSTANTS:
      BEGIN OF message_severity,
        cancelled   TYPE errortyp VALUE 'A', " Cancelled (internal error)
        fatal_error TYPE errortyp VALUE 'F', " Fatal error
        error       TYPE errortyp VALUE 'E', " Error (could not execute function)
        warning     TYPE errortyp VALUE 'W', " Warning
        information TYPE errortyp VALUE 'I', " Information
        status      TYPE errortyp VALUE 'S', " Status
        success     TYPE errortyp VALUE 'N', " Success (function executed)
        open        TYPE errortyp VALUE ' ', " Open (function not executed yet)
      END OF message_severity.

    CONSTANTS:
      BEGIN OF problem_class,
        very_important       TYPE balprobcl VALUE '1', " Very important
        important            TYPE balprobcl VALUE '2', " Important
        medium               TYPE balprobcl VALUE '3', " Medium
        additional_informatn TYPE balprobcl VALUE '4', " Additional information
        other                TYPE balprobcl VALUE ' ', " Other
      END OF problem_class.
    CONSTANTS:
      BEGIN OF message_type,
        exit        TYPE symsgty VALUE 'X', " Exit (Crash)
        abend       TYPE symsgty VALUE 'A', " Abend (Abort)
        error       TYPE symsgty VALUE 'E', " Error
        warning     TYPE symsgty VALUE 'W', " Warning
        information TYPE symsgty VALUE 'I', " Information
        success     TYPE symsgty VALUE 'S', " Success
      END OF message_type.

    DATA file_name TYPE string.

    CLASS-METHODS create_new_log
      IMPORTING activity_type     TYPE c
                directory_type    TYPE c                          DEFAULT 'T'
                system_name       TYPE csequence
                transport_request TYPE csequence
                log_name          TYPE csequence                  OPTIONAL
                check_levels      TYPE xfeld
                settings          TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result)     TYPE REF TO zcl_logger_cts.
ENDCLASS.


CLASS zcl_logger_cts IMPLEMENTATION.
  METHOD constructor.
    super->constructor( settings ).
    start_new_section = abap_true.
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

  METHOD get_message_level.
    " Determine message level from problem class or message type
    result = COND #( WHEN importance  = problem_class-very_important
                       OR importance  = problem_class-important
                       OR messagetype = message_type-abend
                       OR messagetype = message_type-error                 THEN message_level-error
                     WHEN importance  = problem_class-medium
                       OR importance  = problem_class-additional_informatn
                       OR messagetype = message_type-warning               THEN message_level-overview
                     ELSE                                                       message_level-detail ).
  ENDMETHOD.

  METHOD get_message_severity.
    result = SWITCH #( messagetype
                       WHEN message_type-exit        THEN message_severity-fatal_error
                       WHEN message_type-abend       THEN message_severity-cancelled
                       WHEN message_type-error       THEN message_severity-error
                       WHEN message_type-warning     THEN message_severity-warning
                       WHEN message_type-information THEN message_severity-information
                       WHEN message_type-success     THEN message_severity-status
                       ELSE                               message_severity-information ).
  ENDMETHOD.

  METHOD save_log.
    CALL FUNCTION 'TR_WRITE_LOG'
*      EXPORTING
*                 iv_log_type       = 'FILE'
*                 iv_logname_file   =
*                 iv_logname_db     =
*                 iv_logname_memory =
*                 iv_append_mode    = ' '
*                 iv_condense       = 'X'
      TABLES     it_msgs           = unsaved_messages
      EXCEPTIONS invalid_input     = 1
                 file_access_error = 2
                 db_access_error   = 3
                 OTHERS            = 4.
    CASE sy-subrc.
      WHEN 0.
        CLEAR unsaved_messages.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |TR_WRITE_LOG exception: Invalid input|.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |TR_WRITE_LOG exception: File access error|.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |TR_WRITE_LOG exception: DB access error|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |TR_WRITE_LOG return code { sy-subrc }|.
    ENDCASE.
  ENDMETHOD.

  METHOD add_message.
    DATA log_message TYPE sprot_u.

    IF application_log_message-detlevel IS NOT INITIAL.
      log_message-level = nmin( val1 = application_log_message-detlevel
                                val2 = message_level-detail ).
    ELSE.
      log_message-level = get_message_level( importance  = application_log_message-probclass
                                             messagetype = application_log_message-msgty ).
    ENDIF.
    log_message-severity = get_message_severity( application_log_message-msgty ).
    log_message-langu    = sy-langu.
    log_message-ag       = application_log_message-msgid.
    log_message-msgnr    = application_log_message-msgno.
    log_message-newobj   = start_new_section.
    log_message-var1     = application_log_message-msgv1.
    log_message-var2     = application_log_message-msgv2.
    log_message-var3     = application_log_message-msgv3.
    log_message-var4     = application_log_message-msgv4.

    APPEND log_message TO unsaved_messages.
    APPEND log_message TO all_messages.
    start_new_section = abap_false.
  ENDMETHOD.

  METHOD add_text_string.
    DATA log_message TYPE sprot_u.
    DATA: BEGIN OF string_as_msgv,
            v1 TYPE symsgv,
            v2 TYPE symsgv,
            v3 TYPE symsgv,
            v4 TYPE symsgv,
          END OF string_as_msgv.

    string_as_msgv = text_string.

    IF detail_level IS SUPPLIED AND detail_level IS NOT INITIAL.
      log_message-level = nmin( val1 = detail_level
                                val2 = message_level-detail ).
    ELSE.
      log_message-level = get_message_level( importance  = importance
                                             messagetype = message_type ).
    ENDIF.
    log_message-severity = get_message_severity( message_type ).
    log_message-langu    = sy-langu.
    log_message-ag       = 'TO'.
    log_message-msgnr    = '000'.
    log_message-newobj   = start_new_section.
    log_message-var1     = string_as_msgv-v1.
    log_message-var2     = string_as_msgv-v2.
    log_message-var3     = string_as_msgv-v3.
    log_message-var4     = string_as_msgv-v4.

    APPEND log_message TO unsaved_messages.
    APPEND log_message TO all_messages.
    start_new_section = abap_false.
  ENDMETHOD.

  METHOD add_exception_with_textid.
    add_text_string( text_string  = exception_data-exception->get_text( )
                     message_type = exception_data-msgty
                     importance   = exception_data-probclass
                     detail_level = exception_data-detlevel ).
  ENDMETHOD.

  METHOD zif_logger~free.
    CALL FUNCTION 'TR_FLUSH_LOG'.
  ENDMETHOD.

  METHOD zif_logger~has_errors.
    result = boolc(    has_message_with_severity( message_severity-error )
                    OR has_message_with_severity( message_severity-fatal_error )
                    OR has_message_with_severity( message_severity-cancelled ) ).
  ENDMETHOD.

  METHOD zif_logger~has_warnings.
    result = has_message_with_severity( message_severity-warning ).
  ENDMETHOD.

  METHOD zif_logger~is_empty.
    result = boolc( all_messages IS INITIAL ).
  ENDMETHOD.

  METHOD zif_logger~length.
    result = lines( all_messages ).
  ENDMETHOD.

  METHOD has_message_with_severity.
    READ TABLE all_messages TRANSPORTING NO FIELDS
         WITH KEY severity = severity.
    IF sy-subrc = 0.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
