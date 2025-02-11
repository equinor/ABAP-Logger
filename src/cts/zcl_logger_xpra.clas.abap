CLASS zcl_logger_xpra DEFINITION
  PUBLIC
  INHERITING FROM zcl_logger_cts
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING settings TYPE REF TO zif_logger_settings OPTIONAL.

  PROTECTED SECTION.
    METHODS save_log REDEFINITION.

  PRIVATE SECTION.
    METHODS log_first_message
      IMPORTING report_name TYPE csequence.

    METHODS exception_after_xpra_log_subrc
      IMPORTING VALUE(return_code)   TYPE syst_subrc
                function_module_name TYPE funcname.
ENDCLASS.


CLASS zcl_logger_xpra IMPLEMENTATION.
  METHOD constructor.
    super->constructor( settings ).
    settings->set_autosave( abap_true ).
  ENDMETHOD.

  METHOD exception_after_xpra_log_subrc.
    CASE return_code.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: File not found|.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: Wrong call|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } return code { return_code }|.
    ENDCASE.
  ENDMETHOD.

  METHOD log_first_message.
    DATA first_message TYPE STANDARD TABLE OF sprot_u.

    APPEND VALUE sprot_u( level    = message_level-summary
                          severity = message_severity-open
                          var1     = report_name )
           TO first_message.

    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES     xmsg           = first_message
      EXCEPTIONS file_not_found = 1
                 wrong_call     = 2.
    IF sy-subrc <> 0.
      exception_after_xpra_log_subrc( return_code          = sy-subrc
                                      function_module_name = 'TR_APPEND_LOG' ).
    ENDIF.
  ENDMETHOD.

  METHOD save_log.
    DATA log_messages TYPE STANDARD TABLE OF sprot_u WITH EMPTY KEY.

    log_messages = messages_from_application_log( ).
    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES     xmsg           = log_messages
      EXCEPTIONS file_not_found = 1
                 wrong_call     = 2.
    IF sy-subrc <> 0.
      exception_after_xpra_log_subrc( return_code          = sy-subrc
                                      function_module_name = 'TR_APPEND_LOG' ).
    ELSE.
      CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
        EXPORTING  i_log_handle  = me->handle
        EXCEPTIONS log_not_found = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
