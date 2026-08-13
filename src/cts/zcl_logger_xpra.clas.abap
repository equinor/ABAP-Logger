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
ENDCLASS.


CLASS zcl_logger_xpra IMPLEMENTATION.
  METHOD constructor.
    super->constructor( settings ).
    settings->set_autosave( abap_false ).
  ENDMETHOD.

  METHOD save_log.
    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES     xmsg           = unsaved_messages
      EXCEPTIONS file_not_found = 1
                 wrong_call     = 2
                 OTHERS         = 3.
    CASE sy-subrc.
      WHEN 0.
        CLEAR unsaved_messages.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |TR_APPEND_LOG exception: File not found|.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |TR_APPEND_LOG exception: Wrong call|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |TR_APPEND_LOG return code { sy-subrc }|.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
