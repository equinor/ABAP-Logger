CLASS zcl_logger_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    INTERFACES zif_logger_settings.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA auto_save                 TYPE abap_bool.
    DATA expiry_date               TYPE aldate_del.
    DATA must_be_kept_until_expiry TYPE del_before.
    DATA max_exception_drill_down  TYPE i.
    DATA use_2nd_db_connection     TYPE flag.
ENDCLASS.



CLASS zcl_logger_settings IMPLEMENTATION.

  METHOD constructor.
    must_be_kept_until_expiry = abap_false.
    max_exception_drill_down  = 10.
    use_2nd_db_connection     = abap_true.
    auto_save                 = abap_true.
  ENDMETHOD.

  METHOD zif_logger_settings~get_autosave.
    result = auto_save.
  ENDMETHOD.

  METHOD zif_logger_settings~set_autosave.
    me->auto_save = auto_save.
    result = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_expiry_date.
    result = expiry_date.
  ENDMETHOD.

  METHOD zif_logger_settings~set_expiry_date.
    me->expiry_date = expiry_date.
    result      = me.
  ENDMETHOD.

  METHOD zif_logger_settings~set_expiry_in_days.
    IF number_of_days > 0.
      expiry_date = sy-datum + number_of_days.
    ENDIF.
    result = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_must_be_kept_until_expiry.
    result = must_be_kept_until_expiry.
  ENDMETHOD.

  METHOD zif_logger_settings~set_must_be_kept_until_expiry.
    me->must_be_kept_until_expiry = must_be_kept_until_expiry.
    result                    = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_max_exception_drill_down.
    result = max_exception_drill_down.
  ENDMETHOD.

  METHOD zif_logger_settings~set_max_exception_drill_down.
    IF max_levels >= 0.
      max_exception_drill_down = max_levels.
    ENDIF.
    result = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_usage_of_secondary_db_conn.
    result = use_2nd_db_connection.
  ENDMETHOD.

  METHOD zif_logger_settings~set_usage_of_secondary_db_conn.
    me->use_2nd_db_connection = use_2nd_db_connection.
    result                = me.
  ENDMETHOD.

ENDCLASS.
