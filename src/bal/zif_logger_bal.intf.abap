INTERFACE zif_logger_bal
  PUBLIC.
  METHODS get_handle
    RETURNING VALUE(result) TYPE balloghndl.

  METHODS get_db_number
    RETURNING VALUE(result) TYPE balognr.

  METHODS get_header
    RETURNING VALUE(result) TYPE bal_s_log.

  METHODS get_control_handle
    RETURNING VALUE(result) TYPE balcnthndl.
ENDINTERFACE.
