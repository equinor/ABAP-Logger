INTERFACE zif_logger_settings PUBLIC.
  "! Is the log automatically saved when adding messages?
  "! See setter for more details.
  "!
  "! @parameter result | Automatic saving is enabled
  METHODS get_autosave
    RETURNING VALUE(result) TYPE abap_bool.

  "! Set to true if the log is automatically saved when adding messages.
  "!
  "! If auto save is disabled, the save() method has to be called manually
  "! to write the data to the database (it commits the LUW).
  "! If auto save is enabled, the save() method has no effect.
  "! By default, auto save is enabled.
  "!
  "! Be careful with enabled auto save when processing mass data because it
  "! can decrease system performance significantly.
  "!
  "! @parameter auto_save | Enable automatic saving
  "! @parameter result    | Self-reference for chained calls
  METHODS set_autosave
    IMPORTING auto_save     TYPE abap_bool
    RETURNING VALUE(result) TYPE REF TO zif_logger_settings.

  "! Get the earliest date on which the log can be deleted.
  "! See setter for more details.
  "!
  "! @parameter result | Expiration date
  METHODS get_expiry_date
    RETURNING VALUE(result) TYPE aldate_del.

  "! Set the earliest date on which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  "!
  "! @parameter expiry_date | Expiration date
  "! @parameter result      | Self-reference for chained calls
  METHODS set_expiry_date
    IMPORTING expiry_date   TYPE aldate_del
    RETURNING VALUE(result) TYPE REF TO zif_logger_settings.

  "! Set the number of days after which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  "!
  "! @parameter number_of_days | Days until expiration date
  "! @parameter result         | Self-reference for chained calls
  METHODS set_expiry_in_days
    IMPORTING number_of_days TYPE i
    RETURNING VALUE(result)  TYPE REF TO zif_logger_settings.

  "! Does the log have to be kept until the expiration date is reached?
  "! See setter for more details.
  "!
  "! @parameter result |  Keep log until expiration date
  METHODS get_must_be_kept_until_expiry
    RETURNING VALUE(result) TYPE del_before.

  "! Set to true if log must be kept until the expiration date is reached. It
  "! cannot be deleted before (in transaction SLG2).
  "! The default is false.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  "!
  "! @parameter must_be_kept_until_expiry | Keep log until expiration date
  "! @parameter result                    | Self-reference for chained calls
  METHODS set_must_be_kept_until_expiry
    IMPORTING must_be_kept_until_expiry TYPE del_before
    RETURNING VALUE(result)             TYPE REF TO zif_logger_settings.

  "! Current limit on the number of previous exceptions that will be logged for an exception
  "!
  "! @parameter result | Limit on number of previous exceptions that are logged for an exception
  METHODS get_max_exception_drill_down
    RETURNING VALUE(result) TYPE i.

  "! Limit the number of previous exceptions that will be logged for an exception
  "!
  "! @parameter max_levels | Limit on number of previous exceptions to log for an exception
  "! @parameter result     | Self-reference for chained calls
  METHODS set_max_exception_drill_down
    IMPORTING max_levels    TYPE i
    RETURNING VALUE(result) TYPE REF TO zif_logger_settings.

  "! Is a secondary database connection used to write the log entries to the database?
  "! See setter for more details.
  "!
  "! @parameter result | Secondary database connection is used
  METHODS get_usage_of_secondary_db_conn
    RETURNING VALUE(result) TYPE flag.

  "! Set to true if secondary database connection should be used to write the log entries to the database.
  "! This is important if main program does a rollback (on purpose or after a dump).
  "! The default is true.
  "!
  "! @parameter use_2nd_db_connection | Use secondary database connection
  "! @parameter result                | Self-reference for chained calls
  METHODS set_usage_of_secondary_db_conn
    IMPORTING use_2nd_db_connection TYPE flag
    RETURNING VALUE(result)         TYPE REF TO zif_logger_settings.
ENDINTERFACE.
