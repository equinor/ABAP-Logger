CLASS zcl_logger_factory DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_injector.

  PUBLIC SECTION.
    "! Starts a new log.
    "!
    "! @parameter object    | Application log object
    "! @parameter subobject | Application log sub-object
    "! @parameter extnumber | External ID for application log
    "! @parameter desc      | External ID for application log (kept temporarily for backwards compatibility)
    "! @parameter context   | Application log context
    "! @parameter settings  | Logger settings
    "! @parameter result    | Logger
    CLASS-METHODS create_log
      IMPORTING !object       TYPE csequence                  OPTIONAL
                subobject     TYPE csequence                  OPTIONAL
                extnumber     TYPE csequence                  OPTIONAL
                desc          TYPE csequence                  OPTIONAL
                !context      TYPE any                        OPTIONAL
                settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zif_logger.

    "! Reopens an already existing log.
    "!
    "! @parameter object                   | Application log object
    "! @parameter subobject                | Application log sub-object
    "! @parameter extnumber                | External ID for application log
    "! @parameter create_if_does_not_exist | Create a new application log if no logs are found
    "! @parameter settings                 | Logger settings
    "! @parameter result                   | Logger
    CLASS-METHODS open_log
      IMPORTING !object                  TYPE csequence
                subobject                TYPE csequence
                extnumber                TYPE csequence                  OPTIONAL
                create_if_does_not_exist TYPE abap_bool                  DEFAULT abap_false
                settings                 TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result)            TYPE REF TO zif_logger.

    "! Creates a settings object which can be modified. It can be pass on
    "! the creation of the logger to change its behavior.
    "!
    "! @parameter result | Logger settings
    CLASS-METHODS create_settings
      RETURNING VALUE(result) TYPE REF TO zif_logger_settings.

    CLASS-METHODS create_collection
      RETURNING VALUE(result) TYPE REF TO zif_logger_collection.

    "! Create display profile
    "!
    "! @parameter predefined_profile_type    | Constants are defined in ZIF_LOGGER_DISPLAY_PROFILE
    "! @parameter result                     | Display profile
    "! @raising   zcx_logger_display_profile | Display profile exception
    CLASS-METHODS create_display_profile
      IMPORTING predefined_profile_type TYPE i OPTIONAL
      RETURNING VALUE(result)           TYPE REF TO zif_logger_display_profile
      RAISING   zcx_logger_display_profile.

    "! Reopens specific log instance.
    "!
    "! @parameter db_number | Log number
    "! @parameter settings  | Logger settings
    "! @parameter result    | Logger
    CLASS-METHODS open_log_by_db_number
      IMPORTING db_number     TYPE balognr
                settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zif_logger.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Name of background job which executes XPRA reports</p>
    CONSTANTS xpra_job_name TYPE btcjob VALUE 'RDDEXECL'.

    CLASS-DATA log_logger          TYPE REF TO zif_logger.
    CLASS-DATA log_settings        TYPE REF TO zif_logger_settings.
    CLASS-DATA log_collection      TYPE REF TO zif_logger_collection.
    CLASS-DATA log_display_profile TYPE REF TO zif_logger_display_profile.

    CLASS-METHODS find_log_headers
      IMPORTING !object       TYPE csequence OPTIONAL
                subobject     TYPE csequence OPTIONAL
                extnumber     TYPE csequence OPTIONAL
                db_number     TYPE balognr   OPTIONAL
      RETURNING VALUE(result) TYPE balhdr_t.

    CLASS-METHODS get_settings
      IMPORTING settings      TYPE REF TO zif_logger_settings
      RETURNING VALUE(result) TYPE REF TO zif_logger_settings.

    CLASS-METHODS is_executing_as_xpra
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS open_log_by_header
      IMPORTING !header       TYPE balhdr
                settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zif_logger.

    CLASS-METHODS create_xpra_logger
      IMPORTING logger_settings TYPE REF TO zif_logger_settings
      RETURNING VALUE(result)   TYPE REF TO zcl_logger_xpra.

    CLASS-METHODS create_sbal_logger
      IMPORTING logger_settings TYPE REF TO zif_logger_settings
                !object         TYPE csequence OPTIONAL
                subobject       TYPE csequence OPTIONAL
                extnumber       TYPE csequence OPTIONAL
                !context        TYPE any       OPTIONAL
      RETURNING VALUE(result)   TYPE REF TO zcl_logger_sbal.
ENDCLASS.


CLASS zcl_logger_factory IMPLEMENTATION.
  METHOD create_collection.
    IF log_collection IS INITIAL.
      CREATE OBJECT result TYPE zcl_logger_collection.
    ELSE.
      result = log_collection.
    ENDIF.
  ENDMETHOD.

  METHOD create_display_profile.
    IF log_display_profile IS INITIAL.
      CREATE OBJECT result TYPE zcl_logger_display_profile.
    ELSE.
      result = log_display_profile.
    ENDIF.

    IF predefined_profile_type IS SUPPLIED.
      result->set_predefined_profile( predefined_profile_type ).
    ENDIF.
  ENDMETHOD.

  METHOD create_log.
    ASSERT desc IS NOT SUPPLIED OR extnumber IS NOT SUPPLIED.

    IF log_logger IS NOT INITIAL.
      result = log_logger.
    ELSEIF is_executing_as_xpra( ) = abap_true.
      result = create_xpra_logger( get_settings( settings ) ).
    ELSE.
      result = create_sbal_logger( object          = object
                                   subobject       = subobject
                                   extnumber       = extnumber
                                   context         = context
                                   logger_settings = get_settings( settings ) ).
    ENDIF.
  ENDMETHOD.

  METHOD create_settings.
    IF log_settings IS INITIAL.
      CREATE OBJECT result TYPE zcl_logger_settings.
    ELSE.
      result = log_settings.
    ENDIF.
  ENDMETHOD.

  METHOD open_log.
    DATA found_headers      TYPE balhdr_t.
    DATA most_recent_header TYPE balhdr.

    found_headers = find_log_headers( object    = object
                                      subobject = subobject
                                      extnumber = extnumber ).

    IF lines( found_headers ) = 0.
      IF create_if_does_not_exist = abap_true.
        result = create_log( object    = object
                             subobject = subobject
                             extnumber = extnumber
                             settings  = settings ).
      ENDIF.
      RETURN.
    ENDIF.

    " Delete all but the last row.
    IF lines( found_headers ) > 1.
      DELETE found_headers TO ( lines( found_headers ) - 1 ).
    ENDIF.
    READ TABLE found_headers INDEX 1 INTO most_recent_header.

    result = open_log_by_header( header   = most_recent_header
                                 settings = settings ).
  ENDMETHOD.

  METHOD open_log_by_db_number.
    DATA header      TYPE balhdr.
    DATA log_headers TYPE balhdr_t.

    log_headers = find_log_headers( db_number = db_number ).
    IF lines( log_headers ) <> 1.
      "^Should find exactly one log since db_number is unique identifier
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    READ TABLE log_headers INDEX 1 INTO header.
    result = open_log_by_header( header   = header
                                 settings = settings ).
  ENDMETHOD.

  METHOD find_log_headers.
    DATA filter      TYPE bal_s_lfil.
    DATA l_object    TYPE balobj_d.
    DATA l_subobject TYPE balsubobj.
    DATA ext_number  TYPE balnrext.
    DATA log_numbers TYPE bal_t_logn.

    l_object    = object.
    l_subobject = subobject.
    ext_number = extnumber.
    IF db_number IS SUPPLIED.
      INSERT db_number INTO TABLE log_numbers.
    ENDIF.

    CALL FUNCTION 'BAL_FILTER_CREATE'
      EXPORTING i_object       = l_object
                i_subobject    = l_subobject
                i_extnumber    = ext_number
                i_t_lognumber  = log_numbers
      IMPORTING e_s_log_filter = filter.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING  i_s_log_filter = filter
      IMPORTING  e_t_log_header = result
      EXCEPTIONS log_not_found  = 1.
  ENDMETHOD.

  METHOD get_settings.
    IF settings IS BOUND AND settings IS NOT INITIAL.
      result = settings.
    ELSE.
      result = create_settings( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_executing_as_xpra.
    " XPRA executes in client 000 in a specific job
    DATA current_job_name TYPE btcjob.

    result = abap_false.
    IF sy-mandt <> '000'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING  jobname         = current_job_name
      EXCEPTIONS no_runtime_info = 1.
    IF sy-subrc <> 0.
      RETURN.
    ELSEIF current_job_name = xpra_job_name.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD open_log_by_header.
    DATA log_headers TYPE balhdr_t.

    INSERT header INTO TABLE log_headers.

    " If you call BAL_DB_LOAD for a log that is already loaded, it doesn't return its handle, so don't rely on returned data
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING  i_t_log_header     = log_headers
      EXCEPTIONS no_logs_specified  = 1                " No logs specified
                 log_not_found      = 2                " Log not found
                 log_already_loaded = 3                " Log is already loaded
                 OTHERS             = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    DATA logger TYPE REF TO zcl_logger_sbal.
    IF log_logger IS INITIAL.
      logger = create_sbal_logger( logger_settings = get_settings( settings ) ).
      logger->handle    = header-log_handle.
      logger->db_number = header-lognumber.
      result = logger.
    ELSE.
      result = log_logger.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING i_log_handle = logger->handle
      IMPORTING e_s_log      = logger->header.
  ENDMETHOD.

  METHOD create_xpra_logger.
    DATA logger TYPE REF TO zcl_logger_xpra.

    CREATE OBJECT logger
      EXPORTING settings = logger_settings.
    result = logger.
  ENDMETHOD.

  METHOD create_sbal_logger.
    DATA logger TYPE REF TO zcl_logger_sbal.

    CREATE OBJECT logger
      EXPORTING object    = object
                subobject = subobject
                extnumber = extnumber
                context   = context
                settings  = logger_settings.
    result = logger.
  ENDMETHOD.
ENDCLASS.
