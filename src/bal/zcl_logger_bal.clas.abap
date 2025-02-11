CLASS zcl_logger_bal DEFINITION
  PUBLIC
  INHERITING FROM zcl_logger
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    INTERFACES zif_logger_bal.
    INTERFACES zif_logger_ui.

    METHODS constructor
      IMPORTING settings TYPE REF TO zif_logger_settings OPTIONAL.

    METHODS zif_logger~set_header                   REDEFINITION.
    METHODS zif_logger_log_object~get_message_table REDEFINITION.
    METHODS zif_logger~export_to_table              REDEFINITION.
    METHODS zif_logger~free                         REDEFINITION.
    METHODS zif_logger~has_errors                   REDEFINITION.
    METHODS zif_logger~has_warnings                 REDEFINITION.
    METHODS zif_logger~length                       REDEFINITION.

  PROTECTED SECTION.
    DATA handle         TYPE balloghndl.
    DATA db_number      TYPE balognr.
    DATA header         TYPE bal_s_log.
    DATA control_handle TYPE balcnthndl.

    METHODS add_exception_with_textid REDEFINITION.
    METHODS add_message               REDEFINITION.
    METHODS add_text_string           REDEFINITION.

    METHODS save_log                  REDEFINITION.

    METHODS get_message_handles
      IMPORTING msgtype                   TYPE symsgty OPTIONAL
      RETURNING VALUE(rt_message_handles) TYPE bal_t_msgh.

  PRIVATE SECTION.
    CLASS-METHODS create_new_log
      IMPORTING !object       TYPE csequence                  OPTIONAL
                subobject     TYPE csequence                  OPTIONAL
                extnumber     TYPE csequence                  OPTIONAL
                !context      TYPE any                        OPTIONAL
                settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_logger_bal.

    CLASS-METHODS open_existing_log
      IMPORTING log_header    TYPE balhdr                     OPTIONAL
                log_handle    TYPE balloghndl                 OPTIONAL
                log_number    TYPE balognr                    OPTIONAL
                settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_logger_bal.

    CLASS-METHODS find_log_headers
      IMPORTING !object       TYPE csequence OPTIONAL
                subobject     TYPE csequence OPTIONAL
                extnumber     TYPE csequence OPTIONAL
                db_number     TYPE balognr   OPTIONAL
      RETURNING VALUE(result) TYPE balhdr_t.

    METHODS exception_after_bal_log_subrc
      IMPORTING VALUE(return_code)   TYPE syst_subrc
                function_module_name TYPE funcname.
ENDCLASS.


CLASS zcl_logger_bal IMPLEMENTATION.
  METHOD constructor.
    super->constructor( settings ).
  ENDMETHOD.

  METHOD create_new_log.
    FIELD-SYMBOLS <context_val> TYPE c.

    CREATE OBJECT result
      EXPORTING settings = settings.

    result->header-object    = object.
    result->header-subobject = subobject.
    result->header-extnumber = extnumber.
    result->header-context   = context.

    " Special case: Logger can work without object - but then the data cannot be written to the database.
    IF result->header-object IS INITIAL.
      result->settings->set_autosave( abap_false ).
    ENDIF.

    " Set deletion date and set if log can be deleted before deletion date is reached.
    result->header-aldate_del = result->settings->get_expiry_date( ).
    result->header-del_before = result->settings->get_must_be_kept_until_expiry( ).

    IF context IS SUPPLIED AND context IS NOT INITIAL.
      result->header-context-tabname =
        cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
      ASSIGN context TO <context_val> CASTING.
      result->header-context-value = <context_val>.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING i_s_log      = result->header
      IMPORTING e_log_handle = result->handle.

    " BAL_LOG_CREATE will fill in some additional header data.
    " This FM updates our instance attribute to reflect that.
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING i_log_handle = result->handle
      IMPORTING e_s_log      = result->header.
  ENDMETHOD.

  METHOD open_existing_log.
    DATA log_headers        TYPE balhdr_t.
    DATA log_handles        TYPE bal_t_logh.
    DATA log_numbers        TYPE bal_t_logn.
    DATA opened_log_handles TYPE bal_t_logh.
    DATA opened_log_handle  LIKE LINE OF opened_log_handles.

    IF log_header IS INITIAL AND log_handle IS INITIAL AND log_number IS INITIAL.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING info = |Log to open must be identified by header, handle or number|.
    ENDIF.

    CREATE OBJECT result
      EXPORTING settings = settings.

    IF log_header IS NOT INITIAL.
      APPEND log_header TO log_headers.
    ENDIF.
    IF log_handle IS NOT INITIAL.
      APPEND log_handle TO log_handles.
    ENDIF.
    IF log_number IS NOT INITIAL.
      APPEND log_number TO log_numbers.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING  i_t_log_header                = log_headers
                 i_t_log_handle                = log_handles
                 i_t_lognumber                 = log_numbers
                 i_exception_if_already_loaded = 'X'
      IMPORTING  e_t_log_handle                = opened_log_handles
      EXCEPTIONS no_logs_specified             = 1
                 log_not_found                 = 2
                 log_already_loaded            = 3
                 OTHERS                        = 4.
    CASE sy-subrc.
      WHEN 0.
        READ TABLE opened_log_handles INDEX 1 INTO opened_log_handle.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |BAL_DB_LOAD exception: No logs specified|.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |BAL_DB_LOAD exception: Log not found|.
      WHEN 3.
        opened_log_handle = log_handle.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |BAL_DB_LOAD return code { sy-subrc }|.
    ENDCASE.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING  i_log_handle  = opened_log_handle
      IMPORTING  e_s_log       = result->header
                 e_lognumber   = result->db_number
      EXCEPTIONS log_not_found = 1
                 OTHERS        = 2.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |BAL_LOG_HDR_READ exception: Log not found|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |BAL_LOG_HDR_READ return code { sy-subrc }|.
    ENDCASE.

    result->handle = opened_log_handle.
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

  METHOD get_message_handles.
    DATA log_handle TYPE bal_t_logh.
    DATA filter     TYPE bal_s_mfil.

    FIELD-SYMBOLS <f> LIKE LINE OF filter-msgty.

    INSERT handle INTO TABLE log_handle.

    IF msgtype IS NOT INITIAL.
      APPEND INITIAL LINE TO filter-msgty ASSIGNING <f>.
      <f>-sign   = 'I'.
      <f>-option = 'EQ'.
      <f>-low    = msgtype.
    ENDIF.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING  i_t_log_handle = log_handle
                 i_s_msg_filter = filter
      IMPORTING  e_t_msg_handle = rt_message_handles
      EXCEPTIONS msg_not_found  = 0.
  ENDMETHOD.

  METHOD zif_logger_ui~display_as_popup.
    " See SBAL_DEMO_04_POPUP for ideas
    DATA relevant_profile TYPE bal_s_prof.
    DATA log_handles      TYPE bal_t_logh.

    INSERT handle INTO TABLE log_handles.

    IF profile IS SUPPLIED AND profile IS NOT INITIAL.
      relevant_profile = profile.
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING e_s_display_profile = relevant_profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING i_s_display_profile = relevant_profile
                i_t_log_handle      = log_handles.
  ENDMETHOD.

  METHOD zif_logger_ui~display_fullscreen.
    DATA relevant_profile TYPE bal_s_prof.
    DATA log_handles      TYPE bal_t_logh.

    INSERT handle INTO TABLE log_handles.

    IF profile IS SUPPLIED AND profile IS NOT INITIAL.
      relevant_profile = profile.
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
        IMPORTING e_s_display_profile = relevant_profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING i_s_display_profile = relevant_profile
                i_t_log_handle      = log_handles.
  ENDMETHOD.

  METHOD zif_logger_ui~display_in_container.
    DATA relevant_profile TYPE bal_s_prof.
    DATA log_handles      TYPE bal_t_logh.

    INSERT handle INTO TABLE log_handles.

    IF control_handle IS INITIAL.

      IF profile IS SUPPLIED AND profile IS NOT INITIAL.
        relevant_profile = profile.
      ELSE.
        CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
          IMPORTING e_s_display_profile = relevant_profile.
      ENDIF.

      " create control to display log
      CALL FUNCTION 'BAL_CNTL_CREATE'
        EXPORTING  i_container          = container
                   i_s_display_profile  = relevant_profile
                   i_t_log_handle       = log_handles
        IMPORTING  e_control_handle     = control_handle
        EXCEPTIONS profile_inconsistent = 1
                   internal_error       = 2.
      ASSERT sy-subrc = 0.

    ELSE.

      " refresh control
      CALL FUNCTION 'BAL_CNTL_REFRESH'
        EXPORTING  i_control_handle  = control_handle
                   i_t_log_handle    = log_handles
        EXCEPTIONS control_not_found = 1
                   internal_error    = 2.
      ASSERT sy-subrc = 0.

    ENDIF.
  ENDMETHOD.

  METHOD add_exception_with_textid.
    CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
      EXPORTING  i_log_handle     = handle
                 i_s_exc          = exception_data
      EXCEPTIONS log_not_found    = 1
                 msg_inconsistent = 2
                 log_is_full      = 3
                 OTHERS           = 4.
    IF sy-subrc <> 0.
      exception_after_bal_log_subrc( return_code          = sy-subrc
                                     function_module_name = 'BAL_LOG_EXCEPTION_ADD' ).
    ENDIF.
  ENDMETHOD.

  METHOD add_message.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING  i_log_handle     = handle
                 i_s_msg          = application_log_message
      EXCEPTIONS log_not_found    = 1
                 msg_inconsistent = 2
                 log_is_full      = 3
                 OTHERS           = 4.
    IF sy-subrc <> 0.
      exception_after_bal_log_subrc( return_code          = sy-subrc
                                     function_module_name = 'BAL_LOG_MSG_ADD' ).
    ENDIF.
  ENDMETHOD.

  METHOD add_text_string.
    TRY.
        CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
          EXPORTING  i_log_handle     = me->handle
                     i_msgty          = message_type
                     i_probclass      = importance
                     i_text           = text_string
                     i_s_context      = log_context
                     i_s_params       = log_parameters
                     i_detlevel       = detail_level
          EXCEPTIONS log_not_found    = 1
                     msg_inconsistent = 2
                     log_is_full      = 3
                     OTHERS           = 4.
        IF sy-subrc <> 0.
          exception_after_bal_log_subrc( return_code          = sy-subrc
                                         function_module_name = 'BAL_LOG_MSG_ADD_FREE_TEXT' ).
        ENDIF.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
          EXPORTING  i_log_handle     = me->handle
                     i_msgty          = message_type
                     i_probclass      = importance
                     i_text           = text_string
                     i_s_context      = log_context
                     i_s_params       = log_parameters
          EXCEPTIONS log_not_found    = 1
                     msg_inconsistent = 2
                     log_is_full      = 3
                     OTHERS           = 4.
        IF sy-subrc <> 0.
          exception_after_bal_log_subrc( return_code          = sy-subrc
                                         function_module_name = 'BAL_LOG_MSG_ADD_FREE_TEXT' ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD save_log.
    DATA log_handles       TYPE bal_t_logh.
    DATA log_numbers       TYPE bal_t_lgnm.
    DATA log_number        TYPE bal_s_lgnm.
    DATA secondary_db_conn TYPE flag.

    secondary_db_conn = settings->get_usage_of_secondary_db_conn( ).

    INSERT me->handle INTO TABLE log_handles.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING  i_t_log_handle       = log_handles
                 i_2th_connection     = secondary_db_conn
                 i_2th_connect_commit = secondary_db_conn
      IMPORTING  e_new_lognumbers     = log_numbers
      EXCEPTIONS log_not_found        = 1
                 save_not_allowed     = 2
                 numbering_error      = 3
                 OTHERS               = 4.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF me->db_number IS INITIAL.
      READ TABLE log_numbers INDEX 1 INTO log_number.
      db_number = log_number-lognumber.
    ENDIF.
    IF sy-batch = abap_true.
      CALL FUNCTION 'BP_ADD_APPL_LOG_HANDLE'
        EXPORTING  loghandle = handle
        EXCEPTIONS OTHERS    = 0.
    ENDIF.
  ENDMETHOD.

  METHOD exception_after_bal_log_subrc.
    CASE return_code.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: Log not found|.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: Message inconsistent|.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } exception: Log is full|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING info = |{ function_module_name } return code { return_code }|.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_logger~set_header.
    me->header-extnumber = description.

    CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
      EXPORTING  i_log_handle            = me->handle
                 i_s_log                 = header
      EXCEPTIONS log_not_found           = 1
                 log_header_inconsistent = 2
                 OTHERS                  = 3.
    ASSERT sy-subrc = 0.

    result = me.
  ENDMETHOD.

  METHOD zif_logger_log_object~get_message_table.
    DATA message_handles TYPE bal_t_msgh.
    DATA message         TYPE bal_s_msg.
    DATA message_result  TYPE zif_logger_log_object~ty_message.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    message_handles = get_message_handles( ).

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING  i_s_msg_handle = <msg_handle>
        IMPORTING  e_s_msg        = message
        EXCEPTIONS OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        message_result-type = message-msgty.
        message_result-symsg-msgid = message-msgid.
        message_result-symsg-msgno = message-msgno.
        message_result-symsg-msgv1 = message-msgv1.
        message_result-symsg-msgv2 = message-msgv2.
        message_result-symsg-msgv3 = message-msgv3.
        message_result-symsg-msgv4 = message-msgv4.
        APPEND message_result TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_logger~export_to_table.
    DATA message_handles TYPE bal_t_msgh.
    DATA message         TYPE bal_s_msg.
    DATA bapiret2        TYPE bapiret2.
    DATA exception_msg   TYPE c LENGTH 255.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    message_handles = get_message_handles( ).

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CLEAR bapiret2.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING  i_s_msg_handle = <msg_handle>
        IMPORTING  e_s_msg        = message
        EXCEPTIONS OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        MESSAGE ID message-msgid
                TYPE message-msgty
                NUMBER message-msgno
                INTO bapiret2-message
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4.

        bapiret2-type       = message-msgty.
        bapiret2-id         = message-msgid.
        bapiret2-number     = message-msgno.
        bapiret2-log_no     = <msg_handle>-log_handle.     " last 2 chars missing!!
        bapiret2-log_msg_no = <msg_handle>-msgnumber.
        bapiret2-message_v1 = message-msgv1.
        bapiret2-message_v2 = message-msgv2.
        bapiret2-message_v3 = message-msgv3.
        bapiret2-message_v4 = message-msgv4.
        bapiret2-system     = sy-sysid.
        APPEND bapiret2 TO result.
      ELSE.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_READ'
          EXPORTING  i_s_msg_handle = <msg_handle>
                     i_langu        = sy-langu
          IMPORTING  e_txt_msg      = exception_msg
          EXCEPTIONS log_not_found  = 1
                     msg_not_found  = 2
                     OTHERS         = 3.
        IF sy-subrc = 0.
          bapiret2-type       = message-msgty.
          bapiret2-log_no     = <msg_handle>-log_handle.
          bapiret2-log_msg_no = <msg_handle>-msgnumber.
          bapiret2-message    = exception_msg.
          bapiret2-system     = sy-sysid.
          APPEND bapiret2 TO result.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_logger~free.
    " Save any messages (safety) only if an object has been defined
    IF me->header-object IS NOT INITIAL.
      zif_logger~save( ).
    ENDIF.

    " Clear log from memory
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING  i_log_handle  = handle
      EXCEPTIONS log_not_found = 1
                 OTHERS        = 2.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD zif_logger~has_errors.
    result = boolc( lines( get_message_handles( msgtype = 'E' ) ) > 0 ).
  ENDMETHOD.

  METHOD zif_logger~has_warnings.
    result = boolc( lines( get_message_handles( msgtype = 'W' ) ) > 0 ).
  ENDMETHOD.

  METHOD zif_logger~length.
    result = lines( get_message_handles( ) ).
  ENDMETHOD.

  METHOD zif_logger_bal~get_control_handle.
    result = control_handle.
  ENDMETHOD.

  METHOD zif_logger_bal~get_db_number.
    result = db_number.
  ENDMETHOD.

  METHOD zif_logger_bal~get_handle.
    result = handle.
  ENDMETHOD.

  METHOD zif_logger_bal~get_header.
    result = header.
  ENDMETHOD.

ENDCLASS.
