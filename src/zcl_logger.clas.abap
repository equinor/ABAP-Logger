"! <p class="shorttext synchronized">ABAP Logger</p>
CLASS zcl_logger DEFINITION
  PUBLIC ABSTRACT
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    INTERFACES zif_logger.
    INTERFACES zif_logger_log_object.

    METHODS constructor
      IMPORTING settings TYPE REF TO zif_logger_settings OPTIONAL.

  PROTECTED SECTION.
    DATA settings TYPE REF TO zif_logger_settings.

    METHODS add_exception_with_textid ABSTRACT
      IMPORTING exception_data TYPE bal_s_exc.

    METHODS add_message ABSTRACT
      IMPORTING application_log_message TYPE bal_s_msg.

    METHODS add_text_string ABSTRACT
      IMPORTING text_string       TYPE csequence
                message_type      TYPE symsgty    OPTIONAL
                importance        TYPE balprobcl  OPTIONAL
                log_context       TYPE bal_s_cont OPTIONAL
                log_parameters    TYPE bal_s_parm OPTIONAL
                detail_level      TYPE ballevel   OPTIONAL
                start_new_section TYPE xfeld      OPTIONAL.

    METHODS save_log ABSTRACT.

  PRIVATE SECTION.
    ALIASES add FOR zif_logger~add.

    TYPES: BEGIN OF ty_exception,
             level     TYPE i,
             exception TYPE REF TO cx_root,
           END OF ty_exception,
           tty_exception TYPE STANDARD TABLE OF ty_exception.

    TYPES tty_exception_data TYPE STANDARD TABLE OF bal_s_exc WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_struct_kind,
        syst               TYPE i VALUE 1,
        bapi               TYPE i VALUE 2,
        bdc                TYPE i VALUE 3,
        sprot              TYPE i VALUE 4,
        bapi_alm           TYPE i VALUE 5,
        bapi_meth          TYPE i VALUE 6,
        bapi_status_result TYPE i VALUE 7,
      END OF c_struct_kind.

    "! Safety limit for previous exception drill down
    "!
    "! @parameter exception  | Exception
    "! @parameter type       | Message type
    "! @parameter importance | Importance
    "! @parameter detlevel   | Detail level
    "! @parameter result     | Exception data
    METHODS drill_down_into_exception
      IMPORTING !exception    TYPE REF TO cx_root
                !type         TYPE symsgty   OPTIONAL
                importance    TYPE balprobcl OPTIONAL
                detlevel      TYPE ballevel  OPTIONAL
      RETURNING VALUE(result) TYPE tty_exception_data.

    METHODS add_structure
      IMPORTING obj_to_log    TYPE any       OPTIONAL
                !context      TYPE any       OPTIONAL
                callback_form TYPE csequence OPTIONAL
                callback_prog TYPE csequence OPTIONAL
                callback_fm   TYPE csequence OPTIONAL
                !type         TYPE symsgty   OPTIONAL
                importance    TYPE balprobcl OPTIONAL
                detlevel      TYPE ballevel  OPTIONAL
                  PREFERRED PARAMETER obj_to_log
      RETURNING VALUE(result)   TYPE REF TO zif_logger.

    METHODS get_struct_kind
      IMPORTING msg_type      TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(result) TYPE string.

    METHODS add_bapi_alm_msg
      IMPORTING obj_to_log          TYPE any
      RETURNING VALUE(result) TYPE bal_s_msg.

    METHODS add_bapi_meth_msg
      IMPORTING obj_to_log          TYPE any
      RETURNING VALUE(result) TYPE bal_s_msg.

    METHODS add_bapi_msg
      IMPORTING obj_to_log          TYPE any
      RETURNING VALUE(result) TYPE bal_s_msg.

    METHODS add_bapi_status_result
      IMPORTING obj_to_log          TYPE any
      RETURNING VALUE(result) TYPE bal_s_msg.

    METHODS add_bdc_msg
      IMPORTING obj_to_log          TYPE any
      RETURNING VALUE(result) TYPE bal_s_msg.

    METHODS add_exception
      IMPORTING exception_data    TYPE bal_s_exc
                formatted_context TYPE bal_s_cont
                formatted_params  TYPE bal_s_parm.

    METHODS add_sprot_msg
      IMPORTING obj_to_log          TYPE any
      RETURNING VALUE(result) TYPE bal_s_msg.

    METHODS add_syst_msg
      IMPORTING obj_to_log          TYPE any
      RETURNING VALUE(result) TYPE bal_s_msg.

ENDCLASS.


CLASS zcl_logger IMPLEMENTATION.
  METHOD add_bapi_alm_msg.
    DATA: " Avoid using concrete type as certain systems may not have BAPI_ALM_RETURN
      BEGIN OF bapi_alm_message,
        type           TYPE bapi_mtype,
        message_id     TYPE symsgid,
        message_number TYPE symsgno,
        message_v1     TYPE symsgv,
        message_v2     TYPE symsgv,
        message_v3     TYPE symsgv,
        message_v4     TYPE symsgv,
      END OF bapi_alm_message.

    MOVE-CORRESPONDING obj_to_log TO bapi_alm_message.
    result-msgty = bapi_alm_message-type.
    result-msgid = bapi_alm_message-message_id.
    result-msgno = bapi_alm_message-message_number.
    result-msgv1 = bapi_alm_message-message_v1.
    result-msgv2 = bapi_alm_message-message_v2.
    result-msgv3 = bapi_alm_message-message_v3.
    result-msgv4 = bapi_alm_message-message_v4.
  ENDMETHOD.

  METHOD add_bapi_meth_msg.
    DATA: " Avoid using concrete type as certain systems may not have BAPI_METH_MESSAGE
      BEGIN OF bapi_meth_message,
        method             TYPE c LENGTH 32, " bapi_method,
        object_type        TYPE c LENGTH 32, " obj_typ,
        internal_object_id TYPE c LENGTH 90, " objidint,
        external_object_id TYPE c LENGTH 90, " objidext,
        message_id         TYPE c LENGTH 20, " bapi_msgid,
        message_number     TYPE msgno,
        message_type       TYPE msgty,
        message_text       TYPE c LENGTH 72, " bapi_text,
      END OF bapi_meth_message.

    MOVE-CORRESPONDING obj_to_log TO bapi_meth_message.
    result-msgty = bapi_meth_message-message_type.
    result-msgid = bapi_meth_message-message_id.
    result-msgno = bapi_meth_message-message_number.
  ENDMETHOD.

  METHOD add_bapi_msg.
    DATA bapi_message TYPE bapiret1.

    MOVE-CORRESPONDING obj_to_log TO bapi_message.
    result-msgty = bapi_message-type.
    result-msgid = bapi_message-id.
    result-msgno = bapi_message-number.
    result-msgv1 = bapi_message-message_v1.
    result-msgv2 = bapi_message-message_v2.
    result-msgv3 = bapi_message-message_v3.
    result-msgv4 = bapi_message-message_v4.
  ENDMETHOD.

  METHOD add_bapi_status_result.
    DATA: " Avoid using concrete type as certain systems may not have BAPI_STATUS_RESULT
      BEGIN OF bapi_status_result,
        objectkey      TYPE c LENGTH 90, "  OBJIDEXT,
        status_action  TYPE c LENGTH 1,  "  BAPI_STATUS_ACTION,
        status_type    TYPE c LENGTH 6,  "  BAPI_STATUS_TYPE,
        message_id     TYPE c LENGTH 20, "  BAPI_MSGID,
        message_number TYPE c LENGTH 3,  "  MSGNO,
        message_type   TYPE c LENGTH 1,  "  MSGTY,
        message_text   TYPE c LENGTH 72, "  BAPI_TEXT,
      END OF bapi_status_result.

    MOVE-CORRESPONDING obj_to_log TO bapi_status_result.
    result-msgty = bapi_status_result-message_type.
    result-msgid = bapi_status_result-message_id.
    result-msgno = bapi_status_result-message_number.
  ENDMETHOD.

  METHOD add_bdc_msg.
    DATA bdc_message TYPE bdcmsgcoll.

    MOVE-CORRESPONDING obj_to_log TO bdc_message.
    result-msgty = bdc_message-msgtyp.
    result-msgid = bdc_message-msgid.
    result-msgno = bdc_message-msgnr.
    result-msgv1 = bdc_message-msgv1.
    result-msgv2 = bdc_message-msgv2.
    result-msgv3 = bdc_message-msgv3.
    result-msgv4 = bdc_message-msgv4.
  ENDMETHOD.

  METHOD add_exception.
    DATA detailed_msg         TYPE bal_s_msg.
    DATA l_t100key            TYPE scx_t100key.
    DATA l_inc                TYPE i.
    DATA l_textid             TYPE sotr_conc.
    DATA l_substitution_table TYPE sotr_params.

    FIELD-SYMBOLS <l_attr>         TYPE scx_t100key-attr1.
    FIELD-SYMBOLS <l_msgv>         TYPE bal_s_msg-msgv1.
    FIELD-SYMBOLS <l_substitution> TYPE sotr_param.

    " exception -> type OTR-message or T100-message?
    cl_message_helper=>check_msg_kind( EXPORTING msg     = exception_data-exception
                                       IMPORTING t100key = l_t100key
                                                 textid  = l_textid ).

    IF l_textid IS NOT INITIAL.
      " If it is a OTR-message
      add_exception_with_textid( exception_data ).
      RETURN.
    ENDIF.

    " get the parameter for text switching
    cl_message_helper=>get_text_params( EXPORTING obj    = exception_data-exception
                                        IMPORTING params = l_substitution_table ).

    " exception with T100 message
    detailed_msg-msgid = l_t100key-msgid.
    detailed_msg-msgno = l_t100key-msgno.

    DO 4 TIMES.
      l_inc = sy-index - 1.
      ASSIGN l_t100key-attr1 INCREMENT l_inc TO <l_attr> RANGE l_t100key.
      IF sy-subrc = 0 AND <l_attr> IS NOT INITIAL.
        READ TABLE l_substitution_table ASSIGNING <l_substitution> WITH KEY param = <l_attr>.
        IF sy-subrc = 0 AND <l_substitution>-value IS NOT INITIAL.
          ASSIGN detailed_msg-msgv1 INCREMENT l_inc TO <l_msgv> RANGE detailed_msg.
          IF sy-subrc = 0.
            <l_msgv> = <l_substitution>-value.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

    detailed_msg-msgty     = exception_data-msgty.
    detailed_msg-probclass = exception_data-probclass.
    detailed_msg-detlevel  = exception_data-detlevel.
    detailed_msg-time_stmp = exception_data-time_stmp.
    detailed_msg-alsort    = exception_data-alsort.
    detailed_msg-context   = formatted_context.
    detailed_msg-params    = formatted_params.

    add_message( detailed_msg ).
  ENDMETHOD.

  METHOD add_sprot_msg.
    DATA sprot_message TYPE sprot_u.

    MOVE-CORRESPONDING obj_to_log TO sprot_message.
    result-msgty = sprot_message-severity.
    result-msgid = sprot_message-ag.
    result-msgno = sprot_message-msgnr.
    result-msgv1 = sprot_message-var1.
    result-msgv2 = sprot_message-var2.
    result-msgv3 = sprot_message-var3.
    result-msgv4 = sprot_message-var4.
  ENDMETHOD.

  METHOD add_structure.
    DATA msg_type        TYPE REF TO cl_abap_typedescr.
    DATA msg_struct_type TYPE REF TO cl_abap_structdescr.
    DATA components      TYPE abap_compdescr_tab.
    DATA component       LIKE LINE OF components.
    DATA component_name  LIKE component-name.
    DATA string_to_log   TYPE string.

    FIELD-SYMBOLS <component> TYPE any.

    msg_struct_type ?= cl_abap_typedescr=>describe_by_data( obj_to_log ).
    components = msg_struct_type->components.
    add( '--- Begin of structure ---' ).
    LOOP AT components INTO component.
      component_name = component-name.
      ASSIGN COMPONENT component_name OF STRUCTURE obj_to_log TO <component>.
      IF sy-subrc <> 0.
        " It might be an unnamed component like .INCLUDE
        component_name = |Include { sy-tabix }|.
        ASSIGN COMPONENT sy-tabix OF STRUCTURE obj_to_log TO <component>.
      ENDIF.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      msg_type = cl_abap_typedescr=>describe_by_data( <component> ).
      IF msg_type->kind = cl_abap_typedescr=>kind_elem.
        string_to_log = |{ to_lower( component_name ) } = { <component> }|.
        add( string_to_log ).
      ELSEIF msg_type->kind = cl_abap_typedescr=>kind_struct.
        result = add_structure( obj_to_log    = <component>
                              context       = context
                              callback_form = callback_form
                              callback_prog = callback_prog
                              callback_fm   = callback_fm
                              type          = type
                              importance    = importance
                              detlevel      = detlevel ).
      ENDIF.
    ENDLOOP.
    add( '--- End of structure ---' ).
  ENDMETHOD.

  METHOD add_syst_msg.
    DATA syst_message TYPE symsg.

    MOVE-CORRESPONDING obj_to_log TO syst_message.
    MOVE-CORRESPONDING syst_message TO result.
  ENDMETHOD.

  METHOD constructor.
    IF settings IS BOUND AND settings IS NOT INITIAL.
      me->settings = settings.
    ELSE.
      me->settings = zcl_logger_factory=>create_settings( ).
    ENDIF.
  ENDMETHOD.

  METHOD drill_down_into_exception.
    DATA i                  TYPE i VALUE 2.
    DATA previous_exception TYPE REF TO cx_root.
    DATA exceptions         TYPE tty_exception.

    FIELD-SYMBOLS <ex>  LIKE LINE OF exceptions.
    FIELD-SYMBOLS <ret> LIKE LINE OF result.

    APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
    <ex>-level     = 1.
    <ex>-exception = exception.

    previous_exception = exception.

    WHILE i <= settings->get_max_exception_drill_down( ).
      IF previous_exception->previous IS NOT BOUND.
        EXIT.
      ENDIF.

      previous_exception ?= previous_exception->previous.

      APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
      <ex>-level     = i.
      <ex>-exception = previous_exception.
      i              = i + 1.
    ENDWHILE.

    " Display the deepest exception first
    SORT exceptions BY level DESCENDING.
    LOOP AT exceptions ASSIGNING <ex>.
      APPEND INITIAL LINE TO result ASSIGNING <ret>.
      <ret>-exception = <ex>-exception.
      <ret>-msgty     = type.
      <ret>-probclass = importance.
      <ret>-detlevel  = detlevel.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_struct_kind.
    DATA msg_struct_kind   TYPE REF TO cl_abap_structdescr.
    DATA components        TYPE abap_compdescr_tab.
    DATA component         LIKE LINE OF components.
    DATA syst_count        TYPE i.
    DATA bapi_count        TYPE i.
    DATA bdc_count         TYPE i.
    DATA sprot_count       TYPE i.
    DATA bapi_alm_count    TYPE i.
    DATA bapi_meth_count   TYPE i.
    DATA bapi_status_count TYPE i.

    IF NOT (    msg_type->type_kind = cl_abap_typedescr=>typekind_struct1
             OR msg_type->type_kind = cl_abap_typedescr=>typekind_struct2 ).
      RETURN.
    ENDIF.

    msg_struct_kind ?= msg_type.
    components = msg_struct_kind->components.

    " Count number of fields expected for each supported type of message structure
    LOOP AT components INTO component.
      IF 'MSGTY,MSGID,MSGNO,MSGV1,MSGV2,MSGV3,MSGV4,' CS |{ component-name },|.
        syst_count = syst_count + 1.
      ENDIF.
      IF 'TYPE,NUMBER,ID,MESSAGE_V1,MESSAGE_V2,MESSAGE_V3,MESSAGE_V4,' CS |{ component-name },|.
        bapi_count = bapi_count + 1.
      ENDIF.
      IF 'MSGTYP,MSGID,MSGNR,MSGV1,MSGV2,MSGV3,MSGV4,' CS |{ component-name },|.
        bdc_count = bdc_count + 1.
      ENDIF.
      IF 'SEVERITY,AG,MSGNR,VAR1,VAR2,VAR3,VAR4,' CS |{ component-name },|.
        sprot_count = sprot_count + 1.
      ENDIF.
      IF 'TYPE,MESSAGE_ID,MESSAGE_NUMBER,MESSAGE_V1,MESSAGE_V2,MESSAGE_V3,MESSAGE_V4,' CS |{ component-name },|.
        bapi_alm_count = bapi_alm_count + 1.
      ENDIF.
      IF 'METHOD,OBJECT_TYPE,INTERNAL_OBJECT_ID,EXTERNAL_OBJECT_ID,MESSAGE_ID,MESSAGE_NUMBER,MESSAGE_TYPE,MESSAGE_TEXT,' CS |{ component-name },|.
        bapi_meth_count = bapi_meth_count + 1.
      ENDIF.
      IF 'OBJECTKEY,STATUS_ACTION,STATUS_TYPE,MESSAGE_ID,MESSAGE_NUMBER,MESSAGE_TYPE,MESSAGE_TEXT,' CS |{ component-name },|.
        bapi_status_count = bapi_status_count + 1.
      ENDIF.
    ENDLOOP.

    " Set message type if all expected fields are present
    IF syst_count = 7.
      result = c_struct_kind-syst.
    ELSEIF bapi_count = 7.
      result = c_struct_kind-bapi.
    ELSEIF bdc_count = 7.
      result = c_struct_kind-bdc.
    ELSEIF sprot_count = 7.
      result = c_struct_kind-sprot.
    ELSEIF bapi_alm_count = 7.
      result = c_struct_kind-bapi_alm.
    ELSEIF bapi_meth_count = 8.
      result = c_struct_kind-bapi_meth.
    ELSEIF bapi_status_count = 7.
      result = c_struct_kind-bapi_status_result.
    ENDIF.
  ENDMETHOD.

  METHOD zif_logger~abend.
    result = add( obj_to_log          = obj_to_log
                  context             = context
                  callback_form       = callback_form
                  callback_prog       = callback_prog
                  callback_fm         = callback_fm
                  callback_parameters = callback_parameters
                  type                = 'A'
                  importance          = importance
                  detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger~add.
    DATA detailed_msg             TYPE bal_s_msg.
    DATA exception_data_table     TYPE tty_exception_data.
    DATA free_text_msg            TYPE char200.
    DATA ctx_type                 TYPE REF TO cl_abap_typedescr.
    DATA ctx_ddic_header          TYPE x030l.
    DATA msg_type                 TYPE REF TO cl_abap_typedescr.
    DATA struct_kind              TYPE i.
    DATA formatted_context        TYPE bal_s_cont.
    DATA formatted_params         TYPE bal_s_parm.
    DATA message_type             TYPE symsgty.
    " these objects could be moved into their own method
    " see adt://***/sap/bc/adt/oo/classes/zcl_logger/source/main#start=391,10;end=415,61
    DATA symsg                    TYPE symsg.
    DATA loggable                 TYPE REF TO zif_logger_log_object.
    DATA loggable_object_messages TYPE zif_logger_log_object=>tty_messages.

    FIELD-SYMBOLS <table_of_messages>       TYPE ANY TABLE.
    FIELD-SYMBOLS <message_line>            TYPE any.
    FIELD-SYMBOLS <context_val>             TYPE any.
    FIELD-SYMBOLS <loggable_object_message> TYPE zif_logger_log_object=>ty_message.

    " Remember system message since it might get changed inadvertently
    IF context IS NOT INITIAL.
      ASSIGN context TO <context_val>.
      formatted_context-value = <context_val>.
      ctx_type                = cl_abap_typedescr=>describe_by_data( context ).

      ctx_type->get_ddic_header( RECEIVING  p_header     = ctx_ddic_header
                                 EXCEPTIONS not_found    = 1
                                            no_ddic_type = 2
                                            OTHERS       = 3 ).
      IF sy-subrc = 0.
        formatted_context-tabname = ctx_ddic_header-tabname.
      ENDIF.
    ENDIF.

    IF callback_fm IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_fm.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = 'F'.
      formatted_params-t_par              = callback_parameters.
    ELSEIF callback_form IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_form.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = ' '.
      formatted_params-t_par              = callback_parameters.
    ENDIF.

    msg_type    = cl_abap_typedescr=>describe_by_data( obj_to_log ).
    struct_kind = get_struct_kind( msg_type ).

    IF struct_kind = c_struct_kind-syst.
      detailed_msg = add_syst_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-bapi.
      detailed_msg = add_bapi_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-bdc.
      detailed_msg = add_bdc_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-sprot.
      detailed_msg = add_sprot_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-bapi_alm.
      detailed_msg = add_bapi_alm_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-bapi_meth.
      detailed_msg = add_bapi_meth_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-bapi_status_result.
      detailed_msg = add_bapi_status_result( obj_to_log ).
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      TRY.
          " BEGIN this could/should be moved into its own method
          loggable ?= obj_to_log.
          loggable_object_messages = loggable->get_message_table( ).
          LOOP AT loggable_object_messages ASSIGNING <loggable_object_message>.
            IF <loggable_object_message>-symsg IS NOT INITIAL.
              MOVE-CORRESPONDING <loggable_object_message>-symsg TO symsg.
              symsg-msgty = <loggable_object_message>-type.
              zif_logger~add( obj_to_log = symsg
                              context    = context
                              importance = importance
                              detlevel   = detlevel ).
            ENDIF.
            IF <loggable_object_message>-exception IS BOUND.
              zif_logger~add( type       = <loggable_object_message>-type
                              obj_to_log = <loggable_object_message>-exception
                              context    = context
                              importance = importance
                              detlevel   = detlevel ).
            ENDIF.
            IF <loggable_object_message>-string IS NOT INITIAL.
              zif_logger~add( type       = <loggable_object_message>-type
                              obj_to_log = <loggable_object_message>-string
                              context    = context
                              importance = importance
                              detlevel   = detlevel ).
            ENDIF.
          ENDLOOP.
          " END this could/should be moved into its own method

        CATCH cx_sy_move_cast_error.
          IF type IS INITIAL.
            message_type = if_msg_output=>msgtype_error.
          ELSE.
            message_type = type.
          ENDIF.
          exception_data_table = drill_down_into_exception( exception  = obj_to_log
                                                            type       = message_type
                                                            importance = importance
                                                            detlevel   = detlevel ).
      ENDTRY.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN obj_to_log TO <table_of_messages>.
      LOOP AT <table_of_messages> ASSIGNING <message_line>.
        IF sy-tabix = 1.
          zif_logger~add( obj_to_log = <message_line>
                          context    = context
                          importance = importance
                          type       = type
                          detlevel   = detlevel ).
        ELSE.
          zif_logger~add( obj_to_log = <message_line>
                          importance = importance
                          type       = type
                          detlevel   = detlevel ).
        ENDIF.
      ENDLOOP.
    ELSEIF    msg_type->type_kind = cl_abap_typedescr=>typekind_struct1     " flat structure
           OR msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.    " deep structure (already when string is used)
      result = add_structure( obj_to_log    = obj_to_log
                              context       = context
                              callback_form = callback_form
                              callback_prog = callback_prog
                              callback_fm   = callback_fm
                              type          = type
                              importance    = importance
                              detlevel      = detlevel ).
    ELSE.
      free_text_msg = obj_to_log.
    ENDIF.

    IF free_text_msg IS NOT INITIAL.
      message_type = type.
      IF message_type IS INITIAL.
        message_type = if_msg_output=>msgtype_success.
      ENDIF.
      add_text_string( text_string       = free_text_msg
                       message_type      = message_type
                       importance        = importance
                       log_context       = formatted_context
                       log_parameters    = formatted_params
                       detail_level      = detlevel
                       start_new_section = start_new_section ).
    ELSEIF exception_data_table IS NOT INITIAL.
      FIELD-SYMBOLS <exception_data> LIKE LINE OF exception_data_table.
      LOOP AT exception_data_table ASSIGNING <exception_data>.
        add_exception( exception_data    = <exception_data>
                       formatted_context = formatted_context
                       formatted_params  = formatted_params ).
      ENDLOOP.
    ELSEIF detailed_msg IS NOT INITIAL.
      detailed_msg-context   = formatted_context.
      detailed_msg-params    = formatted_params.
      detailed_msg-probclass = importance.
      detailed_msg-detlevel  = detlevel.
      IF type IS NOT INITIAL.
        detailed_msg-msgty = type.
      ENDIF.

      add_message( detailed_msg ).
    ENDIF.

    IF me->settings->get_autosave( ) = abap_true.
      save_log( ).
    ENDIF.
    result = me.
  ENDMETHOD.

  METHOD zif_logger~debug.
    result = add( obj_to_log          = obj_to_log
                  context             = context
                  callback_form       = callback_form
                  callback_prog       = callback_prog
                  callback_fm         = callback_fm
                  callback_parameters = callback_parameters
                  type                = ' '
                  importance          = importance
                  detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger~error.
    result = add( obj_to_log          = obj_to_log
                  context             = context
                  callback_form       = callback_form
                  callback_prog       = callback_prog
                  callback_fm         = callback_fm
                  callback_parameters = callback_parameters
                  type                = 'E'
                  importance          = importance
                  detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger~exit.
    result = add( obj_to_log          = obj_to_log
                  context             = context
                  callback_form       = callback_form
                  callback_prog       = callback_prog
                  callback_fm         = callback_fm
                  callback_parameters = callback_parameters
                  type                = 'X'
                  importance          = importance
                  detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger~export_to_table.
  ENDMETHOD.

  METHOD zif_logger~free.
  ENDMETHOD.

  METHOD zif_logger~has_errors.
  ENDMETHOD.

  METHOD zif_logger~has_warnings.
  ENDMETHOD.

  METHOD zif_logger~info.
    result = add( obj_to_log          = obj_to_log
                  context             = context
                  callback_form       = callback_form
                  callback_prog       = callback_prog
                  callback_fm         = callback_fm
                  callback_parameters = callback_parameters
                  type                = 'I'
                  importance          = importance
                  detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger~is_empty.
    result = boolc( zif_logger~length( ) = 0 ).
  ENDMETHOD.

  METHOD zif_logger~length.
  ENDMETHOD.

  METHOD zif_logger~save.
    CHECK settings->get_autosave( ) = abap_false.
    save_log( ).
  ENDMETHOD.

  METHOD zif_logger~success.
    result = add( obj_to_log          = obj_to_log
                  context             = context
                  callback_form       = callback_form
                  callback_prog       = callback_prog
                  callback_fm         = callback_fm
                  callback_parameters = callback_parameters
                  type                = 'S'
                  importance          = importance
                  detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger~warning.
    result = add( obj_to_log          = obj_to_log
                  context             = context
                  callback_form       = callback_form
                  callback_prog       = callback_prog
                  callback_fm         = callback_fm
                  callback_parameters = callback_parameters
                  type                = 'W'
                  importance          = importance
                  detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger~set_header.
  ENDMETHOD.

  METHOD zif_logger_deprecated~a.
    result = zif_logger~abend( obj_to_log          = obj_to_log
                               context             = context
                               callback_form       = callback_form
                               callback_prog       = callback_prog
                               callback_fm         = callback_fm
                               callback_parameters = callback_parameters
                               importance          = importance
                               detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger_deprecated~e.
    result = zif_logger~error( obj_to_log          = obj_to_log
                               context             = context
                               callback_form       = callback_form
                               callback_prog       = callback_prog
                               callback_fm         = callback_fm
                               callback_parameters = callback_parameters
                               importance          = importance
                               detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger_deprecated~i.
    result = zif_logger~info( obj_to_log          = obj_to_log
                              context             = context
                              callback_form       = callback_form
                              callback_prog       = callback_prog
                              callback_fm         = callback_fm
                              callback_parameters = callback_parameters
                              importance          = importance
                              detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger_deprecated~s.
    result = zif_logger~success( obj_to_log          = obj_to_log
                                 context             = context
                                 callback_form       = callback_form
                                 callback_prog       = callback_prog
                                 callback_fm         = callback_fm
                                 callback_parameters = callback_parameters
                                 importance          = importance
                                 detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger_deprecated~w.
    result = zif_logger~warning( obj_to_log          = obj_to_log
                                 context             = context
                                 callback_form       = callback_form
                                 callback_prog       = callback_prog
                                 callback_fm         = callback_fm
                                 callback_parameters = callback_parameters
                                 importance          = importance
                                 detlevel            = detlevel ).
  ENDMETHOD.

  METHOD zif_logger_log_object~get_message_table.
  ENDMETHOD.
ENDCLASS.
