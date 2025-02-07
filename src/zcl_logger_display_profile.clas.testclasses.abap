*"* use this source file for your ABAP unit test classes
CLASS lcl_test DEFINITION DEFERRED.

CLASS zcl_logger_display_profile DEFINITION LOCAL FRIENDS lcl_test.

CLASS lcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_logger_display_profile.

    CLASS-DATA profile_single_log             TYPE bal_s_prof.

    CLASS-METHODS class_setup.

    METHODS setup.
    METHODS teardown.

    METHODS set_predefined_profile FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD class_setup.
    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING e_s_display_profile = profile_single_log.
  ENDMETHOD.

  METHOD setup.
    CREATE OBJECT cut.
  ENDMETHOD.

  METHOD teardown.
    CLEAR cut.
  ENDMETHOD.

  METHOD set_predefined_profile.
    DATA display_profile TYPE bal_s_prof.

    " given
    cut->zif_logger_display_profile~set_predefined_profile( zif_logger_display_profile=>application_log_profile-single_log ).

    " when
    display_profile = cut->zif_logger_display_profile~get( ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = profile_single_log
                                        act = display_profile ).
  ENDMETHOD.
ENDCLASS.
