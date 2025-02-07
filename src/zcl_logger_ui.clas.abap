CLASS zcl_logger_ui DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_logger_ui.
ENDCLASS.


CLASS zcl_logger_ui IMPLEMENTATION.
  METHOD zif_logger_ui~display_as_popup.
    " Currently implemented in ZCL_LOGGER_SBAL
  ENDMETHOD.

  METHOD zif_logger_ui~display_fullscreen.
    " Currently implemented in ZCL_LOGGER_SBAL
  ENDMETHOD.

  METHOD zif_logger_ui~display_in_container.
    " Currently implemented in ZCL_LOGGER_SBAL
  ENDMETHOD.
ENDCLASS.
