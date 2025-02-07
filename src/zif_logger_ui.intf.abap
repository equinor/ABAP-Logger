INTERFACE zif_logger_ui
  PUBLIC.
  METHODS display_fullscreen
    IMPORTING profile TYPE bal_s_prof OPTIONAL.

  METHODS display_as_popup
    IMPORTING profile TYPE bal_s_prof OPTIONAL.

  METHODS display_in_container
    IMPORTING container TYPE REF TO cl_gui_container
              profile   TYPE bal_s_prof OPTIONAL.
ENDINTERFACE.
