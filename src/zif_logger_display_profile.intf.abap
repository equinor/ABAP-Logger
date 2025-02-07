INTERFACE zif_logger_display_profile
  PUBLIC.
  CONSTANTS:
    BEGIN OF application_log_profile,
      multiple_logs                TYPE i VALUE 0,
      single_log                   TYPE i VALUE 1,
      fullscreen_without_hierarchy TYPE i VALUE 2,
      popup_without_hierarchy      TYPE i VALUE 3,
      hierarchy_by_detail_level    TYPE i VALUE 4,
    END OF application_log_profile.

  METHODS get
    RETURNING VALUE(result) TYPE bal_s_prof.

  METHODS set_predefined_profile
    IMPORTING profile_type  TYPE i
    RETURNING VALUE(result) TYPE REF TO zif_logger_display_profile.

  METHODS set_grid
    IMPORTING grid_mode     TYPE clike
    RETURNING VALUE(result) TYPE REF TO zif_logger_display_profile.

  METHODS set_value
    IMPORTING !field        TYPE clike
              !value        TYPE any
    RETURNING VALUE(result) TYPE REF TO zif_logger_display_profile.

  METHODS set_context_tree
    IMPORTING context_structure TYPE clike
              under_log         TYPE clike DEFAULT space
    RETURNING VALUE(result)     TYPE REF TO zif_logger_display_profile.

  METHODS set_context_message
    IMPORTING context_structure TYPE clike
    RETURNING VALUE(result)       TYPE REF TO zif_logger_display_profile.
ENDINTERFACE.
