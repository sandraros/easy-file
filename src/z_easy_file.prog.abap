*&---------------------------------------------------------------------*
*& Report z_easy_file
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_easy_file.

TYPE-POOLS abap.
TYPES:BEGIN OF ty_us_config,
        xpgmove TYPE string,
        xpgmov3 TYPE string,
        xpgcopy TYPE string,
        xpgcpy3 TYPE string,
        xpgmkdi TYPE string,
        xpgrmdi TYPE string,
      END OF ty_us_config.

TABLES sscrfields.

*
CLASS lcl_user_settings DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS add_menu IMPORTING io_ctmenu TYPE REF TO cl_ctmenu.
    CLASS-METHODS load_variant_and_file.
    CLASS-METHODS load_variant.
    CLASS-METHODS load_file.
    CLASS-METHODS save_variant.
    CLASS-METHODS save_file IMPORTING i_file TYPE csequence.
ENDCLASS."

*
CLASS lcl_dynp_values DEFINITION.
  PUBLIC SECTION.
    METHODS add_field
      IMPORTING
        name  TYPE csequence
        stepl TYPE dynpread-stepl DEFAULT 0
      CHANGING
        value TYPE simple.
    METHODS read.
    METHODS update.
    CLASS-METHODS read_single_field
      IMPORTING
        name  TYPE csequence
        stepl TYPE dynpread-stepl DEFAULT 0.
    CLASS-METHODS update_single_field
      IMPORTING
        name  TYPE csequence
        stepl TYPE dynpread-stepl DEFAULT 0
        value TYPE simple.
  PRIVATE SECTION.
    TYPES : BEGIN OF ty_is_field,
              name  TYPE dynpread-fieldname,
              stepl TYPE dynpread-stepl,
              value TYPE REF TO data,
            END OF ty_is_field.
    DATA: ait_field TYPE TABLE OF ty_is_field.
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_gui_alv_tree DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_gui_alv_tree DEFINITION INHERITING FROM cl_gui_alv_tree.
  PROTECTED SECTION.
    METHODS set_children_at_front REDEFINITION.
ENDCLASS.                    "lcl_gui_alv_tree DEFINITION

*
CLASS lcl_ctmenu DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS deserialize_menu
      IMPORTING
        io_ctmenu TYPE REF TO cl_ctmenu
        it_menu   TYPE sctx_serialize.
  PRIVATE SECTION.
    CLASS-METHODS _deserialize_menu
      IMPORTING
        io_ctmenu TYPE REF TO cl_ctmenu
        it_menu   TYPE sctx_serialize
        i_level   TYPE i
      CHANGING
        c_tabix   TYPE sytabix.
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcx_freetext DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_freetext DEFINITION INHERITING FROM cx_no_check.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor
      IMPORTING
        text     TYPE clike
        previous TYPE REF TO cx_root OPTIONAL.
    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.
ENDCLASS."

*----------------------------------------------------------------------*
*       INTERFACE lif_sscr
*----------------------------------------------------------------------*
INTERFACE lif_sscr.
  DATA sscr TYPE sydynnr.
  METHODS pbo.
  METHODS pai.
  METHODS exit.
ENDINTERFACE."


*----------------------------------------------------------------------*
*       INTERFACE lif_sscr_h
*----------------------------------------------------------------------*
INTERFACE lif_sscr_h.
  INTERFACES lif_sscr.
  TYPES:
    BEGIN OF ty_s_sscr,
      sscr      TYPE sydynnr,
      o_handler TYPE REF TO lif_sscr,
    END OF ty_s_sscr.
  DATA at_sscr TYPE TABLE OF ty_s_sscr.
  METHODS set_sscr_handler
    IMPORTING
      sscr    TYPE sydynnr
      handler TYPE REF TO lif_sscr.
ENDINTERFACE."

CLASS lcl_dof DEFINITION DEFERRED.
*----------------------------------------------------------------------*
*       INTERFACE lif_file_system DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_file_system.

  DATA type(1) TYPE c.
  TYPES : BEGIN OF ty_us_splitted_full_path,
            name TYPE string,
            path TYPE string,
          END OF ty_us_splitted_full_path.

  METHODS get_file_sep
    RETURNING
      VALUE(file_sep) TYPE char1.

  METHODS directory_list_files
    IMPORTING
      path                 TYPE csequence
      depth                TYPE numeric DEFAULT 0
      filter               TYPE csequence OPTIONAL
      only_first_directory TYPE abap_bool DEFAULT abap_false
      only_directories     TYPE abap_bool DEFAULT abap_false
    EXPORTING
      files                TYPE REF TO data.

  METHODS normalize_dir_path
    IMPORTING
      path          TYPE csequence
    RETURNING
      VALUE(result) TYPE string.

  METHODS build_file_path_long
    IMPORTING
      start_path TYPE csequence
      dir_name   TYPE csequence
    EXPORTING
      dir_path   TYPE csequence.

  METHODS file_read_binary
    IMPORTING
      fullpath TYPE csequence
    EXPORTING
      content  TYPE xstring.

  METHODS file_write_binary
    IMPORTING
      fullpath TYPE csequence
      content  TYPE xstring.

  METHODS file_read_text
    IMPORTING
      fullpath TYPE csequence
    EXPORTING
      content  TYPE string_table.

  METHODS file_write_text
    IMPORTING
      fullpath TYPE csequence
      content  TYPE string_table.

  METHODS file_exist
    IMPORTING
      fullpath      TYPE csequence
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS directory_exist
    IMPORTING
      fullpath      TYPE csequence
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS file_move
    IMPORTING
      source    TYPE csequence
      target    TYPE csequence
      overwrite TYPE abap_bool DEFAULT abap_false
      binary    TYPE abap_bool DEFAULT abap_false.

  METHODS file_copy
    IMPORTING
      source    TYPE csequence
      target    TYPE csequence
      overwrite TYPE abap_bool DEFAULT abap_false
      binary    TYPE abap_bool DEFAULT abap_false.

  METHODS file_create
    IMPORTING
      fullpath TYPE csequence.

  METHODS directory_create
    IMPORTING
      fullpath TYPE csequence.

  METHODS file_delete
    IMPORTING
      fullpath TYPE csequence.

  METHODS directory_delete
    IMPORTING
      fullpath TYPE csequence.

  METHODS file_rename
    IMPORTING
      fullpath     TYPE csequence
      new_filename TYPE csequence.

  METHODS directory_rename
    IMPORTING
      fullpath    TYPE csequence
      new_dirname TYPE csequence.

  METHODS execute
    IMPORTING
      fullpath TYPE csequence.

  METHODS open_with
    IMPORTING
      fullpath TYPE csequence.

  EVENTS dof_added
    EXPORTING
      VALUE(fullpath) TYPE string.

  EVENTS dof_removed
    EXPORTING
      VALUE(fullpath) TYPE string.

  EVENTS file_changed
    EXPORTING
      VALUE(fullpath) TYPE string.

  METHODS get_full_path
    IMPORTING
      name             TYPE csequence
      path             TYPE csequence
    RETURNING
      VALUE(full_path) TYPE string.

  METHODS split_full_path
    IMPORTING
      full_path     TYPE csequence
    RETURNING
      VALUE(result) TYPE ty_us_splitted_full_path.

ENDINTERFACE."


*----------------------------------------------------------------------*
*       CLASS lcl_dof DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_dof DEFINITION.
  PUBLIC SECTION.
    DATA: file_system TYPE REF TO lif_file_system READ-ONLY,
          fullpath    TYPE string READ-ONLY,
          path        TYPE string READ-ONLY,
          filename    TYPE string READ-ONLY,
          dirname     TYPE string READ-ONLY,
          isdir       TYPE int1 READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        file_system TYPE REF TO lif_file_system
        path        TYPE csequence OPTIONAL
        filename    TYPE csequence OPTIONAL
        fullpath    TYPE csequence OPTIONAL
        isdir       TYPE int1 OPTIONAL
      RETURNING
        VALUE(dof)  TYPE REF TO lcl_dof.

    METHODS constructor
      IMPORTING
        file_system TYPE REF TO lif_file_system
        path        TYPE csequence OPTIONAL
        filename    TYPE csequence OPTIONAL
        fullpath    TYPE csequence OPTIONAL
        isdir       TYPE int1 OPTIONAL.

    METHODS dof_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS delete.

    METHODS rename
      IMPORTING
        new_name TYPE csequence.

    METHODS execute.

    METHODS open_with.

    METHODS file_copy
      IMPORTING
        to        TYPE REF TO lcl_dof
        overwrite TYPE abap_bool DEFAULT abap_false.

    METHODS read
      EXPORTING
        content TYPE xstring.

    METHODS get_file
      IMPORTING
        filename   TYPE csequence
      RETURNING
        VALUE(dof) TYPE REF TO lcl_dof.

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_dofs DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_dofs DEFINITION.
  PUBLIC SECTION.
    TYPES:
          ty_uto_dof    TYPE TABLE OF REF TO lcl_dof.
    DATA: auto_dof      TYPE ty_uto_dof READ-ONLY.

    METHODS constructor
      IMPORTING
        dofs TYPE ty_uto_dof.
ENDCLASS."


*----------------------------------------------------------------------*
*       INTERFACE lif_file_event DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_file_event.

  DATA sender_control TYPE REF TO cl_gui_control.

  EVENTS dof_open_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_open_with_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_move_requested
        EXPORTING
          VALUE(source) TYPE REF TO lcl_dofs
          VALUE(target) TYPE REF TO lcl_dof.

  EVENTS dof_copy_requested
        EXPORTING
          VALUE(source) TYPE REF TO lcl_dofs
          VALUE(target) TYPE REF TO lcl_dof.

  EVENTS new_file_requested
        EXPORTING
          VALUE(dof) TYPE REF TO lcl_dof.

  EVENTS new_folder_requested
        EXPORTING
          VALUE(dof) TYPE REF TO lcl_dof.

  EVENTS dof_delete_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_rename_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_copy_to_clpb_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_cut_to_clpb_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_paste_from_clpb_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_copy_path_2_clpb_requested
        EXPORTING
          VALUE(dofs) TYPE REF TO lcl_dofs.

  EVENTS dof_custom_action_requested
        EXPORTING
          VALUE(ucomm) TYPE syucomm
          VALUE(dofs) TYPE REF TO lcl_dofs.

ENDINTERFACE."



*
CLASS lcl_gui_toolbar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      add_toolbar_to_container
        IMPORTING
          io_parent           TYPE REF TO cl_gui_container
        EXPORTING
          eo_splitter         TYPE REF TO cl_gui_splitter_container
          eo_toolbar          TYPE REF TO cl_gui_toolbar
          eo_bottom_container TYPE REF TO cl_gui_container.
ENDCLASS."

*----------------------------------------------------------------------*
*       INTERFACE lif_cross_action DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_cross_action.
  DATA: auo_clipboard_dofs     TYPE REF TO lcl_dofs,
        au_clipboard_operation TYPE i.
  CONSTANTS:
    BEGIN OF cs_clpb_operation,
      copy TYPE i VALUE 1,
      cut  TYPE i VALUE 2,
    END OF cs_clpb_operation.

*  METHODS:
*        start_confirm_dialog
*                            IMPORTING
*                              handler       TYPE REF TO lif_sscr.

ENDINTERFACE."


CLASS lcl_appserv DEFINITION DEFERRED.
*----------------------------------------------------------------------*
*       CLASS lcl_f4_as_dirs DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_f4_as_dirs DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        parent     TYPE REF TO cl_gui_container
        start_path TYPE csequence.

    METHODS get_selected_directory
      RETURNING VALUE(dir) TYPE string.

    METHODS free.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_is_dir_node,
        fullpath TYPE string,
        node_key TYPE lvc_nkey,
      END OF ty_is_dir_node.
    DATA: ai_node_number  TYPE i,
          aio_parent      TYPE REF TO cl_gui_container,
          aio_splitter    TYPE REF TO cl_gui_splitter_container,
          aio_bottom      TYPE REF TO cl_gui_container,
          aio_toolbar     TYPE REF TO cl_gui_toolbar,
          auo_tree        TYPE REF TO cl_gui_simple_tree,
          aio_file_system TYPE REF TO lcl_appserv,
          airt_dir        TYPE REF TO data,
          ait_dir_node    TYPE TABLE OF ty_is_dir_node.


    METHODS create_tree_control
      RETURNING
        VALUE(ro_tree) TYPE REF TO cl_gui_simple_tree.

    METHODS add_nodes
      IMPORTING
        dirs        TYPE REF TO data
        parent_node TYPE lvc_nkey
        path        TYPE string.

    METHODS on_expand
      FOR EVENT expand_no_children
          OF cl_tree_control_base
      IMPORTING
          node_key.

    METHODS on_button_pressed
      FOR EVENT function_selected
          OF cl_gui_toolbar
      IMPORTING
          fcode.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_initial_screen DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_initial_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_config_screen DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_config_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
    CLASS-DATA ku_current_slset TYPE syslset.
    CLASS-DATA ku_save_slset TYPE syslset.
ENDCLASS."
*
CLASS lcl_config_unix_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
ENDCLASS."
*
CLASS lcl_config_windows_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
ENDCLASS."
*
CLASS lcl_config_icon_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
ENDCLASS."
*
CLASS lcl_config_prog_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
ENDCLASS."
*
CLASS lcl_config_misc_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
ENDCLASS."
*
CLASS lcl_config_user_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
*  PRIVATE SECTION.
*    DATA: ai_first_pbo_done TYPE abap_bool.
ENDCLASS."


CLASS lcl_ui_file_manager DEFINITION DEFERRED.
*----------------------------------------------------------------------*
*       CLASS lcl_main_screen DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_main_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
    INTERFACES lif_cross_action.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_is_container_fileman,
        o_container TYPE REF TO cl_gui_container,
        o_fileman   TYPE REF TO lcl_ui_file_manager,
      END OF ty_is_container_fileman.
    DATA: aio_splitter          TYPE REF TO cl_gui_splitter_container,
          ait_container_fileman TYPE TABLE OF ty_is_container_fileman.
    METHODS test_zip.
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_popup_confirm DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_popup_confirm DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_us_confirm,
        selected(1)      TYPE c, "checkbox
        message          TYPE string,
        button(40)       TYPE c,
        binary(1)        TYPE c, "checkbox
        original_name    TYPE string,
        new_name         TYPE string,
        overwritten_file TYPE string,
        overwrite        TYPE abap_bool,
        t_style          TYPE lvc_t_styl,
        o_source_dof     TYPE REF TO lcl_dof,
        o_target_dof     TYPE REF TO lcl_dof,
        target_dirpath   TYPE string,
        done             TYPE abap_bool,
      END OF ty_us_confirm,

      ty_ut_confirm TYPE STANDARD TABLE OF ty_us_confirm,

      BEGIN OF ty_us_result,
        error   TYPE abap_bool,
        message TYPE string,
      END OF ty_us_result.

    CONSTANTS:
      BEGIN OF cs_operation,
        copy   TYPE i VALUE 1,
        rename TYPE i VALUE 2,
        move   TYPE i VALUE 3,
        delete TYPE i VALUE 4,
      END OF cs_operation.

    INTERFACES lif_sscr.

    METHODS set_data
      IMPORTING
        it_confirm   TYPE ty_ut_confirm
      RETURNING
        VALUE(error) TYPE abap_bool.

    METHODS adjust_field_catalog ABSTRACT
      EXPORTING
        et_field TYPE lvc_t_fnam
      CHANGING
        ct_fcat  TYPE lvc_t_fcat.

    METHODS adjust_buttons ABSTRACT
      CHANGING
        ct_fcode TYPE ui_functions.

    METHODS check_files
      RETURNING
        VALUE(error) TYPE abap_bool.

    METHODS on_toolbar
      FOR EVENT toolbar
          OF cl_gui_alv_grid
      IMPORTING
          e_object
          e_interactive.

    METHODS on_button_click
      FOR EVENT button_click
          OF cl_gui_alv_grid
      IMPORTING
          es_col_id
          es_row_no.

    METHODS check_file ABSTRACT
      EXPORTING
        error      TYPE abap_bool
      CHANGING
        cs_confirm TYPE ty_us_confirm.

    METHODS skip_dialog
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS before_operation ABSTRACT
      CHANGING
        cs_confirm TYPE ty_us_confirm.

    METHODS execute_operation ABSTRACT
      CHANGING
        cs_confirm TYPE ty_us_confirm.

    METHODS confirm
      IMPORTING
        is_row TYPE lvc_s_roid.

    METHODS refresh.

    METHODS prepare_confirm_dialog
      IMPORTING
        source    TYPE REF TO lcl_dofs
        target    TYPE REF TO lcl_dof OPTIONAL " optional for Rename and Delete
      EXPORTING
        t_confirm TYPE lcl_popup_confirm=>ty_ut_confirm.

    DATA: operation TYPE i READ-ONLY,
          auo_alv   TYPE REF TO cl_gui_alv_grid READ-ONLY,
          aut_fcat  TYPE lvc_t_fcat READ-ONLY.

  PROTECTED SECTION.
    DATA: aot_confirm       TYPE TABLE OF lcl_popup_confirm=>ty_us_confirm,
          aoo_source_dof    TYPE REF TO lcl_dof,
          aoo_target_dof    TYPE REF TO lcl_dof,
          ao_target_dirpath TYPE string.

  PRIVATE SECTION.
    DATA: ais_layout TYPE lvc_s_layo,
          ai_dialog  TYPE abap_bool.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_move DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_confirm_move DEFINITION INHERITING FROM lcl_popup_confirm.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS display
      IMPORTING
        source TYPE REF TO lcl_dofs
        target TYPE REF TO lcl_dof.
*                            IMPORTING
*                              io_frontend   TYPE REF TO lif_file_system
*                              io_appserv    TYPE REF TO lif_file_system.
    METHODS adjust_field_catalog REDEFINITION.
    METHODS adjust_buttons REDEFINITION.
    METHODS check_file REDEFINITION.
    METHODS skip_dialog REDEFINITION.
    METHODS before_operation REDEFINITION.
    METHODS execute_operation REDEFINITION.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_copy DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_confirm_copy DEFINITION INHERITING FROM lcl_popup_confirm.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS display
      IMPORTING
        source TYPE REF TO lcl_dofs
        target TYPE REF TO lcl_dof.
*                            IMPORTING
*                              io_frontend   TYPE REF TO lif_file_system
*                              io_appserv    TYPE REF TO lif_file_system.
    METHODS adjust_field_catalog REDEFINITION.
    METHODS adjust_buttons REDEFINITION.
    METHODS check_file REDEFINITION.
    METHODS skip_dialog REDEFINITION.
    METHODS before_operation REDEFINITION.
    METHODS execute_operation REDEFINITION.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_rename DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_confirm_rename DEFINITION INHERITING FROM lcl_popup_confirm.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS display
      IMPORTING
*        source TYPE REF TO lcl_dofs
*        target TYPE REF TO lcl_dof.
        dofs TYPE REF TO lcl_dofs.
*                            IMPORTING
*                              io_frontend   TYPE REF TO lif_file_system
*                              io_appserv    TYPE REF TO lif_file_system.
    METHODS adjust_field_catalog REDEFINITION.
    METHODS adjust_buttons REDEFINITION.
    METHODS check_file REDEFINITION.
    METHODS before_operation REDEFINITION.
    METHODS execute_operation REDEFINITION.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_delete DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_confirm_delete DEFINITION INHERITING FROM lcl_popup_confirm.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS display
      IMPORTING
        dofs TYPE REF TO lcl_dofs.
    METHODS adjust_field_catalog REDEFINITION.
    METHODS adjust_buttons REDEFINITION.
    METHODS check_file REDEFINITION.
    METHODS before_operation REDEFINITION.
    METHODS execute_operation REDEFINITION.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_f4_as_dirs_sscr DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_f4_as_dirs_sscr DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sscr.
    METHODS constructor.
    METHODS display
      IMPORTING
        parent          TYPE REF TO cl_gui_container
        start_path      TYPE csequence
      EXPORTING
        selected_folder TYPE csequence.
  PRIVATE SECTION.
    DATA folder TYPE string.
    DATA aio_f4_as_dirs TYPE REF TO lcl_f4_as_dirs.
    DATA aio_container TYPE REF TO cl_gui_container.
ENDCLASS."

*
CLASS lcl_folder_move_screen DEFINITION.
  PUBLIC SECTION.
    DATA choice TYPE i READ-ONLY.
    CONSTANTS:
      fp_sp TYPE i VALUE 1,
      fp_sm TYPE i VALUE 2,
      fm_sp TYPE i VALUE 3,
      fm_sm TYPE i VALUE 4.
    INTERFACES lif_sscr.
    METHODS display.
ENDCLASS."

*
CLASS lcl_folder_copy_screen DEFINITION.
  PUBLIC SECTION.
    DATA choice TYPE i READ-ONLY.
    CONSTANTS:
      fp_sp TYPE i VALUE 1,
      fp_sm TYPE i VALUE 2,
      fm_sp TYPE i VALUE 3,
      fm_sm TYPE i VALUE 4.
    INTERFACES lif_sscr.
    METHODS display.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_sscr DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_sscr DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS disable_all_function_codes
      IMPORTING
        i_pf_status TYPE gui_status
        except      TYPE ui_functions   OPTIONAL.
ENDCLASS.                    "lcl_sscr DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_string DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_string DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS swa_string_to_table
      IMPORTING
        character_string                      TYPE  string
        VALUE(append)                         TYPE  xfeld DEFAULT space
        VALUE(line_size)                      TYPE  i OPTIONAL
        VALUE(check_table_type)               TYPE  xfeld DEFAULT space
        VALUE(move_trailing_blanks_next_line) TYPE  xfeld DEFAULT space "sro1
      EXPORTING
        character_table                       TYPE  table
        VALUE(total_length)                   TYPE  i
        VALUE(line_size_used)                 TYPE  i
        VALUE(lines_filled)                   TYPE  i
        VALUE(last_line_length)               TYPE  i
      EXCEPTIONS
        no_flat_charlike_structure.

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_rtti DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_rtti DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF ty_gs_component,
              name        TYPE string,
              o_typedescr TYPE REF TO cl_abap_typedescr,
            END OF ty_gs_component.
    TYPES ty_gt_component TYPE TABLE OF ty_gs_component WITH DEFAULT KEY.

    CLASS-METHODS get_low_level_components
      IMPORTING
        io_typedescr TYPE REF TO cl_abap_typedescr
      EXPORTING
        et_component TYPE ty_gt_component.

    CLASS-METHODS get_component_name
      IMPORTING
        is_any                TYPE any
        i_structure_field     TYPE any
      RETURNING
        VALUE(component_name) TYPE string.

  PRIVATE SECTION.

    CLASS-METHODS get_low_level_components_recur
      IMPORTING
        i_name       TYPE string
        io_typedescr TYPE REF TO cl_abap_typedescr
      CHANGING
        ct_component TYPE ty_gt_component.

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_lvc_fcat
      IMPORTING
        it_std         TYPE STANDARD TABLE
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS LCL_HTML_VIEWER_HELPER DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_viewer_helper DEFINITION.
  PUBLIC SECTION.
    TYPES:BEGIN OF ty_us_table_postdata2,
            index TYPE string,
            value TYPE string,
          END OF ty_us_table_postdata2,

          ty_it_table_postdata2 TYPE STANDARD TABLE OF ty_us_table_postdata2 WITH DEFAULT KEY,

          BEGIN OF ty_us_table_postdata,
            name    TYPE string,
            t_value TYPE ty_it_table_postdata2,
          END OF ty_us_table_postdata,

          ty_ut_table_postdata TYPE STANDARD TABLE OF ty_us_table_postdata WITH DEFAULT KEY.

    CLASS-METHODS decode_values
      IMPORTING
        it_postdata          TYPE cnht_post_data_tab
        i_name_to_lower_case TYPE abap_bool DEFAULT abap_true
      EXPORTING
        et_simple_postdata   TYPE tihttpnvp
        et_table_postdata    TYPE ty_ut_table_postdata.
ENDCLASS."


*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
DEFINE mac_alias.
  aliases &2 for &1~&2.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*       CLASS lcl_appserv DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_appserv DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF ty_gs_file,
              dirname(175) TYPE c, " name of directory (possibly truncated)
              name(175)    TYPE c, " name of entry (possibly truncated)
              type(10)     TYPE c,            " type of entry
              isdir        TYPE int1,         " isdir 0 = no, 1 = yes
              len(16)      TYPE p DECIMALS 0, " length in bytes
              owner(8)     TYPE c,            " owner of the entry
              mdate        TYPE d,
              mtime        TYPE t,
              mode(9)      TYPE c, " like "rwx-r-x--x": protection mode
              errno(3)     TYPE c,
              errmsg(40)   TYPE c,
            END OF ty_gs_file,
            ty_gt_file TYPE TABLE OF ty_gs_file.

    METHODS constructor.

    CLASS-METHODS directory_browse
      IMPORTING
        container    TYPE REF TO cl_gui_container
        folder       TYPE csequence
      RETURNING
        VALUE(ro_f4) TYPE REF TO lcl_f4_as_dirs.

    INTERFACES lif_file_system DATA VALUES type = 'S'.

    mac_alias lif_file_system :
      get_file_sep,
      directory_list_files,
      normalize_dir_path,
      build_file_path_long,
      file_read_binary,
      file_write_binary,
      file_read_text,
      file_write_text,
      file_exist,
      directory_exist,
      file_move,
      file_copy,
      file_create,
      directory_create,
      file_delete,
      directory_delete,
      file_rename,
      directory_rename,
      execute,
      open_with,
      dof_added,
      dof_removed,
      file_changed,
      get_full_path,
      split_full_path.

  PRIVATE SECTION.
*    DATA: ais_config  TYPE ty_us_config.
    DATA: ai_file_sep(1)      TYPE c,
          ai_split_path_regex TYPE string.

    METHODS file_write_prepare
      IMPORTING
        fullpath TYPE csequence.
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_frontend DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_frontend DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF ty_gs_file.
    TYPES dir   TYPE string.
    INCLUDE TYPE file_info AS f.
    TYPES : END OF ty_gs_file,
    ty_gt_file TYPE TABLE OF ty_gs_file.

    METHODS constructor.

    INTERFACES lif_file_system DATA VALUES type = 'F'.

    mac_alias lif_file_system :
      get_file_sep,
      directory_list_files,
      normalize_dir_path,
      build_file_path_long,
      file_read_binary,
      file_write_binary,
      file_read_text,
      file_write_text,
      file_exist,
      directory_exist,
      file_move,
      file_copy,
      file_create,
      directory_create,
      file_delete,
      directory_delete,
      file_rename,
      directory_rename,
      execute,
      open_with,
      dof_added,
      dof_removed,
      file_changed,
      get_full_path,
      split_full_path.

    DATA: BEGIN OF aus_directory READ-ONLY,
            desktop TYPE string,
            sapgui  TYPE string,
            workdir TYPE string,
            system  TYPE string,
            temp    TYPE string,
            windows TYPE string,
          END OF aus_directory.

  PRIVATE SECTION.
    DATA: ai_file_sep(1)      TYPE c,
          ai_split_path_regex TYPE string.

ENDCLASS."


*
INTERFACE lif_ui_dir_tree.
  EVENTS directory_clicked
        EXPORTING
          VALUE(path)   TYPE csequence.

  INTERFACES lif_file_event.

  METHODS on_expand
    FOR EVENT expand_nc
        OF cl_gui_alv_tree
    IMPORTING
        node_key.

  METHODS on_link_click
    FOR EVENT link_click
        OF cl_gui_alv_tree
    IMPORTING
        fieldname
        node_key.

  METHODS on_node_context_menu_request
    FOR EVENT node_context_menu_request
        OF cl_gui_alv_tree
    IMPORTING
        node_key
        menu.

  METHODS on_node_context_menu_selected
    FOR EVENT node_context_menu_selected
        OF cl_gui_alv_tree
    IMPORTING
        node_key
        fcode.

  METHODS on_item_context_menu_request
    FOR EVENT item_context_menu_request
        OF cl_gui_alv_tree
    IMPORTING
        node_key
        fieldname
        menu.

  METHODS on_item_context_menu_selected
    FOR EVENT item_context_menu_selected
        OF cl_gui_alv_tree
    IMPORTING
        node_key
        fieldname
        fcode.

  METHODS on_context_menu_request
    IMPORTING
      node_key TYPE tv_nodekey
      menu     TYPE REF TO cl_ctmenu.

  METHODS on_context_menu_selected
    IMPORTING
      node_key TYPE tv_nodekey
      fcode    TYPE syucomm.

  METHODS on_drag_multiple
    FOR EVENT on_drag_multiple
        OF cl_gui_alv_tree
    IMPORTING
        drag_drop_object
        fieldname
        node_key_table.

*  METHODS on_drop
*    FOR EVENT on_drop
*        OF cl_gui_alv_tree
*    IMPORTING
*        drag_drop_object
*        node_key
*        sender.

  METHODS on_drop_external_files
    FOR EVENT on_drop_external_files
        OF cl_gui_alv_tree
    IMPORTING
        node_key
        files.

ENDINTERFACE.

*
INTERFACE lif_ui_dir_content.

  INTERFACES lif_file_event.

ENDINTERFACE.

*
CLASS lcl_ui_dir_tree DEFINITION DEFERRED.
*
INTERFACE lif_ui_dir_tree_builder.
  METHODS create
    IMPORTING
      parent          TYPE REF TO cl_gui_container
      file_system     TYPE REF TO lif_file_system
      start_path      TYPE csequence
      pfm             TYPE REF TO lcl_ui_file_manager
    RETURNING
      VALUE(instance) TYPE REF TO lif_ui_dir_tree.
ENDINTERFACE.

*
CLASS lcl_ui_dir_content DEFINITION DEFERRED.
*
INTERFACE lif_ui_dir_content_builder.
  METHODS create
    IMPORTING
      parent                  TYPE REF TO cl_gui_container
      start_path              TYPE csequence
      file_system             TYPE REF TO lif_file_system
      container_dir_input     TYPE REF TO cl_gui_container
      change_on_selected_dirs TYPE REF TO lif_ui_dir_tree
      pfm                     TYPE REF TO lcl_ui_file_manager
    RETURNING
      VALUE(instance)         TYPE REF TO lif_ui_dir_content.
ENDINTERFACE.

*
CLASS lcl_ui_dir_tree_builder_fs DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ui_dir_tree_builder.
ENDCLASS."

*
CLASS lcl_ui_zip_builder DEFINITION.
  PUBLIC SECTION.
    DATA auo_zip TYPE REF TO cl_abap_zip READ-ONLY.
    METHODS constructor IMPORTING dof TYPE REF TO lcl_dof.
    INTERFACES lif_ui_dir_tree_builder.
    INTERFACES lif_ui_dir_content_builder.
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_tree DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_tree DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_ui_dir_tree.

    METHODS constructor
      IMPORTING
        parent      TYPE REF TO cl_gui_container
        file_system TYPE REF TO lif_file_system
        start_path  TYPE csequence
        pfm         TYPE REF TO lcl_ui_file_manager.

    DATA: auo_tree        TYPE REF TO cl_gui_alv_tree.

    mac_alias lif_ui_dir_tree :
      directory_clicked,
      on_expand,
      on_link_click,
      on_node_context_menu_request,
      on_node_context_menu_selected,
      on_item_context_menu_request,
      on_item_context_menu_selected,
      on_context_menu_request,
      on_context_menu_selected,
      on_drag_multiple,
*      on_drop,
      on_drop_external_files.

    mac_alias lif_file_event :
      dof_move_requested,
      dof_copy_requested.


  PROTECTED SECTION.

    TYPES:BEGIN OF ty_is_ndof,
            fullpath TYPE string,
            filename TYPE string,
            path     TYPE string,
            isdir    TYPE int1,
          END OF ty_is_ndof,
          BEGIN OF ty_is_dir_node,
            fullpath TYPE string,
            node_key TYPE lvc_nkey,
          END OF ty_is_dir_node.
    DATA: aio_parent      TYPE REF TO cl_gui_container,
          aio_file_system TYPE REF TO lif_file_system,
          ai_start_path   TYPE string,
          aio_pfm         TYPE REF TO lcl_ui_file_manager,
          airt_dir        TYPE REF TO data,
          airs_dir        TYPE REF TO data,
          ait_dir_node    TYPE TABLE OF ty_is_dir_node.
    CONSTANTS:
      BEGIN OF cs_fcode,
        copy_file_path TYPE salv_de_function VALUE 'COPY_FILE_PATH', "#ec notext
        rename         TYPE salv_de_function VALUE 'RENAME',         "#ec notext
        delete         TYPE salv_de_function VALUE 'DELETE',         "#ec notext
        new_folder     TYPE salv_de_function VALUE 'NEW_FOLDER',     "#ec notext
        paste          TYPE salv_de_function VALUE 'PASTE',          "#ec notext
        paste_sub      TYPE salv_de_function VALUE 'PASTE_SUB',      "#ec notext
        cut            TYPE salv_de_function VALUE 'CUT',            "#ec notext
        copy           TYPE salv_de_function VALUE 'COPY',           "#ec notext
        open           TYPE salv_de_function VALUE 'OPEN',           "#ec notext
        refresh        TYPE salv_de_function VALUE 'REFRESH',        "#ec notext
        back           TYPE salv_de_function VALUE 'BACK',           "#ec notext
        forward        TYPE salv_de_function VALUE 'FORWARD',        "#ec notext
      END OF cs_fcode.

    METHODS create_tree_control.

    METHODS add_nodes ABSTRACT
      IMPORTING
        dirs        TYPE REF TO data "lcl_frontend=>ty_gt_file.
        parent_node TYPE lvc_nkey
        path        TYPE csequence.

    METHODS get_outtab_line ABSTRACT
      IMPORTING
        node_key TYPE salv_de_node_key
      EXPORTING
        dof      TYPE REF TO lcl_dof
        ndof     TYPE ty_is_ndof.

    METHODS get_dof ABSTRACT
      IMPORTING
        alv_line   TYPE any
      RETURNING
        VALUE(dof) TYPE REF TO lcl_dof.

    METHODS normalize_file_info ABSTRACT
      IMPORTING
        alv_line    TYPE any
      RETURNING
        VALUE(ndof) TYPE ty_is_ndof.

ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_tree_fs DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_tree_fs DEFINITION INHERITING FROM lcl_ui_dir_tree.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        parent      TYPE REF TO cl_gui_container
        file_system TYPE REF TO lif_file_system
        start_path  TYPE csequence
        pfm         TYPE REF TO lcl_ui_file_manager.

  PROTECTED SECTION.

    METHODS add_nodes REDEFINITION.
    METHODS get_outtab_line REDEFINITION.
    METHODS get_dof REDEFINITION.
    METHODS normalize_file_info REDEFINITION.

ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_tree_zip DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_tree_zip DEFINITION INHERITING FROM lcl_ui_dir_tree.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        parent      TYPE REF TO cl_gui_container
        file_system TYPE REF TO lif_file_system
        start_path  TYPE csequence
        pfm         TYPE REF TO lcl_ui_file_manager
        zip_content TYPE REF TO lcl_ui_zip_builder.

  PROTECTED SECTION.

    METHODS add_nodes REDEFINITION.
    METHODS get_outtab_line REDEFINITION.
    METHODS get_dof REDEFINITION.
    METHODS normalize_file_info REDEFINITION.

  PRIVATE SECTION.
    TYPES : BEGIN OF ty_is_dir,
              name TYPE string,
            END OF ty_is_dir,
            ty_it_dir TYPE TABLE OF ty_is_dir.
    DATA: aio_zip_content TYPE REF TO lcl_ui_zip_builder,
          ait_dir         TYPE ty_it_dir.

ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_file_manager DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ui_file_manager DEFINITION.
  PUBLIC SECTION.

    DATA: au_dir_fullpath   TYPE string READ-ONLY,
          au_appserv        TYPE abap_bool READ-ONLY,
          au_display_hier   TYPE abap_bool READ-ONLY,
          gr_dragdrop_d0100 TYPE REF TO cl_dragdrop READ-ONLY,
          g_dragdrop_handle TYPE i READ-ONLY.

    METHODS constructor
      IMPORTING
        io_ui_dir_tree_builder    TYPE REF TO lif_ui_dir_tree_builder
        io_ui_dir_content_builder TYPE REF TO lif_ui_dir_content_builder
        container                 TYPE REF TO cl_gui_container
        dir_fullpath              TYPE csequence
        appserv                   TYPE abap_bool
        display_hier              TYPE abap_bool
        handler                   TYPE REF TO lif_cross_action.

    METHODS free.

    METHODS on_dof_open_requested
      FOR EVENT dof_open_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_open_with_requested
      FOR EVENT dof_open_with_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_move_requested
      FOR EVENT dof_move_requested
          OF lif_file_event
      IMPORTING
          source
          target
          sender.

    METHODS on_dof_copy_requested
      FOR EVENT dof_copy_requested
          OF lif_file_event
      IMPORTING
          source
          target.

    METHODS on_new_file_requested
      FOR EVENT new_file_requested
          OF lif_file_event
      IMPORTING
          dof.

    METHODS on_new_folder_requested
      FOR EVENT new_folder_requested
          OF lif_file_event
      IMPORTING
          dof.

    METHODS on_dof_delete_requested
      FOR EVENT dof_delete_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_rename_requested
      FOR EVENT dof_rename_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_copy_to_clpb_requested
      FOR EVENT dof_copy_to_clpb_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_cut_to_clpb_requested
      FOR EVENT dof_cut_to_clpb_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_paste_from_clpb_requ
      FOR EVENT dof_paste_from_clpb_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_copy_path_2_clpb_requ
      FOR EVENT dof_copy_path_2_clpb_requested
          OF lif_file_event
      IMPORTING
          dofs.

    METHODS on_dof_custom_action_requested
      FOR EVENT dof_custom_action_requested
          OF lif_file_event
      IMPORTING
          ucomm
          dofs.

    CLASS-METHODS ask_closed_question
      IMPORTING
        question      TYPE csequence
        answer1       TYPE csequence
        answer2       TYPE csequence
      RETURNING
        VALUE(answer) TYPE char1.

    CLASS-METHODS ask_new_dof_name
      IMPORTING
        request         TYPE csequence
        directory       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(dof_name) TYPE string.

  PRIVATE SECTION.
    DATA: aio_splitter       TYPE REF TO cl_gui_splitter_container,
          aio_file_system    TYPE REF TO lif_file_system,
          aio_frontend       TYPE REF TO lcl_frontend,
          aio_appserv        TYPE REF TO lcl_appserv,
          ais_config         TYPE ty_us_config,
          aio_ui_dir_tree    TYPE REF TO lif_ui_dir_tree,
          aio_ui_dir_content TYPE REF TO lif_ui_dir_content,
          aio_container      TYPE REF TO cl_gui_container,
          aio_cross_action   TYPE REF TO lif_cross_action.

    METHODS clipboard_export
      IMPORTING
        i_string TYPE string.

    METHODS move
      IMPORTING
        source TYPE REF TO lcl_dofs
        target TYPE REF TO lcl_dof.

    METHODS copy
      IMPORTING
        source TYPE REF TO lcl_dofs
        target TYPE REF TO lcl_dof.

    METHODS rename
      IMPORTING
        dofs TYPE REF TO lcl_dofs.

ENDCLASS."

**
*CLASS lcl_ui_dir_content_builder DEFINITION.
*  PUBLIC SECTION.
*    INTERFACES lif_ui_dir_content_builder.
*ENDCLASS."

*
CLASS lcl_ui_dir_content_builder_fs DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ui_dir_content_builder.
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_content DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_content DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_ui_dir_content.

    DATA: auo_table TYPE REF TO cl_gui_alv_grid READ-ONLY,
          au_path   TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        parent                  TYPE REF TO cl_gui_container
        start_path              TYPE csequence
        file_system             TYPE REF TO lif_file_system
        container_dir_input     TYPE REF TO cl_gui_container
        change_on_selected_dirs TYPE REF TO lif_ui_dir_tree
        pfm                     TYPE REF TO lcl_ui_file_manager.
    METHODS initialize.

    METHODS create_control.

    mac_alias lif_file_event :
      dof_open_requested,
      dof_open_with_requested,
      dof_move_requested,
      dof_copy_requested,
      new_file_requested,
      new_folder_requested,
      dof_delete_requested,
      dof_rename_requested,
      dof_copy_to_clpb_requested,
      dof_cut_to_clpb_requested,
      dof_paste_from_clpb_requested,
      dof_copy_path_2_clpb_requested,
      dof_custom_action_requested.

    EVENTS directory_changed
          EXPORTING
            VALUE(dir_fullpath)   TYPE string.

  PROTECTED SECTION.
    TYPES BEGIN OF ty_is_f_alv.
    TYPES icon TYPE iconname.
    INCLUDE TYPE lcl_frontend=>ty_gs_file AS s_file.
    TYPES END OF ty_is_f_alv.
    TYPES ty_it_f_alv     TYPE STANDARD TABLE OF ty_is_f_alv.

    TYPES BEGIN OF ty_is_s_alv.
    TYPES icon TYPE iconname.
    INCLUDE TYPE lcl_appserv=>ty_gs_file AS s_file.
    TYPES END OF ty_is_s_alv.
    TYPES ty_it_s_alv     TYPE STANDARD TABLE OF ty_is_s_alv.
    TYPES:BEGIN OF ty_is_ndof,
            filename TYPE string,
            dir      TYPE string,
            fullpath TYPE string,
            isdir    TYPE int1,
          END OF ty_is_ndof.

    CONSTANTS:
      BEGIN OF cs_fcode, " enumeration
        copy_file_path TYPE salv_de_function VALUE 'COPY_FILE_PATH', "#ec notext
        rename         TYPE salv_de_function VALUE 'RENAME',         "#ec notext
        delete         TYPE salv_de_function VALUE 'DELETE',         "#ec notext
        new_file       TYPE salv_de_function VALUE 'NEW_FILE',       "#ec notext
        new_folder     TYPE salv_de_function VALUE 'NEW_FOLDER',     "#ec notext
        paste          TYPE salv_de_function VALUE 'PASTE',          "#ec notext
        paste_sub      TYPE salv_de_function VALUE 'PASTE_SUB',      "#ec notext
        cut            TYPE salv_de_function VALUE 'CUT',            "#ec notext
        copy           TYPE salv_de_function VALUE 'COPY',           "#ec notext
        open           TYPE salv_de_function VALUE 'OPEN',           "#ec notext
        open_with      TYPE salv_de_function VALUE 'OPEN_WITH',      "#ec notext
        open_notepad   TYPE salv_de_function VALUE 'OPEN_NOTEPAD',   "#ec notext

        refresh        TYPE salv_de_function VALUE 'REFRESH',        "#ec notext
        up             TYPE salv_de_function VALUE 'UP',             "#ec notext
        back           TYPE salv_de_function VALUE 'BACK',           "#ec notext
        forward        TYPE salv_de_function VALUE 'FORWARD',        "#ec notext
      END OF cs_fcode.

    DATA: aio_parent                  TYPE REF TO cl_gui_container,
          aio_file_system             TYPE REF TO lif_file_system,
          ai_start_path               TYPE string,
          aio_pfm                     TYPE REF TO lcl_ui_file_manager,
          aio_container_dir_input     TYPE REF TO cl_gui_container,
          aio_change_on_selected_dirs TYPE REF TO lif_ui_dir_tree,
          aio_htmlview                TYPE REF TO cl_gui_html_viewer,
          airt_file                   TYPE REF TO data,
          airt_alv                    TYPE REF TO data,
          ait_dirfullpath             TYPE TABLE OF string,
          ai_dirfullpath_index        TYPE sytabix.


    METHODS get_dof ABSTRACT
      IMPORTING
        alv_line   TYPE any
      RETURNING
        VALUE(dof) TYPE REF TO lcl_dof.

    METHODS normalize_file_info ABSTRACT
      IMPORTING
        alv_line    TYPE any
      RETURNING
        VALUE(ndof) TYPE ty_is_ndof.



    METHODS on_toolbar
      FOR EVENT toolbar
          OF cl_gui_alv_grid
      IMPORTING
          e_object
          sender.

    METHODS on_user_command
      FOR EVENT user_command
          OF cl_gui_alv_grid
      IMPORTING
          e_ucomm
          sender.

    METHODS on_double_click
      FOR EVENT double_click
          OF cl_gui_alv_grid
      IMPORTING
          es_row_no.

    METHODS on_drag
      FOR EVENT ondrag
          OF cl_gui_alv_grid
      IMPORTING
          e_row
          e_column
          es_row_no
          e_dragdropobj.

    METHODS on_drop
      FOR EVENT ondrop
          OF cl_gui_alv_grid
      IMPORTING
          e_row
          e_column
          es_row_no
          e_dragdropobj.

    METHODS on_drop_external_files
      FOR EVENT drop_external_files
          OF cl_gui_alv_grid
      IMPORTING
          files.

    METHODS on_context_menu_request
      FOR EVENT context_menu_request
          OF cl_gui_alv_grid
      IMPORTING
          e_object.

    METHODS directory_list_files ABSTRACT
      IMPORTING
        path    TYPE csequence OPTIONAL
        filter  TYPE csequence OPTIONAL
        refresh TYPE abap_bool DEFAULT abap_true
          PREFERRED PARAMETER path.

    CLASS-METHODS get_file_icon
      IMPORTING
        file_name        TYPE csequence
      RETURNING
        VALUE(icon_name) TYPE iconname.

    METHODS on_directory_clicked
      FOR EVENT directory_clicked
          OF lif_ui_dir_tree
      IMPORTING
          path.

    METHODS on_sapevent
      FOR EVENT sapevent
          OF cl_gui_html_viewer
      IMPORTING
          action
          frame
          getdata
          postdata
          query_table.

    METHODS on_dof_added
      FOR EVENT dof_added
          OF lif_file_system
      IMPORTING
          fullpath
          sender.

    METHODS on_dof_removed
      FOR EVENT dof_removed
          OF lif_file_system
      IMPORTING
          fullpath.

    METHODS change_text_dir_input
      IMPORTING
        text TYPE csequence.

    METHODS refresh_table_display.

ENDCLASS."

*
CLASS lcl_ui_dir_content_fs DEFINITION INHERITING FROM lcl_ui_dir_content.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        parent                  TYPE REF TO cl_gui_container
        start_path              TYPE csequence
        file_system             TYPE REF TO lif_file_system
        container_dir_input     TYPE REF TO cl_gui_container
        change_on_selected_dirs TYPE REF TO lif_ui_dir_tree
        pfm                     TYPE REF TO lcl_ui_file_manager.
  PROTECTED SECTION.
    METHODS get_dof REDEFINITION.
    METHODS normalize_file_info REDEFINITION.
    METHODS directory_list_files REDEFINITION.
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_content_zip DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_content_zip DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ui_dir_content.

    METHODS constructor
      IMPORTING
        parent                  TYPE REF TO cl_gui_container
        start_path              TYPE csequence
        file_system             TYPE REF TO lif_file_system
        container_dir_input     TYPE REF TO cl_gui_container
        change_on_selected_dirs TYPE REF TO lif_ui_dir_tree
        pfm                     TYPE REF TO lcl_ui_file_manager
        zip_content             TYPE REF TO lcl_ui_zip_builder.

*  PROTECTED SECTION.
*
*    METHODS initialize REDEFINITION.
*    METHODS create_tree_control REDEFINITION.
*    METHODS add_nodes REDEFINITION.
*    METHODS get_outtab_line REDEFINITION.
*    METHODS get_dof REDEFINITION.
*    METHODS normalize_file_info REDEFINITION.

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FRIENDS  lcl_initial_screen
                                  lcl_appserv
                                  lcl_frontend
                                  lcl_main_screen
                                  lcl_confirm_move
                                  lcl_confirm_copy
                                  lcl_confirm_rename
                                  lcl_confirm_delete
                                  lcl_ui_dir_content
                                  lcl_ui_dir_tree
                                  lcl_ui_file_manager
                                  lcl_folder_move_screen
                                  lcl_folder_copy_screen.
  PUBLIC SECTION.
    INTERFACES lif_sscr_h.

    METHODS constructor.
    METHODS listbox_text_mode.
    METHODS listbox_dir_type  IMPORTING
                                screen_field_name TYPE csequence.
    METHODS listbox_gui       IMPORTING
                                screen_field_name TYPE csequence.
    METHODS f4_directory2
      CHANGING
        dir TYPE csequence.
    METHODS f4_directory
      IMPORTING
        screen_field_name  TYPE csequence
        dirtype_field_name TYPE csequence
        appserv_field_name TYPE csequence.

    METHODS f4_sap_directory
      IMPORTING
        sapdir_scrfname      TYPE csequence
        dirfullpath_scrfname TYPE csequence.

    METHODS f4_icon
      IMPORTING
        icon_scrfname     TYPE csequence
        iconname_scrfname TYPE csequence.

    METHODS get_sap_gui_directory
      IMPORTING
        gu         TYPE csequence
        logargs    TYPE csequence
      RETURNING
        VALUE(dir) TYPE string.

    METHODS get_sap_directory
      IMPORTING
        sap_dir                 TYPE csequence
      RETURNING
        VALUE(sap_dir_fullpath) TYPE string.

    METHODS is_text_file
      IMPORTING
        file_name           TYPE csequence
      RETURNING
        VALUE(is_text_type) TYPE abap_bool.

    METHODS dof_copy
      CHANGING
        cs_confirm TYPE lcl_popup_confirm=>ty_us_confirm.

    METHODS file_copy
      CHANGING
        cs_confirm TYPE lcl_popup_confirm=>ty_us_confirm.

    METHODS dir_copy
      CHANGING
        cs_confirm TYPE lcl_popup_confirm=>ty_us_confirm.

    DATA: au_syslset         TYPE sy-slset READ-ONLY,
          aio_confirm_move   TYPE REF TO lcl_confirm_move,
          aio_confirm_copy   TYPE REF TO lcl_confirm_copy,
          aio_confirm_rename TYPE REF TO lcl_confirm_rename,
          aio_confirm_delete TYPE REF TO lcl_confirm_delete.


  PRIVATE SECTION.
    TYPES : BEGIN OF ty_is_sscr_text,
              dynnr       TYPE sydynnr,
              selname     TYPE rsscr_name,
              seltext(30) TYPE c,
            END OF ty_is_sscr_text.
    TYPES : BEGIN OF ty_is_sscr_title,
              dynnr     TYPE sydynnr,
              title(70) TYPE c,
            END OF ty_is_sscr_title.
    DATA: aio_frontend              TYPE REF TO lcl_frontend, "lif_file_system,
          aio_appserv               TYPE REF TO lcl_appserv, "lif_file_system,
          aio_initial_screen        TYPE REF TO lcl_initial_screen,
          aio_detail_screen         TYPE REF TO lcl_main_screen,
          aio_f4_as_dirs_sscr       TYPE REF TO lcl_f4_as_dirs_sscr,
          aio_config_screen         TYPE REF TO lcl_config_screen,
          aio_config_unix_screen    TYPE REF TO lcl_config_unix_screen,
          aio_config_windows_screen TYPE REF TO lcl_config_windows_screen,
          aio_config_icon_screen    TYPE REF TO lcl_config_icon_screen,
          aio_config_prog_screen    TYPE REF TO lcl_config_prog_screen,
          aio_config_misc_screen    TYPE REF TO lcl_config_misc_screen,
          aio_config_user_screen    TYPE REF TO lcl_config_user_screen,
          aio_dir_move              TYPE REF TO lcl_folder_move_screen,
          aio_dir_copy              TYPE REF TO lcl_folder_copy_screen,
          ais_config_as             TYPE ty_us_config,
          ais_config_fe             TYPE ty_us_config.
    DATA ait_sscr_text TYPE TABLE OF ty_is_sscr_text.
    DATA ait_sscr_title TYPE TABLE OF ty_is_sscr_title.
ENDCLASS."













*----------------------------------------------------------------------*
*       Global variables
*----------------------------------------------------------------------*
DATA: go_app   TYPE REF TO lcl_app,
      owndirec TYPE string.









*----------------------------------------------------------------------*
*       S E L E C T I O N    S C R E E N S
*----------------------------------------------------------------------*



*-------------------------------------------------------------
* initial screen
*-------------------------------------------------------------
* SELECTION-SCREEN BEGIN OF SCREEN 1000
DEFINE mac_sscr_directory.
  selection-screen begin of block b0&1 with frame title text_b0&1.

  parameters dirtype&1 type char1 as listbox visible length 30 obligatory user-command as&1.

  selection-screen begin of line.
  parameters appserv&1 as checkbox modif id fi&1.
  selection-screen comment (31) text_as&1 for field appserv&1 modif id fi&1.
  parameters hier&1 as checkbox default abap_true modif id hi&1.
  selection-screen comment (31) text_hi&1 for field hier&1 modif id hi&1.
  selection-screen end of line.

  parameters lf&1 type fileintern modif id lf&1.

  parameters lp&1 type filepath-pathintern modif id lp&1.

  parameters gu&1 type char1 as listbox visible length 30 modif id gu&1 user-command gu&1.

  parameters logargs&1 type string lower case modif id la&1.

  parameters sd&1 type user_dir-aliass modif id sd&1.

  parameters dir&1 type rvari_val_255 lower case modif id fi&1.

  selection-screen end of block b0&1.

  selection-screen skip 1.
END-OF-DEFINITION.

DEFINE mac_sscr_directory_f4.

  at selection-screen on value-request for dirtype&1.
    go_app->listbox_dir_type( 'DIRTYPE&1' ).

  at selection-screen on value-request for gu&1.
    go_app->listbox_gui( 'GU&1' ).

  at selection-screen on value-request for dir&1.
    go_app->f4_directory( screen_field_name = 'DIR&1' dirtype_field_name = 'DIRTYPE&1' appserv_field_name = 'APPSERV&1' ).

  at selection-screen on value-request for lf&1.
    perform f4_logical_file using 'LF&1' 'LOGARGS&1' 'LP&1' 'DIR&1'
                                  lf&1 logargs&1 lp&1 dir&1.

  at selection-screen on value-request for lp&1.
    perform f4_logical_path using 'LP&1' 'LOGARGS&1' 'DIR&1'
                                  lp&1 logargs&1 dir&1.

  at selection-screen on value-request for sd&1.
    go_app->f4_sap_directory( sapdir_scrfname = 'SD&1' dirfullpath_scrfname = 'DIR&1' ).
END-OF-DEFINITION.



SELECTION-SCREEN FUNCTION KEY : 1, 2.

mac_sscr_directory: 1, 2, 3.

" Program specific to the current variant
DEFINE mac_sscr_prog1.
  selection-screen begin of block bp&1 with frame title text_bp&1.
  parameters prg&1text type gui_text lower case. " text in the contextual menu
  parameters prg&1name type progname matchcode object progname. " program to trigger
  parameters prg&1vari type variant.
  parameters prg&1pffp type rsscr_name. " parameter to contain the full path on frontend
  parameters prg&1pafp type rsscr_name. " parameter to contain the full path on application server
  selection-screen end of block bp&1.
END-OF-DEFINITION.
mac_sscr_prog1 : 8, 9.



*-------------------------------------------------------------
* Configuration settings
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1006 AS WINDOW.

SELECTION-SCREEN FUNCTION KEY : 1.

SELECTION-SCREEN BEGIN OF TABBED BLOCK tabstrip FOR 20 LINES.
SELECTION-SCREEN TAB (78) tab1titl USER-COMMAND ucomm_tab1. "1008 : UNIX commands
SELECTION-SCREEN TAB (78) tab5titl USER-COMMAND ucomm_tab5. "1014 : WINDOWS commands
SELECTION-SCREEN TAB (78) tab2titl USER-COMMAND ucomm_tab2. "1009 : file types (icons, etc.)
SELECTION-SCREEN TAB (78) tab3titl USER-COMMAND ucomm_tab3. "1010 : ABAP programs for general use
SELECTION-SCREEN TAB (78) tab4titl USER-COMMAND ucomm_tab4. "1012 : other settings
SELECTION-SCREEN END OF BLOCK tabstrip.

SELECTION-SCREEN END OF SCREEN 1006.




*- - - - - - - - - - - - - - - - - - - - - - - - - - - -
* Config - Unix commands
*- - - - - - - - - - - - - - - - - - - - - - - - - - - -
SELECTION-SCREEN BEGIN OF SCREEN 1008 AS SUBSCREEN.

" FILES
SELECTION-SCREEN BEGIN OF BLOCK bfs WITH FRAME TITLE text_bfs.

*** MOVE ONE FILE ***
SELECTION-SCREEN BEGIN OF BLOCK bsm WITH FRAME TITLE text_bsm.
PARAMETERS xpgsmove TYPE string LOWER CASE DEFAULT 'mv "%1" "%2" %3'.
PARAMETERS xpgsmov3 TYPE string LOWER CASE DEFAULT ''.
SELECTION-SCREEN END OF BLOCK bsm.

*** COPY ONE FILE *** (%1 = source, %2 = target, %3 = replaced by xpgscpy3 if overwrite, otherwise blank)
SELECTION-SCREEN BEGIN OF BLOCK bsc WITH FRAME TITLE text_bsc.
PARAMETERS xpgscopy TYPE string LOWER CASE DEFAULT 'cp "%1" "%2" %3"'.
PARAMETERS xpgscpy3 TYPE string LOWER CASE DEFAULT ''.
SELECTION-SCREEN END OF BLOCK bsc.

*** PRE-CREATION OF FILE ***
SELECTION-SCREEN BEGIN OF BLOCK bsf WITH FRAME TITLE text_bsf.
PARAMETERS xpgsmkf0 AS CHECKBOX DEFAULT abap_true.
PARAMETERS xpgsmkf1 TYPE string LOWER CASE DEFAULT 'touch "%1"'.
PARAMETERS xpgsmkf2 TYPE string LOWER CASE DEFAULT 'chmod 777 "%1"'.
SELECTION-SCREEN END OF BLOCK bsf.

SELECTION-SCREEN END OF BLOCK bfs.

SELECTION-SCREEN BEGIN OF BLOCK bds WITH FRAME TITLE text_bds.

PARAMETERS xpgsmkdi TYPE string LOWER CASE DEFAULT 'mkdir "%1"'.

PARAMETERS xpgsrmdi TYPE string LOWER CASE DEFAULT 'rmdir "%1"'.

SELECTION-SCREEN BEGIN OF BLOCK bcs WITH FRAME TITLE text_bcs.
*** COPY ONE DIRECTORY, ITS FILES, ITS SUB-DIRECTORIES ***
PARAMETERS xpgscpy4 TYPE string LOWER CASE DEFAULT 'cp -r "%1" "%2"'.

*** COPY ONE DIRECTORY, ITS FILES, BUT NOT ITS SUB-DIRECTORIES ***
PARAMETERS xpgscpy5 TYPE string LOWER CASE DEFAULT 'cp "%1" "%2"'.

*** COPY ONE DIRECTORY, ITS SUB-DIRECTORIES, BUT NOT ITS FILES ***
PARAMETERS xpgscpy6 TYPE string LOWER CASE DEFAULT 'mkdir "%1"'.
PARAMETERS xpgscpy7 TYPE string LOWER CASE DEFAULT 'cd "%1"'.
PARAMETERS xpgscpy8 TYPE string LOWER CASE DEFAULT 'find . -type d | cpio -pdm "%2"'.
SELECTION-SCREEN END OF BLOCK bcs.

SELECTION-SCREEN END OF BLOCK bds.

SELECTION-SCREEN END OF SCREEN 1008.

*- - - - - - - - - - - - - - - - - - - -
* Config - Windows commands
*- - - - - - - - - - - - - - - - - - - -
SELECTION-SCREEN BEGIN OF SCREEN 1014 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK bff WITH FRAME TITLE text_bff.

SELECTION-SCREEN BEGIN OF BLOCK bfm WITH FRAME TITLE text_bfm.
PARAMETERS xpgfmove TYPE string LOWER CASE DEFAULT 'move "%1" "%2" %3'.
PARAMETERS xpgfmov3 TYPE string LOWER CASE DEFAULT '/Y'.
SELECTION-SCREEN END OF BLOCK bfm.

SELECTION-SCREEN BEGIN OF BLOCK bfc WITH FRAME TITLE text_bfc.
PARAMETERS xpgfcopy TYPE string LOWER CASE DEFAULT 'copy "%1" "%2" %3'.
PARAMETERS xpgfcpy3 TYPE string LOWER CASE DEFAULT '/Y'.
SELECTION-SCREEN END OF BLOCK bfc.

SELECTION-SCREEN END OF BLOCK bff.

SELECTION-SCREEN BEGIN OF BLOCK bdf WITH FRAME TITLE text_bdf.

PARAMETERS xpgfmkdi TYPE string LOWER CASE DEFAULT 'mkdir "%1"'.

PARAMETERS xpgfrmdi TYPE string LOWER CASE DEFAULT 'rmdir "%1"'.

SELECTION-SCREEN BEGIN OF BLOCK bcf WITH FRAME TITLE text_bcf.
*** COPY ONE DIRECTORY, ITS FILES, ITS SUB-DIRECTORIES ***
PARAMETERS xpgffpsp TYPE string LOWER CASE DEFAULT 'xcopy "%1" "%2" /E'.

*** COPY ONE DIRECTORY, ITS FILES, BUT NOT ITS SUB-DIRECTORIES ***
PARAMETERS xpgffpsm TYPE string LOWER CASE DEFAULT 'copy "%1" "%2"'.

*** COPY ONE DIRECTORY, ITS SUB-DIRECTORIES, BUT NOT ITS FILES ***
PARAMETERS xpgffmsp TYPE string LOWER CASE DEFAULT 'xcopy "%1" "%2" /T /E'.


SELECTION-SCREEN END OF BLOCK bcf.

SELECTION-SCREEN END OF BLOCK bdf.

SELECTION-SCREEN END OF SCREEN 1014.



*- - - - - - - - - - - - - - - - - - - -
* File types & icons
*- - - - - - - - - - - - - - - - - - - -
DEFINE mac_icon.
  selection-screen begin of line.
  parameters fileext&1 type string lower case default &2.
  selection-screen comment (3) space&1_a.
  parameters filetxt&1 as checkbox default &3.
  selection-screen comment (4) space&1_b.
  selection-screen comment (4) icodext&1 visible length 2 modif id out.
  parameters iconext&1 type iconname default &4.
  selection-screen end of line.
END-OF-DEFINITION.

DEFINE mac_icon_f4.
  at selection-screen on value-request for iconext&1.
    go_app->f4_icon( icon_scrfname = 'ICODEXT&1' iconname_scrfname = 'ICONEXT&1' ).
END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF SCREEN 1009 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (45) fileexth.
SELECTION-SCREEN COMMENT (10) filetxth.
SELECTION-SCREEN COMMENT (4)  icodexth.
SELECTION-SCREEN COMMENT (30) iconexth.
SELECTION-SCREEN END OF LINE.
*           Regular expression             Text file  Icon
*           -----------------------------     ---     ---------------
mac_icon: 1 '\.xml'                           'X'    'ICON_XML_DOC',
          2 '\.xls.*'                         ''     'ICON_XLS',
          3 '\.doc.*'                         ''     'ICON_DOC',
          4 '\.htm.*'                         'X'    'ICON_HTM',
          5 '\.bmp|\.jpg|\.gif|\.png|\.ico'   ''     'ICON_BMP',
          6 '\.zip'                           ''     'ICON_CLOSED_FOLDER_UPTODATE',
          7 '\.ini|\.txt'                     'X'    'ICON_WRI',
          8 '\.pdf'                           ''     'ICON_PDF',
          9 '\.exe|\.dll|\.bat|\.vbs|\.com'   ''     'ICON_OPERATION',
          a '\.csv'                           'X'    'ICON_XLS',
          b ''                                ''     '',
          c ''                                ''     '',
          d ''                                ''     '',
          e ''                                ''     '',
          f ''                                ''     '',
          z 'All other files'(035)            ''     'ICON_ANY_DOCUMENT'.
SELECTION-SCREEN END OF SCREEN 1009.

SELECTION-SCREEN BEGIN OF SCREEN 1010 AS SUBSCREEN.
mac_sscr_prog1 : 1, 2, 3, 4, 5, 6, 7.
*" - physical file (full access path) on frontend
*" - physical file (full access path) on application server
*" - physical path + file name on frontend
*" - physical path + file name on application server
*" - logical file + param1 + param2 + param3
*" - logical path + filename + param1 + param2 + param3
SELECTION-SCREEN END OF SCREEN 1010.


*- - - - - - - - - - - - - - - - - - - -
* Config - Other settings
*- - - - - - - - - - - - - - - - - - - -
SELECTION-SCREEN BEGIN OF SCREEN 1012 AS SUBSCREEN.

" temporary directory on Application Server for executing ABAP programs
" on frontend files, although the program can only work with files on application server.
PARAMETERS tempdir TYPE string LOWER CASE.

" Execute OS command %1 at frontend (Windows here)
PARAMETERS xpgfcmdc TYPE string LOWER CASE DEFAULT 'cmd'.
PARAMETERS xpgfcmdp TYPE string LOWER CASE DEFAULT '/C "%1"'.

SELECTION-SCREEN END OF SCREEN 1012.




*-------------------------------------------------------------
* User settings
*-------------------------------------------------------------
DEFINE mac_sscr_dir.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS dir&1find TYPE string LOWER CASE modif id inp. " ^/int/..4/.../(.+)$
  PARAMETERS dir&1repl TYPE string LOWER CASE modif id inp. " \\2kze0.ze0.erd.edf.fr\dp_sauv\SR0D12FN\SAP FS\PI\$1
  SELECTION-SCREEN END OF LINE.
END-OF-DEFINITION.

DEFINE mac_sscr_dir_f4.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR dir&1find.
    DATA lo_f4_as_dirs_sscr TYPE REF TO lcl_f4_as_dirs_sscr.
    lcl_dynp_values=>read_single_field( 'DIR&1FIND' ).
    CREATE OBJECT lo_f4_as_dirs_sscr.
    CALL METHOD lo_f4_as_dirs_sscr->display
      EXPORTING
        parent          = cl_gui_container=>screen2
        start_path      = dir&1FIND
      IMPORTING
        selected_folder = dir&1FIND.
    lcl_dynp_values=>update_single_field( name = 'DIR&1FIND' value = dir&1FIND ).

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR dir&1repl.
    lcl_dynp_values=>read_single_field( 'DIR&1REPL' ).
    PERFORM f4_select_frontend_file CHANGING dir&1repl.

END-OF-DEFINITION.

DEFINE mac_sscr_usrf.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS usrf&1txt TYPE string LOWER CASE modif id inp. " Notepad++
  PARAMETERS usrf&1prg TYPE string LOWER CASE modif id inp. " C:\Notepad++\notepad++.exe
  SELECTION-SCREEN END OF LINE.
END-OF-DEFINITION.

DEFINE mac_sscr_usrfprg_f4.
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR usrf&1prg.
    lcl_dynp_values=>read_single_field( 'USRF&1PRG' ).
    PERFORM f4_select_frontend_file CHANGING usrf&1prg.
END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF SCREEN 1011 AS WINDOW.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS usrfile AS CHECKBOX USER-COMMAND usrfile.
SELECTION-SCREEN COMMENT (32) usrfilet FOR FIELD usrfile.
PARAMETERS usrconf TYPE string LOWER CASE MODIF ID fil. " path to configuration file
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b06.
PARAMETERS settvar TYPE vari-variant MODIF ID inp.

*PARAMETERS owndirec TYPE string LOWER CASE modif id inp.
SELECTION-SCREEN BEGIN OF BLOCK bdm WITH FRAME TITLE text_bdm.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (45) dirfindh.
SELECTION-SCREEN COMMENT (45) dirreplh.
SELECTION-SCREEN END OF LINE.
mac_sscr_dir : 1, 2, 3.

SELECTION-SCREEN END OF BLOCK bdm.

PARAMETERS autotemp AS CHECKBOX MODIF ID inp.

PARAMETERS alwconfi AS CHECKBOX MODIF ID inp. " systematic confirmation dialog for copy/move

" copy text mode (conversion between CRLF and LF)
PARAMETERS txtcopy TYPE char1 AS LISTBOX OBLIGATORY VISIBLE LENGTH 10 MODIF ID inp.

SELECTION-SCREEN BEGIN OF BLOCK bua WITH FRAME TITLE text_bua.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (45) usrfhtxt.
SELECTION-SCREEN COMMENT (45) usrfhprg.
SELECTION-SCREEN END OF LINE.

mac_sscr_usrf : 1, 2, 3, 4, 5, 6.

SELECTION-SCREEN END OF BLOCK bua.

SELECTION-SCREEN END OF BLOCK b06.

SELECTION-SCREEN END OF SCREEN 1011.


*-------------------------------------------------------------
* Detail screen
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1001.
SELECTION-SCREEN FUNCTION KEY 1.
PARAMETERS dumy1001.
SELECTION-SCREEN END OF SCREEN 1001.


*-------------------------------------------------------------
* F4 directories on application server
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1007 AS WINDOW.
PARAMETERS dumy1007.
SELECTION-SCREEN END OF SCREEN 1007.


*-------------------------------------------------------------
* Dialog for MOVE
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1002 AS WINDOW.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
PARAMETERS dumy1002.
SELECTION-SCREEN END OF SCREEN 1002.


*-------------------------------------------------------------
* Dialog for COPY
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1003 AS WINDOW.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
PARAMETERS dumy1003.
SELECTION-SCREEN END OF SCREEN 1003.


*-------------------------------------------------------------
* Dialog for RENAME
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1004 AS WINDOW.
SELECTION-SCREEN FUNCTION KEY 2.
PARAMETERS dumy1004.
SELECTION-SCREEN END OF SCREEN 1004.


*-------------------------------------------------------------
* Dialog for DELETE
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1005 AS WINDOW.
SELECTION-SCREEN FUNCTION KEY 2.
PARAMETERS dumy1005.
SELECTION-SCREEN END OF SCREEN 1005.

*-------------------------------------------------------------
* Dialog for choosing the type of move/copy for directories
*-------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1013 AS WINDOW.

SELECTION-SCREEN COMMENT /1(45) text_d01.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /15(30) text_d02.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 22(6) text_d04.
SELECTION-SCREEN COMMENT 29(6) text_d05.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (14) text_d03.
SELECTION-SCREEN COMMENT 15(6) text_d06.
SELECTION-SCREEN POSITION 22.
PARAMETERS p_fpsp RADIOBUTTON GROUP rb1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 29.
PARAMETERS p_fpsm RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 15(6) text_d07.
SELECTION-SCREEN POSITION 22.
PARAMETERS p_fmsp RADIOBUTTON GROUP rb1.
SELECTION-SCREEN POSITION 29.
PARAMETERS p_fmsm RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF SCREEN 1013.



INITIALIZATION.
  CREATE OBJECT go_app.

AT SELECTION-SCREEN OUTPUT.
  go_app->lif_sscr~pbo( ).

AT SELECTION-SCREEN.
  go_app->lif_sscr~pai( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  go_app->lif_sscr~exit( ).





*-------------------------------------------------------------------------------------------------
* ON VALUE-REQUEST
*-------------------------------------------------------------------------------------------------
  mac_sscr_directory_f4 : 1, 2, 3.
  mac_icon_f4: 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f, z.
  mac_sscr_usrfprg_f4 : 1, 2, 3, 4, 5, 6.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR usrconf.
  PERFORM f4_select_frontend_file CHANGING usrconf.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR owndirec.
*  PERFORM f4_select_frontend_dir CHANGING owndirec.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR txtcopy.
  go_app->listbox_text_mode( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR tempdir.
  DATA lo_f4_as_dirs_sscr TYPE REF TO lcl_f4_as_dirs_sscr.
  lcl_dynp_values=>read_single_field( 'TEMPDIR' ).
  CREATE OBJECT lo_f4_as_dirs_sscr.
  CALL METHOD lo_f4_as_dirs_sscr->display
    EXPORTING
      parent          = cl_gui_container=>screen2
      start_path      = tempdir
    IMPORTING
      selected_folder = tempdir.
  lcl_dynp_values=>update_single_field( name = 'TEMPDIR' value = tempdir ).


*&---------------------------------------------------------------------*
*&      Form  f4_logical_file
*&---------------------------------------------------------------------*
FORM f4_logical_file
      USING tlf tlog_args tlf_path tlf_actu
            lf log_args lf_path dir.

  PERFORM start_lf_search_help USING tlf tlog_args CHANGING lf log_args.
  PERFORM get_lf_fields USING lf log_args CHANGING lf_path dir.
  PERFORM transfer_lf_screen_fields USING tlf tlog_args tlf_path tlf_actu
                                          lf log_args lf_path dir.

ENDFORM."


*&---------------------------------------------------------------------*
*&      Form  start_lf_search_help
*&---------------------------------------------------------------------*
FORM start_lf_search_help USING tlf tlog_args CHANGING lf log_args.

  DATA: lt_ddshretval TYPE rsdm_f4_return_values,
        lt_dynpfield  TYPE TABLE OF dynpread,
        ls_dynpfield  TYPE dynpread.
  FIELD-SYMBOLS:
        <ls_ddshretval>   TYPE ddshretval.

  REFRESH lt_dynpfield.
  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlf.
  APPEND ls_dynpfield TO lt_dynpfield.
  ls_dynpfield-fieldname  = tlog_args.
  APPEND ls_dynpfield TO lt_dynpfield.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfield
    EXCEPTIONS
      OTHERS     = 9.
  CHECK sy-subrc = 0.

  READ TABLE lt_dynpfield WITH KEY fieldname = tlf INTO ls_dynpfield.
  CHECK sy-subrc = 0.
  lf = ls_dynpfield-fieldvalue.

  READ TABLE lt_dynpfield WITH KEY fieldname = tlog_args INTO ls_dynpfield.
  CHECK sy-subrc = 0.
  log_args = ls_dynpfield-fieldvalue.


  " selection_screen  = 'X' : value table has to be used when no other value help is defined
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = 'FILENAMECI'
      fieldname         = 'FILEINTERN'
      dynpprog          = sy-repid
      dynpnr            = sy-dynnr
      dynprofield       = tlf
      selection_screen  = 'X'
    TABLES
      return_tab        = lt_ddshretval
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.
  CHECK sy-subrc = 0.


  READ TABLE lt_ddshretval INDEX 1 ASSIGNING <ls_ddshretval>.
  CHECK sy-subrc = 0.
  lf = <ls_ddshretval>-fieldval.

ENDFORM."

*&---------------------------------------------------------------------*
*&      Form  start_shlp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_SHLPNAME     text
*      -->I_MULTISEL     text
*      -->ET_DDSHRETVAL  text
*----------------------------------------------------------------------*
FORM start_shlp USING i_shlpname TYPE shlpname i_multisel TYPE ddshf4ctrl-multisel
                CHANGING et_ddshretval TYPE rsdm_f4_return_values.
  DATA ls_shlp TYPE shlp_descr.
  FIELD-SYMBOLS <ls_fieldprop> TYPE ddshfprop.
  DATA ls_interface TYPE ddshiface.
  DATA l_rc TYPE sysubrc.

  REFRESH et_ddshretval.
  CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
    EXPORTING
      shlpname = i_shlpname
    IMPORTING
      shlp     = ls_shlp.

  " quels paramètres on souhaite récupérer? ici, tous ceux possibles.
  LOOP AT ls_shlp-fieldprop ASSIGNING <ls_fieldprop> WHERE shlpoutput = 'X'.
    ls_interface-f4field    = 'X'.
    ls_interface-valtabname = 'DUMMY'.
    ls_interface-valfield   = 'DUMMY'.
    MODIFY ls_shlp-interface FROM ls_interface TRANSPORTING f4field valtabname valfield WHERE shlpfield = <ls_fieldprop>-fieldname.
  ENDLOOP.

  CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
    EXPORTING
      shlp          = ls_shlp
      multisel      = i_multisel
    IMPORTING
      rc            = l_rc
    TABLES
      return_values = et_ddshretval.
ENDFORM."

*&---------------------------------------------------------------------*
*&      Form  f4_select_frontend_file
*&---------------------------------------------------------------------*
FORM f4_select_frontend_file CHANGING usrfprg.
  DATA:
    lt_filetable TYPE filetable,
    l_rc         TYPE i,
    l_action     TYPE i.
  FIELD-SYMBOLS:
        <ls_file> TYPE file_table.


  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = lt_filetable
      rc                      = l_rc
      user_action             = l_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc NE 0.
    " TODO
  ELSEIF l_action NE cl_gui_frontend_services=>action_ok.
    " Télé-chargement annulé par l'utilisateur
  ELSE.
    " 1 ou plusieurs fichiers sélectionnés
    READ TABLE lt_filetable INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc = 0.
      usrfprg = <ls_file>-filename.
    ENDIF.
  ENDIF.

ENDFORM."


*&---------------------------------------------------------------------*
*&      Form  f4_select_frontend_dir
*&---------------------------------------------------------------------*
FORM f4_select_frontend_dir CHANGING directory TYPE csequence.
  DATA: l_folder2      TYPE string,
        l_dummy_string TYPE string.

  l_folder2 = directory.
  IF l_folder2 IS INITIAL.
    l_folder2 = '\'.
  ENDIF.
  l_dummy_string = 'Choose a folder'(032).
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = l_dummy_string
      initial_folder       = l_folder2
    CHANGING
      selected_folder      = l_folder2
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    " TODO
  ENDIF.
  IF l_folder2 IS INITIAL.
    " cancelled dialog
  ELSE.
    " folder was selected
    directory = l_folder2.
  ENDIF.
ENDFORM."

*&---------------------------------------------------------------------*
*&      Form  get_lf_fields
*&---------------------------------------------------------------------*
FORM get_lf_fields USING lf log_args CHANGING lf_path dir.
  DATA: l_string      TYPE string,
        lt_field      TYPE tihttpnvp,
        ls_filenameci TYPE filenameci,
        ls_opsystem   TYPE opsystem,
        ls_path       TYPE path,
        l_parameter_1 TYPE ihttpnvp-value,
        l_parameter_2 TYPE ihttpnvp-value,
        l_parameter_3 TYPE ihttpnvp-value,
        l_file_format TYPE filename-fileformat,
        l_file_name   TYPE string,
        l_file_path   TYPE string.
  FIELD-SYMBOLS:
        <ls_field>    TYPE ihttpnvp.

  l_string = log_args.
  lt_field = cl_http_utility=>string_to_fields( l_string ).


  SELECT SINGLE * FROM filenameci INTO ls_filenameci WHERE fileintern = lf.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SELECT SINGLE * FROM opsystem INTO ls_opsystem WHERE opsys = sy-opsys.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM path INTO ls_path
          WHERE pathintern = ls_filenameci-pathintern
            AND filesys = ls_opsystem-filesys.
    IF sy-subrc = 0.
      lf_path = ls_path-pathintern.
    ENDIF.
  ENDIF.

  LOOP AT lt_field ASSIGNING <ls_field> WHERE name CA '1'.
    l_parameter_1 = <ls_field>-value.
  ENDLOOP.
  LOOP AT lt_field ASSIGNING <ls_field> WHERE name CA '2'.
    l_parameter_2 = <ls_field>-value.
  ENDLOOP.
  LOOP AT lt_field ASSIGNING <ls_field> WHERE name CA '3'.
    l_parameter_3 = <ls_field>-value.
  ENDLOOP.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = lf
      parameter_1      = l_parameter_1
      parameter_2      = l_parameter_2
      parameter_3      = l_parameter_3
      eleminate_blanks = abap_false
    IMPORTING
      file_format      = l_file_format
      file_name        = l_file_name
    EXCEPTIONS
      file_not_found   = 1.

  IF l_file_format = 'DIR'.
    dir = l_file_name.
  ELSE.
    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name = l_file_name
      IMPORTING
        file_path = l_file_path.
    dir = l_file_path.
  ENDIF.

ENDFORM."

*&---------------------------------------------------------------------*
*&      Form  transfer_lf_screen_fields
*&---------------------------------------------------------------------*
FORM transfer_lf_screen_fields
      USING
        tlf tlog_args tlf_path tlf_actu
        lf log_args lf_path lf_actu.

  DATA: lt_dynpfield TYPE TABLE OF dynpread,
        ls_dynpfield TYPE dynpread.


  REFRESH lt_dynpfield.

  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlf.
  ls_dynpfield-fieldvalue = lf.
  APPEND ls_dynpfield TO lt_dynpfield.

  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlog_args.
  ls_dynpfield-fieldvalue = log_args.
  APPEND ls_dynpfield TO lt_dynpfield.

  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlf_path.
  ls_dynpfield-fieldvalue = lf_path.
  APPEND ls_dynpfield TO lt_dynpfield.

  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlf_actu.
  ls_dynpfield-fieldvalue = lf_actu.
  APPEND ls_dynpfield TO lt_dynpfield.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfield
    EXCEPTIONS
      OTHERS     = 8.

ENDFORM."




*&---------------------------------------------------------------------*
*&      Form  f4_logical_path
*&---------------------------------------------------------------------*
FORM f4_logical_path
      USING tlp tlog_args tlf_actu
            lp log_args dir.

  PERFORM start_lp_search_help USING tlp tlog_args CHANGING lp log_args.
  PERFORM get_lp_fields USING lp log_args CHANGING dir.
  PERFORM transfer_lp_screen_fields USING tlp tlog_args tlf_actu
                                          lp log_args dir.

ENDFORM."


*&---------------------------------------------------------------------*
*&      Form  start_lp_search_help
*&---------------------------------------------------------------------*
FORM start_lp_search_help USING tlp tlog_args CHANGING lp log_args.

  DATA: lt_ddshretval TYPE rsdm_f4_return_values,
        lt_dynpfield  TYPE TABLE OF dynpread,
        ls_dynpfield  TYPE dynpread.
  FIELD-SYMBOLS:
        <ls_ddshretval>   TYPE ddshretval.

  REFRESH lt_dynpfield.
  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlp.
  APPEND ls_dynpfield TO lt_dynpfield.
  ls_dynpfield-fieldname  = tlog_args.
  APPEND ls_dynpfield TO lt_dynpfield.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfield
    EXCEPTIONS
      OTHERS     = 9.
  CHECK sy-subrc = 0.

  READ TABLE lt_dynpfield WITH KEY fieldname = tlp INTO ls_dynpfield.
  CHECK sy-subrc = 0.
  lp = ls_dynpfield-fieldvalue.

  READ TABLE lt_dynpfield WITH KEY fieldname = tlog_args INTO ls_dynpfield.
  CHECK sy-subrc = 0.
  log_args = ls_dynpfield-fieldvalue.


  " selection_screen  = 'X' : value table has to be used when no other value help is defined
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = 'PATH'
      fieldname         = 'PATHINTERN'
      dynpprog          = sy-repid
      dynpnr            = sy-dynnr
      dynprofield       = tlp
      selection_screen  = 'X'
    TABLES
      return_tab        = lt_ddshretval
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.
  CHECK sy-subrc = 0.


  READ TABLE lt_ddshretval INDEX 1 ASSIGNING <ls_ddshretval>.
  CHECK sy-subrc = 0.
  lp = <ls_ddshretval>-fieldval.

ENDFORM."

*&---------------------------------------------------------------------*
*&      Form  get_lp_fields
*&---------------------------------------------------------------------*
FORM get_lp_fields USING lp log_args CHANGING dir.
  DATA: l_string      TYPE string,
        lt_field      TYPE tihttpnvp,
        ls_path       TYPE path,
        ls_opsystem   TYPE opsystem,
        l_parameter_1 TYPE ihttpnvp-value,
        l_parameter_2 TYPE ihttpnvp-value,
        l_parameter_3 TYPE ihttpnvp-value,
        l_file_name   TYPE string,
        l_file_path   TYPE string.
  FIELD-SYMBOLS:
        <ls_field>    TYPE ihttpnvp.


  SELECT SINGLE * FROM opsystem INTO ls_opsystem WHERE opsys = sy-opsys.
  CHECK sy-subrc = 0.

  SELECT SINGLE * FROM path INTO ls_path WHERE pathintern = lp
            AND filesys = ls_opsystem-filesys.
  CHECK sy-subrc = 0.


  l_string = log_args.
  lt_field = cl_http_utility=>string_to_fields( l_string ).

  LOOP AT lt_field ASSIGNING <ls_field> WHERE name CA '1'.
    l_parameter_1 = <ls_field>-value.
  ENDLOOP.
  LOOP AT lt_field ASSIGNING <ls_field> WHERE name CA '2'.
    l_parameter_2 = <ls_field>-value.
  ENDLOOP.
  LOOP AT lt_field ASSIGNING <ls_field> WHERE name CA '3'.
    l_parameter_3 = <ls_field>-value.
  ENDLOOP.


  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
      logical_path               = lp
      parameter_1                = l_parameter_1
      parameter_2                = l_parameter_2
      parameter_3                = l_parameter_3
      file_name                  = 'any name' "L_file_name
      eleminate_blanks           = abap_false
    IMPORTING
      file_name_with_path        = l_file_name
    EXCEPTIONS
      path_not_found             = 1
      missing_parameter          = 2
      operating_system_not_found = 3
      file_system_not_found      = 4.
  IF sy-subrc <> 0.
    " TODO
  ENDIF.

  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name = l_file_name
    IMPORTING
      file_path = l_file_path.

  dir = l_file_path.

ENDFORM."

*&---------------------------------------------------------------------*
*&      Form  transfer_lp_screen_fields
*&---------------------------------------------------------------------*
FORM transfer_lp_screen_fields
      USING
        tlp tlog_args tlp_actu
        lp log_args lp_actu.

  DATA: lt_dynpfield TYPE TABLE OF dynpread,
        ls_dynpfield TYPE dynpread.


  REFRESH lt_dynpfield.

  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlp.
  ls_dynpfield-fieldvalue = lp.
  APPEND ls_dynpfield TO lt_dynpfield.

  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlog_args.
  ls_dynpfield-fieldvalue = log_args.
  APPEND ls_dynpfield TO lt_dynpfield.

  CLEAR ls_dynpfield.
  ls_dynpfield-fieldname  = tlp_actu.
  ls_dynpfield-fieldvalue = lp_actu.
  APPEND ls_dynpfield TO lt_dynpfield.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfield
    EXCEPTIONS
      OTHERS     = 8.
  IF sy-subrc <> 0.
    " TODO
  ENDIF.

ENDFORM."


*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

    DATA ls_sscr_text TYPE ty_is_sscr_text.
    DATA ls_sscr_title TYPE ty_is_sscr_title.

    DEFINE mac_sscr_text.
      CLEAR ls_sscr_text.
      ls_sscr_text-dynnr   = &1.
      ls_sscr_text-selname = &2.
      ls_sscr_text-seltext = &3.
      APPEND ls_sscr_text TO ait_sscr_text.
    END-OF-DEFINITION.

    DEFINE mac_sscr_text2.
      mac_sscr_text:
        &1 'PRG&2TEXT' 'Text in contextual menu'(051),
        &1 'PRG&2NAME' 'Program name'(052),
        &1 'PRG&2VARI' 'Program variant'(053),
        &1 'PRG&2PFFP' 'Parameter frontend full path'(054),
        &1 'PRG&2PAFP' 'Parameter app.srv. full path'(055),
        &1 'PRG&2PLF'  'Parameter logical file'(056),
        &1 'PRG&2PLP'  'Parameter logical path'(057).
    END-OF-DEFINITION.

    REFRESH ait_sscr_title.

    "----------------------
    " Initial screen
    "----------------------
    CLEAR ls_sscr_title.
    ls_sscr_title-dynnr = '1000'.
    ls_sscr_title-title = 'File Manager'(036).
    APPEND ls_sscr_title TO ait_sscr_title.

    text_b01 = 'Directory 1'(b01).
    text_b02 = 'Directory 2'(b02).
    text_b03 = 'Directory 3'(b03).
    text_as1 = text_as2 = text_as3 = 'Application Server'(as_).
    text_hi1 = text_hi2 = text_hi3 = 'Display hierarchy'(hi_).

    text_bp8 = 'ABAP program 1'(bp8).
    text_bp9 = 'ABAP program 2'(bp9).

    mac_sscr_text:
      '1000' 'DIRTYPE1' 'Type of directory'(043),
      '1000' 'DIRTYPE2' 'Type of directory'(043),
      '1000' 'DIRTYPE3' 'Type of directory'(043),
      '1000' 'DIR1'     'Directory'(044),
      '1000' 'DIR2'     'Directory'(044),
      '1000' 'DIR3'     'Directory'(044),
      '1000' 'LF1'      'Logical file'(045),
      '1000' 'LF2'      'Logical file'(045),
      '1000' 'LF3'      'Logical file'(045),
      '1000' 'LP1'      'Logical path'(046),
      '1000' 'LP2'      'Logical path'(046),
      '1000' 'LP3'      'Logical path'(046),
      '1000' 'GU1'      'SAP GUI directory'(047),
      '1000' 'GU2'      'SAP GUI directory'(047),
      '1000' 'GU3'      'SAP GUI directory'(047),
      '1000' 'LOGARGS1' 'Arguments logical fi./pa.'(048),
      '1000' 'LOGARGS2' 'Arguments logical fi./pa.'(048),
      '1000' 'LOGARGS3' 'Arguments logical fi./pa.'(048),
      '1000' 'SD1'      'SAP directory DIR_'(049),
      '1000' 'SD2'      'SAP directory DIR_'(049),
      '1000' 'SD3'      'SAP directory DIR_'(049).

    mac_sscr_text2 '1000' : 8, 9.


    "----------------------
    " General settings
    "----------------------
    CLEAR ls_sscr_title.
    ls_sscr_title-dynnr = '1006'.
    ls_sscr_title-title = 'General settings'(037).
    APPEND ls_sscr_title TO ait_sscr_title.

    tabstrip-prog = sy-repid.
    tabstrip-dynnr = '1008'.
    tabstrip-activetab = 'UCOMM_TAB1'.

    tab1titl = 'UNIX commands'(068).
    tab5titl = 'WINDOWS commands'(094).
    tab2titl = 'Icons'(069).
    tab3titl = 'General actions'(070).
    tab4titl = 'Other settings'(078).

    text_bsm = 'Move'(bsm).
    text_bsc = 'Copy'(bsc).
    text_bsf = 'Command for file pre-creation'(bsf).
    text_bfs = 'File commands'(bfs).
    text_bds = 'Directory commands'(bds).
    text_bcs = 'Copy directory'(bcs).

    text_bfm = 'Move'(bfm).
    text_bfc = 'Copy'(bfc).
    text_bff = 'File commands'(bff).
    text_bdf = 'Directory commands'(bdf).
    text_bcf = 'Copy directory'(bcf).

    text_bp1 = 'ABAP program'(bp1).
    text_bp2 = 'ABAP program'(bp2).
    text_bp3 = 'ABAP program'(bp3).

    text_d01 = 'Copy selected directory/ies'(d01).
    text_d02 = 'Subdirectories'(d02).
    text_d03 = 'Files'(d03).
    text_d04 = 'Yes'(d04).
    text_d05 = 'No'(d05).
    text_d06 = 'Yes'(d06).
    text_d07 = 'No'(d07).

    fileexth = 'File name (regular expr.)'(ice).
    filetxth = 'Basic text'(ict).
    icodexth = 'Icon'(icl).
    iconexth = 'Icon name'(icn).

    mac_sscr_text:
      '1008' 'XPGSMOVE' 'Command'(058),
      '1008' 'XPGSMOV3' 'Argument if overwrite needed'(060),
      '1008' 'XPGSCOPY' 'Command'(058),
      '1008' 'XPGSCPY3' 'Argument if overwrite needed'(060),
      '1008' 'XPGSMKDI' 'Command for creating directory'(080),
      '1008' 'XPGSRMDI' 'Command for removing directory'(107),
      '1008' 'XPGSMKF0' 'Execute commands'(061),
      '1008' 'XPGSMKF1' 'First command'(062),
      '1008' 'XPGSMKF2' 'Second command (optional)'(063),
      '1008' 'XPGSCPY4' 'With files and subdirectories'(095),
      '1008' 'XPGSCPY5' 'With files, W/O subdirectories'(096),
      '1008' 'XPGSCPY6' 'W/O files, with subdirectories'(097),
      '1008' 'XPGSCPY7' '',
      '1008' 'XPGSCPY8' ''.

    mac_sscr_text:
      '1014' 'XPGFMOVE' 'Command'(058),
      '1014' 'XPGFMOV3' 'Argument if overwrite needed'(060),
      '1014' 'XPGFCOPY' 'Command'(058),
      '1014' 'XPGFCPY3' 'Argument if overwrite needed'(060),
      '1014' 'XPGFMKDI' 'Command for creating directory'(080),
      '1014' 'XPGFRMDI' 'Command for removing directory'(107),
      '1014' 'XPGFFPSP' 'With files and subdirectories'(095),
      '1014' 'XPGFFPSM' 'With files, W/O subdirectories'(096),
      '1014' 'XPGFFMSP' 'W/O files, with subdirectories'(097).

    mac_sscr_text2 '1010' : 1, 2, 3, 4, 5, 6, 7.

    mac_sscr_text:
      '1012' 'TEMPDIR'  'Temporary directory for edit'(077),
      '1012' 'XPGFCMDC' 'Command on Windows frontend'(099),
      '1012' 'XPGFCMDP' 'Argument on Windows frontend'(100).


    "----------------------
    " User settings
    "----------------------
    CLEAR ls_sscr_title.
    ls_sscr_title-dynnr = '1011'.
    ls_sscr_title-title = 'User settings'(038).
    APPEND ls_sscr_title TO ait_sscr_title.

    " TXTCOPY : A = always, N = never, D = ask (dialog)
    txtcopy = 'A'.                                          "#EC NOTEXT

    usrfilet = 'Unique settings file at frontend'(111).

    text_bdm = 'Directory mapping'(116).
    dirfindh = 'Application server (regex)'(117).
    dirreplh = 'Frontend equivalence (regex)'(118).
    text_bua = 'User applications'(115).
    usrfhtxt = 'Text in contextual menu'(113).
    usrfhprg = 'Program on frontend'(114).

    mac_sscr_text:
*      '1011' 'USRINPUT' 'Free input'(112),
*      '1011' 'USRCONF'  'Frontend file saving settings'(064),
      '1011' 'OWNDIREC' 'Own directory in own window'(065),
      '1011' 'AUTOTEMP' 'Use temp folder without dialog'(079),
      '1011' 'ALWCONFI' 'Always display confirm. dialog'(066),
      '1011' 'TXTCOPY'  'Conv CR+LF <-> LF during copy'(067),
      '1011' 'SETTVAR'  'Variant containing the configuration'(050).


    "----------------------
    " Dialogs for copy/move
    "----------------------
    CLEAR ls_sscr_title.
    ls_sscr_title-dynnr = '1002'.
    ls_sscr_title-title = 'Move confirmation'(039).
    APPEND ls_sscr_title TO ait_sscr_title.
    CLEAR ls_sscr_title.
    ls_sscr_title-dynnr = '1003'.
    ls_sscr_title-title = 'Copy confirmation'(040).
    APPEND ls_sscr_title TO ait_sscr_title.
    CLEAR ls_sscr_title.
    ls_sscr_title-dynnr = '1004'.
    ls_sscr_title-title = 'Rename confirmation'(041).
    APPEND ls_sscr_title TO ait_sscr_title.
    CLEAR ls_sscr_title.
    ls_sscr_title-dynnr = '1005'.
    ls_sscr_title-title = 'Delete confirmation'(042).
    APPEND ls_sscr_title TO ait_sscr_title.


    "----------------------
    " Dialog for directory copy/move
    "----------------------
    mac_sscr_text:
      '1013' 'P_FPSP'   'With files and subdirectories'(095),
      '1013' 'P_FPSM'   'With files, W/O subdirectories'(096),
      '1013' 'P_FMSP'   'W/O files, with subdirectories'(097),
      '1013' 'P_FMSM'   'W/O files, W/O subdirectories'(090).


    "----------------------
    " Instantiations
    "----------------------
    CREATE OBJECT aio_frontend.
    CREATE OBJECT aio_appserv.
    CREATE OBJECT aio_dir_move.
    CREATE OBJECT aio_dir_copy.

    CREATE OBJECT aio_confirm_move.
    CREATE OBJECT aio_confirm_copy.
    CREATE OBJECT aio_confirm_rename.
    CREATE OBJECT aio_confirm_delete.
    CREATE OBJECT aio_initial_screen.
    CREATE OBJECT aio_detail_screen.
    CREATE OBJECT aio_config_screen.
    CREATE OBJECT aio_config_unix_screen.
    CREATE OBJECT aio_config_windows_screen.
    CREATE OBJECT aio_config_icon_screen.
    CREATE OBJECT aio_config_prog_screen.
    CREATE OBJECT aio_config_misc_screen.
    CREATE OBJECT aio_config_user_screen.

    lif_sscr_h~set_sscr_handler( sscr = '1000' handler = aio_initial_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1001' handler = aio_detail_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1002' handler = aio_confirm_move ).
    lif_sscr_h~set_sscr_handler( sscr = '1003' handler = aio_confirm_copy ).
    lif_sscr_h~set_sscr_handler( sscr = '1004' handler = aio_confirm_rename ).
    lif_sscr_h~set_sscr_handler( sscr = '1005' handler = aio_confirm_delete ).
    lif_sscr_h~set_sscr_handler( sscr = '1006' handler = aio_config_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1008' handler = aio_config_unix_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1014' handler = aio_config_windows_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1009' handler = aio_config_icon_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1010' handler = aio_config_prog_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1012' handler = aio_config_misc_screen ).
    lif_sscr_h~set_sscr_handler( sscr = '1011' handler = aio_config_user_screen ).

    "-----------------------
    " USER SETTINGS
    "-----------------------
    lcl_user_settings=>load_variant_and_file( ).

  ENDMETHOD."


  METHOD lif_sscr_h~set_sscr_handler.
    DATA: ls_sscr     TYPE lif_sscr_h~ty_s_sscr.
    FIELD-SYMBOLS: <ls_sscr> TYPE lif_sscr_h~ty_s_sscr.

    READ TABLE lif_sscr_h~at_sscr WITH KEY sscr = sscr ASSIGNING <ls_sscr>.
    IF sy-subrc <> 0.
      ls_sscr-sscr      = sscr.
      ls_sscr-o_handler = handler.
      APPEND ls_sscr TO lif_sscr_h~at_sscr.
    ELSE.
      <ls_sscr>-o_handler = handler.
    ENDIF.
  ENDMETHOD."


  METHOD lif_sscr~pbo.
    DATA:
      l_fieldname TYPE fieldname,
      lx_root     TYPE REF TO cx_root,
      l_text      TYPE string.
    FIELD-SYMBOLS:
      <ls_sscr>       TYPE lif_sscr_h~ty_s_sscr,
      <ls_sscr_title> TYPE ty_is_sscr_title,
      <ls_sscr_text>  TYPE ty_is_sscr_text,
      <l_field>       TYPE clike.

    TRY.

        READ TABLE ait_sscr_title ASSIGNING <ls_sscr_title> WITH KEY dynnr = sy-dynnr.
        IF sy-subrc = 0.
          SET TITLEBAR 'TIT' OF PROGRAM 'SAPLDSYP' WITH <ls_sscr_title>-title.
        ENDIF.

        " in case the selection texts (text pool) are not defined
        "   (fast distribution of the tool)
        LOOP AT ait_sscr_text ASSIGNING <ls_sscr_text>
              WHERE dynnr = sy-dynnr.
          CONCATENATE '%_' <ls_sscr_text>-selname '_%_APP_%-TEXT' INTO l_fieldname.
          ASSIGN (l_fieldname) TO <l_field>.
          IF sy-subrc = 0 AND <l_field> = <ls_sscr_text>-selname.
            <l_field> = <ls_sscr_text>-seltext.
          ENDIF.
        ENDLOOP.

        READ TABLE lif_sscr_h~at_sscr ASSIGNING <ls_sscr> WITH KEY sscr = sy-dynnr.
        IF sy-subrc = 0.
          <ls_sscr>-o_handler->pbo( ).
        ENDIF.

      CATCH cx_root INTO lx_root.
        l_text = lx_root->get_text( ).
        MESSAGE l_text TYPE 'S'.
    ENDTRY.

  ENDMETHOD."


  METHOD lif_sscr~pai.
    DATA: lx_root TYPE REF TO cx_root,
          l_text  TYPE string.
    FIELD-SYMBOLS:
          <ls_sscr>   TYPE lif_sscr_h~ty_s_sscr.

    TRY.
        READ TABLE lif_sscr_h~at_sscr ASSIGNING <ls_sscr> WITH KEY sscr = sy-dynnr.
        IF sy-subrc = 0.
          <ls_sscr>-o_handler->pai( ).
        ENDIF.
      CATCH cx_root INTO lx_root.
        l_text = lx_root->get_text( ).
        MESSAGE l_text TYPE 'E'.
    ENDTRY.
  ENDMETHOD."


  METHOD lif_sscr~exit.
    FIELD-SYMBOLS:
          <ls_sscr>   TYPE lif_sscr_h~ty_s_sscr.
    READ TABLE lif_sscr_h~at_sscr ASSIGNING <ls_sscr> WITH KEY sscr = sy-dynnr.
    IF sy-subrc = 0.
      <ls_sscr>-o_handler->exit( ).
    ENDIF.
  ENDMETHOD."


  METHOD listbox_text_mode.
    DATA: lt_value TYPE vrm_values,
          ls_value TYPE vrm_value.
    ls_value-key = 'N'.                                     "#EC NOTEXT
    ls_value-text = 'Never'(071).
    APPEND ls_value TO lt_value.
    ls_value-key = 'A'.                                     "#EC NOTEXT
    ls_value-text = 'Always'(072).
    APPEND ls_value TO lt_value.
    ls_value-key = 'D'.                                     "#EC NOTEXT
    ls_value-text = 'Ask'(073).
    APPEND ls_value TO lt_value.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'TXTCOPY'
        values          = lt_value
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD."


  METHOD listbox_dir_type.
    DATA: lt_value TYPE vrm_values,
          ls_value TYPE vrm_value.

    ls_value-key = 'F'.
    ls_value-text = 'Free input'(dtf).
    APPEND ls_value TO lt_value.
    ls_value-key = 'L'.
    ls_value-text = 'Logical file'(dtl).
    APPEND ls_value TO lt_value.
    ls_value-key = 'P'.
    ls_value-text = 'Logical path'(dtp).
    APPEND ls_value TO lt_value.
    ls_value-key = 'S'.
    ls_value-text = 'SAP directory'(dts).
    APPEND ls_value TO lt_value.
    ls_value-key = 'G'.
    ls_value-text = 'SAP GUI directory'(dtg).
    APPEND ls_value TO lt_value.
    ls_value-key = 'N'.
    ls_value-text = 'None'(dtn).
    APPEND ls_value TO lt_value.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = screen_field_name
        values          = lt_value
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD."


  METHOD listbox_gui.
    DATA: lt_value TYPE vrm_values,
          ls_value TYPE vrm_value.

    ls_value-key = 'D'.
    ls_value-text = 'Desktop'(gud).
    APPEND ls_value TO lt_value.
    ls_value-key = 'R'.
    ls_value-text = 'Root'(gur).
    APPEND ls_value TO lt_value.
    ls_value-key = 'W'.
    ls_value-text = 'Work directory'(guw).
    APPEND ls_value TO lt_value.
    ls_value-key = 'S'.
    ls_value-text = 'System directory'(gus).
    APPEND ls_value TO lt_value.
    ls_value-key = 'T'.
    ls_value-text = 'Temp directory'(gut).
    APPEND ls_value TO lt_value.
    ls_value-key = 'O'.
    ls_value-text = 'Windows directory'(guo).
    APPEND ls_value TO lt_value.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = screen_field_name
        values          = lt_value
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD."


  METHOD f4_directory2.
  ENDMETHOD."


  METHOD f4_directory.
    DATA: dynp_values    TYPE REF TO lcl_dynp_values,
          l_appserv      TYPE abap_bool,
          l_folder       TYPE string,
          l_folder2      TYPE string,
          l_dummy_string TYPE string.

    CREATE OBJECT dynp_values.
    dynp_values->add_field( EXPORTING name = screen_field_name CHANGING value = l_folder ).
    dynp_values->add_field( EXPORTING name = appserv_field_name CHANGING value = l_appserv ).
    dynp_values->read( ).

    CASE l_appserv.
      WHEN abap_true.
        CREATE OBJECT aio_f4_as_dirs_sscr.
        CALL METHOD aio_f4_as_dirs_sscr->display
          EXPORTING
            parent          = cl_gui_container=>screen1
            start_path      = l_folder
          IMPORTING
            selected_folder = l_folder.
      WHEN abap_false.
        l_folder2 = l_folder.
        IF l_folder2 IS INITIAL.
          l_folder2 = '\'.
        ENDIF.
        l_dummy_string = 'Choose a folder'(032).
        CALL METHOD cl_gui_frontend_services=>directory_browse
          EXPORTING
            window_title         = l_dummy_string
            initial_folder       = l_folder2
          CHANGING
            selected_folder      = l_folder2
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.
        IF sy-subrc <> 0.
          " TODO
        ENDIF.
        IF l_folder2 IS INITIAL.
          " cancelled dialog
        ELSE.
          " folder was selected
          l_folder = l_folder2.
        ENDIF.
    ENDCASE.


    CREATE OBJECT dynp_values.
    dynp_values->add_field( EXPORTING name = screen_field_name CHANGING value = l_folder ).
    dynp_values->update( ).


  ENDMETHOD."


  METHOD f4_sap_directory.
    TYPES : BEGIN OF ty_ls_dir,
              name    TYPE user_dir-aliass,
              text    TYPE tpfypropty-descr,
              type    TYPE char1,
              svrname TYPE btcsrvname,
            END OF ty_ls_dir.
    DATA: lt_tpfypropty TYPE TABLE OF tpfypropty,
          ls_dir        TYPE ty_ls_dir,
          lt_dir        TYPE TABLE OF ty_ls_dir,
          l_host(120)   TYPE c,
          lt_user_dir   TYPE TABLE OF user_dir,
          lo_elem       TYPE REF TO cl_abap_elemdescr,
          ls_field      TYPE dfies,
          lt_field      TYPE TABLE OF dfies,
          lt_ddshretval TYPE rsdm_f4_return_values,
          lt_dynpfield  TYPE TABLE OF dynpread,
          ls_dynpfield  TYPE dynpread.
    FIELD-SYMBOLS:
      <ls_tpfypropty> TYPE tpfypropty,
      <ls_user_dir>   TYPE user_dir,
      <ls_dir>        TYPE ty_ls_dir,
      <ls_ddshretval> TYPE ddshretval.


    SELECT * FROM tpfypropty INTO TABLE lt_tpfypropty
          WHERE obj_name LIKE 'DIR#_%' ESCAPE '#'.
    LOOP AT lt_tpfypropty ASSIGNING <ls_tpfypropty>.
      ls_dir-name = <ls_tpfypropty>-obj_name.
      ls_dir-text = <ls_tpfypropty>-descr.
      ls_dir-type = 'S'.
      APPEND ls_dir TO lt_dir.
    ENDLOOP.

    CALL 'C_SAPGPARAM'
          ID 'NAME'  FIELD 'rdisp/myname'
          ID 'VALUE' FIELD l_host.

    SELECT * FROM user_dir INTO TABLE lt_user_dir
          WHERE svrname = l_host
            OR  svrname = 'all'.
    LOOP AT lt_user_dir ASSIGNING <ls_user_dir>.
      ls_dir-name = <ls_user_dir>-aliass.
      ls_dir-text = <ls_user_dir>-dirname.
      ls_dir-type = 'C'.
      ls_dir-svrname = <ls_user_dir>-svrname.
      APPEND ls_dir TO lt_dir.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = sapdir_scrfname " pour filtrer la liste affichée selon la valeur actuelle
        retfield    = 'NAME'                                "#EC NOTEXT
        value_org   = 'S'
      TABLES
        value_tab   = lt_dir
        return_tab  = lt_ddshretval.

    READ TABLE lt_ddshretval INDEX 1 ASSIGNING <ls_ddshretval>.
    CHECK sy-subrc = 0.

    REFRESH lt_dynpfield.
    CLEAR ls_dynpfield.
    ls_dynpfield-fieldname  = sapdir_scrfname.
    ls_dynpfield-fieldvalue = <ls_ddshretval>-fieldval.
    APPEND ls_dynpfield TO lt_dynpfield.

    CLEAR ls_dynpfield.
    ls_dynpfield-fieldname = dirfullpath_scrfname.
    ls_dynpfield-fieldvalue = get_sap_directory( <ls_ddshretval>-fieldval ).
    APPEND ls_dynpfield TO lt_dynpfield.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynpfield
      EXCEPTIONS
        OTHERS     = 8.

  ENDMETHOD."


  METHOD f4_icon.
    DATA:
      ls_field      TYPE dfies,
      lt_field      TYPE TABLE OF dfies,
      lt_ddshretval TYPE TABLE OF ddshretval,
      lt_dynpfield  TYPE TABLE OF dynpread,
      ls_dynpfield  TYPE dynpread.
    FIELD-SYMBOLS:
      <ls_ddshretval> TYPE ddshretval.

    PERFORM start_shlp USING 'H_ICON' abap_false CHANGING lt_ddshretval.

    READ TABLE lt_ddshretval INDEX 1 ASSIGNING <ls_ddshretval>.
    CHECK sy-subrc = 0.

    REFRESH lt_dynpfield.
    CLEAR ls_dynpfield.
    ls_dynpfield-fieldname  = icon_scrfname.
    ls_dynpfield-fieldvalue = <ls_ddshretval>-fieldval.
    APPEND ls_dynpfield TO lt_dynpfield.

    CLEAR ls_dynpfield.
    ls_dynpfield-fieldname  = iconname_scrfname.
    SELECT SINGLE name FROM icon INTO ls_dynpfield-fieldvalue
          WHERE id = <ls_ddshretval>-fieldval.
    APPEND ls_dynpfield TO lt_dynpfield.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynpfield
      EXCEPTIONS
        OTHERS     = 8.

  ENDMETHOD."


  METHOD get_sap_gui_directory.
    DATA: l_string TYPE string.

    CASE gu.
      WHEN 'D'.
        l_string = aio_frontend->aus_directory-desktop.
      WHEN 'R'.
        l_string = aio_frontend->aus_directory-sapgui.
      WHEN 'W'.
        l_string = aio_frontend->aus_directory-workdir.
      WHEN 'S'.
        l_string = aio_frontend->aus_directory-system.
      WHEN 'T'.
        l_string = aio_frontend->aus_directory-temp.
      WHEN 'O'.
        l_string = aio_frontend->aus_directory-windows.
    ENDCASE.
    IF logargs IS INITIAL.
      dir = l_string.
    ELSE.
      CONCATENATE l_string '\' logargs INTO dir.
    ENDIF.
  ENDMETHOD."


  METHOD get_sap_directory.
    DATA: l_value(120) TYPE c,
          ls_user_dir  TYPE user_dir.

    CLEAR sap_dir_fullpath.

    IF sap_dir CP 'DIR_*'.
      CALL 'C_SAPGPARAM'
            ID 'NAME' FIELD sap_dir
            ID 'VALUE' FIELD l_value.
      sap_dir_fullpath = l_value.
    ENDIF.

    IF sap_dir_fullpath IS INITIAL.
      CALL 'C_SAPGPARAM'
            ID 'NAME'  FIELD 'rdisp/myname'
            ID 'VALUE' FIELD l_value.
      SELECT SINGLE * FROM user_dir INTO ls_user_dir
            WHERE svrname = l_value
              OR  svrname = 'all'.
      IF sy-subrc = 0.
        sap_dir_fullpath = ls_user_dir-dirname.
      ENDIF.
    ENDIF.

  ENDMETHOD."


  METHOD is_text_file.
    DEFINE mac_is_text_file.
      if fileext&1 is not initial.
        find regex fileext&1 in file_name.
        if sy-subrc = 0.
          is_text_type = filetxt&1.
          return.
        endif.
      endif.
    END-OF-DEFINITION.

    mac_is_text_file: 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f.
    is_text_type = abap_undefined.

  ENDMETHOD."


  METHOD dof_copy.
    IF cs_confirm-o_source_dof->isdir = 0.
      file_copy( CHANGING cs_confirm = cs_confirm ).
    ELSE.
      dir_copy( CHANGING cs_confirm = cs_confirm ).
    ENDIF.
  ENDMETHOD."


  METHOD file_copy.
    DATA:
      lo_target_dof TYPE REF TO lcl_dof,
      l_action(2)   TYPE c,
      lt_text       TYPE string_table,
      l_binary      TYPE xstring.

    IF cs_confirm-o_target_dof->isdir = 1.
      lo_target_dof = cs_confirm-o_target_dof->get_file( cs_confirm-o_source_dof->filename ).
    ELSE.
      lo_target_dof = cs_confirm-o_target_dof.
    ENDIF.

    l_action = cs_confirm-o_source_dof->file_system->type && lo_target_dof->file_system->type.

    CASE l_action.
      WHEN 'FF'.
        " copy from frontend to frontend
        go_app->aio_frontend->file_copy(  source    = cs_confirm-o_source_dof->fullpath
                                          target    = lo_target_dof->fullpath
                                          overwrite = cs_confirm-overwrite
                                          binary    = cs_confirm-binary ).
      WHEN 'SS'.
        " copy from application server to application server
        go_app->aio_appserv->file_copy(   source    = cs_confirm-o_source_dof->fullpath
                                          target    = lo_target_dof->fullpath
                                          overwrite = cs_confirm-overwrite
                                          binary    = cs_confirm-binary ).
      WHEN 'FS'.
        " copy from frontend to application server
        CASE cs_confirm-binary.
          WHEN abap_false.
            go_app->aio_frontend->file_read_text( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = lt_text ).
            go_app->aio_appserv->file_write_text( EXPORTING fullpath = lo_target_dof->fullpath content = lt_text ).
          WHEN abap_true.
            go_app->aio_frontend->file_read_binary( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = l_binary ).
            go_app->aio_appserv->file_write_binary( EXPORTING fullpath = lo_target_dof->fullpath content = l_binary ).
        ENDCASE.
      WHEN 'SF'.
        " copy from application server to frontend
        CASE cs_confirm-binary.
          WHEN abap_false.
            go_app->aio_appserv->file_read_text( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = lt_text ).
            go_app->aio_frontend->file_write_text( EXPORTING fullpath = lo_target_dof->fullpath content = lt_text ).
          WHEN abap_true.
            go_app->aio_appserv->file_read_binary( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = l_binary ).
            go_app->aio_frontend->file_write_binary( EXPORTING fullpath = lo_target_dof->fullpath content = l_binary ).
        ENDCASE.
    ENDCASE.

    CONCATENATE icon_okay ` ` 'File has been copied' INTO cs_confirm-message.
    cs_confirm-done = abap_true.

  ENDMETHOD."


  METHOD dir_copy.

    DATA:
      l_action(2)   TYPE c,
      l_copy_done   TYPE abap_bool,
      lo_target_dof TYPE REF TO lcl_dof, " l_fullpath type string,
      dref          TYPE REF TO data,
      ls_confirm    TYPE lcl_popup_confirm=>ty_us_confirm.
    FIELD-SYMBOLS:
      <lt_s_file> TYPE lcl_appserv=>ty_gt_file,
      <ls_s_file> TYPE lcl_appserv=>ty_gs_file.


    l_action = cs_confirm-o_source_dof->file_system->type && cs_confirm-o_target_dof->file_system->type.

    l_copy_done = abap_false.

    CASE l_action.
      WHEN 'FF' OR 'SS'.
        CASE aio_dir_copy->choice.
          WHEN aio_dir_copy->fp_sp.
            " Windows: XCOPY "source" "target" /E
            " Unix: cp -r "source" "target"
            " TODO
            l_copy_done = abap_true.
          WHEN aio_dir_copy->fp_sm.
            " Windows: create target directory + COPY "source" "target"
            " Unix: cp "source" "target"
            " TODO
            l_copy_done = abap_true.
          WHEN aio_dir_copy->fm_sp.
            " Windows: XCOPY "source" "target" /T /E
            " Unix: mkdir "target" + cd "source" + find . -type d | cpio -pvdm "target" (source: unix.com)
            "    cpio: p = list from standard input, v = verbose, d = create directories if needed, m = retain file dates
            " TODO
            l_copy_done = abap_true.
          WHEN aio_dir_copy->fm_sm.
            " Windows: create target directory
            " Unix: mkdir "target"
            " TODO
            l_copy_done = abap_true.
        ENDCASE.
    ENDCASE.

    IF l_copy_done = abap_false.
      "------------
      " Step 1: create target directory
      "------------
      CREATE OBJECT lo_target_dof
        EXPORTING
          file_system = cs_confirm-o_target_dof->file_system
          path        = cs_confirm-o_target_dof->fullpath
          filename    = cs_confirm-o_source_dof->dirname
          isdir       = 1.
      cs_confirm-o_target_dof->file_system->directory_create( lo_target_dof->fullpath ).
      "------------
      " Step 2: eventually copy files and subdirectories
      "------------
      IF aio_dir_copy->choice <> aio_dir_copy->fm_sm.
        cs_confirm-o_source_dof->file_system->directory_list_files(
          EXPORTING
            path             = cs_confirm-o_source_dof->fullpath
            depth            = 0
          IMPORTING
            files            = dref
            ).
        ASSIGN dref->* TO <lt_s_file>.
        LOOP AT <lt_s_file> ASSIGNING <ls_s_file>.
          CLEAR ls_confirm.
          CREATE OBJECT ls_confirm-o_source_dof
            EXPORTING
              file_system = cs_confirm-o_source_dof->file_system
              path        = cs_confirm-o_source_dof->fullpath
              filename    = <ls_s_file>-name
              isdir       = <ls_s_file>-isdir.
          ls_confirm-o_target_dof = lo_target_dof.
          CASE <ls_s_file>-isdir.
            WHEN 0.
              CASE aio_dir_copy->choice.
                WHEN aio_dir_copy->fp_sp OR aio_dir_copy->fp_sm.
                  ls_confirm-binary = abap_true. "to avoid code page errors
                  file_copy( CHANGING cs_confirm = ls_confirm ).
              ENDCASE.
            WHEN 1.
              CASE aio_dir_copy->choice.
                WHEN aio_dir_copy->fp_sp OR aio_dir_copy->fm_sp.
                  dir_copy( CHANGING cs_confirm = ls_confirm ).
              ENDCASE.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDIF.

    CONCATENATE icon_okay ` ` 'Directory has been copied' INTO cs_confirm-message.
    cs_confirm-done = abap_true.

  ENDMETHOD."

ENDCLASS."

*
CLASS lcl_dynp_values IMPLEMENTATION.

  METHOD add_field.
    DATA: ls_field TYPE ty_is_field.
    ls_field-name = name.
    ls_field-stepl = stepl.
    GET REFERENCE OF value INTO ls_field-value.
    APPEND ls_field TO ait_field.
  ENDMETHOD."

  METHOD read.
    DATA: lt_dynpfield TYPE TABLE OF dynpread,
          ls_dynpfield TYPE dynpread.
    FIELD-SYMBOLS:
      <ls_field>     TYPE ty_is_field,
      <l_value>      TYPE simple,
      <ls_dynpfield> TYPE dynpread.

    REFRESH lt_dynpfield.
    LOOP AT ait_field ASSIGNING <ls_field>.
      ls_dynpfield-fieldname = <ls_field>-name.
      APPEND ls_dynpfield TO lt_dynpfield.
    ENDLOOP.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
        request    = 'A' "A = lire toutes les zones à l'écran
      TABLES
        dynpfields = lt_dynpfield
      EXCEPTIONS
        OTHERS     = 9.
    IF sy-subrc <> 0. RETURN. ENDIF.

    LOOP AT ait_field ASSIGNING <ls_field>.
      ASSIGN <ls_field>-value->* TO <l_value>.
      CHECK sy-subrc = 0.
      READ TABLE lt_dynpfield WITH KEY fieldname = <ls_field>-name ASSIGNING <ls_dynpfield>.
      CHECK sy-subrc = 0.
      <l_value> = <ls_dynpfield>-fieldvalue.
    ENDLOOP.
  ENDMETHOD."

  METHOD update.
    DATA: lt_dynpfield TYPE TABLE OF dynpread,
          ls_dynpfield TYPE dynpread.
    FIELD-SYMBOLS:
      <ls_field> TYPE ty_is_field,
      <l_value>  TYPE simple.

    REFRESH lt_dynpfield.

    LOOP AT ait_field ASSIGNING <ls_field>.
      ASSIGN <ls_field>-value->* TO <l_value>.
      CHECK sy-subrc = 0.
      CLEAR: ls_dynpfield.
      ls_dynpfield-fieldname  = <ls_field>-name.
      ls_dynpfield-stepl      = 0.
      ls_dynpfield-fieldvalue = <l_value>.
      APPEND ls_dynpfield TO lt_dynpfield.
    ENDLOOP.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynpfield
      EXCEPTIONS
        OTHERS     = 8.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.

  ENDMETHOD."

  METHOD read_single_field.
    DATA lo_dynp_values TYPE REF TO lcl_dynp_values .
    FIELD-SYMBOLS:
          <l_value> TYPE simple.

    ASSIGN (name) TO <l_value>.
    CHECK sy-subrc = 0.
    CREATE OBJECT lo_dynp_values.
    lo_dynp_values->add_field( EXPORTING name = name stepl = stepl CHANGING value = <l_value> ).
    lo_dynp_values->read( ).
  ENDMETHOD."

  METHOD update_single_field.
    DATA lo_dynp_values TYPE REF TO lcl_dynp_values .
    FIELD-SYMBOLS:
          <l_value> TYPE simple.

    ASSIGN (name) TO <l_value>.
    CHECK sy-subrc = 0.
    CREATE OBJECT lo_dynp_values.
    lo_dynp_values->add_field( EXPORTING name = name stepl = stepl CHANGING value = <l_value> ).
    lo_dynp_values->update( ).
  ENDMETHOD."
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_gui_alv_tree IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_alv_tree IMPLEMENTATION.
  METHOD set_children_at_front.

    CALL METHOD super->set_children_at_front
      EXPORTING
        i_node_key     = i_node_key
      EXCEPTIONS
        node_not_found = 1.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.                    "SET_CHILDREN_AT_FRONT
ENDCLASS.                    "lcl_gui_alv_tree IMPLEMENTATION

*
CLASS lcl_user_settings IMPLEMENTATION.

  METHOD load_variant_and_file.
    load_variant( ).
    IF usrconf IS NOT INITIAL.
      load_file( ).
    ENDIF.
  ENDMETHOD."

  METHOD load_variant.
    TYPE-POOLS sydb0.
    DATA:
*          l_variant  TYPE rsvar-variant,
      l_function    TYPE syucomm,
      ls_screen     TYPE rsdynnr,
      lt_screen     TYPE TABLE OF rsdynnr,
      lt_config     TYPE string_table,
      l_config_line TYPE string,
      lt_sel        TYPE TABLE OF rsparams,
      lt_sel_255    TYPE TABLE OF rsparamsl_255,
      ls_varid      TYPE varid,
      ls_vari_text  TYPE varit,
      lt_vari_text  TYPE TABLE OF varit.
    DATA lt_value_dummy TYPE TABLE OF rsparams.
    DATA lt_vanz TYPE TABLE OF vanz.

*    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
*      EXPORTING
*        curr_report         = sy-repid
*      TABLES
*        selection_table     = lt_sel
*        selection_table_255 = lt_sel_255
*      EXCEPTIONS
*        not_found       = 1
*        no_report       = 2
*        OTHERS          = 3.
*    IF sy-subrc <> 0.
*      " TODO
*    ENDIF.

*    DATA l_variant TYPE variant.
    CLEAR ls_varid.
    ls_varid-report = sy-repid.
    CONCATENATE '$_' sy-uname INTO ls_varid-variant.

    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = ls_varid-report
        variant              = ls_varid-variant
        execute_direct       = 'X'    "<==important
      TABLES
        objects              = lt_vanz
        valutab              = lt_value_dummy
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        error_message        = 3      "<==important
        OTHERS               = 4.
    IF sy-subrc <> 0.

*
*    SELECT SINGLE * FROM varid INTO ls_varid
*          WHERE report = ls_varid-report
*            AND variant = ls_varid-variant.
*    IF sy-subrc <> 0.

      ls_varid-transport = 'N'. " only in catalog
      ls_varid-environmnt = 'A'.
      ls_varid-protected = 'S'. " user-specific

      SET BIT 3 OF ls_varid-xflag1 TO 1. "bit 3 = 1 : restricted to a few screens

      REFRESH lt_screen.
      CLEAR ls_screen.
      ls_screen-dynnr = '1011'.
      ls_screen-kind = 'S'.
      APPEND ls_screen TO lt_screen.

      REFRESH lt_vari_text.
      CLEAR ls_vari_text.
      ls_vari_text-langu = sy-langu.
      ls_vari_text-report = ls_varid-report.
      ls_vari_text-variant = ls_varid-variant.
      CONCATENATE 'Settings for user'(074) sy-uname INTO ls_vari_text-vtext SEPARATED BY space.
      APPEND ls_vari_text TO lt_vari_text.

      CALL FUNCTION 'RS_CREATE_VARIANT'
        EXPORTING
          curr_report               = ls_varid-report
          curr_variant              = ls_varid-variant
          vari_desc                 = ls_varid
        TABLES
          vari_contents             = lt_sel
          vari_text                 = lt_vari_text
          vscreens                  = lt_screen
        EXCEPTIONS
          illegal_report_or_variant = 1
          illegal_variantname       = 2
          not_authorized            = 3
          not_executed              = 4
          report_not_existent       = 5
          report_not_supplied       = 6
          variant_exists            = 7
          variant_locked            = 8
          OTHERS                    = 9.

      IF sy-subrc <> 0.
        " TODO
      ENDIF.

    ENDIF.

  ENDMETHOD."

  METHOD add_menu.
    DATA:
      lo_ctmenu TYPE REF TO cl_ctmenu,
      l_text    TYPE gui_text.

    DEFINE mac_usrf.
      IF usrf&1txt IS NOT INITIAL.
        l_text = usrf&1txt.
        lo_ctmenu->add_function( text = l_text fcode = 'USER&1' ).
      ENDIF.
    END-OF-DEFINITION.

    CREATE OBJECT lo_ctmenu.
    mac_usrf : 1, 2, 3, 4, 5, 6.
    io_ctmenu->add_submenu(  menu = lo_ctmenu text = 'User actions'(033) ).
  ENDMETHOD."


  METHOD load_file.
    " load user settings from the file at the frontend
    DATA: l_section    TYPE string,
          l_item_name  TYPE string,
          l_item_value TYPE string,
          l_rsscr_name TYPE rsscr_name.
    DATA: lt_config    TYPE string_table.
    FIELD-SYMBOLS:
      <l_config_line> TYPE string,
      <l_rsscr_field> TYPE clike.


    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = usrconf
        filetype = 'ASC'                                    "#EC NOTEXT
      CHANGING
        data_tab = lt_config
      EXCEPTIONS
        OTHERS   = 17.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.

    LOOP AT lt_config ASSIGNING <l_config_line>.
      IF <l_config_line> CP '[*]'.
        l_section = <l_config_line>.
      ELSE.
        SPLIT <l_config_line> AT '=' INTO l_item_name l_item_value.
        IF l_item_name CP 'Item+'.                          "#EC NOTEXT
          CASE l_section.
            WHEN '[Text]'.                                  "#EC NOTEXT
              DEFINE mac_usrftxt.
                IF l_item_name = 'Item&1'.
                  usrf&1txt = l_item_value.
                ENDIF.
              END-OF-DEFINITION.
              mac_usrftxt : 1, 2, 3, 4, 5, 6.
            WHEN '[Program]'.                               "#EC NOTEXT
              DEFINE mac_usrfprg.
                IF l_item_name = 'Item&1'.
                  usrf&1prg = l_item_value.
                ENDIF.
              END-OF-DEFINITION.
              mac_usrfprg : 1, 2, 3, 4, 5, 6.
            WHEN '[DirectoryMappingSource]'.                "#EC NOTEXT
              DEFINE mac_dir.
                IF l_item_name = 'Item&1'.
                  dir&1find = l_item_value.
                ENDIF.
              END-OF-DEFINITION.
              mac_dir : 1, 2, 3.
            WHEN '[DirectoryMappingTarget]'.                "#EC NOTEXT
              DEFINE mac_dir.
                IF l_item_name = 'Item&1'.
                  dir&1repl = l_item_value.
                ENDIF.
              END-OF-DEFINITION.
              mac_dir : 1, 2, 3.
          ENDCASE.
        ELSE.
          CASE l_section.
            WHEN '[Misc]'.                                  "#EC NOTEXT
              IF l_item_name = 'TextModeCopy'.              "#EC NOTEXT
                txtcopy = l_item_value.
              ENDIF.
              IF l_item_name = 'AlwaysConfirmationDialog'.  "#EC NOTEXT
                alwconfi = l_item_value.
              ENDIF.
*              IF l_item_name = 'OwnDirectoryWindow'.        "#EC NOTEXT
*                owndirec = l_item_value.
*              ENDIF.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD."

  METHOD save_variant.
*    TYPE-POOLS sydb0.
    DATA: l_variant  TYPE rsvar-variant,
          l_function TYPE syucomm.
*          ls_screen     TYPE sydb0_screen,
*          lt_screen     TYPE sydb0_screen_t,
*          lt_config     TYPE string_table,
*          l_config_line TYPE string.

*    REFRESH lt_screen.
*    CLEAR ls_screen.
*    ls_screen-program = sy-repid.
*    ls_screen-dynnr = '1011'.
*    ls_screen-type = 'W'.
*    APPEND ls_screen TO lt_screen.
    CALL FUNCTION 'RS_VARIANT_SAVE_FROM_SELSCREEN'
      EXPORTING
        curr_report          = sy-repid
      IMPORTING
        variant              = l_variant
        function             = l_function
*      TABLES
*       p_screens            = lt_screen
      EXCEPTIONS
        illegal_variant_name = 1
        not_authorized       = 2
        no_report            = 3
        report_not_existent  = 4
        report_not_supplied  = 5
        OTHERS               = 6.
    IF sy-subrc = 0 AND l_function = 'SAVE'.
      COMMIT WORK. "obligatoire sinon variante n'est pas sauvée
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD."

  METHOD save_file.
    DATA: l_file        TYPE string,
          lt_config     TYPE string_table,
          l_config_line TYPE string.

    REFRESH lt_config.
    APPEND '[Text]' TO lt_config.                           "#EC NOTEXT
    DEFINE mac_usrftxt.
      CONCATENATE 'Item&1=' usrf&1txt INTO l_config_line.   "#EC NOTEXT
      APPEND l_config_line TO lt_config.
    END-OF-DEFINITION.
    mac_usrftxt : 1, 2, 3, 4, 5, 6.

    APPEND '[Program]' TO lt_config.                        "#EC NOTEXT
    DEFINE mac_usrfprg.
      CONCATENATE 'Item&1=' usrf&1prg INTO l_config_line.   "#EC NOTEXT
      APPEND l_config_line TO lt_config.
    END-OF-DEFINITION.
    mac_usrfprg : 1, 2, 3, 4, 5, 6.

    APPEND '[Misc]' TO lt_config.                           "#EC NOTEXT
    CONCATENATE 'TextModeCopy' '=' txtcopy INTO l_config_line. "#EC NOTEXT
    APPEND l_config_line TO lt_config.
    CONCATENATE 'AlwaysConfirmationDialog' '=' alwconfi INTO l_config_line. "#EC NOTEXT
    APPEND l_config_line TO lt_config.

    APPEND '[DirectoryMappingSource]' TO lt_config.         "#EC NOTEXT
    DEFINE mac_dir.
      CONCATENATE 'Item&1=' dir&1find INTO l_config_line.   "#EC NOTEXT
      APPEND l_config_line TO lt_config.
    END-OF-DEFINITION.
    mac_dir : 1, 2, 3.

    APPEND '[DirectoryMappingTarget]' TO lt_config.         "#EC NOTEXT
    DEFINE mac_dir.
      CONCATENATE 'Item&1=' dir&1repl INTO l_config_line.   "#EC NOTEXT
      APPEND l_config_line TO lt_config.
    END-OF-DEFINITION.
    mac_dir : 1, 2, 3.
*      CONCATENATE 'OwnDirectoryWindow' '=' owndirec INTO l_config_line. "#EC NOTEXT
*      APPEND l_config_line TO lt_config.

    l_file = usrconf.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename = l_file
        filetype = 'ASC'                                  "#EC NOTEXT
      CHANGING
        data_tab = lt_config
      EXCEPTIONS
        OTHERS   = 17.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD."

ENDCLASS."
*
CLASS lcl_ctmenu IMPLEMENTATION.

  METHOD deserialize_menu.
    DATA: l_tabix TYPE sytabix.

    l_tabix = 1.
    _deserialize_menu(  EXPORTING
                          io_ctmenu   = io_ctmenu
                          it_menu     = it_menu
                          i_level     = 1
                        CHANGING
                          c_tabix     = l_tabix ).

  ENDMETHOD."

  METHOD _deserialize_menu.
    DATA: lo_ctmenu TYPE REF TO cl_ctmenu,
          l_level   TYPE i,
          l_ftype   TYPE cua_ftyp,
          l_fcode   TYPE ui_func.
    FIELD-SYMBOLS :
          <ls_menu>     TYPE sctx_serialize_entry.


    DO.
      READ TABLE it_menu ASSIGNING <ls_menu> INDEX c_tabix.
      IF sy-subrc <> 0 OR <ls_menu>-level < i_level.
        EXIT.
      ENDIF.

      ADD 1 TO c_tabix.

      CASE <ls_menu>-type.
        WHEN sctx_c_type_menu.
          CREATE OBJECT lo_ctmenu.
          l_level = <ls_menu>-level + 1.
          _deserialize_menu( EXPORTING io_ctmenu = lo_ctmenu it_menu = it_menu i_level = l_level
                             CHANGING c_tabix = c_tabix ).
          io_ctmenu->add_menu( menu = lo_ctmenu ).

        WHEN sctx_c_type_submenu.
          CREATE OBJECT lo_ctmenu.
          l_level = <ls_menu>-level + 1.
          _deserialize_menu( EXPORTING io_ctmenu = lo_ctmenu it_menu = it_menu i_level = l_level
                             CHANGING c_tabix = c_tabix ).
          io_ctmenu->add_submenu(   menu        = lo_ctmenu
                                    text        = <ls_menu>-text
                                    icon        = <ls_menu>-icon
                                    disabled    = <ls_menu>-disabled
                                    accelerator = <ls_menu>-accelerator ).

        WHEN sctx_c_type_function.
          IF <ls_menu>-fcode CP '/E*'. "case insensitive
            l_fcode = <ls_menu>-fcode+2.
            l_ftype = 'E'.
          ELSEIF <ls_menu>-fcode(1) = '?'.
            l_fcode = <ls_menu>-fcode+1.
            l_ftype = 'H'.
          ELSEIF <ls_menu>-fcode CP '/N*'. "case insensitive
            l_fcode = <ls_menu>-fcode+2.
            l_ftype = 'T'.
          ELSE.
            l_fcode = <ls_menu>-fcode.
            l_ftype = space.
          ENDIF.
          io_ctmenu->add_function(  fcode       = l_fcode
                                    text        = <ls_menu>-text
                                    icon        = <ls_menu>-icon
                                    ftype       = l_ftype
                                    disabled    = <ls_menu>-disabled
                                    checked     = <ls_menu>-checked
                                    accelerator = <ls_menu>-accelerator ).

        WHEN sctx_c_type_separator.
          io_ctmenu->add_separator( ).

      ENDCASE.

    ENDDO.

  ENDMETHOD."
ENDCLASS."
*
CLASS lcl_gui_toolbar IMPLEMENTATION.

  METHOD add_toolbar_to_container.
    DATA lo_cont TYPE REF TO cl_gui_container.

    CREATE OBJECT eo_splitter
      EXPORTING
        parent  = io_parent
        rows    = 2
        columns = 1.

    lo_cont = eo_splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT eo_toolbar
      EXPORTING
        parent = lo_cont.

    eo_bottom_container = eo_splitter->get_container( row = 2 column = 1 ).


    eo_splitter->set_row_mode( mode = cl_gui_splitter_container=>mode_absolute ).
    eo_splitter->set_row_height( EXPORTING id = 1 height = 22 ).

    CALL METHOD eo_splitter->set_row_sash
      EXPORTING
        id    = 1
        type  = cl_gui_splitter_container=>type_sashvisible
        value = cl_gui_splitter_container=>false.

    CALL METHOD eo_splitter->set_row_sash
      EXPORTING
        id    = 1
        type  = cl_gui_splitter_container=>type_movable
        value = cl_gui_splitter_container=>false.

  ENDMETHOD."
ENDCLASS."


DEFINE mac_show.
  if screen-group1 = &1.
    if &2 ca &3.
      screen-active = '1'.
      modify screen.
    else.
      screen-active = '0'.
      modify screen.
    endif.
  endif.
END-OF-DEFINITION.
DEFINE mac_enable.
  if screen-group1 = &1.
    if &2 ca &3.
      screen-input = '1'.
      modify screen.
    else.
      screen-input = '0'.
      modify screen.
    endif.
  endif.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_initial_screen IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_initial_screen IMPLEMENTATION.

  METHOD lif_sscr~pbo.
    DATA: lt_itab    TYPE TABLE OF sy-ucomm.


    sscrfields-functxt_01 = 'Configuration settings'(set).
    sscrfields-functxt_02 = 'User settings'(seu).

    IF dirtype1 IS INITIAL. dirtype1 = 'N'. ENDIF.
    IF dirtype2 IS INITIAL. dirtype2 = 'N'. ENDIF.
    IF dirtype3 IS INITIAL. dirtype3 = 'N'. ENDIF.


    LOOP AT SCREEN.
      mac_show:
                'FI1' dirtype1 'FLPSG',
                'HI1' dirtype1 'FLPSG',
                'LF1' dirtype1 'L',
                'LP1' dirtype1 'LP',
                'LA1' dirtype1 'LPG',
                'SD1' dirtype1 'S',
                'GU1' dirtype1 'G',

                'FI2' dirtype2 'FLPSG',
                'HI2' dirtype2 'FLPSG',
                'LF2' dirtype2 'L',
                'LP2' dirtype2 'LP',
                'LA2' dirtype2 'LPG',
                'SD2' dirtype2 'S',
                'GU2' dirtype2 'G',

                'FI3' dirtype3 'FLPSG',
                'HI3' dirtype3 'FLPSG',
                'LF3' dirtype3 'L',
                'LP3' dirtype3 'LP',
                'LA3' dirtype3 'LPG',
                'SD3' dirtype3 'S',
                'GU3' dirtype3 'G'.
      mac_enable:
                'FI1' dirtype1 'F',
                'LP1' dirtype1 'P',

                'FI2' dirtype2 'F',
                'LP2' dirtype2 'P',

                'FI3' dirtype3 'F',
                'LP3' dirtype3 'P'.
    ENDLOOP.

    "-----------------------
    " SAP bug: if buttons have been hidden in other screens (config screen), they
    " are also disabled when back to here, so we need to explicitly restore them!
    "-----------------------
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '%_00'                                  "#EC NOTEXT
      TABLES
        p_exclude = lt_itab.

  ENDMETHOD."


  METHOD lif_sscr~pai.
    TYPE-POOLS sydb0.
    DATA: l_variant  TYPE rsvar-variant,
          l_function TYPE syucomm,
          ls_screen  TYPE sydb0_screen,
          lt_screen  TYPE sydb0_screen_t.
    DEFINE mac_sscr_pai.
      case dirtype&1.
        when 'L'.
          appserv&1 = abap_true.
          perform get_lf_fields using lf&1 logargs&1 changing lp&1 dir&1.
        when 'G'.
          appserv&1 = abap_false.
          dir&1 = go_app->get_sap_gui_directory( gu = gu&1 logargs = logargs&1 ).
        when 'P'.
          appserv&1 = abap_true.
          perform get_lp_fields using lp&1 logargs&1 changing dir&1.
        when 'S'.
          appserv&1 = abap_true.
          dir&1 = go_app->get_sap_directory( sd&1 ).
      endcase.
    END-OF-DEFINITION.



    mac_sscr_pai : 1, 2, 3.

    IF settvar IS NOT INITIAL.
      DATA lt_value_dummy TYPE TABLE OF rsparams.
      CALL FUNCTION 'RS_VARIANT_CONTENTS'
        EXPORTING
          report               = sy-repid
          variant              = settvar
          execute_direct       = 'X'    "<==important
        TABLES
          valutab              = lt_value_dummy
        EXCEPTIONS
          variant_non_existent = 1
          variant_obsolete     = 2
          error_message        = 3      "<==important
          OTHERS               = 4.
    ENDIF.


    CASE sscrfields-ucomm.
      WHEN 'SPOS'.
        REFRESH lt_screen.
        CLEAR ls_screen.
        ls_screen-program = sy-repid.
        ls_screen-dynnr = '1000'.
        ls_screen-type = 'S'.
        APPEND ls_screen TO lt_screen.
        IF settvar IS INITIAL.
          " save also the settings from the config screen
          ls_screen-dynnr = '1006'.
          APPEND ls_screen TO lt_screen.
          ls_screen-dynnr = '1008'.
          APPEND ls_screen TO lt_screen.
          ls_screen-dynnr = '1009'.
          APPEND ls_screen TO lt_screen.
          ls_screen-dynnr = '1010'.
          APPEND ls_screen TO lt_screen.
        ENDIF.
        CALL FUNCTION 'RS_VARIANT_SAVE_FROM_SELSCREEN'
          EXPORTING
            curr_report          = sy-repid
          IMPORTING
            variant              = l_variant
            function             = l_function
          TABLES
            p_screens            = lt_screen
          EXCEPTIONS
            illegal_variant_name = 1
            not_authorized       = 2
            no_report            = 3
            report_not_existent  = 4
            report_not_supplied  = 5
            OTHERS               = 6.
        IF l_function = 'SAVE'.
          COMMIT WORK. "obligatoire sinon variante n'est pas sauvée
        ENDIF.
        CLEAR sscrfields-ucomm.
      WHEN 'FC01'.
        " general settings
        CALL SELECTION-SCREEN 1006 STARTING AT 30 5 ENDING AT 150 30.
      WHEN 'FC02'.
        " user settings
        " two goals for loading here: make sure that SAVE will propose the variant $_<user>,
        "                             (must be done before the CALL/useless in the PBO)
        "                             and refresh the settings from the file.
        lcl_user_settings=>load_variant_and_file( ).
        CALL SELECTION-SCREEN 1011 STARTING AT 30 5 ENDING AT 150 30.
      WHEN 'ONLI'.
        " main screen of file manager
        CALL SELECTION-SCREEN 1001.
    ENDCASE.

  ENDMETHOD."


  METHOD lif_sscr~exit.
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_config_screen IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_config_screen IMPLEMENTATION.

  METHOD lif_sscr~pbo.
    DATA: ls_functxt TYPE smp_dyntxt,
          lt_itab    TYPE TABLE OF sy-ucomm.

    " if a new variant is selected, set fields in display mode
    IF sy-slset <> ku_save_slset.
      ku_save_slset = sy-slset.
      ku_current_slset = ku_save_slset.
    ENDIF.

    IF ku_current_slset IS NOT INITIAL.
      ls_functxt-icon_id   = icon_change.
      ls_functxt-icon_text = 'Toggle display -> change'(aaa).
      sscrfields-functxt_01 = ls_functxt.
      " afficher le bouton
      CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
        EXPORTING
          p_status  = '%_CSP'
        TABLES
          p_exclude = lt_itab.
    ELSE.
      APPEND: 'FC01' TO lt_itab.
      CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
        EXPORTING
          p_status  = '%_CSP'
        TABLES
          p_exclude = lt_itab.
    ENDIF.

*    LOOP AT SCREEN.
*      IF ku_current_slset IS INITIAL.
*        screen-input = '1'.
*      ELSE.
*        screen-input = '0'.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.

  ENDMETHOD."


  METHOD lif_sscr~pai.
    TYPE-POOLS sydb0.
    DATA: l_variant  TYPE rsvar-variant,
          l_function TYPE syucomm,
          ls_screen  TYPE sydb0_screen,
          lt_screen  TYPE sydb0_screen_t.

    CASE sscrfields-ucomm.
      WHEN 'FC01'.
        " Toggle
        CLEAR ku_current_slset.
      WHEN 'SPOS'.
        REFRESH lt_screen.
        CLEAR ls_screen.
        ls_screen-program = sy-repid.
        ls_screen-dynnr = '1006'.
        ls_screen-type = 'S'.
        APPEND ls_screen TO lt_screen.
        ls_screen-dynnr = '1008'.
        APPEND ls_screen TO lt_screen.
        ls_screen-dynnr = '1009'.
        APPEND ls_screen TO lt_screen.
        ls_screen-dynnr = '1010'.
        APPEND ls_screen TO lt_screen.
        ls_screen-dynnr = '1012'.
        APPEND ls_screen TO lt_screen.
        CALL FUNCTION 'RS_VARIANT_SAVE_FROM_SELSCREEN'
          EXPORTING
            curr_report          = sy-repid
          IMPORTING
            variant              = l_variant
            function             = l_function
          TABLES
            p_screens            = lt_screen
          EXCEPTIONS
            illegal_variant_name = 1
            not_authorized       = 2
            no_report            = 3
            report_not_existent  = 4
            report_not_supplied  = 5
            OTHERS               = 6.
        IF l_function = 'SAVE'.
          COMMIT WORK. "obligatoire sinon variante n'est pas sauvée
          ku_current_slset = l_variant.
        ENDIF.
        CLEAR sscrfields-ucomm.
      WHEN 'CRET'.
        settvar = ku_current_slset.
      WHEN 'UCOMM_TAB1'.
        tabstrip-dynnr = '1008'.
        tabstrip-activetab = sscrfields-ucomm.
      WHEN 'UCOMM_TAB2'.
        tabstrip-dynnr = '1009'.
        tabstrip-activetab = sscrfields-ucomm.
      WHEN 'UCOMM_TAB3'.
        tabstrip-dynnr = '1010'.
        tabstrip-activetab = sscrfields-ucomm.
      WHEN 'UCOMM_TAB4'.
        tabstrip-dynnr = '1012'.
        tabstrip-activetab = sscrfields-ucomm.
      WHEN 'UCOMM_TAB5'.
        tabstrip-dynnr = '1014'.
        tabstrip-activetab = sscrfields-ucomm.
    ENDCASE.
  ENDMETHOD."


  METHOD lif_sscr~exit.
  ENDMETHOD."

ENDCLASS."

*
CLASS lcl_config_unix_screen IMPLEMENTATION.
  METHOD lif_sscr~pbo.
    LOOP AT SCREEN.
      IF lcl_config_screen=>ku_current_slset IS NOT INITIAL.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD."
  METHOD lif_sscr~pai.
  ENDMETHOD."
  METHOD lif_sscr~exit.
  ENDMETHOD."
ENDCLASS."

*
CLASS lcl_config_windows_screen IMPLEMENTATION.
  METHOD lif_sscr~pbo.
    LOOP AT SCREEN.
      IF lcl_config_screen=>ku_current_slset IS NOT INITIAL.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD."
  METHOD lif_sscr~pai.
  ENDMETHOD."
  METHOD lif_sscr~exit.
  ENDMETHOD."
ENDCLASS."

*
CLASS lcl_config_icon_screen IMPLEMENTATION.
  METHOD lif_sscr~pbo.
    LOOP AT SCREEN.
      IF screen-name = 'FILETXTZ'.
        screen-active = '0'.
        MODIFY SCREEN.
      ELSEIF screen-name = 'FILEEXTZ'.
        screen-input = '0'.
        screen-display_3d = '0'.
        MODIFY SCREEN.
      ELSEIF lcl_config_screen=>ku_current_slset IS NOT INITIAL.
        screen-input = '0'.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'OUT'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    DEFINE  mac_icon_pbo.
      select single id from icon into icodext&1
            where name = iconext&1.
    END-OF-DEFINITION.
    mac_icon_pbo : 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f, z.
  ENDMETHOD."


  METHOD lif_sscr~pai.
  ENDMETHOD."


  METHOD lif_sscr~exit.
  ENDMETHOD."
ENDCLASS."

*
CLASS lcl_config_prog_screen IMPLEMENTATION.
  METHOD lif_sscr~pbo.
    LOOP AT SCREEN.
      IF lcl_config_screen=>ku_current_slset IS NOT INITIAL.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD."
  METHOD lif_sscr~pai.
  ENDMETHOD."
  METHOD lif_sscr~exit.
  ENDMETHOD."
ENDCLASS."

*
CLASS lcl_config_misc_screen IMPLEMENTATION.
  METHOD lif_sscr~pbo.
    LOOP AT SCREEN.
      IF lcl_config_screen=>ku_current_slset IS NOT INITIAL.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD."
  METHOD lif_sscr~pai.
  ENDMETHOD."
  METHOD lif_sscr~exit.
  ENDMETHOD."
ENDCLASS."

*
CLASS lcl_config_user_screen IMPLEMENTATION.
  METHOD lif_sscr~pbo.
    DATA: lt_itab   TYPE ui_functions.

*    IF ai_first_pbo_done = abap_false.
*      " two goals: make sure that SAVE will propose the variant $_<user>,
*      "            and refresh the settings from the file
*      lcl_user_settings=>load_variant_and_file( ).
*      ai_first_pbo_done = abap_true.
*    ENDIF.

    sscrfields-functxt_01 = 'TODO???'(bbb).

    APPEND 'CRET' TO lt_itab.
    APPEND 'CCAN' TO lt_itab.
    APPEND 'SPOS' TO lt_itab.
    lcl_sscr=>disable_all_function_codes( i_pf_status = '%_CSP' except = lt_itab ).

    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'FIL'.
          IF usrfile = abap_true.
            screen-input = '1'.
          ELSE.
            screen-input = '0'.
          ENDIF.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD."


  METHOD lif_sscr~pai.
    CASE sscrfields-ucomm.
      WHEN 'USRFILE'.
        SET CURSOR FIELD 'USRCONF'.
      WHEN 'CRET'.
        IF usrconf IS NOT INITIAL.
          lcl_user_settings=>save_file( usrconf ).
        ENDIF.
      WHEN 'SPOS'.
        lcl_user_settings=>save_variant( ).
        CLEAR sscrfields-ucomm.
    ENDCASE.
  ENDMETHOD."


  METHOD lif_sscr~exit.
*    ai_first_pbo_done = abap_false.
  ENDMETHOD."
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_main_screen IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_main_screen IMPLEMENTATION.

  METHOD lif_sscr~pbo.
    DATA: ls_functxt           TYPE smp_dyntxt,
          lt_itab              TYPE ui_functions,
          lo_container         TYPE REF TO cl_gui_container,
          lo_pfm               TYPE REF TO lcl_ui_file_manager,
          l_columns            TYPE i,
          l_column             TYPE i,
          ls_container_fileman TYPE ty_is_container_fileman.

    ls_functxt-icon_id   = icon_refresh.
    ls_functxt-icon_text = 'Refresh all'(ref).
    sscrfields-functxt_01 = ls_functxt.

    " hide all buttons except exit and FC01
    APPEND 'FC01' TO lt_itab.                               "#EC NOTEXT
    APPEND 'CEND' TO lt_itab.                               "#EC NOTEXT
    lcl_sscr=>disable_all_function_codes( i_pf_status = '%_CS' except = lt_itab ). "#EC NOTEXT

    " Avoid the blinking (flash display of selection screen)
    " when the GUI control is freed
    LOOP AT SCREEN.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.


    IF aio_splitter IS NOT BOUND.

      mac_icon_pbo : 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f, z.

      go_app->ais_config_fe-xpgmove = xpgfmove.
      go_app->ais_config_fe-xpgmov3 = xpgfmov3.
      go_app->ais_config_fe-xpgcopy = xpgfcopy.
      go_app->ais_config_fe-xpgcpy3 = xpgfcpy3.
      go_app->ais_config_fe-xpgmkdi = xpgfmkdi.
      go_app->ais_config_fe-xpgrmdi = xpgfrmdi.

      go_app->ais_config_as-xpgmove = xpgsmove.
      go_app->ais_config_as-xpgmov3 = xpgsmov3.
      go_app->ais_config_as-xpgcopy = xpgscopy.
      go_app->ais_config_as-xpgcpy3 = xpgscpy3.
      go_app->ais_config_as-xpgmkdi = xpgsmkdi.
      go_app->ais_config_as-xpgrmdi = xpgsrmdi.

      l_columns = 0.
      IF dirtype1 <> 'N' AND dir1 IS NOT INITIAL.
        ADD 1 TO l_columns.
      ENDIF.
      IF dirtype2 <> 'N' AND dir2 IS NOT INITIAL.
        ADD 1 TO l_columns.
      ENDIF.
      IF dirtype3 <> 'N' AND dir3 IS NOT INITIAL.
        ADD 1 TO l_columns.
      ENDIF.

      DEFINE mac_dir2.
        IF dir&1find IS NOT INITIAL AND dir&1repl IS NOT INITIAL.
          REPLACE REGEX dir&1find IN owndirec WITH dir&1repl.
          IF sy-subrc = 0.
            EXIT.
          ENDIF.
        ENDIF.
      END-OF-DEFINITION.
      DEFINE mac_dir.
        owndirec = dir&1.
        mac_dir2 : 1, 2, 3.
      END-OF-DEFINITION.
      DO.
        mac_dir : 1, 2, 3.
        CLEAR owndirec.
        EXIT.
      ENDDO.
      IF owndirec IS NOT INITIAL.
        owndirec = go_app->aio_frontend->normalize_dir_path( owndirec ).
        ADD 1 TO l_columns.
      ENDIF.

      CHECK l_columns >= 1.

      DATA lo_ui_dir_tree_builder_fs TYPE REF TO lcl_ui_dir_tree_builder_fs.
      DATA lo_ui_dir_content_builder_fs TYPE REF TO lcl_ui_dir_content_builder_fs.

      CREATE OBJECT lo_ui_dir_tree_builder_fs.
      CREATE OBJECT lo_ui_dir_content_builder_fs.

      CREATE OBJECT aio_splitter
        EXPORTING
          parent  = cl_gui_container=>screen0
          rows    = 1
          columns = l_columns.

      l_column = 0.
      IF dirtype1 <> 'N' AND dir1 IS NOT INITIAL.
        ADD 1 TO l_column.
        lo_container = aio_splitter->get_container( row = 1 column = l_column ).
        CREATE OBJECT lo_pfm
          EXPORTING
            io_ui_dir_tree_builder    = lo_ui_dir_tree_builder_fs
            io_ui_dir_content_builder = lo_ui_dir_content_builder_fs
            container                 = lo_container
            dir_fullpath              = dir1
            appserv                   = appserv1
            display_hier              = hier1
            handler                   = me.
      ENDIF.

      IF dirtype2 <> 'N' AND dir2 IS NOT INITIAL.
        ADD 1 TO l_column.
        lo_container = aio_splitter->get_container( row = 1 column = l_column ).
        CREATE OBJECT lo_pfm
          EXPORTING
            io_ui_dir_tree_builder    = lo_ui_dir_tree_builder_fs
            io_ui_dir_content_builder = lo_ui_dir_content_builder_fs
            container                 = lo_container
            dir_fullpath              = dir2
            appserv                   = appserv2
            display_hier              = hier2
            handler                   = me.
      ENDIF.

      IF dirtype3 <> 'N' AND dir3 IS NOT INITIAL.
        ADD 1 TO l_column.
        lo_container = aio_splitter->get_container( row = 1 column = l_column ).
        CREATE OBJECT lo_pfm
          EXPORTING
            io_ui_dir_tree_builder    = lo_ui_dir_tree_builder_fs
            io_ui_dir_content_builder = lo_ui_dir_content_builder_fs
            container                 = lo_container
            dir_fullpath              = dir3
            appserv                   = appserv3
            display_hier              = hier3
            handler                   = me.
      ENDIF.

      IF owndirec IS NOT INITIAL.
        ADD 1 TO l_column.
        lo_container = aio_splitter->get_container( row = 1 column = l_column ).
        CREATE OBJECT lo_pfm
          EXPORTING
            io_ui_dir_tree_builder    = lo_ui_dir_tree_builder_fs
            io_ui_dir_content_builder = lo_ui_dir_content_builder_fs
            container                 = lo_container
            dir_fullpath              = owndirec
            appserv                   = abap_false
            display_hier              = abap_false
            handler                   = me.
      ENDIF.

      test_zip( ).

    ENDIF.

  ENDMETHOD."


  METHOD test_zip.
    DATA: l_fullpath        TYPE string,
          lo_dialogbox      TYPE REF TO cl_gui_dialogbox_container,
          lo_pfm            TYPE REF TO lcl_ui_file_manager,
*          lo_frontend       TYPE REF TO lcl_frontend,
          lo_dof            TYPE REF TO lcl_dof,
          lo_ui_zip_builder TYPE REF TO lcl_ui_zip_builder.

*  CREATE OBJECT lo_frontend.
    CREATE OBJECT lo_dof EXPORTING file_system = go_app->aio_frontend fullpath = `C:\Users\sandra.rossi\Documents\ggg.zip` isdir = 0.
    CREATE OBJECT lo_ui_zip_builder EXPORTING dof = lo_dof.

    CREATE OBJECT lo_dialogbox
      EXPORTING
        width  = 540
        height = 100
        top    = 150
        left   = 150
        repid  = sy-repid
        dynnr  = sy-dynnr.

    CREATE OBJECT lo_pfm
      EXPORTING
        io_ui_dir_tree_builder    = lo_ui_zip_builder
        io_ui_dir_content_builder = lo_ui_zip_builder
        container                 = lo_dialogbox
        dir_fullpath              = lo_dof->fullpath
        appserv                   = abap_false
        display_hier              = abap_true
        handler                   = me.

  ENDMETHOD."


  METHOD lif_sscr~pai.
    "CLEAR sscrfields-ucomm.
  ENDMETHOD."


  METHOD lif_sscr~exit.
  ENDMETHOD."


*  METHOD lif_cross_action~start_confirm_dialog.
*    CALL SELECTION-SCREEN 1005 STARTING AT 30 10 ENDING AT 130 30.
*  ENDMETHOD."


ENDCLASS."


*
CLASS lcl_folder_move_screen IMPLEMENTATION.
  METHOD display.
    go_app->lif_sscr_h~set_sscr_handler( sscr = '1013' handler = me ).
    CALL SELECTION-SCREEN 1013 STARTING AT 30 10 ENDING AT 90 20.
  ENDMETHOD."


  METHOD lif_sscr~pbo.
    DATA: lt_itab   TYPE ui_functions.

    APPEND 'CRET' TO lt_itab.
    APPEND 'CCAN' TO lt_itab.
    lcl_sscr=>disable_all_function_codes( i_pf_status = '%_CSP' except = lt_itab ).
  ENDMETHOD."


  METHOD lif_sscr~pai.
    CASE abap_true.
      WHEN p_fpsp.
        choice = fp_sp.
      WHEN p_fpsm.
        choice = fp_sm.
      WHEN p_fmsp.
        choice = fm_sp.
      WHEN p_fmsm.
        choice = fm_sm.
    ENDCASE.
  ENDMETHOD."


  METHOD lif_sscr~exit.
  ENDMETHOD."
ENDCLASS."

*
CLASS lcl_folder_copy_screen IMPLEMENTATION.
  METHOD display.
    go_app->lif_sscr_h~set_sscr_handler( sscr = '1013' handler = me ).
    CALL SELECTION-SCREEN 1013 STARTING AT 30 10 ENDING AT 90 20.
  ENDMETHOD."


  METHOD lif_sscr~pbo.
    DATA: lt_itab   TYPE ui_functions.

    APPEND 'CRET' TO lt_itab.
    APPEND 'CCAN' TO lt_itab.
    lcl_sscr=>disable_all_function_codes( i_pf_status = '%_CSP' except = lt_itab ).
  ENDMETHOD."


  METHOD lif_sscr~pai.
    CASE abap_true.
      WHEN p_fpsp.
        choice = fp_sp.
      WHEN p_fpsm.
        choice = fp_sm.
      WHEN p_fmsp.
        choice = fm_sp.
      WHEN p_fmsm.
        choice = fm_sm.
    ENDCASE.
  ENDMETHOD."


  METHOD lif_sscr~exit.
  ENDMETHOD."
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_file_manager IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ui_file_manager IMPLEMENTATION.

  METHOD constructor.
    DATA: lo_container      TYPE REF TO cl_gui_container,
          lo_splitter       TYPE REF TO cl_gui_splitter_container,
          l_fieldname       TYPE string,
          ls_confirm        TYPE lcl_popup_confirm=>ty_us_confirm,
          lt_field          TYPE string_table,
          lt_itab           TYPE string_table,
          lo_cont_dir_input TYPE REF TO cl_gui_container,
          lo_cont           TYPE REF TO cl_gui_container,
          l_effect          TYPE i.


    aio_container = container.
    au_dir_fullpath = dir_fullpath.
    au_appserv = appserv.
    au_display_hier = display_hier.
    aio_cross_action = handler.


    IF appserv = abap_true.
      aio_file_system = go_app->aio_appserv.
    ELSE.
      aio_file_system = go_app->aio_frontend.
    ENDIF.

    IF aio_splitter IS NOT BOUND.

      CREATE OBJECT gr_dragdrop_d0100.

      l_effect = cl_dragdrop=>move + cl_dragdrop=>copy.

      CALL METHOD gr_dragdrop_d0100->add
        EXPORTING
          flavor     = 'Line'                               "#EC NOTEXT
          dragsrc    = abap_true
          droptarget = abap_true
          effect     = l_effect.

      CALL METHOD gr_dragdrop_d0100->get_handle
        IMPORTING
          handle = g_dragdrop_handle.

      CREATE OBJECT aio_splitter
        EXPORTING
          parent  = aio_container
          rows    = 2
          columns = 1.

      lo_cont_dir_input = aio_splitter->get_container( row = 1 column = 1 ).

      lo_cont = aio_splitter->get_container( row = 2 column = 1 ).


      aio_splitter->set_row_mode( mode = cl_gui_splitter_container=>mode_absolute ).
      aio_splitter->set_row_height( EXPORTING id = 1 height = 26 ).

      CALL METHOD aio_splitter->set_row_sash
        EXPORTING
          id    = 1
          type  = cl_gui_splitter_container=>type_sashvisible
          value = cl_gui_splitter_container=>false.

      CALL METHOD aio_splitter->set_row_sash
        EXPORTING
          id    = 1
          type  = cl_gui_splitter_container=>type_movable
          value = cl_gui_splitter_container=>false.

      aio_frontend = go_app->aio_frontend.
      aio_appserv = go_app->aio_appserv.

      IF au_display_hier = abap_false.
        lo_container = lo_cont.
      ELSE.
        CREATE OBJECT aio_splitter
          EXPORTING
            parent  = lo_cont
            rows    = 1
            columns = 2.

        lo_container = aio_splitter->get_container( row = 1 column = 1 ).

        aio_ui_dir_tree = io_ui_dir_tree_builder->create(
                              parent      = lo_container
                              file_system = aio_file_system
                              start_path  = au_dir_fullpath
                              pfm         = me
                              ).

        lo_container = aio_splitter->get_container( row = 1 column = 2 ).

        SET HANDLER on_dof_open_requested         FOR aio_ui_dir_tree.
        SET HANDLER on_dof_open_with_requested    FOR aio_ui_dir_tree.
        SET HANDLER on_dof_move_requested         FOR aio_ui_dir_tree.
        SET HANDLER on_dof_copy_requested         FOR aio_ui_dir_tree.
        SET HANDLER on_new_file_requested         FOR aio_ui_dir_tree.
        SET HANDLER on_new_folder_requested       FOR aio_ui_dir_tree.
        SET HANDLER on_dof_delete_requested       FOR aio_ui_dir_tree.
        SET HANDLER on_dof_rename_requested       FOR aio_ui_dir_tree.
        SET HANDLER on_dof_copy_to_clpb_requested FOR aio_ui_dir_tree.
        SET HANDLER on_dof_cut_to_clpb_requested  FOR aio_ui_dir_tree.
        SET HANDLER on_dof_paste_from_clpb_requ   FOR aio_ui_dir_tree.
        SET HANDLER on_dof_copy_path_2_clpb_requ  FOR aio_ui_dir_tree.

      ENDIF.

      aio_ui_dir_content = io_ui_dir_content_builder->create(
                                parent                  = lo_container
                                file_system             = aio_file_system
                                start_path              = au_dir_fullpath
                                container_dir_input     = lo_cont_dir_input
                                change_on_selected_dirs = aio_ui_dir_tree
                                pfm                     = me
                                ).

      SET HANDLER on_dof_open_requested         FOR aio_ui_dir_content.
      SET HANDLER on_dof_open_with_requested    FOR aio_ui_dir_content.
      SET HANDLER on_dof_move_requested         FOR aio_ui_dir_content.
      SET HANDLER on_dof_copy_requested         FOR aio_ui_dir_content.
      SET HANDLER on_new_file_requested         FOR aio_ui_dir_content.
      SET HANDLER on_new_folder_requested       FOR aio_ui_dir_content.
      SET HANDLER on_dof_delete_requested       FOR aio_ui_dir_content.
      SET HANDLER on_dof_rename_requested       FOR aio_ui_dir_content.
      SET HANDLER on_dof_copy_to_clpb_requested FOR aio_ui_dir_content.
      SET HANDLER on_dof_cut_to_clpb_requested  FOR aio_ui_dir_content.
      SET HANDLER on_dof_paste_from_clpb_requ   FOR aio_ui_dir_content.
      SET HANDLER on_dof_copy_path_2_clpb_requ  FOR aio_ui_dir_content.
      SET HANDLER on_dof_custom_action_requested FOR aio_ui_dir_content.

    ENDIF.

  ENDMETHOD."


  METHOD free.
    IF aio_splitter IS BOUND.
      aio_splitter->free( ).
    ENDIF.
  ENDMETHOD."


  METHOD on_dof_open_requested.
    DATA: lo_dof        TYPE REF TO lcl_dof.

    LOOP AT dofs->auto_dof INTO lo_dof.
      lo_dof->execute( ).
    ENDLOOP.

  ENDMETHOD."


  METHOD on_dof_open_with_requested.
    DATA: lo_dof        TYPE REF TO lcl_dof.

    LOOP AT dofs->auto_dof INTO lo_dof.
      lo_dof->open_with( ).
    ENDLOOP.

  ENDMETHOD."


  METHOD on_dof_custom_action_requested.
    DATA: l_application TYPE string,
          l_parameters  TYPE string,
          lo_dof        TYPE REF TO lcl_dof,
          lo_dof2       TYPE REF TO lcl_dof,
          lto_dof       TYPE TABLE OF REF TO lcl_dof,
          lt_param      TYPE TABLE OF rsparams,
          ls_param      TYPE rsparams.


    IF ucomm = 'PRG8'.

      IF aio_file_system->type = 'F' AND prg8pffp IS INITIAL.
        " If the file is on the frontend but the program is only able to
        " process the file on the application server, then copy the file
        " to the application server temporary directory
        IF tempdir IS INITIAL. RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'Temporary folder needed'. ENDIF.
        LOOP AT dofs->auto_dof INTO lo_dof.
          CREATE OBJECT lo_dof2
            EXPORTING
              file_system = aio_appserv
              path        = tempdir
              filename    = lo_dof->filename
              isdir       = 0.
          lo_dof->file_copy( to = lo_dof2 ).
          APPEND lo_dof2 TO lto_dof.
        ENDLOOP.
      ELSEIF aio_file_system->type = 'S' AND prg8pafp IS INITIAL.
        " copy the file to the frontend temporary directory
        LOOP AT dofs->auto_dof INTO lo_dof.
          CREATE OBJECT lo_dof2
            EXPORTING
              file_system = aio_frontend
              path        = go_app->aio_frontend->aus_directory-temp
              filename    = lo_dof->filename
              isdir       = 0.
          lo_dof->file_copy( to = lo_dof2 ).
          APPEND lo_dof2 TO lto_dof.
        ENDLOOP.
      ELSE.
        lto_dof = dofs->auto_dof.
      ENDIF.

      LOOP AT lto_dof INTO lo_dof.
        REFRESH lt_param.
        CLEAR ls_param.
        IF prg8pffp IS NOT INITIAL.
          ls_param-selname = prg8pffp.
        ELSE.
          ls_param-selname = prg8pafp.
        ENDIF.
        ls_param-kind = if_rsda_constants=>selkind-parameter.
        ls_param-low = lo_dof->fullpath.
        APPEND ls_param TO lt_param.
        IF prg8vari IS NOT INITIAL.
          SUBMIT (prg8name)
                USING SELECTION-SET prg8vari
                WITH SELECTION-TABLE lt_param
                AND RETURN.
        ELSE.
          SUBMIT (prg8name)
                WITH SELECTION-TABLE lt_param
                AND RETURN.
        ENDIF.
        lo_dof->delete( ).
      ENDLOOP.

    ELSE.

      "--------------------------------------------------------
      " custom actions
      "--------------------------------------------------------
      " file can be processed only if it's on the frontend
      IF aio_file_system->type = 'S'.
        LOOP AT dofs->auto_dof INTO lo_dof.
          CREATE OBJECT lo_dof2
            EXPORTING
              file_system = aio_frontend
              path        = go_app->aio_frontend->aus_directory-temp
              filename    = lo_dof->filename
              isdir       = 0.
          lo_dof->file_copy( to = lo_dof2 ).
          APPEND lo_dof2 TO lto_dof.
        ENDLOOP.
      ELSE.
        lto_dof = dofs->auto_dof.
      ENDIF.

      DEFINE mac_usrf_ucomm.
        IF ucomm = 'USER&1'.
                l_application = usrf&1prg.
        ENDIF.
      END-OF-DEFINITION.
      mac_usrf_ucomm: 1, 2, 3, 4, 5, 6.

      LOOP AT lto_dof INTO lo_dof.
        l_parameters = lo_dof->fullpath.
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            application = l_application
            parameter   = l_parameters
          EXCEPTIONS
            OTHERS      = 1.
      ENDLOOP.

    ENDIF.

  ENDMETHOD."


  METHOD on_dof_move_requested.
    DATA: lo_dof   TYPE REF TO lcl_dof,
          lo_dofs  TYPE REF TO lcl_dofs,
          l_answer TYPE char1.

    READ TABLE source->auto_dof INDEX 1 INTO lo_dof.
    CHECK sy-subrc = 0.

    IF lo_dof->file_system->type = target->file_system->type AND lo_dof->path = target->path.
      l_answer = ask_closed_question( question = 'What do you want to do?'(081) answer1 = 'Rename'(082) answer2 = 'Copy'(083) ).
      CASE l_answer.
        WHEN '1'.
          CREATE OBJECT lo_dofs EXPORTING dofs = source->auto_dof.
          rename( lo_dofs )."source = source target = target ).
        WHEN '2'.
          copy( source = source target = target ).
      ENDCASE.
    ELSE.
      move( source = source target = target ).
    ENDIF.

  ENDMETHOD."


  METHOD on_dof_copy_requested.
    copy( source = source target = target ).
  ENDMETHOD."


  METHOD on_new_file_requested.
    DATA: l_filename TYPE string,
          xxx        TYPE sdok_filnm,
          l_fullpath TYPE string.

    CHECK dof IS BOUND.

    l_filename = ask_new_dof_name( 'Enter the new file name'(076) ).
    IF l_filename IS NOT INITIAL.
      xxx = l_filename.
      CALL METHOD cl_mime_services=>concatenate_path_name
        EXPORTING
          i_file_name = xxx
          i_directory = dof->path
        IMPORTING
          e_pathname  = l_fullpath.
      aio_file_system->file_create( l_fullpath ).
    ENDIF.
  ENDMETHOD."


  METHOD on_new_folder_requested.
    DATA: l_dirname  TYPE string,
          l_fullpath TYPE string.

    CHECK dof IS BOUND.

    l_dirname = ask_new_dof_name( request = 'Enter the new folder name'(098) directory = abap_true ).
    IF l_dirname IS NOT INITIAL.
      l_fullpath = aio_file_system->get_full_path( name = l_dirname path = dof->path ).
      IF l_fullpath IS NOT INITIAL.
        aio_file_system->directory_create( l_fullpath ).
      ENDIF.
    ENDIF.
  ENDMETHOD."


  METHOD on_dof_delete_requested.
    go_app->aio_confirm_delete->display( dofs = dofs ).
  ENDMETHOD."


  METHOD on_dof_rename_requested.
    go_app->aio_confirm_rename->display( dofs ).
  ENDMETHOD."


  METHOD on_dof_copy_to_clpb_requested.
    aio_cross_action->auo_clipboard_dofs = dofs.
    aio_cross_action->au_clipboard_operation = lif_cross_action=>cs_clpb_operation-copy.
  ENDMETHOD."


  METHOD on_dof_cut_to_clpb_requested.
    aio_cross_action->auo_clipboard_dofs = dofs.
    aio_cross_action->au_clipboard_operation = lif_cross_action=>cs_clpb_operation-cut.
  ENDMETHOD."


  METHOD on_dof_paste_from_clpb_requ.
    DATA: lo_dof    TYPE REF TO lcl_dof.

    READ TABLE dofs->auto_dof INDEX 1 INTO lo_dof.
    IF sy-subrc = 0.
      CASE aio_cross_action->au_clipboard_operation.
        WHEN lif_cross_action=>cs_clpb_operation-copy.
          copy( source = aio_cross_action->auo_clipboard_dofs target = lo_dof ).
        WHEN lif_cross_action=>cs_clpb_operation-cut.
          move( source = aio_cross_action->auo_clipboard_dofs target = lo_dof ).
      ENDCASE.
    ENDIF.
  ENDMETHOD."


  METHOD on_dof_copy_path_2_clpb_requ.
    DATA: lo_dof      TYPE REF TO lcl_dof,
          lt_fullpath TYPE string_table,
          l_fullpaths TYPE string.

    REFRESH lt_fullpath.
    LOOP AT dofs->auto_dof INTO lo_dof.
      APPEND lo_dof->fullpath TO lt_fullpath.
    ENDLOOP.

    CONCATENATE LINES OF lt_fullpath INTO l_fullpaths SEPARATED BY ';'.
    clipboard_export( l_fullpaths ).
  ENDMETHOD."


  METHOD move.
    go_app->aio_confirm_move->display( source = source target = target ).
  ENDMETHOD."


  METHOD copy.
    go_app->aio_confirm_copy->display( source = source target = target ).
  ENDMETHOD."


  METHOD rename.
    go_app->aio_confirm_rename->display( dofs )."source = source target = target ).
  ENDMETHOD."


  METHOD ask_closed_question.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = question
        text_button_1  = answer1
        text_button_2  = answer2
        start_column   = 25
        start_row      = 6
      IMPORTING
        answer         = answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
  ENDMETHOD."


  METHOD ask_new_dof_name.
    DATA: ls_field     TYPE sval,
          lt_field     TYPE TABLE OF sval,
          l_returncode TYPE flag,
          dummy1       TYPE admi_files-filename.
    FIELD-SYMBOLS:
          <ls_field>    TYPE sval.

    REFRESH lt_field.
    IF directory = abap_true.
      ls_field-tabname = 'CST_RSWATCH01_ALV'.
      ls_field-fieldname = 'DIRNAME'.
    ELSE.
      ls_field-tabname = 'ADMI_FILES'.
      ls_field-fieldname = 'FILENAME'.
    ENDIF.
    APPEND ls_field TO lt_field.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = request
      IMPORTING
        returncode      = l_returncode
      TABLES
        fields          = lt_field
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF l_returncode IS INITIAL.
      READ TABLE lt_field WITH KEY fieldname = ls_field-fieldname ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      dof_name = <ls_field>-value.
    ENDIF.
  ENDMETHOD."


  METHOD clipboard_export.
    TYPES ty_ls_clpb_line TYPE c LENGTH 65535.
    DATA: lt_clpb TYPE TABLE OF ty_ls_clpb_line,
          l_rc    TYPE i.

    CALL METHOD lcl_string=>swa_string_to_table
      EXPORTING
        character_string               = i_string
        move_trailing_blanks_next_line = 'X' "<===IMPORTANT
      IMPORTING
        character_table                = lt_clpb[].
    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data                 = lt_clpb
      CHANGING
        rc                   = l_rc
      EXCEPTIONS
        cntl_error           = 0
        error_no_gui         = 1
        not_supported_by_gui = 2.
  ENDMETHOD."


ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_f4_as_dirs IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_f4_as_dirs IMPLEMENTATION.

  METHOD constructor.
    DATA: l_file_sep       TYPE char1,
          l_start_path     TYPE string,
          lt_chunk         TYPE string_table,
          l_chunk          TYPE string,
          l_path           TYPE string,
          lrt_dir          TYPE REF TO data,
          ls_dir           TYPE lcl_appserv=>ty_gs_file,
          l_dir            TYPE string,
          xxx              TYPE sdok_filnm,
          l_relatship      TYPE treev_node-relatship,
          l_text           TYPE lvc_value,
          l_node_key       TYPE salv_de_node_key,
          lt_selected_node TYPE lvc_t_nkey,
          l_node_key_2     TYPE salv_de_node_key,
          lt_node          TYPE TABLE OF mtreesnode,
          ls_node          TYPE mtreesnode.
    FIELD-SYMBOLS:
      <l_first_chunk> TYPE string,
      <lt_dir>        TYPE lcl_appserv=>ty_gt_file,
      <ls_dir>        TYPE lcl_appserv=>ty_gs_file,
      <ls_dir_node>   TYPE ty_is_dir_node,
      <ls_node>       TYPE mtreesnode.

    aio_parent = parent.

    CREATE OBJECT aio_file_system.


    l_start_path = start_path.
    l_file_sep = aio_file_system->get_file_sep( ).

    IF l_file_sep = '/' AND l_start_path(1) <> l_file_sep.
      " si start_path = aaa/bbb/ccc alors path = /aaa/bbb/ccc
      CONCATENATE l_file_sep l_start_path INTO l_start_path.
    ENDIF.
    SPLIT l_start_path AT l_file_sep INTO TABLE lt_chunk.


    CLEAR l_path.

    LOOP AT lt_chunk INTO l_chunk.

      IF l_path IS INITIAL.
        "-----------------------------
        " root node
        "-----------------------------

        " if start_path = /aaa/bbb/ccc then first directory will
        "   be empty so need to change it to /
        " si start_path = c:\aaa\bbb\ccc then first directory will
        "   be c: so need to change it to c:\
        CONCATENATE l_chunk l_file_sep INTO l_path.

        CALL METHOD aio_file_system->directory_list_files
          EXPORTING
            path                 = l_path
            depth                = 1
            only_first_directory = abap_true
          IMPORTING
            files                = lrt_dir.

        airt_dir = lrt_dir.

        lcl_gui_toolbar=>add_toolbar_to_container(
                                  EXPORTING
                                    io_parent           = aio_parent
                                  IMPORTING
                                    eo_splitter         = aio_splitter
                                    eo_toolbar          = aio_toolbar
                                    eo_bottom_container = aio_bottom
                                ).

        DATA lt_event TYPE cntl_simple_events.
        DATA ls_event TYPE LINE OF cntl_simple_events.
        CLEAR ls_event.
        ls_event-eventid = cl_gui_toolbar=>m_id_function_selected.
        APPEND ls_event TO lt_event.
        CALL METHOD aio_toolbar->set_registered_events
          EXPORTING
            events                    = lt_event
          EXCEPTIONS
            cntl_error                = 1
            cntl_system_error         = 2
            illegal_event_combination = 3
            OTHERS                    = 4.
        SET HANDLER on_button_pressed FOR aio_toolbar.

        CALL METHOD aio_toolbar->add_button
          EXPORTING
            fcode     = 'OKAY'
            icon      = icon_okay
            butn_type = cntb_btype_button
            text      = 'Select'(030).
        CALL METHOD aio_toolbar->add_button
          EXPORTING
            fcode     = 'CLOSE'
            icon      = icon_close
            butn_type = cntb_btype_button
            text      = 'Close window'(031).

        auo_tree = create_tree_control( ).

        " add first node
        l_node_key = 'ROOT'.

        ls_dir-name = start_path.

        REFRESH lt_node.
        CLEAR ls_node.
        ls_node-node_key = l_node_key.
        ls_node-isfolder = abap_true. "TODO true if node has children, false otherwise
        ls_node-text = 'ROOT'.
        APPEND ls_node TO lt_node.

        CALL METHOD auo_tree->add_nodes
          EXPORTING
            table_structure_name           = 'MTREESNODE'
            node_table                     = lt_node
          EXCEPTIONS
            error_in_node_table            = 1
            failed                         = 2
            dp_error                       = 3
            table_structure_name_not_found = 4.
        IF sy-subrc <> 0.
*         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ELSE.

        CONCATENATE l_path l_chunk l_file_sep INTO l_path.

        CALL METHOD aio_file_system->directory_list_files
          EXPORTING
            path                 = l_path
            depth                = 1
            only_first_directory = abap_true
          IMPORTING
            files                = lrt_dir.

        " get the node key where TRUNK directory is
        READ TABLE ait_dir_node WITH KEY fullpath = l_path ASSIGNING <ls_dir_node>.
        IF sy-subrc = 0.
          l_node_key = <ls_dir_node>-node_key.
        ENDIF.

      ENDIF.


      " add child nodes
      add_nodes( dirs = lrt_dir parent_node = l_node_key path = l_path ).


      CALL METHOD auo_tree->expand_node
        EXPORTING
          node_key            = l_node_key
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5.
      IF sy-subrc <> 0.
        " TODO
      ENDIF.

*      CALL METHOD auo_tree->set_selected_node
*        EXPORTING
*          node_key                   = l_node_key
*        EXCEPTIONS
*          failed                     = 1
*          single_node_selection_only = 2
*          node_not_found             = 3
*          cntl_system_error          = 4.
*      IF sy-subrc <> 0.
*        " TODO
*      ENDIF.

    ENDLOOP.

  ENDMETHOD."


  METHOD create_tree_control.
    TYPE-POOLS cntl.
    DATA: lt_event       TYPE cntl_simple_events,
          ls_event       TYPE LINE OF cntl_simple_events,
          lrt_dir        TYPE REF TO data,
          ls_variant     TYPE disvariant,
          ls_hier_header TYPE treev_hhdr.
    FIELD-SYMBOLS:
          <lt_dir>      TYPE STANDARD TABLE.

    TRY.

        ASSIGN airt_dir->* TO <lt_dir>.

        CREATE OBJECT ro_tree
          EXPORTING
            parent                      = aio_bottom
            node_selection_mode         = cl_tree_control_base=>node_sel_mode_single
          EXCEPTIONS
            lifetime_error              = 1
            cntl_system_error           = 2
            create_error                = 3
            failed                      = 4
            illegal_node_selection_mode = 5.
        IF sy-subrc <> 0.
*         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        SET HANDLER on_expand FOR ro_tree.

        CLEAR ls_event.
        ls_event-eventid = cl_tree_control_base=>eventid_expand_no_children.
        APPEND ls_event TO lt_event.

        CALL METHOD ro_tree->set_registered_events
          EXPORTING
            events                    = lt_event
          EXCEPTIONS
            cntl_error                = 1
            cntl_system_error         = 2
            illegal_event_combination = 3
            OTHERS                    = 4.


      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD add_nodes.
    TYPE-POOLS icon.
    DATA: l_dir          TYPE string,
          xxx            TYPE sdok_filnm,
          l_relatship    TYPE treev_node-relatship,
          l_text         TYPE lvc_value,
          l_node_key     TYPE salv_de_node_key,
          l_new_node_key TYPE lvc_nkey,
          l_file_sep     TYPE char1,
          ls_dir_node    TYPE ty_is_dir_node,
          lo_cx_root     TYPE REF TO cx_root,
          lt_node        TYPE TABLE OF mtreesnode,
          ls_node        TYPE mtreesnode.
    FIELD-SYMBOLS:
      <lt_dir> TYPE lcl_appserv=>ty_gt_file,
      <ls_dir> TYPE lcl_appserv=>ty_gs_file.


    TRY.

        IF parent_node IS INITIAL.
          CLEAR l_relatship.
        ELSE.
          l_relatship = cl_tree_control_base=>relat_last_child.
        ENDIF.

        l_node_key = parent_node.

        ASSIGN dirs->* TO <lt_dir>.

        l_file_sep = aio_file_system->get_file_sep( ).


        LOOP AT <lt_dir> ASSIGNING <ls_dir>
              WHERE dirname = path
                AND isdir = 1.

          ADD 1 TO ai_node_number.

          CLEAR ls_node.
          ls_node-node_key = ai_node_number.
          ls_node-relatship = l_relatship.
          ls_node-relatkey = parent_node.

          ls_node-isfolder = abap_true.

          xxx = <ls_dir>-name.
          CALL METHOD cl_mime_services=>concatenate_path_name
            EXPORTING
              i_file_name = xxx
              i_directory = path
            IMPORTING
              e_pathname  = l_dir.


          READ TABLE <lt_dir> TRANSPORTING NO FIELDS
                WITH KEY
                  dirname = l_dir
                  isdir = 1.
          IF sy-subrc = 0.
            ls_node-expander = abap_true.
          ELSE.
            ls_node-expander = abap_false.
          ENDIF.

          ls_node-text = <ls_dir>-name.
          APPEND ls_node TO lt_node.


          CONCATENATE path ls_node-text l_file_sep INTO ls_dir_node-fullpath.
          ls_dir_node-node_key = ls_node-node_key.
          APPEND ls_dir_node TO ait_dir_node.
        ENDLOOP.

        CALL METHOD auo_tree->add_nodes
          EXPORTING
            table_structure_name           = 'MTREESNODE'
            node_table                     = lt_node
          EXCEPTIONS
            error_in_node_table            = 1
            failed                         = 2
            dp_error                       = 3
            table_structure_name_not_found = 4.
        IF sy-subrc <> 0.
*           MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      CATCH cx_root INTO lo_cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD on_expand.
    DATA: ls_dir   TYPE lcl_appserv=>ty_gs_file,
          xxx      TYPE sdok_filnm,
          l_string TYPE string,
          l_dir    TYPE string,
          lrt_dir  TYPE REF TO data.
    FIELD-SYMBOLS:
          <ls_dir_node>       TYPE ty_is_dir_node.

    TRY.

        READ TABLE ait_dir_node ASSIGNING <ls_dir_node>
              WITH KEY node_key = node_key.
        CHECK sy-subrc = 0.

        l_dir = <ls_dir_node>-fullpath.

        CALL METHOD aio_file_system->directory_list_files
          EXPORTING
            path                 = l_dir
            depth                = 1
            only_first_directory = abap_true
          IMPORTING
            files                = lrt_dir.

        add_nodes( dirs = lrt_dir parent_node = node_key path = l_dir ).

        cl_gui_cfw=>update_view( ).

      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD on_button_pressed.
    DATA: l_node_key TYPE tv_nodekey,
          lt_node    TYPE lvc_t_nkey,
          ls_node    TYPE lvc_s_nkey,
          ls_dir     TYPE lcl_appserv=>ty_gs_file,
          xxx        TYPE sdok_filnm,
          l_string   TYPE string.
    FIELD-SYMBOLS:
          <ls_dir_node>       TYPE ty_is_dir_node.

    TRY.

        CASE fcode.
          WHEN 'OKAY'.
*            CALL METHOD auo_tree->get_selected_node
*              IMPORTING
*                node_key                   = l_node_key
*              EXCEPTIONS
*                failed                     = 1
*                single_node_selection_only = 2
*                cntl_system_error          = 3.
*            CHECK sy-subrc = 0.
*
*            READ TABLE ait_dir_node ASSIGNING <ls_dir_node>
*                  WITH KEY node_key = l_node_key.
*            CHECK sy-subrc = 0.
*
*            me->folder = <ls_dir_node>-fullpath.

          WHEN 'CLOSE'.
        ENDCASE.

      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD get_selected_directory.
    DATA: l_node_key TYPE tv_nodekey,
          lt_node    TYPE lvc_t_nkey,
          ls_node    TYPE lvc_s_nkey,
          ls_dir     TYPE lcl_appserv=>ty_gs_file,
          xxx        TYPE sdok_filnm,
          l_string   TYPE string.
    FIELD-SYMBOLS:
          <ls_dir_node>       TYPE ty_is_dir_node.

    TRY.

        CALL METHOD auo_tree->get_selected_node
          IMPORTING
            node_key                   = l_node_key
          EXCEPTIONS
            failed                     = 1
            single_node_selection_only = 2
            cntl_system_error          = 3.
        CHECK sy-subrc = 0.

        READ TABLE ait_dir_node ASSIGNING <ls_dir_node>
              WITH KEY node_key = l_node_key.
        CHECK sy-subrc = 0.

        dir = <ls_dir_node>-fullpath.

      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD free.
    IF auo_tree IS BOUND.
      CALL METHOD auo_tree->free.
      FREE auo_tree.
    ENDIF.
    IF aio_bottom IS BOUND.
      CALL METHOD aio_bottom->free.
      FREE aio_bottom.
    ENDIF.
    IF aio_toolbar IS BOUND.
      CALL METHOD aio_toolbar->free.
      FREE aio_toolbar.
    ENDIF.
    IF aio_splitter IS BOUND.
      CALL METHOD aio_splitter->free.
      FREE aio_splitter.
    ENDIF.
  ENDMETHOD."

ENDCLASS."


*
CLASS lcl_ui_dir_tree_builder_fs IMPLEMENTATION.
  METHOD lif_ui_dir_tree_builder~create.
    CREATE OBJECT instance TYPE lcl_ui_dir_tree_fs
      EXPORTING
        parent      = parent
        file_system = file_system
        start_path  = start_path
        pfm         = pfm.
  ENDMETHOD."
ENDCLASS."

*
CLASS lcl_ui_zip_builder IMPLEMENTATION.

  METHOD constructor.
    DATA: l_xstring TYPE xstring.

    dof->read( IMPORTING content = l_xstring ).
    CREATE OBJECT auo_zip.
    auo_zip->load( l_xstring ).
  ENDMETHOD."

  METHOD lif_ui_dir_tree_builder~create.
    CREATE OBJECT instance TYPE lcl_ui_dir_tree_zip
      EXPORTING
        parent      = parent
        file_system = file_system
        start_path  = start_path
        pfm         = pfm
        zip_content = me.
  ENDMETHOD."

  METHOD lif_ui_dir_content_builder~create.
    CREATE OBJECT instance TYPE lcl_ui_dir_content_zip
      EXPORTING
        parent                  = parent
        start_path              = start_path
        file_system             = file_system
        container_dir_input     = container_dir_input
        change_on_selected_dirs = change_on_selected_dirs
        pfm                     = pfm
        zip_content             = me.
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_tree IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_tree IMPLEMENTATION.

  METHOD constructor.

    aio_parent = parent.
    aio_file_system = file_system.
    aio_pfm = pfm.
    ai_start_path = start_path.

    IF aio_parent IS NOT BOUND OR aio_file_system IS NOT BOUND OR start_path IS INITIAL.
      RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'Missing parameter'(026).
    ENDIF.

  ENDMETHOD."


  METHOD create_tree_control.
    TYPE-POOLS cntl.
    DATA: lt_fcat        TYPE lvc_t_fcat,
          lt_event       TYPE cntl_simple_events,
          ls_event       TYPE LINE OF cntl_simple_events,
          lrt_dir        TYPE REF TO data,
          ls_variant     TYPE disvariant,
          ls_hier_header TYPE treev_hhdr.
    FIELD-SYMBOLS:
          <lt_dir>      TYPE STANDARD TABLE.

    TRY.

        ASSIGN airt_dir->* TO <lt_dir>.

        lt_fcat = lcl_alv=>get_lvc_fcat( <lt_dir> ).

        CREATE OBJECT auo_tree
          EXPORTING
            parent              = aio_parent
            node_selection_mode = cl_gui_column_tree=>node_sel_mode_multiple
            no_html_header      = abap_true.

        lif_file_event~sender_control = auo_tree.

        SET HANDLER on_expand FOR auo_tree.
        SET HANDLER on_link_click FOR auo_tree.
        SET HANDLER on_drag_multiple FOR auo_tree.
*        SET HANDLER on_drop FOR auo_tree.
        SET HANDLER on_drop_external_files FOR auo_tree.
        SET HANDLER on_node_context_menu_request FOR auo_tree.
        SET HANDLER on_node_context_menu_selected FOR auo_tree.
        SET HANDLER on_item_context_menu_request FOR auo_tree.
        SET HANDLER on_item_context_menu_selected FOR auo_tree.

        CLEAR ls_event.
        ls_event-eventid = cl_item_tree_control=>eventid_link_click.
        APPEND ls_event TO lt_event.
        CLEAR ls_event.
        ls_event-eventid = cl_item_tree_control=>eventid_expand_no_children.
        APPEND ls_event TO lt_event.
        CLEAR ls_event.
        ls_event-eventid = cl_item_tree_control=>eventid_on_drop_external_files.
        APPEND ls_event TO lt_event.
        CLEAR ls_event.
        ls_event-eventid = cl_tree_control_base=>eventid_node_context_menu_req.
        APPEND ls_event TO lt_event.
        CLEAR ls_event.
        ls_event-eventid = cl_item_tree_control=>eventid_item_context_menu_req.
        APPEND ls_event TO lt_event.

        CALL METHOD auo_tree->set_registered_events
          EXPORTING
            events                    = lt_event
          EXCEPTIONS
            cntl_error                = 1
            cntl_system_error         = 2
            illegal_event_combination = 3
            OTHERS                    = 4.

        " Do not use table AIRT_DIR->* because it's emptied here.
        " Instead, use a dummy internal table with same type.
        CREATE DATA lrt_dir LIKE <lt_dir>.
        ASSIGN lrt_dir->* TO <lt_dir>.

        CLEAR ls_variant.
        ls_variant-report = sy-repid.
        ls_variant-handle = 'H_' && aio_file_system->type.

        CLEAR ls_hier_header.
        ls_hier_header-width = 40.
        ls_hier_header-width_pix = abap_false. "abap_true.
        CALL METHOD auo_tree->set_table_for_first_display
          EXPORTING
            is_variant          = ls_variant
            i_save              = 'A'
            is_hierarchy_header = ls_hier_header
          CHANGING
            it_outtab           = <lt_dir>
            it_fieldcatalog     = lt_fcat.

      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD on_link_click.
    DATA: ls_dir  TYPE ty_is_ndof,
          lx_root TYPE REF TO cx_root.

    TRY.
        CASE fieldname.

          WHEN cl_gui_alv_tree=>c_hierarchy_column_name.
            get_outtab_line( EXPORTING node_key = node_key IMPORTING ndof = ls_dir ).
            RAISE EVENT directory_clicked EXPORTING path = ls_dir-fullpath.

        ENDCASE.

      CATCH cx_root INTO lx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD on_node_context_menu_request.
    CALL METHOD on_context_menu_request( node_key = node_key menu = menu ).
  ENDMETHOD."


  METHOD on_node_context_menu_selected.
    CALL METHOD on_context_menu_selected( node_key = node_key fcode = fcode ).
  ENDMETHOD."


  METHOD on_item_context_menu_request.
    CALL METHOD on_context_menu_request( node_key = node_key menu = menu ).
  ENDMETHOD."


  METHOD on_item_context_menu_selected.
    CALL METHOD on_context_menu_selected( node_key = node_key fcode = fcode ).
  ENDMETHOD."


  METHOD on_context_menu_request.
    " this method handles the node context menu request event of the
    " tree control instance
    "   - MENU : référence vers CL_CTMENU (instanciée par SAP)
    DATA: text TYPE gui_text.

    menu->add_function( text = '----------------------------------------'
                        fcode = ''
                        disabled = abap_true
                        accelerator = ''
                        insert_at_the_top = abap_true ).
    menu->add_function( text = 'Copy file path'(006)
                        fcode = cs_fcode-copy_file_path
                        insert_at_the_top = abap_true ).
    menu->add_function( text = 'Rename'(005)
                        fcode = cs_fcode-rename
                        insert_at_the_top = abap_true ).
    menu->add_function( text = 'Delete'(004)
                        fcode = cs_fcode-delete
                        insert_at_the_top = abap_true ).
    menu->add_function( text = 'New folder'(012)
                        fcode = cs_fcode-new_folder
                        insert_at_the_top = abap_true ).
    menu->add_function( text = 'Paste'(028)
                        fcode = cs_fcode-paste
                        insert_at_the_top = abap_true ).
    menu->add_function( text = 'Cut'(003)
                        fcode = cs_fcode-cut
                        insert_at_the_top = abap_true ).
    menu->add_function( text = 'Copy'(001)
                        fcode = cs_fcode-copy
                        insert_at_the_top = abap_true ).

  ENDMETHOD."


  METHOD on_context_menu_selected.
    " TODO
    CASE fcode.
      WHEN cs_fcode-copy.
      WHEN cs_fcode-cut.
      WHEN cs_fcode-paste.
      WHEN cs_fcode-new_folder.
      WHEN cs_fcode-delete.
      WHEN cs_fcode-rename.
      WHEN cs_fcode-copy_file_path.
    ENDCASE.
  ENDMETHOD."


*  METHOD on_drag.
*    DATA: lo_source TYPE REF TO lcl_dof,
*          ls_dir    TYPE ty_is_ndof,
*          lx_root   TYPE REF TO cx_root.
*
*    TRY.
*        get_outtab_line( EXPORTING node_key = node_key IMPORTING dof = lo_source ndof = ls_dir ).
*        drag_drop_object->object = lo_source.
*      CATCH cx_root INTO lx_root.
*        BREAK-POINT.
*    ENDTRY.
*
*  ENDMETHOD."


  METHOD on_drag_multiple.
    DATA: lto_dof   TYPE lcl_dofs=>ty_uto_dof,
          lo_source TYPE REF TO lcl_dof,
          lo_dofs   TYPE REF TO lcl_dofs,
          ls_dir    TYPE ty_is_ndof,
          lx_root   TYPE REF TO cx_root.
    FIELD-SYMBOLS:
          <l_node_key> TYPE lvc_nkey.

    TRY.
        REFRESH lto_dof.
        LOOP AT node_key_table ASSIGNING <l_node_key>.
          get_outtab_line( EXPORTING node_key = <l_node_key> IMPORTING dof = lo_source ndof = ls_dir ).
          APPEND lo_source TO lto_dof.
        ENDLOOP.
        CREATE OBJECT lo_dofs EXPORTING dofs = lto_dof.
        drag_drop_object->object = lo_dofs.

        " to solve bug selection screen GUI status not restored
        cl_gui_cfw=>set_new_ok_code( '=' ).


      CATCH cx_root INTO lx_root.
        MESSAGE lx_root TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD."


*  METHOD on_drop.
*    DATA: lo_source TYPE REF TO lcl_dofs,
*          xxx       TYPE sdok_filnm,
*          l_path    TYPE string,
*          lo_target TYPE REF TO lcl_dof,
*          lx_root   TYPE REF TO cx_root.
*
*    BREAK-POINT. " TODO: pourquoi est-ce ON_DROP et pas ON_DROP_MULTIPLE ?
*    TRY.
*        get_outtab_line( EXPORTING node_key = node_key IMPORTING dof = lo_target ).
*        lo_source ?= drag_drop_object->object.
*
*        READ TABLE lo_source->auto_dof WITH KEY table_line->isdir = 1 TRANSPORTING NO FIELDS.
*        IF sy-subrc = 0.
*          go_app->aio_dir_copy->display( ).
*          CHECK go_app->aio_dir_copy->choice <> 0.
*        ENDIF.
*
*        CASE drag_drop_object->effect.
*          WHEN cl_dragdrop=>move.
*            RAISE EVENT dof_move_requested EXPORTING source = lo_source target = lo_target.
*          WHEN cl_dragdrop=>copy.
*            RAISE EVENT dof_copy_requested EXPORTING source = lo_source target = lo_target.
*        ENDCASE.
*      CATCH cx_root INTO lx_root.
*        BREAK-POINT.
*    ENDTRY.
*
*  ENDMETHOD."


  METHOD on_drop_external_files.
    DATA: lo_target   TYPE REF TO lcl_dof,
          lo_frontend TYPE REF TO lcl_frontend,
          lt_file     TYPE string_table,
          lo_dof      TYPE REF TO lcl_dof,
          lo_source   TYPE REF TO lcl_dofs,
          lto_dof     TYPE lcl_dofs=>ty_uto_dof,
          lx_root     TYPE REF TO cx_root.
    FIELD-SYMBOLS:
          <l_file>    TYPE string.

    TRY.
        get_outtab_line( EXPORTING node_key = node_key IMPORTING dof = lo_target ).

        CREATE OBJECT lo_frontend TYPE lcl_frontend.
        SPLIT files AT ';' INTO TABLE lt_file.
        LOOP AT lt_file ASSIGNING <l_file>.
          CREATE OBJECT lo_dof
            EXPORTING
              file_system = lo_frontend
              fullpath    = <l_file>.
          APPEND lo_dof TO lto_dof.
        ENDLOOP.
        CREATE OBJECT lo_source
          EXPORTING
            dofs = lto_dof.

        " to solve bug selection screen GUI status not restored
        cl_gui_cfw=>set_new_ok_code( '=' ).


      CATCH cx_root INTO lx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD on_expand.
    DATA: xxx     TYPE sdok_filnm,
          lrt_dir TYPE REF TO data,
          ls_dir  TYPE ty_is_ndof.

    TRY.

        get_outtab_line( EXPORTING node_key = node_key IMPORTING ndof = ls_dir ).

        CALL METHOD aio_file_system->directory_list_files
          EXPORTING
            path                 = ls_dir-fullpath
            depth                = 1
            only_first_directory = abap_true
          IMPORTING
            files                = lrt_dir.

        add_nodes( dirs = lrt_dir parent_node = node_key path = ls_dir-fullpath ).

        cl_gui_cfw=>update_view( ).

      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


ENDCLASS."

*
CLASS lcl_ui_dir_content_builder_fs IMPLEMENTATION.
  METHOD lif_ui_dir_content_builder~create.
    CREATE OBJECT instance TYPE lcl_ui_dir_content_fs
      EXPORTING
        parent                  = parent
        start_path              = start_path
        file_system             = file_system
        container_dir_input     = container_dir_input
        change_on_selected_dirs = change_on_selected_dirs
        pfm                     = pfm.
  ENDMETHOD."
ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_content IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_content IMPLEMENTATION.

  METHOD constructor.
    aio_parent = parent.
    aio_file_system = file_system.
    ai_start_path = start_path.
    aio_pfm = pfm.
    aio_container_dir_input = container_dir_input.
    aio_change_on_selected_dirs = change_on_selected_dirs.
  ENDMETHOD."

  METHOD initialize.
    TYPE-POOLS cntl.
    DATA: l_dirfullpath TYPE string,
          lt_event      TYPE cntl_simple_events,
          ls_event      TYPE cntl_simple_event.

    l_dirfullpath = aio_file_system->normalize_dir_path( ai_start_path ).
    directory_list_files( path = l_dirfullpath ).

    CREATE OBJECT aio_htmlview
      EXPORTING
        parent = aio_container_dir_input.


    REFRESH lt_event.
    CLEAR ls_event.
    ls_event-eventid = cl_gui_html_viewer=>m_id_sapevent.
    ls_event-appl_event = ''. "space = do not trigger PAI
    APPEND ls_event TO lt_event.
    aio_htmlview->set_registered_events( events = lt_event ).

    change_text_dir_input( l_dirfullpath ).

    ai_dirfullpath_index = 1.
    REFRESH ait_dirfullpath.
    APPEND ai_start_path TO ait_dirfullpath.

    create_control( ).

    IF aio_change_on_selected_dirs IS BOUND.
      SET HANDLER on_directory_clicked FOR aio_change_on_selected_dirs.
    ENDIF.

    SET HANDLER on_sapevent FOR aio_htmlview.
    SET HANDLER on_dof_added FOR go_app->aio_frontend.
    SET HANDLER on_dof_added FOR go_app->aio_appserv.
    SET HANDLER on_dof_removed FOR go_app->aio_frontend.
    SET HANDLER on_dof_removed FOR go_app->aio_appserv.

  ENDMETHOD."


  METHOD get_file_icon.
    DEFINE mac_icon_find.
      if fileext&1 is not initial.
        find regex fileext&1 in file_name.
        if sy-subrc = 0.
          icon_name = icodext&1.
          return.
        endif.
      endif.
    END-OF-DEFINITION.

    mac_icon_find: 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f.
    icon_name = icodextz.

  ENDMETHOD."


  METHOD create_control.
    DATA: lt_fcat     TYPE lvc_t_fcat,
          ls_dragdrop TYPE lvc_s_dd01,
          ls_variant  TYPE disvariant,
          ls_layout   TYPE lvc_s_layo.
    FIELD-SYMBOLS:
          <lt_alv>    TYPE STANDARD TABLE.

    ASSIGN airt_alv->* TO <lt_alv>.

    CREATE OBJECT auo_table
      EXPORTING
        i_parent = aio_parent.

    lif_file_event~sender_control = auo_table.

    SET HANDLER on_toolbar FOR auo_table.
    SET HANDLER on_drag FOR auo_table.
    SET HANDLER on_drop FOR auo_table.
    SET HANDLER on_drop_external_files FOR auo_table.
    SET HANDLER on_context_menu_request FOR auo_table.
    SET HANDLER on_user_command FOR auo_table.
    SET HANDLER on_double_click FOR auo_table.

    CLEAR ls_variant.
    ls_variant-report = sy-repid.
    ls_variant-handle = 'T_' && aio_file_system->type.

    CLEAR ls_dragdrop.
    ls_dragdrop-cntr_ddid = aio_pfm->g_dragdrop_handle.
    ls_dragdrop-grid_ddid = space.
    ls_dragdrop-col_ddid  = space.
    ls_dragdrop-row_ddid  = aio_pfm->g_dragdrop_handle.
    ls_dragdrop-fieldname = space.

    CLEAR ls_layout.
    ls_layout-s_dragdrop  = ls_dragdrop.
    ls_layout-sel_mode    = 'A'.

    lt_fcat = lcl_alv=>get_lvc_fcat( <lt_alv> ).
    CALL METHOD auo_table->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = <lt_alv>
        it_fieldcatalog               = lt_fcat    " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'SET_TABLE_FOR_FIRST_DISPLAY'(020).
    ENDIF.

  ENDMETHOD."


  METHOD on_drag.
    DATA: lo_selection TYPE REF TO cl_salv_selections,
          lt_row       TYPE lvc_t_roid,
          ls_row       TYPE lvc_s_roid,
          lrt_alv      TYPE REF TO data,
          lo_dof       TYPE REF TO lcl_dof,
          lto_dof      TYPE lcl_dofs=>ty_uto_dof,
          lo_dofs      TYPE REF TO lcl_dofs.
    FIELD-SYMBOLS:
      <lt_alv>  TYPE STANDARD TABLE,
      <lt_alv2> TYPE STANDARD TABLE,
      <ls_alv>  TYPE any,
      <ls_row>  TYPE lvc_s_roid.

    auo_table->get_selected_rows( IMPORTING et_row_no = lt_row ).
    auo_table->get_current_cell( IMPORTING es_row_no = ls_row ).
    READ TABLE lt_row WITH KEY row_id = ls_row-row_id TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND ls_row TO lt_row.
    ENDIF.

    ASSIGN airt_alv->* TO <lt_alv>.

    CREATE DATA lrt_alv LIKE <lt_alv>.
    ASSIGN lrt_alv->* TO <lt_alv2>.

    LOOP AT lt_row ASSIGNING <ls_row>.
      READ TABLE <lt_alv> ASSIGNING <ls_alv> INDEX <ls_row>-row_id.
      IF sy-subrc = 0.
        lo_dof = get_dof( <ls_alv> ).
        APPEND lo_dof TO lto_dof.
        APPEND <ls_alv> TO <lt_alv2>.
      ENDIF.
    ENDLOOP.

    CREATE OBJECT lo_dofs
      EXPORTING
        dofs = lto_dof.
    e_dragdropobj->object = lo_dofs.

  ENDMETHOD."


  METHOD on_drop.
    DATA: lo_dofs   TYPE REF TO lcl_dofs,
          lo_dof    TYPE REF TO lcl_dof,
          lo_target TYPE REF TO lcl_dof.
    FIELD-SYMBOLS:
      <lt_alv> TYPE STANDARD TABLE,
      <ls_alv> TYPE any.

    TRY.

        lo_dofs ?= e_dragdropobj->object.

        IF e_row IS INITIAL.
          " drop NOT on any line of the ALV grid, but somewhere else in the control
          CREATE OBJECT lo_target
            EXPORTING
              file_system = aio_file_system
              filename    = space
              path        = au_path
              isdir       = 1.
        ELSE.
          ASSIGN airt_alv->* TO <lt_alv>.
          READ TABLE <lt_alv> ASSIGNING <ls_alv> INDEX e_row.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
          lo_target = get_dof( <ls_alv> ).
        ENDIF.


        READ TABLE lo_dofs->auto_dof WITH KEY table_line->isdir = 1 TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          go_app->aio_dir_copy->display( ).
          CHECK go_app->aio_dir_copy->choice <> 0.
        ENDIF.


        CASE e_dragdropobj->effect.
          WHEN cl_dragdrop=>move.
            RAISE EVENT dof_move_requested EXPORTING source = lo_dofs target = lo_target.
          WHEN cl_dragdrop=>copy.
            RAISE EVENT dof_copy_requested EXPORTING source = lo_dofs target = lo_target.
        ENDCASE.

        " to solve bug selection screen GUI status not restored
        cl_gui_cfw=>set_new_ok_code( '=' ).


      CATCH cx_root.
        MESSAGE 'error in on_drop' TYPE 'I'.
    ENDTRY.

  ENDMETHOD."


  METHOD on_drop_external_files.
    DATA: lo_frontend TYPE REF TO lcl_frontend,
          lt_file     TYPE string_table,
          lo_dof      TYPE REF TO lcl_dof,
          lto_dof     TYPE TABLE OF REF TO lcl_dof,
          lo_dofs     TYPE REF TO lcl_dofs,
          lo_target   TYPE REF TO lcl_dof.
    FIELD-SYMBOLS:
          <l_file>    TYPE string.

    CREATE OBJECT lo_frontend.

    SPLIT files AT ';' INTO TABLE lt_file.
    LOOP AT lt_file ASSIGNING <l_file>.
      CREATE OBJECT lo_dof
        EXPORTING
          file_system = lo_frontend
          fullpath    = <l_file>
          isdir       = 0. "really not a directory??
      APPEND lo_dof TO lto_dof.
    ENDLOOP.

    CREATE OBJECT lo_dofs
      EXPORTING
        dofs = lto_dof.

    CREATE OBJECT lo_target
      EXPORTING
        file_system = aio_file_system
        fullpath    = au_path
        isdir       = 1.

    RAISE EVENT dof_copy_requested EXPORTING source = lo_dofs target = lo_target.

  ENDMETHOD."


  METHOD on_context_menu_request.
    DATA:
      lt_menu    TYPE sctx_serialize,
      lt_menu2   TYPE sctx_serialize,
      lo_ctmenu2 TYPE REF TO cl_ctmenu.
    FIELD-SYMBOLS :
          <ls_menu>  TYPE sctx_serialize_entry.


    "---------------------
    " custom menu
    "---------------------


    CREATE OBJECT lo_ctmenu2.
    lo_ctmenu2->add_function( text = 'Open'(007) fcode = cs_fcode-open ).
    lo_ctmenu2->add_function( text = 'Open with'(119) fcode = cs_fcode-open_with ).
    IF prg8text IS NOT INITIAL.
      lo_ctmenu2->add_function( text = prg8text fcode = 'PRG8' ).
    ENDIF.
    lcl_user_settings=>add_menu( lo_ctmenu2 ).
    lo_ctmenu2->add_function( text = 'Copy'(001) fcode = cs_fcode-copy ).
    lo_ctmenu2->add_function( text = 'Cut'(003) fcode = cs_fcode-cut ).
    lo_ctmenu2->add_function( text = 'Paste in selected subdirectory'(027) fcode = cs_fcode-paste_sub ).
    lo_ctmenu2->add_function( text = 'Paste (in current directory)'(002) fcode = cs_fcode-paste ).
    lo_ctmenu2->add_function( text = 'New file'(029) fcode = cs_fcode-new_file ).
    lo_ctmenu2->add_function( text = 'New folder'(012) fcode = cs_fcode-new_folder ).
    lo_ctmenu2->add_function( text = 'Delete'(004) fcode = cs_fcode-delete ).
    lo_ctmenu2->add_function( text = 'Rename'(005) fcode = cs_fcode-rename ).
    lo_ctmenu2->add_function( text = 'Copy file path'(006) fcode = cs_fcode-copy_file_path ).
    lo_ctmenu2->add_function( text = '----------------------------------------'
                              fcode = ''
                              disabled = abap_true
                              accelerator = '' ).

    "---------------------
    " merge custom menu into the input menu
    "---------------------
    e_object->if_ctxmnu_internal~serialize_menu( CHANGING menu = lt_menu ).
    lo_ctmenu2->if_ctxmnu_internal~serialize_menu( CHANGING menu = lt_menu2 ).
    INSERT LINES OF lt_menu2 INTO lt_menu INDEX 1.

    e_object->clear( ).
    lcl_ctmenu=>deserialize_menu( io_ctmenu = e_object it_menu = lt_menu ).


  ENDMETHOD."


  METHOD on_toolbar.
    DATA: ls_toolbar TYPE stb_button,
          lt_toolbar TYPE TABLE OF stb_button.

    REFRESH lt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = cs_fcode-refresh.
    ls_toolbar-icon = icon_refresh.
    APPEND ls_toolbar TO lt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = cs_fcode-up.
    ls_toolbar-icon = icon_previous_hierarchy_level.
    APPEND ls_toolbar TO lt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = cs_fcode-back.
    ls_toolbar-icon = icon_arrow_left.
    APPEND ls_toolbar TO lt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = cs_fcode-forward.
    ls_toolbar-icon = icon_arrow_right.
    APPEND ls_toolbar TO lt_toolbar.

    "---------------
    " add buttons before the first position
    "---------------
    INSERT LINES OF lt_toolbar INTO e_object->mt_toolbar INDEX 1.

  ENDMETHOD."


  METHOD on_user_command.
    DATA: l_file_sep    TYPE char1,
          lt_chunk      TYPE string_table,
          l_dirfullpath TYPE string,
          lt_row        TYPE lvc_t_row,
          ls_row        TYPE lvc_s_row,
          lrt_alv       TYPE REF TO data,
          l_row         TYPE i,
          lo_dof        TYPE REF TO lcl_dof,
          lto_dof       TYPE TABLE OF REF TO lcl_dof,
          lo_dofs       TYPE REF TO lcl_dofs,
          l_value(99)   TYPE c,
          l_col         TYPE i,
          ls_col_id     TYPE lvc_s_col,
          ls_row_no     TYPE lvc_s_roid,
          lo_cx_root    TYPE REF TO cx_root.
    FIELD-SYMBOLS:
      <lt_alv>  TYPE STANDARD TABLE,
      <lt_alv2> TYPE STANDARD TABLE,
      <ls_row>  TYPE lvc_s_row,
      <ls_alv>  TYPE any.

    TRY.

        CASE e_ucomm.

          WHEN cs_fcode-refresh.
            directory_list_files( au_path ).
            refresh_table_display( ).

          WHEN cs_fcode-up.
            l_file_sep = aio_file_system->get_file_sep( ).
            SPLIT au_path AT l_file_sep INTO TABLE lt_chunk.
            DESCRIBE TABLE lt_chunk.
            DELETE lt_chunk INDEX sy-tfill.
            CONCATENATE LINES OF lt_chunk INTO l_dirfullpath SEPARATED BY l_file_sep.
            directory_list_files( l_dirfullpath ).
            refresh_table_display( ).
            RAISE EVENT directory_changed EXPORTING dir_fullpath = l_dirfullpath.
            change_text_dir_input( l_dirfullpath ).
            ADD 1 TO ai_dirfullpath_index.
            DELETE ait_dirfullpath FROM ai_dirfullpath_index.
            APPEND l_dirfullpath TO ait_dirfullpath.


          WHEN cs_fcode-back.
            IF ai_dirfullpath_index > 1.
              SUBTRACT 1 FROM ai_dirfullpath_index.
              READ TABLE ait_dirfullpath INDEX ai_dirfullpath_index INTO l_dirfullpath.
              ASSERT sy-subrc = 0.
              directory_list_files( l_dirfullpath ).
              refresh_table_display( ).
              RAISE EVENT directory_changed EXPORTING dir_fullpath = l_dirfullpath.
              change_text_dir_input( l_dirfullpath ).
            ENDIF.

          WHEN cs_fcode-forward.
            IF ai_dirfullpath_index < lines( ait_dirfullpath ).
              ADD 1 TO ai_dirfullpath_index.
              READ TABLE ait_dirfullpath INDEX ai_dirfullpath_index INTO l_dirfullpath.
              ASSERT sy-subrc = 0.
              directory_list_files( l_dirfullpath ).
              refresh_table_display( ).
              RAISE EVENT directory_changed EXPORTING dir_fullpath = l_dirfullpath.
              change_text_dir_input( l_dirfullpath ).
            ENDIF.

          WHEN OTHERS.

            CALL METHOD sender->get_selected_rows
              IMPORTING
                et_index_rows = lt_row.
            CALL METHOD sender->get_current_cell
              IMPORTING
                e_row     = l_row
                e_value   = l_value
                e_col     = l_col
                es_row_id = ls_row
                es_col_id = ls_col_id
                es_row_no = ls_row_no.
            READ TABLE lt_row WITH KEY table_line = ls_row TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND ls_row TO lt_row.
            ENDIF.

            ASSIGN airt_alv->* TO <lt_alv>.

            CREATE DATA lrt_alv LIKE <lt_alv>.
            ASSIGN lrt_alv->* TO <lt_alv2>.

            LOOP AT lt_row ASSIGNING <ls_row>.
              READ TABLE <lt_alv> ASSIGNING <ls_alv> INDEX <ls_row>-index.
              IF sy-subrc = 0.
                lo_dof = get_dof( <ls_alv> ).
                APPEND lo_dof TO lto_dof.
                APPEND <ls_alv> TO <lt_alv2>.
              ENDIF.
            ENDLOOP.

            CREATE OBJECT lo_dofs
              EXPORTING
                dofs = lto_dof.

            IF lo_dof IS NOT BOUND.
              CREATE OBJECT lo_dof
                EXPORTING
                  file_system = me->aio_file_system
                  fullpath    = me->au_path
                  isdir       = 1.
            ENDIF.

            CASE e_ucomm.
              WHEN cs_fcode-open.
                RAISE EVENT dof_open_requested EXPORTING dofs = lo_dofs.
              WHEN cs_fcode-open_with.
                RAISE EVENT dof_open_with_requested EXPORTING dofs = lo_dofs.
              WHEN cs_fcode-copy.
                RAISE EVENT dof_copy_to_clpb_requested EXPORTING dofs = lo_dofs.
              WHEN cs_fcode-cut.
                RAISE EVENT dof_cut_to_clpb_requested EXPORTING dofs = lo_dofs.
              WHEN cs_fcode-paste.
                RAISE EVENT dof_paste_from_clpb_requested EXPORTING dofs = lo_dofs.
              WHEN cs_fcode-new_file.
                RAISE EVENT new_file_requested EXPORTING dof = lo_dof.
              WHEN cs_fcode-new_folder.
                RAISE EVENT new_folder_requested EXPORTING dof = lo_dof.
              WHEN cs_fcode-delete.
                RAISE EVENT dof_delete_requested EXPORTING dofs = lo_dofs.
              WHEN cs_fcode-rename.
                RAISE EVENT dof_rename_requested EXPORTING dofs = lo_dofs.
              WHEN cs_fcode-copy_file_path.
                RAISE EVENT dof_copy_path_2_clpb_requested EXPORTING dofs = lo_dofs.
              WHEN OTHERS.
                RAISE EVENT dof_custom_action_requested EXPORTING ucomm = e_ucomm dofs = lo_dofs.
            ENDCASE.
        ENDCASE.

      CATCH cx_root INTO lo_cx_root.
        MESSAGE lo_cx_root TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD."


  METHOD on_double_click.
    DATA: ls_ndof TYPE ty_is_ndof,
          lo_dof  TYPE REF TO lcl_dof,
          lto_dof TYPE lcl_dofs=>ty_uto_dof,
          lo_dofs TYPE REF TO lcl_dofs.
    FIELD-SYMBOLS:
      <lt_alv> TYPE STANDARD TABLE,
      <ls_alv> TYPE any.

    ASSIGN airt_alv->* TO <lt_alv>.
    READ TABLE <lt_alv> ASSIGNING <ls_alv> INDEX es_row_no-row_id.
    IF sy-subrc = 0.
      ls_ndof = normalize_file_info( <ls_alv> ).
      CASE ls_ndof-isdir.
        WHEN 0.
          lo_dof = get_dof( <ls_alv> ).
          APPEND lo_dof TO lto_dof.
          CREATE OBJECT lo_dofs
            EXPORTING
              dofs = lto_dof.
          RAISE EVENT dof_open_requested EXPORTING dofs = lo_dofs.

        WHEN 1.
          directory_list_files( ls_ndof-fullpath ).

          ADD 1 TO ai_dirfullpath_index.
          DELETE ait_dirfullpath FROM ai_dirfullpath_index.
          APPEND ls_ndof-fullpath TO ait_dirfullpath.

          refresh_table_display( ).

          RAISE EVENT directory_changed EXPORTING dir_fullpath = ls_ndof-fullpath.
          change_text_dir_input( ls_ndof-fullpath ).

      ENDCASE.
    ENDIF.
  ENDMETHOD."


  METHOD on_directory_clicked.

    IF path = au_path.
      RETURN.
    ENDIF.

    directory_list_files( path ).

    ADD 1 TO ai_dirfullpath_index.
    DELETE ait_dirfullpath FROM ai_dirfullpath_index.
    APPEND path TO ait_dirfullpath.

    refresh_table_display( ).

    RAISE EVENT directory_changed EXPORTING dir_fullpath = path.
    change_text_dir_input( path ).

  ENDMETHOD."


  METHOD on_dof_added.
    DATA: ls_split TYPE lif_file_system=>ty_us_splitted_full_path.

    ls_split = aio_file_system->split_full_path( fullpath ).

    IF ls_split-path = au_path.
      directory_list_files( path = ls_split-path filter = ls_split-name refresh = abap_false ).
      refresh_table_display( ).
    ENDIF.

  ENDMETHOD."


  METHOD on_dof_removed.
    DATA: l_path  TYPE string,
          ls_ndof TYPE ty_is_ndof.
    FIELD-SYMBOLS:
      <lt_alv> TYPE STANDARD TABLE,
      <ls_alv> TYPE any.

    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name = fullpath
      IMPORTING
        file_path = l_path
      EXCEPTIONS
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.

    IF l_path = au_path.
      ASSIGN airt_alv->* TO <lt_alv>.

      LOOP AT <lt_alv> ASSIGNING <ls_alv>.
        ls_ndof = normalize_file_info( <ls_alv> ).
        IF ls_ndof-fullpath = fullpath.
          DELETE <lt_alv>.
          EXIT.
        ENDIF.
      ENDLOOP.
      " REFRESH_TABLE_DISPLAY can do a SORT, so it must be outside LOOP AT
      refresh_table_display( ).
    ENDIF.

  ENDMETHOD."


  METHOD change_text_dir_input.
    DATA: l_text  TYPE string,
          l_fs    TYPE string,
          l_dir   TYPE string,
          lt_text TYPE TABLE OF text255,
          l_size  TYPE i,
          l_url   TYPE cndp_url.

    CASE aio_file_system->type.
      WHEN 'S'.
        l_fs = 'Application server'.
      WHEN 'F'.
        l_fs = 'Frontend'.
    ENDCASE.
    l_fs = cl_http_utility=>escape_html( unescaped = l_fs ).
    REPLACE ALL OCCURRENCES OF ` ` IN l_dir WITH '&nbsp;'.
    REPLACE ALL OCCURRENCES OF '"' IN l_dir WITH '&quot;'.

    l_dir = cl_http_utility=>escape_html( unescaped = text ).
    REPLACE ALL OCCURRENCES OF '"' IN l_dir WITH '&quot;'.

    CONCATENATE
                '<head>'
                '<script language="javascript">'
                'function _onclick() {'
                'document.getElementById("zid").focus();'
                '}'
                '</script>'
*                '<style type="text/css">'
*                'td { font-family: arial; font-size: 12px; }'
*                'input.c1 { border: none; background-color: white; color: red !important; }'
*                '</style>'
                '</head>'
                '<body style="overflow: hidden;">'
                '<div style="position: relative; top: -17px;">'
                '<form method="POST" action="SAPEVENT:submit">'
*                '<table><tr>'
*                '<td>'
                '<input style="border: none; background-color: white;" value="$1" readonly onclick="javascript:_onclick();">'
                '&nbsp;'
*                '</td>'  "<=== $1
*                '<td style="width: 100%;">'
                `<input name="directory" style="width: 100%;" value="$2" id="zid">` "<=== $2
                '&nbsp;'
                `<input type="submit" value="ok">` "<=== $2
*                '</td>'
*                '</tr></table>'
                '</form>'
                '</div>'
                '</body>'
                INTO l_text.

    REPLACE '$1' IN l_text WITH l_fs.
    REPLACE '$2' IN l_text WITH l_dir.

    l_size = strlen( l_text ).
    REFRESH lt_text.
    APPEND l_text TO lt_text.
    CALL FUNCTION 'SWA_STRING_TO_TABLE'
      EXPORTING
        character_string = l_text
      IMPORTING
        character_table  = lt_text[].
    CALL METHOD aio_htmlview->load_data
      EXPORTING
        type         = 'text'
        subtype      = 'html'
        size         = l_size
      IMPORTING
        assigned_url = l_url
      CHANGING
        data_table   = lt_text. "any table of C or X any length
    CALL METHOD aio_htmlview->show_url
      EXPORTING
        url = l_url.

  ENDMETHOD."


  METHOD on_sapevent.
    DATA: lt_simple_postdata TYPE tihttpnvp,
          lt_table_postdata  TYPE lcl_html_viewer_helper=>ty_ut_table_postdata.
    FIELD-SYMBOLS:
         <ls_simple_postdata> TYPE ihttpnvp.

    IF action = 'submit'.

      lcl_html_viewer_helper=>decode_values(  EXPORTING it_postdata = postdata
                      IMPORTING et_simple_postdata = lt_simple_postdata
                                et_table_postdata = lt_table_postdata ).

      READ TABLE lt_simple_postdata WITH KEY name = 'directory' ASSIGNING <ls_simple_postdata>.
      IF sy-subrc = 0 AND <ls_simple_postdata>-value IS NOT INITIAL.

        directory_list_files( <ls_simple_postdata>-value ).

        ADD 1 TO ai_dirfullpath_index.
        DELETE ait_dirfullpath FROM ai_dirfullpath_index.
        APPEND <ls_simple_postdata>-value TO ait_dirfullpath.

        refresh_table_display( ).

      ENDIF.
    ENDIF.
  ENDMETHOD."


  METHOD refresh_table_display.
    DATA ls_layout TYPE lvc_s_layo.

    " bug of save layout? Value '1' is saved when optimize is selected; it
    " does not work (or works as 'X')
    " should be 'A' especially for column "file name"
    auo_table->get_frontend_layout( IMPORTING es_layout = ls_layout ).
    IF ls_layout-cwidth_opt = '1'.
      ls_layout-cwidth_opt = 'A'.
    ENDIF.
    auo_table->set_frontend_layout( is_layout = ls_layout ).

    " refresh layout
    auo_table->refresh_table_display( ).
  ENDMETHOD."


ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_tree_fs IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_tree_fs IMPLEMENTATION.

  METHOD constructor.
    DATA: l_file_sep       TYPE char1,
          l_start_path     TYPE string,
          lt_chunk         TYPE string_table,
          l_chunk          TYPE string,
          l_path           TYPE string,
          lrt_dir          TYPE REF TO data,
          l_dir            TYPE string,
          xxx              TYPE sdok_filnm,
          l_relatship      TYPE treev_node-relatship,
          l_text           TYPE lvc_value,
          l_node_key       TYPE salv_de_node_key,
          lt_selected_node TYPE lvc_t_nkey,
          l_node_key_2     TYPE salv_de_node_key,
          ls_item_layout   TYPE lvc_s_layi,
          lt_item_layout   TYPE lvc_t_layi,
          ls_node_layout   TYPE lvc_s_layn.
    FIELD-SYMBOLS:
      <l_first_chunk> TYPE string,
      <lt_dir>        TYPE STANDARD TABLE,
      <ls_dir>        TYPE any,
      <ls_f_file>     TYPE lcl_frontend=>ty_gs_file,
      <ls_s_file>     TYPE lcl_appserv=>ty_gs_file,
      <ls_dir_node>   TYPE ty_is_dir_node.



    CALL METHOD super->constructor
      EXPORTING
        parent      = parent
        file_system = file_system
        start_path  = start_path
        pfm         = pfm.

    l_start_path = ai_start_path.
    l_file_sep = aio_file_system->get_file_sep( ).

    IF l_file_sep = '/' AND l_start_path(1) <> l_file_sep.
      " if the start_path is aaa/bbb/ccc then it's changed to /aaa/bbb/ccc
      CONCATENATE l_file_sep l_start_path INTO l_start_path.
    ENDIF.
    SPLIT l_start_path AT l_file_sep INTO TABLE lt_chunk.


    CLEAR l_path.

    LOOP AT lt_chunk INTO l_chunk.

      IF l_path IS INITIAL.
        "-----------------------------
        " root node
        "-----------------------------

        " if start_path = /aaa/bbb/ccc then first directory will
        "   be empty so need to change it to /
        " si start_path = c:\aaa\bbb\ccc then first directory will
        "   be c: so need to change it to c:\
        CONCATENATE l_chunk l_file_sep INTO l_path.

        CALL METHOD aio_file_system->directory_list_files
          EXPORTING
            path                 = l_path
            depth                = 1
            only_first_directory = abap_true
          IMPORTING
            files                = lrt_dir.

        airt_dir = lrt_dir.
        ASSIGN airt_dir->* TO <lt_dir>.
        CREATE DATA airs_dir LIKE LINE OF <lt_dir>.
        ASSIGN airs_dir->* TO <ls_dir>.

        create_tree_control( ).

        " add first node
        CLEAR l_node_key.
        CLEAR l_relatship.
        CASE aio_file_system->type.
          WHEN 'F'.
            ASSIGN <ls_dir> TO <ls_f_file>.
            <ls_f_file>-filename = ai_start_path.
          WHEN 'S'.
            ASSIGN <ls_dir> TO <ls_s_file>.
            <ls_s_file>-name = ai_start_path.
        ENDCASE.
        l_text = l_path.
        CLEAR ls_node_layout.
        ls_node_layout-dragdropid = aio_pfm->g_dragdrop_handle.
        ls_node_layout-isfolder = abap_true.
        REFRESH lt_item_layout.
        CLEAR ls_item_layout.
        ls_item_layout-fieldname = cl_gui_alv_tree=>c_hierarchy_column_name.
        ls_item_layout-class = cl_gui_column_tree=>item_class_link.
        APPEND ls_item_layout TO lt_item_layout.
        CALL METHOD auo_tree->add_node
          EXPORTING
            i_relat_node_key     = l_node_key   " Initial Node
            i_relationship       = l_relatship  " cl_gui_column_tree=>relat_last_child
            is_outtab_line       = <ls_dir>
            i_node_text          = l_text
            is_node_layout       = ls_node_layout
            it_item_layout       = lt_item_layout
          IMPORTING
            e_new_node_key       = l_node_key
          EXCEPTIONS
            relat_node_not_found = 1
            node_not_found       = 2
            OTHERS               = 3.
        IF sy-subrc <> 0.
          " TODO
        ENDIF.

      ELSE.

        CONCATENATE l_path l_chunk l_file_sep INTO l_path.

        CALL METHOD aio_file_system->directory_list_files
          EXPORTING
            path                 = l_path
            depth                = 1
            only_first_directory = abap_true
          IMPORTING
            files                = lrt_dir.

        " get the node key where TRUNK directory is
        READ TABLE ait_dir_node WITH KEY fullpath = l_path ASSIGNING <ls_dir_node>.
        IF sy-subrc = 0.
          l_node_key = <ls_dir_node>-node_key.
        ENDIF.

      ENDIF.


      " add child nodes
      add_nodes( dirs = lrt_dir parent_node = l_node_key path = l_path ).


      CALL METHOD auo_tree->expand_node
        EXPORTING
          i_node_key          = l_node_key    " Node Key
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5
          OTHERS              = 6.
      IF sy-subrc <> 0.
        " TODO
      ENDIF.

*      APPEND l_node_key TO lt_selected_node.

    ENDLOOP.


    REFRESH lt_selected_node.
    APPEND l_node_key TO lt_selected_node.
    CALL METHOD auo_tree->set_selected_nodes
      EXPORTING
        it_selected_nodes       = lt_selected_node
      EXCEPTIONS
        cntl_system_error       = 1
        dp_error                = 2
        failed                  = 3
        error_in_node_key_table = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.

  ENDMETHOD."


  METHOD add_nodes.
    TYPE-POOLS icon.
    DATA:
      l_dir          TYPE string,
      xxx            TYPE sdok_filnm,
      l_relatship    TYPE treev_node-relatship,
      l_text         TYPE lvc_value,
      l_node_key     TYPE salv_de_node_key,
      l_new_node_key TYPE lvc_nkey,
      ls_item_layout TYPE lvc_s_layi,
      lt_item_layout TYPE lvc_t_layi,
      ls_node_layout TYPE lvc_s_layn,
      ls_file        TYPE ty_is_ndof,
      lt_file        TYPE TABLE OF ty_is_ndof,
      l_file_sep     TYPE char1,
      ls_dir_node    TYPE ty_is_dir_node.
    FIELD-SYMBOLS:
      <lt_dir> TYPE STANDARD TABLE,
      <ls_dir> TYPE any.


    TRY.

        ASSIGN dirs->* TO <lt_dir>.

        IF parent_node IS INITIAL.
          CLEAR l_relatship.
        ELSE.
          l_relatship = cl_gui_column_tree=>relat_last_child.
        ENDIF.

        l_node_key = parent_node.

        REFRESH lt_item_layout.
        CLEAR ls_item_layout.
        ls_item_layout-fieldname = cl_gui_alv_tree=>c_hierarchy_column_name.
        ls_item_layout-class = cl_gui_column_tree=>item_class_link.
        APPEND ls_item_layout TO lt_item_layout.

        REFRESH lt_file.
        LOOP AT <lt_dir> ASSIGNING <ls_dir>.
          ls_file = normalize_file_info( <ls_dir> ).
          IF ls_file-isdir = 0.
            DELETE <lt_dir>.
          ELSEIF ls_file-path <> path.
            APPEND ls_file TO lt_file.
          ENDIF.
        ENDLOOP.

        l_file_sep = aio_file_system->get_file_sep( ).


        LOOP AT <lt_dir> ASSIGNING <ls_dir>.

          ls_file = normalize_file_info( <ls_dir> ).
          CHECK ls_file-path = path.

          CLEAR ls_node_layout.
          ls_node_layout-dragdropid = aio_pfm->g_dragdrop_handle.
          ls_node_layout-isfolder = abap_true.

          xxx = ls_file-filename.
          CALL METHOD cl_mime_services=>concatenate_path_name
            EXPORTING
              i_file_name = xxx
              i_directory = path
            IMPORTING
              e_pathname  = l_dir.
          READ TABLE lt_file TRANSPORTING NO FIELDS WITH KEY path = l_dir.
          IF sy-subrc = 0.
            ls_node_layout-expander = abap_true.
          ELSE.
            ls_node_layout-expander = abap_false.
          ENDIF.

          l_text = ls_file-filename.

          CALL METHOD auo_tree->add_node
            EXPORTING
              i_relat_node_key = l_node_key   " Node Already in Tree Hierarchy
              i_relationship   = l_relatship  " cl_gui_column_tree=>relat_last_child
              is_outtab_line   = <ls_dir>
              i_node_text      = l_text
              is_node_layout   = ls_node_layout
              it_item_layout   = lt_item_layout
            IMPORTING
              e_new_node_key   = l_new_node_key.

          CONCATENATE path l_text l_file_sep INTO ls_dir_node-fullpath.
          ls_dir_node-node_key = l_new_node_key.
          APPEND ls_dir_node TO ait_dir_node.
        ENDLOOP.

        auo_tree->frontend_update( ).

      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."


  METHOD get_dof.
    DATA: ls_ndof       TYPE ty_is_ndof.

    ls_ndof = normalize_file_info( alv_line ).

    CREATE OBJECT dof
      EXPORTING
        file_system = aio_file_system
        filename    = ls_ndof-filename
        path        = ls_ndof-path
        isdir       = ls_ndof-isdir.

  ENDMETHOD."


  METHOD normalize_file_info.
    DATA: xxx           TYPE sdok_filnm.
    FIELD-SYMBOLS:
      <ls_f_file> TYPE lcl_frontend=>ty_gs_file,
      <ls_s_file> TYPE lcl_appserv=>ty_gs_file.

    CASE aio_file_system->type.
      WHEN 'F'.
        ASSIGN alv_line TO <ls_f_file>.
        ndof-filename    = <ls_f_file>-filename.
        ndof-path        = <ls_f_file>-dir.
        ndof-isdir       = <ls_f_file>-isdir.
      WHEN 'S'.
        ASSIGN alv_line TO <ls_s_file>.
        ndof-filename    = <ls_s_file>-name.
        ndof-path        = <ls_s_file>-dirname.
        ndof-isdir       = <ls_s_file>-isdir.
    ENDCASE.

    IF ndof-path IS INITIAL.
      " ROOT PATH (C:\ or /)
      ndof-fullpath = ndof-filename.
    ELSE.
      xxx = ndof-filename.
      CALL METHOD cl_mime_services=>concatenate_path_name
        EXPORTING
          i_file_name = xxx
          i_directory = ndof-path
        IMPORTING
          e_pathname  = ndof-fullpath.
    ENDIF.
    IF ndof-isdir = 1.
      ndof-fullpath = aio_file_system->normalize_dir_path( ndof-fullpath ).
    ENDIF.

  ENDMETHOD."


  METHOD get_outtab_line.
    FIELD-SYMBOLS:
          <ls_dir>      TYPE any.

    ASSIGN airs_dir->* TO <ls_dir>.

    CALL METHOD auo_tree->get_outtab_line
      EXPORTING
        i_node_key     = node_key
      IMPORTING
        e_outtab_line  = <ls_dir>
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      IF dof IS SUPPLIED.
        dof = get_dof( <ls_dir> ).
      ENDIF.
      IF ndof IS SUPPLIED.
        ndof = normalize_file_info( <ls_dir> ).
      ENDIF.
    ENDIF.

  ENDMETHOD."

ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_content IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_content_fs IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        parent                  = parent
        start_path              = start_path
        file_system             = file_system
        container_dir_input     = container_dir_input
        change_on_selected_dirs = change_on_selected_dirs
        pfm                     = pfm.
    initialize( ).
  ENDMETHOD."

  METHOD directory_list_files.
    DATA: ls_f_alv TYPE ty_is_f_alv,
          ls_s_alv TYPE ty_is_s_alv.
    FIELD-SYMBOLS:
      <lt_f_file> TYPE lcl_frontend=>ty_gt_file,
      <ls_f_file> TYPE lcl_frontend=>ty_gs_file,
      <lt_s_file> TYPE lcl_appserv=>ty_gt_file,
      <ls_s_file> TYPE lcl_appserv=>ty_gs_file,
      <lt_alv>    TYPE STANDARD TABLE.

    " stocker AU_PATH avec / ou \ final, pour normaliser les comparaisons
    au_path = aio_file_system->normalize_dir_path( path ).

    CALL METHOD aio_file_system->directory_list_files
      EXPORTING
        path   = au_path
        filter = filter
      IMPORTING
        files  = airt_file.

    CASE aio_file_system->type.
      WHEN 'F'.
        ASSIGN airt_file->* TO <lt_f_file>.
        IF airt_alv IS NOT BOUND.
          CREATE DATA airt_alv TYPE ty_it_f_alv.
        ENDIF.
        ASSIGN airt_alv->* TO <lt_alv>.
        IF refresh = abap_true.
          REFRESH <lt_alv>.
        ENDIF.
        LOOP AT <lt_f_file> ASSIGNING <ls_f_file>.
          CLEAR ls_f_alv.
          IF <ls_f_file>-isdir = 1.
            ls_f_alv-icon = icon_folder.
          ELSE.
            ls_f_alv-icon = get_file_icon( <ls_f_file>-filename ).
          ENDIF.
          ls_f_alv-s_file = <ls_f_file>.
          APPEND ls_f_alv TO <lt_alv>.
        ENDLOOP.

      WHEN 'S'.
        ASSIGN airt_file->* TO <lt_s_file>.
        IF airt_alv IS NOT BOUND.
          CREATE DATA airt_alv TYPE ty_it_s_alv.
        ENDIF.
        ASSIGN airt_alv->* TO <lt_alv>.
        IF refresh = abap_true.
          REFRESH <lt_alv>.
        ENDIF.
        LOOP AT <lt_s_file> ASSIGNING <ls_s_file>.
          CLEAR ls_s_alv.
          IF <ls_s_file>-isdir = 1.
            ls_s_alv-icon = icon_folder.
          ELSE.
            ls_s_alv-icon = get_file_icon( <ls_s_file>-name ).
          ENDIF.
          ls_s_alv-s_file = <ls_s_file>.
          APPEND ls_s_alv TO <lt_alv>.
        ENDLOOP.
    ENDCASE.

  ENDMETHOD."


  METHOD get_dof.
    DATA: ls_ndof       TYPE ty_is_ndof.

    ls_ndof = normalize_file_info( alv_line ).

    CREATE OBJECT dof
      EXPORTING
        file_system = aio_file_system
        filename    = ls_ndof-filename
        path        = ls_ndof-dir
        isdir       = ls_ndof-isdir.

  ENDMETHOD."


  METHOD normalize_file_info.
    DATA: xxx           TYPE sdok_filnm.
    FIELD-SYMBOLS:
      <ls_f_file> TYPE ty_is_f_alv,
      <ls_s_file> TYPE ty_is_s_alv.

    CASE aio_file_system->type.
      WHEN 'F'.
        ASSIGN alv_line TO <ls_f_file>.
        ndof-filename    = <ls_f_file>-filename.
        ndof-dir         = <ls_f_file>-dir.
        ndof-isdir       = <ls_f_file>-isdir.
      WHEN 'S'.
        ASSIGN alv_line TO <ls_s_file>.
        ndof-filename    = <ls_s_file>-name.
        ndof-dir         = <ls_s_file>-dirname.
        ndof-isdir       = <ls_s_file>-isdir.
    ENDCASE.

    xxx = ndof-filename.
    CALL METHOD cl_mime_services=>concatenate_path_name
      EXPORTING
        i_file_name = xxx
        i_directory = ndof-dir
      IMPORTING
        e_pathname  = ndof-fullpath.

  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_tree_zip IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_tree_zip IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        parent      = parent
        file_system = file_system
        start_path  = start_path
        pfm         = pfm.
    DATA: l_file_sep       TYPE char1,
          l_start_path     TYPE string,
          lt_chunk         TYPE string_table,
          l_chunk          TYPE string,
          l_path           TYPE string,
          lrt_dir          TYPE REF TO data,
          l_dir            TYPE string,
          xxx              TYPE sdok_filnm,
          l_relatship      TYPE treev_node-relatship,
          l_text           TYPE lvc_value,
          l_node_key       TYPE salv_de_node_key,
          lt_selected_node TYPE lvc_t_nkey,
          l_node_key_2     TYPE salv_de_node_key,
          ls_item_layout   TYPE lvc_s_layi,
          lt_item_layout   TYPE lvc_t_layi,
          ls_node_layout   TYPE lvc_s_layn.
    FIELD-SYMBOLS:
      <l_first_chunk> TYPE string,
      <lt_dir>        TYPE STANDARD TABLE,
      <ls_dir>        TYPE any,
      <ls_f_file>     TYPE lcl_frontend=>ty_gs_file,
      <ls_s_file>     TYPE lcl_appserv=>ty_gs_file,
      <ls_dir_node>   TYPE ty_is_dir_node.


    GET REFERENCE OF ait_dir INTO airt_dir.
    ASSIGN airt_dir->* TO <lt_dir>.
    CREATE DATA airs_dir LIKE LINE OF <lt_dir>.
    ASSIGN airs_dir->* TO <ls_dir>.

    create_tree_control( ).

    " add first node
    CLEAR l_node_key.
    CLEAR l_relatship.
    l_text = '\'.
    CLEAR ls_node_layout.
    ls_node_layout-dragdropid = aio_pfm->g_dragdrop_handle.
    ls_node_layout-isfolder = abap_true.
    REFRESH lt_item_layout.
    CLEAR ls_item_layout.
    ls_item_layout-fieldname = cl_gui_alv_tree=>c_hierarchy_column_name.
    ls_item_layout-class = cl_gui_column_tree=>item_class_link.
    APPEND ls_item_layout TO lt_item_layout.

    CALL METHOD auo_tree->add_node
      EXPORTING
        i_relat_node_key     = l_node_key   " Initial Node
        i_relationship       = l_relatship  " cl_gui_column_tree=>relat_last_child
        is_outtab_line       = <ls_dir>
        i_node_text          = l_text
        is_node_layout       = ls_node_layout
        it_item_layout       = lt_item_layout
      IMPORTING
        e_new_node_key       = l_node_key
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.


    " add child nodes
    add_nodes( dirs = airt_dir parent_node = l_node_key path = l_path ).


  ENDMETHOD."


  METHOD add_nodes.
    TYPE-POOLS icon.
    DATA:
      l_dir          TYPE string,
      l_relatship    TYPE treev_node-relatship,
      l_text         TYPE lvc_value,
      l_node_key     TYPE salv_de_node_key,
      l_new_node_key TYPE lvc_nkey,
      ls_item_layout TYPE lvc_s_layi,
      lt_item_layout TYPE lvc_t_layi,
      ls_node_layout TYPE lvc_s_layn,
      ls_file        TYPE ty_is_ndof,
      lt_file        TYPE TABLE OF ty_is_ndof,
      l_file_sep     TYPE char1,
      ls_dir_node    TYPE ty_is_dir_node.
    FIELD-SYMBOLS:
      <lt_dir> TYPE ty_it_dir,
      <ls_dir> TYPE ty_is_dir.


    TRY.

        ASSIGN dirs->* TO <lt_dir>.

        IF parent_node IS INITIAL.
          CLEAR l_relatship.
        ELSE.
          l_relatship = cl_gui_column_tree=>relat_last_child.
        ENDIF.

        l_node_key = parent_node.

        REFRESH lt_item_layout.
        CLEAR ls_item_layout.
        ls_item_layout-fieldname = cl_gui_alv_tree=>c_hierarchy_column_name.
        ls_item_layout-class = cl_gui_column_tree=>item_class_link.
        APPEND ls_item_layout TO lt_item_layout.

        LOOP AT <lt_dir> ASSIGNING <ls_dir>.

          CLEAR ls_node_layout.
          ls_node_layout-dragdropid = aio_pfm->g_dragdrop_handle.
          ls_node_layout-isfolder = abap_true.

          READ TABLE lt_file TRANSPORTING NO FIELDS WITH KEY path = l_dir.
          IF sy-subrc = 0.
            ls_node_layout-expander = abap_true.
          ELSE.
            ls_node_layout-expander = abap_false.
          ENDIF.

          l_text = <ls_dir>-name.

          CALL METHOD auo_tree->add_node
            EXPORTING
              i_relat_node_key = l_node_key   " Node Already in Tree Hierarchy
              i_relationship   = l_relatship  " cl_gui_column_tree=>relat_last_child
              is_outtab_line   = <ls_dir>
              i_node_text      = l_text
              is_node_layout   = ls_node_layout
              it_item_layout   = lt_item_layout
            IMPORTING
              e_new_node_key   = l_new_node_key.

        ENDLOOP.

        auo_tree->frontend_update( ).

      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD."

  METHOD get_outtab_line.
  ENDMETHOD."

  METHOD get_dof.
  ENDMETHOD."

  METHOD normalize_file_info.
  ENDMETHOD."

ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_ui_dir_content_zip IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ui_dir_content_zip IMPLEMENTATION.

  METHOD constructor.
*    call method super->constructor
*          EXPORTING
*            parent = parent
*            file_system = file_system
*            start_path = start_path
*            pfm = pfm.
  ENDMETHOD."

*  METHOD initialize.
*  ENDMETHOD."
*
*  METHOD create_tree_control.
*  ENDMETHOD."
*
*  METHOD add_nodes.
*  ENDMETHOD."
*
*  METHOD get_outtab_line.
*  ENDMETHOD."
*
*  METHOD get_dof.
*  ENDMETHOD."
*
*  METHOD normalize_file_info.
*  ENDMETHOD."

ENDCLASS."

*----------------------------------------------------------------------*
*       CLASS lcl_popup_confirm IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_popup_confirm IMPLEMENTATION.

  METHOD set_data.
    DATA:
      ls_confirm               TYPE ty_us_confirm.
*      ls_style_binary_disabled TYPE lvc_s_styl.
    FIELD-SYMBOLS:
                   <ls_confirm> TYPE ty_us_confirm.

    aot_confirm = it_confirm.

    READ TABLE aot_confirm INDEX 1 INTO ls_confirm.
    IF sy-subrc = 0.
      aoo_source_dof = ls_confirm-o_source_dof.
      aoo_target_dof = ls_confirm-o_target_dof.
      ao_target_dirpath = ls_confirm-target_dirpath.
    ENDIF.

    ls_confirm-selected = abap_true.
    MODIFY aot_confirm FROM ls_confirm TRANSPORTING selected WHERE selected <> ls_confirm-selected.

*    ls_style_binary_disabled-fieldname = lcl_rtti=>get_component_name( is_any = ls_confirm i_structure_field = ls_confirm-binary ).
*    ls_style_binary_disabled-style = cl_gui_alv_grid=>mc_style_disabled.
    LOOP AT aot_confirm ASSIGNING <ls_confirm>.
      <ls_confirm>-selected = abap_true.
*      IF txtcopy CA 'AN' "always/never
*            AND abap_undefined <> go_app->is_text_file( <ls_confirm>-o_source_dof->fullpath ).
*        APPEND ls_style_binary_disabled TO <ls_confirm>-t_style.
*      ENDIF.
      IF abap_false = go_app->is_text_file( <ls_confirm>-o_source_dof->fullpath ).
        <ls_confirm>-binary = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD."


  METHOD lif_sscr~pbo.
    DATA: l_error     TYPE abap_bool,
          ls_confirm  TYPE ty_us_confirm,
          l_fullpath  TYPE string,
          lt_field    TYPE lvc_t_fnam,
          l_fieldname TYPE string,
          lt_fcode    TYPE ui_functions.
    FIELD-SYMBOLS:
      <ls_confirm> TYPE ty_us_confirm,
      <ls_fcat>    TYPE lvc_s_fcat.

    IF auo_alv IS NOT BOUND.

      ai_dialog = abap_true.

      READ TABLE aot_confirm ASSIGNING <ls_confirm> INDEX 1.
      ASSERT sy-subrc = 0.

      l_error = check_files( ).

      IF l_error = abap_false AND alwconfi = abap_false AND abap_true = skip_dialog( ).
        SUPPRESS DIALOG.
        sscrfields-ucomm = 'CRET'.
        ai_dialog = abap_false.
      ENDIF.

      aut_fcat = lcl_alv=>get_lvc_fcat( aot_confirm ).

      CREATE OBJECT auo_alv
        EXPORTING
          i_parent = cl_gui_container=>screen1.

      auo_alv->set_ready_for_input( 1 ).


      ais_layout-no_toolbar = abap_true.
      ais_layout-cwidth_opt = 'A'.
      ais_layout-stylefname = lcl_rtti=>get_component_name( is_any = ls_confirm i_structure_field = ls_confirm-t_style ).


      "          SELECTED  BINARY  ORIGINAL_NAME  NEW_NAME  DONE  MESSAGE  button  OVERWRITTEN_FILE  OVERWRITE  TARGET_DIRPATH
      "          --------  ------  -------------  --------  ----  -------  ------  ----------------  ---------  ---------
      " MOVE     X         X       X              X         X     X                X                 X          X
      " COPY     X         X       X              X         X     X                X                 X          X
      " RENAME   X                 X              X         X     X
      " DELETE   X                 X                        X     X

      DEFINE mac_alv_confirm.
        l_fieldname = lcl_rtti=>get_component_name( is_any = ls_confirm i_structure_field = ls_confirm-&1 ).
        READ TABLE aut_fcat ASSIGNING <ls_fcat> WITH KEY fieldname = l_fieldname.
        IF sy-subrc = 0.
          <ls_fcat>-tech      = &2.
          <ls_fcat>-no_out    = &2.
          <ls_fcat>-checkbox  = &3.
          <ls_fcat>-edit      = &4.
          <ls_fcat>-scrtext_l = &5.
          <ls_fcat>-scrtext_m = &5.
          <ls_fcat>-scrtext_s = &5.
        ENDIF.
      END-OF-DEFINITION.

      mac_alv_confirm:" NAME              TECHNICAL CHECKBOX EDIT COLUMN HEADING
                        selected          ''        'X'      'X'  'Process'(084),
                        message           ''        ''       ''   'Message'(085),
                        button            ''        ''       ''   'Overwrite'(086),
                        binary            ''        'X'      'X'  'Binary file'(087),
                        original_name     ''        ''       ''   'Original name'(088),
                        new_name          ''        ''       'X'  'New name'(089),
                        done              'X'       ''       ''   'Done', "#EC NOTEXT
                        overwrite         'X'       ''       ''   'Overwrite', "#EC NOTEXT
                        overwritten_file  'X'       ''       ''   'Overwritten file', "#EC NOTEXT
                        target_dirpath    'X'       ''       ''   'TARGET_DIRPATH'. "#EC NOTEXT


*      SET HANDLER on_toolbar FOR auo_alv.

      CALL METHOD auo_alv->set_table_for_first_display
        EXPORTING
          is_layout       = ais_layout
        CHANGING
          it_fieldcatalog = aut_fcat
          it_outtab       = aot_confirm.

      refresh( ).

      SET HANDLER on_button_click FOR auo_alv.

    ENDIF.


    sscrfields-functxt_01 = 'Overwrite selected files'.
    sscrfields-functxt_02 = 'Skip selected files'.
    sscrfields-functxt_03 = 'Binary mode for selected files'.

    " hide all buttons except okay, exit, FC01, FC02
    REFRESH lt_fcode.
    APPEND 'CRET' TO lt_fcode.
    APPEND 'CCAN' TO lt_fcode.
    APPEND 'FC02' TO lt_fcode.
    adjust_buttons( CHANGING ct_fcode = lt_fcode ).

    lcl_sscr=>disable_all_function_codes( i_pf_status = '%_CSP' except = lt_fcode ).

    " Avoid the blinking (flash display of selection screen)
    " when the GUI control is freed
    LOOP AT SCREEN.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD."


  METHOD lif_sscr~pai.
    DATA: lt_row     TYPE lvc_t_roid,
          l_error    TYPE abap_bool,
          l_message1 TYPE string,
          l_message2 TYPE string,
          ls_result  TYPE ty_us_result,
          lx_root    TYPE REF TO cx_root,
          ls_style   TYPE lvc_s_styl.
    FIELD-SYMBOLS:
      <ls_row>     TYPE lvc_s_roid,
      <ls_confirm> TYPE ty_us_confirm.

    CASE sscrfields-ucomm.

      WHEN 'FC01'.
        " overwrite selected files
        auo_alv->get_selected_rows( IMPORTING et_row_no = lt_row ).
        LOOP AT lt_row ASSIGNING <ls_row>.
          confirm( <ls_row> ).
        ENDLOOP.
        refresh( ).

      WHEN 'FC02'.
        " skip selected files
        auo_alv->get_selected_rows( IMPORTING et_row_no = lt_row ).
        LOOP AT lt_row ASSIGNING <ls_row>.
          READ TABLE aot_confirm INDEX <ls_row>-row_id ASSIGNING <ls_confirm>.
          IF sy-subrc = 0 AND <ls_confirm>-done = abap_false.
            IF <ls_confirm>-selected = abap_false.
              <ls_confirm>-selected = abap_true.
            ELSE.
              <ls_confirm>-selected = abap_false.
            ENDIF.
          ENDIF.
        ENDLOOP.
        refresh( ).

      WHEN 'FC03'.
        " text or binary mode for selected files
        auo_alv->get_selected_rows( IMPORTING et_row_no = lt_row ).
        LOOP AT lt_row ASSIGNING <ls_row>.
          READ TABLE aot_confirm INDEX <ls_row>-row_id ASSIGNING <ls_confirm>.
          IF sy-subrc = 0 AND <ls_confirm>-done = abap_false.
            IF <ls_confirm>-binary = abap_false.
              <ls_confirm>-binary = abap_true.
            ELSE.
              <ls_confirm>-binary = abap_false.
            ENDIF.
          ENDIF.
        ENDLOOP.
        refresh( ).

      WHEN 'CRET'.
        auo_alv->check_changed_data( ).

        " if there's no line selected (either manually either automatically if
        " all files have been "processed"), then leave the dialog.
        READ TABLE aot_confirm WITH KEY selected = abap_true TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          " close the dialog
          CALL METHOD lif_sscr~exit.
          RETURN.
        ENDIF.

        l_error = check_files( ).

        IF l_error = abap_false.

          " change message of all selected files before processing them
          LOOP AT aot_confirm ASSIGNING <ls_confirm> WHERE selected = abap_true.
            before_operation( CHANGING cs_confirm = <ls_confirm> ).
          ENDLOOP.

          LOOP AT aot_confirm ASSIGNING <ls_confirm> WHERE selected = abap_true.

            CLEAR ls_result.
            TRY.

                execute_operation( CHANGING cs_confirm = <ls_confirm> ).

              CATCH cx_root INTO lx_root.
                <ls_confirm>-done = abap_false.
                <ls_confirm>-message = lx_root->get_text( ).
                CONCATENATE icon_cancel ` ` <ls_confirm>-message INTO <ls_confirm>-message.
            ENDTRY.

            IF <ls_confirm>-done = abap_false.
              ai_dialog = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.

        ENDIF.

        refresh( ).

        IF ai_dialog = abap_true.
          " display the dialog again
          CLEAR sscrfields-ucomm.
        ELSE.
          CALL METHOD lif_sscr~exit.
        ENDIF.

      WHEN OTHERS.
        CLEAR sscrfields-ucomm.

    ENDCASE.
  ENDMETHOD."


  METHOD lif_sscr~exit.
    IF auo_alv IS BOUND.
      auo_alv->free( ).
      FREE auo_alv.
    ENDIF.
  ENDMETHOD."


  METHOD check_files.
    DATA: l_error TYPE abap_bool,
          lx_root TYPE REF TO cx_root.
    FIELD-SYMBOLS:
          <ls_confirm>    TYPE ty_us_confirm.

    error = abap_false.

    LOOP AT aot_confirm ASSIGNING <ls_confirm> WHERE done = abap_false.

      l_error = abap_false.
      TRY.
          check_file( IMPORTING error = l_error CHANGING cs_confirm = <ls_confirm> ).
        CATCH cx_root INTO lx_root.
          l_error = abap_true.
      ENDTRY.

      IF l_error = abap_true AND <ls_confirm>-selected = abap_true.
        error = abap_true.
      ENDIF.

    ENDLOOP.
  ENDMETHOD."


  METHOD skip_dialog.
    result = abap_false.
  ENDMETHOD."


  METHOD refresh.
    DATA: ls_stable      TYPE lvc_s_stbl,
          l_soft_refresh TYPE char01,
          lt_row         TYPE lvc_t_roid,
          ls_style       TYPE lvc_s_styl.
    FIELD-SYMBOLS:
      <ls_confirm> TYPE ty_us_confirm,
      <ls_style>   TYPE lvc_s_styl.

    auo_alv->get_selected_rows( IMPORTING et_row_no = lt_row ).
    ls_stable-col = abap_true.
    ls_stable-row = abap_true.
    l_soft_refresh = abap_true.

    LOOP AT aot_confirm ASSIGNING <ls_confirm>.
      IF <ls_confirm>-button IS NOT INITIAL.
        CLEAR ls_style.
        ls_style-fieldname = lcl_rtti=>get_component_name( is_any = <ls_confirm> i_structure_field = <ls_confirm>-button ).
        READ TABLE <ls_confirm>-t_style WITH KEY fieldname = ls_style-fieldname ASSIGNING <ls_style>.
        IF sy-subrc <> 0.
          ls_style-style = cl_gui_alv_grid=>mc_style_button.
          INSERT ls_style INTO TABLE <ls_confirm>-t_style.
        ELSE.
          <ls_style>-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.
      ENDIF.
      IF <ls_confirm>-done = abap_true.
        <ls_confirm>-selected = abap_false.
        " disable all CELLS in this ROW
        CLEAR ls_style.
        ls_style-fieldname = space. "all columns
        READ TABLE <ls_confirm>-t_style WITH KEY fieldname = ls_style-fieldname ASSIGNING <ls_style>.
        IF sy-subrc <> 0.
          ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT ls_style INTO TABLE <ls_confirm>-t_style.
        ELSE.
          <ls_style>-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
      ENDIF.
    ENDLOOP.

    auo_alv->refresh_table_display(
          is_stable       = ls_stable
          i_soft_refresh  = l_soft_refresh
          ).
    auo_alv->set_selected_rows( it_row_no = lt_row ).

  ENDMETHOD."


  METHOD on_toolbar.
*    TYPE-POOLS: cntb.
*    DATA: ls_toolbar TYPE stb_button,
*          lt_toolbar TYPE TABLE OF stb_button.
*
*    REFRESH lt_toolbar.
**    sscrfields-functxt_01 = 'Overwrite selected files'.
**    sscrfields-functxt_02 = 'Skip selected files'.
**    sscrfields-functxt_03 = 'Binary mode for selected files'.
*    CLEAR ls_toolbar.
*    ls_toolbar-function = 'FC01'.
**  ls_toolbar-icon = 'ICON_FC01'.
*    ls_toolbar-text = 'Overwrite selected files'. "TODO translatable
*    ls_toolbar-butn_type = cntb_btype_button.
*    APPEND ls_toolbar TO lt_toolbar.
*    CLEAR ls_toolbar.
*    ls_toolbar-function = 'FC02'.
**  ls_toolbar-icon = 'ICON_FC01'.
*    ls_toolbar-text = 'Skip selected files'. "TODO translatable
*    ls_toolbar-butn_type = cntb_btype_button.
*    APPEND ls_toolbar TO lt_toolbar.
*    CLEAR ls_toolbar.
*    ls_toolbar-function = 'FC03'.
**  ls_toolbar-icon = 'ICON_FC01'.
*    ls_toolbar-text = 'Binary mode for selected files'. "TODO translatable
*    ls_toolbar-butn_type = cntb_btype_button.
*    APPEND ls_toolbar TO lt_toolbar.
*
*    INSERT LINES OF lt_toolbar INTO e_object->mt_toolbar INDEX 1.

  ENDMETHOD."


  METHOD on_button_click.
    " ES_COL_ID     TYPE LVC_S_COL
    " ES_ROW_NO     TYPE LVC_S_ROID
    DATA: ls_confirm  TYPE ty_us_confirm,
          l_fieldname TYPE string.
    FIELD-SYMBOLS:
          <ls_confirm>    TYPE ty_us_confirm.

    l_fieldname = lcl_rtti=>get_component_name( is_any = ls_confirm i_structure_field = ls_confirm-button ).
    IF es_col_id-fieldname = l_fieldname.
      confirm( es_row_no ).
      refresh( ).
    ENDIF.
  ENDMETHOD."


  METHOD confirm.
    FIELD-SYMBOLS:
          <ls_confirm>    TYPE ty_us_confirm.

    READ TABLE aot_confirm INDEX is_row-row_id ASSIGNING <ls_confirm>.
    IF sy-subrc = 0.
      auo_alv->check_changed_data( ).
      CASE <ls_confirm>-button.
        WHEN 'Overwrite'.
          <ls_confirm>-button = 'Cancel overwrite'.
          <ls_confirm>-message = icon_led_yellow && ` ` && 'File exists but will be overwritten'.
          <ls_confirm>-overwritten_file = <ls_confirm>-new_name.
          <ls_confirm>-overwrite = abap_true.
        WHEN 'Cancel overwrite'.
          <ls_confirm>-button = 'Overwrite'.
          <ls_confirm>-message = icon_led_red && ` ` && 'File already exists'.
          <ls_confirm>-overwritten_file = space.
          <ls_confirm>-overwrite = abap_false.
      ENDCASE.
    ENDIF.
  ENDMETHOD."


  METHOD prepare_confirm_dialog.
    DATA: lo_dof     TYPE REF TO lcl_dof,
          ls_confirm TYPE lcl_popup_confirm=>ty_us_confirm.

    REFRESH t_confirm.
    LOOP AT source->auto_dof INTO lo_dof.
      CLEAR ls_confirm.
      ls_confirm-original_name = lo_dof->filename.
      ls_confirm-new_name = lo_dof->filename.
      ls_confirm-o_source_dof = lo_dof.
      ls_confirm-o_target_dof = target.
      IF target IS BOUND.
        ls_confirm-target_dirpath = target->path.
      ENDIF.
      APPEND ls_confirm TO t_confirm.
    ENDLOOP.
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_move IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_confirm_move IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor.
    operation = cs_operation-move.
*    aio_frontend = io_frontend.
*    aio_appserv = io_appserv.
  ENDMETHOD."


  METHOD display.
*    CALL METHOD aio_cross_action->start_confirm_dialog( aio_confirm_move ).

    DATA: lt_confirm    TYPE lcl_popup_confirm=>ty_ut_confirm.

    prepare_confirm_dialog( EXPORTING source = source target = target IMPORTING t_confirm = lt_confirm ).
    go_app->aio_confirm_move->set_data( lt_confirm ).
    CALL SELECTION-SCREEN 1002 STARTING AT 30 10 ENDING AT 130 30.
  ENDMETHOD."


  METHOD adjust_field_catalog.
*    DATA: l_fieldname TYPE string,
*          ls_confirm  TYPE ty_us_confirm.
*
*    DEFINE mac_select_field.
*      l_fieldname = lcl_rtti=>get_component_name( is_any = ls_confirm i_structure_field = ls_confirm-&1 ).
*      append l_fieldname to et_field.
*    END-OF-DEFINITION.
*
*    REFRESH et_field.
*
*    mac_select_field:
*          selected,
*          original_name,
*          new_name,
*          done,
*          message,
*          button,
*          overwritten_file,
*          overwrite.
*
*    IF aoo_source_dof->file_system->type <> aoo_target_dof->file_system->type.
*      mac_select_field: binary.
*    ENDIF.

  ENDMETHOD."


  METHOD adjust_buttons.
    APPEND 'FC01' TO ct_fcode.
    IF aoo_source_dof->file_system->type <> aoo_target_dof->file_system->type.
      APPEND 'FC03' TO ct_fcode. " binary
    ENDIF.
  ENDMETHOD."


  METHOD check_file.
    DATA: lo_dof   TYPE REF TO lcl_dof,
          ls_style TYPE lvc_s_styl.
    FIELD-SYMBOLS:
          <ls_style> TYPE lvc_s_styl.

    error = abap_false.

    CREATE OBJECT lo_dof
      EXPORTING
        file_system = cs_confirm-o_target_dof->file_system
        filename    = cs_confirm-new_name
        path        = cs_confirm-target_dirpath
        isdir       = cs_confirm-o_source_dof->isdir.
    cs_confirm-o_target_dof = lo_dof.

    ls_style-fieldname = lcl_rtti=>get_component_name( is_any = cs_confirm i_structure_field = cs_confirm-button ).

    IF cs_confirm-o_target_dof->fullpath = cs_confirm-o_source_dof->fullpath
          AND cs_confirm-o_target_dof->file_system->type = cs_confirm-o_source_dof->file_system->type.
      error = abap_true.
      cs_confirm-message = icon_led_red && ` ` && 'Target file cannot be same as source'.
      cs_confirm-button = 'Overwrite'.
      DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
    ELSEIF abap_true = cs_confirm-o_target_dof->dof_exists( ).
      IF cs_confirm-new_name = cs_confirm-overwritten_file.
        cs_confirm-message = icon_led_red && ` ` && 'File already exists'.
        cs_confirm-button = 'Overwrite'.
        DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
      ELSE.
        error = abap_true.
        TYPE-POOLS icon.
        cs_confirm-message = icon_led_red && ` ` && 'File already exists'.
        cs_confirm-button = 'Overwrite'.
        READ TABLE cs_confirm-t_style WITH KEY fieldname = ls_style-fieldname ASSIGNING <ls_style>.
        IF sy-subrc <> 0.
          ls_style-style = cl_gui_alv_grid=>mc_style_button.
          INSERT ls_style INTO TABLE cs_confirm-t_style.
        ELSE.
          <ls_style>-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.
      ENDIF.
    ELSE.
      cs_confirm-message = icon_led_green && ` ` && 'File can be moved'.
      cs_confirm-button = space.
      DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
    ENDIF.

  ENDMETHOD."


  METHOD skip_dialog.
    result = abap_false.
    " The dialog is not displayed if you copy/move in the same file system,
    " but is displayed if you copy/move in the same directory to ask if you
    " want to copy or rename.
    IF aoo_source_dof->file_system->type = aoo_target_dof->file_system->type.
      IF aoo_source_dof->path <> ao_target_dirpath.
        result = abap_true.
      ENDIF.
    ELSE.
      " If you copy/move in another file system, then it's not displayed
      "     if the user has defined a Never/Always conversion.
      IF txtcopy CA 'NA'.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD."


  METHOD before_operation.
    IF cs_confirm-selected = abap_true.
      cs_confirm-message = icon_led_yellow && ` ` && 'File about to be moved'.
    ELSE.
      cs_confirm-message = icon_led_yellow && ` ` && 'File not to be moved'.
    ENDIF.
  ENDMETHOD."


  METHOD execute_operation.
    DATA: l_action(2) TYPE c,
          lt_text     TYPE string_table,
          l_binary    TYPE xstring.

    l_action = cs_confirm-o_source_dof->file_system->type && cs_confirm-o_target_dof->file_system->type.

    CASE l_action.
      WHEN 'FF'.
        go_app->aio_frontend->file_move(  source    = cs_confirm-o_source_dof->fullpath
                                  target    = cs_confirm-o_target_dof->fullpath
                                  overwrite = cs_confirm-overwrite ).
      WHEN 'SS'.
        " move from application server to application server
        go_app->aio_appserv->file_copy(  source    = cs_confirm-o_source_dof->fullpath
                                  target    = cs_confirm-o_target_dof->fullpath
                                  overwrite = cs_confirm-overwrite ).
        go_app->aio_appserv->file_delete( cs_confirm-o_source_dof->fullpath ).
      WHEN 'FS'.
        " move from frontend to application server
        CASE cs_confirm-binary.
          WHEN abap_false.
            go_app->aio_frontend->file_read_text( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = lt_text ).
            go_app->aio_appserv->file_write_text( EXPORTING fullpath = cs_confirm-o_target_dof->fullpath content = lt_text ).
          WHEN abap_true.
            go_app->aio_frontend->file_read_binary( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = l_binary ).
            go_app->aio_appserv->file_write_binary( EXPORTING fullpath = cs_confirm-o_target_dof->fullpath content = l_binary ).
        ENDCASE.
        go_app->aio_frontend->file_delete( cs_confirm-o_source_dof->fullpath ).
      WHEN 'SF'.
        CASE cs_confirm-binary.
          WHEN abap_false.
            go_app->aio_appserv->file_read_text( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = lt_text ).
            go_app->aio_frontend->file_write_text( EXPORTING fullpath = cs_confirm-o_target_dof->fullpath content = lt_text ).
          WHEN abap_true.
            go_app->aio_appserv->file_read_binary( EXPORTING fullpath = cs_confirm-o_source_dof->fullpath IMPORTING content = l_binary ).
            go_app->aio_frontend->file_write_binary( EXPORTING fullpath = cs_confirm-o_target_dof->fullpath content = l_binary ).
        ENDCASE.
        go_app->aio_appserv->file_delete( cs_confirm-o_source_dof->fullpath ).
    ENDCASE.

    CONCATENATE icon_okay ` ` 'File has been moved' INTO cs_confirm-message.
    cs_confirm-done = abap_true.

  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_copy IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_confirm_copy IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    operation = cs_operation-copy.
  ENDMETHOD."


  METHOD display.
    DATA: lt_confirm    TYPE lcl_popup_confirm=>ty_ut_confirm.

    prepare_confirm_dialog( EXPORTING source = source target = target IMPORTING t_confirm = lt_confirm ).
    set_data( lt_confirm ).
    CALL SELECTION-SCREEN 1003 STARTING AT 30 10 ENDING AT 130 30.
  ENDMETHOD."


  METHOD adjust_field_catalog.
*    DATA: l_fieldname TYPE string,
*          ls_confirm  TYPE ty_us_confirm.
*
*    REFRESH et_field.
*
*    mac_select_field:
*          selected,
*          " binary -> see condition below
*          original_name,
*          new_name,
*          done,
*          message,
*          button,
*          overwritten_file,
*          overwrite.
*
*    IF aoo_source_dof->file_system->type <> aoo_target_dof->file_system->type.
*      mac_select_field: binary.
*    ENDIF.
  ENDMETHOD."


  METHOD adjust_buttons.
    APPEND 'FC01' TO ct_fcode. " overwrite
    IF aoo_source_dof->file_system->type <> aoo_target_dof->file_system->type.
      APPEND 'FC03' TO ct_fcode. " binary
    ENDIF.
  ENDMETHOD."


  METHOD check_file.
    DATA: lo_dof   TYPE REF TO lcl_dof,
          ls_style TYPE lvc_s_styl.
    FIELD-SYMBOLS:
          <ls_style> TYPE lvc_s_styl.

    error = abap_false.

    CREATE OBJECT lo_dof
      EXPORTING
        file_system = cs_confirm-o_target_dof->file_system
        filename    = cs_confirm-new_name
        path        = cs_confirm-target_dirpath
        isdir       = cs_confirm-o_source_dof->isdir.
    cs_confirm-o_target_dof = lo_dof.

    ls_style-fieldname = lcl_rtti=>get_component_name( is_any = cs_confirm i_structure_field = cs_confirm-button ).

    IF cs_confirm-o_target_dof->fullpath = cs_confirm-o_source_dof->fullpath
          AND aoo_target_dof->file_system->type = aoo_source_dof->file_system->type.
      error = abap_true.
      cs_confirm-message = icon_led_red && ` ` && 'Target file cannot be same as source'.
      cs_confirm-button = 'Overwrite'.
    ELSEIF abap_true = cs_confirm-o_target_dof->dof_exists( ).
      IF cs_confirm-new_name = cs_confirm-overwritten_file.
        cs_confirm-message = icon_led_red && ` ` && 'File already exists'.
        cs_confirm-button = 'Overwrite'.
        DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
      ELSE.
        error = abap_true.
        TYPE-POOLS icon.
        cs_confirm-message = icon_led_red && ` ` && 'File already exists'.
        cs_confirm-button = 'Overwrite'.
        READ TABLE cs_confirm-t_style WITH KEY fieldname = ls_style-fieldname ASSIGNING <ls_style>.
        IF sy-subrc <> 0.
          ls_style-style = cl_gui_alv_grid=>mc_style_button.
          INSERT ls_style INTO TABLE cs_confirm-t_style.
        ELSE.
          <ls_style>-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.
      ENDIF.
    ELSE.
      cs_confirm-message = icon_led_green && ` ` && 'File can be copied'.
      cs_confirm-button = space.
      DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
    ENDIF.

  ENDMETHOD."


  METHOD skip_dialog.
    result = abap_false.
    IF aoo_source_dof->file_system->type = aoo_target_dof->file_system->type
          AND aoo_source_dof->path <> ao_target_dirpath.
      result = abap_true.
    ENDIF.
  ENDMETHOD."


  METHOD before_operation.
    IF cs_confirm-selected = abap_true.
      cs_confirm-message = icon_led_yellow && ` ` && 'File about to be copied'.
    ELSE.
      cs_confirm-message = icon_led_yellow && ` ` && 'File not to be copied'.
    ENDIF.
  ENDMETHOD."


  METHOD execute_operation.
    go_app->dof_copy( CHANGING cs_confirm = cs_confirm ).
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_rename IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_confirm_rename IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    operation = cs_operation-rename.
*    aio_frontend = io_frontend.
*    aio_appserv = io_appserv.
  ENDMETHOD."


  METHOD display.
    DATA: lt_confirm    TYPE lcl_popup_confirm=>ty_ut_confirm.

    prepare_confirm_dialog( EXPORTING source = dofs IMPORTING t_confirm = lt_confirm ).
    go_app->aio_confirm_rename->set_data( lt_confirm ).
    CALL SELECTION-SCREEN 1004 STARTING AT 30 10 ENDING AT 130 30.
  ENDMETHOD."


  METHOD adjust_field_catalog.
*    DATA: l_fieldname TYPE string,
*          ls_confirm  TYPE ty_us_confirm.
*
*    REFRESH et_field.
*
*    mac_select_field:
*          selected,
*          original_name,
*          new_name,
*          done,
*          message.
*
  ENDMETHOD."


  METHOD adjust_buttons.
  ENDMETHOD."


  METHOD check_file.
    DATA: lo_dof   TYPE REF TO lcl_dof,
          ls_style TYPE lvc_s_styl.
    FIELD-SYMBOLS:
          <ls_style> TYPE lvc_s_styl.

    error = abap_false.

    CREATE OBJECT lo_dof
      EXPORTING
        file_system = cs_confirm-o_source_dof->file_system
        filename    = cs_confirm-new_name
        path        = cs_confirm-target_dirpath
        isdir       = cs_confirm-o_source_dof->isdir.
    cs_confirm-o_target_dof = lo_dof.

    ls_style-fieldname = lcl_rtti=>get_component_name( is_any = cs_confirm i_structure_field = cs_confirm-button ).

    IF cs_confirm-o_target_dof->fullpath = cs_confirm-o_source_dof->fullpath
          AND cs_confirm-o_target_dof->file_system->type = cs_confirm-o_source_dof->file_system->type.
      error = abap_true.
      cs_confirm-message = icon_led_red && ` ` && 'Target file cannot be same as source'.
      cs_confirm-button = 'Overwrite'.
      DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
    ELSEIF abap_true = cs_confirm-o_target_dof->dof_exists( ).
      IF cs_confirm-new_name = cs_confirm-overwritten_file.
        cs_confirm-message = icon_led_red && ` ` && 'File already exists'.
        cs_confirm-button = 'Overwrite'.
        DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
      ELSE.
        error = abap_true.
        TYPE-POOLS icon.
        cs_confirm-message = icon_led_red && ` ` && 'File already exists'.
        cs_confirm-button = 'Overwrite'.
        READ TABLE cs_confirm-t_style WITH KEY fieldname = ls_style-fieldname ASSIGNING <ls_style>.
        IF sy-subrc <> 0.
          ls_style-style = cl_gui_alv_grid=>mc_style_button.
          INSERT ls_style INTO TABLE cs_confirm-t_style.
        ELSE.
          <ls_style>-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.
      ENDIF.
    ELSE.
      cs_confirm-message = icon_led_green && ` ` && 'File can be renamed'.
      cs_confirm-button = space.
      DELETE cs_confirm-t_style WHERE fieldname = ls_style-fieldname.
    ENDIF.

*    error = abap_false.
*
*    IF cs_confirm-new_name = cs_confirm-original_name.
*      error = abap_true.
*      cs_confirm-message = icon_led_red && ` ` && 'New name must be different from original'.
*    ELSE.
*      CREATE OBJECT cs_confirm-o_target_dof
*        EXPORTING
*          file_system = cs_confirm-o_source_dof->file_system
*          filename    = cs_confirm-new_name
*          path        = cs_confirm-o_source_dof->path
*          isdir       = cs_confirm-o_source_dof->isdir.
*      IF abap_true = cs_confirm-o_target_dof->dof_exists( ).
*        error = abap_true.
*        cs_confirm-message = icon_led_red && ` ` && 'File already exists'.
*      ENDIF.
*    ENDIF.

  ENDMETHOD."


  METHOD before_operation.
    IF cs_confirm-selected = abap_true.
      cs_confirm-message = icon_led_yellow && ` ` && 'File about to be renamed'.
    ELSE.
      cs_confirm-message = icon_led_yellow && ` ` && 'File not to be renamed'.
    ENDIF.
  ENDMETHOD."


  METHOD execute_operation.
    cs_confirm-o_source_dof->rename( cs_confirm-new_name ).

    CONCATENATE icon_okay ` ` 'File has been renamed' INTO cs_confirm-message.
    cs_confirm-done = abap_true.
  ENDMETHOD."
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_confirm_delete IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_confirm_delete IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    operation = cs_operation-delete.
*    aio_frontend = io_frontend.
*    aio_appserv = io_appserv.
  ENDMETHOD."


  METHOD display.
    DATA: lt_confirm    TYPE lcl_popup_confirm=>ty_ut_confirm.

    prepare_confirm_dialog( EXPORTING source = dofs IMPORTING t_confirm = lt_confirm ).
    go_app->aio_confirm_delete->set_data( lt_confirm ).
    CALL SELECTION-SCREEN 1005 STARTING AT 30 10 ENDING AT 130 30.
  ENDMETHOD."


  METHOD adjust_field_catalog.
*    DATA: l_fieldname TYPE string,
*          ls_confirm  TYPE ty_us_confirm.
*
*    REFRESH et_field.
*
*    mac_select_field:
*          selected,
*          original_name,
*          done,
*          message.
*
  ENDMETHOD."


  METHOD adjust_buttons.
  ENDMETHOD."


  METHOD check_file.
    error = abap_false.
    IF abap_false = cs_confirm-o_source_dof->dof_exists( ).
      error = abap_true.
      CONCATENATE icon_led_red ` ` 'File does not exist' INTO cs_confirm-message.
    ELSE.
      CONCATENATE icon_led_yellow ` ` 'File can be deleted' INTO cs_confirm-message.
    ENDIF.
  ENDMETHOD."


  METHOD before_operation.
    IF cs_confirm-selected = abap_true.
      cs_confirm-message = icon_led_yellow && ` ` && 'File about to be deleted'.
    ELSE.
      cs_confirm-message = icon_led_yellow && ` ` && 'File not to be deleted'.
    ENDIF.
  ENDMETHOD."


  METHOD execute_operation.
    cs_confirm-o_source_dof->delete( ).
    CONCATENATE icon_okay ` ` 'File has been deleted' INTO cs_confirm-message.
    cs_confirm-done = abap_true.
  ENDMETHOD."
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_f4_as_dirs_sscr IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_f4_as_dirs_sscr IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD."

  METHOD display.
    aio_container = parent.
    go_app->lif_sscr_h~set_sscr_handler( sscr = '1007' handler = me ).
    " TODO déterminer les chemins racine
    IF start_path IS INITIAL.
      folder = 'C:\'.
    ELSE.
      folder = start_path.
    ENDIF.
    CALL SELECTION-SCREEN 1007 STARTING AT 30 5 ENDING AT 120 25.
    selected_folder = folder.
  ENDMETHOD."

  METHOD lif_sscr~pbo.
    IF aio_f4_as_dirs IS NOT BOUND.
      CREATE OBJECT aio_f4_as_dirs
        EXPORTING
          parent     = aio_container
          start_path = folder.
    ENDIF.
    LOOP AT SCREEN.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD."

  METHOD lif_sscr~pai.
    folder = aio_f4_as_dirs->get_selected_directory( ).
    aio_f4_as_dirs->free( ).
    FREE aio_f4_as_dirs.
  ENDMETHOD."

  METHOD lif_sscr~exit.
    aio_f4_as_dirs->free( ).
    FREE aio_f4_as_dirs.
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_appserv IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_appserv IMPLEMENTATION.

*  METHOD class_constructor.
*    CREATE OBJECT file_system TYPE lcl_appserv.
*  ENDMETHOD."
  METHOD constructor.
*    ais_config = is_config.
    IF sy-opsys CP 'Win*'.
      ai_file_sep = '\'.
    ELSE.
      ai_file_sep = '/'.
    ENDIF.
    CONCATENATE '^(.+[\' ai_file_sep '])([^\' ai_file_sep ']+)$' INTO ai_split_path_regex.
  ENDMETHOD."


  METHOD directory_browse.

    CREATE OBJECT ro_f4
      EXPORTING
        parent     = container
        start_path = folder.

  ENDMETHOD."


  METHOD file_write_prepare.
    DATA: l_cmd     TYPE ocs_car, "254
          lt_result TYPE TABLE OF ocs_car. "254


    IF xpgsmkf0 = abap_true
          AND abap_false = file_exist( fullpath ).

      IF xpgsmkf1 IS NOT INITIAL.
        l_cmd = xpgsmkf1.
        REPLACE '%1' IN l_cmd WITH fullpath.
        CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd ID 'TAB' FIELD lt_result.
        IF sy-subrc <> 0.
          MESSAGE 'error pre-creation' TYPE 'I'.
          RETURN.
        ENDIF.
        RAISE EVENT dof_added EXPORTING fullpath = fullpath.
      ENDIF.

      IF xpgsmkf2 IS NOT INITIAL.
        l_cmd = xpgsmkf2.
        REPLACE '%1' IN l_cmd WITH fullpath.
        CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd ID 'TAB' FIELD lt_result.
        IF sy-subrc <> 0.
          MESSAGE 'error pre-creation' TYPE 'I'.
          RETURN.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD."


  METHOD get_file_sep.
    file_sep = ai_file_sep.
  ENDMETHOD."


  METHOD directory_list_files.
    DATA: ls_file       TYPE ty_gs_file,
          dir_name      TYPE c LENGTH 255,
          l_filter      TYPE c LENGTH 255,
          l_mtime(6)    TYPE p,
          timezone_sec  TYPE i,
          lt_file       TYPE ty_gt_file,
          l_path        TYPE string,
          l_depth       TYPE i,
          error_counter TYPE i,
          lrt_file      TYPE REF TO data.
    DATA: l_tstmp_unix_era TYPE tzntstmpl,
          l_tstmp          TYPE tzntstmpl.
    FIELD-SYMBOLS:
      <lt_file>  TYPE ty_gt_file,
      <lt_file2> TYPE ty_gt_file,
      <ls_file>  TYPE ty_gs_file.
    CONSTANTS:
          utc               TYPE systzonlo VALUE IS INITIAL.


    CREATE DATA files TYPE TABLE OF ty_gs_file.
    ASSIGN files->* TO <lt_file>.

    " close previous directory access, just to be sure
    CALL 'C_DIR_READ_FINISH'
          ID 'ERRNO'  FIELD ls_file-errno
          ID 'ERRMSG' FIELD ls_file-errmsg.

    dir_name = path.
    l_filter = filter.
    CALL 'C_DIR_READ_START'
          ID 'DIR'    FIELD dir_name
          ID 'FILE'   FIELD l_filter
          ID 'ERRNO'  FIELD ls_file-errno
          ID 'ERRMSG' FIELD ls_file-errmsg.

    DO.
      CLEAR ls_file.
      ls_file-dirname = path.

      CALL 'C_DIR_READ_NEXT'
            ID 'TYPE'   FIELD ls_file-type
            ID 'NAME'   FIELD ls_file-name
            ID 'LEN'    FIELD ls_file-len
            ID 'OWNER'  FIELD ls_file-owner
            ID 'MTIME'  FIELD l_mtime
            ID 'MODE'   FIELD ls_file-mode
            ID 'ERRNO'  FIELD ls_file-errno
            ID 'ERRMSG' FIELD ls_file-errmsg.

      IF sy-subrc = 0.
        IF ls_file-name <> '.' AND ls_file-name <> '..'.
          IF ls_file-type(1) = 'd'. "directory
            ls_file-isdir = 1.
          ELSE.
            ls_file-isdir = 0.
          ENDIF.

          IF only_directories = abap_false OR ls_file-isdir = 1.

            CONVERT DATE '19700101' TIME '000000' INTO TIME STAMP l_tstmp_unix_era TIME ZONE utc.
            l_tstmp = cl_abap_tstmp=>add( tstmp = l_tstmp_unix_era secs = l_mtime ).
            CONVERT TIME STAMP l_tstmp TIME ZONE sy-zonlo INTO DATE ls_file-mdate TIME ls_file-mtime.

            APPEND ls_file TO <lt_file>.

            IF ls_file-isdir = 1 AND depth = 0 AND only_first_directory = abap_true.
              CALL 'C_DIR_READ_FINISH'
                    ID 'ERRNO'  FIELD ls_file-errno
                    ID 'ERRMSG' FIELD ls_file-errmsg.
              EXIT.
            ENDIF.
          ENDIF.

        ENDIF.
      ELSEIF sy-subrc = 1.
        EXIT.
      ELSE.
        IF error_counter > 1000.
          CALL 'C_DIR_READ_FINISH'
                ID 'ERRNO'  FIELD ls_file-errno
                ID 'ERRMSG' FIELD ls_file-errmsg.
          EXIT.
        ENDIF.
        ADD 1 TO error_counter.
      ENDIF.
    ENDDO.

    IF depth >= 1.
      l_depth = depth - 1.

      REFRESH lt_file.
      LOOP AT <lt_file> INTO ls_file WHERE isdir = 1.

        CALL METHOD build_file_path_long
          EXPORTING
            start_path = path
            dir_name   = ls_file-name
          IMPORTING
            dir_path   = l_path.

        CALL METHOD directory_list_files
          EXPORTING
            path                 = l_path
            depth                = l_depth
            only_first_directory = only_first_directory
          IMPORTING
            files                = lrt_file.
        ASSIGN lrt_file->* TO <lt_file2>.
        APPEND LINES OF <lt_file2> TO lt_file.

      ENDLOOP.
      APPEND LINES OF lt_file TO <lt_file>.
    ENDIF.

  ENDMETHOD."


  METHOD normalize_dir_path.
    DATA: l_file_sep TYPE char1,
          l_pattern  TYPE string.

    result = path.
    l_file_sep = get_file_sep( ).
    REPLACE ALL OCCURRENCES OF REGEX '[/\\]' IN result WITH l_file_sep.
    CONCATENATE '*' l_file_sep INTO l_pattern.
    IF result NP l_pattern.
      CONCATENATE result l_file_sep INTO result.
    ENDIF.
  ENDMETHOD."


  METHOD build_file_path_long.
    IF abap_true = cl_abap_matcher=>contains( pattern = '[/\\] *$' text = start_path ).
      CONCATENATE start_path dir_name INTO dir_path.
    ELSEIF sy-opsys CP 'win*'.
      CONCATENATE start_path '\' dir_name INTO dir_path.
    ELSE.
      CONCATENATE start_path '/' dir_name INTO dir_path.
    ENDIF.
  ENDMETHOD."


  METHOD file_read_binary.
    DATA: l_message   TYPE string.

    OPEN DATASET fullpath
          FOR INPUT
          IN BINARY MODE
          MESSAGE l_message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = l_message.
    ENDIF.

    READ DATASET fullpath INTO content.
    CLOSE DATASET fullpath.

  ENDMETHOD."


  METHOD file_write_binary.
    DATA: l_message   TYPE string.

    file_write_prepare( fullpath ).

    OPEN DATASET fullpath
          FOR OUTPUT
          IN BINARY MODE
          MESSAGE l_message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = l_message.
    ENDIF.

    TRANSFER content TO fullpath.
    CLOSE DATASET fullpath.

    RAISE EVENT dof_added EXPORTING fullpath = fullpath.
  ENDMETHOD."


  METHOD file_read_text.
    DATA: l_message TYPE string,
          l_string  TYPE string,
          l_length  TYPE i.
*          lx_root type ref to cx_root.

    REFRESH content.

    OPEN DATASET fullpath
          FOR INPUT
          IN TEXT MODE
          ENCODING DEFAULT
          MESSAGE l_message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = l_message.
    ENDIF.

    TRY.
        DO.
          READ DATASET fullpath INTO l_string ACTUAL LENGTH l_length.
          IF sy-subrc <> 0 AND l_length = 0.
            EXIT.
          ENDIF.
          APPEND l_string TO content.
        ENDDO.
        CLOSE DATASET fullpath .
      CLEANUP.
        CLOSE DATASET fullpath .
    ENDTRY.

  ENDMETHOD."


  METHOD file_write_text.
    DATA: l_message   TYPE string.
    FIELD-SYMBOLS:
          <l_clike>   TYPE clike.

    file_write_prepare( fullpath ).

    OPEN DATASET fullpath
          FOR OUTPUT
          IN TEXT MODE
          ENCODING DEFAULT
          MESSAGE l_message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = l_message.
    ENDIF.

    LOOP AT content ASSIGNING <l_clike>.
      TRANSFER <l_clike> TO fullpath.
    ENDLOOP.
    CLOSE DATASET fullpath.

    RAISE EVENT dof_added EXPORTING fullpath = fullpath.
  ENDMETHOD."


  METHOD file_exist.
    DATA: l_message   TYPE string.

    OPEN DATASET fullpath
          FOR INPUT
          IN BINARY MODE
          MESSAGE l_message.
    IF sy-subrc = 0.
      result = abap_true.
      CLOSE DATASET fullpath.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD."


  METHOD directory_exist.
    DATA: ls_file    TYPE ty_gs_file,
          dir_name   TYPE c LENGTH 255,
          l_mtime(6) TYPE p,
          l_filter   TYPE c LENGTH 255.

    " close previous directory access, just to be sure
    CALL 'C_DIR_READ_FINISH'
          ID 'ERRNO'  FIELD ls_file-errno
          ID 'ERRMSG' FIELD ls_file-errmsg.

    dir_name = fullpath.
    l_filter = ''.
    CALL 'C_DIR_READ_START'
          ID 'DIR'    FIELD dir_name
          ID 'FILE'   FIELD l_filter
          ID 'ERRNO'  FIELD ls_file-errno
          ID 'ERRMSG' FIELD ls_file-errmsg.

    IF sy-subrc = 0.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.

    " mandatory call (at least one) otherwise
    " next C_DIR_READ_START will always fail
    CALL 'C_DIR_READ_NEXT'
          ID 'TYPE'   FIELD ls_file-type
          ID 'NAME'   FIELD ls_file-name
          ID 'LEN'    FIELD ls_file-len
          ID 'OWNER'  FIELD ls_file-owner
          ID 'MTIME'  FIELD l_mtime
          ID 'MODE'   FIELD ls_file-mode
          ID 'ERRNO'  FIELD ls_file-errno
          ID 'ERRMSG' FIELD ls_file-errmsg.

    CALL 'C_DIR_READ_FINISH'
          ID 'ERRNO'  FIELD ls_file-errno
          ID 'ERRMSG' FIELD ls_file-errmsg.

  ENDMETHOD."


  METHOD file_move.
    DATA: l_cmd     TYPE ocs_car, "254
          lt_result TYPE TABLE OF ocs_car. "254

    " move from server to server
    IF go_app->ais_config_as-xpgmove IS NOT INITIAL.
      l_cmd = go_app->ais_config_as-xpgmove.
      REPLACE '%1' IN l_cmd WITH source.
      REPLACE '%2' IN l_cmd WITH target.
      IF overwrite = abap_true.
        REPLACE '%3' IN l_cmd WITH go_app->ais_config_as-xpgmov3.
      ELSE.
        REPLACE '%3' IN l_cmd WITH space.
      ENDIF.
      CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd ID 'TAB' FIELD lt_result.
      IF sy-subrc = 0.
        RAISE EVENT dof_removed EXPORTING fullpath = source.
        RAISE EVENT dof_added EXPORTING fullpath = target.
      ENDIF.
    ELSE.
      file_copy(  source    = source
                  target    = target
                  overwrite = overwrite ).
      file_delete( source ).
    ENDIF.

  ENDMETHOD."


  METHOD file_copy.
    DATA: l_message       TYPE string,
          l_content(1000) TYPE x,
          l_length        TYPE i.

    OPEN DATASET source
          FOR INPUT
          IN BINARY MODE
          MESSAGE l_message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = l_message.
    ENDIF.

    OPEN DATASET target
          FOR OUTPUT
          IN BINARY MODE
          MESSAGE l_message.
    IF sy-subrc <> 0.
      CLOSE DATASET source.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = l_message.
    ENDIF.

    RAISE EVENT dof_added EXPORTING fullpath = target.

    DO.
      READ DATASET source INTO l_content ACTUAL LENGTH l_length.
      IF sy-subrc <> 0.
        " end of file
        EXIT.
      ENDIF.
      TRANSFER l_content TO target LENGTH l_length.
    ENDDO.

    CLOSE DATASET source.
    CLOSE DATASET target.

  ENDMETHOD."


  METHOD file_create.
    DATA: l_null_content  TYPE xstring.

    file_write_binary(  fullpath  = fullpath
                        content   = l_null_content ).
  ENDMETHOD."


  METHOD directory_create.
    DATA: l_cmd     TYPE ocs_car, "254
          lt_result TYPE TABLE OF ocs_car. "254

    IF go_app->ais_config_as-xpgmkdi IS NOT INITIAL.
      l_cmd = go_app->ais_config_as-xpgmkdi.
      REPLACE '%1' IN l_cmd WITH fullpath.
      CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd ID 'TAB' FIELD lt_result.
      IF sy-subrc = 0.
        RAISE EVENT dof_added EXPORTING fullpath = fullpath.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'No command to create dirs'(108).
    ENDIF.
  ENDMETHOD."


  METHOD file_delete.
    DELETE DATASET fullpath.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'Dataset not deleted'(021).
    ENDIF.

    RAISE EVENT dof_removed EXPORTING fullpath = fullpath.
  ENDMETHOD."


  METHOD directory_delete.
    DATA: l_cmd     TYPE ocs_car, "254
          lt_result TYPE TABLE OF ocs_car. "254

    IF go_app->ais_config_as-xpgrmdi IS NOT INITIAL.
      l_cmd = go_app->ais_config_as-xpgrmdi.
      REPLACE '%1' IN l_cmd WITH fullpath.
      CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd ID 'TAB' FIELD lt_result.
      IF sy-subrc = 0.
        RAISE EVENT dof_added EXPORTING fullpath = fullpath.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'No command to delete dirs'(106).
    ENDIF.
    RAISE EVENT dof_removed EXPORTING fullpath = fullpath.
  ENDMETHOD."


  METHOD file_rename.
    DATA: ls_split   TYPE lif_file_system=>ty_us_splitted_full_path,
          l_fullpath TYPE string.

    ls_split = split_full_path( fullpath ).
    l_fullpath = get_full_path( name = new_filename path = ls_split-path ).
    file_copy( source = fullpath target = l_fullpath ).
    file_delete( fullpath ).
  ENDMETHOD."


  METHOD directory_rename.
    " TODO
  ENDMETHOD."


  METHOD execute.
    DATA: lo_dof      TYPE REF TO lcl_dof,
          lo_dof_temp TYPE REF TO lcl_dof.

    lo_dof = lcl_dof=>create( file_system = me fullpath = fullpath ).
    CREATE OBJECT lo_dof_temp
      EXPORTING
        file_system = go_app->aio_frontend
        path        = go_app->aio_frontend->aus_directory-temp
        filename    = lo_dof->filename.
    lo_dof->file_copy( to = lo_dof_temp ).
    go_app->aio_frontend->execute( lo_dof_temp->fullpath ).
    lo_dof_temp->delete( ).
  ENDMETHOD."


  METHOD open_with.
    DATA: lo_dof      TYPE REF TO lcl_dof,
          lo_dof_temp TYPE REF TO lcl_dof.

    lo_dof = lcl_dof=>create( file_system = me fullpath = fullpath ).
    CREATE OBJECT lo_dof_temp
      EXPORTING
        file_system = go_app->aio_frontend
        path        = go_app->aio_frontend->aus_directory-temp
        filename    = lo_dof->filename.
    lo_dof->file_copy( to = lo_dof_temp ).
    go_app->aio_frontend->open_with( lo_dof_temp->fullpath ).
    lo_dof_temp->delete( ).
  ENDMETHOD."


  METHOD get_full_path.
    DATA: l_file_name TYPE sdok_filnm,
          l_directory TYPE string.

    l_file_name = name.
    l_directory = path.
    CALL METHOD cl_mime_services=>concatenate_path_name
      EXPORTING
        i_file_name = l_file_name
        i_directory = l_directory
      IMPORTING
        e_pathname  = full_path.
  ENDMETHOD."


  METHOD split_full_path.
*    DATA: l_file_sep(1) TYPE c.

*    l_file_sep = get_file_sep( ).
    FIND REGEX ai_split_path_regex IN full_path SUBMATCHES result-path result-name.
    IF sy-subrc <> 0.
      result-path = full_path.
    ENDIF.
*    SPLIT full_path AT l_file_sep INTO result-name result-path.
*    IF result-name IS INITIAL.
*      SPLIT result-path AT l_file_sep INTO result-name result-path.
*    ENDIF.
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_frontend IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_frontend IMPLEMENTATION.

*  METHOD class_constructor.
*    CREATE OBJECT file_system TYPE lcl_frontend.
*  ENDMETHOD."

  METHOD constructor.
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory    = aus_directory-desktop
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3.
    cl_gui_cfw=>flush( ).
    CALL METHOD cl_gui_frontend_services=>get_sapgui_directory
      CHANGING
        sapgui_directory     = aus_directory-sapgui
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3.
    cl_gui_cfw=>flush( ).
    CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
      CHANGING
        sapworkdir           = aus_directory-workdir
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3.
    cl_gui_cfw=>flush( ).
    CALL METHOD cl_gui_frontend_services=>get_system_directory
      CHANGING
        system_directory     = aus_directory-system
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3.
    cl_gui_cfw=>flush( ).
    CALL METHOD cl_gui_frontend_services=>get_temp_directory
      CHANGING
        temp_dir             = aus_directory-temp
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3.
    cl_gui_cfw=>flush( ).
    CALL METHOD cl_gui_frontend_services=>get_windows_directory
      CHANGING
        windows_directory    = aus_directory-windows
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3.
    cl_gui_cfw=>flush( ).
    CALL METHOD cl_gui_frontend_services=>get_file_separator
      CHANGING
        file_separator       = ai_file_sep
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CONCATENATE '^(.+[\' ai_file_sep '])([^\' ai_file_sep ']+)$' INTO ai_split_path_regex.
  ENDMETHOD."


  METHOD get_file_sep.
    file_sep = ai_file_sep.
  ENDMETHOD."


  METHOD directory_list_files.
    DATA: l_filter TYPE string,
          l_path   TYPE string,
          ls_dir   TYPE ty_gs_file,
          lt_file  TYPE rstt_t_files,
          lrt_file TYPE REF TO data,
          lt_file2 TYPE ty_gt_file,
          lt_file3 TYPE ty_gt_file,
          ls_file  TYPE file_info,
          l_count  TYPE i,
          xxx      TYPE sdok_filnm,
          l_depth  TYPE i.
    FIELD-SYMBOLS:
      <lt_file>  TYPE ty_gt_file,
      <lt_file2> TYPE ty_gt_file.


    CREATE DATA files TYPE TABLE OF ty_gs_file.
    ASSIGN files->* TO <lt_file>.

    l_path = path.
    l_filter = filter.
    IF l_filter IS INITIAL.
      l_filter = '*.*'.
    ENDIF.
    CALL METHOD cl_gui_frontend_services=>directory_list_files
      EXPORTING
        directory                   = l_path
        filter                      = l_filter
        directories_only            = only_directories
      CHANGING
        file_table                  = lt_file
        count                       = l_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend directory list files'(022).
    ENDIF.

    l_depth = depth - 1.

    LOOP AT lt_file INTO ls_file.

      ls_dir-dir = path.
      ls_dir-f = ls_file.
      APPEND ls_dir TO <lt_file>.

      IF ls_file-isdir = 1 AND depth >= 1.

        xxx = ls_file-filename.
        CALL METHOD cl_mime_services=>concatenate_path_name
          EXPORTING
            i_file_name = xxx
            i_directory = path
          IMPORTING
            e_pathname  = l_path.

        CALL METHOD directory_list_files
          EXPORTING
            path  = l_path
            depth = l_depth
          IMPORTING
            files = lrt_file.
        ASSIGN lrt_file->* TO <lt_file2>.
        APPEND LINES OF <lt_file2> TO lt_file3.

      ENDIF.
    ENDLOOP.

    APPEND LINES OF lt_file3 TO <lt_file>.

  ENDMETHOD."


  METHOD normalize_dir_path.
    DATA: l_file_sep TYPE char1,
          l_pattern  TYPE string.

    result = path.
    l_file_sep = get_file_sep( ).
    REPLACE ALL OCCURRENCES OF REGEX '[/\\]' IN result WITH l_file_sep.
    CONCATENATE '*' l_file_sep INTO l_pattern.
    IF result NP l_pattern.
      CONCATENATE result l_file_sep INTO result.
    ENDIF.
  ENDMETHOD."


  METHOD build_file_path_long.
    IF abap_true = cl_abap_matcher=>contains( pattern = '[/\\] *$' text = start_path ).
      CONCATENATE start_path dir_name INTO dir_path.
    ELSE.
      CONCATENATE start_path ai_file_sep dir_name INTO dir_path.
    ENDIF.
*    IF sy-opsys CP 'win*'.
*      CONCATENATE start_path '\' dir_name INTO dir_path.
*    ELSE.
*      CONCATENATE start_path '/' dir_name INTO dir_path.
*    ENDIF.
  ENDMETHOD."


  METHOD file_read_binary.
    DATA: l_filename TYPE string,
          l_length   TYPE i,
          lt_x255    TYPE TABLE OF x255.

    l_filename = fullpath.
    " Note: impossible de mettre dans une variable de type TABLE OF xstring sinon dump ASSIGN_TYPE_ILLEGAL_CAST
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = l_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = l_length
      CHANGING
        data_tab   = lt_x255
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend upload'(014).
    ENDIF.

    CALL METHOD cl_swf_utl_convert_xstring=>table_to_xstring
      EXPORTING
        i_table  = lt_x255
        i_size   = l_length
      RECEIVING
        r_stream = content
      EXCEPTIONS
        OTHERS   = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'table to xstring'(015).
    ENDIF.

  ENDMETHOD."


  METHOD file_write_binary.
    TYPES xx(50) TYPE x.
    DATA: lt_xstring TYPE TABLE OF xx,
          l_length   TYPE i,
          l_filename TYPE string.

    l_filename = fullpath.

    CALL METHOD cl_swf_utl_convert_xstring=>xstring_to_table
      EXPORTING
        i_stream = content
      IMPORTING
        e_table  = lt_xstring
      EXCEPTIONS
        OTHERS   = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'xstring to table'(016).
    ENDIF.

    l_length = xstrlen( content ).

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = l_length
        filename     = l_filename
        filetype     = 'BIN'
      CHANGING
        data_tab     = lt_xstring
      EXCEPTIONS
        OTHERS       = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend download'(017).
    ENDIF.

    RAISE EVENT dof_added EXPORTING fullpath = fullpath.

  ENDMETHOD."


  METHOD file_read_text.

    REFRESH content.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = fullpath
        filetype = 'ASC'
      CHANGING
        data_tab = content
      EXCEPTIONS
        OTHERS   = 17.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend upload text'(018).
    ENDIF.

  ENDMETHOD."


  METHOD file_write_text.
    DATA: l_length   TYPE i,
          l_filename TYPE string,
          lt_dup     TYPE string_table.

    l_filename = fullpath.

    lt_dup = content.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename = l_filename
        filetype = 'ASC'
      CHANGING
        data_tab = lt_dup
      EXCEPTIONS
        OTHERS   = 17.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend download text'(019).
    ENDIF.

    RAISE EVENT dof_added EXPORTING fullpath = fullpath.

  ENDMETHOD."


  METHOD file_exist.
    DATA: l_name    TYPE string.

    l_name = fullpath.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = l_name
      RECEIVING
        result               = result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend file exist'(023).
    ENDIF.
  ENDMETHOD."


  METHOD directory_exist.
    DATA: l_name    TYPE string.
    l_name = fullpath.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = l_name
      RECEIVING
        result               = result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend directory exist'(105).
    ENDIF.

  ENDMETHOD."


  METHOD file_move.
    DATA: l_application TYPE string,
          l_parameters  TYPE string.

    " move from frontend to frontend
    IF xpgfcmdc IS NOT INITIAL AND go_app->ais_config_fe-xpgmove IS NOT INITIAL.
      l_application = xpgfcmdc.
      l_parameters = xpgfcmdp.
      REPLACE '%1' IN l_parameters WITH go_app->ais_config_fe-xpgmove.
      REPLACE '%1' IN l_parameters WITH source.
      REPLACE '%2' IN l_parameters WITH target.
      IF overwrite = abap_true.
        REPLACE '%3' IN l_parameters WITH go_app->ais_config_fe-xpgmov3.
      ELSE.
        REPLACE '%3' IN l_parameters WITH space.
      ENDIF.
      CALL METHOD cl_gui_frontend_services=>execute
        EXPORTING
          application = l_application
          parameter   = l_parameters
          minimized   = 'X'
          synchronous = 'X'
        EXCEPTIONS
          OTHERS      = 1.
      IF sy-subrc = 0.
        RAISE EVENT dof_removed EXPORTING fullpath = source.
        RAISE EVENT dof_added EXPORTING fullpath = target.
      ENDIF.
    ELSE.
      file_copy(  source    = source
                  target    = target
                  overwrite = overwrite ).
      file_delete( source ).
    ENDIF.

  ENDMETHOD."


  METHOD file_copy.
    DATA: l_application TYPE string,
          l_parameters  TYPE string.

    " copy from frontend to frontend (Note: same machine, so binary is implicitly used)

    IF xpgfcmdc IS NOT INITIAL AND go_app->ais_config_fe-xpgcopy IS NOT INITIAL.
      " direct OS command
      l_application = xpgfcmdc.
      l_parameters = xpgfcmdp.
      REPLACE '%1' IN l_parameters WITH go_app->ais_config_fe-xpgcopy.

      REPLACE '%1' IN l_parameters WITH source.
      REPLACE '%2' IN l_parameters WITH target.
      IF overwrite = abap_true.
        REPLACE '%3' IN l_parameters WITH go_app->ais_config_fe-xpgcpy3.
      ELSE.
        REPLACE '%3' IN l_parameters WITH space.
      ENDIF.
      CALL METHOD cl_gui_frontend_services=>execute
        EXPORTING
          application = l_application
          parameter   = l_parameters
          minimized   = 'X'
          synchronous = 'X'
        EXCEPTIONS
          OTHERS      = 1.
    ELSE.
      " via SAP GUI
      CALL METHOD cl_gui_frontend_services=>file_copy
        EXPORTING
          source               = source
          destination          = target
          overwrite            = abap_true
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          disk_full            = 4
          access_denied        = 5
          file_not_found       = 6
          destination_exists   = 7
          unknown_error        = 8
          path_not_found       = 9
          disk_write_protect   = 10
          drive_not_ready      = 11
          not_supported_by_gui = 12
          OTHERS               = 13.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_freetext
          EXPORTING
            text = 'Frontend file copy'(024).
      ENDIF.
    ENDIF.

    RAISE EVENT dof_added EXPORTING fullpath = target.

  ENDMETHOD."


  METHOD file_create.
    DATA: l_null_content  TYPE xstring.

    file_write_binary(  fullpath  = fullpath
                        content   = l_null_content ).
  ENDMETHOD."


  METHOD directory_create.
    DATA: l_rc    TYPE i,
          l_text  TYPE string,
          l_text2 TYPE string.

    CALL METHOD cl_gui_frontend_services=>directory_create
      EXPORTING
        directory                = fullpath
      CHANGING
        rc                       = l_rc    " Return code
      EXCEPTIONS
        directory_create_failed  = 1
        cntl_error               = 2
        error_no_gui             = 3
        directory_access_denied  = 4
        directory_already_exists = 5
        path_not_found           = 6
        unknown_error            = 7
        not_supported_by_gui     = 8
        wrong_parameter          = 9
        OTHERS                   = 10.
    IF sy-subrc = 0.
      RAISE EVENT dof_added EXPORTING fullpath = fullpath.
    ELSE.
      CASE sy-subrc.
        WHEN 4. RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'Not authorized'(109).
        WHEN 5. RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'Already exists'(102).
        WHEN 6. RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = 'Wrong path'. "TODO
        WHEN OTHERS.
          l_text = l_rc.
          l_text2 = sy-subrc.
          CONCATENATE 'Unknown return code'(101) ` ` l_text ` ` l_text2 INTO l_text.
          RAISE EXCEPTION TYPE lcx_freetext EXPORTING text = l_text.
      ENDCASE.
    ENDIF.
  ENDMETHOD."


  METHOD file_delete.
    DATA: rc    TYPE i.

    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename             = fullpath
      CHANGING
        rc                   = rc    " Return code
      EXCEPTIONS
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      " TODO
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend file delete'(025).
    ENDIF.

    RAISE EVENT dof_removed EXPORTING fullpath = fullpath.

  ENDMETHOD."


  METHOD directory_delete.
    DATA: rc    TYPE i.

    CALL METHOD cl_gui_frontend_services=>directory_delete
      EXPORTING
        directory               = fullpath
      CHANGING
        rc                      = rc    " Return code
      EXCEPTIONS
        directory_delete_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        path_not_found          = 4
        directory_access_denied = 5
        unknown_error           = 6
        not_supported_by_gui    = 7
        wrong_parameter         = 8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_freetext
        EXPORTING
          text = 'Frontend directory delete'(110).
    ENDIF.

    RAISE EVENT dof_removed EXPORTING fullpath = fullpath.
  ENDMETHOD."


  METHOD file_rename.
    DATA: l_path     TYPE string,
          xxx        TYPE sdok_filnm,
          l_fullpath TYPE string.

    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name = fullpath
      IMPORTING
        file_path = l_path
      EXCEPTIONS
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.

    xxx = new_filename.
    CALL METHOD cl_mime_services=>concatenate_path_name
      EXPORTING
        i_file_name = xxx
        i_directory = l_path
      IMPORTING
        e_pathname  = l_fullpath.

    file_copy( source = fullpath target = l_fullpath ).
    file_delete( fullpath ).

  ENDMETHOD."


  METHOD directory_rename.
    " TODO
  ENDMETHOD."


  METHOD execute.
    DATA: l_document    TYPE string.

    l_document = fullpath.
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document = l_document
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD."


  METHOD open_with.
    DATA: l_parameter  TYPE string.

    " Pour un frontend sous Windows Server 2008, il faut que ça commence par " mais pas de " final !
    " (un " final lui fait afficher une popup fichier xxxx" non trouvé)
    CONCATENATE 'SHELL32.DLL,OpenAs_RunDLL "' fullpath INTO l_parameter ##NO_TEXT.
    CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            application            = 'RUNDLL32.EXE' ##NO_TEXT
            parameter              = l_parameter
            synchronous            = 'X' ##NO_TEXT
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD."


  METHOD get_full_path.
    IF abap_true = cl_abap_matcher=>contains( pattern = '[/\\] *$' text = path ).
      CONCATENATE path name INTO full_path.
    ELSE."IF sy-opsys CP 'win*'.
      CONCATENATE path ai_file_sep name INTO full_path.
*    ELSE.
*      CONCATENATE path '/' name INTO full_path.
    ENDIF.
  ENDMETHOD."

  METHOD split_full_path.
    FIND REGEX ai_split_path_regex IN full_path SUBMATCHES result-path result-name.
    IF sy-subrc <> 0.
      result-path = full_path.
    ENDIF.
*    DATA: l_file_sep(1) TYPE c.
*
*    l_file_sep = get_file_sep( ).
*    SPLIT full_path AT l_file_sep INTO result-name result-path.
*    IF result-name IS INITIAL.
*      SPLIT result-path AT l_file_sep INTO result-name result-path.
*    ENDIF.
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_dofs IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_dofs IMPLEMENTATION.
  METHOD constructor.
    auto_dof = dofs.
  ENDMETHOD."
ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_dof IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_dof IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT dof
      EXPORTING
        file_system = file_system
        path        = path
        filename    = filename
        fullpath    = fullpath
        isdir       = isdir.
  ENDMETHOD."


  METHOD constructor.

    me->file_system = file_system.
    me->path = path.
    me->filename = filename.
    me->fullpath = fullpath.
    me->isdir = isdir.

    IF path IS NOT INITIAL AND filename IS NOT INITIAL.
      me->fullpath = file_system->get_full_path( name = filename path = path ).
    ELSEIF path IS NOT INITIAL AND filename IS INITIAL.
      me->fullpath = path.
    ELSEIF fullpath IS NOT INITIAL.
      me->fullpath = fullpath.
    ELSE.
      " exception TODO
    ENDIF.

    IF me->fullpath IS NOT INITIAL.
      CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
        EXPORTING
          full_name     = me->fullpath
        IMPORTING
          stripped_name = me->filename
          file_path     = me->path
        EXCEPTIONS
          OTHERS        = 2.
      IF sy-subrc <> 0.
        " TODO
      ENDIF.
    ENDIF.

    IF isdir = 1.
      me->path = me->fullpath.
      me->dirname = me->filename.
      me->filename = ''.
    ENDIF.

  ENDMETHOD."


  METHOD dof_exists.
    IF isdir = 0.
      result = file_system->file_exist( fullpath ).
    ELSE.
      result = file_system->directory_exist( fullpath ).
    ENDIF.
  ENDMETHOD."


  METHOD delete.
    IF isdir = 0.
      file_system->file_delete( fullpath ).
    ELSE.
      file_system->directory_delete( fullpath ).
    ENDIF.
  ENDMETHOD."


  METHOD rename.
    IF isdir = 0.
      file_system->file_rename( fullpath = fullpath new_filename = new_name ).
      me->filename = new_name.
      me->fullpath = file_system->get_full_path( name = me->filename path = path ).
    ELSE.
      file_system->directory_rename( fullpath = fullpath new_dirname = new_name ).
      me->dirname = new_name.
      me->fullpath = file_system->get_full_path( name = me->dirname path = path ).
    ENDIF.
  ENDMETHOD."


  METHOD execute.
    file_system->execute( fullpath ).
  ENDMETHOD."


  METHOD open_with.
    file_system->open_with( fullpath ).
  ENDMETHOD."


  METHOD file_copy.
    DATA ls_confirm TYPE lcl_popup_confirm=>ty_us_confirm.

    ls_confirm-o_source_dof = me.
    ls_confirm-o_target_dof = to.
    ls_confirm-overwrite = overwrite.
    IF abap_true = go_app->is_text_file( me->filename ).
      ls_confirm-binary = abap_false.
    ELSE.
      ls_confirm-binary = abap_true.
    ENDIF.
    go_app->file_copy( CHANGING cs_confirm = ls_confirm ).
  ENDMETHOD."


  METHOD read.
    file_system->file_read_binary( EXPORTING fullpath = me->fullpath IMPORTING content = content ).
  ENDMETHOD."


  METHOD get_file.
    IF me->isdir = 1.
      CREATE OBJECT dof
        EXPORTING
          file_system = me->file_system
          path        = me->path
          filename    = filename
          isdir       = 0.
    ENDIF.
  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_string IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_string IMPLEMENTATION.
  METHOD swa_string_to_table.

    DATA: abap_linesize    TYPE i,
          data_ref         TYPE REF TO data,
          remaining_length TYPE i,
          current_offset   TYPE i,
          last_offset      TYPE i.

    FIELD-SYMBOLS: <wa> TYPE any.

    total_length = strlen( character_string ).
    CHECK total_length > 0.

    IF NOT check_table_type IS INITIAL.
**** verify that the structure is flat and that all components are
**** of type C or N.
      GET REFERENCE OF character_table INTO data_ref.
      CALL FUNCTION 'SWA_COMPLEX_IS_CHARLIKE'
        EXPORTING
          ref_to_complex_field = data_ref
        EXCEPTIONS
          OTHERS               = 1.
      IF sy-subrc <> 0.
        RAISE no_flat_charlike_structure.
      ENDIF.
    ENDIF.

    CREATE DATA data_ref LIKE LINE OF character_table.
    ASSIGN data_ref->* TO <wa>.
**** UK 00/06/14 Unicode syntax
    DESCRIBE FIELD <wa> LENGTH abap_linesize IN CHARACTER MODE.

    IF    ( line_size <= 0 )
       OR ( line_size > abap_linesize ).
      line_size_used = abap_linesize.
    ELSE.
      line_size_used = line_size.
    ENDIF.

    remaining_length = total_length.
    last_offset      = total_length - line_size_used.
    CLEAR current_offset.
    IF append IS INITIAL.
      CLEAR character_table[].
    ENDIF.

    TRY.

        WHILE remaining_length >= line_size_used.

          <wa> = character_string+current_offset(line_size_used).
          APPEND <wa> TO character_table.
          IF move_trailing_blanks_next_line IS NOT INITIAL. "sro1
            DATA zzlength TYPE i.                           "sro1
            zzlength = strlen( <wa> ).                      "sro1
            IF zzlength = 0.                                "sro1
              ADD line_size_used TO current_offset.         "sro1
              SUBTRACT line_size_used FROM remaining_length."sro1
            ELSE.                                           "sro1
              ADD zzlength TO current_offset.               "sro1
              SUBTRACT zzlength FROM remaining_length.      "sro1
            ENDIF.                                          "sro1
          ELSE.                                             "sro1
            ADD line_size_used TO current_offset.
            SUBTRACT line_size_used FROM remaining_length.
          ENDIF.                                            "sro1

        ENDWHILE.  " remaining_length > line_size_used

**** handle the last line (which may not be filled completely)

        IF remaining_length > 0.  " some data left for the last line
          last_line_length = remaining_length.
          <wa> = character_string+current_offset(remaining_length).
          APPEND <wa> TO character_table.
        ELSEIF lines_filled > 1.
****  perfect fit: line_size_used divides total_length
          last_line_length = line_size_used.
        ELSE.
****  the entire string fits into one line of the table
          last_line_length = total_length.
        ENDIF.

        DESCRIBE TABLE character_table LINES lines_filled.

      CATCH cx_root.
    ENDTRY.

    IF sy-subrc <> 0.
      RAISE no_flat_charlike_structure.
    ENDIF.

  ENDMETHOD."

ENDCLASS.                    "lcl_string IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_rtti IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_rtti IMPLEMENTATION.

  METHOD get_low_level_components.
    ASSERT io_typedescr IS BOUND.
    ASSERT io_typedescr->kind = cl_abap_typedescr=>kind_struct.
    REFRESH et_component.

    CALL METHOD get_low_level_components_recur
      EXPORTING
        i_name       = ''
        io_typedescr = io_typedescr
      CHANGING
        ct_component = et_component.
  ENDMETHOD."


  METHOD get_low_level_components_recur.
    DATA: lo_structdescr    TYPE REF TO cl_abap_structdescr,
          lo_typedescr      TYPE REF TO cl_abap_typedescr,
          l_name            TYPE string,
          lt_view_component TYPE cl_abap_structdescr=>included_view.
    FIELD-SYMBOLS:
      <ls_view_component> TYPE abap_simple_componentdescr,
      <ls_component>      TYPE ty_gs_component.

    CASE io_typedescr->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        lo_structdescr ?= io_typedescr.
        lt_view_component = lo_structdescr->get_included_view( p_level = 1 ).
        LOOP AT lt_view_component ASSIGNING <ls_view_component>.
          IF i_name IS INITIAL.
            l_name = <ls_view_component>-name.
          ELSE.
            CONCATENATE i_name '-' <ls_view_component>-name INTO l_name.
          ENDIF.
          lo_typedescr = <ls_view_component>-type.
          CALL METHOD get_low_level_components_recur
            EXPORTING
              i_name       = l_name
              io_typedescr = lo_typedescr
            CHANGING
              ct_component = ct_component.
        ENDLOOP.
      WHEN OTHERS.
        APPEND INITIAL LINE TO ct_component ASSIGNING <ls_component>.
        <ls_component>-name = i_name.
        <ls_component>-o_typedescr = io_typedescr.
    ENDCASE.
  ENDMETHOD."


  METHOD get_component_name.
    DATA:
      l_byte_offset_of_field   TYPE i,
      l_byte_offset_of_field_2 TYPE i,
      lo_struct                TYPE REF TO cl_abap_structdescr,
      l_byte_offset_counter    TYPE i.
    FIELD-SYMBOLS:
      <any>     TYPE any,
      <ls_comp> TYPE abap_compdescr.

    DESCRIBE DISTANCE BETWEEN is_any AND i_structure_field INTO l_byte_offset_of_field IN BYTE MODE.
    CHECK sy-subrc = 0.


    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE is_any TO <any>.
      CHECK sy-subrc = 0.
      DESCRIBE DISTANCE BETWEEN is_any AND <any> INTO l_byte_offset_of_field_2 IN BYTE MODE.

      IF l_byte_offset_of_field = l_byte_offset_of_field_2.
        lo_struct ?= cl_abap_structdescr=>describe_by_data( is_any ).
        READ TABLE lo_struct->components INDEX sy-index ASSIGNING <ls_comp>.
        CHECK sy-subrc = 0.
        component_name = <ls_comp>-name.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD."


ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD get_lvc_fcat.
    DATA lo_table TYPE REF TO cl_salv_table.
    DATA lo_columns TYPE REF TO cl_salv_columns_list.
    DATA lo_agg TYPE REF TO cl_salv_aggregations.
    DATA lref_t_std TYPE REF TO data.
    DATA ls_fcat TYPE lvc_s_fcat.
    FIELD-SYMBOLS <lt_std> TYPE STANDARD TABLE.

    " Il faut créer une 2ème itab dont la structure est identique à
    " IT_STD car FACTORY passe la table interne en CHANGING
    " tandis que IT_STD existe en IMPORTING (sinon dump
    " à l'exécution si on passe CHANGING t_table = it_std).
    CREATE DATA lref_t_std LIKE it_std.
    ASSIGN lref_t_std->* TO <lt_std>.

    REFRESH rt_fcat.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = <lt_std>[].
      CATCH cx_salv_msg.
    ENDTRY.
    lo_columns = lo_table->get_columns( ).
    lo_agg = lo_table->get_aggregations( ).
    rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns = lo_columns r_aggregations = lo_agg ).
    " si aucun en-tête de colonne n'a pu être déterminé, utiliser
    " les noms des champs.
    LOOP AT rt_fcat INTO ls_fcat.
      IF ls_fcat-scrtext_l IS INITIAL
            AND ls_fcat-scrtext_m IS INITIAL
            AND ls_fcat-scrtext_s IS INITIAL
            AND ls_fcat-coltext IS INITIAL.
        ls_fcat-scrtext_l = ls_fcat-fieldname.
        MODIFY rt_fcat FROM ls_fcat TRANSPORTING scrtext_l.
      ENDIF.
    ENDLOOP.
  ENDMETHOD."

ENDCLASS.                    "lcl_alv IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_sscr IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_sscr IMPLEMENTATION.

  METHOD disable_all_function_codes.
    DATA: lt_fkey TYPE TABLE OF rsmpe_keys,
          lt_itab TYPE TABLE OF sy-ucomm.
    FIELD-SYMBOLS:
          <ls_fkey>     TYPE rsmpe_keys.

    CALL FUNCTION 'RS_CUA_GET_STATUS'
      EXPORTING
        program               = 'RSSYSTDB'
        status                = i_pf_status
        suppress_cmod_entries = abap_false
      TABLES
        functionkeys          = lt_fkey
      EXCEPTIONS
        not_found_program     = 1
        not_found_status      = 2
        recursive_menues      = 3
        empty_list            = 4
        not_found_menu        = 5
        OTHERS                = 6.
    LOOP AT lt_fkey ASSIGNING <ls_fkey>.
      READ TABLE except WITH KEY table_line = <ls_fkey>-code TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND <ls_fkey>-code TO lt_itab.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = i_pf_status
        p_program = sy-repid
      TABLES
        p_exclude = lt_itab.

  ENDMETHOD."

ENDCLASS."


*----------------------------------------------------------------------*
*       CLASS lcx_freetext IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcx_freetext IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->text = text.
  ENDMETHOD.                    "constructor
  METHOD get_text.
    DATA: lo_freetext_message   TYPE REF TO cl_freetext_message.

    CREATE OBJECT lo_freetext_message
      EXPORTING
        the_raw_text = text
        the_subject  = me.
    result = lo_freetext_message->get_text( ).
  ENDMETHOD.                    "get_text
  METHOD get_longtext.
    result = 'No details as the message is built as a "free text"'(013).
  ENDMETHOD.                    "get_longtext
ENDCLASS.                    "lcx_freetext IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_HTML_VIEWER_HELPER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_viewer_helper IMPLEMENTATION.

  METHOD decode_values.
    DATA: l_postdata         TYPE string,
          lt_simple_postdata TYPE tihttpnvp,
          l_name             TYPE string,
          l_index            TYPE string.
    FIELD-SYMBOLS:
      <ls_simple_postdata> TYPE ihttpnvp,
      <ls_table_postdata>  TYPE ty_us_table_postdata,
      <ls_table_postdata2> TYPE ty_us_table_postdata2.

    REFRESH : et_simple_postdata,
              et_table_postdata.

    " Décodage des valeurs de postdata
    " (on pourrait utiliser query_table mais c'est limité à des valeurs jusque 255 caractères)
    CALL FUNCTION 'SWA_STRING_FROM_TABLE'
      EXPORTING
        character_table            = it_postdata
        keep_trailing_spaces       = 'X'
      IMPORTING
        character_string           = l_postdata
      EXCEPTIONS
        no_flat_charlike_structure = 1
        OTHERS                     = 2.
    IF sy-subrc NE 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    "--------------------
    " remove trailing spaces
    "--------------------
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = l_postdata ).

    lt_simple_postdata = cl_http_utility=>string_to_fields( l_postdata ).
    LOOP AT lt_simple_postdata ASSIGNING <ls_simple_postdata>.
      FIND REGEX '^ *([^ \[]+)(?:\[(\d+)\])? *$' IN <ls_simple_postdata>-name SUBMATCHES l_name l_index.
      IF abap_true = i_name_to_lower_case.
        TRANSLATE l_name TO LOWER CASE.
      ENDIF.
      IF l_index IS INITIAL.
        APPEND <ls_simple_postdata> TO et_simple_postdata.
      ELSE.
        READ TABLE et_table_postdata WITH KEY name = l_name ASSIGNING <ls_table_postdata>.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO et_table_postdata ASSIGNING <ls_table_postdata>.
          <ls_table_postdata>-name = l_name.
        ENDIF.
        APPEND INITIAL LINE TO <ls_table_postdata>-t_value ASSIGNING <ls_table_postdata2>.
        <ls_table_postdata2>-index = l_index.
        <ls_table_postdata2>-value = <ls_simple_postdata>-value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD."

ENDCLASS."
