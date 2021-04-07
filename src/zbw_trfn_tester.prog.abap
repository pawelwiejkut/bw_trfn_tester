*&---------------------------------------------------------------------*
*& Report zbw_trfn_tester
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_trfn_tester.

TABLES sscrfields.

TYPES: BEGIN OF ty_type_input,
         input_type_name TYPE string,
         input_type      TYPE string.
TYPES: END OF ty_type_input.

DATA: lt_type_result   TYPE STANDARD TABLE OF ty_type_input,
      lt_fieldcatalog  TYPE lvc_t_fcat,
      ls_comp          TYPE  cl_abap_structdescr=>component,
      lt_comp          TYPE cl_abap_structdescr=>component_table,
      lr_data_src      TYPE REF TO data,
      lt_data_fcat_src TYPE  slis_t_fieldcat_alv,
      lr_data_res      TYPE REF TO data,
      lt_data_fcat_res TYPE  slis_t_fieldcat_alv,
      lv_repid         LIKE sy-repid.

FIELD-SYMBOLS:
  <lt_data_type> TYPE STANDARD TABLE,
  <lt_data_res>  TYPE STANDARD TABLE.

PARAMETERS: pa_strfn TYPE sobj_name,
            pa_ttrfn TYPE sobj_name.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    pa_new RADIOBUTTON GROUP rgr1 USER-COMMAND uc1 DEFAULT 'X',
    pa_ovn RADIOBUTTON GROUP rgr1,
    pa_pte RADIOBUTTON GROUP rgr1.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
  PARAMETERS:
    pa_stabl RADIOBUTTON GROUP rgr2 MODIF ID g1 USER-COMMAND uc2 DEFAULT 'X',
    pa_sownd RADIOBUTTON GROUP rgr2 MODIF ID g1.

  PARAMETERS: pa_stabn TYPE dd02l-tabname MODIF ID g12.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (25) TEXT-003 USER-COMMAND create_input MODIF ID g13.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004 .
  PARAMETERS:
    pa_rtabl RADIOBUTTON GROUP rgr3 MODIF ID g3 USER-COMMAND uc3 DEFAULT 'X',
    pa_rownd RADIOBUTTON GROUP rgr3 MODIF ID g3.

  PARAMETERS: pa_rtabn TYPE dd02l-tabname MODIF ID g14.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (25) TEXT-005 USER-COMMAND create_result MODIF ID g15.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-006 .
  PARAMETERS:
    pa_ctrfn TYPE string MODIF ID g2.
SELECTION-SCREEN END OF BLOCK b4.

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN  'CREATE_INPUT'.

      CLEAR lt_comp.

      DO 10 TIMES.
        lt_type_result = VALUE #( BASE lt_type_result (  input_type_name = '' ) ).
      ENDDO.

      DATA(lt_fcat) = VALUE slis_t_fieldcat_alv(
      ( seltext_m = 'Field name' fieldname = 'INPUT_TYPE_NAME'  outputlen = '20' edit = abap_true )
      ( seltext_m = 'Field type' fieldname = 'INPUT_TYPE'       outputlen = '20' edit = abap_true ) ).

      lv_repid = sy-repid.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program      = lv_repid
          it_fieldcat             = lt_fcat
          i_screen_start_column   = 1
          i_screen_start_line     = 1
          i_screen_end_column     = 60
          i_screen_end_line       = 10
          i_callback_user_command = 'USER_COMMAND'
        TABLES
          t_outtab                = lt_type_result
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM lt_type_result COMPARING ALL FIELDS.

      CHECK lt_type_result IS NOT INITIAL.

      LOOP AT lt_type_result REFERENCE INTO DATA(lr_type_input).
        ls_comp-name =  lr_type_input->input_type_name.
        ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( to_upper( lr_type_input->input_type ) ) .
        IF sy-subrc <> 0.
          MESSAGE 'Type not found' TYPE 'E'.
        ENDIF.
        APPEND ls_comp TO lt_comp.

        lt_data_fcat_src = VALUE #(
        BASE lt_data_fcat_src (
        seltext_m = lr_type_input->input_type_name
        fieldname = to_upper( lr_type_input->input_type_name )
        edit = abap_true ) ).

      ENDLOOP.
      TRY.
          DATA(lr_type_str) = cl_abap_structdescr=>create( p_components = lt_comp ).
        CATCH cx_sy_struct_creation.
          MESSAGE 'Error suring str creation' TYPE 'E'.
      ENDTRY.

      TRY.
          DATA(lr_type_table) = cl_abap_tabledescr=>create( lr_type_str ).
        CATCH cx_sy_table_creation .
      ENDTRY.

      CREATE DATA: lr_data_src TYPE HANDLE lr_type_table.
      ASSIGN lr_data_src->* TO <lt_data_type>.

      IF sy-subrc <> 0.
        MESSAGE 'Error during type creation' TYPE 'E'.
      ENDIF.

      DO 10 TIMES.
        APPEND INITIAL LINE TO <lt_data_type>.
      ENDDO.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program      = lv_repid
          it_fieldcat             = lt_data_fcat_src
          i_screen_start_column   = 1
          i_screen_start_line     = 1
          i_screen_end_column     = 60
          i_screen_end_line       = 10
          i_callback_user_command = 'USER_COMMAND'
        TABLES
          t_outtab                = <lt_data_type>
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM <lt_data_type> COMPARING ALL FIELDS.

    WHEN  'CREATE_RESULT'.

      CLEAR lt_comp.

      DO 10 TIMES.
        lt_type_result = VALUE #( BASE lt_type_result (  input_type_name = '' ) ).
      ENDDO.

      lt_fcat = VALUE slis_t_fieldcat_alv(
      ( seltext_m = 'Field name' fieldname = 'INPUT_TYPE_NAME'  outputlen = '20' edit = abap_true )
      ( seltext_m = 'Field type' fieldname = 'INPUT_TYPE'       outputlen = '20' edit = abap_true ) ).

      lv_repid = sy-repid.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program      = lv_repid
          it_fieldcat             = lt_fcat
          i_screen_start_column   = 1
          i_screen_start_line     = 1
          i_screen_end_column     = 60
          i_screen_end_line       = 10
          i_callback_user_command = 'USER_COMMAND'
        TABLES
          t_outtab                = lt_type_result
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM lt_type_result COMPARING ALL FIELDS.

      CHECK lt_type_result IS NOT INITIAL.

      LOOP AT lt_type_result REFERENCE INTO DATA(lr_type_result).
        ls_comp-name =  lr_type_result->input_type_name.
        ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( to_upper( lr_type_result->input_type ) ) .
        IF sy-subrc <> 0.
          MESSAGE 'Type not found' TYPE 'E'.
        ENDIF.
        APPEND ls_comp TO lt_comp.

        lt_data_fcat_res = VALUE #(
        BASE lt_data_fcat_res (
        seltext_m = lr_type_result->input_type_name
        fieldname = to_upper( lr_type_result->input_type_name )
        edit = abap_true ) ).

      ENDLOOP.
      TRY.
          DATA(lr_type_res) = cl_abap_structdescr=>create( p_components = lt_comp ).
        CATCH cx_sy_struct_creation.
          MESSAGE 'Error suring str creation' TYPE 'E'.
      ENDTRY.

      TRY.
          DATA(lr_type_table_res) = cl_abap_tabledescr=>create( lr_type_res ).
        CATCH cx_sy_table_creation .
      ENDTRY.

      CREATE DATA: lr_data_res TYPE HANDLE lr_type_res.
      ASSIGN lr_data_res->* TO <lt_data_type>.

      IF sy-subrc <> 0.
        MESSAGE 'Error during type creation' TYPE 'E'.
      ENDIF.

      DO 10 TIMES.
        APPEND INITIAL LINE TO <lt_data_res>.
      ENDDO.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program      = lv_repid
          it_fieldcat             = lt_data_fcat_res
          i_screen_start_column   = 1
          i_screen_start_line     = 1
          i_screen_end_column     = 60
          i_screen_end_line       = 10
          i_callback_user_command = 'USER_COMMAND'
        TABLES
          t_outtab                = <lt_data_res>
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM <lt_data_res> COMPARING ALL FIELDS.

  ENDCASE.

FORM user_command USING ucomm LIKE sy-ucomm
                        selfield TYPE slis_selfield.

  DATA:
    lo_grid TYPE REF TO cl_gui_alv_grid.

  IF lo_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = lo_grid.
  ENDIF.
  IF lo_grid IS NOT INITIAL.
    lo_grid->check_changed_data( ).
  ENDIF.
ENDFORM.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF pa_new = abap_true.

      IF screen-group1 = 'G1'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G2'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_ovn = abap_true.

      IF screen-group1 = 'G2'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G1'
      OR screen-group1 = 'G12'
      OR screen-group1 = 'G13'
      OR screen-group1 = 'G3'
      OR screen-group1 = 'G14'
      OR screen-group1 = 'G15'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_pte = abap_true.

      IF screen-group1 = 'G4'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G1'
      OR screen-group1 = 'G12'
      OR screen-group1 = 'G13'
      OR screen-group1 = 'G3'
      OR screen-group1 = 'G14'
      OR screen-group1 = 'G15'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_stabl = abap_true.

      IF screen-group1 = 'G12'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G13'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.
    IF pa_sownd = abap_true.

      IF screen-group1 = 'G13'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G12'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_rtabl = abap_true.

      IF screen-group1 = 'G14'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G15'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_rownd = abap_true.

      IF screen-group1 = 'G15'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G14'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

END-OF-SELECTION.

  TRY.
      DATA(lobj_trfn_tester) = NEW zcl_bw_trfn_tester(  iv_strfn = pa_strfn
                                                        iv_ttrfn = pa_ttrfn ).
    CATCH zcx_bw_trfn_tester.
      MESSAGE 'Tranformation do not exist or not active' TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

  IF pa_new = abap_true.

    lobj_trfn_tester->test_new_scenario(
        iv_source_ddic_table = CONV #( pa_stabn )
        iv_result_ddic_table = CONV #( pa_rtabn )
        ir_source_user_table = lr_data_src
    ).

  ENDIF.

  CHECK pa_ctrfn IS INITIAL.
