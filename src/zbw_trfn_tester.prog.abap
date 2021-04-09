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

PARAMETERS: pa_strfn TYPE rstran-sourcename,
            pa_ttrfn TYPE rstran-targetname.
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
    pa_sownd RADIOBUTTON GROUP rgr2 MODIF ID g1,
    pa_lsdat RADIOBUTTON GROUP rgr2 MODIF ID g1.

  PARAMETERS: pa_stabn TYPE dd02l-tabname MODIF ID g12.
  PARAMETERS: pa_stemp TYPE dd02l-tabname MODIF ID g13.
  PARAMETERS: pa_svnam TYPE dd02l-tabname MODIF ID g17.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (25) TEXT-003 USER-COMMAND create_input MODIF ID g13.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (29) TEXT-007 USER-COMMAND load_input MODIF ID g16.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004 .
  PARAMETERS:
    pa_rtabl RADIOBUTTON GROUP rgr3 MODIF ID g3 USER-COMMAND uc3 DEFAULT 'X',
    pa_rownd RADIOBUTTON GROUP rgr3 MODIF ID g3,
    pa_lrdat RADIOBUTTON GROUP rgr3 MODIF ID g3.

  PARAMETERS: pa_rtabn TYPE dd02l-tabname MODIF ID g14.
  PARAMETERS: pa_rtemp TYPE dd02l-tabname MODIF ID g15.
  PARAMETERS: pa_rvnam TYPE dd02l-tabname MODIF ID g15.
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

      IF pa_stemp IS NOT INITIAL.
        SELECT fieldname, rollname
        FROM dd03l
        INTO TABLE @DATA(lt_dd03l)
        WHERE as4local = 'A'
        AND tabname = @pa_stemp.
        IF sy-subrc <> 0.
          MESSAGE 'Error during type select' TYPE 'E'.
        ENDIF.

        lt_type_result = CORRESPONDING #(  lt_dd03l MAPPING input_type = rollname
                                                            input_type_name = fieldname ).
      ELSE.

        DO 10 TIMES.
          lt_type_result = VALUE #( BASE lt_type_result (  input_type_name = '' ) ).
        ENDDO.
      ENDIF.
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

      DELETE lt_type_result WHERE input_type_name IS INITIAL.

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

      IF pa_svnam IS NOT INITIAL.

        DATA: lt_var_tab  TYPE STANDARD TABLE OF zbwtrfn_var_type,
              ls_var_tab  TYPE zbwtrfn_var_type,
              lt_data_src TYPE STANDARD TABLE OF zbwtrfn_var_data,
              ls_data_src TYPE zbwtrfn_var_data.

        LOOP AT lt_type_result REFERENCE INTO DATA(lr_type_res_src).

          ls_var_tab-template_table = pa_stemp.
          ls_var_tab-variant = pa_svnam.
          ls_var_tab-data_type = 'SRC'.
          ls_var_tab-type = lr_type_res_src->input_type.
          ls_var_tab-fieldnm = lr_type_res_src->input_type_name.

          APPEND ls_var_tab TO lt_var_tab.

        ENDLOOP.

        INSERT zbwtrfn_var_type FROM TABLE @lt_var_tab.
        IF sy-subrc <> 0 .
          MESSAGE 'Error during variant' TYPE 'E'.
        ENDIF.

        LOOP AT <lt_data_type> ASSIGNING FIELD-SYMBOL(<ls_data_type>).
          DATA(lv_rnr) = sy-tabix.
          DO lines( lt_type_result ) TIMES.
            ASSIGN COMPONENT sy-index OF STRUCTURE <ls_data_type> TO FIELD-SYMBOL(<ls_component>).
            IF sy-subrc <> 0.
              MESSAGE 'Error component assign' TYPE 'E'.
            ENDIF.
            READ TABLE lt_type_result INDEX sy-index REFERENCE INTO lr_type_res_src.

            IF sy-subrc = 0.
              ls_data_src-rownr = lv_rnr.
              ls_data_src-template_table = pa_stemp.
              ls_data_src-variant = pa_svnam.
              ls_data_src-data_type = 'SRC'.
              ls_data_src-fieldnm = lr_type_res_src->input_type_name.
              ls_data_src-value = <ls_component>.

              APPEND ls_data_src TO lt_data_src.

            ENDIF.

          ENDDO.

        ENDLOOP.

        INSERT zbwtrfn_var_data FROM TABLE @lt_data_src.
        IF sy-subrc <> 0.
          MESSAGE 'Error during assign' TYPE 'E'.
        ENDIF.

      ENDIF.

    WHEN  'CREATE_RESULT'.

      CLEAR lt_comp.

      IF pa_rtemp IS NOT INITIAL.
        SELECT fieldname, rollname
        FROM dd03l
        INTO TABLE @lt_dd03l
        WHERE as4local = 'A'
        AND tabname = @pa_rtemp.
        IF sy-subrc <> 0.
          MESSAGE 'Error during type select' TYPE 'E'.
        ENDIF.


        lt_type_result = CORRESPONDING #(  lt_dd03l MAPPING input_type = rollname
                                                            input_type_name = fieldname ).

      ELSE.
        DO 10 TIMES.
          lt_type_result = VALUE #( BASE lt_type_result (  input_type_name = '' ) ).
        ENDDO.
      ENDIF.

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
          i_screen_end_column     = 100
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

      DELETE lt_type_result WHERE input_type_name IS INITIAL.

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

      CREATE DATA: lr_data_res TYPE HANDLE lr_type_table_res.
      ASSIGN lr_data_res->* TO <lt_data_res>.

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

    WHEN  'LOAD_INPUT'.

      CLEAR lt_data_fcat_res.

      DATA: lr_saved_str TYPE REF TO data.

      FIELD-SYMBOLS: <ls_saved_data> TYPE any.

      UNASSIGN <lt_data_type>.
      FREE lr_data_src.
      CLEAR lt_comp.

      SELECT * FROM
      zbwtrfn_var_type
      INTO TABLE @DATA(lt_var_type)
      WHERE variant = @pa_svnam.
      IF sy-subrc <> 0.
        MESSAGE 'Error during select' TYPE 'E'.
      ENDIF.

      LOOP AT lt_var_type REFERENCE INTO DATA(lr_var_type).
        ls_comp-name =  lr_var_type->fieldnm.
        ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( to_upper( lr_var_type->type ) ) .
        APPEND ls_comp TO lt_comp.
      ENDLOOP.

      TRY.
          lr_type_str = cl_abap_structdescr=>create( p_components = lt_comp ).
        CATCH cx_sy_struct_creation.
          MESSAGE 'Error suring str creation' TYPE 'E'.
      ENDTRY.

      TRY.
          lr_type_table = cl_abap_tabledescr=>create( lr_type_str ).
        CATCH cx_sy_table_creation .
      ENDTRY.

      CREATE DATA: lr_data_src TYPE HANDLE lr_type_table.
      ASSIGN lr_data_src->* TO <lt_data_type>.
      IF sy-subrc <> 0.
        MESSAGE 'Error during assign' TYPE 'E'.
      ENDIF.

      CREATE DATA lr_saved_str TYPE HANDLE lr_type_str.
      ASSIGN lr_saved_str->* TO <ls_saved_data>.
      IF sy-subrc <> 0.
        MESSAGE 'Error during assign' TYPE 'E'.
      ENDIF.

      SELECT * FROM
      zbwtrfn_var_data
      INTO TABLE @DATA(lt_var_data)
      WHERE variant = @pa_svnam.
      IF sy-subrc <> 0.
        MESSAGE 'Error during variant select' TYPE 'E'.
      ENDIF.

      LOOP AT lt_var_data REFERENCE INTO DATA(lr_var_group) GROUP BY lr_var_group->rownr.

        LOOP AT GROUP lr_var_group REFERENCE INTO DATA(lr_var_data).

          ASSIGN COMPONENT lr_var_data->fieldnm OF STRUCTURE <ls_saved_data> TO FIELD-SYMBOL(<ls_field>).
          IF sy-subrc <> 0.
            MESSAGE 'Error during type select' TYPE 'E'.
          ENDIF.
          <ls_field> = lr_var_data->value.

          IF lr_var_group->rownr = 1.
            lt_data_fcat_res = VALUE #(
             BASE lt_data_fcat_res (
             seltext_m = lr_var_data->fieldnm
             fieldname = to_upper( lr_var_data->fieldnm )
             edit = abap_true ) ).
          ENDIF.


        ENDLOOP.

        APPEND <ls_saved_data> TO <lt_data_type>.
        CLEAR <ls_saved_data> .

      ENDLOOP.

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
          t_outtab                = <lt_data_type>
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

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
      ELSEIF screen-group1 = 'G13'
      OR screen-group1 = 'G16'
      OR screen-group1 = 'G17'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.
    IF pa_sownd = abap_true.

      IF screen-group1 = 'G13'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G12' OR
      screen-group1 = 'G16'.
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

    IF pa_lsdat = abap_true.
      IF screen-group1 = 'G16'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G13' OR
      screen-group1 = 'G12'.
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
