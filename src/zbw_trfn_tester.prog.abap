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

DATA: lr_data_src TYPE REF TO data,
      lr_data_res TYPE REF TO data.

PARAMETERS: pa_strfn TYPE rstran-sourcename OBLIGATORY,
            pa_ttrfn TYPE rstran-targetname OBLIGATORY.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  pa_new RADIOBUTTON GROUP rgr1 USER-COMMAND uc1 DEFAULT 'X',
  pa_ovn RADIOBUTTON GROUP rgr1,
  pa_pte RADIOBUTTON GROUP rgr1.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
PARAMETERS:
  pa_scttb RADIOBUTTON GROUP rgr2 MODIF ID g1 USER-COMMAND uc2 DEFAULT 'X',
  pa_stabl RADIOBUTTON GROUP rgr2 MODIF ID g1,
  pa_sownd RADIOBUTTON GROUP rgr2 MODIF ID g1,
  pa_lsdat RADIOBUTTON GROUP rgr2 MODIF ID g1.

PARAMETERS: pa_stabn TYPE dd02l-tabname MODIF ID g12.
PARAMETERS: pa_stemp TYPE dd02l-tabname MODIF ID g20.
PARAMETERS: pa_svnam TYPE char20 MODIF ID g17.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON (25) TEXT-003 USER-COMMAND create_input MODIF ID g13.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON (29) TEXT-007 USER-COMMAND load_input MODIF ID g16.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004 .
PARAMETERS:
  pa_rcttb RADIOBUTTON GROUP rgr3 MODIF ID g3 USER-COMMAND uc3 DEFAULT 'X',
  pa_rtabl RADIOBUTTON GROUP rgr3 MODIF ID g3,
  pa_rownd RADIOBUTTON GROUP rgr3 MODIF ID g3,
  pa_lrdat RADIOBUTTON GROUP rgr3 MODIF ID g3.

PARAMETERS: pa_rtabn TYPE dd02l-tabname MODIF ID g14.
PARAMETERS: pa_rtemp TYPE dd02l-tabname MODIF ID g21.
PARAMETERS: pa_rvnam TYPE char20 MODIF ID g19.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON (25) TEXT-005 USER-COMMAND create_result MODIF ID g15.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON (29) TEXT-008 USER-COMMAND load_result MODIF ID g18.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-006 .
PARAMETERS:
  pa_ctrfn TYPE string MODIF ID g2.
SELECTION-SCREEN END OF BLOCK b4.

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.

    WHEN  'CREATE_INPUT'.
      lr_data_src = zcl_bw_trfn_tester_ui=>create_data_package(
           iv_stemp    = pa_stemp
           iv_svnam    = pa_svnam
           iv_ttrfn    = pa_ttrfn
           iv_strfn    = pa_strfn
           iv_repid    = sy-repid
           iv_scttb    = pa_scttb
           iv_ttttb    = pa_rcttb
           iv_type     = 'SRC' ).

    WHEN  'CREATE_RESULT'.
      lr_data_res = zcl_bw_trfn_tester_ui=>create_data_package(
          iv_stemp    = pa_rtemp
          iv_svnam    = pa_rvnam
          iv_ttrfn    = pa_ttrfn
          iv_strfn    = pa_strfn
          iv_repid    = sy-repid
          iv_scttb    = pa_scttb
          iv_ttttb    = pa_rcttb
          iv_type     = 'RES' ).

    WHEN  'LOAD_INPUT'.
      lr_data_src =  zcl_bw_trfn_tester_ui=>load_variant(
           iv_type  = 'SRC'
           iv_svnam = pa_svnam
           iv_repid = sy-repid ).

    WHEN 'LOAD_RESULT'.
      lr_data_res = zcl_bw_trfn_tester_ui=>load_variant(
          iv_type  = 'RES'
          iv_svnam = pa_rvnam
          iv_repid = sy-repid ).

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

    IF screen-name = 'PA_OVN'
    OR screen-name = 'PA_PTE'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.

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
      OR screen-group1 = 'G15'
      OR screen-group1 = 'G20'
      OR screen-group1 = 'G21'.

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

    IF pa_scttb = abap_true.
      IF screen-group1 = 'G13'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G16' OR
      screen-group1 = 'G12' OR
      screen-group1 = 'G20'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF pa_stabl = abap_true.

      IF screen-group1 = 'G12'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G13'
      OR screen-group1 = 'G16'
      OR screen-group1 = 'G17'
      OR screen-group1 = 'G20'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.
    IF pa_sownd = abap_true.

      IF screen-group1 = 'G13'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G12'
      OR screen-group1 = 'G16'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_rtabl = abap_true.

      IF screen-group1 = 'G14'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G15'
      OR screen-group1 = 'G18'
      OR screen-group1 = 'G19'
      OR screen-group1 = 'G21'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_rownd = abap_true.

      IF screen-group1 = 'G15'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G14'
      OR screen-group1 = 'G18'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF pa_rcttb = abap_true.

      IF screen-group1 = 'G15'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G14' OR
      screen-group1 = 'G18'
       OR screen-group1 = 'G21'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF pa_lsdat = abap_true.
      IF screen-group1 = 'G16'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G13' OR
      screen-group1 = 'G12'
      OR screen-group1 = 'G20'
      OR screen-group1 = 'G21'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF pa_lrdat = abap_true.
      IF screen-group1 = 'G18'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G14' OR
      screen-group1 = 'G15'
      OR screen-group1 = 'G21'.
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
      EXIT.
  ENDTRY.

  IF pa_new = abap_true.

    IF lr_data_src IS NOT BOUND OR lr_data_res IS NOT BOUND.
      MESSAGE 'Please choose both - source and target data' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    lobj_trfn_tester->test_new_scenario(
        iv_source_ddic_table = CONV #( pa_stabn )
        iv_result_ddic_table = CONV #( pa_rtabn )
        ir_source_user_table = lr_data_src
        ir_result_user_table = lr_data_res
    ).

  ENDIF.

  CHECK pa_ctrfn IS INITIAL.
