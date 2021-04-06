*&---------------------------------------------------------------------*
*& Report zbw_trfn_tester
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_trfn_tester_standalone.

CLASS zcl_bw_trfn_tester DEFINITION DEFERRED.
CLASS zcx_bw_trfn_tester DEFINITION
INHERITING FROM CX_STATIC_CHECK
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcx_bw_trfn_tester IMPLEMENTATION.
ENDCLASS.

CLASS zcl_bw_trfn_tester DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter pa_trfnid | <p class="shorttext synchronized" lang="en">Transformation ID</p>
    "! @raising zcx_bw_trfn_tester | <p class="shorttext synchronized" lang="en">Invalid TRFN ID</p>
    METHODS constructor
      IMPORTING pa_trfnid TYPE rstranid
      RAISING   zcx_bw_trfn_tester.

    "! <p class="shorttext synchronized" lang="en"></p>
    "! Calculate program ID based on TRFN ID
    "! @parameter rv_progid | <p class="shorttext synchronized" lang="en"></p>
    METHODS caluclate_program_id
      RETURNING VALUE(rv_progid) TYPE string.

    "! <p class="shorttext synchronized" lang="en"></p>
    "! Test new scenario - input->calculation->compare result with provided result data
    "! @parameter ir_source_user_table | <p class="shorttext synchronized" lang="en">Source table to compare</p>
    "! @parameter ir_result_user_table | <p class="shorttext synchronized" lang="en">Result table to compare</p>
    METHODS test_new_scenario
      IMPORTING iv_source_ddic_table TYPE string OPTIONAL
                iv_result_ddic_table TYPE string OPTIONAL
                ir_source_user_table TYPE REF TO data OPTIONAL
                ir_result_user_table TYPE REF TO data OPTIONAL.
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get data from DDIC table name
    "! @parameter iv_table_name | <p class="shorttext synchronized" lang="en">DDIC Table name</p>
    "! @parameter et_table | <p class="shorttext synchronized" lang="en">Table data</p>
    METHODS get_data_from_table
      IMPORTING iv_table_name TYPE string
      EXPORTING et_table      TYPE ANY TABLE.

  PROTECTED SECTION.

    DATA: gv_trfnid TYPE rstranid.

  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_bw_trfn_tester IMPLEMENTATION.
  METHOD constructor.

    SELECT SINGLE @abap_true    ##SUBRC_OK
    FROM rstran
    INTO @DATA(lv_trfn_exist)
    WHERE tranid = @pa_trfnid
    AND objvers = 'A'
    AND objstat = 'ACT'.

    IF lv_trfn_exist = abap_true.
      gv_trfnid = pa_trfnid.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bw_trfn_tester.
    ENDIF.

  ENDMETHOD.

  METHOD caluclate_program_id.

    TRY.
        DATA(lobj_rstran_miantain) = NEW cl_rstran_maintain(
                                         i_tranid = gv_trfnid ).
      CATCH cx_rstran_not_found.
      CATCH cx_rstran_input_invalid.
      CATCH cx_rstran_cancelled.
      CATCH cx_rstran_not_authorized.
      CATCH cx_rstran_already_exist.
      CATCH cx_rstran_error_with_message.
    ENDTRY.

    lobj_rstran_miantain->get_progid( IMPORTING e_progid = DATA(lv_progid) ).

    rv_progid = |GP{ lv_progid }|.

  ENDMETHOD.

  METHOD test_new_scenario.

    DATA: lr_source_table TYPE REF TO data,
          lr_result_table TYPE REF TO data.

    FIELD-SYMBOLS: <lt_source_table> TYPE STANDARD TABLE,
                   <lt_result_table> TYPE STANDARD TABLE.

    IF iv_source_ddic_table IS NOT INITIAL.

      CREATE DATA lr_source_table TYPE STANDARD TABLE OF (iv_source_ddic_table).
      ASSIGN lr_source_table->* TO <lt_source_table>.

      IF sy-subrc = 0.
        get_data_from_table(
           EXPORTING
            iv_table_name = iv_source_ddic_table
           IMPORTING
            et_table      = <lt_source_table>
        ).
      ENDIF.

    ENDIF.

    IF iv_result_ddic_table IS NOT INITIAL.

      CREATE DATA lr_result_table TYPE STANDARD TABLE OF (iv_result_ddic_table).
      ASSIGN lr_result_table->* TO <lt_result_table>.
      IF sy-subrc = 0.
        get_data_from_table(
          EXPORTING
            iv_table_name = iv_result_ddic_table
          IMPORTING
            et_table      = <lt_result_table>
        ).
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD get_data_from_table.

    SELECT *             ##SUBRC_OK
    FROM (iv_table_name)
    INTO TABLE @et_table.

  ENDMETHOD.

ENDCLASS.

PARAMETERS: pa_trfn TYPE rstranid.
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

  PARAMETERS: pa_stabn TYPE char15 MODIF ID g12.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (25) TEXT-003 USER-COMMAND create MODIF ID g13.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004 .
  PARAMETERS:
    pa_rtabl RADIOBUTTON GROUP rgr3 MODIF ID g3 USER-COMMAND uc3 DEFAULT 'X',
    pa_rownd RADIOBUTTON GROUP rgr3 MODIF ID g3.

  PARAMETERS: pa_rtabn TYPE char15 MODIF ID g14.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (25) TEXT-005 USER-COMMAND create MODIF ID g15.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-006 .
  PARAMETERS:
    pa_ctrfn TYPE string MODIF ID g2.
SELECTION-SCREEN END OF BLOCK b4.

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
      DATA(lobj_trfn_tester) = NEW zcl_bw_trfn_tester( pa_trfnid = pa_trfn ).
    CATCH zcx_bw_trfn_tester.
      MESSAGE 'Tranformation do not exist or not active' TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

  IF pa_new = abap_true.

    lobj_trfn_tester->test_new_scenario(
        iv_source_ddic_table = CONV #( pa_stabn )
        iv_result_ddic_table = CONV #( pa_rtabn )
    ).

  ENDIF.

  CHECK pa_ctrfn IS INITIAL.

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.14.3 - 2021-04-06T18:23:34.219Z
ENDINTERFACE.
****************************************************