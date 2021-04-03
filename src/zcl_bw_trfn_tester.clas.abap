CLASS zcl_bw_trfn_tester DEFINITION
  PUBLIC
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
