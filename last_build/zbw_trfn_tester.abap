*&---------------------------------------------------------------------*
*& Report zbw_trfn_tester
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_trfn_tester_standalone.

CLASS zcl_bw_trfn_tester_ui DEFINITION DEFERRED.
CLASS zcl_bw_trfn_tester DEFINITION DEFERRED.
CLASS zcx_bw_trfn_tester DEFINITION
INHERITING FROM cx_static_check
  PUBLIC
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
      IMPORTING iv_strfn TYPE sobj_name
                iv_ttrfn TYPE sobj_name
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

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter rr_log | <p class="shorttext synchronized" lang="en">Log reference</p>
    METHODS create_log
      RETURNING VALUE(rr_log) TYPE REF TO cl_rsbm_log_cursor_step_dtp.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter iv_source_ddic_table | <p class="shorttext synchronized" lang="en">DDIC source table name</p>
    "! @parameter iv_result_ddic_table | <p class="shorttext synchronized" lang="en">DDIC result table name</p>
    "! @parameter er_src_str_trfn | <p class="shorttext synchronized" lang="en">Source TRFN structure</p>
    "! @parameter er_res_str_trfn | <p class="shorttext synchronized" lang="en">Result TRFN structure</p>
    "! @parameter er_src_tab_trfn | <p class="shorttext synchronized" lang="en">Source TRFN table</p>
    "! @parameter er_source_table | <p class="shorttext synchronized" lang="en">User source table</p>
    "! @parameter er_result_table | <p class="shorttext synchronized" lang="en">User result table to compare</p>
    METHODS create_data_references
      IMPORTING iv_source_ddic_table TYPE string
                iv_result_ddic_table TYPE string
      EXPORTING er_src_str_trfn      TYPE REF TO data
                er_res_str_trfn      TYPE REF TO data
                er_src_tab_trfn      TYPE REF TO data
                er_source_table      TYPE REF TO data
                er_result_table      TYPE REF TO data.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter ir_trfn_result | <p class="shorttext synchronized" lang="en">TRFN result data</p>
    "! @parameter ir_user_result | <p class="shorttext synchronized" lang="en">User result data</p>
    "! @parameter iv_user_table_name | <p class="shorttext synchronized" lang="en">User result DDIC table name</p>
    METHODS compare_trfn_to_user_table
      IMPORTING ir_trfn_result     TYPE REF TO cl_rsbk_data_segment
                ir_user_result     TYPE REF TO data
                iv_user_table_name TYPE string.

    "! <p class="shorttext synchronized" lang="en"></p>
    "! Create type based on basic description
    "! @parameter iv_intype | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_leng | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_decim | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_type | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS create_type
      IMPORTING iv_intype      TYPE string
                iv_leng        TYPE ddleng
                iv_decim       TYPE decimals
      RETURNING VALUE(rv_type) TYPE REF TO cl_abap_datadescr.
  PROTECTED SECTION.

    DATA: gv_trfnid TYPE rstranid,
          gv_dtp    TYPE rsbkdtpnm.

  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_bw_trfn_tester_ui DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en"></p>
      "! Create user data package
      "! @parameter iv_stemp | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter iv_svnam | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter iv_repid | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter iv_type | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter er_data_pkg | <p class="shorttext synchronized" lang="en"></p>
      create_data_package
        IMPORTING iv_stemp           TYPE tabname
                  iv_svnam           TYPE char20
                  iv_repid           TYPE sy-repid
                  iv_type            TYPE char3
        RETURNING VALUE(er_data_pkg) TYPE REF TO data.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en"></p>
      "! Load saved data from database
      "! @parameter iv_type | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter iv_svnam | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter iv_repid | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter er_data_pkg | <p class="shorttext synchronized" lang="en"></p>
      load_variant
        IMPORTING iv_type            TYPE char3
                  iv_svnam           TYPE char20
                  iv_repid           TYPE sy-repid
        RETURNING VALUE(er_data_pkg) TYPE REF TO data.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_bw_trfn_tester_ui IMPLEMENTATION.

  METHOD create_data_package.

    TYPES: BEGIN OF ty_type_input,
             input_name    TYPE string,
             input_length  TYPE ddleng,
             input_dec     TYPE decimals,
             input_inntype TYPE inttype.
    TYPES: END OF ty_type_input.

    DATA: lt_type_result   TYPE STANDARD TABLE OF ty_type_input,
          lt_fcat          TYPE slis_t_fieldcat_alv,
          ls_comp          TYPE cl_abap_structdescr=>component,
          lt_comp          TYPE cl_abap_structdescr=>component_table,
          lt_data_fcat_src TYPE slis_t_fieldcat_alv,
          lr_data_src      TYPE REF TO data.

    FIELD-SYMBOLS:
        <lt_data_type> TYPE STANDARD TABLE.

    CLEAR: lt_type_result,lt_fcat,ls_comp,lt_comp,lt_data_fcat_src.

    FREE: lr_data_src.

    UNASSIGN <lt_data_type>.
    IF iv_stemp IS NOT INITIAL.
      SELECT fieldname, rollname, leng, decimals,inttype
      FROM dd03l
      INTO TABLE @DATA(lt_dd03l)
      WHERE as4local = 'A'
      AND tabname = @iv_stemp.
      IF sy-subrc <> 0.
        MESSAGE 'Error during type select' TYPE 'E'.
      ENDIF.

      lt_type_result = CORRESPONDING #(  lt_dd03l MAPPING input_name = fieldname
                                                          input_inntype = inttype
                                                          input_length = leng
                                                          input_dec   = decimals ).
    ELSE.

      DO 10 TIMES.
        lt_type_result = VALUE #( BASE lt_type_result (  input_name = '' ) ).
      ENDDO.
    ENDIF.
    lt_fcat = VALUE slis_t_fieldcat_alv(
    ( seltext_m = 'Name'     fieldname = 'INPUT_NAME'     outputlen = '20' edit = abap_true )
    ( seltext_m = 'Type'     fieldname = 'INPUT_INNTYPE'  outputlen = '5' edit = abap_true )
    ( seltext_m = 'Lenght'   fieldname = 'INPUT_LENGTH'   outputlen = '5' edit = abap_true )
    ( seltext_m = 'Decimals' fieldname = 'INPUT_DEC'      outputlen = '5' edit = abap_true ) ).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = iv_repid
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

    DELETE lt_type_result WHERE input_name IS INITIAL.

    CHECK lt_type_result IS NOT INITIAL.

    LOOP AT lt_type_result REFERENCE INTO DATA(lr_type_input).
      ls_comp-name =  lr_type_input->input_name.
      ls_comp-type = zcl_bw_trfn_tester=>create_type(
                       iv_intype = CONV #( lr_type_input->input_inntype )
                       iv_leng   = lr_type_input->input_length
                       iv_decim  = lr_type_input->input_dec
                     ).

      APPEND ls_comp TO lt_comp.

      lt_data_fcat_src = VALUE #(
      BASE lt_data_fcat_src (
      seltext_m = lr_type_input->input_name
      fieldname = to_upper( lr_type_input->input_name )
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
        i_callback_program      = iv_repid
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

    LOOP AT <lt_data_type> ASSIGNING FIELD-SYMBOL(<ls_data>).
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_data>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF <lv_data> IS NOT INITIAL.
          DATA(lv_not_clear_line) = abap_true.
        ENDIF.
      ENDDO.
      IF lv_not_clear_line = abap_false.
        DELETE <lt_data_type>.
      ENDIF.
      CLEAR lv_not_clear_line.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM <lt_data_type> COMPARING ALL FIELDS.

    er_data_pkg = lr_data_src.

    IF iv_svnam IS NOT INITIAL.

      DATA: lt_var_tab  TYPE STANDARD TABLE OF zbwtrfn_var_type,
            ls_var_tab  TYPE zbwtrfn_var_type,
            lt_data_src TYPE STANDARD TABLE OF zbwtrfn_var_data,
            ls_data_src TYPE zbwtrfn_var_data.

      LOOP AT lt_type_result REFERENCE INTO DATA(lr_type_res_src).

        ls_var_tab-template_table = iv_stemp.
        ls_var_tab-variant = iv_svnam.
        ls_var_tab-data_type = iv_type.
        ls_var_tab-type = lr_type_res_src->input_inntype.
        ls_var_tab-fieldnm = lr_type_res_src->input_name.
        ls_var_tab-length =  lr_type_res_src->input_length.
        ls_var_tab-decim =   lr_type_res_src->input_dec.

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
            ls_data_src-template_table = iv_stemp.
            ls_data_src-variant = iv_svnam.
            ls_data_src-data_type = iv_type.
            ls_data_src-fieldnm = lr_type_res_src->input_name.
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

  ENDMETHOD.

  METHOD load_variant.

    TYPES: BEGIN OF ty_type_input,
             input_type_name TYPE string,
             input_type      TYPE string.
    TYPES: END OF ty_type_input.

    DATA: lt_type_result   TYPE STANDARD TABLE OF ty_type_input,
          lt_fcat          TYPE slis_t_fieldcat_alv,
          ls_comp          TYPE  cl_abap_structdescr=>component,
          lt_comp          TYPE cl_abap_structdescr=>component_table,
          lt_data_fcat_src TYPE  slis_t_fieldcat_alv,
          lr_data_src      TYPE REF TO data.

    FIELD-SYMBOLS:
        <lt_data_type> TYPE STANDARD TABLE.

    CLEAR: lt_type_result,lt_fcat,ls_comp,lt_comp,lt_data_fcat_src.

    FREE: lr_data_src.

    UNASSIGN <lt_data_type>.

    DATA: lr_saved_str TYPE REF TO data.

    FIELD-SYMBOLS: <ls_saved_data> TYPE any.
    SELECT * FROM
    zbwtrfn_var_type
    INTO TABLE @DATA(lt_var_type)
    WHERE variant = @iv_svnam
    AND data_type = @iv_type.
    IF sy-subrc <> 0.
      MESSAGE 'Error during select' TYPE 'E'.
    ENDIF.

    LOOP AT lt_var_type REFERENCE INTO DATA(lr_var_type).
      ls_comp-name =  lr_var_type->fieldnm.
      ls_comp-type = zcl_bw_trfn_tester=>create_type(
                       iv_intype = CONV #( lr_var_type->type )
                       iv_leng   = lr_var_type->length
                       iv_decim  = lr_var_type->decim
                     ) .
      APPEND ls_comp TO lt_comp.
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
    WHERE variant = @iv_svnam
    AND data_type = @iv_type.
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
          lt_data_fcat_src = VALUE #(
           BASE lt_data_fcat_src (
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
        i_callback_program      = iv_repid
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

    er_data_pkg = lr_data_src.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_bw_trfn_tester IMPLEMENTATION.
  METHOD constructor.

    SELECT SINGLE tranid
    FROM rstran
    INTO @DATA(lv_trfn_id)
    WHERE sourcename = @iv_strfn
    AND targetname = @iv_ttrfn
    AND objvers = 'A'
    AND objstat = 'ACT'.

    IF sy-subrc = 0.
      gv_trfnid = lv_trfn_id.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bw_trfn_tester.
    ENDIF.

    SELECT SINGLE dtp
    FROM rsbkdtp
    WHERE src = @iv_strfn
    AND tgt = @iv_ttrfn
    INTO @DATA(lv_dtp).

    IF sy-subrc = 0.
      gv_dtp = lv_dtp.
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
      CATCH cx_rstran_display_only.
      CATCH cx_rstran_error_with_message.
    ENDTRY.

    lobj_rstran_miantain->get_progid( IMPORTING e_progid = DATA(lv_progid) ).

    rv_progid = |GP{ lv_progid }|.

  ENDMETHOD.

  METHOD test_new_scenario.

    DATA: lobj_trfn_prog  TYPE REF TO object.

    FIELD-SYMBOLS: <lt_source_table> TYPE STANDARD TABLE,
                   <lt_result_table> TYPE STANDARD TABLE,
                   <ls_trfn_source>  TYPE any,
                   <lt_trfn_source>  TYPE STANDARD TABLE,
                   <ls_trfn_result>  TYPE any.

    create_data_references(
      EXPORTING
        iv_source_ddic_table = iv_source_ddic_table
        iv_result_ddic_table = iv_result_ddic_table
      IMPORTING
        er_src_str_trfn      = DATA(lr_src_str_trfn)
        er_res_str_trfn      = DATA(lr_res_str_trfn)
        er_src_tab_trfn      = DATA(lr_src_tab_trfn)
        er_source_table      = DATA(lr_source_table)
        er_result_table      = DATA(lr_result_table)
    ).

    IF iv_source_ddic_table IS NOT INITIAL.
      ASSIGN lr_source_table->* TO <lt_source_table>.
      IF sy-subrc = 0.
        get_data_from_table(
           EXPORTING
            iv_table_name = iv_source_ddic_table
           IMPORTING
            et_table      = <lt_source_table>
        ).
      ENDIF.
    ELSE.
      ASSIGN  ir_source_user_table->* TO <lt_source_table>.
      IF sy-subrc <> 0.
        MESSAGE 'Error during assign' TYPE 'E'.
      ENDIF.
    ENDIF.

    IF iv_result_ddic_table IS NOT INITIAL.
      ASSIGN lr_result_table->* TO <lt_result_table>.
      IF sy-subrc = 0.
        get_data_from_table(
           EXPORTING
            iv_table_name = iv_result_ddic_table
           IMPORTING
            et_table      = <lt_result_table>
        ).
      ENDIF.
    ELSE.
      ASSIGN  ir_result_user_table->* TO <lt_result_table>.
      IF sy-subrc <> 0.
        MESSAGE 'Error during assign' TYPE 'E'.
      ENDIF.
    ENDIF.

    ASSIGN lr_src_str_trfn->* TO <ls_trfn_source>.
    IF sy-subrc <> 0.
      MESSAGE 'Error during assign' TYPE 'E'.
    ENDIF.

    ASSIGN lr_res_str_trfn->* TO <ls_trfn_result>.
    IF sy-subrc <> 0.
      MESSAGE 'Error during assign' TYPE 'E'.
    ENDIF.

    ASSIGN lr_src_tab_trfn->* TO <lt_trfn_source>.
    IF sy-subrc <> 0.
      MESSAGE 'Error during assign' TYPE 'E'.
    ENDIF.

    <lt_trfn_source> = CORRESPONDING #( <lt_source_table> ).

    DATA(lro_inbound)  = NEW cl_rsbk_data( ).
    DATA(lro_outbound) = NEW cl_rsbk_data( ).

    lro_inbound->add_segment_from_reference(
      EXPORTING
        i_r_s_data  = lr_src_str_trfn
      RECEIVING
        r_r_segment = DATA(lro_segment_inbound)
    ).

    lro_outbound->add_segment_from_reference( i_r_s_data  = lr_res_str_trfn ).

    lro_segment_inbound->insert_table( i_r_t_data = lr_src_tab_trfn ).

    DATA(lro_log) = create_log( ).
    DATA(lv_program_id) = caluclate_program_id( ).

    CONCATENATE '\PROGRAM=' lv_program_id '\CLASS=LCL_TRANSFORM' INTO DATA(lv_trfn_prog).
    CREATE OBJECT lobj_trfn_prog TYPE (lv_trfn_prog).

    CALL METHOD lobj_trfn_prog->('EXECUTE')
      EXPORTING
        i_master_data_exist = ''
        i_r_inbound         = lro_inbound
        i_r_log             = lro_log
*       i_r_request         = l_r_request
*       i_r_trfn_cmd        = me
      IMPORTING
        e_r_outbound        = lro_outbound.
    TRY.
        DATA(lro_segment_outbound) = lro_outbound->get_segment( i_segid = 001 ).
      CATCH cx_rs_not_found.
        MESSAGE 'Outbound segment do not exist' TYPE 'E'.
    ENDTRY.

    compare_trfn_to_user_table(
        ir_trfn_result = lro_segment_outbound
        ir_user_result = ir_result_user_table
        iv_user_table_name = iv_result_ddic_table
    ).

  ENDMETHOD.

  METHOD get_data_from_table.

    SELECT *             ##SUBRC_OK
    FROM (iv_table_name)
    INTO TABLE @et_table
    UP TO 50000 ROWS.

  ENDMETHOD.

  METHOD create_log.

    DATA: lobj_dtp TYPE REF TO cl_rsbk_dtp.

    DATA(lro_dp_log) = NEW cl_rsbm_log_dtp_dp(
       i_requid    = ''
       i_datapakid = 1
       i_no_db     =  ''
       i_runid     =  1
     ).

    lobj_dtp = cl_rsbk_dtp=>factory( gv_dtp ).

    TRY.
        lobj_dtp->if_rsbk_dtp_maintain~set_simulation( i_simulation = abap_true ).
      CATCH cx_rs_failed.
        MESSAGE 'Set DTP to simmulation mode failed' TYPE 'E'.
    ENDTRY.

    TRY.
        DATA(lr_request) = lobj_dtp->create_request( ).
      CATCH cx_rs_not_found.
        MESSAGE 'Request not found' TYPE 'E'.
      CATCH cx_rs_foreign_lock.
        MESSAGE 'Foregin lock' TYPE 'E'.
      CATCH cx_rs_failed.
        MESSAGE 'Create request failed' TYPE 'E'.
    ENDTRY.

    TRY.
        lro_dp_log->create_dp(
          EXPORTING
            i_r_request = lr_request
            i_datapakid = 1
            i_runid     = 1
            i_no_db     = abap_true
          RECEIVING
            r_ref_dplog = lro_dp_log
        ).
      CATCH cx_rs_existing.
        MESSAGE 'Data package exists' TYPE 'E'.
      CATCH cx_rs_para_not_exist.
        MESSAGE 'Parameter do not exist' TYPE 'E'.
      CATCH cx_rs_foreign_lock.
        MESSAGE 'Foregin lock' TYPE 'E'.
    ENDTRY.

    TRY.
        cl_rsbm_error_handler=>factory(
          EXPORTING
            i_request        = lr_request->get_requid( )
            i_no_monitor     = lr_request->get_no_monitor( )
            i_errorhandling  = lr_request->get_errorhandling( )
            i_number_at_err  = lr_request->get_number_at_err( )
            i_t_step         = lr_request->get_t_stepid( )
          RECEIVING
            r_r_errorhandler = DATA(lr_error_hanlder)
        ).
      CATCH cx_rs_access_error.
        MESSAGE 'Access error during error handler factory' TYPE 'E'.
    ENDTRY.

    rr_log = NEW cl_rsbm_log_cursor_step_dtp(
      i_r_errorlog = lr_error_hanlder
      i_stepid     = '2-'
      i_r_dp_log   = lro_dp_log
      i_requid     =  '3'
    ).

  ENDMETHOD.

  METHOD create_data_references.

    DATA(lv_programid) = caluclate_program_id( ).

    CONCATENATE '\PROGRAM=' lv_programid '\CLASS=LCL_TRANSFORM\TYPE=_TY_S_SC_1' INTO DATA(lv_src_trfn_str_type).
    CONCATENATE '\PROGRAM=' lv_programid '\CLASS=LCL_TRANSFORM\TYPE=_TY_S_TG_1' INTO DATA(lv_res_trfn_str_type).
    CONCATENATE '\PROGRAM=' lv_programid '\CLASS=LCL_TRANSFORM\TYPE=_TY_T_SC_1' INTO DATA(lv_src_trfn_tab_type).

    CREATE DATA er_src_str_trfn TYPE (lv_src_trfn_str_type).
    CREATE DATA er_res_str_trfn TYPE (lv_res_trfn_str_type).
    CREATE DATA er_src_tab_trfn TYPE (lv_src_trfn_tab_type).

    IF iv_source_ddic_table IS NOT INITIAL.
      CREATE DATA er_source_table TYPE STANDARD TABLE OF (iv_source_ddic_table).
    ENDIF.
    IF iv_result_ddic_table IS NOT INITIAL.
      CREATE DATA er_result_table TYPE STANDARD TABLE OF (iv_result_ddic_table).
    ENDIF.

  ENDMETHOD.

  METHOD compare_trfn_to_user_table.

    DATA: lv_where           TYPE string,
          lv_no_changes      TYPE flag,
          lr_trfn_no_tech    TYPE REF TO data,
          lr_user_result     TYPE REF TO data,
          ls_comp            TYPE  cl_abap_structdescr=>component,
          lt_comp            TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <lt_user_result>  TYPE STANDARD TABLE,
                   <lt_no_tech_trfn> TYPE STANDARD TABLE,
                   <lt_trfn_result>  TYPE STANDARD TABLE.

    DATA(lt_component) = ir_trfn_result->get_fieldlist( i_with_recno = rs_c_false ).

    LOOP AT lt_component REFERENCE INTO DATA(lr_component) WHERE fieldname <> 'SID' AND
                                                           fieldname <> 'DATAPAKID' AND
                                                           fieldname <> 'RECORD' AND
                                                           fieldname <> 'REQTSN'.
      ls_comp-name = lr_component->fieldname.
      ls_comp-type = create_type(
                       iv_intype = CONV #( lr_component->inttype )
                       iv_leng   = lr_component->leng
                       iv_decim  = lr_component->decimals
                     ).
      APPEND ls_comp TO lt_comp.

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

    CREATE DATA: lr_trfn_no_tech TYPE HANDLE lr_type_table.
    ASSIGN lr_trfn_no_tech->* TO <lt_no_tech_trfn>.
    IF sy-subrc <> 0.
      MESSAGE 'Error during assign' TYPE 'E'.
    ENDIF.

    DATA(lr_trfn_result) = ir_trfn_result->get_data( ).

    ASSIGN lr_trfn_result->* TO <lt_trfn_result>.
    IF sy-subrc <> 0.
      MESSAGE 'Error during assign' TYPE 'E'.
    ENDIF.

    MOVE-CORRESPONDING <lt_trfn_result> TO <lt_no_tech_trfn>.

    IF iv_user_table_name IS NOT INITIAL.

      SELECT fieldname, keyflag
      FROM dd03l
      INTO TABLE @DATA(lt_dd03l)
      WHERE tabname = @iv_user_table_name.

      IF sy-subrc <> 0.
        MESSAGE 'No fields found for result table' TYPE 'E'.
      ENDIF.

      CREATE DATA lr_user_result TYPE STANDARD TABLE OF (iv_user_table_name).
      ASSIGN lr_user_result->* TO <lt_user_result>.

      IF sy-subrc <> 0.
        MESSAGE 'Error during assign' TYPE 'E'.
      ENDIF.

      LOOP AT lt_dd03l REFERENCE INTO DATA(lr_dd03l) WHERE keyflag = abap_true.

        IF lv_where IS INITIAL.
          lv_where = |{ lr_dd03l->fieldname } = @<lt_trfn_result>-{  lr_dd03l->fieldname } |.
        ELSE.
          lv_where = |{ lv_where } and { lr_dd03l->fieldname } = @<lt_trfn_result>-{ lr_dd03l->fieldname }|.
        ENDIF.

      ENDLOOP.
      TRY.
          SELECT * FROM
          (iv_user_table_name)
          FOR ALL ENTRIES IN @<lt_trfn_result>
          WHERE (lv_where)
          INTO TABLE @<lt_user_result>.
          IF sy-subrc <> 0.
            MESSAGE 'Error during  select' TYPE 'E'.
          ENDIF.
        CATCH cx_sy_dynamic_osql_semantics.
          MESSAGE ' Error during result table check' TYPE 'E'.
      ENDTRY.

    ELSE.

      ASSIGN ir_user_result->* TO <lt_user_result>.
      IF sy-subrc <> 0.
        MESSAGE 'Error during assign' TYPE 'E'.
      ENDIF.

    ENDIF.

    SORT <lt_user_result>.
    SORT <lt_no_tech_trfn>.

    CALL FUNCTION 'CTVB_COMPARE_TABLES_3'
      EXPORTING
        it_table_old  = <lt_user_result>
        it_table_new  = <lt_no_tech_trfn>
        iv_key_count  = lines( lt_dd03l )
*       iv_key_table  =
      IMPORTING
*       et_table_del  =
*       et_table_add  =
*       et_table_mod  =
        ev_no_changes = lv_no_changes.

    IF lv_no_changes = abap_true.
      MESSAGE 'Result is compatible with table data' TYPE 'S'.
    ELSE.
      MESSAGE 'Tables are different' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.

  ENDMETHOD.

  METHOD create_type.

    rv_type = COND #(
        WHEN iv_intype = 'STRING'  THEN cl_abap_elemdescr=>get_string( )
        WHEN iv_intype = 'XSTRING' THEN cl_abap_elemdescr=>get_xstring( )
        WHEN iv_intype = 'I' THEN cl_abap_elemdescr=>get_i( )
        WHEN iv_intype = 'F' THEN cl_abap_elemdescr=>get_f( )
        WHEN iv_intype = 'D' THEN cl_abap_elemdescr=>get_d( )
        WHEN iv_intype = 'T' THEN cl_abap_elemdescr=>get_t( )
        WHEN iv_intype = 'C' THEN cl_abap_elemdescr=>get_c( p_length = CONV #( iv_leng ) )
        WHEN iv_intype = 'N' THEN cl_abap_elemdescr=>get_n( p_length = CONV #( iv_leng  ) )
        WHEN iv_intype = 'X' THEN cl_abap_elemdescr=>get_x( p_length = CONV #( iv_leng  ) )
        WHEN iv_intype = 'P' THEN cl_abap_elemdescr=>get_p( p_length = CONV #( iv_leng  )
                                                          p_decimals = CONV #( iv_decim ) ) ).

  ENDMETHOD.

ENDCLASS.

TABLES sscrfields.

TYPES: BEGIN OF ty_type_input,
         input_type_name TYPE string,
         input_type      TYPE string.
TYPES: END OF ty_type_input.

DATA: lr_data_src TYPE REF TO data,
      lr_data_res TYPE REF TO data.

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
    pa_rtabl RADIOBUTTON GROUP rgr3 MODIF ID g3 USER-COMMAND uc3 DEFAULT 'X',
    pa_rownd RADIOBUTTON GROUP rgr3 MODIF ID g3,
    pa_lrdat RADIOBUTTON GROUP rgr3 MODIF ID g3.

  PARAMETERS: pa_rtabn TYPE dd02l-tabname MODIF ID g14.
  PARAMETERS: pa_rtemp TYPE dd02l-tabname MODIF ID g15.
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
           iv_repid    = sy-repid
           iv_type     = 'SRC' ).

    WHEN  'CREATE_RESULT'.
      lr_data_res = zcl_bw_trfn_tester_ui=>create_data_package(
          iv_stemp    = pa_rtemp
          iv_svnam    = pa_rvnam
          iv_repid    = sy-repid
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
            OR screen-group1 = 'G19'.
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

    IF pa_lsdat = abap_true.
      IF screen-group1 = 'G16'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G13' OR
      screen-group1 = 'G12'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF pa_lrdat = abap_true.
      IF screen-group1 = 'G18'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G14' OR
      screen-group1 = 'G15'.
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

    lobj_trfn_tester->test_new_scenario(
        iv_source_ddic_table = CONV #( pa_stabn )
        iv_result_ddic_table = CONV #( pa_rtabn )
        ir_source_user_table = lr_data_src
        ir_result_user_table = lr_data_res
    ).

  ENDIF.

  CHECK pa_ctrfn IS INITIAL.

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.14.3 - 2021-04-10T16:11:49.223Z
ENDINTERFACE.
****************************************************