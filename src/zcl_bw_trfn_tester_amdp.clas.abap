CLASS zcl_bw_trfn_tester_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS execute_trfn.

    METHODS constructor
      IMPORTING iv_strfn TYPE sobj_name
                iv_ttrfn TYPE sobj_name
      RAISING   zcx_bw_trfn_tester.


  PROTECTED SECTION.

    DATA: gv_trfnid TYPE rstranid,
          gv_dtp    TYPE rsbkdtpnm,
          gv_amdp   TYPE boolean.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bw_trfn_tester_amdp IMPLEMENTATION.


  METHOD execute_trfn.

    DATA: lobj_dtp       TYPE REF TO cl_rsbk_dtp,
          lt_trfn        TYPE rsbk_t_transformation,
          lr_log         TYPE REF TO cl_rsbm_log_cursor_process,
          lt_bp          TYPE cl_rsbk_cmd=>th_bp_cmd,
          lt_field       TYPE rsbk_th_field_rt,
          l_s_rsbk_field TYPE rsbk_s_field_rt.

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
        cl_rsdso_dtp_proxy=>if_rstran_target~get_obj_ref(
          EXPORTING
            i_objnm    = CONV #( lobj_dtp->get_tgtnm( ) )
*         i_tlogo    =
*         i_subtype  =
*         i_objvers  = rs_c_objvers-active
          IMPORTING
            e_r_target = DATA(lr_target)
        ).
      CATCH cx_rstran_not_found cx_rstran_not_authorized cx_rstran_not_allowed.
        "handle exception
    ENDTRY.


    TRY.
        lr_target->get_list(
          EXPORTING
            i_objvers        = rs_c_objvers-active
          IMPORTING
            e_t_field        = DATA(lt_tgt_fields)
*    e_t_field_append =
*    e_t_segtxt       =
        ).
      CATCH cx_rstran_error_with_message.
        "handle exception
    ENDTRY.


    LOOP AT lt_tgt_fields REFERENCE INTO DATA(lr_tgt_fields).

      l_s_rsbk_field-fieldname     = lr_tgt_fields->fieldnm.
      l_s_rsbk_field-position      = lr_tgt_fields->posit.
      l_s_rsbk_field-keyflag       = lr_tgt_fields->is_key.
      l_s_rsbk_field-inttype       = lr_tgt_fields->inttype.
      l_s_rsbk_field-intlen        = lr_tgt_fields->intlen.
      l_s_rsbk_field-decimals      = lr_tgt_fields->decimals.
      l_s_rsbk_field-lowercase     = lr_tgt_fields->lowercase.
      l_s_rsbk_field-convexit      = lr_tgt_fields->convexit.
      l_s_rsbk_field-txtlg         = lr_tgt_fields->txtlg.
      l_s_rsbk_field-datatype      = lr_tgt_fields->datatype.
      l_s_rsbk_field-leng          = lr_tgt_fields->leng.
      l_s_rsbk_field-unifieldnm    = lr_tgt_fields->unifieldnm.
      l_s_rsbk_field-aggrgen       = lr_tgt_fields->aggrgen.

      INSERT l_s_rsbk_field INTO TABLE lt_field .

    ENDLOOP.

    DELETE lt_field WHERE fieldname = 'DATAPAKID'.
    DELETE lt_field WHERE fieldname = 'REQTSN'.
    DELETE lt_field WHERE fieldname = 'RECORD'.



    DATA(lro_outbound) = NEW cl_rsbk_data( ).

    lro_outbound->add_segment_from_fieldlist(
      EXPORTING
*        i_segid      =
*        i_txtlg      =
        i_th_field   = lt_field
*        i_unique_key = rs_c_false
*      RECEIVING
*        r_r_segment  =
    ).



    APPEND VALUE #( tf = gv_trfnid ) TO lt_trfn.

    TRY.
        cl_rstran_db_stat=>execute_haap(
          EXPORTING
       i_processing_phase      =  lr_request->if_rsbk_request~get_stage( )-stage_id
            i_target_request        = lr_request->if_rsbk_request~get_requid( )
*      i_r_dp                  =
            i_dtp                   = lobj_dtp->n_dtp
            i_t_trfn                =  lr_request->if_rsbk_request~get_t_transformation( i_only_she_relevant = rs_c_true )
            i_r_log                 = lr_request->if_rsbk_request~get_log( )
            i_simulation            = lr_request->get_simulation( )
*      i_get_number_of_records = rs_c_false
            i_th_bp                 = lt_bp
      i_r_outbound            = lro_outbound
    IMPORTING
      e_r_analysis_rt         = DATA(lr_analysis)
*      e_cursor_closed         =
*      e_rows                  =
        ).
      CATCH cx_rs_step_failed.
      CATCH cx_rsb_no_more_data.
      CATCH cx_rs_not_found.
    ENDTRY.

    TRY.
        DATA(lro_segment_outbound) = lro_outbound->get_segment( i_segid = 001 ).
      CATCH cx_rs_not_found.
        "handle exception
    ENDTRY.


  ENDMETHOD.


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

    gv_amdp = abap_true.

  ENDMETHOD.

ENDCLASS.
