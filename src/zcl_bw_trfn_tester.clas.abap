CLASS zcl_bw_trfn_tester DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS run_abap_processing.

    CLASS-METHODS run_amdp_porcessing.

    CLASS-METHODS check_trfn_type
      IMPORTING iv_trfnid         TYPE rstranid
      RETURNING VALUE(rv_is_amdp) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter pa_trfnid | <p class="shorttext synchronized" lang="en">Transformation ID</p>
    "! @raising zcx_bw_trfn_tester | <p class="shorttext synchronized" lang="en">Invalid TRFN ID</p>
    METHODS constructor
      IMPORTING iv_strfn TYPE sobj_name
                iv_ttrfn TYPE sobj_name
      RAISING   zcx_bw_trfn_tester.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gv_trfnid TYPE rstranid,
          gv_dtp    TYPE rsbkdtpnm,
          gv_amdp   TYPE boolean.

ENDCLASS.



CLASS zcl_bw_trfn_tester IMPLEMENTATION.

  METHOD check_trfn_type.

    DATA(lobj_msg) = NEW cl_rso_msg(  ).

    DATA(lt_tlogo) = VALUE rstran_t_tlogo( ( tlogo = 'TRFN' objnm = iv_trfnid ) ).

    DATA(lv_realized) = cl_rstran_db_stat=>clarify_realization(
       EXPORTING
         i_t_path                    = lt_tlogo
         i_r_log                     = lobj_msg ).

    IF lv_realized = abap_true.
      rv_is_amdp = abap_true.
    ENDIF.


  ENDMETHOD.

  METHOD run_abap_processing.



  ENDMETHOD.

  METHOD run_amdp_porcessing.

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


  ENDMETHOD.

ENDCLASS.
