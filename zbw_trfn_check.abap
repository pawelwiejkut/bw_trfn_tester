*&---------------------------------------------------------------------*
*& Report /sgd/bi_trfn_perf_check
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sgd/bi_trfn_perf_check.

PARAMETERS:
  pa_oprg TYPE string OBLIGATORY,
  pa_psa  TYPE string OBLIGATORY,
  pa_mul  TYPE i OBLIGATORY DEFAULT 1,
  pa_upto TYPE i OBLIGATORY,
  pa_ncls TYPE string OBLIGATORY,
  pa_rtyp TYPE string OBLIGATORY DEFAULT 'START_ROUTINE',
  pa_amdp AS CHECKBOX,
  pa_pkgs TYPE i OBLIGATORY DEFAULT 50000,
  r_pchec RADIOBUTTON GROUP rad1 DEFAULT 'X',
  r_lchec RADIOBUTTON GROUP rad1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(60) com1.
SELECTION-SCREEN END OF LINE.

AT SELECTION-SCREEN OUTPUT.

  DATA: lf_tab_cnt TYPE i.

  LOOP AT SCREEN.
    IF screen-name = 'PA_RTYP'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  IF pa_psa IS NOT INITIAL.
    SELECT COUNT(*) FROM (pa_psa) INTO (lf_tab_cnt) .
    com1 =  |Total initial table size is { lf_tab_cnt * pa_mul }, hit enter to refresh|.
  ENDIF.

END-OF-SELECTION.

  DATA: lr_psa            TYPE REF TO data,
        lobj_prog_old     TYPE REF TO object,
        lobj_clas_new     TYPE REF TO object,
        lr_result         TYPE REF TO data,
        lr_result_line    TYPE REF TO data,
        lr_result_package TYPE REF TO data,
        lr_result_pkg_new TYPE REF TO data,
        lf_counter        TYPE i,
        lf_pkg_cnt        TYPE i,
        lf_percent        TYPE p.

  FIELD-SYMBOLS: <lt_result>         TYPE ANY TABLE,
                 <lt_result_package> TYPE ANY TABLE,
                 <lt_result_pkg_new> TYPE ANY TABLE,
                 <lt_psa>            TYPE ANY TABLE,
                 <ls_line>           TYPE any.

  CREATE DATA lr_psa TYPE STANDARD TABLE OF (pa_psa).
  ASSIGN lr_psa->* TO <lt_psa>.

  DO pa_mul TIMES.
    "Get Data from PSA
    SELECT * FROM (pa_psa)
    APPENDING CORRESPONDING FIELDS OF TABLE @<lt_psa>
    UP TO @pa_upto ROWS
    ORDER BY request,datapakid,partno,record.
  ENDDO.

  "Create start routine table - old
  CONCATENATE '\PROGRAM=' pa_oprg '\CLASS=LCL_TRANSFORM\TYPE=_TY_T_SC_1' INTO DATA(lf_src_type).
  CONCATENATE '\PROGRAM=' pa_oprg '\CLASS=LCL_TRANSFORM\TYPE=_TY_S_SC_1' INTO DATA(lf_src_str).

  CREATE DATA lr_result TYPE (lf_src_type).
  ASSIGN lr_result->* TO <lt_result>.
  <lt_result> =  CORRESPONDING #( <lt_psa> ).
  "Create start routine object - old
  CONCATENATE '\PROGRAM=' pa_oprg '\CLASS=LCL_TRANSFORM' INTO DATA(lf_prg_lcl_type).

  CREATE DATA lr_result_package TYPE (lf_src_type).
  ASSIGN lr_result_package->* TO <lt_result_package>.

  CREATE DATA lr_result_pkg_new TYPE (lf_src_type).
  ASSIGN lr_result_pkg_new->* TO <lt_result_pkg_new>.

  CREATE DATA lr_result_line TYPE (lf_src_str).
  ASSIGN lr_result_line->* TO <ls_line>.

**********************************************************************
*Performance check
**********************************************************************
  IF r_pchec = abap_true.

    "Test old solution
    cl_abap_trace_switch=>on(
    p_prog   =  '/sgd/bi_trfn_perf_check'
    p_comm   = 'Old solution' ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    SET RUN TIME ANALYZER ON.
    CREATE OBJECT lobj_prog_old TYPE (lf_prg_lcl_type).
    LOOP AT <lt_result> ASSIGNING FIELD-SYMBOL(<ls_result>).

      <lt_result_package> = VALUE #( BASE <lt_result_package> ( <ls_result> ) ).

      IF lines( <lt_result_package> ) >= pa_pkgs.
        lf_counter = lf_counter + 1.

        CALL METHOD lobj_prog_old->(pa_rtyp)
          EXPORTING
            request        = 'DUMMY'
            datapackid     = '0'
            segid          = '0'
          CHANGING
            source_package = <lt_result_package>.

        lf_pkg_cnt = lf_pkg_cnt + pa_pkgs.

        lf_percent = ( lf_pkg_cnt / lines( <lt_result> ) ) * 100.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lf_percent
            text       = |Testing old solution, package: { lf_counter } |.

        FREE <lt_result_package>.

      ENDIF.
    ENDLOOP.
    "Process last package or initial small one
    IF <lt_result_package> IS NOT INITIAL.

      lf_counter = lf_counter + 1.

      CALL METHOD lobj_prog_old->(pa_rtyp)
        EXPORTING
          request        = 'DUMMY'
          datapackid     = '0'
          segid          = '0'
        CHANGING
          source_package = <lt_result_package>.

      lf_pkg_cnt =  pa_pkgs + lf_pkg_cnt.

      lf_percent = ( lf_pkg_cnt / lines( <lt_result> ) ) * 100.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lf_percent
          text       = |Testing old solution, package: { lf_counter } |.

      FREE: <lt_result_package>, lf_pkg_cnt.

    ENDIF.

    FREE lf_counter.
    SET RUN TIME ANALYZER OFF.
    cl_abap_trace_switch=>off( ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Test new solution
    cl_abap_trace_switch=>on(
    p_prog   =  '/sgd/bi_trfn_perf_check'
    p_comm   = 'New solution' ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET RUN TIME ANALYZER ON.

    CREATE OBJECT lobj_clas_new TYPE (pa_ncls).

    IF pa_amdp = abap_true.
      CALL METHOD lobj_clas_new->('PROCEDURE')
        EXPORTING
          i_error_handling = ''
          intab            = <lt_result>
        IMPORTING
          outtab           = <lt_result_pkg_new>.
    ELSE.

      LOOP AT <lt_result> ASSIGNING <ls_result>.

        <lt_result_package> = VALUE #( BASE <lt_result_package> ( <ls_result> ) ).

        IF lines( <lt_result_package> ) >= pa_pkgs.
          lf_counter = lf_counter + 1.

          CALL METHOD lobj_clas_new->(pa_rtyp)
            EXPORTING
              it_source_package = <lt_result_package>
            IMPORTING
              et_source_package = <lt_result_pkg_new>.
        ENDIF.

        lf_pkg_cnt =  pa_pkgs + lf_pkg_cnt.

        lf_percent = ( lf_pkg_cnt / lines( <lt_result> ) ) * 100.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lf_percent
            text       = |Testing new solution, package: { lf_counter } |.

        FREE: <lt_result_package>, <lt_result_pkg_new>.

      ENDLOOP.
    ENDIF.

    IF <lt_result_package> IS NOT INITIAL and pa_amdp = abap_false.

      lf_counter = lf_counter + 1.

        CALL METHOD lobj_clas_new->(pa_rtyp)
          EXPORTING
            it_source_package = <lt_result_package>
          IMPORTING
            et_source_package = <lt_result_pkg_new>.

      lf_pkg_cnt =  pa_pkgs + lf_pkg_cnt.

      lf_percent = ( lf_pkg_cnt / lines( <lt_result> ) ) * 100.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lf_percent
          text       = |Testing new solution, package: { lf_counter } |.

      FREE: <lt_result_package>, <lt_result_pkg_new>.

    ENDIF.
    SET RUN TIME ANALYZER OFF.
    cl_abap_trace_switch=>off( ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL TRANSACTION 'SAT'.

  ELSE.
**********************************************************************
*Calculations check
**********************************************************************
    CREATE OBJECT lobj_prog_old TYPE (lf_prg_lcl_type).
    CREATE OBJECT lobj_clas_new TYPE (pa_ncls).
    DATA lf_cflag TYPE flag .

    DATA lro_str TYPE REF TO cl_abap_typedescr.
    lro_str = cl_abap_structdescr=>describe_by_data( <ls_line> ).
    DATA(lf_length) = lro_str->length.

    LOOP AT <lt_result> ASSIGNING <ls_result>.

      <lt_result_package> = VALUE #( BASE <lt_result_package> ( <ls_result> ) ).

      IF lines( <lt_result_package> ) >= pa_pkgs.
        lf_counter = lf_counter + 1.

        IF pa_amdp = abap_true.
          CALL METHOD lobj_clas_new->('PROCEDURE')
            EXPORTING
              i_error_handling = ''
              intab            = <lt_result_package>
            IMPORTING
              outtab           = <lt_result_pkg_new>.
        ELSE.
          CALL METHOD lobj_clas_new->(pa_rtyp)
            EXPORTING
              it_source_package = <lt_result_package>
            IMPORTING
              et_source_package = <lt_result_pkg_new>.
        ENDIF.

        CALL METHOD lobj_prog_old->(pa_rtyp)
          EXPORTING
            request        = 'DUMMY'
            datapackid     = '0'
            segid          = '0'
          CHANGING
            source_package = <lt_result_package>.

        CALL FUNCTION 'CTVB_COMPARE_TABLES_3'
          EXPORTING
            it_table_old  = <lt_result_package>
            it_table_new  = <lt_result_pkg_new>
            iv_key_count  = lf_length
          IMPORTING
*           et_table_del  =
*           et_table_add  =
*           et_table_mod  =
            ev_no_changes = lf_cflag.

        IF lf_cflag <> abap_true.
          FREE <lt_result_package>.
          EXIT.
        ENDIF.

        lf_pkg_cnt =  pa_pkgs + lf_pkg_cnt.

        lf_percent = ( lf_pkg_cnt / lines( <lt_result> ) ) * 100.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lf_percent
            text       = |Comparing tables, package: { lf_counter } |.

        FREE: <lt_result_package>, <lt_result_pkg_new>.

      ENDIF.

    ENDLOOP.

    FREE: lf_counter,lf_pkg_cnt.

    IF <lt_result_package> IS NOT INITIAL.

      lf_counter = lf_counter + 1.

      IF pa_amdp = abap_true.
        CALL METHOD lobj_clas_new->('PROCEDURE')
          EXPORTING
            i_error_handling = ''
            intab            = <lt_result_package>
          IMPORTING
            outtab           = <lt_result_pkg_new>.
      ELSE.
        CALL METHOD lobj_clas_new->(pa_rtyp)
          EXPORTING
            it_source_package = <lt_result_package>
          IMPORTING
            et_source_package = <lt_result_pkg_new>.
      ENDIF.

      CALL METHOD lobj_prog_old->(pa_rtyp)
        EXPORTING
          request        = 'DUMMY'
          datapackid     = '0'
          segid          = '0'
        CHANGING
          source_package = <lt_result_package>.

      CALL FUNCTION 'CTVB_COMPARE_TABLES_3'
        EXPORTING
          it_table_old  = <lt_result_package>
          it_table_new  = <lt_result_pkg_new>
          iv_key_count  = lf_length
        IMPORTING
*         et_table_del  =
*         et_table_add  =
*         et_table_mod  =
          ev_no_changes = lf_cflag.

      lf_pkg_cnt =  pa_pkgs + lf_pkg_cnt.

      lf_percent = ( lf_pkg_cnt / lines( <lt_result> ) ) * 100.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lf_percent
          text       = |Testing old solution, package: { lf_counter } |.

      FREE: <lt_result_package>, <lt_result_pkg_new>.

    ENDIF.

    IF lf_cflag = abap_true.
      MESSAGE ' All entries are same' TYPE 'S'.

    ELSE.
      MESSAGE 'Tables not equal' TYPE 'W'.

    ENDIF.

  ENDIF.
