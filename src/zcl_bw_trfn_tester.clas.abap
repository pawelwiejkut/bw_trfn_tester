CLASS zcl_bw_trfn_tester DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING pa_trfnid TYPE rstranid
      RAISING   zcx_bw_trfn_tester.

  PROTECTED SECTION.

    DATA: gv_trfnid TYPE rstranid.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_bw_trfn_tester IMPLEMENTATION.


  METHOD constructor.

    SELECT SINGLE @abap_true  "#EC CI_SUBRC
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

ENDCLASS.
