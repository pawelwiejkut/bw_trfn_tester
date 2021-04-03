CLASS ltcl_ DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA gobj_test TYPE REF TO zcl_bw_trfn_tester.

    METHODS: setup.
    METHODS:
      caluclate_program_id_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_ IMPLEMENTATION.

  METHOD caluclate_program_id_test.

    DATA(lv_programid) = 'GP04TFNXDAAVG3ZA8ISGWUZELWI'.

    DATA(lv_rec_progid) = gobj_test->caluclate_program_id( ).

    CL_ABAP_UNIT_ASSERT=>assert_equals(
      EXPORTING
        act                  = lv_programid
        exp                  = lv_rec_progid ).

  ENDMETHOD.

  METHOD setup.

    gobj_test = new #( pa_trfnid = '0AF4GR51GOQ3SJNAB3T5A44I9YKCL0AY' ).

  endmethod.

ENDCLASS.
