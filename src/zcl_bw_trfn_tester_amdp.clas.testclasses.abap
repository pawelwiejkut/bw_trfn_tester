class ltcl_ definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      first_test for testing raising cx_static_check.
endclass.


class ltcl_ implementation.

  method first_test.

    DATA(lobj_amdp) = new zcl_bw_trfn_tester_amdp(
      iv_strfn = 'ADSOAMDP'
      iv_ttrfn = 'ADSOAMDP'
    ).

    lobj_amdp->execute_trfn( ).

*    CATCH zcx_bw_trfn_tester.
  endmethod.

endclass.
