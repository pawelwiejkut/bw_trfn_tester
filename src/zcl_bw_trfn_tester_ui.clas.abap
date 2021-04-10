CLASS zcl_bw_trfn_tester_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS: create_data_package
      IMPORTING iv_stemp    TYPE tabname
                iv_svnam    TYPE char20
                iv_repid    TYPE sy-repid
                iv_type     TYPE char3
      EXPORTING er_data_pkg TYPE REF TO data.

    CLASS-METHODS: load_variant
      IMPORTING iv_type     TYPE char3
                iv_svnam    TYPE char20
                iv_repid    TYPE sy-repid
      EXPORTING er_data_pkg TYPE REF TO data.

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
