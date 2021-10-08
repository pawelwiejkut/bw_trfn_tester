CLASS zcl_bw_trfn_tester_ui DEFINITION
  PUBLIC
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

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en"></p>
      "! Show table with different values
      "! @parameter ir_user_result | <p class="shorttext synchronized" lang="en"></p>
      "! @parameter ir_trfn_result | <p class="shorttext synchronized" lang="en"></p>
      show_differences
        IMPORTING ir_user_result TYPE REF TO data
                  ir_trfn_result TYPE REF TO data.

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

      DATA: lr_dat_tab TYPE REF TO data,
            lr_dat_str TYPE REF TO data,
            lr_typ_tab TYPE REF TO data,
            lr_typ_str TYPE REF TO data.

      zcl_bw_trfn_tester=>create_global_ddic( ).

      CREATE DATA lr_typ_tab  TYPE STANDARD TABLE OF ('ZBWTRFN_VAR_TYPE').
      CREATE DATA lr_typ_str TYPE ('ZBWTRFN_VAR_TYPE').
      CREATE DATA lr_dat_tab TYPE STANDARD TABLE OF ('ZBWTRFN_VAR_DAT').
      CREATE DATA lr_dat_str TYPE ('ZBWTRFN_VAR_DAT').

      FIELD-SYMBOLS: <lt_typ_tab> TYPE STANDARD TABLE,
                     <ls_typ_tab> TYPE any,
                     <lt_dat_tab> TYPE STANDARD TABLE,
                     <ls_dat_tab> TYPE any.

      ASSIGN lr_typ_tab->* TO <lt_typ_tab>.
      ASSIGN lr_typ_str->* TO <ls_typ_tab>.
      ASSIGN lr_dat_tab->* TO <lt_dat_tab>.
      ASSIGN lr_dat_str->* TO <ls_dat_tab>.

      LOOP AT lt_type_result REFERENCE INTO DATA(lr_type_res_src).

        ASSIGN COMPONENT 'TEMPLATE_TABLE' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_template_table>).
        <lv_template_table> = iv_stemp.
        ASSIGN COMPONENT 'VARIANT' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_variant>).
        <lv_variant> = iv_svnam.
        ASSIGN COMPONENT 'DATA_TYPE' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_data_type>).
        <lv_data_type> = iv_type.
        ASSIGN COMPONENT 'FIELDNM' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_field_name>).
        <lv_field_name> = lr_type_res_src->input_name.
        ASSIGN COMPONENT 'LENGTH' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_length>).
        <lv_length> = lr_type_res_src->input_length.
        ASSIGN COMPONENT 'DECIM' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_decim>).
        <lv_decim> = lr_type_res_src->input_dec.
        ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_type>).
        <lv_type> = lr_type_res_src->input_inntype.

        APPEND <ls_typ_tab>  TO <lt_typ_tab> .

      ENDLOOP.
      TRY.
          INSERT ('ZBWTRFN_VAR_TYPE') FROM TABLE @<lt_typ_tab> .
        CATCH cx_sy_open_sql_db.
          MESSAGE 'Error during variant insert' TYPE 'E'.
      ENDTRY.

      LOOP AT <lt_data_type> ASSIGNING FIELD-SYMBOL(<ls_data_type>).
        DATA(lv_rnr) = sy-tabix.
        DO lines( lt_type_result ) TIMES.
          ASSIGN COMPONENT sy-index OF STRUCTURE <ls_data_type> TO FIELD-SYMBOL(<ls_component>).
          IF sy-subrc <> 0.
            MESSAGE 'Error component assign' TYPE 'E'.
          ENDIF.
          READ TABLE lt_type_result INDEX sy-index REFERENCE INTO lr_type_res_src.

          IF sy-subrc = 0.
            ASSIGN COMPONENT 'ROWNR' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_rownr>).
            <lv_rownr> = lv_rnr.
            ASSIGN COMPONENT 'TEMPLATE_TABLE' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_temp_table>).
            <lv_temp_table> = iv_stemp.
            ASSIGN COMPONENT 'VARIANT' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_variant_d>).
            <lv_variant_d> = iv_svnam.
            ASSIGN COMPONENT 'DATA_TYPE' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_dat_typ>).
            <lv_dat_typ> = iv_type.
            ASSIGN COMPONENT 'FIELDNM' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_FIELDNM>).
            <lv_fieldnm> = lr_type_res_src->input_name.
            ASSIGN COMPONENT 'VALUE' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_VALUE>).
            <lv_value> = <ls_component>.


            APPEND  <ls_dat_tab> TO  <lt_dat_tab>.

          ENDIF.

        ENDDO.

      ENDLOOP.

      INSERT ('ZBWTRFN_VAR_DAT') FROM TABLE @<lt_dat_tab>.
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
          lr_data_src      TYPE REF TO data,
          lr_dat_tab       TYPE REF TO data,
          lr_dat_str       TYPE REF TO data,
          lr_typ_tab       TYPE REF TO data,
          lr_typ_str       TYPE REF TO data.

    zcl_bw_trfn_tester=>create_global_ddic( ).

    CREATE DATA lr_typ_tab  TYPE STANDARD TABLE OF ('ZBWTRFN_VAR_TYPE').
    CREATE DATA lr_typ_str TYPE ('ZBWTRFN_VAR_TYPE').
    CREATE DATA lr_dat_tab TYPE STANDARD TABLE OF ('ZBWTRFN_VAR_DAT').
    CREATE DATA lr_dat_str TYPE ('ZBWTRFN_VAR_DAT').

    FIELD-SYMBOLS: <lt_typ_tab> TYPE STANDARD TABLE,
                   <ls_typ_tab> TYPE any,
                   <lt_dat_tab> TYPE STANDARD TABLE,
                   <ls_dat_tab> TYPE any.

    ASSIGN lr_typ_tab->* TO <lt_typ_tab>.
    ASSIGN lr_typ_str->* TO <ls_typ_tab>.
    ASSIGN lr_dat_tab->* TO <lt_dat_tab>.
    ASSIGN lr_dat_str->* TO <ls_dat_tab>.

    FIELD-SYMBOLS:
        <lt_data_type> TYPE STANDARD TABLE.

    CLEAR: lt_type_result,lt_fcat,ls_comp,lt_comp,lt_data_fcat_src.

    FREE: lr_data_src.

    UNASSIGN <lt_data_type>.

    DATA: lr_saved_str TYPE REF TO data.

    FIELD-SYMBOLS: <ls_saved_data> TYPE any.

    zcl_bw_trfn_tester=>create_global_ddic( ).

    SELECT * FROM
    ('ZBWTRFN_VAR_TYPE')
    INTO TABLE @<lt_typ_tab>
    WHERE variant = @iv_svnam
    AND data_type = @iv_type.
    IF sy-subrc <> 0.
      MESSAGE 'Error during select' TYPE 'E'.
    ENDIF.

    LOOP AT <lt_typ_tab> ASSIGNING <ls_typ_tab>.

      ASSIGN COMPONENT 'FIELDNM' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_field_name>).
      ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_type>).
      ASSIGN COMPONENT 'LENGTH' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_length>).
      ASSIGN COMPONENT 'DECIM' OF STRUCTURE <ls_typ_tab> TO FIELD-SYMBOL(<lv_decim>).

      ls_comp-name =  <lv_field_name>.
      ls_comp-type = zcl_bw_trfn_tester=>create_type(
                       iv_intype = CONV #( <lv_type> )
                       iv_leng   = <lv_length>
                       iv_decim  = <lv_decim>
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
    ('ZBWTRFN_VAR_DAT')
    INTO TABLE @<lt_dat_tab>
    WHERE variant = @iv_svnam
    AND data_type = @iv_type.
    IF sy-subrc <> 0.
      MESSAGE 'Error during variant select' TYPE 'E'.
    ENDIF.

    DATA(lv_groupby) = 'ROWNR'.

    LOOP AT <lt_dat_tab> ASSIGNING FIELD-SYMBOL(<ls_group>) GROUP BY lv_groupby.

      LOOP AT GROUP <ls_group> ASSIGNING <ls_dat_tab>.

        ASSIGN COMPONENT 'FIELDNM' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_fieldnm>).
        ASSIGN COMPONENT 'ROWNR' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_rownr>).
        ASSIGN COMPONENT 'VALUE' OF STRUCTURE <ls_dat_tab> TO FIELD-SYMBOL(<lv_VALUE>).

        ASSIGN COMPONENT <lv_fieldnm> OF STRUCTURE <ls_saved_data> TO FIELD-SYMBOL(<ls_field>).
        IF sy-subrc <> 0.
          MESSAGE 'Error during type select' TYPE 'E'.
        ENDIF.
        <ls_field> = <lv_VALUE>.

        IF <lv_rownr> = 1.
          lt_data_fcat_src = VALUE #(
           BASE lt_data_fcat_src (
           seltext_m = <lv_fieldnm>
           fieldname = to_upper( <lv_fieldnm> )
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

  METHOD show_differences.

    FIELD-SYMBOLS: <lt_user_result> TYPE STANDARD TABLE,
                   <lt_trfn_result> TYPE STANDARD TABLE.

    ASSIGN ir_trfn_result->* TO <lt_trfn_result>.
    ASSIGN ir_user_result->* TO <lt_user_result>.

  ENDMETHOD.

ENDCLASS.
