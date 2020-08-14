*----------------------------------------------------------------------*
***INCLUDE ZBAS_NUEVO_TICKET_F01.
*&---------------------------------------------------------------------*
*&
*& Copyright (C) Lautaro Capella - All Rights Reserved
*& Unauthorized copying of this file, via any medium is strictly prohibited
*& Proprietary and confidential
*& Written by Lautaro Capella <laucape@gmail.com>, Aug 2020
*&
*----------------------------------------------------------------------*
* CLASSES IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_status_hotspot_click. " IMPORTING e_row_id e_column_id.
    READ TABLE gt_status_list INTO DATA(wa_status_click) INDEX e_row_id.
    IF sy-subrc IS INITIAL.
      PERFORM f_nav_to_status USING wa_status_click-bassts.

      PERFORM f_update_ticket_screen_data USING    go_ticket
                                                   go_ticket_sts_alv_grid
                                          CHANGING gs_ticket_screen
                                                   gt_status_list.
      cl_gui_cfw=>set_new_ok_code( '/00' ).
    ENDIF.
  ENDMETHOD.
  METHOD on_zbas_cde_double_click. " IMPORTING  e_row e_column es_row_no sender
    DATA lv_cancelar TYPE c.

    IF sender->is_ready_for_input( ) IS INITIAL.
      MESSAGE 'No se permite el borrado de la línea'
        TYPE 'S'.
      RETURN.
    ENDIF.

    CASE sender.
      WHEN go_zbas_c_alv_grid.
        PERFORM f_popup_tc USING    ''
                                    'Confirmar eliminación.'
                                    'Realmente desea eliminar la línea de la lista de Carga?'
                           CHANGING lv_cancelar.
        CHECK lv_cancelar IS INITIAL.
        READ TABLE gt_zbas_c INTO DATA(wa_zbas_c) INDEX e_row-index.
        IF sy-subrc IS INITIAL.
          go_ticket->borrar_carga( wa_zbas_c-posnr ).
        ENDIF.

      WHEN go_zbas_d_alv_grid.
        PERFORM f_popup_tc USING    ''
                                    'Confirmar eliminación.'
                                    '¿Realmente desea eliminar la línea de la lista de Descarga?'
                           CHANGING lv_cancelar.
        CHECK lv_cancelar IS INITIAL.
        READ TABLE gt_zbas_d INTO DATA(wa_zbas_d) INDEX e_row-index.
        IF sy-subrc IS INITIAL.
          go_ticket->borrar_descarga( wa_zbas_d-posnr ).
        ENDIF.

      WHEN go_zbas_e_alv_grid.
        PERFORM f_popup_tc USING    ''
                                    'Confirmar eliminación.'
                                    '¿Realmente desea eliminar la línea de la lista de Embalajes?'
                           CHANGING lv_cancelar.
        CHECK lv_cancelar IS INITIAL.
        READ TABLE gt_zbas_e INTO DATA(wa_zbas_e) INDEX e_row-index.
        IF sy-subrc IS INITIAL.
          go_ticket->borrar_embalaje( wa_zbas_e-posnr ).
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GC_ALV_CONTAINER_NAME  text
*----------------------------------------------------------------------*
FORM f_init_container  USING    pvi_container_name TYPE scrfname
                       CHANGING poe_container      TYPE REF TO cl_gui_custom_container.

  CREATE OBJECT poe_container
    EXPORTING
      container_name = pvi_container_name.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GO_ALV_CONTAINER  text
*      -->P_GT_STATUS_LIST  text
*      <--P_go_ticket_sts_alv_grid  text
*----------------------------------------------------------------------*
FORM f_init_ticket_sts_alv  USING    poi_alv_container TYPE REF TO cl_gui_custom_container
                            CHANGING pti_status_list   TYPE tyt_status_list
                                     poe_status_grid   TYPE REF TO cl_gui_alv_grid.

  DATA: wa_status_grid_layo TYPE lvc_s_layo,
        wa_status_grid_fcat TYPE lvc_s_fcat,
        lt_status_grid_fcat TYPE lvc_t_fcat,
        lo_event_handler    TYPE REF TO lcl_event_handler.

  wa_status_grid_layo-no_toolbar = 'X'.
  wa_status_grid_layo-info_fname = 'ROW_COLOR'.
  wa_status_grid_layo-grid_title = 'Registro de estatus del Ticket'.
  wa_status_grid_layo-smalltitle = 'X'.
  wa_status_grid_layo-no_vgridln = 'X'.

  wa_status_grid_fcat-key = 'X'.
  wa_status_grid_fcat-outputlen = '12'.
  wa_status_grid_fcat-fieldname = 'BASSTS'.
  APPEND wa_status_grid_fcat TO lt_status_grid_fcat.
  CLEAR wa_status_grid_fcat.

  wa_status_grid_fcat-icon = 'X'.
  wa_status_grid_fcat-hotspot = 'X'.
  wa_status_grid_fcat-fieldname = 'STSICON'.
  APPEND wa_status_grid_fcat TO lt_status_grid_fcat.
  CLEAR wa_status_grid_fcat.

  wa_status_grid_fcat-hotspot = 'X'.
  wa_status_grid_fcat-fieldname = 'STSTXT'.
  APPEND wa_status_grid_fcat TO lt_status_grid_fcat.
  CLEAR wa_status_grid_fcat.

  wa_status_grid_fcat-fieldname = 'ERDAT'.
  APPEND wa_status_grid_fcat TO lt_status_grid_fcat.
  CLEAR wa_status_grid_fcat.

  wa_status_grid_fcat-fieldname = 'ERZET'.
  APPEND wa_status_grid_fcat TO lt_status_grid_fcat.
  CLEAR wa_status_grid_fcat.

  wa_status_grid_fcat-fieldname = 'ERNAM'.
  APPEND wa_status_grid_fcat TO lt_status_grid_fcat.
  CLEAR wa_status_grid_fcat.

  LOOP AT lt_status_grid_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-col_pos = sy-tabix.
    <fs_fcat>-tabname = 'GT_STATUS_LIST'.
    <fs_fcat>-ref_table = 'ZBAS_TICKET_STATUS_TEXT_T'.
  ENDLOOP.

  CREATE OBJECT poe_status_grid
    EXPORTING
      i_parent = poi_alv_container.

  poe_status_grid->set_table_for_first_display(
    EXPORTING
      is_layout        = wa_status_grid_layo
*        i_structure_name = 'ZBAS_TICKET_STATUS_TEXT_T'
    CHANGING
      it_fieldcatalog  = lt_status_grid_fcat
      it_outtab        = pti_status_list ).

  CREATE OBJECT lo_event_handler .
  SET HANDLER lo_event_handler->on_status_hotspot_click FOR poe_status_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ANULAR_TICKET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_TICKET_SCREEN  text
*----------------------------------------------------------------------*
FORM f_anular_ticket USING psi_ticket_screen TYPE tys_ticket_screen.

  DATA: lv_cancelar     TYPE c,
        lv_nuevo_bassts TYPE zbas_s-bassts.

  PERFORM f_popup_tc USING gc_popup_warning
                           'Anulación'
                           '¿Desea anular el ticket? (Esta acción no puede deshacerse)'
                     CHANGING lv_cancelar.
  CHECK lv_cancelar IS INITIAL.

  PERFORM f_guardar_ticket USING psi_ticket_screen.
  lv_nuevo_bassts = go_ticket->anular( ).

  IF lv_nuevo_bassts EQ zcl_bas_ticket=>c_sts_anulado.
    MESSAGE 'El ticket ha sido anulado' TYPE 'S'.
  ENDIF.

  PERFORM f_update_ticket_screen_data USING    go_ticket
                                               go_ticket_sts_alv_grid
                                      CHANGING gs_ticket_screen
                                               gt_status_list.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PASAR_STATUS_SIGUIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_TICKET_SCREEN  text
*----------------------------------------------------------------------*
FORM f_pasar_status_siguiente USING psi_ticket_screen TYPE tys_ticket_screen.

  DATA: lv_nuevo_bassts TYPE zbas_s-bassts.

  PERFORM f_guardar_ticket USING psi_ticket_screen.
  go_ticket->validar_status_completo( EXCEPTIONS status_incomplete = 1
                                                 error_interno     = 2 ).
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Faltan Campos obligatorios para pasar al siguiente estatus.'(e08)
      TYPE 'S'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  lv_nuevo_bassts = go_ticket->pasar_a_status_siguiente( ).

  IF lv_nuevo_bassts IS NOT INITIAL.
    MESSAGE 'Se ha cambiado el estatus del Ticket' TYPE 'S'.
    PERFORM f_set_curr_status USING lv_nuevo_bassts.
  ENDIF.

  PERFORM f_update_ticket_screen_data USING    go_ticket
                                               go_ticket_sts_alv_grid
                                      CHANGING gs_ticket_screen
                                               gt_status_list.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GUARDAR_TICKET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_TICKET_SCREEN  text
*----------------------------------------------------------------------*
FORM f_guardar_ticket USING psi_ticket_screen TYPE tys_ticket_screen.

  DATA: wa_zbas_h           TYPE zbas_h,
        wa_zbas_conductores TYPE zbas_conductores,
        wa_zbas_transportes TYPE zbas_transportes.
**      Header
*         bastk                     TYPE zbas_h-bastk,
*         werks                     TYPE zbas_h-werks,
*         tdlnr                     TYPE zbas_h-tdlnr,
**      Tipo de Operación
*         tipooperacion             TYPE zbas_h-tipooperacion,
*         tipooperacion_descripcion TYPE zbas_tipooper-descripcion,
**      Báscula
*         basid                     TYPE zbas_h-basid,
*         basid_denominacion        TYPE zbas_basculas-denominacion,

  MOVE-CORRESPONDING psi_ticket_screen     TO wa_zbas_h.

  MOVE: psi_ticket_screen-matricula_transp    TO wa_zbas_transportes-matricula,
        psi_ticket_screen-trans_tipo_vehiculo TO wa_zbas_transportes-tipo_vehiculo,
        psi_ticket_screen-tdlnr               TO wa_zbas_transportes-tdlnr,
        psi_ticket_screen-trans_zhabsenasa    TO wa_zbas_transportes-zhabsenasa,
        psi_ticket_screen-trans_zdatumsenasa  TO wa_zbas_transportes-zdatumsenasa.

  MOVE: psi_ticket_screen-dni_conductor      TO wa_zbas_conductores-dni,
        psi_ticket_screen-conductor_nombre   TO wa_zbas_conductores-nombre,
        psi_ticket_screen-conductor_apellido TO wa_zbas_conductores-apellido.

  IF wa_zbas_transportes-tdlnr IS NOT INITIAL
    AND wa_zbas_transportes-matricula IS NOT INITIAL.
    zcl_bas=>set_transporte( wa_zbas_transportes ).
  ENDIF.
  IF wa_zbas_conductores-dni IS NOT INITIAL.
    zcl_bas=>set_conductor( wa_zbas_conductores ).
  ENDIF.
  go_ticket->set_head_data( wa_zbas_h ).
  MESSAGE 'Datos del Ticket guardados' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_TICKET_SCREEN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GO_TICKET  text
*      <--P_GS_TICKET_SCREEN  text
*      <--P_GT_STATUS_LIST  text
*----------------------------------------------------------------------*
FORM f_update_ticket_screen_data  USING    poi_ticket         TYPE REF TO zcl_bas_ticket
                                           poi_status_grid    TYPE REF TO cl_gui_alv_grid
                                  CHANGING pse_ticket_screen  TYPE tys_ticket_screen
                                           pte_status_list    TYPE tyt_status_list.
  DATA: wa_ticket_header TYPE zbas_h,
        wa_status_list   TYPE tys_status_list,
        lt_status_tab    TYPE zbas_ticket_status_text_tab_t.

* Datos de Cabecera del ticket
  wa_ticket_header = poi_ticket->get_head_data( ).

* En el caso de Importar Centro desde otra Trx. (CREAR)
  IF pse_ticket_screen-werks IS NOT INITIAL.
    DATA(lv_save_werks) = pse_ticket_screen-werks.
    MOVE-CORRESPONDING wa_ticket_header TO pse_ticket_screen.
    pse_ticket_screen-werks = lv_save_werks.

* En el caso de Importar Centro desde otra Trx. (VISUAL./TRATAR)
  ELSE.
    MOVE-CORRESPONDING wa_ticket_header TO pse_ticket_screen.
  ENDIF.

* Tabla de Status
  lt_status_tab = poi_ticket->get_status_tab( ).
  CLEAR pte_status_list[].
  LOOP AT lt_status_tab INTO DATA(wa_status_tab).
    MOVE-CORRESPONDING wa_status_tab TO wa_status_list.
    IF gv_current_bassts EQ wa_status_tab-bassts.
      MOVE gc_selected_status_row_color TO wa_status_list-row_color.
    ELSE.
      CLEAR wa_status_list-row_color.
    ENDIF.
    APPEND wa_status_list TO pte_status_list.
  ENDLOOP.

  IF poi_status_grid IS NOT INITIAL.
    poi_status_grid->refresh_table_display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0212   text
*      -->P_0213   text
*      <--P_LV_CANCELAR  text
*----------------------------------------------------------------------*
FORM f_popup_tc  USING    VALUE(pvi_type)
                          pvi_title
                          pvi_msg
                 CHANGING pve_cancel TYPE c.
  DATA: lv_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar      = pvi_title
      text_question = pvi_msg
      text_button_1 = 'Si'
*     icon_button_1 = 'ICON_OKAY'
      text_button_2 = 'No'
*     icon_button_2 = 'ICON_CANCEL'
      popup_type    = pvi_type
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer        = lv_answer.

  IF lv_answer EQ '1'.
    CLEAR pve_cancel.
  ELSE.
    MOVE 'X' TO pve_cancel.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_TICKET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BASTK  text
*      -->P_GV_BLOQUEO  text
*      <--P_GO_TICKET  text
*----------------------------------------------------------------------*
FORM f_cargar_ticket  USING    pvi_bastk   TYPE zbas_h-bastk
                               pvi_bloqueo TYPE enqmode
                      CHANGING poe_ticket  TYPE REF TO zcl_bas_ticket.

  zcl_bas_ticket=>cargar_ticket( EXPORTING
                                   i_bastk            = pvi_bastk
                                   i_bloqueo          = pvi_bloqueo
                                 IMPORTING
                                   e_ticket           = poe_ticket
                                 EXCEPTIONS
                                   error_al_crear     = 1
                                   ticket_inexistente = 2
                                   foreign_lock       = 3
                                   lock_status_error  = 4
                                   OTHERS             = 5 ).
  IF sy-subrc EQ 1.
    MESSAGE 'Se ha producido un error inesperado al cargar el Ticket'(e03) TYPE 'E'.
  ELSEIF sy-subrc EQ 2.
    MESSAGE 'El Ticket solicitado no existe'(e04) TYPE 'I'.
  ELSEIF sy-subrc EQ 3.
    MESSAGE 'El Ticket está siendo tratado por otro usuario'(e05) TYPE 'I'.
  ELSEIF sy-subrc EQ 4.
    MESSAGE 'El Ticket se encuentra Cerrado o Anulado, imposible modificar'(e06) TYPE 'I'.
  ENDIF.

  IF poe_ticket IS NOT INITIAL.
    DATA(lv_curr_sts) = poe_ticket->get_status_actual( ).
    PERFORM f_set_curr_status USING lv_curr_sts.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREAR_TICKET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GO_TICKET  text
*----------------------------------------------------------------------*
FORM f_crear_ticket  CHANGING pvi_werks  TYPE zbas_h-werks
                              poe_ticket TYPE REF TO zcl_bas_ticket.

  zcl_bas_ticket=>nuevo_ticket( IMPORTING
                                  e_ticket       = poe_ticket
                                EXCEPTIONS
                                  error_al_crear = 1
                                  error_de_rango = 2
                                  error_en_db    = 3
                                  OTHERS         = 4 ).
  IF sy-subrc <> 0.
    MESSAGE 'Se ha producido un error inesperado al intentar crear un nuevo Ticket'(e01) TYPE 'E'.
  ENDIF.

  IF poe_ticket IS NOT INITIAL.
    DATA(lv_curr_sts) = poe_ticket->get_status_actual( ).
    PERFORM f_set_curr_status USING lv_curr_sts.

    gs_ticket_screen-werks = pvi_werks.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LOOP_SCREEN_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_loop_screen_pbo .

  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_lectura
    OR gv_active_status_subscr NS sy-dynnr.
    LOOP AT SCREEN.
      IF screen-input = 1
        AND screen-group4 NE 'EXC'. " Excluidos
        screen-input = 0.
      ENDIF.
*      IF screen-active = 1.
*        screen-active = 0.
*      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_BUSCAR_CONDUCTOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_TICKET_SCREEN_DNI_CONDUCTOR  text
*      <--P_GS_TICKET_SCREEN_CONDUCTOR_NOM  text
*      <--P_GS_TICKET_SCREEN_CONDUCTOR_APE  text
*----------------------------------------------------------------------*
FORM f_popup_buscar_conductor  USING    pvi_nombre   TYPE zbas_conductores-nombre
                                        pvi_apellido TYPE zbas_conductores-apellido
                               CHANGING pve_dni      TYPE zbas_conductores-dni.

  DATA lt_conductores TYPE zbas_conductores_tab_t.

  zcl_bas=>buscar_conductor( EXPORTING
                               i_nombre      = pvi_nombre
                               i_apellido    = pvi_apellido
                             RECEIVING
                               r_conductores = lt_conductores
                             EXCEPTIONS
                               no_encontrado = 1
                               OTHERS        = 2 ).
  IF lt_conductores[] IS INITIAL.
    MESSAGE 'No se encontraron coincidencias' TYPE 'S'.
    RETURN.
  ENDIF.

  DATA: lv_tabix TYPE sy-tabix.

  PERFORM f_popup_tsel USING    lt_conductores
                       CHANGING lv_tabix.
  IF lv_tabix IS NOT INITIAL.
    READ TABLE lt_conductores INTO DATA(wa_conductores) INDEX lv_tabix.
    IF sy-subrc IS INITIAL.
      MOVE wa_conductores-dni TO pve_dni.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_BUSCAR_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_TICKET_SCREEN_MATRICULA_TRA  text
*      <--P_GS_TICKET_SCREEN_tdlnr  text
*----------------------------------------------------------------------*
FORM f_popup_buscar_transporte  CHANGING pve_matricula TYPE zbas_transportes-matricula
                                         pve_tdlnr     TYPE zbas_transportes-tdlnr.
  DATA: lt_transportes TYPE zbas_transportes_tab_t.

  zcl_bas=>buscar_transporte(
    EXPORTING
      i_matricula   = pve_matricula
      i_tdlnr       = pve_tdlnr
    RECEIVING
      r_transportes = lt_transportes
    EXCEPTIONS
      no_encontrado = 1
      OTHERS        = 2 ).

  IF lt_transportes[] IS INITIAL.
    MESSAGE 'No se encontraron coincidencias' TYPE 'S'.
    RETURN.
  ENDIF.

  DATA: lv_tabix TYPE sy-tabix.

  PERFORM f_popup_tsel USING    lt_transportes
                       CHANGING lv_tabix.
  IF lv_tabix IS NOT INITIAL.
    READ TABLE lt_transportes INTO DATA(wa_transportes) INDEX lv_tabix.
    IF sy-subrc IS INITIAL.
      MOVE: wa_transportes-matricula TO pve_matricula,
            wa_transportes-tdlnr     TO pve_tdlnr.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_NAV_TO_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_STATUS_CLICK_BASSTS  text
*----------------------------------------------------------------------*
FORM f_nav_to_status  USING    pvi_to_bassts TYPE zbas_s-bassts.

  gv_current_bassts = pvi_to_bassts.
  gv_current_subscr = 9100 + gv_current_bassts.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SET_CURR_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_POE_TICKET_>GET_STATUS_ACTUAL(  text
*      -->P_)  text
*----------------------------------------------------------------------*
FORM f_set_curr_status  USING pvi_curr_bassts TYPE zbas_s-bassts.

  gv_current_bassts = pvi_curr_bassts.
  gv_active_status_subscr = 9100 + gv_current_bassts.
  gv_current_subscr = gv_active_status_subscr.

  IF gv_current_subscr EQ 9130.
    CONCATENATE '9130'
                '9231'
                '9232'
                '9233'
           INTO gv_active_status_subscr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_CAMPOS_9110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_TICKET_SCREEN  text
*      <--P_GV_OKCODE  text
*----------------------------------------------------------------------*
FORM f_validar_campos_9110  USING    psi_ticket_screen TYPE tys_ticket_screen
                            CHANGING pve_okcode TYPE sy-ucomm.

  IF psi_ticket_screen-bastk IS INITIAL
    OR psi_ticket_screen-werks IS INITIAL
    OR psi_ticket_screen-tipooperacion IS INITIAL
    OR psi_ticket_screen-tipooperacion_descripcion IS INITIAL
    OR psi_ticket_screen-basid IS INITIAL
    OR psi_ticket_screen-basid_denominacion IS INITIAL
    OR psi_ticket_screen-dni_conductor IS INITIAL
    OR psi_ticket_screen-conductor_nombre IS INITIAL
    OR psi_ticket_screen-conductor_apellido IS INITIAL
    OR psi_ticket_screen-matricula_transp IS INITIAL
    OR psi_ticket_screen-trans_tipo_vehiculo IS INITIAL
    OR psi_ticket_screen-tdlnr IS INITIAL
    OR psi_ticket_screen-trans_zhabsenasa IS INITIAL
    OR psi_ticket_screen-trans_zdatumsenasa IS INITIAL.
    MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
    CLEAR pve_okcode.
    RETURN.
  ENDIF.

  IF psi_ticket_screen-trans_zdatumsenasa LT sy-datum.
    MESSAGE 'Hab.SENASA vencida. Actualice la Fecha de Vto.' TYPE 'S' DISPLAY LIKE 'E'.
    CLEAR pve_okcode.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TSEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CONDUCTORES  text
*      <--P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM f_popup_tsel  USING    pti_table TYPE ANY TABLE
                   CHANGING pve_tabix TYPE sy-tabix.

  DATA: lo_salv TYPE REF TO cl_salv_table.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_salv
        CHANGING
          t_table      = pti_table ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  DATA(lr_functions) = lo_salv->get_functions( ).
  lr_functions->set_all( 'X' ).

  DATA(lr_columns) = lo_salv->get_columns( ).
  lr_columns->set_optimize( 'X' ).

*  TRY.
*  CALL METHOD lo_salv->set_screen_status
*    EXPORTING
*      report        = 'SALV_DEMO_TABLE_EVENTS'
*      pfstatus      = 'D0100'
**      SET_FUNCTIONS = C_FUNCTIONS_NONE
  .
*  ENDTRY.

  TRY.
      DATA(lr_column) = lr_columns->get_column( 'MANDT' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  lo_salv->set_screen_popup( start_column = 10
                             end_column   = 90
                             start_line   = 1
                             end_line     = 15 ).

  DATA(lr_selections) = lo_salv->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

  lo_salv->display( ).

  lr_selections = lo_salv->get_selections( ).
  DATA(lt_sel) = lr_selections->get_selected_rows( ).

  READ TABLE lt_sel INTO DATA(wa_sel) INDEX 1.
  IF sy-subrc IS INITIAL.
    MOVE wa_sel TO pve_tabix.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_BASID_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_init_basid_listbox USING pvi_werks TYPE zbas_basculas-werks.

  DATA: lv_id    TYPE vrm_id,
        wa_value TYPE vrm_value,
        lt_value TYPE vrm_values.

  lv_id = 'GS_TICKET_SCREEN-BASID'.

  SELECT *
    FROM zbas_basculas
    INTO TABLE @DATA(lt_zbas_basculas)
    WHERE werks EQ @pvi_werks.

  CHECK sy-subrc IS INITIAL.

  SORT lt_zbas_basculas BY basid.

  LOOP AT lt_zbas_basculas INTO DATA(wa_zbas_basculas).
    MOVE: wa_zbas_basculas-basid TO wa_value-key,
          wa_zbas_basculas-denominacion TO wa_value-text.
    APPEND wa_value TO lt_value.
    CLEAR wa_value.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_id
      values = lt_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_BASID_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_init_tipocarga_listbox USING pvi_werks TYPE zbas_basculas-werks.

  DATA: lv_id    TYPE vrm_id,
        wa_value TYPE vrm_value,
        lt_value TYPE vrm_values.

  lv_id = 'GS_TICKET_SCREEN-TIPO_CARGA'.

  SELECT *
    FROM zbas_tipocarg_ce
    INTO TABLE @DATA(lt_zbas_tipocarg_ce)
    WHERE werks EQ @pvi_werks.

  CHECK sy-subrc IS INITIAL.

  SELECT *
    FROM zbas_tipocarga
    INTO TABLE @DATA(lt_zbas_tipocarga)
    FOR ALL ENTRIES IN @lt_zbas_tipocarg_ce
    WHERE tipocarga EQ @lt_zbas_tipocarg_ce-tipocarga.

  CHECK sy-subrc IS INITIAL.

  SORT lt_zbas_tipocarga BY tipocarga.

  LOOP AT lt_zbas_tipocarga INTO DATA(wa_zbas_tipocarga).
    MOVE: wa_zbas_tipocarga-tipocarga   TO wa_value-key,
          wa_zbas_tipocarga-descripcion TO wa_value-text.
    APPEND wa_value TO lt_value.
    CLEAR wa_value.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_id
      values = lt_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_MODIF_BASMODO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_TICKET_SCREEN_BASID  text
*      -->P_GS_TICKET_SCREEN_BAS_MODO_ING  text
*----------------------------------------------------------------------*
FORM f_popup_modif_basmodo  USING    pvi_basid    TYPE zbas_basculas-basid
                                     pvi_bastk    TYPE zbas_h-bastk
                                     pvi_werks    TYPE zbas_basculas-werks
                            CHANGING pve_bas_modo TYPE zbas_basculas-modo.

  AUTHORITY-CHECK OBJECT 'ZBAS_ROLES'
           ID 'WERKS' FIELD pvi_werks
           ID 'ZBAS_ROL' FIELD 'MB'.

  IF sy-subrc IS NOT INITIAL.

    pve_bas_modo = zcl_bas=>get_bascula_modo( pvi_basid ).

    MESSAGE 'No posee autorización para modificar el modo de la Báscula'
      TYPE 'S'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA: wa_basmodo_log TYPE zbas_basmodo_log.

  MOVE: pvi_basid     TO wa_basmodo_log-basid,
        sy-datum      TO wa_basmodo_log-datum,
        sy-uzeit      TO wa_basmodo_log-uzeit,
        pve_bas_modo  TO wa_basmodo_log-modo,
        pvi_bastk     TO wa_basmodo_log-bastk.

  DATA: lv_retcode TYPE c,
        wa_fields  TYPE sval,
        lt_fields  TYPE STANDARD TABLE OF sval.

  wa_fields-tabname = 'ZBAS_BASMODO_LOG'.
  wa_fields-fieldname = 'BASID'.
  wa_fields-field_attr = '03'.
  wa_fields-value =  wa_basmodo_log-basid.
  APPEND wa_fields TO lt_fields.
  CLEAR wa_fields.

  wa_fields-tabname = 'ZBAS_BASMODO_LOG'.
  wa_fields-fieldname = 'DATUM'.
  wa_fields-field_attr = '03'.
  wa_fields-value =  wa_basmodo_log-datum.
  APPEND wa_fields TO lt_fields.
  CLEAR wa_fields.

  wa_fields-tabname = 'ZBAS_BASMODO_LOG'.
  wa_fields-fieldname = 'UZEIT'.
  wa_fields-field_attr = '03'.
  wa_fields-value =  wa_basmodo_log-uzeit.
  APPEND wa_fields TO lt_fields.
  CLEAR wa_fields.

  wa_fields-tabname = 'ZBAS_BASMODO_LOG'.
  wa_fields-fieldname = 'MODO'.
  wa_fields-field_attr = '03'.
  wa_fields-value =  wa_basmodo_log-modo.
  APPEND wa_fields TO lt_fields.
  CLEAR wa_fields.

  wa_fields-tabname = 'ZBAS_BASMODO_LOG'.
  wa_fields-fieldname = 'BASTK'.
  wa_fields-field_attr = '03'.
  wa_fields-value =  wa_basmodo_log-bastk.
  APPEND wa_fields TO lt_fields.
  CLEAR wa_fields.

  wa_fields-tabname = 'ZBAS_BASMODO_LOG'.
  wa_fields-fieldname = 'RAZON'.
  wa_fields-field_obl = 'X'.
  APPEND wa_fields TO lt_fields.
  CLEAR wa_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Debe informar un motivo para el cambio de Modo'
    IMPORTING
      returncode      = lv_retcode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0
    OR lv_retcode EQ 'A'. " Cancelar

    pve_bas_modo = zcl_bas=>get_bascula_modo( pvi_basid ).
    MESSAGE 'Cambio de Modo cancelado'
      TYPE 'S'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE lt_fields INTO wa_fields WITH KEY fieldname = 'RAZON'.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Error imprevisto en función popup. Guarde el ticket y finalice la transacción' TYPE 'E'.
  ELSE.
    MOVE wa_fields-value TO wa_basmodo_log-razon.
    pve_bas_modo = zcl_bas=>set_bascula_modo( wa_basmodo_log ).
  ENDIF.

* TODO : Popup para ingresar motivo
*    -> se le abre popup para dejar constancia del motivo
*        -> se logea el cambio de modo para la bascula (con motivo y nro de ticket)
*        -> se cambia el modo de la bascula en ZBAS_BASCULAS
*        -> se guarda el ticket con nuevo modo

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_ZBAS_CDE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GC_ZBAS_C_ALV_CONTAINER_NAME  text
*      -->P_GO_ZBAS_C_ALV_CONTAINER  text
*      <--P_GT_ZBAS_C  text
*      <--P_GO_ZBAS_C_ALV_GRID  text
*----------------------------------------------------------------------*
FORM f_init_zbas_cde_alv    USING    pvi_zbas_cde_container_name
                                     poi_alv_container   TYPE REF TO cl_gui_custom_container
                            CHANGING pti_zbas_cde_list   TYPE STANDARD TABLE
                                     poe_zbas_cde_grid   TYPE REF TO cl_gui_alv_grid.

  DATA: wa_layo           TYPE lvc_s_layo,
        wa_fcat           TYPE lvc_s_fcat,
        lt_fcat           TYPE lvc_t_fcat,
        lo_event_handler  TYPE REF TO lcl_event_handler,
        lv_structure_name TYPE dd02l-tabname.

  wa_layo-no_toolbar = 'X'.
*  wa_layo-smalltitle = 'X'.

  CASE pvi_zbas_cde_container_name.
    WHEN gc_zbas_c_alv_container_name.
*      wa_layo-grid_title  = 'Carga'.
      lv_structure_name   = 'ZBAS_C'.
    WHEN gc_zbas_d_alv_container_name.
*      wa_layo-grid_title  = 'Descarga'.
      lv_structure_name   = 'ZBAS_D'.
    WHEN gc_zbas_e_alv_container_name.
*      wa_layo-grid_title  = 'Embalajes'.
      lv_structure_name   = 'ZBAS_E'.
    WHEN OTHERS.
  ENDCASE.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = lv_structure_name
    CHANGING
      ct_fieldcat      = lt_fcat.

  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'MANDT' OR 'BASTK' OR 'ERNAM'
        OR 'ERDAT' OR 'ERZET'.
        <fs_fcat>-no_out = 'X'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  CREATE OBJECT poe_zbas_cde_grid
    EXPORTING
      i_parent = poi_alv_container.

  poe_zbas_cde_grid->set_table_for_first_display(
    EXPORTING
      is_layout        = wa_layo
*      i_structure_name = lv_structure_name
    CHANGING
      it_fieldcatalog  = lt_fcat
      it_outtab        = pti_zbas_cde_list ).

* Bloquear grilla y acciones
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_lectura
    OR sy-dynnr NE gv_active_status_subscr.
    poe_zbas_cde_grid->set_ready_for_input( 0 ).

  ENDIF.
* Habilitar grilla y borrado
*  ELSE.
  CREATE OBJECT lo_event_handler .
  SET HANDLER lo_event_handler->on_zbas_cde_double_click FOR poe_zbas_cde_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_CONTROL_PESOS_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GC_ZBAS_C_ALV_CONTAINER_NAME  text
*      -->P_GO_ZBAS_C_ALV_CONTAINER  text
*      <--P_GT_ZBAS_C  text
*      <--P_GO_ZBAS_C_ALV_GRID  text
*----------------------------------------------------------------------*
FORM f_init_control_pesos_alv  USING    poi_alv_container  TYPE REF TO cl_gui_custom_container
                               CHANGING pti_control_pesos_list  TYPE tyt_control_pesos
                                        poe_control_pesos_grid  TYPE REF TO cl_gui_alv_grid.

  DATA: wa_layo           TYPE lvc_s_layo,
        wa_fcat           TYPE lvc_s_fcat,
        lt_fcat           TYPE lvc_t_fcat,
        lo_event_handler  TYPE REF TO lcl_event_handler,
        lv_structure_name TYPE dd02l-tabname.

  wa_layo-no_toolbar = 'X'.
  wa_layo-no_vgridln = 'X'.
  wa_layo-no_headers = 'X'.
  wa_layo-info_fname = 'ROW_COLOR'.
*  wa_layo-smalltitle = 'X'.


*   ROW      TYPE sy-tabix,
*   CONCEPTO TYPE ddtext,
*   PESO     TYPE zbas_h-peso_ing,
*   GEWEI    TYPE zbas_h-gewei,
*   ERDAT    TYPE zbas_h-erdat,
*   ERZET    TYPE zbas_h-erzet,
*   ERNAM    TYPE zbas_h-ernam,

  wa_fcat-fieldname = 'ROW'.
  wa_fcat-tabname = 'GT_CONTROL_PESOS'.
  wa_fcat-seltext = 'Órden'.
  wa_fcat-key = 'X'.
  wa_fcat-no_out = 'X'.
  APPEND wa_fcat TO lt_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'CONCEPTO'.
  wa_fcat-tabname = 'GT_CONTROL_PESOS'.
  wa_fcat-outputlen = 40.
  wa_fcat-seltext = '+'.
  wa_fcat-key = 'X'.
  APPEND wa_fcat TO lt_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'PESO'.
  wa_fcat-tabname = 'GT_CONTROL_PESOS'.
  wa_fcat-outputlen = 16.
  wa_fcat-seltext = '+'.
*  wa_fcat-qfieldname = 'GEWEI'.
  wa_fcat-decimals_o = 2.
  APPEND wa_fcat TO lt_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'GEWEI'.
  wa_fcat-tabname = 'GT_CONTROL_PESOS'.
  wa_fcat-outputlen = 2.
  wa_fcat-seltext = '+'.
  APPEND wa_fcat TO lt_fcat.
  CLEAR wa_fcat.

*  wa_fcat-fieldname = 'ERDAT'.
*  wa_fcat-tabname = 'GT_CONTROL_PESOS'.
*  wa_fcat-ref_table = 'ZBAS_H'.
*  wa_fcat-no_out = 'X'.
*  APPEND wa_fcat TO lt_fcat.
*  CLEAR wa_fcat.
*
*  wa_fcat-fieldname = 'ERZET'.
*  wa_fcat-tabname = 'GT_CONTROL_PESOS'.
*  wa_fcat-ref_table = 'ZBAS_H'.
*  wa_fcat-no_out = 'X'.
*  APPEND wa_fcat TO lt_fcat.
*  CLEAR wa_fcat.
*
*  wa_fcat-fieldname = 'ERNAM'.
*  wa_fcat-tabname = 'GT_CONTROL_PESOS'.
*  wa_fcat-ref_table = 'ZBAS_H'.
*  wa_fcat-no_out = 'X'.
*  APPEND wa_fcat TO lt_fcat.
*  CLEAR wa_fcat.


  CREATE OBJECT poe_control_pesos_grid
    EXPORTING
      i_parent = poi_alv_container.

  poe_control_pesos_grid->set_table_for_first_display(
    EXPORTING
      is_layout        = wa_layo
*      i_structure_name = lv_structure_name
    CHANGING
      it_fieldcatalog  = lt_fcat
      it_outtab        = pti_control_pesos_list ).

* Bloquear grilla y acciones
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_lectura
    OR sy-dynnr NE gv_active_status_subscr.
    poe_control_pesos_grid->set_ready_for_input( 0 ).

  ENDIF.
* Habilitar grilla y borrado
*  ELSE.
*  CREATE OBJECT lo_event_handler .
*  SET HANDLER lo_event_handler->on_CONTROL_PESOS_double_click FOR poe_CONTROL_PESOS_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RECARGAR_ZBAS_C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GO_TICKET  text
*      <--P_GT_ZBAS_C  text
*----------------------------------------------------------------------*
FORM f_recargar_zbas_cde  USING    poi_ticket TYPE REF TO zcl_bas_ticket
                          CHANGING pte_zbas_c TYPE zbas_c_tab_t
                                   pte_zbas_d TYPE zbas_d_tab_t
                                   pte_zbas_e TYPE zbas_e_tab_t.

  pte_zbas_c = poi_ticket->get_carga_tab( ).
  pte_zbas_d = poi_ticket->get_descarga_tab( ).
  pte_zbas_e = poi_ticket->get_embalajes_tab( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_TIPOEMBA_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_init_tipoemba_listbox .

  DATA: lv_id    TYPE vrm_id,
        wa_value TYPE vrm_value,
        lt_value TYPE vrm_values.

  lv_id = 'GS_ZBAS_E-TIPO_EMBA'.

  SELECT *
    FROM zbas_tipoemba
    INTO TABLE @DATA(lt_zbas_tipoemba).

  CHECK sy-subrc IS INITIAL.

  SORT lt_zbas_tipoemba BY tipoembalaje.

  LOOP AT lt_zbas_tipoemba INTO DATA(wa_zbas_tipoemba).
    MOVE: wa_zbas_tipoemba-tipoembalaje TO wa_value-key,
          wa_zbas_tipoemba-descripcion  TO wa_value-text.
    APPEND wa_value TO lt_value.
    CLEAR wa_value.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_id
      values = lt_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONTROL_DE_PESOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GO_TICKET  text
*      <--P_GT_CONTROL_PESOS  text
*----------------------------------------------------------------------*
FORM f_control_de_pesos  USING    poi_ticket        TYPE REF TO zcl_bas_ticket
                                  psi_ticket_screen TYPE tys_ticket_screen
                         CHANGING pte_control_pesos TYPE tyt_control_pesos.

  CONSTANTS: BEGIN OF ls_conceptos,
               01_tara        TYPE ddtext VALUE 'TARA',
               02_peso_doc    TYPE ddtext VALUE 'PESO DOCUMENTADO',
               03_embalaje    TYPE ddtext VALUE 'EMBALAJE',
               04_bruto       TYPE ddtext VALUE 'BRUTO',
               05_diferencia  TYPE ddtext VALUE 'DIFERENCIA BRUTO-TARA',
*               06             TYPE ddtext VALUE '',
               07_dif_carga   TYPE ddtext VALUE 'DIFERENCIA DE CARGA',
               08_tolerancia  TYPE ddtext VALUE 'TOLERANCIA MAT%',
               09_toler_kg    TYPE ddtext VALUE 'TOLERANCIA KG',
               10_error_bas   TYPE ddtext VALUE 'ERROR BASCULA',
               11_p_fuera_tol TYPE ddtext VALUE 'PESO FUERA DE TOLERANCIA',
             END OF ls_conceptos.

  DATA: BEGIN OF ls_refs,
          01_tara        TYPE REF TO tys_control_pesos,
          02_peso_doc    TYPE REF TO tys_control_pesos,
          03_embalaje    TYPE REF TO tys_control_pesos,
          04_bruto       TYPE REF TO tys_control_pesos,
          05_diferencia  TYPE REF TO tys_control_pesos,
*          06             TYPE REF TO tys_control_pesos,
          07_dif_carga   TYPE REF TO tys_control_pesos,
          08_tolerancia  TYPE REF TO tys_control_pesos,
          09_toler_kg    TYPE REF TO tys_control_pesos,
          10_error_bas   TYPE REF TO tys_control_pesos,
          11_p_fuera_tol TYPE REF TO tys_control_pesos,
        END OF ls_refs.

  DATA: lv_tolerancia     TYPE zbas_tipocarga-tolerancia,
        lv_gewei          TYPE tys_control_pesos-gewei,
        wa_control_pesos  TYPE tys_control_pesos.

  REFRESH pte_control_pesos.

  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE ls_conceptos TO FIELD-SYMBOL(<concepto>).
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
    ASSIGN COMPONENT sy-index OF STRUCTURE ls_refs TO FIELD-SYMBOL(<valor>).
    MOVE <concepto> TO wa_control_pesos-concepto.
    APPEND wa_control_pesos TO  pte_control_pesos REFERENCE INTO <valor>.
    CLEAR wa_control_pesos.
  ENDDO.

  poi_ticket->get_totales_cde( IMPORTING  e_total_embalaje      = ls_refs-03_embalaje->*-peso
                                          e_peso_ing            = ls_refs-01_tara->*-peso
                                          e_peso_doc            = ls_refs-02_peso_doc->*-peso
                                          e_peso_egr            = ls_refs-04_bruto->*-peso
                                          e_diferencia          = ls_refs-05_diferencia->*-peso
                                          e_dif_carga           = ls_refs-07_dif_carga->*-peso
                                          e_tolerancia          = lv_tolerancia " ls_refs-08_tolerancia->*-peso
                                          e_toler_kg            = ls_refs-09_toler_kg->*-peso
                                          e_error_bas           = ls_refs-10_error_bas->*-peso
                                          e_peso_fuera_tol      = ls_refs-11_p_fuera_tol->*-peso
                                          e_gewei               = lv_gewei
                                        EXCEPTIONS
                                          unit_conversion_error = 1
                                          OTHERS                = 2 ).

  IF sy-subrc <> 0.
    MESSAGE 'Error en conversión de Pesos' TYPE 'A' DISPLAY LIKE 'I'.
  ENDIF.
  ls_refs-08_tolerancia->*-peso   = lv_tolerancia.

  MOVE lv_gewei TO: ls_refs-01_tara->*-gewei,
                    ls_refs-02_peso_doc->*-gewei,
                    ls_refs-03_embalaje->*-gewei,
                    ls_refs-04_bruto->*-gewei,
                    ls_refs-05_diferencia->*-gewei,
                    ls_refs-07_dif_carga->*-gewei,
                    ls_refs-09_toler_kg->*-gewei,
                    ls_refs-10_error_bas->*-gewei,
                    ls_refs-11_p_fuera_tol->*-gewei.
  MOVE '%'      TO  ls_refs-08_tolerancia->*-gewei.

  MOVE: 'C300' TO ls_refs-01_tara->*-row_color,
        'C401' TO ls_refs-02_peso_doc->*-row_color,
        'C401' TO ls_refs-03_embalaje->*-row_color,
        'C300' TO ls_refs-04_bruto->*-row_color,
        'C401' TO ls_refs-05_diferencia->*-row_color,
        'C500' TO ls_refs-08_tolerancia->*-row_color,
        'C500' TO ls_refs-09_toler_kg->*-row_color,
        'C500' TO ls_refs-10_error_bas->*-row_color.

  IF ls_refs-07_dif_carga->*-peso GT 0.
    MOVE: 'C600' TO ls_refs-07_dif_carga->*-row_color.
  ELSE.
    MOVE: 'C500' TO ls_refs-07_dif_carga->*-row_color.
  ENDIF.

  IF ls_refs-11_p_fuera_tol->*-peso GT 0.
    MOVE: 'C610' TO ls_refs-11_p_fuera_tol->*-row_color.
  ELSE.
    MOVE: 'C510' TO ls_refs-11_p_fuera_tol->*-row_color.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0062   text
*      -->P_0063   text
*----------------------------------------------------------------------*
FORM f_auth_check  USING pvi_rol TYPE zbas_roles_t
                         pvi_err.

  AUTHORITY-CHECK OBJECT 'ZBAS_ROLES'
           ID 'WERKS' FIELD gs_ticket_screen-werks
           ID 'ZBAS_ROL' FIELD pvi_rol.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE pvi_err TYPE 'E'.
  ENDIF.

ENDFORM.