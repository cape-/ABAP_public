*----------------------------------------------------------------------*
***INCLUDE ZBAS_NUEVO_TICKET_STATUS_O01.
*----------------------------------------------------------------------*
*&
*& Copyright (C) Lautaro Capella - All Rights Reserved
*& Unauthorized copying of this file, via any medium is strictly prohibited
*& Proprietary and confidential
*& Written by Lautaro Capella <laucape@gmail.com>, Aug 2020
*&
*&---------------------------------------------------------------------*
*&      Module  PBO_ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9000 OUTPUT.

  SET PF-STATUS 'ST9000'.
  SET TITLEBAR 'TIT9000' WITH go_ticket->bastk.

  IF go_ticket_sts_alv_container IS INITIAL.

    PERFORM f_init_container USING    gc_ticket_sts_container_name
                             CHANGING go_ticket_sts_alv_container.

    PERFORM f_init_ticket_sts_alv USING    go_ticket_sts_alv_container
                                  CHANGING gt_status_list
                                           go_ticket_sts_alv_grid.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9110 OUTPUT.
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'CR'
                               'No posee autorización para esta acción'.
  ELSE.
    PERFORM f_auth_check USING 'VI'
                               'No posee autorización para visualizar'.
  ENDIF.

  IF ( gv_okcode_9000 EQ 'INPUT_BASID'
       AND gs_ticket_screen-basid IS NOT INITIAL )
    OR ( gs_ticket_screen-basid_denominacion IS INITIAL
         AND gs_ticket_screen-basid IS NOT INITIAL ).

    zcl_bas=>get_bascula( EXPORTING i_basid = gs_ticket_screen-basid
                          IMPORTING e_denominacion = gs_ticket_screen-basid_denominacion ).
  ENDIF.

  IF ( gv_okcode_9000 EQ 'INPUT_TIPOOPERACION'
    AND gs_ticket_screen-tipooperacion IS NOT INITIAL )
    OR ( gs_ticket_screen-tipooperacion_descripcion IS INITIAL
         AND gs_ticket_screen-tipooperacion IS NOT INITIAL ).

    zcl_bas=>get_tipooperacion( EXPORTING i_tipooperacion = gs_ticket_screen-tipooperacion
                                IMPORTING e_descripcion = gs_ticket_screen-tipooperacion_descripcion ).
  ENDIF.

  IF gv_okcode_9000 EQ 'INPUT_MATRICULA_TRANSP'
    OR gv_okcode_9000 EQ 'INPUT_TDLNR'.

    DATA: lv_multi_trans TYPE c.
    zcl_bas=>get_transporte( EXPORTING i_matricula     = gs_ticket_screen-matricula_transp
                                       i_tdlnr         = gs_ticket_screen-tdlnr
                             IMPORTING e_tipo_vehiculo = gs_ticket_screen-trans_tipo_vehiculo
                                       e_tdlnr         = gs_ticket_screen-tdlnr
                                       e_zhabsenasa    = gs_ticket_screen-trans_zhabsenasa
                                       e_zdatumsenasa  = gs_ticket_screen-trans_zdatumsenasa
                                       e_multiple      = lv_multi_trans ).
    IF lv_multi_trans IS NOT INITIAL.
      PERFORM f_popup_buscar_transporte CHANGING gs_ticket_screen-matricula_transp
                                                 gs_ticket_screen-tdlnr.
      zcl_bas=>get_transporte( EXPORTING i_matricula     = gs_ticket_screen-matricula_transp
                                         i_tdlnr         = gs_ticket_screen-tdlnr
                               IMPORTING e_tipo_vehiculo = gs_ticket_screen-trans_tipo_vehiculo
                                         e_tdlnr         = gs_ticket_screen-tdlnr
                                         e_zhabsenasa    = gs_ticket_screen-trans_zhabsenasa
                                         e_zdatumsenasa  = gs_ticket_screen-trans_zdatumsenasa ).
    ENDIF.
  ENDIF.

  IF gv_okcode_9000 EQ 'BUSCAR_TRANSP'.
    PERFORM f_popup_buscar_transporte CHANGING gs_ticket_screen-matricula_transp
                                               gs_ticket_screen-tdlnr.
    zcl_bas=>get_transporte( EXPORTING i_matricula     = gs_ticket_screen-matricula_transp
                                       i_tdlnr         = gs_ticket_screen-tdlnr
                             IMPORTING e_tipo_vehiculo = gs_ticket_screen-trans_tipo_vehiculo
                                       e_tdlnr         = gs_ticket_screen-tdlnr
                                       e_zhabsenasa    = gs_ticket_screen-trans_zhabsenasa
                                       e_zdatumsenasa  = gs_ticket_screen-trans_zdatumsenasa ).
  ENDIF.

  IF gv_okcode_9000 EQ 'INPUT_DNI_CONDUCTOR'
    OR gv_okcode_9000 EQ 'BUSCAR_CONDUCTOR'.
    zcl_bas=>get_conductor( EXPORTING i_dni     = gs_ticket_screen-dni_conductor
                            IMPORTING e_nombre   = gs_ticket_screen-conductor_nombre
                                      e_apellido = gs_ticket_screen-conductor_apellido ).
  ENDIF.

  IF gv_okcode_9000 EQ 'BUSCAR_CONDUCTOR_NYA'.
*   POPUP buscar conductor
    PERFORM f_popup_buscar_conductor USING    gs_ticket_screen-conductor_nombre
                                              gs_ticket_screen-conductor_apellido
                                     CHANGING gs_ticket_screen-dni_conductor.
    zcl_bas=>get_conductor( EXPORTING i_dni     = gs_ticket_screen-dni_conductor
                            IMPORTING e_nombre   = gs_ticket_screen-conductor_nombre
                                      e_apellido = gs_ticket_screen-conductor_apellido ).
  ENDIF.

  IF gv_basid_listbox_init IS INITIAL.
    PERFORM f_init_basid_listbox USING gs_ticket_screen-werks.
    MOVE 'X' TO gv_basid_listbox_init.
  ENDIF.

  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9120 OUTPUT.
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'IE'
                               'No posee autorización para esta acción'.
  ELSE.
    PERFORM f_auth_check USING 'VI'
                               'No posee autorización para visualizar'.
  ENDIF.

  CASE gv_okcode_9000.
    WHEN 'BAS_MODE_ING'.  " Cambio de modo de báscula
    WHEN 'CAPTURAR_PESAJE_ING'. " Botón: Capturar Pesaje
    WHEN 'CHECKS_INGRESO'. " Tildes de revisión
  ENDCASE.

* Checks para activar pesaje
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.

    IF gs_ticket_screen-check1_ing IS INITIAL
      OR gs_ticket_screen-check2_ing IS INITIAL .
      LOOP AT SCREEN.
        IF screen-group2 EQ 'CHI'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.

      IF gs_ticket_screen-gewei IS INITIAL.
        MOVE gc_default_gewei TO gs_ticket_screen-gewei.
      ENDIF.

      IF gs_ticket_screen-bas_modo_ing IS INITIAL.
        zcl_bas=>get_bascula(
            EXPORTING
              i_basid         = gs_ticket_screen-basid
            IMPORTING
              e_modo         = gs_ticket_screen-bas_modo_ing
              e_denominacion = gs_ticket_screen-basid_denominacion ).
      ENDIF.

      IF gs_ticket_screen-bas_modo_ing NE zcl_bas=>gc_bas_modo_manual. " !M = !input Manual
        LOOP AT SCREEN.
          IF screen-group3 EQ 'BMI'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.

  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9130 OUTPUT.
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'CD'
                               'No posee autorización para esta acción'.
  ELSE.
    PERFORM f_auth_check USING 'VI'
                               'No posee autorización para visualizar'.
  ENDIF.

  IF gv_tipocarga_listbox_init IS INITIAL.
    PERFORM f_init_tipocarga_listbox USING gs_ticket_screen-werks.
    MOVE 'X' TO gv_tipocarga_listbox_init.
  ENDIF.

  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9140  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9140 OUTPUT.
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'IE'
                               'No posee autorización para esta acción'.
  ELSE.
    PERFORM f_auth_check USING 'VI'
                               'No posee autorización para visualizar'.
  ENDIF.

  CASE gv_okcode_9000.
    WHEN 'BAS_MODE_ING'.  " Cambio de modo de báscula
    WHEN 'CAPTURAR_PESAJE_ING'. " Botón: Capturar Pesaje
    WHEN 'CHECKS_INGRESO'. " Tildes de revisión
  ENDCASE.

* Checks para activar pesaje
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.

    IF gs_ticket_screen-check1_egr IS INITIAL
      OR gs_ticket_screen-check2_egr IS INITIAL .
      LOOP AT SCREEN.
        IF screen-group2 EQ 'CHI'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.

      IF gs_ticket_screen-gewei IS INITIAL.
        MOVE gc_default_gewei TO gs_ticket_screen-gewei.
      ENDIF.

      IF gs_ticket_screen-bas_modo_egr IS INITIAL.
        zcl_bas=>get_bascula(
            EXPORTING
              i_basid         = gs_ticket_screen-basid
            IMPORTING
              e_modo         = gs_ticket_screen-bas_modo_egr
              e_denominacion = gs_ticket_screen-basid_denominacion ).
      ENDIF.

      IF gs_ticket_screen-bas_modo_egr NE zcl_bas=>gc_bas_modo_manual. " !M = !input Manual
        LOOP AT SCREEN.
          IF screen-group3 EQ 'BMI'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.

* CONTROL DE PESOS
  PERFORM f_control_de_pesos  USING    go_ticket
                                       gs_ticket_screen
                              CHANGING gt_control_pesos.

  IF go_control_pesos_container IS INITIAL.

    PERFORM f_init_container USING    gc_control_pesos_container_nam
                             CHANGING go_control_pesos_container.

    PERFORM f_init_control_pesos_alv USING    go_control_pesos_container
                                     CHANGING gt_control_pesos
                                              go_control_pesos_grid.

  ELSE.
    go_control_pesos_grid->refresh_table_display( ).

  ENDIF.


  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9150  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9150 OUTPUT.
  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'A1'
                               'No posee autorización para esta acción'.
  ELSE.
    PERFORM f_auth_check USING 'VI'
                               'No posee autorización para visualizar'.
  ENDIF.

  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9160  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9160 OUTPUT.

  IF gs_ticket_screen-erdat_60 IS INITIAL.
    READ TABLE gt_status_list INTO DATA(wa_sts_cerrado)
                              WITH KEY bassts = gv_current_bassts.
    IF sy-subrc IS INITIAL.
      MOVE: wa_sts_cerrado-erdat TO gs_ticket_screen-erdat_60,
            wa_sts_cerrado-erzet TO gs_ticket_screen-erzet_60,
            wa_sts_cerrado-ernam TO gs_ticket_screen-ernam_60.
    ENDIF.
  ENDIF.
  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9199  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9199 OUTPUT.

  IF gs_ticket_screen-erdat_99 IS INITIAL.
    READ TABLE gt_status_list INTO DATA(wa_sts_anulado)
                              WITH KEY bassts = gv_current_bassts.
    IF sy-subrc IS INITIAL.
      MOVE: wa_sts_anulado-erdat TO gs_ticket_screen-erdat_99,
            wa_sts_anulado-erzet TO gs_ticket_screen-erzet_99,
            wa_sts_anulado-ernam TO gs_ticket_screen-ernam_99.
    ENDIF.
  ENDIF.
  PERFORM f_loop_screen_pbo.

ENDMODULE.

MODULE cde_tabs_pbo OUTPUT.
  CASE gv_okcode_9000.
    WHEN 'CDE_TAB_C'.
      MOVE: gv_okcode_9000 TO g_cde_tabs-pressed_tab,
            '9231'         TO g_cde_tabs-subscr.
    WHEN 'CDE_TAB_D'.
      MOVE: gv_okcode_9000 TO g_cde_tabs-pressed_tab,
            '9232'         TO g_cde_tabs-subscr.
    WHEN 'CDE_TAB_E'.
      MOVE: gv_okcode_9000 TO g_cde_tabs-pressed_tab,
            '9233'         TO g_cde_tabs-subscr.
    WHEN OTHERS.
  ENDCASE.
  cde_tabs-activetab = g_cde_tabs-pressed_tab.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9231  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9231 OUTPUT.

  PERFORM f_recargar_zbas_cde USING    go_ticket
                              CHANGING gt_zbas_c
                                       gt_zbas_d
                                       gt_zbas_e.

  IF go_zbas_c_alv_container IS INITIAL.

    PERFORM f_init_container USING    gc_zbas_c_alv_container_name
                             CHANGING go_zbas_c_alv_container.

    PERFORM f_init_zbas_cde_alv USING    gc_zbas_c_alv_container_name
                                         go_zbas_c_alv_container
                                CHANGING gt_zbas_c
                                         go_zbas_c_alv_grid.

  ELSE.
    go_zbas_c_alv_grid->refresh_table_display( ).

  ENDIF.

  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9232  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9232 OUTPUT.

  PERFORM f_recargar_zbas_cde USING    go_ticket
                              CHANGING gt_zbas_c
                                       gt_zbas_d
                                       gt_zbas_e.

  IF go_zbas_d_alv_container IS INITIAL.

    PERFORM f_init_container USING    gc_zbas_d_alv_container_name
                             CHANGING go_zbas_d_alv_container.

    PERFORM f_init_zbas_cde_alv USING    gc_zbas_d_alv_container_name
                                         go_zbas_d_alv_container
                                CHANGING gt_zbas_d
                                         go_zbas_d_alv_grid.

  ELSE.
    go_zbas_d_alv_grid->refresh_table_display( ).

  ENDIF.

  PERFORM f_loop_screen_pbo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9233  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9233 OUTPUT.

  PERFORM f_recargar_zbas_cde USING    go_ticket
                              CHANGING gt_zbas_c
                                       gt_zbas_d
                                       gt_zbas_e.

  IF go_zbas_e_alv_container IS INITIAL.

    PERFORM f_init_container USING    gc_zbas_e_alv_container_name
                             CHANGING go_zbas_e_alv_container.

    PERFORM f_init_zbas_cde_alv USING    gc_zbas_e_alv_container_name
                                         go_zbas_e_alv_container
                                CHANGING gt_zbas_e
                                         go_zbas_e_alv_grid.

  ELSE.
    go_zbas_e_alv_grid->refresh_table_display( ).

  ENDIF.

  IF gv_tipoemba_listbox_init IS INITIAL.
    PERFORM f_init_tipoemba_listbox.
    MOVE 'X' TO gv_tipoemba_listbox_init.
  ENDIF.

  PERFORM f_loop_screen_pbo.

ENDMODULE.