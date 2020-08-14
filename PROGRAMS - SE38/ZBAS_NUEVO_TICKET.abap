*&---------------------------------------------------------------------*
*& Modulpool  ZBAS_NUEVO_TICKET
*&
*&---------------------------------------------------------------------*
*&
*& Autor: Lautaro Capella (Nativa Consulting)
*& Fecha: 24/07/2020
*& Proyecto: QFPRO-3 - Proyecto BÁSCULAS
*&
*&---------------------------------------------------------------------*
*&
*& Copyright (C) Lautaro Capella - All Rights Reserved
*& Unauthorized copying of this file, via any medium is strictly prohibited
*& Proprietary and confidential
*& Written by Lautaro Capella <laucape@gmail.com>, Aug 2020
*&
*&---------------------------------------------------------------------*
PROGRAM zbas_nuevo_ticket.

SELECTION-SCREEN BEGIN OF SCREEN 9900 AS WINDOW.
SELECTION-SCREEN COMMENT /1(30) text-991.
PARAMETERS: p_werks TYPE zbas_h-werks.
SELECTION-SCREEN: SKIP,
                  COMMENT /1(30) text-992.
PARAMETERS: p_bastk TYPE zbas_h-bastk, " NO-DISPLAY. " TODO : Chequear NO-DISPLAY
            p_edita RADIOBUTTON GROUP rbg1,
            p_visua RADIOBUTTON GROUP rbg1.
SELECTION-SCREEN END OF SCREEN 9900.

* Types
TYPES: BEGIN OF tys_ticket_screen,
*      Header
         bastk                     TYPE zbas_h-bastk,
         werks                     TYPE zbas_h-werks,
*      Tipo de Operación
         tipooperacion             TYPE zbas_h-tipooperacion,
         tipooperacion_descripcion TYPE zbas_tipooper-descripcion,
*      Báscula
         basid                     TYPE zbas_h-basid,
         basid_denominacion        TYPE zbas_basculas-denominacion,
*      Conductor
         dni_conductor             TYPE zbas_h-dni_conductor,
         conductor_nombre          TYPE zbas_conductores-nombre,
         conductor_apellido        TYPE zbas_conductores-apellido,
*      Transporte
         matricula_transp          TYPE zbas_h-matricula_transp,
         tdlnr                     TYPE zbas_h-tdlnr,
         trans_tipo_vehiculo       TYPE zbas_transportes-tipo_vehiculo,
         trans_zhabsenasa          TYPE zbas_transportes-zhabsenasa,
         trans_zdatumsenasa        TYPE zbas_transportes-zdatumsenasa,
*      Pantalla Ingreso
         check1_ing                TYPE zbas_h-check1_ing,
         check2_ing                TYPE zbas_h-check2_ing,
         bas_modo_ing              TYPE zbas_h-bas_modo_ing,
         peso_ing                  TYPE zbas_h-peso_ing,
         gewei                     TYPE zbas_h-gewei,
*      Pantalla Carga / Descarga
         tipo_carga                TYPE zbas_h-tipo_carga,
*      Pantalla Egreso
         check1_egr                TYPE zbas_h-check1_egr,
         check2_egr                TYPE zbas_h-check2_egr,
         bas_modo_egr              TYPE zbas_h-bas_modo_egr,
         peso_egr                  TYPE zbas_h-peso_egr,
*      Cerrado
         erdat_60                  TYPE zbas_h-erdat,
         erzet_60                  TYPE zbas_h-erzet,
         ernam_60                  TYPE zbas_h-ernam,
*      Anulado
         erdat_99                  TYPE zbas_h-erdat,
         erzet_99                  TYPE zbas_h-erzet,
         ernam_99                  TYPE zbas_h-ernam,
*      Auditoría
         erdat                     TYPE zbas_h-erdat,
         erzet                     TYPE zbas_h-erzet,
         ernam                     TYPE zbas_h-ernam,
       END OF tys_ticket_screen.

TYPES: BEGIN OF tys_status_list.
        INCLUDE STRUCTURE zbas_ticket_status_text_t.
TYPES:   row_color(4) TYPE c,
         END OF tys_status_list,
         tyt_status_list TYPE STANDARD TABLE OF tys_status_list.

TYPES: BEGIN OF tys_control_pesos,
         row          TYPE sy-tabix,
         concepto     TYPE ddtext,
         peso         TYPE zbas_h-peso_ing,
         gewei        TYPE zbas_h-gewei,
*         erdat        TYPE zbas_h-erdat,
*         erzet        TYPE zbas_h-erzet,
*         ernam        TYPE zbas_h-ernam,
         row_color(4) TYPE c,
       END OF tys_control_pesos,
       tyt_control_pesos TYPE STANDARD TABLE OF tys_control_pesos.

* Globals
DATA: gv_okcode                 TYPE sy-ucomm,
      gv_okcode_9000            TYPE sy-ucomm,
      gv_current_bassts         TYPE zbas_s-bassts,
      gv_active_status_subscr   TYPE string,
      gv_current_subscr         TYPE sy-dynnr,
      gv_tipocarga_listbox_init TYPE c,
      gv_tipoemba_listbox_init  TYPE c,
      gv_basid_listbox_init     TYPE c.

* ALV objects
DATA: go_ticket_sts_alv_grid      TYPE REF TO cl_gui_alv_grid,
      go_ticket_sts_alv_container TYPE REF TO cl_gui_custom_container,
      go_zbas_c_alv_grid          TYPE REF TO cl_gui_alv_grid,
      go_zbas_c_alv_container     TYPE REF TO cl_gui_custom_container,
      go_zbas_d_alv_grid          TYPE REF TO cl_gui_alv_grid,
      go_zbas_d_alv_container     TYPE REF TO cl_gui_custom_container,
      go_zbas_e_alv_grid          TYPE REF TO cl_gui_alv_grid,
      go_zbas_e_alv_container     TYPE REF TO cl_gui_custom_container,
      go_control_pesos_grid       TYPE REF TO cl_gui_alv_grid,
      go_control_pesos_container  TYPE REF TO cl_gui_custom_container.

* Ticket object & Ticket data
DATA: gv_crear         TYPE c,
      gv_imp_bloqueo   TYPE enqmode,
      go_ticket        TYPE REF TO zcl_bas_ticket,
      gs_ticket_screen TYPE tys_ticket_screen,
      gt_status_list   TYPE tyt_status_list,
      gs_zbas_c        TYPE zbas_c,
      gt_zbas_c        TYPE STANDARD TABLE OF zbas_c,
      gs_zbas_d        TYPE zbas_d,
      gt_zbas_d        TYPE STANDARD TABLE OF zbas_d,
      gs_zbas_e        TYPE zbas_e,
      gt_zbas_e        TYPE STANDARD TABLE OF zbas_e,
      gt_control_pesos TYPE tyt_control_pesos.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'CDE_TABS'
*CONSTANTS: BEGIN OF c_cde_tabs,
*             tab1 LIKE sy-ucomm VALUE 'CDE_TAB_C',
*             tab2 LIKE sy-ucomm VALUE 'CDE_TAB_D',
*             tab3 LIKE sy-ucomm VALUE 'CDE_TAB_E',
*           END OF c_cde_tabs.
*&SPWIZARD: DATA FOR TABSTRIP 'CDE_TABS'
CONTROLS:  cde_tabs TYPE TABSTRIP.
DATA:      BEGIN OF g_cde_tabs,
             subscr      LIKE sy-dynnr VALUE '9231',
             pressed_tab LIKE sy-ucomm VALUE 'CDE_TAB1',
           END OF g_cde_tabs.
*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'CDE_TABS'

CONSTANTS: gc_ticket_sts_container_name    TYPE scrfname      VALUE 'TICKET_STS_CONTAINER',
           gc_zbas_c_alv_container_name    TYPE scrfname      VALUE 'ZBAS_C_ALV_CONTAINER',
           gc_zbas_d_alv_container_name    TYPE scrfname      VALUE 'ZBAS_D_ALV_CONTAINER',
           gc_zbas_e_alv_container_name    TYPE scrfname      VALUE 'ZBAS_E_ALV_CONTAINER',
           gc_control_pesos_container_nam  TYPE scrfname      VALUE 'CONTROL_PESOS_CONTAINER',
           gc_selected_status_row_color(4) TYPE c             VALUE 'C300',
           gc_default_gewei                TYPE zbas_h-gewei  VALUE 'KG',
           gc_popup_question               TYPE icon-name     VALUE 'ICON_MESSAGE_QUESTION',
           gc_popup_info                   TYPE icon-name     VALUE 'ICON_MESSAGE_INFORMATION',
           gc_popup_warning                TYPE icon-name     VALUE 'ICON_MESSAGE_WARNING',
           gc_popup_error                  TYPE icon-name     VALUE 'ICON_MESSAGE_ERROR',
           gc_popup_critical               TYPE icon-name     VALUE 'ICON_MESSAGE_CRITICAL',
           gc_popup_noicon                 TYPE icon-name     VALUE 'NO_ICON'.

*----------------------------------------------------------------------*
* CLASSES DECLARATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_status_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                  e_column_id.
    METHODS on_zbas_cde_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                  e_column
                  sender.
ENDCLASS.

IMPORT bastk = p_bastk
       bloqueo = gv_imp_bloqueo
       crear = gv_crear
       werks = p_werks
       FROM MEMORY ID 'ZBAS_MANAGER_PARAMS'.
FREE MEMORY ID 'ZBAS_MANAGER_PARAMS'.

IF p_bastk IS NOT INITIAL.
  PERFORM f_cargar_ticket USING    p_bastk
                                   gv_imp_bloqueo
                          CHANGING go_ticket.
ELSEIF gv_crear IS NOT INITIAL.
  PERFORM f_crear_ticket USING    p_werks
                         CHANGING go_ticket.
ELSE.
  DO.
    CALL SCREEN 9900 STARTING AT 15 5.
    IF sy-subrc IS NOT INITIAL.
      LEAVE PROGRAM.
    ENDIF.

    IF p_bastk IS NOT INITIAL.
      IF p_edita IS NOT INITIAL.
        MOVE zcl_bas_ticket=>c_bloqueo_escritura TO gv_imp_bloqueo.
      ELSEIF p_visua IS NOT INITIAL.
        MOVE zcl_bas_ticket=>c_bloqueo_lectura TO gv_imp_bloqueo.
      ENDIF.
      PERFORM f_cargar_ticket USING    p_bastk
                                       gv_imp_bloqueo
                              CHANGING go_ticket.
    ELSE.
      PERFORM f_crear_ticket USING    p_werks
                             CHANGING go_ticket.
    ENDIF.
    IF go_ticket IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
ENDIF.

IF go_ticket IS NOT INITIAL.
  PERFORM f_update_ticket_screen_data USING    go_ticket
                                               go_ticket_sts_alv_grid
                                      CHANGING gs_ticket_screen
                                               gt_status_list.

  CALL SCREEN 9000.
* CALL SCREEN 9110.
* CALL SCREEN 9120.
* CALL SCREEN 9130.
* CALL SCREEN 9140.
* CALL SCREEN 9150.
* CALL SCREEN 9160.
* CALL SCREEN 9199.
ENDIF.

INCLUDE zbas_nuevo_ticket_o01.
INCLUDE zbas_nuevo_ticket_i01.
INCLUDE zbas_nuevo_ticket_f01.