*----------------------------------------------------------------------*
***INCLUDE ZBAS_NUEVO_TICKET_I01.
*----------------------------------------------------------------------*
*&
*& Copyright (C) Lautaro Capella - All Rights Reserved
*& Unauthorized copying of this file, via any medium is strictly prohibited
*& Proprietary and confidential
*& Written by Lautaro Capella <laucape@gmail.com>, Aug 2020
*&
*&---------------------------------------------------------------------*
*&      Module  CANCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cancel INPUT.
  CASE gv_okcode.
    WHEN '&F15' OR '&F12'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  OKCODE_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9000 INPUT.
  CALL METHOD cl_gui_cfw=>dispatch.
  MOVE gv_okcode TO gv_okcode_9000.

  CASE gv_okcode_9000.
    WHEN 'ANULAR_TICKET'.
      PERFORM f_anular_ticket USING gs_ticket_screen.
    WHEN 'PASAR_STATUS_SIG'.
      PERFORM f_pasar_status_siguiente USING gs_ticket_screen.
    WHEN 'GUARDAR_TICKET'.
      PERFORM f_guardar_ticket USING gs_ticket_screen.
*    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR gv_okcode.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9110 INPUT.

  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'CR'
                               'No posee autorización para esta acción en este Centro'.
  ENDIF.

  CASE gv_okcode.
*    WHEN 'ANULAR_TICKET'.
    WHEN 'PASAR_STATUS_SIG'.
      PERFORM f_validar_campos_9110 USING gs_ticket_screen
                                    CHANGING gv_okcode.
*    WHEN 'GUARDAR_TICKET'.
*    WHEN OTHERS.
*     do nothing
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9120 INPUT.

  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'IE'
                               'No posee autorización para esta acción en este Centro'.
  ENDIF.

  CASE gv_okcode.
*    WHEN 'ANULAR_TICKET'.
    WHEN 'PASAR_STATUS_SIG'.
      IF gs_ticket_screen-check1_ing IS INITIAL
        OR gs_ticket_screen-check2_ing IS INITIAL
        OR gs_ticket_screen-bas_modo_ing IS INITIAL.
        MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR gv_okcode.
        RETURN.
      ENDIF.

      IF gs_ticket_screen-peso_ing IS INITIAL.
        MESSAGE 'Pesaje igual a CERO kg no permitido' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR gv_okcode.
        RETURN.
      ENDIF.

    WHEN 'BAS_MODE_ING'.  " Cambio de modo de báscula
      PERFORM f_popup_modif_basmodo USING    gs_ticket_screen-basid
                                             gs_ticket_screen-bastk
                                             gs_ticket_screen-werks
                                    CHANGING gs_ticket_screen-bas_modo_ing.

      PERFORM f_guardar_ticket USING gs_ticket_screen.

    WHEN 'CAPTURAR_PESAJE_ING'. " Botón: Capturar Pesaje
    WHEN 'CHECKS_INGRESO'. " Tildes de revisión
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9130 INPUT.

  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'CD'
                               'No posee autorización para esta acción en este Centro'.
  ENDIF.

  CASE gv_okcode.
    WHEN 'PASAR_STATUS_SIG'.
      IF gs_ticket_screen-tipo_carga IS INITIAL.
        MESSAGE 'Ingrese Tipo de Carga' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR gv_okcode.
        RETURN.
      ENDIF.

    WHEN 'ZBAS_C_AG_REMITO'.
      IF gs_zbas_c-belnr_r IS INITIAL
        OR gs_zbas_c-brgew IS INITIAL.
        MESSAGE 'Complete los campos requeridos: Tipo Embalaje y Cantidad'
          TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      CALL METHOD go_ticket->agregar_carga_remito
        EXPORTING
          i_belnr_r               = gs_zbas_c-belnr_r
          i_brgew                 = gs_zbas_c-brgew
*         i_gewei                 = 'KG'
        EXCEPTIONS
          no_encontrado           = 1
          no_permitido            = 2
          peso_no_positivo        = 3
          incluido_en_otro_ticket = 4
          OTHERS                  = 10.

    WHEN 'ZBAS_C_AG_TRANSPORTE'.
      IF gs_zbas_c-tknum_t IS INITIAL.
        MESSAGE 'Indique un Transporte'
          TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      CALL METHOD go_ticket->agregar_carga_transporte
        EXPORTING
          i_tknum_t        = gs_zbas_c-tknum_t
        EXCEPTIONS
          no_encontrado    = 1
          no_permitido     = 2
          peso_no_positivo = 3
          OTHERS           = 10.

    WHEN 'ZBAS_C_AG_ENTREGA'.
      IF gs_zbas_c-vbeln_e IS INITIAL.
        MESSAGE 'Indique una Entrega'
          TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      CALL METHOD go_ticket->agregar_carga_entrega
        EXPORTING
          i_vbeln_e               = gs_zbas_c-vbeln_e
        EXCEPTIONS
          no_encontrado           = 1
          no_permitido            = 2
          peso_no_positivo        = 3
          incluido_en_otro_ticket = 4
          OTHERS                  = 10.

    WHEN 'ZBAS_C_AG_TRASLADO'.
      IF gs_zbas_c-traslado IS INITIAL.
        MESSAGE 'Indique un Traslado'
          TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      CALL METHOD go_ticket->agregar_carga_traslado
        EXPORTING
          i_traslado              = gs_zbas_c-traslado
        EXCEPTIONS
          no_encontrado           = 1
          no_permitido            = 2
          peso_no_positivo        = 3
          incluido_en_otro_ticket = 4
          OTHERS                  = 10.

    WHEN 'ZBAS_D_AG_REMITO'.
      IF gs_zbas_d-belnr_r IS INITIAL
        OR gs_zbas_d-ebeln IS INITIAL
        OR gs_zbas_d-bedat IS INITIAL
        OR gs_zbas_d-brgew IS INITIAL.
        MESSAGE 'Complete los campos requeridos: Fe. Remito, N° Remito, OC y Peso'
          TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      CALL METHOD go_ticket->agregar_descarga_remito
        EXPORTING
          i_belnr_r        = gs_zbas_d-belnr_r
          i_ebeln          = gs_zbas_d-ebeln
          i_bedat          = gs_zbas_d-bedat
          i_brgew          = gs_zbas_d-brgew
*         i_gewei          = 'KG'
        EXCEPTIONS
          no_encontrado    = 1
          no_permitido     = 2
          peso_no_positivo = 3
          OTHERS           = 10.

    WHEN 'ZBAS_E_AGREGAR'.
      IF gs_zbas_e-tipo_emba IS INITIAL
        OR gs_zbas_e-menge IS INITIAL.
        MESSAGE 'Complete los campos requeridos: Tipo Embalaje y Cantidad'
          TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      CALL METHOD go_ticket->agregar_embalaje
        EXPORTING
          i_tipo_emba          = gs_zbas_e-tipo_emba
          i_menge              = gs_zbas_e-menge
        EXCEPTIONS
          embalaje_inexistente = 1
          no_permitido         = 2
          cant_no_positiva     = 3
          OTHERS               = 10.

    WHEN OTHERS.
      RETURN. "Evitar manejo de SY-SUBRC

  ENDCASE.

*MANEJO GENERAL DEL SY-SUBRC DE TODOS LOS MÉTODOS
  CASE sy-subrc.
    WHEN 0. " Ok!
      CASE gv_okcode.
        WHEN 'ZBAS_C_AG_REMITO'.
          MESSAGE 'Se ha agregado el Remito a la Carga' TYPE 'S'.
        WHEN 'ZBAS_C_AG_TRANSPORTE'.
          MESSAGE 'Se han agregado las Entregas del Transporte a la Carga' TYPE 'S'.
        WHEN 'ZBAS_C_AG_ENTREGA'.
          MESSAGE 'Se ha agregado la Entrega a la Carga' TYPE 'S'.
        WHEN 'ZBAS_C_AG_TRASLADO'.
          MESSAGE 'Se ha agregado el Traslado a la Carga' TYPE 'S'.
        WHEN 'ZBAS_D_AG_REMITO'.
          MESSAGE 'Se ha agregado el Remito a la Descarga' TYPE 'S'.
        WHEN 'ZBAS_E_AGREGAR'.
          MESSAGE 'Se ha agregado el Embalaje' TYPE 'S'.
        WHEN OTHERS.
      ENDCASE.
      CLEAR: gs_zbas_c,
             gs_zbas_d,
             gs_zbas_e.

    WHEN 1. " No Encontrado / Inexistente
      CASE gv_okcode.
        WHEN 'ZBAS_C_AG_REMITO'.
          MESSAGE 'No se ha encontrado el Remito indicado' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_C_AG_TRANSPORTE'.
          MESSAGE 'No se ha encontrado el Transporte indicado' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_C_AG_ENTREGA'.
          MESSAGE 'No se ha encontrado la Entrega indicado' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_C_AG_TRASLADO'.
          MESSAGE 'No se ha encontrado el Traslado indicado' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_D_AG_REMITO'.
          MESSAGE 'No se ha encontrado el Remito indicado' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_E_AGREGAR'.
          MESSAGE 'No se ha encontrado el Embalaje indicado' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN OTHERS.
      ENDCASE.

    WHEN 2. " No Permitido
      MESSAGE 'Accion no permitida' TYPE 'S' DISPLAY LIKE 'E'.

    WHEN 3. " Cant/Pedo no positivo
      CASE gv_okcode.
        WHEN 'ZBAS_C_AG_REMITO' OR 'ZBAS_C_AG_TRANSPORTE' OR 'ZBAS_C_AG_ENTREGA'
          OR 'ZBAS_C_AG_TRASLADO' OR 'ZBAS_D_AG_REMITO'.
          MESSAGE 'El Peso debe ser positivo' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_E_AGREGAR'.
          MESSAGE 'Ingrese una Cantidad positiva' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN OTHERS.
      ENDCASE.

    WHEN 4. " Incluido en otro ticket
      CASE gv_okcode.
        WHEN 'ZBAS_C_AG_REMITO'.
          MESSAGE 'El Remito indicado ya se encuentra registrado en otro Ticket' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_C_AG_ENTREGA'.
          MESSAGE 'La Entrega indicado ya se encuentra registrada en otro Ticket' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN 'ZBAS_C_AG_TRASLADO'.
          MESSAGE 'El Traslado indicado ya se encuentra registrado en otro Ticket' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN OTHERS.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9140 INPUT.

  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'IE'
                               'No posee autorización para esta acción en este Centro'.
  ENDIF.

  CASE gv_okcode.
*    WHEN 'ANULAR_TICKET'.
    WHEN 'PASAR_STATUS_SIG'.
      IF gs_ticket_screen-check1_egr IS INITIAL
        OR gs_ticket_screen-check2_egr IS INITIAL
        OR gs_ticket_screen-bas_modo_egr IS INITIAL.
        MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR gv_okcode.
        RETURN.
      ENDIF.

      IF gs_ticket_screen-peso_egr IS INITIAL.
        MESSAGE 'Pesaje igual a CERO kg no permitido' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR gv_okcode.
        RETURN.
      ENDIF.

    WHEN 'BAS_MODE_EGR'.  " Cambio de modo de báscula
      PERFORM f_popup_modif_basmodo USING    gs_ticket_screen-basid
                                             gs_ticket_screen-bastk
                                             gs_ticket_screen-werks
                                    CHANGING gs_ticket_screen-bas_modo_egr.

      PERFORM f_guardar_ticket USING gs_ticket_screen.

    WHEN 'CAPTURAR_PESAJE_EGR'. " Botón: Capturar Pesaje
    WHEN 'CHECKS_EGRESO'. " Tildes de revisión
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9150 INPUT.

  IF go_ticket->bloqueo EQ zcl_bas_ticket=>c_bloqueo_escritura.
    PERFORM f_auth_check USING 'A1'
                               'No posee autorización para esta acción en este Centro'.
  ENDIF.

  CASE gv_okcode.
    WHEN 'PASAR_STATUS_SIG'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9160 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9199 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FIELDS_9110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fields_9110 INPUT.
ENDMODULE.