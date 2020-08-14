*& Copyright (C) Lautaro Capella - All Rights Reserved
*& Unauthorized copying of this file, via any medium is strictly prohibited
*& Proprietary and confidential
*& Written by Lautaro Capella <laucape@gmail.com>, Aug 2020
*&
class ZCL_BAS_TICKET definition
  public
  final
  create public .

public section.

  constants C_GEWEI_DEFAULT type ZBAS_C-GEWEI value 'KG' ##NO_TEXT.
  data BLOQUEO type ENQMODE read-only .
  constants C_BLOQUEO_ESCRITURA type ENQMODE value 'E' ##NO_TEXT.
  constants C_BLOQUEO_LECTURA type ENQMODE value 'S' ##NO_TEXT.
  data BASTK type ZBAS_H-BASTK read-only .
  constants C_STS_INICIAL type ZBAS_TICKET_STATUS_T value 10 ##NO_TEXT.
  constants C_STS_INGRESO type ZBAS_TICKET_STATUS_T value 20 ##NO_TEXT.
  constants C_STS_CARGA_DESCARGA type ZBAS_TICKET_STATUS_T value 30 ##NO_TEXT.
  constants C_STS_EGRESO type ZBAS_TICKET_STATUS_T value 40 ##NO_TEXT.
  constants C_STS_AUTORIZARSALIDA type ZBAS_TICKET_STATUS_T value 50 ##NO_TEXT.
  constants C_STS_CERRADO type ZBAS_TICKET_STATUS_T value 60 ##NO_TEXT.
  constants C_STS_ANULADO type ZBAS_TICKET_STATUS_T value 99 ##NO_TEXT.

  methods GET_TOTALES_CDE
    exporting
      !E_TOTAL_CARGA type ZBAS_H-PESO_ING
      !E_TOTAL_DESCARGA type ZBAS_H-PESO_ING
      !E_TOTAL_EMBALAJE type ZBAS_H-PESO_ING
      !E_PESO_ING type ZBAS_H-PESO_ING
      !E_PESO_DOC type ZBAS_H-PESO_ING
      !E_PESO_EGR type ZBAS_H-PESO_ING
      !E_DIFERENCIA type ZBAS_H-PESO_ING
      !E_DIF_CARGA type ZBAS_H-PESO_ING
      !E_TOLERANCIA type ZBAS_TIPOCARGA-TOLERANCIA
      !E_TOLER_KG type ZBAS_H-PESO_ING
      !E_ERROR_BAS type ZBAS_H-PESO_ING
      !E_PESO_FUERA_TOL type ZBAS_H-PESO_ING
      !E_GEWEI type ZBAS_H-GEWEI
    exceptions
      UNIT_CONVERSION_ERROR .
  methods GET_EMBALAJES_TAB
    returning
      value(R_ZBAS_E) type ZBAS_E_TAB_T .
  methods GET_DESCARGA_TAB
    returning
      value(R_ZBAS_D) type ZBAS_D_TAB_T .
  methods GET_CARGA_TAB
    returning
      value(R_ZBAS_C) type ZBAS_C_TAB_T .
  methods CONSTRUCTOR
    importing
      !I_UNAME type SY-UNAME optional
    exceptions
      CONSTRUCTOR_ERROR .
  class-methods NUEVO_TICKET
    exporting
      !E_TICKET type ref to ZCL_BAS_TICKET
    exceptions
      ERROR_AL_CREAR
      ERROR_DE_RANGO
      ERROR_EN_DB .
  methods GET_STATUS_ACTUAL
    exporting
      !E_STATUS_ACTUAL type ZBAS_S
    returning
      value(R_BASSTS) type ZBAS_TICKET_STATUS_T
    exceptions
      ERROR_INTERNO .
  methods STATUS_SIGUIENTE
    returning
      value(R_BASSTS) type ZBAS_TICKET_STATUS_T
    exceptions
      ERROR_INTERNO
      STATUS_FINAL .
  methods REQUIERE_AUTORIZAR_SALIDA
    returning
      value(R_REQ_AUT_SAL) type CHAR1 .
  methods PASAR_A_STATUS_SIGUIENTE
    returning
      value(R_BASSTS) type ZBAS_TICKET_STATUS_T
    exceptions
      ERROR_EN_DB
      NO_PERMITIDO .
  class-methods CARGAR_TICKET
    importing
      !I_BASTK type ZBAS_TICKET_NR_T
      value(I_BLOQUEO) type ENQMODE default 'S'
    exporting
      !E_TICKET type ref to ZCL_BAS_TICKET
    exceptions
      ERROR_AL_CREAR
      TICKET_INEXISTENTE
      FOREIGN_LOCK
      LOCK_STATUS_ERROR .
  methods GET_STATUS_TAB
    returning
      value(R_STATUS_TAB) type ZBAS_TICKET_STATUS_TEXT_TAB_T .
  methods GET_HEAD_DATA
    returning
      value(R_HEADER) type ZBAS_H .
  class-methods GET_STATUS_TEXT
    importing
      !I_BASSTS type ZBAS_TICKET_STATUS_T
    returning
      value(R_STSTEXT) type STRING
    exceptions
      NOT_FOUND .
  methods ANULAR
    returning
      value(R_BASSTS) type ZBAS_S-BASSTS
    exceptions
      NO_PERMITIDO
      ERROR_EN_DB .
  class-methods GET_STATUS_ICON
    importing
      !I_BASSTS type ZBAS_TICKET_STATUS_T
    returning
      value(R_STSICON) type ICON_D
    exceptions
      NOT_FOUND .
  methods VALIDAR_STATUS_COMPLETO
    exceptions
      STATUS_INCOMPLETE
      ERROR_INTERNO .
  methods SET_HEAD_DATA
    importing
      !I_DATA type ZBAS_H
    exceptions
      NO_PERMITIDO .
  methods AGREGAR_CARGA_REMITO
    importing
      !I_BELNR_R type ZBAS_C-BELNR_R
      !I_BRGEW type ZBAS_C-BRGEW
      !I_GEWEI type ZBAS_C-GEWEI default 'KG'
    exceptions
      NO_ENCONTRADO
      NO_PERMITIDO
      PESO_NO_POSITIVO
      INCLUIDO_EN_OTRO_TICKET .
  methods AGREGAR_CARGA_ENTREGA
    importing
      !I_VBELN_E type ZBAS_C-VBELN_E
      !I_TKNUM_T type ZBAS_C-TKNUM_T optional
    exceptions
      NO_ENCONTRADO
      NO_PERMITIDO
      PESO_NO_POSITIVO
      INCLUIDO_EN_OTRO_TICKET .
  methods AGREGAR_CARGA_TRANSPORTE
    importing
      !I_TKNUM_T type ZBAS_C-TKNUM_T
    exceptions
      NO_ENCONTRADO
      NO_PERMITIDO
      PESO_NO_POSITIVO .
  methods AGREGAR_CARGA_TRASLADO
    importing
      !I_TRASLADO type ZZCAJA-TRASLADO
    exceptions
      NO_ENCONTRADO
      NO_PERMITIDO
      PESO_NO_POSITIVO
      INCLUIDO_EN_OTRO_TICKET .
  methods BORRAR_CARGA
    importing
      !I_POSNR type ZBAS_C-POSNR
    exceptions
      NO_PERMITIDO .
  methods AGREGAR_DESCARGA_REMITO
    importing
      !I_BELNR_R type ZBAS_D-BELNR_R
      !I_EBELN type ZBAS_D-EBELN
      !I_BEDAT type ZBAS_D-BEDAT
      !I_BRGEW type ZBAS_D-BRGEW
      value(I_GEWEI) type ZBAS_D-GEWEI default 'KG'
    exceptions
      NO_ENCONTRADO
      NO_PERMITIDO
      PESO_NO_POSITIVO .
  methods BORRAR_DESCARGA
    importing
      !I_POSNR type ZBAS_C-POSNR
    exceptions
      NO_PERMITIDO .
  methods AGREGAR_EMBALAJE
    importing
      !I_TIPO_EMBA type ZBAS_E-TIPO_EMBA
      !I_MENGE type ZBAS_E-MENGE
    exceptions
      EMBALAJE_INEXISTENTE
      NO_PERMITIDO
      CANT_NO_POSITIVA .
  methods BORRAR_EMBALAJE
    importing
      !I_POSNR type ZBAS_C-POSNR
    exceptions
      NO_PERMITIDO .
protected section.
private section.

  data CDE_POSNR type POSNR value 0 ##NO_TEXT.
  data H type ZBAS_H .
  data S type ZBAS_TICKET_STATUS_TAB_T .
  data C type ZBAS_C_TAB_T .
  data D type ZBAS_D_TAB_T .
  data E type ZBAS_E_TAB_T .

  methods NEXT_CDE_POS
    returning
      value(R_POSNR) type POSNR .
  methods ENQUEUE
    importing
      value(I_BLOQUEO) type ENQMODE default 'S'
    exceptions
      FOREIGN_LOCK
      LOCK_STATUS_ERROR .
  methods DEQUEUE
    importing
      value(I_BLOQUEO) type ENQMODE default 'S'
    exceptions
      FOREIGN_LOCK .
  methods UPDATE_H
    raising
      CX_SQL_EXCEPTION .
  methods UPDATE_S
    raising
      CX_SQL_EXCEPTION .
  methods UPDATE_C
    raising
      CX_SQL_EXCEPTION .
  methods UPDATE_D
    raising
      CX_SQL_EXCEPTION .
  methods UPDATE_E
    raising
      CX_SQL_EXCEPTION .
ENDCLASS.



CLASS ZCL_BAS_TICKET IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->AGREGAR_CARGA_ENTREGA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VBELN_E                      TYPE        ZBAS_C-VBELN_E
* | [--->] I_TKNUM_T                      TYPE        ZBAS_C-TKNUM_T(optional)
* | [EXC!] NO_ENCONTRADO
* | [EXC!] NO_PERMITIDO
* | [EXC!] PESO_NO_POSITIVO
* | [EXC!] INCLUIDO_EN_OTRO_TICKET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD agregar_carga_entrega.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    SELECT SINGLE vbeln, btgew, gewei
      FROM likp
      INTO @DATA(wa_likp)
      WHERE vbeln EQ @i_vbeln_e
        AND werks eq @me->h-werks.

    IF sy-subrc IS NOT INITIAL.
      RAISE no_encontrado.
    ENDIF.

    IF wa_likp-btgew LT 1.
      RAISE peso_no_positivo.
    ENDIF.

*   No puede la entrega incluirse en más de un ticket
    SELECT SINGLE vbeln_e
      FROM zbas_c
      INTO @DATA(lv_vbeln)
      WHERE vbeln_e EQ @i_vbeln_e.

    IF sy-subrc IS INITIAL.
      RAISE incluido_en_otro_ticket.
    ENDIF.

    DATA: wa_zbas_c TYPE zbas_c.

    MOVE: me->bastk           TO wa_zbas_c-bastk,
          me->next_cde_pos( ) TO wa_zbas_c-posnr,
          wa_likp-vbeln       TO wa_zbas_c-vbeln_e,
          wa_likp-btgew       TO wa_zbas_c-brgew,
          wa_likp-gewei       TO wa_zbas_c-gewei,
          sy-datum            TO wa_zbas_c-erdat,
          sy-uzeit            TO wa_zbas_c-erzet,
          sy-uname            TO wa_zbas_c-ernam.

    IF i_tknum_t IS SUPPLIED.
      MOVE i_tknum_t TO wa_zbas_c-tknum_t.
    ENDIF.

    APPEND wa_zbas_c TO me->c.
    me->update_c( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->AGREGAR_CARGA_REMITO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BELNR_R                      TYPE        ZBAS_C-BELNR_R
* | [--->] I_BRGEW                        TYPE        ZBAS_C-BRGEW
* | [--->] I_GEWEI                        TYPE        ZBAS_C-GEWEI (default ='KG')
* | [EXC!] NO_ENCONTRADO
* | [EXC!] NO_PERMITIDO
* | [EXC!] PESO_NO_POSITIVO
* | [EXC!] INCLUIDO_EN_OTRO_TICKET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD agregar_carga_remito.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    SELECT SINGLE belnr_r
      FROM zmm_remitos
      INTO @DATA(lv_belnr_r)
      WHERE belnr_r EQ @i_belnr_r
        AND origen  EQ @me->h-werks.

    IF sy-subrc IS NOT INITIAL.
      RAISE no_encontrado.
    ENDIF.

    IF i_brgew LT 1.
      RAISE peso_no_positivo.
    ENDIF.

*   No puede el remito incluirse en más de un ticket
    SELECT SINGLE belnr_r
      FROM zbas_c
      INTO lv_belnr_r
      WHERE belnr_r EQ lv_belnr_r.

    IF sy-subrc IS INITIAL.
      RAISE incluido_en_otro_ticket.
    ENDIF.

    DATA: wa_zbas_c TYPE zbas_c.

    MOVE: me->bastk           TO wa_zbas_c-bastk,
          me->next_cde_pos( ) TO wa_zbas_c-posnr,
          i_belnr_r           TO wa_zbas_c-belnr_r,
          i_brgew             TO wa_zbas_c-brgew,
          i_gewei             TO wa_zbas_c-gewei,
          sy-datum            TO wa_zbas_c-erdat,
          sy-uzeit            TO wa_zbas_c-erzet,
          sy-uname            TO wa_zbas_c-ernam.

    APPEND wa_zbas_c TO me->c.
    me->update_c( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->AGREGAR_CARGA_TRANSPORTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TKNUM_T                      TYPE        ZBAS_C-TKNUM_T
* | [EXC!] NO_ENCONTRADO
* | [EXC!] NO_PERMITIDO
* | [EXC!] PESO_NO_POSITIVO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD agregar_carga_transporte.
    DATA: lv_error TYPE c.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    SELECT tknum, vbeln
      FROM vttp
      INTO TABLE @DATA(lt_vttp)
      WHERE tknum EQ @i_tknum_t.

    IF sy-subrc IS NOT INITIAL.
      RAISE no_encontrado.
    ENDIF.

    SORT lt_vttp BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vttp COMPARING vbeln.

    LOOP AT lt_vttp INTO DATA(wa_vttp).

      CALL METHOD me->agregar_carga_entrega
        EXPORTING
          i_vbeln_e               = wa_vttp-vbeln
          i_tknum_t               = wa_vttp-tknum
        EXCEPTIONS
          no_encontrado           = 1
          no_permitido            = 2
          peso_no_positivo        = 3
          incluido_en_otro_ticket = 4
          OTHERS                  = 5.

*      IF sy-subrc IS INITIAL.
*        wa_log-status = 3.
*        wa_log-msg = |La Entrega { wa_vttp-vbeln } fue agregada correctamente|.
*      ELSE.
*        MOVE 'X' TO lv_error.
*        IF sy-subrc EQ 1.
*          wa_log-status = 1.
*          wa_log-msg = |La Entrega { wa_vttp-vbeln } no existe|.
*        ELSEIF sy-subrc EQ 3.
*          wa_log-status = 1.
*          wa_log-msg = |La Entrega { wa_vttp-vbeln } no tiene peso no positivo|.
*        ELSEIF sy-subrc EQ 4.
*          wa_log-status = 1.
*          wa_log-msg = |La Entrega { wa_vttp-vbeln } ya fue registrada en otro Ticket|.
*        ENDIF.
*      ENDIF.
*
*      APPEND wa_log TO lt_log.
*      CLEAR wa_log.

    ENDLOOP.

*    IF lv_error IS NOT INITIAL.
*      MOVE lt_log TO e_log.
*    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->AGREGAR_CARGA_TRASLADO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TRASLADO                     TYPE        ZZCAJA-TRASLADO
* | [EXC!] NO_ENCONTRADO
* | [EXC!] NO_PERMITIDO
* | [EXC!] PESO_NO_POSITIVO
* | [EXC!] INCLUIDO_EN_OTRO_TICKET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD agregar_carga_traslado.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    SELECT SINGLE traslado, ntgew, tarag, meins
      FROM zzcaja
      INTO @DATA(wa_zzcaja)
      WHERE traslado EQ @i_traslado
        AND werks    EQ @me->h-werks.

    IF sy-subrc IS NOT INITIAL.
      RAISE no_encontrado.
    ENDIF.

    IF wa_zzcaja-ntgew LT 1.
      RAISE peso_no_positivo.
    ENDIF.

*   No puede el traslado incluirse en más de un ticket
    SELECT SINGLE traslado
      FROM zbas_c
      INTO @DATA(lv_traslado)
      WHERE traslado EQ @i_traslado.

    IF sy-subrc IS INITIAL.
      RAISE incluido_en_otro_ticket.
    ENDIF.

    DATA: wa_zbas_c TYPE zbas_c.

    MOVE: me->bastk           TO wa_zbas_c-bastk,
          me->next_cde_pos( ) TO wa_zbas_c-posnr,
          wa_zzcaja-traslado  TO wa_zbas_c-traslado,
          wa_zzcaja-meins     TO wa_zbas_c-gewei,
          sy-datum            TO wa_zbas_c-erdat,
          sy-uzeit            TO wa_zbas_c-erzet,
          sy-uname            TO wa_zbas_c-ernam.

    wa_zbas_c-brgew = wa_zzcaja-ntgew + wa_zzcaja-tarag.

    APPEND wa_zbas_c TO me->c.
    me->update_c( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->AGREGAR_DESCARGA_REMITO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BELNR_R                      TYPE        ZBAS_D-BELNR_R
* | [--->] I_EBELN                        TYPE        ZBAS_D-EBELN
* | [--->] I_BEDAT                        TYPE        ZBAS_D-BEDAT
* | [--->] I_BRGEW                        TYPE        ZBAS_D-BRGEW
* | [--->] I_GEWEI                        TYPE        ZBAS_D-GEWEI (default ='KG')
* | [EXC!] NO_ENCONTRADO
* | [EXC!] NO_PERMITIDO
* | [EXC!] PESO_NO_POSITIVO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD agregar_descarga_remito.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    SELECT SINGLE ebeln
      FROM ekko
      INTO @DATA(lv_ebeln)
      WHERE ebeln EQ @i_ebeln.

    IF sy-subrc IS NOT INITIAL.
      RAISE no_encontrado.
    ENDIF.

    IF i_brgew LT 1.
      RAISE peso_no_positivo.
    ENDIF.

    DATA: wa_zbas_d TYPE zbas_d.

    MOVE: me->bastk             TO wa_zbas_d-bastk,
          me->next_cde_pos( )   TO wa_zbas_d-posnr,
          i_belnr_r             TO wa_zbas_d-belnr_r,
          i_ebeln               TO wa_zbas_d-ebeln,
          i_bedat               TO wa_zbas_d-bedat,
          i_brgew               TO wa_zbas_d-brgew,
          i_gewei               TO wa_zbas_d-gewei,
          sy-datum              TO wa_zbas_d-erdat,
          sy-uzeit              TO wa_zbas_d-erzet,
          sy-uname              TO wa_zbas_d-ernam.


    APPEND wa_zbas_d TO me->d.
    me->update_d( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->AGREGAR_EMBALAJE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TIPO_EMBA                    TYPE        ZBAS_E-TIPO_EMBA
* | [--->] I_MENGE                        TYPE        ZBAS_E-MENGE
* | [EXC!] EMBALAJE_INEXISTENTE
* | [EXC!] NO_PERMITIDO
* | [EXC!] CANT_NO_POSITIVA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD agregar_embalaje.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    SELECT SINGLE *
      FROM zbas_tipoemba
      INTO @DATA(wa_zbas_tipoemba)
      WHERE tipoembalaje EQ @i_tipo_emba.

    IF sy-subrc IS NOT INITIAL.
      RAISE embalaje_inexistente.
    ENDIF.

    IF i_menge LT 1.
      RAISE cant_no_positiva.
    ENDIF.

    DATA: wa_zbas_e TYPE zbas_e.

    MOVE: me->bastk               TO wa_zbas_e-bastk,
          me->next_cde_pos( )     TO wa_zbas_e-posnr,
          i_tipo_emba             TO wa_zbas_e-tipo_emba,
          i_menge                 TO wa_zbas_e-menge,
          wa_zbas_tipoemba-gewei  TO wa_zbas_e-gewei,
          sy-datum                TO wa_zbas_e-erdat,
          sy-uzeit                TO wa_zbas_e-erzet,
          sy-uname                TO wa_zbas_e-ernam.

    wa_zbas_e-brgew = wa_zbas_tipoemba-brgew * i_menge.

    APPEND wa_zbas_e TO me->e.
    me->update_e( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->ANULAR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_BASSTS                       TYPE        ZBAS_S-BASSTS
* | [EXC!] NO_PERMITIDO
* | [EXC!] ERROR_EN_DB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD anular.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

    IF me->get_status_actual( ) EQ c_sts_anulado
      or me->get_status_actual( ) EQ c_sts_cerrado.
      RAISE no_permitido.
    ENDIF.

*   Nuevo Status Anulado
    DATA: wa_s TYPE zbas_s.
    MOVE: me->h-bastk               TO wa_s-bastk,
          c_sts_anulado             TO wa_s-bassts,
          sy-datum                  TO wa_s-erdat,
          sy-uzeit                  TO wa_s-erzet,
          sy-uname                  TO wa_s-ernam.

    APPEND wa_s TO me->s.

*   Update DB
    TRY.
        me->update_s( ).
      CATCH cx_sql_exception.
        RAISE error_en_db.
    ENDTRY.

    r_bassts = me->get_status_actual( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->BORRAR_CARGA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_POSNR                        TYPE        ZBAS_C-POSNR
* | [EXC!] NO_PERMITIDO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD borrar_carga.
*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    READ TABLE me->c WITH KEY posnr = i_posnr
      TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    DELETE me->c INDEX sy-tabix.
    me->update_c( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->BORRAR_DESCARGA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_POSNR                        TYPE        ZBAS_C-POSNR
* | [EXC!] NO_PERMITIDO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD borrar_descarga.
*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    READ TABLE me->d WITH KEY posnr = i_posnr
      TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    DELETE me->d INDEX sy-tabix.
    me->update_d( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->BORRAR_EMBALAJE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_POSNR                        TYPE        ZBAS_C-POSNR
* | [EXC!] NO_PERMITIDO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD borrar_embalaje.
*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Status Carga/Descarga?
    IF me->get_status_actual( ) NE c_sts_carga_descarga.
      RAISE no_permitido.
    ENDIF.

    READ TABLE me->e WITH KEY posnr = i_posnr
      TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    DELETE me->e INDEX sy-tabix.
    me->update_e( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS_TICKET=>CARGAR_TICKET
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BASTK                        TYPE        ZBAS_TICKET_NR_T
* | [--->] I_BLOQUEO                      TYPE        ENQMODE (default ='S')
* | [<---] E_TICKET                       TYPE REF TO ZCL_BAS_TICKET
* | [EXC!] ERROR_AL_CREAR
* | [EXC!] TICKET_INEXISTENTE
* | [EXC!] FOREIGN_LOCK
* | [EXC!] LOCK_STATUS_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD cargar_ticket.
  DATA: lv_bastk        TYPE zbas_h-bastk,
        lo_nuevo_ticket TYPE REF TO zcl_bas_ticket.

* Nuevo Objeto vacío
  CREATE OBJECT lo_nuevo_ticket
    EXCEPTIONS
      constructor_error = 1.

  IF sy-subrc IS  NOT INITIAL.
    RAISE error_al_crear.
  ENDIF.

  e_ticket ?= lo_nuevo_ticket.

* Conversion Nr. Ticket
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_bastk
    IMPORTING
      output = lo_nuevo_ticket->bastk.

* Recuperar Cabecera
  SELECT SINGLE *
    FROM zbas_h
    INTO lo_nuevo_ticket->h
    WHERE bastk EQ lo_nuevo_ticket->bastk.

* Error: Ticket No Existe
  IF sy-subrc IS NOT INITIAL.
    lo_nuevo_ticket->dequeue( ).
    FREE e_ticket.
    RAISE ticket_inexistente.
  ENDIF.

* Recuperar Status
  SELECT *
    FROM zbas_s
    INTO TABLE lo_nuevo_ticket->s
    WHERE bastk EQ lo_nuevo_ticket->bastk.

  IF sy-subrc IS NOT INITIAL.
    lo_nuevo_ticket->dequeue( ).
    FREE e_ticket.
    RAISE ticket_inexistente.
  ENDIF.

* Recuperar Cargas
  SELECT *
    FROM zbas_c
    INTO TABLE lo_nuevo_ticket->c
    WHERE bastk EQ lo_nuevo_ticket->bastk.

* Recuperar Descargas
  SELECT *
    FROM zbas_d
    INTO TABLE lo_nuevo_ticket->d
    WHERE bastk EQ lo_nuevo_ticket->bastk.

* Recuperar Embalajes
  SELECT *
    FROM zbas_e
    INTO TABLE lo_nuevo_ticket->e
    WHERE bastk EQ lo_nuevo_ticket->bastk.

* Max Posnr de Carga/Descarga/Embalajes
  lo_nuevo_ticket->cde_posnr = 0.
  LOOP AT lo_nuevo_ticket->c INTO DATA(wa_c).
    IF wa_c-posnr GT lo_nuevo_ticket->cde_posnr.
      MOVE wa_c-posnr TO lo_nuevo_ticket->cde_posnr.
    ENDIF.
  ENDLOOP.
  LOOP AT lo_nuevo_ticket->d INTO DATA(wa_d).
    IF wa_d-posnr GT lo_nuevo_ticket->cde_posnr.
      MOVE wa_d-posnr TO lo_nuevo_ticket->cde_posnr.
    ENDIF.
  ENDLOOP.
  LOOP AT lo_nuevo_ticket->e INTO DATA(wa_e).
    IF wa_e-posnr GT lo_nuevo_ticket->cde_posnr.
      MOVE wa_e-posnr TO lo_nuevo_ticket->cde_posnr.
    ENDIF.
  ENDLOOP.


* Tipos de bloqueos permitidos (escritura o lectura)
  IF i_bloqueo NE c_bloqueo_lectura
  AND i_bloqueo NE c_bloqueo_escritura.
    MOVE c_bloqueo_lectura TO i_bloqueo.
  ENDIF.

* Intentar Bloquear ticket
  lo_nuevo_ticket->enqueue( EXPORTING i_bloqueo = i_bloqueo
                            EXCEPTIONS foreign_lock      = 1
                                       lock_status_error = 2 ).

* Error: Objeto bloqueado
  IF sy-subrc EQ 1.
    FREE e_ticket.
    RAISE foreign_lock.

  ELSEIF sy-subrc EQ 2.
    FREE e_ticket.
    RAISE lock_status_error.
  ENDIF.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_UNAME                        TYPE        SY-UNAME(optional)
* | [EXC!] CONSTRUCTOR_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->DEQUEUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BLOQUEO                      TYPE        ENQMODE (default ='S')
* | [EXC!] FOREIGN_LOCK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dequeue.

    CHECK me->h-bastk IS NOT INITIAL.

    CALL FUNCTION 'DEQUEUE_EZ_BAS_TICKET'
      EXPORTING
        mode_zbas_h = me->bloqueo
        mode_zbas_s = me->bloqueo
        bastk       = me->h-bastk.

    CLEAR me->bloqueo.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->ENQUEUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BLOQUEO                      TYPE        ENQMODE (default ='S')
* | [EXC!] FOREIGN_LOCK
* | [EXC!] LOCK_STATUS_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD enqueue.

    CHECK me->h-bastk IS NOT INITIAL.

    IF i_bloqueo EQ c_bloqueo_escritura
      AND ( me->get_status_actual( ) EQ c_sts_cerrado
        OR  me->get_status_actual( ) EQ c_sts_anulado ).
      RAISE lock_status_error.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_EZ_BAS_TICKET'
      EXPORTING
        mode_zbas_h    = i_bloqueo
        mode_zbas_s    = i_bloqueo
        bastk          = me->h-bastk
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS INITIAL.
      MOVE i_bloqueo TO me->bloqueo.
    ELSEIF sy-subrc EQ 1.
      RAISE foreign_lock.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->GET_CARGA_TAB
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_ZBAS_C                       TYPE        ZBAS_C_TAB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_carga_tab.
    r_zbas_c = me->c[].
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->GET_DESCARGA_TAB
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_ZBAS_D                       TYPE        ZBAS_D_TAB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_DESCARGA_TAB.
    r_zbas_d = me->d[].
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->GET_EMBALAJES_TAB
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_ZBAS_E                       TYPE        ZBAS_E_TAB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_EMBALAJES_TAB.
    r_zbas_e = me->e[].
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->GET_HEAD_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_HEADER                       TYPE        ZBAS_H
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_HEAD_DATA.

    MOVE me->h TO r_header.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->GET_STATUS_ACTUAL
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_STATUS_ACTUAL                TYPE        ZBAS_S
* | [<-()] R_BASSTS                       TYPE        ZBAS_TICKET_STATUS_T
* | [EXC!] ERROR_INTERNO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_STATUS_ACTUAL.

    DATA wa_last_status TYPE zbas_s.

    LOOP AT me->s INTO DATA(wa_s).
      IF ( wa_s-erdat GT wa_last_status-erdat )
        OR ( wa_s-erdat  EQ wa_last_status-erdat
         AND wa_s-erzet  GT wa_last_status-erzet )
        OR ( wa_s-erdat  EQ wa_last_status-erdat
         AND wa_s-erzet  EQ wa_last_status-erzet
         AND wa_s-bassts GT wa_last_status-bassts ).

        MOVE wa_s TO wa_last_status.
      ENDIF.
    ENDLOOP.

    MOVE: wa_last_status        TO e_status_actual,
          wa_last_status-bassts TO r_bassts.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS_TICKET=>GET_STATUS_ICON
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BASSTS                       TYPE        ZBAS_TICKET_STATUS_T
* | [<-()] R_STSICON                      TYPE        ICON_D
* | [EXC!] NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_status_icon.

    CASE i_bassts.
      WHEN c_sts_inicial.
        MOVE '@0Y@' TO r_stsicon.
      WHEN c_sts_ingreso.
        MOVE '@4A@' TO r_stsicon.
      WHEN c_sts_carga_descarga.
        MOVE '@7V@' TO r_stsicon.
      WHEN c_sts_egreso.
        MOVE '@7Q@' TO r_stsicon.
      WHEN c_sts_autorizarsalida.
        MOVE '@8N@' TO r_stsicon.
      WHEN c_sts_cerrado.
        MOVE '@W5@' TO r_stsicon.
      WHEN c_sts_anulado.
        MOVE '@W7@' TO r_stsicon.
      WHEN OTHERS.
        MOVE '@35@' TO r_stsicon.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->GET_STATUS_TAB
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_STATUS_TAB                   TYPE        ZBAS_TICKET_STATUS_TEXT_TAB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_status_tab.
    DATA: wa_status_tab TYPE zbas_ticket_status_text_t,
          lt_status_tab TYPE zbas_ticket_status_text_tab_t.

    LOOP AT me->s INTO DATA(wa_s).
      MOVE-CORRESPONDING wa_s TO wa_status_tab.
      wa_status_tab-ststxt = zcl_bas_ticket=>get_status_text( wa_s-bassts ).
      wa_status_tab-stsicon = zcl_bas_ticket=>get_status_icon( wa_s-bassts ).
      APPEND wa_status_tab TO lt_status_tab.
    ENDLOOP.

    IF sy-subrc IS INITIAL.
      MOVE lt_status_tab[] TO r_status_tab[].
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS_TICKET=>GET_STATUS_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BASSTS                       TYPE        ZBAS_TICKET_STATUS_T
* | [<-()] R_STSTEXT                      TYPE        STRING
* | [EXC!] NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_status_text.
    CONSTANTS lc_domain_name TYPE dd01l-domname VALUE 'ZBAS_TICKET_STATUS_D'.
    STATICS: lt_domain_a TYPE STANDARD TABLE OF dd07v,
             lt_domain_n TYPE STANDARD TABLE OF dd07v.

    IF lt_domain_a[] IS INITIAL.
      CALL FUNCTION 'DD_DOMA_GET'
        EXPORTING
          domain_name   = lc_domain_name
        TABLES
          dd07v_tab_a   = lt_domain_a
          dd07v_tab_n   = lt_domain_n
        EXCEPTIONS
          illegal_value = 1
          op_failure    = 2
          OTHERS        = 3.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE lt_domain_a INTO DATA(wa_domain) WITH KEY domname    = lc_domain_name
                                                         domvalue_l = i_bassts
                                                         TRANSPORTING ddtext.
    IF sy-subrc IS INITIAL.
      MOVE wa_domain-ddtext TO r_ststext.
    ELSE.
      RAISE not_found.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->GET_TOTALES_CDE
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_TOTAL_CARGA                  TYPE        ZBAS_H-PESO_ING
* | [<---] E_TOTAL_DESCARGA               TYPE        ZBAS_H-PESO_ING
* | [<---] E_TOTAL_EMBALAJE               TYPE        ZBAS_H-PESO_ING
* | [<---] E_PESO_ING                     TYPE        ZBAS_H-PESO_ING
* | [<---] E_PESO_DOC                     TYPE        ZBAS_H-PESO_ING
* | [<---] E_PESO_EGR                     TYPE        ZBAS_H-PESO_ING
* | [<---] E_DIFERENCIA                   TYPE        ZBAS_H-PESO_ING
* | [<---] E_DIF_CARGA                    TYPE        ZBAS_H-PESO_ING
* | [<---] E_TOLERANCIA                   TYPE        ZBAS_TIPOCARGA-TOLERANCIA
* | [<---] E_TOLER_KG                     TYPE        ZBAS_H-PESO_ING
* | [<---] E_ERROR_BAS                    TYPE        ZBAS_H-PESO_ING
* | [<---] E_PESO_FUERA_TOL               TYPE        ZBAS_H-PESO_ING
* | [<---] E_GEWEI                        TYPE        ZBAS_H-GEWEI
* | [EXC!] UNIT_CONVERSION_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_totales_cde.

    DATA: lv_brgew_convertido TYPE zbas_c-brgew.

    MOVE: 0               TO e_total_carga,
          0               TO e_total_descarga,
          0               TO e_total_embalaje,
          c_gewei_default TO e_gewei.

      LOOP AT me->c INTO DATA(wa_c).
        IF wa_c-gewei EQ c_gewei_default.
          ADD wa_c-brgew TO e_total_carga.
        ELSE.
          CLEAR lv_brgew_convertido.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input                = wa_c-brgew
              unit_in              = wa_c-gewei
              unit_out             = c_gewei_default
            IMPORTING
              output               = lv_brgew_convertido
            EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              OTHERS               = 10.
          IF sy-subrc IS NOT INITIAL.
            RAISE unit_conversion_error.
          ENDIF.
          ADD lv_brgew_convertido TO e_total_carga.
        ENDIF.
      ENDLOOP.

      LOOP AT me->d INTO DATA(wa_d).
        IF wa_d-gewei EQ c_gewei_default.
          ADD wa_d-brgew TO e_total_descarga.
        ELSE.
          CLEAR lv_brgew_convertido.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input                = wa_d-brgew
              unit_in              = wa_d-gewei
              unit_out             = c_gewei_default
            IMPORTING
              output               = lv_brgew_convertido
            EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              OTHERS               = 10.
          IF sy-subrc IS NOT INITIAL.
            RAISE unit_conversion_error.
          ENDIF.
          ADD lv_brgew_convertido TO e_total_descarga.
        ENDIF.
      ENDLOOP.

      LOOP AT me->e INTO DATA(wa_e).
        IF wa_e-gewei EQ c_gewei_default.
          ADD wa_e-brgew TO e_total_embalaje.
        ELSE.
          CLEAR lv_brgew_convertido.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input                = wa_e-brgew
              unit_in              = wa_e-gewei
              unit_out             = c_gewei_default
            IMPORTING
              output               = lv_brgew_convertido
            EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              OTHERS               = 10.
          IF sy-subrc IS NOT INITIAL.
            RAISE unit_conversion_error.
          ENDIF.
          ADD lv_brgew_convertido TO e_total_embalaje.
        ENDIF.
      ENDLOOP.

  zcl_bas=>get_tipocarga( EXPORTING i_tipocarga = me->h-tipo_carga
                          IMPORTING e_tolerancia = e_tolerancia ).

  e_peso_ing        = me->h-peso_ing.
  e_peso_doc        = e_total_carga - e_total_descarga.
  e_peso_egr        = me->h-peso_egr.
  e_diferencia      = abs( e_peso_egr - e_peso_ing ).
  e_dif_carga       = abs( e_diferencia - e_peso_doc - e_total_embalaje ).
  e_toler_kg        = e_peso_doc * e_tolerancia / 100.
  e_error_bas       = 10.
  e_peso_fuera_tol  = e_dif_carga - e_toler_kg - e_error_bas.

  IF e_peso_fuera_tol LT 0.
    e_peso_fuera_tol = 0.
  ENDIF.
*
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->NEXT_CDE_POS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_POSNR                        TYPE        POSNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD next_cde_pos.

    ADD 10 TO me->cde_posnr.
    r_posnr = me->cde_posnr.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS_TICKET=>NUEVO_TICKET
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_TICKET                       TYPE REF TO ZCL_BAS_TICKET
* | [EXC!] ERROR_AL_CREAR
* | [EXC!] ERROR_DE_RANGO
* | [EXC!] ERROR_EN_DB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD nuevo_ticket.
    DATA: lo_nuevo_ticket TYPE REF TO zcl_bas_ticket.

*   Nuevo Objeto vacío
    CREATE OBJECT lo_nuevo_ticket
      EXCEPTIONS
        constructor_error = 1.

    IF sy-subrc IS  NOT INITIAL.
      RAISE error_al_crear.
    ENDIF.

    e_ticket ?= lo_nuevo_ticket.

*   Obtener nuevo numero del Rango
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZBAS_TKT_R'
        quantity                = '1'
      IMPORTING
        number                  = lo_nuevo_ticket->h-bastk
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

*   Error: Error en Rango
    IF sy-subrc IS NOT INITIAL.
      RAISE error_de_rango.
    ENDIF.

*   Crear nuevo Status inicial
    DATA: wa_s TYPE zbas_s.
    MOVE: lo_nuevo_ticket->h-bastk  TO wa_s-bastk,
          c_sts_inicial             TO wa_s-bassts,
          sy-datum                  TO wa_s-erdat,
          sy-uzeit                  TO wa_s-erzet,
          sy-uname                  TO wa_s-ernam.

*   Cargar nuevo Status al Objeto
    APPEND wa_s TO lo_nuevo_ticket->s.

*   Cargar datos de Cabecera
    MOVE: lo_nuevo_ticket->h-bastk  TO lo_nuevo_ticket->bastk,
          sy-datum                  TO lo_nuevo_ticket->h-erdat,
          sy-uzeit                  TO lo_nuevo_ticket->h-erzet,
          sy-uname                  TO lo_nuevo_ticket->h-ernam.

*   Actualizar cabecera en BD.
    TRY.
        lo_nuevo_ticket->update_h( ).
      CATCH cx_sql_exception.
        CLEAR e_ticket.
        RAISE error_en_db.
    ENDTRY.

*   Actualizar status en BD.
    TRY.
        lo_nuevo_ticket->update_s( ).
      CATCH cx_sql_exception.
        CLEAR e_ticket.
        RAISE error_en_db.
    ENDTRY.

*   Bloquear ticket p.escritura
    lo_nuevo_ticket->enqueue( EXPORTING  i_bloqueo = c_bloqueo_escritura
                              EXCEPTIONS foreign_lock      = 1
                                         lock_status_error = 2 ).

*   Error: No se pudo Bloquear
    IF sy-subrc IS NOT INITIAL.
      CLEAR e_ticket.
      RAISE error_en_db.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->PASAR_A_STATUS_SIGUIENTE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_BASSTS                       TYPE        ZBAS_TICKET_STATUS_T
* | [EXC!] ERROR_EN_DB
* | [EXC!] NO_PERMITIDO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD pasar_a_status_siguiente.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

*   Nuevo Status
    DATA: wa_s TYPE zbas_s.
    MOVE: me->h-bastk               TO wa_s-bastk,
          sy-datum                  TO wa_s-erdat,
          sy-uzeit                  TO wa_s-erzet,
          sy-uname                  TO wa_s-ernam.

    CALL METHOD me->status_siguiente
      RECEIVING
        r_bassts      = wa_s-bassts
      EXCEPTIONS
        error_interno = 1
        status_final  = 2
        OTHERS        = 3.

*   Status es FINAL?
    IF sy-subrc EQ 2.
      r_bassts = me->get_status_actual( ).
      RETURN.
    ENDIF.

    me->validar_status_completo( ).

    APPEND wa_s TO me->s.

*   Update DB
    TRY.
        me->update_s( ).
      CATCH cx_sql_exception.
        RAISE error_en_db.
    ENDTRY.

    r_bassts = me->get_status_actual( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->REQUIERE_AUTORIZAR_SALIDA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_REQ_AUT_SAL                  TYPE        CHAR1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD requiere_autorizar_salida.

    DATA lv_peso_fuera_tol TYPE zbas_h-peso_ing.

    me->get_totales_cde( IMPORTING e_peso_fuera_tol = lv_peso_fuera_tol ).

    IF lv_peso_fuera_tol EQ 0.
      CLEAR r_req_aut_sal.
    ELSE.
      MOVE 'X' TO r_req_aut_sal.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->SET_HEAD_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATA                         TYPE        ZBAS_H
* | [EXC!] NO_PERMITIDO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_head_data.

*   Bloqueo de Escritura?
    IF me->bloqueo NE c_bloqueo_escritura.
      RAISE no_permitido.
    ENDIF.

    MOVE-CORRESPONDING i_data TO  me->h.

    SELECT SINGLE bukrs
      FROM t001k
      INTO me->h-bukrs
      WHERE bwkey EQ i_data-werks.

    me->update_h( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->STATUS_SIGUIENTE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_BASSTS                       TYPE        ZBAS_TICKET_STATUS_T
* | [EXC!] ERROR_INTERNO
* | [EXC!] STATUS_FINAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD status_siguiente.

    CASE me->get_status_actual( ).

      WHEN c_sts_inicial. " Inicial > Ingreso
        r_bassts = c_sts_ingreso.

      WHEN c_sts_ingreso. " Ingreso > Carga / Descarga
        r_bassts = c_sts_carga_descarga.

      WHEN c_sts_carga_descarga. " Carga / Descarga > Egreso
        r_bassts = c_sts_egreso.

      WHEN c_sts_egreso. " Egreso > Autorizar Salida | Cerrado
        IF me->requiere_autorizar_salida( ) IS NOT INITIAL.
          r_bassts = c_sts_autorizarsalida.
        ELSE.
          r_bassts = c_sts_cerrado.
        ENDIF.

      WHEN c_sts_autorizarsalida. " Autorizar Salida > Cerrado
        r_bassts = c_sts_cerrado.

      WHEN c_sts_cerrado. " Cerrado ! Status Final
        RAISE status_final.

      WHEN c_sts_anulado. " Anulado ! Status Final
        RAISE status_final.

      WHEN OTHERS.
        RAISE error_interno.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->UPDATE_C
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_c.

    MODIFY zbas_c FROM TABLE me->c.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_sql_exception.
    ELSE.
      LOOP AT me->c ASSIGNING FIELD-SYMBOL(<fs_c>).
        MOVE sy-mandt TO <fs_c>-mandt.
      ENDLOOP.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->UPDATE_D
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPDATE_D.

    MODIFY zbas_d FROM TABLE me->d.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_sql_exception.
    ELSE.
      LOOP AT me->d ASSIGNING FIELD-SYMBOL(<fs_d>).
        MOVE sy-mandt TO <fs_d>-mandt.
      ENDLOOP.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->UPDATE_E
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPDATE_E.

    MODIFY zbas_e FROM TABLE me->e.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_sql_exception.
    ELSE.
      LOOP AT me->e ASSIGNING FIELD-SYMBOL(<fs_e>).
        MOVE sy-mandt TO <fs_e>-mandt.
      ENDLOOP.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->UPDATE_H
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_h.

    MODIFY zbas_h FROM me->h.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_sql_exception.
    ELSE.
      MOVE sy-mandt TO me->h-mandt.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BAS_TICKET->UPDATE_S
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_s.

    MODIFY zbas_s FROM TABLE me->s.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_sql_exception.
    ELSE.
      LOOP AT me->s ASSIGNING FIELD-SYMBOL(<fs_s>).
        MOVE sy-mandt TO <fs_s>-mandt.
      ENDLOOP.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BAS_TICKET->VALIDAR_STATUS_COMPLETO
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] STATUS_INCOMPLETE
* | [EXC!] ERROR_INTERNO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validar_status_completo.

    CASE me->get_status_actual( ).
      WHEN c_sts_inicial. " Inicial > Ingreso
        IF me->h-werks IS INITIAL
          OR me->h-basid IS INITIAL
          OR me->h-dni_conductor IS INITIAL
          OR me->h-matricula_transp IS INITIAL
          OR me->h-tdlnr IS INITIAL
          OR me->h-tipooperacion IS INITIAL.
          RAISE status_incomplete.
        ENDIF.

      WHEN c_sts_ingreso. " Ingreso > Carga / Descarga
        IF me->h-check1_ing IS INITIAL
          OR me->h-check2_ing IS INITIAL
          OR me->h-bas_modo_ing IS INITIAL
          OR me->h-peso_ing IS INITIAL
          OR me->h-gewei IS INITIAL.
          RAISE status_incomplete.
        ENDIF.

      WHEN c_sts_carga_descarga. " Carga / Descarga > Egreso
        IF me->h-tipo_carga IS INITIAL.
          RAISE status_incomplete.
        ENDIF.

        IF c[] IS INITIAL
          AND d[] IS INITIAL
          AND e[] IS INITIAL.
          RAISE status_incomplete.
        ENDIF.

      WHEN c_sts_egreso. " Egreso > Autorizar Salida | Cerrado
      WHEN c_sts_autorizarsalida. " Autorizar Salida > Cerrado
      WHEN c_sts_cerrado. " Cerrado ! Status Final
      WHEN c_sts_anulado. " Anulado ! Status Final
      WHEN OTHERS.
        RAISE error_interno.
    ENDCASE.


  ENDMETHOD.
ENDCLASS.