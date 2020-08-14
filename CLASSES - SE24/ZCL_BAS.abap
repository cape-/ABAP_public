*& Copyright (C) Lautaro Capella - All Rights Reserved
*& Unauthorized copying of this file, via any medium is strictly prohibited
*& Proprietary and confidential
*& Written by Lautaro Capella <laucape@gmail.com>, Aug 2020
*&
class ZCL_BAS definition
  public
  final
  create public .

public section.

  constants GC_TTL_BAS_MODO_HS type I value 24 ##NO_TEXT.
  constants GC_BAS_MODO_AUTOMATICO type ZBAS_BAS_MODO_T value 'A' ##NO_TEXT.
  constants GC_BAS_MODO_MANUAL type ZBAS_BAS_MODO_T value 'M' ##NO_TEXT.

  class-methods GET_BASCULA_MODO
    importing
      !I_BASID type ZBAS_BASCULAS-BASID
    returning
      value(R_MODO) type ZBAS_BASCULAS-MODO .
  class-methods SET_BASCULA_MODO
    importing
      !I_BASMODO_LOG type ZBAS_BASMODO_LOG
    returning
      value(R_MODO) type ZBAS_BASCULAS-MODO
    exceptions
      NO_PERMITIDO
      BASMODO_LOG_INCOMPLETO
      BASID_NO_EXISTE .
  class-methods GET_TIPOOPERACION
    importing
      !I_TIPOOPERACION type ZBAS_TIPOOPER-TIPOOPERACION
    exporting
      value(E_DESCRIPCION) type ZBAS_TIPOOPER-DESCRIPCION
    returning
      value(R_ZBAS_TIPOOPER) type ZBAS_TIPOOPER .
  class-methods GET_TIPOCARGA
    importing
      !I_TIPOCARGA type ZBAS_TIPOCARGA-TIPOCARGA
    exporting
      value(E_DESCRIPCION) type ZBAS_TIPOCARGA-DESCRIPCION
      value(E_TOLERANCIA) type ZBAS_TIPOCARGA-TOLERANCIA
    returning
      value(R_ZBAS_TIPOCARGA) type ZBAS_TIPOCARGA .
  class-methods GET_TIPOEMBA
    importing
      !I_TIPOEMBALAJE type ZBAS_TIPOEMBA-TIPOEMBALAJE
    exporting
      value(E_DESCRIPCION) type ZBAS_TIPOEMBA-DESCRIPCION
      value(E_BRGEW) type ZBAS_TIPOEMBA-BRGEW
      value(E_GEWEI) type ZBAS_TIPOEMBA-GEWEI
    returning
      value(R_ZBAS_TIPOEMBA) type ZBAS_TIPOEMBA .
  class-methods GET_TRANSPORTE
    importing
      !I_MATRICULA type ZBAS_TRANSPORTES-MATRICULA optional
      !I_TDLNR type ZBAS_TRANSPORTES-TDLNR optional
    exporting
      value(E_FAB_YEAR) type ZBAS_TRANSPORTES-FAB_YEAR
      value(E_TIPO_VEHICULO) type ZBAS_TRANSPORTES-TIPO_VEHICULO
      value(E_TDLNR) type ZBAS_TRANSPORTES-TDLNR
      value(E_ZHABSENASA) type ZBAS_TRANSPORTES-ZHABSENASA
      value(E_ZDATUMSENASA) type ZBAS_TRANSPORTES-ZDATUMSENASA
      value(E_MULTIPLE) type XFELD
    returning
      value(R_ZBAS_TRANSPORTES) type ZBAS_TRANSPORTES .
  class-methods SET_TRANSPORTE
    importing
      value(I_TRANSPORTES) type ZBAS_TRANSPORTES
    exceptions
      INCOMPLETE_KEY .
  class-methods BUSCAR_TRANSPORTE
    importing
      value(I_MATRICULA) type ZBAS_TRANSPORTES-MATRICULA optional
      value(I_TDLNR) type ZBAS_TRANSPORTES-TDLNR optional
    returning
      value(R_TRANSPORTES) type ZBAS_TRANSPORTES_TAB_T
    exceptions
      NO_ENCONTRADO .
  class-methods GET_CONDUCTOR
    importing
      !I_DNI type ZBAS_CONDUCTORES-DNI
    exporting
      value(E_NOMBRE) type ZBAS_CONDUCTORES-NOMBRE
      value(E_APELLIDO) type ZBAS_CONDUCTORES-APELLIDO
    returning
      value(R_ZBAS_CONDUCTORES) type ZBAS_CONDUCTORES .
  class-methods SET_CONDUCTOR
    importing
      value(I_CONDUCTORES) type ZBAS_CONDUCTORES
    exceptions
      INCOMPLETE_KEY .
  class-methods BUSCAR_CONDUCTOR
    importing
      value(I_NOMBRE) type ZBAS_CONDUCTORES-NOMBRE optional
      value(I_APELLIDO) type ZBAS_CONDUCTORES-APELLIDO optional
    returning
      value(R_CONDUCTORES) type ZBAS_CONDUCTORES_TAB_T
    exceptions
      NO_ENCONTRADO .
  class-methods GET_BASCULA
    importing
      !I_BASID type ZBAS_BASCULAS-BASID
    exporting
      value(E_WERKS) type ZBAS_BASCULAS-WERKS
      value(E_DENOMINACION) type ZBAS_BASCULAS-DENOMINACION
      value(E_HOST) type ZBAS_BASCULAS-HOST
      value(E_PORT) type ZBAS_BASCULAS-PORT
      value(E_MODO) type ZBAS_BASCULAS-MODO
    returning
      value(R_ZBAS_BASCULAS) type ZBAS_BASCULAS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BAS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>BUSCAR_CONDUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NOMBRE                       TYPE        ZBAS_CONDUCTORES-NOMBRE(optional)
* | [--->] I_APELLIDO                     TYPE        ZBAS_CONDUCTORES-APELLIDO(optional)
* | [<-()] R_CONDUCTORES                  TYPE        ZBAS_CONDUCTORES_TAB_T
* | [EXC!] NO_ENCONTRADO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD buscar_conductor.

    DATA: lv_search_pattern_nombre   TYPE zbas_conductores-nombre,
          lv_search_pattern_apellido TYPE zbas_conductores-apellido.

    lv_search_pattern_nombre   = |%{ i_nombre }%|.
    lv_search_pattern_apellido = |%{ i_apellido }%|.

    SELECT *
      FROM zbas_conductores
      INTO TABLE r_conductores
      WHERE nombre   LIKE lv_search_pattern_nombre
        AND apellido LIKE lv_search_pattern_apellido.

    IF sy-subrc IS INITIAL.
      RETURN.
    ELSEIF sy-subrc IS NOT INITIAL
      AND i_nombre IS NOT INITIAL
      AND i_apellido IS NOT INITIAL.
      SELECT *
        FROM zbas_conductores
        INTO TABLE r_conductores
        WHERE nombre   LIKE lv_search_pattern_nombre
          OR  apellido LIKE lv_search_pattern_apellido.
      IF sy-subrc IS NOT INITIAL.
        RAISE no_encontrado.
      ENDIF.
    ELSE.
      RAISE no_encontrado.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>BUSCAR_TRANSPORTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MATRICULA                    TYPE        ZBAS_TRANSPORTES-MATRICULA(optional)
* | [--->] I_TDLNR                        TYPE        ZBAS_TRANSPORTES-TDLNR(optional)
* | [<-()] R_TRANSPORTES                  TYPE        ZBAS_TRANSPORTES_TAB_T
* | [EXC!] NO_ENCONTRADO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD buscar_transporte.

    DATA: wa_zbas_transportes         TYPE zbas_transportes,
          lv_search_pattern_matricula TYPE zbas_transportes-matricula,
          lv_search_pattern_tdlnr     TYPE zbas_transportes-tdlnr.

    lv_search_pattern_matricula = |%{ i_matricula }%|.
    SHIFT i_tdlnr LEFT DELETING LEADING '0'.
    lv_search_pattern_tdlnr     = |%{ i_tdlnr }%|.

*--  ZTM_MATRICULAS
    SELECT *
      FROM ztm_matriculas
      INTO TABLE @DATA(lt_ztm_matriculas)
      WHERE matricula LIKE @lv_search_pattern_matricula
        AND tdlnr     LIKE @lv_search_pattern_tdlnr.

    IF sy-subrc IS INITIAL.
      LOOP AT lt_ztm_matriculas INTO DATA(wa_ztm_matriculas).
        MOVE-CORRESPONDING wa_ztm_matriculas TO wa_zbas_transportes.
        APPEND wa_zbas_transportes TO r_transportes.
        CLEAR wa_zbas_transportes.
      ENDLOOP.
    ENDIF.

*--  ZBAS_TRANSPORTES
    SELECT *
      FROM zbas_transportes
      APPENDING TABLE r_transportes
      WHERE matricula LIKE lv_search_pattern_matricula
        AND tdlnr     LIKE lv_search_pattern_tdlnr.

    DESCRIBE TABLE r_transportes.

    IF sy-tfill IS INITIAL
      AND i_matricula IS NOT INITIAL
      AND i_tdlnr IS NOT INITIAL.

*--     ZTM_MATRICULAS
      SELECT *
        FROM ztm_matriculas
        INTO TABLE lt_ztm_matriculas
        WHERE matricula LIKE lv_search_pattern_matricula
           OR tdlnr     LIKE lv_search_pattern_tdlnr.

      IF sy-subrc IS INITIAL.
        LOOP AT lt_ztm_matriculas INTO wa_ztm_matriculas.
          MOVE-CORRESPONDING wa_ztm_matriculas TO wa_zbas_transportes.
          APPEND wa_zbas_transportes TO r_transportes.
          CLEAR wa_zbas_transportes.
        ENDLOOP.
      ENDIF.

*--     ZBAS_TRANSPORTES
      SELECT *
        FROM zbas_transportes
        APPENDING TABLE r_transportes
        WHERE matricula LIKE lv_search_pattern_matricula
          OR  tdlnr     LIKE lv_search_pattern_tdlnr.
      IF sy-subrc IS NOT INITIAL.
        RAISE no_encontrado.
      ENDIF.
    ENDIF.

    DESCRIBE TABLE r_transportes.
    IF sy-tfill IS INITIAL.
      RAISE no_encontrado.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>GET_BASCULA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BASID                        TYPE        ZBAS_BASCULAS-BASID
* | [<---] E_WERKS                        TYPE        ZBAS_BASCULAS-WERKS
* | [<---] E_DENOMINACION                 TYPE        ZBAS_BASCULAS-DENOMINACION
* | [<---] E_HOST                         TYPE        ZBAS_BASCULAS-HOST
* | [<---] E_PORT                         TYPE        ZBAS_BASCULAS-PORT
* | [<---] E_MODO                         TYPE        ZBAS_BASCULAS-MODO
* | [<-()] R_ZBAS_BASCULAS                TYPE        ZBAS_BASCULAS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_bascula.

    SELECT SINGLE *
      FROM zbas_basculas
      INTO r_zbas_basculas
      WHERE basid EQ i_basid.

    IF sy-subrc IS INITIAL.
      MOVE r_zbas_basculas-werks        TO e_werks.
      MOVE r_zbas_basculas-denominacion TO e_denominacion.
      MOVE r_zbas_basculas-host         TO e_host.
      MOVE r_zbas_basculas-port         TO e_port.
      e_modo = zcl_bas=>get_bascula_modo( i_basid ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>GET_BASCULA_MODO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BASID                        TYPE        ZBAS_BASCULAS-BASID
* | [<-()] R_MODO                         TYPE        ZBAS_BASCULAS-MODO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_bascula_modo.

    SELECT *
      FROM zbas_basmodo_log
      INTO TABLE @DATA(lt_basmodo_log)
      WHERE basid EQ @i_basid.

    IF sy-subrc IS INITIAL.
      SORT lt_basmodo_log DESCENDING BY datum uzeit.
      READ TABLE lt_basmodo_log INTO DATA(wa_basmodo_log) INDEX 1.

*     Modifcación por Automático
      IF wa_basmodo_log-modo EQ gc_bas_modo_automatico.
        MOVE wa_basmodo_log-modo TO r_modo.

*     Modifcación por Manual
      ELSEIF wa_basmodo_log-modo EQ gc_bas_modo_manual.
        CALL METHOD cl_abap_tstmp=>td_subtract
          EXPORTING
            date1    = sy-datum
            time1    = sy-uzeit
            date2    = wa_basmodo_log-datum
            time2    = wa_basmodo_log-uzeit
          IMPORTING
            res_secs = DATA(lv_secs).

        DATA(lv_diff_hs) = lv_secs DIV ( 60 * 60 * 24 ).
*       Modificación vencida!
        IF lv_diff_hs GT gc_ttl_bas_modo_hs.
          MOVE gc_bas_modo_automatico TO r_modo.
          UPDATE zbas_basculas SET modo = r_modo
                               WHERE basid EQ i_basid.

*       Modificación vigente!
        ELSE.
          MOVE wa_basmodo_log-modo TO r_modo.
        ENDIF.
      ENDIF.

*   No hay modificaciones en el log
    ELSE.
      SELECT SINGLE modo
        FROM zbas_basculas
        INTO r_modo
        WHERE basid EQ i_basid.
      IF r_modo IS INITIAL.
        MOVE gc_bas_modo_automatico TO r_modo.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>GET_CONDUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DNI                          TYPE        ZBAS_CONDUCTORES-DNI
* | [<---] E_NOMBRE                       TYPE        ZBAS_CONDUCTORES-NOMBRE
* | [<---] E_APELLIDO                     TYPE        ZBAS_CONDUCTORES-APELLIDO
* | [<-()] R_ZBAS_CONDUCTORES             TYPE        ZBAS_CONDUCTORES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_conductor.

    SELECT SINGLE *
      FROM zbas_conductores
      INTO r_zbas_conductores
      WHERE dni EQ i_dni.

    IF sy-subrc IS INITIAL.
      MOVE r_zbas_conductores-nombre    TO e_nombre.
      MOVE r_zbas_conductores-apellido  TO e_apellido.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>GET_TIPOCARGA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TIPOCARGA                    TYPE        ZBAS_TIPOCARGA-TIPOCARGA
* | [<---] E_DESCRIPCION                  TYPE        ZBAS_TIPOCARGA-DESCRIPCION
* | [<---] E_TOLERANCIA                   TYPE        ZBAS_TIPOCARGA-TOLERANCIA
* | [<-()] R_ZBAS_TIPOCARGA               TYPE        ZBAS_TIPOCARGA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tipocarga.

    SELECT SINGLE *
      FROM zbas_tipocarga
      INTO r_zbas_tipocarga
      WHERE tipocarga EQ i_tipocarga.

    IF sy-subrc IS INITIAL.
      MOVE r_zbas_tipocarga-descripcion TO e_descripcion.
      MOVE r_zbas_tipocarga-tolerancia  TO e_tolerancia.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>GET_TIPOEMBA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TIPOEMBALAJE                 TYPE        ZBAS_TIPOEMBA-TIPOEMBALAJE
* | [<---] E_DESCRIPCION                  TYPE        ZBAS_TIPOEMBA-DESCRIPCION
* | [<---] E_BRGEW                        TYPE        ZBAS_TIPOEMBA-BRGEW
* | [<---] E_GEWEI                        TYPE        ZBAS_TIPOEMBA-GEWEI
* | [<-()] R_ZBAS_TIPOEMBA                TYPE        ZBAS_TIPOEMBA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tipoemba.

    SELECT SINGLE *
      FROM zbas_tipoemba
      INTO r_zbas_tipoemba
      WHERE tipoembalaje EQ i_tipoembalaje.

    IF sy-subrc IS INITIAL.
      MOVE r_zbas_tipoemba-descripcion TO e_descripcion.
      MOVE r_zbas_tipoemba-brgew       TO e_brgew.
      MOVE r_zbas_tipoemba-gewei       TO e_gewei.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>GET_TIPOOPERACION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TIPOOPERACION                TYPE        ZBAS_TIPOOPER-TIPOOPERACION
* | [<---] E_DESCRIPCION                  TYPE        ZBAS_TIPOOPER-DESCRIPCION
* | [<-()] R_ZBAS_TIPOOPER                TYPE        ZBAS_TIPOOPER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tipooperacion.

    SELECT SINGLE *
      FROM zbas_tipooper
      INTO r_zbas_tipooper
      WHERE tipooperacion EQ i_tipooperacion.

    IF sy-subrc IS INITIAL.
      MOVE r_zbas_tipooper-descripcion TO e_descripcion.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>GET_TRANSPORTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MATRICULA                    TYPE        ZBAS_TRANSPORTES-MATRICULA(optional)
* | [--->] I_TDLNR                        TYPE        ZBAS_TRANSPORTES-TDLNR(optional)
* | [<---] E_FAB_YEAR                     TYPE        ZBAS_TRANSPORTES-FAB_YEAR
* | [<---] E_TIPO_VEHICULO                TYPE        ZBAS_TRANSPORTES-TIPO_VEHICULO
* | [<---] E_TDLNR                        TYPE        ZBAS_TRANSPORTES-TDLNR
* | [<---] E_ZHABSENASA                   TYPE        ZBAS_TRANSPORTES-ZHABSENASA
* | [<---] E_ZDATUMSENASA                 TYPE        ZBAS_TRANSPORTES-ZDATUMSENASA
* | [<---] E_MULTIPLE                     TYPE        XFELD
* | [<-()] R_ZBAS_TRANSPORTES             TYPE        ZBAS_TRANSPORTES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_transporte.
    DATA: lr_matricula        TYPE RANGE OF zbas_transportes-matricula,
          wa_lr_matricula     LIKE LINE OF lr_matricula,
          lr_tdlnr            TYPE RANGE OF zbas_transportes-tdlnr,
          wa_lr_tdlnr         LIKE LINE OF lr_tdlnr,
          wa_zbas_transportes TYPE zbas_transportes,
          lt_zbas_transportes TYPE STANDARD TABLE OF zbas_transportes.

    CLEAR r_zbas_transportes.

    IF i_matricula IS NOT INITIAL.
      MOVE: 'I'         TO wa_lr_matricula-sign,
            'EQ'        TO wa_lr_matricula-option,
            i_matricula TO wa_lr_matricula-low.
      APPEND wa_lr_matricula TO lr_matricula.
    ENDIF.

    IF i_tdlnr IS NOT INITIAL.
      MOVE: 'I'     TO wa_lr_tdlnr-sign,
            'EQ'    TO wa_lr_tdlnr-option,
            i_tdlnr TO wa_lr_tdlnr-low.
      APPEND wa_lr_tdlnr TO lr_tdlnr.
    ENDIF.

*--  ZTM_MATRICULAS
    SELECT *
      FROM ztm_matriculas
      INTO TABLE @DATA(lt_ztm_matriculas)
      WHERE matricula IN @lr_matricula
        AND tdlnr     IN @lr_tdlnr.

    IF sy-subrc IS INITIAL.
      LOOP AT lt_ztm_matriculas INTO DATA(wa_ztm_matriculas).
        MOVE-CORRESPONDING wa_ztm_matriculas TO wa_zbas_transportes.
        APPEND wa_zbas_transportes TO lt_zbas_transportes.
        CLEAR wa_zbas_transportes.
      ENDLOOP.
    ENDIF.

*--  ZBAS_TRANSPORTES
    SELECT *
      FROM zbas_transportes
      APPENDING TABLE lt_zbas_transportes
      WHERE matricula IN lr_matricula
        AND tdlnr     IN lr_tdlnr.

    DESCRIBE TABLE lt_zbas_transportes.

    IF sy-tfill EQ 1.
      READ TABLE lt_zbas_transportes INTO r_zbas_transportes INDEX 1.
      MOVE: space                            TO e_multiple,
            r_zbas_transportes-fab_year      TO e_fab_year,
            r_zbas_transportes-tipo_vehiculo TO e_tipo_vehiculo,
            r_zbas_transportes-tdlnr         TO e_tdlnr,
            r_zbas_transportes-zhabsenasa    TO e_zhabsenasa,
            r_zbas_transportes-zdatumsenasa  TO e_zdatumsenasa.

    ELSEIF sy-tfill GT 1.
      MOVE 'X' TO e_multiple.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>SET_BASCULA_MODO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BASMODO_LOG                  TYPE        ZBAS_BASMODO_LOG
* | [<-()] R_MODO                         TYPE        ZBAS_BASCULAS-MODO
* | [EXC!] NO_PERMITIDO
* | [EXC!] BASMODO_LOG_INCOMPLETO
* | [EXC!] BASID_NO_EXISTE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_bascula_modo.

    IF i_basmodo_log-basid IS INITIAL
      OR i_basmodo_log-datum IS INITIAL
      OR i_basmodo_log-uzeit IS INITIAL
      OR i_basmodo_log-modo IS INITIAL
      OR i_basmodo_log-razon IS INITIAL
      OR i_basmodo_log-bastk IS INITIAL.
      RAISE basmodo_log_incompleto.
    ENDIF.

    SELECT SINGLE werks
      FROM zbas_basculas
      INTO @DATA(lv_werks)
      WHERE basid EQ @i_basmodo_log-basid.

    IF sy-subrc IS NOT INITIAL.
      RAISE basid_no_existe.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'ZBAS_ROLES'
             ID 'WERKS'    FIELD lv_werks
             ID 'ZBAS_ROL' FIELD 'MB'.

    IF sy-subrc IS NOT INITIAL.
      RAISE no_permitido.
    ENDIF.

    DATA: wa_zbas_basmodo_log TYPE zbas_basmodo_log.
    MOVE-CORRESPONDING i_basmodo_log TO wa_zbas_basmodo_log.
    MOVE: sy-datum      TO wa_zbas_basmodo_log-erdat,
          sy-uzeit      TO wa_zbas_basmodo_log-erzet,
          sy-uname      TO wa_zbas_basmodo_log-ernam.

    INSERT zbas_basmodo_log FROM wa_zbas_basmodo_log.

    IF sy-subrc IS INITIAL.
      MOVE i_basmodo_log-modo TO r_modo.

      UPDATE zbas_basculas SET modo = r_modo
                           WHERE basid EQ wa_zbas_basmodo_log-basid.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>SET_CONDUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CONDUCTORES                  TYPE        ZBAS_CONDUCTORES
* | [EXC!] INCOMPLETE_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SET_CONDUCTOR.

    IF i_conductores-dni IS INITIAL.
      RAISE incomplete_key.
    ENDIF.

    SELECT SINGLE *
      FROM zbas_conductores
      INTO @DATA(wa_zbas_conductores)
      WHERE dni EQ @i_conductores-dni.

    IF sy-subrc IS NOT INITIAL.
*     Nuevo
      MOVE: sy-datum TO i_conductores-erdat,
            sy-uzeit TO i_conductores-erzet,
            sy-uname TO i_conductores-ernam.
      INSERT zbas_conductores FROM i_conductores.

    ELSE.
*     Existente
      MOVE: sy-datum TO i_conductores-erdat,
            sy-uzeit TO i_conductores-erzet,
            sy-uname TO i_conductores-ernam.
      UPDATE zbas_conductores FROM i_conductores.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BAS=>SET_TRANSPORTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TRANSPORTES                  TYPE        ZBAS_TRANSPORTES
* | [EXC!] INCOMPLETE_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_transporte.

    IF i_transportes-matricula IS INITIAL
      OR i_transportes-tdlnr IS INITIAL.
      RAISE incomplete_key.
    ENDIF.

*--  ZBAS_TRANSPORTES
    SELECT SINGLE *
      FROM zbas_transportes
      INTO @DATA(wa_zbas_transportes)
      WHERE matricula EQ @i_transportes-matricula
        AND tdlnr     EQ @i_transportes-tdlnr.

    IF sy-subrc IS NOT INITIAL.
*     Nuevo
      MOVE: sy-datum TO i_transportes-erdat,
            sy-uzeit TO i_transportes-erzet,
            sy-uname TO i_transportes-ernam.
      INSERT zbas_transportes FROM i_transportes.

    ELSE.
*     Existente
      MOVE: sy-datum TO i_transportes-erdat,
            sy-uzeit TO i_transportes-erzet,
            sy-uname TO i_transportes-ernam.
      UPDATE zbas_transportes FROM i_transportes.
    ENDIF.

*--  ZTM_MATRICULAS
    SELECT SINGLE *
      FROM ztm_matriculas
      INTO @DATA(wa_ztm_matriculas)
      WHERE matricula EQ @i_transportes-matricula.

    IF sy-subrc IS INITIAL.
*     Existente
      UPDATE ztm_matriculas SET zhabsenasa = i_transportes-zhabsenasa
                                zdatumsenasa = i_transportes-zdatumsenasa
                                WHERE matricula EQ i_transportes-matricula.
    ENDIF.

  ENDMETHOD.
ENDCLASS.