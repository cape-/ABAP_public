*&---------------------------------------------------------------------*
*& Report  ZBAS_MONITOR
*&   Proyecto ZBAS
*&   -- Monitor de Tickets de Básculas.
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
REPORT zbas_monitor.
TABLES: zbas_h, zbas_s.

SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE text-t01 NO INTERVALS.
PARAMETERS p_werks TYPE zbas_h-werks MEMORY ID wrk OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl0.

SELECTION-SCREEN BEGIN OF BLOCK bll WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_erdat FOR zbas_h-erdat DEFAULT sy-datum,
                s_erzet FOR zbas_h-erzet,
                s_basid FOR zbas_h-basid NO INTERVALS,
                s_matric FOR zbas_h-matricula_transp NO INTERVALS,
                s_tdlnr  FOR zbas_h-tdlnr NO INTERVALS,
                s_tipoop FOR zbas_h-tipooperacion NO INTERVALS,
                s_ernam  FOR zbas_h-ernam NO INTERVALS DEFAULT sy-uname.
SELECTION-SCREEN END OF BLOCK bll.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02 NO INTERVALS.
SELECTION-SCREEN: SKIP.
PARAMETERS: p_st_ini AS CHECKBOX,
            p_st_ing AS CHECKBOX DEFAULT 'X',
            p_st_car AS CHECKBOX DEFAULT 'X',
            p_st_egr AS CHECKBOX DEFAULT 'X',
            p_st_req AS CHECKBOX DEFAULT 'X',
            p_st_cer AS CHECKBOX,
            p_st_anu AS CHECKBOX.
SELECTION-SCREEN: SKIP,
                  BEGIN OF LINE,
                  PUSHBUTTON 4(12) pb_sts_a USER-COMMAND stsact,
                  PUSHBUTTON 18(10) pb_sts_t USER-COMMAND stsall,
                  PUSHBUTTON 30(12) pb_sts_n USER-COMMAND stsnon,
                  END OF LINE.
SELECTION-SCREEN END OF BLOCK bl2.

TYPES: BEGIN OF tye_zbas_alv.
        INCLUDE STRUCTURE zbas_h.
TYPES:   bassts  TYPE zbas_ticket_status_text_t-bassts,
         ststxt  TYPE zbas_ticket_status_text_t-ststxt,
         stsicon TYPE zbas_ticket_status_text_t-stsicon,
         END OF tye_zbas_alv,
         tyt_zbas_alv TYPE STANDARD TABLE OF tye_zbas_alv.

DATA: gt_zbas_alv TYPE tyt_zbas_alv.

CONSTANTS: gc_auth_rol_crear TYPE zbas_roles_t VALUE 'CR',
           gc_auth_rol_visua TYPE zbas_roles_t VALUE 'VI'.

INITIALIZATION.
  pb_sts_a = text-pb0. " 'Sólo Activos'
  pb_sts_t = text-pb1. " 'Sel. Todos'
  pb_sts_n = text-pb2. " 'Sel. Ninguno'

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'STS0'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'STSACT'.
      MOVE 'X' TO: p_st_ing,
                   p_st_car,
                   p_st_egr,
                   p_st_req.
      CLEAR: p_st_ini,
             p_st_cer,
             p_st_anu.
    WHEN 'STSALL'.
      MOVE 'X' TO: p_st_ini,
                   p_st_ing,
                   p_st_car,
                   p_st_egr,
                   p_st_req,
                   p_st_cer,
                   p_st_anu.
    WHEN 'STSNON'.
      CLEAR: p_st_ini,
             p_st_ing,
             p_st_car,
             p_st_egr,
             p_st_req,
             p_st_cer,
             p_st_anu.
    WHEN 'ZNUEVOTKT'.
      PERFORM f_auth_check USING p_werks
                                 gc_auth_rol_crear.

      EXPORT crear = 'X'
             werks = p_werks
             TO MEMORY ID 'ZBAS_MANAGER_PARAMS'.
      CALL TRANSACTION 'ZBAS_NUEVO'.

  ENDCASE.

START-OF-SELECTION.
  PERFORM f_auth_check USING p_werks
                             gc_auth_rol_visua.

  PERFORM f_get_data CHANGING gt_zbas_alv.

END-OF-SELECTION.
  CHECK gt_zbas_alv[] IS NOT INITIAL.
  PERFORM f_alv USING gt_zbas_alv.

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_ZBAS_H  text
*----------------------------------------------------------------------*
FORM f_get_data  CHANGING pte_zbas_alv TYPE tyt_zbas_alv.

  DATA: lr_bassts    TYPE RANGE OF zbas_s-bassts,
        wa_lr_bassts LIKE LINE OF lr_bassts,
        wa_zbas_alv  TYPE tye_zbas_alv,
        lt_zbas_h    TYPE STANDARD TABLE OF zbas_h,
        lt_zbas_s    TYPE STANDARD TABLE OF zbas_s.

  IF NOT ( p_st_ini IS NOT INITIAL
       AND p_st_ing IS NOT INITIAL
       AND p_st_car IS NOT INITIAL
       AND p_st_egr IS NOT INITIAL
       AND p_st_req IS NOT INITIAL
       AND p_st_cer IS NOT INITIAL
       AND p_st_anu IS NOT INITIAL ).

    MOVE: 'I'  TO wa_lr_bassts-sign,
          'EQ' TO wa_lr_bassts-option.

    IF p_st_ini IS NOT INITIAL.
      wa_lr_bassts-low = zcl_bas_ticket=>c_sts_inicial.
      APPEND wa_lr_bassts TO lr_bassts.
    ENDIF.

    IF p_st_ing IS NOT INITIAL.
      wa_lr_bassts-low = zcl_bas_ticket=>c_sts_ingreso.
      APPEND wa_lr_bassts TO lr_bassts.
    ENDIF.

    IF p_st_car IS NOT INITIAL.
      wa_lr_bassts-low = zcl_bas_ticket=>c_sts_carga_descarga.
      APPEND wa_lr_bassts TO lr_bassts.
    ENDIF.

    IF p_st_egr IS NOT INITIAL.
      wa_lr_bassts-low = zcl_bas_ticket=>c_sts_egreso.
      APPEND wa_lr_bassts TO lr_bassts.
    ENDIF.

    IF p_st_req IS NOT INITIAL.
      wa_lr_bassts-low = zcl_bas_ticket=>c_sts_autorizarsalida.
      APPEND wa_lr_bassts TO lr_bassts.
    ENDIF.

    IF p_st_cer IS NOT INITIAL.
      wa_lr_bassts-low = zcl_bas_ticket=>c_sts_cerrado.
      APPEND wa_lr_bassts TO lr_bassts.
    ENDIF.

    IF p_st_anu IS NOT INITIAL.
      wa_lr_bassts-low = zcl_bas_ticket=>c_sts_anulado.
      APPEND wa_lr_bassts TO lr_bassts.
    ENDIF.

    IF lr_bassts[] IS INITIAL. " Caso de todos los check en blanco
      MESSAGE 'Debe seleccionar al menos un Status' TYPE 'S'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zbas_h
    INTO TABLE lt_zbas_h
    WHERE werks EQ p_werks
      AND basid  IN s_basid
      AND matricula_transp IN s_matric
      AND tdlnr  IN s_tdlnr
      AND tipooperacion IN s_tipoop
      AND erdat  IN s_erdat
      AND erzet  IN s_erzet
      AND ernam  IN s_ernam.

  SELECT *
    FROM zbas_s
    INTO TABLE lt_zbas_s
    FOR ALL ENTRIES IN lt_zbas_h
    WHERE bastk EQ lt_zbas_h-bastk.

  SORT lt_zbas_s BY bastk ASCENDING erdat DESCENDING erzet DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_zbas_s COMPARING bastk.
  DELETE lt_zbas_s WHERE bassts NOT IN lr_bassts.

  LOOP AT lt_zbas_h INTO DATA(wa_zbas_h).
    MOVE wa_zbas_h TO wa_zbas_alv.
    READ TABLE lt_zbas_s INTO DATA(wa_zbas_s) WITH KEY bastk = wa_zbas_h-bastk BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      MOVE wa_zbas_s-bassts TO wa_zbas_alv-bassts.
      wa_zbas_alv-ststxt  = zcl_bas_ticket=>get_status_text( wa_zbas_alv-bassts ).
      wa_zbas_alv-stsicon = zcl_bas_ticket=>get_status_icon( wa_zbas_alv-bassts ).
      APPEND wa_zbas_alv TO pte_zbas_alv.
      CLEAR wa_zbas_alv.
    ENDIF.
  ENDLOOP.

  IF pte_zbas_alv[] IS INITIAL.
    MESSAGE 'No se encontraron Tickets para los criterios de selección' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ZBAS_H  text
*----------------------------------------------------------------------*
FORM f_alv  USING    pti_zbas_alv TYPE tyt_zbas_alv.

  DATA: lv_titl TYPE lvc_title,
        wa_layo TYPE lvc_s_layo,
        wa_fcat TYPE lvc_s_fcat,
        lt_fcat TYPE lvc_t_fcat.

  lv_titl = |Tickets de Báscula para el Centro { p_werks }|.
  wa_layo-sel_mode   = 'A'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZBAS_H'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  wa_fcat-fieldname = 'BASSTS'.
  wa_fcat-ref_table = 'ZBAS_TICKET_STATUS_TEXT_T'.
  APPEND wa_fcat TO lt_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'STSTXT'.
  wa_fcat-ref_table = 'ZBAS_TICKET_STATUS_TEXT_T'.
  APPEND wa_fcat TO lt_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'STSICON'.
  wa_fcat-ref_table = 'ZBAS_TICKET_STATUS_TEXT_T'.
  wa_fcat-icon      = 'X'.
  APPEND wa_fcat TO lt_fcat.
  CLEAR wa_fcat.

  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-tabname = 'GT_ZBAS_ALV'.
    CASE <fs_fcat>-fieldname.
      WHEN 'BASTK'.
        <fs_fcat>-col_pos = 1.
      WHEN 'BASSTS'.
        <fs_fcat>-col_pos = 2.
      WHEN 'STSICON'.
        <fs_fcat>-col_pos = 3.
      WHEN 'STSTXT'.
        <fs_fcat>-col_pos = 4.
      WHEN 'WERKS'.
        <fs_fcat>-col_pos = 5.
      WHEN 'TIPOOPERACION'.
        <fs_fcat>-col_pos = 6.
      WHEN OTHERS.
        CLEAR <fs_fcat>-col_pos.
    ENDCASE.
  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE 'Error imprevisto en ALV' TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'F_ALV_PFSTS'
      i_callback_user_command  = 'F_ALV_UCOMM'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
      i_structure_name         = 'ZBAS_H'
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = lv_titl
*     I_GRID_SETTINGS          =
      is_layout_lvc            = wa_layo
      it_fieldcat_lvc          = lt_fcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = pti_zbas_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error imprevisto en ALV' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ZBAS_H  text
*----------------------------------------------------------------------*
FORM f_alv_ucomm  USING pvi_ucomm     TYPE syucomm
                        psi_selfield  TYPE slis_selfield.

  READ TABLE gt_zbas_alv INTO DATA(wa_zbas_alv) INDEX psi_selfield-tabindex.

  CASE pvi_ucomm.
    WHEN 'VISUAL_TKT'.
      AUTHORITY-CHECK OBJECT 'ZBAS_ROLES'
               ID 'WERKS' FIELD p_werks
               ID 'ZBAS_ROL' FIELD 'VI'.
      IF sy-subrc <> 0.
        MESSAGE 'No posee autorización para visualizar' TYPE 'S'
          DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      EXPORT bastk = wa_zbas_alv-bastk
             bloqueo = zcl_bas_ticket=>c_bloqueo_lectura
             TO MEMORY ID 'ZBAS_MANAGER_PARAMS'.
      CALL TRANSACTION 'ZBAS_NUEVO'.

    WHEN 'TRATAR_TKT'.
      EXPORT bastk = wa_zbas_alv-bastk
             bloqueo = zcl_bas_ticket=>c_bloqueo_escritura
             TO MEMORY ID 'ZBAS_MANAGER_PARAMS'.
      CALL TRANSACTION 'ZBAS_NUEVO'.

    WHEN OTHERS.

  ENDCASE.

  MOVE 'X' TO:  psi_selfield-refresh,
                psi_selfield-row_stable,
                psi_selfield-col_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ZBAS_H  text
*----------------------------------------------------------------------*
FORM f_alv_pfsts  USING lt_extab.

  SET PF-STATUS 'ZALV'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*----------------------------------------------------------------------*
FORM f_auth_check  USING    pvi_werks TYPE zbas_h-werks
                            pvi_rol   TYPE zbas_roles_t.

  AUTHORITY-CHECK OBJECT 'ZBAS_ROLES'
           ID 'WERKS' FIELD pvi_werks
           ID 'ZBAS_ROL' FIELD pvi_rol. "Visualizar

  IF sy-subrc <> 0.
    MESSAGE 'No posee autorización para realizar la acción en este Centro' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.