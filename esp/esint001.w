&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esint001 1.00.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esint001 <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

    DEFINE VARIABLE hnd_apps_server    AS HANDLE    NO-UNDO. /* handle do server */
    DEFINE VARIABLE hnd_apps_prog      AS HANDLE    NO-UNDO. /* handle para programa persistent */
    DEFINE VARIABLE p-string-conexao   AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 RECT-2 RECT-3 rs-tipo-trans ~
bt-pesq fi-cd-sistema 
&Scoped-Define DISPLAYED-OBJECTS rs-tipo-trans fi-cd-sistema ~
fi-desc-sistema 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ativa 
     LABEL "Ativar" 
     SIZE 18 BY 1.5.

DEFINE BUTTON bt-desativa 
     LABEL "Desativar" 
     SIZE 18 BY 1.5.

DEFINE BUTTON bt-pesq 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     LABEL "Pesquisar" 
     SIZE 5.29 BY 1.13.

DEFINE VARIABLE fi-cd-sistema AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Sistema" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-sistema AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 49 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tipo-trans AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Importaá∆o", 1,
"Exportaá∆o", 2
     SIZE 44 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.75.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 9.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 6.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     rs-tipo-trans AT ROW 3 COL 24 NO-LABEL WIDGET-ID 16
     bt-pesq AT ROW 3.88 COL 76.72 WIDGET-ID 20
     fi-cd-sistema AT ROW 3.96 COL 15 COLON-ALIGNED WIDGET-ID 12
     fi-desc-sistema AT ROW 3.96 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     bt-ativa AT ROW 8.67 COL 36 WIDGET-ID 2
     bt-desativa AT ROW 10.42 COL 36 WIDGET-ID 4
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.5 COL 1 WIDGET-ID 6
     RECT-2 AT ROW 5.46 COL 1 WIDGET-ID 8
     RECT-3 AT ROW 6.92 COL 21 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Processo de Integraá∆o SFA"
         HEIGHT             = 14.13
         WIDTH              = 90.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 105.14
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 105.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* SETTINGS FOR BUTTON bt-ativa IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desativa IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-sistema IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Processo de Integraá∆o SFA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Processo de Integraá∆o SFA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ativa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ativa w-livre
ON CHOOSE OF bt-ativa IN FRAME f-cad /* Ativar */
DO:
    DEF VAR i-seq AS INTEGER NO-UNDO.
    
    DEF BUFFER bf-es-api-exec FOR es-api-exec.

    ASSIGN p-string-conexao = "-DirectConnect -H 192.168.51.141 -S 39090".

    ASSIGN FRAME {&FRAME-NAME} rs-tipo-trans fi-cd-sistema.

    RUN pi-verifica-conexao.
    
    FIND LAST bf-es-api-exec NO-LOCK NO-ERROR.
    IF AVAIL bf-es-api-exec THEN
        ASSIGN i-seq = bf-es-api-exec.id-exec + 1.
    ELSE
        ASSIGN i-seq = 1.
    
    CREATE es-api-exec.
    ASSIGN es-api-exec.id-exec = i-seq
           es-api-exec.data-inicio = NOW
           es-api-exec.data-fim    = ?
           es-api-exec.ind-tipo-trans = rs-tipo-trans
           es-api-exec.cd-sistema     = fi-cd-sistema.

    RUN pi-ativa.

    RUN pi-executa-apps.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desativa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desativa w-livre
ON CHOOSE OF bt-desativa IN FRAME f-cad /* Desativar */
DO:
 
    STATUS INPUT "Status Integraá∆o: Finalizando...".

    FIND LAST es-api-exec WHERE es-api-exec.data-fim = ? EXCLUSIVE-LOCK no-error.
    ASSIGN es-api-exec.data-fim    = NOW.

    RUN pi-finaliza.

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pesq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pesq w-livre
ON CHOOSE OF bt-pesq IN FRAME f-cad /* Pesquisar */
DO:
    ASSIGN FRAME {&FRAME-NAME} rs-tipo-trans.


    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans =  rs-tipo-trans
                              AND es-api-param.cd-sistema = INT(fi-cd-sistema:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF AVAIL es-api-param THEN DO:

        FIND FIRST es-api-exec WHERE es-api-exec.cd-sistema     = es-api-param.cd-sistema
                                 AND es-api-exec.ind-tipo-trans = es-api-param.ind-tipo-trans
                                 AND es-api-exec.data-fim = ? NO-LOCK NO-ERROR.
        IF AVAIL es-api-exec THEN
            RUN pi-ativa.
        ELSE RUN pi-desativa.
    END.
    ELSE DO:
        MESSAGE "ParÉmetro de integraá∆o n∆o localizado!" SKIP
                "Favor realizar a parametrizaá∆o no esint004"  VIEW-AS ALERT-BOX ERROR BUTTONS OK.

        APPLY "entry" TO rs-tipo-trans.
        RETURN NO-APPLY.
    END.




    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cd-sistema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cd-sistema w-livre
ON LEAVE OF fi-cd-sistema IN FRAME f-cad /* Sistema */
DO:
    FIND FIRST es-api-app WHERE es-api-app.cd-sistema = INT(fi-cd-sistema:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF AVAIL es-api-app THEN
        ASSIGN fi-desc-sistema:SCREEN-VALUE = es-api-app.des-sistema .
    ELSE
        ASSIGN fi-desc-sistema:SCREEN-VALUE = "".

        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             rs-tipo-trans:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY rs-tipo-trans fi-cd-sistema fi-desc-sistema 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 RECT-2 RECT-3 rs-tipo-trans bt-pesq fi-cd-sistema 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "esint001" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ativa w-livre 
PROCEDURE pi-ativa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN bt-ativa:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
ASSIGN bt-desativa:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.

STATUS INPUT "Status Integraá∆o: Processando...".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-conectar-apps w-livre 
PROCEDURE pi-conectar-apps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ierro  AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-erro AS CHARACTER   NO-UNDO.

CREATE SERVER hnd_apps_server.

hnd_apps_server:CONNECT( p-string-conexao, "", "" ) NO-ERROR.

IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:      
    MESSAGE ERROR-STATUS:NUM-MESSAGES " erros ocorridos na conexao." SKIP         
        "Visualizar os erros ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO    
        UPDATE view-errs AS LOGICAL.      

    IF view-errs THEN DO:     
        DO ierro = 1 TO ERROR-STATUS:NUM-MESSAGES:        
            ASSIGN c-erro = c-erro + STRING(ERROR-STATUS:GET-NUMBER(ierro)) + " - " + ERROR-STATUS:GET-MESSAGE(ierro) + CHR(10).
        END.

        MESSAGE c-erro VIEW-AS ALERT-BOX ERROR.

        RETURN "NOK":U.
    END.
END.
RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desativa w-livre 
PROCEDURE pi-desativa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN bt-ativa:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
ASSIGN bt-desativa:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

STATUS INPUT "Status Integraá∆o: Desativada".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desconectar-apps w-livre 
PROCEDURE pi-desconectar-apps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hnd_apps_server) THEN DO:
    hnd_apps_server:DISCONNECT() NO-ERROR.
    DELETE OBJECT hnd_apps_server.
    ASSIGN hnd_apps_server = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executa-apps w-livre 
PROCEDURE pi-executa-apps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-path-config AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-result      AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-arquivo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-seq         AS INTEGER   NO-UNDO.
DEFINE VARIABLE c_dir         AS CHARACTER NO-UNDO.

ASSIGN FRAME {&FRAME-NAME}  rs-tipo-trans fi-cd-sistema.

RUN pi-conectar-apps.

IF NOT (hnd_apps_server:CONNECTED()) THEN DO:
    MESSAGE "AppServer n∆o conectado" VIEW-AS ALERT-BOX.
    RETURN .
END.

IF (hnd_apps_server:CONNECTED()) THEN DO:
    IF rs-tipo-trans = 1 THEN
        RUN esp/esint001irp.p ON hnd_apps_server ASYNCHRONOUS SET hnd_apps_server
    /*EVENT-PROCEDURE "pi-finaliza" */ (INPUT rs-tipo-trans, INPUT fi-cd-sistema).

    ELSE
        RUN esp/esint001erp.p ON hnd_apps_server ASYNCHRONOUS SET hnd_apps_server
    /*EVENT-PROCEDURE "pi-finaliza" */ (INPUT rs-tipo-trans, INPUT fi-cd-sistema).
END.

/*
RUN esp/esint001rp.p (INPUT rs-tipo-trans, INPUT fi-cd-sistema).
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-finaliza w-livre 
PROCEDURE pi-finaliza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN pi-desativa.
RUN pi-desconectar-apps .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica-conexao w-livre 
PROCEDURE pi-verifica-conexao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-conectar-apps.

 IF (hnd_apps_server:CONNECTED()) THEN DO:
     RUN  pi-desconectar-apps.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

