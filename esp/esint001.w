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

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND-VERSION}" >= "1.00" &THEN
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

    DEFINE VARIABLE hnd-apps-server-1-1       AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
    DEFINE VARIABLE hnd-apps-server-async-1-1 AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
                                                                
    DEFINE VARIABLE hnd-apps-server-1-2       AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
    DEFINE VARIABLE hnd-apps-server-async-1-2 AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
                                                                
    DEFINE VARIABLE hnd-apps-server-2-1       AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
    DEFINE VARIABLE hnd-apps-server-async-2-1 AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
                                                                
    DEFINE VARIABLE hnd-apps-server-2-2       AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
    DEFINE VARIABLE hnd-apps-server-async-2-2 AS HANDLE  EXTENT 20  NO-UNDO. /* handle do server */
    
    DEFINE VARIABLE hnd-apps-prog      AS HANDLE    NO-UNDO. /* handle para programa persistent */
    DEFINE VARIABLE p-string-conexao   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i-tot-serv         AS INTEGER   NO-UNDO.

    DEFINE TEMP-TABLE tt-apps NO-UNDO
        LIKE es-api-appserver
        FIELD i-agent-conect AS INTEGER
        FIELD l-padrao       AS LOGICAL
        INDEX i1 IS PRIMARY ind-tipo-trans 
                            cd-sistema
                            host-appserver.

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
&Scoped-Define ENABLED-OBJECTS fi-sfa-entrada fi-direcao-1 fi-sfa-2 ~
fi--direcao-2 fi-ariba-1 fi-direcao-3 fi-ariba-2 fi-direcao-4 RECT-2 ~
rt-button RECT-3 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS fi-sfa-entrada fi-direcao-1 fi-sfa-2 ~
fi--direcao-2 fi-ariba-1 fi-direcao-3 fi-ariba-2 fi-direcao-4 ~
fi-dt-ativa-sfa1 fi-dt-ativa-sfa2 fi-dt-ativa-ariba1 fi-dt-ativa-ariba2 ~
fi-agentes-sfa1 fi-agentes-sfa2 fi-agentes-ariba1 fi-agentes-ariba2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ativaAriba1 
     IMAGE-UP FILE "image/im-play1.bmp":U NO-FOCUS
     LABEL "Ativar" 
     SIZE 7 BY 1 TOOLTIP "Ativar Integra‡Æo".

DEFINE BUTTON bt-ativaAriba2 
     IMAGE-UP FILE "image/im-play1.bmp":U NO-FOCUS
     LABEL "Ativar" 
     SIZE 7 BY 1 TOOLTIP "Ativar Integra‡Æo".

DEFINE BUTTON bt-ativaSFA1 
     IMAGE-UP FILE "image/im-play1.bmp":U NO-FOCUS
     LABEL "Ativar" 
     SIZE 7 BY 1 TOOLTIP "Ativar Integra‡Æo".

DEFINE BUTTON bt-ativaSFA2 
     IMAGE-UP FILE "image/im-play1.bmp":U NO-FOCUS
     LABEL "Ativar" 
     SIZE 7 BY 1 TOOLTIP "Ativar Integra‡Æo".

DEFINE BUTTON bt-desativaAriba1 
     IMAGE-UP FILE "image/im-stop.bmp":U NO-FOCUS
     LABEL "Desativar" 
     SIZE 7 BY 1 TOOLTIP "Desativar Integra‡Æo".

DEFINE BUTTON bt-desativaAriba2 
     IMAGE-UP FILE "image/im-stop.bmp":U NO-FOCUS
     LABEL "Desativar" 
     SIZE 7 BY 1 TOOLTIP "Desativar Integra‡Æo".

DEFINE BUTTON bt-desativaSFA1 
     IMAGE-UP FILE "image/im-stop.bmp":U NO-FOCUS
     LABEL "Desativar" 
     SIZE 7 BY 1 TOOLTIP "Desativar Integra‡Æo".

DEFINE BUTTON bt-desativaSFA2 
     IMAGE-UP FILE "image/im-stop.bmp":U NO-FOCUS
     LABEL "Desativar" 
     SIZE 7 BY 1 TOOLTIP "Desativar Integra‡Æo".

DEFINE BUTTON bt-sair 
     IMAGE-UP FILE "image/toolbar/im-exi.bmp":U
     LABEL "Sair" 
     SIZE 5 BY 1.21.

DEFINE VARIABLE fi--direcao-2 AS CHARACTER FORMAT "X(50)":U INITIAL "RESPONSE" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-agentes-ariba1 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-agentes-ariba2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-agentes-sfa1 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-agentes-sfa2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ariba-1 AS CHARACTER FORMAT "X(50)":U INITIAL "ARIBA" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ariba-2 AS CHARACTER FORMAT "X(50)":U INITIAL "ARIBA" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-direcao-1 AS CHARACTER FORMAT "X(50)":U INITIAL "REQUEST" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-direcao-3 AS CHARACTER FORMAT "X(50)":U INITIAL "REQUEST" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-direcao-4 AS CHARACTER FORMAT "X(50)":U INITIAL "RESPONSE" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ativa-ariba1 AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ativa-ariba2 AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ativa-sfa1 AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ativa-sfa2 AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sfa-2 AS CHARACTER FORMAT "X(50)":U INITIAL "SALESFORCE" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sfa-entrada AS CHARACTER FORMAT "X(50)":U INITIAL "SALESFORCE" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 10.96.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 5.25.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 5.25.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 5.25.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 83 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-ativaAriba1 AT ROW 7.63 COL 31.57 WIDGET-ID 60
     bt-ativaAriba2 AT ROW 8.67 COL 31.57 WIDGET-ID 64
     bt-ativaSFA1 AT ROW 5.5 COL 31.57 WIDGET-ID 2
     bt-ativaSFA2 AT ROW 6.58 COL 31.57 WIDGET-ID 56
     bt-desativaAriba1 AT ROW 7.63 COL 39.29 WIDGET-ID 62
     bt-desativaAriba2 AT ROW 8.67 COL 39.29 WIDGET-ID 66
     bt-desativaSFA1 AT ROW 5.5 COL 39.29 WIDGET-ID 4
     bt-desativaSFA2 AT ROW 6.58 COL 39.29 WIDGET-ID 58
     bt-sair AT ROW 1.13 COL 78.43 WIDGET-ID 78
     fi-sfa-entrada AT ROW 5.5 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 52 NO-TAB-STOP 
     fi-direcao-1 AT ROW 5.5 COL 17.29 COLON-ALIGNED NO-LABEL WIDGET-ID 40 NO-TAB-STOP 
     fi-sfa-2 AT ROW 6.58 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 48 NO-TAB-STOP 
     fi--direcao-2 AT ROW 6.58 COL 17.29 COLON-ALIGNED NO-LABEL WIDGET-ID 14 NO-TAB-STOP 
     fi-ariba-1 AT ROW 7.67 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 50 NO-TAB-STOP 
     fi-direcao-3 AT ROW 7.67 COL 17.29 COLON-ALIGNED NO-LABEL WIDGET-ID 34 NO-TAB-STOP 
     fi-ariba-2 AT ROW 8.71 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 54 NO-TAB-STOP 
     fi-direcao-4 AT ROW 8.71 COL 17.29 COLON-ALIGNED NO-LABEL WIDGET-ID 46 NO-TAB-STOP 
     fi-dt-ativa-sfa1 AT ROW 5.54 COL 46.43 COLON-ALIGNED NO-LABEL WIDGET-ID 68 NO-TAB-STOP 
     fi-dt-ativa-sfa2 AT ROW 6.58 COL 46.43 COLON-ALIGNED NO-LABEL WIDGET-ID 70 NO-TAB-STOP 
     fi-dt-ativa-ariba1 AT ROW 7.63 COL 46.43 COLON-ALIGNED NO-LABEL WIDGET-ID 72 NO-TAB-STOP 
     fi-dt-ativa-ariba2 AT ROW 8.71 COL 46.43 COLON-ALIGNED NO-LABEL WIDGET-ID 74 NO-TAB-STOP 
     fi-agentes-sfa1 AT ROW 5.54 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 104 NO-TAB-STOP 
     fi-agentes-sfa2 AT ROW 6.58 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 106 NO-TAB-STOP 
     fi-agentes-ariba1 AT ROW 7.63 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 100 NO-TAB-STOP 
     fi-agentes-ariba2 AT ROW 8.71 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 102 NO-TAB-STOP 
     " Integra‡äes" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 4.75 COL 3.86 WIDGET-ID 82
     " Data/Hora In¡cio" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 4.63 COL 48 WIDGET-ID 86
     " Agentes Conectados" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 4.63 COL 66 WIDGET-ID 90
     RECT-2 AT ROW 2.63 COL 1 WIDGET-ID 8
     rt-button AT ROW 1 COL 1 WIDGET-ID 76
     RECT-3 AT ROW 5 COL 3 WIDGET-ID 80
     RECT-4 AT ROW 5 COL 47.57 WIDGET-ID 84
     RECT-5 AT ROW 5 COL 65 WIDGET-ID 88
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
         TITLE              = "Processo de Integra‡Æo SFA"
         HEIGHT             = 12.83
         WIDTH              = 83.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 109.86
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 109.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON bt-ativaAriba1 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       bt-ativaAriba1:PRIVATE-DATA IN FRAME f-cad     = 
                "2;1".

/* SETTINGS FOR BUTTON bt-ativaAriba2 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       bt-ativaAriba2:PRIVATE-DATA IN FRAME f-cad     = 
                "2;2".

/* SETTINGS FOR BUTTON bt-ativaSFA1 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       bt-ativaSFA1:PRIVATE-DATA IN FRAME f-cad     = 
                "1;1".

/* SETTINGS FOR BUTTON bt-ativaSFA2 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       bt-ativaSFA2:PRIVATE-DATA IN FRAME f-cad     = 
                "1;2".

/* SETTINGS FOR BUTTON bt-desativaAriba1 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desativaAriba2 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desativaSFA1 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desativaSFA2 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-sair IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi--direcao-2:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-agentes-ariba1 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-agentes-ariba1:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-agentes-ariba2 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-agentes-ariba2:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-agentes-sfa1 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-agentes-sfa1:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-agentes-sfa2 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-agentes-sfa2:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fi-ariba-1:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fi-ariba-2:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fi-direcao-1:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fi-direcao-3:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fi-direcao-4:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-dt-ativa-ariba1 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-dt-ativa-ariba1:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-dt-ativa-ariba2 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-dt-ativa-ariba2:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-dt-ativa-sfa1 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-dt-ativa-sfa1:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-dt-ativa-sfa2 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fi-dt-ativa-sfa2:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fi-sfa-2:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fi-sfa-entrada:READ-ONLY IN FRAME f-cad        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Processo de Integra‡Æo SFA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Processo de Integra‡Æo SFA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ativaAriba1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ativaAriba1 w-livre
ON CHOOSE OF bt-ativaAriba1 IN FRAME f-cad /* Ativar */
DO:

    DEF VAR l-ativa      AS LOGICAL NO-UNDO.
    DEF VAR i-ind-trans  AS INTEGER NO-UNDO INITIAL 1.
    DEF VAR i-cd-sistema AS INTEGER NO-UNDO INITIAL 2.
    DEF VAR i-count      AS INTEGER NO-UNDO.
    DEF VAR da-data      AS DATETIME NO-UNDO.

    RUN pi-cria-tt-apps (INPUT i-ind-trans,
                         INPUT i-cd-sistema).

    RUN pi-ativa (INPUT-OUTPUT hnd-apps-server-1-2,       
                  INPUT-OUTPUT hnd-apps-server-async-1-2,
                  INPUT i-ind-trans,
                  input i-cd-sistema).

    RUN pi-verif-exec (input i-ind-trans, 
                       input i-cd-sistema,
                       OUTPUT l-ativa,
                       OUTPUT da-data).
    IF l-ativa THEN
        ASSIGN SELF:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
               bt-desativaAriba1:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    ELSE
        ASSIGN self:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
               bt-desativaAriba1:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
                  
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ativaAriba2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ativaAriba2 w-livre
ON CHOOSE OF bt-ativaAriba2 IN FRAME f-cad /* Ativar */
DO:

    DEF VAR l-ativa AS LOGICAL NO-UNDO.
    DEF VAR i-ind-trans  AS INTEGER NO-UNDO INITIAL 2.
    DEF VAR i-cd-sistema AS INTEGER NO-UNDO INITIAL 2.
    DEF VAR da-data AS DATETIME NO-UNDO.

    RUN pi-cria-tt-apps (INPUT i-ind-trans,
                         INPUT i-cd-sistema).

    RUN pi-ativa (INPUT-OUTPUT hnd-apps-server-2-2,       
                  INPUT-OUTPUT hnd-apps-server-async-2-2,
                  INPUT i-ind-trans,
                  input i-cd-sistema).

    RUN pi-verif-exec (input i-ind-trans, 
                       input i-cd-sistema,
                       OUTPUT l-ativa,
                       OUTPUT da-data).

    IF l-ativa THEN
        ASSIGN SELF:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
               bt-desativaAriba2:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    ELSE
        ASSIGN self:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
               bt-desativaAriba2:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
                  

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ativaSFA1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ativaSFA1 w-livre
ON CHOOSE OF bt-ativaSFA1 IN FRAME f-cad /* Ativar */
DO:

    DEF VAR l-ativa      AS LOGICAL NO-UNDO.
    DEF VAR i-ind-trans  AS INTEGER NO-UNDO INITIAL 1.
    DEF VAR i-cd-sistema AS INTEGER NO-UNDO INITIAL 1.
    DEF VAR i-count      AS INTEGER NO-UNDO.
    DEF VAR da-data      AS DATETIME NO-UNDO.
    DEF VAR i-agent      AS INTEGER  NO-UNDO.

    RUN pi-cria-tt-apps (INPUT i-ind-trans,
                         INPUT i-cd-sistema).

    RUN pi-ativa (input-OUTPUT hnd-apps-server-1-1,       
                  input-OUTPUT hnd-apps-server-async-1-1,
                  INPUT i-ind-trans,
                  input i-cd-sistema).

    RUN pi-verif-exec (input i-ind-trans, 
                       input i-cd-sistema,
                       OUTPUT l-ativa,
                       OUTPUT da-data,
                       OUTPUT i-agent).
    IF l-ativa THEN
        ASSIGN SELF:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
               bt-desativaSFA1:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
               fi-dt-ativa-sfa1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-agentes-sfa1:SCREEN-VALUE = "".
               
    ELSE
        ASSIGN self:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
               bt-desativaSFA1:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
               fi-agentes-sfa1:SCREEN-VALUE = string(i-agent)
               fi-dt-ativa-sfa1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(da-data).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ativaSFA2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ativaSFA2 w-livre
ON CHOOSE OF bt-ativaSFA2 IN FRAME f-cad /* Ativar */
DO:

    DEF VAR l-ativa      AS LOGICAL  NO-UNDO.
    DEF VAR i-ind-trans  AS INTEGER  NO-UNDO INITIAL 2.
    DEF VAR i-cd-sistema AS INTEGER  NO-UNDO INITIAL 1.
    DEF VAR i-count      AS INTEGER  NO-UNDO.
    DEF VAR da-data      AS DATETIME NO-UNDO.
    DEF VAR i-agent      AS INTEGER  NO-UNDO.

    RUN pi-cria-tt-apps (INPUT i-ind-trans,
                         INPUT i-cd-sistema).

    RUN pi-ativa (input-OUTPUT hnd-apps-server-2-1,       
                  input-OUTPUT hnd-apps-server-async-2-1,
                  INPUT i-ind-trans,
                  input i-cd-sistema).

    RUN pi-verif-exec (input i-ind-trans, 
                       input i-cd-sistema,
                       OUTPUT l-ativa,
                       OUTPUT da-data,
                       OUTPUT i-agent).
    IF l-ativa THEN
        ASSIGN SELF:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
               bt-desativaSFA2:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
               fi-agentes-sfa2:SCREEN-VALUE = ""
               fi-dt-ativa-sfa2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    ELSE
        ASSIGN self:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
               bt-desativaSFA2:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
               fi-agentes-sfa2:SCREEN-VALUE = STRING(i-agent)
               fi-dt-ativa-sfa2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(da-data).

                  
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desativaAriba1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desativaAriba1 w-livre
ON CHOOSE OF bt-desativaAriba1 IN FRAME f-cad /* Desativar */
DO:
 
    FOR EACH es-api-exec WHERE es-api-exec.ind-tipo-trans = 1
                           AND es-api-exec.cd-sistema     = 2
                           AND es-api-exec.data-fim       = ? NO-LOCK:
        FIND CURRENT es-api-exec EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN es-api-exec.data-fim    = NOW.
    END.

    FOR EACH tt-apps WHERE tt-apps.ind-tipo-trans = 1
                       AND tt-apps.cd-sistema     = 2:
        DELETE tt-apps.
    END.

    ASSIGN bt-desativaAriba1:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desativaAriba2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desativaAriba2 w-livre
ON CHOOSE OF bt-desativaAriba2 IN FRAME f-cad /* Desativar */
DO:
 
    FOR EACH es-api-exec WHERE es-api-exec.ind-tipo-trans = 2
                           AND es-api-exec.cd-sistema     = 2
                           AND es-api-exec.data-fim       = ? NO-LOCK:
        FIND CURRENT es-api-exec EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN es-api-exec.data-fim    = NOW.
    END.
    FOR EACH tt-apps WHERE tt-apps.ind-tipo-trans = 2
                       AND tt-apps.cd-sistema     = 2:
        DELETE tt-apps.
    END.

    ASSIGN bt-desativaAriba2:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desativaSFA1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desativaSFA1 w-livre
ON CHOOSE OF bt-desativaSFA1 IN FRAME f-cad /* Desativar */
DO:
 
    FOR EACH es-api-exec WHERE es-api-exec.ind-tipo-trans = 1
                           AND es-api-exec.cd-sistema     = 1
                           AND es-api-exec.data-fim       = ? NO-LOCK:
        FIND CURRENT es-api-exec EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN es-api-exec.data-fim    = NOW.        
    END.

    FOR EACH tt-apps WHERE tt-apps.ind-tipo-trans = 1
                       AND tt-apps.cd-sistema     = 1:
        DELETE tt-apps.
    END.

    ASSIGN bt-desativasfa1:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desativaSFA2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desativaSFA2 w-livre
ON CHOOSE OF bt-desativaSFA2 IN FRAME f-cad /* Desativar */
DO:
    
    FOR EACH es-api-exec WHERE es-api-exec.ind-tipo-trans = 2
                           AND es-api-exec.cd-sistema     = 1
                           AND es-api-exec.data-fim       = ? NO-LOCK:
        FIND CURRENT es-api-exec EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN es-api-exec.data-fim    = NOW.
    END.

    FOR EACH tt-apps WHERE tt-apps.ind-tipo-trans = 2
                       AND tt-apps.cd-sistema     = 1:
        DELETE tt-apps.
    END.

    ASSIGN bt-desativasfa2:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair w-livre
ON CHOOSE OF bt-sair IN FRAME f-cad /* Sair */
DO:
  
    APPLY 'close' TO THIS-PROCEDURE.

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
  DISPLAY fi-sfa-entrada fi-direcao-1 fi-sfa-2 fi--direcao-2 fi-ariba-1 
          fi-direcao-3 fi-ariba-2 fi-direcao-4 fi-dt-ativa-sfa1 fi-dt-ativa-sfa2 
          fi-dt-ativa-ariba1 fi-dt-ativa-ariba2 fi-agentes-sfa1 fi-agentes-sfa2 
          fi-agentes-ariba1 fi-agentes-ariba2 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE fi-sfa-entrada fi-direcao-1 fi-sfa-2 fi--direcao-2 fi-ariba-1 
         fi-direcao-3 fi-ariba-2 fi-direcao-4 RECT-2 rt-button RECT-3 RECT-4 
         RECT-5 
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

  RUN pi-ativa-botoes.


  FOR EACH es-api-exec WHERE es-api-exec.cd-sistema = 1 
                         AND es-api-exec.data-fim = ?:
      ASSIGN es-api-exec.data-fim = NOW.
  END.

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
DEFINE INPUT-OUTPUT PARAMETER p-hdl-server      AS HANDLE EXTENT 20 NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-hdl-server-sync AS HANDLE EXTENT 20 NO-UNDO.
DEFINE INPUT        PARAMETER p-ind-trans       AS INTEGER          NO-UNDO.
DEFINE INPUT        PARAMETER p-cod-sistema     AS INTEGER          NO-UNDO.

DEFINE VARIABLE i-seq        AS INTEGER   NO-UNDO.   
DEFINE VARIABLE i-seq-app    AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-erro       AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-nr-agentes AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-host-app   AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-porta-app  AS INTEGER   NO-UNDO.
DEFINE VARIABLE r-table      AS ROWID     NO-UNDO.
DEFINE VARIABLE p-string-aux AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-program     AS CHARACTER NO-UNDO INITIAL "esp/esint001irp.p,esp/esint001erp.p".
DEFINE VARIABLE c-nm-appserv  AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-es-api-exec FOR es-api-exec.

FIND FIRST es-api-app NO-LOCK WHERE es-api-app.cd-sistema = p-cod-sistema NO-ERROR.
IF NOT AVAIL es-api-app THEN RETURN "NOK".

FIND LAST bf-es-api-exec NO-LOCK NO-ERROR.
IF AVAIL bf-es-api-exec THEN
    ASSIGN i-seq = bf-es-api-exec.id-exec + 1.
ELSE ASSIGN i-seq = 1.

CREATE es-api-exec.
ASSIGN es-api-exec.id-exec        = i-seq
       es-api-exec.data-inicio    = NOW
       es-api-exec.data-fim       = ?
       es-api-exec.ind-tipo-trans = p-ind-trans
       es-api-exec.cd-sistema     = p-cod-sistema.   

ASSIGN i-seq     = 0
       i-seq-app = 1.

/*----------- cria temp para fazer o load-balance (caso exista mais de um appserver) -----------*/
FOR EACH tt-apps WHERE tt-apps.ind-tipo-trans = p-ind-trans
                   AND tt-apps.cd-sistema     = p-cod-sistema NO-LOCK BREAK BY tt-apps.nome-appserver:

    IF LAST-OF(tt-apps.nome-appserver) THEN DO:

        ASSIGN p-string-conexao = "-DirectConnect -H " + tt-apps.host-appserver +  " -S " + string(tt-apps.porta-appserver)
               p-string-aux     = p-string-conexao
               r-table          = ROWID(tt-apps)
               i-nr-agentes     = tt-apps.agentes-appserver
               c-nm-appserv     = tt-apps.nome-appserver.
        
        DO i-seq = i-seq-app TO (i-nr-agentes + i-seq-app - 1):

            MESSAGE 'i-seq: ' i-seq SKIP
                    'i-seq-app: ' i-seq-app 
                    'c-nm-app: ' c-nm-appserv VIEW-AS ALERT-BOX INFO BUTTONS OK.

            ASSIGN p-string-conexao = p-string-aux.

            CREATE SERVER p-hdl-server[i-seq].
            RUN pi-conectar-apps (INPUT-OUTPUT p-hdl-server,
                                  INPUT-OUTPUT p-hdl-server-sync,
                                  INPUT i-seq,
                                  INPUT p-ind-trans,   
                                  INPUT p-cod-sistema, 
                                  INPUT r-table,
                                  OUTPUT c-erro) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).

            IF (p-hdl-server[i-seq]:CONNECTED()) THEN DO:

                RUN pi-executa-apps (INPUT-output p-hdl-server,
                                     INPUT-output p-hdl-server-sync,
                                     INPUT i-seq,
                                     INPUT c-nm-appserv,
                                     INPUT p-ind-trans,
                                     INPUT p-cod-sistema,
                                     OUTPUT c-erro) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN ASSIGN c-erro = c-erro + ERROR-STATUS:GET-MESSAGE(1).
            END.
        END.

        i-seq-app        = i-seq-app + i-nr-agentes.

    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ativa-botoes w-livre 
PROCEDURE pi-ativa-botoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE l-ativa AS LOGICAL  NO-UNDO.
DEFINE VARIABLE da-data AS DATETIME NO-UNDO.
DEFINE VARIABLE i-agent AS INTEGER  NO-UNDO.


RUN pi-verif-exec (INPUT 1,
                   INPUT 1,
                   OUTPUT l-ativa,
                   OUTPUT da-data,
                   OUTPUT i-agent).
IF l-ativa THEN
    ASSIGN bt-ativasfa1:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
           bt-desativaSFA1:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
           fi-dt-ativa-sfa1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-agentes-sfa1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           bt-sair:SENSITIVE = YES.
ELSE
    ASSIGN bt-ativasfa1:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
           bt-desativaSFA1:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
           fi-dt-ativa-sfa1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(da-data)
           fi-agentes-sfa1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-agent)
           bt-sair:SENSITIVE = NO.


RUN pi-verif-exec (INPUT 2,
                   INPUT 1,
                   OUTPUT l-ativa,
                   OUTPUT da-data,
                   OUTPUT i-agent).
IF l-ativa THEN
    ASSIGN bt-ativasfa2:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
           bt-desativaSFA2:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
           fi-dt-ativa-sfa2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-agentes-sfa2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           bt-sair:SENSITIVE = YES.
ELSE
    ASSIGN bt-ativasfa2:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
           bt-desativaSFA2:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
           fi-dt-ativa-sfa2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(da-data)
           fi-agentes-sfa2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-agent)
           bt-sair:SENSITIVE = NO.

/*
RUN pi-verif-exec (INPUT 1,
                   INPUT 2,
                   OUTPUT l-ativa).
IF l-ativa THEN
    ASSIGN bt-ativaAriba1:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
           bt-desativaAriba1:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
ELSE 
    ASSIGN bt-ativaAriba1:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
           bt-desativaAriba1:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.

RUN pi-verif-exec (INPUT 2,
                   INPUT 2,
                   OUTPUT l-ativa).
IF l-ativa THEN
    ASSIGN bt-ativaAriba2:SENSITIVE IN FRAME {&FRAME-NAME}    = TRUE
           bt-desativaAriba2:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
ELSE
    ASSIGN bt-ativaAriba2:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
           bt-desativaAriba2:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.

  */

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

DEFINE INPUT-OUTPUT PARAMETER p-hdl-server      AS HANDLE EXTENT 20 NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-hdl-server-sync AS HANDLE EXTENT 20 NO-UNDO.
DEFINE INPUT        PARAMETER p-agente          AS INTEGER          NO-UNDO.
DEFINE INPUT        PARAMETER p-ind-trans       AS INTEGER          NO-UNDO.
DEFINE INPUT        PARAMETER p-cd-sistema      AS INTEGER          NO-UNDO.
DEFINE INPUT        PARAMETER p-rowid           AS ROWID            NO-UNDO.
DEFINE OUTPUT       PARAMETER p-erro            AS CHARACTER        NO-UNDO.
    
DEFINE BUFFER bf-tt-apps FOR tt-apps.

DEFINE VARIABLE ierro  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-serv AS INTEGER     NO-UNDO.

FIND bf-tt-apps WHERE rowid(bf-tt-apps) = p-rowid EXCLUSIVE-LOCK NO-ERROR.

p-hdl-server[p-agente]:CONNECT( p-string-conexao, "", "" ) NO-ERROR.
IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:      
    DO ierro = 1 TO ERROR-STATUS:NUM-MESSAGES:        
        ASSIGN p-erro = p-erro + STRING(ERROR-STATUS:GET-NUMBER(ierro)) + " - " + ERROR-STATUS:GET-MESSAGE(ierro) + CHR(10).
    END.    
END.

IF p-hdl-server[p-agente]:CONNECTED() THEN DO:
    ASSIGN bf-tt-apps.i-agent-conect = bf-tt-apps.i-agent-conect + 1.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-tt-apps w-livre 
PROCEDURE pi-cria-tt-apps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-ind-trans  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-cd-sistema AS INTEGER NO-UNDO.

DEF VAR i-count AS INTEGER NO-UNDO.

FIND FIRST es-api-appserver NO-LOCK WHERE es-api-appserver.ind-tipo-trans = p-ind-trans
                                      AND es-api-appserver.cd-sistema = p-cd-sistema NO-ERROR.
IF NOT AVAIL es-api-appserver THEN DO:
    MESSAGE "Parƒmetro de AppServer nÆo informado no cadastro de Sistemas (esint003)" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN NO-APPLY.
END.

IF NOT CAN-FIND(FIRST tt-apps WHERE tt-apps.ind-tipo-trans = p-ind-trans
                                AND tt-apps.cd-sistema     = p-cd-sistema ) THEN DO:
    ASSIGN i-count = 0.

    FOR EACH es-api-appserver WHERE es-api-appserver.ind-tipo-trans = p-ind-trans
                                AND es-api-appserver.cd-sistema     = p-cd-sistema NO-LOCK:       
        ASSIGN i-count = i-count + 1.

        CREATE tt-apps.
        BUFFER-COPY es-api-appserver TO tt-apps.
        ASSIGN tt-apps.i-agent-conect = 0.

        IF i-count = 1 THEN
            ASSIGN tt-apps.l-padrao = YES.
    END.
END.


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
DEFINE INPUT PARAM p-hnd-apps-server AS HANDLE  NO-UNDO.
DEFINE INPUT PARAM p-agente          AS INTEGER NO-UNDO.

IF VALID-HANDLE(p-hnd-apps-server) THEN DO:
    p-hnd-apps-server:CANCEL-REQUESTS().
    p-hnd-apps-server:DISCONNECT() .
    DELETE OBJECT p-hnd-apps-server.
    ASSIGN p-hnd-apps-server = ?.
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

DEFINE INPUT-OUTPUT PARAMETER p-hdl-server      AS HANDLE EXTENT 20 NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-hdl-server-sync AS HANDLE EXTENT 20 NO-UNDO.
DEFINE INPUT        PARAMETER p-agente          AS INTEGER          NO-UNDO.
DEFINE INPUT        PARAMETER p-nm-appserv      AS CHARACTER        NO-UNDO.
DEFINE INPUT        PARAMETER p-ind-trans       AS INTEGER          NO-UNDO.
DEFINE INPUT        PARAMETER p-cod-sist        AS INTEGER          NO-UNDO.     
DEFINE OUTPUT       PARAMETER p-erro            AS CHARACTER        NO-UNDO.

DEFINE VARIABLE i-ind-trans   AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-cod-sist    AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-agente      AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-program     AS CHARACTER NO-UNDO INITIAL "esp/esint001irp.p,esp/esint001erp.p".
DEFINE VARIABLE c-nm-appserv  AS CHARACTER NO-UNDO.

ASSIGN i-ind-trans  = p-ind-trans
       i-cod-sist   = p-cod-sist
       i-agente     = p-agente
       c-nm-appserv = p-nm-appserv.

IF (p-hdl-server[p-agente]:CONNECTED()) THEN DO:
    RUN VALUE(entry(p-ind-trans,c-program)) ON p-hdl-server[p-agente] ASYNCHRONOUS SET p-hdl-server-sync[p-agente]
    EVENT-PROCEDURE "pi-finaliza" (INPUT-OUTPUT i-ind-trans, INPUT-OUTPUT i-cod-sist, INPUT-OUTPUT i-agente, INPUT-OUTPUT c-nm-appserv).     
END.


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

DEFINE INPUT PARAMETER p-ind-trans  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-cd-sist    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-seq        AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-nm-appserv AS CHARACTER NO-UNDO.

CASE p-ind-trans:

    WHEN 1 THEN DO:
        IF p-cd-sist = 1 THEN RUN pi-desconectar-apps (INPUT hnd-apps-server-1-1[p-seq], INPUT p-seq) .
        ELSE IF p-cd-sist = 2 THEN RUN pi-desconectar-apps (INPUT hnd-apps-server-1-2[p-seq], INPUT p-seq) .
    END.
    WHEN 2 THEN DO:
        IF p-cd-sist = 1 THEN RUN pi-desconectar-apps (INPUT hnd-apps-server-2-1[p-seq], INPUT p-seq) .
        ELSE IF p-cd-sist = 2 THEN RUN pi-desconectar-apps (INPUT hnd-apps-server-2-2[p-seq], INPUT p-seq) .
    END.

END CASE.

RUN pi-ativa-botoes.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verif-exec w-livre 
PROCEDURE pi-verif-exec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER i-ind-trans  AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER i-cd-sistema AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER p-ativa      AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER p-data       AS DATETIME NO-UNDO.
DEFINE OUTPUT PARAMETER p-agentes    AS INTEGER  NO-UNDO.

FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = i-ind-trans
                          AND es-api-param.cd-sistema     = i-cd-sistema NO-LOCK NO-ERROR.
IF AVAIL es-api-param THEN DO:

    FIND FIRST es-api-exec WHERE es-api-exec.cd-sistema     = es-api-param.cd-sistema
                             AND es-api-exec.ind-tipo-trans = es-api-param.ind-tipo-trans
                             AND es-api-exec.data-fim = ? NO-LOCK NO-ERROR.
    IF AVAIL es-api-exec THEN DO:
        ASSIGN p-ativa = NO
               p-data  = es-api-exec.data-inicio.
        FOR EACH tt-apps WHERE tt-apps.ind-tipo-trans = es-api-param.ind-tipo-trans
                           AND tt-apps.cd-sistema     = es-api-param.cd-sistema:
            ASSIGN p-agentes = p-agentes  + tt-apps.i-agent-conect.
        END.
    END.
    ELSE
        ASSIGN p-ativa = YES
               p-data  = ?
               p-agentes = 0.
END.


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

/*
RUN pi-conectar-apps.

 IF (hnd_apps_server[1]:CONNECTED()) THEN DO:
     RUN  pi-desconectar-apps.
 END.
 */

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

