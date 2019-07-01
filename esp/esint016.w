&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcam            PROGRESS
*/
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
{include/i-prgvrs.i esint016 1.00.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esint016 <m¢dulo>}
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
DEF BUFFER portador FOR mgcad.portador.

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
&Scoped-Define ENABLED-FIELDS es-api-param-cliente.portador ~
es-api-param-cliente.modalidade es-api-param-cliente.port-prefer ~
es-api-param-cliente.tp-rec-padrao es-api-param-cliente.cod-cond-pag ~
es-api-param-cliente.cod-transp es-api-param-cliente.cod-gr-cli ~
es-api-param-cliente.nat-operacao es-api-param-cliente.perc-fat-ped ~
es-api-param-cliente.nat-ope-ext es-api-param-cliente.mod-prefer ~
es-api-param-cliente.log-optan-suspens-ipi es-api-param-cliente.emite-bloq ~
es-api-param-cliente.agente-retencao es-api-param-cliente.ind-fat-par ~
es-api-param-cliente.log-calcula-pis-cofins-unid ~
es-api-param-cliente.log-nf-eletro es-api-param-cliente.natureza ~
es-api-param-cliente.esp-pd-venda 
&Scoped-define ENABLED-TABLES es-api-param-cliente
&Scoped-define FIRST-ENABLED-TABLE es-api-param-cliente
&Scoped-Define ENABLED-OBJECTS bt-alterar bt-salvar rt-button RECT-14 ~
RECT-18 RECT-22 RECT-23 RECT-24 RECT-25 
&Scoped-Define DISPLAYED-FIELDS es-api-param-cliente.portador ~
es-api-param-cliente.modalidade es-api-param-cliente.port-prefer ~
es-api-param-cliente.tp-rec-padrao es-api-param-cliente.cod-cond-pag ~
es-api-param-cliente.cod-transp es-api-param-cliente.cod-gr-cli ~
es-api-param-cliente.nat-operacao es-api-param-cliente.perc-fat-ped ~
es-api-param-cliente.nat-ope-ext es-api-param-cliente.mod-prefer ~
es-api-param-cliente.log-optan-suspens-ipi es-api-param-cliente.emite-bloq ~
es-api-param-cliente.agente-retencao es-api-param-cliente.ind-fat-par ~
es-api-param-cliente.log-calcula-pis-cofins-unid ~
es-api-param-cliente.log-nf-eletro es-api-param-cliente.natureza ~
es-api-param-cliente.esp-pd-venda 
&Scoped-define DISPLAYED-TABLES es-api-param-cliente
&Scoped-define FIRST-DISPLAYED-TABLE es-api-param-cliente
&Scoped-Define DISPLAYED-OBJECTS fi-det-1 fi-det-2 fi-det-3 fi-det-4 ~
fi-det-5 fi-det-6 

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
DEFINE BUTTON bt-alterar 
     IMAGE-UP FILE "image/toolbar/im-mod.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Alterar" 
     SIZE 4.29 BY 1.29 TOOLTIP "Gravar parÉmetros".

DEFINE BUTTON bt-salvar 
     IMAGE-UP FILE "image/toolbar/im-tick.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Salvar" 
     SIZE 4.29 BY 1.29 TOOLTIP "Gravar parÉmetros".

DEFINE VARIABLE fi-det-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-det-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-det-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-det-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-det-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-det-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.72 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 8.5.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 8.75.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.43 BY 4.04.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 8.75.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 4.08.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 4.08.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 106 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-alterar AT ROW 1.08 COL 1.43 WIDGET-ID 124
     bt-salvar AT ROW 1.08 COL 6 WIDGET-ID 120
     es-api-param-cliente.portador AT ROW 3.63 COL 24 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     fi-det-1 AT ROW 3.63 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     es-api-param-cliente.modalidade AT ROW 3.88 COL 85 NO-LABEL WIDGET-ID 40
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Cb Simples", 1,
"Desconto", 2,
"Cauá∆o", 3,
"Judicial", 4,
"Repres", 5,
"Carteira", 6,
"Vendor", 7,
"Cheque", 8,
"Nota Promiss¢ria", 9
          SIZE 19.72 BY 7.5
     es-api-param-cliente.port-prefer AT ROW 4.63 COL 24 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     fi-det-2 AT ROW 4.63 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     es-api-param-cliente.tp-rec-padrao AT ROW 5.58 COL 24 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     fi-det-3 AT ROW 5.58 COL 31.14 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     es-api-param-cliente.cod-cond-pag AT ROW 6.54 COL 24 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     fi-det-4 AT ROW 6.54 COL 31.14 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     es-api-param-cliente.cod-transp AT ROW 7.5 COL 24 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     fi-det-5 AT ROW 7.5 COL 32.14 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     es-api-param-cliente.cod-gr-cli AT ROW 8.46 COL 24 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     fi-det-6 AT ROW 8.46 COL 32.29 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     es-api-param-cliente.nat-operacao AT ROW 9.42 COL 24 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     es-api-param-cliente.perc-fat-ped AT ROW 9.46 COL 66.43 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     es-api-param-cliente.nat-ope-ext AT ROW 10.33 COL 24 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     es-api-param-cliente.mod-prefer AT ROW 12.67 COL 85 NO-LABEL WIDGET-ID 30
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Cb Simples", 1,
"Desconto", 2,
"Cauá∆o", 3,
"Judicial", 4,
"Repres", 5,
"Carteira", 6,
"Vendor", 7,
"Cheque", 8,
"Nota Promiss¢ria", 9
          SIZE 19.72 BY 7.5
     es-api-param-cliente.log-optan-suspens-ipi AT ROW 12.75 COL 4 WIDGET-ID 122
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .83
     es-api-param-cliente.emite-bloq AT ROW 12.75 COL 47 WIDGET-ID 16
          VIEW-AS TOGGLE-BOX
          SIZE 17.72 BY .83
     es-api-param-cliente.agente-retencao AT ROW 13.46 COL 4 WIDGET-ID 8
          VIEW-AS TOGGLE-BOX
          SIZE 19.14 BY .83
     es-api-param-cliente.ind-fat-par AT ROW 13.46 COL 47 WIDGET-ID 22
          VIEW-AS TOGGLE-BOX
          SIZE 22.14 BY .83
     es-api-param-cliente.log-calcula-pis-cofins-unid AT ROW 14.21 COL 4 WIDGET-ID 24
          VIEW-AS TOGGLE-BOX
          SIZE 31.43 BY .83
     es-api-param-cliente.log-nf-eletro AT ROW 14.21 COL 47 WIDGET-ID 26
          VIEW-AS TOGGLE-BOX
          SIZE 15.43 BY .83
     es-api-param-cliente.natureza AT ROW 17 COL 5 NO-LABEL WIDGET-ID 54
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Pessoa F°sica", 1,
"Pessoa Jur°dica", 2,
"Estrangeiro", 3,
"Trading", 4
          SIZE 20 BY 3.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 132.86 BY 19.67 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-cad
     es-api-param-cliente.esp-pd-venda AT ROW 17.25 COL 44 NO-LABEL WIDGET-ID 18
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Pedido Simples", 1,
"Programaá∆o Entrega", 2
          SIZE 23.57 BY 1.67
     " Modalidade" VIEW-AS TEXT
          SIZE 8.86 BY .67 AT ROW 2.75 COL 83.14 WIDGET-ID 118
     " Modalidade Preferencial" VIEW-AS TEXT
          SIZE 17 BY .67 AT ROW 11.79 COL 83 WIDGET-ID 116
     " EspÇcie Pedido de Venda" VIEW-AS TEXT
          SIZE 20 BY .67 AT ROW 16.25 COL 43 WIDGET-ID 114
     " Natureza" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 16.17 COL 3.29 WIDGET-ID 112
     rt-button AT ROW 1 COL 1
     RECT-14 AT ROW 12 COL 82 WIDGET-ID 72
     RECT-18 AT ROW 3 COL 82 WIDGET-ID 80
     RECT-22 AT ROW 12 COL 1.57 WIDGET-ID 104
     RECT-23 AT ROW 3 COL 1.72 WIDGET-ID 106
     RECT-24 AT ROW 16.42 COL 1.86 WIDGET-ID 108
     RECT-25 AT ROW 16.42 COL 42 WIDGET-ID 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 132.86 BY 19.67 WIDGET-ID 100.


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
         TITLE              = "ParÉmetro padr∆o de clientes - Integraá∆o SFA"
         HEIGHT             = 19.67
         WIDTH              = 106.29
         MAX-HEIGHT         = 19.67
         MAX-WIDTH          = 133
         VIRTUAL-HEIGHT     = 19.67
         VIRTUAL-WIDTH      = 133
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
/* SETTINGS FOR FILL-IN fi-det-1 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-det-2 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-det-3 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-det-4 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-det-5 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-det-6 IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* ParÉmetro padr∆o de clientes - Integraá∆o SFA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* ParÉmetro padr∆o de clientes - Integraá∆o SFA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-livre
ON CHOOSE OF bt-alterar IN FRAME f-cad /* Alterar */
DO:
  
    RUN pi-enable.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-livre
ON CHOOSE OF bt-salvar IN FRAME f-cad /* Salvar */
DO:
  
    IF NOT AVAIL es-api-param-cliente THEN
        CREATE es-api-param-cliente.

    ASSIGN FRAME {&FRAME-NAME}
         es-api-param-cliente.agente-retencao es-api-param-cliente.cod-cond-pag es-api-param-cliente.cod-gr-cli 
        es-api-param-cliente.cod-transp es-api-param-cliente.emite-bloq es-api-param-cliente.esp-pd-venda 
        es-api-param-cliente.ind-fat-par es-api-param-cliente.log-calcula-pis-cofins-unid es-api-param-cliente.log-nf-eletro 
         es-api-param-cliente.mod-prefer es-api-param-cliente.modalidade 
        es-api-param-cliente.nat-ope-ext es-api-param-cliente.nat-operacao es-api-param-cliente.natureza es-api-param-cliente.perc-fat-ped 
        es-api-param-cliente.port-prefer es-api-param-cliente.portador es-api-param-cliente.tp-rec-padrao
        es-api-param-cliente.log-optan-suspens-ipi . 

    RUN pi-disable.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-cliente.cod-cond-pag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-cond-pag w-livre
ON F5 OF es-api-param-cliente.cod-cond-pag IN FRAME f-cad /* Condiá∆o Pagamento */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="adzoom/z01ad039.w"
                  &campo=es-api-param-cliente.cod-cond-pag
                  &campozoom=cod-cond-pag}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-cond-pag w-livre
ON LEAVE OF es-api-param-cliente.cod-cond-pag IN FRAME f-cad /* Condiá∆o Pagamento */
DO:
    FIND FIRST cond-pagto WHERE cond-pagto.cod-cond-pag = input frame {&frame-name} es-api-param-cliente.cod-cond-pag NO-LOCK NO-ERROR.
    IF AVAIL cond-pagto THEN
        DISP cond-pagto.descricao @ fi-det-4 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-cond-pag w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.cod-cond-pag IN FRAME f-cad /* Condiá∆o Pagamento */
DO:
  APPLY 'F5' TO es-api-param-cliente.cod-cond-pag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-cliente.cod-gr-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-gr-cli w-livre
ON F5 OF es-api-param-cliente.cod-gr-cli IN FRAME f-cad /* Grupo */
DO:
  assign l-implanta = yes.
  {include/zoom.i &prog-zoom="adzoom/z01ad129.w"
                  &tabela=es-api-param-cliente
                  &atributo=cod-gr-cli}  
                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-gr-cli w-livre
ON LEAVE OF es-api-param-cliente.cod-gr-cli IN FRAME f-cad /* Grupo */
DO:

    FIND FIRST gr-cli WHERE gr-cli.cod-gr-cli = input frame {&frame-name} es-api-param-cliente.cod-gr-cli NO-LOCK NO-ERROR.
    IF AVAIL gr-cli THEN
        DISP gr-cli.descricao @ fi-det-6 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-gr-cli w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.cod-gr-cli IN FRAME f-cad /* Grupo */
DO:
  APPLY 'F5' TO es-api-param-cliente.cod-gr-cli.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-cliente.cod-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-transp w-livre
ON F5 OF es-api-param-cliente.cod-transp IN FRAME f-cad /* Transportador Padr∆o */
DO:
  assign l-implanta = yes.
  {include/zoom.i &prog-zoom="adzoom/z01ad268.w"
                     &tabela=es-api-param-cliente
                   &atributo=cod-transp}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-transp w-livre
ON LEAVE OF es-api-param-cliente.cod-transp IN FRAME f-cad /* Transportador Padr∆o */
DO:
  

    FIND FIRST transporte WHERE transporte.cod-transp = input frame {&frame-name} es-api-param-cliente.cod-transp NO-LOCK NO-ERROR.
    IF AVAIL transporte THEN
        DISP transporte.nome-abrev @ fi-det-5 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.cod-transp w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.cod-transp IN FRAME f-cad /* Transportador Padr∆o */
DO:
  APPLY 'F5' TO es-api-param-cliente.cod-transp.
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


&Scoped-define SELF-NAME es-api-param-cliente.nat-ope-ext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.nat-ope-ext w-livre
ON F5 OF es-api-param-cliente.nat-ope-ext IN FRAME f-cad /* Natureza Interestadual */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                     &campo=es-api-param-cliente.nat-ope-ext
                     &campozoom=nat-operacao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.nat-ope-ext w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.nat-ope-ext IN FRAME f-cad /* Natureza Interestadual */
DO:
  APPLY 'F5' TO es-api-param-cliente.nat-ope-ext.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-cliente.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.nat-operacao w-livre
ON F5 OF es-api-param-cliente.nat-operacao IN FRAME f-cad /* Natureza Operaá∆o */
DO:
    assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                     &campo=es-api-param-cliente.nat-operacao
                     &campozoom=nat-operacao}
                     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.nat-operacao w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.nat-operacao IN FRAME f-cad /* Natureza Operaá∆o */
DO:
  APPLY 'F5' TO es-api-param-cliente.nat-operacao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-cliente.port-prefer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.port-prefer w-livre
ON F5 OF es-api-param-cliente.port-prefer IN FRAME f-cad /* Port Preferencial */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="adzoom/z01ad209.w"
                     &campo=es-api-param-cliente.port-prefer
                     &campozoom=cod-portador}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.port-prefer w-livre
ON LEAVE OF es-api-param-cliente.port-prefer IN FRAME f-cad /* Port Preferencial */
DO:
    FIND FIRST portador WHERE portador.cod-portador = input frame {&frame-name} es-api-param-cliente.port-prefer NO-LOCK NO-ERROR.
    IF AVAIL portador THEN
        DISP portador.nome @ fi-det-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.port-prefer w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.port-prefer IN FRAME f-cad /* Port Preferencial */
DO:
  APPLY 'F5' TO es-api-param-cliente.port-pref.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-cliente.portador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.portador w-livre
ON F5 OF es-api-param-cliente.portador IN FRAME f-cad /* Portador */
DO:
  ASSIGN l-implanta = yes.
  {include/zoomvar.i &prog-zoom="adzoom/z01ad209.w"
                     &campo=es-api-param-cliente.portador
                     &campozoom=cod-portador}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.portador w-livre
ON LEAVE OF es-api-param-cliente.portador IN FRAME f-cad /* Portador */
DO:
    FIND FIRST portador WHERE portador.cod-portador = input frame {&frame-name} es-api-param-cliente.portador NO-LOCK NO-ERROR.
    IF AVAIL portador THEN
        DISP portador.nome @ fi-det-1 WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.portador w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.portador IN FRAME f-cad /* Portador */
DO:
  APPLY 'F5' TO es-api-param-cliente.portador.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-cliente.tp-rec-padrao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.tp-rec-padrao w-livre
ON F5 OF es-api-param-cliente.tp-rec-padrao IN FRAME f-cad /* Receita Padr∆o */
DO:
  assign l-implanta = yes.
     {include/zoomvar.i &prog-zoom=adzoom/z01ad259.w
                        &campo=es-api-param-cliente.tp-rec-padrao
                        &campozoom=tp-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.tp-rec-padrao w-livre
ON LEAVE OF es-api-param-cliente.tp-rec-padrao IN FRAME f-cad /* Receita Padr∆o */
DO:
 
    FIND FIRST tipo-rec-desp WHERE tipo-rec-desp.tp-codigo = input frame {&frame-name} es-api-param-cliente.tp-rec-padrao NO-LOCK NO-ERROR.
    IF AVAIL tipo-rec-desp THEN
        DISP tipo-rec-desp.descricao @ fi-det-3 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-cliente.tp-rec-padrao w-livre
ON MOUSE-SELECT-DBLCLICK OF es-api-param-cliente.tp-rec-padrao IN FRAME f-cad /* Receita Padr∆o */
DO:
  APPLY 'F5' TO es-api-param-cliente.tp-rec-padrao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

es-api-param-cliente.portador:load-mouse-pointer      ('image/lupa.cur') in frame {&frame-name}.  
es-api-param-cliente.port-prefer:load-mouse-pointer   ('image/lupa.cur') in frame {&frame-name}.  
es-api-param-cliente.tp-rec-padrao:load-mouse-pointer ('image/lupa.cur') in frame {&frame-name}.  
es-api-param-cliente.cod-cond-pag:load-mouse-pointer  ('image/lupa.cur') in frame {&frame-name}.  
es-api-param-cliente.cod-transp:load-mouse-pointer    ('image/lupa.cur') in frame {&frame-name}.  
es-api-param-cliente.cod-gr-cli:load-mouse-pointer    ('image/lupa.cur') in frame {&frame-name}.  
es-api-param-cliente.nat-operacao:load-mouse-pointer    ('image/lupa.cur') in frame {&frame-name}.  
es-api-param-cliente.nat-ope-ext:load-mouse-pointer    ('image/lupa.cur') in frame {&frame-name}.

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
       RUN set-position IN h_p-exihel ( 1.13 , 90.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
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
  DISPLAY fi-det-1 fi-det-2 fi-det-3 fi-det-4 fi-det-5 fi-det-6 
      WITH FRAME f-cad IN WINDOW w-livre.
  IF AVAILABLE es-api-param-cliente THEN 
    DISPLAY es-api-param-cliente.portador es-api-param-cliente.modalidade 
          es-api-param-cliente.port-prefer es-api-param-cliente.tp-rec-padrao 
          es-api-param-cliente.cod-cond-pag es-api-param-cliente.cod-transp 
          es-api-param-cliente.cod-gr-cli es-api-param-cliente.nat-operacao 
          es-api-param-cliente.perc-fat-ped es-api-param-cliente.nat-ope-ext 
          es-api-param-cliente.mod-prefer 
          es-api-param-cliente.log-optan-suspens-ipi 
          es-api-param-cliente.emite-bloq es-api-param-cliente.agente-retencao 
          es-api-param-cliente.ind-fat-par 
          es-api-param-cliente.log-calcula-pis-cofins-unid 
          es-api-param-cliente.log-nf-eletro es-api-param-cliente.natureza 
          es-api-param-cliente.esp-pd-venda 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-alterar bt-salvar rt-button RECT-14 RECT-18 RECT-22 RECT-23 RECT-24 
         RECT-25 es-api-param-cliente.portador es-api-param-cliente.modalidade 
         es-api-param-cliente.port-prefer es-api-param-cliente.tp-rec-padrao 
         es-api-param-cliente.cod-cond-pag es-api-param-cliente.cod-transp 
         es-api-param-cliente.cod-gr-cli es-api-param-cliente.nat-operacao 
         es-api-param-cliente.perc-fat-ped es-api-param-cliente.nat-ope-ext 
         es-api-param-cliente.mod-prefer 
         es-api-param-cliente.log-optan-suspens-ipi 
         es-api-param-cliente.emite-bloq es-api-param-cliente.agente-retencao 
         es-api-param-cliente.ind-fat-par 
         es-api-param-cliente.log-calcula-pis-cofins-unid 
         es-api-param-cliente.log-nf-eletro es-api-param-cliente.natureza 
         es-api-param-cliente.esp-pd-venda 
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

  {utp/ut9000.i "esint016" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

  RUN pi-inicio.

  RUN pi-disable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-disable w-livre 
PROCEDURE pi-disable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE es-api-param-cliente.agente-retencao es-api-param-cliente.cod-cond-pag 
      es-api-param-cliente.cod-gr-cli es-api-param-cliente.cod-transp es-api-param-cliente.emite-bloq 
      es-api-param-cliente.esp-pd-venda es-api-param-cliente.ind-fat-par es-api-param-cliente.log-calcula-pis-cofins-unid 
      es-api-param-cliente.log-nf-eletro es-api-param-cliente.mod-prefer 
      es-api-param-cliente.modalidade es-api-param-cliente.nat-ope-ext es-api-param-cliente.nat-operacao 
      es-api-param-cliente.natureza es-api-param-cliente.perc-fat-ped es-api-param-cliente.port-prefer 
      es-api-param-cliente.portador es-api-param-cliente.tp-rec-padrao es-api-param-cliente.log-optan-suspens-ipi WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-enable w-livre 
PROCEDURE pi-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ENABLE es-api-param-cliente.agente-retencao es-api-param-cliente.cod-cond-pag 
      es-api-param-cliente.cod-gr-cli es-api-param-cliente.cod-transp es-api-param-cliente.emite-bloq 
      es-api-param-cliente.esp-pd-venda es-api-param-cliente.ind-fat-par es-api-param-cliente.log-calcula-pis-cofins-unid 
      es-api-param-cliente.log-nf-eletro es-api-param-cliente.mod-prefer 
      es-api-param-cliente.modalidade es-api-param-cliente.nat-ope-ext es-api-param-cliente.nat-operacao 
      es-api-param-cliente.natureza es-api-param-cliente.perc-fat-ped es-api-param-cliente.port-prefer 
      es-api-param-cliente.portador es-api-param-cliente.tp-rec-padrao es-api-param-cliente.log-optan-suspens-ipi WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inicio w-livre 
PROCEDURE pi-inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST es-api-param-cliente NO-ERROR.
IF NOT AVAIL es-api-param-cliente THEN DO:
    CREATE es-api-param-cliente.
    ASSIGN FRAME {&FRAME-NAME} es-api-param-cliente.agente-retencao es-api-param-cliente.cod-cond-pag 
    es-api-param-cliente.cod-gr-cli es-api-param-cliente.cod-transp es-api-param-cliente.emite-bloq 
    es-api-param-cliente.esp-pd-venda es-api-param-cliente.ind-fat-par es-api-param-cliente.log-calcula-pis-cofins-unid 
    es-api-param-cliente.log-nf-eletro es-api-param-cliente.mod-prefer 
    es-api-param-cliente.modalidade es-api-param-cliente.nat-ope-ext es-api-param-cliente.nat-operacao 
    es-api-param-cliente.natureza es-api-param-cliente.perc-fat-ped es-api-param-cliente.port-prefer 
    es-api-param-cliente.portador es-api-param-cliente.tp-rec-padrao es-api-param-cliente.log-optan-suspens-ipi.
END.

DISP es-api-param-cliente.agente-retencao es-api-param-cliente.cod-cond-pag 
    es-api-param-cliente.cod-gr-cli es-api-param-cliente.cod-transp es-api-param-cliente.emite-bloq 
    es-api-param-cliente.esp-pd-venda es-api-param-cliente.ind-fat-par es-api-param-cliente.log-calcula-pis-cofins-unid 
    es-api-param-cliente.log-nf-eletro es-api-param-cliente.mod-prefer 
    es-api-param-cliente.modalidade es-api-param-cliente.nat-ope-ext es-api-param-cliente.nat-operacao 
    es-api-param-cliente.natureza es-api-param-cliente.perc-fat-ped es-api-param-cliente.port-prefer 
    es-api-param-cliente.portador es-api-param-cliente.tp-rec-padrao  es-api-param-cliente.log-optan-suspens-ipi WITH FRAME {&FRAME-NAME}.

APPLY 'leave' TO es-api-param-cliente.portador.
APPLY 'leave' TO es-api-param-cliente.port-prefer.
APPLY 'leave' TO es-api-param-cliente.tp-rec-padrao.
APPLY 'leave' TO es-api-param-cliente.cod-cond-pag.
APPLY 'leave' TO es-api-param-cliente.cod-gr-cli.
APPLY 'leave' TO es-api-param-cliente.cod-transp.

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

