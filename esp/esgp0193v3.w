&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcam            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i ESGP0193V3 2.09.00.002 } /*** "019002" ***/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEFINE VARIABLE c-origem  AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE c-destino AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE c-rota    AS CHARACTER FORMAT "x(50)" NO-UNDO.

DEFINE VARIABLE h-bodi582    AS HANDLE      NO-UNDO.
/* {dibo/bodi582.i1 tt-teste-negoc tt-teste-rota tt-teste-faixa tt-teste-tarifa tt-teste-comp-tarifa tt-teste-cotacao tt-teste-tab-frete}  */

DEFINE TEMP-TABLE tt-teste-negoc NO-UNDO
        FIELD GV9_FILIAL AS CHAR                       
        FIELD GV9_CDEMIT AS CHAR                      
        FIELD GV9_NRTAB  AS CHAR                    
        FIELD GV9_NRNEG  AS CHAR                     
        FIELD GV9_CDCLFR AS CHAR                      
        FIELD GV9_DSCLFR AS CHAR                      
        FIELD GV9_CDTPOP AS CHAR                      
        FIELD GV9_DTVALI AS CHAR                      
        FIELD GV9_DTVALF AS CHAR                     
        FIELD GV9_TPLOTA AS CHAR                      
        FIELD GV9_SITMLA AS CHAR                      
        FIELD GV9_SIT    AS CHAR                     
        FIELD GV9_DTCRIA AS CHAR                      
        FIELD GV9_USUCRI AS CHAR. 
       
DEFINE TEMP-TABLE tt-teste-rota NO-UNDO
        FIELD GV8_FILIAL AS CHAR                       
        FIELD GV8_CDEMIT AS CHAR                      
        FIELD GV8_NRTAB  AS CHAR                    
        FIELD GV8_NRNEG  AS CHAR 
        FIELD GV8_NRROTA AS CHAR
        FIELD GV8_TPORIG AS CHAR
        FIELD GV8_NRCIOR AS CHAR
        FIELD GV8_DSCIOR AS CHAR
        FIELD GV8_DSTORI AS CHAR
        FIELD GV8_DSTORF AS CHAR
        FIELD GV8_NRREOR AS CHAR
        FIELD GV8_CDPAOR AS CHAR
        FIELD GV8_DSPAOR AS CHAR
        FIELD GV8_CDUFOR AS CHAR
        FIELD GV8_NMREM  AS CHAR
        FIELD GV8_TPDEST AS CHAR
        FIELD GV8_NRCIDS AS CHAR
        FIELD GV8_DSCIDS AS CHAR
        FIELD GV8_DSTDEI AS CHAR
        FIELD GV8_DSTDEF AS CHAR
        FIELD GV8_NRREDS AS CHAR
        FIELD GV8_CDPADS AS CHAR
        FIELD GV8_DSPADS AS CHAR
        FIELD GV8_CDUFDS AS CHAR
        FIELD GV8_CDDEST AS CHAR
        FIELD GV8_NMDEST AS CHAR
        FIELD GV8_CDREM  AS CHAR.
        
DEFINE TEMP-TABLE tt-teste-faixa NO-UNDO
        FIELD GV7_FILIAL AS CHAR                       
        FIELD GV7_CDEMIT AS CHAR                      
        FIELD GV7_NRTAB  AS CHAR                    
        FIELD GV7_NRNEG  AS CHAR
        FIELD GV7_CDFXTV AS CHAR        
        FIELD GV7_CDTPVC AS CHAR
        FIELD GV7_DSTPVC AS CHAR
        FIELD GV7_QTFXFI AS CHAR
        FIELD GV7_UNICAL AS CHAR
        FIELD GV7_VLALUG AS CHAR
        FIELD GV7_FRQKM  AS CHAR
        FIELD GV7_VLKMEX AS CHAR.
        
DEFINE TEMP-TABLE tt-teste-tarifa NO-UNDO
        FIELD GV6_FILIAL AS CHAR
        FIELD GV6_CDEMIT AS CHAR
        FIELD GV6_NRTAB  AS CHAR
        FIELD GV6_NRNEG  AS CHAR
        FIELD GV6_CDFXTV AS CHAR
        FIELD GV6_NRROTA AS CHAR
        FIELD GV6_CONSPZ AS CHAR
        FIELD GV6_QTPRAZ AS CHAR
        FIELD GV6_TPPRAZ AS CHAR
        FIELD GV6_CONTPZ AS CHAR.
        
DEFINE TEMP-TABLE tt-teste-comp-tarifa NO-UNDO
        FIELD GV1_FILIAL AS CHAR
        FIELD GV1_CDEMIT AS CHAR
        FIELD GV1_NRTAB  AS CHAR
        FIELD GV1_NRNEG  AS CHAR
        FIELD GV1_CDFXTV AS CHAR
        FIELD GV1_NRROTA AS CHAR
        FIELD GV1_CDCOMP AS CHAR
        FIELD GV1_VLFIXN AS CHAR
        FIELD GV1_PCNORM AS CHAR
        FIELD GV1_VLUNIN AS CHAR
        FIELD GV1_VLFRAC AS CHAR
        FIELD GV1_VLMINN AS CHAR
        FIELD GV1_VLLIM  AS CHAR
        FIELD GV1_VLFIXE AS CHAR
        FIELD GV1_PCEXTR AS CHAR
        FIELD GV1_VLUNIE AS CHAR
        FIELD GV1_CALCEX AS CHAR.
        
DEFINE TEMP-TABLE tt-teste-cotacao NO-UNDO
        FIELD GVQ_FILIAL AS CHAR
        FIELD GVQ_CDEMIT AS CHAR
        FIELD GVQ_NRTAB  AS CHAR
        FIELD GVQ_NRNEG  AS CHAR
        FIELD GVQ_CODCOT AS CHAR
        FIELD GVQ_SEQCOT AS CHAR
        FIELD GVQ_OBSCOT AS CHAR
        FIELD GVQ_VALCOT AS CHAR.
        
DEFINE TEMP-TABLE tt-teste-tab-frete NO-UNDO
        FIELD GVA_FILIAL AS CHAR
        FIELD GVA_EMIVIN AS CHAR
        FIELD GVA_TABVIN AS CHAR
        FIELD GVA_CDEMIT AS CHAR
        FIELD GVA_NRTAB  AS CHAR
        FIELD GVA_DSTAB  AS CHAR
        FIELD GVA_DTATU  AS CHAR
        FIELD GVA_HRATU  AS CHAR
        FIELD GVA_VLMULT AS CHAR
        FIELD GVA_VLADIC AS CHAR.

DEFINE VARIABLE vdt-val-ini AS DATE NO-UNDO.
DEFINE VARIABLE vdt-val-fim AS DATE NO-UNDO.

DEF VAR vnrneg AS CHAR NO-UNDO.
DEF VAR vnrtab AS CHAR    NO-UNDO.
DEF VAR c-hoje AS CHAR    NO-UNDO.

DEFINE VARIABLE vc-cnpj   LIKE emitente.cgc      NO-UNDO.
DEFINE VARIABLE cRota     AS CHAR FORMAT "x(45)" NO-UNDO.

DEF BUFFER bf-emitente FOR emitente.

{esp\esint001rp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables  */
&Scoped-define EXTERNAL-TABLES es-gp-mix-vendedor
&Scoped-define FIRST-EXTERNAL-TABLE es-gp-mix-vendedor


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-gp-mix-vendedor.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-gp-mix-vendedor.cod-emitente 
&Scoped-define ENABLED-TABLES es-gp-mix-vendedor
&Scoped-define FIRST-ENABLED-TABLE es-gp-mix-vendedor
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS es-gp-mix-vendedor.cod-emitente 
&Scoped-define DISPLAYED-TABLES es-gp-mix-vendedor
&Scoped-define FIRST-DISPLAYED-TABLE es-gp-mix-vendedor
&Scoped-Define DISPLAYED-OBJECTS fi-desc-cliente 
                                                                           
/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-desc-cliente AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-gp-mix-vendedor.cod-emitente AT ROW 1.54 COL 17.57 COLON-ALIGNED WIDGET-ID 2
          LABEL "C¢d. Emitente":R20
          VIEW-AS FILL-IN 
          SIZE 7.57 BY .88
     fi-desc-cliente AT ROW 1.54 COL 25.29 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: es-gp-mix-vendedor
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 2.29
         WIDTH              = 65.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN es-gp-mix-vendedor.cod-emitente IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-desc-cliente IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME es-gp-mix-vendedor.cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-vendedor.cod-emitente V-table-Win
ON F5 OF es-gp-mix-vendedor.cod-emitente IN FRAME f-main /* C¢d. Emitente */
DO:
    DEFINE VARIABLE wh-pesquisa AS HANDLE NO-UNDO.

    {include/zoomvar.i &prog-zoom=adzoom/z02ad098.w
                       &campo=es-gp-mix-vendedor.cod-emitente
                       &campozoom=cod-emitente}
    
    WAIT-FOR CLOSE OF wh-pesquisa.
    
    APPLY "LEAVE" TO SELF. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-vendedor.cod-emitente V-table-Win
ON LEAVE OF es-gp-mix-vendedor.cod-emitente IN FRAME f-main /* C¢d. Emitente */
DO:
        
    FOR FIRST emitente FIELD(nome-emit)
        WHERE emitente.cod-emitente = INT(INPUT FRAME {&FRAME-NAME} es-gp-mix-vendedor.cod-emitente) 
          AND emitente.identific   <> 2 NO-LOCK:
    
        ASSIGN fi-desc-cliente:SCREEN-VALUE = emitente.nome-emit.
    
    END.
    IF NOT AVAIL emitente THEN
        ASSIGN fi-desc-cliente:SCREEN-VALUE = "".
        
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-vendedor.cod-emitente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-gp-mix-vendedor.cod-emitente IN FRAME f-main /* C¢d. Emitente */
DO:
  
    APPLY "F5" TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/
es-gp-mix-vendedor.cod-emitente:load-mouse-pointer("image/lupa.cur") in frame {&frame-name}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-gp-mix-vendedor"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-gp-mix-vendedor"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /* Ponha na pi-validate todas as validaá‰es */
    /* N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN pi-validate.
    IF RETURN-VALUE = "ADM-ERROR":U THEN
        RETURN "ADM-ERROR":U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    /* Destino + Canal de venda */
    
    DEFINE VARIABLE h-acomp AS HANDLE NO-UNDO.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    RUN pi-inicializar IN h-acomp ('Criando tabelas de preáo GEOSALES...').
 
    FIND FIRST es-gp-param-frete NO-LOCK NO-ERROR.

    IF CAN-FIND(FIRST es-gp-tb-preco
                WHERE es-gp-tb-preco.cod-emitente = es-gp-mix-vendedor.cod-emitente
                  AND es-gp-tb-preco.tipo         = "Cliente" NO-LOCK) THEN DO:

       FOR EACH es-gp-tb-preco
           WHERE es-gp-tb-preco.cod-emitente = es-gp-mix-vendedor.cod-emitente 
             AND es-gp-tb-preco.tipo         = "Cliente" NO-LOCK:

            RUN pi-acompanhar IN h-acomp (INPUT es-gp-tb-preco.nr-tabpre).
            
            IF NOT CAN-FIND(FIRST geo-org_venda_cliente
                            WHERE geo-org_venda_cliente.cd_org_venda   = es-gp-tb-preco.cod-estabel      
                              AND geo-org_venda_cliente.cd_cliente     = es-gp-mix-vendedor.cod-emitente
                              AND geo-org_venda_cliente.cd_vendedor    = es-gp-mix-vendedor.cod-rep      
                              AND geo-org_venda_cliente.cd_tab_preco   = INT(es-gp-tb-preco.nr-tabpre) NO-LOCK) THEN DO:
            
                CREATE geo-org_venda_cliente.
                ASSIGN geo-org_venda_cliente.cd_org_venda    = es-gp-tb-preco.cod-estabel                
                       geo-org_venda_cliente.cd_cliente      = es-gp-mix-vendedor.cod-emitente
                       geo-org_venda_cliente.cd_vendedor     = es-gp-mix-vendedor.cod-rep                     
                       geo-org_venda_cliente.cd_tab_preco    = INT(es-gp-tb-preco.nr-tabpre)            
                       geo-org_venda_cliente.cd_grupo_mpgto  = es-gp-mix-vendedor.cod-emitente.
        
            END.
       
            /* CONDICAO DE PAGAMENTO */
            FIND FIRST geo-grupo_mpgto_org 
                 WHERE geo-grupo_mpgto_org.cd_org_venda   = es-gp-tb-preco.cod-estabel
                   AND geo-grupo_mpgto_org.cd_grupo_mpgto = es-gp-mix-vendedor.cod-emitente NO-LOCK NO-ERROR.
            IF NOT AVAIL geo-grupo_mpgto_org THEN DO:
            
                CREATE geo-grupo_mpgto_org.
                ASSIGN geo-grupo_mpgto_org.cd_org_venda   = es-gp-tb-preco.cod-estabel                
                       geo-grupo_mpgto_org.cd_grupo_mpgto = es-gp-mix-vendedor.cod-emitente.
            END.
        END.
    END.
    
    ELSE DO:
    
       FIND emitente WHERE emitente.cod-emitente = es-gp-mix-vendedor.cod-emitente NO-LOCK NO-ERROR.
       IF AVAIL emitente THEN DO:

            /* gfe */
              ASSIGN c-origem  = ''
                     c-destino = ''.

              ASSIGN vc-cnpj = REPLACE(es-gp-param-frete.cgc,".","")
                     vc-cnpj = REPLACE(vc-cnpj,"/","")
                     vc-cnpj = REPLACE(vc-cnpj,"-","").

              EMPTY TEMP-TABLE tt-teste-negoc.
              EMPTY TEMP-TABLE tt-teste-rota.
              EMPTY TEMP-TABLE tt-teste-faixa.
              EMPTY TEMP-TABLE tt-teste-tarifa.
              EMPTY TEMP-TABLE tt-teste-comp-tarifa.
              EMPTY TEMP-TABLE tt-teste-cotacao.

              FIND FIRST bf-emitente WHERE
                      bf-emitente.cgc = vc-cnpj NO-LOCK NO-ERROR.

              /* Destroy o Handle da BO utilizada. */
              IF VALID-HANDLE(h-bodi582) THEN DO:
                  RUN destroy IN h-bodi582.
              END.

              /* Executando a BO da tab-generica de forma persistente. */
              IF  NOT VALID-HANDLE(h-bodi582) THEN
                  RUN dibo/bodi582.p PERSISTENT SET h-bodi582.

              /* Setando a BO para trabalhar com as Negocia†øes. */
              RUN setConstraintUtilizacao IN h-bodi582(INPUT "Negociacao":U).

              RUN openQueryStatic IN h-bodi582 (INPUT "MAIN":U) NO-ERROR.

              RUN getBatchRecordsNegoc IN h-bodi582 (INPUT "",    /* estab inic */
                                                     INPUT "zzz", /* estab final */
                                                     INPUT bf-emitente.cod-emitente,              /* emit inic */
                                                     INPUT bf-emitente.cod-emitente,              /* emit final */
                                                     INPUT 0,   /* nr da tabela inicial */
                                                     INPUT 9999999,   /* nr da tabela final   */
                                                     OUTPUT TABLE tt-teste-negoc).

              /* Destroy o Handle da BO utilizada. */
              IF VALID-HANDLE(h-bodi582) THEN DO:
                  RUN destroy IN h-bodi582.
              END.

              ASSIGN vnrneg = ''
                     vnrtab = ''.
              ASSIGN c-hoje = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99").
              
              FOR EACH tt-teste-negoc 
                 WHERE tt-teste-negoc.gv9_cdclfr  =  es-gp-param-frete.cdclfr   /* 13 transferencia */
                   AND tt-teste-negoc.GV9_DTVALI <= c-hoje  
                   AND tt-teste-negoc.GV9_DTVALF >= c-hoje  
                       BREAK BY tt-teste-negoc.GV9_DTVALI DESC 
                             BY tt-teste-negoc.GV9_NRNEG  DESC   .
              
                  IF STRING(tt-teste-negoc.GV9_DTVALF) < c-hoje  THEN NEXT.
              
                  ASSIGN vnrneg = tt-teste-negoc.GV9_NRNEG
                         vnrtab = tt-teste-negoc.GV9_NRTAB.
                  LEAVE.
              END.

/*               ASSIGN vnrneg = ''.                                      */
/*                                                                        */
/*               FOR EACH tt-teste-negoc WHERE                            */
/*                   tt-teste-negoc.gv9_cdclfr = es-gp-param-frete.cdclfr */
/*                   BREAK BY tt-teste-negoc.GV9_DTVALI DESC.             */
/*                                                                        */
/*                   ASSIGN vnrneg = tt-teste-negoc.GV9_NRNEG.            */
/*                   LEAVE.                                               */
/*               END.                                                     */

              /* Executando a BO da tab-generica de forma persistente. */
              IF  NOT VALID-HANDLE(h-bodi582) THEN
                  RUN dibo/bodi582.p PERSISTENT SET h-bodi582.

              /* Setando a BO para trabalhar com as Rotas. */
              RUN setConstraintUtilizacao IN h-bodi582(INPUT "Rota":U).

              RUN openQueryStatic IN h-bodi582 (INPUT "MAIN":U) NO-ERROR.

              RUN getBatchRecordsRota IN h-bodi582 (INPUT "",
                                                    INPUT "zzz",
                                                    INPUT bf-emitente.cod-emitente,
                                                    INPUT bf-emitente.cod-emitente,
                                                    INPUT vnrtab,
                                                    INPUT vnrtab,
                                                    INPUT vnrneg,
                                                    INPUT vnrneg,
                                                    OUTPUT TABLE tt-teste-rota).

              /* Destroy o Handle da BO utilizada. */
              IF VALID-HANDLE(h-bodi582) THEN DO:
                  RUN destroy IN h-bodi582.
              END.

            /* */
            
            FOR EACH es-gp-tb-preco
               WHERE es-gp-tb-preco.cod-canal-venda = emitente.cod-canal-venda
                 AND es-gp-tb-preco.tipo            = "Canal" NO-LOCK:
 
                RUN pi-acompanhar IN h-acomp (INPUT es-gp-tb-preco.nr-tabpre).
 
                /* gfe */
                FIND FIRST tt-teste-rota WHERE
                    int(tt-teste-rota.gv8_nrrota) = es-gp-tb-preco.nr-rota AND
                    tt-teste-rota.gv8_cdufds      = emitente.estado        NO-LOCK NO-ERROR.

                /* &MESSAGE Pendente teste - avail tt-teste-rota ? avaliar a BO que a TOTVS devolveu no GV8 (UF) !!! 

                    MESSAGE 'Pendente teste - avail tt-teste-rota ? avaliar a BO que a TOTVS devolveu no GV8 (UF) !!!' AVAIL tt-teste-rota 
                        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                    
                /*-------------- COMENTADO EM 07/06/2019 POR BCC ------------------
                    IF NOT AVAIL tt-teste-rota THEN DO:

                        MESSAGE "erro.. n∆o ser∆o criadas geo-org_venda_cliente e geo-grupo_mpgto_org, verifique!!!"
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    END.
                 -----------------------------------------------------------*/

                IF AVAIL tt-teste-rota THEN DO:
              
                /* tms
                FIND FIRST tf-cli-prod-rota 
                      WHERE tf-cli-prod-rota.cgc             = es-gp-param-frete.cgc                 
                        AND tf-cli-prod-rota.nr-tabela-frete = es-gp-param-frete.nr-tabela-frete                   
                        AND tf-cli-prod-rota.cd-produto      = es-gp-param-frete.cd-produto 
                        AND tf-cli-prod-rota.nr-rota         = es-gp-tb-preco.nr-rota NO-LOCK NO-ERROR . 

                 IF AVAIL tf-cli-prod-rota
                 AND tf-cli-prod-rota.cd-uf-destino = emitente.estado THEN DO:
                 */
                 
                    IF NOT CAN-FIND(FIRST geo-org_venda_cliente
                                    WHERE geo-org_venda_cliente.cd_org_venda   = es-gp-tb-preco.cod-estabel      
                                      AND geo-org_venda_cliente.cd_cliente     = es-gp-mix-vendedor.cod-emitente
                                      AND geo-org_venda_cliente.cd_vendedor    = es-gp-mix-vendedor.cod-rep      
                                      AND geo-org_venda_cliente.cd_tab_preco   = INT(es-gp-tb-preco.nr-tabpre) NO-LOCK) THEN DO:
                    
                        CREATE geo-org_venda_cliente.
                        ASSIGN geo-org_venda_cliente.cd_org_venda    = es-gp-tb-preco.cod-estabel                
                               geo-org_venda_cliente.cd_cliente      = es-gp-mix-vendedor.cod-emitente
                               geo-org_venda_cliente.cd_vendedor     = es-gp-mix-vendedor.cod-rep                     
                               geo-org_venda_cliente.cd_tab_preco    = INT(es-gp-tb-preco.nr-tabpre)            
                               geo-org_venda_cliente.cd_grupo_mpgto  = es-gp-mix-vendedor.cod-emitente.
                    END.
 
                 /* CONDICAO DE PAGAMENTO */
                    FIND FIRST geo-grupo_mpgto_org 
                         WHERE geo-grupo_mpgto_org.cd_org_venda   = es-gp-tb-preco.cod-estabel
                           AND geo-grupo_mpgto_org.cd_grupo_mpgto = es-gp-mix-vendedor.cod-emitente NO-LOCK NO-ERROR.
                    IF NOT AVAIL geo-grupo_mpgto_org THEN DO:
                    
                        CREATE geo-grupo_mpgto_org.
                        ASSIGN geo-grupo_mpgto_org.cd_org_venda   = es-gp-tb-preco.cod-estabel                
                               geo-grupo_mpgto_org.cd_grupo_mpgto = es-gp-mix-vendedor.cod-emitente.
                    END.
 
                END.
            
            END.
        END.
    END.

    IF AVAIL es-gp-mix-vendedor THEN DO:
        /* ------- Integraªío Salesforce --------- */
       FIND FIRST es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
                                         AND es-api-param.cd-tipo-integr = 7 /*---- Integraªío repres  ------*/ NO-ERROR.
       IF AVAIL es-api-param THEN DO:
        
          IF NOT CAN-FIND(FIRST sfa-export WHERE sfa-export.chave = string(es-gp-mix-vendedor.cod-rep)
                                             AND sfa-export.ind-situacao < 2) THEN DO: 
             CREATE sfa-export-repres.
             ASSIGN sfa-export-repres.cd-tipo-integr = es-api-param.cd-tipo-integr
                    sfa-export-repres.id-movto       = NEXT-VALUE(seq-export)
                    sfa-export-repres.cod-rep        = es-gp-mix-vendedor.cod-rep           
                    sfa-export-repres.data-movto     = NOW
                    sfa-export-repres.c-json         = ?.
            
             CREATE sfa-export.
             ASSIGN sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                    sfa-export.id-movto       = sfa-export-repres.id-movto
                    sfa-export.cd-tipo-integr = sfa-export-repres.cd-tipo-integr
                    sfa-export.chave          = string(sfa-export-repres.cod-rep)
                    sfa-export.cod-status     = 0      /* ---- sem status ----*/
                    sfa-export.data-fim       = ?
                    sfa-export.data-inicio    = ?
                    sfa-export.data-movto     = NOW
                    sfa-export.ind-situacao   = 1       /*---- Pendente -----*/.
             RUN pi-processa (INPUT 2, INPUT 7).          
          END.
       END.
    END.
    RUN pi-finalizar IN h-acomp.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch in THIS-PROCEDURE ('create-record').
    
    FOR FIRST es-gp-mix FIELD (cod-mix)
        WHERE ROWID(es-gp-mix) = v-row-parent NO-LOCK:
        
        ASSIGN es-gp-mix-vendedor.cod-rep = es-gp-mix.cod-rep.
        
        RELEASE es-gp-mix.
    END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro  2
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
    
    FOR FIRST es-gp-mix
        WHERE ROWID(es-gp-mix) = v-row-parent NO-LOCK:

        IF CAN-FIND(FIRST es-gp-mix-vendedor
                    WHERE es-gp-mix-vendedor.cod-rep      = es-gp-mix.cod-rep
                      AND es-gp-mix-vendedor.cod-emitente = INPUT FRAME {&FRAME-NAME} es-gp-mix-vendedor.cod-emitente NO-LOCK) THEN DO:

            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 1,
                               INPUT "Mix Vendedor").
            
            APPLY "ENTRY":U TO es-gp-mix-vendedor.cod-emitente IN FRAME {&FRAME-NAME}.
    
            RETURN "ADM-ERROR":U.
            
        END.
            
        IF NOT CAN-FIND(FIRST emitente
                        WHERE emitente.cod-emitente = INT(INPUT FRAME {&FRAME-NAME} es-gp-mix-vendedor.cod-emitente)
                          AND emitente.identific   <> 2 NO-LOCK) THEN DO:
                                         
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 785,
                               INPUT INPUT FRAME {&FRAME-NAME} es-gp-mix-vendedor.cod-emitente).
        
            APPLY "ENTRY":U TO es-gp-mix-vendedor.cod-emitente IN FRAME {&FRAME-NAME}.
        
            RETURN "ADM-ERROR":U.
        
        END.
    END.
    IF NOT AVAIL es-gp-mix THEN DO:

        RUN utp/ut-msgs.p (INPUT "show":U,
                           INPUT 15217,
                           INPUT "Mix Vendedor").
        
        APPLY "ENTRY":U TO es-gp-mix-vendedor.cod-emitente IN FRAME {&FRAME-NAME}.
    
        RETURN "ADM-ERROR":U.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-gp-mix-vendedor"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

