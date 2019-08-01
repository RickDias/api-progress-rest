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
{include/i-prgvrs.i ESGP0193V2 2.09.00.001 } /*** 010001 ***/

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-gp-mix-produto
&Scoped-define FIRST-EXTERNAL-TABLE es-gp-mix-produto


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-gp-mix-produto.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-gp-mix-produto.fm-cod-com ~
es-gp-mix-produto.it-codigo 
&Scoped-define ENABLED-TABLES es-gp-mix-produto
&Scoped-define FIRST-ENABLED-TABLE es-gp-mix-produto
&Scoped-Define ENABLED-OBJECTS rt-mold 
&Scoped-Define DISPLAYED-FIELDS es-gp-mix-produto.fm-cod-com ~
es-gp-mix-produto.it-codigo 
&Scoped-define DISPLAYED-TABLES es-gp-mix-produto
&Scoped-define FIRST-DISPLAYED-TABLE es-gp-mix-produto
&Scoped-Define DISPLAYED-OBJECTS fi-desc-familia fi-desc-item 

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
cod-rep|y|y|mgcam.es-gp-mix-produto.cod-rep
it-codigo||y|mgcam.es-gp-mix-produto.it-codigo
fm-cod-com||y|mgcam.es-gp-mix-produto.fm-cod-com
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod-rep",
     Keys-Supplied = "cod-rep,it-codigo,fm-cod-com"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-desc-familia AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 2.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-gp-mix-produto.fm-cod-com AT ROW 1.38 COL 10.43 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-desc-familia AT ROW 1.38 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     es-gp-mix-produto.it-codigo AT ROW 2.38 COL 10.43 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 2.38 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     rt-mold AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcam.es-gp-mix-produto
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
         HEIGHT             = 2.96
         WIDTH              = 63.43.
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

/* SETTINGS FOR FILL-IN fi-desc-familia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
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

&Scoped-define SELF-NAME es-gp-mix-produto.fm-cod-com
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-produto.fm-cod-com V-table-Win
ON F5 OF es-gp-mix-produto.fm-cod-com IN FRAME f-main /* Fam°lia */
DO:
    DEFINE VARIABLE wh-pesquisa AS HANDLE NO-UNDO.

    {include/zoomvar.i &prog-zoom=dizoom/z01di050.w
                       &campo=es-gp-mix-produto.fm-cod-com
                       &campozoom=fm-cod-com}
    
    WAIT-FOR CLOSE OF wh-pesquisa.
    
    APPLY "LEAVE" TO SELF. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-produto.fm-cod-com V-table-Win
ON LEAVE OF es-gp-mix-produto.fm-cod-com IN FRAME f-main /* Fam°lia */
DO:
    IF INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com = "*" THEN
        ASSIGN fi-desc-familia:SCREEN-VALUE = "Todas Fam°lias".
    ELSE DO:
        
        FOR FIRST fam-comerc FIELD(descricao)
            WHERE fam-comerc.fm-cod-com = INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com NO-LOCK:
        
            ASSIGN fi-desc-familia:SCREEN-VALUE = fam-comerc.descricao.
            
        END.
        IF NOT AVAIL fam-comerc THEN
            ASSIGN fi-desc-familia:SCREEN-VALUE = "".

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-produto.fm-cod-com V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-gp-mix-produto.fm-cod-com IN FRAME f-main /* Fam°lia */
DO:
  
    APPLY "F5" TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-produto.fm-cod-com V-table-Win
ON VALUE-CHANGED OF es-gp-mix-produto.fm-cod-com IN FRAME f-main /* Fam°lia */
DO:
  
    IF SELF:SCREEN-VALUE = "*" THEN
        ASSIGN es-gp-mix-produto.it-codigo:SENSITIVE    = FALSE
               es-gp-mix-produto.it-codigo:SCREEN-VALUE = "".
    ELSE
        ASSIGN es-gp-mix-produto.it-codigo:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-gp-mix-produto.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-produto.it-codigo V-table-Win
ON F5 OF es-gp-mix-produto.it-codigo IN FRAME f-main /* Item */
DO:
  
    DEFINE VARIABLE wh-pesquisa AS HANDLE NO-UNDO.

    {include/zoomvar.i &prog-zoom=inzoom/z02in172.w
                       &campo=es-gp-mix-produto.it-codigo
                       &campozoom=it-codigo}

    WAIT-FOR CLOSE OF wh-pesquisa.
    
    APPLY "LEAVE" TO SELF. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-produto.it-codigo V-table-Win
ON LEAVE OF es-gp-mix-produto.it-codigo IN FRAME f-main /* Item */
DO:
    IF INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.it-codigo = "*" THEN
        ASSIGN fi-desc-item:SCREEN-VALUE = "Todos Itens".
    ELSE DO:
        
        FOR FIRST item FIELD(desc-item)
            WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.it-codigo NO-LOCK:
        
            ASSIGN fi-desc-item:SCREEN-VALUE = item.desc-item.
            
        END.
        IF NOT AVAIL item THEN
            ASSIGN fi-desc-item:SCREEN-VALUE = "".

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-mix-produto.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-gp-mix-produto.it-codigo IN FRAME f-main /* Item */
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

es-gp-mix-produto.fm-cod-com:load-mouse-pointer("image/lupa.cur")      in frame {&frame-name}.
es-gp-mix-produto.it-codigo:load-mouse-pointer("image/lupa.cur")      in frame {&frame-name}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'cod-rep':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = es-gp-mix-produto
           &WHERE = "WHERE es-gp-mix-produto.cod-rep eq INTEGER(key-value)"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "es-gp-mix-produto"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-gp-mix-produto"}

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

    IF AVAIL es-gp-mix-produto THEN DO:
        /* ------- Integraá∆o Salesforce --------- */
        FIND FIRST es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
                                          AND es-api-param.cd-tipo-integr = 7 /*---- Integraá∆o repres  ------*/ NO-ERROR.
        IF AVAIL es-api-param THEN DO:
        
            IF NOT CAN-FIND(FIRST es-api-export WHERE es-api-export.chave = string(es-gp-mix-produto.cod-rep)
                                               AND es-api-export.ind-situacao < 2) THEN DO: 
                CREATE es-api-export-repres.
                ASSIGN es-api-export-repres.cd-tipo-integr = es-api-param.cd-tipo-integr
                       es-api-export-repres.id-movto       = NEXT-VALUE(seq-export)
                       es-api-export-repres.cod-rep        = es-gp-mix-produto.cod-rep           
                       es-api-export-repres.data-movto     = NOW
                       es-api-export-repres.c-json         = ?.
            
                CREATE es-api-export.
                ASSIGN es-api-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                       es-api-export.id-movto       = es-api-export-repres.id-movto
                       es-api-export.cd-tipo-integr = es-api-export-repres.cd-tipo-integr
                       es-api-export.chave          = string(es-api-export-repres.cod-rep)
                       es-api-export.cod-status     = 0      /* ---- sem status ----*/
                       es-api-export.data-fim       = ?
                       es-api-export.data-inicio    = ?
                       es-api-export.data-movto     = NOW
                       es-api-export.ind-situacao   = 1       /*---- Pendente -----*/.
                RUN pi-processa (INPUT 2, INPUT 7).          
            END.
        END.
    END.
    
    /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */


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
        
        ASSIGN es-gp-mix-produto.cod-rep = es-gp-mix.cod-rep.
        
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF AVAIL es-gp-mix-produto THEN DO:

        APPLY "LEAVE":U TO es-gp-mix-produto.fm-cod-com     IN FRAME {&FRAME-NAME}.
        APPLY "LEAVE":U TO es-gp-mix-produto.it-codigo      IN FRAME {&FRAME-NAME}.

    END.
    ELSE
        ASSIGN fi-desc-familia:SCREEN-VALUE    = ""
               fi-desc-item:SCREEN-VALUE       = "".


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

        IF CAN-FIND(FIRST es-gp-mix-produto
                    WHERE es-gp-mix-produto.cod-rep    = es-gp-mix.cod-rep
                      AND es-gp-mix-produto.fm-cod-com = INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com
                      AND es-gp-mix-produto.it-codigo  = INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.it-codigo NO-LOCK) THEN DO:

            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 1,
                               INPUT "Mix Produto").
            
            APPLY "ENTRY":U TO es-gp-mix-produto.fm-cod-com IN FRAME {&FRAME-NAME}.
    
            RETURN "ADM-ERROR":U.
            
        END.
        
        IF CAN-FIND(FIRST es-gp-mix-produto
                    WHERE es-gp-mix-produto.cod-rep    = es-gp-mix.cod-rep
                      AND es-gp-mix-produto.fm-cod-com = "*" NO-LOCK) THEN DO:

            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 17006,
                               INPUT "Mix Produto~~J† cadastrado todas as familias e itens para o representante informado.").
            
            APPLY "ENTRY":U TO es-gp-mix-produto.fm-cod-com IN FRAME {&FRAME-NAME}.
            
            RETURN "ADM-ERROR":U.  

        END.

        IF INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com <> "*" THEN DO:

            IF CAN-FIND(FIRST es-gp-mix-produto
                        WHERE es-gp-mix-produto.cod-rep    = es-gp-mix.cod-rep
                          AND es-gp-mix-produto.fm-cod-com = INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com 
                          AND es-gp-mix-produto.it-codigo  = "*" NO-LOCK) THEN DO:

                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 17006,
                                   INPUT "Mix Produto~~J† cadastrado todas os itens para a familia e representante informado.").
            
                APPLY "ENTRY":U TO es-gp-mix-produto.fm-cod-com IN FRAME {&FRAME-NAME}.
                
                RETURN "ADM-ERROR":U. 

            END.
            
            IF NOT CAN-FIND(FIRST fam-comerc
                            WHERE fam-comerc.fm-cod-com = INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com NO-LOCK) THEN DO:
                                 
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 7636,
                                   INPUT "").
            
                APPLY "ENTRY":U TO es-gp-mix-produto.fm-cod-com IN FRAME {&FRAME-NAME}.
            
                RETURN "ADM-ERROR":U.                                                                 
            
            END.
        END.

        IF INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.it-codigo <> "*" THEN DO:
            
            FIND ITEM
           WHERE ITEM.it-codigo = INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.it-codigo NO-LOCK NO-ERROR.
            IF NOT AVAIL ITEM THEN DO:
                
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 90,
                                   INPUT INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.it-codigo).
            
                APPLY "ENTRY":U TO es-gp-mix-produto.it-codigo IN FRAME {&FRAME-NAME}.
            
                RETURN "ADM-ERROR":U.
            
            END.
            ELSE DO:

                IF INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com <> "*" THEN DO:
                    
                    IF ITEM.fm-cod-com <> INPUT FRAME {&FRAME-NAME} es-gp-mix-produto.fm-cod-com THEN DO:

                        RUN utp/ut-msgs.p (INPUT "show":U,
                                           INPUT 17006,
                                           INPUT "Mix Produto~~Item n∆o pertence a fam°lia comercial").
                                
                        APPLY "ENTRY":U TO es-gp-mix-produto.fm-cod-com IN FRAME {&FRAME-NAME}.
                        
                        RETURN "ADM-ERROR":U.

                    END.
                END.
            END.
        END.
    END.
    IF NOT AVAIL es-gp-mix THEN DO:

        RUN utp/ut-msgs.p (INPUT "show":U,
                           INPUT 15217,
                           INPUT "Mix Produto").
        
        APPLY "ENTRY":U TO es-gp-mix-produto.fm-cod-com IN FRAME {&FRAME-NAME}.
    
        RETURN "ADM-ERROR":U.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod-rep" "es-gp-mix-produto" "cod-rep"}
  {src/adm/template/sndkycas.i "it-codigo" "es-gp-mix-produto" "it-codigo"}
  {src/adm/template/sndkycas.i "fm-cod-com" "es-gp-mix-produto" "fm-cod-com"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "es-gp-mix-produto"}

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

