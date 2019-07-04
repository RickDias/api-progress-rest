&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : consultaFornecedor.p
    Purpose     : Programa Consulta Forncedor TOTVS X ARIBA

    Syntax      :

    Description :

    Author(s)   : TOTVS
    Created     : 04/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE consulta-fornecedor NO-UNDO SERIALIZE-NAME "Consulta_Fornecedor"
    FIELD CreationDateTime                    AS CHAR
    FIELD InboundServiceName                  AS CHAR.


DEF TEMP-TABLE cadastro-fornecedor NO-UNDO SERIALIZE-NAME "Cadastro_Fornecedor"
    FIELD Corporate-Name                      AS CHAR
    FIELD Trading-name                        AS CHAR
    FIELD Number                              AS CHAR
    FIELD CNPJ                                AS CHAR
    FIELD CPF                                 AS CHAR
    FIELD PIS-Number                          AS CHAR
    FIELD NIS-Number                          AS CHAR
    FIELD IE                                  AS CHAR
    FIELD State                               AS CHAR
    FIELD Street                              AS CHAR
    FIELD Complement                          AS CHAR
    FIELD District                            AS CHAR
    FIELD Zip-Code                            AS CHAR
    FIELD Country                             AS CHAR
    FIELD Pais                                AS CHAR
    FIELD CNAE-principal                      AS CHAR
    FIELD E-mail                              AS CHAR
    FIELD Simples-Nacional                    AS CHAR
    FIELD Municipality                        AS CHAR
    FIELD Nome-Responsavel                    AS CHAR
    FIELD Date-Birth                          AS CHAR
    FIELD Codigo-Pais                         AS CHAR
    FIELD Codigo-area                         AS CHAR
    FIELD Numero-Telefone                     AS CHAR
    FIELD Banco                               AS CHAR
    FIELD Agencia                             AS CHAR
    FIELD Dig-Agencia                         AS CHAR
    FIELD Conta-corrente                      AS CHAR
    FIELD Dig-conta-corrente                  AS CHAR

    FIELD DeletedIndicator                    AS CHAR
    FIELD BlockedIndicator                    AS CHAR
    FIELD BuildingID                          AS CHAR
    FIELD POBoxDeviatingCityName              AS CHAR.

DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEFINE TEMP-TABLE ttError      SERIALIZE-NAME "Retorno"
    FIELD SessionId         AS CHARACTER
    FIELD referencia        AS CHARACTER
    FIELD codigo            AS CHARACTER
    FIELD descricao         AS CHARACTER
    INDEX idx01 referencia codigo.

DEF NEW GLOBAL SHARED VAR c-seg-usuario        AS c FORMAT "x(12)" NO-UNDO.
DEF                   VAR l-retorno-fornecedor AS l                NO-UNDO.

{esp\esint001rp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.29
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN pi-00-consulta-fornecedor.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-00-consulta-fornecedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-00-consulta-fornecedor Procedure 
PROCEDURE pi-00-consulta-fornecedor :

    FIND FIRST es-api-param NO-LOCK                               
         WHERE es-api-param.ind-tipo-trans = 2  /*---- Saida ----*/
           AND es-api-param.cd-tipo-integr = 25 /*---- Integra‡Æo B2E pj ------*/ 
      NO-ERROR.

    IF AVAIL es-api-param THEN 
    DO:

        CREATE api-import-for.
        ASSIGN api-import-for.cd-tipo-integr     = es-api-param.cd-tipo-integr
               api-import-for.id-movto           = NEXT-VALUE(seq-export)
               api-import-for.data-movto         = TODAY
               api-import-for.c-json             = ?.
        
        CREATE sfa-export.
        ASSIGN sfa-export.ind-tipo-trans          = es-api-param.ind-tipo-trans
               sfa-export.id-movto                = api-import-for.id-movto
               sfa-export.cd-tipo-integr          = api-import-for.cd-tipo-integr
               sfa-export.chave                   = STRING(api-import-for.id-movto)
               sfa-export.cod-status              = 0      /* ---- sem status ----*/
               sfa-export.data-fim                = ?
               sfa-export.data-inicio             = ?
               sfa-export.data-movto              = NOW
               sfa-export.ind-situacao            = 1      /*---- Pendente -----*/.
        
        
        /*--CHAMA ROTINA DE PROCESSAMENTO CADASTRADO NO ESIN004 --*/
        RUN pi-processa (es-api-param.ind-tipo-trans,
                         es-api-param.cd-tipo-integr).  
        
    END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

