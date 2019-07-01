&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999RP 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.

define temp-table tt-param       no-undo
    field destino                as integer
    field arquivo                as char format "x(35)"
    field usuario                as char format "x(12)"
    field data-exec              as date
    field hora-exec              as integer
    field classifica             as integer
    field desc-classifica        as char format "x(40)"
    field modelo-rtf             as char format "x(35)"
    field l-habilitaRtf          as l
    
    field i-cd-sistema-fim       as integer
    field i-cd-sistema-ini       as integer
    field da-periodo-fim         as DATETIME
    field da-periodo-ini         as DATETIME
    field i-cd-tipo-integr-fim   as integer 
    field i-cd-tipo-integr-ini   as integer 
    field i-relat                as integer 
    field l-integrado            as l
    field l-nao-integrado        as l
    .

define temp-table tt-digita
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.
 
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp         as handle no-undo.    

form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
    tt-param.arquivo    no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PARÂMETROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÇÃO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÃO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usuário * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

find first param-global no-lock no-error.

{utp/ut-liter.i titulo_sistema * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.

DEF VAR excelAppl  AS COM-HANDLE NO-UNDO.
DEF VAR chquery    AS COM-HANDLE NO-UNDO.

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
   Type: Procedure Template
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
         HEIGHT             = 6.04
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

do on stop undo, leave:
    run utp/ut-acomp.p persistent set h-acomp.  
    
    {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}
    
    run pi-inicializar in h-acomp ("Gerando Planilha":U). 
    OUTPUT TO VALUE(tt-param.arquivo) PAGE-SIZE 0 CONVERT TARGET "iso8859-1".
    PUT
       "Tipo"           CHR(9)
       "Nome"           CHR(9)
       "Sistema"        CHR(9)
       "Nom"            CHR(9)
       "Chave"          CHR(9)
       "Status"         CHR(9)
       "Inicio"         CHR(9)
       "Fim"            CHR(9)
       "Data Movimento" CHR(9)
       "Situa‡ao"       CHR(9)
       SKIP.

    FOR EACH es-api-param NO-LOCK
       WHERE es-api-param.cd-sistema     >= i-cd-sistema-ini       
         AND es-api-param.cd-sistema     <= i-cd-sistema-fim       
         AND es-api-param.cd-tipo-integr >= i-cd-tipo-integr-ini   
         AND es-api-param.cd-tipo-integr <= i-cd-tipo-integr-fim,
       FIRST es-api-app NO-LOCK
          OF es-api-param:
       FOR EACH sfa-export NO-LOCK
             OF es-api-param
          WHERE sfa-export.data-movto      >= da-periodo-ini
            AND sfa-export.data-movto      <= da-periodo-fim
            AND sfa-export.ind-tipo-trans  =  i-relat:
           PUT
               sfa-export.cd-tipo-integr                                                 CHR(9)
               es-api-param.des-tipo-integr                                              CHR(9)
               es-api-app.cd-sistema                                                     CHR(9)
               es-api-app.des-sistema                                                    CHR(9)
               sfa-export.chave                                                          CHR(9)
               ENTRY(sfa-export.cod-status,"Integrado,Nao Integrado")                    CHR(9)
               sfa-export.data-inicio                                                    CHR(9)
               sfa-export.data-fim                                                       CHR(9)
               sfa-export.data-movto                                                     CHR(9)
               ENTRY(sfa-export.ind-situacao - 1,"Pendente,Em processamento,Processado") CHR(9)
               SKIP.
       END.
    END.

    OUTPUT CLOSE.
    run pi-finalizar in h-acomp.
end.

CREATE "Excel.Application" excelAppl.
excelAppl:SheetsInNewWorkbook = 1.
excelAppl:Workbooks:ADD.
excelAppl:Sheets(1):SELECT.
excelAppl:range("a1"):SELECT.
chquery = excelAppl:activesheet:QueryTables:ADD("TEXT;" + SEARCH(tt-param.arquivo),excelAppl:range("a1")).
chquery:REFRESH().
excelAppl:Cells:EntireColumn:Autofit.
excelAppl:VISIBLE = YES.
RELEASE OBJECT chquery.
RELEASE OBJECT excelAppl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


