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

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.


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
DEF NEW GLOBAL SHARED VAR v_cdn_empres_usuar AS c NO-UNDO.
find mgcad.empresa no-lock 
     where empresa.ep-codigo = v_cdn_empres_usuar
     no-error.
find first param-global no-lock 
     no-error.

{utp/ut-liter.i titulo_sistema * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.

DEF BUFFER bf-fornecedor-ariba FOR es-fornecedor-ariba.

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
         HEIGHT             = 10.96
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
    {include/i-rpout.i}
    view frame f-cabec.
    view frame f-rodape.    
    run utp/ut-acomp.p persistent set h-acomp.  
    
    run pi-inicializar in h-acomp (input "Enviando":U). 
    
    MESSAGE 1.
    for each es-fornecedor-ariba
       where es-fornecedor-ariba.enviado-b2e   = YES
         AND es-fornecedor-ariba.callback-b2e  = YES
         AND es-fornecedor-ariba.reenviado-b2e = NO
         AND es-fornecedor-ariba.sintegraativo = ?:
        run pi-acompanhar in h-acomp (es-fornecedor-ariba.cnpj + es-fornecedor-ariba.cpf).
        MESSAGE 2.
        CREATE bf-fornecedor-ariba.
        BUFFER-COPY es-fornecedor-ariba 
                 TO bf-fornecedor-ariba.
        MESSAGE 3.
        ASSIGN
           es-fornecedor-ariba.reenviado-b2e = YES

           bf-fornecedor-ariba.dt-consulta   = TODAY
           bf-fornecedor-ariba.enviado-b2e   = YES
           bf-fornecedor-ariba.callback-b2e  = NO
           bf-fornecedor-ariba.reenviado-b2e = NO 
           bf-fornecedor-ariba.sintegraativo = ?
           NO-ERROR.
        MESSAGE 4.

        {esp/esint021ae.i1 &Tabela=bf-fornecedor-ariba}

        MESSAGE 5.
    end.
    
end.
MESSAGE 6.

run pi-finalizar in h-acomp.
{include/i-rpclo.i}



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


