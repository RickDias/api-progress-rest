/*----------------------------------------------------------------------------------------------/
 Programa..: esint020ai.p
 Objetivo..: Interface Integra‡Æo Callback Fornecedores B2E
 Data......: 27/05/2019
 Autor.....: Marcelo Brasil
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

/* ------- Importa‡Æo de Classes ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{utp/ut-glob.i} 
/*{include/i-rpexa.i}*/

/* ------- Defini‡Æo de Parƒmetros ----- */
DEF INPUT  PARAM r-table    AS ROWID      NO-UNDO.
DEF OUTPUT PARAM c-erro     AS CHARACTER  NO-UNDO.
DEF OUTPUT PARAM c-chave    AS CHARACTER  NO-UNDO.
DEF INPUT  PARAM pJsonInput AS JsonObject NO-UNDO.

/* ------- Defini‡Æo de Vari veis ------ */
DEF VAR cLongJson           AS LONGCHAR   NO-UNDO.
DEF VAR lRetJson            AS LOGICAL    NO-UNDO.
DEF VAR body                AS JsonObject NO-UNDO.
DEF VAR jsonOutput          AS JsonObject NO-UNDO.
DEF VAR arrJson             AS JsonArray  NO-UNDO.
DEF VAR oJsonObject         AS JsonObject NO-UNDO.
DEF VAR hBufferMain         AS HANDLE     NO-UNDO.
DEF VAR hBufferSec          AS HANDLE     NO-UNDO.
DEF VAR oJsonObjectMain     AS JsonObject NO-UNDO.
DEF VAR oJsonObjectSec      AS JsonObject NO-UNDO.
DEF VAR oJsonArrayMain      AS JsonArray  NO-UNDO.
DEF VAR oJsonArraySec       AS JsonArray  NO-UNDO.
DEF VAR iCountMain          AS INTEGER    NO-UNDO.
DEF VAR iCountSec           AS INTEGER    NO-UNDO.
DEF VAR cprop               AS CHARACTER  NO-UNDO.


DEF VAR i                   AS i   NO-UNDO.

/* ------- Defini‡Æo de Temp-Tables e Datasets ------ */

{esp\esint020ai.i}
/*{esp\esapi007.i}*/
{method/dbotterr.i}


DEF TEMP-TABLE tt-sf-pedido              NO-UNDO 
          LIKE geo-pedido.
DEF TEMP-TABLE tt-sf-item-pedido         NO-UNDO 
          LIKE geo-item_pedido.
DEF TEMP-TABLE tt-pedido-erro NO-UNDO
         FIELD cd-pedido-palm LIKE tt-sf-pedido.cd_pedido_palm
         FIELD cd-vendedor    LIKE tt-sf-pedido.cd_vendedor
         FIELD cod-msg        AS INT
         FIELD msg-erro       AS CHAR
         FIELD msg-padrao     AS LOG
         FIELD cd-cliente     AS INT.


/*------------------------------ Main Begin ----------------------------*/
ASSIGN c-erro = "".

FIND FIRST sfa-import NO-LOCK WHERE ROWID(sfa-import) = r-table NO-ERROR.
IF AVAIL sfa-import 
THEN DO:
    /* ------- Grava clob para longchar ----- */
    FIND FIRST api-import-for EXCLUSIVE-LOCK
            OF sfa-import 
         NO-ERROR.
    IF AVAIL api-import-for 
    THEN DO:

       COPY-LOB api-import-for.c-json TO cLongJson.

       MESSAGE "***** JSON B2E" STRING(api-import-for.c-json).

       /* ---- Lˆ propriedade Principal ---- */        
       oJsonArrayMain = pJsonInput:GetJsonObject("payload":U)
                                  :GetJsonArray("req":U).     
       CREATE tt-sf-pedido.
       blk: DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
           oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

           CREATE tt-integra.
           IF oJsonObjectMain:Has("CNPJ")                    THEN ASSIGN tt-integra.CNPJ                   = oJsonObjectMain:GetCharacter("CNPJ"                  ). 
           MESSAGE "***** CNPJ: " tt-integra.CNPJ. 

           IF oJsonObjectMain:Has("RazÆoSocial")             THEN ASSIGN tt-integra.RazaoSocial            = oJsonObjectMain:GetCharacter("RazaoSocial"           ). 
           MESSAGE "***** RazaoSocial: " tt-integra.RazaoSocial.  

           IF oJsonObjectMain:Has("NaturezaJuridica")        THEN ASSIGN tt-integra.NaturezaJuridica       = oJsonObjectMain:GetCharacter("NaturezaJuridica"      ). 
           MESSAGE "***** NaturezaJuridica: " tt-integra.NaturezaJuridica. 

           IF oJsonObjectMain:Has("CEP")                     THEN ASSIGN tt-integra.CEP                    = oJsonObjectMain:GetCharacter("CEP"                   ). 
           MESSAGE "***** CEP: " tt-integra.CEP. 

           IF oJsonObjectMain:Has("LOGRADOURO")              THEN ASSIGN tt-integra.LOGRADOURO             = oJsonObjectMain:GetCharacter("LOGRADOURO"            ). 
           MESSAGE "***** LOGRADOURO: " tt-integra.LOGRADOURO. 

           IF oJsonObjectMain:Has("COMPLEMENTO")             THEN ASSIGN tt-integra.COMPLEMENTO            = oJsonObjectMain:GetCharacter("COMPLEMENTO"           ). 
           MESSAGE "***** COMPLEMENTO: " tt-integra.COMPLEMENTO. 

           IF oJsonObjectMain:Has("CIDADE")                  THEN ASSIGN tt-integra.CIDADE                 = oJsonObjectMain:GetCharacter("CIDADE"                ). 
           MESSAGE "***** CIDADE: " tt-integra.CIDADE. 

           IF oJsonObjectMain:Has("BAIRRO")                  THEN ASSIGN tt-integra.BAIRRO                 = oJsonObjectMain:GetCharacter("BAIRRO"                ). 
           MESSAGE "***** BAIRRO: " tt-integra.BAIRRO. 

           IF oJsonObjectMain:Has("UF")                      THEN ASSIGN tt-integra.UF                     = oJsonObjectMain:GetCharacter("UF"                    ).
           MESSAGE "***** UF: " tt-integra.UF.

           IF oJsonObjectMain:Has("SintegraAtivo")           THEN ASSIGN tt-integra.SintegraAtivo          = oJsonObjectMain:GetCharacter("SintegraAtivo"         ).
           MESSAGE "***** SintegraAtivo: " tt-integra.SintegraAtivo.

           IF oJsonObjectMain:Has("CNPJAtivo")               THEN ASSIGN tt-integra.CNPJAtivo              = oJsonObjectMain:GetCharacter("CNPJAtivo"             ).
           MESSAGE "***** CNPJAtivo: " tt-integra.CNPJAtivo.

           IF oJsonObjectMain:Has("OptanteSimplesNacional")  THEN ASSIGN tt-integra.OptanteSimplesNacional = oJsonObjectMain:GetCharacter("OptanteSimplesNacional").
           MESSAGE "***** OptanteSimplesNacional: " tt-integra.OptanteSimplesNacional.

           IF oJsonObjectMain:Has("CPF")                     THEN ASSIGN tt-integra.CPF                    = oJsonObjectMain:GetCharacter("CPF"                   ).
           MESSAGE "***** CPF: " tt-integra.CPF.

           IF oJsonObjectMain:Has("NOME")                    THEN ASSIGN tt-integra.NOME                   = oJsonObjectMain:GetCharacter("NOME"                  ).
           MESSAGE "***** NOME: " tt-integra.NOME.

           IF oJsonObjectMain:Has("CPFAtivo")                THEN ASSIGN tt-integra.CPFAtivo               = oJsonObjectMain:GetCharacter("CPFAtivo"              ). 
           MESSAGE "***** CPFAtivo: " tt-integra.CPFAtivo. 

           IF oJsonObjectMain:Has("InscricaoEstadual")       THEN ASSIGN tt-integra.InscricaoEstadual      = oJsonObjectMain:GetCharacter("InscricaoEstadual"     ). 
           MESSAGE "***** InscricaoEstadual: " tt-integra.InscricaoEstadual. 

           IF oJsonObjectMain:Has("CNAE")                    THEN ASSIGN tt-integra.CNAE                   = oJsonObjectMain:GetCharacter("CNAE"                  ). 
           MESSAGE "***** CNAE: " tt-integra.CNAE. 

           IF oJsonObjectMain:Has("Mensagem")                THEN ASSIGN tt-integra.Mensagem               = oJsonObjectMain:GetCharacter("Mensagem"              ). 
           MESSAGE "***** Mensagem: " tt-integra.Mensagem. 

           IF oJsonObjectMain:Has("Parecer")                 THEN ASSIGN tt-integra.Parecer                = oJsonObjectMain:GetCharacter("Parecer"               ). 
           MESSAGE "***** Parecer: " tt-integra.Parecer. 

           IF oJsonObjectMain:Has("Motivo")                  THEN ASSIGN tt-integra.Motivo                 = oJsonObjectMain:GetCharacter("Motivo"                ). 
           MESSAGE "***** Motivo: " tt-integra.Motivo. 

           IF oJsonObjectMain:Has("CodigoPropostaCliente")   THEN ASSIGN tt-integra.CodigoPropostaCliente  = oJsonObjectMain:GetCharacter("CodigoPropostaCliente" ). 

           IF tt-integra.CodigoPropostaCliente = ""
           THEN ASSIGN tt-integra.CodigoPropostaCliente  = "???". 
           MESSAGE "***** CodigoPropostaCliente: " tt-integra.CodigoPropostaCliente. 
       
       END.

       MESSAGE "***** Buscando Fornecedor " STRING(tt-integra.CodigoPropostaCliente).  

       FIND FIRST es-fornecedor-ariba EXCLUSIVE-LOCK
            WHERE es-fornecedor-ariba.cod-proposta-b2e = tt-integra.CodigoPropostaCliente
            NO-ERROR.

       IF NOT AVAIL es-fornecedor-ariba
       THEN DO:
          ASSIGN
             c-erro = "NÆo foi encontrada proposta de n£mero: " + REPLACE(tt-integra.CodigoPropostaCliente,"???",""). 
          MESSAGE "***** Erro: " c-erro. 
       END.
       ELSE DO:
           /*
           IF tt-integra.sintegra = ""
           THEN DO:
                 es-fornecedor-ariba.sintegraativo = ?

                 c-erro = "Sem retorno do SINTEGRA". 
              MESSAGE "***** " c-erro.
              MESSAGE "***** callback " es-fornecedor-ariba.callback-b2e.
              RETURN.
           END.
           */
           ASSIGN 
              es-fornecedor-ariba.callback-b2e = YES
              es-fornecedor-ariba.mensagem     = tt-integra.mensagem             
              es-fornecedor-ariba.parecer      = tt-integra.parecer              
              es-fornecedor-ariba.motivo       = tt-integra.motivo.              
    
           IF YES //tt-integra.parecer MATCHES "*APROVADO*"
           AND c-erro            = ""
           THEN DO:
              MESSAGE "**** Iniciando cria‡Æo de fornecedor".
    /*****/
              ASSIGN
    /*
                 es-fornecedor-ariba.Number             =                                 
                 es-fornecedor-ariba.cod-emitente       =                                 
                 es-fornecedor-ariba.CNPJ               =                                 
                 es-fornecedor-ariba.CPF                =                                 
                 es-fornecedor-ariba.PIS-Number         =                                 
                 es-fornecedor-ariba.NIS-Number         =                                 
    */             
    
    /*           es-fornecedor-ariba.IE                 = tt-integra.InscricaoEstadual    ******/

    /*

                 es-fornecedor-ariba.State              = tt-integra.uf                   
                 es-fornecedor-ariba.Street             = tt-integra.logradouro

                 es-fornecedor-ariba.Complement         = tt-integra.complemento         

                 es-fornecedor-ariba.Municipality       = tt-integra.cidade               
                 es-fornecedor-ariba.District           = tt-integra.bairro               
                 es-fornecedor-ariba.Zip-Code           = tt-integra.cep                  
    
    */

                 es-fornecedor-ariba.Country            = "Brasil"                        
                 es-fornecedor-ariba.Pais               = "Brasil"                        
    
                 es-fornecedor-ariba.CNAE-principal     = tt-integra.cnae                 
                 
                 
    /*
                 es-fornecedor-ariba.Nome-Responsavel   =
                 es-fornecedor-ariba.Date-Birth         =
                 es-fornecedor-ariba.Codigo-Pais        =
                 es-fornecedor-ariba.Codigo-area        =
                 es-fornecedor-ariba.Numero-Telefone    =
                 es-fornecedor-ariba.E-mail             =
                 es-fornecedor-ariba.Banco              =
                 es-fornecedor-ariba.Agencia            =
                 es-fornecedor-ariba.Dig-Agencia        =
                 es-fornecedor-ariba.Conta-corrente     =
                 es-fornecedor-ariba.Dig-conta-corrente =
    */             
    
    /*             es-fornecedor-ariba.aprovado-b2e       =*/
                 
                 es-fornecedor-ariba.Simples-Nacional   = IF tt-integra.OptanteSimplesNacional = "S" THEN YES ELSE NO .

                 /********es-fornecedor-ariba.SintegraAtivo      = tt-integra.SintegraAtivo        ********/
    /*             es-fornecedor-ariba.SimplesAtivo       =*/
                 
                 .
    /***/         
              ASSIGN
                 es-fornecedor-ariba.callback-b2e       = YES.                             
    /***
              IF tt-integra.cpf > ""
              THEN ASSIGN
                 es-fornecedor-ariba.Corporate-Name = tt-integra.nome.
    ***/                 
    
              RUN esp/esint020aif.p ("",
                                     ROWID(es-fornecedor-ariba),
                                     OUTPUT c-erro).

           END.
       END.
       MESSAGE "**** Finalizando cria‡Æo de fornecedor".
    END.
    IF AVAIL api-import-for
    THEN RELEASE api-import-for.
    IF AVAIL es-fornecedor-ariba
    THEN RELEASE es-fornecedor-ariba.

END.


PROCEDURE piGravaBuffer:

    DEFINE INPUT PARAMETER  pTable     AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER pBuffer    AS HANDLE     NO-UNDO.
    
    DEFINE VARIABLE hBufferAux       AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hTempAux         AS HANDLE      NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE oJsonArray       AS JsonArray   NO-UNDO.
    DEFINE VARIABLE oJsonObject      AS JsonObject  NO-UNDO.

    CREATE BUFFER hBufferAux FOR TABLE pTable NO-ERROR.
    CREATE temp-table hTempAux.
    hTempAux:CREATE-LIKE(hBufferAux).
    hTempAux:TEMP-TABLE-PREPARE("dados").
    pBuffer = hTempAux:DEFAULT-BUFFER-HANDLE.

    pBuffer:BUFFER-CREATE().
    
END PROCEDURE.

PROCEDURE piGravaInfoJson:

    DEFINE INPUT PARAMETER pJsonObject AS JsonObject NO-UNDO.
    DEFINE INPUT PARAMETER pBuffer     AS HANDLE     NO-UNDO.

    DEFINE VARIABLE icount AS INTEGER NO-UNDO.
    DEFINE VARIABLE hField AS HANDLE  NO-UNDO.

    DO icount = 1 to pBuffer:NUM-FIELDS:

        ASSIGN hField = pBuffer:BUFFER-FIELD(icount).

        IF pJsonObject:Has(hField:NAME) THEN DO:

            CASE hField:DATA-TYPE:
                WHEN "Character":U THEN do:
                    ASSIGN hField:BUFFER-VALUE = pJsonObject:GetCharacter(hField:NAME).                         
                END.
                WHEN "Logical":U   THEN ASSIGN hField:BUFFER-VALUE = pJsonObject:GetLogical  (hField:NAME).
                WHEN "Integer":U   THEN ASSIGN hField:BUFFER-VALUE = pJsonObject:GetInteger  (hField:NAME).
                WHEN "Decimal":U   THEN ASSIGN hField:BUFFER-VALUE = pJsonObject:GetDecimal  (hField:NAME).
            END CASE.  
        END.          
    END.

END PROCEDURE.


