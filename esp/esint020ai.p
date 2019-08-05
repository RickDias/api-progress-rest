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


 
/*------------------------------ Main Begin ----------------------------*/
ASSIGN c-erro = "".

FIND FIRST sfa-import NO-LOCK WHERE ROWID(sfa-import) = r-table NO-ERROR.
IF AVAIL sfa-import 
THEN DO:
    /* ------- Grava clob para longchar ----- */
    FIND FIRST api-import-for EXCLUSIVE-LOCK OF sfa-import NO-ERROR.
    IF AVAIL api-import-for 
    THEN DO:

       COPY-LOB api-import-for.c-json TO cLongJson.

       MESSAGE "***** JSON B2E" STRING(api-import-for.c-json).

       /* ---- Lˆ propriedade Principal ---- */        
       oJsonArrayMain = pJsonInput:GetJsonObject("payload":U)
                                  :GetJsonArray("req":U).     
       //CREATE tt-sf-pedido.
       blk: 
       DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
           oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

           CREATE ttRetfornecedores.
           IF oJsonObjectMain:Has("CodigoPropostaCliente")        THEN ASSIGN ttRetfornecedores.CodigoPropostaCliente        = oJsonObjectMain:GetCharacter("CodigoPropostaCliente")        NO-ERROR.
           IF oJsonObjectMain:Has("CNPJ")                         THEN ASSIGN ttRetfornecedores.CNPJ                         = oJsonObjectMain:GetCharacter("CNPJ")                         NO-ERROR.
           IF oJsonObjectMain:Has("RazaoSocial")                  THEN ASSIGN ttRetfornecedores.RazaoSocial                  = oJsonObjectMain:GetCharacter("RazaoSocial")                  NO-ERROR.
           IF oJsonObjectMain:Has("NaturezaJuridica")             THEN ASSIGN ttRetfornecedores.NaturezaJuridica             = oJsonObjectMain:GetCharacter("NaturezaJuridica ")            NO-ERROR.
           IF oJsonObjectMain:Has("CNPJAtivo")                    THEN ASSIGN ttRetfornecedores.CNPJAtivo                    = oJsonObjectMain:GetCharacter("CNPJAtivo")                    NO-ERROR.
           //IF oJsonObjectMain:Has("CpfAtivo")                     THEN ASSIGN ttRetfornecedores.CpfAtivo                     = oJsonObjectMain:GetCharacter("CpfAtivo")                     NO-ERROR.
           IF oJsonObjectMain:Has("OptanteSimplesNacional")       THEN ASSIGN ttRetfornecedores.OptanteSimplesNacional       = oJsonObjectMain:GetCharacter("OptanteSimplesNacional")       NO-ERROR.
           IF oJsonObjectMain:Has("InscricaoEstadual")            THEN ASSIGN ttRetfornecedores.InscricaoEstadual            = oJsonObjectMain:GetCharacter("InscricaoEstadual")            NO-ERROR.
           IF oJsonObjectMain:Has("Cnae")                         THEN ASSIGN ttRetfornecedores.Cnae                         = oJsonObjectMain:GetCharacter("Cnae")                         NO-ERROR.
           IF oJsonObjectMain:Has("Cep")                          THEN ASSIGN ttRetfornecedores.CEP                          = oJsonObjectMain:GetCharacter("Cep")                          NO-ERROR.
           IF oJsonObjectMain:Has("Logradouro")                   THEN ASSIGN ttRetfornecedores.Logradouro                   = oJsonObjectMain:GetCharacter("Logradouro")                   NO-ERROR.
           IF oJsonObjectMain:Has("Complemento")                  THEN ASSIGN ttRetfornecedores.Complemento                  = oJsonObjectMain:GetCharacter("Complemento")                  NO-ERROR.
           IF oJsonObjectMain:Has("Cidade")                       THEN ASSIGN ttRetfornecedores.Cidade                       = oJsonObjectMain:GetCharacter("Cidade")                       NO-ERROR.
           IF oJsonObjectMain:Has("Bairro")                       THEN ASSIGN ttRetFornecedores.Bairro                       = oJsonObjectMain:GetCharacter("Bairro")                       NO-ERROR.
           IF oJsonObjectMain:Has("Uf")                           THEN ASSIGN ttRetFornecedores.UF                           = oJsonObjectMain:GetCharacter("Uf")                           NO-ERROR.
           IF oJsonObjectMain:Has("SintegraAtivo")                THEN ASSIGN ttRetFornecedores.SintegraAtivo                = oJsonObjectMain:GetCharacter("SintegraAtivo")                NO-ERROR.
           IF oJsonObjectMain:Has("Mensagem")                     THEN ASSIGN ttRetFornecedores.Mensagem                     = oJsonObjectMain:GetCharacter("Mensagem")                     NO-ERROR.  
           IF oJsonObjectMain:Has("Parecer")                      THEN ASSIGN ttRetFornecedores.Parecer                      = oJsonObjectMain:GetCharacter("Parecer")                      NO-ERROR.  
           IF oJsonObjectMain:Has("Motivo")                       THEN ASSIGN ttRetFornecedores.Motivo                       = oJsonObjectMain:GetCharacter("Motivo")                       NO-ERROR.  
           IF oJsonObjectMain:Has("ValidacaoSintegra")            THEN ASSIGN ttRetFornecedores.ValidacaoSintegra            = oJsonObjectMain:GetCharacter("ValidacaoSintegra")            NO-ERROR.
           IF oJsonObjectMain:Has("CepIsncricaoEstadual")         THEN ASSIGN ttRetFornecedores.CepIsncricaoEstadual         = oJsonObjectMain:GetCharacter("CepIsncricaoEstadual")         NO-ERROR.
           IF oJsonObjectMain:Has("LogradouroIsncricaoEstadual")  THEN ASSIGN ttRetFornecedores.LogradouroIsncricaoEstadual  = oJsonObjectMain:GetCharacter("LogradouroIsncricaoEstadual")  NO-ERROR.
           IF oJsonObjectMain:Has("ComplementoIsncricaoEstadual") THEN ASSIGN ttRetFornecedores.ComplementoIsncricaoEstadual = oJsonObjectMain:GetCharacter("ComplementoIsncricaoEstadual") NO-ERROR.
           IF oJsonObjectMain:Has("CidadeIsncricaoEstadual")      THEN ASSIGN ttRetFornecedores.CidadeIsncricaoEstadual      = oJsonObjectMain:GetCharacter("CidadeIsncricaoEstadual")      NO-ERROR.
           IF oJsonObjectMain:Has("BairroIsncricaoEstadual")      THEN ASSIGN ttRetFornecedores.BairroIsncricaoEstadual      = oJsonObjectMain:GetCharacter("BairroIsncricaoEstadual")      NO-ERROR.
           IF oJsonObjectMain:Has("UfIsncricaoEstadual")          THEN ASSIGN ttRetFornecedores.UfIsncricaoEstadual          = oJsonObjectMain:GetCharacter("UfIsncricaoEstadual")          NO-ERROR.
           
       
       END.

       MESSAGE "***** Buscando Fornecedor " STRING(ttRetfornecedores.CodigoPropostaCliente).  

       FIND FIRST es-fornecedor-ariba EXCLUSIVE-LOCK
            WHERE es-fornecedor-ariba.cod-proposta-b2e = ttRetfornecedores.CodigoPropostaCliente
       NO-ERROR.

       IF NOT AVAIL es-fornecedor-ariba THEN
       DO:
          ASSIGN c-erro = "NÆo foi encontrada proposta de n£mero: " + REPLACE(ttRetfornecedores.CodigoPropostaCliente,"???",""). 
          MESSAGE "***** Erro: " c-erro. 
       END.
       ELSE 
       DO:

           MESSAGE "ttRetfornecedores.mensagem            " ttRetfornecedores.mensagem            skip
                   "ttRetfornecedores.parecer             " ttRetfornecedores.parecer             skip
                   "ttRetfornecedores.motivo              " ttRetfornecedores.motivo              skip
                   "ttRetfornecedores.cnae                " ttRetfornecedores.cnae                skip
                   "ttRetfornecedores.uf                  " ttRetfornecedores.uf                  skip
                   "ttRetfornecedores.logradouro          " ttRetfornecedores.logradouro          skip
                   "ttRetfornecedores.complemento         " ttRetfornecedores.complemento         skip
                   "ttRetfornecedores.cidade              " ttRetfornecedores.cidade              skip
                   "ttRetfornecedores.bairro              " ttRetfornecedores.bairro              skip
                   "ttRetfornecedores.cep                 " ttRetfornecedores.cep                 skip
                   "ttRetfornecedores.InscricaoEstadual   " ttRetfornecedores.InscricaoEstadual   skip.


           ASSIGN es-fornecedor-ariba.callback-b2e      = YES                                                                
                  es-fornecedor-ariba.mensagem          = ttRetfornecedores.mensagem                                         
                  es-fornecedor-ariba.parecer           = ttRetfornecedores.parecer                                          
                  es-fornecedor-ariba.motivo            = ttRetfornecedores.motivo                                           
                  es-fornecedor-ariba.Simples-Nacional  = IF ttRetfornecedores.OptanteSimplesNacional = "S" THEN YES ELSE NO 
                  es-fornecedor-ariba.CNPJAtivo         = IF ttRetFornecedores.CNPJAtivo = "ATIVA" THEN YES ELSE NO   
                  //es-fornecedor-ariba.CpfAtivo          = IF ttRetFornecedores.CpfAtivo = "S" THEN YES ELSE NO   
                  es-fornecedor-ariba.SintegraAtivo     = IF ttRetFornecedores.SintegraAtivo = "S" THEN YES ELSE NO.


           /*-- quando os campos retornar em branco da B2e, manter o que foi informado no ARIBA --*/
           IF ttRetfornecedores.logradouro <> "" THEN
               ASSIGN es-fornecedor-ariba.Street         = ttRetfornecedores.logradouro.

           IF ttRetfornecedores.complemento <> "" THEN   
               ASSIGN  es-fornecedor-ariba.Complement    = ttRetfornecedores.complemento.

           IF ttRetfornecedores.cidade <> "" THEN        
               ASSIGN es-fornecedor-ariba.Municipality   = ttRetfornecedores.cidade.

           IF ttRetfornecedores.bairro <> "" THEN        
               ASSIGN es-fornecedor-ariba.District       = ttRetfornecedores.bairro.

           IF ttRetfornecedores.InscricaoEstadual <> "" THEN
               ASSIGN es-fornecedor-ariba.ie             = ttRetfornecedores.InscricaoEstadual.
           
           IF ttRetfornecedores.cnae <> "" THEN
               ASSIGN es-fornecedor-ariba.CNAE-principal    = ttRetfornecedores.cnae.

           IF ttRetfornecedores.uf <> "" THEN                                         
               ASSIGN es-fornecedor-ariba.State             = ttRetfornecedores.uf.   
                                                                                      
           IF ttRetfornecedores.cep = ""  THEN                                        
               ASSIGN es-fornecedor-ariba.Zip-Code          = ttRetfornecedores.cep. 


           
           
           
           
                      
                          
           

           
           

           
           
    
           RUN esp/esint020aif.p ("",
                                  ROWID(es-fornecedor-ariba),
                                  OUTPUT c-erro).

           
       END.
       MESSAGE "**** Finalizando cria‡Æo de fornecedor".
    END.
    IF AVAIL api-import-for THEN RELEASE api-import-for.
    IF AVAIL es-fornecedor-ariba THEN RELEASE es-fornecedor-ariba.

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


