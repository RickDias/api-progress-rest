/*----------------------------------------------------------------------------------------------/
 Programa..: esint020be.p
 Objetivo..: Interface Chamada B2E Fornecedores PF Ariba
 Data......: 29/05/2019
 Autor.....: Marcelo Brasil
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

/* ------ Definiá∆o das classes de objetos ------ */
USING Progress.Json.OBJECTModel.*.
USING System.Text.RegularExpressions.*. 

{include/i-prgvrs.i ESINT020BE 1.00.00.000} 

/* ------- Definiá∆o de ParÉmetros ----- */
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.

/* ------- Definiá∆o de Vari†veis ----- */
DEFINE VARIABLE oJsonObjMain      AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonArrayMain    AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJsonObjIni       AS jsonObject        NO-UNDO.
DEFINE VARIABLE ojsonArrayIni     AS JsonArray         NO-UNDO.
DEFINE VARIABLE ojsonObjAux       AS JsonObject        NO-UNDO.
DEFINE VARIABLE ojsonArrayAux     AS JsonArray         NO-UNDO.
                                                      
DEFINE VARIABLE h-temp            AS HANDLE            NO-UNDO.
DEFINE VARIABLE h-esint002        AS HANDLE            NO-UNDO.
DEFINE VARIABLE c-json            AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE lEnviou           AS LOGICAL           NO-UNDO.
DEFINE VARIABLE c-arq-json        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE lresp             AS LOGICAL           NO-UNDO.

DEF         VAR ojson             AS jsonobject        NO-UNDO.
DEF         VAR ojsonarraysec     AS jsonarray         NO-UNDO.
DEF         VAR oJsonObjectSec    AS jsonobject        NO-UNDO.
DEF         VAR iCountSec         AS i                 NO-UNDO.
DEF         VAR myParser          AS ObjectModelParser NO-UNDO.

DEF         VAR cSucesso          AS l                 NO-UNDO.


DEF BUFFER pais FOR mgcad.pais.


/* ------- Definiá∆o de Temp-Tables ------ */
{esp/esint020ae.i}

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

FUNCTION fncValidaMail RETURN CHARACTER
    (INPUT pmail AS CHARACTER ) FORWARD.

FUNCTION fncFormataTelefone RETURN CHARACTER 
    (INPUT pFone AS CHARACTER) FORWARD.


/*------------------------------ Main Begin ----------------------------*/

ASSIGN c-erro = "".

/* ---- Chama o programa persistent ----- */
RUN esp/esint002.p PERSISTENT SET h-esint002 NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
    RETURN "NOK".
END.

/* --------- Cria Objeto ------------------*/
RUN piGeraObjJson IN h-esint002 (OUTPUT oJsonObjMain) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
    RETURN "NOK".
END.
FIND FIRST sfa-export NO-LOCK 
     WHERE ROWID(sfa-export) = r-table 
     NO-ERROR.
IF AVAIL sfa-export 
THEN DO:
    FIND FIRST es-api-param NO-LOCK 
         WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
           AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr 
         NO-ERROR. 

    FIND FIRST api-export-b2e-pf OF sfa-export NO-ERROR.
    IF AVAIL api-export-b2e-pf THEN 
    DO:
        RUN piGravaTTFornecedor (OUTPUT c-json,
                                 OUTPUT c-erro).
        
        ASSIGN api-export-b2e-pf.c-json = c-Json.
        /* ------------ Envia Objeto Json --------- */
        RUN piPostJson IN h-esint002 (INPUT c-Json,
                                      INPUT rowid(es-api-param),
                                      OUTPUT lResp,
                                      OUTPUT TABLE RowErrors).

        MESSAGE "**** esint020be " RETURN-VALUE 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.


        ASSIGN api-export-b2e-pf.text-retorno = RETURN-VALUE
               sfa-export.text-retorno        = RETURN-VALUE.
        
        //IF RETURN-VALUE > "" THEN 
        //DO:
        //    myParser = NEW ObjectModelParser().
        //    oJson = CAST(myParser:Parse(RETURN-VALUE), JsonObject).
        //    IF oJson:Has("Sucesso") 
        //    THEN ASSIGN cSucesso = oJson:GetLogical("Sucesso").
        //    IF oJson:Has("Mensagens") 
        //    THEN 
        //    DO:  
        //        ASSIGN  c-erro = c-erro + oJsonObjectSec:GetCharacter("Descricao").
        //    END.      
        //END.
        
        IF TEMP-TABLE rowErrors:HAS-RECORDS 
        THEN DO:
            FOR EACH rowErrors:
                ASSIGN 
                   c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                DELETE OBJECT h-esint002.
            END.
        END.     
    END.
    ELSE 
        ASSIGN c-erro = c-erro + "Registro tabela do fornecedor n∆o localizada".
    IF c-erro > "" THEN RETURN "NOK".

    FIND FIRST es-fornecedor-ariba EXCLUSIVE-LOCK
         WHERE es-fornecedor-ariba.chave = STRING(api-export-b2e-pf.id-movto)
    NO-ERROR.
    ASSIGN es-fornecedor-ariba.enviado-b2e = YES.
    FIND CURRENT es-fornecedor-ariba NO-LOCK NO-ERROR.
END.

IF VALID-HANDLE(h-esint002) 
THEN DELETE OBJECT h-esint002.

/* -------------------------------------------------------------- Procedures ------------------------------------------------*/
PROCEDURE piGravaTTFornecedor:
    
    DEFINE OUTPUT PARAMETER pTemp   AS LONGCHAR    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro   AS CHARACTER   NO-UNDO.
    DEF    VAR      cCodArea        AS c           NO-UNDO.
    DEF    VAR      cTelefone       AS c           NO-UNDO.
    DEF    VAR      cDataNascimento AS c           NO-UNDO.       
                                                   
    DEFINE VARIABLE i               AS INTEGER     NO-UNDO. 


    FIND FIRST es-ariba-b2e-param NO-LOCK
         NO-ERROR.

    IF NOT AVAIL es-ariba-b2e-param
    THEN DO:
        pErro = "ParÉmetros de integraá∆o Ariba/B2E n∆o cadastrados".
        RETURN "NOK".
    END.

    FIND FIRST es-fornecedor-ariba  
         WHERE es-fornecedor-ariba.chave = STRING(api-export-b2e-pf.id-movto) 
         NO-ERROR.
    IF NOT AVAIL es-fornecedor-ariba 
    THEN DO:
        pErro = "Registro Ariba n∆o localizado com a chave: " + STRING(api-export-b2e-pf.id-movto).
        RETURN "NOK".
    END.
    
    ASSIGN
       cCodArea        = REPLACE(es-fornecedor-ariba.codigo-area,")","")
       cCodArea        = REPLACE(cCodArea,"(","")
       cTelefone       = REPLACE(es-fornecedor-ariba.numero-telefone,")","")
       cTelefone       = REPLACE(cTelefone,"(","")
       cTelefone       = REPLACE(cTelefone,"-","")
       .
    IF es-fornecedor-ariba.date-birth <> ?
    THEN ASSIGN
       cDataNascimento = STRING( YEAR(es-fornecedor-ariba.date-birth))
                       + "-" 
                       + STRING(MONTH(es-fornecedor-ariba.date-birth))
                       + "-" 
                       + STRING(  DAY(es-fornecedor-ariba.date-birth))
                       // + 'T00:00:00.000Z'
                       .
    ELSE ASSIGN
       cDataNascimento = 'null'.
/*                 "DataNascimento:" ' + cDataNascimento + ',
*/
    /* pTemp = '~{                                                                            */
    /*          "CodigoPropostaCliente":"' + STRING (api-export-b2e-pf.id-movto) + '",        */
    /*           "CodigoInstituicao":"' + es-ariba-b2e-param.insitituicao-b2b-pf + '",        */
    /*          "DadosProponente":~{                                                          */
    /*             "Nome":"' + es-fornecedor-ariba.corporate-name + '",                       */
    /*             "CPF":"' + es-fornecedor-ariba.cpf + '",                                   */
    /*             "DataNascimento": "' + cDataNascimento + '",                               */
    /*             "Enderecos":[                                                              */
    /*                ~{                                                                      */
    /*                   "UF":"' + es-fornecedor-ariba.state + '",                            */
    /*                   "Tipo": "COMERCIAL"                                                  */
    /*                ~}                                                                      */
    /*             ]                                                                          */
    /*          ~},                                                                           */
    /*          "InformacoesAdicionais": [                                                    */
    /*            ~{                                                                          */
    /*              "Grupo": null,                                                            */
    /*              "Nome": "Mercado_Interno",                                                */
    /*              "Valor": ""                                                               */
    /*            ~}                                                                          */
    /*          ],                                                                            */
    /*          "TipoFornecedor":"Mercado_Interno"                                            */
    /*        ~}'.                                                                            */


    pTemp = '~{                                                                      
                "CodigoPropostaCliente":"' + STRING (api-export-b2e-pf.id-movto) + '",  
                 "CodigoInstituicao":"' + es-ariba-b2e-param.insitituicao-b2b-pf + '",  
                "DadosProponente":~{                                                    
                   "Nome":"' + es-fornecedor-ariba.corporate-name + '",                 
                   "CPF":"' + es-fornecedor-ariba.cpf + '",                             
                   "DataNascimento": "' + cDataNascimento + '",                         
                   "Enderecos":[                                                        
                      ~{                                                                
                         "UF":"' + es-fornecedor-ariba.state + '",                      
                         "Tipo": "COMERCIAL"                                            
                      ~}                                                                
                   ],
                   "InformacoesAdicionais": [
                      ~{
                          "Grupo": "",
                          "Nome": "inscricao_estadual",
                          "Valor": "' + es-fornecedor-ariba.ie + '"
                      ~}
                   ]
                   
                ~},                                                                     
                "InformacoesAdicionais": [                                              
                  ~{                                                                    
                    "Grupo": "",                                                      
                    "Nome": "Reenvio",                                          
                    "Valor": "N"                                                         
                  ~}                                                                    
                 ]                                  
              ~}'.



/*
   MESSAGE STRING(pTemp)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   CLIPBOARD:VALUE = STRING(pTemp).
*/
END PROCEDURE.

                                                   



