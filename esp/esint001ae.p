/*----------------------------------------------------------------------------------------------/
 Programa..: esint001ae.p
 Objetivo..: Interface Exporta‡Æo Clientes SFA
 Data......: 26/02/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Defini‡Æo das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.
USING System.Text.RegularExpressions.*. 

{include/i-prgvrs.i esint001AE 1.00.00.000} 

/* ------- Defini‡Æo de Parƒmetros ----- */
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.

/* ------- Defini‡Æo de Vari veis ----- */
DEFINE VARIABLE oJsonObjMain      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain    AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObjIni       AS jsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayIni     AS JsonArray  NO-UNDO.
DEFINE VARIABLE ojsonObjAux       AS JsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayAux     AS JsonArray  NO-UNDO.

DEFINE VARIABLE h-temp              AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-esint002          AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-json              AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lEnviou             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-arq-json          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lresp               AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-retorno           AS LONGCHAR   NO-UNDO.

DEF BUFFER pais FOR mgcad.pais.


/* ------- Defini‡Æo de Temp-Tables ------ */
{esp/esint001a.i}

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

FIND FIRST sfa-export NO-LOCK WHERE ROWID(sfa-export) = r-table NO-ERROR.
IF AVAIL sfa-export THEN DO:

    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
                              AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr NO-LOCK NO-ERROR. 

    FIND FIRST sfa-export-cli OF sfa-export NO-ERROR.
    IF AVAIL sfa-export-cli THEN DO:

        /*------------------------------------------ Emitente -------------------------------------------*/
        RUN piGravaTTEmitente (OUTPUT h-temp,
                               OUTPUT c-erro).
        IF valid-handle(h-temp) THEN DO:

            RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                         OUTPUT ojsonObjIni,
                                         OUTPUT ojsonArrayIni,
                                         INPUT NO) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                DELETE OBJECT h-temp.
                RETURN "NOK".
            END.            
            DELETE OBJECT h-temp.
        END.

        /*--------------------------------------- Endere‡o List -----------------------------------------*/
        RUN piGravaTTEnderecoList (OUTPUT h-temp).

        IF valid-handle(h-temp) THEN DO:

            /* ------ Adiciona Array -----*/
            RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                         OUTPUT ojsonObjAux,
                                         OUTPUT ojsonArrayAux,
                                         INPUT YES) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                DELETE OBJECT h-temp.
                RETURN "NOK".
            END.

            ojsonObjIni:ADD("EnderecoList",ojsonArrayAux).

            DELETE OBJECT h-temp.
        END.
                
        /*-------------------------------- Condi‡Æo de Pagamento List ----------------------------------*/
        RUN piGravaTTCondicaoPagto (OUTPUT h-temp).

        IF valid-handle(h-temp) THEN DO:

            /* ------ Adiciona Array -----*/
            RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                         OUTPUT ojsonObjAux,
                                         OUTPUT ojsonArrayAux,
                                         INPUT YES) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                DELETE OBJECT h-temp.
                RETURN "NOK".
            END.
            
            ojsonObjIni:ADD("CondicaoPagamentoList",ojsonArrayAux).

            DELETE OBJECT h-temp.
        END.
        
        /*---------------------------------------- Contact List ---------------------------------------*/
        RUN piGravaTTContato (OUTPUT h-temp).

        IF valid-handle(h-temp) THEN DO:

            /* ------ Adiciona Array -----*/
            RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                         OUTPUT ojsonObjAux,
                                         OUTPUT ojsonArrayAux,
                                         INPUT YES) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                DELETE OBJECT h-temp.
                RETURN "NOK".
            END.

            ojsonObjIni:ADD("ContactList",ojsonArrayAux).
            
            DELETE OBJECT h-temp.
        END.

        
        oJsonArrayMain = NEW JsonArray().
        oJsonArrayMain:ADD(ojsonObjIni).
        
        /* ----- Cria Json Principal ------- */
        oJsonObjMain = NEW JsonObject().
        oJsonObjMain:ADD("req",oJsonArrayMain).

        /*
        /*----------- Grava Json ---------- */
        RUN piGeraArqJson  IN h-esint002 (INPUT oJsonObjMain,
                                          INPUT es-api-param.dir-export,
                                          INPUT "cli",
                                          OUTPUT c-arq-json) .
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.
        */
        

        /* ------ Grava conteudo do Json em variavel -----*/
        RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                         OUTPUT c-Json) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.

        ASSIGN sfa-export-cli.c-json = c-Json.

        /* ------------ Envia Objeto Json --------- */
        RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                      INPUT rowid(es-api-param),
                                      OUTPUT lResp,
                                      OUTPUT TABLE RowErrors,
                                      OUTPUT c-retorno).
        IF c-retorno <> "" THEN
            ASSIGN sfa-export.text-retorno = c-retorno.

        IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
            FOR EACH rowErrors:
                ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                DELETE OBJECT h-esint002.
                RETURN "NOK".
            END.
        END.       
        
        
        
    END.
    ELSE do: 
        ASSIGN c-erro = "Registro Tabela de Cliente nÆo localizado".
        RETURN "NOK".
    END.
END.

IF VALID-HANDLE(h-esint002) THEN
    DELETE OBJECT h-esint002.



/* -------------------------------------------------------------- Procedures ------------------------------------------------*/
PROCEDURE piGravaTTEmitente:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i            AS INTEGER     NO-UNDO. 
    DEFINE VARIABLE cTipoCredito AS CHARACTER   NO-UNDO INITIAL "Normal,Autom tico,S¢ Imp Ped, Suspenso, Pg … Vista".
    DEFINE VARIABLE cNatPessoa   AS CHARACTER   NO-UNDO INITIAL "Pessoa F¡sica,Pessoa Jur¡dica,Estrangeiro,Trading".
    
    DEFINE BUFFER bf-emitente FOR emitente.

    FIND FIRST emitente NO-LOCK WHERE emitente.cgc = sfa-export-cli.cgc NO-ERROR.
    IF AVAIL emitente THEN DO:
        
        FIND FIRST es-emitente OF emitente NO-LOCK NO-ERROR.

        FIND FIRST gr-cli OF emitente NO-LOCK NO-ERROR.

        FIND FIRST bf-emitente WHERE bf-emitente.nome-abrev = emitente.nome-matriz NO-LOCK NO-ERROR.
        
        CREATE tt_emitente.
        ASSIGN tt_emitente.RazaoSocial             = emitente.nome-emit             
               tt_emitente.CNPJ                    = emitente.cgc                
               tt_emitente.IE                      = trim(emitente.ins-estadual)
               tt_emitente.Email                   = fncValidaMail(emitente.e-mail)
               tt_emitente.Telefone                = replace(fncFormataTelefone(emitente.telefone[1])," ","")
               tt_emitente.CodigoCliente           = emitente.cod-emitente          
               tt_emitente.GrupoEconomico          = gr-cli.descricao
               tt_emitente.TipoCliente             = string(emitente.identific          )
               tt_emitente.CanalCliente            = emitente.cod-canal-venda  
               tt_emitente.EmailFinanceiro         = fncValidaMail(emitente.e-mail)
               tt_emitente.TelefoneFinanceiro      = replace(fncFormataTelefone(emitente.telefone[1])," ","")
               tt_emitente.ContribuinteICMS        = emitente.contrib-icms         
               tt_emitente.Suframa                 = emitente.cod-suframa           
               tt_emitente.DataValidadeSuframa     = emitente.dat-valid-suframa  
               tt_emitente.NomeAbreviado           = emitente.nome-abrev            
               tt_emitente.LimiteCredito           = dec(emitente.lim-credito) 
               tt_emitente.AvaliacaoCredito        = LOG(emitente.ind-aval   )            
               tt_emitente.TipoCredito             = entry(emitente.Ind-cre-cli,cTipoCredito)
               tt_emitente.Banco                   = IF emitente.cod-banco > 0 THEN string(emitente.cod-banco,"999" ) ELSE ""
               tt_emitente.Agencia                 = string(emitente.agencia,"X(4)")               
               tt_emitente.Conta                   = string(emitente.conta-corren,"X(10)")
               tt_emitente.IM                      = emitente.ins-municipal       
               tt_emitente.NaturezaCliente         = entry(emitente.natureza,cNatPessoa)
               tt_emitente.NaturezaOperacao        = emitente.nat-operacao          
               tt_emitente.Matriz                  = emitente.nome-matriz           
               tt_emitente.Representante           = emitente.cod-rep              
               tt_emitente.Microrregiao            = emitente.nome-mic-reg   
               tt_emitente.BairroEntrega           = emitente.bairro                
               tt_emitente.LogradouroEntrega       = emitente.endereco             
               tt_emitente.ClienteExigeLoteUnico   = IF AVAIL es-emitente THEN es-emitente.exige-loteu      else FALSE
               tt_emitente.ExigeCertifAnalise      = IF AVAIL es-emitente THEN es-emitente.exige-laudo-cq   else FALSE
               tt_emitente.NaoRecebeLoteProxVencto = IF AVAIL es-emitente THEN es-emitente.rejeita-prox-ven else FALSE
               tt_emitente.RamoAtividade           = emitente.atividade                  
               tt_emitente.CNPJCobranca            = IF AVAIL bf-emitente THEN bf-emitente.cgc  ELSE emitente.cgc     
               tt_emitente.ComplementoEntrega      = emitente.endereco_text 
               tt_emitente.CidadeEntrega           = emitente.cidade                
               tt_emitente.BairroEntrega           = emitente.bairro                
               tt_emitente.EstadoEntrega           = emitente.estado                
               tt_emitente.LogradouroEntrega       = emitente.endereco              
               tt_emitente.CepEntrega              = emitente.cep                                             
               tt_emitente.ComplementoCobranca     = emitente.endereco-cob-text   
               tt_emitente.CidadeCobranca          = emitente.cidade-cob
               tt_emitente.BairroCobranca          = emitente.bairro-cob            
               tt_emitente.EstadoCobranca          = emitente.estado-cob            
               tt_emitente.LogradouroCobranca      = emitente.endereco              
               tt_emitente.CepCobranca             = emitente.cep-cob.    

        IF emitente.dt-lim-cred > ADD-INTERVAL(TODAY,100,"year") THEN
            ASSIGN tt_emitente.DataLimiteCredito       = ADD-INTERVAL(TODAY,100,"year").
        ELSE IF emitente.dt-lim-cred < ADD-INTERVAL(TODAY,-100,"year") THEN
            ASSIGN tt_emitente.DataLimiteCredito       = ADD-INTERVAL(TODAY,-100,"year").
        ELSE ASSIGN tt_emitente.DataLimiteCredito       = emitente.dt-lim-cred.

        IF AVAIL es-emitente THEN do:
            IF es-emitente.regime-trib = ? THEN
                ASSIGN tt_emitente.RegimeTributario = "NÆo Classificado".
            ELSE ASSIGN tt_emitente.RegimeTributario = es-emitente.regime-trib.
        END.
        ELSE ASSIGN tt_emitente.RegimeTributario = "NÆo Classificado".

        FIND FIRST pais WHERE pais.nome-pais = emitente.pais NO-LOCK NO-ERROR.
        IF AVAIL pais THEN
            ASSIGN tt_emitente.PaisEntrega = substring(pais.char-1,23,02).

        FIND FIRST pais WHERE pais.nome-pais = emitente.pais-cob NO-LOCK NO-ERROR.
        IF AVAIL pais THEN
            ASSIGN tt_emitente.PaisCobranca = substring(pais.char-1,23,02).

    END.
    ELSE DO:
        pErro = "Registro Emitente nÆo localizado com o campo CGC: " + sfa-export-cli.cgc.
        RETURN "NOK".
    END.

    IF TEMP-TABLE tt_emitente:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_emitente:HANDLE.

END PROCEDURE.

PROCEDURE piGravaTTEnderecoList:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE NO-UNDO.

    FIND FIRST emitente NO-LOCK WHERE emitente.cgc = sfa-export-cli.cgc NO-ERROR.
    IF AVAIL emitente THEN DO:

        FOR EACH loc-entr WHERE loc-entr.nome-abrev = emitente.nome-abrev NO-LOCK:
            CREATE tt_EnderecoList.
            ASSIGN tt_EnderecoList.Pais         = loc-entr.pais          
                   tt_enderecoList.logradouro   = caps(loc-entr.endereco      )
                   tt_EnderecoList.Complemento	= caps(loc-entr.endereco_text )
                   tt_EnderecoList.Cidade	    = loc-entr.cidade       
                   tt_EnderecoList.Bairro	    = caps(loc-entr.bairro        )
                   tt_EnderecoList.Estado	    = loc-entr.estado   
                   tt_EnderecoList.Cep	        = caps(loc-entr.cep           )
                   tt_EnderecoList.CodEntrega   = caps(loc-entr.cod-entrega   )      .

            IF loc-entr.cod-entrega = "PadrÆo" THEN
                ASSIGN tt_enderecoList.Padrao       = TRUE.
            ELSE ASSIGN tt_enderecoList.Padrao       = FALSE.

            FIND FIRST pais WHERE pais.nome-pais = loc-entr.pais NO-LOCK NO-ERROR.
            IF AVAIL pais THEN
                ASSIGN tt_EnderecoList.Pais = substring(pais.char-1,23,02).
        END.
    END.
    

    IF TEMP-TABLE  tt_EnderecoList:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_EnderecoList:HANDLE.

END PROCEDURE.


PROCEDURE piGravaTTCondicaoPagto:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE NO-UNDO.

    FIND FIRST emitente NO-LOCK WHERE emitente.cgc = sfa-export-cli.cgc NO-ERROR.
    IF AVAIL emitente THEN DO:

        FOR EACH es-gp-emit-cond-pagto WHERE es-gp-emit-cond-pagto.cod-emitente = emitente.cod-emitente NO-LOCK:

            FOR EACH cond-pagto NO-LOCK WHERE cond-pagto.cod-cond-pag = es-gp-emit-cond-pagto.cod-cond-pag :
            
                CREATE tt_CondicaoPagamentoList.
                ASSIGN tt_CondicaoPagamentoList.codigoCondicao = string(es-gp-emit-cond-pagto.cod-cond-pag)
                       tt_CondicaoPagamentoList.descricao      = caps(cond-pagto.descricao).
            END.
        END.
    END.

    IF TEMP-TABLE tt_CondicaoPagamentoList:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_CondicaoPagamentoList:HANDLE.

END PROCEDURE.

PROCEDURE piGravaTTContato:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE NO-UNDO.
    DEF VAR i-aplic AS INTEGER NO-UNDO.

    FIND FIRST emitente NO-LOCK WHERE emitente.cgc = sfa-export-cli.cgc NO-ERROR.
    IF AVAIL emitente THEN DO:

        FOR EACH cont-emit OF emitente NO-LOCK:

            CREATE tt_ContatoList.
            ASSIGN tt_ContatoList.Nome	         = Cont-emit.nome
                   tt_ContatoList.CodigoContato  = Cont-emit.sequencia
                   tt_ContatoList.Sobrenome      = ""
                   tt_ContatoList.AreaContato	 = Cont-emit.area
                   tt_ContatoList.Cargo	         = Cont-emit.cargo
                   tt_ContatoList.Email	         = fncValidaMail(Cont-emit.e-mail)
                   tt_ContatoList.Telefone	     = replace(fncFormataTelefone(Cont-emit.telefone)," ","")
                   tt_ContatoList.Descricao	     = Cont-emit.observacao 
                   tt_ContatoList.CNPJ_CPF	     = SUBSTRING(cont-emit.char-2,1,20)
                   tt_ContatoList.Fax	         = replace(fncFormataTelefone(Cont-emit.telefax)," ","")
                   tt_ContatoList.Ramal          = Cont-emit.ramal
                   tt_ContatoList.RamalFax	     = Cont-emit.ramal-fax.



        END.
    END.

    IF TEMP-TABLE tt_ContatoList:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_ContatoList:HANDLE.


END PROCEDURE.

FUNCTION fncValidaMail RETURN CHARACTER
    (INPUT pmail AS CHARACTER ):

    DEFINE VARIABLE regexp       AS CLASS Regex NO-UNDO. 
    DEFINE VARIABLE i            AS INTEGER     NO-UNDO.

    regexp = NEW Regex("^[^\x00-\x1F^\(^\)^\<^\>^\@^\,^\;^\:^\\^\~"^\.^\[^\]^\s]+(\.[^\x00-\x1F^\(^\)^\<^\>^\@^\,^\;^\:^\\^\~"^\.^\[^\]^\s]+)*@([^\x00-\x1F^\(^\)^\<^\> ^\@^\,^\;^\:^\\^\~"^\.^\[^\]^\s]+(\.[^\x00-\x1F^\(^\)^\<^\>^\@^\,^\;^\:^\\^\~"^\.^\[^\]^\s]+))+$"). 

    DO i = 1 TO 100: 
        IF regexp:IsMatch(pmail) THEN RETURN pmail.
        ELSE RETURN "". 
    END.
    DELETE OBJECT regexp.

END FUNCTION.

FUNCTION fncFormataTelefone RETURN CHARACTER 
    (INPUT pFone AS CHARACTER):

    ASSIGN pFone = replace(replace(replace(replace(pfone,"-",""),")",""),"(","")," ","").

    IF LENGTH(pFone) > 10 THEN
        RETURN STRING(pFone,"(99)99999-9999").
    ELSE
        RETURN STRING(pFone,"(99)9999-9999").


END FUNCTION.
