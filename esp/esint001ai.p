/*----------------------------------------------------------------------------------------------/
 Programa..: esint001ai.p
 Objetivo..: Interface Integraá∆o Clientes SFA - Importaá∆o
 Data......: 26/02/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

/* ------- Importaá∆o de Classes ------ */
using Progress.Json.OBJECTModel.*.

DEFINE TEMP-TABLE tt-erros-integracao   NO-UNDO
            FIELD erro      AS CHAR
            FIELD descricao AS CHAR.

DEF VAR pTTEmitente         AS CHARACTER NO-UNDO.
DEF VAR pCodEmitenteReturn  AS INTEGER   NO-UNDO.
DEF VAR cAux                AS CHARACTER NO-UNDO.

/* ------- Definiá∆o de ParÉmetros ----- */
DEFINE INPUT  PARAMETER r-table     AS ROWID      NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pJsonInput  AS JsonObject NO-UNDO.

/* ------- Definiá∆o de Vari†veis ------ */
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE body             AS JsonObject NO-UNDO.
DEFINE VARIABLE jsonOutput       AS JsonObject NO-UNDO.
DEFINE VARIABLE arrJson          AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject NO-UNDO.
DEFINE VARIABLE hBufferMain      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBufferSec       AS HANDLE     NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonArraySec    AS JsonArray  NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER    NO-UNDO.
DEFINE VARIABLE cprop            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-natureza       AS CHARACTER  NO-UNDO INITIAL "Pessoa F°sica,Pessoa Jur°dica,Estrangeiro,Trading". 
DEFINE VARIABLE c-tipocredito    AS CHARACTER  NO-UNDO INITIAL "Normal,Autom†tico,Suspenso,S¢ Imp Ped,Pg Ö Vista".
DEFINE VARIABLE m-json           AS MEMPTR     NO-UNDO.
DEFINE VARIABLE myParser         AS ObjectModelParser NO-UNDO. 

/* ------- Definiá∆o de Temp-Tables e Datasets ------ */
{esp\esint001ai.i}
{esp\esint001ar.i}
{esp\esint007.i}
   
{method/dbotterr.i}

/*------------------------------ Main Begin ----------------------------*/
ASSIGN c-erro = "".

FIND FIRST sfa-import NO-LOCK WHERE ROWID(sfa-import) = r-table NO-ERROR.
IF AVAIL sfa-import THEN DO:

    /* ------- Grava clob para longchar ----- */
    FIND FIRST sfa-import-cli OF sfa-import NO-ERROR.
    IF AVAIL sfa-import-cli THEN DO:

        /* ------ RogÇrio Dias - Gera Json Ö partir de Longchar convertendo para UTF8 ----- */
        FIX-CODEPAGE(cLongJson) = "UTF-8".

        COPY-LOB sfa-import-cli.c-json TO m-json.
        COPY-LOB m-json TO cLongJson NO-CONVERT.

        myParser = NEW ObjectModelParser(). 
        pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).

        /*---------------------------------------------------------------------------------*/
        
        /* ---- Là propriedade Principal ---- */        
        oJsonArrayMain = pJsonInput /*:GetJsonObject("payload":U)*/
                                   :GetJsonArray("req":U).    

        FIND FIRST es-api-param-cliente NO-LOCK NO-ERROR.

        CREATE tt_emitente.
        
        DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

            oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).
            
            IF oJsonObjectMain:Has("RazaoSocial")            then do:
                tt_emitente.RazaoSocial             = oJsonObjectMain:GetCharacter("RazaoSocial")   NO-ERROR      .          /* Nome_emit                      = cFieldsTT[004] */                                    
            END.

            if oJsonObjectMain:Has("CNPJ")                   then do:
                tt_emitente.CNPJ                    = REPLACE(REPLACE(REPLACE(oJsonObjectMain:GetCharacter("CNPJ"),".",""),"/",""),"-","")   NO-ERROR             .          /* cpfcnpj                        = cFieldsTT[043] */                            
            END.
            if oJsonObjectMain:Has("IE")                     then tt_emitente.IE                      = oJsonObjectMain:GetCharacter("IE")                    no-error .          /* Ins_estadual                   = cFieldsTT[044] */                    
            if oJsonObjectMain:Has("Email")                  then tt_emitente.Email                   = oJsonObjectMain:GetCharacter("Email")                 no-error .          /* email                          = cFieldsTT[041] */                    
            if oJsonObjectMain:Has("Telefone")               then tt_emitente.Telefone                = oJsonObjectMain:GetCharacter("Telefone")              no-error .          /* Telefone                       = cFieldsTT[032] */ 
            if oJsonObjectMain:Has("CodigoCliente")          then tt_emitente.CodigoCliente           = oJsonObjectMain:GetInteger("CodigoCliente")           no-error .          /* cod_cliente                    = cFieldsTT[007] */              
            if oJsonObjectMain:Has("TipoClienteCanal")       then tt_emitente.TipoClienteCanal        = oJsonObjectMain:GetCharacter("TipoClienteCanal")      no-error .          /* cod_canal_venda                = cFieldsTT[048] */      
            if oJsonObjectMain:Has("EmailXML")               then tt_emitente.EmailXML                = oJsonObjectMain:GetCharacter("EmailXML")              no-error .          /* email_nfe                      = cFieldsTT[118] */   
            if oJsonObjectMain:Has("CanalCliente")           then tt_emitente.CanalCliente            = integer(oJsonObjectMain:GetCharacter("CanalCliente")) no-error .          /* cod-canal-venda2               = cFieldsTT[090] */
            if oJsonObjectMain:Has("GrupoEconomico")         then tt_emitente.GrupoEconomico          = oJsonObjectMain:GetCharacter("GrupoEconomico")        no-error .          /* cod-canal-venda2               = cFieldsTT[090] */
            if oJsonObjectMain:Has("EmailFinanceiro")        then tt_emitente.EmailFinanceiro         = oJsonObjectMain:GetCharacter("EmailFinanceiro")       no-error .          /* email_financ                   = cFieldsTT[117] */   
            if oJsonObjectMain:Has("TelefoneFinanceiro")     then tt_emitente.TelefoneFinanceiro      = oJsonObjectMain:GetCharacter("TelefoneFinanceiro")    no-error .          /* nr_telefone_finan              = cFieldsTT[114] */              
            if oJsonObjectMain:Has("RegimeTributario")       then tt_emitente.RegimeTributario        = oJsonObjectMain:GetCharacter("RegimeTributario")      no-error .          /* regime-trib                    = cFieldsTT[089] */
            if oJsonObjectMain:Has("ContribuinteICMS")       then tt_emitente.ContribuinteICMS        = oJsonObjectMain:GetLogical("ContribuinteICMS")        no-error . /* contrib-icms                   = cFieldsTT[121] */             
            if oJsonObjectMain:Has("Suframa")                then tt_emitente.Suframa                 = oJsonObjectMain:GetCharacter("Suframa")               no-error .          /* cod-suframa                    = cFieldsTT[201] */             
            if oJsonObjectMain:Has("DataVencimentoSuframa")  then tt_emitente.DataValidadeSuframa     = oJsonObjectMain:GetDate("DataVencimentoSuframa")      no-error . /* dat_valid_suframa              = cFieldsTT[202] */                        
            if oJsonObjectMain:Has("NomeAbreviado")          then tt_emitente.NomeAbreviado           = oJsonObjectMain:GetCharacter("NomeAbreviado")         no-error .              /* nome_abreviado                 = cFieldsTT[003] */ 
            if oJsonObjectMain:Has("LimiteCredito")          then do:

                MESSAGE ">> Limite: " STRING(oJsonObjectMain:GetDecimal("LimiteCredito")).

                tt_emitente.LimiteCredito           = oJsonObjectMain:GetDecimal("LimiteCredito")           no-error .         /* lim-credito                    = cFieldsTT[203] */ 
            END.
            if oJsonObjectMain:Has("AvaliacaoCredito")       then tt_emitente.AvaliacaoCredito        = oJsonObjectMain:GetInteger("AvaliacaoCredito")        no-error . /* ind-aval                       = cFieldsTT[204] */    

            if oJsonObjectMain:Has("TipoCredito")            then do:
                ASSIGN tt_emitente.TipoCredito             = oJsonObjectMain:GetCharacter("TipoCredito")                    no-error .          /* ind-cre-cli                    = cFieldsTT[205] */      
                ASSIGN tt_emitente.TipoCredito  = string(LOOKUP(oJsonObjectMain:GetCharacter("TipoCredito"),c-tipocredito)) NO-ERROR.   
                IF tt_emitente.tipoCredito = ? THEN
                    ASSIGN tt_emitente.tipoCredito = "1".

                MESSAGE 'oJsonObjectMain:GetCharacter("TipoCredito") : ' oJsonObjectMain:GetCharacter("TipoCredito") .
            END.

            MESSAGE ">>> tipo de credito: " tt_emitente.tipoCredito .

            if oJsonObjectMain:Has("DataLimiteCredito")  then tt_emitente.DataLimiteCredito       = oJsonObjectMain:GetDate("DataLimiteCredito")          no-error .             /* dt-lim-credito                 = cFieldsTT[206] */               
            if oJsonObjectMain:Has("Banco")              then tt_emitente.Banco                   = oJsonObjectMain:GetCharacter("Banco")                 no-error .                            /* cod-banco                      = cFieldsTT[207] */                
            if oJsonObjectMain:Has("Agencia")            then tt_emitente.Agencia                 = oJsonObjectMain:GetCharacter("Agencia")               no-error .          /* agencia                        = cFieldsTT[208] */                              
            if oJsonObjectMain:Has("Conta")              then tt_emitente.Conta                   = oJsonObjectMain:GetCharacter("Conta")                 no-error .          /* conta-corren                   = cFieldsTT[209] */                              
                                                                                                                                                               
            if oJsonObjectMain:Has("NaturezaCliente") then do:
                IF oJsonObjectMain:GetCharacter("NaturezaCliente") MATCHES "*dica*" THEN
                    ASSIGN tt_emitente.NaturezaCliente  = "2" NO-ERROR.
                ELSE IF oJsonObjectMain:GetCharacter("NaturezaCliente") MATCHES "*sica*" THEN
                    ASSIGN tt_emitente.NaturezaCliente  = "1" NO-ERROR.
                ELSE 
                    ASSIGN tt_emitente.NaturezaCliente  = string(LOOKUP(oJsonObjectMain:GetCharacter("NaturezaCliente"),c-natureza)) NO-ERROR.                   
            END.

            if oJsonObjectMain:Has("NaturezaOperacao")       then do:
                tt_emitente.NaturezaOperacao        = oJsonObjectMain:GetCharacter("NaturezaOperacao")       no-error.          /* natureza_operacao              = cFieldsTT[051] */                 
                IF tt_emitente.naturezaOperacao = ? THEN
                    IF AVAIL es-api-param-cliente AND es-api-param-cliente.nat-operacao <> ? THEN
                        ASSIGN tt_emitente.naturezaOperacao = es-api-param-cliente.nat-operacao.                    
            END.
            if oJsonObjectMain:Has("Matriz")                 then tt_emitente.Matriz                  = oJsonObjectMain:GetCharacter("Matriz")                 no-error.          /* nome_cliente                   = cFieldsTT[008] */                              
            if oJsonObjectMain:Has("Representante")          then tt_emitente.Representante           = integer(oJsonObjectMain:Getcharacter("Representante"))            no-error.          /* cod_representante              = cFieldsTT[009] */           
            if oJsonObjectMain:Has("Microrregiao")           then tt_emitente.Microrregiao            = oJsonObjectMain:GetCharacter("Microrregiao")           no-error.          /* nome_mic_reg                   = cFieldsTT[211] */             
            if oJsonObjectMain:Has("ClienteExigeLoteUnico")  then tt_emitente.ClienteExigeLoteUnico   = oJsonObjectMain:GetLogical("ClienteExigeLoteUnico")    no-error.  /* exige-loteu                    = cFieldsTT[087] */                           
            if oJsonObjectMain:Has("ExigeCertifAnalise")     then tt_emitente.ExigeCertifAnalise      = oJsonObjectMain:GetLogical("ExigeCertifAnalise")       no-error.      /* exige-laudo                    = cFieldsTT[086] */             
            if oJsonObjectMain:Has("NaoRecebeLoteProxVencto")then tt_emitente.NaoRecebeLoteProxVencto = oJsonObjectMain:GetLogical("NaoRecebeLoteProxVencto")  no-error. /* rejeita-prox-vem               = cFieldsTT[088] */                               
            if oJsonObjectMain:Has("RamoAtividade")          then do:
                tt_emitente.RamoAtividade           = substring(oJsonObjectMain:GetCharacter("RamoAtividade"),1,18)          no-error.          /* nm_ramo_atividade              = cFieldsTT[079] */  
            END.
            if oJsonObjectMain:Has("CNPJCobranca")           then tt_emitente.CNPJCobranca            = REPLACE(REPLACE(REPLACE(oJsonObjectMain:GetCharacter("CNPJCobranca"),".",""),"/",""),"-","")  NO-ERROR .          /* cpfcnpjCobranca                = cFieldsTT[046] */                
            if oJsonObjectMain:Has("PaisEntrega")            then tt_emitente.PaisEntrega             = oJsonObjectMain:GetCharacter("PaisEntrega")            no-error.          /* pais                           = cFieldsTT[097] */                                
            if oJsonObjectMain:Has("ComplementoEntrega")     then tt_emitente.ComplementoEntrega      = oJsonObjectMain:GetCharacter("ComplementoEntrega")     no-error.          /* end-complemento                = cFieldsTT[212] */                                                
            if oJsonObjectMain:Has("CidadeEntrega")          then tt_emitente.CidadeEntrega           = oJsonObjectMain:GetCharacter("CidadeEntrega")          no-error.          /* cidade                         = cFieldsTT[095] */                                                                
            if oJsonObjectMain:Has("BairroEntrega")          then tt_emitente.BairroEntrega           = oJsonObjectMain:GetCharacter("BairroEntrega")          no-error.          /* bairro                         = cFieldsTT[015] */                                                                
            if oJsonObjectMain:Has("EstadoEntrega")          then tt_emitente.EstadoEntrega           = oJsonObjectMain:GetCharacter("EstadoEntrega")          no-error.          /* estado                         = cFieldsTT[096] */                                                                
            if oJsonObjectMain:Has("LogradouroEntrega")      then tt_emitente.LogradouroEntrega       = oJsonObjectMain:GetCharacter("LogradouroEntrega")      no-error.          /* endereco                       = cFieldsTT[096] */                                                                
            if oJsonObjectMain:Has("CepEntrega")             then do:
                tt_emitente.CepEntrega              = replace(oJsonObjectMain:GetCharacter("CepEntrega"),"-","")             no-error.          /* cep                            = cFieldsTT[016] */                                                                                
            END.
            if oJsonObjectMain:Has("PaisCobranca")           then tt_emitente.PaisCobranca            = oJsonObjectMain:GetCharacter("PaisCobranca")           no-error.          /* Pais_cob                       = cFieldsTT[024] */                 
            if oJsonObjectMain:Has("ComplementoCobranca")    then tt_emitente.ComplementoCobranca     = oJsonObjectMain:GetCharacter("ComplementoCobranca")    no-error.          /* end-completo-cob               = cFieldsTT[213] */              
            if oJsonObjectMain:Has("CidadeCobranca")         then tt_emitente.CidadeCobranca          = oJsonObjectMain:GetCharacter("CidadeCobranca")         no-error.          /* Cidade_cob                     = cFieldsTT[030] */               
            if oJsonObjectMain:Has("BairroCobranca")         then tt_emitente.BairroCobranca          = oJsonObjectMain:GetCharacter("BairroCobranca")         no-error.          /* bairro_cob                     = cFieldsTT[026] */                
            if oJsonObjectMain:Has("EstadoCobranca")         then tt_emitente.EstadoCobranca          = oJsonObjectMain:GetCharacter("EstadoCobranca")         no-error.          /* Estado_cob                     = cFieldsTT[028] */                
            if oJsonObjectMain:Has("LogradouroCobranca")     then tt_emitente.LogradouroCobranca      = oJsonObjectMain:GetCharacter("LogradouroCobranca")     no-error.          /* endereco_cob                   = cFieldsTT[023] */               
            if oJsonObjectMain:Has("CepCobranca")            then do:
                tt_emitente.CepCobranca             = replace(oJsonObjectMain:GetCharacter("CepCobranca"),"-","")            no-error.          /* Cep_cob                        = cFieldsTT[027] */
            END.

            /*
            IF tt_emitente.Suframa = ? THEN DO:
                ASSIGN c-erro = c-erro + "C¢digo Suframa inv†lido ou n∆o informado" + CHR(10).

            END.
            */

            IF tt_emitente.CNPJCobranca = ? THEN
                ASSIGN c-erro = c-erro + "CNPJ cobranáa inv†lido" + CHR(10).

            IF tt_emitente.TipoClienteCanal = ? THEN 
                ASSIGN c-erro = c-erro + "TipoClienteCanal inv†lido" + CHR(10).

            IF tt_emitente.CanalCliente = ? THEN
                ASSIGN c-erro = c-erro + "CanalCliente inv†lido" + CHR(10).

            IF tt_emitente.GrupoEconomico = ? THEN DO:
                IF AVAIL es-api-param-cliente AND es-api-param-cliente.cod-gr-cli <> ? THEN
                    ASSIGN tt_emitente.GrupoEconomico  = STRING(es-api-param-cliente.cod-gr-cli).
                ELSE
                    ASSIGN c-erro = c-erro + "GrupoEconomico inv†lido" + CHR(10).
            END.

            IF tt_emitente.NaturezaCliente = ? THEN DO:
                IF AVAIL es-api-param-cliente AND es-api-param-cliente.natureza <> ? THEN
                    ASSIGN tt_emitente.NaturezaCliente  = STRING(es-api-param-cliente.natureza).
                ELSE
                    ASSIGN c-erro = c-erro + "NaturezaCliente inv†lida" + CHR(10).
            END.

            IF tt_emitente.AvaliacaoCredito = ? THEN
                ASSIGN c-erro = c-erro + "AvaliacaoCredito inv†lido" + CHR(10).

            IF tt_emitente.Telefone = ? THEN
                ASSIGN c-erro = c-erro + "Telefone inv†lido ou n∆o informado" + CHR(10).

            IF tt_emitente.TelefoneFinanceiro = ? THEN
                ASSIGN c-erro = c-erro + "Telefone Financeiro inv†lido ou n∆o informado" + CHR(10).

            IF tt_emitente.Representante = 0 THEN DO:
                ASSIGN c-erro = c-erro + "Representante n∆o cadastrado".
            END.
            
            IF c-erro <> "" THEN 
                RETURN "NOK".

            /*if oJsonObjectMain:Has("IM")                     the") = oJsonObjectMain:GetCharacter("IM")                  .                         */
            /*if oJsonObjectMain:Has("SaldoCredito")           the") = STRING(oJsonObjectMain:GetDecimal("SaldoCredito")).                       */
            /*
            /* ------ Objetos diferentes de array  ------ */
            IF oJsonObjectMain:Has("EnderecoList") THEN DO:  
                oJsonArraySec = oJsonObjectMain:GetJsonArray("EnderecoList").

                CREATE tt_EnderecoList.

                DO iCountSec = 1 TO oJsonArraySec:LENGTH:
                    oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec).                               

                    if oJsonObjectSec:Has("Pais")         then ENTRY(000,pTTEmitente,"|") = oJsonObjectSec:GetCharacter("Pais")         . /* cod_pais                       = cFieldsTT[013] */
                    if oJsonObjectSec:Has("Logradouro")   then ENTRY(000,pTTEmitente,"|") = oJsonObjectSec:GetCharacter("Logradouro")   . /* endereco                       = cFieldsTT[012] */
                    if oJsonObjectSec:Has("Complemento")  then ENTRY(000,pTTEmitente,"|") = oJsonObjectSec:GetCharacter("Complemento")  .
                    if oJsonObjectSec:Has("Cidade")       then ENTRY(000,pTTEmitente,"|") = oJsonObjectSec:GetCharacter("Cidade")       . /* cod_cidade                     = cFieldsTT[020] */
                    if oJsonObjectSec:Has("Bairro")       then ENTRY(000,pTTEmitente,"|") = oJsonObjectSec:GetCharacter("Bairro")       . /* bairro                         = cFieldsTT[015] */
                    if oJsonObjectSec:Has("Estado")       then ENTRY(000,pTTEmitente,"|") = oJsonObjectSec:GetCharacter("Estado")       . /* cod_estado                     = cFieldsTT[018] */
                    if oJsonObjectSec:Has("Cep")          then ENTRY(000,pTTEmitente,"|") = oJsonObjectSec:GetCharacter("Cep")          . /* Cep                            = cFieldsTT[016] */                   

                END.
            END.
            */

            IF oJsonObjectMain:Has("ContactList") THEN DO:

                oJsonArraySec = oJsonObjectMain:GetJsonArray("ContactList").

                DO iCountSec = 1 TO oJsonArraySec:LENGTH:
                    CREATE tt_ContatoList.

                    oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec). 
                    ASSIGN fld-rel = iCountSec.

                    if oJsonObjectSec:Has("Nome")         then tt_ContatoList.Nome           = oJsonObjectSec:GetCharacter("Nome")               no-error.
                    if oJsonObjectSec:Has("CodigoContato")then tt_ContatoList.CodigoContato  = INT(oJsonObjectSec:GetCharacter("CodigoContato")) no-error.
                    if oJsonObjectSec:Has("Sobrenome")    then tt_ContatoList.Sobrenome      = oJsonObjectSec:GetCharacter("Sobrenome")          no-error.
                    if oJsonObjectSec:Has("AreaContato")  then tt_ContatoList.AreaContato    = oJsonObjectSec:GetCharacter("AreaContato")        no-error.
                    if oJsonObjectSec:Has("Cargo")        then tt_ContatoList.Cargo          = oJsonObjectSec:GetCharacter("Cargo")              no-error.
                    if oJsonObjectSec:Has("Email")        then tt_ContatoList.Email          = oJsonObjectSec:GetCharacter("Email")              no-error.
                    if oJsonObjectSec:Has("Telefone")     then tt_ContatoList.Telefone       = oJsonObjectSec:GetCharacter("Telefone")           no-error.
                    if oJsonObjectSec:Has("Aplicacao")    then tt_ContatoList.Aplicacao      = oJsonObjectSec:GetCharacter("Aplicacao")          no-error.
                    if oJsonObjectSec:Has("Descricao")    then tt_ContatoList.Descricao      = oJsonObjectSec:GetCharacter("Descricao")          no-error.
                    if oJsonObjectSec:Has("CNPJ_CPF")     then tt_ContatoList.CNPJ_CPF       = REPLACE(REPLACE(REPLACE(oJsonObjectSec:GetCharacter("CNPJ_CPF"),".",""),"/",""),"-","") NO-ERROR.
                    if oJsonObjectSec:Has("Fax")          then tt_ContatoList.Fax            = oJsonObjectSec:GetCharacter("Fax")                NO-ERROR.  
                    if oJsonObjectSec:Has("Ramal")        then tt_ContatoList.Ramal          = oJsonObjectSec:GetCharacter("Ramal")              NO-ERROR.
                    if oJsonObjectSec:Has("RamalFAX")     then tt_ContatoList.RamalFAX       = oJsonObjectSec:GetCharacter("RamalFAX")           NO-ERROR.
                    
                END.
            END.
            
            IF oJsonObjectMain:Has("CondicaoPagamentoList") THEN DO:
                oJsonArraySec = oJsonObjectMain:GetJsonArray("CondicaoPagamentoList").

                
                CREATE tt_CondicaoPagamentoList.

                DO iCountSec = 1 TO oJsonArraySec:LENGTH:
                    oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec). 

                    if oJsonObjectSec:Has("codigoCondicao")  then do:
                        tt_CondicaoPagamentoList.codigoCondicao = oJsonObjectSec:GetCharacter("codigoCondicao") NO-ERROR. /* cond_pag                       = cFieldsTT[010] */
                        IF tt_condicaoPagamentoList.codigoCondicao = ? THEN
                            ASSIGN tt_condicaopagamentoList.codigoCondicao = string(es-api-param-cliente.cod-cond-pag).
                    END.
                    if oJsonObjectSec:Has("descricao")       then tt_CondicaoPagamentoList.descricaoCondicao = oJsonObjectSec:GetCharacter("descricao")   NO-ERROR. /* nome_pag                       = cFieldsTT[011] */
                END.
            END.      
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = c-erro + ERROR-STATUS:GET-MESSAGE(1).
                RETURN "NOK".
            END.
        END.
        
        IF NOT TEMP-TABLE tt_emitente:HAS-RECORDS THEN ASSIGN c-erro = c-erro + "N∆o h† registros para processar". 
        ELSE DO:
            RUN esp/esint001aic.p (INPUT  TABLE tt_emitente,
                                   INPUT  TABLE tt_ContatoList,
                                   INPUT  TABLE tt_CondicaoPagamentoList,
                                   OUTPUT TABLE tt-erros-integracao,
                                   OUTPUT pCodEmitenteReturn) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                c-erro = c-erro + ERROR-STATUS:GET-MESSAGE(1).
                RETURN "NOK".
            END.

            IF c-erro = "" THEN DO:

                FOR EACH tt-erros-integracao:
                    
                    IF c-erro > "" THEN 
                        ASSIGN c-erro = CHR(13) + c-erro.
    
                    ASSIGN c-erro = c-erro + tt-erros-integracao.erro + " - " + tt-erros-integracao.descricao.                    
               END.

            END.
        END.
    END.
END.
