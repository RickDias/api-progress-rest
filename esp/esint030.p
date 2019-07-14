/*----------------------------------------------------------------------------------------------/
 Programa..: esint030.p
 Objetivo..: Interface Integraá∆o Contrto de Fornecedores 
 Data......: 
 Autor.....: 
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/
/* ------- Importaá∆o de Classes ------ */
using Progress.Json.OBJECTModel.*.

/* ------- Definiá∆o de ParÉmetros ----- */
DEFINE INPUT  PARAMETER r-table     AS ROWID      NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pJsonInput  AS JsonObject NO-UNDO.

/* ------- Definiá∆o de Vari†veis ------ */
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE body             AS JsonObject NO-UNDO.
DEFINE VARIABLE jsonOutput       AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectMat   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonArraySec    AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonArrayMat    AS JsonArray  NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER    NO-UNDO.
DEFINE VARIABLE cprop            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE m-json           AS MEMPTR     NO-UNDO.
DEFINE VARIABLE myParser         AS ObjectModelParser NO-UNDO. 

/* ------- Definiá∆o de Temp-Tables e Datasets ------ */
//{esp\esint030.i}
{esp\esint030a.p}

/*------------------------------ Main Begin ----------------------------*/
ASSIGN c-erro = "".

FIND FIRST sfa-import NO-LOCK WHERE ROWID(sfa-import) = r-table NO-ERROR.
IF NOT AVAIL sfa-import THEN RETURN "NOK".

/* ------- Grava clob para longchar ----- */
FIND FIRST sfa-import-contr OF sfa-import NO-ERROR.
IF NOT AVAIL sfa-import-contr THEN RETURN "NOK".

/* ------ Gera Json Ö partir de Longchar convertendo para UTF8 ----- */
FIX-CODEPAGE(cLongJson) = "UTF-8".


COPY-LOB sfa-import-contr.c-json TO m-json.
COPY-LOB m-json TO cLongJson NO-CONVERT.

 myParser = NEW ObjectModelParser(). 
 pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).

 /*---------------------------------------------------------------------------------*/

 /* ---- Là propriedade Principal ---- */        
oJsonArrayMain = pJsonInput /*GetJsonObject("payload":U)*/
                            //:GetJsonArray("req":U).
                            :GetJsonArray("ContratoFornecedor":U).

//Preciso Criar a tabela de parametros para os dados/informaá‰es que n∆o consta no Ariba
//depois que cirar o 
FIND FIRST tt-es-api-param-contr NO-LOCK NO-ERROR.

 //Coltar aqui as Validaá‰es e Carregar as tamp-tables  
 DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

      oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

      RUN pi-criaTTContrato.

      RUN pi-criaTTJItemContrato.

     // RUN pi-criaTTJMatrizItemContrato.

      /* -- Validaá‰es _____*/
      // RUN pi-valida-dados.   necess†ri criar as validaá‰es. 

 END.

 RUN pi-processa.  //chama a a pi-processa do esint030a.p


 PROCEDURE pi-criaTTContrato.

     EMPTY TEMP-TABLE ttj-contrato-for. //Limpa a tabela antes de ser populada pelo Json 

       IF   oJsonObjectMain:Has("nr-contrato")              THEN ttj-contrato-for.des-contrat           = oJsonObjectMain:GetCharacter("nr-contrato")                NO-ERROR.
       IF	oJsonObjectMain:Has("cod-emitente ")            THEN ttj-contrato-for.cod-emitente        	= INT(oJsonObjectMain:GetCharacter("cod-emitentte"))         NO-ERROR.
       IF	oJsonObjectMain:Has("dt-ini-validade")          THEN ttj-contrato-for.dt-ini-validade     	= oJsonObjectMain:Getdate("dt-ini-validade")                 NO-ERROR.
       IF	oJsonObjectMain:Has("dt-ter-validade")          THEN ttj-contrato-for.dt-ter-validade     	= oJsonObjectMain:Getdate("dt-ter-validade")                 NO-ERROR.
       IF	oJsonObjectMain:Has("cod-comprado")          	THEN ttj-contrato-for.cod-comprado        	= oJsonObjectMain:GetCharacter("cod-comprado")               NO-ERROR.
       IF	oJsonObjectMain:Has("cod-cond-pag")       	    THEN ttj-contrato-for.cod-cond-pag        	= INT(oJsonObjectMain:GetCharacter("cod-cond-pag"))          NO-ERROR.
       IF	oJsonObjectMain:Has("cod-estabel")             	THEN ttj-contrato-for.cod-estabel         	= oJsonObjectMain:GetCharacter("cod-estabel")                NO-ERROR.
       if	oJsonObjectMain:Has("val-total")               	THEN ttj-contrato-for.val-total           	= INT(oJsonObjectMain:GetCharacter("val-total"))             NO-ERROR.
       if	oJsonObjectMain:Has("val-fatur-minimo")     	THEN ttj-contrato-for.val-fatur-minimo    	= decimal(oJsonObjectMain:GetCharacter("val-fatur-minimo"))  NO-ERROR.
       If	oJsonObjectMain:Has("acum-val-pago")           	THEN ttj-contrato-for.acum-val-pago       	= decimal(oJsonObjectMain:GetCharacter("acum-val-pago"))     NO-ERROR.
       IF	oJsonObjectMain:Has("mo-codigo")            	THEN ttj-contrato-for.mo-codigo           	= int(oJsonObjectMain:GetCharacter("mo-codigo"))             NO-ERROR.

       ASSIGN i-num-contrato = 0.

         FIND LAST contrato-for WHERE contrato-for.nr-contrato <= param-contrat.num-seq-fim-central and
                                 contrato-for.nr-contrato >= param-contrat.num-seq-ini-central no-lock no-error.

            IF AVAIL contrato-for THEN DO:          
                ASSIGN i-num-contrato = contrato-for.nr-contrato + 1.  
            END.
        
       CREATE tt-imp-contrato-for.
       ASSIGN tt-imp-contrato-for.nr-contrato     = string(i-num-contrato)
              tt-imp-contrato-for.des-contrat     = ttj-contrato-for.des-contrat
              tt-imp-contrato-for.cod-emitente    = ttj-contrato-for.cod-emitente
              tt-imp-contrato-for.dt-ini-validade = ttj-contrato-for.dt-ini-validade
              tt-imp-contrato-for.dt-ter-validade =  ttj-contrato-for.dt-ter-validade
           .

  //   IF	oJsonObjectMain:Has("dt-contrato")              THEN ttj-contrato-for.dt-contrato         	= oJsonObjectMain:Getdate("dt-contrato")                     NO-ERROR.
  //   IF	oJsonObjectMain:Has("via-transp")               THEN ttj-contrato-for.via-transp          	= int(oJsonObjectMain:GetCharacter("via-transp"))            NO-ERROR.
  //   if	oJsonObjectMain:Has("cod-transp")               THEN ttj-contrato-for.cod-transp          	= int(oJsonObjectMain:GetCharacter("cod-transp"))            NO-ERROR.
  //   IF	oJsonObjectMain:Has("tp-fornecim")         	    THEN ttj-contrato-for.tp-fornecim         	= int(oJsonObjectMain:GetCharacter("tp-fornecim"))           NO-ERROR.
  //   IF	oJsonObjectMain:Has("frete")        	        THEN ttj-contrato-for.frete               	= int(oJsonObjectMain:GetCharacter("frete"))                 NO-ERROR.
  //   IF	oJsonObjectMain:Has("natureza")     	        THEN ttj-contrato-for.natureza            	= int(oJsonObjectMain:GetCharacter("natureza"))              NO-ERROR.
  //   IF	oJsonObjectMain:Has("moeda")                  	THEN ttj-contrato-for.moeda               	= int(oJsonObjectMain:GetCharacter("moeda"))                 NO-ERROR.
  //   IF	oJsonObjectMain:Has("contato")                	THEN ttj-contrato-for.contato             	= oJsonObjectMain:GetCharacter("contato")                    NO-ERROR.
  //   IF	oJsonObjectMain:Has("impr-contrat")           	THEN ttj-contrato-for.impr-contrat        	= logical(oJsonObjectMain:GetCharacter("impr-contrat"))      NO-ERROR.
  //   IF	oJsonObjectMain:Has("cod-tipo-contrat")        	THEN ttj-contrato-for.cod-tipo-contrat    	= int(oJsonObjectMain:GetCharacter("cod-tipo-contrat"))      NO-ERROR.
  //   IF	oJsonObjectMain:Has("gestor-tecnico")          	THEN ttj-contrato-for.gestor-tecnico      	= oJsonObjectMain:GetCharacter("gestor-tecnico")             NO-ERROR.
  //   IF	oJsonObjectMain:Has("variacao-qtd")          	THEN ttj-contrato-for.variacao-qtd        	= decimal(oJsonObjectMain:GetCharacter("variacao-qtd"))      NO-ERROR.
  //   IF oJsonObjectMain:Has("variacao-preco")          	THEN ttj-contrato-for.variacao-preco      	= int(oJsonObjectMain:GetCharacter("variacao-preco"))        NO-ERROR.
  //   IF	oJsonObjectMain:Has("cod-estab-orig")       	THEN ttj-contrato-for.cod-estab-orig      	= oJsonObjectMain:GetCharacter("cod-estab-orig")             NO-ERROR.
  //   if	oJsonObjectMain:Has("cod-estab-cobr")          	THEN ttj-contrato-for.cod-estab-cobr      	= oJsonObjectMain:GetCharacter("cod-estab-cobr")             NO-ERROR.

  //   if	oJsonObjectMain:Has("cod-estab-entr")          	THEN ttj-contrato-for.cod-estab-entr      	= oJsonObjectMain:GetCharacter("cod-estab-entr")             NO-ERROR.
  //   if	oJsonObjectMain:Has("qtd-total")            	THEN ttj-contrato-for.qtd-total           	= decimal(oJsonObjectMain:GetCharacter("qtd-total"))         NO-ERROR.
  //   if	oJsonObjectMain:Has("sld-qtd")                	THEN ttj-contrato-for.sld-qtd             	= decimal(oJsonObjectMain:GetCharacter("sld-qtd"))           NO-ERROR.
  //   if	oJsonObjectMain:Has("sld-val")                 	THEN ttj-contrato-for.sld-val             	= decimal(oJsonObjectMain:GetCharacter("sld-val"))           NO-ERROR.
  //   if	oJsonObjectMain:Has("acum-rec-qtd")           	THEN ttj-contrato-for.acum-rec-qtd        	= decimal(oJsonObjectMain:GetCharacter("acum-rec-qtd"))      NO-ERROR.
  //   //if	oJsonObjectMain:Has("acum-rec-val")           	THEN ttj-contrato-for.acum-rec-val        	= DECIMAL(oJsonObjectMain:GetCharacter("acum-rec-val"))      NO-ERROR.
     //if	oJsonObjectMain:Has("sld-qtd-liber")         	THEN ttj-contrato-for.sld-qtd-liber       	= decimal(oJsonObjectMain:GetCharacter("sld-qtd-liber"))     NO-ERROR.
     //if	oJsonObjectMain:Has("sld-val-liber")         	THEN ttj-contrato-for.sld-val-liber       	= decimal(oJsonObjectMain:GetCharacter("sld-val-liber"))     NO-ERROR.

     //if	oJsonObjectMain:Has("des-contrat")          	THEN ttj-contrato-for.des-contrat         	= oJsonObjectMain:GetCharacter("des-contrat")                NO-ERROR.

     //if	oJsonObjectMain:Has("log-libera")            	THEN ttj-contrato-for.log-libera          	= logical(oJsonObjectMain:GetCharacter("log-libera"))        NO-ERROR.
     //if	oJsonObjectMain:Has("sld-qtd-med")          	THEN ttj-contrato-for.sld-qtd-med         	= decimal(oJsonObjectMain:GetCharacter("sld-qtd-med"))       NO-ERROR.
     //if	oJsonObjectMain:Has("sal-qtd-liber-med")       	THEN ttj-contrato-for.sal-qtd-liber-med   	= decimal(oJsonObjectMain:GetCharacter("sal-qtd-liber-med")) NO-ERROR.
     //if	oJsonObjectMain:Has("sld-val-med")          	THEN ttj-contrato-for.sld-val-med         	= decimal(oJsonObjectMain:GetCharacter("sld-val-med"))       NO-ERROR.
     //if	oJsonObjectMain:Has("sld-val-liber-med")      	THEN ttj-contrato-for.sld-val-liber-med   	= decimal(oJsonObjectMain:GetCharacter("sld-val-liber-med")) NO-ERROR.
     //if	oJsonObjectMain:Has("cod-projeto")             	THEN ttj-contrato-for.cod-projeto         	= oJsonObjectMain:GetCharacter("cod-projeto")                NO-ERROR.
     //if	oJsonObjectMain:Has("ind-sit-contrat")          THEN ttj-contrato-for.ind-sit-contrat     	= int(oJsonObjectMain:GetCharacter("ind-sit-contrat"))       NO-ERROR.
     //if	oJsonObjectMain:Has("narrat-contrat")       	THEN ttj-contrato-for.narrat-contrat      	= oJsonObjectMain:GetCharacter("narrat-contrat")             NO-ERROR.
     //if	oJsonObjectMain:Has("ind-preco")         	    THEN ttj-contrato-for.ind-preco           	= int(oJsonObjectMain:GetCharacter("ind-preco"))             NO-ERROR.
     //if	oJsonObjectMain:Has("sc-codigo")             	THEN ttj-contrato-for.sc-codigo           	= oJsonObjectMain:GetCharacter("sc-codigo")                  NO-ERROR.
     //if	oJsonObjectMain:Has("ct-codigo")            	THEN ttj-contrato-for.ct-codigo           	= oJsonObjectMain:GetCharacter("cod-emitentte")              NO-ERROR.

     IF oJsonObjectMain:Has("ItensContrato") THEN DO:
         RUN pi-criaTTJItemContrato.
     END.


     //Comentei a Criaá∆o da Matriz de Rateio

/*      IF oJsonObjectMain:Has("MatrizItemContrato") THEN DO: */
/*          RUN pi-criaTTJMatrizItemContrato.                 */
/*      END.                                                  */
        
 END PROCEDURE.


 PROCEDURE pi-criaTTJItemContrato.

     EMPTY TEMP-TABLE ttj-itenscontrato-for.  //Limpa a tabela antes de de ser populada pelo Json 

            oJsonArraySec = oJsonObjectMain:GetJsonArray("ItensContrato").
            CREATE  ttj-itenscontrato-for.

            DO iCountSec = 1 TO oJsonArraySec:LENGTH: 

                oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec). 
                //ASSIGN fld-rel = iCountSec.

                IF oJsonObjectSec:Has("nr-contrato")       THEN ttj-itenscontrato-for.nr-contrato             = INT(oJsonObjectSec:GetCharacter("nr-contrato"))               NO-ERROR.
                IF oJsonObjectSec:Has("cod-emitente ")     THEN ttj-itenscontrato-for.cod-emitente            = INT(oJsonObjectSec:GetCharacter("cod-emitentte"))             NO-ERROR.
                IF oJsonObjectSec:Has("preco-unit")        THEN ttj-itenscontrato-for.preco-unit              = DECIMAL(oJsonObjectSec:GetCharacter("preco-unit"))            NO-ERROR.
                IF oJsonObjectSec:Has("it-codigo")         THEN ttj-itenscontrato-for.it-codigo               = oJsonObjectSec:GetCharacter("it-codigo")                      NO-ERROR.
                IF oJsonObjectSec:Has("narrat-item")       THEN ttj-itenscontrato-for.narrat-item             = oJsonObjectSec:GetCharacter("narrat-item")                    NO-ERROR.
                IF oJsonObjectSec:Has("preco-fornec")      THEN ttj-itenscontrato-for.preco-fornec            = decimal(oJsonObjectSec:GetCharacter("preco-fornec"))          NO-ERROR.
                IF oJsonObjectSec:Has("val-frete")         THEN ttj-itenscontrato-for.val-frete               = DECIMAL(oJsonObjectSec:GetCharacter("val-frete"))             NO-ERROR.

                //IF oJsonObjectSec:Has("qtd-minima")        THEN ttj-itenscontrato-for.qtd-minima              = DECIMAL(oJsonObjectSec:GetCharacter("qtd-monimat"))           NO-ERROR.
                //IF oJsonObjectSec:Has("sld-val")           THEN ttj-itenscontrato-for.sld-val                 = DECIMAL(oJsonObjectSec:GetCharacter("sld-val"))               NO-ERROR.
                //IF oJsonObjectSec:Has("mo-codigo")         THEN ttj-itenscontrato-for.mo-codigo               = DECIMAL(oJsonObjectSec:GetCharacter("mo-codigo"))             NO-ERROR.
                //IF oJsonObjectSec:HAS("val-total")         THEN ttj-itenscontrato-for.val-total               = DECIMAL(oJsonObjectSec:GetCharacter("val-total"))             NO-ERROR.
                //IF oJsonObjectSec:Has("codigo-icm")        THEN ttj-itenscontrato-for.codigo-icm              = INT(oJsonObjectSec:GetCharacter("codigo-icm"))                NO-ERROR.                                                                                                                                             
                //IF oJsonObjectSec:Has("un")                THEN ttj-itenscontrato-for.un                      = oJsonObjectSec:GetCharacter("un")                             NO-ERROR.
                //IF oJsonObjectSec:Has("contato ")          THEN ttj-itenscontrato-for.contato                 = oJsonObjectSec:GetCharacter("contato")                        NO-ERROR.
                //IF oJsonObjectSec:Has("num-seq-item")      THEN ttj-itenscontrato-for.num-seq-item            = INT(oJsonObjectSec:GetCharacter("num-seq-item"))              NO-ERROR.
                //IF oJsonObjectSec:Has("frequencia")        THEN ttj-itenscontrato-for.frequencia              = INT(oJsonObjectSec:GetCharacter("frequentcia"))               NO-ERROR.
                //IF oJsonObjectSec:Has("qtd-total")         THEN ttj-itenscontrato-for.qtd-total               = DECIMAL(oJsonObjectSec:GetCharacter("qtd-total"))             NO-ERROR.
                //IF oJsonObjectSec:Has("ind-un-contrato")   THEN ttj-itenscontrato-for.ind-un-contrato         = DECIMAL(oJsonObjectSec:GetCharacter("ind-un-contrato"))       NO-ERROR.
                //IF oJsonObjectSec:HAS("sld-qtd")           THEN ttj-itenscontrato-for.sld-qtd                 = decimal(oJsonObjectSec:GetCharacter("sld-qtd"))               NO-ERROR.
                //IF oJsonObjectSec:Has("acum-rec-val")      THEN ttj-itenscontrato-for.acum-rec-val            = DECIMAL(oJsonObjectSec:GetCharacter("acum-rec-val"))          NO-ERROR.                                                                                                                                               
                //IF oJsonObjectSec:Has("log-control-event") THEN ttj-itenscontrato-for.log-control-event       = logical(oJsonObjectSec:GetCharacter("log-control-envent"))    NO-ERROR. 
                //IF oJsonObjectSec:Has("log-obrig-item")    THEN ttj-itenscontrato-for.log-obrig-item          = logical(oJsonObjectSec:GetCharacter("log-obrig-item"))        NO-ERROR.      
                //IF oJsonObjectSec:Has("preco-unit")        THEN ttj-itenscontrato-for.preco-unit              = DECIMAL(oJsonObjectSec:GetCharacter("preco-unit"))            NO-ERROR.           
                //IF oJsonObjectSec:Has("log-ind-multa")     THEN ttj-itenscontrato-for.log-ind-multa           = LOGICAL(oJsonObjectSec:GetCharacter("log-ind-multa"))         NO-ERROR.
                //IF oJsonObjectSec:Has("perc-multa-dia")    THEN ttj-itenscontrato-for.perc-multa-dia          = DECIMAL(oJsonObjectSec:GetCharacter("per-multa-dia"))         NO-ERROR.
                //IF oJsonObjectSec:Has("perc-multa-limite") THEN ttj-itenscontrato-for.perc-multa-limite       = DECIMAL(oJsonObjectSec:GetCharacter("log-obrig-item"))        NO-ERROR.
                //IF oJsonObjectSec:HAS("cod-depos")         THEN ttj-itenscontrato-for.cod-depos               = oJsonObjectSec:GetCharacter("cod-depos")                      NO-ERROR.
                //IF oJsonObjectSec:Has("aliquota-icm")      THEN ttj-itenscontrato-for.aliquota-icm            = DECIMAL(oJsonObjectSec:GetCharacter("aliquota-icm"))          NO-ERROR.                                        
                //IF oJsonObjectSec:Has("aliquota-ipi")      THEN ttj-itenscontrato-for.aliquota-ipi            = INT(oJsonObjectSec:GetCharacter("aliquota-ipi"))              NO-ERROR.
                //IF oJsonObjectSec:Has("tp-despesa ")       THEN ttj-itenscontrato-for.tp-despesa              = int(oJsonObjectSec:GetCharacter("tp-despesa"))                NO-ERROR.
                //IF oJsonObjectSec:Has("cod-cond-pag")      THEN ttj-itenscontrato-for.cod-cond-pag            = int(oJsonObjectSec:GetCharacter("cod-cond-pag"))              NO-ERROR.
                //IF oJsonObjectSec:Has("prazo-ent")         THEN ttj-itenscontrato-for.prazo-ent               = INT(oJsonObjectSec:GetCharacter("prazo-ent"))                 NO-ERROR.
                //IF oJsonObjectSec:Has("preco-base")        THEN ttj-itenscontrato-for.preco-base              = DECIMAL(oJsonObjectSec:GetCharacter("preco-base"))            NO-ERROR.
                //IF oJsonObjectSec:HAS("cod-comprado")      THEN ttj-itenscontrato-for.cod-comprado            = oJsonObjectSec:GetCharacter("cod-comprado")                   NO-ERROR.
                //IF oJsonObjectSec:Has("perc-desconto")     THEN ttj-itenscontrato-for.perc-desconto           = decimal(oJsonObjectSec:GetCharacter("perc-desconto"))         NO-ERROR.
                //IF oJsonObjectSec:Has("narrat-compra")     THEN ttj-itenscontrato-for.narrat-compra           = oJsonObjectSec:GetCharacter("narrat-compra")                  NO-ERROR.
                //IF oJsonObjectSec:Has("pre-unit-for")      THEN ttj-itenscontrato-for.pre-unit-for            = DECIMAL(oJsonObjectSec:GetCharacter("pre-unit-for"))          NO-ERROR.
                //IF oJsonObjectSec:Has("sld-qtd-receb")     THEN ttj-itenscontrato-for.sld-qtd-receb           = DECIMAL(oJsonObjectSec:GetCharacter("sld-qtd-receb"))         NO-ERROR.
                //IF oJsonObjectSec:Has("sld-val-receb")     THEN ttj-itenscontrato-for.sld-val-receb           = DECIMAL(oJsonObjectSec:GetCharacter("sld-val-receb"))         NO-ERROR.
            END.

 END PROCEDURE. 


PROCEDURE pi-criaTTJMatrizItemContrato.

    EMPTY TEMP-TABLE ttj-Matrizitenscontrato-for.  //Limpa a tabela antes de ser populada pelo Json

       oJsonArrayMat = oJsonObjectMain:GetJsonArray("MatrizItemContrato").
       CREATE  ttj-Matrizitenscontrato-for.

            DO iCountSec = 1 TO oJsonArrayMat:LENGTH:

                 oJsonObjectMat =  oJsonArrayMat:GetJsonObject(iCountSec). 

                 IF oJsonObjectMat:HAS("nr-contrato")     THEN ttj-Matrizitenscontrato-for.nr-contrato      = INT(oJsonObjectMat:GetCharacter("nr-contrato"))      NO-ERROR.
                 IF oJsonObjectMat:HAS("ct-codigo")       THEN ttj-Matrizitenscontrato-for.ct-codigo        = oJsonObjectMat:GetCharacter("ct-codigo")             NO-ERROR.
                 IF oJsonObjectMat:HAS("sc-codigo")       THEN ttj-Matrizitenscontrato-for.sc-codigo        = oJsonObjectMat:GetCharacter("sc-codigo")             NO-ERROR.
                 IF oJsonObjectMat:HAS("perc-rateio")     THEN ttj-Matrizitenscontrato-for.perc-rateio      = DECIMAL(oJsonObjectMat:GetCharacter("perc-rateio"))  NO-ERROR.
                 IF oJsonObjectMat:HAS("cod-unid-negoc")  THEN ttj-Matrizitenscontrato-for.cod-unid-negoc   = oJsonObjectMat:GetCharacter("cod-unid-negoc")        NO-ERROR.

                // IF oJsonObjectMat:HAS("num-seq-item")    THEN ttj-Matrizitenscontrato-for.num-seq-item     = INT(oJsonObjectMat:GetCharacter("num-seq-item"))     NO-ERROR.
                // IF oJsonObjectMat:HAS("it-codigo")       THEN ttj-Matrizitenscontrato-for.it-codigo        = oJsonObjectMat:GetCharacter("it-codigo")             NO-ERROR.

            END.

END PROCEDURE.
