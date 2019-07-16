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
DEFINE VARIABLE iSeqItem         AS INTEGER    NO-UNDO.

/* ------- Definiá∆o de Temp-Tables e Datasets ------ */
//{esp\esint030.i}
{esp\esint030a.i}
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
oJsonArrayMain = pJsonInput:GetJsonArray("ContratoFornecedor":U).
//Preciso Criar a tabela de parametros para os dados/informaá‰es que n∆o consta no Ariba
//depois que cirar o 
FIND FIRST tt-es-api-param-contr NO-LOCK NO-ERROR.

  EMPTY TEMP-TABLE ttCapaContrato. //Limpa a tabela antes de ser populada pelo Json       
  EMPTY TEMP-TABLE tt-imp-contrato-for.                                                   

 //Coltar aqui as Validaá‰es e Carregar as tamp-tables  
 CREATE ttCapaContrato.
 
 DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

      oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

      RUN pi-criaTTContrato.

      RUN pi-criaTTJItemContrato.

     // RUN pi-criaTTJMatrizItemContrato.

      /* -- Validaá‰es _____*/
      // RUN pi-valida-dados.   necess†ri criar as validaá‰es. 

 END.

 RUN pi-processa.  //chama a a pi-processa do esint030a.i


 PROCEDURE pi-criaTTContrato.
                                                                                                                              
     IF oJsonObjectMain:Has("nr-contrato")              THEN ttCapaContrato.des-contrat          = oJsonObjectMain:GetCharacter("nr-contrato")      NO-ERROR.
     IF	oJsonObjectMain:Has("cod-emitente ")            THEN ttCapaContrato.cod-emitente         = oJsonObjectMain:GetCharacter("cod-emitentte")    NO-ERROR.
     IF	oJsonObjectMain:Has("dt-ini-validade")          THEN ttCapaContrato.dt-ini-validade      = oJsonObjectMain:GetCharacter("dt-ini-validade")  NO-ERROR.
     IF	oJsonObjectMain:Has("dt-ter-validade")          THEN ttCapaContrato.dt-ter-validade      = oJsonObjectMain:GetCharacter("dt-ter-validade")  NO-ERROR.
     IF	oJsonObjectMain:Has("cod-comprado")          	THEN ttCapaContrato.cod-comprado         = oJsonObjectMain:GetCharacter("cod-comprado")     NO-ERROR.
     IF	oJsonObjectMain:Has("cod-cond-pag")       	    THEN ttCapaContrato.cod-cond-pag         = oJsonObjectMain:GetCharacter("cod-cond-pag")     NO-ERROR.
     IF	oJsonObjectMain:Has("cod-estabel")             	THEN ttCapaContrato.cod-estabel          = oJsonObjectMain:GetCharacter("cod-estabel")      NO-ERROR.
     if	oJsonObjectMain:Has("val-total")               	THEN ttCapaContrato.val-total            = oJsonObjectMain:GetCharacter("val-total")        NO-ERROR.
     if	oJsonObjectMain:Has("val-fatur-minimo")     	THEN ttCapaContrato.val-fatur-minimo     = oJsonObjectMain:GetCharacter("val-fatur-minimo") NO-ERROR.
     If	oJsonObjectMain:Has("acum-val-pago")           	THEN ttCapaContrato.acum-val-pago        = oJsonObjectMain:GetCharacter("acum-val-pago")    NO-ERROR.
     IF	oJsonObjectMain:Has("mo-codigo")            	THEN ttCapaContrato.mo-codigo            = oJsonObjectMain:GetCharacter("mo-codigo")        NO-ERROR.

       ASSIGN i-num-contrato = 0.
       
       FIND FIRST param-contrat NO-LOCK NO-ERROR.

       FIND LAST contrato-for 
           WHERE contrato-for.nr-contrato <= param-contrat.num-seq-fim-central 
             AND contrato-for.nr-contrato >= param-contrat.num-seq-ini-central
       NO-LOCK NO-ERROR.
       IF AVAIL contrato-for THEN 
           ASSIGN i-num-contrato = contrato-for.nr-contrato + 1.  

       MESSAGE "####-CAPA DO CONTRATO - NUMERO: " STRING(i-num-contrato).

       FIND FIRST ttCapaContrato NO-LOCK NO-ERROR.
       IF AVAIL ttCapaContrato THEN
       DO:
           MESSAGE "ttCapaContrato.des-contrat       " ttCapaContrato.des-contrat         .
           MESSAGE "ttCapaContrato.cod-emitente      (" ttCapaContrato.cod-emitente ")"   .
           MESSAGE "ttCapaContrato.dt-ini-validade   " ttCapaContrato.dt-ini-validade     .
           MESSAGE "ttCapaContrato.dt-ter-validade   " ttCapaContrato.dt-ter-validade     .
           MESSAGE "ttCapaContrato.cod-comprado      " ttCapaContrato.cod-comprado        .
           MESSAGE "ttCapaContrato.cod-cond-pag      " ttCapaContrato.cod-cond-pag        .
           MESSAGE "ttCapaContrato.cod-estabel       " ttCapaContrato.cod-estabel         .
           MESSAGE "ttCapaContrato.val-total         " ttCapaContrato.val-total           .
           MESSAGE "ttCapaContrato.val-fatur-minimo  " ttCapaContrato.val-fatur-minimo    .
           MESSAGE "ttCapaContrato.acum-val-pago     " ttCapaContrato.acum-val-pago       .
           MESSAGE "ttCapaContrato.mo-codigo         " ttCapaContrato.mo-codigo           .


           CREATE tt-imp-contrato-for.
           ASSIGN tt-imp-contrato-for.nr-contrato     = i-num-contrato
                  tt-imp-contrato-for.des-contrat     = ttCapaContrato.des-contrat
                  tt-imp-contrato-for.cod-emitente    = INT(TRIM(ttCapaContrato.cod-emitente))
                  tt-imp-contrato-for.dt-ini-validade = TODAY
                  tt-imp-contrato-for.dt-ter-validade = DATE(ttCapaContrato.dt-ter-validade)
                  tt-imp-contrato-for.ind-tipo-movto  = 1
                  tt-imp-contrato-for.ind-sit-contrat = 1 /*NAO EMITIDO*/
               .

           /*------------------------
            validar com igor da summit os sequinte campos
            cod-emitente    - campo emitente esta vindo em branco
            dt-ini-validade - campos datas est∆o vindo errados
            dt-ter-validade - campos datas est∆o vindo errados
           
           */
       END.
         
      

      /** Removido pq est† sendo chamado em duplicidade**/
     //IF oJsonObjectMain:Has("ItensContrato") THEN DO:
     //    RUN pi-criaTTJItemContrato.
     //END.


     //Comentei a Criaá∆o da Matriz de Rateio

/*      IF oJsonObjectMain:Has("MatrizItemContrato") THEN DO: */
/*          RUN pi-criaTTJMatrizItemContrato.                 */
/*      END.                                                  */
        
 END PROCEDURE.


 PROCEDURE pi-criaTTJItemContrato:

     MESSAGE "#####-ITENS DO CONTRATO --".
     
     IF oJsonObjectMain:Has("ItensContrato") THEN DO:

         oJsonArraySec = oJsonObjectMain:GetJsonArray("ItensContrato").
       
         DO iCountSec = 1 TO oJsonArraySec:LENGTH: 
    
             CREATE  ttItensContrato.
    
             oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec). 
             //ASSIGN fld-rel = iCountSec.
    
             IF oJsonObjectSec:Has("nr-contrato")       THEN ttItensContrato.nr-contrato             = oJsonObjectSec:GetCharacter("nr-contrato")    NO-ERROR.   
             IF oJsonObjectSec:Has("cod-emitente ")     THEN ttItensContrato.cod-emitente            = oJsonObjectSec:GetCharacter("cod-emitentte")  NO-ERROR.   
             IF oJsonObjectSec:Has("preco-unit")        THEN ttItensContrato.preco-unit              = oJsonObjectSec:GetCharacter("preco-unit")     NO-ERROR.   
             IF oJsonObjectSec:Has("it-codigo")         THEN ttItensContrato.it-codigo               = oJsonObjectSec:GetCharacter("it-codigo")      NO-ERROR.   
             IF oJsonObjectSec:Has("narrat-item")       THEN ttItensContrato.narrat-item             = oJsonObjectSec:GetCharacter("narrat-item")    NO-ERROR.   
             IF oJsonObjectSec:Has("preco-fornec")      THEN ttItensContrato.preco-fornec            = oJsonObjectSec:GetCharacter("preco-fornec")   NO-ERROR.   
             IF oJsonObjectSec:Has("val-frete")         THEN ttItensContrato.val-frete               = oJsonObjectSec:GetCharacter("val-frete")      NO-ERROR.
    
             IF AVAIL ttItensContrato THEN
             DO:
                 MESSAGE "####-IMPRINDO DADOS DO ITEM DO CONTRATO".
                 MESSAGE "ttItensContrato.nr-contrato   " string(ttItensContrato.nr-contrato )  .
                 MESSAGE "ttItensContrato.cod-emitente  " string(ttItensContrato.cod-emitente)  .
                 MESSAGE "ttItensContrato.preco-unit    " string(ttItensContrato.preco-unit  )  .
                 MESSAGE "ttItensContrato.it-codigo     " string(ttItensContrato.it-codigo   )  .
                 MESSAGE "ttItensContrato.narrat-item   " string(ttItensContrato.narrat-item )  .
                 MESSAGE "ttItensContrato.preco-fornec  " string(ttItensContrato.preco-fornec)  .
                 MESSAGE "ttItensContrato.val-frete     " string(ttItensContrato.val-frete   )  .
                 MESSAGE "NUMERO DO CONTRATO            " STRING(i-num-contrato).
             
             END.
             
             
             
             CREATE tt-imp-item-contrato.
             ASSIGN tt-imp-item-contrato.nr-contrato    = i-num-contrato
                    tt-imp-item-contrato.num-seq-item   = iCountSec
                    tt-imp-item-contrato.cod-emitente   = INT(ttItensContrato.cod-emitente) 
                    tt-imp-item-contrato.preco-unit     = DEC(ttItensContrato.preco-unit)     
                    //codigo do item est† vindo em branco
                    tt-imp-item-contrato.it-codigo      = ttItensContrato.it-codigo     
                    tt-imp-item-contrato.narrat-compra  = ttItensContrato.narrat-item   
                    tt-imp-item-contrato.pre-unit-for   = 0
                    tt-imp-item-contrato.preco-fornec   = DEC(ttItensContrato.preco-fornec)    
                    tt-imp-item-contrato.val-frete      = DEC(ttItensContrato.val-frete).

             /*
                Precisa tratar os campos de valor com o cara do apigee
                preco-fornec
                val-frete
                preco-unit
                
                Est† vindo em branco
                it-codigo
             
             */
                    


    
        END.
     END.

 END PROCEDURE. 


PROCEDURE pi-criaTTJMatrizItemContrato:

    EMPTY TEMP-TABLE ttj-Matrizitenscontrato-for.  //Limpa a tabela antes de ser populada pelo Json

    IF oJsonObjectMain:Has("MatrizItemContrato") THEN
    DO:
        oJsonArrayMat = oJsonObjectMain:GetJsonArray("MatrizItemContrato").
        CREATE  ttj-Matrizitenscontrato-for.

        DO iCountSec = 1 TO oJsonArrayMat:LENGTH:

             oJsonObjectMat =  oJsonArrayMat:GetJsonObject(iCountSec). 

             IF oJsonObjectMat:HAS("nr-contrato")     THEN ttj-Matrizitenscontrato-for.nr-contrato      = INT(oJsonObjectMat:GetCharacter("nr-contrato"))      NO-ERROR.
             IF oJsonObjectMat:HAS("ct-codigo")       THEN ttj-Matrizitenscontrato-for.ct-codigo        = oJsonObjectMat:GetCharacter("ct-codigo")             NO-ERROR.
             IF oJsonObjectMat:HAS("sc-codigo")       THEN ttj-Matrizitenscontrato-for.sc-codigo        = oJsonObjectMat:GetCharacter("sc-codigo")             NO-ERROR.
             IF oJsonObjectMat:HAS("perc-rateio")     THEN ttj-Matrizitenscontrato-for.perc-rateio      = DECIMAL(oJsonObjectMat:GetCharacter("perc-rateio"))  NO-ERROR.
             IF oJsonObjectMat:HAS("cod-unid-negoc")  THEN ttj-Matrizitenscontrato-for.cod-unid-negoc   = oJsonObjectMat:GetCharacter("cod-unid-negoc")        NO-ERROR.

             /*
             cria a tabela de rateio do item do contrato
             
             */
            

        END.
    END.


END PROCEDURE.

