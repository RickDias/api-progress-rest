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

CURRENT-LANGUAGE = CURRENT-LANGUAGE.

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
DEFINE VARIABLE lretOK           AS LOG        NO-UNDO.
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

 pJsonInput:writefile("c:\temp\jsoncontroato.json").
 
 /* ---- Là propriedade Principal ---- */        
oJsonArrayMain = pJsonInput:GetJsonArray("ContratoFornecedor":U).

//Preciso Criar a tabela de parametros para os dados/informaá‰es que n∆o consta no Ariba
//depois que cirar o 
FIND FIRST tt-es-api-param-contr NO-LOCK NO-ERROR.

  EMPTY TEMP-TABLE ttCapaContrato. //Limpa a tabela antes de ser populada pelo Json       
  EMPTY TEMP-TABLE tt-imp-contrato-for.
  EMPTY TEMP-TABLE tt-imp-item-contrato.


MESSAGE "Json: " STRING(cLongJson).


lretOK = DATASET httContratoCompra:READ-JSON("LONGCHAR", cLongJson, "empty") NO-ERROR.

IF lretOK = NO THEN DO:

    MESSAGE "CONTRATO NOK".
    
    CREATE tt-retorno-nok.
    ASSIGN tt-retorno-nok.data        = NOW
           tt-retorno-nok.cod-erro    = 0
           tt-retorno-nok.desc-erro   = "N∆o foi poss°vel fazer o parse do arquivo"
           tt-retorno-nok.sequencia   = 1.
    
END.
ELSE
    MESSAGE "CONTRATO OK".


 //Coltar aqui as Validaá‰es e Carregar as tamp-tables  
 CREATE ttCapaContrato.
 DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

      oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

      RUN pi-criaTTContrato.

END.

 RUN pi-processa.  //chama a a pi-processa do esint030a.i


PROCEDURE pi-criaTTContrato.

       ASSIGN i-num-contrato = 0. 

       FIND FIRST param-contrat NO-LOCK NO-ERROR. //cn0101 - Parametros do Contrato 

       FIND LAST contrato-for 
           WHERE contrato-for.nr-contrato <= param-contrat.num-seq-fim-central 
             AND contrato-for.nr-contrato >= param-contrat.num-seq-ini-central
       NO-LOCK NO-ERROR.

       IF AVAIL contrato-for THEN 
           ASSIGN i-num-contrato = contrato-for.nr-contrato + 1.  

       MESSAGE "####-CAPA DO CONTRATO - NUMERO: " STRING(i-num-contrato).

    FOR EACH  ttCapaContrato NO-LOCK:
       
           MESSAGE "ttCapaContrato.des-contrat       " ttCapaContrato.nr-contrato         .
           MESSAGE "ttCapaContrato.cod-emitente      " ttCapaContrato.cod-emitente        .
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
                  tt-imp-contrato-for.des-contrat     = ttCapaContrato.nr-contrato
                  tt-imp-contrato-for.cod-emitente    = int(trim(ttCapaContrato.cod-emitente))    //INT(TRIM(ttCapaContrato.cod-emitente)) - Verificar por que temptable n∆o informa o C¢digo
                  tt-imp-contrato-for.dt-ini-validade = TODAY
                  tt-imp-contrato-for.dt-ter-validade = DATE(ttCapaContrato.dt-ter-validade)
                  tt-imp-contrato-for.ind-tipo-movto  = 1
                  tt-imp-contrato-for.ind-sit-contrat = 1 /*NAO EMITIDO*/
                  tt-imp-contrato-for.cod-estabel     = (trim(ttCapaContrato.cod-estabel))
                  tt-imp-contrato-for.val-total       = decimal(trim(ttCapaContrato.val-total))
                  tt-imp-contrato-for.cod-cond-pag    = INT(trim(ttCapaContrato.cod-cond-pag))
               .

           RUN pi-criaTTJItemContrato.
               
    END.

END PROCEDURE.


 PROCEDURE pi-criaTTJItemContrato:

     //EMPTY TEMP-TABLE ttEstabelecimento.

     MESSAGE "#####-ITENS DO CONTRATO --".

     DEF VAR i-seq-item AS INTEGER.
     DEF VAR estab-aux  AS CHAR FORMAT "x(05)".
     DEF VAR estab-aux1 AS CHAR FORMAT "x(05)".

     i-seq-item = 0.           
     
     FOR EACH  ttItensContrato WHERE ttItensContrato.nr-contrato = ttCapaContrato.nr-contrato  NO-LOCK:
            
            MESSAGE "####-IMPRINDO DADOS DO ITEM DO CONTRATO".

            MESSAGE "ttItensContrato.nr-contrato   " string(ttItensContrato.nr-contrato )  .
            MESSAGE "ttItensContrato.cod-emitente  " string(ttItensContrato.cod-emitente)  .
            MESSAGE "ttItensContrato.preco-unit    " string(ttItensContrato.preco-unit  )  .
            MESSAGE "ttItensContrato.it-codigo     " string(ttItensContrato.it-codigo   )  .
            MESSAGE "ttItensContrato.narrat-item   " string(ttItensContrato.narrat-item )  .
            MESSAGE "ttItensContrato.preco-fornec  " string(ttItensContrato.preco-fornec)  .
            MESSAGE "ttItensContrato.val-frete     " string(ttItensContrato.val-frete   )  .
            MESSAGE "ttItensContrato.cod-estabel   " STRING(ttItensContrato.cod-estabel)   .
            MESSAGE "NUMERO DO CONTRATO            " STRING(i-num-contrato).
            
             i-seq-item = i-seq-item + 1.
             
             CREATE tt-imp-item-contrato.
             ASSIGN tt-imp-item-contrato.nr-contrato    = i-num-contrato
                    tt-imp-item-contrato.num-seq-item   = i-seq-item //iCountSec
                    tt-imp-item-contrato.cod-emitente   = INT(trim(ttItensContrato.cod-emitente)) 
                    tt-imp-item-contrato.preco-unit     = DEC(ttItensContrato.preco-unit)     
                    tt-imp-item-contrato.it-codigo      = ttItensContrato.it-codigo     
                    tt-imp-item-contrato.narrat-compra  = ttItensContrato.narrat-item   
                    tt-imp-item-contrato.pre-unit-for   = 0
                    tt-imp-item-contrato.preco-fornec   = DEC(ttItensContrato.preco-fornec)    
                    tt-imp-item-contrato.val-frete      = DEC(ttItensContrato.val-frete)
                    tt-imp-item-contrato.ind-tipo-movto = 1
                 .

           ASSIGN estab-aux = ttItensContrato.cod-estabel. 

           FIND FIRST ttEstabelecimento WHERE ttEstabelecimento.cod-estabel = estab-aux NO-LOCK NO-ERROR.

           IF NOT AVAIL ttEstabelecimento THEN DO:

               CREATE ttEstabelecimento.
               ASSIGN ttEstabelecimento.cod-estabel = ttItensContrato.cod-estabel.
               ASSIGN estab-aux = "". 

           END.               

              RUN pi-criaTTJMatrizItemContrato.

     END.
 END PROCEDURE. 


PROCEDURE pi-criaTTJMatrizItemContrato:

    FOR EACH ttMaTrizContrato WHERE ttMaTrizContrato.it-codigo = ttItensContrato.it-codigo NO-LOCK.

        FIND FIRST ITEM WHERE ITEM.it-codigo  = TRIM(ttMaTrizContrato.it-codigo)
                          AND ITEM.tipo-contr <> 4 NO-ERROR.

        IF AVAIL ITEM THEN DO:
                MESSAGE ">>>>###-PI-PROCESSA-GERANDO MATRIZ 1".

                     MESSAGE  "####-IMPRINDO DADOS DO MATRIZ DE RATEIO CONTRATO".
                     MESSAGE "ttMaTrizContrato.nr-contrato "    string(i-num-contrato)  .
                     MESSAGE "ttMaTrizContrato.ct-codigo "      string( ttMaTrizContrato.ct-codigo)  .
                     MESSAGE "ttMaTrizContrato.sc-codigo "      string(ttMaTrizContrato.sc-codigo).
                     MESSAGE "ttMaTrizContrato.perc-rateio "    STRING(ttMaTrizContrato.perc-rateio).
                     MESSAGE "ttMaTrizContrato.cod-unid-negoc " STRING( ttMaTrizContrato.cod-unid-negoc).

                     /*matriz-rat-contr*/
                     CREATE tt-imp-matriz-rat-contrato.
                     ASSIGN tt-imp-matriz-rat-contrato.nr-contrato         = INT(i-num-contrato)   //ttMaTrizContrato.nr-contrato
                            tt-imp-matriz-rat-contrato.ct-codigo           = string(ttMaTrizContrato.ct-codigo)
                            tt-imp-matriz-rat-contrato.sc-codigo           = ttMaTrizContrato.sc-codigo
                            tt-imp-matriz-rat-contrato.perc-rateio         = decimal(ttMaTrizContrato.perc-rateio)
                            tt-imp-matriz-rat-contrato.cod-unid-negoc      = string(ttMaTrizContrato.cod-unid-negoc)
                            tt-imp-matriz-rat-contrato.ind-tipo-movto      = 1
                          //  tt-imp-matriz-rat-contrato.it-codigo           = ttMaTrizContrato.it-codigo
                         .
                MESSAGE ">>>>###-PI-PROCESSA-GERANDO MATRIZ 1 PASSEI".
        END.
        ELSE DO: 

            DEF VAR c-contabil LIKE matriz-rat-item.conta-contabil NO-UNDO.

            ASSIGN c-contabil = string(ttMaTrizContrato.ct-codigo) + STRING(tt-imp-item-contrato.num-seq-item) + ttMaTrizContrato.sc-codigo + ttMaTrizContrato.sc-codigo.

                    //Verificar a matriz do item
                     CREATE tt-imp-matriz-rat-item.
                     ASSIGN tt-imp-matriz-rat-item.nr-contrato     = INT(i-num-contrato) 
                            tt-imp-matriz-rat-item.num-seq-item    = tt-imp-item-contrato.num-seq-item
                            tt-imp-matriz-rat-item.sc-codigo       = ttMaTrizContrato.sc-codigo
                            tt-imp-matriz-rat-item.ct-codigo       = string(ttMaTrizContrato.ct-codigo)
                            tt-imp-matriz-rat-item.perc-rateio     = decimal(ttMaTrizContrato.perc-rateio)
                            tt-imp-matriz-rat-item.it-codigo       = tt-imp-item-contrato.it-codigo
                            tt-imp-matriz-rat-item.dec-1           = 0
                            tt-imp-matriz-rat-item.conta-contabil  = c-contabil
                         .
        END.
    END.
END PROCEDURE.
