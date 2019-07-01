/*----------------------------------------------------------------------------------------------/
 Programa..: esint001ii.p
 Objetivo..: Interface Integraá∆o Clientes SFA - Importaá∆o
 Data......: 26/02/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

/* ------- Importaá∆o de Classes ------ */
using Progress.Json.OBJECTModel.*.

/* ------- Definiá∆o de ParÉmetros ----- */
DEFINE INPUT  PARAMETER r-table     AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pJsonInput  AS JsonObject NO-UNDO.

/* ------- Definiá∆o de Vari†veis ------ */
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray  NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER    NO-UNDO.
DEFINE VARIABLE h-dt-calc        AS HANDLE     NO-UNDO.
DEFINE VARIABLE d-dt-entrega     AS DATE       NO-UNDO.
DEFINE VARIABLE d-dt-fatura      AS DATE       NO-UNDO.               

/* ------- Definiá∆o de Temp-Tables e Datasets ------ */
{esp\esint001i.i}

{method/dbotterr.i}

RUN esp/esgp0188ca.p  PERSISTENT SET h-dt-calc.

MESSAGE "**** esint001ii Inicio".

/*------------------------------ Main Begin ----------------------------*/
ASSIGN c-erro = "".

FIND FIRST sfa-import NO-LOCK WHERE ROWID(sfa-import) = r-table NO-ERROR.
IF AVAIL sfa-import THEN DO:
    MESSAGE "**** esint001ii 1".

    /* ------- Grava clob para longchar ----- */
    FIND FIRST sfa-import-lead OF sfa-import NO-ERROR.
    MESSAGE "**** esint001ii 1" AVAIL sfa-import-lead.
    IF AVAIL sfa-import-lead THEN DO:
        
        COPY-LOB sfa-import-lead.c-json TO cLongJson.

        MESSAGE "**** esint001ii 2".

        /* ---- Là propriedade Principal ---- */        
        oJsonArrayMain = pJsonInput:GetJsonObject("payload":U)
                                   :GetJsonArray("req":U).     
        MESSAGE "**** esint001ii 3" AVAIL sfa-import-lead.

        CREATE tt_leadtime.

        DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

            oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).
            
            IF oJsonObjectMain:Has("CodEstabel")   then tt_leadtime.cod-estabel = oJsonObjectMain:GetCharacter("CodEstabel"). 
            if oJsonObjectMain:Has("Estado")       then tt_leadtime.uf          = oJsonObjectMain:GetCharacter("Estado")    .
            if oJsonObjectMain:Has("Cidade")       then tt_leadtime.cidade      = oJsonObjectMain:GetCharacter("Cidade")    . 
            if oJsonObjectMain:Has("CEP")          then tt_leadtime.cep         = oJsonObjectMain:GetCharacter("CEP")       . 
            if oJsonObjectMain:Has("Peso")         then tt_leadtime.peso        = oJsonObjectMain:GetDecimal  ("Peso")      . 
            if oJsonObjectMain:Has("NomeAbrev")    then tt_leadtime.nome-abrev  = oJsonObjectMain:GetCharacter("NomeAbrev") .                       

        END.
        
        MESSAGE "**** esint001ii " TEMP-TABLE tt_leadtime:HAS-RECORDS.

        IF NOT TEMP-TABLE tt_leadtime:HAS-RECORDS THEN c-erro = c-erro + "N∆o h† registros para processar". 
        ELSE DO:
           RUN piGeraLeadTime (OUTPUT d-dt-entrega,
                               OUTPUT d-dt-fatura,
                               OUTPUT c-erro).

           IF c-erro = "" THEN DO:

               IF d-dt-entrega = ? THEN
                   ASSIGN c-erro = c-erro + "Data de entrega n∆o gerada".
               ELSE ASSIGN sfa-import-lead.dt-entrega = d-dt-entrega.

               IF d-dt-fatura = ? THEN
                   ASSIGN c-erro = c-erro + "Data de faturamento n∆o gerada".
               ELSE ASSIGN sfa-import-lead.dt-fatura = d-dt-fatura.
              
               ASSIGN sfa-import.chave = STRING(tt_leadtime.nome-abrev)
                      sfa-import-lead.nome-abrev = STRING(tt_leadtime.nome-abrev)
                      c-erro           = "".
           END.
        END.            
    END.
    MESSAGE "**** esint001ii 99".
END.
MESSAGE "**** esint001ii termino".


IF VALID-HANDLE (h-dt-calc) THEN
    DELETE OBJECT h-dt-calc NO-ERROR.



/*--------------------------------------------------------------------*/

PROCEDURE piGeraLeadTime:

    DEFINE OUTPUT PARAM pDtEntrega AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAM pDtFatura  AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAM perro      AS CHARACTER NO-UNDO.

    RUN piDtEntrega IN h-dt-calc (INPUT tt_leadtime.cod-estabel,
                                  INPUT tt_leadtime.uf,
                                  INPUT tt_leadtime.cidade,
                                  INPUT tt_leadtime.cep,
                                  INPUT tt_leadtime.peso,
                                  INPUT tt_leadtime.nome-abrev,
                                  INPUT TODAY,
                                  OUTPUT pDtEntrega,
                                  OUTPUT pDtFatura,
                                  OUTPUT perro).
END.
