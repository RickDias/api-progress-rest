/*******************************************************************************
**   Programa.: Escm0103RP
**   Data.....: 16/05/2019
**   Objetivo.: Gera consulta de Cr‚dito
**   Autor....: Thiago Primo - Private.
*******************************************************************************/
/* include para controle de versao */

{include/i-prgvrs.i escm0103rp 2.06.00.000}

{esp/esint001rp.i}
/* definicao das temp-table para recebimento de parametros */

DEFINE TEMP-TABLE tt-param NO-UNDO
    field destino          as INTEGER 
    field arquivo          as CHAR FORMAT  "x(35)"
    field usuario          as CHAR FORMAT  "x(12)"
    field data-exec        as DATE 
    field hora-exec        as INTEGER 
    field classifica       as INTEGER 
    field desc-classifica  as CHAR FORMAT  "x(40)"
    FIELD l-csv            AS LOG INIT NO
    FIELD l-reprocessa     AS LOG INIT NO
    FIELD l-aval-credito   AS LOG INIT NO
    FIELD l-lead-time      AS LOG INIT NO.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.
   
DEFINE TEMP-TABLE tt-integr-credito
    FIELD cod-emitente    LIKE emitente.cod-emitente
    FIELD nome-abrev      LIKE emitente.nome-abrev
    FIELD cgc             LIKE emitente.cgc
    FIELD val-sdo-credito as DECIMAL FORMAT  "->>>>,>>>,>>9.99" 
    FIELD val-divida      as DECIMAL FORMAT  "->>>>,>>>,>>9.99" 
    FIELD dt-div-mlonga   as DATE 
    FIELD dt-atualizacao  as DATE  
    INDEX i1 IS PRIMARY cod-emitente.

   
/* recebimento de parametros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/*******************************Defini‡Æo de Variaveis */
DEFINE VARIABLE i-cont          AS INT    NO-UNDO.
DEFINE VARIABLE h-esint017      AS HANDLE NO-UNDO.
DEFINE VARIABLE c-caminho       AS CHAR   NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE NO-UNDO.


/* include padrao para variaveis de relatorio */
{include/i-rpvar.i}

/* definicao de variaveis */

/* include padrao para output de relatorio */
{include/i-rpout.i }

/* include com a definicao da frame de cabecalho e rodape **/
{include/i-rpcab.i }

/* bloco principal do programa */
find first param-global no-lock.
find first param-estoq  no-lock.

assign c-programa     = "escm0103"
       c-versao       = "2.06"
       c-revisao      = " .00.000"
       c-empresa      = param-global.grupo
       c-titulo-relat = "Relat¢rio de P.C.P.".

view FRAME f-cabec.
view frame f-rodape.



FIND FIRST tt-param.
EMPTY TEMP-TABLE tt-integr-credito.


/*------ Processa Cr‚dito ----- */
IF tt-param.l-aval-credito THEN DO:

    RUN esp/esint017.p PERSISTENT SET h-esint017.
    IF NOT VALID-HANDLE(h-esint017) THEN
        RETURN "NOK".
    
    FOR EACH emitente
        WHERE emitente.identific <> 2 
          AND emitente.natureza   = 2 
          NO-LOCK BY emitente.cod-emitente :

        /*------retirar -----*/
        IF emitente.ind-cre-cli > 2 THEN NEXT.

        ASSIGN i-cont = i-cont + 1.

        RUN pi-processa-cred IN h-esint017 (INPUT emitente.cod-emitente,
                                            INPUT-OUTPUT TABLE tt-integr-credito).

       IF i-cont = 2000 THEN LEAVE.
    
    END.

    DELETE OBJECT h-esint017.

END.

/* ------- Processa Lead-time ----- */
IF tt-param.l-lead-time THEN
    RUN pi-processa-lead.


IF tt-param.l-csv THEN DO:

    ASSIGN c-caminho = SESSION:TEMP-DIRECTORY + 
                   "LOG_AVALIACAO_CREDITO" + "_" +
                   STRING(DAY(TODAY))      + "_" +
                   STRING(MONTH(TODAY))    + "_" + 
                   STRING(TIME) + ".csv".

    OUTPUT TO VALUE(c-caminho) NO-CONVERT.
    PUT UNFORMATTED
        "Cliente ; Nome Abrev ; Cgc ; Val. saldo cr‚dito ; Val. d¡vida ; Dt. div. Mais longa; Dt. Atualizacao" SKIP. 
END.

RUN pi-cria-avaliacao.
RUN pi-cria-export.

IF tt-param.l-csv THEN
    OUTPUT CLOSE.

{include/i-rpclo.i}



PROCEDURE pi-cria-avaliacao.
    
    run utp/ut-acomp.p persistent set h-acomp.
    {utp/ut-liter.i Imprimindo *}
    run pi-inicializar in h-acomp (input return-value).

    FOR EACH tt-integr-credito:

        RUN pi-acompanhar IN h-acomp (INPUT "Criando: " + STRING(tt-integr-credito.cod-emitente)).

        FIND FIRST sfa-aval-credito WHERE sfa-aval-credito.cod-emitente = tt-integr-credito.cod-emitente EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL sfa-aval-credito THEN DO:

            IF NOT tt-param.l-reprocessa THEN DO:
                IF  (sfa-aval-credito.val-sdo-credito  = tt-integr-credito.val-sdo-credito
                     OR sfa-aval-credito.val-divida       = tt-integr-credito.val-divida     
                     OR sfa-aval-credito.dt-div-mlonga    = tt-integr-credito.dt-div-mlonga)  THEN DO:
                    ASSIGN sfa-aval-credito.val-sdo-credito  = tt-integr-credito.val-sdo-credito
                           sfa-aval-credito.val-divida       = tt-integr-credito.val-divida     
                           sfa-aval-credito.dt-div-mlonga    = tt-integr-credito.dt-div-mlonga  
                           sfa-aval-credito.dt-atualizacao   = tt-integr-credito.dt-atualizacao .
                END.
            END.
            ELSE DO:
                ASSIGN sfa-aval-credito.val-sdo-credito  = tt-integr-credito.val-sdo-credito
                       sfa-aval-credito.val-divida       = tt-integr-credito.val-divida     
                       sfa-aval-credito.dt-div-mlonga    = tt-integr-credito.dt-div-mlonga  
                       sfa-aval-credito.dt-atualizacao   = tt-integr-credito.dt-atualizacao.
            END.
        END.
        ELSE DO:
            CREATE sfa-aval-credito.
            ASSIGN sfa-aval-credito.cod-emitente     = tt-integr-credito.cod-emitente   
                   sfa-aval-credito.nome-abrev       = tt-integr-credito.nome-abrev     
                   sfa-aval-credito.cgc              = tt-integr-credito.cgc            
                   sfa-aval-credito.val-sdo-credito  = tt-integr-credito.val-sdo-credito
                   sfa-aval-credito.val-divida       = tt-integr-credito.val-divida     
                   sfa-aval-credito.dt-div-mlonga    = tt-integr-credito.dt-div-mlonga  
                   sfa-aval-credito.dt-atualizacao   = tt-integr-credito.dt-atualizacao .
        END.

        IF tt-param.l-csv THEN DO:
            PUT UNFORMATTED
                tt-integr-credito.cod-emitente     ";"
                tt-integr-credito.nome-abrev       ";"
                tt-integr-credito.cgc              ";"
                tt-integr-credito.val-sdo-credito  ";"
                tt-integr-credito.val-divida       ";"
                tt-integr-credito.dt-div-mlonga    ";"
                tt-integr-credito.dt-atualizacao SKIP.
        END.
    END.

    RUN pi-finalizar IN h-acomp.

END PROCEDURE.

PROCEDURE pi-cria-export.


    FIND FIRST es-api-param NO-LOCK 
        WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
          AND es-api-param.cd-tipo-integr = 11 /*---- Integra‡Æo Cr‚dito Cliente ------*/ NO-ERROR.
        
    IF AVAIL es-api-param THEN DO:
        IF NOT CAN-FIND(FIRST sfa-export 
                        WHERE sfa-export.chave = string(today,"99/99/9999")
                          AND sfa-export.ind-situacao < 2) THEN DO:
            
            CREATE sfa-export-cred.
            ASSIGN sfa-export-cred.cd-tipo-integr = es-api-param.cd-tipo-integr
                   sfa-export-cred.id-movto       = NEXT-VALUE(seq-export)     
                   sfa-export-cred.cgc            = ?                          
                   sfa-export-cred.data-movto     = NOW                        
                   sfa-export-cred.c-json         = ?.

            CREATE sfa-export.
            ASSIGN sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                   sfa-export.id-movto       = sfa-export-cred.id-movto
                   sfa-export.cd-tipo-integr = sfa-export-cred.cd-tipo-integr
                   sfa-export.chave          = string(today,"99/99/9999")
                   sfa-export.cod-status     = 0      /* ---- sem status ----*/
                   sfa-export.data-fim       = ?
                   sfa-export.data-inicio    = ?
                   sfa-export.data-movto     = NOW
                   sfa-export.ind-situacao   = 1       /*---- Pendente -----*/.
                
            RUN pi-processa (INPUT 2, INPUT 11). 
        END.     
    END.
    

END PROCEDURE.

PROCEDURE pi-processa-lead:

    
    DEFINE BUFFER b-geo-parametro_data_entrega FOR geo-parametro_data_entrega.
    DEFINE BUFFER b-es-gp-lead-time            FOR es-gp-lead-time.
    
    DEFINE VARIABLE h-calc      AS HANDLE NO-UNDO.
    DEFINE VARIABLE i-lead-time AS INT    NO-UNDO.
    DEFINE VARIABLE i-cont      AS INT    NO-UNDO.
    DEFINE VARIABLE i-seq-param AS INT    NO-UNDO.
    DEFINE VARIABLE c-erro      AS CHAR   NO-UNDO.

    IF NOT tt-param.l-lead-time THEN
        RETURN.

     run utp/ut-acomp.p persistent set h-acomp.
    {utp/ut-liter.i Imprimindo *}
    run pi-inicializar in h-acomp (input return-value).

    RUN esp/esgp0188ca.p PERSISTENT SET h-calc.
    
    FOR EACH es-gp-lead-time NO-LOCK WHERE ( es-gp-lead-time.cod-estabel-orig = '01'
                                        OR   es-gp-lead-time.cod-estabel-orig = '04'
                                        OR   es-gp-lead-time.cod-estabel-orig = '28'
                                        OR   es-gp-lead-time.cod-estabel-orig = '38') :

        ASSIGN i-cont = i-cont + 1.

        RUN pi-acompanhar IN h-acomp (INPUT STRING(i-cont)).

        ASSIGN i-lead-time = 0.
        RUN piDtEntregaSFA IN h-calc (BUFFER es-gp-lead-time,
                                      OUTPUT i-lead-time,
                                      OUTPUT c-erro).

        FIND FIRST sfa-lead-time EXCLUSIVE-LOCK WHERE sfa-lead-time.cod-estabel-orig  = es-gp-lead-time.cod-estabel-orig
                                                  AND sfa-lead-time.estado-dest       = es-gp-lead-time.estado-dest     
                                                  AND sfa-lead-time.cidade-dest       = es-gp-lead-time.cidade-dest     
                                                  AND sfa-lead-time.cep-ini           = es-gp-lead-time.cep-ini         
                                                  AND sfa-lead-time.cep-fim           = es-gp-lead-time.cep-fim         
                                                  AND sfa-lead-time.peso-ini          = es-gp-lead-time.peso-ini        
                                                  AND sfa-lead-time.peso-fim          = es-gp-lead-time.peso-fim NO-ERROR.
        IF NOT AVAIL sfa-lead-time THEN DO:
            CREATE sfa-lead-time.
            BUFFER-COPY es-gp-lead-time TO sfa-lead-time. 
            ASSIGN sfa-lead-time.lead-time    = i-lead-time
                   sfa-lead-time.data-atualiz = TODAY.
        END.
        ELSE DO:
            IF sfa-lead-time.lead-time <> i-lead-time THEN
                ASSIGN sfa-lead-time.lead-time    = i-lead-time
                       sfa-lead-time.data-atualiz = TODAY.
        END.

        /*IF i-cont > 1050 THEN LEAVE.*/

    END.

    /* Gera registro para integra‡Æo */
    FIND FIRST es-api-param NO-LOCK 
        WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
          AND es-api-param.cd-tipo-integr = 15 /*---- Integra‡Æo Cr‚dito Cliente ------*/ NO-ERROR.
        
    IF AVAIL es-api-param THEN DO:
        IF NOT CAN-FIND(FIRST sfa-export WHERE sfa-export.chave = string(today,"99/99/9999")
                                           AND sfa-export.ind-situacao < 2) THEN DO:

            CREATE sfa-export-lead.
            ASSIGN sfa-export-lead.cd-tipo-integr = es-api-param.cd-tipo-integr
                   sfa-export-lead.id-movto       = NEXT-VALUE(seq-export)     
                   sfa-export-lead.data           = TODAY
                   sfa-export-lead.data-movto     = NOW                        
                   sfa-export-lead.c-json         = ?.

            CREATE sfa-export.
            ASSIGN sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                   sfa-export.id-movto       = sfa-export-lead.id-movto
                   sfa-export.cd-tipo-integr = sfa-export-lead.cd-tipo-integr
                   sfa-export.chave          = STRING(sfa-export-lead.data,"99/99/9999")
                   sfa-export.cod-status     = 0      /* ---- sem status ----*/
                   sfa-export.data-fim       = ?
                   sfa-export.data-inicio    = ?
                   sfa-export.data-movto     = NOW
                   sfa-export.ind-situacao   = 1       /*---- Pendente -----*/.
                
            RUN pi-processa (INPUT 2, INPUT 15). 
        END.     
    END.
    RUN pi-finalizar IN h-acomp.
END PROCEDURE.
