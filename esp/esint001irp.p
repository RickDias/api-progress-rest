/*----------------------------------------------------------------------------------------------/
 Programa..: esint001irp.p
 Objetivo..: API Integra‡äes JSON
 Data......: 26/02/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


{include/i-prgvrs.i esint001irp 1.00.00.000} 
    
/* ---------- Defini‡Æo de Parƒmetros ---------- */
DEFINE INPUT-output PARAMETER p_transacao   AS INTEGER   NO-UNDO.
DEFINE INPUT-output PARAMETER p_cod_sistema AS INTEGER   NO-UNDO.
DEFINE INPUT-output PARAMETER p_cod_agente  AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p_nome_app    AS CHARACTER NO-UNDO.


/* ---------- Defini‡Æo de Vari veis ---------- */
DEFINE VARIABLE l_log        AS LOGICAL NO-UNDO.
DEFINE VARIABLE i_time       AS INTEGER NO-UNDO.
DEFINE VARIABLE l_proc       AS LOGICAL NO-UNDO.

/* ----Define qual tabela executar ----*/
&GLOBAL-DEFINE transacao IMPORT

FUNCTION fncFinaliza RETURN LOGICAL
    (p-ind-tipo-trans AS INTEGER,
     p-cod-sistema    AS INTEGER) FORWARD.


/* ---------- Main Block Begin ---------- */

ASSIGN i_time = 0.

REPEAT:

    PROCESS EVENTS.          

    ASSIGN l_proc = NO.

    /* ---- Finaliza processo ---*/
    IF fncFinaliza(p_transacao, p_cod_sistema) THEN LEAVE.

    RUN pi-carrega-pend NO-ERROR.    
    RUN pi-processa NO-ERROR.

    /* ------ Aumenta tempo caso nÆo tenha registros -----*/   
    IF NOT l_proc THEN DO:
        IF i_time < 60 THEN
            ASSIGN i_time = i_time + 5.
    END.
    ELSE ASSIGN i_time = 0.
    
    PAUSE i_time.   
END.

RETURN "OK".

/* ---------- Main Block END ---------- */


/*---------------------------------------- Functions -------------------------------------*/

FUNCTION fncAgendaAtiva RETURN LOGICAL
    ( p-es-api-param AS INTEGER ) :

    FIND FIRST es-api-schedule WHERE es-api-schedule.cd-tipo-integr = p-es-api-param
                                 AND es-api-schedule.ativo 
                                 AND es-api-schedule.data-inicio <= TODAY
                                 AND es-api-schedule.data-fim    >= TODAY NO-LOCK NO-ERROR.
    IF AVAIL es-api-schedule THEN DO:

        IF es-api-schedule.hora-inicio <= TIME AND 
            es-api-schedule.hora-fim >= TIME THEN RETURN NO.
        ELSE RETURN YES.
    END.
    ELSE RETURN NO.   
    
END FUNCTION.

FUNCTION fncFinaliza RETURN LOGICAL
    (p-ind-tipo-trans AS INTEGER,
     p-cod-sistema    AS INTEGER):

    IF NOT CAN-FIND(FIRST es-api-exec WHERE es-api-exec.ind-tipo-trans = p-ind-tipo-trans
                                        AND es-api-exec.cd-sistema     = p-cod-sistema
                                        AND es-api-exec.data-fim       = ? ) THEN RETURN YES.
    ELSE RETURN NO.

END FUNCTION.

PROCEDURE pi-carrega-pend:

    FOR EACH es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = p_transacao
                                    AND es-api-param.cd-sistema     = p_cod_sistema
                                    AND es-api-param.ativo:

        /* ------ Verifica se existe agENDa de integra‡Æo v lida ----*/
        IF fncAgendaAtiva(es-api-param.cd-tipo-integr) THEN NEXT.

        FOR EACH es-api-{&transacao} OF es-api-param WHERE es-api-{&transacao}.ind-situacao = 0
                                                    AND es-api-{&transacao}.cd-agente    = 0 
                                                    AND es-api-{&transacao}.nm-appserv   = "" NO-LOCK:

            FIND CURRENT es-api-{&transacao} EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN es-api-{&transacao}.ind-situacao = 1
                   es-api-{&transacao}.cd-agente    = p_cod_agente
                   es-api-{&transacao}.nm-appserv   = p_nome_app .
        END.        
    END.

END PROCEDURE.



PROCEDURE pi-processa:

    DEF VAR c-erro AS CHARACTER NO-UNDO.

    FOR EACH es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = p_transacao
                                   AND es-api-param.cd-sistema      = p_cod_sistema
                                   AND es-api-param.ativo:

        /* ------ Verifica se existe agENDa de integra‡Æo v lida ----*/
        IF fncAgendaAtiva(es-api-param.cd-tipo-integr) THEN NEXT.

        IF SEARCH(es-api-param.programa-integr ) = ? THEN NEXT.

        FOR EACH es-api-{&transacao} OF es-api-param WHERE es-api-{&transacao}.ind-situacao = 1 
                                                    AND es-api-{&transacao}.cd-agente    = p_cod_agente
                                                    AND es-api-{&transacao}.nm-appserv   = p_nome_app  NO-LOCK:

            FIND CURRENT es-api-{&transacao} EXCLUSIVE-LOCK NO-ERROR.

            ASSIGN es-api-{&transacao}.data-inicio  = NOW
                   l_proc = YES.

            /* ------ Executa progama espec¡fico para o tipo de integra‡Æo ------ */
            RUN VALUE( es-api-param.programa-integr ) (INPUT ROWID(es-api-{&transacao}),
                                                       OUTPUT c-erro) NO-ERROR.  

            ASSIGN es-api-{&transacao}.data-fim     = NOW
                   es-api-{&transacao}.ind-situacao = 2 .

            /* ------ Gerencia retorno do processo -----*/
            IF c-erro = "" THEN
                ASSIGN es-api-{&transacao}.cod-status = 1.
            ELSE
                ASSIGN es-api-{&transacao}.cod-status = 2.
                
            RUN pi-gera-status (INPUT c-erro).            
        END.        
    END.

END PROCEDURE.                           

PROCEDURE pi-gera-status:

    DEFINE INPUT PARAMETER c-erro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.

    FIND LAST es-api-{&transacao}-log NO-LOCK OF es-api-{&transacao} NO-ERROR.
    IF AVAIL es-api-{&transacao}-log THEN
        ASSIGN i-nr-seq = es-api-{&transacao}-log.nr-seq + 1.
    ELSE i-nr-seq = 1.

    CREATE es-api-{&transacao}-log.
    ASSIGN es-api-{&transacao}-log.ind-tipo-trans = es-api-{&transacao}.ind-tipo-trans
           es-api-{&transacao}-log.cd-tipo-integr = es-api-{&transacao}.cd-tipo-integr
           es-api-{&transacao}-log.id-movto       = es-api-{&transacao}.id-movto      
           es-api-{&transacao}-log.data-log       = NOW
           es-api-{&transacao}-log.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro
           es-api-{&transacao}-log.nr-seq         = i-nr-seq.
    
END PROCEDURE.
