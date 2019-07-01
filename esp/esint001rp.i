PROCEDURE pi-processa:

    DEFINE INPUT PARAM p_transacao   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAM p_cd_integr   AS INTEGER NO-UNDO.
    DEF VAR c-erro AS CHARACTER NO-UNDO.
    /*DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO.*/
    DEFINE VARIABLE c-json AS CHARACTER NO-UNDO.

    DEFINE VARIABLE h-esint002 AS HANDLE NO-UNDO.

    FOR FIRST es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans  = p_transacao
                                     AND es-api-param.cd-tipo-integr  = p_cd_integr
                                     AND es-api-param.ativo:
        IF es-api-param.ind-tipo-trans = 2 THEN DO:
            /* ------ Verifica se existe agENDa de integra‡Æo v lida ----*/
            /*IF fncAgendaAtiva(es-api-param.cd-tipo-integr) THEN NEXT.*/
    
            IF SEARCH(es-api-param.programa-integr ) = ? THEN NEXT.
            IF AVAIL sfa-export AND sfa-export.ind-situacao = 1 THEN DO:
                ASSIGN sfa-export.data-inicio  = NOW.
                /* ------ Executa progama espec¡fico para o tipo de integra‡Æo ------ */
                RUN VALUE( es-api-param.programa-integr ) (INPUT ROWID(sfa-export),
                                                           OUTPUT c-erro) NO-ERROR. 
    
                ASSIGN sfa-export.data-fim     = NOW
                       sfa-export.ind-situacao = 2 .
    
                /* ------ Gerencia retorno do processo -----*/
                IF c-erro = "" THEN
                    ASSIGN sfa-export.cod-status = 1.
                ELSE DO:
                    /*
                    MESSAGE "Ocorreram erros na integra‡Æo com Salesforce! " SKIP 
                            "Descri‡Æo: " c-erro VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
                    ASSIGN sfa-export.cod-status = 2.
                END.
                    
                RUN pi-gera-status (INPUT p_transacao, INPUT c-erro).            
            END.        
        END.
        ELSE DO:
            /*
            RUN esp/esint002.p PERSISTENT SET h-esint002.

            /* ------ Verifica se existe agENDa de integra‡Æo v lida ----*/
            /*IF fncAgendaAtiva(es-api-param.cd-tipo-integr) THEN NEXT.*/
            IF SEARCH(es-api-param.programa-integr ) = ? THEN NEXT.
            
            IF AVAIL sfa-import AND sfa-import.ind-situacao = 1 THEN DO:

                IF sfa-import.cd-tipo-integr = 1 THEN DO:
                    FIND FIRST sfa-import-cli  OF sfa-import NO-LOCK NO-ERROR.
                    IF AVAIL sfa-import-cli THEN DO:
                        c-json = sfa-import-cli.c-json.

                        RUN piConvLongObj IN h-esint002 (INPUT c-json,
                                                         OUTPUT oJsonObject).
                    END.
                END.
                ELSE IF sfa-import.cd-tipo-integr = 3 THEN DO:
                    FIND FIRST sfa-import-ped  OF sfa-import NO-LOCK NO-ERROR.
                    IF AVAIL sfa-import-ped THEN DO:
                        c-json = sfa-import-cli.c-json.
                        RUN piConvLongObj IN h-esint002 (INPUT c-json,
                                                         OUTPUT oJsonObject).
                    END.
                END.


                ASSIGN sfa-import.data-inicio  = NOW.
                /* ------ Executa progama espec¡fico para o tipo de integra‡Æo ------ */
                RUN VALUE( es-api-param.programa-integr ) (INPUT ROWID(sfa-import),
                                                           OUTPUT c-erro,
                                                           INPUT ojsonObject) NO-ERROR. 
                IF ERROR-STATUS:ERROR THEN
                    MESSAGE 'deu erro: ' ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX INFO BUTTONS OK.

                ASSIGN sfa-import.data-fim     = NOW
                       sfa-import.ind-situacao = 2 .

                /* ------ Gerencia retorno do processo -----*/
                IF c-erro = "" THEN
                    ASSIGN sfa-import.cod-status = 1.
                ELSE DO:

                    /*
                    MESSAGE "Ocorreram erros na integra‡Æo com Salesforce! " SKIP 
                            "Descri‡Æo: " c-erro VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
                    ASSIGN sfa-import.cod-status = 2.
                END.

                DELETE OBJECT h-esint002.
                
                RUN pi-gera-status (INPUT p_transacao, INPUT c-erro).            
               
            END.
            */
        END.
    END.

END PROCEDURE.                           

PROCEDURE pi-gera-status:

    DEFINE INPUT PARAMETER p_transacao AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER c-erro      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.

    IF p_transacao = 2 THEN DO:
        
        FIND LAST sfa-export-log NO-LOCK OF sfa-export NO-ERROR.
        IF AVAIL sfa-export-log THEN
            ASSIGN i-nr-seq = sfa-export-log.nr-seq + 1.
        ELSE i-nr-seq = 1.
    
        CREATE sfa-export-log.
        ASSIGN sfa-export-log.ind-tipo-trans = sfa-export.ind-tipo-trans
               sfa-export-log.cd-tipo-integr = sfa-export.cd-tipo-integr
               sfa-export-log.id-movto       = sfa-export.id-movto      
               sfa-export-log.data-log       = NOW
               sfa-export-log.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro
               sfa-export-log.nr-seq         = i-nr-seq.
    END.
    ELSE DO:
        FIND LAST sfa-import-log NO-LOCK OF sfa-import NO-ERROR.
        IF AVAIL sfa-import-log THEN
            ASSIGN i-nr-seq = sfa-import-log.nr-seq + 1.
        ELSE i-nr-seq = 1.
    
        CREATE sfa-import-log.
        ASSIGN sfa-import-log.ind-tipo-trans = sfa-import.ind-tipo-trans
               sfa-import-log.cd-tipo-integr = sfa-import.cd-tipo-integr
               sfa-import-log.id-movto       = sfa-import.id-movto      
               sfa-import-log.data-log       = NOW
               sfa-import-log.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro
               sfa-import-log.nr-seq         = i-nr-seq.

    END.
    
END PROCEDURE.
