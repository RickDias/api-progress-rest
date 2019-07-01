/* TRIGGER DE WRITE PARA EMITENTE, GRAVADA NA MANUTEN€ÇO DE TABELAS */

/*******************************************************************************
*
* Altera‡Æo: Ramon - Kraft 08/2014 - Painel Avalia‡Æo Credito
*            Cria‡Æo tabela LOG altera‡Æo dos campos 
*            emitente.lim-credito, emitente.ind-cre-cli, emitente.dt-lim-cred 
*            emitente.lim-adicional, emitente.dt-fim-cred  
*
*            Erika - 11/11/2014
*            solicita‡Æo marcos Rodrigues e C¡cero para que se crie campo de usu rio
*            de cria‡Æo do emitente
*
******************************************************************************/
{utp/ut-glob.i}
{include/i-prgvrs.i ESTR001 2.09.00.001 } /*** 010001 ***/

DEFINE PARAMETER BUFFER b-emitente     for emitente.
DEFINE PARAMETER BUFFER b-old-emitente FOR emitente.

DEF VAR level        AS INT  INITIAL 1.
DEF VAR c-arquivo    AS CHAR FORMAT "x(30)".
DEF VAR i-clas-pilar AS INT  NO-UNDO.
DEF VAR c-cod-pilar  AS CHAR NO-UNDO.
DEF VAR i-ind        AS INT  NO-UNDO.
DEF VAR c-prg-alt    AS CHAR NO-UNDO.


def new global shared var v_cod_usuar_corren as character format "x(12)":U label "Usuario Corr" column-label "Usuario Corr" no-undo.

DEFINE BUFFER b-es-gp-emit-cond-pagto FOR es-gp-emit-cond-pagto.
DEFINE BUFFER b-es-lib-cml            FOR es-lib-cml.

IF NOT AVAIL b-emitente THEN RETURN.

IF b-emitente.cgc          <> b-old-emitente.cgc          OR      
   b-emitente.identific    <> b-old-emitente.identific    OR
   b-emitente.ins-estadual <> b-old-emitente.ins-estadual OR
   b-emitente.nome-abrev   <> b-old-emitente.nome-abrev   OR
   b-emitente.nome-emit    <> b-old-emitente.nome-emit    OR
   b-emitente.nome-matriz  <> b-old-emitente.nome-matriz  THEN DO:

    FIND FIRST es-alt-emitente  EXCLUSIVE-LOCK USE-INDEX codigo
        WHERE es-alt-emitente.cod-emitente = b-emitente.cod-emitente NO-ERROR.
    IF AVAIL es-alt-emitente THEN DO:        
        ASSIGN es-alt-emitente.cont-index                                   = IF es-alt-emitente.cont-index = 10 THEN 1 ELSE es-alt-emitente.cont-index + 1
               es-alt-emitente.cgc                                          = b-old-emitente.cgc         
               es-alt-emitente.identific                                    = b-old-emitente.identific   
               es-alt-emitente.ins-estadual                                 = b-old-emitente.ins-estadual
               es-alt-emitente.nome-abrev                                   = b-old-emitente.nome-abrev  
               es-alt-emitente.nome-emit                                    = b-old-emitente.nome-emit   
               es-alt-emitente.nome-matriz                                  = b-old-emitente.nome-matriz 
               es-alt-emitente.cgc-alt[es-alt-emitente.cont-index]          = b-emitente.cgc         
               es-alt-emitente.identific-alt[es-alt-emitente.cont-index]    = b-emitente.identific   
               es-alt-emitente.ins-estadual-alt[es-alt-emitente.cont-index] = b-emitente.ins-estadual
               es-alt-emitente.nome-abrev-alt[es-alt-emitente.cont-index]   = b-emitente.nome-abrev  
               es-alt-emitente.nome-emit-alt[es-alt-emitente.cont-index]    = b-emitente.nome-emit   
               es-alt-emitente.nome-matriz-alt[es-alt-emitente.cont-index]  = b-emitente.nome-matriz
               es-alt-emitente.usuario[es-alt-emitente.cont-index]          = c-seg-usuario
               es-alt-emitente.data-alt[es-alt-emitente.cont-index]         = TODAY.       
    END.
    ELSE do:           
       CREATE es-alt-emitente.
       ASSIGN es-alt-emitente.cod-emitente = b-emitente.cod-emitente 
              es-alt-emitente.cgc          = b-emitente.cgc          
              es-alt-emitente.identific    = b-emitente.identific    
              es-alt-emitente.ins-estadual = b-emitente.ins-estadual 
              es-alt-emitente.nome-abrev   = b-emitente.nome-abrev   
              es-alt-emitente.nome-emit    = b-emitente.nome-emit    
              es-alt-emitente.nome-matriz  = b-emitente.nome-matriz.
    
    END.
END.

IF b-emitente.nome-abrev <> b-old-emitente.nome-abrev THEN DO:

    FOR EACH es-lib-cml NO-LOCK
        WHERE es-lib-cml.nome-abrev = b-old-emitente.nome-abrev:
        
        FIND FIRST b-es-lib-cml EXCLUSIVE-LOCK
            WHERE ROWID(b-es-lib-cml) = ROWID(es-lib-cml) NO-ERROR.
        IF AVAIL b-es-lib-cml THEN
            ASSIGN b-es-lib-cml.nome-abrev = b-emitente.nome-abrev.
    END.
END.

IF b-emitente.cod-rep <> b-old-emitente.cod-rep THEN DO:
    
    CREATE ext-emitente-01. 
    ASSIGN ext-emitente-01.usuario        = c-seg-usuario
           ext-emitente-01.cod-emitente      = b-emitente.cod-emitente
           ext-emitente-01.cod-rep-atu       = b-emitente.cod-rep 
           ext-emitente-01.cod-rep-ant       = b-old-emitente.cod-rep
           ext-emitente-01.cod-canal-cli-ant = b-old-emitente.cod-canal-venda
           ext-emitente-01.cod-canal-cli-atu = b-emitente.cod-canal-venda
           ext-emitente-01.dt-trans          = TODAY
           ext-emitente-01.cod-programa      = PROGRAM-NAME(6)
           ext-emitente-01.hr-trans          = STRING(TIME,"hh:mm").

    RELEASE ext-emitente-01.
END.
 
/* Tabela es-gp-emitente s¢ deve ser alterada se mudar o canal de venda       */
/* Se houver outras alteracoes a ser feita deve ser acrescentado no if abaixo */
IF b-emitente.cod-canal-venda <> b-old-emitente.cod-canal-venda OR BUFFER b-emitente:NEW THEN DO:

    CREATE ext-emitente-01. 
    ASSIGN ext-emitente-01.usuario        = c-seg-usuario
           ext-emitente-01.cod-emitente      = b-emitente.cod-emitente
           ext-emitente-01.cod-rep-atu       = b-emitente.cod-rep 
           ext-emitente-01.cod-rep-ant       = b-old-emitente.cod-rep
           ext-emitente-01.cod-canal-cli-ant = b-old-emitente.cod-canal-venda
           ext-emitente-01.cod-canal-cli-atu = b-emitente.cod-canal-venda
           ext-emitente-01.dt-trans          = TODAY
           ext-emitente-01.cod-programa      = PROGRAM-NAME(6)
           ext-emitente-01.hr-trans          = STRING(TIME,"hh:mm").

    RELEASE ext-emitente-01.
             
    FIND FIRST es-gp-emitente EXCLUSIVE-LOCK
        WHERE es-gp-emitente.cod-emitente = b-emitente.cod-emitente NO-ERROR.
    IF NOT AVAIL es-gp-emitente THEN DO:
        CREATE es-gp-emitente.
        ASSIGN es-gp-emitente.cod-emitente = b-emitente.cod-emitente.
    END.
    
    FOR FIRST es-gp-meso-cidade NO-LOCK
        WHERE es-gp-meso-cidade.cidade = b-emitente.cidade
          AND es-gp-meso-cidade.estado = b-emitente.estado:
        
        /* Pilar */
        FOR EACH es-gp-meso-segm NO-LOCK
           WHERE es-gp-meso-segm.cod-meso = es-gp-meso-cidade.cod-meso:
        
            FOR FIRST es-gp-pilar FIELD(clas-pilar cod-pilar)
                WHERE es-gp-pilar.cod-pilar = es-gp-meso-segm.cod-pilar NO-LOCK:
                
                IF es-gp-pilar.clas-pilar < i-clas-pilar OR c-cod-pilar  = "" THEN DO:
                    ASSIGN i-clas-pilar = es-gp-pilar.clas-pilar
                           c-cod-pilar  = es-gp-pilar.cod-pilar.
                END.
            END.
        END.
        
        FOR FIRST es-gp-canal-pilar FIELD(cod-nivel)
            WHERE es-gp-canal-pilar.cod-canal-venda = b-emitente.cod-canal-venda
              AND es-gp-canal-pilar.cod-pilar       = c-cod-pilar NO-LOCK.
            
            ASSIGN es-gp-emitente.cod-nivel = es-gp-canal-pilar.cod-nivel.
        END.
    END.

    IF BUFFER b-emitente:NEW THEN DO:

        /* Busca cliente condi‡åesde pagamento padrÆo */
        FOR EACH b-es-gp-emit-cond-pagto NO-LOCK
           WHERE b-es-gp-emit-cond-pagto.cod-emitente = 0:
        
            IF NOT CAN-FIND(FIRST es-gp-emit-cond-pagto NO-LOCK
                            WHERE es-gp-emit-cond-pagto.cod-emitente  = b-emitente.cod-emitente
                              AND es-gp-emit-cond-pagto.cod-cond-pag  = b-es-gp-emit-cond-pagto.cod-cond-pag) THEN DO:
        
                CREATE es-gp-emit-cond-pagto.
                ASSIGN es-gp-emit-cond-pagto.cod-emitente = b-emitente.cod-emitente            
                       es-gp-emit-cond-pagto.cod-cond-pag = b-es-gp-emit-cond-pagto.cod-cond-pag.
        
            END.
        END.
    END.

    RELEASE es-gp-emitente.
END. /* cod canal or new */

/************************ Painel Avalia‡Æo de Cr‚dito ****************************/
/*CRIACAO LOG REF ALTERA€åES DE CAMPOS RELATIVOS A AVALIA€ÇO DE CREDITO*/
IF b-emitente.lim-credito   <> b-old-emitente.lim-credito  
OR b-emitente.ind-cre-cli   <> b-old-emitente.ind-cre-cli  
OR b-emitente.dt-lim-cred   <> b-old-emitente.dt-lim-cred  
OR b-emitente.lim-adicional <> b-old-emitente.lim-adicional
OR b-emitente.dt-fim-cred   <> b-old-emitente.dt-fim-cred THEN DO:
    
    ASSIGN c-prg-alt = "".

    DO i-ind = 1 TO 20:
        IF PROGRAM-NAME(i-ind) MATCHES ("*v32ad098*") THEN ASSIGN c-prg-alt = "CM0102".
    END.

    IF c-prg-alt = "" THEN ASSIGN c-prg-alt = PROGRAM-NAME(1).
 
    IF b-emitente.lim-credito <> b-old-emitente.lim-credito THEN DO: 
        CREATE es-emit-cred-log.
        ASSIGN es-emit-cred-log.cod-emitente = b-emitente.cod-emitente
               es-emit-cred-log.data-alt     = TODAY
               es-emit-cred-log.hora-alt     = STRING(TIME,"hh:mm:ss")
               es-emit-cred-log.user-alt     = c-seg-usuario
               es-emit-cred-log.prg-alt      = c-prg-alt
               es-emit-cred-log.tab-alt      = "emitente"
               es-emit-cred-log.cpo-alt      = "lim-credito"
               es-emit-cred-log.lab-alt      = "Limite Cr‚dito"
               es-emit-cred-log.cpo-antes    = TRIM(STRING(b-old-emitente.lim-credito,">>>,>>>,>>>,>>9.99"))
               es-emit-cred-log.cpo-depois   = TRIM(STRING(b-emitente.lim-credito,">>>,>>>,>>>,>>9.99")).
    END.

     IF b-old-emitente.nome-abrev <> "" THEN DO: /*NAO E INCLUSAO*/
         
         IF b-emitente.ind-cre-cli   <> b-old-emitente.ind-cre-cli THEN DO: 
             CREATE es-emit-cred-log.
             ASSIGN es-emit-cred-log.cod-emitente = b-emitente.cod-emitente
                    es-emit-cred-log.data-alt     = TODAY
                    es-emit-cred-log.hora-alt     = STRING(TIME,"hh:mm:ss")
                    es-emit-cred-log.user-alt     = c-seg-usuario
                    es-emit-cred-log.prg-alt      = c-prg-alt
                    es-emit-cred-log.tab-alt      = "emitente"
                    es-emit-cred-log.cpo-alt      = "ind-cre-cli"
                    es-emit-cred-log.lab-alt      = "Cr‚dito (Situa‡Æo)".
             ASSIGN es-emit-cred-log.cpo-antes    = STRING(INT(b-old-emitente.ind-cre-cli)) + "-"
                                                  + ( IF b-old-emitente.ind-cre-cli = 1 THEN "Normal" ELSE
                                                          IF b-old-emitente.ind-cre-cli = 2 THEN "Automatico" ELSE
                                                              IF b-old-emitente.ind-cre-cli = 3 THEN "S¢ Imp Ped" ELSE
                                                                  IF b-old-emitente.ind-cre-cli = 4 THEN "Suspenso" ELSE  
                                                                      IF b-old-emitente.ind-cre-cli = 5 THEN "Pg a Vista" ELSE "" ).
             ASSIGN es-emit-cred-log.cpo-depois   = STRING(INT(b-emitente.ind-cre-cli))  + "-" 
                                                  + ( IF b-emitente.ind-cre-cli = 1 THEN "Normal" ELSE
                                                          IF b-emitente.ind-cre-cli = 2 THEN "Automatico" ELSE
                                                              IF b-emitente.ind-cre-cli = 3 THEN "S¢ Imp Ped" ELSE
                                                                  IF b-emitente.ind-cre-cli = 4 THEN "Suspenso" ELSE  
                                                                      IF b-emitente.ind-cre-cli = 5 THEN "Pg a Vista" ELSE "" ).
         END.

         IF b-emitente.dt-lim-cred   <> b-old-emitente.dt-lim-cred THEN DO:
             CREATE es-emit-cred-log.
             ASSIGN es-emit-cred-log.cod-emitente = b-emitente.cod-emitente
                    es-emit-cred-log.data-alt     = TODAY
                    es-emit-cred-log.hora-alt     = STRING(TIME,"hh:mm:ss")
                    es-emit-cred-log.user-alt     = c-seg-usuario
                    es-emit-cred-log.prg-alt      = c-prg-alt
                    es-emit-cred-log.tab-alt      = "emitente"
                    es-emit-cred-log.cpo-alt      = "dt-lim-cred"
                    es-emit-cred-log.lab-alt      = "Dt Lim Cred"
                    es-emit-cred-log.cpo-antes    = STRING(b-old-emitente.dt-lim-cred,"99/99/9999")
                    es-emit-cred-log.cpo-depois   = STRING(b-emitente.dt-lim-cred,"99/99/9999").
         END.
 
         IF b-emitente.lim-adicional <> b-old-emitente.lim-adicional THEN DO:
             CREATE es-emit-cred-log.
             ASSIGN es-emit-cred-log.cod-emitente = b-emitente.cod-emitente
                    es-emit-cred-log.data-alt     = TODAY
                    es-emit-cred-log.hora-alt     = STRING(TIME,"hh:mm:ss")
                    es-emit-cred-log.user-alt     = c-seg-usuario
                    es-emit-cred-log.prg-alt      = c-prg-alt
                    es-emit-cred-log.tab-alt      = "emitente"
                    es-emit-cred-log.cpo-alt      = "lim-adicional"
                    es-emit-cred-log.lab-alt      = "Lim Cr‚d Adic"
                    es-emit-cred-log.cpo-antes    = TRIM(STRING(b-old-emitente.lim-adicional,">>>,>>>,>>>,>>9.99"))
                    es-emit-cred-log.cpo-depois   = TRIM(STRING(b-emitente.lim-adicional,">>>,>>>,>>>,>>9.99")).
         END.
 
         IF b-emitente.dt-fim-cred   <> b-old-emitente.dt-fim-cred   THEN DO:
             CREATE es-emit-cred-log.
             ASSIGN es-emit-cred-log.cod-emitente = b-emitente.cod-emitente
                    es-emit-cred-log.data-alt     = TODAY
                    es-emit-cred-log.hora-alt     = STRING(TIME,"hh:mm:ss")
                    es-emit-cred-log.user-alt     = c-seg-usuario
                    es-emit-cred-log.prg-alt      = c-prg-alt
                    es-emit-cred-log.tab-alt      = "emitente"
                    es-emit-cred-log.cpo-alt      = "dt-fim-cred"
                    es-emit-cred-log.lab-alt      = "Fim Lim Cr‚d Adic"
                    es-emit-cred-log.cpo-antes    = STRING(b-old-emitente.dt-fim-cred,"99/99/9999")
                    es-emit-cred-log.cpo-depois   = STRING(b-emitente.dt-fim-cred,"99/99/9999").
         END.
    END.
END.    
/************************ Painel Avalia‡Æo de Cr‚dito ****************************/

FIND FIRST es-emitente EXCLUSIVE-LOCK
     WHERE es-emitente.cod-emitente = b-emitente.cod-emitente NO-ERROR.
IF NOT AVAIL es-emitente THEN DO:
    IF b-emitente.data-implant >= 11/11/2014 THEN DO:
        CREATE es-emitente.
        ASSIGN es-emitente.cod-emitente = b-emitente.cod-emitente.
    END.
END.
ELSE IF es-emitente.usuar-criacao = "" THEN do:
    ASSIGN es-emitente.usuar-criacao = c-seg-usuario. /* para somente gravar na cria‡Æo do emitente, s¢ na primeira vez */

END.

/* ------ Projeto SFA ----- */
IF (b-emitente.identific = 1 OR b-emitente.identific = 3) AND b-emitente.natureza = 2 THEN DO:

    FIND FIRST es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
                                      AND es-api-param.cd-tipo-integr = 2 /*---- Integra‡Æo Cliente ------*/ NO-ERROR.
    IF AVAIL es-api-param THEN DO:


        IF (NOT CAN-FIND(FIRST sfa-export WHERE sfa-export.chave = b-emitente.cgc
                                           AND sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                                           AND sfa-export.cd-tipo-integr = es-api-param.cd-tipo-integr
                                           AND sfa-export.ind-situacao < 2) ) THEN DO:

            CREATE sfa-export-cli.
            ASSIGN sfa-export-cli.cd-tipo-integr = es-api-param.cd-tipo-integr
                   sfa-export-cli.id-movto       = NEXT-VALUE(seq-export)
                   sfa-export-cli.cgc            = b-emitente.cgc
                   sfa-export-cli.data-movto     = NOW
                   sfa-export-cli.c-json         = ?.
    
            CREATE sfa-export.
            ASSIGN sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                   sfa-export.id-movto       = sfa-export-cli.id-movto
                   sfa-export.cd-tipo-integr = sfa-export-cli.cd-tipo-integr
                   sfa-export.chave          = sfa-export-cli.cgc
                   sfa-export.cod-status     = 0      /* ---- sem status ----*/
                   sfa-export.data-fim       = ?
                   sfa-export.data-inicio    = ?
                   sfa-export.data-movto     = NOW
                   sfa-export.ind-situacao   = 1       /*---- Pendente -----*/.

            RUN pi-processa (INPUT 2, INPUT 2).  

        END.
    END.
END.
    

RETURN "OK".
     
{esp\esint001rp.i}



