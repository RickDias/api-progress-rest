/*******************************************************************************
**   Programa.: Escm0103RP
**   Data.....: 16/05/2019
**   Objetivo.: Gera consulta de Cr?dito
**   Autor....: Thiago Primo - Private.
*******************************************************************************/
/* include para controle de versao */

{include/i-prgvrs.i escm0103rp 2.06.00.000}

{esp/esint001rp.i}
/* definicao das temp-table para recebimento de parametros */

DEFINE TEMP-TABLE tt-param NO-UNDO
    field destino          AS INTEGER 
    field arquivo          as CHAR FORMAT  "x(35)"
    field usuario          as CHAR FORMAT  "x(12)"
    field data-exec        as DATE 
    field hora-exec        AS INTEGER 
    field classifica       AS INTEGER 
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
    FIELD val-sdo-credito AS DECIMAL FORMAT  "->>>>,>>>,>>9.99" 
    FIELD val-divida      AS DECIMAL FORMAT  "->>>>,>>>,>>9.99" 
    FIELD dt-div-mlonga   as DATE 
    FIELD dt-atualizacao  as DATE  
    INDEX i-1 IS PRIMARY cod-emitente.

/*
DEF TEMP-TABLE emitente
    FIELD cod-emitente    LIKE emitente.cod-emitente
    FIELD nome-abrev      LIKE emitente.nome-abrev
    INDEX ch-cod          IS PRIMARY cod-emitente 
    INDEX ch-nome     nome-abrev.
    */
    
/* recebimento de parametros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/*******************************Defini??o de Variaveis */


DEFINE VARIABLE d-dt-lim-cred   AS DATE  NO-UNDO.
DEFINE VARIABLE d-dt-fim-cred   AS DATE  NO-UNDO.
DEFINE VARIABLE v-lim-total     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-lim-disp      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-lim-aut-camil AS DECIMAL   NO-UNDO.
DEFINE VARIABLE c-caminho       AS CHAR  NO-UNDO.
DEFINE VARIABLE v-nf-nao-atu    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-val-tit-apb   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tit-tot       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-ped-apr-tot   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dt-divida       AS DATE  NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER   NO-UNDO.
DEFine VARiable v-ped-rep-tot   AS DECIMAL   NO-UNDO.

DEF NEW GLOBAL SHARED VAR v_cod_empres_usuar AS CHARACTER FORMAT "x(3)"   NO-UNDO .

DEFINE BUFFER cliente_ems5   FOR ems5.cliente.

/* include padrao para variaveis de relatorio */
{include/i-rpvar.i}

/* definicao de variaveis */
DEFINE VARIABLE h-acomp as handle no-undo.     

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
       c-titulo-relat = "Relat?rio de P.C.P.".

view FRAME f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Imprimindo *}

run pi-inicializar in h-acomp (input return-value).

FIND FIRST tt-param.
EMPTY TEMP-TABLE tt-integr-credito.


/*------ Processa Cr?dito ----- */
IF tt-param.l-aval-credito THEN DO:
    
    FOR EACH emitente WHERE emitente.identific <> 2 
                        AND emitente.natureza   = 2 NO-LOCK BY emitente.cod-emitente :
    
        ASSIGN i-cont = i-cont + 1.

        RUN pi-processa-cred.      
    
    END.
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
        "Cliente ; Nome Abrev ; Cgc ; Val. saldo cr?dito ; Val. d?vida ; Dt. div. Mais longa; Dt. Atualizacao" SKIP. 
END.

RUN pi-cria-avaliacao.
RUN pi-cria-export.

IF tt-param.l-csv THEN
    OUTPUT CLOSE.

{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


PROCEDURE pi-processa-cred :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE v_num_cont_aux_2           AS INTEGER  FORMAT ">>>>,>>9"    INITIAL 1       NO-UNDO.
    DEFINE VARIABLE v_cod_finalid_histor_clien AS CHAR FORMAT "x(10)"                       NO-UNDO.

    ASSIGN v_cod_finalid_histor_clien = "".
  
    FIND FIRST es-param-painel NO-LOCK NO-ERROR.

    IF NOT AVAIL es-param-painel THEN NEXT.

    ASSIGN dt-divida  = ?.
    
    CREATE tt-integr-credito.
    ASSIGN tt-integr-credito.cod-emitente = emitente.cod-emitente
           tt-integr-credito.nome-abrev   = emitente.nome-abrev 
           tt-integr-credito.cgc          = emitente.cgc.

    FIND LAST param_empres_acr NO-LOCK 
        WHERE param_empres_acr.cod_empresa = v_cod_empres_usuar NO-ERROR.

    IF AVAIL param_empres_acr THEN  
        ASSIGN v_cod_finalid_histor_clien = param_empres_acr.cod_finalid_histor_clien.
  
 
    ASSIGN v-lim-disp    = 0
           v-lim-total   = 0
           v-tit-tot     = 0
           v-ped-apr-tot = 0
           v-nf-nao-atu  = 0.
        
    RUN pi-monta-dados-limites.
    RUN pi-processa-pedidos.
    RUN pi-processa-nf.
    RUN pi-processa-acr.
    RUN pi-processa-apb.

    /*LIMITE DISPONIVEL*/
    ASSIGN v-lim-disp = (   v-lim-total       /*Limite Total*/
                          - v-tit-tot       /*Total ACR em aberto (somente DM e ST)*/
                          - v-ped-apr-tot   /*Total Pedidos Aprovados*/ 
                          - v-nf-nao-atu ). /*NF emitidas e ainda n?o atualizadas no ACR*/
    ASSIGN tt-integr-credito.val-sdo-credito = v-lim-disp
           tt-integr-credito.val-divida      = v-tit-tot
           tt-integr-credito.dt-div-mlonga   = dt-divida
           tt-integr-credito.dt-atualizacao  = TODAY.
    
END PROCEDURE.


PROCEDURE pi-monta-dados-limites :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN d-dt-lim-cred = ?
           d-dt-fim-cred = ?.
 
    DEFINE VARIABLE v-lim-credito   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-lim-adic      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-lim-aut-camil AS DECIMAL   NO-UNDO.

    RUN pi-acompanhar IN h-acomp (INPUT "Analisando Cliente: " + STRING(emitente.cod-emitente) + "  " + emitente.nome-abrev).

    FIND es-emit-cred WHERE es-emit-cred.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
                       
    IF emitente.dt-lim-cred >= TODAY THEN DO:
        ASSIGN v-lim-credito  = v-lim-credito   + emitente.lim-credito 
               d-dt-lim-cred  = (IF d-dt-lim-cred = ? OR emitente.dt-lim-cred < d-dt-lim-cred THEN  emitente.dt-lim-cred ELSE d-dt-lim-cred) .
    END.
    IF emitente.dt-fim-cred >= TODAY THEN DO:
        ASSIGN v-lim-adic     = v-lim-adic + emitente.lim-adicional 
               d-dt-fim-cred  = (IF d-dt-fim-cred = ? OR emitente.dt-fim-cred < d-dt-fim-cred THEN  emitente.dt-fim-cred ELSE d-dt-fim-cred) .
    END.
    
    ASSIGN v-lim-total     = ( v-lim-credito + v-lim-adic )
           v-lim-aut-camil = v-lim-aut-camil + ( IF AVAIL es-emit-cred THEN es-emit-cred.lim-aut-camil ELSE 0 ).     

END PROCEDURE.

PROCEDURE pi-cria-avaliacao.

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
END PROCEDURE.

PROCEDURE pi-cria-export.

    FIND FIRST es-api-param NO-LOCK 
        WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
          AND es-api-param.cd-tipo-integr = 11 /*---- Integra??o Cr?dito Cliente ------*/ NO-ERROR.
        
    IF AVAIL es-api-param THEN DO:
        IF NOT CAN-FIND(FIRST es-api-export 
                        WHERE es-api-export.chave = string(today,"99/99/9999")
                          AND es-api-export.ind-situacao < 2) THEN DO:
            
            CREATE es-api-export-cred.
            ASSIGN es-api-export-cred.cd-tipo-integr = es-api-param.cd-tipo-integr
                   es-api-export-cred.id-movto       = NEXT-VALUE(seq-export)     
                   es-api-export-cred.cgc            = ?                          
                   es-api-export-cred.data-movto     = NOW                        
                   es-api-export-cred.c-json         = ?.

            CREATE es-api-export.
            ASSIGN es-api-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                   es-api-export.id-movto       = es-api-export-cred.id-movto
                   es-api-export.cd-tipo-integr = es-api-export-cred.cd-tipo-integr
                   es-api-export.chave          = string(today,"99/99/9999")
                   es-api-export.cod-status     = 0      /* ---- sem status ----*/
                   es-api-export.data-fim       = ?
                   es-api-export.data-inicio    = ?
                   es-api-export.data-movto     = NOW
                   es-api-export.ind-situacao   = 1       /*---- Pendente -----*/.
                
            RUN pi-processa (INPUT 2, INPUT 11). 
        END.     
    END.

END PROCEDURE.

PROCEDURE pi-processa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFine VARiable v-ped-apr-d     AS DECIMAL   NO-UNDO.
    DEFine VARiable v-ped-rep-d     AS DECIMAL   NO-UNDO.
    DEFine VARiable v-ped-apr-dm    AS DECIMAL   NO-UNDO.
    DEFine VARiable v-ped-rep-dm    AS DECIMAL   NO-UNDO.



    FIND es-emit-cred WHERE es-emit-cred.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.

    ASSIGN v-ped-apr-tot = 0
           v-ped-rep-tot = 0.
 
    FOR EACH ped-venda 
       WHERE ped-venda.nome-abrev  = emitente.nome-abrev 
         AND ped-venda.cod-sit-ped < 3 NO-LOCK:
         
        RUN pi-acompanhar IN h-acomp (INPUT "Analisando Pedido: " + ped-venda.nome-abrev + "  " + ped-venda.nr-pedcli).
 
        /*APROVADOS*/
        IF ped-venda.cod-sit-aval = 2   /*1-nao avalidado 2-avalidado 3-aprovado 4-nao aprovado 5-pendente informacao*/
        OR ped-venda.cod-sit-aval = 3 THEN DO:
            IF ped-venda.dt-entrega = TODAY THEN 
                ASSIGN v-ped-apr-d  = v-ped-apr-d  + ped-venda.vl-liq-abe.  
            ELSE
                ASSIGN v-ped-apr-dm = v-ped-apr-dm + ped-venda.vl-liq-abe.
        END.
        /*PENDENTES*/
        ELSE DO: 
            IF ped-venda.dt-entrega = TODAY THEN 
                ASSIGN v-ped-rep-d  = v-ped-rep-d  + ped-venda.vl-liq-abe.  
            ELSE
                ASSIGN v-ped-rep-dm = v-ped-rep-dm + ped-venda.vl-liq-abe.
        END.    
    END.

    ASSIGN v-ped-apr-tot = v-ped-apr-d + v-ped-apr-dm
           v-ped-rep-tot = v-ped-rep-d + v-ped-rep-dm.
       
END PROCEDURE.

PROCEDURE pi-processa-apb:

    FOR EACH tit_ap NO-LOCK 
        WHERE tit_ap.cod_empresa    = v_cod_empres_usuar 
          AND tit_ap.cdn_fornecedor = emitente.cod-emitente:

        RUN pi-acompanhar IN h-acomp (INPUT "Analisando APB: " + STRING(tit_ap.cdn_fornecedor) + "  "  ).

        IF tit_ap.val_sdo_tit_ap > 0 THEN
            ASSIGN v-val-tit-apb = v-val-tit-apb + tit_ap.val_sdo_tit_ap.
    END.


END PROCEDURE.

PROCEDURE pi-processa-nf:

    /*NOTAS FISCALIS NAO ATUALIZADAS NO CR*/
    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.dt-atual-cr  = ?
                                   AND nota-fiscal.cod-emitente = emitente.cod-emitente 
                                   AND nota-fiscal.dt-cancela   = ?
        /* AND nota-fiscal.emite-duplic = NO */ :

        IF nota-fiscal.dt-emis-nota < 01/01/2014 THEN NEXT.
 
        RUN pi-acompanhar IN h-acomp (INPUT "Analisando NF n?o atu.CR: " + STRING(nota-fiscal.cod-emitente) + "  " + TRIM(nota-fiscal.nr-nota-fis) ).
 
        /*IF nota-fiscal.dt-cancela   <> ? THEN NEXT.*/
       /* IF nota-fiscal.emite-duplic = NO THEN NEXT.*/
        ASSIGN v-nf-nao-atu = v-nf-nao-atu + nota-fiscal.vl-tot-nota . 
 
    END.
END PROCEDURE.

PROCEDURE pi-processa-acr:

    DEFINE VARIABLE v_num_cont_aux         AS INT  FORMAT ">9"                          NO-UNDO.
    DEFINE VARIABLE p_dat_tit_acr_aber     AS DATE FORMAT "99/99/9999"  INIT 01/01/0001 NO-UNDO.
    DEFINE VARIABLE v_dat_liquidac_tit_acr AS DATE FORMAT "99/99/9999"                  NO-UNDO.
    DEFINE VARIABLE iDiasAtraso            AS INTEGER                                   NO-UNDO.
    DEFINE VARIABLE v-val-antecip          AS DECimal                                   NO-UNDO.
    DEFINE VARIABLE v-tit-avencer          AS DECimal                                   NO-UNDO.
    DEFINE VARIABLE v-tit-vencido          AS DECimal                                   NO-UNDO.

    FIND es-emit-cred WHERE es-emit-cred.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.

    FIND FIRST cliente_ems5 WHERE cliente_ems5.cod_empresa = v_cod_empres_usuar  
                              AND cliente_ems5.nom_abrev   = emitente.nome-abrev  NO-LOCK NO-ERROR .

    FIND FIRST clien_financ WHERE clien_financ.cod_empresa = cliente_ems5.cod_empresa 
                              AND clien_financ.cdn_cliente = cliente_ems5.cdn_cliente NO-LOCK NO-ERROR .
    IF NOT AVAIL clien_financ THEN RETURN.

    //RUN pi-acompanhar IN h-acomp (INPUT "Analisando ACR: " + STRING(clien_financ.cdn_cliente) + "  "  ).

    ASSIGN dt-divida = ?.
    
    FOR EACH estabelecimento FIELDS(cod_estab cod_empresa) NO-LOCK WHERE estabelecimento.cod_empresa = v_cod_empres_usuar:

        IF NOT CAN-FIND(FIRST tit_acr WHERE tit_acr.cod_estab   = estabelecimento.cod_estab 
                                        AND tit_acr.cdn_cliente = clien_financ.cdn_cliente) THEN NEXT.

        ASSIGN p_dat_tit_acr_aber = 01/01/2010.

        IF AVAIL es-param-painel THEN
            ASSIGN p_dat_tit_acr_aber = es-param-painel.dt-ini-acr.

        dat_block:
        DO v_dat_liquidac_tit_acr = (p_dat_tit_acr_aber) TO (TODAY + 10 ) :

            FIND FIRST tit_acr NO-LOCK WHERE tit_acr.cod_estab             = estabelecimento.cod_estab
                                         AND tit_acr.cdn_cliente           = clien_financ.cdn_cliente
                                         AND tit_acr.dat_liquidac_tit_acr >= v_dat_liquidac_tit_acr 
                                         AND tit_acr.val_sdo_tit_acr       > 0 NO-ERROR .
            IF AVAIL tit_acr THEN 
                ASSIGN v_dat_liquidac_tit_acr = tit_acr.dat_liquidac_tit_acr.
            ELSE LEAVE dat_block.
 
            tit_block:
            FOR EACH tit_acr NO-LOCK WHERE tit_acr.cod_estab            = estabelecimento.cod_estab
                                       AND tit_acr.cdn_cliente          = clien_financ.cdn_cliente
                                       AND tit_acr.dat_liquidac_tit_acr = v_dat_liquidac_tit_acr
                                       AND tit_acr.val_sdo_tit_acr      > 0:

                RUN pi-acompanhar IN h-acomp (INPUT "Analisando ACR: " + STRING(tit_acr.cdn_cliente) + "  " + STRING(tit_acr.cod_tit_acr) ).

                /*SO ANTECIPACOES*/
                IF  (tit_acr.cod_espec_docto = "AC" OR  tit_acr.cod_espec_docto = "CR" ) AND tit_acr.LOG_tit_acr_estordo = NO  THEN DO:

                    IF NOT CAN-FIND(FIRST movto_tit_acr WHERE movto_tit_acr.cod_estab           = tit_acr.cod_estab 
                                                          AND movto_tit_acr.cod_espec_docto     = tit_acr.cod_espec_docto  
                                                          AND movto_tit_acr.num_id_tit_acr      = tit_acr.num_id_tit_acr 
                                                          AND (movto_tit_acr.ind_trans_acr_abrev = "LIQ"  
                                                           OR movto_tit_acr.ind_trans_acr_abrev = "LQPD" ) ) THEN
                        ASSIGN v-val-antecip   = v-val-antecip +  tit_acr.val_sdo_tit_acr .

                    NEXT tit_block.
                END.
  
              /*SO TITULOS*/
                IF tit_acr.cod_espec_docto <> "DM" AND tit_acr.cod_espec_docto <> "ST" AND tit_acr.cod_espec_docto <> "DE" THEN NEXT.

                IF tit_acr.ind_tip_espec_docto <> "Normal" OR tit_acr.LOG_tit_acr_estordo = YES THEN NEXT.

                /* nao considera liquidacao de perda dedutivel */
                IF CAN-FIND(FIRST movto_tit_acr WHERE movto_tit_acr.cod_estab         = tit_acr.cod_estab
                                                  AND movto_tit_acr.num_id_tit_acr    = tit_acr.num_id_tit_acr
                                                  AND movto_tit_acr.log_movto_estordo = no
                                                  AND ( movto_tit_acr.ind_trans_acr BEGINS "Liquida‡Æo Perda Dedut¡vel"
                                                   OR movto_tit_acr.ind_trans_acr   = "Extorno de T¡tulo"  ) ) THEN NEXT.

                IF dt-divida = ? THEN dt-divida = tit_acr.dat_vencto_tit_acr .
                ELSE IF tit_acr.dat_vencto_tit_acr < dt-divida THEN
                    ASSIGN dt-divida = tit_acr.dat_vencto_tit_acr .

                ASSIGN v-tit-tot         = v-tit-tot     + tit_acr.val_sdo_tit_acr 
                       v-tit-avencer     = v-tit-avencer + IF tit_acr.dat_vencto_tit_acr >= TODAY THEN tit_acr.val_sdo_tit_acr ELSE 0
                       v-tit-vencido     = v-tit-vencido + IF tit_acr.dat_vencto_tit_acr <  TODAY THEN tit_acr.val_sdo_tit_acr ELSE 0.
 
                /*AGING*/
                ASSIGN iDiasAtraso = 0.
    
                IF ( tit_acr.dat_liquidac_tit_acr = 12/31/9999   
                  OR tit_acr.dat_liquidac_tit_acr = ?)         
                AND tit_acr.dat_vencto_tit_acr  < TODAY      THEN
                      ASSIGN iDiasAtraso = TODAY - tit_acr.dat_vencto_tit_acr.
                                        
            END. 
        END.    
    END.        
    

    
END PROCEDURE.

PROCEDURE pi-processa-lead:

    DEFINE BUFFER b-geo-parametro_data_entrega FOR geo-parametro_data_entrega.
    DEFINE BUFFER b-es-gp-lead-time            FOR es-gp-lead-time.
    
    DEFINE VARIABLE h-calc      AS HANDLE NO-UNDO.
    DEFINE VARIABLE i-lead-time AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i-cont      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i-seq-param AS INTEGER    NO-UNDO.
    DEFINE VARIABLE c-erro      AS CHAR       NO-UNDO.
    DEFINE VARIABLE c-cidade    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-estado    AS CHARACTER  NO-UNDO.

    DEF BUFFER bf-cidade FOR mgdis.cidade.

    IF NOT tt-param.l-lead-time THEN
        RETURN.

    RUN esp/esgp0188ca.p PERSISTENT SET h-calc.
    
    FOR EACH es-gp-lead-time NO-LOCK:

        ASSIGN i-cont = i-cont + 1.

        RUN pi-acompanhar IN h-acomp (INPUT STRING(i-cont)).

        FIND FIRST mgdis.cidade NO-LOCK WHERE cidade.cidade = es-gp-lead-time.cidade-dest
                                         AND cidade.estado = es-gp-lead-time.estado-dest NO-ERROR.
        IF AVAIL cidade THEN DO:

            IF cidade.pais <> 'Brasil' OR cidade.estado = "EX" THEN NEXT.

            FIND FIRST bf-cidade WHERE bf-cidade.cidade               BEGINS cidade-dest
                                   AND bf-cidade.estado               = estado-dest
                                   AND bf-cidade.cdn-munpio-ibge      = cidade.cdn-munpio-ibge
                                   AND LENGTH(TRIM(bf-cidade.cidade)) > LENGTH(TRIM(cidade.cidade)) NO-LOCK NO-ERROR.
            IF AVAIL bf-cidade THEN
                ASSIGN c-cidade = caps(trim(bf-cidade.cidade))
                       c-estado = CAPS(trim(bf-cidade.estado)).
            ELSE ASSIGN c-cidade = caps(trim(cidade.cidade))
                        c-estado = CAPS(trim(cidade.estado)).
        END.
        ELSE ASSIGN c-cidade = caps(trim(es-gp-lead-time.cidade-dest))
                    c-estado = caps(trim(es-gp-lead-time.estado-dest)). 


        ASSIGN i-lead-time = 0.
        RUN piDtEntregaSFA IN h-calc (BUFFER es-gp-lead-time,
                                      OUTPUT i-lead-time,
                                      OUTPUT c-erro).

        FIND FIRST sfa-lead-time EXCLUSIVE-LOCK WHERE sfa-lead-time.cod-estabel-orig = es-gp-lead-time.cod-estabel-orig
                                                  AND sfa-lead-time.estado-dest      = c-estado
                                                  AND sfa-lead-time.cidade-dest      = c-cidade
                                                  AND sfa-lead-time.cep-ini          = es-gp-lead-time.cep-ini         
                                                  AND sfa-lead-time.cep-fim          = es-gp-lead-time.cep-fim         
                                                  AND sfa-lead-time.peso-ini         = es-gp-lead-time.peso-ini        
                                                  AND sfa-lead-time.peso-fim         = es-gp-lead-time.peso-fim NO-ERROR.
        IF NOT AVAIL sfa-lead-time THEN DO:
            CREATE sfa-lead-time.
            BUFFER-COPY es-gp-lead-time TO sfa-lead-time. 
            ASSIGN sfa-lead-time.lead-time    = i-lead-time
                   sfa-lead-time.data-atualiz = TODAY
                   sfa-lead-time.cidade       = c-cidade
                   sfa-lead-time.estado       = c-estado.
        END.
        
        IF sfa-lead-time.lead-time <> i-lead-time THEN
            ASSIGN sfa-lead-time.lead-time    = i-lead-time
                   sfa-lead-time.data-atualiz = TODAY.
        
    END.

    DELETE OBJECT h-calc.

    /* Gera registro para integra??o */
    FIND FIRST es-api-param NO-LOCK 
        WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
          AND es-api-param.cd-tipo-integr = 15 /*---- Integra??o Cr?dito Cliente ------*/ NO-ERROR.
        
    IF AVAIL es-api-param THEN DO:
        IF NOT CAN-FIND(FIRST es-api-export WHERE es-api-export.chave = string(today,"99/99/9999")
                                           AND es-api-export.ind-situacao < 2) THEN DO:

            CREATE es-api-export-lead.
            ASSIGN es-api-export-lead.cd-tipo-integr = es-api-param.cd-tipo-integr
                   es-api-export-lead.id-movto       = NEXT-VALUE(seq-export)     
                   es-api-export-lead.data           = TODAY
                   es-api-export-lead.data-movto     = NOW                        
                   es-api-export-lead.c-json         = ?.

            CREATE es-api-export.
            ASSIGN es-api-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                   es-api-export.id-movto       = es-api-export-lead.id-movto
                   es-api-export.cd-tipo-integr = es-api-export-lead.cd-tipo-integr
                   es-api-export.chave          = STRING(es-api-export-lead.data,"99/99/9999")
                   es-api-export.cod-status     = 0      /* ---- sem status ----*/
                   es-api-export.data-fim       = ?
                   es-api-export.data-inicio    = ?
                   es-api-export.data-movto     = NOW
                   es-api-export.ind-situacao   = 1       /*---- Pendente -----*/.
                
            RUN pi-processa (INPUT 2, INPUT 15). 
        END.     
    END.


END.
