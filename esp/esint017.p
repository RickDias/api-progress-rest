

/* ------- Definiá∆o de temp-tables ------- */
DEFINE TEMP-TABLE tt-integr-credito
    FIELD cod-emitente    LIKE emitente.cod-emitente
    FIELD nome-abrev      LIKE emitente.nome-abrev
    FIELD cgc             LIKE emitente.cgc
    FIELD val-sdo-credito as DECIMAL FORMAT  "->>>>,>>>,>>9.99" 
    FIELD val-divida      as DECIMAL FORMAT  "->>>>,>>>,>>9.99" 
    FIELD dt-div-mlonga   as DATE 
    FIELD dt-atualizacao  as DATE  
    .

/* ------- Definiá∆o de Vari†veis------- */
DEFINE VARIABLE h-acomp             as handle no-undo.     
DEFINE VARIABLE d-dt-lim-cred       AS DATE  NO-UNDO.
DEFINE VARIABLE d-dt-fim-cred       AS DATE  NO-UNDO.
DEFINE VARIABLE v-lim-total         AS DEC   NO-UNDO.
DEFINE VARIABLE v-lim-disp          AS DEC   NO-UNDO.
DEFINE VARIABLE v-lim-aut-camil     AS DEC   NO-UNDO.
DEFINE VARIABLE v-nf-nao-atu        AS DEC   NO-UNDO.
DEFINE VARIABLE v-val-tit-apb       AS DEC   NO-UNDO.
DEFINE VARIABLE v-tit-tot           AS DEC   NO-UNDO.
DEFINE VARIABLE v-ped-apr-tot       AS DEC   NO-UNDO.
DEFINE VARIABLE dt-divida           AS DATE  NO-UNDO.
DEFINE VARIABLE v-ped-rep-tot       AS DEC   NO-UNDO.
DEFINE VARIABLE v_des_estab_select  AS CHAR FORMAT "x(500)"               NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_cod_empres_usuar AS CHARACTER FORMAT "x(3)"   NO-UNDO .

/* ------- Definiá∆o de Buffers------- */
DEFINE BUFFER cliente_ems5   FOR ems5.cliente.

return "OK":U.


PROCEDURE pi-processa-cred :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-cod-emitente AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-integr-credito.

    DEF VAR v_num_cont_aux_2           AS INT  FORMAT ">>>>,>>9"    INITIAL 1       NO-UNDO.
    DEF VAR v_cod_finalid_histor_clien AS CHAR FORMAT "x(10)"                       NO-UNDO.

    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input return-value).

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = p-cod-emitente NO-ERROR.
    IF NOT AVAIL emitente THEN
        RETURN "NOK".

    CREATE tt-integr-credito.
    ASSIGN tt-integr-credito.cod-emitente = emitente.cod-emitente
           tt-integr-credito.nome-abrev   = emitente.nome-abrev 
           tt-integr-credito.cgc          = emitente.cgc.

    ASSIGN v_des_estab_select         = "".
           v_cod_finalid_histor_clien = "".
  
    FIND FIRST es-param-painel NO-LOCK NO-ERROR.

    IF NOT AVAIL es-param-painel THEN NEXT.

    FOR EACH estabelecimento NO-LOCK WHERE estabelecimento.cod_empresa = v_cod_empres_usuar:

        IF v_des_estab_select = " " THEN 
            ASSIGN v_des_estab_select = estabelecimento.cod_estab.
        ELSE 
            ASSIGN v_des_estab_select = v_des_estab_select + "," + estabelecimento.cod_estab.
    END.

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
    ASSIGN v-lim-disp = ( v-lim-total       /*Limite Total*/
                          - v-tit-tot       /*Total ACR em aberto (somente DM e ST)*/
                          - v-ped-apr-tot   /*Total Pedidos Aprovados*/ 
                          - v-nf-nao-atu ). /*NF emitidas e ainda n∆o atualizadas no ACR*/

    ASSIGN tt-integr-credito.val-sdo-credito = v-lim-disp
           tt-integr-credito.val-divida      = v-tit-tot
           tt-integr-credito.dt-div-mlonga   = dt-divida
           tt-integr-credito.dt-atualizacao  = TODAY.

    RUN pi-finalizar IN h-acomp.

END PROCEDURE.


PROCEDURE pi-monta-dados-limites :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN d-dt-lim-cred = ?
           d-dt-fim-cred = ?.
 
    DEFINE VARIABLE v-lim-credito   AS DEC   NO-UNDO.
    DEFINE VARIABLE v-lim-adic      AS DEC   NO-UNDO.
    DEFINE VARIABLE v-lim-aut-camil AS DEC   NO-UNDO.

    RUN pi-acompanhar IN h-acomp (INPUT "Analisando Cliente: " + STRING(emitente.cod-emitente) + "  " + emitente.nome-abrev).

    FIND es-emit-cred WHERE es-emit-cred.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
                       
    IF emitente.dt-lim-cred >= TODAY THEN
        ASSIGN v-lim-credito  = v-lim-credito   + emitente.lim-credito 
               d-dt-lim-cred  = (IF d-dt-lim-cred = ? OR emitente.dt-lim-cred < d-dt-lim-cred THEN  emitente.dt-lim-cred ELSE d-dt-lim-cred) .
    
    IF emitente.dt-fim-cred >= TODAY THEN
        ASSIGN v-lim-adic     = v-lim-adic + emitente.lim-adicional 
               d-dt-fim-cred  = (IF d-dt-fim-cred = ? OR emitente.dt-fim-cred < d-dt-fim-cred THEN  emitente.dt-fim-cred ELSE d-dt-fim-cred) .
   
    ASSIGN v-lim-total     = ( v-lim-credito + v-lim-adic )
           v-lim-aut-camil = v-lim-aut-camil + ( IF AVAIL es-emit-cred THEN es-emit-cred.lim-aut-camil ELSE 0 ).     

    
END PROCEDURE.

PROCEDURE pi-processa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-ped-apr-d     AS DEC   NO-UNDO.
    DEFINE VARIABLE v-ped-rep-d     AS DEC   NO-UNDO.
    DEFINE VARIABLE v-ped-apr-dm    AS DEC   NO-UNDO.
    DEFINE VARIABLE v-ped-rep-dm    AS DEC   NO-UNDO.

    
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
          AND tit_ap.cdn_fornecedor = emitente.cod-emitente USE-INDEX titap_fornec_financ:

        RUN pi-acompanhar IN h-acomp (INPUT "Analisando APB: " + STRING(tit_ap.cdn_fornecedor) + "  "  ).

        IF tit_ap.val_sdo_tit_ap > 0 THEN
            ASSIGN v-val-tit-apb = v-val-tit-apb + tit_ap.val_sdo_tit_ap.
    END.

    
END PROCEDURE.

PROCEDURE pi-processa-nf:

    
    /*NOTAS FISCALIS NAO ATUALIZADAS NO CR*/
    FOR EACH nota-fiscal NO-LOCK   USE-INDEX ch-atual-cr
       WHERE nota-fiscal.dt-atual-cr  = ?
         AND nota-fiscal.cod-emitente = emitente.cod-emitente 
         AND nota-fiscal.dt-cancela   = ?
        /* AND nota-fiscal.emite-duplic = NO */ :
 
        RUN pi-acompanhar IN h-acomp (INPUT "Analisando NF n∆o atu.CR: " + STRING(nota-fiscal.cod-emitente) + "  " + TRIM(nota-fiscal.nr-nota-fis) ).
 
        /*IF nota-fiscal.dt-cancela   <> ? THEN NEXT.*/
       /* IF nota-fiscal.emite-duplic = NO THEN NEXT.*/
        IF nota-fiscal.dt-emis-nota < 01/01/2014 THEN NEXT.

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
    IF AVAIL clien_financ THEN DO:

        RUN pi-acompanhar IN h-acomp (INPUT "Analisando ACR: " + STRING(clien_financ.cdn_cliente) + "  "  ).

        ASSIGN dt-divida = ?.
        
        blk-estab:
        DO v_num_cont_aux = 1 TO NUM-ENTRIES(v_des_estab_select):

            FOR EACH estabelecimento FIELDS(cod_estab cod_empresa) NO-LOCK  
               WHERE estabelecimento.cod_estab  = ENTRY(v_num_cont_aux, v_des_estab_select):

                IF NOT CAN-FIND(FIRST tit_acr WHERE tit_acr.cod_estab = estabelecimento.cod_estab) THEN NEXT blk-estab.
    
                IF NOT CAN-FIND(FIRST tit_acr WHERE tit_acr.cod_estab   = estabelecimento.cod_estab 
                                                AND tit_acr.cdn_cliente = clien_financ.cdn_cliente) THEN NEXT blk-estab.

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
                    FOR EACH tit_acr USE-INDEX titacr_clien_datas NO-LOCK 
                        WHERE tit_acr.cod_estab            = estabelecimento.cod_estab
                        AND   tit_acr.cdn_cliente          = clien_financ.cdn_cliente
                        AND   tit_acr.dat_liquidac_tit_acr = v_dat_liquidac_tit_acr
                        AND   tit_acr.val_sdo_tit_acr      > 0:
  
                        RUN pi-acompanhar IN h-acomp (INPUT "Analisando ACR: " + STRING(tit_acr.cdn_cliente) + "  " + STRING(tit_acr.cod_tit_acr) ).

                        /*SO ANTECIPACOES*/
                        IF  tit_acr.cod_espec_docto = "AC" OR  tit_acr.cod_espec_docto = "CR"   THEN DO:

                            FIND FIRST movto_tit_acr WHERE movto_tit_acr.cod_estab           = tit_acr.cod_estab 
                                                       AND movto_tit_acr.cod_espec_docto     = tit_acr.cod_espec_docto  
                                                       AND movto_tit_acr.num_id_tit_acr      = tit_acr.num_id_tit_acr 
                                                       AND (movto_tit_acr.ind_trans_acr_abrev = "LIQ"  
                                                        OR movto_tit_acr.ind_trans_acr_abrev = "LQPD" ) NO-LOCK NO-ERROR.
                            IF NOT AVAIL movto_tit_acr AND  tit_acr.LOG_tit_acr_estordo = NO THEN
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
                                                          AND ( movto_tit_acr.ind_trans_acr BEGINS "Liquidaá∆o Perda Dedut°vel"
                                                           OR movto_tit_acr.ind_trans_acr   = "Extorno de T°tulo"  ) ) THEN NEXT.

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
        END.        
    END.

    
END PROCEDURE.
