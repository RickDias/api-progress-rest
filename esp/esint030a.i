
/*----------------------------------------------------------------------------------------------/
 Programa..: esint030a.p
 Objetivo..: Interface Integra‡Æo Contrto de Fornecedores 
 Data......: 26/02/2019
 Autor.....: 
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/
{esp\esint030.i}
{cnp\cn0206.i}

{ccp/ccapi202.i}
{ccp/ccapi203.i}
{ccp/ccapi205.i}
{ccp/ccapi207.i}
{utp/utapi019.i} 

// ------- Proceduress Cadatra a Capa do Contrato 
PROCEDURE pi-processa.
/*     MESSAGE "pi-processa"                  */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    
    CREATE tt-versao-integr.
    ASSIGN tt-versao-integr.cod-versao-integracao = 001
           tt-versao-integr.ind-origem-msg        = 01.

/*     MESSAGE "PI Cria Contrato"             */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */


    MESSAGE "###-PI-PROCESSA-GERANDO CONTRATO".

    FOR EACH tt-imp-contrato-for NO-LOCK:
        CASE tt-imp-contrato-for.ind-tipo-movto:
            WHEN 1 THEN RUN pi-geraContratoFor.
            WHEN 2 THEN RUN pi-geraContratoFor.
            WHEN 3 THEN RUN pi-geraContratoFor.
        END CASE.

         MESSAGE "###-PI-PROCESSA-GERANDO MATRIZ CONTRATO". //16/07/2019 - Leandro Policarpo

         FOR EACH tt-imp-item-contrato                                                                
             WHERE tt-imp-item-contrato.nr-contrato = tt-imp-contrato-for.nr-contrato NO-LOCK:                                                                                        
             CASE tt-imp-contrato-for.ind-tipo-movto:                                                 
                 WHEN 1 THEN RUN pi-geraItemContrato.                                                 
                 WHEN 2 THEN RUN pi-geraItemContrato.                                                 
                 WHEN 3 THEN RUN pi-geraItemContrato.                                                 
             END CASE. 

                 MESSAGE "###-PI-PROCESSA-GERANDO MATRIZ CONTRATO". //16/07/2019 - Leandro Policarpo
                 //Criando para gerar a Matriz do Contrato
                 FOR EACH tt-imp-matriz-rat-contrato NO-LOCK:
                    // WHERE tt-imp-matriz-rat-contrato.nr-contrato = tt-imp-contrato-for.nr-contrato  NO-LOCK:                         Leandro Policarpo 17/07/2019 
        
                     CASE tt-imp-matriz-rat-contrato.ind-tipo-movto:
                         WHEN 1 THEN RUN pi-geraMatrizRateioContrato.                                                 
                         WHEN 2 THEN RUN pi-geraMatrizRateioContrato.                                                 
                         WHEN 3 THEN RUN pi-geraMatrizRateioContrato. 
                     END CASE.
                 END.

         END.
    END.
 END.
END PROCEDURE.


PROCEDURE pi-geraContratoFor.

    MESSAGE "####-GERANDO A CAPA DO CONTRATO " tt-imp-contrato-for.nr-contrato.
 
    CREATE contrato-for.
    ASSIGN contrato-for.nr-contrato         = tt-imp-contrato-for.nr-contrato
           contrato-for.des-contrat         = string(tt-imp-contrato-for.des-contrat)
           contrato-for.cod-emitente        = tt-imp-contrato-for.cod-emitente
           contrato-for.dt-contrato         = TODAY //tt-imp-contrato-for.dt-contrato
           contrato-for.dt-ini-validade     = tt-imp-contrato-for.dt-ini-validade
           contrato-for.dt-ter-validade     = tt-imp-contrato-for.dt-ter-validade
           contrato-for.cod-comprado        = "01MDGCUNHA"
           contrato-for.cod-cond-pag        = tt-imp-contrato-for.cod-cond-pag //IF tt-imp-contrato-for.cod-cond-pag = 0 OR tt-imp-contrato-for.cod-cond-pag = ? THEN 30 ELSE tt-imp-contrato-for.cod-cond-pag
           contrato-for.via-transp          = 1 /*1-Rodoviario*/
           contrato-for.cod-transp          = 0 
           contrato-for.tp-fornecim         = 1 /*1-reposicao*/
           contrato-for.frete               = 1 /*1-Pago ou 2-A Pagar*/
           contrato-for.natureza            = 1 /*1-Compra*/
           contrato-for.moeda               = 0 /*alterado para zero, pq est  vindo brl na integracao ‚ preciso tratar isso  tt-imp-contrato-for.mo-codigo*/
           contrato-for.cod-estabel         = tt-imp-contrato-for.cod-estabel //IF tt-imp-contrato-for.cod-estabel = "" OR tt-imp-contrato-for.cod-estabel = ? THEN "10" ELSE tt-imp-contrato-for.cod-estabel
           contrato-for.contato             = tt-imp-contrato-for.contato
           contrato-for.impr-contrat        = YES 
           contrato-for.cod-tipo-contrat    = 01
           contrato-for.gestor-tecnico      = "01mdgcunha"     //usu ria padrÆo de GestÆo de Contrato.
           contrato-for.variacao-qtd        = 0
           contrato-for.variacao-preco      = 0
           contrato-for.cod-mensagem        = 100
           contrato-for.cod-estab-orig      = tt-imp-contrato-for.cod-estabel
           contrato-for.cod-estab-cobr      = tt-imp-contrato-for.cod-estabel
           contrato-for.val-total           = tt-imp-contrato-for.val-total
           contrato-for.cod-estab-entr      = tt-imp-contrato-for.cod-estabel
           contrato-for.qtd-total           = 0                                                                                                     //tt-imp-contrato-for.qtd-total
           contrato-for.sld-qtd             = 0                                                                                                     //tt-imp-contrato-for.sld-qtd
           contrato-for.sld-val             = 0                                                                                                     //tt-imp-contrato-for.sld-val
           contrato-for.acum-rec-qtd        = 0                                                                                                     //tt-imp-contrato-for.acum-rec-qtd
           contrato-for.acum-rec-val        = 0                                                                                                     //tt-imp-contrato-for.acum-rec-val
           contrato-for.sld-qtd-liber       = 0                                                                                                     //tt-imp-contrato-for.sld-qtd-liber
           contrato-for.sld-val-liber       = 0                                                                                                     //tt-imp-contrato-for.sld-val-liber
           contrato-for.val-fatur-minimo    = 0                                                                                                     //tt-imp-contrato-for.val-fatur-minimo
           contrato-for.des-contrat         = tt-imp-contrato-for.des-contrat                                                                       
           contrato-for.acum-val-pago       = 0                                                                                                     //tt-imp-contrato-for.acum-val-pago
           contrato-for.mo-codigo           = 0 /*-fixado como real, at‚ vir correto na integra‡Æo*/                                                //tt-imp-contrato-for.mo-codigo
/*         contrato-for.log-libera          = tt-imp-contrato-for.log-libera  */                                                                    
           contrato-for.sld-qtd-med         = 0                                                                                                     //tt-imp-contrato-for.sld-qtd-med
           contrato-for.sal-qtd-liber-med   = 0                                                                                                     //tt-imp-contrato-for.sal-qtd-liber-med
           contrato-for.sld-val-med         = 0                                                                                                     //tt-imp-contrato-for.sld-val-med
           contrato-for.sld-val-liber-med   = 0                                                                                                     
           /*este campo s¢ pode ser preenchido quando houver Ordem de Investimento*/                                                                //tt-imp-contrato-for.sld-val-liber-med
           contrato-for.cod-projeto         = ""                                                             //string(tt-imp-contrato-for.nr-contrato) //tt-imp-contrato-for.cod-projeto
           contrato-for.ind-sit-contrat     = 2 //Emitido                                                 // tt-imp-contrato-for.ind-sit-contrat
           contrato-for.narrat-contrat      = tt-imp-contrato-for.narrat-contrat
           contrato-for.ind-preco           = 1
           contrato-for.sc-codigo           = tt-imp-contrato-for.sc-codigo
           contrato-for.ct-codigo           = tt-imp-contrato-for.ct-codigo
           contrato-for.log-libera          = YES
           contrato-for.dec-2               = tt-imp-contrato-for.val-total
           .

    FIND FIRST emitente USE-INDEX codigo
        WHERE emitente.cod-emitente = tt-imp-contrato-for.cod-emitente
        NO-LOCK NO-ERROR.

    IF AVAIL emitente THEN DO:
        ASSIGN contrato-for.cod-transp        = emitente.cod-transp.
        
        FIND FIRST transporte WHERE
                   transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
        IF AVAIL transporte THEN
            ASSIGN contrato-for.via-transp = transporte.via-transp.

    END.     
            
    RUN pi-geraPedidoContrato.

END PROCEDURE. 


//gera as informa‡äes os Itens do Contrto 
PROCEDURE pi-geraItemContrato.

/*     MESSAGE "Esto na Pi-geraItemContrato"  */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    FIND FIRST ITEM USE-INDEX codigo WHERE ITEM.it-codigo = tt-imp-item-contrato.it-codigo NO-LOCK NO-ERROR.
    
    /*item-contrat*/
    CREATE item-contrat.
    ASSIGN item-contrat.cod-emitente            = tt-imp-contrato-for.cod-emitente
           item-contrat.nr-contrato             = tt-imp-contrato-for.nr-contrato
           item-contrat.preco-unit              = tt-imp-item-contrato.preco-unit
           item-contrat.qtd-minima              = tt-imp-item-contrato.qtd-minima
           item-contrat.sld-val                 = 0 /**/
           item-contrat.mo-codigo               = tt-imp-item-contrato.mo-codigo
           item-contrat.it-codigo               = tt-imp-item-contrato.it-codigo
           item-contrat.val-total               = 0                                                                             //tt-imp-item-contrato.val-total
           item-contrat.cod-refer               = (IF AVAIL ITEM THEN ITEM.cod-refer ELSE "")
           item-contrat.codigo-ipi              = (IF tt-imp-item-contrato.ipi-incluso = "S" THEN YES ELSE NO)
           item-contrat.codigo-icm              = 0                                                                             //tt-imp-item-contrato.codigo-icm
           item-contrat.un                      = "un"                                                                          // tt-imp-item-contrato.un
           item-contrat.contato                 = tt-imp-item-contrato.contato
           item-contrat.num-seq-item            = tt-imp-item-contrato.num-seq-item
           item-contrat.frequencia              = 1 /*Parametro*/
           item-contrat.qtd-total               = 0                                                                             //tt-imp-item-contrato.qtd-total
           item-contrat.ind-un-contrato         = 1                                                                             //tt-imp-item-contrato.ind-un-contrato
           item-contrat.sld-qtd                 = 0                                                                             //tt-imp-item-contrato.sld-qtd
           item-contrat.acum-rec-val            = 0                                                                             //tt-imp-item-contrato.acum-rec-val
           item-contrat.log-control-event       = YES /*Parametro*/
           item-contrat.acum-rec-qtd            = tt-imp-item-contrato.acum-rec-qtd
           item-contrat.log-obrig-item          = YES /*Parametro*/
           item-contrat.narrat-item             = (IF AVAIL ITEM THEN ITEM.narrativa ELSE "Ariba")
           item-contrat.log-ind-multa           = YES /*Parametro*/
           item-contrat.perc-multa-dia          = 0
           item-contrat.perc-multa-limite       = 0
           item-contrat.cod-depos               = (IF AVAIL ITEM THEN ITEM.deposito-pad ELSE "ALM")
           item-contrat.aliquota-icm            = 0                                                                             //tt-imp-item-contrato.aliquota-icm
           item-contrat.aliquota-ipi            = 0                                                                             //tt-imp-item-contrato.aliquota-ipi
           item-contrat.tp-despesa              = 999                                                                           //Conforme solicitado pela Marta (Camil)                                                                           //tt-imp-item-contrato.tp-despesa
           item-contrat.cod-cond-pag            = IF tt-imp-contrato-for.cod-cond-pag = 0 OR tt-imp-contrato-for.cod-cond-pag = ? THEN 30 ELSE tt-imp-contrato-for.cod-cond-pag                                                                    //tt-imp-item-contrato.cod-cond-pag
           item-contrat.preco-fornec            = tt-imp-item-contrato.preco-fornec
           item-contrat.val-frete               = 0                                                                             //tt-imp-item-contrato.val-frete
           item-contrat.prazo-ent               = 0                                                                             //tt-imp-item-contrato.prazo-ent
           item-contrat.preco-base              = tt-imp-item-contrato.preco-base
           item-contrat.cod-comprado            = "01MDGCUNHA"
           item-contrat.perc-desconto           = 0                                                                             //tt-imp-item-contrato.perc-desconto
           item-contrat.narrat-compra           = tt-imp-item-contrato.narrat-compra
           item-contrat.pre-unit-for            = tt-imp-item-contrato.pre-unit-for
           item-contrat.sld-qtd-receb           = tt-imp-item-contrato.sld-qtd-receb
           item-contrat.sld-val-receb           = tt-imp-item-contrato.sld-val-receb
           item-contrat.log-libera              = YES
           item-contrat.frete                   = YES
           .

          IF ITEM.tipo-contr = 1 OR ITEM.tipo-contr = 3 THEN DO:

                /*Matriz-rat-item*/
                CREATE matriz-rat-item.
                ASSIGN matriz-rat-item.nr-contrato          = int(tt-imp-contrato-for.nr-contrato)
                       matriz-rat-item.num-seq-item         = tt-imp-item-contrato.num-seq-item
                       matriz-rat-item.it-codigo            = tt-imp-item-contrato.it-codigo
                       matriz-rat-item.ct-codigo            = tt-imp-item-contrato.ct-codigo
                       matriz-rat-item.sc-codigo            = tt-imp-item-contrato.sc-codigo
                       matriz-rat-item.perc-rateio          = tt-imp-item-contrato.perc-rateio
                       matriz-rat-item.cod-unid-negoc       = tt-imp-item-contrato.cod-unid-negoc
                       matriz-rat-item.conta-contabil       = tt-imp-item-contrato.ct-codigo +  tt-imp-item-contrato.sc-codigo.
          END. 

END PROCEDURE.


PROCEDURE pi-geraMatrizRateioContrato. // 16/07/2019 - Leandro Policarpo

     MESSAGE ">>>>###-PI-PROCESSA-GERANDO MATRIZ 2".

                /*Matriz Rateia Contrato */
                CREATE matriz-rat-contr.
                ASSIGN matriz-rat-contr.nr-contrato    = tt-imp-matriz-rat-contrato.nr-contrato     
                       matriz-rat-contr.cod-unid-negoc = tt-imp-matriz-rat-contrato.cod-unid-negoc  
                       matriz-rat-contr.ct-codigo      = tt-imp-matriz-rat-contrato.ct-codigo       
                       matriz-rat-contr.sc-codigo      = tt-imp-matriz-rat-contrato.sc-codigo       
                       matriz-rat-contr.perc-rateio    = tt-imp-matriz-rat-contrato.perc-rateio
                       matriz-rat-contr.conta-contabi  = tt-imp-matriz-rat-contrato.ct-codigo + tt-imp-matriz-rat-contrato.sc-codigo.

END PROCEDURE.



PROCEDURE pi-geraPedidoContrato.


    FOR EACH ttEstabelecimento NO-LOCK.
   

            DEFINE VARIABLE i-num-pedido AS INTEGER NO-UNDO.
         
             ASSIGN i-num-pedido = 0.
         
             IF NOT VALID-HANDLE(h-boin295) THEN
             RUN inbo/boin295.p PERSISTENT SET h-boin295.
         
             RUN geraNumeroPedidoCompra IN h-boin295 (OUTPUT i-num-pedido).
         
             IF VALID-HANDLE(h-boin295) THEN DO:       
                 DELETE PROCEDURE h-boin295.           
                 ASSIGN h-boin295 = ?.                 
             END. 
         
             CREATE pedido-compr.
             assign pedido-compr.num-pedido    = i-num-pedido  
                    pedido-compr.natureza      = 1
                    pedido-compr.cod-emitente  = tt-imp-contrato-for.cod-emitente
                    pedido-compr.cod-emit-terc = tt-imp-contrato-for.cod-emitente
                    pedido-compr.cod-cond-pag = IF tt-imp-contrato-for.cod-cond-pag = 0 OR tt-imp-contrato-for.cod-cond-pag = ?  THEN 30 ELSE tt-imp-contrato-for.cod-cond-pag
                    pedido-compr.data-pedido  = TODAY
                    pedido-compr.situacao     = 2 //NÆo Impresso
                    pedido-compr.responsavel  = "01mdgcunha"
                    pedido-compr.end-entrega  = ttEstabelecimento.cod-estabel //tt-imp-contrato-for.cod-estabel
                    pedido-compr.end-cobranca = ttEstabelecimento.cod-estabel // tt-imp-contrato-for.cod-estabel 
                    pedido-compr.frete        = 1 //Pago      
                    pedido-compr.cod-transp   = 99999  
                    pedido-compr.via-transp   = 1      
                    pedido-compr.cod-mensagem = 100    
                    pedido-compr.impr-pedido  = YES    
                    pedido-compr.emergencial  = YES    
                    pedido-compr.nr-prox-ped  = 1
                    pedido-compr.contr-forn   = NO
                    pedido-compr.nr-processo  = 0
                    pedido-compr.cod-estabel  = ttEstabelecimento.cod-estabel //IF tt-imp-contrato-for.cod-estabel = "" OR tt-imp-contrato-for.cod-estabel = ? THEN "01" ELSE tt-imp-contrato-for.cod-estabel
                    pedido-compr.nr-contrato  = tt-imp-contrato-for.nr-contrato. 
         
             RELEASE pedido-compr.
         
             CREATE es-contrato-ariba-totvs.
             ASSIGN es-contrato-ariba-totvs.nr-contrato       = tt-imp-contrato-for.nr-contrato
                    es-contrato-ariba-totvs.nr-pedido-tovs    = i-num-pedido
                    es-contrato-ariba-totvs.nr-contrato-ariba = string(tt-imp-contrato-for.des-contrat)
                    es-contrato-ariba-totvs.cod-estabel       =  tt-imp-contrato-for.cod-estabel //ttEstabelecimento.cod-estabel .
                .
              RELEASE es-contrato-ariba-totvs.

        END.

END PROCEDURE. 

PROCEDURE pi-email.
   // DEFINE INPUT PARAM mail          AS CHAR.

    RUN utp/utapi019.p PERSISTENT SET h-utapi019.


    FOR LAST es-contrato-ariba-totvs NO-LOCK.

        CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
               tt-envio2.servidor          = "nemesis.camil.com.br" 
               tt-envio2.porta             = 25
               tt-envio2.remetente         = "policarpo.leandro@totvspartners.com.br"
               tt-envio2.destino           = "carlos.gonzaga@totvs.com.br"
               tt-envio2.copia             = ""
               tt-envio2.assunto           =  Assunto + " : "  + " " + string(es-contrato-ariba-totvs.nr-contrato) + " " + STRING(es-contrato-ariba-totvs.nr-pedido-tovs) + " " + es-contrato-ariba-totvs.nr-contrato-ariba.
               tt-envio2.formato           = "HTML".

               CREATE tt-mensagem.
               ASSIGN tt-mensagem.seq-mensagem = 1
                      tt-mensagem.mensagem     = "Prezados Sr(a)s<BR><BR>".

               CREATE tt-mensagem.
               ASSIGN tt-mensagem.seq-mensagem = 2
                      tt-mensagem.mensagem     =  "<BR><b>E-mail gerado automaticamente</b>".


                   RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                                  INPUT  TABLE tt-mensagem,
                                                  OUTPUT TABLE tt-erros).

                   IF RETURN-VALUE = "NOK" THEN 
                     DO: 
                          FOR EACH tt-erros:
                              //MESSAGE tt-erros.
                         END.                               
                     END.
                 END.

    END PROCEDURE. 



PROCEDURE pi-valida
    .
EMPTY TEMP-TABLE ttError.
EMPTY TEMP-TABLE tt-retorno-nok.

IF CAN-FIND (FIRST tt-imp-contrato-for) THEN DO:
    FOR FIRST tt-imp-contrato-for.
       /*INCLUS€O*/
        ASSIGN tt-imp-contrato-for.ind-tipo-movto = 1.

        // Validar a opera»’o
        CASE tt-imp-contrato-for.ind-tipo-movto:
            WHEN 1 THEN DO: // Inclusao de registro
                // Verificar se o Contrato Fornecimento jÿ existe cadastrado 
                IF tt-imp-contrato-for.nr-contrato  <> 0 THEN DO:
                    FIND FIRST contrato-for NO-LOCK
                         WHERE contrato-for.nr-contrato = tt-imp-contrato-for.nr-contrato NO-ERROR.

                    IF AVAIL contrato-for THEN DO:
                        CREATE ttError.
                        ASSIGN //ttError.SessionId   = p-session 
                               ttError.referencia  = "TOTVS"
                               ttError.codigo      = "1"
                               ttError.descricao   = "Contrato de Compra jÿ existe com o nœmero " + STRING(tt-imp-contrato-for.nr-contrato).
                    END.

                    //Valida o C¢digo do Fornecedor
                    FIND FIRST emitente NO-LOCK
                        WHERE emitente.cod-emitente = tt-imp-contrato-for.cod-emitente NO-ERROR.

                    IF NOT AVAIL emitente THEN DO: 
                        CREATE ttError.
                        ASSIGN //ttError.SessionId   = p-session 
                               ttError.referencia  = "TOTVS"
                               ttError.codigo      = "1"
                               ttError.descricao   = "Codigo do Fornecedor nÆo encontrato " + STRING(tt-imp-contrato-for.cod-emitente).
                    END.

                    //Valida se o cotrato do Ariba possui relacionamento com Totvs
                    FIND FIRST es-contrato-ariba-totvs
                        WHERE  es-contrato-ariba-totvs.nr-contrato-ariba = string(tt-imp-contrato-for.des-contrat) NO-ERROR.

                    IF AVAIL es-contrato-ariba-totvs THEN DO:
                        CREATE ttError.
                        ASSIGN ttError.referencia = "TOTVS"
                               ttError.codigo     = "1"
                               ttError.descricao  = "Contrato de Compra Ariba j existe com relacionamento :" + STRING(es-contrato-ariba-totvs.nr-contrato).
                    END.

                    //Verifica se a contat cont bil ‚ valida ************************

                    
                    //Verifica se o Centro de Custo ‚ Valido ************************
                    

                    //VErifica se a Unidade de Neg¢cio ‚ valida *********************
                    //Table unid-negoc field: cod-unid-neg 

              

                    //Verifica se o Item do Contrato ‚ valido
                    FIND FIRST ITEM 
                        WHERE ITEM.it-codigo = tt-imp-item-contrato.it-codigo 
                          AND item.cod-obsoleto <> 1 NO-ERROR.

                    IF NOT AVAIL ITEM THEN DO:
                        CREATE ttError.
                        ASSIGN ttError.referencia = "TOTVS"
                               ttError.codigo     = "1"
                               ttError.descricao  = "Item nao cadastado ou obsoleto  :" + STRING(tt-imp-item-contrato.it-codigo).
                    END.

                    //Verifica se o Dep¢sito inforado esta cadastrado 
                    FIND FIRST deposito 
                        WHERE deposito.cod-depos = "01" /* tt-imp-item-contrato.cod-depos */ NO-ERROR.

                    IF NOT AVAIL deposito THEN DO:
                        CREATE ttError.
                        ASSIGN ttError.referencia = "Totvs"
                               ttError.codigo     = "1"
                               ttError.descricao  = " Deposito nÆo encontrto ".
                    END.

                    //Verificar a Contato Contabil **********************


                    //Verifica se O Item esta relacionado ao Fornecedor 
                    FIND FIRST item-fornec
                        WHERE item-fornec.it-codigo    = tt-imp-item-contrato.it-codigo 
                          AND item-fornec.cod-emitente = tt-imp-item-contrato.cod-emitente NO-ERROR. 

                   IF NOT AVAIL item-fornec THEN DO:

                        CREATE ttError.  
                        ASSIGN ttError.referencia = "Totvs"    
                               ttError.codigo     = "1"
                               ttError.descricao  = "Item nÆo Relacionado ao Fonrecedor". 
                       .
                    END.
                    
                    //Verifica se o Item est  vinculado ao Fornecedor x Estabelecimento 
                    FIND first item-fornec-estab 
                         where item-fornec-estab.it-codigo    = tt-imp-item-contrato.it-codigo
                           AND item-fornec-estab.cod-emitente = tt-imp-contrato-for.cod-emitente
                           AND item-fornec-estab.cod-estabel  = tt-imp-contrato-for.cod-estabel
                           AND item-fornec-estab.ativo.
                     IF NOT AVAIL item-fornec-estab THEN DO:

                         CREATE ttError.
                         ASSIGN ttError.referencia = "Totvs"
                                ttError.codigo     = "1"
                                ttError.descricao  = "Nao encontrado relacionamento Item x Fornecedor ".
                         
                     END.

                END.
            END.
            WHEN 2 THEN DO:
                FIND FIRST contrato-for NO-LOCK
                    WHERE contrato-for.nr-contrato = tt-imp-contrato-for.nr-contrato NO-ERROR.

                IF NOT AVAIL contrato-for THEN DO:
                    CREATE ttError.
                    ASSIGN //ttError.SessionId   = p-session 
                           ttError.referencia  = "TOTVS"
                           ttError.codigo      = "2"
                           ttError.descricao   = "Contrato de Compra n’o localizada com o nœmero " + STRING(tt-imp-contrato-for.nr-contrato).
                    
                END.
            END.
            WHEN 3 THEN DO:
                FIND FIRST contrato-for NO-LOCK
                    WHERE contrato-for.nr-contrato = tt-imp-contrato-for.nr-contrato NO-ERROR.
                
                IF NOT AVAIL pedido-compr THEN DO:
                    
                    CREATE ttError.
                    ASSIGN //ttError.SessionId   = p-session 
                           ttError.referencia  = "TOTVS"
                           ttError.codigo      = "3"
                           ttError.descricao   = "Contrato de Compra n’o localizada com o nœmero " + STRING(tt-imp-contrato-for.nr-contrato).
                    
                END.
            END.
            OTHERWISE DO:
                CREATE ttError.
                ASSIGN //ttError.SessionId   = p-session 
                       ttError.referencia  = "TOTVS"
                       ttError.codigo      = "4"
                       ttError.descricao   = "N’o identificada a opera»’o a ser realizada na TAG <ind-tipo-movto>.".    
            END.
        END CASE.

        // Caso n’o localize os erros, processar a integra»’o 
        IF NOT CAN-FIND(FIRST ttError) THEN DO:

            RUN pi-processa.
        END.
        ELSE DO:
            
            CREATE tt-retorno-nok.
            ASSIGN tt-retorno-nok.data        = NOW
                   tt-retorno-nok.cod-erro    = INT(tterror.codigo)
                   tt-retorno-nok.desc-erro   = ttError.descricao
                   tt-retorno-nok.sequencia   = 1
                   tt-retorno-nok.UniqueName  = string(tt-imp-contrato-for.nr-contrato)
                                   .

        END.
    END.
END.
ELSE DO:   
    CREATE ttError.
    ASSIGN //ttError.SessionId   = p-session 
           ttError.referencia  = "TOTVS"
           ttError.codigo      = "5"
           ttError.descricao   = "Dados invalidos no XML/JSON".
END.
END PROCEDURE.


PROCEDURE pi-log-erros-geral.
    FOR EACH tt-erros-geral NO-LOCK.
    CREATE ttError.
    ASSIGN //ttError.SessionId   = p-session 
           ttError.referencia  = "TOTVS"
           ttError.codigo      = STRING(tt-erros-geral.cod-erro)
           ttError.descricao   = tt-erros-geral.des-erro.
           //p-erro              = TRUE. 
END.
END PROCEDURE.












