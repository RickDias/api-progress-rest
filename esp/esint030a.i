
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

    FOR EACH tt-imp-contrato-for EXCLUSIVE-LOCK:
        CASE tt-imp-contrato-for.ind-tipo-movto:
            WHEN 1 THEN RUN pi-geraContratoFor.
            WHEN 2 THEN RUN pi-geraContratoFor.
            WHEN 3 THEN RUN pi-geraContratoFor.
        END CASE.

         FOR EACH tt-imp-item-contrato                                                                
             WHERE tt-imp-item-contrato.nr-contrato = tt-imp-contrato-for.nr-contrato EXCLUSIVE-LOCK: 
                                                                                                      
             CASE tt-imp-contrato-for.ind-tipo-movto:                                                 
                 WHEN 1 THEN RUN pi-geraItemContrato.                                                 
                 WHEN 2 THEN RUN pi-geraItemContrato.                                                 
                 WHEN 3 THEN RUN pi-geraItemContrato.                                                 
             END CASE.                                                                                
                                                                                            
         END.                                                                                         
    END.  
END.
END PROCEDURE.


PROCEDURE pi-geraContratoFor.
 
    CREATE contrato-for.
    ASSIGN contrato-for.nr-contrato         = INT(tt-imp-contrato-for.nr-contrato)
           contrato-for.des-contrat         = "SAP ARIBA : " + string(tt-imp-contrato-for.des-contrat)
           contrato-for.cod-emitente        = tt-imp-contrato-for.cod-emitente
           contrato-for.dt-contrato         = TODAY //tt-imp-contrato-for.dt-contrato
           contrato-for.dt-ini-validade     = tt-imp-contrato-for.dt-ini-validade
           contrato-for.dt-ter-validade     = tt-imp-contrato-for.dt-ter-validade
           contrato-for.cod-comprado        = "01MDGCUNHA"
           contrato-for.cod-cond-pag        = IF tt-imp-contrato-for.cod-cond-pag = 0 THEN 1 ELSE tt-imp-contrato-for.cod-cond-pag
           contrato-for.via-transp          = 1 /*1-Rodoviÿrio*/
           contrato-for.cod-transp          = 0 
           contrato-for.tp-fornecim         = 1 /*1-reposicao*/
           contrato-for.frete               = 1 /*1-Pago ou 2-A Pagar*/
           contrato-for.natureza            = 1 /*1-Compra*/
           contrato-for.moeda               = tt-imp-contrato-for.mo-codigo
           contrato-for.cod-estabel         = "01"
           contrato-for.contato             = tt-imp-contrato-for.contato
           contrato-for.impr-contrat        = YES 
           contrato-for.cod-tipo-contrat    = 01
           contrato-for.gestor-tecnico      = "01mdgcunha"     //usu ria padrÆo de GestÆo de Contrato.
           contrato-for.variacao-qtd        = 0
           contrato-for.variacao-preco      = 0
           contrato-for.cod-mensagem        = 100
           contrato-for.cod-estab-orig      = "03"
           contrato-for.cod-estab-cobr      = "10"
           contrato-for.val-total           = tt-imp-contrato-for.val-total
           contrato-for.cod-estab-entr      = "01"
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
           contrato-for.mo-codigo           = 1                                                                                                     //tt-imp-contrato-for.mo-codigo
/*         contrato-for.log-libera          = tt-imp-contrato-for.log-libera  */
           contrato-for.sld-qtd-med         = 0                                                                                                     //tt-imp-contrato-for.sld-qtd-med
           contrato-for.sal-qtd-liber-med   = 0                                                                                                     //tt-imp-contrato-for.sal-qtd-liber-med
           contrato-for.sld-val-med         = 0                                                                                                     //tt-imp-contrato-for.sld-val-med
           contrato-for.sld-val-liber-med   = 0                                                                                                     //tt-imp-contrato-for.sld-val-liber-med
           contrato-for.cod-projeto         = string(ttj-contrato-for.nr-contrato) //tt-imp-contrato-for.cod-projeto
           contrato-for.ind-sit-contrat     = tt-imp-contrato-for.ind-sit-contrat
           contrato-for.narrat-contrat      = tt-imp-contrato-for.narrat-contrat
           contrato-for.ind-preco           = 1
           contrato-for.sc-codigo           = tt-imp-contrato-for.sc-codigo
           contrato-for.ct-codigo           = tt-imp-contrato-for.ct-codigo
           .

    IF tt-imp-contrato-for.log-libera = "S" THEN DO:
        ASSIGN contrato-for.log-libera = YES.
    END.
    ELSE DO:
        ASSIGN contrato-for.log-libera = NO.
    END.

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

    /*matriz-rat-contr*/
    CREATE matriz-rat-contr.
    ASSIGN matriz-rat-contr.nr-contrato         = tt-imp-contrato-for.nr-contrato
           matriz-rat-contr.ct-codigo           = tt-imp-contrato-for.ct-codigo
           matriz-rat-contr.sc-codigo           = tt-imp-contrato-for.sc-codigo
           matriz-rat-contr.perc-rateio         = tt-imp-contrato-for.perc-rateio
           matriz-rat-contr.cod-unid-negoc      = tt-imp-contrato-for.cod-unid-negoc.

  //  RUN pi-geraPedidoContrato.

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
           item-contrat.narrat-item             = (IF AVAIL ITEM THEN ITEM.narrativa ELSE "")
           item-contrat.log-ind-multa           = YES /*Parametro*/
           item-contrat.perc-multa-dia          = 0
           item-contrat.perc-multa-limite       = 0
           item-contrat.cod-depos               = (IF AVAIL ITEM THEN ITEM.deposito-pad ELSE "")
           item-contrat.aliquota-icm            = 0                                                                             //tt-imp-item-contrato.aliquota-icm
           item-contrat.aliquota-ipi            = 0                                                                             //tt-imp-item-contrato.aliquota-ipi
           item-contrat.tp-despesa              = 730                                                                           //tt-imp-item-contrato.tp-despesa
           item-contrat.cod-cond-pag            = 10                                                                            //tt-imp-item-contrato.cod-cond-pag
           item-contrat.preco-fornec            = tt-imp-item-contrato.preco-fornec
           item-contrat.val-frete               = 0                                                                             //tt-imp-item-contrato.val-frete
           item-contrat.prazo-ent               = 0                                                                             //tt-imp-item-contrato.prazo-ent
           item-contrat.preco-base              = tt-imp-item-contrato.preco-base
           item-contrat.cod-comprado            = "01MDGCUNHA"
           item-contrat.perc-desconto           = 0                                                                             //tt-imp-item-contrato.perc-desconto
           item-contrat.narrat-compra           = tt-imp-item-contrato.narrat-compra
           item-contrat.pre-unit-for            = tt-imp-item-contrato.pre-unit-for
           item-contrat.sld-qtd-receb           = tt-imp-item-contrato.sld-qtd-receb
           item-contrat.sld-val-receb           = tt-imp-item-contrato.sld-val-receb.

    IF tt-imp-item-contrato.log-libera = "S" THEN DO:
        ASSIGN item-contrat.log-libera = YES.
    END.
    ELSE DO:
        ASSIGN item-contrat.log-libera = NO.
    END.

    IF tt-imp-item-contrato.frete = "S" THEN DO:
        ASSIGN item-contrat.frete = YES.
    END.
    ELSE DO:
        ASSIGN item-contrat.frete = NO.
    END.

    /*Matriz-rat-item*/
    CREATE matriz-rat-item.
    ASSIGN matriz-rat-item.nr-contrato          = tt-imp-contrato-for.nr-contrato
           matriz-rat-item.num-seq-item         = tt-imp-item-contrato.num-seq-item
           matriz-rat-item.it-codigo            = tt-imp-item-contrato.it-codigo
           matriz-rat-item.ct-codigo            = tt-imp-item-contrato.ct-codigo
           matriz-rat-item.sc-codigo            = tt-imp-item-contrato.sc-codigo
           matriz-rat-item.perc-rateio          = tt-imp-item-contrato.perc-rateio
           matriz-rat-item.cod-unid-negoc       = tt-imp-item-contrato.cod-unid-negoc.
END PROCEDURE.


PROCEDURE pi-geraPedidoContrato.

    DEFINE VARIABLE i-num-pedido AS INTEGER NO-UNDO.

    ASSIGN i-num-pedido = 0.

      IF NOT VALID-HANDLE(h-boin295) THEN
        RUN inbo/boin295.p PERSISTENT SET h-boin295.

       RUN geraNumeroPedidoCompra IN h-boin295 (OUTPUT i-num-pedido).

    CREATE tt-pedido-compr.
    ASSIGN tt-pedido-compr.num-pedido              =  i-num-pedido
           tt-pedido-compr.nr-contrato             =  int(tt-imp-contrato-for.nr-contrato)
           tt-pedido-compr.char-2                  =  STRING(tt-imp-contrato-for.nr-contrato)
           tt-pedido-compr.cod-estabel             =  tt-imp-contrato-for.cod-estabel
           tt-pedido-compr.cod-emitente            =  INT(tt-imp-contrato-for.cod-emitente)
           tt-pedido-compr.cod-emit-terc           =  INT(tt-imp-contrato-for.cod-emitente)
           tt-pedido-compr.cod-cond-pag            =  INT(tt-imp-contrato-for.cod-cond-pag)
        //   tt-pedido-compr.natureza                =  tt-imp-contrato-for.natureza     
           tt-pedido-compr.data-pedido             =  TODAY  
           tt-pedido-compr.situacao                =  2                                        /*FIXO*/
           tt-pedido-compr.responsavel             =  "01mdgcunha" //tt-imp-pedido-compr.responsavel  
           tt-pedido-compr.impr-pedido             =  YES
           tt-pedido-compr.comentarios             =  ""  
           tt-pedido-compr.mot-elimina             =  "" //tt-imp-pedido-compr.mot-elimina  
           tt-pedido-compr.emergencial             =  YES
           tt-pedido-compr.contr-forn              =  YES //string(tt-imp-contrato-for.nr-contrato)   
           tt-pedido-compr.compl-entrega           =  ""
           tt-pedido-compr.end-entrega             =  ""  
           tt-pedido-compr.end-cobranca            =  "" 
           tt-pedido-compr.cod-mensagem            =  100.                         /*VERIFICA*/

    FOR FIRST emitente WHERE
              emitente.cod-emitente = INT(tt-imp-contrato-for.cod-emitente) NO-LOCK:

        ASSIGN tt-pedido-compr.cod-transp     = emitente.cod-transp.
               //tt-imp-pedido-compr.cod-transp = emitente.cod-transp.

        FIND FIRST transporte WHERE
                   transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
        IF AVAIL transporte THEN

            ASSIGN tt-pedido-compr.via-transp = transporte.via-transp.

    END.

//    ASSIGN tt-imp-pedido-compr.num-pedido-totvs = i-num-pedido.
//‚ preciso chara a bo para criar o pedido de Compras
    
    IF VALID-HANDLE(h-boin295) THEN DO:
        DELETE PROCEDURE h-boin295.
        ASSIGN h-boin295 = ?.
    END.
END PROCEDURE. 



PROCEDURE pi-valida.
EMPTY TEMP-TABLE ttError.

IF CAN-FIND (FIRST tt-imp-contrato-for) THEN DO:
    FOR FIRST tt-imp-contrato-for.
       /*INCLUS€O*/
        ASSIGN tt-imp-contrato-for.ind-tipo-movto = 1.

        // Validar a opera»’o
        CASE tt-imp-contrato-for.ind-tipo-movto:
            WHEN 1 THEN DO: // Inclusao de registro
                // Verificar se o codigo do emitente jÿ existe cadastrado 
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

            //RUN pi-02-processa.
            RUN pi-processa.
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


//Verifico se o contrato j  existe o contrato
PROCEDURE pi-consulta-contrato.
    

END.

//Consulta se o Contrato j  Existe
FUNCTION ft-consulto-contrato RETURNS CHARACTER (INPUT nr-contrato AS CHARACTER).

    find first contrato-for where 
               contrato-for.nr-contrato = int(nr-contrato) EXCLUSIVE-LOCK no-error.
       if avail contrato-for then do:
                
           assign l-com-problema = yes
                      l-prob[48] = YES.

                ASSIGN nr-contrato = "YES".

       end.
       ELSE  nr-contrato = "NO".

             RETURN nr-contrato.
END.

//Consulta o Cadastro do Emitente: 
FUNCTION ft-cons-emitente RETURNS CHARACTER (INPUT nr-emitente AS CHARACTER).

    FIND FIRST emitente WHERE 
        emitente.cod-emitente = INT(nr-emitente) EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN DO:
         nr-emitente = "NO".
    END.

    RETURN nr-emitente.

END.













