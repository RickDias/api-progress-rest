
/*----------------------------------------------------------------------------------------------/
 Programa..: esintt030.i
 Objetivo..: Interface Integra‡Æo Contrto de Fornecedores 
 Data......: 
 Autor.....: 
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/
/* ------- Importa‡Æo de Classes ------ */

DEFINE BUFFER estabelec-entrega     FOR estabelec.
DEFINE BUFFER estabelec-faturamento FOR estabelec.


DEFINE VARIABLE i-num-contrato LIKE contrato-for.nr-contrato NO-UNDO.

DEFINE VARIABLE h-boin295   AS HANDLE     NO-UNDO.


DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

DEFINE TEMP-TABLE ttError      SERIALIZE-NAME "Retorno"
    FIELD SessionId         AS CHARACTER
    FIELD referencia        AS CHARACTER
    FIELD codigo            AS CHARACTER
    FIELD descricao         AS CHARACTER
    INDEX idx01 referencia codigo.




/*********************************************************************************
 definicao das temp-tables para resgatar os atributos do json - inicio
*********************************************************************************/
DEFINE TEMP-TABLE ttCapaContrato NO-UNDO SERIALIZE-NAME "ContratoFornecedor"
    FIELD des-contrat           AS CHARACTER SERIALIZE-NAME "nr-contrato"
    FIELD cod-emitente          AS CHARACTER SERIALIZE-NAME "cod-emitentte"
    FIELD dt-ini-validade       AS CHARACTER SERIALIZE-NAME "dt-ini-validade"
    FIELD dt-ter-validade       AS CHARACTER SERIALIZE-NAME "dt-ter-validade"
    FIELD cod-comprado          AS CHARACTER SERIALIZE-NAME "cod-comprado"
    FIELD cod-cond-pag          AS CHARACTER SERIALIZE-NAME "cod-cond-pag"  
    FIELD cod-estabel           AS CHARACTER SERIALIZE-NAME "cod-estabel "  
    FIELD val-total             AS CHARACTER SERIALIZE-NAME "val-total"  
    FIELD val-fatur-minimo      AS CHARACTER SERIALIZE-NAME "val-fatur-minimo"
    FIELD acum-val-pago         AS CHARACTER SERIALIZE-NAME "acum-val-pago"
    FIELD mo-codigo             AS CHARACTER SERIALIZE-NAME "mo-codigo".



DEFINE TEMP-TABLE ttItensContrato NO-UNDO SERIALIZE-NAME "ItensContrato"
    FIELD nr-contrato   AS CHARACTER SERIALIZE-NAME "nr-contrato"   
    FIELD cod-emitente  AS CHARACTER SERIALIZE-NAME "cod-emitente" 
    FIELD preco-unit    AS CHARACTER SERIALIZE-NAME "preco-unit "   
    FIELD it-codigo     AS CHARACTER SERIALIZE-NAME "it-codigo  "   
    FIELD narrat-item   AS CHARACTER SERIALIZE-NAME "narrat-item"   
    FIELD preco-fornec  AS CHARACTER SERIALIZE-NAME "preco-fornec" 
    FIELD val-frete     AS CHARACTER SERIALIZE-NAME "val-frete  "   
    .
    
/*********************************************************************************
 definicao das temp-tables para resgatar os atributos do json - fim
*********************************************************************************/



DEFINE TEMP-TABLE ttj-contrato-for  NO-UNDO   //alimenta a informa‡Æo do que vem do json para alimentar a temp-table para tt-imp-contrato-for 
    LIKE contrato-for.

DEFINE TEMP-TABLE ttj-itenscontrato-for NO-UNDO  //popula a tabela auxiliar do Json para os itens do Contrato.
    LIKE item-contrat.

DEFINE TEMP-TABLE ttj-Matrizitenscontrato-for NO-UNDO  //popula o valor das Matrizes de Rateio dos itens 
    LIKE matriz-rat-item. 
                                                                                                         
DEFINE TEMP-TABLE tt-versao-integr 
    FIELD cod-versao-integracaoo AS INTEGER
    FIELD ind-origem-msg         AS INTEGER.

DEFINE TEMP-TABLE tt-erros-integracao   NO-UNDO
            FIELD erro      AS CHAR
            FIELD descricao AS CHAR.

DEFINE TEMP-TABLE tt-contrato-ariba-totvs NO-UNDO 
    FIELD nr-contrato-ariba  AS CHAR 
    FIELD ped-contrato-ariba AS CHAR
    FIELD it-contrato-ariba  AS CHAR
    FIELD nr-contrato-tovs   AS CHAR
    FIELD ped-contrato-totvs AS CHAR 
    FIELD nr-ordem-tovs      AS CHAR
    FIELD item-ordem-tovs    AS CHAR.

DEFINE TEMP-TABLE tt-es-api-param-contr NO-UNDO         //‚ necess rio criar todos os campos fixos 
    FIELD cod-emitente      LIKE contrato-for.cod-emitente
    FIELD dt-contrato       LIKE contrato-for.dt-contrato
    FIELD dt-ini-validade   LIKE contrato-for.dt-ini-validade
    FIELD dt-ter-validade   LIKE contrato-for.dt-ter-validade
    FIELD cod-comprado      LIKE contrato-for.cod-comprado
    FIELD cod-cond-pag      LIKE contrato-for.cod-cond-pag
    FIELD via-transp        LIKE contrato-for.via-transp
    FIELD cod-transp        LIKE contrato-for.cod-transp
    FIELD tp-fornecim       LIKE contrato-for.tp-fornecim
    FIELD frete             LIKE contrato-for.frete
    FIELD natureza          LIKE contrato-for.natureza
    FIELD cod-mensagem      LIKE contrato-for.cod-mensagem
    FIELD moeda             LIKE contrato-for.moeda
    FIELD observacao        LIKE contrato-for.observacao
    FIELD versao            LIKE contrato-for.versao
    FIELD conta-caont       LIKE contrato-for.conta-cont
    FIELD cod-estabel       LIKE contrato-for.cod-estabel
    FIELD tp-freq           LIKE contrato-for.tp-freq
    FIELD ind-via-envio     LIKE contrato-for.ind-via-envio
    FIELD nro-proc-saida    LIKE contrato-for.nro-proc-saida
    FIELD char-1            LIKE contrato-for.char-1
    FIELD char-2            LIKE contrato-for.char-2
    FIELD dec-1             LIKE contrato-for.dec-1
    FIELD dec-2             LIKE contrato-for.dec-2
    FIELD int-1             LIKE contrato-for.int-1
    FIELD int-2             LIKE contrato-for.int-2
    FIELD log-1             LIKE contrato-for.log-1
    FIELD log-2             LIKE contrato-for.log-2
    FIELD data-1            LIKE contrato-for.data-1
    FIELD data-2            LIKE contrato-for.data-2
    FIELD contato           LIKE contrato-for.contato
    FIELD impr-contrat      LIKE contrato-for.impr-contrat
    FIELD cod-tipo-contrat  LIKE contrato-for.cod-tipo-contrat
    FIELD gestor-tecnico    LIKE contrato-for.gestor-tecnico
    FIELD variacao-qtd      LIKE contrato-for.variacao-qtd
    FIELD variacao-preco    LIKE contrato-for.variacao-preco
    FIELD cod-estab-orig    LIKE contrato-for.cod-estab-orig
    FIELD cod-estab-cobr    LIKE contrato-for.cod-estab-cobr
    FIELD val-total         LIKE contrato-for.val-total
    FIELD cod-estab-entr    LIKE contrato-for.cod-estab-entr
    FIELD qtd-total         LIKE contrato-for.qtd-total 
    FIELD sld-qtd           LIKE contrato-for.sld-qtd
    FIELD sld-val           LIKE contrato-for.sld-val
    FIELD acum-rec-qtd      LIKE contrato-for.acum-rec-qtd
    FIELD acum-rec-val      LIKE contrato-for.acum-rec-val
    FIELD sld-qtd-liber     LIKE contrato-for.sld-qtd-liber  
    FIELD sld-val-liber     LIKE contrato-for.sld-val-liber
    FIELD val-fatur-minimo  LIKE contrato-for.val-fatur-minimo
    FIELD des-contrat       LIKE contrato-for.des-contrat
    FIELD acum-val-pago     LIKE contrato-for.acum-val-pago
    FIELD motivo-cancel     LIKE contrato-for.motivo-cancel
    FIELD dat-revisao       LIKE contrato-for.dat-revisao
    FIELD mo-codigo         LIKE contrato-for.mo-codigo
    FIELD log-libera        LIKE contrato-for.log-libera
    FIELD sld-qtd-med       LIKE contrato-for.sld-qtd-med
    FIELD sal-qtd-liber-med LIKE contrato-for.sal-qtd-liber-med
    FIELD sld-val-med       LIKE contrato-for.sld-val-med 
    FIELD sld-val-liber-med LIKE contrato-for.sld-val-liber-med
    FIELD cod-projeto       LIKE contrato-for.cod-projeto
    FIELD cod-cond-fatur    LIKE contrato-for.cod-cond-fatur
    FIELD sld-val-receb     LIKE contrato-for.sld-val-receb
    FIELD ind-control-rec   LIKE contrato-for.cod-cond-fatur
    FIELD ind-sit-contrat   LIKE contrato-for.ind-sit-contrat
    FIELD narrat-contrat    LIKE contrato-for.narrat-contrat
    FIELD check-sum         LIKE contrato-for.check-sum 
    FIELD gera-edi          LIKE contrato-for.gera-edi
    FIELD cod-estab-gestor  LIKE contrato-for.cod-estab-gestor
    FIELD nr-contrato       LIKE contrato-for.nr-contrato
    FIELD nr-tab            LIKE contrato-for.nr-tab
    FIELD ind-preco         LIKE contrato-for.ind-preco
    FIELD nr-contrato-comp  LIKE contrato-for.nr-contrato-comp
    FIELD perc-alert-saldo  LIKE contrato-for.perc-alert-saldo 
    FIELD email-alert       LIKE contrato-for.email-alert 
    FIELD num-ord-inv       LIKE contrato-for.num-ord-inv 
    FIELD ep-codigo         LIKE contrato-for.num-ord-inv 
    FIELD num-ord-invest    LIKE contrato-for.num-ord-invest
    FIELD sc-codigo         LIKE contrato-for.sc-codigo
    FIELD ct-codigo         LIKE contrato-for.ct-codigo.

DEF TEMP-TABLE tt-imp-contrato-for NO-UNDO // SERIALIZE-NAME 'Contrato_Compra'
    FIELD nr-contrato           AS INT
    FIELD cod-emitente          AS INT
    FIELD des-contrat           AS CHAR
    FIELD cod-cond-pag          AS INT
    FIELD val-total             AS DEC FORMAT "->>>,>>>,>>9.9999"
    FIELD dt-contrato           AS DATE
    FIELD cod-comprador         AS CHAR
    FIELD ind-sit-contrat       AS INT 
    FIELD qtd-total             AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-qtd               AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-val               AS DEC FORMAT "->>>,>>>,>>>.9999"
    FIELD acum-rec-qtd          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD acum-rec-val          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-qtd-liber         AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-val-liber         AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD val-fatur-minimo      AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD dt-ini-validade       AS DATE
    FIELD dt-ter-validade       AS DATE
    FIELD cod-estabel           AS CHAR
    FIELD cod-estab-cobr        AS CHAR
    FIELD cod-estab-entr        AS CHAR
    FIELD acum-val-pago         AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD mo-codigo             AS INT
    FIELD log-libera            AS CHAR FORMAT "x(01)"
    FIELD sld-qtd-med           AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sal-qtd-liber-med     AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-val-med           AS DEC FORMAT "->>>,>>>,>>9.999"
    FIELD sld-val-liber-med     AS DEC FORMAT "->>>,>>>,>>9.9999"
    FIELD cod-projeto           AS CHAR
    FIELD contato               AS CHAR
    FIELD narrat-contrat        AS CHAR
    /*Codigo do Registro (MC00)*/
    FIELD ct-codigo             AS CHAR
    FIELD sc-codigo             AS CHAR
    FIELD perc-rateio           AS DEC FORMAT ">>9.99"
    FIELD cod-unid-negoc        AS CHAR
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99".

DEF TEMP-TABLE tt-imp-item-contrato NO-UNDO //SERIALIZE-NAME "Item_Contrato"
    /*Codigo do Registro (IC00)*/
    FIELD nr-contrato           AS INT
    FIELD num-seq-item          AS INT
    FIELD it-codigo             AS CHAR
    FIELD un                    AS CHAR
    FIELD preco-unit            AS DEC FORMAT ">>>,>>>,>>9.99999"
    FIELD qtd-minima            AS DEC FORMAT ">>>>,>>9.9999"
    FIELD val-fatur-minimo      AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD mo-codigo             AS INT
    FIELD log-libera            AS CHAR FORMAT "x(01)"
    FIELD val-total             AS DEC FORMAT "->>>,>>>,>>9.99999"
    FIELD ipi-incluso           AS CHAR FORMAT "x(01)"
    FIELD codigo-icm            AS INT /*1-Tributado 2-Isento 3-Reduzido 4-Outros*/
    FIELD qtd-total             AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD ind-un-contrato       AS INT
    FIELD sld-qtd               AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD acum-rec-val          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD acum-rec-qtd          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD aliquota-icm          AS DEC FORMAT ">>9.99"
    FIELD aliquota-ipi          AS DEC FORMAT ">>9.99"
    FIELD tp-despesa            AS INT
    FIELD cod-cond-pag          AS INT
    FIELD frete                 AS CHAR FORMAT "x(01)"
    FIELD contato               AS CHAR
    FIELD cod-emitente          AS INT
    /*Codigo do Registro (IC01)*/
    FIELD preco-fornec          AS DEC FORMAT ">>>>>,>>>,>>9.99999"
    FIELD val-frete             AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD prazo-ent             AS INT
    FIELD preco-base            AS DEC FORMAT ">>>>>,>>>,>>9.99999"
    FIELD cod-comprador         AS CHAR
    FIELD perc-desconto         AS DEC FORMAT ">>9.99"
    FIELD narrat-compra         AS CHAR
    FIELD pre-unit-for          AS DEC FORMAT ">>>>>,>>>,>>9.99999"
    FIELD sld-qtd-receb         AS DEC FORMAT "->>>>>,>>9.9999"
    FIELD sld-val-receb         AS DEC FORMAT "->>>>>,>>9.9999"
    FIELD narrat-item           AS CHAR
    /*Codigo do Registro (MI00)*/
    FIELD ct-codigo             AS CHAR
    FIELD sc-codigo             AS CHAR
    FIELD perc-rateio           AS DEC FORMAT ">>9.99"
    FIELD cod-unid-negoc        AS CHAR
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99".

DEFINE TEMP-TABLE tt-es-api-param-contr-it NO-UNDO      //‚ necess rio criar os campos do item do contrato
    FIELD cod-emitente      LIKE contrato-for.cod-emitente
    FIELD dt-contrato       LIKE contrato-for.dt-contrato
    FIELD dt-ini-validade   LIKE contrato-for.dt-ini-validade
    FIELD dt-ter-validade   LIKE contrato-for.dt-ter-validade
    FIELD cod-comprado      LIKE contrato-for.cod-comprado
    FIELD cod-cond-pag      LIKE contrato-for.cod-cond-pag
    FIELD via-transp        LIKE contrato-for.via-transp
    FIELD cod-transp        LIKE contrato-for.cod-transp
    FIELD tp-fornecim       LIKE contrato-for.tp-fornecim
    FIELD frete             LIKE contrato-for.frete
    FIELD natureza          LIKE contrato-for.natureza
    FIELD cod-mensagem      LIKE contrato-for.cod-mensagem
    FIELD moeda             LIKE contrato-for.moeda
    FIELD observacao        LIKE contrato-for.observacao
    FIELD versao            LIKE contrato-for.versao
    FIELD conta-caont       LIKE contrato-for.conta-cont
    FIELD cod-estabel       LIKE contrato-for.cod-estabel
    FIELD tp-freq           LIKE contrato-for.tp-freq
    FIELD ind-via-envio     LIKE contrato-for.ind-via-envio
    FIELD nro-proc-saida    LIKE contrato-for.nro-proc-saida
    FIELD char-1            LIKE contrato-for.char-1
    FIELD char-2            LIKE contrato-for.char-2
    FIELD dec-1             LIKE contrato-for.dec-1
    FIELD dec-2             LIKE contrato-for.dec-2
    FIELD int-1             LIKE contrato-for.int-1
    FIELD int-2             LIKE contrato-for.int-2
    FIELD log-1             LIKE contrato-for.log-1
    FIELD log-2             LIKE contrato-for.log-2
    FIELD data-1            LIKE contrato-for.data-1
    FIELD data-2            LIKE contrato-for.data-2
    FIELD contato           LIKE contrato-for.contato
    FIELD impr-contrat      LIKE contrato-for.impr-contrat
    FIELD cod-tipo-contrat  LIKE contrato-for.cod-tipo-contrat
    FIELD gestor-tecnico    LIKE contrato-for.gestor-tecnico
    FIELD variacao-qtd      LIKE contrato-for.variacao-qtd
    FIELD variacao-preco    LIKE contrato-for.variacao-preco
    FIELD cod-estab-orig    LIKE contrato-for.cod-estab-orig
    FIELD cod-estab-cobr    LIKE contrato-for.cod-estab-cobr
    FIELD val-total         LIKE contrato-for.val-total
    FIELD cod-estab-entr    LIKE contrato-for.cod-estab-entr
    FIELD qtd-total         LIKE contrato-for.qtd-total 
    FIELD sld-qtd           LIKE contrato-for.sld-qtd
    FIELD sld-val           LIKE contrato-for.sld-val
    FIELD acum-rec-qtd      LIKE contrato-for.acum-rec-qtd
    FIELD acum-rec-val      LIKE contrato-for.acum-rec-val
    FIELD sld-qtd-liber     LIKE contrato-for.sld-qtd-liber  
    FIELD sld-val-liber     LIKE contrato-for.sld-val-liber
    FIELD val-fatur-minimo  LIKE contrato-for.val-fatur-minimo
    FIELD des-contrat       LIKE contrato-for.des-contrat
    FIELD acum-val-pago     LIKE contrato-for.acum-val-pago
    FIELD motivo-cancel     LIKE contrato-for.motivo-cancel
    FIELD dat-revisao       LIKE contrato-for.dat-revisao
    FIELD mo-codigo         LIKE contrato-for.mo-codigo
    FIELD log-libera        LIKE contrato-for.log-libera
    FIELD sld-qtd-med       LIKE contrato-for.sld-qtd-med
    FIELD sal-qtd-liber-med LIKE contrato-for.sal-qtd-liber-med
    FIELD sld-val-med       LIKE contrato-for.sld-val-med 
    FIELD sld-val-liber-med LIKE contrato-for.sld-val-liber-med
    FIELD cod-projeto       LIKE contrato-for.cod-projeto
    FIELD cod-cond-fatur    LIKE contrato-for.cod-cond-fatur
    FIELD sld-val-receb     LIKE contrato-for.sld-val-receb
    FIELD ind-control-rec   LIKE contrato-for.cod-cond-fatur
    FIELD ind-sit-contrat   LIKE contrato-for.ind-sit-contrat
    FIELD narrat-contrat    LIKE contrato-for.narrat-contrat
    FIELD check-sum         LIKE contrato-for.check-sum 
    FIELD gera-edi          LIKE contrato-for.gera-edi
    FIELD cod-estab-gestor  LIKE contrato-for.cod-estab-gestor
    FIELD nr-contrato       LIKE contrato-for.nr-contrato
    FIELD nr-tab            LIKE contrato-for.nr-tab
    FIELD ind-preco         LIKE contrato-for.ind-preco
    FIELD nr-contrato-comp  LIKE contrato-for.nr-contrato-comp
    FIELD perc-alert-saldo  LIKE contrato-for.perc-alert-saldo 
    FIELD email-alert       LIKE contrato-for.email-alert 
    FIELD num-ord-inv       LIKE contrato-for.num-ord-inv 
    FIELD ep-codigo         LIKE contrato-for.num-ord-inv 
    FIELD num-ord-invest    LIKE contrato-for.num-ord-invest
    FIELD sc-codigo         LIKE contrato-for.sc-codigo
    FIELD ct-codigo         LIKE contrato-for.ct-codigo.


DEFINE TEMP-TABLE tt-log-import-contrato NO-UNDO
    FIELD nf-contr-ariba AS CHAR 
    FIELD pd-contr-ariba AS CHAR
    FIELD it-contr-ariba AS CHAR
    FIELD estados        AS CHAR 
    FIELD mensagem       AS CHAR.

DEFINE TEMP-TABLE tt-erros-geral 
    FIELD identif-msg        AS CHARACTER 
    FIELD num-sequencia-erro AS INTEGER
    FIELD cod-erro           AS INTEGER
    FIELD des-erro           AS CHARACTER FORMAT "X(200)"
    FIELD cod-maq-origem     AS INTEGER 
    FIELD num-processo       AS INTEGER.


/* DEFINE DATASET httContratoCompra SERIALIZE-HIDDEN  FOR tt-imp-contrato-for, tt-imp-item-contrato  */
/*     DATA-RELATION dr-ContratoCompra FOR tt-imp-contrato-for, tt-imp-item-contrato                 */
/*        RELATION-FIELDS (Nr-Contrato, Nr-Contrato)  NESTED.                                        */



