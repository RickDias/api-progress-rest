/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESREAP152 2.09.00.006 } /*** "019006" ***/

/*
&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i reap152 MRE}
&ENDIF
*/

/* ---------------------[ VERSAO ]-------------------- */
/*****************************************************************************
**
** Programa: ESREAP152.P
**
** Data....: julho de 1997      
**    
** Autor...: DATASUL S.A.
** 
** Objetivo: Atualiza item-doc-est.(Efetivaá∆o do Recebimento de Nota Fiscal)
**
** Versao..: I.00.000- Esidoro  Fabio - Geraá∆o do programa
**
** VERSAO CAMIL DO REAP152
**
*****************************************************************************/
{utp/ut-glob.i}
{rep/reapi151.i}  /*temp-table dcoum-est */
{rep/reapi151.i1} /*temp-table item-doc-est */
{cdp/cdapi150.i3} /*temp-table de vers∆o */ 
{cdp/cdapi150.i4} /*temp-table de erros */

define temp-table tt-docum-est-aux NO-UNDO LIKE tt-docum-est.
define temp-table tt-item-doc-est-aux NO-UNDO LIKE tt-item-doc-est.

DEF VAR vlo-debug AS LOG NO-UNDO INIT NO.

DEF INPUT  PARAMETER i-nr-ticket LIKE es-ticket.nr-ticket.
def input  parameter table for tt-docum-est.
DEF INPUT  PARAMETER TABLE FOR tt-docum-est-aux.
def input  parameter table for tt-item-doc-est.
def input  parameter table for tt-item-doc-est-aux.
def input-output param i-conta-msg as integer.
def input-output parameter table for tt_erros_modulo. 

def buffer b-item-doc-est for item-doc-est.
def buffer b-saldo-estoq  for saldo-estoq. 

def new global shared var r-RE0301-documento as rowid no-undo.
def new global shared var c-RE0301-origem as character no-undo.
def new global shared var c-RE0301-usuario like param-re.usuario no-undo.
def new global shared var i-ct-componente like item-doc-est.ct-codigo no-undo.
def new global shared var i-sc-componente like item-doc-est.sc-codigo no-undo.
def new global shared var l-consiste-saldo as logical initial yes no-undo.
/* Esta variavel sera utilizada exclusivamente pela DAKO para consistir o 
   saldo-estoq ou nao. */

DEF NEW GLOBAL SHARED VAR h-bodi515       AS HANDLE NO-UNDO.

def new shared var de-qtd-tot-ordem as decimal no-undo.

def shared var c-ult-ver   as character format "x(8)" no-undo.

{esp/escd9999.i3}

def var i-nr-item as int no-undo.
def var l-industria as logical no-undo.
def var l-comercio as logical no-undo.
def var l-devolucao as logical no-undo.
def var l-servicos as logical no-undo.
def var l-remessa as logical no-undo.
def var l-retorno as logical no-undo.
def var l-entrada as logical no-undo.
def var l-transf as logical no-undo.
def var l-ent-cons as logical no-undo.
def var l-sai-cons as logical no-undo.

def var l-arredonda as logical init yes no-undo.
def var l-compras as logical no-undo.
def var l-ordens as logical no-undo.
def var i-arredonda as integer no-undo.
def var i-pedido like pedido-compr.num-pedido no-undo.
def var i-parcela like prazo-compra.parcela no-undo.
def var c-serie like docum-est.serie-docto no-undo.
def var i-nro like docum-est.nro-docto no-undo.
def var c-aux like docum-est.nat-operacao no-undo.
def var c-un  like item.un no-undo.
def var de-taxa        as decimal no-undo.
def var i-seq as integer no-undo.
def var de-aux-1 like item-doc-est.quantidade no-undo.
def var de-aux-2 like de-aux-1.
def var de-aux-3 as decimal no-undo.
def var de-indice as decimal no-undo.
def var de-quant like item-doc-est.quantidade no-undo.
def var de-tot-peso like item-doc-est.peso-liquido no-undo.
def var l-resposta as logical format "SIM/NAO" init yes no-undo.
def var c-item-do-forn like item-fornec.item-do-forn no-undo.
def var c-pai like item.it-codigo no-undo.
def var de-qtd-estrut like estrutura.quant-usada.
def var i-ord-prod like ord-prod.nr-ord-prod no-undo.
def var c-mo-1 as character format "x(10)" no-undo.
def var c-mo-2 as character format "x(10)" no-undo.
def var de-mo-cot1 as decimal decimals 4 format ">>>,>>>,>>9.9999" no-undo.
def var de-mo-cot2 as decimal decimals 4 format ">>>,>>>,>>9.9999" no-undo.
def var de-aliq-icm as decimal no-undo.
def var de-conversao as decimal no-undo.
def var de-conv-aux  as decimal no-undo.
def var da-dt-conv   as date no-undo.
def var de-desc-tot-ord  as decimal no-undo.
def var de-preco-tot-ord as decimal no-undo.
def var de-desconto  as decimal no-undo.
def var l-dig-qtd as logical no-undo.

def var i-empresa like param-global.empresa-prin no-undo.
{cdp/cdcfgdis.i}

/* Variaveis para calculo dos impostos */
{rep/re9301.i04 new}

find first param-global no-lock no-error.
find first param-estoq  no-lock no-error.
find first param-compra no-lock no-error.
find first param-cq     no-lock no-error.

if param-estoq.moeda1 <> 0 then 
find moeda where moeda.mo-codigo = param-estoq.moeda1.
if  avail moeda then
    c-mo-1 = moeda.descricao.

if param-estoq.moeda2 <> 0 then 
find moeda where moeda.mo-codigo = param-estoq.moeda2.
if  avail moeda then
    c-mo-2 = moeda.descricao.

find param-re use-index ind-usu where param-re.usuario = c-RE0301-usuario no-lock no-error.
find docum-est where rowid(docum-est) = r-RE0301-documento no-lock no-error.
find natur-oper where natur-oper.nat-operacao = docum-est.nat-operacao no-lock no-error.
find emitente where emitente.cod-emit = docum-est.cod-emit no-lock no-error.
find first para-fat no-lock no-error.

/* Seta flags referentes a codigo de observacao */
{rep/re0152.i}

FIND es-ticket NO-LOCK WHERE es-ticket.nr-ticket = i-nr-ticket NO-ERROR.

find first tt-docum-est-aux where 
           tt-docum-est-aux.nro-docto    = docum-est.nro-docto
     and   tt-docum-est-aux.cod-emitente = docum-est.cod-emitente
     and   tt-docum-est-aux.serie-docto  = docum-est.serie-docto
     and   tt-docum-est-aux.nat-operacao = docum-est.nat-operacao no-lock no-error.

IF AVAIL tt-docum-est-aux THEN DO:

    assign i-nr-item = 0.                       
                                        
    IF es-ticket.peso-liq <> es-ticket.peso-desmem[1] THEN DO:
        IF es-ticket.peso-desmem[1] < es-ticket.peso-liq THEN DO:            
    
           /*RUN pi-cria-item-docum-est-comp.*/
        END.
        ELSE DO:
            RUN pi-cria-item-docum-est-dev.
        END.
    END.     
    
END.       
ELSE DO:
    find first tt-docum-est where 
               tt-docum-est.nro-docto    = docum-est.nro-docto
         and   tt-docum-est.cod-emitente = docum-est.cod-emitente
         and   tt-docum-est.serie-docto  = docum-est.serie-docto
         and   tt-docum-est.nat-operacao = docum-est.nat-operacao no-lock no-error.
    
    IF AVAIL tt-docum-est THEN DO:
        
        assign i-nr-item = 0.
        
        RUN pi-cria-item-docum-est.
    
    END.
END.

     
PROCEDURE pi-cria-item-docum-est:

    for each tt-item-doc-est where 
         tt-item-doc-est.nro-docto    = docum-est.nro-docto 
    and  tt-item-doc-est.cod-emitente = docum-est.cod-emitente
    and  tt-item-doc-est.serie-docto  = docum-est.serie-docto
    and  tt-item-doc-est.nat-operacao = docum-est.nat-operacao no-lock trans:


    find last item-doc-est {esp/escd8900.i item-doc-est docum-est} no-lock no-error.
    if  avail item-doc-est then
       assign i-seq = item-doc-est.sequencia + param-re.inc-seq.
    else            
       assign i-seq = param-re.seq-item-um.

    assign i-nr-item = i-nr-item + 1.
   
    /*****Numero de itens da nota excede o m†ximo permitido pelo faturamento 48*****/  
    if  param-global.modulo-ft and natur-oper.imp-nota and avail para-fat then do:
            if  i-nr-item > para-fat.nr-item-nota then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6342,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6342
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
      
            undo, leave.
        end.
    end. 
  
    create item-doc-est.
           
    assign item-doc-est.sequencia      = i-seq                   
           item-doc-est.aliquota-icm   = tt-item-doc-est.aliquota-icm       
           item-doc-est.aliquota-ipi   = tt-item-doc-est.aliquota-ipi       
           item-doc-est.aliquota-iss   = tt-item-doc-est.aliquota-iss       
           item-doc-est.class-fiscal   = tt-item-doc-est.class-fiscal       
           item-doc-est.cod-depos      = tt-item-doc-est.cod-depos
           item-doc-est.cod-emitente   = tt-item-doc-est.cod-emitente      
           item-doc-est.cod-refer      = tt-item-doc-est.cod-refer        
           item-doc-est.desconto[1]    = tt-item-doc-est.desconto[1]
           item-doc-est.despesas[1]    = tt-item-doc-est.despesas[1]     
           item-doc-est.dt-vali-lote   = tt-item-doc-est.dt-vali-lote   
           item-doc-est.lote           = tt-item-doc-est.lote
           item-doc-est.cod-localiz    = tt-item-doc-est.cod-localiz  
           item-doc-est.encerra-pa     = tt-item-doc-est.encerra-pa     
           item-doc-est.it-codigo      = tt-item-doc-est.it-codigo      
           item-doc-est.nat-comp       = tt-item-doc-est.nat-comp       
           item-doc-est.nat-operacao   = tt-item-doc-est.nat-operacao   
           item-doc-est.nat-of         = tt-item-doc-est.nat-operacao   
           item-doc-est.nr-ord-prod    = tt-item-doc-est.nr-ord-prod        
           item-doc-est.nr-pd-seq      = tt-item-doc-est.nr-pd-seq         
           item-doc-est.nr-pedcli      = tt-item-doc-est.nr-pedcli         
           item-doc-est.nro-comp       = tt-item-doc-est.nro-comp         
           item-doc-est.nro-docto      = tt-item-doc-est.nro-docto        
           item-doc-est.num-pedido     = tt-item-doc-est.num-pedido         
           item-doc-est.numero-ordem   = tt-item-doc-est.numero-ordem      
           item-doc-est.parcela        = tt-item-doc-est.parcela         
           item-doc-est.peso-liquido   = tt-item-doc-est.peso-liquido    
           item-doc-est.preco-total[1] = tt-item-doc-est.preco-total[1]  
           item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-unit[1]   
           item-doc-est.qt-do-forn     = tt-item-doc-est.qt-do-forn      
           item-doc-est.serie-docto    = tt-item-doc-est.serie-docto     
           item-doc-est.valor-ipi[1]   = tt-item-doc-est.valor-ipi[1]. 


    /* aqui22 - rde - 08/01/2018 - migraá∆o totvs 12 */
    ASSIGN item-doc-est.quantidade = tt-item-doc-est.qt-do-forn.
   
    IF vlo-debug THEN
    MESSAGE 'esreap152.p, criando o item-doc-est' skip(1)
        'item-doc-est.preco-total[1] = tt-item-doc-est.preco-total[1] ' item-doc-est.preco-total[1] SKIP
        'item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-unit[1]' item-doc-est.preco-unit[1] SKIP
        'item-doc-est.qt-do-forn     = tt-item-doc-est.qt-do-forn    ' item-doc-est.qt-do-forn SKIP
        'item-doc-est.quantidade' item-doc-est.quantidade




        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                                                      
    /*****Classificaá∆o fiscal n∆o cadastrada 49******/
    find classif-fisc where classif-fisc.class-fiscal = item-doc-est.class-fiscal no-lock no-error.
        if  not avail classif-fisc then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6744,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6744
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
    end. 
      
    /*****Item n∆o cadastrado 50*****/  
    find item where item.it-codigo = item-doc-est.it-codigo no-lock no-error.
        if  not avail item then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',166,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 166
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
    end.    
    else 

    /*****Ordem de produá∆o n∆o cadastrada 51*****/
    find familia where item.fm-codigo = familia.fm-codigo no-lock no-error.
    if  item-doc-est.nr-ord-prod <> 0 then do:

        find ord-prod where ord-prod.nr-ord-prod = item-doc-est.nr-ord-prod no-lock no-error.

        if  not avail ord-prod then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1879,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1879
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
        else do: 
        /*****Ordem de produá∆o n∆o pertence ao estabelecimento 52******/                
        if  ord-prod.cod-estabel = docum-est.cod-estabel then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6347,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6347
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
                       
        /*****Ordem de produá∆o j† est† finalizada 53*****/              
        if  ord-prod.estado = 7 or ord-prod.estado = 8 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1202,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1202
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
   
        /*****Ordem de manutená∆o suspensa ou terminada 54*****/                                                    
        if  param-global.modulo-mi then do:

            find ord-manut where ord-manut.nr-ord-prod = ord-prod.nr-ord-prod no-lock no-error.

            if  avail ord-manut then do:
                if  ord-manut.estado-om = 3 or ord-manut.estado-om = 4 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',5676,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 5676
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
            end. 
        end.
        end.
              
       assign item-doc-est.ct-codigo = ord-prod.ct-codigo
              item-doc-est.sc-codigo = ord-prod.sc-codigo.
       end.
    end.
          
    /*****Pedido n∆o cadastrado 55*****/  
    if  item-doc-est.num-pedido <> 0 then do:
        assign l-compras = yes.
        find pedido-compr where pedido-compr.num-pedido = item-doc-est.num-pedido no-lock no-error.
    
        if  not avail pedido-compr then do: 
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6269,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6269
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
         
        /******Pedido eliminado 56*****/ 
        if  avail pedido-compr and pedido-compr.situacao = 3 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1813,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1813
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  

        /*****Ordem invalida 57*****/                   
        if  item-doc-est.numero-ordem = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',3138,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 3138
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.

        /*****Parcela invalida 58*****/
        if  item-doc-est.parcela = 0 then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6348,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6348
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                  string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                  string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                  string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                  string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 
     
     /*****Ordem compra n∆o cadastrada 59*****/
     if  item-doc-est.numero-ordem <> 0 then do: 

         find ordem-compra where ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
         if  not avail ordem-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6349,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6349
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end.
      
        /*****Ordem de compra com situaá∆o invalida 60*****/                  
        find first ordem-compra use-index pedido where ordem-compra.num-pedido = item-doc-est.num-pedido
        and ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
        if  l-entrada and avail ordem-compra and ordem-compra.situacao <> 2 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6350,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6350
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.     
        else do:                   
            /*****Ordem de compra com situaá∆o invalida para devoluá∆o 61*****/           
            if  avail ordem-compra and ordem-compra.situacao <> 2 and 
                ordem-compra.situacao <> 6 then do:
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',6352,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 6352
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                          string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                          string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                          string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                          string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
            end. 
        end.
        
          /*****Item possui O.C. de investimento n∆o pode possuir O.P. 62******/                 
        if  avail ordem-compra and ordem-compra.num-ord-inv <> 0 AND param-global.modulo-in
                       and item-doc-est.nr-ord-prod <> 0  then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6353,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6353
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
                
        /*****Pedido inv†lido 63*****/
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6354,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6354
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
 
        /*****Parcela inv†lida 64*****/               
        if  item-doc-est.parcela = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6348,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6348
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

     /*****Parcela n∆o cadastrada 65*****/ 
     if  item-doc-est.parcela <> 0 then do:
         find prazo-compra where prazo-compra.numero-ordem = item-doc-est.numero-ordem and
                            prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
         if  not avail prazo-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6357,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6357
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end. 
                  
         /*****Parcela com situaá∆o inv†lida 66*****/                 
         if  l-entrada and avail prazo-compra and prazo-compra.situacao <> 2 then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6359,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6359
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end. 
         else do:
                   
             /*****Parcela com situaá∆o inv†lida para devoluá∆o 67*****/
             if  avail prazo-compra and prazo-compra.situacao <> 2 and 
                 prazo-compra.situacao <> 6 then do:
                 i-conta-msg = i-conta-msg + 1.
                 run utp/ut-msgs.p ('msg',6360,''). 
                 create tt_erros_modulo.
                 assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                        tt_erros_modulo.cod-erro           = 6360
                        tt_erros_modulo.des-erro           = return-value
                        tt_erros_modulo.identifi-msg       = 
                        string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                        string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                        string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                        string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
             end.               
         end.  

        /*****Pedido compra inv†lido 68*****/  
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6363,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6363
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 

       /*****Ordem compra inv†lida 69*****/
       if  item-doc-est.numero-ordem = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6364,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6364
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
       end. 
                
       assign de-qtd-tot-ordem = 0.
       for each prazo-compra
                where prazo-compra.numero-ordem = item-doc-est.numero-ordem no-lock:
           assign de-qtd-tot-ordem = de-qtd-tot-ordem + prazo-compra.quantidade.
       end.
     end. 
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                      string(tt-docum-est.nro-docto)    + chr(24) +
                                                      string(tt-docum-est.cod-emitente) + chr(24) +
                                                      string(tt-docum-est.nat-operacao)) then
     undo, next.
            
     if  (can-do("4,1",string(item.tipo-contr)) and (item-doc-est.nr-ord-prod = 0 
          or  (l-remessa and item-doc-est.baixa-ce 
               and item-doc-est.nr-ord-prod = 0)))
          or  (item.tipo-contr = 2 and l-devolucao = no and
               item-doc-est.nr-ord-prod = 0)
          or  ((l-retorno or l-remessa) and item-doc-est.nr-ord-prod = 0) 
          or  (avail ordem-compra and ordem-compra.num-ord-inv <> 0 AND param-global.modulo-in) 
          or  (l-sai-cons and (not item-doc-est.baixa-ce and
               item.tipo-contr <> 3)) then do:
               assign
               item-doc-est.ct-codigo = if  item-doc-est.ct-codigo <>
                                                ct-initial then
                                                    item-doc-est.ct-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-ct-componente
                                                else
                                                if avail ordem-compra
                                                then ordem-compra.ct-codigo
                                                else item.ct-codigo
               item-doc-est.sc-codigo = if  item-doc-est.sc-codigo <>
                                                sc-initial then
                                                    item-doc-est.sc-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-sc-componente
                                                else
                                                if avail ordem-compra
                                                then ordem-compra.sc-codigo
                                                else item.sc-codigo.
               if can-do("4,1",string(item.tipo-contr))
               or item-doc-est.ct-codigo <> ct-initial
               or item-doc-est.sc-codigo <> sc-initial
               or item-doc-est.baixa-ce = no then do:
               
                  assign i-empresa = param-global.empresa-prin.
                
                  &if defined (bf_dis_consiste_conta) &then
                     find docum-est where
                          docum-est.serie-docto  = item-doc-est.serie-docto  and
                          docum-est.nro-docto    = item-doc-est.nro-docto    and
                          docum-est.cod-emitente = item-doc-est.cod-emitente and
                          docum-est.nat-operacao = item-doc-est.nat-operacao no-lock no-error.
                     if avail docum-est then do:

                        find estabelec where
                             estabelec.cod-estabel = docum-est.cod-estabel no-lock no-error.
                    
                        run cdp/cd9970.p (input rowid(estabelec),
                                          output i-empresa).
                     end.

                  &endif
                      
                  /* 
                  find conta-contab where
                       conta-contab.ep-codigo = i-empresa              and
                       conta-contab.ct-codigo = item-doc-est.ct-codigo and
                       conta-contab.sc-codigo = item-doc-est.sc-codigo no-lock no-error.
                  */

                /*LOCALIZANDO PLANO CONTAS PRINCIPAL*/
                  FIND FIRST plano_cta_unid_organ
                       WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar
                         AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio"
                         AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
                         AND plano_cta_unid_organ.dat_fim_valid          >= TODAY 
                             NO-LOCK NO-ERROR. 
                  IF AVAIL plano_cta_unid_organ THEN DO:
                      FIND FIRST plano_cta_ctbl 
                           WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl
                                 NO-LOCK NO-ERROR.
                  END.

                  FIND FIRST cta_ctbl 
                       WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                         AND cta_ctbl.cod_cta_ctbl       = item-doc-est.ct-codigo
                             NO-LOCK NO-ERROR.
 
                  /*****Conta contabil n∆o cadastrada 70*****/
                  if not avail cta_ctbl then do: /*if not avail conta-contab then do:*/
                     i-conta-msg = i-conta-msg + 1.
                     run utp/ut-msgs.p ('msg',1060,''). 
                     create tt_erros_modulo.
                     assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                            tt_erros_modulo.cod-erro           = 1060
                            tt_erros_modulo.des-erro           = return-value
                            tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                  end.

                 /*****Conta n∆o Ç de sistema 71*****/ 
                 IF AVAIL cta_ctbl AND cta_ctbl.ind_espec_cta_ctbl <> "Anal°tica" THEN DO:    /*if avail conta-contab and conta-contab.estado <> 3 then do:*/
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',443,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 443
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 END. 
                        
                 /*****Conta deve ser de despesa receita, ativo ou passivo 72*****/                        
                 IF AVAIL cta_ctbl AND NOT CAN-DO ("1,2,3,4",STRING (cta_ctbl.cod_grp_cta_ctbl)) THEN DO: /*if avail conta-contab and not can-do("1,2,4,5",string(conta-contab.tipo)) then do:*/
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',6365,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 6365
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end.

                 /*
                 /*****Conta n∆o permite lanáamento de estoque 73*****/                        
                 if avail conta-contab and conta-contab.estoq = 0 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',445,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 445
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                 */
                 /*
                 /*****Conta n∆o aceita lanáamentos referentes a requisiá‰es 74*****/
                 if avail conta-contab and not can-do("1,2,3,5,6",string(conta-contab.estoq)) then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',107,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 107
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                 */
               end.
     end.
           
     find prazo-compra where prazo-compra.numero-ordem = item-doc-est.numero-ordem and
                        prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
     assign de-indice = 1.
     if item.tipo-contr <> 4 then do:
        find item-fornec where item-fornec.it-codigo = item-doc-est.it-codigo and
                                     item-fornec.cod-emite = item-doc-est.cod-emite no-lock no-error.
        if available item-fornec then do:
           assign de-indice = item-fornec.fator-conver / exp(10,item-fornec.num-casa-dec)
                  c-un      = item-fornec.unid-med-for. 
        end.
        else
            assign c-un      = item.un.
     end.
     else
     if avail ordem-compra then do:
        find cotacao-item where 
        cotacao-item.numero-ordem = ordem-compra.numero-ordem
        and cotacao-item.cod-emitente = ordem-compra.cod-emitente
        and cotacao-item.data-cotacao = ordem-compra.data-cotacao no-lock no-error.
        if available cotacao-item then do:
           assign c-un      = cotacao-item.un.
           find tab-conv-un where tab-conv-un.unid-med-for = cotacao-item.un
                              and tab-conv-un.un           = prazo-compra.un no-lock no-error.
           if available tab-conv-un then
           assign de-indice = tab-conv-un.fator-conver / exp(10,tab-conv-un.num-casa-dec).
        end.
        else assign  c-un      = item.un.
     end.
   
     assign de-qtd-estrut   = de-qtd-estrut * de-indice
                              item-doc-est.un = c-un.
     if  not l-devolucao and not l-transf and l-compras then 
     assign de-aux-1 = round(prazo-compra.quant-saldo +
            (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4)
            de-aux-2 = round(prazo-compra.quant-saldo - (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4).
     if (l-entrada and l-retorno = no and l-sai-cons = no) and
         not(l-entrada and docum-est.esp-docto = 20) then do:
         assign item-doc-est.quantidade = item-doc-est.qt-do-forn / de-indice.
     end. 
               
     /*Quantidade estaÔfora da variaá∆o permitida 74*****/  
     if l-entrada and not l-devolucao and not l-transf and l-compras then do:
        if item-doc-est.quantidade > de-aux-1 and param-re.aceita-var = no then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5928,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5928
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

     /*****Quantidade n∆o pode ser fracionada 75*****/ 
     if not item.fraciona then do:
        if integer(item-doc-est.quantidade) <> item-doc-est.quantidade then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6381,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6381
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.     
     end.

     /*****Quantidade informada n∆o pode ser igual a 0 (zero) 76*****/
     if l-remessa or l-ent-cons or (l-transf and not l-entrada) then do:
        if item-doc-est.quantidade = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1521,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1521
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.  

     /*****Deposito n∆o cadastrado 77*****/
     find deposito where deposito.cod-depos = item-doc-est.cod-depos no-lock no-error.
     if not avail deposito then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',530,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 530
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
  
     /*****N∆o pode efetuar saida de CQ sem aprovaá∆o do controle de qualidade 78*****/              
     if  avail deposito and natur-oper.tipo = 2 
         and (deposito.ind-dep-cq = yes or deposito.ind-dep-rej = yes) 
         and item.contr-qualid = yes 
         and param-global.modulo-cq and param-cq.tipo-cq > 0 then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6382,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6382
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
    
              
     /*****Localizaá∆o unica para o item 79****/
     if  avail item and item.loc-unica and item.cod-localiz <> item-doc-est.cod-localiz then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6383,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6383
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
    
     /*****Lote deve ser diferente de branco 80*****/
     if  avail item and item.tipo-con-est <> 1 and 
         item-doc-est.lote = "" then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',1818,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 1818
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end.
 
     /*****Item controlado por numero de serie j† existente em outra sequencia do documento 81*****/              
     if item.tipo-con-est = 2 and item-doc-est.quantidade <> 0 then do:
        find first b-item-doc-est {esp/escd8900.i b-item-doc-est docum-est}
            and b-item-doc-est.it-codigo = item.it-codigo
            and b-item-doc-est.lote      = item-doc-est.lote
            and b-item-doc-est.sequencia <> item-doc-est.sequencia no-lock no-error.
        if avail b-item-doc-est then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6388,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6388
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.         
        /*****Item controlado por numero de serie j† existente em saldo de estoque 82*****/  
        find first b-saldo-estoq use-index lote where 
           b-saldo-estoq.lote = item-doc-est.lote
           and b-saldo-estoq.it-codigo = item.it-codigo
           and b-saldo-estoq.qtidade-atu <> 0 no-lock no-error.
        if avail b-saldo-estoq and ((b-saldo-estoq.qtidade-atu < 0 and  natur-oper.tipo = 2)
                         or (b-saldo-estoq.qtidade-atu > 0 and natur-oper.tipo = 1)) then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1252,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1252
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.
  
     if natur-oper.tipo = 2 and item.tipo-contr <> 4 
                                and item-doc-est.baixa-ce and 
        not((l-entrada = no and not l-transf and not l-remessa and
        not l-sai-cons) and item-doc-est.nr-ord-prod <> 0) then do:
        find saldo-estoq use-index estabel-dep where
             saldo-estoq.cod-estab = docum-est.cod-estab        and 
             saldo-estoq.cod-depos = item-doc-est.cod-depos     and
             saldo-estoq.cod-localiz = item-doc-est.cod-localiz and
             saldo-estoq.lote = item-doc-est.lote               and
             saldo-estoq.it-codigo = item-doc-est.it-codigo     and
             saldo-estoq.cod-refer = item-doc-est.cod-refer no-lock no-error.
             de-quant = if avail saldo-estoq then
                        saldo-estoq.qtidade-atu -
                        saldo-estoq.qt-alocada  -
                        saldo-estoq.qt-aloc-ped -
                        saldo-estoq.qt-aloc-prod
                        else 0.
                    
        /*****Quantidade de saida maior que a disponivel em estoque 83*****/                       
        if l-consiste-saldo and item-doc-est.quantidade  > de-quant 
                    and item-doc-est.quantidade <> 0  
                    and item.perm-saldo-neg = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5266,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5266
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.
      
        /*****Referància do lote n∆o confere com a referància informada 84*****/
     if item.tipo-con-est = 4 then do:
        find first saldo-estoq where saldo-estoq.it-codigo   = item-doc-est.it-codigo 
                                and saldo-estoq.lote        = item-doc-est.lote no-lock no-error.
        if avail saldo-estoq then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1785,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1785
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

           /*****Somat¢ria dos pesos dos itens Ç maior que peso toatal do documento 85*****/  
     if param-re.rateia-frete = 1 and docum-est.valor-frete > 0 and not l-servicos then do:
        assign de-tot-peso = 0.
        for each b-item-doc-est {esp/escd8900.i docum-est b-item-doc-est}:
            assign de-tot-peso = de-tot-peso + b-item-doc-est.peso-liquido.
        end.
        assign item-doc-est.peso-liquido = item.peso-liquido
                                          * item-doc-est.quantidade
                            de-tot-peso = de-tot-peso + item-doc-est.peso-liquido.
        if de-tot-peso > docum-est.tot-peso then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6394,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6394
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.    
     end.
     else do:
        if  l-servicos then 
        assign item-doc-est.peso-liquido = tt-item-doc-est.peso-liquido.
        if  l-servicos = no then do:
            assign item-doc-est.peso-liquido = item.peso-liquido
                                                  * item-doc-est.quantidade.
        end.
     end.
  
     if  param-re.rateia-desc and natur-oper.tipo =1 
           and not l-ent-cons and not l-retorno
           and not l-devolucao and not natur-oper.transf then do:
  
        assign item-doc-est.desconto[1] =
                         truncate(item-doc-est.preco-total[1] *
                         docum-est.tot-desconto *
                         integer(docum-est.valor-mercad <> 0) /
                         (docum-est.valor-mercad +
                         integer(docum-est.valor-mercad = 0)),2).
  
        for each b-item-doc-est {esp/escd8900.i b-item-doc-est docum-est} 
                  no-lock:
                  accum b-item-doc-est.preco-total(total).
                  accum b-item-doc-est.desconto(total).
        end.
        assign de-aux-1 = accum total b-item-doc-est.preco-total[1].
                     de-aux-2 = accum total b-item-doc-est.desconto[1].
  
        if de-aux-1 = docum-est.valor-mercad then
        if de-aux-2 <> docum-est.tot-desconto then
           assign item-doc-est.desconto[1] = item-doc-est.desconto[1] +
                                             docum-est.tot-desconto - de-aux-2.
  
        if item-doc-est.desconto[1] = ? then
                 assign item-doc-est.desconto[1] = 0.
     end.
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                      string(tt-docum-est.nro-docto)    + chr(24) +
                                                      string(tt-docum-est.cod-emitente) + chr(24) +
                                                      string(tt-docum-est.nat-operacao)) then
        undo, next.
             
           /* calculo dos impostos */
           run esp/esreap152b.p(input table tt-docum-est,
                              rowid(item-doc-est),
                              input-output i-conta-msg,
                              input-output table tt_erros_modulo).
     
end.  
END PROCEDURE.  
  /* fim reap152a.p*/



                

                

PROCEDURE pi-cria-item-docum-est-atual-dev:

    for each tt-item-doc-est where 
         tt-item-doc-est.nro-docto    = docum-est.nro-docto 
    and  tt-item-doc-est.cod-emitente = docum-est.cod-emitente
    and  tt-item-doc-est.serie-docto  = docum-est.serie-docto
    and  tt-item-doc-est.nat-operacao = docum-est.nat-operacao no-lock trans:

    find last item-doc-est {esp/escd8900.i item-doc-est docum-est} no-lock no-error.
    if  avail item-doc-est then
       assign i-seq = item-doc-est.sequencia + param-re.inc-seq.
    else            
       assign i-seq = param-re.seq-item-um.

    assign i-nr-item = i-nr-item + 1.

    /*****Numero de itens da nota excede o m†ximo permitido pelo faturamento 48*****/  
    if  param-global.modulo-ft and natur-oper.imp-nota and avail para-fat then do:
            if  i-nr-item > para-fat.nr-item-nota then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6342,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6342
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
      
            undo, leave.
        end.
    end. 

    create item-doc-est.
           
    assign item-doc-est.sequencia      = i-seq                   
           item-doc-est.aliquota-icm   = tt-item-doc-est.aliquota-icm       
           item-doc-est.aliquota-ipi   = tt-item-doc-est.aliquota-ipi       
           item-doc-est.aliquota-iss   = tt-item-doc-est.aliquota-iss       
           item-doc-est.class-fiscal   = tt-item-doc-est.class-fiscal       
           item-doc-est.cod-depos      = tt-item-doc-est.cod-depos
           item-doc-est.cod-emitente   = tt-item-doc-est.cod-emitente      
           item-doc-est.cod-refer      = tt-item-doc-est.cod-refer        
           item-doc-est.desconto[1]    = tt-item-doc-est.desconto[1]
           item-doc-est.despesas[1]    = tt-item-doc-est.despesas[1]     
           item-doc-est.dt-vali-lote   = tt-item-doc-est.dt-vali-lote   
           item-doc-est.lote           = tt-item-doc-est.lote
           item-doc-est.cod-localiz    = tt-item-doc-est.cod-localiz  
           item-doc-est.encerra-pa     = tt-item-doc-est.encerra-pa     
           item-doc-est.it-codigo      = tt-item-doc-est.it-codigo      
           item-doc-est.nat-comp       = tt-item-doc-est.nat-comp       
           item-doc-est.nat-operacao   = tt-item-doc-est.nat-operacao   
           item-doc-est.nr-ord-prod    = tt-item-doc-est.nr-ord-prod        
           item-doc-est.nr-pd-seq      = tt-item-doc-est.nr-pd-seq         
           item-doc-est.nr-pedcli      = tt-item-doc-est.nr-pedcli         
           item-doc-est.nro-comp       = tt-item-doc-est.nro-comp         
           item-doc-est.nro-docto      = tt-item-doc-est.nro-docto    
           item-doc-est.num-pedido     = tt-item-doc-est.num-pedido         
           item-doc-est.numero-ordem   = tt-item-doc-est.numero-ordem      
           item-doc-est.parcela        = tt-item-doc-est.parcela         
           item-doc-est.peso-liquido   = tt-item-doc-est.peso-liquido    
           item-doc-est.preco-total[1] = tt-item-doc-est.preco-total[1]  
           item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-unit[1]   
           item-doc-est.qt-do-forn     = tt-item-doc-est.qt-do-forn - (es-ticket.peso-desmem[1] - es-ticket.peso-liq)       
           item-doc-est.serie-docto    = tt-item-doc-est.serie-docto     
           item-doc-est.valor-ipi[1]   = tt-item-doc-est.valor-ipi[1].
  
    /* aqui22 - rde - 08/01/2018 - migraá∆o totvs 12 */
    ASSIGN item-doc-est.quantidade = tt-item-doc-est.qt-do-forn.

    IF vlo-debug THEN
    MESSAGE 'esreap152.p, criando o item-doc-est' skip(1)
        'item-doc-est.preco-total[1] = tt-item-doc-est.preco-total[1] ' item-doc-est.preco-total[1] SKIP
        'item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-unit[1]' item-doc-est.preco-unit[1] SKIP
        'item-doc-est.qt-do-forn     = tt-item-doc-est.qt-do-forn    ' item-doc-est.qt-do-forn SKIP
        'item-doc-est.quantidade' item-doc-est.quantidade




        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                                                      
    /*****Classificaá∆o fiscal n∆o cadastrada 49******/
    find classif-fisc where classif-fisc.class-fiscal = item-doc-est.class-fiscal no-lock no-error.
        if  not avail classif-fisc then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6744,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6744
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
    end. 
      
    /*****Item n∆o cadastrado 50*****/  
    find item where item.it-codigo = item-doc-est.it-codigo no-lock no-error.
        if  not avail item then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',166,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 166
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
    end.    
    else 

    /*****Ordem de produá∆o n∆o cadastrada 51*****/
    find familia where item.fm-codigo = familia.fm-codigo no-lock no-error.
    if  item-doc-est.nr-ord-prod <> 0 then do:

        find ord-prod where ord-prod.nr-ord-prod = item-doc-est.nr-ord-prod no-lock no-error.

        if  not avail ord-prod then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1879,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1879
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
        else do: 
        /*****Ordem de produá∆o n∆o pertence ao estabelecimento 52******/                
        if  ord-prod.cod-estabel = docum-est.cod-estabel then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6347,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6347
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
                       
        /*****Ordem de produá∆o j† est† finalizada 53*****/              
        if  ord-prod.estado = 7 or ord-prod.estado = 8 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1202,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1202
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
   
        /*****Ordem de manutená∆o suspensa ou terminada 54*****/                                                    
        if  param-global.modulo-mi then do:

            find ord-manut where ord-manut.nr-ord-prod = ord-prod.nr-ord-prod no-lock no-error.

            if  avail ord-manut then do:
                if  ord-manut.estado-om = 3 or ord-manut.estado-om = 4 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',5676,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 5676
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
            end. 
        end.
        end.
              
       assign item-doc-est.ct-codigo = ord-prod.ct-codigo
              item-doc-est.sc-codigo = ord-prod.sc-codigo.
       end.
    end.
          
    /*****Pedido n∆o cadastrado 55*****/  
    if  item-doc-est.num-pedido <> 0 then do:
        assign l-compras = yes.
        find pedido-compr where pedido-compr.num-pedido = item-doc-est.num-pedido no-lock no-error.
    
        if  not avail pedido-compr then do: 
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6269,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6269
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
         
        /******Pedido eliminado 56*****/ 
        if  avail pedido-compr and pedido-compr.situacao = 3 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1813,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1813
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  

        /*****Ordem invalida 57*****/                   
        if  item-doc-est.numero-ordem = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',3138,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 3138
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.

        /*****Parcela invalida 58*****/
        if  item-doc-est.parcela = 0 then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6348,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6348
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                  string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                  string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                  string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                  string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 
     
     /*****Ordem compra n∆o cadastrada 59*****/
     if  item-doc-est.numero-ordem <> 0 then do: 

         find ordem-compra where ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
         if  not avail ordem-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6349,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6349
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end.
      
        /*****Ordem de compra com situaá∆o invalida 60*****/                  
        find first ordem-compra use-index pedido where ordem-compra.num-pedido = item-doc-est.num-pedido
        and ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
        if  l-entrada and avail ordem-compra and ordem-compra.situacao <> 2 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6350,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6350
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.     
        else do:                   
            /*****Ordem de compra com situaá∆o invalida para devoluá∆o 61*****/           
            if  avail ordem-compra and ordem-compra.situacao <> 2 and 
                ordem-compra.situacao <> 6 then do:
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',6352,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 6352
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                          string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                          string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                          string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                          string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
            end. 
        end.
        
          /*****Item possui O.C. de investimento n∆o pode possuir O.P. 62******/                 
        if  avail ordem-compra and ordem-compra.num-ord-inv <> 0 AND param-global.modulo-in
                       and item-doc-est.nr-ord-prod <> 0  then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6353,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6353
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
                
        /*****Pedido inv†lido 63*****/
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6354,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6354
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
 
        /*****Parcela inv†lida 64*****/               
        if  item-doc-est.parcela = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6348,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6348
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

     /*****Parcela n∆o cadastrada 65*****/ 
     if  item-doc-est.parcela <> 0 then do:
         find prazo-compra where prazo-compra.numero-ordem = item-doc-est.numero-ordem and
                            prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
         if  not avail prazo-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6357,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6357
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end. 
                  
         /*****Parcela com situaá∆o inv†lida 66*****/                 
         if  l-entrada and avail prazo-compra and prazo-compra.situacao <> 2 then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6359,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6359
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end. 
         else do:
                   
             /*****Parcela com situaá∆o inv†lida para devoluá∆o 67*****/
             if  avail prazo-compra and prazo-compra.situacao <> 2 and 
                 prazo-compra.situacao <> 6 then do:
                 i-conta-msg = i-conta-msg + 1.
                 run utp/ut-msgs.p ('msg',6360,''). 
                 create tt_erros_modulo.
                 assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                        tt_erros_modulo.cod-erro           = 6360
                        tt_erros_modulo.des-erro           = return-value
                        tt_erros_modulo.identifi-msg       = 
                        string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                        string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                        string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                        string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
             end.               
         end.  

        /*****Pedido compra inv†lido 68*****/  
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6363,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6363
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 

       /*****Ordem compra inv†lida 69*****/
       if  item-doc-est.numero-ordem = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6364,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6364
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
       end. 
                
       assign de-qtd-tot-ordem = 0.
       for each prazo-compra
                where prazo-compra.numero-ordem = item-doc-est.numero-ordem no-lock:
           assign de-qtd-tot-ordem = de-qtd-tot-ordem + prazo-compra.quantidade.
       end.
     end. 
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                      string(tt-docum-est.nro-docto)    + chr(24) +
                                                      string(tt-docum-est.cod-emitente) + chr(24) +
                                                      string(tt-docum-est.nat-operacao)) then
     undo, next.
            
     if  (can-do("4,1",string(item.tipo-contr)) and (item-doc-est.nr-ord-prod = 0 
          or  (l-remessa and item-doc-est.baixa-ce 
               and item-doc-est.nr-ord-prod = 0)))
          or  (item.tipo-contr = 2 and l-devolucao = no and
               item-doc-est.nr-ord-prod = 0)
          or  ((l-retorno or l-remessa) and item-doc-est.nr-ord-prod = 0) 
          or  (avail ordem-compra and ordem-compra.num-ord-inv <> 0 AND param-global.modulo-in) 
          or  (l-sai-cons and (not item-doc-est.baixa-ce and
               item.tipo-contr <> 3)) then do:
               assign
               item-doc-est.ct-codigo = if  item-doc-est.ct-codigo <>
                                                ct-initial then
                                                    item-doc-est.ct-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-ct-componente
                                                else
                                                if avail ordem-compra
                                                then ordem-compra.ct-codigo
                                                else item.ct-codigo
               item-doc-est.sc-codigo = if  item-doc-est.sc-codigo <>
                                                sc-initial then
                                                    item-doc-est.sc-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-sc-componente
                                                else
                                                if avail ordem-compra
                                                then ordem-compra.sc-codigo
                                                else item.sc-codigo.
               if can-do("4,1",string(item.tipo-contr))
               or item-doc-est.ct-codigo <> ct-initial
               or item-doc-est.sc-codigo <> sc-initial
               or item-doc-est.baixa-ce = no then do:
               
                  assign i-empresa = param-global.empresa-prin.
                
                  &if defined (bf_dis_consiste_conta) &then
                     find docum-est where
                          docum-est.serie-docto  = item-doc-est.serie-docto  and
                          docum-est.nro-docto    = item-doc-est.nro-docto    and
                          docum-est.cod-emitente = item-doc-est.cod-emitente and
                          docum-est.nat-operacao = item-doc-est.nat-operacao no-lock no-error.
                     if avail docum-est then do:

                        find estabelec where
                             estabelec.cod-estabel = docum-est.cod-estabel no-lock no-error.
                    
                        run cdp/cd9970.p (input rowid(estabelec),
                                          output i-empresa).
                     end.

                  &endif
                      
                  /* 
                  find conta-contab where
                       conta-contab.ep-codigo = i-empresa              and
                       conta-contab.ct-codigo = item-doc-est.ct-codigo and
                       conta-contab.sc-codigo = item-doc-est.sc-codigo no-lock no-error.
                  */

                /*LOCALIZANDO PLANO CONTAS PRINCIPAL*/
                  FIND FIRST plano_cta_unid_organ
                       WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar
                         AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio"
                         AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
                         AND plano_cta_unid_organ.dat_fim_valid          >= TODAY 
                             NO-LOCK NO-ERROR. 
                  IF AVAIL plano_cta_unid_organ THEN DO:
                      FIND FIRST plano_cta_ctbl 
                           WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl
                                 NO-LOCK NO-ERROR.
                  END.

                  FIND FIRST cta_ctbl 
                       WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                         AND cta_ctbl.cod_cta_ctbl       = item-doc-est.ct-codigo
                             NO-LOCK NO-ERROR.
 
                  /*****Conta contabil n∆o cadastrada 70*****/
                  if not avail cta_ctbl then do: /*if not avail conta-contab then do:*/
                     i-conta-msg = i-conta-msg + 1.
                     run utp/ut-msgs.p ('msg',1060,''). 
                     create tt_erros_modulo.
                     assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                            tt_erros_modulo.cod-erro           = 1060
                            tt_erros_modulo.des-erro           = return-value
                            tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                  end.

                 /*****Conta n∆o Ç de sistema 71*****/ 
                 IF AVAIL cta_ctbl AND cta_ctbl.ind_espec_cta_ctbl <> "Anal°tica" THEN DO:    /*if avail conta-contab and conta-contab.estado <> 3 then do:*/
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',443,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 443
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 END. 
                        
                 /*****Conta deve ser de despesa receita, ativo ou passivo 72*****/                        
                 IF AVAIL cta_ctbl AND NOT CAN-DO ("1,2,3,4",STRING (cta_ctbl.cod_grp_cta_ctbl)) THEN DO: /*if avail conta-contab and not can-do("1,2,4,5",string(conta-contab.tipo)) then do:*/
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',6365,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 6365
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end.

                 /*
                 /*****Conta n∆o permite lanáamento de estoque 73*****/                        
                 if avail conta-contab and conta-contab.estoq = 0 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',445,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 445
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                 */
                 /*
                 /*****Conta n∆o aceita lanáamentos referentes a requisiá‰es 74*****/
                 if avail conta-contab and not can-do("1,2,3,5,6",string(conta-contab.estoq)) then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',107,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 107
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                 */
               end.
     end.
           
     find prazo-compra where prazo-compra.numero-ordem = item-doc-est.numero-ordem and
                        prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
     assign de-indice = 1.
     if item.tipo-contr <> 4 then do:
        find item-fornec where item-fornec.it-codigo = item-doc-est.it-codigo and
                                     item-fornec.cod-emite = item-doc-est.cod-emite no-lock no-error.
        if available item-fornec then do:
           assign de-indice = item-fornec.fator-conver / exp(10,item-fornec.num-casa-dec)
                  c-un      = item-fornec.unid-med-for. 
        end.
        else
            assign c-un      = item.un.
     end.
     else
     if avail ordem-compra then do:
        find cotacao-item where 
        cotacao-item.numero-ordem = ordem-compra.numero-ordem
        and cotacao-item.cod-emitente = ordem-compra.cod-emitente
        and cotacao-item.data-cotacao = ordem-compra.data-cotacao no-lock no-error.
        if available cotacao-item then do:
           assign c-un      = cotacao-item.un.
           find tab-conv-un where tab-conv-un.unid-med-for = cotacao-item.un
                              and tab-conv-un.un           = prazo-compra.un no-lock no-error.
           if available tab-conv-un then
           assign de-indice = tab-conv-un.fator-conver / exp(10,tab-conv-un.num-casa-dec).
        end.
        else assign  c-un      = item.un.
     end.
   
     assign de-qtd-estrut   = de-qtd-estrut * de-indice
                              item-doc-est.un = c-un.
     if  not l-devolucao and not l-transf and l-compras then 
     assign de-aux-1 = round(prazo-compra.quant-saldo +
            (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4)
            de-aux-2 = round(prazo-compra.quant-saldo - (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4).
     if (l-entrada and l-retorno = no and l-sai-cons = no) and
         not(l-entrada and docum-est.esp-docto = 20) then do:
         assign item-doc-est.quantidade = item-doc-est.qt-do-forn / de-indice.
     end. 
               
     /*Quantidade estaÔfora da variaá∆o permitida 74*****/  
     if l-entrada and not l-devolucao and not l-transf and l-compras then do:
        if item-doc-est.quantidade > de-aux-1 and param-re.aceita-var = no then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5928,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5928
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

     /*****Quantidade n∆o pode ser fracionada 75*****/ 
     if not item.fraciona then do:
        if integer(item-doc-est.quantidade) <> item-doc-est.quantidade then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6381,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6381
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.     
     end.

     /*****Quantidade informada n∆o pode ser igual a 0 (zero) 76*****/
     if l-remessa or l-ent-cons or (l-transf and not l-entrada) then do:
        if item-doc-est.quantidade = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1521,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1521
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.  

     /*****Deposito n∆o cadastrado 77*****/
     find deposito where deposito.cod-depos = item-doc-est.cod-depos no-lock no-error.
     if not avail deposito then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',530,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 530
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
  
     /*****N∆o pode efetuar saida de CQ sem aprovaá∆o do controle de qualidade 78*****/              
     if  avail deposito and natur-oper.tipo = 2 
         and (deposito.ind-dep-cq = yes or deposito.ind-dep-rej = yes) 
         and item.contr-qualid = yes 
         and param-global.modulo-cq and param-cq.tipo-cq > 0 then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6382,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6382
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
    
              
     /*****Localizaá∆o unica para o item 79****/
     if  avail item and item.loc-unica and item.cod-localiz <> item-doc-est.cod-localiz then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6383,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6383
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
    
     /*****Lote deve ser diferente de branco 80*****/
     if  avail item and item.tipo-con-est <> 1 and 
         item-doc-est.lote = "" then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',1818,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 1818
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end.
 
     /*****Item controlado por numero de serie j† existente em outra sequencia do documento 81*****/              
     if item.tipo-con-est = 2 and item-doc-est.quantidade <> 0 then do:
        find first b-item-doc-est {esp/escd8900.i b-item-doc-est docum-est}
            and b-item-doc-est.it-codigo = item.it-codigo
            and b-item-doc-est.lote      = item-doc-est.lote
            and b-item-doc-est.sequencia <> item-doc-est.sequencia no-lock no-error.
        if avail b-item-doc-est then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6388,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6388
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.         
        /*****Item controlado por numero de serie j† existente em saldo de estoque 82*****/  
        find first b-saldo-estoq use-index lote where 
           b-saldo-estoq.lote = item-doc-est.lote
           and b-saldo-estoq.it-codigo = item.it-codigo
           and b-saldo-estoq.qtidade-atu <> 0 no-lock no-error.
        if avail b-saldo-estoq and ((b-saldo-estoq.qtidade-atu < 0 and  natur-oper.tipo = 2)
                         or (b-saldo-estoq.qtidade-atu > 0 and natur-oper.tipo = 1)) then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1252,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1252
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.
  
     if natur-oper.tipo = 2 and item.tipo-contr <> 4 
                                and item-doc-est.baixa-ce and 
        not((l-entrada = no and not l-transf and not l-remessa and
        not l-sai-cons) and item-doc-est.nr-ord-prod <> 0) then do:
        find saldo-estoq use-index estabel-dep where
             saldo-estoq.cod-estab = docum-est.cod-estab        and 
             saldo-estoq.cod-depos = item-doc-est.cod-depos     and
             saldo-estoq.cod-localiz = item-doc-est.cod-localiz and
             saldo-estoq.lote = item-doc-est.lote               and
             saldo-estoq.it-codigo = item-doc-est.it-codigo     and
             saldo-estoq.cod-refer = item-doc-est.cod-refer no-lock no-error.
             de-quant = if avail saldo-estoq then
                        saldo-estoq.qtidade-atu -
                        saldo-estoq.qt-alocada  -
                        saldo-estoq.qt-aloc-ped -
                        saldo-estoq.qt-aloc-prod
                        else 0.
                    
        /*****Quantidade de saida maior que a disponivel em estoque 83*****/                       
        if l-consiste-saldo and item-doc-est.quantidade  > de-quant 
                    and item-doc-est.quantidade <> 0  
                    and item.perm-saldo-neg = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5266,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5266
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.
      
        /*****Referància do lote n∆o confere com a referància informada 84*****/
     if item.tipo-con-est = 4 then do:
        find first saldo-estoq where saldo-estoq.it-codigo   = item-doc-est.it-codigo 
                                and saldo-estoq.lote        = item-doc-est.lote no-lock no-error.
        if avail saldo-estoq then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1785,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1785
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

           /*****Somat¢ria dos pesos dos itens Ç maior que peso toatal do documento 85*****/  
     if param-re.rateia-frete = 1 and docum-est.valor-frete > 0 and not l-servicos then do:
        assign de-tot-peso = 0.
        for each b-item-doc-est {esp/escd8900.i docum-est b-item-doc-est}:
            assign de-tot-peso = de-tot-peso + b-item-doc-est.peso-liquido.
        end.
        assign item-doc-est.peso-liquido = item.peso-liquido
                                          * item-doc-est.quantidade
                            de-tot-peso = de-tot-peso + item-doc-est.peso-liquido.
        if de-tot-peso > docum-est.tot-peso then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6394,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6394
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.    
     end.
     else do:
        if  l-servicos then 
        assign item-doc-est.peso-liquido = tt-item-doc-est.peso-liquido.
        if  l-servicos = no then do:
            assign item-doc-est.peso-liquido = item.peso-liquido
                                                  * item-doc-est.quantidade.
        end.
     end.
  
     if  param-re.rateia-desc and natur-oper.tipo =1 
           and not l-ent-cons and not l-retorno
           and not l-devolucao and not natur-oper.transf then do:
  
        assign item-doc-est.desconto[1] =
                         truncate(item-doc-est.preco-total[1] *
                         docum-est.tot-desconto *
                         integer(docum-est.valor-mercad <> 0) /
                         (docum-est.valor-mercad +
                         integer(docum-est.valor-mercad = 0)),2).
  
        for each b-item-doc-est {esp/escd8900.i b-item-doc-est docum-est} 
                  no-lock:
                  accum b-item-doc-est.preco-total(total).
                  accum b-item-doc-est.desconto(total).
        end.
        assign de-aux-1 = accum total b-item-doc-est.preco-total[1].
                     de-aux-2 = accum total b-item-doc-est.desconto[1].
  
        if de-aux-1 = docum-est.valor-mercad then
        if de-aux-2 <> docum-est.tot-desconto then
           assign item-doc-est.desconto[1] = item-doc-est.desconto[1] +
                                             docum-est.tot-desconto - de-aux-2.
  
        if item-doc-est.desconto[1] = ? then
                 assign item-doc-est.desconto[1] = 0.
     end.
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                      string(tt-docum-est.nro-docto)    + chr(24) +
                                                      string(tt-docum-est.cod-emitente) + chr(24) +
                                                      string(tt-docum-est.nat-operacao)) then
        undo, next.
             
           /* calculo dos impostos */
           run esp/esreap152b.p(input table tt-docum-est,
                              rowid(item-doc-est),
                              input-output i-conta-msg,
                              input-output table tt_erros_modulo).
     
end.  
END PROCEDURE.  
PROCEDURE pi-cria-item-docum-est-dev:

    /*diego*/

    FIND FIRST tt-item-doc-est-aux where                                              
         tt-item-doc-est-aux.nro-docto    = docum-est.nro-docto                     
    and  tt-item-doc-est-aux.cod-emitente = docum-est.cod-emitente                  
    and  tt-item-doc-est-aux.serie-docto  = docum-est.serie-docto                   
    and  tt-item-doc-est-aux.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.

    find last item-doc-est {esp/escd8900.i item-doc-est docum-est} no-lock no-error.
    if  avail item-doc-est then
       assign i-seq = item-doc-est.sequencia + param-re.inc-seq.
    else            
       assign i-seq = param-re.seq-item-um.

    assign i-nr-item = i-nr-item + 1.

    /*****Numero de itens da nota excede o m†ximo permitido pelo faturamento 48*****/  
    if  param-global.modulo-ft and natur-oper.imp-nota and avail para-fat then do:
            if  i-nr-item > para-fat.nr-item-nota then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6342,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6342
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-item-doc-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-item-doc-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-item-doc-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-item-doc-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
      
            undo, leave.
        end.
    end. 

    FIND es-ticket NO-LOCK WHERE es-ticket.nr-ticket = i-nr-ticket NO-ERROR.

    create item-doc-est.

    assign item-doc-est.sequencia      = i-seq                   
           item-doc-est.aliquota-icm   = tt-item-doc-est-aux.aliquota-icm       
           item-doc-est.aliquota-ipi   = tt-item-doc-est-aux.aliquota-ipi       
           item-doc-est.aliquota-iss   = tt-item-doc-est-aux.aliquota-iss       
           item-doc-est.class-fiscal   = tt-item-doc-est-aux.class-fiscal       
           item-doc-est.cod-depos      = tt-item-doc-est-aux.cod-depos
           item-doc-est.cod-emitente   = tt-item-doc-est-aux.cod-emitente      
           item-doc-est.cod-refer      = tt-item-doc-est-aux.cod-refer        
           item-doc-est.desconto[1]    = tt-item-doc-est-aux.desconto[1]
           item-doc-est.despesas[1]    = tt-item-doc-est-aux.despesas[1]     
           item-doc-est.dt-vali-lote   = tt-item-doc-est-aux.dt-vali-lote   
           item-doc-est.lote           = tt-item-doc-est-aux.lote
           item-doc-est.cod-localiz    = tt-item-doc-est-aux.cod-localiz  
           item-doc-est.encerra-pa     = tt-item-doc-est-aux.encerra-pa     
           item-doc-est.it-codigo      = tt-item-doc-est-aux.it-codigo      
           item-doc-est.nat-comp       = tt-item-doc-est-aux.nat-comp       
           item-doc-est.nat-operacao   = tt-item-doc-est-aux.nat-operacao   
           item-doc-est.nr-ord-prod    = tt-item-doc-est-aux.nr-ord-prod        
           item-doc-est.nr-pd-seq      = tt-item-doc-est-aux.nr-pd-seq         
           item-doc-est.nr-pedcli      = tt-item-doc-est-aux.nr-pedcli         
           item-doc-est.nro-comp       = tt-item-doc-est-aux.nro-comp         
           item-doc-est.nro-docto      = tt-item-doc-est-aux.nro-docto   
           item-doc-est.num-pedido     = tt-item-doc-est-aux.num-pedido         
           item-doc-est.numero-ordem   = tt-item-doc-est-aux.numero-ordem      
           item-doc-est.parcela        = tt-item-doc-est-aux.parcela         
           item-doc-est.peso-liquido   = (es-ticket.peso-desmem[1] - es-ticket.peso-liq)    
           item-doc-est.preco-total[1] = (item-doc-est.peso-liquido * es-ticket.vl-quilo) /*tt-item-doc-est-aux.preco-total[1]*/
           item-doc-est.preco-unit[1]  = es-ticket.vl-quilo  /*preco-unit[1]*/
           item-doc-est.qt-do-forn     = item-doc-est.peso-liquido     
           item-doc-est.serie-docto    = tt-item-doc-est-aux.serie-docto     
           item-doc-est.valor-ipi[1]   = tt-item-doc-est-aux.valor-ipi[1]
           item-doc-est.quantidade     = item-doc-est.qt-do-forn.

    create rat-lote.
    assign 
         rat-lote.serie-docto    = item-doc-est.serie-docto
         rat-lote.cod-emitente   = item-doc-est.cod-emitente
         rat-lote.nro-docto      = string(item-doc-est.nro-docto,"9999999")
         rat-lote.nat-operacao   = item-doc-est.nat-operacao
         rat-lote.it-codigo      = item-doc-est.it-codigo
         rat-lote.sequencia      = item-doc-est.sequencia
         rat-lote.cod-depos      = item-doc-est.cod-depos
         rat-lote.cod-localiz    = item-doc-est.cod-localiz
         rat-lote.dt-vali-lote   = item-doc-est.dt-vali-lote  
         rat-lote.lote           = item-doc-est.lote
         rat-lote.quantidade     = item-doc-est.quantidade.

    IF vlo-debug THEN
    MESSAGE 'esreap152.p, criando o item-doc-est' skip(1)
        'item-doc-est.preco-total[1] = tt-item-doc-est.preco-total[1] ' item-doc-est.preco-total[1] SKIP
        'item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-unit[1]' item-doc-est.preco-unit[1] SKIP
        'item-doc-est.qt-do-forn     = tt-item-doc-est.qt-do-forn    ' item-doc-est.qt-do-forn SKIP
        'item-doc-est.quantidade' item-doc-est.quantidade




        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                                                      
    /*****Classificaá∆o fiscal n∆o cadastrada 49******/
    find classif-fisc where classif-fisc.class-fiscal = item-doc-est.class-fiscal no-lock no-error.
        if  not avail classif-fisc then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6744,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6744
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
    end. 
      
    /*****Item n∆o cadastrado 50*****/  
    find item where item.it-codigo = item-doc-est.it-codigo no-lock no-error.
        if  not avail item then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',166,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 166
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
    end.    
    else 

    /*****Ordem de produá∆o n∆o cadastrada 51*****/
    find familia where item.fm-codigo = familia.fm-codigo no-lock no-error.
    if  item-doc-est.nr-ord-prod <> 0 then do:

        find ord-prod where ord-prod.nr-ord-prod = item-doc-est.nr-ord-prod no-lock no-error.

        if  not avail ord-prod then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1879,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1879
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.  
        else do: 
        /*****Ordem de produá∆o n∆o pertence ao estabelecimento 52******/                
        if  ord-prod.cod-estabel = docum-est.cod-estabel then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6347,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6347
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.
                       
        /*****Ordem de produá∆o j† est† finalizada 53*****/              
        if  ord-prod.estado = 7 or ord-prod.estado = 8 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1202,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1202
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.  
   
        /*****Ordem de manutená∆o suspensa ou terminada 54*****/                                                    
        if  param-global.modulo-mi then do:

            find ord-manut where ord-manut.nr-ord-prod = ord-prod.nr-ord-prod no-lock no-error.

            if  avail ord-manut then do:
                if  ord-manut.estado-om = 3 or ord-manut.estado-om = 4 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',5676,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 5676
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
            end. 
        end.
        end.
              
       assign item-doc-est.ct-codigo = ord-prod.ct-codigo
              item-doc-est.sc-codigo = ord-prod.sc-codigo.
       end.
    end.
          
    /*****Pedido n∆o cadastrado 55*****/  
    if  item-doc-est.num-pedido <> 0 then do:
        assign l-compras = yes.
        find pedido-compr where pedido-compr.num-pedido = item-doc-est.num-pedido no-lock no-error.
    
        if  not avail pedido-compr then do: 
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6269,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6269
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.
         
        /******Pedido eliminado 56*****/ 
        if  avail pedido-compr and pedido-compr.situacao = 3 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1813,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1813
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.  

        /*****Ordem invalida 57*****/                   
        if  item-doc-est.numero-ordem = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',3138,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 3138
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.

        /*****Parcela invalida 58*****/
        if  item-doc-est.parcela = 0 then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6348,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6348
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                  string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                  string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                  string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                  string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
     end. 
     
     /*****Ordem compra n∆o cadastrada 59*****/
     if  item-doc-est.numero-ordem <> 0 then do: 

         find ordem-compra where ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
         if  not avail ordem-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6349,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6349
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
         end.
      
        /*****Ordem de compra com situaá∆o invalida 60*****/                  
        find first ordem-compra use-index pedido where ordem-compra.num-pedido = item-doc-est.num-pedido
        and ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
        if  l-entrada and avail ordem-compra and ordem-compra.situacao <> 2 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6350,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6350
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.     
        else do:                   
            /*****Ordem de compra com situaá∆o invalida para devoluá∆o 61*****/           
            if  avail ordem-compra and ordem-compra.situacao <> 2 and 
                ordem-compra.situacao <> 6 then do:
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',6352,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 6352
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                          string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                          string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                          string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                          string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
            end. 
        end.
        
          /*****Item possui O.C. de investimento n∆o pode possuir O.P. 62******/                 
        if  avail ordem-compra and ordem-compra.num-ord-inv <> 0 AND param-global.modulo-in
                       and item-doc-est.nr-ord-prod <> 0  then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6353,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6353
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
                
        /*****Pedido inv†lido 63*****/
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6354,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6354
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.
 
        /*****Parcela inv†lida 64*****/               
        if  item-doc-est.parcela = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6348,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6348
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
     end. 

     /*****Parcela n∆o cadastrada 65*****/ 
     if  item-doc-est.parcela <> 0 then do:
         find prazo-compra where prazo-compra.numero-ordem = item-doc-est.numero-ordem and
                            prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
         if  not avail prazo-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6357,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6357
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
         end. 
                  
         /*****Parcela com situaá∆o inv†lida 66*****/                 
         if  l-entrada and avail prazo-compra and prazo-compra.situacao <> 2 then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6359,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6359
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
         end. 
         else do:
                   
             /*****Parcela com situaá∆o inv†lida para devoluá∆o 67*****/
             if  avail prazo-compra and prazo-compra.situacao <> 2 and 
                 prazo-compra.situacao <> 6 then do:
                 i-conta-msg = i-conta-msg + 1.
                 run utp/ut-msgs.p ('msg',6360,''). 
                 create tt_erros_modulo.
                 assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                        tt_erros_modulo.cod-erro           = 6360
                        tt_erros_modulo.des-erro           = return-value
                        tt_erros_modulo.identifi-msg       = 
                        string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                        string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                        string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                        string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
             end.               
         end.  

        /*****Pedido compra inv†lido 68*****/  
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6363,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6363
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 

       /*****Ordem compra inv†lida 69*****/
       if  item-doc-est.numero-ordem = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6364,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6364
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
       end. 
                
       assign de-qtd-tot-ordem = 0.
       for each prazo-compra
                where prazo-compra.numero-ordem = item-doc-est.numero-ordem no-lock:
           assign de-qtd-tot-ordem = de-qtd-tot-ordem + prazo-compra.quantidade.
       end.
     end. 
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est-aux.serie-docto)  + chr(24) +
                                                      string(tt-docum-est-aux.nro-docto)    + chr(24) +
                                                      string(tt-docum-est-aux.cod-emitente) + chr(24) +
                                                      string(tt-docum-est-aux.nat-operacao)) then
     undo, next.
            
     if  (can-do("4,1",string(item.tipo-contr)) and (item-doc-est.nr-ord-prod = 0 
          or  (l-remessa and item-doc-est.baixa-ce 
               and item-doc-est.nr-ord-prod = 0)))
          or  (item.tipo-contr = 2 and l-devolucao = no and
               item-doc-est.nr-ord-prod = 0)
          or  ((l-retorno or l-remessa) and item-doc-est.nr-ord-prod = 0) 
          or  (avail ordem-compra and ordem-compra.num-ord-inv <> 0 AND param-global.modulo-in) 
          or  (l-sai-cons and (not item-doc-est.baixa-ce and
               item.tipo-contr <> 3)) then do:
               assign
               item-doc-est.ct-codigo = if  item-doc-est.ct-codigo <>
                                                ct-initial then
                                                    item-doc-est.ct-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-ct-componente
                                                else
                                                if avail ordem-compra
                                                then ordem-compra.ct-codigo
                                                else item.ct-codigo
               item-doc-est.sc-codigo = if  item-doc-est.sc-codigo <>
                                                sc-initial then
                                                    item-doc-est.sc-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-sc-componente
                                                else
                                                if avail ordem-compra
                                                then ordem-compra.sc-codigo
                                                else item.sc-codigo.
               if can-do("4,1",string(item.tipo-contr))
               or item-doc-est.ct-codigo <> ct-initial
               or item-doc-est.sc-codigo <> sc-initial
               or item-doc-est.baixa-ce = no then do:
               
                  assign i-empresa = param-global.empresa-prin.
                
                  &if defined (bf_dis_consiste_conta) &then
                     find docum-est where
                          docum-est.serie-docto  = item-doc-est.serie-docto  and
                          docum-est.nro-docto    = item-doc-est.nro-docto    and
                          docum-est.cod-emitente = item-doc-est.cod-emitente and
                          docum-est.nat-operacao = item-doc-est.nat-operacao no-lock no-error.
                     if avail docum-est then do:

                        find estabelec where
                             estabelec.cod-estabel = docum-est.cod-estabel no-lock no-error.
                    
                        run cdp/cd9970.p (input rowid(estabelec),
                                          output i-empresa).
                     end.

                  &endif
                      
                  /* 
                  find conta-contab where
                       conta-contab.ep-codigo = i-empresa              and
                       conta-contab.ct-codigo = item-doc-est.ct-codigo and
                       conta-contab.sc-codigo = item-doc-est.sc-codigo no-lock no-error.
                  */

                /*LOCALIZANDO PLANO CONTAS PRINCIPAL*/
                  FIND FIRST plano_cta_unid_organ
                       WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar
                         AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio"
                         AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
                         AND plano_cta_unid_organ.dat_fim_valid          >= TODAY 
                             NO-LOCK NO-ERROR. 
                  IF AVAIL plano_cta_unid_organ THEN DO:
                      FIND FIRST plano_cta_ctbl 
                           WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl
                                 NO-LOCK NO-ERROR.
                  END.

                  FIND FIRST cta_ctbl 
                       WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                         AND cta_ctbl.cod_cta_ctbl       = item-doc-est.ct-codigo
                             NO-LOCK NO-ERROR.
 
                  /*****Conta contabil n∆o cadastrada 70*****/
                  if not avail cta_ctbl then do: /*if not avail conta-contab then do:*/
                     i-conta-msg = i-conta-msg + 1.
                     run utp/ut-msgs.p ('msg',1060,''). 
                     create tt_erros_modulo.
                     assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                            tt_erros_modulo.cod-erro           = 1060
                            tt_erros_modulo.des-erro           = return-value
                            tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
                  end.

                 /*****Conta n∆o Ç de sistema 71*****/ 
                 IF AVAIL cta_ctbl AND cta_ctbl.ind_espec_cta_ctbl <> "Anal°tica" THEN DO:    /*if avail conta-contab and conta-contab.estado <> 3 then do:*/
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',443,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 443
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
                 END. 
                        
                 /*****Conta deve ser de despesa receita, ativo ou passivo 72*****/                        
                 IF AVAIL cta_ctbl AND NOT CAN-DO ("1,2,3,4",STRING (cta_ctbl.cod_grp_cta_ctbl)) THEN DO: /*if avail conta-contab and not can-do("1,2,4,5",string(conta-contab.tipo)) then do:*/
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',6365,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 6365
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
                 end.

                 /*
                 /*****Conta n∆o permite lanáamento de estoque 73*****/                        
                 if avail conta-contab and conta-contab.estoq = 0 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',445,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 445
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                 */
                 /*
                 /*****Conta n∆o aceita lanáamentos referentes a requisiá‰es 74*****/
                 if avail conta-contab and not can-do("1,2,3,5,6",string(conta-contab.estoq)) then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',107,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 107
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                 */
               end.
     end.
           
     find prazo-compra where prazo-compra.numero-ordem = item-doc-est.numero-ordem and
                        prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
     assign de-indice = 1.
     if item.tipo-contr <> 4 then do:
        find item-fornec where item-fornec.it-codigo = item-doc-est.it-codigo and
                                     item-fornec.cod-emite = item-doc-est.cod-emite no-lock no-error.
        if available item-fornec then do:
           assign de-indice = item-fornec.fator-conver / exp(10,item-fornec.num-casa-dec)
                  c-un      = item-fornec.unid-med-for. 
        end.
        else
            assign c-un      = item.un.
     end.
     else
     if avail ordem-compra then do:
        find cotacao-item where 
        cotacao-item.numero-ordem = ordem-compra.numero-ordem
        and cotacao-item.cod-emitente = ordem-compra.cod-emitente
        and cotacao-item.data-cotacao = ordem-compra.data-cotacao no-lock no-error.
        if available cotacao-item then do:
           assign c-un      = cotacao-item.un.
           find tab-conv-un where tab-conv-un.unid-med-for = cotacao-item.un
                              and tab-conv-un.un           = prazo-compra.un no-lock no-error.
           if available tab-conv-un then
           assign de-indice = tab-conv-un.fator-conver / exp(10,tab-conv-un.num-casa-dec).
        end.
        else assign  c-un      = item.un.
     end.
   
     assign de-qtd-estrut   = de-qtd-estrut * de-indice
                              item-doc-est.un = c-un.
     if  not l-devolucao and not l-transf and l-compras then 
     assign de-aux-1 = round(prazo-compra.quant-saldo +
            (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4)
            de-aux-2 = round(prazo-compra.quant-saldo - (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4).
     if (l-entrada and l-retorno = no and l-sai-cons = no) and
         not(l-entrada and docum-est.esp-docto = 20) then do:
         assign item-doc-est.quantidade = item-doc-est.qt-do-forn / de-indice.
     end. 
               
     /*Quantidade estaÔfora da variaá∆o permitida 74*****/  
     if l-entrada and not l-devolucao and not l-transf and l-compras then do:
        if item-doc-est.quantidade > de-aux-1 and param-re.aceita-var = no then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5928,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5928
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
     end. 

     /*****Quantidade n∆o pode ser fracionada 75*****/ 
     if not item.fraciona then do:
        if integer(item-doc-est.quantidade) <> item-doc-est.quantidade then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6381,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6381
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.     
     end.

     /*****Quantidade informada n∆o pode ser igual a 0 (zero) 76*****/
     if l-remessa or l-ent-cons or (l-transf and not l-entrada) then do:
        if item-doc-est.quantidade = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1521,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1521
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
     end.  

     /*****Deposito n∆o cadastrado 77*****/
     find deposito where deposito.cod-depos = item-doc-est.cod-depos no-lock no-error.
     if not avail deposito then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',530,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 530
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
     end. 
  
     /*****N∆o pode efetuar saida de CQ sem aprovaá∆o do controle de qualidade 78*****/              
     if  avail deposito and natur-oper.tipo = 2 
         and (deposito.ind-dep-cq = yes or deposito.ind-dep-rej = yes) 
         and item.contr-qualid = yes 
         and param-global.modulo-cq and param-cq.tipo-cq > 0 then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6382,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6382
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
     end. 
    
              
     /*****Localizaá∆o unica para o item 79****/
     if  avail item and item.loc-unica and item.cod-localiz <> item-doc-est.cod-localiz then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6383,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6383
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
     end. 
    
     /*****Lote deve ser diferente de branco 80*****/
     if  avail item and item.tipo-con-est <> 1 and 
         item-doc-est.lote = "" then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',1818,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 1818
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
     end.
 
     /*****Item controlado por numero de serie j† existente em outra sequencia do documento 81*****/              
     if item.tipo-con-est = 2 and item-doc-est.quantidade <> 0 then do:
        find first b-item-doc-est {esp/escd8900.i b-item-doc-est docum-est}
            and b-item-doc-est.it-codigo = item.it-codigo
            and b-item-doc-est.lote      = item-doc-est.lote
            and b-item-doc-est.sequencia <> item-doc-est.sequencia no-lock no-error.
        if avail b-item-doc-est then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6388,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6388
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.         
        /*****Item controlado por numero de serie j† existente em saldo de estoque 82*****/  
        find first b-saldo-estoq use-index lote where 
           b-saldo-estoq.lote = item-doc-est.lote
           and b-saldo-estoq.it-codigo = item.it-codigo
           and b-saldo-estoq.qtidade-atu <> 0 no-lock no-error.
        if avail b-saldo-estoq and ((b-saldo-estoq.qtidade-atu < 0 and  natur-oper.tipo = 2)
                         or (b-saldo-estoq.qtidade-atu > 0 and natur-oper.tipo = 1)) then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1252,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1252
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
     end.
  
     if natur-oper.tipo = 2 and item.tipo-contr <> 4 
                                and item-doc-est.baixa-ce and 
        not((l-entrada = no and not l-transf and not l-remessa and
        not l-sai-cons) and item-doc-est.nr-ord-prod <> 0) then do:
        find saldo-estoq use-index estabel-dep where
             saldo-estoq.cod-estab = docum-est.cod-estab        and 
             saldo-estoq.cod-depos = item-doc-est.cod-depos     and
             saldo-estoq.cod-localiz = item-doc-est.cod-localiz and
             saldo-estoq.lote = item-doc-est.lote               and
             saldo-estoq.it-codigo = item-doc-est.it-codigo     and
             saldo-estoq.cod-refer = item-doc-est.cod-refer no-lock no-error.
             de-quant = if avail saldo-estoq then
                        saldo-estoq.qtidade-atu -
                        saldo-estoq.qt-alocada  -
                        saldo-estoq.qt-aloc-ped -
                        saldo-estoq.qt-aloc-prod
                        else 0.
                    
        /*****Quantidade de saida maior que a disponivel em estoque 83*****/                       
        if l-consiste-saldo and item-doc-est.quantidade  > de-quant 
                    and item-doc-est.quantidade <> 0  
                    and item.perm-saldo-neg = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5266,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5266
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
     end.
      
        /*****Referància do lote n∆o confere com a referància informada 84*****/
     if item.tipo-con-est = 4 then do:
        find first saldo-estoq where saldo-estoq.it-codigo   = item-doc-est.it-codigo 
                                and saldo-estoq.lote        = item-doc-est.lote no-lock no-error.
        if avail saldo-estoq then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1785,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1785
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end. 
     end. 

           /*****Somat¢ria dos pesos dos itens Ç maior que peso toatal do documento 85*****/  
     if param-re.rateia-frete = 1 and docum-est.valor-frete > 0 and not l-servicos then do:
        assign de-tot-peso = 0.
        for each b-item-doc-est {esp/escd8900.i docum-est b-item-doc-est}:
            assign de-tot-peso = de-tot-peso + b-item-doc-est.peso-liquido.
        end.
        assign item-doc-est.peso-liquido = item.peso-liquido
                                          * item-doc-est.quantidade
                            de-tot-peso = de-tot-peso + item-doc-est.peso-liquido.
        if de-tot-peso > docum-est.tot-peso then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6394,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6394
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est-aux.serie-docto)    = ? then ' ' else string(tt-docum-est-aux.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est-aux.nro-docto)      = ? then ' ' else string(tt-docum-est-aux.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est-aux.cod-emitente)   = ? then ' ' else string(tt-docum-est-aux.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est-aux.nat-operacao)   = ? then ' ' else string(tt-docum-est-aux.nat-operacao)).
        end.    
     end.
     else do:
        if  l-servicos then 
        assign item-doc-est.peso-liquido = tt-item-doc-est.peso-liquido.
        if  l-servicos = no then do:
            assign item-doc-est.peso-liquido = item.peso-liquido
                                                  * item-doc-est.quantidade.
        end.
     end.
  
     if  param-re.rateia-desc and natur-oper.tipo =1 
           and not l-ent-cons and not l-retorno
           and not l-devolucao and not natur-oper.transf then do:
  
        assign item-doc-est.desconto[1] =
                         truncate(item-doc-est.preco-total[1] *
                         docum-est.tot-desconto *
                         integer(docum-est.valor-mercad <> 0) /
                         (docum-est.valor-mercad +
                         integer(docum-est.valor-mercad = 0)),2).
  
        for each b-item-doc-est {esp/escd8900.i b-item-doc-est docum-est} 
                  no-lock:
                  accum b-item-doc-est.preco-total(total).
                  accum b-item-doc-est.desconto(total).
        end.
        assign de-aux-1 = accum total b-item-doc-est.preco-total[1].
                     de-aux-2 = accum total b-item-doc-est.desconto[1].
  
        if de-aux-1 = docum-est.valor-mercad then
        if de-aux-2 <> docum-est.tot-desconto then
           assign item-doc-est.desconto[1] = item-doc-est.desconto[1] +
                                             docum-est.tot-desconto - de-aux-2.
  
        if item-doc-est.desconto[1] = ? then
                 assign item-doc-est.desconto[1] = 0.
     end.
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est-aux.serie-docto)  + chr(24) +
                                                      string(tt-docum-est-aux.nro-docto)    + chr(24) +
                                                      string(tt-docum-est-aux.cod-emitente) + chr(24) +
                                                      string(tt-docum-est-aux.nat-operacao)) then
        undo, next.
             
           /* calculo dos impostos */
           run esp/esreap152b.p(input table tt-docum-est,
                              rowid(item-doc-est),
                              input-output i-conta-msg,
                              input-output table tt_erros_modulo).
    

END PROCEDURE.  
