/******************************************************************************
** EMPRESA  : CAMIL
** PROGRAMA : ESAPI004
** DESCRICAO: ATUALIZAÄ«O DO RECEBIMENTO 
** AUTOR    : DATASUL METROPOLITANA
** DATA     : NOVEMBRO DE 2002
** VERSAO   : 2.04.00.000 - Marcos Hoff - Versao Inicial.
** set/2016 - SMF - Kraft - convers∆o campos totvs 12
*******************************************************************************/
/*{include/i-bfems2cad.i}*/
{utp/ut-glob.i}
{include/i-epc200.i esapi004} /** Upc **/

{rep/reapi151.i}  /*temp-table dcoum-est */
{rep/reapi151.i1} /*temp-table item-doc-est */
{cdp/cdapi150.i3} /*temp-table de versío */ 
{cdp/cdapi150.i4} /*temp-table de erros */
{esp/esbodi515.i tt-nota-fisc-adc}


DEF INPUT PARAM i-nr-ticket LIKE es-ticket.nr-ticket.
def input  parameter table for tt_versao_efetiv_edi.
def input  parameter table for tt-docum-est.
def input  parameter table for tt-item-doc-est.
def output parameter table for tt_erros_modulo.

/* Definiªío de Vari veis */ 

define temp-table tt-docum-est-aux NO-UNDO LIKE tt-docum-est.
define temp-table tt-item-doc-est-aux NO-UNDO LIKE tt-item-doc-est.
 
def buffer b-estab for estabelec.
def buffer b-docum-est for docum-est.


def var i-conta-msg           as integer              no-undo init 0.
def var cod-versao-integracao as integer format "999" no-undo. 
def new global shared var c-RE0301-usuario    like param-re.usuario no-undo.
def new global shared var r-RE0301-item-docto     as rowid     no-undo.
def new global shared var r-RE0301-documento      as rowid     no-undo. 
def new global shared var r-RE0301-documento-aux  as rowid     no-undo. 
def new global shared var c-RE0301-origem         as character no-undo.

def new shared var de-agreg-aca as de no-undo.
def new shared var de-mat-env-a as de no-undo.
def new shared var l-obrig as logical format "Sim/Nao" init no.
def new shared var l-erro-lote as logical no-undo.
def new shared var l-erro-fc   as logical no-undo init no.
def new shared var l-sem-valor as logical no-undo.
def new shared var l-sem-cotacao as logical no-undo.

def new shared var c-ult-ver   as character format "x(8)" no-undo.

def var l-industria as logical no-undo.
def var l-devolucao as logical no-undo.
def var l-comercio  as logical no-undo.
def var l-servicos  as logical no-undo.
def var l-remessa   as logical no-undo.
def var l-retorno   as logical no-undo.
def var l-entrada   as logical no-undo.
def var l-transf    as logical no-undo.
def var l-ent-cons  as logical no-undo.
def var l-sai-cons  as logical no-undo.

def var de-aux-1 as decimal no-undo.
def var de-aux-2 as decimal no-undo.
def var de-tot-desp as decimal no-undo.
def var de-desp as decimal no-undo.
def var de-vl-frete as decimal no-undo.

def var l-resposta  as logical no-undo.
def var l-peso      as logical no-undo.
def var l-notas     as logical no-undo.
def var l-descontos as logical no-undo.
def var l-valor     as logical no-undo.
def var l-cif-fob   as logical no-undo.
def var l-cliente   as logical format "CIF/FOB" no-undo.
def var c-natureza   like docum-est.nat-operacao no-undo.
def var rw-registro  as rowid no-undo.

DEF VAR i-nr-docto    AS INT FORMAT "9999999" INIT 0 NO-UNDO.
def var c-docto-aux like docum-est.nro-docto.

DEF VAR de-cont        AS INT NO-UNDO.
DEF VAR i-esp-docto    AS INT NO-UNDO.
DEF VAR i-tipo-docto   AS INT NO-UNDO.
def var v_tt_campo     as char no-undo.
def var v_tt_campo_aux as char no-undo.
def var v_num_add      as dec  no-undo.
def var v-tt-valor     as DEC  no-undo.
def var v-tt-val-qtd   as DEC  no-undo.

DEF NEW GLOBAL SHARED VAR h-bodi515       AS HANDLE NO-UNDO.


/************************ ATRIBUI∞ÄO DA VERSÄO DE INTEGRA∞ÄO ************************/
assign cod-versao-integracao = 001.

find first tt_versao_efetiv_edi no-lock no-error.
if tt_versao_efetiv_edi.tta_cdn_versao_integracao <> cod-versao-integracao
then do:
    return "NOK".       
end.

find first docum-est no-lock no-error.
find first param-estoq no-lock no-error.
find first param-global no-lock no-error.
FIND FIRST estabelec NO-LOCK NO-ERROR.

/*****Os par≥metros globais nío estío cadastrados 01 *****/
if  not avail param-global then do:
    i-conta-msg = i-conta-msg + 1.                             
    run utp/ut-msgs.p ('msg',2314,''). 
    create tt_erros_modulo.
    assign tt_erros_modulo.identifi-msg       = ''
           tt_erros_modulo.num-sequencia-erro = i-conta-msg
           tt_erros_modulo.cod-erro           = 2314
           tt_erros_modulo.des-erro           = return-value.
    return.
end.              
    
/*****Os par≥metros de estoque nío estío cadastrados 02 *****/    
if  not avail param-estoq then do:
    i-conta-msg = i-conta-msg + 1.                             
    run utp/ut-msgs.p ('msg',2316,''). 
    create tt_erros_modulo.
    assign tt_erros_modulo.identifi-msg       = ''
           tt_erros_modulo.num-sequencia-erro = i-conta-msg
           tt_erros_modulo.cod-erro           = 2316
           tt_erros_modulo.des-erro           = return-value.
    return.
end.                       

ASSIGN l-sem-valor = YES.

FIND es-ticket NO-LOCK WHERE es-ticket.nr-ticket = i-nr-ticket NO-ERROR.

IF es-ticket.peso-liq <> es-ticket.peso-desmem[1] THEN DO:
    IF es-ticket.peso-desmem[1] < es-ticket.peso-liq THEN DO:
        
        RUN pi-gera-nota-atual.   

        for each tt-docum-est transaction:

            FIND FIRST docum-est 
                WHERE  docum-est.serie-docto  = tt-docum-est.serie-docto
                   AND docum-est.nro-docto    = tt-docum-est.nro-docto
                   and docum-est.cod-emitente = tt-docum-est.cod-emitente
                   and docum-est.nat-operacao = tt-docum-est.nat-operacao 
                   NO-LOCK USE-INDEX documento NO-ERROR.
            
            IF AVAIL docum-est THEN DO:

                FIND FIRST docum-est-ext 
                    WHERE  docum-est-ext.serie-docto  = docum-est.serie-docto  
                       AND docum-est-ext.nro-docto    = docum-est.nro-docto    
                       and docum-est-ext.cod-emitente = docum-est.cod-emitente 
                       and docum-est-ext.nat-operacao = docum-est.nat-operacao 
                       NO-LOCK USE-INDEX documento NO-ERROR.

                IF NOT AVAIL docum-est-ext THEN DO:

                    CREATE docum-est-ext.
                    ASSIGN docum-est-ext.serie-docto         = docum-est.serie-docto 
                           docum-est-ext.nro-docto           = docum-est.nro-docto   
                           docum-est-ext.cod-emitente        = docum-est.cod-emitente
                           docum-est-ext.nat-operacao        = docum-est.nat-operacao
                           docum-est-ext.log-identifica-nota = YES.
                END.
            END.
        END.
    END.
    ELSE DO:
        
        RUN pi-gera-nota-devol.
    END.
        

END.
ELSE DO:
    
    RUN pi-gera-nota-atual.
END.

PROCEDURE pi-gera-nota-atual.


    for each tt-docum-est transaction:

        assign i-conta-msg = 0.
                   
        /*****Número de documento deve ser diferentes de brancos 04 *****/           
        if  tt-docum-est.nro-docto = " " or tt-docum-est.nro-docto = ? then do:  
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',5579,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 5597
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
        
        /*****Emitente nío pode ser desconhecido 05 *****/
        if  tt-docum-est.cod-emitente = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6189,'').         
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6189
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.        
        
        /*****Emitente nío cadastrado 06 *****/
        find emitente where emitente.cod-emitente = tt-docum-est.cod-emitente no-lock no-error.
        if  not avail emitente then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',2804,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 2804
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
       
        /*****Estabelecimento nío cadastrado 07 *****/
        find estabelec where estabelec.cod-estabel = tt-docum-est.cod-estabel no-lock no-error.
        if  not avail estabelec then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',537,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 537
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Estabelecimento fiscal nío cadastrado 08 *****/
        if  not can-find(estabelec where estabelec.cod-estabel = tt-docum-est.estab-fisc) then do: 
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6694,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6694
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Estabelecimento de destino deve ser diferente do estabelecimento emitente 09 *****/      
        if  tt-docum-est.estab-de-or = tt-docum-est.cod-estabel then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',213,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 213
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Serie nío cadastrada 10 *****/
        find  serie where serie.serie = tt-docum-est.serie-docto no-lock no-error.
        if  not avail serie then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',5580,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 5580
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Natureza de operaªío nío cadastrada 11 *****/ 
        find natur-oper where natur-oper.nat-operacao = tt-docum-est.nat-operacao no-lock no-error.
        if  not avail natur-oper then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',2050,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 2050
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Tipo de Movimentaªío da natureza de operaªío inv lida 12 *****/
        if  avail natur-oper and natur-oper.tipo = ? then do:    
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6200,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6200
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
   
    /*****Documento j  cadastrado 03 *****/
        FIND FIRST docum-est 
             WHERE  docum-est.serie-docto  = tt-docum-est.serie-docto
                AND docum-est.nro-docto    = tt-docum-est.nro-docto
                and docum-est.cod-emitente = tt-docum-est.cod-emitente
                and docum-est.nat-operacao = tt-docum-est.nat-operacao 
            NO-LOCK USE-INDEX documento NO-ERROR.
        
            IF  AVAIL docum-est THEN DO:
        /*         MESSAGE "Documento: " tt-docum-est.nro-docto  " j† cadastrado no Recebimento " */
        /*         "para o Fornecedor: " STRING(tt-docum-est.cod-emitente)                        */
        /*         VIEW-AS ALERT-BOX ERROR.                                                       */
                /* erika entra direto para abortar! 04/02/2012 */
                
            
                    RUN esp/MESSAGE.p (INPUT "Documento: " + tt-docum-est.nro-docto + " j† cadastrado no Recebimento.",
                                       INPUT "Documento: " + tt-docum-est.nro-docto + " j† cadastrado no Recebimento " +
                                             "para o Fornecedor: " + STRING(tt-docum-est.cod-emitente) + 
                                             ".        Verificar o N£mero da Nota do Fornecedor!").        
                    
                    ASSIGN i-conta-msg = i-conta-msg + 1.
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 9999
                           tt_erros_modulo.des-erro           = "Documento: " + tt-docum-est.nro-docto + " j† cadastrado no Recebimento."
                           tt_erros_modulo.identifi-msg       = 
                           string(if string(tt-docum-est.serie-docto)    = ? 
                           then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                           string(if string(tt-docum-est.nro-docto)      = ? 
                           then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                           string(if string(tt-docum-est.cod-emitente)   = ? 
                           then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                           string(if string(tt-docum-est.nat-operacao)   = ? 
                           then ' ' else string(tt-docum-est.nat-operacao)).
            END.
      
       
        /*****Usu rio nío cadastrado nos parametros do usu rio 14 *****/
        find param-re where param-re.usuario = tt-docum-est.usuario no-lock no-error.
        if  not avail param-re then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6203,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6203
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.   
        else 
            assign c-RE0301-usuario = param-re.usuario. 
               
        /******Natureza de operaªío nío pode atualizar estat≠stica de faturamento 15 *****/
        if  param-global.modulo-ft and avail natur-oper and natur-oper.imp-nota then do:
        
            if  natur-oper.atual-estat or natur-oper.baixa-estoq then do:    
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',5186,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 5186
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            END.

            /*****Natureza operaªío nío pode baixar estoque 16*****/
            if  natur-oper.baixa-estoq then do:
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',5184,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 5184
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            end.
        END.
     
        /*****Nío ≤ possivel devolver a cliente 17*****/
        if  avail natur-oper and avail emitente and  natur-oper.tipo = 2              and 
            not can-do('3,2',string(emitente.identific)) and not natur-oper.terceiros and 
            not natur-oper.transf then do:
          
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',5107,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 5107
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
              
         /*****Empresa possui apenas um estabelec . Transferencia impossivel 18*****/  
        if  avail natur-oper and natur-oper.transf and natur-oper.tipo = 2 and not param-estoq.estab-uni then do:
            
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6215,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6215
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
               
        /*****Data invalida 19*****/
        if  avail natur-oper and not(natur-oper.transf and natur-oper.tipo = 1) then do:
    
            if  not(tt-docum-est.dt-trans >= tt-docum-est.dt-emissao and 
                    tt-docum-est.dt-trans >= estabelec.medio-ate   and 
                    tt-docum-est.dt-trans <= today) then do:
    
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',173,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 173
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            END.
 
        /******Data de transaªío esta fora do parametros estipulados 20*****/
        if  avail param-re and tt-docum-est.dt-trans < (today - param-re.var-atual) then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6217,''). 
            create tt_erros_modulo. 
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6217
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        END.


        /*****Relaªío serie  x estabelecimento nío cadastrado 21*****/           
        if  avail serie and serie.forma-emis = 1     and 
            avail natur-oper and natur-oper.imp-nota and 
            param-global.modulo-ft then do:
           
            find ser-estab where
                 ser-estab.serie       = tt-docum-est.serie-docto and
                 ser-estab.cod-estabel = tt-docum-est.cod-estabel no-lock no-error.
                if  not avail ser-estab then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',5172,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 5172
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                           string(if string(tt-docum-est.serie-docto)    = ? 
                           then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                           string(if string(tt-docum-est.nro-docto)      = ? 
                           then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                           string(if string(tt-docum-est.cod-emitente)   = ? 
                           then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                           string(if string(tt-docum-est.nat-operacao)   = ? 
                           then ' ' else string(tt-docum-est.nat-operacao)).
                end.                       
                else do:
                    /*****Data da nota nao pode ser maior que a data do proximo faturamneto 22*****/  
                    if  ser-estab.ind-prox-dt and tt-docum-est.dt-trans > ser-estab.dt-prox-fat then do:
                        i-conta-msg = i-conta-msg + 1.
                        run utp/ut-msgs.p ('msg',6218,''). 
                        create tt_erros_modulo.
                        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                               tt_erros_modulo.cod-erro           = 6218
                               tt_erros_modulo.des-erro           = return-value
                               tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? 
                               then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? 
                               then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? 
                               then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? 
                               then ' ' else string(tt-docum-est.nat-operacao)).
                    end.
                    /******Data da nota nío pode ser menor que a data do ultimo faturamento 23*****/
                    if  tt-docum-est.dt-trans < ser-estab.dt-ult-fat then do:
                        i-conta-msg = i-conta-msg + 1.
                        run utp/ut-msgs.p ('msg',6708,''). 
                        create tt_erros_modulo.
                        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                               tt_erros_modulo.cod-erro           = 6708
                               tt_erros_modulo.des-erro           = return-value
                               tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? 
                               then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? 
                               then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? 
                               then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? 
                               then ' ' else string(tt-docum-est.nat-operacao)).                            
                    end.
                END.
            END.
        END.
    
        if  can-find(first tt_erros_modulo where
                           tt_erros_modulo.cod-erro    <> 5268 AND  
                           tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                          string(tt-docum-est.nro-docto)    + chr(24) +
                                                          string(tt-docum-est.cod-emitente) + chr(24) +
                                                          string(tt-docum-est.nat-operacao)) then
            undo, next.
        
        create docum-est.
        assign docum-est.esp-docto    = tt-docum-est.esp-docto
               docum-est.nro-docto    = tt-docum-est.nro-docto
               docum-est.serie-docto  = tt-docum-est.serie-docto
               docum-est.nat-operacao = tt-docum-est.nat-operacao 
               docum-est.cod-emitente = tt-docum-est.cod-emitente
               docum-est.cod-observa  = tt-docum-est.cod-observa
               docum-est.aliquota-icm = natur-oper.aliquota-icm
               docum-est.aliquota-iss = tt-docum-est.aliquota-iss
               docum-est.base-icm     = tt-docum-est.base-icm
               docum-est.base-ipi     = tt-docum-est.base-ipi 
               docum-est.base-iss     = tt-docum-est.base-iss
               docum-est.base-subs    = tt-docum-est.base-subs
               docum-est.cod-estabel  = tt-docum-est.cod-estabel
               docum-est.despesa-nota = tt-docum-est.despesa-nota
               docum-est.dt-emissao   = tt-docum-est.dt-emissao
               docum-est.dt-trans     = tt-docum-est.dt-trans
               docum-est.dt-venc-icm  = tt-docum-est.dt-venc-icm
               docum-est.dt-venc-ipi  = tt-docum-est.dt-venc-ipi
               docum-est.dt-venc-iss  = tt-docum-est.dt-venc-iss
               docum-est.estab-de-or  = tt-docum-est.estab-de-or
               docum-est.estab-fisc   = tt-docum-est.estab-fisc
               docum-est.estorn-comis = tt-docum-est.estorn-comis
               docum-est.icm-deb-cre  = tt-docum-est.icm-deb-cre
               docum-est.icm-fonte    = tt-docum-est.icm-fonte
               docum-est.ipi-deb-cre  = tt-docum-est.ipi-deb-cre
               docum-est.iss-deb-cre  = tt-docum-est.iss-deb-cre
               docum-est.mod-frete    = tt-docum-est.mod-frete
               docum-est.nff          = tt-docum-est.nff
               docum-est.tot-desconto = tt-docum-est.tot-desconto
               docum-est.tot-peso     = tt-docum-est.tot-peso
               docum-est.tot-valor    = tt-docum-est.tot-valor
               docum-est.uf           = tt-docum-est.uf
               docum-est.usuario      = tt-docum-est.usuario
               docum-est.valor-embal  = tt-docum-est.valor-embal
               docum-est.valor-frete  = tt-docum-est.valor-frete
               docum-est.valor-mercad = tt-docum-est.valor-mercad
               docum-est.valor-outras = tt-docum-est.valor-outras
               docum-est.valor-seguro = tt-docum-est.valor-seguro
               docum-est.via-transp   = tt-docum-est.via-transp
               docum-est.vl-subs      = tt-docum-est.vl-subs
               docum-est.observacao   = tt-docum-est.observacao
               /* 190805 */
               docum-est.esp-fiscal   = tt-docum-est.esp-fiscal
               docum-est.pais-origem  = tt-docum-est.pais-origem
    
               /* 23/11/2011 */
               docum-est.cidade       = tt-docum-est.cidade          
               docum-est.uf           = tt-docum-est.uf              
               docum-est.pais         = tt-docum-est.pais            
               docum-est.bairro       = tt-docum-est.bairro          
               docum-est.cep          = tt-docum-est.cep             
               docum-est.endereco     = tt-docum-est.endereco        
               docum-est.cod-entrega  = tt-docum-est.cod-entrega.

        assign r-RE0301-documento    = rowid(docum-est)
               c-RE0301-origem       = "RE0301"      
               c-RE0301-usuario      = docum-est.usuario.
      
        if  natur-oper.transf then
            assign docum-est.esp-docto = 23.
        else
            if  natur-oper.terceiros then do:
                if  natur-oper.tipo = 1 then
                    docum-est.esp-docto = 21.
                if  natur-oper.tipo = 2 or natur-oper.tipo = 3 then 
                    docum-est.esp-docto = 22.
            end.
            else do:
                assign docum-est.esp-docto = if natur-oper.tipo = 1 then 21
                                                else 20 .
            end.
             
        if  natur-oper.tipo = 1  and docum-est.esp-docto = 20 then 
            assign docum-est.cod-observa = 2.
       
        {rep/re0152.i} 
    
       
        /*****Estabelecimento de destino nío cadastrado 25*****/
        find b-estab where b-estab.cod-estabel = tt-docum-est.estab-de-or no-lock no-error.
            if  not available b-estab and l-transf then do:
            
            
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',4256,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 4256
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
            END.
        
        /*****Documento ja existente em O.F. com data diferente 31*****/ 
        if  not(natur-oper.transf and natur-oper.tipo = 1 ) then do:                   
            assign l-obrig = no.
            if  not(param-global.modulo-ft and docum-est.esp-docto = 22) and param-global.modulo-of then do:
            find doc-fiscal where
                  doc-fiscal.cod-estabel  = docum-est.estab-fisc   and
                  doc-fiscal.serie        = docum-est.serie-docto  and
                  doc-fiscal.nr-doc-fis   = docum-est.nro-docto    and
                  doc-fiscal.cod-emitente = docum-est.cod-emitente and
                  doc-fiscal.nat-operacao = docum-est.nat-operacao no-lock no-error.
                if  available doc-fiscal then do:
                    if  doc-fiscal.dt-docto <> docum-est.dt-trans then do:
                        i-conta-msg = i-conta-msg + 1.
                        run utp/ut-msgs.p ('msg',6275,''). 
                        create tt_erros_modulo.
                        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                               tt_erros_modulo.cod-erro           = 6275
                               tt_erros_modulo.des-erro           = return-value
                               tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? 
                               then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? 
                               then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? 
                               then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? 
                               then ' ' else string(tt-docum-est.nat-operacao)).
                    END.
                END.
            END.
        END.

    assign docum-est.ct-transit = if  l-entrada and not l-transf   and not 
                                      l-remessa and not l-retorno  and
                                      docum-est.esp-docto <> 20 then
                                      param-estoq.ct-tr-fornec 
                                                                                                                          
                                  else if not l-entrada  and
                                       not l-transf and
                                       not l-remessa and not l-retorno then
                                       param-estoq.ct-tr-devol                          
                                             
                                  else if l-entrada  and
                                       docum-est.esp-docto = 20 then
                                       param-estoq.ct-tr-dev-cli 
    
                                  else if l-transf then
                                       param-estoq.ct-tr-transf
    
                                  else if l-sai-cons or l-ent-cons then
                                             param-estoq.ct-tr-consig
    
                                  else if l-remessa or l-retorno then
                                       param-estoq.ct-terceiros
       
                                  else ''. 
       
       docum-est.sc-transit = if  l-entrada  and not l-transf and not 
                                  l-remessa and not l-retorno and
                                  docum-est.esp-docto <> 20 then
                                  param-estoq.sc-tr-fornec  else 
  
                                  if not l-entrada  and 
                                     not l-transf and
                                     not l-remessa and not l-retorno then
                                         param-estoq.sc-tr-devol 

                                  else if l-entrada  and
                                       docum-est.esp-docto = 20 then
                                       param-estoq.sc-tr-dev-cli 
                                       
                                  else if l-transf then
                                         param-estoq.sc-tr-transf
  
                                  else if l-sai-cons or l-ent-cons then
                                       param-estoq.sc-tr-consig
   
                                  else if l-remessa or l-retorno then
                                       param-estoq.sc-terceiros
                                  else ''.


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
             AND cta_ctbl.cod_cta_ctbl       = docum-est.ct-transit
                 NO-LOCK NO-ERROR.
    
    /*****Conta transitΩria nío existe  34*****/  
    IF NOT AVAILABLE cta_ctbl THEN DO: /*if  not available conta-contab then do:*/
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',1781,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 1781
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
               string(if string(tt-docum-est.serie-docto)    = ? 
               then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
               string(if string(tt-docum-est.nro-docto)      = ? 
               then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
               string(if string(tt-docum-est.cod-emitente)   = ? 
               then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
               string(if string(tt-docum-est.nat-operacao)   = ? 
               then ' ' else string(tt-docum-est.nat-operacao)).
    end.   
    else do:

    /*****Conta nío ≤ de sistema 35*****/
        IF AVAIL cta_ctbl AND cta_ctbl.ind_espec_cta_ctbl <> "Anal°tica" THEN DO: /*if  conta-contab.estado <> 3 then do:*/
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',443,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 443
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        END.

    END.
 
   
    assign l-resposta  = yes
           l-peso      = not l-servicos
           l-descontos = not l-transf      and
                             l-entrada     and 
                             not l-retorno and 
                             not l-ent-cons
           l-valor     = yes
           l-notas     = l-retorno         or 
                         l-sai-cons        or 
                         l-devolucao.                                
  

    run esp/esreap152.p (INPUT  es-ticket.nr-ticket,     
                         input  table tt-docum-est,      
                         input  table tt-docum-est-aux,  
                         INPUT  TABLE tt-item-doc-est,   
                         INPUT  TABLE tt-item-doc-est-aux,
                         input-output i-conta-msg,
                         input-output table tt_erros_modulo). /* atualiza item-doc-est */ 


                     
    if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.cod-erro     <> 5268 AND     
                       tt_erros_modulo.identifi-msg  = string(tt-docum-est.serie-docto)  + chr(24) +
                                                       string(tt-docum-est.nro-docto)    + chr(24) +
                                                       string(tt-docum-est.cod-emitente) + chr(24) +
                                                       string(tt-docum-est.nat-operacao)) then
        undo, next.
                    
    run esp/esreap152a.p (input rowid(docum-est),
                        input-output i-conta-msg,
                        input-output table tt_erros_modulo,
                        input table tt-docum-est).         
              
    FIND FIRST tt-item-doc-est OF docum-est NO-LOCK NO-ERROR.
    FIND FIRST item-doc-est OF docum-est NO-ERROR.
       
    IF AVAIL item-doc-est then
       ASSIGN item-doc-est.narrativa  = tt-item-doc-est.narrativa.
    
        if  can-find(first tt_erros_modulo where
                           tt_erros_modulo.cod-erro    <> 5268 AND 
                           tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                          string(tt-docum-est.nro-docto)    + chr(24) +
                                                          string(tt-docum-est.cod-emitente) + chr(24) +
                                                          string(tt-docum-est.nat-operacao)) THEN DO:
            undo, next.
        END.
    END. /* fim reapi152.p */    
END.

PROCEDURE pi-gera-nota-devol:

    for each tt-docum-est transaction:
    
        assign i-conta-msg = 0.
                   
        /*****Número de documento deve ser diferentes de brancos 04 *****/           
        if  tt-docum-est.nro-docto = " " or tt-docum-est.nro-docto = ? then do:  
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',5579,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 5597
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
        
        /*****Emitente nío pode ser desconhecido 05 *****/
        if  tt-docum-est.cod-emitente = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6189,'').         
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6189
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.        
        
        /*****Emitente nío cadastrado 06 *****/
        find emitente where emitente.cod-emitente = tt-docum-est.cod-emitente no-lock no-error.
        if  not avail emitente then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',2804,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 2804
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
       
        /*****Estabelecimento nío cadastrado 07 *****/
        find estabelec where estabelec.cod-estabel = tt-docum-est.cod-estabel no-lock no-error.
        if  not avail estabelec then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',537,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 537
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Estabelecimento fiscal nío cadastrado 08 *****/
        if  not can-find(estabelec where estabelec.cod-estabel = tt-docum-est.estab-fisc) then do: 
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6694,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6694
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Estabelecimento de destino deve ser diferente do estabelecimento emitente 09 *****/      
        if  tt-docum-est.estab-de-or = tt-docum-est.cod-estabel then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',213,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 213
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Serie nío cadastrada 10 *****/
        find  serie where serie.serie = tt-docum-est.serie-docto no-lock no-error.
        if  not avail serie then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',5580,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 5580
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Natureza de operaªío nío cadastrada 11 *****/ 
        find natur-oper where natur-oper.nat-operacao = tt-docum-est.nat-operacao no-lock no-error.
        if  not avail natur-oper then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',2050,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 2050
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
          
        /*****Tipo de Movimentaªío da natureza de operaªío inv lida 12 *****/
        if  avail natur-oper and natur-oper.tipo = ? then do:    
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6200,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6200
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.

        /*****Documento j  cadastrado 03 *****/
        FIND FIRST docum-est 
             WHERE  docum-est.serie-docto  = tt-docum-est.serie-docto
                AND docum-est.nro-docto    = tt-docum-est.nro-docto
                and docum-est.cod-emitente = tt-docum-est.cod-emitente
                and docum-est.nat-operacao = tt-docum-est.nat-operacao 
            NO-LOCK USE-INDEX documento NO-ERROR.
        
        IF  AVAIL docum-est THEN DO:
    /*         MESSAGE "Documento: " tt-docum-est.nro-docto  " j† cadastrado no Recebimento " */
    /*         "para o Fornecedor: " STRING(tt-docum-est.cod-emitente)                        */
    /*         VIEW-AS ALERT-BOX ERROR.                                                       */
            /* erika entra direto para abortar! 04/02/2012 */
            
        
                RUN esp/MESSAGE.p (INPUT "Documento: " + tt-docum-est.nro-docto + " j† cadastrado no Recebimento.",
                                   INPUT "Documento: " + tt-docum-est.nro-docto + " j† cadastrado no Recebimento " +
                                         "para o Fornecedor: " + STRING(tt-docum-est.cod-emitente) + 
                                         ".        Verificar o N£mero da Nota do Fornecedor!").        
                
                ASSIGN i-conta-msg = i-conta-msg + 1.
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 9999
                       tt_erros_modulo.des-erro           = "Documento: " + tt-docum-est.nro-docto + " j† cadastrado no Recebimento."
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
        END.

        /*****Usu rio nío cadastrado nos parametros do usu rio 14 *****/
        find param-re where param-re.usuario = tt-docum-est.usuario no-lock no-error.
        if  not avail param-re then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6203,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6203
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.   
        else 
            assign c-RE0301-usuario = param-re.usuario. 
               
        /******Natureza de operaªío nío pode atualizar estat≠stica de faturamento 15 *****/
        if  param-global.modulo-ft and avail natur-oper and natur-oper.imp-nota then do:
        
            if  natur-oper.atual-estat or natur-oper.baixa-estoq then do:    
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',5186,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 5186
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            end.  
    
            /*****Natureza operaªío nío pode baixar estoque 16*****/
            if  natur-oper.baixa-estoq then do:
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',5184,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 5184
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            end.
        end.
         
        /*****Nío ≤ possivel devolver a cliente 17*****/
        if  avail natur-oper and avail emitente and  natur-oper.tipo = 2              and 
            not can-do('3,2',string(emitente.identific)) and not natur-oper.terceiros and 
            not natur-oper.transf then do:
          
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',5107,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 5107
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.
              
         /*****Empresa possui apenas um estabelec . Transferencia impossivel 18*****/  
        if  avail natur-oper and natur-oper.transf and natur-oper.tipo = 2 and not param-estoq.estab-uni then do:
            
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6215,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6215
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
                   
        /*****Data invalida 19*****/
        if  avail natur-oper and not(natur-oper.transf and natur-oper.tipo = 1) then do:
    
            if  not(tt-docum-est.dt-trans >= tt-docum-est.dt-emissao and 
                    tt-docum-est.dt-trans >= estabelec.medio-ate   and 
                    tt-docum-est.dt-trans <= today) then do:
    
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',173,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 173
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            end.
     
            /******Data de transaªío esta fora do parametros estipulados 20*****/
            if  avail param-re and tt-docum-est.dt-trans < (today - param-re.var-atual) then do:
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',6217,''). 
                create tt_erros_modulo. 
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 6217
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            end.
    
    
            /*****Relaªío serie  x estabelecimento nío cadastrado 21*****/           
            if  avail serie and serie.forma-emis = 1     and 
                avail natur-oper and natur-oper.imp-nota and 
                param-global.modulo-ft then do:
               
                find ser-estab where
                     ser-estab.serie       = tt-docum-est.serie-docto and
                     ser-estab.cod-estabel = tt-docum-est.cod-estabel no-lock no-error.
                if  not avail ser-estab then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',5172,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 5172
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                           string(if string(tt-docum-est.serie-docto)    = ? 
                           then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                           string(if string(tt-docum-est.nro-docto)      = ? 
                           then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                           string(if string(tt-docum-est.cod-emitente)   = ? 
                           then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                           string(if string(tt-docum-est.nat-operacao)   = ? 
                           then ' ' else string(tt-docum-est.nat-operacao)).
                end.                       
                else do:
                    /****Data da nota nao pode ser maior que a data do proximo faturamneto 22****  */
                    if  ser-estab.ind-prox-dt and tt-docum-est.dt-trans > ser-estab.dt-prox-fat then do:
                        i-conta-msg = i-conta-msg + 1.
                        run utp/ut-msgs.p ('msg',6218,''). 
                        create tt_erros_modulo.
                        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                               tt_erros_modulo.cod-erro           = 6218
                               tt_erros_modulo.des-erro           = return-value
                               tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? 
                               then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? 
                               then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? 
                               then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? 
                               then ' ' else string(tt-docum-est.nat-operacao)).
                    end. 
                    /******Data da nota nío pode ser menor que a data do ultimo faturamento 23*****/
                    if  tt-docum-est.dt-trans < ser-estab.dt-ult-fat then do:
                        i-conta-msg = i-conta-msg + 1.
                        run utp/ut-msgs.p ('msg',6708,''). 
                        create tt_erros_modulo.
                        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                               tt_erros_modulo.cod-erro           = 6708
                               tt_erros_modulo.des-erro           = return-value
                               tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? 
                               then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? 
                               then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? 
                               then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? 
                               then ' ' else string(tt-docum-est.nat-operacao)).                            
                    end.
                end.            
            end.                                
        end.        
    
        /*****Unidade de federaªío nío cadatrada 24*****/   
        if  not can-find(unid-feder where
                         unid-feder.pais   = emitente.pais and
                         unid-feder.estado = tt-docum-est.uf) then do:
     
     
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',4256,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 4256
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     
        /*diego*/
        FIND FIRST estabelec-ext 
             WHERE estabelec-ext.cod-estabel = tt-docum-est.cod-estabel NO-LOCK NO-ERROR.
        if  avail estabelec-ext and estabelec-ext.cod-natur-oper-devol = "" then do:  
            ASSIGN i-conta-msg = i-conta-msg + 1.
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 9999
                   tt_erros_modulo.des-erro           = "Estabelecimento: " + tt-docum-est.cod-estabel + " n∆o gera Contra Nota. ê necess†rio informar no estabelecimento a Natureza de Operaá∆o do tipo Devoluá∆o."
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        end.

        if  can-find(first tt_erros_modulo where
                           tt_erros_modulo.cod-erro    <> 5268 AND  
                           tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                          string(tt-docum-est.nro-docto)    + chr(24) +
                                                          string(tt-docum-est.cod-emitente) + chr(24) +
                                                          string(tt-docum-est.nat-operacao)) then
            undo, next.
            
        RUN piCriaDocumEst (INPUT tt-docum-est.esp-docto,
                            INPUT tt-docum-est.tipo-docto,
                            INPUT tt-docum-est.nat-operacao,
                            INPUT tt-docum-est.nro-docto,
                            INPUT tt-docum-est.cod-emitente,
                            INPUT tt-docum-est.tot-peso,    
                            INPUT tt-docum-est.tot-valor,   
                            INPUT tt-docum-est.valor-mercad,
                            INPUT tt-docum-est.base-icm,
                            INPUT tt-docum-est.base-ipi).

        assign r-RE0301-documento    = rowid(docum-est)   
               c-RE0301-origem       = "RE0301"      
               c-RE0301-usuario      = docum-est.usuario.

        ASSIGN r-RE0301-documento-aux = r-RE0301-documento.

        run esp/esreap152.p (INPUT  es-ticket.nr-ticket,
                             input  table tt-docum-est,        
                             input  table tt-docum-est-aux,    
                             INPUT  TABLE tt-item-doc-est,     
                             INPUT  TABLE tt-item-doc-est-aux, 
                             input-output i-conta-msg,
                             input-output table tt_erros_modulo). /* atualiza item-doc-est */ 
                           
        if  can-find(first tt_erros_modulo where
                           tt_erros_modulo.cod-erro     <> 5268 AND     
                           tt_erros_modulo.identifi-msg  = string(tt-docum-est.serie-docto)  + chr(24) +
                                                           string(tt-docum-est.nro-docto)    + chr(24) +
                                                           string(tt-docum-est.cod-emitente) + chr(24) +
                                                           string(tt-docum-est.nat-operacao)) then
            undo, next.
        
        run esp/esreap152a.p (input rowid(docum-est),
                            input-output i-conta-msg,
                            input-output table tt_erros_modulo,
                            input table tt-docum-est).         
                  
        FIND FIRST tt-item-doc-est OF docum-est NO-LOCK NO-ERROR.
        FIND FIRST item-doc-est OF docum-est NO-ERROR.
           
        IF AVAIL item-doc-est then
           ASSIGN item-doc-est.narrativa  = tt-item-doc-est.narrativa.
        
        if  can-find(first tt_erros_modulo where
                           tt_erros_modulo.cod-erro    <> 5268 AND 
                           tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                          string(tt-docum-est.nro-docto)    + chr(24) +
                                                          string(tt-docum-est.cod-emitente) + chr(24) +
                                                          string(tt-docum-est.nat-operacao)) THEN DO:
            undo, next.
        END.
        
        assign v_tt_campo = tt-docum-est.nro-docto.

        repeat :
        
            FIND FIRST docum-est 
                WHERE  docum-est.serie-docto  = tt-docum-est.serie-docto
                   AND docum-est.nro-docto    = v_tt_campo
                   and docum-est.cod-emitente = tt-docum-est.cod-emitente
                   and docum-est.nat-operacao = tt-docum-est.nat-operacao NO-LOCK NO-ERROR.
    
            IF AVAIL docum-est THEN DO:

                assign v_num_add = dec(v_tt_campo) + 1
                   v_tt_campo_aux = string(v_num_add).
                if length(v_tt_campo_aux) <> length(v_tt_campo) then
                    assign v_tt_campo_aux = fill("0",length(v_tt_campo) - length(v_tt_campo_aux)) + v_tt_campo_aux.

                LEAVE.
                
            END.
            ELSE LEAVE.
            
        end.

        ASSIGN v-tt-valor   = 0.
        ASSIGN v-tt-val-qtd = 0.
        ASSIGN i-esp-docto  = 20.
        ASSIGN i-tipo-docto = 2.

        ASSIGN v-tt-valor = ((es-ticket.peso-desmem[1] - es-ticket.peso-liq) * es-ticket.vl-quilo).
        ASSIGN v-tt-val-qtd = es-ticket.peso-desmem[1] - es-ticket.peso-liq.

        FIND FIRST estabelec-ext NO-LOCK
             WHERE estabelec-ext.cod-estabel = tt-docum-est.cod-estabel NO-ERROR.

        RUN piCriaDocumEst (INPUT i-esp-docto,
                            INPUT i-tipo-docto,
                            INPUT estabelec-ext.cod-natur-oper-devol,
                            INPUT v_tt_campo_aux,
                            INPUT tt-docum-est.cod-emitente,
                            INPUT v-tt-val-qtd,
                            INPUT v-tt-valor,
                            INPUT v-tt-valor,
                            INPUT v-tt-valor,  
                            INPUT v-tt-valor). 

        CREATE tt-docum-est-aux.
        BUFFER-COPY tt-docum-est TO tt-docum-est-aux.
    
        ASSIGN tt-docum-est-aux.nat-operacao  = estabelec-ext.cod-natur-oper-devol
               tt-docum-est-aux.nro-docto     = v_tt_campo_aux
               tt-docum-est-aux.esp-docto     = i-esp-docto
               tt-docum-est-aux.tipo-docto    = i-tipo-docto.

        CREATE tt-item-doc-est-aux.
        BUFFER-COPY tt-item-doc-est TO tt-item-doc-est-aux.
    
        ASSIGN tt-item-doc-est-aux.nat-operacao  = estabelec-ext.cod-natur-oper-devol
               tt-item-doc-est-aux.nro-docto     = v_tt_campo_aux.

        assign r-RE0301-documento        = rowid(docum-est)
               r-RE0301-item-docto       = ROWID(item-doc-est)
               c-RE0301-origem           = "RE0301"      
               c-RE0301-usuario          = docum-est.usuario.

        run esp/esreap152.p (INPUT es-ticket.nr-ticket,
                             input  table tt-docum-est,        
                             input  table tt-docum-est-aux,    
                             INPUT  TABLE tt-item-doc-est,     
                             INPUT  TABLE tt-item-doc-est-aux, 
                             input-output i-conta-msg,
                             input-output table tt_erros_modulo). /* atualiza item-doc-est */ 
                         
        if  can-find(first tt_erros_modulo where
                           tt_erros_modulo.cod-erro     <> 5268 AND     
                           tt_erros_modulo.identifi-msg  = string(tt-docum-est.serie-docto)  + chr(24) +
                                                           string(tt-docum-est.nro-docto)    + chr(24) +
                                                           string(tt-docum-est.cod-emitente) + chr(24) +
                                                           string(tt-docum-est.nat-operacao)) then
            undo, next.
                        
        run esp/esreap152a.p (input rowid(docum-est),
                              input-output i-conta-msg,
                              input-output table tt_erros_modulo,
                              input table tt-docum-est).         
                  
        FIND FIRST tt-item-doc-est OF docum-est NO-LOCK NO-ERROR.
        FIND FIRST item-doc-est OF docum-est NO-ERROR.
           
        IF AVAIL item-doc-est then
           ASSIGN item-doc-est.narrativa  = tt-item-doc-est-aux.narrativa.
        
        if  can-find(first tt_erros_modulo where
                           tt_erros_modulo.cod-erro    <> 5268 AND 
                           tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                          string(tt-docum-est.nro-docto)    + chr(24) +
                                                          string(tt-docum-est.cod-emitente) + chr(24) +
                                                          string(tt-docum-est.nat-operacao)) THEN DO:
            undo, next.
        END.

        RUN pi-nota-adicional-dev.

        ASSIGN r-RE0301-documento        = r-RE0301-documento-aux.

        if  natur-oper.transf then
            assign docum-est.esp-docto = 23.
        else
            if  natur-oper.terceiros then do:
                if  natur-oper.tipo = 1 then
                    docum-est.esp-docto = 21.
                if  natur-oper.tipo = 2 or natur-oper.tipo = 3 then 
                    docum-est.esp-docto = 22.
            end.
            else do:
                assign docum-est.esp-docto = if natur-oper.tipo = 1 then 21
                                                else 20 .
            end.
             
        if  natur-oper.tipo = 1  and docum-est.esp-docto = 20 then 
            assign docum-est.cod-observa = 2.
       
        {rep/re0152.i} 
        
       
        /*****Estabelecimento de destino nío cadastrado 25*****/
        find b-estab where b-estab.cod-estabel = tt-docum-est.estab-de-or no-lock no-error.
            if  not available b-estab and l-transf then do:
                
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',4256,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 4256
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            END.
                
            /*****Tipo de documento invalido 27*****/                         
            if  l-devolucao or l-remessa or l-retorno or /*** l-transf or***/ l-entrada = no /*or  
                l-ent-cons*/  or l-sai-cons or docum-est.esp-docto = 20 then do:
        
                
                
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',3722,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 3722
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? 
                       then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? 
                       then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? 
                       then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? 
                       then ' ' else string(tt-docum-est.nat-operacao)).
            end. 
            
            if  can-find(first tt_erros_modulo where
                               tt_erros_modulo.cod-erro    <> 5268 AND  
                               tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                              string(tt-docum-est.nro-docto)    + chr(24) +
                                                              string(tt-docum-est.cod-emitente) + chr(24) +
                                                              string(tt-docum-est.nat-operacao)) then
              undo, next.
                    
                      
            /*****Documento ja existente em O.F. com data diferente 31*****/ 
            if  not(natur-oper.transf and natur-oper.tipo = 1 ) then do:                   
            assign l-obrig = no.
                if  not(param-global.modulo-ft and docum-est.esp-docto = 22) and param-global.modulo-of then do:
                find doc-fiscal where
                     doc-fiscal.cod-estabel  = docum-est.estab-fisc   and
                     doc-fiscal.serie        = docum-est.serie-docto  and
                     doc-fiscal.nr-doc-fis   = docum-est.nro-docto    and
                     doc-fiscal.cod-emitente = docum-est.cod-emitente and
                     doc-fiscal.nat-operacao = docum-est.nat-operacao no-lock no-error.
                    if  available doc-fiscal then do:
                        if  doc-fiscal.dt-docto <> docum-est.dt-trans then do:
                            i-conta-msg = i-conta-msg + 1.
                            run utp/ut-msgs.p ('msg',6275,''). 
                            create tt_erros_modulo.
                            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                                   tt_erros_modulo.cod-erro           = 6275
                                   tt_erros_modulo.des-erro           = return-value
                                   tt_erros_modulo.identifi-msg       = 
                                   string(if string(tt-docum-est.serie-docto)    = ? 
                                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                                   string(if string(tt-docum-est.nro-docto)      = ? 
                                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                                   string(if string(tt-docum-est.cod-emitente)   = ? 
                                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                                   string(if string(tt-docum-est.nat-operacao)   = ? 
                                   then ' ' else string(tt-docum-est.nat-operacao)).
                        END.
                    END.
                END.
            END.
    END.
    FIND CURRENT docum-est EXCLUSIVE-LOCK.
    assign docum-est.ct-transit = if  l-entrada and not l-transf   and not 
                                  l-remessa and not l-retorno  and
                                  docum-est.esp-docto <> 20 then
                                  param-estoq.ct-tr-fornec 
                                                                                                                      
                              else if not l-entrada  and
                                   not l-transf and
                                   not l-remessa and not l-retorno then
                                   param-estoq.ct-tr-devol                          
                                         
                              else if l-entrada  and
                                   docum-est.esp-docto = 20 then
                                   param-estoq.ct-tr-dev-cli 

                              else if l-transf then
                                   param-estoq.ct-tr-transf

                              else if l-sai-cons or l-ent-cons then
                                         param-estoq.ct-tr-consig

                              else if l-remessa or l-retorno then
                                   param-estoq.ct-terceiros
   
                              else ''. 
   
       docum-est.sc-transit = if  l-entrada  and not l-transf and not 
                                  l-remessa and not l-retorno and
                                  docum-est.esp-docto <> 20 then
                                  param-estoq.sc-tr-fornec  else 
  
                                  if not l-entrada  and 
                                     not l-transf and
                                     not l-remessa and not l-retorno then
                                         param-estoq.sc-tr-devol 

                                  else if l-entrada  and
                                       docum-est.esp-docto = 20 then
                                       param-estoq.sc-tr-dev-cli 
                                       
                                  else if l-transf then
                                         param-estoq.sc-tr-transf
  
                                  else if l-sai-cons or l-ent-cons then
                                       param-estoq.sc-tr-consig
   
                                  else if l-remessa or l-retorno then
                                       param-estoq.sc-terceiros
                                  else ''.
    FIND CURRENT docum-est NO-LOCK.

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
             AND cta_ctbl.cod_cta_ctbl       = docum-est.ct-transit
                 NO-LOCK NO-ERROR.

    /*****Conta transitΩria nío existe  34*****/  
    IF NOT AVAILABLE cta_ctbl THEN DO: /*if  not available conta-contab then do:*/
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',1781,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 1781
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
               string(if string(tt-docum-est.serie-docto)    = ? 
               then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
               string(if string(tt-docum-est.nro-docto)      = ? 
               then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
               string(if string(tt-docum-est.cod-emitente)   = ? 
               then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
               string(if string(tt-docum-est.nat-operacao)   = ? 
               then ' ' else string(tt-docum-est.nat-operacao)).
    end.   
    else do:

    /*****Conta nío ≤ de sistema 35*****/
        IF AVAIL cta_ctbl AND cta_ctbl.ind_espec_cta_ctbl <> "Anal°tica" THEN DO: /*if  conta-contab.estado <> 3 then do:*/
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',443,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 443
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? 
                   then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? 
                   then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? 
                   then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? 
                   then ' ' else string(tt-docum-est.nat-operacao)).
        END.
    END.
 
    assign l-resposta  = yes
           l-peso      = not l-servicos
           l-descontos = not l-transf      and
                             l-entrada     and 
                             not l-retorno and 
                             not l-ent-cons
           l-valor     = yes
           l-notas     = l-retorno         or 
                         l-sai-cons        or 
                         l-devolucao.                                
      
 

END.

PROCEDURE piCriaDocumEst:

    DEFINE INPUT PARAM pEspDocto  LIKE docum-est.esp-docto    NO-UNDO.
    DEFINE INPUT PARAM pTpDocto   LIKE docum-est.tipo-docto   NO-UNDO.
    DEFINE INPUT PARAM pNaturOper LIKE docum-est.nat-operacao NO-UNDO.
    DEFINE INPUT PARAM pNrDocto   LIKE docum-est.nro-docto    NO-UNDO.
    DEFINE INPUT PARAM pCodEmi    LIKE docum-est.cod-emitente NO-UNDO.
    DEFINE INPUT PARAM pTotPeso   LIKE docum-est.tot-peso     NO-UNDO. 
    DEFINE INPUT PARAM pTotVal    LIKE docum-est.tot-valor    NO-UNDO. 
    DEFINE INPUT PARAM pValMerc   LIKE docum-est.valor-mercad NO-UNDO. 
    DEFINE INPUT PARAM pBaseIcm   LIKE docum-est.base-icm     NO-UNDO. 
    DEFINE INPUT PARAM pBaseIpi   LIKE docum-est.base-ipi     NO-UNDO. 
    
        create docum-est.
        assign docum-est.esp-docto    = tt-docum-est.esp-docto
               docum-est.nro-docto    = pNrDocto
               docum-est.serie-docto  = tt-docum-est.serie-docto
               docum-est.nat-operacao = pNaturOper
               docum-est.cod-emitente = pCodEmi
               docum-est.tipo-docto   = pTpDocto
               docum-est.cod-observa  = tt-docum-est.cod-observa
               docum-est.aliquota-icm = natur-oper.aliquota-icm
               docum-est.aliquota-iss = tt-docum-est.aliquota-iss
               docum-est.base-icm     = pBaseIcm
               docum-est.base-ipi     = pBaseIpi
               docum-est.base-iss     = tt-docum-est.base-iss
               docum-est.base-subs    = tt-docum-est.base-subs
               docum-est.cod-estabel  = tt-docum-est.cod-estabel
               docum-est.despesa-nota = tt-docum-est.despesa-nota
               docum-est.dt-emissao   = tt-docum-est.dt-emissao
               docum-est.dt-trans     = tt-docum-est.dt-trans
               docum-est.dt-venc-icm  = tt-docum-est.dt-venc-icm
               docum-est.dt-venc-ipi  = tt-docum-est.dt-venc-ipi
               docum-est.dt-venc-iss  = tt-docum-est.dt-venc-iss
               docum-est.estab-de-or  = tt-docum-est.estab-de-or
               docum-est.estab-fisc   = tt-docum-est.estab-fisc
               docum-est.estorn-comis = tt-docum-est.estorn-comis
               docum-est.icm-deb-cre  = tt-docum-est.icm-deb-cre
               docum-est.icm-fonte    = tt-docum-est.icm-fonte
               docum-est.ipi-deb-cre  = tt-docum-est.ipi-deb-cre
               docum-est.iss-deb-cre  = tt-docum-est.iss-deb-cre
               docum-est.mod-frete    = tt-docum-est.mod-frete
               docum-est.nff          = tt-docum-est.nff
               docum-est.tot-desconto = tt-docum-est.tot-desconto
               docum-est.tot-peso     = pTotPeso
               docum-est.tot-valor    = pTotVal 
               docum-est.uf           = tt-docum-est.uf
               docum-est.usuario      = tt-docum-est.usuario
               docum-est.valor-embal  = tt-docum-est.valor-embal
               docum-est.valor-frete  = tt-docum-est.valor-frete
               docum-est.valor-mercad = pValMerc
               docum-est.valor-outras = tt-docum-est.valor-outras
               docum-est.valor-seguro = tt-docum-est.valor-seguro
               docum-est.via-transp   = tt-docum-est.via-transp
               docum-est.vl-subs      = tt-docum-est.vl-subs
               docum-est.observacao   = tt-docum-est.observacao
               docum-est.esp-fiscal   = tt-docum-est.esp-fiscal
               docum-est.pais-origem  = tt-docum-est.pais-origem
               docum-est.cidade       = tt-docum-est.cidade          
               docum-est.uf           = tt-docum-est.uf              
               docum-est.pais         = tt-docum-est.pais            
               docum-est.bairro       = tt-docum-est.bairro          
               docum-est.cep          = tt-docum-est.cep             
               docum-est.endereco     = tt-docum-est.endereco        
               docum-est.cod-entrega  = tt-docum-est.cod-entrega.

        CREATE docum-est-ext.
        ASSIGN docum-est-ext.serie-docto         = docum-est.serie-docto 
               docum-est-ext.nro-docto           = docum-est.nro-docto   
               docum-est-ext.cod-emitente        = docum-est.cod-emitente
               docum-est-ext.nat-operacao        = docum-est.nat-operacao
               docum-est-ext.log-identifica-nota = YES.

    
END PROCEDURE.

PROCEDURE pi-nota-adicional-dev:

    DEF VAR h-bodi515       AS HANDLE NO-UNDO.
    
    RUN dibo/bodi515.p PERSISTENT SET h-bodi515.
    
    RUN openQueryStatic IN h-bodi515 (INPUT "Main":U).
    
    CREATE tt-nota-fisc-adc.
    
    ASSIGN tt-nota-fisc-adc.cod-estab                = tt-docum-est.cod-estab
           tt-nota-fisc-adc.cod-serie                = tt-docum-est.serie
           tt-nota-fisc-adc.cod-nota-fisc            = tt-docum-est-aux.nro-docto 
           tt-nota-fisc-adc.cdn-emitente             = tt-docum-est.cod-emitente
           tt-nota-fisc-adc.cod-natur-operac         = tt-docum-est-aux.nat-operacao 
           tt-nota-fisc-adc.idi-tip-dado             = 3                           
           tt-nota-fisc-adc.cod-ser-docto-referado   = tt-docum-est.serie  
           tt-nota-fisc-adc.cod-docto-referado       = tt-docum-est.nro-docto     
           tt-nota-fisc-adc.cdn-emit-docto-referado  = tt-docum-est.cod-emitente
           tt-nota-fisc-adc.dat-docto-referado       = tt-docum-est.dt-emissao
           tt-nota-fisc-adc.idi-tip-emit-referado    = 1
           tt-nota-fisc-adc.idi-tip-docto-referado   = 2 .      

    RUN newRecord IN h-bodi515.
    RUN setRecord IN h-bodi515 (INPUT TABLE tt-nota-fisc-adc).
    RUN createRecord IN h-bodi515.

    DELETE PROCEDURE h-bodi515.

END PROCEDURE.
