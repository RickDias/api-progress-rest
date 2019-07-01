/* include de controle de vers∆o */
{include/i-prgvrs.i esint008 12.00.00.000}

{esp/esint008rp.i} /* -- Definitions -- */

def var bo-ped-venda     as handle no-undo.
def var bo-ped-venda-sdf as handle no-undo.
def var bo-ped-item      as handle no-undo.
def var bo-ped-item-sdf  as handle no-undo.
def var bo-ped-venda-cal as handle no-undo.
def var bo-ped-venda-com as handle no-undo.
def var bo-ped-repre     as handle no-undo.
def var bo-ped-venda-sus as handle no-undo.

/**************************************************************/

/* definiá∆o dos buffers para verificacao de erros */
DEF BUFFER bf-ped-venda  FOR ped-venda.
DEF BUFFER bf-emitente   FOR emitente.

def  input param TABLE for tt-param-ped-venda.
def  input param TABLE for tt-param-ped-item.
DEF OUTPUT PARAM TABLE FOR tt-param-RowErrors.

/* include padr∆o para vari†veis para o log  */
{include/i-rpvar.i}
 
/* ------------------------------------------  INICIO DO PROGRAMA  -----------------------------------------  */

DEFINE VARIABLE c-tipo-operacao         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE i-sequencia             AS INTEGER      NO-UNDO.
DEFINE VARIABLE i-nr-pedido-next        AS INTEGER      NO-UNDO.
DEFINE VARIABLE r-row-pedido            AS ROWID        NO-UNDO.

DEFINE VARIABLE h_acomp_rp              AS HANDLE       NO-UNDO.
DEFINE VARIABLE caminho-txt             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE arquivo-txt             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE memXML                  AS MEMPTR       NO-UNDO.
DEFINE VARIABLE lcXML                   AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE seq-iten-txt            AS CHARACTER    NO-UNDO.

def         var c-cIDProduct            as c            no-undo.    
DEF         VAR l-erro-item             AS L            NO-UNDO.


RUN utp/ut-acomp.p PERSISTENT SET h_acomp_rp.
RUN pi-inicializar IN h_acomp_rp (INPUT "Iniciando Importaáao dos Pedidos de Venda").
RUN pi-acompanhar  IN h_acomp_rp (INPUT "Importando Pedidos de Venda").

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "esint008.tmp").

if not valid-handle(bo-ped-venda) or
    bo-ped-venda:type <> "PROCEDURE":U or
    bo-ped-venda:file-name <> "dibo/bodi159.p" then
    run dibo/bodi159.p persistent set bo-ped-venda.
    
if not valid-handle(bo-ped-venda-sdf) or
    bo-ped-venda-sdf:type <> "PROCEDURE":U or
    bo-ped-venda-sdf:file-name <> "dibo/bodi159sdf.p" then
    run dibo/bodi159sdf.p persistent set bo-ped-venda-sdf.

if not valid-handle(bo-ped-venda-cal) or
    bo-ped-venda-cal:type <> "PROCEDURE":U or
    bo-ped-venda-cal:file-name <> "dibo/bodi159cal.p" then
    run dibo/bodi159cal.p persistent set bo-ped-venda-cal.

if not valid-handle(bo-ped-venda-com) or
    bo-ped-venda-com:type <> "PROCEDURE":U or
    bo-ped-venda-com:file-name <> "dibo/bodi159com.p" then
    run dibo/bodi159com.p persistent set bo-ped-venda-com.

if not valid-handle(bo-ped-item) or
    bo-ped-item:type <> "PROCEDURE":U or
    bo-ped-item:file-name <> "dibo/bodi154.p" then
    run dibo/bodi154.p persistent set bo-ped-item.

if not valid-handle(bo-ped-item-sdf) or
    bo-ped-item-sdf:type <> "PROCEDURE":U or
    bo-ped-item-sdf:file-name <> "dibo/bodi154sdf.p" then
    run dibo/bodi154sdf.p persistent set bo-ped-item-sdf.

if not valid-handle(bo-ped-repre) or
    bo-ped-repre:type <> "PROCEDURE":U or 
    bo-ped-repre:file-name <> "dibo/bodi157.p":U then do:
    run dibo/bodi157.p persistent set bo-ped-repre.
    run openQueryStatic in bo-ped-repre(input "Defaultpd4000":U).
end.

bloco-Heade:
FOR EACH tt-param-ped-venda NO-LOCK:
    EMPTY TEMP-TABLE tt-ped-venda.
    EMPTY TEMP-TABLE tt-ped-repre.
    EMPTY TEMP-TABLE tt-ped-item.

    RUN pi-acompanhar  IN h_acomp_rp (INPUT "Importando: " + tt-param-ped-venda.nome-abrev + " " + tt-param-ped-venda.nr-pedcli).
        
    ASSIGN
       l-erro-item           = no.
       
    FIND FIRST para-ped NO-LOCK NO-ERROR.

    ASSIGN c-tipo-operacao      = "".
    
    RUN setDefaultOrderNumber IN bo-ped-venda-sdf (OUTPUT i-nr-pedido-next).
    RUN openQueryStatic in bo-ped-venda(input "Defaultpd4000":u).

    CREATE tt-ped-venda.

    /* Informa o Cliente e Nro Pedido */
    ASSIGN 
        tt-ped-venda.nr-pedcli  = tt-param-ped-venda.nr-pedcli
        tt-ped-venda.nome-abrev = tt-param-ped-venda.nome-abrev
        tt-ped-venda.nr-pedido  = i-nr-pedido-next
        tt-ped-venda.tp-preco   = 1  /* Informado */
        tt-ped-venda.esp-ped    = 1. /*1-simples, 2-Programacao de entrega*/

    /* coloca valores Default para o Cliente */
    run emptyRowErrors in bo-ped-venda-sdf.
    run inputTable in bo-ped-venda-sdf (input table tt-ped-venda).
    run setDefaultCustomer in bo-ped-venda-sdf.
    run outputTable in bo-ped-venda-sdf (output table tt-ped-venda).
    run getRowErrors in bo-ped-venda-sdf(output table RowErrors).
    RUN pi-carga-erro.
    
    FIND FIRST tt-ped-venda 
         WHERE tt-ped-venda.nr-pedcli           = tt-ped-venda.nr-pedcli 
           AND tt-ped-venda.nome-abrev          = tt-ped-venda.nome-abrev
         NO-ERROR.
    
    ASSIGN tt-ped-venda.cod-estabel             = tt-param-ped-venda.cod-estabel
           tt-ped-venda.dt-implant              = TODAY
           tt-ped-venda.nr-pedrep               = tt-param-ped-venda.nr-pedrep
           tt-ped-venda.nome-abrev              = tt-param-ped-venda.nome-abrev
           tt-ped-venda.no-ab-rep               = tt-param-ped-venda.no-ab-rep
           tt-ped-venda.nat-operacao            = tt-param-ped-venda.nat-operacao
           tt-ped-venda.dt-emissao              = tt-param-ped-venda.dt-emissao
           tt-ped-venda.dt-entrega              = tt-param-ped-venda.dt-entrega
           tt-ped-venda.dt-entori               = tt-param-ped-venda.dt-entori 
           tt-ped-venda.cod-des-merc            = 1 /* Comercio/Indústria */
           tt-ped-venda.ind-fat-par             = tt-param-ped-venda.ind-fat-par 
           tt-ped-venda.mo-codigo               = tt-param-ped-venda.mo-codigo   
           tt-ped-venda.ind-lib-nota            = tt-param-ped-venda.ind-lib-nota
           tt-ped-venda.ind-tp-frete            = tt-param-ped-venda.ind-tp-frete
           tt-ped-venda.nr-ind-finan            = tt-param-ped-venda.nr-ind-finan
           tt-ped-venda.ind-antecip             = no
           tt-ped-venda.cd-origem               = 2 /* EDI */
           tt-ped-venda.origem                  = 4 /* Bacth */
           tt-ped-venda.cod-sit-aval            = 1
           tt-ped-venda.log-usa-tabela-desconto = no
           tt-ped-venda.log-ped-bonif-pendente  = no
           tt-ped-venda.log-cotacao             = no
           tt-ped-venda.nr-tab-finan            = 1
           tt-ped-venda.observacoes             = tt-param-ped-venda.observacoes 
           tt-ped-venda.cod-cond-pag            = tt-param-ped-venda.cod-cond-pag.

    if tt-ped-venda.ind-tp-frete = 1
    then assign
        tt-ped-venda.cidade-cif              = "CIF" /*(if   avail loc-entr
                                               then loc-entr.nom-cidad-cif
                                               else "")*/ .
    
    for first estab-cli no-lock where
        estab-cli.cod-estabel = tt-param-ped-venda.cod-estabel and
        estab-cli.nome-abrev  = tt-param-ped-venda.nome-abrev:
    end.
    if avail estab-cli 
    then assign 
       tt-ped-venda.cod-rota = estab-cli.cod-rota.
    
    run emptyRowErrors  in bo-ped-venda.
    run inputRowParam   in bo-ped-venda(input table rowpedparam).
    run setRecord       in bo-ped-venda(input table tt-ped-venda).
    run createRecord    in bo-ped-venda.
    run getrowErrors    in bo-ped-venda(output table RowErrors).
    RUN pi-carga-erro.
    if can-find(first RowErrors where
                rowErrors.errorType <> 'INTERNAL':u and
                rowErrors.errorSubType begins 'ERRO':U no-lock) then do:
    end.
    else do:
        /*
        for first ped-venda where
            ped-venda.nome-abrev    = tt-ped-venda.nome-abrev and
            ped-venda.nr-pedcli     = tt-ped-venda.nr-pedcli and
            ped-venda.no-ab-reppri <> "":
            empty temp-table tt-ped-repre.
            create tt-ped-repre. 
            assign tt-ped-repre.nr-pedido    = ped-venda.nr-pedido
                   tt-ped-repre.nome-ab-rep  = repres.nome-abrev
                   tt-ped-repre.perc-comis   = repres.comis-direta
                   tt-ped-repre.comis-emis   = repres.comis-emis
                   tt-ped-repre.ind-repbase  = yes.
            
            run emptyRowErrors in bo-ped-repre.
            run setRecord      in bo-ped-repre(input table tt-ped-repre).
            run createRecord   in bo-ped-repre.
            run getRowErrors   in bo-ped-repre(output table RowErrors).
        end.
        */

        ASSIGN i-sequencia = 0.
        IF NOT CAN-FIND(FIRST RowErrors 
                        WHERE rowErrors.errorType <> 'INTERNAL':u 
                          AND rowErrors.errorSubType BEGINS 'ERRO':U)
        THEN DO:
            ASSIGN i-sequencia = 10.
            FOR EACH tt-param-ped-item NO-LOCK,
               FIRST item NO-LOCK WHERE item.it-codigo = tt-param-ped-item.it-codigo:
               
                RUN openQueryStatic in bo-ped-venda (input "Default":U).
            
                RUN goToKey IN bo-ped-venda (INPUT tt-param-ped-venda.nome-abrev,     /*Nome abrev*/
                                             INPUT tt-param-ped-venda.nr-pedcli).   /* Numero pedido */
                IF RETURN-VALUE = "NOK":U THEN
                    NEXT bloco-Heade.
            
                RUN getRowid IN bo-ped-venda (OUTPUT r-row-pedido).
            
                RUN reloadOrder in bo-ped-venda(INPUT  r-row-pedido,
                                                OUTPUT TABLE tt-ped-venda).
                                                
                FIND FIRST tt-ped-venda WHERE tt-ped-venda.nr-pedcli        = tt-param-ped-venda.nome-abrev
                                          AND tt-ped-venda.nome-abrev       = tt-param-ped-venda.nr-pedcli
                                     NO-ERROR.
                IF NOT AVAIL tt-ped-venda THEN NEXT bloco-Heade.

                EMPTY TEMP-TABLE tt-ped-item.
                CREATE tt-ped-item.
                ASSIGN tt-ped-item.nome-abrev       = tt-ped-venda.nome-abrev
                       tt-ped-item.nr-pedcli        = tt-ped-venda.nr-pedcli
                       tt-ped-item.nr-sequencia     = i-sequencia
                       tt-ped-item.it-codigo        = item.it-codigo
                       tt-ped-item.nat-operacao     = tt-ped-venda.nat-operacao
                       tt-ped-item.cod-un           = ""
                       tt-ped-item.qt-pedida        = tt-param-ped-item.qt-pedida           /*Quantidade */
                       tt-ped-item.qt-un-fat        = tt-param-ped-item.qt-un-fat           /*Quantidade Unidade Faturamento*/
                       tt-ped-item.vl-preori        = tt-param-ped-item.vl-preori.          /*Preªo */
                /* coloca valores Default */
                run inputTable in bo-ped-item-sdf (input table tt-ped-item).
                run setDefaultItem in bo-ped-item-sdf.
                run outputTable in bo-ped-item-sdf (output table tt-ped-item).

                FIND FIRST tt-ped-item WHERE tt-ped-item.nome-abrev       = tt-ped-venda.nome-abrev
                                         AND tt-ped-item.nr-pedcli        = tt-ped-venda.nr-pedcli
                                         AND tt-ped-item.nr-sequencia     = i-sequencia
                                         AND tt-ped-item.it-codigo        = item.it-codigo
                                         NO-ERROR.

                run openQueryStatic in bo-ped-item(input "default":U).
                run emptyRowErrors  in bo-ped-item.
                run inputRowParam   in bo-ped-item(input table RowPedParam).
                run setRecord       in bo-ped-item(input table tt-ped-item).
                run createRecord    in bo-ped-item.
                run getRowErrors    in bo-ped-item(output table RowErrors).
                RUN pi-carga-erro.

                if can-find(first RowErrors where
                            rowErrors.errorType <> 'INTERNAL':u and
                            rowErrors.errorSubType begins 'ERRO':U) then do:
                    IF VALID-HANDLE(bo-ped-venda) 
                    THEN RUN deleteRecord    IN bo-ped-venda.
                end.
                ASSIGN 
                   i-sequencia = i-sequencia + 10.
            END.
        end.
        if  i-sequencia = 0
        AND VALID-HANDLE(bo-ped-venda) 
        THEN RUN deleteRecord  IN bo-ped-venda.
        else do:
           run pi-CompletaPedido.
           find first ped-venda 
                where ped-venda.nome-abrev    = tt-ped-venda.nome-abrev 
                  and ped-venda.nr-pedcli     = tt-ped-venda.nr-pedcli 
                no-error.
           IF AVAIL imp_ped_venda
           THEN RUN pi-move.
        end.
    end.
END.

delete procedure bo-ped-venda.
delete procedure bo-ped-venda-sdf.
delete procedure bo-ped-venda-cal.
delete procedure bo-ped-venda-com.
delete procedure bo-ped-item.
delete procedure bo-ped-item-sdf.
delete procedure bo-ped-repre.

OUTPUT CLOSE.

IF VALID-HANDLE(hbodi159) THEN
    DELETE PROCEDURE hbodi159.

RUN pi-finalizar IN h_acomp_rp.


PROCEDURE pi-CompletaPedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR h-bodi159com      AS HANDLE NO-UNDO.
    DEF VAR p-de-volume-total AS DECIMAL NO-UNDO.
    DEF VAR p-i-qt-itens      AS INTEGER NO-UNDO.
    DEF VAR p-de-preco-calc   AS DECIMAL NO-UNDO.

    run pi-acompanhar  in h_acomp_rp(input "Cliente: " + tt-ped-venda.nome-abrev + " Nﬂmero: " + tt-ped-venda.nr-pedcli).

    if not valid-handle(h-bodi159com) or
       h-bodi159com:type <> "PROCEDURE":U or
       h-bodi159com:file-name <> "dibo/bodi159com.p":U then
        run dibo/bodi159com.p persistent set h-bodi159com.

    run setUserLog    in h-bodi159com(input c-seg-usuario).

    for first ped-venda 
        where ped-venda.nome-abrev    = tt-ped-venda.nome-abrev 
          and ped-venda.nr-pedcli     = tt-ped-venda.nr-pedcli:
                                                  
       run completeOrder   in h-bodi159com (input  rowid(ped-venda),
                                            output table Rowerrors).
       RUN pi-carga-erro.
    end.

END PROCEDURE.

PROCEDURE pi-carga-erro:
   FOR EACH RowErrors:
       CREATE tt-param-RowErrors.
       BUFFER-COPY RowErrors TO tt-param-RowErrors.
   END.
END.
