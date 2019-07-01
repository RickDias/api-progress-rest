/*************************************************************************************************************************/
/* Autor....: Victor Alves                                                                                               */
/* Data.....: 30/06/2015                                                                                                 */
/* Descriá∆o: Importar Pedido sf-pedido                                                                                 */
/*                                                                                                                       */
/* 12/08/2016 17:52 - RNK - TKT 28478 Correá∆o regra RS - RS                                                             */
/* 29/08/2016 - SMF - Kraft - No cancelamento verificar se o ped-item.it-codigo est† cadastrado em es-sld-item-alter-4%, */
/*                            e verificar se item original est† cadastrado na tabela de preáo                            */
/* 08/09/2016 - 2.06.00.002 - Victor Alves - Adicionar cancelamento tabela de preco (pi-cancela-tb-preco)                */
/*                                                                                                                       */
/* 14/12/2016 - Ramon Kraft - Correá∆o cod-mensagem do item, que estava ficando s¢ a do ultimo item para todos os itens  */
/*                            Agora esta carregando na tab.tempor†ria tt-item-prod-nat onde foi criado campo cod_mensagem*/
/*                                                                                                                       */
/* 06/01/2017 - 2.06.00.003 - Criar procedure processa-pedido para entrar pedidos de acerto de preco e bonificao junto   */
/*                          - Alteracao natureza de operacao "ES-CFOP" para regra de busca                               */ 
/*************************************************************************************************************************/

MESSAGE '**** esint001birp'.
MESSAGE "****x 0.1".

/* buffer */
{include/i-bfems2.i}                       

/* include de controle de vers∆o */
{include/i-prgvrs.i ESGP0197RP 2.09.00.013 } /*** "019013" ***/

{utp/ut-glob.i} 
{method/dbotterr.i}

/* Temporary Table Definitions ---                                      */
DEF TEMP-TABLE tt-param NO-UNDO 
    FIELD destino           AS INTEGER 
    FIELD arquivo           AS CHAR FORMAT "x(35)"
    FIELD usuario           AS CHAR FORMAT "x(12)"
    FIELD data-exec         AS DATE 
    FIELD tipo-excel        AS INT
    FIELD hora-exec         AS INTEGER                     
    FIELD cod-rep-ini       LIKE repres.cod-rep INITIAL 0
    FIELD cod-rep-fim       LIKE repres.cod-rep INITIAL 9999999
    FIELD dt-dig-ini        AS DATE
    FIELD dt-dig-fim        AS DATE
    FIELD cod-emit-ini      LIKE emitente.cod-emit INITIAL 0
    FIELD cod-emit-fim      LIKE emitente.cod-emit INITIAL 9999999.
                          
define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEF VAR c-user-pedidos AS c INITIAL "INTEGRASFA" NO-UNDO.
ASSIGN c-user-pedidos = "GEOSALES".

DEF TEMP-TABLE tt-sf-pedido              NO-UNDO 
          LIKE geo-pedido
    FIELD nr-pedcli-orig AS CHARACTER.    

DEF TEMP-TABLE tt-sf-item-pedido         NO-UNDO 
    LIKE geo-item_pedido
    FIELD it-codigo AS CHARACTER.

DEF TEMP-TABLE tt-sf-pedido-retorno      NO-UNDO
          LIKE geo-pedido_retorno.
DEF TEMP-TABLE tt-sf-item-pedido-retorno NO-UNDO
          LIKE geo-item_pedido_retorno.

/* Nao usa no-undo para se cancelar pi-acompanhar nao imprimir erro */
DEFINE TEMP-TABLE tt-pedido-erro NO-UNDO
    FIELD cd-pedido-palm LIKE tt-sf-pedido.cd_pedido_palm
    FIELD cd-vendedor    LIKE tt-sf-pedido.cd_vendedor
    FIELD cod-msg        AS INT
    FIELD msg-erro       AS CHAR
    FIELD msg-padrao     AS LOG
    FIELD cd-cliente     AS INT.

DEFINE TEMP-TABLE tt-pedido-erro-dest NO-UNDO LIKE tt-pedido-erro 
    FIELD e-mail   AS CHAR
    FIELD ind-rep  AS LOG.

DEFINE temp-table tt-raw-digita
       field raw-digita as raw.

DEF TEMP-TABLE tt-tt-sf-pedido NO-UNDO 
          LIKE geo-pedido.

DEF VAR c-nr-pedcli LIKE ped-venda.nr-pedcli NO-UNDO.

DEF INPUT  PARAM raw-param as raw no-undo.
DEF INPUT  PARAM table for tt-raw-digita.
DEF INPUT  PARAM table for tt-sf-pedido.
DEF INPUT  PARAM table for tt-sf-item-pedido.
DEF OUTPUT PARAM TABLE FOR tt-pedido-erro.
DEF OUTPUT PARAM c-nr-pedcli-ret LIKE ped-venda.nr-pedcli NO-UNDO.


CREATE tt-param.
RAW-TRANSFER raw-param to tt-param.

/*Temp Table Pedido*/
DEF TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda 
    FIELD r-rowid AS ROWID.

/*Temp Table Itens do Pedido*/
DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-desc-ped-item NO-UNDO LIKE desc-ped-item 
    FIELD r-rowid AS ROWID.
                           
{utp/utapi019.i}
                                             
/*Param Ped-venda*/
DEF TEMP-TABLE tt-ped-param NO-UNDO
    FIELD relacao-item-cli     AS LOG INIT YES     
    FIELD tp-relacao-item-cli  AS INT INIT 1
    FIELD qtde-un-medida-cli   AS LOG INIT yes
    FIELD multiplicar-qtde     AS LOG INIT yes
    FIELD atribuir-preco-comp  AS LOG INIT no
    FIELD tp-exp-nat-oper      AS INT INIT 1
    FIELD tp-exp-dt-entrega    AS INT INIT 1
    FIELD exp-nat-cons-final   AS LOG INIT no
    FIELD exp-nat-cod-mensagem AS LOG INIT no
    FIELD atualizar-entregas   AS LOG INIT yes
    FIELD arredondar-qtde-lote AS LOG INIT no
    FIELD gerar-proc-exp       AS LOG INIT no
    FIELD itinerario           AS INT. 

DEFINE TEMP-TABLE tt-item-prod-nat NO-UNDO
    FIELD cd_pedido_palm LIKE tt-sf-item-pedido.cd_pedido_palm
    FIELD it-codigo      AS CHARACTER 
    FIELD cd_produto     LIKE tt-sf-item-pedido.cd_produto
    FIELD nat_operacao   LIKE natur-oper.nat-operacao
    FIELD tp_codigo		 LIKE es-ped-item.tp-codigo
    FIELD cod_mensagem   LIKE es-ped-item.cod-mensagem.  

/* DEFINICOES DE VARIAVEIS */
DEFINE VARIABLE h-acomp             AS HANDLE                NO-UNDO.
DEFINE VARIABLE bo-ped-venda        AS HANDLE                NO-UNDO.
DEFINE VARIABLE bo-ped-repre        AS HANDLE                NO-UNDO.
DEFINE VARIABLE bo-ped-item         AS HANDLE                NO-UNDO.
DEFINE VARIABLE h-bodi159com        AS HANDLE                NO-UNDO.
DEFINE VARIABLE h-bodi159del        AS HANDLE                NO-UNDO.
DEFINE VARIABLE h-bodi154can        AS HANDLE                NO-UNDO.
DEFINE VARIABLE h-bodi159can        AS HANDLE                NO-UNDO.
DEFINE VARIABLE h-dt-calc           AS HANDLE                NO-UNDO.
DEFINE VARIABLE c-nr-pedido-ems     AS INT                   NO-UNDO.
DEFINE VARIABLE i-sequencia-item    AS INT                   NO-UNDO. 
DEFINE VARIABLE i-qtde-fat          AS DECIMAL               NO-UNDO.
DEFINE VARIABLE d-peso              AS DEC                   NO-UNDO.
DEFINE VARIABLE c-natureza          AS CHAR                  NO-UNDO.
DEFINE VARIABLE c-um-fat            AS CHAR                  NO-UNDO.
DEFINE VARIABLE c-erro              AS CHAR                  NO-UNDO.
DEFINE VARIABLE c-tipo-pedido       AS CHAR                  NO-UNDO.
DEFINE VARIABLE dt-fatura           AS DATE                  NO-UNDO.
DEFINE VARIABLE c-tp-pedido         AS CHAR                  NO-UNDO.
DEFINE VARIABLE i-cod-mensagem      AS INT                   NO-UNDO.
DEFINE VARIABLE i-cont-item         AS INT                   NO-UNDO.
DEFINE VARIABLE i-cont-item-cancela AS INT                   NO-UNDO.
DEFINE VARIABLE c-nr-tabpre         AS CHAR                  NO-UNDO.
DEFINE VARIABLE i-seq-mensagem      AS INT                   NO-UNDO.
DEFINE VARIABLE i-cod-cond-pag      AS INT                   NO-UNDO.
DEFINE VARIABLE l-kg-mt             AS LOG                   NO-UNDO.
                                 
DEFINE BUFFER b-tt-sf-pedido       FOR tt-sf-pedido.
DEFINE BUFFER b2-tt-sf-pedido      FOR tt-sf-pedido.
DEFINE BUFFER b-tt-sf-pedido-bonif FOR tt-sf-pedido.

{include/i-rpvar.i} 

FIND FIRST empresa NO-LOCK NO-ERROR .
IF NOT AVAIL empresa THEN RETURN "ADM-ERROR":U.  

ASSIGN c-programa     = "ESGP0197RP"
       c-versao       = "2.09"
       c-revisao      = "00.003"
       c-empresa      = empresa.nome
       c-sistema      = "SFA"
       c-titulo-relat = "Integraá∆o SFA".

DEF STREAM str-rp.

{include/i-rpout.i &STREAM="stream str-rp" &TOFILE=tt-param.arquivo &pagesize=0}
   
{include/i-rpcab.i}
    
VIEW stream str-rp FRAME f-cabec.
VIEW stream str-rp FRAME f-rodape.

FORM HEADER FILL("-",150) FORMAT "x(150)" SKIP
            "Pedido Palm"  AT 06
            "|"            AT 21
            "Cod Cliente"  AT 23
            "|"            AT 35
            "Nome Abrev"   AT 37
            "|"            AT 53
            "Nr Pedcli"    AT 55
            "|"            AT 71
            "Mensagem"     AT 73
            SKIP
            FILL("-",150) FORMAT "x(150)"
    WITH WIDTH 188 STREAM-IO FRAME ws-cabec.

VIEW STREAM str-rp FRAME ws-cabec.

FUNCTION fncNullEmptyChar RETURNS CHAR ( INPUT pc-string AS CHAR )  FORWARD.

/*Inicializa as Handles*/
RUN dibo/bodi159.p    PERSISTENT SET bo-ped-venda.  
RUN dibo/bodi154.p    PERSISTENT SET bo-ped-item.  
RUN dibo/bodi157.p    PERSISTENT SET bo-ped-repre.  
RUN dibo/bodi159com.p PERSISTENT SET h-bodi159com.   
RUN dibo/bodi159del.p PERSISTENT SET h-bodi159del. 
/*run dibo/bodi154can.p PERSISTENT SET h-bodi154can.*/
RUN dibo/bodi159can.p PERSISTENT SET h-bodi159can.
RUN esp/esgp0188ca.p  PERSISTENT SET h-dt-calc.  
RUN utp/ut-acomp.p    PERSISTENT SET h-acomp.
RUN utp/utapi019.p    PERSISTENT SET h-utapi019.
                        
RUN pi-inicializar IN h-acomp (INPUT "Processando Pedidos...").

MESSAGE "****x 0.2".

/* busca primeiro pedidos PAI */
blk_pedido:
FOR EACH b-tt-sf-pedido NO-LOCK
    WHERE b-tt-sf-pedido.nome-abrev = ""
      AND b-tt-sf-pedido.nr-pedcli  = "" 
    BREAK BY b-tt-sf-pedido.cd_tipo_pedido
          BY b-tt-sf-pedido.cd_pedido_palm_pai 
          BY b-tt-sf-pedido.cd_pedido_palm:
    MESSAGE "****x 0.3".
/*
    IF b-tt-sf-pedido.cd_vendedor < tt-param.cod-rep-ini OR
       b-tt-sf-pedido.cd_vendedor > tt-param.cod-rep-fim THEN NEXT.

    IF b-tt-sf-pedido.cd_cliente < tt-param.cod-emit-ini OR
       b-tt-sf-pedido.cd_cliente > tt-param.cod-emit-fim THEN NEXT.

    IF DATE(b-tt-sf-pedido.dt_emissao) < tt-param.dt-dig-ini OR
       DATE(b-tt-sf-pedido.dt_emissao) > tt-param.dt-dig-fim THEN NEXT.
*/
    RUN pi-acompanhar IN h-acomp (INPUT STRING(b-tt-sf-pedido.cd_pedido_palm)).

    IF CAN-FIND(FIRST ext-ped-venda WHERE ext-ped-venda.nr-ped-sfa = b-tt-sf-pedido.id_pedido_web) THEN DO:
       c-erro = "ERRO: Pedido ja Integrado.".
       RETURN "NOK".
    END.
    
    ASSIGN c-nr-tabpre = ""
           c-nr-pedcli = "".

    /* Analisar se um pedido foi registrado por outro usuario ou se entrou pela BONIFICACAO no pedido PAI */
    /* Quando carrega as tabelas na busca nao esta registrado como vazio ainda                            */
    IF  b-tt-sf-pedido.nome-abrev <> "" AND b-tt-sf-pedido.nr-pedcli  <> "" THEN DO:
        c-erro = "Ocorreram erros durante o processamento: Nome abreviado e n£mero do pedido do cliente n∆o informados".
        RETURN "NOK".
    END.

    /* Buca tabela de preªo */
    /* Salva tabela de preªo na variavel c-nr-tabpre */
    /* Nío pode retirar DO TRANS porque se nío existir a tabela de preªo utilizando int() da erro no resto da transacao do programa */
    MESSAGE "****x 0.4".
    blk_preco:
    DO TRANS ON ERROR  UNDO, LEAVE blk_preco
             ON STOP   UNDO, LEAVE blk_preco
             ON ENDKEY UNDO, LEAVE blk_preco:
      
        FIND FIRST tb-preco
             WHERE INT(tb-preco.nr-tabpre) = b-tt-sf-pedido.cd_tab_preco NO-LOCK NO-ERROR.
         IF AVAIL tb-preco THEN
             ASSIGN c-nr-tabpre = tb-preco.nr-tabpre.
    END.
    MESSAGE "****x 0.5".
    blk_trans:
    DO TRANS ON ERROR  UNDO, LEAVE blk_trans
             ON STOP   UNDO, LEAVE blk_trans
             ON ENDKEY UNDO, LEAVE blk_trans:

        /* Busca nr-pedcli primeiro para acerto de preco e pedido pai sempre ter mesmo numero */
        MESSAGE "****x 0.6".

        FIND FIRST ext-ped-venda WHERE ext-ped-venda.nr-ped-sfa = b-tt-sf-pedido.id_pedido_web NO-ERROR.
        IF NOT AVAIL ext-ped-venda THEN DO:
            CREATE ext-ped-venda.
            ASSIGN ext-ped-venda.nr-ped-sfa = b-tt-sf-pedido.id_pedido_web.
        END.

        MESSAGE 'antes do pi-buscar' .
            
        RUN pi-buscar-nr-pedcli (OUTPUT c-nr-pedcli).

        IF AVAIL ext-ped-venda THEN DO:
            FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = b-tt-sf-pedido.cd_cliente NO-ERROR.
            IF AVAIL emitente THEN
                ASSIGN ext-ped-venda.nome-abrev = emitente.nome-abrev
                       ext-ped-venda.nr-pedcli  = c-nr-pedcli.
            ELSE
                MESSAGE 'nao achou emitente'.
        END.
        ELSE
            MESSAGE 'nao achou ext-ped-venda'.

        MESSAGE "****x 0.6.1 " c-nr-pedcli.
        MESSAGE "****x 0.6.2 " b-tt-sf-pedido.cd_tipo_pedido.

        IF c-nr-pedcli = "" OR c-nr-pedcli = ? THEN DO:
            MESSAGE "**** 0.6.3 next" VIEW-AS ALERT-BOX INFO BUTTONS OK.
            NEXT blk_pedido.        
        END.

        
        
        /* Logica para sempre manter a regra de sempre entrar um pedido acerto de preco junto ao pedido principal */
        /* Se o pedido for normal, primeiro integra os pedidos de acerto de preco                                 */
        IF b-tt-sf-pedido.cd_tipo_pedido = "1" THEN DO:
            MESSAGE "****x 0.7".
            IF b-tt-sf-pedido.cd_pedido_palm > 0
            AND CAN-FIND(FIRST b-tt-sf-pedido-bonif NO-LOCK
                        WHERE b-tt-sf-pedido-bonif.cd_pedido_palm_pai = b-tt-sf-pedido.cd_pedido_palm) THEN DO:
                MESSAGE "****x 0.7.1".
                FOR FIRST b-tt-sf-pedido-bonif NO-LOCK
                    WHERE b-tt-sf-pedido-bonif.cd_pedido_palm_pai = b-tt-sf-pedido.cd_pedido_palm: 
                    MESSAGE "****x 0.7.2".
                    ASSIGN c-nr-pedcli = c-nr-pedcli + "B".

                    /* Se vai criar bonificacao, antes do pedido principal. Adiciona um "B" antes do pedido */
                    /* So existe uma bonificacao por pedido no GEOSALES                                     */
                    RUN pi-processa-pedido (INPUT b-tt-sf-pedido-bonif.cd_pedido_palm).

                    IF RETURN-VALUE = "NEXT":U THEN
                        UNDO blk_trans, NEXT blk_pedido.

                    /* Retira o "B" do pedido para o principal */
                    ASSIGN c-nr-pedcli = SUBSTRING(c-nr-pedcli,1,LENGTH(c-nr-pedcli) - 1).
                END.
            END.
        
            RUN pi-processa-pedido (INPUT b-tt-sf-pedido.cd_pedido_palm).
            
            MESSAGE "**** pi-processa-pedido " RETURN-VALUE " " c-nr-pedcli.


            IF RETURN-VALUE = "NEXT":U THEN
                UNDO blk_trans, NEXT blk_pedido.        
        END.
        ELSE IF b-tt-sf-pedido.cd_tipo_pedido = "2" THEN DO: /* Acerto de preco, se o pedido principal ja foi integrado pela tabela tt-sf-pedido e nao estava o pedido de acerto de preco integrado */
            MESSAGE "****x 0.8".
            FOR FIRST tt-sf-pedido NO-LOCK
                WHERE tt-sf-pedido.cd_pedido_palm = b-tt-sf-pedido.cd_pedido_palm_pai 
                  AND tt-sf-pedido.nome-abrev    <> "" 
                  AND tt-sf-pedido.nr-pedcli     <> "":

                ASSIGN c-nr-pedcli = tt-sf-pedido.nr-pedcli + "B".
                
                RUN pi-processa-pedido (INPUT b-tt-sf-pedido.cd_pedido_palm).

                IF RETURN-VALUE = "NEXT":U THEN
                    UNDO blk_trans, NEXT blk_pedido.

            END. 
            IF NOT AVAIL tt-sf-pedido THEN DO:

				/*analisar andre*/
                RUN pi-create-erro (INPUT 15,
                                    INPUT FALSE,
                                    INPUT SUBSTITUTE("Pedido &1 de acerto de preáo. N∆o integrado o pedido principal &2.",b-tt-sf-pedido.cd_pedido_palm, b-tt-sf-pedido.cd_pedido_palm_pai) + " (3)").
                NEXT blk_pedido.
            END.
        END.
        /* Bonificacao */
        ELSE IF b-tt-sf-pedido.cd_tipo_pedido = "4" THEN DO:
            MESSAGE "****x 0.9".
            RUN pi-processa-pedido (INPUT b-tt-sf-pedido.cd_pedido_palm).

            IF RETURN-VALUE = "NEXT":U THEN
                UNDO blk_trans, NEXT blk_pedido.
        END.
    END.

    RELEASE ped-venda NO-ERROR.
END.         

IF VALID-HANDLE (bo-ped-venda) THEN
    DELETE OBJECT bo-ped-venda NO-ERROR.

IF VALID-HANDLE (bo-ped-item) THEN
    DELETE OBJECT bo-ped-item  NO-ERROR.

IF VALID-HANDLE (bo-ped-repre) THEN
    DELETE OBJECT bo-ped-repre NO-ERROR.

IF VALID-HANDLE (h-bodi159com) THEN
    DELETE OBJECT h-bodi159com NO-ERROR.

IF VALID-HANDLE (h-bodi159del) THEN
    DELETE OBJECT h-bodi159del NO-ERROR.

IF VALID-HANDLE (h-bodi159can) THEN
    DELETE OBJECT h-bodi159can NO-ERROR.

IF VALID-HANDLE (h-bodi154can) THEN
    DELETE OBJECT h-bodi154can NO-ERROR.    

IF VALID-HANDLE (h-dt-calc) THEN
    DELETE OBJECT h-dt-calc NO-ERROR.

RUN pi-inicializar IN h-acomp (INPUT 'Criando lista de destinat†rios...').
                                             
FOR EACH tt-pedido-erro
    BREAK BY tt-pedido-erro.cod-msg:
    
    RUN pi-acompanhar IN h-acomp (INPUT STRING(tt-pedido-erro.cod-msg)).

    IF NOT CAN-FIND(FIRST es-gp-ped-msg-dest NO-LOCK
                    WHERE es-gp-ped-msg-dest.cod-msg = tt-pedido-erro.cod-msg) THEN DO:

        /** ANDRE - N∆o existe indice para esta tabela **/
        FOR EACH es-usr-coml NO-LOCK
            WHERE es-usr-coml.ind-rec-email-integ:

            FOR FIRST usuar_mestre FIELD(cod_e_mail_local)
                WHERE usuar_mestre.cod_usuario = es-usr-coml.cod-usuario NO-LOCK:
            
                CREATE tt-pedido-erro-dest.
                ASSIGN tt-pedido-erro-dest.cd-pedido-palm = tt-pedido-erro.cd-pedido-palm
                       tt-pedido-erro-dest.cd-vendedor    = tt-pedido-erro.cd-vendedor   
                       tt-pedido-erro-dest.cod-msg        = tt-pedido-erro.cod-msg       
                       tt-pedido-erro-dest.msg-erro       = tt-pedido-erro.msg-erro      
                       tt-pedido-erro-dest.msg-padrao     = tt-pedido-erro.msg-padrao 
                       tt-pedido-erro-dest.cd-cliente     = tt-pedido-erro.cd-cliente
                       tt-pedido-erro-dest.e-mail         = usuar_mestre.cod_e_mail_local
                       tt-pedido-erro-dest.ind-rep        = FALSE.
            END.
        END.
    END.
    ELSE DO:
        
        FOR EACH es-gp-ped-msg-dest NO-LOCK
            WHERE es-gp-ped-msg-dest.cod-msg = tt-pedido-erro.cod-msg:
       
            CASE es-gp-ped-msg-dest.tp-envio:
                WHEN 1 THEN DO: /* Representante */
                    CREATE tt-pedido-erro-dest.
                    ASSIGN tt-pedido-erro-dest.cd-pedido-palm = tt-pedido-erro.cd-pedido-palm
                           tt-pedido-erro-dest.cd-vendedor    = tt-pedido-erro.cd-vendedor   
                           tt-pedido-erro-dest.cod-msg        = tt-pedido-erro.cod-msg       
                           tt-pedido-erro-dest.msg-erro       = tt-pedido-erro.msg-erro      
                           tt-pedido-erro-dest.msg-padrao     = tt-pedido-erro.msg-padrao    
                           tt-pedido-erro-dest.cd-cliente     = tt-pedido-erro.cd-cliente
                           tt-pedido-erro-dest.e-mail         = es-gp-ped-msg-dest.e-mail
                           tt-pedido-erro-dest.ind-rep        = FALSE.      
                END.
                WHEN 2 THEN DO: /* Vendedor E-mail */
                    FIND FIRST repres NO-LOCK
                        WHERE repres.cod-rep = tt-pedido-erro.cd-vendedor NO-ERROR.
                    IF NOT AVAIL repres OR repres.e-mail = "" THEN DO:
                        
                        /** ANDRE - N∆o existe indice para esta tabela **/
                        FOR EACH es-usr-coml NO-LOCK
                            WHERE es-usr-coml.ind-rec-email-integ:
                
                            FOR FIRST usuar_mestre FIELD(cod_e_mail_local)
                                WHERE usuar_mestre.cod_usuario = es-usr-coml.cod-usuario NO-LOCK:
                             
                                CREATE tt-pedido-erro-dest.
                                ASSIGN tt-pedido-erro-dest.cd-pedido-palm = tt-pedido-erro.cd-pedido-palm
                                       tt-pedido-erro-dest.cd-vendedor    = tt-pedido-erro.cd-vendedor   
                                       tt-pedido-erro-dest.cod-msg        = tt-pedido-erro.cod-msg       
                                       tt-pedido-erro-dest.msg-erro       = tt-pedido-erro.msg-erro      
                                       tt-pedido-erro-dest.msg-padrao     = tt-pedido-erro.msg-padrao  
                                       tt-pedido-erro-dest.cd-cliente     = tt-pedido-erro.cd-cliente
                                       tt-pedido-erro-dest.e-mail         = usuar_mestre.cod_e_mail_local
                                       tt-pedido-erro-dest.ind-rep        = FALSE.
                            END.
                        END.   
                    END.
                    ELSE DO:                
                        CREATE tt-pedido-erro-dest.
                        ASSIGN tt-pedido-erro-dest.cd-pedido-palm = tt-pedido-erro.cd-pedido-palm
                               tt-pedido-erro-dest.cd-vendedor    = tt-pedido-erro.cd-vendedor   
                               tt-pedido-erro-dest.cod-msg        = tt-pedido-erro.cod-msg       
                               tt-pedido-erro-dest.msg-erro       = tt-pedido-erro.msg-erro      
                               tt-pedido-erro-dest.msg-padrao     = tt-pedido-erro.msg-padrao 
                               tt-pedido-erro-dest.cd-cliente     = tt-pedido-erro.cd-cliente
                               tt-pedido-erro-dest.e-mail         = repres.e-mail
                               tt-pedido-erro-dest.ind-rep        = TRUE.                    
                    END.
                END.    
            END CASE.
        END.    
    END.
END.

FIND FIRST param-global NO-LOCK NO-ERROR.
/* Gera tabela de retorno de erro GEOSALES fora da transacao */

RUN pi-inicializar IN h-acomp (INPUT 'Enviando e-mail...').

DEFINE VARIABLE c-cod-msg AS CHAR NO-UNDO.

FOR EACH tt-pedido-erro-dest
    BREAK BY tt-pedido-erro-dest.e-mail:

    RUN pi-acompanhar IN h-acomp (INPUT tt-pedido-erro-dest.cod-msg).
                          
    IF FIRST-OF(tt-pedido-erro-dest.e-mail) THEN DO:

        IF NOT tt-pedido-erro-dest.ind-rep THEN
            ASSIGN c-cod-msg = "<th> C¢d Msg </th>" +
                               "<th> Tp Msg  </th>".
        ELSE
            ASSIGN c-cod-msg = "".
        
        CREATE tt-mensagem.
        ASSIGN i-seq-mensagem           = i-seq-mensagem + 1
               tt-mensagem.seq-mensagem = i-seq-mensagem
               tt-mensagem.mensagem     = "<h2> Integraá∆o de Pedidos SFA </h2> <br/>"                                                                +         
                                          "<p> Erros encontrado na integraá∆o de pedidos no Sistema Camil, favor verificar e corrigir os erros .</p>" + 
                                          "</br>"                                 + 
                                          "<table border = '1'>"                  +
                                          "<thead>"                               +
                                          "<tr>"                                  +
                                          c-cod-msg                               +                                
                                          "<th> Representante </th>"              +
                                          "<th> Nome Repres   </th>"              +
                                          "<th> Raz∆o Social  </th>"              +
                                          "<th> Nome Abrev    </th>"              +
                                          "<th> C¢d Cliente   </th>"              +
                                          "<th> Pedido Palm   </th>"              +
                                          "<th> Erro          </th>"              +
                                          "</tr>"                                 + 
                                          "</thead>"                              +
                                          "<tbody>".  
    END.

    FIND FIRST repres NO-LOCK
        WHERE repres.cod-rep = tt-pedido-erro-dest.cd-vendedor NO-ERROR.

    FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = tt-pedido-erro-dest.cd-cliente NO-ERROR.

    IF NOT tt-pedido-erro-dest.ind-rep THEN
        ASSIGN c-cod-msg = "<td>" + STRING(tt-pedido-erro-dest.cod-msg) + "</td><td>" + (IF tt-pedido-erro-dest.msg-padrao THEN "Padr∆o" ELSE "Espec°fica") + "</td>".
    ELSE
        ASSIGN c-cod-msg = "".

    CREATE tt-mensagem.
    ASSIGN i-seq-mensagem           = i-seq-mensagem + 1
           tt-mensagem.seq-mensagem = i-seq-mensagem                                                
           tt-mensagem.mensagem     = "<tr>"    +
                                      c-cod-msg +
                                      "<td>"    + (IF AVAIL repres   THEN STRING(repres.cod-rep)        ELSE "") + "</td>" +     
                                      "<td>"    + (IF AVAIL repres   THEN repres.nome-abrev             ELSE "") + "</td>" + 
                                      "<td>"    + (IF AVAIL emitente THEN emitente.nome-emit            ELSE "") + "</td>" +
                                      "<td>"    + (IF AVAIL emitente THEN emitente.nome-abrev           ELSE "") + "</td>" +  
                                      "<td>"    + (IF AVAIL emitente THEN STRING(emitente.cod-emitente) ELSE "") + "</td>" +
                                      "<td>"    + STRING(tt-pedido-erro-dest.cd-pedido-palm)                     + "</td>" +
                                      "<td>"    + tt-pedido-erro-dest.msg-erro                                   + "</td>" +
                                      "</tr>".   

    IF LAST-OF(tt-pedido-erro-dest.e-mail) THEN DO:
               
        CREATE tt-mensagem.
        ASSIGN i-seq-mensagem           = i-seq-mensagem + 1
               tt-mensagem.seq-mensagem = i-seq-mensagem
               tt-mensagem.mensagem     = "</tbody>" +
                                          "</table>" + 
                                          "</br>".
        
        RUN pi-envia-email (INPUT tt-pedido-erro-dest.e-mail).
                               
        EMPTY TEMP-TABLE tt-mensagem.
        ASSIGN i-seq-mensagem = 0.                  

    END.                                                 
END.

RUN pi-inicializar IN h-acomp (INPUT 'Salvando registros enviado...').

FOR EACH tt-pedido-erro 
    BREAK BY tt-pedido-erro.cd-pedido-palm:

    RUN pi-acompanhar IN h-acomp (INPUT STRING(tt-pedido-erro.cd-pedido-palm)).

    FOR FIRST tt-sf-pedido NO-LOCK
        WHERE tt-sf-pedido.cd_pedido_palm = tt-pedido-erro.cd-pedido-palm:
        
        
        /* Copia tt-sf-pedido PARA ENVIO DE EMAIL */                        
        FOR EACH tt-sf-item-pedido OF tt-sf-pedido NO-LOCK:
            
            FIND FIRST tt-sf-item-pedido-retorno EXCLUSIVE-LOCK
                WHERE tt-sf-item-pedido-retorno.cd_pedido_palm = tt-sf-item-pedido.cd_pedido_palm
                  AND tt-sf-item-pedido-retorno.cd_produto     = tt-sf-item-pedido.cd_produto NO-ERROR.
            IF NOT AVAIL tt-sf-item-pedido-retorno THEN DO:                
                CREATE tt-sf-item-pedido-retorno.
                ASSIGN tt-sf-item-pedido-retorno.cd_pedido_palm = tt-sf-item-pedido.cd_pedido_palm  
                       tt-sf-item-pedido-retorno.cd_produto     = tt-sf-item-pedido.cd_produto.        
            END.
        
            BUFFER-COPY tt-sf-item-pedido EXCEPT cd_pedido_palm cd_produto TO tt-sf-item-pedido-retorno NO-ERROR.
        
            RELEASE tt-sf-item-pedido-retorno NO-ERROR.
        END.
        

        FIND FIRST tt-sf-pedido-retorno EXCLUSIVE-LOCK
            WHERE tt-sf-pedido-retorno.cd_pedido_palm = tt-sf-pedido.cd_pedido_palm NO-ERROR.
        IF NOT AVAIL tt-sf-pedido-retorno THEN DO:            
            CREATE tt-sf-pedido-retorno.
            ASSIGN tt-sf-pedido-retorno.cd_pedido_palm = tt-sf-pedido.cd_pedido_palm.        
        END.
        
        BUFFER-COPY tt-sf-pedido EXCEPT cd_st_pedido TO tt-sf-pedido-retorno NO-ERROR.
        
        ASSIGN tt-sf-pedido-retorno.cd_st_pedido = 0 /* Nao integrado Datasul */
               tt-sf-pedido-retorno.msg_erro     = tt-pedido-erro.msg-erro.
                     
        RELEASE tt-sf-pedido-retorno NO-ERROR.
        
    END.
END.

IF VALID-HANDLE (h-utapi019) THEN
    DELETE PROCEDURE h-utapi019 NO-ERROR.

RUN pi-finalizar in h-acomp.
{include/i-rpclo.i "stream str-rp"}

RETURN "OK":U.

/**************************************************************************************************************/
/* Procedure                                                                                                  */
/**************************************************************************************************************/ 

PROCEDURE pi-processa-pedido:
    MESSAGE "****x 0.0.1".
    DEFINE INPUT PARAMETER pi-cd-pedido-palm LIKE tt-sf-pedido.cd_pedido_palm NO-UNDO.
                   
    FOR EACH tt-ped-venda:
        DELETE tt-ped-venda.
    END.

    FOR EACH tt-ped-item:
        DELETE tt-ped-item.
    END.

    FOR EACH tt-desc-ped-item:
        DELETE tt-desc-ped-item.
    END.

    FOR EACH tt-ped-param:
        DELETE tt-ped-param.
    END.
    MESSAGE "****x 0.0.2".
    FOR FIRST tt-sf-pedido EXCLUSIVE-LOCK
        WHERE tt-sf-pedido.cd_pedido_palm = pi-cd-pedido-palm
          AND tt-sf-pedido.nome-abrev     = "" 
          AND tt-sf-pedido.nr-pedcli      = "":
        MESSAGE "****x 0.0.3".
        ASSIGN i-sequencia-item = 0
               i-cod-cond-pag   = 0
               /* Bonificacao */
               c-tipo-pedido    = IF tt-sf-pedido.cd_tipo_pedido = "2" OR tt-sf-pedido.cd_tipo_pedido = "4"  THEN "52" ELSE
                                  IF tt-sf-pedido.cd_tipo_pedido = "3"                                     THEN "49" ELSE
                                  IF tt-sf-pedido.cd_tipo_pedido = "1" AND tt-sf-pedido.cd_org_venda  = "30" THEN "150" ELSE "01".
       
        IF NOT CAN-FIND(FIRST tt-sf-item-pedido NO-LOCK
                        WHERE tt-sf-item-pedido.cd_pedido_palm = tt-sf-pedido.cd_pedido_palm) THEN RETURN "NEXT":U.
    
        ASSIGN c-nr-pedido-ems = NEXT-VALUE(seq-nr-pedido).

        FIND FIRST estabelec NO-LOCK
            WHERE estabelec.cod-estabel = tt-sf-pedido.cd_org_venda NO-ERROR. 
        IF NOT AVAIL estabelec THEN DO:
    
            RUN utp/ut-msgs.p (INPUT "msg",
                               INPUT 13,
                               INPUT STRING(tt-sf-pedido.cd_org_venda)).
							   
			/*analisar andre*/    
            RUN pi-create-erro (INPUT 13,
                                INPUT TRUE,
                                INPUT RETURN-VALUE + " (4)").
            RETURN "NEXT":U.   
        END.
MESSAGE "****x 0.0.4".
        FIND FIRST repres NO-LOCK
            WHERE repres.cod-rep = tt-sf-pedido.cd_vendedor NO-ERROR.
        IF NOT AVAIL repres THEN DO:
    
            RUN utp/ut-msgs.p (INPUT "msg",
                               INPUT 372,
                               INPUT STRING(tt-sf-pedido.cd_vendedor)).
    
			/*analisar andre*/ 
            RUN pi-create-erro (INPUT 372,
                                INPUT TRUE,
                                INPUT RETURN-VALUE + " (5)").
            RETURN "NEXT":U.    
        END.
MESSAGE "****x 0.0.5".
        FIND FIRST emitente NO-LOCK
            WHERE emitente.cod-emitente = tt-sf-pedido.cd_cliente NO-ERROR.
        IF NOT AVAIL emitente THEN DO:   
    
            RUN utp/ut-msgs.p (INPUT "msg",
                               INPUT 785,
                               INPUT STRING(tt-sf-pedido.cd_cliente)).
    
            RUN pi-create-erro (INPUT 785,
                                INPUT TRUE,
                                INPUT RETURN-VALUE + " (6)").
    
            RETURN "NEXT":U.    
        END.
MESSAGE "****x 0.0.6".

        /* Definido que ser† fixo transportadora RETIRA */
        FIND FIRST transporte NO-LOCK
            WHERE transporte.cod-transp = 1 NO-ERROR.   
        IF NOT AVAIL transporte THEN DO:   
    
            RUN pi-create-erro (INPUT 1,
                                INPUT FALSE,
                                INPUT "Transportadora RETIRA eliminada." + " (7)").
            RETURN "NEXT":U.    
        END.
MESSAGE "****x 0.0.7".
        /*Processa Natureza para o Pedido*/
        RUN pi-processa-natureza-item (OUTPUT c-natureza,
                                       OUTPUT d-peso,
                                       OUTPUT i-cod-mensagem,
                                       OUTPUT c-tp-pedido).
    
        IF RETURN-VALUE = "NEXT":U THEN RETURN "NEXT":U.
MESSAGE "****x 0.0.8".                            
        /*Valida se o Pedido ja foi Implantado com o mesmo nrPedCli*/
        FIND FIRST ped-venda NO-LOCK
            WHERE ped-venda.nome-abrev = emitente.nome-abrev
              AND ped-venda.nr-pedcli  = c-nr-pedcli NO-ERROR.
        IF AVAIL ped-venda THEN DO:
            
            RUN pi-create-erro (INPUT 3,
                                INPUT FALSE,
                                INPUT "Chave entrada para o n£mero do pedido cliente j† existe." + " (8)").
            RETURN "NEXT":U.    
        END.
MESSAGE "****x 0.0.9".    
        FIND FIRST natur-oper NO-LOCK
             WHERE natur-oper.nat-operacao = c-natureza NO-ERROR.
        IF NOT AVAIL natur-oper THEN DO:
    
            RUN pi-create-erro (INPUT 6,
                                INPUT FALSE,
                                INPUT SUBSTITUTE("Natureza de operaá∆o &1 n∆o cadastrada.",c-natureza) + " (9)").
    
            RETURN "NEXT":U.
        END.
MESSAGE "****x 0.0.10".        
        /* Carrega cond pagto */
/*
        IF(STRING(c-tipo-pedido,"99")  = "52"  OR                                           
           STRING(c-tipo-pedido,"99")  = "30"  OR                                           
           STRING(c-tipo-pedido,"99")  = "87"  OR                                           
           STRING(c-tipo-pedido,"99")  = "33"  OR                                           
           STRING(c-tipo-pedido,"99")  = "14"  OR                                           
           STRING(c-tipo-pedido,"999") = "453" OR                                                
           STRING(c-tipo-pedido,"99")  = "62") OR                                               
           STRING(c-tipo-pedido,"99")  = "86"  OR                                               
           STRING(c-tipo-pedido,"99")  = "28"  THEN 
            ASSIGN i-cod-cond-pag = 0.
        ELSE DO:
            
            ASSIGN i-cod-cond-pag = tt-sf-pedido.cd_cond_pgto.
    
            IF NOT CAN-FIND(FIRST cond-pagto NO-LOCK
                            WHERE cond-pagto.cod-cond-pag = i-cod-cond-pag) THEN DO:
            
                RUN pi-create-erro (INPUT 8,
                                    INPUT FALSE,
                                    INPUT SUBSTITUTE("Condiá∆o de pagamento &1 n∆o cadastrada.",STRING(i-cod-cond-pag)) + " (10)").
            
                RETURN "NEXT":U.           
            END.
        END.
*/

        IF tt-sf-pedido.cd_tipo_pedido = "2"
        THEN ASSIGN i-cod-cond-pag = 0.
        ELSE DO:
            ASSIGN i-cod-cond-pag = tt-sf-pedido.cd_cond_pgto.
            IF NOT CAN-FIND(FIRST cond-pagto NO-LOCK
                            WHERE cond-pagto.cod-cond-pag = i-cod-cond-pag) 
            THEN DO:
                RUN pi-create-erro (INPUT 8,
                                    INPUT FALSE,
                                    INPUT SUBSTITUTE("Condiá∆o de pagamento &1 n∆o cadastrada.",STRING(i-cod-cond-pag)) + " (10)").
            
                RETURN "NEXT":U.           
            END.
        END.

MESSAGE "****x 0.0.11".            
        RUN setConstraintQuotation  IN bo-ped-venda (INPUT NO).
        RUN setConstraintDefault    IN bo-ped-venda.
        RUN openQueryStatic         IN bo-ped-venda (INPUT "Default":U).
MESSAGE "****x 0.0.12".            
        /*INICIA A CRIAÄ«O DO PEDIDO*/
        CREATE tt-ped-venda.
        ASSIGN tt-ped-venda.cod-estabel      = estabelec.cod-estabel
               tt-ped-venda.nome-abrev       = emitente.nome-abrev 
               tt-ped-venda.nr-pedcli        = c-nr-pedcli
               tt-ped-venda.nr-pedido        = c-nr-pedido-ems          
               tt-ped-venda.cod-cond-pag     = IF(STRING(c-tipo-pedido,"99")  = "52"  OR 
                                                  STRING(c-tipo-pedido,"99")  = "30"  OR 
                                                  STRING(c-tipo-pedido,"99")  = "87"  OR 
                                                  STRING(c-tipo-pedido,"99")  = "33"  OR 
                                                  STRING(c-tipo-pedido,"99")  = "14"  OR
                                                  STRING(c-tipo-pedido,"999") = "453" OR 
                                                  STRING(c-tipo-pedido,"99")  = "62") OR 
                                                  STRING(c-tipo-pedido,"99")  = "86"  OR 
                                                  STRING(c-tipo-pedido,"99")  = "28"  THEN 0 ELSE tt-sf-pedido.cd_cond_pgto
               tt-ped-venda.nat-operacao     = c-natureza
               tt-ped-venda.cod-canal-venda  = emitente.cod-canal-venda
               tt-ped-venda.nr-tab-finan     = 1                                        
               tt-ped-venda.vl-tot-ped       = 0  
               tt-ped-venda.no-ab-reppri     = repres.nome-abrev  
               tt-ped-venda.cod-des-merc     = IF natur-oper.consum-final AND NOT emitente.contrib-icms THEN 2 ELSE 1
               tt-ped-venda.mo-codigo        = 0    
               tt-ped-venda.nr-pedrep        = IF tt-sf-pedido.cd_pedido_cliente = ? THEN "" ELSE tt-sf-pedido.cd_pedido_cliente
               tt-ped-venda.contato          = fncNullEmptyChar(SUBSTRING(tt-ped-venda.nr-pedrep,1,15))
               tt-ped-venda.cod-portador     = emitente.portador                                     
               tt-ped-venda.modalidade       = emitente.modalidade                                   
               tt-ped-venda.observacoes      = fncNullEmptyChar(tt-sf-pedido.ds_observacao)
               tt-ped-venda.cond-espec       = ""
               tt-ped-venda.cond-redespa     = ""
               tt-ped-venda.tp-pedido        = c-tipo-pedido
               tt-ped-venda.ind-tp-frete     = IF tt-sf-pedido.id_tipo_frete = "CIF" THEN 1 ELSE 2
               tt-ped-venda.nome-transp      = transporte.nome-abrev WHEN AVAIL transporte
               tt-ped-venda.dt-implant       = TODAY
               tt-ped-venda.cod-entrega      = ""
               tt-ped-venda.cgc              = fncNullEmptyChar(tt-sf-pedido.nr_cnpj_cpf_entr) 
               tt-ped-venda.ins-estadual     = fncNullEmptyChar(emitente.ins-estadual)       
               tt-ped-venda.cep              = fncNullEmptyChar(tt-sf-pedido.nr_cep_entr)   
               tt-ped-venda.local-entreg     = fncNullEmptyChar(tt-sf-pedido.ds_endereco_entr) 
               tt-ped-venda.bairro           = fncNullEmptyChar(tt-sf-pedido.nm_bairro_entr) 
               tt-ped-venda.cidade           = fncNullEmptyChar(tt-sf-pedido.nm_cidade_entr)
               tt-ped-venda.cidade-cif       = IF tt-ped-venda.ind-tp-frete = 1 THEN fncNullEmptyChar(tt-sf-pedido.nm_cidade_entr) ELSE ""
               tt-ped-venda.estado           = fncNullEmptyChar(tt-sf-pedido.nm_estado_entr)
               tt-ped-venda.endereco         = fncNullEmptyChar(tt-sf-pedido.ds_endereco_entr)
               tt-ped-venda.pais             = fncNullEmptyChar(emitente.pais)
               tt-ped-venda.dt-entorig       = tt-sf-pedido.dt_entrega
               tt-ped-venda.user-impl        = c-user-pedidos
                   
               tt-ped-venda.cod-ped-clien-mp = tt-sf-pedido.id_pedido_web
               .
       
        /******
        FIND FIRST geo-endereco_pedido
             WHERE geo-endereco_pedido.cd_endereco = tt-sf-pedido.cd_endereco_entrega NO-LOCK NO-ERROR.
        IF AVAIL geo-endereco_pedido THEN DO:
    
            FIND loc-entr
           WHERE loc-entr.nome-abrev  = geo-endereco_pedido.nome_abrev  
             AND loc-entr.cod-entrega = geo-endereco_pedido.cod_entrega NO-LOCK NO-ERROR.
            IF AVAIL loc-entr THEN
                ASSIGN tt-ped-venda.cod-entrega = loc-entr.cod-entrega
                       tt-ped-venda.bairro      = loc-entr.bairro   
                       tt-ped-venda.cidade      = loc-entr.cidade   
                       tt-ped-venda.estado      = loc-entr.estado   
                       tt-ped-venda.endereco    = loc-entr.endereco
                       tt-ped-venda.cep         = loc-entr.cep.
                        
    
        END.
    
        /* Se nao existir nem local de entrega nem endereco pedido busca o padrao ou o primeiro */
        IF NOT AVAIL loc-entr THEN DO:
                                                                                        
            FIND loc-entr
           WHERE loc-entr.nome-abrev  = emitente.nome-abrev
             AND loc-entr.cod-entrega = "Padrao" NO-LOCK NO-ERROR.
            IF AVAIL loc-entr THEN
                ASSIGN tt-ped-venda.cod-entrega = loc-entr.cod-entrega.
            ELSE 
                FIND FIRST loc-entr
                     WHERE loc-entr.nome-abrev = emitente.nome-abrev NO-LOCK NO-ERROR.
                IF AVAIL loc-entr THEN
                    ASSIGN tt-ped-venda.cod-entrega = loc-entr.cod-entrega.
        END.
        *****/
MESSAGE "****x 0.0.13".        
        FIND FIRST loc-entr NO-LOCK 
             WHERE loc-entr.nome-abrev  = geo-endereco_pedido.nome_abrev  
               AND loc-entr.cod-entrega = geo-endereco_pedido.cod_entrega 
             NO-ERROR.
        IF NOT AVAIL loc-entr 
        THEN DO:
            FIND FIRST loc-entr NO-LOCK 
                 WHERE loc-entr.nome-abrev  = emitente.nome-abrev
                   AND loc-entr.cod-entrega = "Padrao" 
                 NO-ERROR.
             IF AVAIL loc-entr 
             THEN FIND FIRST loc-entr NO-LOCK 
                 WHERE loc-entr.nome-abrev = emitente.nome-abrev 
                 NO-ERROR.
             ASSIGN 
                tt-ped-venda.cod-entrega = loc-entr.cod-entrega.
        END.
MESSAGE "****x 0.0.14".        
        IF AVAIL loc-entr 
        THEN ASSIGN 
           tt-ped-venda.cod-entrega = loc-entr.cod-entrega
           tt-ped-venda.bairro      = loc-entr.bairro   
           tt-ped-venda.cidade      = loc-entr.cidade   
           tt-ped-venda.estado      = loc-entr.estado   
           tt-ped-venda.endereco    = loc-entr.endereco
           tt-ped-venda.cep         = loc-entr.cep.

        
        /* Ticket 33207 - Victor */
        IF  tt-sf-pedido.id_tipo_frete = "F" 
        AND tt-sf-pedido.dt_entrega    = ? THEN
            ASSIGN tt-ped-venda.dt-entrega = TODAY + 1
                   tt-ped-venda.dt-entorig = TODAY + 1.
        ELSE DO:
            
            /* Pedido acerto de preáo */
            IF tt-sf-pedido.cd_tipo_pedido      = "2" AND 
               tt-sf-pedido.cd_pedido_palm_pai <> ? THEN DO:
            
                /* Busca pedido pai */
                FOR FIRST b2-tt-sf-pedido FIELDS(nome-abrev nr-pedcli)
                    WHERE b2-tt-sf-pedido.cd_pedido_palm = tt-sf-pedido.cd_pedido_palm_pai NO-LOCK:
            
                    /* efetiva data de entrega do pedido pai */
                    FOR FIRST ped-venda FIELDS(nome-abrev nr-pedcli dt-entrega)
                        WHERE ped-venda.nome-abrev = b2-tt-sf-pedido.nome-abrev
                          AND ped-venda.nr-pedcli  = b2-tt-sf-pedido.nr-pedcli NO-LOCK:
            
                        /* Efetiva do pedido de acerto de preáo e n∆o envia e-mail */
                        ASSIGN tt-ped-venda.dt-entrega = ped-venda.dt-entrega.
            
                    END.
                END.
            END.
            ELSE DO:            
                /* Calcula data de entrega simulada, n∆o efetiva data de faturamento porque n∆o foi aprovado ainda */
                /* Data de faturamento s¢ Ç efetivada na liberaá∆o do pedido                                       */
                RUN piDtEntrega IN h-dt-calc (INPUT  tt-ped-venda.cod-estabel,
                                              INPUT  tt-ped-venda.estado,
                                              INPUT  tt-ped-venda.cidade,
                                              INPUT  tt-ped-venda.cep,
                                              INPUT  d-peso,
                                              INPUT  tt-ped-venda.nome-abrev,
                                              INPUT  TODAY,
                                              OUTPUT tt-ped-venda.dt-entrega,
                                              OUTPUT dt-fatura,
                                              OUTPUT c-erro).

                MESSAGE "**** Local entrega " c-erro.

                MESSAGE "**** tt-ped-venda.cod-estabel" tt-ped-venda.cod-estabel.
                MESSAGE "**** tt-ped-venda.cod-entrega" tt-ped-venda.cod-entrega.
                MESSAGE "**** tt-ped-venda.estado     " tt-ped-venda.estado     .
                MESSAGE "**** tt-ped-venda.cidade     " tt-ped-venda.cidade     .
                MESSAGE "**** tt-ped-venda.cep        " tt-ped-venda.cep        .
                MESSAGE "**** d-peso                  " d-peso                  .
                MESSAGE "**** tt-ped-venda.nome-abrev " tt-ped-venda.nome-abrev .
                MESSAGE "**** tt-ped-venda.dt-entrega " tt-ped-venda.dt-entrega .
                MESSAGE "**** dt-fatura               " dt-fatura               .

                /* Se o representante informar a data */
                IF tt-sf-pedido.dt_entrega <> tt-sf-pedido.dt_entrega_calc THEN DO:
                                        
                    /* Apenas dia util - Ticket 29976 */
                    IF date(tt-sf-pedido.dt_entrega) > tt-ped-venda.dt-entrega THEN
                        ASSIGN tt-ped-venda.dt-entrega = DYNAMIC-FUNCTION("fncDiasUteis" IN h-dt-calc,tt-ped-venda.cod-estabel, tt-sf-pedido.dt_entrega).                                                                                    
                END.
            END.
        
            /* Se a data de entrega original tiver como vazia, assume a data de entrega calculada */
            IF tt-ped-venda.dt-entorig = ? THEN
                ASSIGN tt-ped-venda.dt-entorig = tt-ped-venda.dt-entrega.

        END.
MESSAGE "****x 0.0.15".        
        IF c-erro <> "" THEN DO:    
            RUN pi-create-erro (INPUT 4,
                                INPUT FALSE,
                                INPUT c-erro + SUBSTITUTE(" / Est: &1 / UF: &2 / Cid: &3, Peso: &4", tt-ped-venda.cod-estabel, tt-ped-venda.estado, tt-ped-venda.cidade, d-peso) + " (11)").
            RETURN "NEXT":U.
        END.  
MESSAGE "****x 0.0.16".        
        FIND FIRST geo-endereco_pedido
             WHERE geo-endereco_pedido.cd_endereco = tt-sf-pedido.cd_endereco_entrega NO-LOCK NO-ERROR.
        IF AVAIL geo-endereco_pedido THEN DO:
            
            FIND loc-entr
           WHERE loc-entr.nome-abrev  = geo-endereco_pedido.nome_abrev  
             AND loc-entr.cod-entrega = geo-endereco_pedido.cod_entrega NO-LOCK NO-ERROR.
            IF AVAIL loc-entr THEN
                ASSIGN tt-ped-venda.cod-entrega = loc-entr.cod-entrega.
    
        END.  
MESSAGE "****x 0.0.17".        
        /* Se nao existir nem local de entrega nem endereco pedido busca o padrao ou o primeiro */
        IF NOT AVAIL loc-entr THEN DO:
    
            FIND loc-entr
           WHERE loc-entr.nome-abrev  = emitente.nome-abrev
             AND loc-entr.cod-entrega = "Padrao" NO-LOCK NO-ERROR.
            IF AVAIL loc-entr THEN
                ASSIGN tt-ped-venda.cod-entrega = loc-entr.cod-entrega.
            ELSE 
                FIND FIRST loc-entr
                     WHERE loc-entr.nome-abrev = emitente.nome-abrev NO-LOCK NO-ERROR.
                IF AVAIL loc-entr THEN
                    ASSIGN tt-ped-venda.cod-entrega = loc-entr.cod-entrega.
    
        END.
MESSAGE "****x 0.0.18" SEARCH("tges/twp/twdi159.p") SEARCH("tges/twp/twdi159.r").            
        /* Altera tt-sf-pedido para chegar na Trigger de write com os valores j† atualizados                */
        /* Se houver eliminacao na tabela do pedido a TRIGGER tges/tdp/tdp159-01.p zera os valores abaixo */
        ASSIGN tt-sf-pedido.nome-abrev         = tt-ped-venda.nome-abrev 
               tt-sf-pedido.nr-pedcli          = tt-ped-venda.nr-pedcli  
               tt-sf-pedido.cd_pedido          = tt-ped-venda.nr-pedcli  
               tt-sf-pedido.dt_importacao_erp  = NOW.  
                        
        /* Limpa buffer loc-entr para proxima busca */
        RELEASE loc-entr NO-ERROR.
    
        RUN emptyRowErrors IN bo-ped-venda.
        RUN setUserLog     IN bo-ped-venda (INPUT c-user-pedidos).
        RUN setRecord      IN bo-ped-venda (INPUT TABLE tt-ped-venda).
        RUN inputRowParam  IN bo-ped-venda (INPUT TABLE tt-ped-param).
        RUN createRecord   IN bo-ped-venda.
        RUN getRowErrors   IN bo-ped-venda (OUTPUT TABLE RowErrors).
MESSAGE "****x 0.0.19".        
        FIND FIRST RowErrors
             WHERE RowErrors.ErrorSubType  <> "WARNING" NO-LOCK NO-ERROR.
        
        IF AVAIL RowErrors THEN DO:
    
            RUN pi-create-erro (INPUT RowErrors.ErrorNumber,
                                INPUT TRUE,
                                INPUT RowErrors.ErrorDescription + " (12)").
    
            RUN pi-delete-pedido-erro(INPUT tt-ped-venda.nr-pedcli,
                                      INPUT tt-ped-venda.nome-abrev).
                
            RETURN "NEXT":U.    
        END.
MESSAGE "****x 0.0.20".            
        FIND FIRST tt-ped-venda NO-ERROR.
        
        RUN getRowid                    IN bo-ped-venda (OUTPUT tt-ped-venda.r-rowid).
        RUN createOrdersRepresentatives IN bo-ped-repre (INPUT tt-ped-venda.r-rowid).    
        RUN setConstraintSituacao       IN bo-ped-venda (INPUT 1).
    
        FIND FIRST tt-ped-venda NO-ERROR.
    
        /*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/
MESSAGE "****x 0.0.21".        
        FOR EACH tt-sf-item-pedido NO-LOCK
            WHERE tt-sf-item-pedido.cd_pedido_palm = tt-sf-pedido.cd_pedido_palm:

            FOR EACH tt-ped-item:
                DELETE tt-ped-item.
            END.

            FOR EACH tt-desc-ped-item:
                DELETE tt-desc-ped-item.
            END.
                                                                                                     
            ASSIGN c-um-fat         = ""
                   i-qtde-fat       = 0
                   i-sequencia-item = i-sequencia-item + 10.
                        
            /* Segundo Murilo n∆o dever† validar ----
            FIND FIRST es-gp-item NO-LOCK
                WHERE es-gp-item.cd-produto = tt-sf-item-pedido.cd_produto NO-ERROR.
            IF NOT AVAIL es-gp-item THEN DO:
                
                RUN pi-create-erro (INPUT 5,
                                    INPUT FALSE,
                                    INPUT "Item " + STRING(tt-sf-item-pedido.cd_produto)  + " N∆o relacionado SFA X Datasul." + " (13)").
    
                RUN pi-delete-pedido-erro(INPUT tt-ped-venda.nr-pedcli,
                                          INPUT tt-ped-venda.nome-abrev).
                
                RETURN "NEXT":U.
            END.
            */

            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.it-codigo = tt-sf-item-pedido.it-codigo NO-ERROR.
            IF NOT AVAIL ITEM THEN DO:              
                
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 90,
                                   INPUT STRING(tt-sf-item-pedido.it-codigo)).
    
                RUN pi-create-erro (INPUT 90,
                                    INPUT TRUE,
                                    INPUT RETURN-VALUE + " (14)").
    
                RUN pi-delete-pedido-erro(INPUT tt-ped-venda.nr-pedcli,
                                          INPUT tt-ped-venda.nome-abrev).
                
                RETURN "NEXT":U.
            END.
    
            /*Item Unidade Estabelecimento*/
            FIND FIRST item-uni-estab NO-LOCK
                WHERE item-uni-estab.cod-estabel = estabelec.cod-estabel 
                  AND item-uni-estab.it-codigo   = ITEM.it-codigo NO-ERROR.
            
            FIND FIRST tt-item-prod-nat NO-LOCK
                WHERE tt-item-prod-nat.cd_pedido_palm = tt-sf-item-pedido.cd_pedido_palm
                  AND tt-item-prod-nat.it-codigo     = tt-sf-item-pedido.it-codigo NO-ERROR.
            IF AVAIL tt-item-prod-nat THEN DO:

                FIND FIRST natur-oper NO-LOCK
                    WHERE natur-oper.nat-oper = tt-item-prod-nat.nat_operacao NO-ERROR. 
                IF NOT AVAIL natur-oper THEN DO:
                    
                    RUN pi-create-erro (INPUT 6,
                                        INPUT FALSE,
                                        INPUT SUBSTITUTE("Natureza de operaá∆o &1 n∆o cadastrada.",tt-item-prod-nat.nat_operacao) + " (15)").
                                                    
                    RUN pi-delete-pedido-erro(INPUT tt-ped-venda.nr-pedcli,
                                              INPUT tt-ped-venda.nome-abrev).
                
                    RETURN "NEXT":U.
                END.
            END.
            
            ASSIGN l-kg-mt = NO.

            FIND FIRST es-classif-fis NO-LOCK
                 WHERE es-classif-fis.class-fiscal = ITEM.class-fiscal NO-ERROR.
            IF AVAIL es-classif-fis THEN
                ASSIGN l-kg-mt = es-classif-fis.l-kg-mt.
            
            /*  Unidade Medida Para acucar    */
            IF ITEM.ge-codigo = 77 OR (estabelec.estado = "MT" AND l-kg-mt) THEN DO:

                RUN pi-quantidade-kg-fd(INPUT  ITEM.it-codigo,
                                        INPUT  tt-sf-item-pedido.qt_item,
                                        OUTPUT c-um-fat,
                                        OUTPUT i-qtde-fat).
    
            END.
    
            CREATE tt-ped-item.
            ASSIGN tt-ped-item.it-codigo           = STRING(tt-sf-item-pedido.it-codigo)
                   tt-ped-item.nome-abrev          = tt-ped-venda.nome-abrev
                   tt-ped-item.nr-pedcli           = tt-ped-venda.nr-pedcli
                   tt-ped-item.cod-entrega         = tt-ped-venda.cod-entrega
                   tt-ped-item.dt-entorig          = tt-ped-venda.dt-entorig  
                   tt-ped-item.dt-entrega          = tt-ped-venda.dt-entrega  
                   tt-ped-item.nr-sequencia        = i-sequencia-item
                   tt-ped-item.nat-operacao        = natur-oper.nat-operacao 
                   tt-ped-item.ind-icm-ret         = natur-oper.subs-trib
                   tt-ped-item.observacao          = ""
                   tt-ped-item.tipo-atend          = 2
                   tt-ped-item.dec-2               = 0
                   tt-ped-item.aliquota-ipi        = ITEM.aliquota-ipi
                   tt-ped-item.cod-unid-negoc      = if avail item-uni-estab then item-uni-estab.cod-unid-negoc else ""
                   SUBSTR(tt-ped-item.char-2,1,8)  = ITEM.class-fiscal
                   SUBSTR(tt-ped-item.char-2,9,2)  = ITEM.un
                   tt-ped-item.qt-pedida           = tt-sf-item-pedido.qt_item
                   /*Customizacao Acucar*/
                   tt-ped-item.des-un-medida       = IF c-um-fat <> "" THEN c-um-fat ELSE ITEM.un
                   tt-ped-item.qt-un-fat           = IF i-qtde-fat > 0 THEN i-qtde-fat ELSE tt-sf-item-pedido.qt_item 
                   tt-ped-item.ind-fat-qtfam       = IF i-qtde-fat > 0 THEN ? ELSE FALSE
                   tt-ped-item.vl-preori           = tt-sf-item-pedido.vr_item 
                   tt-ped-item.vl-pretab           = tt-sf-item-pedido.vr_item 
                   tt-ped-item.vl-preuni           = tt-sf-item-pedido.vr_item 
                   tt-ped-item.vl-tot-it           = tt-sf-item-pedido.qt_item * tt-sf-item-pedido.vr_item
                   tt-ped-item.nr-tabpre           = tt-ped-venda.nr-tabpre
                   tt-ped-item.user-impl           = c-user-pedidos.
    
            EMPTY TEMP-TABLE RowErrors.
            
            RUN openQueryStatic     IN bo-ped-item(INPUT "Default":U).
            RUN inputRowParam       IN bo-ped-item(INPUT TABLE tt-ped-param).
            RUN setUserLog          IN bo-ped-item(INPUT c-user-pedidos).
            RUN inputRowDescPedItem IN bo-ped-item(INPUT TABLE tt-desc-ped-item).
            RUN setRecord           IN bo-ped-item(INPUT TABLE tt-ped-item).
            RUN createRecord        IN bo-ped-item.
            RUN getRowErrors        IN bo-ped-item(OUTPUT TABLE RowErrors).
            
            FIND FIRST RowErrors 
                WHERE RowErrors.ErrorSubType  <> "WARNING" NO-ERROR.
            IF AVAIL RowErrors THEN DO:
    
                RUN pi-create-erro (INPUT RowErrors.ErrorNumber,
                                    INPUT TRUE,
                                    INPUT "Erro ao criar Item do pedido: " +  tt-sf-item-pedido.it-codigo + " Erro: " + RowErrors.ErrorDescription + " (16)").
                  
                RUN pi-delete-pedido-erro(INPUT tt-ped-venda.nr-pedcli,
                                          INPUT tt-ped-venda.nome-abrev).
                
                RETURN "NEXT":U.
            END.
            /*GRAVA A EXTENS«O DO PEDIDO */
            FIND FIRST es-ped-item NO-LOCK
                WHERE es-ped-item.nome-abrev    = emitente.nome-abrev 
                  AND es-ped-item.nr-ped-cli    = tt-ped-venda.nr-pedcli 
                  AND es-ped-item.nr-sequencia  = i-sequencia-item 
                  AND es-ped-item.it-codigo     =  tt-sf-item-pedido.it-codigo NO-ERROR.
            IF NOT AVAIL es-ped-item THEN DO:               
                CREATE es-ped-item.
                ASSIGN es-ped-item.nome-abrev   = emitente.nome-abrev 
                       es-ped-item.nr-ped-cli   = tt-ped-venda.nr-pedcli   
                       es-ped-item.nr-sequencia = i-sequencia-item 
                       es-ped-item.it-codigo    = tt-sf-item-pedido.it-codigo 
                       es-ped-item.tp-codigo    = tt-item-prod-nat.tp_codigo
                       es-ped-item.cod-mensagem = tt-item-prod-nat.cod_mensagem.              
            END.
        END.
MESSAGE "****x 0.0.22".                
        RUN pi-cancela-tb-preco.
        
        IF ped-venda.cod-sit-ped <> 6 THEN DO:
            
            /*Libera Logistica*/
            IF NOT CAN-FIND (FIRST es-lib-cml NO-LOCK
                             WHERE es-lib-cml.nome-abrev   = ped-venda.nome-abrev
                               AND es-lib-cml.nr-pedcli    = ped-venda.nr-pedcli) THEN DO:
                CREATE es-lib-cml.
                ASSIGN es-lib-cml.ocor-ult-lib = "LIB-" + string(TODAY) + c-user-pedidos
                       es-lib-cml.dt-lib-cml   = TODAY 
                       es-lib-cml.nome-abrev   = ped-venda.nome-abrev
                       es-lib-cml.nr-pedcli    = ped-venda.nr-pedcli
                       es-lib-cml.sit-log      = YES
                       es-lib-cml.usr-lib-cml  = c-user-pedidos
                       es-lib-cml.u-char-1     = STRING(TIME,"HH:MM:SS"). /* hora liberaá∆o */
            END.
        END.
MESSAGE "****x 0.0.23".                
        MESSAGE "**** c-nr-pedcli " c-nr-pedcli.
        
        /*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/
        /*Completa Pedido*/
        RUN pi-completa-pedido(INPUT ROWID(ped-venda)).        
MESSAGE "****x 0.0.24".        
    END.

END PROCEDURE.
PROCEDURE pi-cancela-tb-preco: 

    DEFINE VARIABLE l-cancela AS LOG NO-UNDO.
    
    ASSIGN i-cont-item         = 0
           i-cont-item-cancela = 0.
    
    FIND FIRST ped-venda NO-LOCK
        WHERE ped-venda.nome-abrev = tt-ped-venda.nome-abrev
          AND ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli NO-ERROR.
    IF AVAIL ped-venda THEN DO:

        FIND FIRST es-gp-ped-venda EXCLUSIVE-LOCK
            WHERE es-gp-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
        IF AVAIL es-gp-ped-venda THEN DO:
            ASSIGN es-gp-ped-venda.nr-tabpre = c-nr-tabpre.

            RELEASE es-gp-ped-venda NO-ERROR.   
        END.

        /* Se nao existir tabela de preco cancela item */
        FOR EACH ped-item OF ped-venda NO-LOCK:

            RUN pi-cancela-preco-item (OUTPUT l-cancela).

            MESSAGE "**** l-cancela " l-cancela.

            IF l-cancela THEN
                ASSIGN i-cont-item-cancela = i-cont-item-cancela + 1.

            ASSIGN i-cont-item = i-cont-item + 1.    
        END.

        /* Se todos itens for cancelado, cancela direto no pedido */
        IF  i-cont-item         > 0
        AND i-cont-item-cancela = i-cont-item THEN DO:
            
            RUN pi-cancela-pedido (INPUT ROWID(ped-venda),
                                   INPUT SUBSTITUTE("Cancelamento autom†tico via ESGP0197. Falta dos itens do pedido na tabela de preáo &1.&2Data &3 &2Hora: &4",c-nr-tabpre,
                                                                                                                                                                 CHR(13),
                                                                                                                                                                 STRING(TODAY,"99/99/9999"),
                                                                                                                                                                 STRING(TIME,"HH:MM:SS"))).

        /* Se nao cancela apenas itens sem tabela de preáo */ 
        END.
        ELSE IF i-cont-item-cancela > 0 THEN DO:
            
            FOR EACH ped-item OF ped-venda NO-LOCK:

                RUN pi-cancela-preco-item (OUTPUT l-cancela).
                
                IF l-cancela THEN
                    RUN pi-cancela-item (INPUT ROWID(ped-item),
                                         INPUT SUBSTITUTE("Cancelamento autom†tico via ESGP0197. Falta do item &1 na tabela de preáo &2.&3Data: &4 &3Hora: &5",ped-item.it-codigo,
                                                                                                                                                               c-nr-tabpre,
                                                                                                                                                               CHR(13),
                                                                                                                                                               STRING(TODAY,"99/99/9999"),
                                                                                                                                                               STRING(TIME,"HH:MM:SS"))).
                
            END.
        END.    
    END.

END PROCEDURE.

PROCEDURE pi-cancela-preco-item: 

    DEFINE OUTPUT PARAMETER pl-cancela AS LOG NO-UNDO.
                 
    /* Se existir preco item E estiver como ativo, nao cancela ITEM                    */
    /* Deve ser verificado primeiro ITEM principal pois tem casos que pode nao alterar */

    MESSAGE "**** c-nr-tabpre " c-nr-tabpre.

    IF NOT CAN-FIND(FIRST es-gp-preco-item NO-LOCK
                    WHERE es-gp-preco-item.nr-tabpre    = c-nr-tabpre
                      AND es-gp-preco-item.it-codigo    = ped-item.it-codigo) 
    OR NOT CAN-FIND(FIRST es-gp-item NO-LOCK
                    WHERE es-gp-item.it-codigo = ped-item.it-codigo 
                      AND es-gp-item.ativo-venda) THEN DO:
        /*
        MESSAGE "**** cancela item " 
            CAN-FIND(FIRST es-gp-preco-item NO-LOCK
                    WHERE es-gp-preco-item.nr-tabpre    = c-nr-tabpre
                      AND es-gp-preco-item.it-codigo    = ped-item.it-codigo) 
            CAN-FIND(FIRST es-gp-item NO-LOCK
                    WHERE es-gp-item.it-codigo = ped-item.it-codigo 
                      AND es-gp-item.ativo-venda).
                      */

        /* Verifica se existe este item como De x Para para itens alternativos */
        FIND FIRST es-sld-item-alter-4% USE-INDEX alter-um
             WHERE es-sld-item-alter-4%.it-alter-um = ped-item.it-codigo
               AND es-sld-item-alter-4%.cod-estabel = ped-venda.cod-estabel NO-LOCK NO-ERROR.
        IF NOT AVAIL es-sld-item-alter-4% THEN 
            FIND FIRST es-sld-item-alter-4% USE-INDEX alter-dois
                 WHERE es-sld-item-alter-4%.it-alter-dois = ped-item.it-codigo
                   AND es-sld-item-alter-4%.cod-estabel   = ped-venda.cod-estabel NO-LOCK NO-ERROR.
        IF NOT AVAIL es-sld-item-alter-4% THEN 
            FIND FIRST es-sld-item-alter-4% USE-INDEX alter-tres
                 WHERE es-sld-item-alter-4%.it-alter-tres = ped-item.it-codigo
                   AND es-sld-item-alter-4%.cod-estabel   = ped-venda.cod-estabel NO-LOCK NO-ERROR.
        IF AVAIL es-sld-item-alter-4% THEN DO:

            /* Verifica se o item pricinpal nao foi eliminado da tabela */
            IF CAN-FIND(FIRST es-gp-preco-item              
                        WHERE es-gp-preco-item.nr-tabpre    = c-nr-tabpre
                          AND es-gp-preco-item.it-codigo    = es-sld-item-alter-4%.it-codigo  NO-LOCK) 
            OR CAN-FIND(FIRST es-gp-item
                        WHERE es-gp-item.it-codigo = es-sld-item-alter-4%.it-codigo
                          AND es-gp-item.ativo-venda NO-LOCK) THEN DO:

                ASSIGN pl-cancela = FALSE.

                RETURN.
            END.
            ELSE DO:                
                /* Cancela */
                ASSIGN pl-cancela = TRUE.

                RETURN.
            END.
        END.
        ELSE DO:
            MESSAGE "**** cancela 1".

            /* Busca tabela de preco historica para clientes suspenso (Se ainda existir na rota entao nao cancela) */
            FOR LAST es-gp-preco-hist NO-LOCK
               WHERE es-gp-preco-hist.nr-tabpre    = c-nr-tabpre
                 AND es-gp-preco-hist.it-codigo    = ped-item.it-codigo
                 AND es-gp-preco-hist.cod-refer    = ped-item.cod-refer
                 AND es-gp-preco-hist.cod-unid-med = ped-item.cod-un:
                MESSAGE "**** cancela 2".                
                /* Busca por rota */
                FIND FIRST es-gp-tb-preco
                     WHERE es-gp-tb-preco.nr-rota = es-gp-preco-hist.nr-rota
                       AND es-gp-tb-preco.tipo    = "Rota" NO-LOCK NO-ERROR.
                IF AVAIL es-gp-tb-preco THEN DO:
                    MESSAGE "**** cancela 3".                    
                    /* Se eixstir o item de tabela de rota nao cancela */
                    IF CAN-FIND(FIRST es-gp-preco-item NO-LOCK
                                WHERE es-gp-preco-item.nr-tabpre    = es-gp-tb-preco.nr-tabpre
                                  AND es-gp-preco-item.it-codigo    = ped-item.it-codigo
                                  AND es-gp-preco-item.cod-refer    = ped-item.cod-refer
                                  AND es-gp-preco-item.cod-unid-med = ped-item.cod-un) THEN DO:
                        MESSAGE "**** cancela 4".
                        ASSIGN pl-cancela = FALSE.

                        RETURN.
                    END.
                    ELSE DO:
                        /* Cancela */
                        ASSIGN pl-cancela = TRUE.
                        MESSAGE "**** cancela 5".

                        RETURN.
                    END.
                END.
                ELSE DO:
                    MESSAGE "**** cancela 6".
                    /* Cancela */
                    ASSIGN pl-cancela = TRUE.

                    RETURN.
                END.
            END.
            IF NOT AVAIL es-gp-preco-hist THEN DO:                
                MESSAGE "**** cancela 7".
                /****************
                /* Cancela */
                ASSIGN pl-cancela = TRUE.
                ****************/
                RETURN.                
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-envia-email:

    DEFINE INPUT PARAMETER pc-destino AS CHAR NO-UNDO.
    
    FOR EACH tt-envio2:
        DELETE tt-envio2.
    END.
    
    IF pc-destino = '' THEN
        RETURN.

    IF NOT AVAIL param-global THEN 
        FIND FIRST param-global NO-LOCK NO-ERROR.

    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = IF AVAIL param-global THEN param-global.log-1            ELSE FALSE
           tt-envio2.servidor          = IF AVAIL param-global THEN param-global.serv-mail        ELSE ""
           tt-envio2.porta             = IF AVAIL param-global THEN param-global.porta-mail       ELSE 25
           tt-envio2.remetente         = "gp@camil.com.br"
           tt-envio2.destino           = pc-destino
           tt-envio2.copia             = ""
           tt-envio2.assunto           = "Pedidos SFA - Camil" 
           tt-envio2.arq-anexo         = ""
           tt-envio2.formato           = "HTML".
    
    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,
                                   OUTPUT TABLE tt-erros).
    
    FOR EACH tt-erros NO-LOCK:
            
        PUT STREAM str-rp "" FORMAT "x(19)" " | "
                          "" FORMAT "x(12)" " | "
                          "" FORMAT "x(12)" " | "
                          "Problema no envio de e-mail. " + tt-erros.desc-erro FORMAT "x(255)" SKIP.     

        MESSAGE "**** Problema no envio de e-mail. " tt-erros.desc-erro.
       
    END.
    
END PROCEDURE.

PROCEDURE pi-processa-natureza-item:
        
    DEFINE OUTPUT PARAMETER pc-natureza-item AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER pd-peso          AS DEC  NO-UNDO.
    DEFINE OUTPUT PARAMETER pi-cod-mensagem  AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER pc-tp-pedido     AS CHAR NO-UNDO.

    DEFINE VARIABLE i-tipo-oper     AS INT  NO-UNDO.
    DEFINE VARIABLE c-natureza-item AS CHAR NO-UNDO.
    DEFINE VARIABLE i-cod-mensagem  AS INT  NO-UNDO.
    DEFINE VARIABLE de-vl-tot-item  AS DEC  NO-UNDO.

    FOR EACH tt-sf-item-pedido NO-LOCK
        WHERE tt-sf-item-pedido.cd_pedido_palm = tt-sf-pedido.cd_pedido_palm:

        ASSIGN de-vl-tot-item = de-vl-tot-item + (tt-sf-item-pedido.qt_item * tt-sf-item-pedido.vr_item).
        
        /*
        FIND FIRST es-gp-item NO-LOCK
             WHERE es-gp-item.cd-produto = tt-sf-item-pedido.it-codigo NO-ERROR.
        IF NOT AVAIL es-gp-item THEN DO:

            RUN pi-create-erro (INPUT 5,
                                INPUT FALSE,
                                INPUT "Item " + STRING(tt-sf-item-pedido.it-codigo)  + " N∆o relacionado SFA X Datasul." + " (17)").

            RETURN "NEXT":U.
        END.   
        */
              
        ASSIGN i-tipo-oper = 0.

        FIND FIRST ITEM NO-LOCK
            WHERE ITEM.it-codigo = tt-sf-item-pedido.it-codigo NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:   

            RUN utp/ut-msgs.p (INPUT "msg",
                               INPUT 90,
                               INPUT STRING(tt-sf-item-pedido.it-codigo)).
            
            RUN pi-create-erro (INPUT 90,
                                INPUT TRUE,
                                INPUT RETURN-VALUE + " (18)").
            
            RETURN "NEXT":U.
        END.

        ASSIGN pd-peso = pd-peso + ((ITEM.peso-bruto * tt-sf-item-pedido.qt_item)).
        
        RUN pi-busca-natureza (INPUT tt-sf-item-pedido.it-codigo,
                               INPUT tt-sf-pedido.cd_org_venda,
                               INPUT emitente.cod-emitente,
                               INPUT STRING(c-tipo-pedido, "999"),
                               OUTPUT c-natureza-item,   
                               OUTPUT i-cod-mensagem).

        MESSAGE "**** Busca de natureza de operacao".
        MESSAGE "**** tt-sf-item-pedido.it-codigo " tt-sf-item-pedido.it-codigo.
        MESSAGE "**** tt-sf-pedido.cd_org_venda " tt-sf-pedido.cd_org_venda.
        MESSAGE "**** emitente.cod-emitente " emitente.cod-emitente.
        MESSAGE "**** c-natureza-item " c-natureza-item.
        MESSAGE "**** i-cod-mensagem " i-cod-mensagem.

        FIND FIRST tt-item-prod-nat 
            WHERE tt-item-prod-nat.cd_pedido_palm = tt-sf-item-pedido.cd_pedido_palm
              AND tt-item-prod-nat.it-codigo     = tt-sf-item-pedido.it-codigo
              AND tt-item-prod-nat.nat_operacao   = c-natureza-item NO-ERROR.
        IF NOT AVAIL tt-item-prod-nat THEN DO:

            CREATE tt-item-prod-nat.
            ASSIGN tt-item-prod-nat.cd_pedido_palm = tt-sf-item-pedido.cd_pedido_palm
                   tt-item-prod-nat.it-codigo     = tt-sf-item-pedido.it-codigo
                   tt-item-prod-nat.nat_operacao   = c-natureza-item 
                   tt-item-prod-nat.tp_codigo	   = STRING(c-tipo-pedido, "999") 
                   tt-item-prod-nat.cod_mensagem   = i-cod-mensagem.
        END.   

        ASSIGN pc-natureza-item = c-natureza-item
               pi-cod-mensagem  = i-cod-mensagem
               pc-tp-pedido     = STRING(c-tipo-pedido, "999").
        
    END.

    IF de-vl-tot-item <> tt-sf-pedido.vr_pedido THEN DO:
        RUN pi-create-erro (INPUT 14,
                            INPUT FALSE,
                            INPUT "Valor da somat¢ria dos itens divergente do valor total do pedido." + " (19)").

        RETURN "NEXT":U.        
    END.

    RETURN "OK":U.

END PROCEDURE.


/*********Efetua a busca de naturezas ***********/
PROCEDURE pi-busca-natureza:
    
    DEFINE INPUT  PARAMETER pc-it-codigo       LIKE ITEM.it-codigo.
    DEFINE INPUT  PARAMETER pc-cod-estabel     LIKE estabelec.cod-estabel.
    DEFINE INPUT  PARAMETER pi-cod-emit        LIKE emitente.cod-emit.
    DEFINE INPUT  PARAMETER pi-tipoPedido      AS INT.
    DEFINE OUTPUT PARAMETER pc-nat-operacao    LIKE natur-oper.nat-operacao.
    DEFINE OUTPUT PARAMETER pc-cod-mensagem    LIKE kpvPedidoItem.tabelaCodigoMensagem.
    
    DEFINE VARIABLE r-cfop                 AS ROWID   NO-UNDO.
    DEFINE VARIABLE l-substitui-tributaria AS LOGICAL NO-UNDO.
          
    /******Substituicao*****/
    FIND FIRST item-uf NO-LOCK
         WHERE item-uf.it-codigo       = ITEM.it-codigo
           AND item-uf.cod-estado-orig = estabelec.estado
           AND item-uf.estado          = emitente.estado NO-ERROR.
     
    ASSIGN l-substitui-tributaria = IF AVAIL item-uf THEN YES ELSE NO.    
    
    RUN tges/twp/twdi154a.p (INPUT estabelec.cod-estabel,
                             INPUT ITEM.ge-codigo,
                             INPUT ITEM.fm-cod-com,
                             INPUT ITEM.fm-codigo,
                             INPUT ITEM.it-codigo,
                             INPUT STRING(pi-tipoPedido,"999"),
                             INPUT l-substitui-tributaria,
                             INPUT emitente.contrib-icms, 
                             INPUT emitente.cod-emitente,
                             INPUT emitente.cidade,
                             INPUT emitente.estado,
                             OUTPUT r-cfop).
    /*CFOP*/
    FIND FIRST es-cfop NO-LOCK
        WHERE ROWID(es-cfop) = r-cfop NO-ERROR.
    IF AVAIL es-cfop THEN DO:

        IF estabelec.estado = emitente.estado AND 
           estabelec.pais   = emitente.pais THEN DO:
            ASSIGN pc-nat-operacao = es-cfop.nat-operacao-interna 
                   pc-cod-mensagem = es-cfop.cod-mensagem-interna.
        END.
        ELSE IF estabelec.estado <> emitente.estado AND 
                estabelec.pais = emitente.pais  THEN DO:
            ASSIGN pc-nat-operacao = es-cfop.nat-operacao-interestadual
                   pc-cod-mensagem = es-cfop.cod-mensagem-interestadual.
        END.
        ELSE IF estabelec.pais <> emitente.pais THEN DO:
            ASSIGN pc-nat-operacao = es-cfop.nat-operacao-exportacao
                   pc-cod-mensagem = es-cfop.cod-mensagem-exportacao.
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-buscar-nr-pedcli:                    
    
    DEFINE OUTPUT  PARAMETER pc-proximoCodigo  AS CHAR NO-UNDO.

    DEFINE BUFFER bf-ext-ped-venda FOR ext-ped-venda.

    MESSAGE "b-tt-sf-pedido.cd_tipo_pedido: " b-tt-sf-pedido.cd_tipo_pedido.
                                       
    IF b-tt-sf-pedido.cd_tipo_pedido <> "2" THEN DO:

        FIND FIRST kpvNrPedRep EXCLUSIVE-LOCK WHERE kpvNrPedRep.cod-repres = b-tt-sf-pedido.cd_vendedor NO-ERROR.
        IF NOT AVAIL kpvNrPedRep THEN DO:
      
            CREATE kpvNrPedRep.
            ASSIGN kpvNrPedRep.cod-repres = b-tt-sf-pedido.cd_vendedor
                   kpvNrPedRep.nr-pedido  = 1.            
        END. 
        ELSE ASSIGN kpvNrPedRep.nr-pedido = kpvNrPedRep.nr-pedido + 1.            
        
        ASSIGN pc-proximoCodigo = STRING(b-tt-sf-pedido.cd_vendedor, "9999") + "6" + STRING(kpvNrPedRep.nr-pedido, "999999").

        MESSAGE "**** diferente".
    
        /* Bonificacao */
        IF b-tt-sf-pedido.cd_tipo_pedido = "4" THEN
            ASSIGN pc-proximoCodigo = pc-proximoCodigo + "B".

        MESSAGE "pc-proximoCodigo: " pc-proximoCodigo.
    END.
    ELSE DO:

        FIND FIRST bf-ext-ped-venda WHERE bf-ext-ped-venda.nr-pedcl = b-tt-sf-pedido.nr-pedcli-orig NO-LOCK NO-ERROR.
        IF AVAIL bf-ext-ped-venda THEN DO:

            IF CAN-FIND(FIRST ped-venda WHERE ped-venda.nome-abrev = bf-ext-ped-venda.nome-abrev
                                          AND ped-venda.nr-pedcli  = bf-ext-ped-venda.nr-pedcli) THEN DO:
                
                ASSIGN pc-proximoCodigo = ext-ped-venda.nr-pedcli + "B".

                IF pc-proximoCodigo = "" OR pc-proximoCodigo = ? THEN DO:
                    RUN pi-create-erro (INPUT 15,
                                        INPUT FALSE,
                                        INPUT SUBSTITUTE("Pedido &1 de acerto de preáo n∆o integrado devido o pedido principal &2 n∆o ter integrado" ,b-tt-sf-pedido.cd_pedido_palm, b-tt-sf-pedido.nr-pedcli-orig)).
                END.
            END.
            ELSE DO:
                RUN pi-create-erro (INPUT 15,
                                        INPUT FALSE,
                                        INPUT SUBSTITUTE("Pedido &1 de acerto de preáo n∆o integrado devido o pedido principal &2 n∆o ter integrado" ,b-tt-sf-pedido.cd_pedido_palm, b-tt-sf-pedido.nr-pedcli-orig)).
            END.
        END.
        ELSE DO:
            RUN pi-create-erro (INPUT 15,
                                        INPUT FALSE,
                                        INPUT SUBSTITUTE("Pedido &1 de acerto de preáo n∆o integrado devido o pedido principal &2 n∆o ter integrado" ,b-tt-sf-pedido.cd_pedido_palm, b-tt-sf-pedido.nr-pedcli-orig)).
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.
                     
PROCEDURE pi-quantidade-kg-fd:

    DEFINE INPUT  PARAMETER pc-item         AS CHAR    NO-UNDO.
    DEFINE INPUT  PARAMETER pi-qtdeKg       AS INT     NO-UNDO.
    DEFINE OUTPUT PARAMETER pc-unid-fat     AS CHAR    NO-UNDO.
    DEFINE OUTPUT PARAMETER pd-qtdeFd       AS DECIMAL NO-UNDO.

    DEFINE VARIABLE d-exp AS INT FORMAT 99999 NO-UNDO.
    
    FIND FIRST ITEM NO-LOCK
         WHERE ITEM.it-codigo = pc-item NO-ERROR. 

    IF ITEM.ge-codigo = 77 THEN
        FIND FIRST item-unid-venda NO-LOCK
             WHERE item-unid-venda.it-codigo = pc-item  NO-ERROR.
    ELSE
        FIND FIRST item-unid-venda NO-LOCK
             WHERE item-unid-venda.it-codigo = pc-item
               AND item-unid-venda.un        = "KG" NO-ERROR.

    IF NOT AVAIL item-unid-venda THEN RETURN. 
    
    ASSIGN d-exp       = EXP(10, item-unid-venda.num-casa-dec)
           pd-qtdeFd   = pi-qtdeKg * (item-unid-venda.fator-conversao / d-exp)
           pc-unid-fat = item-unid-venda.un.  

    RETURN "OK":U.

END PROCEDURE.       

/*****************************************************************/
/*************Delete os Pedido se caso encontrar erro durante a implantaáao*************/
/*****************************************************************/
PROCEDURE pi-delete-pedido-erro:
        
    DEF INPUT PARAMETER p-nr-pedcli     AS CHAR NO-UNDO.
    DEF INPUT PARAMETER p-nome-abrev    AS CHAR NO-UNDO.

    DEF VAR r-ped-venda AS ROWID NO-UNDO.

    FIND FIRST ped-venda NO-LOCK
        WHERE ped-venda.nr-pedcli  = p-nr-pedcli
          AND ped-venda.nome-abrev = p-nome-abrev NO-ERROR.
    IF NOT AVAIL ped-venda THEN RETURN.
   
    IF VALID-HANDLE(h-bodi159del) THEN DO:
      
        RUN emptyRowErrors IN h-bodi159del.
        RUN validateDelete IN h-bodi159del (INPUT ROWID(ped-venda), OUTPUT TABLE RowErrors).

        IF RETURN-VALUE = "NOK" THEN PUT "*******************ERRO DELETE 1" SKIP.  
        ELSE DO:          
            RUN emptyRowErrors IN h-bodi159del.
            RUN UpdateDelete   IN h-bodi159del (INPUT ROWID(ped-venda)).
          
            IF RETURN-VALUE = "NOK" THEN DO:
                PUT UNFORMATTED "***************ERRO DELETE2" SKIP.
                RUN GETRowErrors in h-bodi159del (OUTPUT TABLE RowErrors).
            END.
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-completa-pedido:
    
    DEFINE INPUT PARAMETER rw-ped-venda AS ROWID   NO-UNDO.
    

    MESSAGE "**** pi-completa-pedido 1 ".

    FIND FIRST ped-venda NO-LOCK
        WHERE ROWID(ped-venda) = rw-ped-venda NO-ERROR . /*Busca o registro do Pedido*/
    MESSAGE "**** pi-completa-pedido 2 " AVAIL ped-venda.
    IF NOT AVAIL ped-venda THEN RETURN. /*Se n∆o encontrou o registro retorna*/
    MESSAGE "**** pi-completa-pedido 3 ".    
    IF ped-venda.cod-sit-ped <> 1 THEN RETURN. /*Caso a situaá∆o do pedido seja diferente de aberto retorna o aviso*/
    MESSAGE "**** pi-completa-pedido 4 ".   
    RUN completeOrder IN h-bodi159com (INPUT rw-ped-venda,
                                       OUTPUT TABLE RowErrors).
    MESSAGE "**** pi-completa-pedido 5 ".
    FIND FIRST RowErrors NO-LOCK NO-ERROR.    
    IF AVAIL RowErrors THEN DO:
       /**************************************************************
       * N£mero do Erro quando o pedido excedeu o limite de crÇdito, * 
       * como o crÇdito foi aprovado pelo portal n∆o Ç necess†rio um *
       * retorno desse tipo                                          *
       **************************************************************/
       MESSAGE "**** pi-completa-pedido 6 " RowErrors.ErrorNumber
           " - " RowErrors.ErrorDescription.


       MESSAGE "**** nr-pedcli " ped-venda.nr-pedcli.
       MESSAGE "**** nr-pedido " ped-venda.nr-pedido.
       MESSAGE "**** nr-pedrep " ped-venda.nr-pedrep.
       
       IF RowErrors.ErrorNumber = 8259 THEN DO:
           /**ASSIGN ttKpvProcesso.com_cod_mensagem = "message.0059".*/
           ASSIGN
              c-nr-pedcli-ret = ped-venda.nr-pedcli.
           RETURN.
       END.

       RUN pi-create-erro ( INPUT RowErrors.ErrorNumber,
                            INPUT TRUE,
                            INPUT RowErrors.ErrorDescription + " (20)").

       RETURN "NOK".
    END.

    ASSIGN
       c-nr-pedcli-ret = ped-venda.nr-pedcli.

    MESSAGE "**** pi-completa-pedido 7 " c-nr-pedcli-ret.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-create-erro:

    DEFINE INPUT PARAMETER pi-cod-mensagem AS INT                  NO-UNDO.
    DEFINE INPUT PARAMETER pl-msg-padrao   AS LOG                  NO-UNDO.
    DEFINE INPUT PARAMETER pc-mensagem     AS CHAR FORMAT 'x(255)' NO-UNDO.

    PUT STREAM str-rp b-tt-sf-pedido.cd_pedido_palm                                         " | "
                      STRING(b-tt-sf-pedido.cd_cliente)                     FORMAT "x(11)"  " | "
                      IF AVAIL emitente THEN emitente.nome-abrev ELSE ""    FORMAT "x(15)"  " | "
                      c-nr-pedcli                                           FORMAT "x(15)"  " | "
                      pc-mensagem SKIP.  
    /*MESSAGE "**** Erro "
            b-tt-sf-pedido.cd_pedido_palm                          " | "
            STRING(b-tt-sf-pedido.cd_cliente)                      " | "
            IF AVAIL emitente THEN emitente.nome-abrev ELSE ""     " | "
            c-nr-pedcli                                            " | "
            pc-mensagem . */


    /* Verifica se j† foi encaminhado e-mail do erro */
    IF NOT CAN-FIND(FIRST tt-sf-pedido-retorno NO-LOCK
                    WHERE tt-sf-pedido-retorno.cd_pedido_palm = b-tt-sf-pedido.cd_pedido_palm
                      AND tt-sf-pedido-retorno.cd_st_pedido   = 0
                      AND tt-sf-pedido-retorno.msg_erro       = pc-mensagem) THEN DO:
                                 
        FIND FIRST tt-pedido-erro
             WHERE tt-pedido-erro.cd-pedido-palm = b-tt-sf-pedido.cd_pedido_palm NO-ERROR.
        IF NOT AVAIL tt-pedido-erro THEN DO:
        
            CREATE tt-pedido-erro.
            ASSIGN tt-pedido-erro.cd-pedido-palm = b-tt-sf-pedido.cd_pedido_palm
                   tt-pedido-erro.cd-vendedor    = b-tt-sf-pedido.cd_vendedor
                   tt-pedido-erro.cd-cliente     = b-tt-sf-pedido.cd_cliente
                   tt-pedido-erro.msg-erro       = pc-mensagem
                   tt-pedido-erro.cod-msg        = pi-cod-mensagem
                   tt-pedido-erro.msg-padrao     = pl-msg-padrao.        
        END.
    END.
                   
    RETURN "OK":U.

END PROCEDURE.
                
PROCEDURE pi-cancela-pedido:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pr-ped-venda AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER pc-motivo    AS CHAR  NO-UNDO.
    
    EMPTY TEMP-TABLE RowErrors.

    /* Define usuario GEOSALES para cancelamento */
    RUN setUserLog IN h-bodi159can (INPUT c-user-pedidos).

    RUN ValidateCancelation IN h-bodi159can(INPUT pr-ped-venda,
                                            OUTPUT TABLE RowErrors).

    FIND FIRST RowErrors NO-LOCK NO-ERROR.

    FIND FIRST RowErrors NO-LOCK NO-ERROR.
    IF AVAIL RowErrors THEN DO:

        RUN pi-create-erro (INPUT RowErrors.ErrorNumber,
                            INPUT TRUE,
                            INPUT RowErrors.ErrorDescription + " (1)").

        RETURN "ADM-ERROR":U.
    
    END.
     
    RUN inputReOpenQuotation IN h-bodi159can(NO). 
    
    RUN UpdateCancelation IN h-bodi159can(INPUT pr-ped-venda,
                                          INPUT pc-motivo,
                                          INPUT TODAY , 
                                          INPUT 7).

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-cancela-item:
    
    DEFINE INPUT PARAMETER pr-ped-item AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER pc-motivo   AS CHAR  NO-UNDO.

    IF NOT VALID-HANDLE(h-bodi154can) THEN
        run dibo/bodi154can.p PERSISTENT SET h-bodi154can.

    EMPTY TEMP-TABLE RowErrors.  /*Limpa a tabela de erros*/
                                                    
    /* Define usuario GEOSALES para cancelamento */
    RUN setUserLog IN h-bodi154can (INPUT c-user-pedidos).
    

    RUN ValidateCancelation in h-bodi154can(INPUT pr-ped-item,
                                            INPUT TODAY,
                                            INPUT-OUTPUT TABLE RowErrors).
    
    run UpdateCancelation in h-bodi154can(INPUT pr-ped-item,
                                          INPUT pc-motivo,
                                          INPUT TODAY, 
                                          INPUT 99).

    FIND FIRST RowErrors NO-ERROR.
    IF AVAIL RowErrors THEN DO:
        
        RUN pi-create-erro (INPUT RowErrors.ErrorNumber, 
                            INPUT TRUE,
                            INPUT RowErrors.ErrorDescription + " (2)").

        RETURN "ADM-ERROR":U.
        
    END.

    DELETE OBJECT h-bodi154can NO-ERROR.
     
    RETURN "OK":U.

END PROCEDURE.

FUNCTION fncNullEmptyChar RETURNS CHAR (INPUT pc-string AS CHAR):
    
    /* Funcao utilizada para converter valores nulo para vazio. */
    /* O Geosales envia os campos vazios como nulo(?) e quando  */
    /* chega no Progress da problema nas atribuicoes de valores */
    
    IF pc-string = ? THEN
        RETURN "".
    ELSE
        RETURN TRIM(pc-string).

END FUNCTION.
