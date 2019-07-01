&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
        
ECCB - novo imposto senar 16/06/2010 
ECCB - Joca/Waldemar tratamento EFD - PIS/COFINS- 28032011
ECCB - ems206
eccb - senar-funrural nova lei do Sefaz para tratar casas decimais 
       arredondadas para passar no ¢rg∆o 06/11/2012
       retirando FR e Senar porque somente aplicaremos no es4027
*/
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
** mai/2017 - SMF - Kraft - convers∆o campos totvs 12
*******************************************************************************/
{include/i-prgvrs.i ES4019B 2.09.00.001}
{include/i-bfems2cad.i}
CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ES4019B
&GLOBAL-DEFINE Version        2.09.00.000

&GLOBAL-DEFINE WindowType     Master

&GLOBAL-DEFINE Folder         NO
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   

&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2
&GLOBAL-DEFINE page1Widgets   bt-atualiza bt-preco nr-nfd serie fl-condicao br_it-nota-fisc de-preco fl-estabel to-rendimento
&GLOBAL-DEFINE page2Widgets   

DEF VAR c-estabel LIKE estabelec.cod-estabel.
DEF VAR c-motivo-compl AS CHAR.
DEF VAR l-motivo-compl AS LOG INITIAL NO.
DEF VAR c-nome-emit-compl LIKE emitente.nome-emit.
DEF VAR c-preco AS CHAR. /* 06/11/12 */

DEF VAR de-perc-sec AS DEC INITIAL 0. /* 170807 */

DEF VAR l-botao-preco     AS LOGICAL INIT NO                 NO-UNDO. 
DEF VAR l-botao-atualiza  AS LOGICAL INIT NO                 NO-UNDO. 
DEF VAR l-digitou-rend    AS LOGICAL INIT NO                 NO-UNDO. 


DEFINE VARIABLE num-nota AS CHARACTER  NO-UNDO FORMAT "x(10)".
DEFINE VARIABLE num-serie AS CHARACTER  NO-UNDO FORMAT "x(10)".
DEF NEW GLOBAL SHARED VAR gr-pedido-compr AS ROWID NO-UNDO.   

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pRowSaldo        AS ROWID        NO-UNDO.
DEFINE INPUT PARAMETER pRowContrato     AS ROWID        NO-UNDO.
DEFINE INPUT PARAMETER pSaldoDisp       AS DECIMAL      NO-UNDO.

DEF VAR de-soma-m AS dec.
DEF VAR de-soma-ii AS dec.
DEF VAR de-soma-qq AS dec.
DEF VAR l-matriz AS LOG INITIAL NO. /* 10/03/04 */
DEF VAR de-vltotal-3 AS DEC.
DEF VAR de-preco-3  AS DEC.
DEF VAR de-val-desc-s AS DEC.
DEF VAR de-val-bon-s AS DEC.
DEF VAR de-val-semdb-s AS DEC.
DEF VAR de-val-liq-s AS DEC.
DEF VAR de-val-unit-s AS DEC.

/* 190306 variaveis para guardar valor original (o primeiro) de descontos e bonificaá‰es sem retirar o j† liquidado
para calcular o valor inicial de secagem*/
DEF VAR de-val-ori-desc AS DEC DECIMALS 4.
DEF VAR de-val-ori-bon  AS DEC DECIMALS 4.

DEF VAR de-preco-salva AS DEC.

DEF VAR c-msg-tipo-rend AS CHAR INITIAL "". 
DEF VAR l-conf-rend     AS LOG  INITIAL NO. 
DEF VAR i-rend-hist     AS INT INITIAL 0. 
DEF VAR c-usr-hist      AS CHAR INITIAL "".
DEF VAR c-rend-hist     AS CHAR INITIAL "".

DEF TEMP-TABLE tt-aplic-desc-bon     LIKE es-aplic-desc-bon.
DEF TEMP-TABLE tt-aplic-sec-desc-bon LIKE es-aplic-sec-desc-bon.

DEFINE TEMP-TABLE tt-it-nota 
    FIELD qt-faturada    LIKE it-nota-fisc.qt-faturada
    FIELD it-codigo      LIKE it-nota-fisc.it-codigo
    FIELD cod-estabel    LIKE it-nota-fisc.cod-estabel
    FIELD nr-docum       LIKE it-nota-fisc.nr-docum
    FIELD serie          LIKE it-nota-fisc.serie.

/* erika 23/07 */
DEF VAR de-perc        AS DEC DECIMALS 4.
DEF VAR de-desc        AS DEC DECIMALS 4.
DEF VAR de-bon         AS DEC DECIMALS 4.

DEF VAR di-quantidade AS DEC NO-UNDO. /* 111104 */
/* 111104 BONIFICACAO */
DEF VAR de-val-bon-limpo AS DEC DECIMALS 4.
DEF VAR val-bon-limpo AS DEC DECIMALS 4.

DEF TEMP-TABLE tt-bonifica
    FIELD nr-ticket     LIKE es-ticket.nr-ticket
    FIELD c-tp-desconto LIKE es-movto-arroz.tp-desconto
    FIELD quantidade    AS DEC.


DEF VAR de-val-bon     AS DEC DECIMALS 4.
DEF VAR de-val-desc    AS DEC DECIMALS 4.
DEF VAR de-val-semdb   AS DEC DECIMALS 4.
DEF VAR de-val-liq     AS DEC DECIMALS 4.
DEF VAR de-val-unit    AS DEC DECIMALS 4.
DEF VAR de-quantidade2 AS DEC DECIMALS 4.
DEF VAR de-tot-a-secar AS DEC DECIMALS 4.
DEF VAR de-entra       AS DEC DECIMALS 4.
DEF VAR de-saida       AS DEC DECIMALS 4.
DEF VAR de-tot-saldo   AS DEC DECIMALS 4.
    
DEF VAR de-tot-secag   AS DEC DECIMALS 4.
DEF VAR de-tot-seco    AS DEC DECIMALS 4.
DEF VAR de-a-secar     AS DEC DECIMALS 4.

DEF VAR vl-tot-secag   AS DEC DECIMALS 4.
DEF VAR vl-tot-seco    AS DEC DECIMALS 4.

DEF VAR vl-a-secar     AS DEC DECIMALS 4.

DEF VAR de-sec-aplic-desc AS DEC DECIMALS 4.
DEF VAR de-sec-aplic-bon  AS DEC DECIMALS 4.

DEF VAR i-nr-contrato    LIKE es-contrato-arroz.nr-contrato.
DEF VAR i-tick-ini       LIKE es-ticket.nr-ticket.
DEF VAR i-tick-fim       LIKE es-ticket.nr-ticket.
DEF VAR de-valor-formula AS DEC.

DEF TEMP-TABLE tt-extrato NO-UNDO
    FIELD nome-matriz       LIKE es-movto-arroz.nome-matriz
    FIELD cod-emitente      LIKE emitente.cod-emitente
    FIELD nr-contrato       LIKE es-contrato-arroz.nr-contrato
    FIELD dt-trans          LIKE es-movto-arroz.dt-trans   
    FIELD nr-ticket         LIKE es-ticket.nr-ticket       
    FIELD nr-placa          LIKE es-ticket.nr-placa-cam    
    FIELD nr-nota-fornec    LIKE es-ticket.nr-nota-fornec  
    FIELD nro-docto         LIKE es-movto-arroz.nro-docto  
    FIELD serie             LIKE es-movto-arroz.serie
    FIELD esp-docto         LIKE es-movto-arroz.esp-docto  
    FIELD tipo-trans        LIKE es-movto-arroz.tipo-trans
    FIELD peso-fornec       LIKE es-ticket.peso-liq
    FIELD peso-liq          AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD gr-umidade        LIKE es-ticket.gr-umidade
    FIELD desc-umidade      LIKE es-movto-arroz.quantidade
    FIELD gr-impureza       LIKE es-ticket.gr-impureza
    FIELD desc-impureza     LIKE es-movto-arroz.quantidade
    FIELD gr-secagem        LIKE es-ticket.gr-secagem
    FIELD desc-secagem      LIKE es-movto-arroz.quantidade
    FIELD rend-inteiro      LIKE es-ticket.rend-inteiro
    FIELD rend-quebrado     LIKE es-ticket.rend-quebr
    FIELD desc-rendimento   LIKE es-movto-arroz.quantidade
    FIELD rend-desc-inteiro LIKE es-ticket.rend-inteiro
    FIELD desc-inteiro      LIKE es-movto-arroz.quantidade
    FIELD gr-amarelo        LIKE es-ticket.gr-amarelo
    FIELD desc-amarelo      LIKE es-movto-arroz.quantidade
    FIELD gr-vermelho       LIKE es-ticket.gr-vermelho
    FIELD desc-vermelho     LIKE es-movto-arroz.quantidade
    FIELD gr-preto-verm     LIKE es-ticket.gr-preto-verm  
    FIELD desc-preto-verm   LIKE es-movto-arroz.quantidade
    FIELD gr-verde          LIKE es-ticket.gr-verde
    FIELD desc-verde        LIKE es-movto-arroz.quantidade
    FIELD gr-barriga        LIKE es-ticket.gr-barriga
    FIELD desc-barriga      LIKE es-movto-arroz.quantidade
    FIELD gr-gesso          LIKE es-ticket.gr-gesso
    FIELD desc-gesso        LIKE es-movto-arroz.quantidade
    FIELD gr-manch-pic      LIKE es-ticket.gr-manch-pic    
    FIELD desc-manch-pic    LIKE es-movto-arroz.quantidade
    FIELD qtd-bonific       LIKE es-movto-arroz.quantidade
    FIELD val-bonific       LIKE es-movto-arroz.valor
    FIELD valor-unit        LIKE es-movto-arroz.valor
    FIELD peso-total          AS DECIMAL  FORMAT "->>,>>>,>>9.99"
    FIELD valor-bruto         AS DECIMAL
    FIELD valor-total         AS DECIMAL
    FIELD rendimento          AS CHAR FORMAT "x(20)"
    INDEX codigo IS PRIMARY UNIQUE 
          cod-emitente 
          nr-contrato 
          nr-ticket
          esp-docto
          nro-docto
          serie.

DEF NEW SHARED TEMP-TABLE tt-detalhes
    FIELD c-esp-docto       LIKE es-movto-arroz.esp-docto
    FIELD i-tipo-trans      LIKE es-movto-arroz.tipo-trans
    FIELD c-tp-desconto     LIKE es-movto-arroz.tp-desconto
    FIELD de-analise        AS DEC DECIMALS 4
    FIELD de-quantidade     LIKE es-movto-arroz.quantidade
    FIELD de-valor-unit     LIKE es-movto-arroz.valor
    FIELD de-valor-tot      LIKE es-movto-arroz.valor
    FIELD de-perc           AS DEC
    FIELD c-historico       LIKE es-movto-arroz.historico.


DEF TEMP-TABLE tt-desc-bon
    FIELD nr-ticket       LIKE es-movto-arroz.nr-ticket
    FIELD qt-bonif        AS DEC
    FIELD qt-desc         AS DEC.

DEF TEMP-TABLE tt-qtde
    FIELD nr-ticket       LIKE es-movto-arroz.nr-ticket
    FIELD qt-ori          AS DEC
    FIELD de-perc-sec     AS DEC. /* 170807 */

DEF TEMP-TABLE tt-qtde-ger
    FIELD nr-ticket       LIKE es-movto-arroz.nr-ticket
    FIELD qt-ori          AS DEC.

/* Local Variable Definitions ---                                       */

DEF VAR i-ord-aux       AS INTEGER NO-UNDO.
DEF VAR i-ordem         AS INTEGER FORMAT ">>>,>>9" NO-UNDO.
DEF VAR l-marca         AS LOG     FORMAT "*/ " INIT NO NO-UNDO.
                        
DEF VAR l-prim          AS LOG     INITIAL YES.
                        
DEF VAR da-data         LIKE es-cotacao.dt-moeda          NO-UNDO.
DEF VAR de-faixa        AS DECI FORMAT 999.9999           NO-UNDO.
DEF VAR i-retorno       AS INTE FORMAT 9                  NO-UNDO.
DEF VAR c-retorno       AS CHAR FORMAT "x(40)"            NO-UNDO.
DEF VAR de-liquido      AS DECI FORMAT ">>>,>>>,>>9.99999" DECIMALS 4 NO-UNDO.
DEF VAR de-bruto        AS DECI FORMAT ">>>,>>>,>>9.99999" DECIMALS 4 NO-UNDO.
DEF VAR de-inteiro      AS DECI FORMAT ">>>,>>>,>>9.9999"  DECIMALS 4 NO-UNDO.
DEF VAR de-quebrado     AS DECI FORMAT ">>>,>>>,>>9.9999"  DECIMALS 4 NO-UNDO.
DEF VAR de-peso-liq     AS DECI FORMAT ">>>,>>>,>>9.99999" DECIMALS 4 NO-UNDO.
DEF VAR i-contador      AS INTE NO-UNDO.

DEF VAR de-inteiro-liq  AS DECI FORMAT ">>>,>>>,>>9.9999"  DECIMALS 4 NO-UNDO.
DEF VAR de-quebrado-liq AS DECI FORMAT ">>>,>>>,>>9.9999"  DECIMALS 4 NO-UNDO.
DEF VAR de-peso-liq-liq AS DECI FORMAT ">>>,>>>,>>9.99999" DECIMALS 4 NO-UNDO.

DEFINE VARIABLE wh-pesquisa AS WIDGET-HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE l-implanta       AS LOGICAL                INITIAL NO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl   AS HANDLE                 NO-UNDO.
/*DEFINE NEW GLOBAL SHARED VARIABLE h-rsocial        AS HANDLE                 NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-achou-prog     AS LOGICAL                NO-UNDO.*/
DEFINE NEW GLOBAL SHARED VARIABLE c-nome-matriz-gl LIKE emitente.nome-matriz NO-UNDO.

DEF BUFFER b1-emitente FOR emitente.
DEF BUFFER b2-emitente FOR emitente.
DEF BUFFER bf-ticket   FOR es-ticket.
DEF BUFFER br-es-ticket FOR es-ticket.
DEF BUFFER bf2-ticket  FOR es-ticket.
DEF BUFFER bf-es-ticket  FOR es-ticket.
DEF BUFFER bf3-contrato  FOR es-contrato-arroz.
DEF BUFFER b-ins-emitente  FOR emitente.
DEF BUFFER b-mat-emitente  FOR emitente.

DEF VAR de-quant-scs    LIKE ordem-compra.qt-solic.
DEF VAR c-it-codigo-ped AS CHAR.

DEF VAR de-grau-secagem AS DEC DECIMALS 4 INITIAL 0.

DEF VAR de-devol AS DEC DECIMALS 4.

DEF NEW SHARED TEMP-TABLE tt-ticket
    FIELD nr-ticket LIKE es-ticket.nr-ticket.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME br_it-nota-fisc

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-usuario

/* Definitions for BROWSE br_it-nota-fisc                               */
&Scoped-define FIELDS-IN-QUERY-br_it-nota-fisc it-nota-fisc.nr-docum es-ticket.nr-nota-fornec es-ticket.ins-produtor IF es-ticket.operacao < 3 THEN es-ticket.nr-ticket ELSE 0 @ es-ticket.nr-ticket it-nota-fisc.it-codigo it-nota-fisc.qt-faturada[1] it-nota-fisc.vl-preuni it-nota-fisc.vl-tot-item es-ticket.rend-inteiro es-ticket.rend-quebr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_it-nota-fisc   
&Scoped-define SELF-NAME br_it-nota-fisc
&Scoped-define OPEN-QUERY-br_it-nota-fisc OPEN QUERY {&SELF-NAME}  FOR EACH es-usuario WHERE es-usuario.cod_usuario = c-seg-usuario                          , ~
      EACH it-nota-fisc NO-LOCK                               WHERE it-nota-fisc.cod-estabel = INPUT fl-estabel AND it-nota-fisc.nr-nota-fis = nr-nfd:SCREEN-VALUE                                 AND it-nota-fisc.serie       = serie:SCREEN-VALUE                            , ~
      FIRST nota-fiscal OF it-nota-fisc, ~
      FIRST ITEM NO-LOCK OF it-nota-fisc                            , ~
      FIRST es-ticket NO-LOCK                               WHERE (es-ticket.cod-estabel   = it-nota-fisc.cod-estabel                                 AND  es-ticket.nro-docto     = it-nota-fisc.nr-docum                                 AND  es-ticket.serie         >= "" /*it-nota-fisc.serie*/ AND es-ticket.cod-emitente = nota-fiscal.cod-emitente) BY es-ticket.nr-ticket /* OR                                 es-ticket.operacao = 3 */.
&Scoped-define TABLES-IN-QUERY-br_it-nota-fisc es-usuario it-nota-fisc nota-fiscal ITEM ~
es-ticket
&Scoped-define FIRST-TABLE-IN-QUERY-br_it-nota-fisc es-usuario
&Scoped-define SECOND-TABLE-IN-QUERY-br_it-nota-fisc it-nota-fisc
&Scoped-define THIRD-TABLE-IN-QUERY-br_it-nota-fisc nota-fiscal
&Scoped-define FOURTH-TABLE-IN-QUERY-br_it-nota-fisc ITEM 
&Scoped-define FifTH-TABLE-IN-QUERY-br_it-nota-fisc es-ticket


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-br_it-nota-fisc}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btOK btCancel btHelp2 RECT-5 rtToolBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "Confirmar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 16.25.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 88 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON bt-atualiza 
     LABEL "OK" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-preco 
     LABEL "Preáo" 
     SIZE 7 BY 1.

DEFINE VARIABLE c-descricao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

 /* 08/02/2012 */
DEFINE VARIABLE to-rendimento AS LOGICAL INITIAL yes 
     LABEL "Imprime Rendimento?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 TOOLTIP "Informe se Imprime Rendimento ns Nota?" NO-UNDO.

DEF VAR c-rend-medio AS CHAR.

DEFINE VARIABLE c-media AS CHARACTER FORMAT "X(256)":U 
     LABEL "Saldo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE c-media-liq AS CHARACTER FORMAT "X(256)":U 
     LABEL "P.Liq" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE c-media-ent AS CHARACTER FORMAT "X(256)":U 
     LABEL "Entr." 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE c-rendimento-contrato AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rendimento Implantaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE de-preco AS DECIMAL FORMAT ">>>,>>>,>>9.99999":U DECIMALS 5 INITIAL 0 
     LABEL "Preáo Tabela" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE de-preco-2 AS DECIMAL FORMAT ">>>,>>>,>>9.99999":U INITIAL 0 
     LABEL "Preáo Unit†rio" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE de-qt-implant AS DECIMAL FORMAT ">>,>>>,>>9.99999":U INITIAL 0 
     LABEL "Qtd Implant" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .79
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE de-quantidade AS DECIMAL FORMAT "->>>,>>>,>>9.99999":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE de-vltotal AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total Tabela" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE de-vltotal-2 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total Calculado" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE dt-vencimento AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Vecto" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fl-condicao AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Cond.Pgto" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fl-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fl-emitente AS CHARACTER FORMAT "X(38)":U 
     LABEL "" VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE VARIABLE i-contrato AS INTEGER FORMAT "zzzzzzzz9":U INITIAL 0 
     LABEL "Nr. Contrato" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i-fornecedor AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornec" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-unidade AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Un.Saco" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE nr-nfd AS CHARACTER FORMAT "x(16)" 
     LABEL "Nr. NFD":R17 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE serie AS CHARACTER FORMAT "x(5)" 
     LABEL "SÇrie":R7 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 85.86 BY 9.04.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 1.0.

DEFINE VARIABLE rs-rendimento AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "1", 1,
"2", 2,
"3", 3
     SIZE 2 BY 3 NO-UNDO.


DEFINE VARIABLE rs-tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Normal", 1,
"Secagem", 2,
"Compl.Preáo", 3
     SIZE 35 BY 0.5 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 4.50.


/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_it-nota-fisc FOR 
      es-usuario, 
      it-nota-fisc, 
      nota-fiscal,
      ITEM, 
      es-ticket SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_it-nota-fisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_it-nota-fisc wWindow _FREEFORM
  QUERY br_it-nota-fisc NO-LOCK DISPLAY
      it-nota-fisc.nr-docum            FORMAT "x(11)" COLUMN-LABEL "Nf Entrada"
    es-ticket.nr-nota-fornec         FORMAT "x(11)" COLUMN-LABEL "Nf Produtor"
    es-ticket.ins-produtor           FORMAT "x(13)" COLUMN-LABEL "Lavoura"
    IF es-ticket.operacao < 3 THEN es-ticket.nr-ticket ELSE 0 @ es-ticket.nr-ticket
    it-nota-fisc.it-codigo           FORMAT "x(06)"
    it-nota-fisc.qt-faturada[1]                     COLUMN-LABEL "Quantidade"
    it-nota-fisc.vl-preuni                          COLUMN-LABEL "Valor Unit."
    it-nota-fisc.vl-tot-item
    es-ticket.rend-inteiro                          COLUMN-LABEL "Int"
    es-ticket.rend-quebr                            COLUMN-LABEL "Qbr"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 85 BY 6.29
         FONT 1 ROW-HEIGHT-CHARS .46 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 17.46 COL 2
     btCancel AT ROW 17.46 COL 13
     btHelp2 AT ROW 17.46 COL 77.57
     RECT-5 AT ROW 1 COL 1
     rtToolBar AT ROW 17.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.14 BY 18.13
         FONT 1.

DEFINE FRAME fPage1
         RECT-21 AT ROW 1.8 COL 15
         rs-tipo AT ROW 2 COL 21 NO-LABEL
         "Tipo de Geraá∆o: " VIEW-AS TEXT 
               SIZE 12 BY .67 AT ROW 2 COL 3.00
         to-rendimento AT ROW 2 COL 68 LABEL "Imprime Rendimento?"
         fl-estabel AT ROW 3 COL 13 COLON-ALIGNED
         fl-condicao AT ROW 3 COL 26 COLON-ALIGNED
         dt-vencimento AT ROW 3 COL 38 COLON-ALIGNED
         i-contrato AT ROW 3 COL 60 COLON-ALIGNED
         i-unidade AT ROW 3 COL 79 COLON-ALIGNED
         nr-nfd AT ROW 4 COL 13 COLON-ALIGNED HELP
              "N£mero da nota fiscal"
         serie AT ROW 4 COL 26 COLON-ALIGNED HELP
              "SÇrie da nota fiscal"
         i-fornecedor AT ROW 4 COL 38 COLON-ALIGNED
         fl-emitente AT ROW 4 COL 45 COLON-ALIGNED NO-LABEL
         c-it-codigo AT ROW 5 COL 13 COLON-ALIGNED
         c-descricao AT ROW 5 COL 23 COLON-ALIGNED NO-LABEL
         de-quantidade AT ROW 6 COL 13 COLON-ALIGNED
         bt-atualiza AT ROW 6 COL 49
         bt-preco AT ROW 6 COL 56
         de-preco AT ROW 7 COL 13 COLON-ALIGNED
         de-vltotal AT ROW 7 COL 47 COLON-ALIGNED
         de-preco-2 AT ROW 8 COL 13 COLON-ALIGNED
         de-vltotal-2 AT ROW 8 COL 47 COLON-ALIGNED
         de-qt-implant AT ROW 9.20 COL 13 COLON-ALIGNED
         c-rendimento-contrato AT ROW 9.20 COL 47 COLON-ALIGNED

         c-media-ent AT ROW 6 COL 67 COLON-ALIGNED
         rs-rendimento AT ROW 6 COL 83 NO-LABEL
         c-media-liq AT ROW 7 COL 67 COLON-ALIGNED
         c-media AT ROW 8 COL 67 COLON-ALIGNED
         RECT-11 AT ROW 5 COL 64
         "Rendimento MÇdio" VIEW-AS TEXT 
              SIZE 17 BY .67 AT ROW 5.25 COL 66.5
         /*c-media-ent AT ROW 3.30 COL 39 COLON-ALIGNED 
         c-media AT ROW 4.20 COL 39 COLON-ALIGNED
         c-media-liq AT ROW 4.20 COL 69 COLON-ALIGNED*
         /*i-unidade AT ROW 4.20 COL 79 COLON-ALIGNED*/


         rs-rendimento AT ROW 7 COL 2 LABEL "Tipo Rendimento"
                                                      */
         br_it-nota-fisc AT ROW 10.25 COL 1
         RECT-7 AT ROW 1.21 COL 1.14
         "Geraá∆o Pedido Compra" VIEW-AS TEXT
              SIZE 19.57 BY .54 AT ROW 1 COL 2
              FONT 6
        WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
             SIDE-LABELS NO-UNDERLINE THREE-D 
             AT COL 1.86 ROW 1.25
             SIZE 86.14 BY 15.75
             FONT 1.

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 18.92
         WIDTH              = 88
         MAX-HEIGHT         = 30.17
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 30.17
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i} 
     {utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB br_it-nota-fisc c-rendimento-contrato fPage1 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_it-nota-fisc
/* Query rebuild information for BROWSE br_it-nota-fisc
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}  FOR EACH es-usuario WHERE es-usuario.cod_usuario = c-seg-usuario
                         ,EACH it-nota-fisc NO-LOCK
                              WHERE it-nota-fisc.cod-estabel = es-usuario.cod-estabel
                                AND it-nota-fisc.nr-nota-fis = nr-nfd:SCREEN-VALUE
                                AND it-nota-fisc.serie       = serie:SCREEN-VALUE
                           ,FIRST ITEM NO-LOCK OF it-nota-fisc
                           ,FIRST nota-fiscal OF it-nota-fis
                           ,FIRST es-ticket NO-LOCK
                              WHERE (es-ticket.cod-estabel   = it-nota-fisc.cod-estabel
                                AND  es-ticket.nro-docto     = it-nota-fisc.nr-docum
                                AND  es-ticket.serie         = it-nota-fisc.serie
                                and  es-ticket.cod-emitente  = nota-fiscal.cod-estabel) /* OR
                                es-ticket.operacao = 3 */.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE br_it-nota-fisc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON ENTRY OF wWindow
DO:
    FIND FIRST es-saldo-arroz NO-LOCK
         WHERE ROWID(es-saldo-arroz) = pRowSaldo NO-ERROR.
    

    IF AVAIL es-saldo-arroz THEN DO:
/*         FIND FIRST item NO-LOCK                                                                                      */
/*              WHERE item.it-codigo = es-saldo-arroz.it-codigo NO-ERROR.                                               */
/*         ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME fPage1 = es-saldo-arroz.it-codigo                                   */
/*                c-descricao:SCREEN-VALUE IN FRAME fPage1 = (IF AVAIL item THEN item.descricao-1 ELSE "Desconhecido"). */
    END.
    ASSIGN dt-vencimento:SCREEN-VALUE IN FRAME fPage1 = STRING(TODAY,"99/99/9999").
    
    ENABLE rs-rendimento rs-tipo WITH FRAME fPage1.
            
    APPLY 'ENTRY':U TO de-quantidade IN FRAME fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME bt-atualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualiza wWindow
ON CHOOSE OF bt-atualiza IN FRAME fPage1 /* OK */
DO:
    IF l-botao-atualiza THEN DO: 
      MESSAGE "Bot∆o OK j† foi selecionado antes!" VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
    END.

    ASSIGN l-botao-atualiza = YES.
    
    ASSIGN c-estabel = INPUT fl-estabel.
    IF input rs-tipo = 3 THEN do:
        DISP 1 @ de-quantidade WITH FRAME {&FRAME-NAME}.
        ASSIGN de-quantidade.
        ENABLE de-preco-2 WITH FRAME {&FRAME-NAME}.
    END.
    ELSE DISABLE de-preco-2 i-fornecedor WITH FRAME {&FRAME-NAME}.
    
    FOR EACH tt-qtde-ger.
       DELETE tt-qtde-ger.
    END.

    FOR EACH tt-aplic-desc-bon.
       DELETE tt-aplic-desc-bon.
    END.
    /* 140306 */
    FOR EACH tt-aplic-sec-desc-bon.
       DELETE tt-aplic-sec-desc-bon.
    END.

    EMPTY TEMP-TABLE tt-extrato.
               
    FIND FIRST es-contrato-arroz NO-LOCK WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.

    FIND es-item NO-LOCK WHERE es-item.it-codigo = es-contrato-arroz.it-codigo NO-ERROR.

    /* troca de lugar 16/04/2013 para consistencia CEI */
    /* complemento de preco o emitente Ç da tela 20/05/2013 */
    IF INPUT rs-tipo = 3 THEN DO:
        FIND FIRST emitente WHERE emitente.cod-emitente = INPUT i-fornecedor  NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        FIND FIRST emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR. /* 28/03/2011 */
    end.
    
/*     FIND FIRST es-emitente-cei WHERE es-emitente-cei.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.                         */
/*     IF AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = YES AND es-emitente-cei.dt-validade-cei < TODAY THEN DO:                 */
/*        MESSAGE "Fornecedor " emitente.cod-emitente  " possui Controle de CEI, porÇm a data de validade da mesma est† VENCIDA!" SKIP */
/*            "Verifique, pois n∆o ser† permitida a sua utilizaá∆o!!!"                                                                 */
/*            VIEW-AS ALERT-BOX ERROR.                                                                                                 */
/*        RETURN NO-APPLY.                                                                                                             */
/*     END.                                                                                                                            */
/*                                                                                                                                     */
/*     IF (AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = NO) OR NOT AVAIL es-emitente-cei THEN DO:                               */
/*             MESSAGE "Fornecedor " emitente.cod-emitente  " possui Controle de CEI INATIVO!" SKIP                                    */
/*                    "Confirma?"                                                                                                      */
/*                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE l-cont-cei-10 AS LOG.                                            */
/*                IF l-cont-cei-10 = NO THEN RETURN NO-APPLY.                                                                          */
/*     END.                                                                                                                            */


    ASSIGN i-unidade = es-item.unidade-saco.

    DO WITH FRAME fPage1:

        FIND FIRST es-saldo-arroz NO-LOCK WHERE ROWID(es-saldo-arroz) = pRowSaldo NO-ERROR.
        
        ASSIGN c-estabel = INPUT fl-estabel.

        FIND FIRST nota-fiscal NO-LOCK
            WHERE nota-fiscal.cod-estabel = INPUT fl-estabel
              AND nota-fiscal.serie       = INPUT serie
              AND nota-fiscal.nr-nota-fis = INPUT nr-nfd
            NO-ERROR.
        IF NOT AVAIL nota-fiscal AND input rs-tipo <> 3 THEN DO:
            MESSAGE "Nota de Devoluá∆o n∆o encontrada!!" VIEW-AS ALERT-BOX ERROR.
            ASSIGN l-botao-atualiza = NO.
            APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
        
       
        /* 2355743 */
        /*IF input rs-tipo = 1 /* rs-tipo <> 3 */ AND nota-fiscal.nat-operacao <> "5949DC" AND emitente.natureza = 1  THEN do: 
            MESSAGE "Natureza de Operaá∆o da Nota Fiscal de Devoluá∆o para PF DEVE ser 5949DC!" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.*/

        IF input rs-tipo = 1 /* rs-tipo <> 3 */ AND nota-fiscal.nat-operacao <> "5949DC" AND emitente.natureza = 1  THEN do: 

            FIND FIRST ext-natur-oper-cfop NO-LOCK
                 WHERE ext-natur-oper-cfop.nat-operacao = nota-fiscal.nat-operacao NO-ERROR.

            IF AVAIL ext-natur-oper-cfop
                 AND NOT ext-natur-oper-cfop.dev-liq-deposito-pf THEN DO:

                MESSAGE "Natureza de Operaá∆o da Nota Fiscal de Devoluá∆o deve ser de Devoluá∆o de Liquidaá∆o de Dep¢sito PF!" VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.

        /* 2355743 */
        /*IF input rs-tipo = 1 /* rs-tipo <> 3 */ AND nota-fiscal.nat-operacao <> "5949SB" AND emitente.natureza >= 2  THEN do:
          MESSAGE "Natureza de Operaá∆o da Nota Fiscal de Devoluá∆o para PJ DEVE ser 5949SB" VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
        END.*/
    
        IF input rs-tipo = 1 /* rs-tipo <> 3 */ AND nota-fiscal.nat-operacao <> "5949SB" AND emitente.natureza >= 2  THEN do:

            FIND FIRST ext-natur-oper-cfop NO-LOCK
                 WHERE ext-natur-oper-cfop.nat-operacao = nota-fiscal.nat-operacao NO-ERROR.

            IF AVAIL ext-natur-oper-cfop
                 AND NOT ext-natur-oper-cfop.dev-liq-deposito-pj THEN DO:

                MESSAGE "Natureza de Operaá∆o da Nota Fiscal de Devoluá∆o deve ser de Devoluá∆o de Liquidaá∆o de Dep¢sito - PJ!" VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.

        /* 2355743 */
        /*IF input rs-tipo = 2 AND nota-fiscal.nat-operacao <> "5949AS" THEN do:
          MESSAGE "Natureza de Operaá∆o da Nota Fiscal de Devoluá∆o deb Secagem DEVE ser 5949AS!" VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
        END.*/

        IF input rs-tipo = 2 AND nota-fiscal.nat-operacao <> "5949AS" THEN do:

            FIND FIRST ext-natur-oper-cfop NO-LOCK
                 WHERE ext-natur-oper-cfop.nat-operacao = nota-fiscal.nat-operacao NO-ERROR.

            IF AVAIL ext-natur-oper-cfop
                 AND NOT ext-natur-oper-cfop.dev-cob-secagem THEN DO:

                MESSAGE "Natureza de Operaá∆o da Nota Fiscal de Devoluá∆o deve ser de Devoluá∆o para Cobranáa de Serviáo de Secagem!" VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.

        IF input rs-tipo <> 3 THEN do:
            FIND FIRST b-ins-emitente WHERE b-ins-emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.  
            IF AVAIL b-ins-emitente THEN DISP b-ins-emitente.cod-emitente  @ i-fornecedor
                                              b-ins-emitente.nome-emit     @ fl-emitente WITH FRAME {&FRAME-NAME}.
        END.
                    


        IF input rs-tipo = 2  THEN DO:
            IF SUBSTR(nota-fiscal.observ-nota,1900,7) <> "SECAGEM" THEN DO:
                MESSAGE "Nota Fiscal n∆o foi gerada como Secagem!" VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
                RETURN NO-APPLY.
            END.
        END.

        IF input rs-tipo = 1 THEN DO:
            IF SUBSTR(nota-fiscal.observ-nota,1900,7) = "SECAGEM" THEN DO:
                MESSAGE "Nota Fiscal n∆o foi gerada como de Arroz a Dep¢sito!" VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
                RETURN NO-APPLY.
            END.
        END.

        IF input rs-tipo = 3 THEN DO: /* complemento */

            FIND FIRST es-contrato-arroz NO-LOCK WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.
            ASSIGN l-prim = NO.
            FIND FIRST b-ins-emitente WHERE b-ins-emitente.cod-emitente = INPUT i-fornecedor NO-LOCK NO-ERROR.  
            ASSIGN c-nome-emit-compl = b-ins-emitente.nome-emit.
            FIND es-param-estab NO-LOCK WHERE es-param-estab.cod-estabel = es-saldo-arroz.cod-estabel NO-ERROR.
            FIND FIRST mgadm.banco WHERE banco.cod-banco = b-ins-emitente.cod-banco NO-LOCK NO-ERROR.
            IF NOT AVAIL banco THEN do:
                MESSAGE  "Esse fornecedor n∆o tem um C¢digo de Banco v†lido em seu cadastro!" skip
                         "Permite gerar Pedido de Complemento SEM Informaá‰es Banc†rias?" view-as alert-box QUESTION BUTTONS YES-NO UPDATE l-sem-inf-bco AS LOG.
                IF l-sem-inf-bco = NO THEN LEAVE.
            END.
        END. /* compl preáo */

        IF input rs-tipo <> 3 THEN DO: 
        
            FIND FIRST b-ins-emitente WHERE b-ins-emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.      
            
            IF nota-fiscal.esp-docto <> 22 THEN DO: /* NFS */
                MESSAGE "Nota Fiscal n∆o Ç de Devoluá∆o de Mercadoria!" VIEW-AS ALERT-BOX ERROR.
                ASSIGN l-botao-atualiza = NO.
                APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.
    
            FIND natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
            IF natur-oper.terceiros <> YES or
               natur-oper.tp-oper-terc <> 5 THEN DO:
                MESSAGE "Natureza da Nota n∆o Ç de Devoluá∆o de Mercadoria em Consignaá∆o!" VIEW-AS ALERT-BOX ERROR.
                ASSIGN l-botao-atualiza = NO.
                APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.
            
    
            FIND FIRST it-nota-fisc NO-LOCK OF nota-fiscal.
        
            FIND b1-emitente NO-LOCK WHERE b1-emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-ERROR.
            
            FIND FIRST es-ticket NO-LOCK
                 WHERE es-ticket.cod-estabel    = it-nota-fisc.cod-estabel
                   AND es-ticket.nro-docto      = it-nota-fisc.nr-docum
                   AND es-ticket.serie          >= "" /*it-nota-fisc.serie*/
                   AND es-ticket.cod-produtor   = b1-emitente.cod-emitente NO-ERROR.
            IF  AVAIL es-ticket THEN DO:
            
              IF es-ticket.nr-contrato <> es-contrato-arroz.nr-contrato THEN DO:
                MESSAGE "A NF de Devoluá∆o pertence ao Contrato " es-ticket.nr-contrato 
                        " e vocà selecionou o contrato " es-contrato-arroz.nr-contrato "!" SKIP
                        "Favor verificar!" VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
              END.
            END.
            

            FOR EACH b2-emitente NO-LOCK WHERE
                     b2-emitente.cod-emitente = es-contrato-arroz.cod-emitente
                     BREAK BY b2-emitente.cod-emitente:
    
                IF b1-emitente.nome-matriz = b2-emitente.nome-matriz THEN l-matriz = YES.
                                                                     ELSE l-matriz = NO.
                
                IF LAST-OF(b2-emitente.cod-emitente) and
                   l-matriz = NO THEN DO: 
                
                    MESSAGE "Esta Nota n∆o pertence ao Contrato." SKIP
                        "Matrizes diferentes, Matriz da Nota Fiscal " b1-emitente.nome-matriz  
                        "Contrato " b2-emitente.nome-matriz  VIEW-AS ALERT-BOX ERROR.
                    APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
                    RETURN NO-APPLY.
                END.
            END.
               
            IF nota-fiscal.nr-proc-exp <> "" THEN DO:
                MESSAGE "Esta Nota j† pertence ao Contrato " nota-fiscal.nr-proc-exp VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.
       END. /* <> compl preáo q n∆o tem nota */


                         
    END.    
                           
    FIND FIRST cond-pagto NO-LOCK
        WHERE cond-pagto.cod-cond-pag = INPUT fl-condicao
        NO-ERROR.

    IF NOT AVAIL cond-pagto THEN DO:
        MESSAGE "Condiá∆o de Pagamento Inv†lida!" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO fl-condicao IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
    END.

    ASSIGN de-quantidade = IF input rs-tipo <> 3 THEN 0 ELSE 1 
           de-inteiro    = 0
           de-quebrado   = 0
           de-peso-liq   = 0
           i-contador    = 0
           l-botao-preco = NO /* 051205 */
           de-quant-scs  = 0 /* 290806 */
           c-it-codigo-ped = "" /* 290806 */.

    
    IF input rs-tipo <> 3 THEN do:
        FOR EACH it-nota-fisc NO-LOCK OF nota-fiscal:
            ASSIGN de-quantidade = de-quantidade + it-nota-fisc.qt-faturada[1].
                         
            FIND FIRST es-ticket NO-LOCK
                WHERE es-ticket.cod-estabel    = it-nota-fisc.cod-estabel
                  AND es-ticket.nro-docto      = it-nota-fisc.nr-docum
                  AND es-ticket.serie          >= "" /*it-nota-fisc.serie*/
                  AND es-ticket.cod-produtor   = b1-emitente.cod-emitente
                NO-ERROR.
    
            IF AVAIL es-ticket THEN
                ASSIGN i-contador  = i-contador  + 1
                       de-peso-liq = de-peso-liq + es-ticket.peso-liq
                       de-inteiro  = de-inteiro  + (es-ticket.rend-inteiro * es-ticket.peso-liq)
                       de-quebrado = de-quebrado + (es-ticket.rend-quebr   * es-ticket.peso-liq).
        END.
        
        FIND FIRST es-movto-arroz NO-LOCK
            WHERE es-movto-arroz.nr-contrato = es-contrato-arroz.nr-contrato
              AND es-movto-arroz.esp-docto   = "IPL"
            NO-ERROR.
         /* 201006 */
        IF NOT AVAIL es-movto-arroz THEN DO:
           FIND FIRST es-movto-arroz NO-LOCK
            WHERE es-movto-arroz.nr-contrato = es-contrato-arroz.nr-contrato
              AND es-movto-arroz.esp-docto   = "TRA"
              AND es-movto-arroz.tipo-trans  = 1
            NO-ERROR.       
        END.
    
        IF AVAIL es-movto-arroz THEN
            ASSIGN i-contador            = i-contador  + 1
                   de-peso-liq           = de-peso-liq + es-movto-arroz.quantidade
                   de-inteiro            = de-inteiro  + (es-contrato-arroz.rend-inteiro * es-movto-arroz.quantidade)
                   de-quebrado           = de-quebrado + (es-contrato-arroz.rend-quebr   * es-movto-arroz.quantidade)
                   de-qt-implant         = es-movto-arroz.quantidade
                   c-rendimento-contrato = STRING(es-contrato-arroz.rend-inteiro,"99.9999") + " X " +
                                           STRING(es-contrato-arroz.rend-quebr,"99.9999").
        ELSE
            ASSIGN de-qt-implant         = 0
                   c-rendimento-contrato = "".
            
        ASSIGN c-media = STRING((de-inteiro  / de-peso-liq),"99.9999") + " X " + 
                         STRING((de-quebrado / de-peso-liq),"99.9999").
    
        
        
        DEF VAR c-rend-medio  AS CHAR FORMAT "x(17)".
            /* alterei para esapi006b em 271004 */
        RUN esp/esapi006b.p (INPUT es-contrato-arroz.nr-contrato,
                            OUTPUT c-rend-medio).
    
        ASSIGN c-media = c-rend-medio.
    
        
        IF es-contrato-arroz.tipo-contrato = 1 THEN DO: /* Compra Direta */
           RUN pi-movto-arroz (INPUT "IPL").
           RUN pi-movto-arroz (INPUT "CMP").
        END.
        IF es-contrato-arroz.tipo-contrato = 2 THEN DO: /* Deposito */
           RUN pi-movto-arroz (INPUT "IPL").
           RUN pi-movto-arroz (INPUT "DEP").
           RUN pi-movto-arroz (INPUT "tra").
           RUN pi-movto-arroz (INPUT "Dsc").
        END.
        IF es-contrato-arroz.tipo-contrato = 3 THEN DO: /* Importacao */
           RUN pi-movto-arroz (INPUT "IPL").
           RUN pi-movto-arroz (INPUT "IMP").
        END.
        /* armazenagem - erika - 07/08 */
        IF es-contrato-arroz.tipo-contrato = 4 OR       /* Armazenagem */
           es-contrato-arroz.tipo-contrato = 5 THEN DO: /* tra-estab */
           RUN pi-movto-arroz (INPUT "DEP").
           RUN pi-movto-arroz (INPUT "DSC").
        END.
    
        FOR EACH tt-extrato:
          de-soma-m = de-soma-m + (tt-extrato.peso-liq - (tt-extrato.desc-impureza - tt-extrato.qtd-bonific) - tt-extrato.desc-umidade).
        END.
    
        FOR EACH tt-extrato:
        
            /* 240805 */
            FIND FIRST tt-bonifica WHERE
                       tt-bonifica.nr-ticket = es-ticket.nr-ticket
                NO-LOCK NO-ERROR.
            IF AVAIL tt-bonifica THEN DO:                                   
                ASSIGN tt-extrato.qtd-bonific = 0.
                FOR EACH tt-bonifica WHERE 
                         tt-bonifica.nr-ticket = es-ticket.nr-ticket NO-LOCK:
                  ASSIGN tt-extrato.qtd-bonific = tt-extrato.qtd-bonific + tt-bonifica.quantidade.
                END.
            END.
    
            ASSIGN de-soma-ii = de-soma-ii + 
                      ((tt-extrato.peso-liq - (tt-extrato.desc-impureza - tt-extrato.qtd-bonific) - tt-extrato.desc-umidade) * tt-extrato.rend-inteiro ) /
                      de-soma-m
                   de-soma-qq = de-soma-qq + 
                      ((tt-extrato.peso-liq - (tt-extrato.desc-impureza - tt-extrato.qtd-bonific) - tt-extrato.desc-umidade) * tt-extrato.rend-quebr ) /
                      de-soma-m.     
                
        END.

        ASSIGN c-media-ent = STRING(DE-SOMA-II,"99.9999") + " X " + 
                             STRING(DE-SOMA-QQ,"99.9999").
        DISP STRING(DE-SOMA-II,"99.9999") + " X " + STRING(DE-SOMA-QQ,"99.9999") @ 
            c-media-ent WITH FRAME {&FRAME-NAME}.
        
        /* fim l¢gica es4021 entradas */ 
        
        
        IF AVAIL es-contrato-arroz AND es-contrato-arroz.u-log-1 = FALSE THEN DO:
    
    
            RUN esp/es4012.w (INPUT ROWID(es-contrato-arroz),
                              INPUT de-quantidade).
    
                           
    
            
            /** Calcula Rendimento Medio das cargas selecionadas **/
        
            ASSIGN 
               de-inteiro    = 0
               de-quebrado   = 0
               de-peso-liq   = 0
               i-contador    = 0
               l-botao-preco = NO.
    
            FOR EACH es-movto-ticket
               WHERE es-movto-ticket.nr-contrato = es-contrato-arroz.nr-contrato
                 AND es-movto-ticket.u-log-2     = FALSE :
    
               FIND es-ticket WHERE 
                    es-ticket.nr-ticket = es-movto-ticket.nr-ticket
                    NO-LOCK NO-ERROR.
    
               ASSIGN i-contador  = i-contador  + 1
                      de-peso-liq = de-peso-liq + es-ticket.peso-liq
                      de-inteiro  = de-inteiro  + (es-ticket.rend-inteiro * es-ticket.peso-liq)
                      de-quebrado = de-quebrado + (es-ticket.rend-quebr   * es-ticket.peso-liq).
    
            END.      
    
            ASSIGN c-media = STRING((de-inteiro  / de-peso-liq),"99.9999") + " X " + 
                             STRING((de-quebrado / de-peso-liq),"99.9999").
    
        END.
         
        
        DISP de-qt-implant
             c-rendimento-contrato
             c-media
             i-unidade
             de-quantidade 
             WITH FRAME {&FRAME-NAME}.
    
        
        
        FIND FIRST es-param-empresa NO-LOCK
            WHERE es-param-empresa.ep-codigo = i-ep-codigo-usuario
            NO-ERROR.
        
        ASSIGN da-data = TODAY
               de-faixa = de-inteiro / de-peso-liq.
        
        RUN esp\esapi001.p
            (INPUT da-data,
             INPUT de-faixa,
             OUTPUT i-retorno,
             OUTPUT c-retorno,
             OUTPUT de-liquido,
             OUTPUT de-bruto).
        
        IF i-retorno > 0 THEN DO:
            MESSAGE c-retorno VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
            /*
            RETURN NO-APPLY.
            */
        END.
                        
        ASSIGN de-preco = de-bruto.
    
        
        ASSIGN de-vltotal = de-quantidade * (de-preco).
    
        DISP de-preco
             de-vltotal
            WITH FRAME {&FRAME-NAME}.
    
    
        ASSIGN c-estabel = INPUT fl-estabel.
        

            /* 080909 */
            FIND FIRST it-nota-fisc NO-LOCK 
                  WHERE it-nota-fisc.cod-estabel = INPUT fl-estabel   
                    AND it-nota-fisc.nr-nota-fis = nr-nfd:SCREEN-VALUE
                    AND it-nota-fisc.serie       = serie:SCREEN-VALUE NO-ERROR.                                                                                                                                     
            IF AVAIL it-nota-fisc THEN DO:
                 FIND FIRST natur-oper WHERE natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK.
                 FIND es-param-estab WHERE es-param-estab.cod-estabel = c-estabel NO-LOCK NO-ERROR.
            END.

    
            FOR EACH es-usuario WHERE 
                     es-usuario.cod_usuario = c-seg-usuario, 
               EACH  it-nota-fisc NO-LOCK 
               WHERE it-nota-fisc.cod-estabel = INPUT fl-estabel
                 AND it-nota-fisc.nr-nota-fis = nr-nfd:SCREEN-VALUE  
                 AND it-nota-fisc.serie       = serie:SCREEN-VALUE,
               FIRST ITEM NO-LOCK OF it-nota-fisc, 
               FIRST nota-fiscal OF it-nota-fisc, 
               FIRST es-ticket NO-LOCK  WHERE (es-ticket.cod-estabel = it-nota-fisc.cod-estabel 
                                        AND   es-ticket.nro-docto    = it-nota-fisc.nr-docum                                 
                                        AND   es-ticket.serie        >= "" /*IF natur-oper.tipo = 1 THEN string(es-param-estab.u-int-1) ELSE string(es-param-estab.u-int-2) */
                                        AND   es-ticket.cod-emitente = nota-fiscal.cod-emitente)
                                        BY es-ticket.nr-ticket:                                                                                                                                      
    
    
                FIND FIRST tt-qtde-ger WHERE
                           tt-qtde-ger.nr-ticket = es-ticket.nr-ticket
                    NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-qtde-ger THEN DO:
                    CREATE tt-qtde-ger.
                    ASSIGN tt-qtde-ger.nr-ticket = es-ticket.nr-ticket
                           tt-qtde-ger.qt-ori =  it-nota-fisc.qt-faturada[1] .
    
                END.
            END.
    
            /* 201005 */
            /* l¢gica liquidaá‰es */
            FOR EACH tt-qtde-ger:
                CREATE tt-ticket.
                ASSIGN tt-ticket.nr-ticket = tt-qtde-ger.nr-ticket.
            END.
            
            FIND FIRST es-contrato-arroz NO-LOCK WHERE 
                ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.
                
            RUN esp/esapi006c.p (INPUT es-contrato-arroz.nr-contrato,
                                 OUTPUT c-media-liq).

    
           
             
            DISP c-media-liq WITH FRAME {&FRAME-NAME}.
    END. /* <> complemento de preáo */
    
    {&OPEN-QUERY-br_it-nota-fisc}
            
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-preco                     
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-preco wWindow
ON CHOOSE OF bt-preco IN FRAME fPage1 /* Preco */
DO:
   
  IF de-preco = 0 THEN DO:
    MESSAGE "Preáo de Tabela Ç zero! Deve ser digitado um valor!" VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END. /* 051205 */

  IF l-botao-preco THEN DO: 
      MESSAGE "Bot∆o Preáo j† foi selecionado antes!" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.

  ASSIGN l-botao-preco = yes.

  IF input rs-tipo = 3 THEN ASSIGN de-preco-2 = de-preco.
  
  IF input rs-tipo = 2 THEN RUN pi-secagem.

  /* complemento de preáo */
  IF input rs-tipo = 3 THEN DO:
       ASSIGN de-vltotal   = de-preco-2 
              de-vltotal-2 = de-preco-2.
       DISP de-preco-2 WITH FRAME {&FRAME-NAME}.
  END.
  /* n∆o Ç complemento de preáo */
  ELSE DO:

       ASSIGN de-val-ori-desc = 0
              de-val-ori-bon  = 0.
    
       FOR EACH tt-bonifica:
           DELETE tt-bonifica.
       END.
    
       FOR EACH tt-qtde.
         DELETE tt-qtde.
       END.
        
       FIND FIRST es-contrato-arroz NO-LOCK WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.
    
       FIND es-item WHERE es-item.it-codigo = es-contrato-arroz.it-codigo NO-LOCK NO-ERROR.
    
       FIND FIRST es-saldo-arroz NO-LOCK WHERE ROWID(es-saldo-arroz) = pRowSaldo NO-ERROR.
        
       /******* IN÷CIO nova tratativa do bot∆o de preáo considerendo bonificaá∆o e descontos proporcionais na baixa parcial  ********/
       FOR EACH es-movto-arroz NO-LOCK 
            WHERE es-movto-arroz.nome-matriz  = es-contrato-arroz.nome-matriz
              AND es-movto-arroz.cod-emitente = nota-fiscal.cod-emitente          
              AND es-movto-arroz.nr-contrato  = es-contrato-arroz.nr-contrato
              AND es-movto-arroz.cod-estabel  = es-contrato-arroz.cod-estabel
              AND (es-movto-arroz.esp-docto     = "DSC" OR es-movto-arroz.esp-docto = "BON"),
               FIRST es-ticket NO-LOCK
                    WHERE es-ticket.nr-ticket = es-movto-arroz.nr-ticket
            BREAK BY es-movto-arroz.cod-emitente
                  BY es-movto-arroz.esp-docto   
                  BY es-movto-arroz.dt-trans
                  BY es-movto-arroz.nr-contrato:
          
            IF es-movto-arroz.tipo-trans = 2 THEN DO:
                FIND es-tipo-desconto WHERE
                     es-tipo-desconto.tp-desconto = es-movto-arroz.tp-desconto
                     NO-LOCK NO-ERROR.
                        
                if es-movto-arroz.tp-desconto <> "implantacao" then do:
                                                                             
                    IF NOT AVAIL es-tipo-desconto THEN NEXT.
                    
                    IF (es-tipo-desconto.financeiro) THEN do:
                      FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = es-ticket.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                      IF NOT AVAIL tt-desc-bon THEN DO:                      
                        /* 1 */
                        CREATE tt-desc-bon.
                        ASSIGN tt-desc-bon.nr-ticket = es-ticket.nr-ticket.
                      END.
                      ASSIGN tt-desc-bon.qt-desc = tt-desc-bon.qt-desc + es-movto-arroz.quantidade
                             de-val-ori-desc     = de-val-ori-desc + es-movto-arroz.quantidade.
    
                    END.
                end. /* nao implantacao */ 
        
            END. /* saidas */
            
            IF  es-movto-arroz.esp-docto = "DSC" THEN DO:
            
            RUN esp/esapi005.p (INPUT es-ticket.nr-ticket).          
    
                    IF es-ticket.dt-entrada > 11/09/04 THEN DO: 
                        
                        RUN  pi-quantidade.
                        FOR EACH tt-detalhes:
                            DELETE tt-detalhes.
                        END.
    
                        RUN esp/esapi005b.p (INPUT es-ticket.nr-ticket,
                                             INPUT di-quantidade).
    
                    END. /* interpreta data */
    
                    FOR EACH tt-detalhes WHERE tt-detalhes.c-esp-docto = "BON":
    
                              FIND FIRST tt-bonifica where
                                         tt-bonifica.nr-ticket   = es-ticket.nr-ticket AND
                                         tt-bonifica.c-tp-desconto = tt-detalhes.c-tp-desconto AND
                                         tt-bonifica.quantidade = tt-detalhes.de-quantidade
                                  NO-LOCK NO-ERROR.
                              IF NOT AVAIL tt-bonifica THEN DO:
    
                                CREATE tt-bonifica.
                                ASSIGN tt-bonifica.nr-ticket     = es-ticket.nr-ticket
                                       tt-bonifica.c-tp-desconto = tt-detalhes.c-tp-desconto
                                       tt-bonifica.quantidade    = tt-bonifica.quantidade + tt-detalhes.de-quantidade /* 101105 */
                                       de-val-ori-bon            = de-val-ori-bon + tt-detalhes.de-quantidade.
    
                             END.
    
                    END. /* for each tt-detalhes */
            end. /* dsc */
              
            /*** BONIFICACAO ***/
            IF  es-movto-arroz.esp-docto = "BON" THEN DO:
                FIND FIRST tt-desc-bon WHERE
                           tt-desc-bon.nr-ticket = es-ticket.nr-ticket
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAIL tt-desc-bon THEN DO:
                    CREATE tt-desc-bon.
                    ASSIGN tt-desc-bon.nr-ticket = es-ticket.nr-ticket.
                END.
                ASSIGN tt-desc-bon.qt-bon = tt-desc-bon.qt-bon + es-movto-arroz.quantidade.
            END. /* se bonificaá∆o */      
    
            /*** quantidade disponivel ***/    
            FIND FIRST tt-qtde WHERE
                       tt-qtde.nr-ticket = es-movto-arroz.nr-ticket
                NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-qtde THEN DO:
                
                ASSIGN de-a-secar = 0
                       de-entra = 0
                       de-saida = 0
                       de-tot-saldo = 0
                       de-tot-secag = 0
                       de-tot-seco = 0
                       de-a-secar = 0
                       vl-tot-secag = 0
                       vl-tot-seco = 0
                       vl-a-secar = 0
                       de-quantidade2 = 0
                       de-tot-a-secar = 0. /* 080404 */
    
                RUN esp/esapi007.p (INPUT  es-movto-arroz.nr-contrato,
                                    INPUT  es-movto-arroz.nr-ticket,
                                    INPUT  es-movto-arroz.nr-ticket,
                                    OUTPUT de-entra,    
                                    OUTPUT de-saida,    
                                    OUTPUT de-tot-saldo,
                                    OUTPUT de-tot-secag,
                                    OUTPUT de-tot-seco, 
                                    OUTPUT de-a-secar,
                                    OUTPUT vl-tot-secag,
                                    OUTPUT vl-tot-seco, 
                                    OUTPUT vl-a-secar).
                
                ASSIGN de-perc-sec = de-a-secar / de-tot-saldo /* 170807 */
                       de-quantidade2 = de-quantidade2 + de-tot-saldo
                       de-tot-a-secar = de-tot-a-secar + de-a-secar /* 280305 arredondar*/
                       de-tot-a-secar = ROUND(de-tot-a-secar,2).
    
                /* 041105 */
                IF input rs-tipo = 2 THEN de-quantidade2 = de-quantidade2.
                ELSE de-quantidade2 = de-quantidade2 - de-tot-a-secar.
                /* 041105 */
                
                CREATE tt-qtde.
                ASSIGN tt-qtde.nr-ticket = es-movto-arroz.nr-ticket
                       tt-qtde.qt-ori    = de-quantidade2
                       tt-qtde.de-perc-sec = de-perc-sec.
           
                
            END. /* not avail tt-qtde */
                
            
    
       END. /* for each es-movto */
    
       ASSIGN val-bon-limpo = 0.
       FOR EACH tt-bonifica:
         ASSIGN val-bon-limpo = val-bon-limpo + tt-bonifica.quantidade.
       END. /* for each tt-bonifica */ 
    
       /* se saldo limpo for zero coloca o outro */
       IF val-bon-limpo = 0 THEN DO:
       
          /* verifica se tem bonificaá∆o limpa sen∆o coloca se houver a suja mesmo */
          FOR EACH es-movto-arroz WHERE
                   es-movto-arroz.nr-contrato = es-contrato-arroz.nr-contrato
                   NO-LOCK BY es-movto-arroz.tipo-trans:
      
             IF es-movto-arroz.esp-docto = "BON" THEN do: 
                 CREATE tt-bonifica.
                 ASSIGN tt-bonifica.nr-ticket     = es-movto-arroz.nr-ticket
                        tt-bonifica.c-tp-desconto = es-movto-arroz.tp-desconto
                        tt-bonifica.quantidade    = tt-bonifica.quantidade + es-movto-arroz.quantidade 
                        de-val-ori-bon            = de-val-ori-bon + es-movto-arroz.quantidade.
                  
             END. /* bon */
          END. /* for each es-movto */
       END. /* val-bon-limpo = 0 */
        
       /* se n∆o gerou a tabela de desconto e de bonificaá∆o aqui eu gero a mesma */
       FIND FIRST tt-qtde NO-LOCK no-error.
       IF NOT AVAIL tt-qtde THEN do:
    
           /*** quantidade disponivel ***/    
           FIND FIRST tt-qtde NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-qtde THEN DO:        
               
               FOR EACH tt-qtde-ger:
                   CREATE tt-qtde.
                   ASSIGN tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket
                          tt-qtde.qt-ori    = 0 /*tt-qtde-ger.qt-ori*/.
    
                   FIND FIRST tt-desc-bon WHERE
                              tt-desc-bon.nr-ticket = tt-qtde-ger.nr-ticket
                       NO-LOCK NO-ERROR.
                   IF NOT AVAIL tt-desc-bon THEN do:
                       CREATE tt-desc-bon.
                   END.
               END.
           END. /* not avail tt-qtde */
       END. /* if not avail tt-qtde */
        
      
       DO TRANS:
    
            /* gera valores de desconto e bonificacao parciais aplicados para cada ticket */
            FOR EACH tt-qtde-ger BREAK BY tt-qtde-ger.nr-ticket:
    
              FIND tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK NO-ERROR.                
                            
              /* interpretacao se n∆o tem desc nem bonificaá∆o */
              FOR EACH tt-qtde WHERE tt-qtde.qt-ori = 0: 
                 
               ASSIGN de-a-secar = 0
                      de-entra = 0
                      de-saida = 0
                      de-tot-saldo = 0
                      de-tot-secag = 0
                      de-tot-seco = 0
                      de-a-secar = 0
                      vl-tot-secag = 0
                      vl-tot-seco = 0
                      vl-a-secar = 0
                      de-quantidade2 = 0
                      de-tot-a-secar = 0.
    
               RUN esp/esapi007.p (INPUT  i-contrato,
                                   INPUT  tt-qtde.nr-ticket,
                                   INPUT  tt-qtde.nr-ticket,
                                   OUTPUT de-entra,    
                                   OUTPUT de-saida,    
                                   OUTPUT de-tot-saldo,
                                   OUTPUT de-tot-secag,
                                   OUTPUT de-tot-seco, 
                                   OUTPUT de-a-secar,
                                   OUTPUT vl-tot-secag,
                                   OUTPUT vl-tot-seco, 
                                   OUTPUT vl-a-secar).
    
               ASSIGN de-quantidade2 = de-quantidade2 + de-tot-saldo
                      de-tot-a-secar = de-tot-a-secar + de-a-secar
                      de-tot-a-secar = ROUND(de-tot-a-secar,2).
               
               IF input rs-tipo = 2 THEN de-quantidade2 = de-quantidade2.
               ELSE de-quantidade2 = de-quantidade2 - de-tot-a-secar.
               ASSIGN tt-qtde.qt-ori    = de-quantidade2.
    
              END. /* for each pra zeradas */
    
              FIND FIRST tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK.
    
              FIND tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
    
              IF not(tt-qtde.qt-ori = tt-qtde-ger.qt-ori) THEN DO:
              
                   FIND FIRST tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK. 
                   
                    ASSIGN de-perc = ROUND((tt-qtde-ger.qt-ori * 100) / tt-qtde.qt-ori,4).
              
                    FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL tt-desc-bon THEN
                      ASSIGN tt-desc-bon.qt-desc = ROUND((tt-desc-bon.qt-desc * de-perc) / 100,4)
                             tt-desc-bon.qt-bon  = ROUND((tt-desc-bon.qt-bon * de-perc) / 100,4).
                    
                    FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                    IF AVAIL tt-bonifica THEN ASSIGN tt-bonifica.quantidade = ROUND((tt-bonifica.quantidade * de-perc) / 100,4).
              
                    /****** mÇdia *******/                   
              END. /* qtde original difere da negociada na liq */
    
            END. /* for each tt-qtde-ger */
    
            /* agora ira calcular o preco unitario */
            FOR EACH tt-qtde-ger: 
            
               IF tt-qtde-ger.qt-ori <= 0 then next.
    
               FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde-ger.nr-ticket EXCLUSIVE-LOCK NO-ERROR.   /* 170807 */         
                IF AVAIL tt-desc-bon THEN do:
                    FIND FIRST tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK NO-ERROR.            
                    IF AVAIL tt-qtde THEN DO:
                        IF tt-qtde.de-perc-sec <> 0 THEN ASSIGN tt-desc-bon.qt-desc = tt-desc-bon.qt-desc - (tt-desc-bon.qt-desc * tt-qtde.de-perc-sec).
                    END.
                    ASSIGN de-desc = de-desc + tt-desc-bon.qt-desc.
                END.
                
                FOR EACH tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK:
                    FIND FIRST tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK NO-ERROR.            
                    IF AVAIL tt-qtde THEN DO:
                        IF tt-qtde.de-perc-sec <> 0 THEN ASSIGN tt-bonifica.quantidade = tt-bonifica.quantidade - (tt-bonifica.quantidade * tt-qtde.de-perc-sec).
                    END.
                    ASSIGN de-bon  = de-bon  + tt-bonifica.quantidade.
                END.
            END. /* TT-QTDE-GER */
    
        END. /* trans */
        
        ASSIGN de-val-desc  = de-desc * INPUT de-preco
               de-val-bon   = de-bon  * INPUT de-preco
               de-val-semdb = de-quantidade * INPUT de-preco
               de-val-liq   = de-val-semdb - de-val-desc + de-val-bon
               de-val-unit  = de-val-liq / de-quantidade.
               
        IF de-val-unit = ? THEN DO:        
            ASSIGN de-val-liq   = de-val-semdb
                   de-val-unit  = de-val-liq / de-quantidade.      
        END.
            
    
        ASSIGN de-vltotal   = de-val-unit * de-quantidade
               de-vltotal-2 = de-val-unit * de-quantidade
               de-preco-2   = de-val-unit.
    
        DISP de-val-unit @ de-preco-2 
             de-vltotal-2 WITH FRAME {&FRAME-NAME}.

  END. /* n∆o Ç secagem e nem complemento de preáo */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWindow
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wWindow
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* Confirmar */
DO:

    IF l-botao-preco = NO THEN DO: 
        MESSAGE "ê necess†rio entrar no Bot∆o Preáo!" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    IF l-botao-atualiza = NO THEN DO: 
        MESSAGE "ê necess†rio entrar no Bot∆o OK!" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    IF rs-tipo = 1 THEN DO:
    
      FIND LAST es-hist-rend-ped WHERE
                es-hist-rend-ped.cod-estabel = fl-estabel AND
                es-hist-rend-ped.nr-contrato = i-contrato
         NO-LOCK NO-ERROR.
      IF NOT AVAIL es-hist-rend-ped AND 
        l-digitou-rend = NO THEN DO:
        MESSAGE "Por ser a primeira Liquidaá∆o Ç necess†rio entrar na Escolha do Tipo de Rendimento!"
          VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
    END.

    
        
    RUN pi-gera-pedido.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME serie
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL input serie wWindow
ON LEAVE OF input serie IN FRAME fPage1 /* serie */
DO:
       FIND FIRST nota-fiscal NO-LOCK
            WHERE nota-fiscal.cod-estabel = INPUT fl-estabel
              AND nota-fiscal.serie       = INPUT serie
              AND nota-fiscal.nr-nota-fis = INPUT nr-nfd
            NO-ERROR.
        IF NOT AVAIL nota-fiscal AND input rs-tipo <> 3 THEN DO:
            MESSAGE "Nota de Devoluá∆o n∆o encontrada!!" VIEW-AS ALERT-BOX ERROR.
            ASSIGN l-botao-atualiza = NO.
            APPLY "ENTRY" TO nr-nfd IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.


        /* troca de lugar 16/04/2013 para consistencia CEI */
        FIND FIRST emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR. /* 28/03/2011 */
        FIND FIRST es-emitente-cei WHERE es-emitente-cei.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
        IF AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = YES AND es-emitente-cei.dt-validade-cei < TODAY THEN DO:

            MESSAGE "Fornecedor " emitente.cod-emitente  " possui  Controle de CEI, porÇm a data de validade da mesma est† VENCIDA!" SKIP
             "Verifique, pois n∆o ser† permitida a sua utilizaá∆o!!!"
             VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
           
        END.

        IF (AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = NO) OR NOT AVAIL es-emitente-cei THEN DO:
             MESSAGE "Fornecedor " emitente.cod-emitente  " possui Controle de CEI INATIVO!" SKIP
                   "Confirma?"
                   VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE l-cont-cei-1 AS LOG.
               IF l-cont-cei-1 = NO THEN RETURN NO-APPLY.
           
       END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME rs-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL input rs-tipo wWindow
ON LEAVE OF input rs-tipo IN FRAME fPage1 /* tipo de pedido */
DO:
    ASSIGN  rs-tipo.

    IF input rs-tipo = 0 THEN ASSIGN  rs-tipo = 1.

    IF input rs-tipo = 3  THEN DO:
        DISABLE nr-nfd serie rs-rendimento WITH FRAME {&FRAME-NAME}.
        ENABLE i-fornecedor c-it-codigo WITH FRAME {&FRAME-NAME}. 
        IF INPUT c-it-codigo = "0040" THEN do:
            MESSAGE "Item para Complemento de Preáo n∆o pode ser 0040!" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        
        IF l-motivo-compl = NO THEN DO: 
          UPDATE c-motivo-compl FORMAT "X(60)" WITH FRAME f-motiv NO-LABEL VIEW-AS DIALOG-BOX TITLE "Motivo Complemento Preáo".
          IF c-motivo-compl = "" THEN do:
            MESSAGE "Motivo do Complemento de Preáo deve ser Informado!" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
          END.
          ELSE ASSIGN l-motivo-compl = YES.
        END. 
    END.
    ELSE DO:

        FIND FIRST es-contrato-arroz NO-LOCK WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.
        IF  AVAIL es-contrato-arroz AND
            es-contrato-arroz.tipo-contrato <> 2 THEN DO:
            MESSAGE "Este Contrato n∆o se refere a Dep¢sito de Produto!" VIEW-AS ALERT-BOX ERROR.            
            RETURN NO-APPLY.
        END.
    
        ASSIGN c-motivo-compl = "".
        ENABLE nr-nfd serie rs-rendimento i-fornecedor WITH FRAME {&FRAME-NAME}.
        DISABLE i-fornecedor WITH FRAME {&FRAME-NAME}.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME i-fornecedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-fornecedor wWindow
ON LEAVE OF i-fornecedor IN FRAME fPage1 /* i-fornecedor */
DO:
    FIND emitente NO-LOCK
        WHERE emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} i-fornecedor NO-ERROR.
    IF AVAIL emitente THEN DO:
      ASSIGN fl-emitente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
      FIND FIRST es-contrato-arroz NO-LOCK WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.
      FIND b-mat-emitente NO-LOCK
        WHERE b-mat-emitente.cod-emitente = es-contrato-arroz.cod-emitente NO-ERROR.
      IF emitente.nome-matriz <> b-mat-emitente.nome-matriz THEN DO:
          MESSAGE "A Matriz do Contrato " b-mat-emitente.nome-matriz " n∆o est† equivaelnte com a Matriz do c¢digo informado!"
              VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
    END.
    ELSE DO:
        MESSAGE "Fornecedor Inv†lido!" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME de-preco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-preco wWindow
ON LEAVE OF de-preco IN FRAME fPage1 /* Preáo Tabela */
DO:
    IF AVAIL es-item THEN DO:
       ASSIGN de-vltotal = de-quantidade * (DEC(INPUT de-preco)).
       ASSIGN de-vltotal-2 = de-quantidade * (DEC(INPUT de-preco-2)).
       
       DISP de-vltotal
            de-vltotal-2
            WITH FRAME {&FRAME-NAME}.
    END.
    ASSIGN de-preco
           de-preco-salva = de-preco.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME rs-rendimento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-rendimento wWindow
ON VALUE-CHANGED OF rs-rendimento IN FRAME fPage1 /* rendimento */
DO:
    
   ASSIGN l-digitou-rend = YES.

    /* 031205 - inicio - interpretaá∆o tipo de rendimento no pedido - hist¢rico */
   FIND LAST es-hist-rend-ped WHERE
              es-hist-rend-ped.cod-estabel = INPUT fl-estabel AND
              es-hist-rend-ped.nr-contrato = INPUT i-contrato
        NO-LOCK USE-INDEX nr-contrato NO-ERROR.
   ASSIGN l-conf-rend = NO.

  
   IF AVAIL es-hist-rend-ped THEN DO:
       
       IF es-hist-rend-ped.tipo-rend <> INT(INPUT rs-rendimento) THEN DO:

         ASSIGN i-rend-hist = es-hist-rend-ped.tipo-rend
                c-usr-hist  = es-hist-rend-ped.usuario
                c-rend-hist = es-hist-rend-ped.rendimento.

         IF es-hist-rend-ped.tipo-rend = 1 THEN DO:  

            IF INT(INPUT rs-rendimento) = 3 THEN DO:   
                ASSIGN c-msg-tipo-rend = "O Rendimento utilizado na Liquidaá∆o anterior foi o RENDIMENTO MêDIO ENTRADAS" + chr(10) +
                                         "PorÇm nesta Liquidaá∆o foi escolhido Rendimento MÇdio Saldo! " + chr(10) +
                                         "CONFIRMA O RENDIMENTO ESCOLHIDO?".
            END. /* rendimento pelas entradas como o do es4021 */
                                                       

            IF INT(INPUT rs-rendimento) = 2 THEN DO:        
                ASSIGN c-msg-tipo-rend = "O Rendimento utilizado na Liquidaá∆o anterior foi o RENDIMENTO MêDIO ENTRADAS" + chr(10) +
                        "PorÇm nesta Liquidaá∆o foi escolhido Rendimento MÇdio Liquidaá∆o! " + chr(10) +
                        "CONFIRMA O RENDIMENTO ESCOLHIDO?".
            END. /* rend liquid */

         END. /* hist¢rico rendimento entradas */


         IF es-hist-rend-ped.tipo-rend = 2 THEN DO:  

            IF INT(INPUT rs-rendimento) = 1 THEN DO:        
                ASSIGN c-msg-tipo-rend = "O Rendimento utilizado na Liquidaá∆o anterior foi o RENDIMENTO MêDIO LIQUIDAÄ«O" + chr(10) +
                        "PorÇm nesta Liquidaá∆o foi escolhido Rendimento MÇdio Entrada! " + chr(10) +
                        "CONFIRMA O RENDIMENTO ESCOLHIDO?".
            END. /* rendimento pelas entradas como o do es4021 */

         
            IF INT(INPUT rs-rendimento) = 3 THEN DO:        
                ASSIGN c-msg-tipo-rend = "O Rendimento utilizado na Liquidaá∆o anterior foi o RENDIMENTO MêDIO LIQUIDAÄ«O" + chr(10) +
                        "PorÇm nesta Liquidaá∆o foi escolhido Rendimento MÇdio Saldo! " + chr(10) +
                        "CONFIRMA O RENDIMENTO ESCOLHIDO?".
            END. /* rend liquid */

         END. /* hist¢rico rendimento liquidacao  */



         IF es-hist-rend-ped.tipo-rend = 3 THEN DO:  

            IF INT(INPUT rs-rendimento) = 1 THEN DO:        
                ASSIGN c-msg-tipo-rend = "O Rendimento utilizado na Liquidaá∆o anterior foi o RENDIMENTO MêDIO SALDO" + chr(10) +
                        "PorÇm nesta Liquidaá∆o foi escolhido Rendimento MÇdio Entradas! " + chr(10) +
                        "CONFIRMA O RENDIMENTO ESCOLHIDO?".
            END. /* rendimento pelas entradas como o do es4021 */


            IF INT(INPUT rs-rendimento) = 2 THEN DO:        
                ASSIGN c-msg-tipo-rend = "O Rendimento utilizado na Liquidaá∆o anterior foi o RENDIMENTO MêDIO SALDO" + chr(10) +
                        "PorÇm nesta Liquidaá∆o foi escolhido Rendimento MÇdio Liquidaá∆o! " + chr(10) +
                        "CONFIRMA O RENDIMENTO ESCOLHIDO?".      
            END. /* rend liquid */

         END. /* hist¢rico rendimento saldo */

         MESSAGE c-msg-tipo-rend VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf-rend.
         IF l-conf-rend = NO THEN LEAVE.
         
       END. /* diferente */

   END. /* j† existe hist¢rico - n∆o Ç a primeira liquidaá∆o */
   /* fim- interpretaá∆o tipo de rendimento no pedido - hist¢rico */

    ASSIGN rs-rendimento.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME de-preco-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-preco-2 wWindow
ON LEAVE OF de-preco-2 IN FRAME fPage1 /* Preáo Unit†rio */
DO:
     IF AVAIL es-item THEN DO:
       ASSIGN de-vltotal = de-quantidade * (DEC(INPUT de-preco)).
       ASSIGN de-vltotal-2 = de-quantidade * (DEC(INPUT de-preco-2)).
       
       DISP de-vltotal
            de-vltotal-2
            WITH FRAME {&FRAME-NAME}.
    END.
    ASSIGN de-preco-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fl-condicao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl-condicao wWindow
ON F5 OF fl-condicao IN FRAME fPage1 /* Cond.Pagto */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad039.w
                       &campo=fl-condicao
                       &campozoom=cod-cond-pag
                       &FRAME=fPage1}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl-condicao wWindow
ON ENTRY OF fl-condicao IN FRAME fPage1 /* Cond.Pagto */
DO:
    /* para secagem coloca condiá∆o a vista direto 130110 */
    IF input rs-tipo = 2 THEN DO:
        DISP "100" @ fl-condicao WITH FRAME {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl-condicao wWindow
ON LEAVE OF fl-condicao IN FRAME fPage1 /* Cond.Pagto */
DO:
  FIND cond-pagto NO-LOCK
      WHERE cond-pagto.cod-cond-pag = INT(fl-condicao:SCREEN-VALUE IN FRAME fpage1)
      NO-ERROR.

  IF NOT AVAIL cond-pagto THEN DO:
      MESSAGE "Condiá∆o de Pagamento Inv†lida" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO fl-condicao IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
  END.
  ELSE
      ASSIGN dt-vencimento:SCREEN-VALUE IN FRAME fpage1 = STRING(TODAY + cond-pagto.prazos[1],"99/99/9999").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl-condicao wWindow
ON MOUSE-SELECT-DBLCLICK OF fl-condicao IN FRAME fPage1 /* Cond.Pagto */
DO:
  APPLY 'f5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fl-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl-estabel wWindow
ON ENTRY OF fl-estabel IN FRAME fPage1 /* Estabelecimento */
DO:
    ASSIGN l-digitou-rend = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fl-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl-estabel wWindow
ON LEAVE OF fl-estabel IN FRAME fPage1 /* Estabelecimento */
DO:
    IF input rs-tipo = 1 THEN DO:
        FIND LAST es-hist-rend-ped WHERE
                  es-hist-rend-ped.cod-estabel = INPUT fl-estabel AND
                  es-hist-rend-ped.nr-contrato = INPUT i-contrato
           NO-LOCK USE-INDEX nr-contrato NO-ERROR.
       IF AVAIL es-hist-rend-ped THEN DO:
           ASSIGN rs-rendimento = es-hist-rend-ped.tipo-rend.
           DISP rs-rendimento WITH FRAME {&FRAME-NAME}.
       END.
       ELSE DO:
           MESSAGE "Essa Ç a primeira liquidaá∆o deste contrato!" SKIP
               "Favor selecionar Tipo de Rendimento MÇdio!" VIEW-AS ALERT-BOX.    
       END.
    END. /* <> compl de preáo */
   ASSIGN fl-estabel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME nr-nfd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nr-nfd wWindow
ON F5 OF nr-nfd IN FRAME fPage1 /* Nr. NFD */
DO:
    FIND emitente NO-LOCK WHERE emitente.cod-emitente = i-fornecedor NO-ERROR.
    ASSIGN c-nome-matriz-gl = emitente.nome-matriz.
    {include/zoomvar.i &prog-zoom=eszoom/z10di135.w
                       &campo=nr-nfd
                       &campozoom=nr-nota-fis
                       &FRAME=fPage1}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nr-nfd wWindow
ON MOUSE-SELECT-DBLCLICK OF nr-nfd IN FRAME fPage1 /* Nr. NFD */
DO:
  APPLY 'f5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME serie
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL serie wWindow
ON F5 OF serie IN FRAME fPage1 /* SÇrie */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z03in407.w
                       &campo=serie
                       &campozoom=serie
                       &FRAME=fPage1}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL serie wWindow
ON MOUSE-SELECT-DBLCLICK OF serie IN FRAME fPage1 /* SÇrie */
DO:
  APPLY 'f5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME br_it-nota-fisc
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*--- L¢gica para inicializaá∆o do programam ---*/
nr-nfd:LOAD-MOUSE-POINTER("image\lupa.cur").
serie:LOAD-MOUSE-POINTER("image\lupa.cur").
fl-condicao:LOAD-MOUSE-POINTER("image\lupa.cur").

FIND FIRST es-contrato-arroz NO-LOCK
     WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.
  

ASSIGN i-contrato              = es-contrato-arroz.nr-contrato
       i-contrato:SCREEN-VALUE = string(i-contrato)
       i-contrato.
APPLY "ENTRY" TO nr-nfd IN FRAME fPage1.
{window/MainBlock.i} 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***8*******************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-pedido wWindow 
PROCEDURE pi-gera-pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR i-nr-pedido      LIKE pedido-compr.num-pedido       NO-UNDO.
DEF VAR i-prim-pedido    LIKE param-compra.num-pedido-ini   NO-UNDO. 
DEF VAR i-ult-pedido     LIKE param-compra.num-pedido-fim   NO-UNDO.
DEF VAR l-preco-bruto    AS LOGICAL INIT NO                 NO-UNDO.
DEF VAR l-modulo-ge      AS LOGICAL                         NO-UNDO.
DEF VAR r-gerencial      AS ROWID                           NO-UNDO.
DEF VAR h-acomp          AS HANDLE                          NO-UNDO.
DEF VAR c-nat-ext        AS CHAR FORMAT "x(14)"             NO-UNDO.
DEF VAR de-total         AS DEC NO-UNDO.
DEF VAR de-cdo-unit      AS DEC NO-UNDO.
DEF VAR de-funrural-unit AS DEC NO-UNDO.
DEF VAR de-senar-unit    AS DEC NO-UNDO.
DEF VAR c-tickets        AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR c-notas          AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR de-unit-ori      AS DEC NO-UNDO.
DEF VAR de-qtd-sel       AS DEC NO-UNDO.
DEF VAR de-cdo-unit-imp  AS DEC NO-UNDO.

DEF VAR i-empresa        LIKE param-global.empresa-prin     NO-UNDO.
{cdp/cdcfgdis.i}

DEFINE BUFFER b-ordem-compra FOR ordem-compra.
    
   
    /* colocando aqui somente a geraá∆o das tabelas */
    /* gera valores de desconto e bonificacao parciais aplicados para cada ticket */
    FOR EACH tt-qtde-ger:

        FIND FIRST tt-qtde WHERE
                   tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket
            NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-qtde THEN DO:
            MESSAGE "N∆o foi acionado o Bot∆o de Preáo!" VIEW-AS ALERT-BOX ERROR.
            LEAVE.
        END.

        FIND FIRST bf3-contrato WHERE
                   bf3-contrato.nr-contrato = es-contrato-arroz.nr-contrato NO-LOCK NO-ERROR.
        
        IF bf3-contrato.u-int-5 > 0 THEN do:
             ASSIGN de-grau-secagem = round((bf3-contrato.u-int-5 / 10000),4).
        END. /* u-int-5 */
        ELSE DO:
            FIND FIRST bf-es-ticket WHERE
                       bf-es-ticket.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK NO-ERROR.  


             RUN esp\esapi009.p (INPUT bf3-contrato.cod-estabel,
                                 INPUT tt-qtde-ger.nr-ticket,
                                 INPUT bf-es-ticket.dt-entrada,
                                 OUTPUT de-valor-formula).  
             
             ASSIGN de-grau-secagem = round((de-valor-formula / 100),4).
             
        END. /* u-log-2 */

      IF round(tt-qtde.qt-ori,2) = round(tt-qtde-ger.qt-ori,2) THEN DO:

            
            FIND FIRST tt-desc-bon WHERE
                 tt-desc-bon.nr-ticket = tt-qtde.nr-ticket
                NO-LOCK NO-ERROR.

            IF   rs-tipo <> 2 THEN DO:

                IF de-grau-secagem = 0 THEN DO:
                          
                    FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                    FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                    FIND FIRST es-aplic-desc-bon WHERE
                               es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                    IF NOT AVAIL es-aplic-desc-bon THEN DO:
                        CREATE es-aplic-desc-bon.
                        ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket.
                    END.
                    ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0
                           es-aplic-desc-bon.qt-bon  = es-aplic-desc-bon.qt-bon + IF AVAIL tt-bonifica THEN tt-bonifica.quantidade ELSE 0.

                END. /* n∆o tem secagem */
                ELSE DO:
                    FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                    FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                    FIND FIRST es-aplic-desc-bon WHERE
                               es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                    IF NOT AVAIL es-aplic-desc-bon THEN DO:
                        CREATE es-aplic-desc-bon.
                        ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket.
                    END.

                    ASSIGN de-sec-aplic-desc = 0
                           de-sec-aplic-bon  = 0.

                    FOR EACH es-aplic-sec-desc-bon WHERE 
                             es-aplic-sec-desc-bon.nr-ticket = tt-qtde.nr-ticket:
                        ASSIGN de-sec-aplic-desc = de-sec-aplic-desc + es-aplic-sec-desc-bon.qt-desc  
                               de-sec-aplic-bon  = de-sec-aplic-bon  + es-aplic-sec-desc-bon.qt-bon.
                    END.

                    /* se iguais j† aplicou todo o dsc do ticket */
                    IF round(de-val-ori-desc * de-grau-secagem,4) = de-sec-aplic-desc THEN DO:
                      ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                        IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de desconto para esse ticket */
                        IF de-sec-aplic-desc = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND((tt-desc-bon.qt-des - ROUND((tt-desc-bon.qt-des * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND(tt-desc-bon.qt-des  - de-sec-aplic-desc,4) 
                                                                      ELSE 0. 
                        END.
                    END.

                    /* se iguais j† aplicou toda bon do ticket */
                    IF round(de-val-ori-bon * de-grau-secagem,4) = de-sec-aplic-bon THEN DO:
                      ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                                        IF AVAIL tt-bonifica THEN tt-bonifica.quantidad ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de bon para esse ticket */
                        IF de-sec-aplic-bon = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                                 IF AVAIL tt-bonifica THEN ROUND((tt-bonifica.quantidad - ROUND((tt-bonifica.quantidad * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                           IF AVAIL tt-bonifica THEN ROUND(tt-bonifica.quantidad - de-sec-aplic-bon,4) 
                                                                      ELSE 0. 
                        END.
                    END.

                    IF es-aplic-desc-bon.qt-desc < 0 THEN ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc * -1. /* para no caso de liquidar tudo que d† negativo */
                    IF es-aplic-desc-bon.qt-bon  < 0 THEN ASSIGN es-aplic-desc-bon.qt-bon  = es-aplic-desc-bon.qt-bon  * -1.

                END. /* tem secagem */
                                   
                /* nova tabela de bon/desc proporcionais por pedido */
                CREATE tt-aplic-desc-bon.
                BUFFER-COPY es-aplic-desc-bon TO tt-aplic-desc-bon.

            END. /* input rs-tipo <> 2 */


            /******* mÇdia ponderada - cria tabela de percetual do ticket utilizado *******/
            FIND FIRST es-ticket-usado WHERE es-ticket-usado.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL es-ticket-usado THEN DO:
                CREATE es-ticket-usado.
                ASSIGN es-ticket-usado.nr-ticket = tt-qtde.nr-ticket.
            END.
            ASSIGN es-ticket-usado.perc = 100.

      END. /* quantidade original = qtde negociada na liqu */

      ELSE DO:      

            FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.

            FIND tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

            IF   rs-tipo <> 2 THEN DO:
                  IF de-grau-secagem = 0 THEN DO:  
                      FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                      FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                      FIND FIRST es-aplic-desc-bon WHERE
                                  es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                      IF NOT AVAIL es-aplic-desc-bon THEN DO:                      
                          CREATE es-aplic-desc-bon.
                          ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket.
                      END.
                      ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0
                             es-aplic-desc-bon.qt-bon  = es-aplic-desc-bon.qt-bon  + IF AVAIL tt-bonifica THEN tt-bonifica.quantidade ELSE 0. /* tt-desc-bon.qt-bon*/                                                                                                                                          

                  END. /* n∆o tem secagem */
                  ELSE DO:
                      FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                      FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                      FIND FIRST es-aplic-desc-bon WHERE
                                  es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                      IF NOT AVAIL es-aplic-desc-bon THEN DO:
                        CREATE es-aplic-desc-bon.
                        ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket.
                      END.
                    ASSIGN de-sec-aplic-desc = 0
                           de-sec-aplic-bon  = 0.

                    FOR EACH es-aplic-sec-desc-bon WHERE 
                             es-aplic-sec-desc-bon.nr-ticket = tt-qtde.nr-ticket:
                        ASSIGN de-sec-aplic-desc = de-sec-aplic-desc + es-aplic-sec-desc-bon.qt-desc  
                               de-sec-aplic-bon  = de-sec-aplic-bon  + es-aplic-sec-desc-bon.qt-bon.
                    END.

                    /* se iguais j† aplicou todo o dsc do ticket */
                    IF round(de-val-ori-desc * de-grau-secagem,4) = de-sec-aplic-desc THEN DO:
                      ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                        IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de desconto para esse ticket */
                        IF de-sec-aplic-desc = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND((tt-desc-bon.qt-des - ROUND((tt-desc-bon.qt-des * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND(tt-desc-bon.qt-des  - de-sec-aplic-desc,4) 
                                                                      ELSE 0. 
                        END.
                    END.
                    /* se iguais j† aplicou toda bon do ticket */
                    IF round(de-val-ori-bon * de-grau-secagem,4) = de-sec-aplic-bon THEN DO:
                      ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                                        IF AVAIL tt-bonifica THEN tt-bonifica.quantidad ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de bon para esse ticket */
                        IF de-sec-aplic-bon = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                                 IF AVAIL tt-bonifica THEN ROUND((tt-bonifica.quantidad - ROUND((tt-bonifica.quantidad * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                           IF AVAIL tt-bonifica THEN ROUND(tt-bonifica.quantidad - de-sec-aplic-bon,4) 
                                                                      ELSE 0. 
                        END.
                    END.
                    IF es-aplic-desc-bon.qt-desc < 0 THEN ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc * -1. /* para no caso de liquidar tudo que d† negativo */
                    IF es-aplic-desc-bon.qt-bon  < 0 THEN ASSIGN es-aplic-desc-bon.qt-bon  = es-aplic-desc-bon.qt-bon  * -1.

                   END. /* tem secagem */
                                                                                                                 
              
               /* nova tabela de bon/desc proporcionais por pedido */
                CREATE tt-aplic-desc-bon.
                BUFFER-COPY es-aplic-desc-bon TO tt-aplic-desc-bon.

                /******* mÇdia ponderada - cria tabela de percetual do ticket utilizado *******/
            
                FIND FIRST es-ticket-usado WHERE es-ticket-usado.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAIL es-ticket-usado THEN DO:
                    CREATE es-ticket-usado.
                    ASSIGN es-ticket-usado.nr-ticket = tt-qtde.nr-ticket.
                END.
                ASSIGN es-ticket-usado.perc = es-ticket-usado.perc + de-perc.
                
            END. /* input rs-tipo <> 2 */
           

            /****** mÇdia *******/
           
      END. /* qtde original difere da negociada na liq */
        /* neste ponto a tabela especifica j† est† gerada assim como o percentual */

    END. /* for each tt-qtde-ger */

               
FIND FIRST nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel = INPUT FRAME fPage1 fl-estabel
      AND nota-fiscal.serie       = INPUT FRAME fPage1 serie
      AND nota-fiscal.nr-nota-fis = INPUT FRAME fPage1 nr-nfd
    NO-ERROR.
IF NOT AVAIL nota-fiscal AND input rs-tipo <> 3 THEN DO:
    MESSAGE "Nota de Devoluá∆o n∆o encontrada!" VIEW-AS ALERT-BOX ERROR.
    ASSIGN l-botao-atualiza = NO.
    APPLY "ENTRY" TO nr-nfd IN FRAME fPage1.
    RETURN NO-APPLY.
END.
IF input rs-tipo <> 3 THEN DO:
    
    
    IF nota-fiscal.esp-docto <> 22 THEN DO: /* NFD */
        MESSAGE "Nota Fiscal n∆o Ç de Devoluá∆o de Mercadoria" VIEW-AS ALERT-BOX ERROR.
        ASSIGN l-botao-atualiza = NO.
        APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
        RETURN NO-APPLY.
    END.
    
    IF input rs-tipo = 2 THEN DO:
        IF SUBSTR(nota-fiscal.observ-nota,1900,7) <> "SECAGEM" THEN DO:
            MESSAGE "Nota Fiscal n∆o foi gerada como secagem!" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
            RETURN NO-APPLY.
        END.
    END.
    
    IF input rs-tipo = 1 THEN DO:
        IF SUBSTR(nota-fiscal.observ-nota,1900,7) = "SECAGEM" THEN DO:
            MESSAGE "Nota Fiscal n∆o foi gerada como de arroz a dep¢sito!" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
            RETURN NO-APPLY.
        END.
    END.
    
    FIND natur-oper OF nota-fiscal
        NO-LOCK NO-ERROR.
    IF natur-oper.terceiros <> YES or
       natur-oper.tp-oper-terc <> 5 THEN DO:
        MESSAGE "Natureza da Nota n∆o Ç de Devoluá∆o de Mercadoria em Consignaá∆o" VIEW-AS ALERT-BOX ERROR.
        ASSIGN l-botao-atualiza = NO.
        APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
        RETURN NO-APPLY.
    END.
    
    
END. /* <> complemento */


ASSIGN de-devol = 0.



IF input rs-tipo <> 2  THEN ASSIGN de-preco = de-preco-2.
IF input rs-tipo = 2 THEN ASSIGN de-preco = de-preco-3.
 de-unit-ori = de-preco /* 100309 novo */.

IF input rs-tipo = 3 THEN de-devol = DEC(de-quantidade:SCREEN-VALUE IN FRAME fPage1) /* complemento n∆o tem nota */.

FOR EACH it-nota-fisc NO-LOCK OF nota-fiscal.
    ASSIGN de-devol = de-devol + it-nota-fisc.qt-faturada[1].
END.

IF DEC(de-quantidade:SCREEN-VALUE IN FRAME fPage1) > de-devol /* pSaldoDisp*/ THEN DO:
    MESSAGE "A quantidade informada " DEC(de-quantidade:SCREEN-VALUE IN FRAME fPage1)
                    " n∆o pode ser superior ao" SKIP
            "saldo dispon°vel!" SKIP(1)
            "Saldo Disp.(Devoluá∆o em KG): " de-devol VIEW-AS ALERT-BOX WARNING TITLE "Atená∆o".
    ASSIGN l-botao-atualiza = NO.
    APPLY 'ENTRY':U TO de-quantidade IN FRAME fPage1.
    RETURN NO-APPLY.
END.

IF DEC(de-preco:SCREEN-VALUE IN FRAME fPage1) = 0 THEN DO:
    MESSAGE "O Preáo Fornecedor deve ser infomado!" VIEW-AS ALERT-BOX WARNING TITLE "Atená∆o".
    ASSIGN l-botao-atualiza = NO.
    APPLY 'ENTRY':U TO de-preco IN FRAME fPage1.
    RETURN NO-APPLY.
END.

IF DEC(de-quantidade:SCREEN-VALUE IN FRAME fPage1) = 0 THEN DO:
    MESSAGE "N∆o existe dados para a Compra!" VIEW-AS ALERT-BOX WARNING TITLE "Atená∆o".
    ASSIGN l-botao-atualiza = NO.
    APPLY 'ENTRY':U TO de-preco IN FRAME fPage1.
    RETURN NO-APPLY.
END.

FIND FIRST cond-pagto NO-LOCK
    WHERE cond-pagto.cod-cond-pag = INT(fl-condicao:SCREEN-VALUE IN FRAME fpage1)
    NO-ERROR.

IF NOT AVAIL cond-pagto THEN DO:
    MESSAGE "Condiá∆o de Pagamento Inv†lida" VIEW-AS ALERT-BOX ERROR.
    ASSIGN l-botao-atualiza = NO.
    APPLY "ENTRY" TO fl-condicao IN FRAME fpage1.
    RETURN NO-APPLY.
END.

IF input rs-tipo <> 3 AND nota-fiscal.nr-proc-exp <> "" THEN DO:
    MESSAGE "Esta Nota j† pertence ao Contrato " nota-fiscal.nr-proc-exp VIEW-AS ALERT-BOX ERROR.
    ASSIGN l-botao-atualiza = NO.
    APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
    RETURN NO-APPLY.
END.

/** Validada quantidade selecionanda **/


IF AVAIL es-contrato-arroz AND es-contrato-arroz.u-log-1 = FALSE THEN DO:

    FOR EACH es-movto-ticket
       WHERE es-movto-ticket.nr-contrato = es-contrato-arroz.nr-contrato
         AND es-movto-ticket.u-log-2     = FALSE
        EXCLUSIVE-LOCK:
    
       de-qtd-sel = de-qtd-sel + es-movto-ticket.qtd-saldo-ticket.
    
    END.
    
    IF de-qtd-sel <> INPUT FRAME fpage1 de-quantidade THEN DO:
    
       MESSAGE "Quantidade de carga selecionada diferente da quantidade solicitada." VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO nr-nfd IN FRAME fpage1.
       RETURN NO-APPLY.
    
    END.

END.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Geracao de Pedido de Compra").
PAUSE 1.

FIND FIRST param-global NO-LOCK NO-ERROR.


DO TRANSACTION:

/** Confirma qtde selecionada **/

FOR EACH es-movto-ticket
   WHERE es-movto-ticket.nr-contrato = es-contrato-arroz.nr-contrato
     AND es-movto-ticket.u-log-2     = FALSE EXCLUSIVE-LOCK:
  ASSIGN es-movto-ticket.u-log-2 = TRUE.                    
END.

FIND FIRST es-saldo-arroz NO-LOCK WHERE ROWID(es-saldo-arroz) = pRowSaldo NO-ERROR.

FIND FIRST es-param-empresa NO-LOCK WHERE es-param-empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.

IF AVAIL es-saldo-arroz THEN DO WITH FRAME fPage1:
    
      FIND LAST pedido-compr 
            WHERE pedido-compr.num-pedido <= i-ult-pedido NO-LOCK NO-ERROR.
        
      IF AVAIL pedido-compr THEN
          IF (pedido-compr.num-pedido + 1) > i-ult-pedido OR
             (pedido-compr.num-pedido + 1) < i-prim-pedido THEN
              ASSIGN i-nr-pedido = i-prim-pedido.
          ELSE ASSIGN i-nr-pedido = pedido-compr.num-pedido + 1.
      ELSE ASSIGN i-nr-pedido = i-prim-pedido.
        
      IF i-nr-pedido = 0 THEN ASSIGN i-nr-pedido = 1.  
        
      FIND pedido-compr USE-INDEX numero
            WHERE pedido-compr.num-pedido = i-nr-pedido NO-LOCK NO-ERROR.
      DO WHILE AVAIL pedido-compr:
            ASSIGN i-nr-pedido = i-nr-pedido + 1.
            FIND pedido-compr USE-INDEX numero WHERE pedido-compr.num-pedido = i-nr-pedido NO-LOCK NO-ERROR.
      END.
                                  
      IF param-global.modulo-bh THEN DO:
            FIND LAST his-pedido-compr NO-LOCK NO-ERROR.
            IF AVAIL his-pedido-compr
                AND his-pedido-compr.num-pedido + 1 > i-nr-pedido THEN
                ASSIGN i-nr-pedido = (his-pedido-compr.num-pedido + 1).
      END.        
    
      FIND FIRST emitente NO-LOCK WHERE emitente.nome-abrev = es-saldo-arroz.nome-matriz NO-ERROR.
      IF NOT AVAIL emitente THEN DO:
            MESSAGE "N∆o existe emitente para" es-saldo-arroz.nome-matriz skip
                "Matriz do Saldo de Arroz ERRADA!" VIEW-AS ALERT-BOX ERROR.
      END.
    
      IF input rs-tipo <> 3 THEN DO:
          

        FOR EACH it-nota-fisc NO-LOCK OF nota-fiscal BREAK BY it-nota-fisc.it-codigo:

            
            FIND tt-it-nota WHERE
                 tt-it-nota.it-codigo = it-nota-fisc.it-codigo
                NO-LOCK NO-ERROR.
            IF NOT avail tt-it-nota THEN DO:
                CREATE tt-it-nota.
                ASSIGN tt-it-nota.it-codigo   = it-nota-fisc.it-codigo
                       tt-it-nota.cod-estabel = it-nota-fisc.cod-estabel
                       tt-it-nota.nr-docum    = it-nota-fisc.nr-docum
                       tt-it-nota.serie       = it-nota-fisc.serie.               

            END.
            ASSIGN tt-it-nota.qt-faturada[1] = tt-it-nota.qt-faturada[1] + it-nota-fisc.qt-faturada[1]. 
    
        END. /* for each it-nota-fis */
        
        FOR EACH tt-it-nota BY tt-it-nota.it-codigo:            
        
            FIND es-param-estab NO-LOCK WHERE es-param-estab.cod-estabel = nota-fiscal.cod-estabel.
               
            FIND LAST b-ordem-compra USE-INDEX item NO-LOCK
                WHERE b-ordem-compra.it-codigo = tt-it-nota.it-codigo NO-ERROR.
        
            FIND FIRST item NO-LOCK WHERE item.it-codigo = tt-it-nota.it-codigo NO-ERROR.
        
            FIND FIRST es-item NO-LOCK WHERE es-item.it-codigo = ITEM.it-codigo NO-ERROR.
            
            FIND FIRST mgadm.banco WHERE banco.cod-banco = b-ins-emitente.cod-banco NO-LOCK NO-ERROR.
            IF NOT AVAIL banco THEN do:
                MESSAGE  "Esse fornecedor n∆o tem um C¢digo de Banco v†lido em seu cadastro!" skip
                         "Permite gerar Pedido SEM Informaá‰es Banc†rias?" view-as alert-box QUESTION BUTTONS YES-NO UPDATE l-sem-inf AS LOG.
                IF l-sem-inf = NO THEN LEAVE.
            END.

            FIND FIRST param-compra NO-LOCK NO-ERROR.
            IF AVAIL param-compra THEN DO:
                  ASSIGN l-preco-bruto = (IF param-compra.ipi-sobre-preco = 1 THEN YES ELSE NO)
                         i-prim-pedido = param-compra.num-pedido-ini 
                         i-ult-pedido  = param-compra.num-pedido-fim.
            END.
            ELSE ASSIGN l-preco-bruto = NO.      
            /**********
            assign i-ord-aux = param-compra.ult-ord-man * 100. 
        
            find last ordem-compra where ordem-compra.numero-ordem <> 0 and
                 ordem-compra.numero-ordem <= i-ord-aux no-lock no-error.
            if  available ordem-compra then do:  
                assign i-ord-aux = truncate(ordem-compra.numero-ordem / 100 , 0) + 1.
                
                if avail param-global and param-global.modulo-bh then do:
                find last his-ord-compra no-lock where
                      his-ord-compra.numero-ordem <= param-compra.ult-ord-man * 100 no-error.
                if avail his-ord-compra and his-ord-compra.numero-ordem > ordem-compra.numero-ordem then
                    assign i-ord-aux = truncate(his-ord-compra.numero-ordem / 100,0) + 1.
                /* Usado para bancos historicos */
                /* Valida o ultimo numero da ordem de compra */
                end.
                
                if i-ord-aux >= truncate(ordem-compra.numero-ordem / 100 , 0) + 1 then
                    assign i-ord-aux = (if i-ord-aux > param-compra.ult-ord-man
                                        or i-ord-aux < param-compra.prim-ord-man
                                       then param-compra.prim-ord-man else i-ord-aux). 
                else assign i-ord-aux = i-ordem.                
            end.
            ELSE assign i-ord-aux = param-compra.prim-ord-man. 
            assign i-ordem = i-ord-aux * 100. 
            *************/

            ASSIGN i-ord-aux = 0.

            RUN ccp/ccapi333.p (INPUT  0,
                                OUTPUT i-ord-aux).

            assign i-ordem = i-ord-aux.



           
            IF l-prim THEN DO:
               IF input rs-tipo <> 2 THEN
                  ASSIGN de-total         = de-quantidade * DEC(de-preco:SCREEN-VALUE)
                         de-cdo-unit      = 0
                         de-funrural-unit = 0
                         de-senar-unit    = 0
                         l-prim           = NO.
               ELSE
                   ASSIGN de-total         = de-quantidade * de-preco
                         de-cdo-unit      = 0
                         de-funrural-unit = 0
                         de-senar-unit    = 0
                         l-prim           = NO.


                
                IF es-item.tem-cdo             AND
                   emitente.estado     = "RS"  AND
                  (emitente.natureza   = 1     OR emitente.natureza = 4) THEN DO:
                   ASSIGN de-cdo-unit = ROUND(es-param-empresa.vl-cdo * (de-quantidade / es-item.unidade-saco),2). /* alt round de 2 para 1  -04/11/12 - voltei 12/11/12 */
                END.
    
                IF es-item.tem-funrural                                 AND 
                  (emitente.natureza  = 1     OR emitente.natureza = 4) THEN DO:

                    /***************
                    ASSIGN de-funrural-unit    = ROUND(((de-total - 0) /
                                                 ((100 - (es-param-empresa.vl-funrural-1 +
                                                          es-param-empresa.vl-funrural-2 +
                                                          es-param-empresa.vl-funrural-3)) / 100)) - de-total,2). /* alt round de 2 para 1  -04/11/12 - voltei 12/11/12 */
                   /* senar 16/06/2010 */
                    ASSIGN de-senar-unit = ROUND(((de-total - 0) / ((100 - (es-param-empresa.vl-senar)) / 100)) - de-total,2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
                    ***************/
 
                    ASSIGN de-funrural-unit    = ROUND((de-total  - de-cdo-unit) *
                                                       ((es-param-empresa.vl-funrural-1 +
                                                        es-param-empresa.vl-funrural-2 +
                                                        es-param-empresa.vl-funrural-3) / 100),2). /* alt round de 2 para 1  -04/11/12 - voltei 12/11/12 */
                    /* senar 16/06/2010 */
                    ASSIGN de-senar-unit = ROUND(((de-total - de-cdo-unit) * (es-param-empresa.vl-senar / 100)),2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
 
                END.

                
                
                IF de-cdo-unit > 0 THEN DO:
    
                    IF input rs-tipo <> 2 THEN ASSIGN de-total = de-vltotal-2 - de-cdo-unit.
                                   ELSE ASSIGN de-total = de-vltotal-3 - de-cdo-unit.

                                   
    
                    ASSIGN de-preco = de-total / de-quantidade
/*                            c-preco = STRING(de-preco, ">>>>>>>>9.99999") */
/*                            de-preco = DEC(c-preco)                       */
                           de-preco:SCREEN-VALUE   = STRING(de-preco,"ZZZZZZ9.9999")
                           de-vltotal:SCREEN-VALUE = STRING((de-preco * de-quantidade),"ZZZ,ZZZ,ZZ9.99").

                           
                END. /* > 0 */
            END. /* l-prim */
           
           /******************
            IF de-funrural-unit <> 0 THEN DO:

                IF de-cdo-unit <> 0 OR de-senar-unit <> 0 THEN
                    MESSAGE "Preáo Unit†rio e Total foram ajustados devido ao CEI, CDO e SENAR de Nota Normal ou Secagem!" VIEW-AS ALERT-BOX INFORMATION.
    
                IF de-cdo-unit = 0 AND de-senar-unit = 0 THEN
                    MESSAGE "Preáo Unit†rio e Total n∆o foram ajustados, sem CEI, CDO e SENAR, para Nota Normal ou Secagem!" VIEW-AS ALERT-BOX INFORMATION. 
            END.
            ELSE DO:
                IF de-cdo-unit <> 0 OR de-senar-unit <> 0 THEN
                    MESSAGE "Preáo Unit†rio e Total foram ajustados devido CDO e SENAR de Nota Normal ou Secagem!" VIEW-AS ALERT-BOX INFORMATION.
    
                IF de-cdo-unit = 0 AND de-senar-unit = 0 THEN
                    MESSAGE "Preáo Unit†rio e Total n∆o foram ajustados, sem CDO e SENAR, para Nota Normal ou Secagem!" VIEW-AS ALERT-BOX INFORMATION. 

            END.
            ******************/

            IF de-cdo-unit <> 0  THEN
                MESSAGE "Preáo Unit†rio foi ajustado para compor a retená∆o de impostos!" VIEW-AS ALERT-BOX INFORMATION.
    
            IF de-cdo-unit = 0 THEN
                    MESSAGE "Preáo Unit†rio foi ajustado para compor a retená∆o de impostos!" VIEW-AS ALERT-BOX INFORMATION. 

            /* 2355743 */
            ASSIGN de-cdo-unit-imp = de-cdo-unit.
           
            ASSIGN DE-CDO-UNIT = 0 . /* VALOR CDO ZERADO EM 04/17/2017 - MUDANÄA CµLCULO */
 
           
            /* normal */
            CREATE ordem-compra.
            ASSIGN ordem-compra.numero-ordem = i-ordem
                   ordem-compra.cod-emitente = emitente.cod-emitente
                   ordem-compra.it-codigo    = IF input rs-tipo = 3 THEN INPUT c-it-codigo ELSE tt-it-nota.it-codigo
                   ordem-compra.natureza     = 1 /* Compra     */
                   ordem-compra.situacao     = 2 /* Confirmada */
                   ordem-compra.cod-estabel  = IF input rs-tipo = 3 THEN es-contrato-arroz.cod-estabel ELSE nota-fiscal.cod-estabel
                   ordem-compra.num-pedido   = i-nr-pedido
                   ordem-compra.data-pedido  = TODAY
                   ordem-compra.qt-solic     = IF input rs-tipo = 3 THEN 1 ELSE tt-it-nota.qt-faturada[1]
                   ordem-compra.aliquota-ipi = 0
                   ordem-compra.preco-fornec = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                   ordem-compra.preco-orig   = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                   ordem-compra.preco-unit   = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                   ordem-compra.pre-unit-for = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                   ordem-compra.cod-comprado = c-seg-usuario
                   ordem-compra.int-1        = es-saldo-arroz.nr-contrato
                   ordem-compra.dep          = es-param-estab.cod-dep-compra
                   ordem-compra.requisitante = c-seg-usuario
                   ordem-compra.tp-despesa   = IF input rs-tipo = 3 THEN 600 ELSE ITEM.tp-desp-padrao.
                    
        
            ASSIGN de-quant-scs    = de-quant-scs + ordem-compra.qt-solic.
            IF c-it-codigo-ped = "" THEN ASSIGN c-it-codigo-ped = ordem-compra.it-codigo.
                                    ELSE ASSIGN c-it-codigo-ped = c-it-codigo-ped + " - " + ordem-compra.it-codigo. 
            
            RUN pi-acompanhar IN h-acomp (INPUT STRING(ordem-compra.numero-ordem)).
            
            FOR EACH unid-neg-fam NO-LOCK
                 WHERE unid-neg-fam.fm-codigo = ITEM.fm-codigo:
            
                 CREATE unid-neg-ordem.
                 ASSIGN unid-neg-ordem.cod_unid_negoc   = unid-neg-fam.cod_unid_negoc
                        unid-neg-ordem.numero-ordem     = ordem-compra.numero-ordem
                        unid-neg-ordem.perc-unid-neg    = unid-neg-fam.perc-unid-neg
                        unid-neg-ordem.qtd-unid-neg     = 0
                        unid-neg-ordem.val-unid-neg[1]  = 0
                        unid-neg-ordem.val-unid-neg[2]  = 0.
            END.
               
            CREATE prazo-compra.
            ASSIGN prazo-compra.numero-ordem = ordem-compra.numero-ordem
                   prazo-compra.situacao     = ordem-compra.situacao
                   prazo-compra.parcela      = 1
                   prazo-compra.it-codigo    = IF input rs-tipo = 3 THEN INPUT c-it-codigo ELSE tt-it-nota.it-codigo
                   prazo-compra.un           = IF input rs-tipo = 3 THEN "kg" ELSE item.un
                   prazo-compra.quantid-orig = ordem-compra.qt-solic
                   prazo-compra.quantidade   = ordem-compra.qt-solic
                   prazo-compra.quant-saldo  = ordem-compra.qt-solic
                   prazo-compra.qtd-do-forn  = ordem-compra.qt-solic
                   prazo-compra.qtd-sal-forn = ordem-compra.qt-solic
                   prazo-compra.data-orig    = TODAY
                   prazo-compra.data-entrega = TODAY.

        
            CREATE cotacao-item.
            ASSIGN cotacao-item.it-codigo    = ordem-compra.it-codigo
                   cotacao-item.cod-emitente = ordem-compra.cod-emitente
                   cotacao-item.numero-ordem = ordem-compra.numero-ordem
                   cotacao-item.seq-cot      = 1
                   cotacao-item.data-cotacao = TODAY
                   cotacao-item.un           = IF input rs-tipo = 3 THEN "kg" ELSE item.un
                   cotacao-item.preco-fornec = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                   cotacao-item.preco-unit   = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                   cotacao-item.pre-unit-for = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                   cotacao-item.dec-1        = de-unit-ori
                   cotacao-item.cod-comprado = ordem-compra.cod-comprado
                   cotacao-item.aprovador    = c-seg-usuario
                   cotacao-item.usuario      = c-seg-usuario
                   cotacao-item.data-atualiz = TODAY
                   cotacao-item.hora-atualiz = STRING(TIME,"HH:MM:SS")
                   cotacao-item.cot-aprovada = YES
                   cotacao-item.aliquota-ipi = 0.


            
            ASSIGN c-notas   = ""
                   c-tickets = "".
        
            IF input rs-tipo <> 3 THEN DO:
                FIND FIRST es-ticket WHERE 
                           es-ticket.cod-estabel  = tt-it-nota.cod-estabel AND 
                           es-ticket.nro-docto    = tt-it-nota.nr-docum    AND 
                           es-ticket.serie        >= "" /*tt-it-nota.serie */      AND 
                           es-ticket.cod-produtor = b1-emitente.cod-emitente NO-LOCK NO-ERROR.
                

                 IF AVAIL es-ticket THEN DO:
                     
                    IF c-notas = "" THEN
                        ASSIGN c-notas = string(es-ticket.nr-nota-fornec,"9999999").
                    ELSE
                        ASSIGN c-notas = c-notas + "," + string(es-ticket.nr-nota-fornec,"9999999").
            
                    IF c-tickets = "" THEN
                        ASSIGN c-tickets = string(es-ticket.nr-ticket,"999999999").
                    ELSE
                        ASSIGN c-tickets = c-tickets + "," + string(es-ticket.nr-ticket,"999999999").
                END.
    
            
                FIND FIRST item NO-LOCK
                     WHERE item.it-codigo = ordem-compra.it-codigo no-error.
                RUN ccp/ccapi001.p (INPUT ROWID(ordem-compra)). /* Calculo do preco para reposicao */
            END. 
            
        END. /* for each tt-it-nota */ 
            
      END. /* <> 3 - n∆o Ç complemento de preáo */


      
      IF input rs-tipo = 3 THEN DO:

          /*******************
          ASSIGN de-total         = de-quantidade * de-preco
                 de-cdo-unit      = 0
                 de-funrural-unit = 0
                 de-senar-unit    = 0.

           IF (emitente.natureza  = 1 OR emitente.natureza = 4) THEN DO:

               
                ASSIGN de-funrural-unit = ROUND(((de-total - 0) /
                                         ((100 - (es-param-empresa.vl-funrural-1 +
                                                  es-param-empresa.vl-funrural-2 +
                                                  es-param-empresa.vl-funrural-3)) / 100)) - de-total,2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */


                IF de-funrural-unit > 0 THEN DO:

                   
                  ASSIGN de-total = de-vltotal-2 + de-funrural-unit
                         de-preco = de-total / de-quantidade
                         de-preco:SCREEN-VALUE   = STRING(de-preco,"ZZZZZZ9.99999") /* erika 09/11/12 5 casas */
                         de-vltotal:SCREEN-VALUE = STRING((de-preco * de-quantidade),"ZZZ,ZZZ,ZZ9.99").
                   


                END. /* fr > 0 */
           END. /* emitente natureza */
          ****************/

          ASSIGN de-total         = de-quantidade * de-preco
                 de-cdo-unit      = 0.
                
          IF (emitente.natureza  = 1 OR emitente.natureza = 4) THEN DO:
                   
                 ASSIGN de-total = de-vltotal-2 
                        de-preco = de-total / de-quantidade
                        de-preco:SCREEN-VALUE   = STRING(de-preco,"ZZZZZZ9.99999") /* erika 09/11/12 5 casas */
                        de-vltotal:SCREEN-VALUE = STRING((de-preco * de-quantidade),"ZZZ,ZZZ,ZZ9.99").
               
          END. /* emitente natureza */
 
          /**************
          /* senar */
          ASSIGN de-senar-unit = ROUND(((de-total - 0) /
                                         ((100 - (es-param-empresa.vl-senar)) / 100)) - de-total,2). /* alt round de 2 para 1  -04/11/12 - voltei 12/11/12 */
          **************/  

          FIND FIRST param-compra NO-LOCK NO-ERROR.
          IF AVAIL param-compra THEN DO:
                ASSIGN l-preco-bruto = (IF param-compra.ipi-sobre-preco = 1 THEN YES ELSE NO)
                       i-prim-pedido = param-compra.num-pedido-ini 
                       i-ult-pedido  = param-compra.num-pedido-fim.
          END.
          ELSE ASSIGN l-preco-bruto = NO.      
        
          /*assign i-ord-aux = param-compra.ult-ord-man * 100. 
    
          find last ordem-compra where ordem-compra.numero-ordem <> 0 and
               ordem-compra.numero-ordem <= i-ord-aux no-lock no-error.
          if  available ordem-compra then do:  
              assign i-ord-aux = truncate(ordem-compra.numero-ordem / 100 , 0) + 1.
              
              if avail param-global and param-global.modulo-bh then do:
              find last his-ord-compra no-lock where
                    his-ord-compra.numero-ordem <= param-compra.ult-ord-man * 100 no-error.
              if avail his-ord-compra and his-ord-compra.numero-ordem > ordem-compra.numero-ordem then
                  assign i-ord-aux = truncate(his-ord-compra.numero-ordem / 100,0) + 1.
              /* Usado para bancos historicos */
              /* Valida o ultimo numero da ordem de compra */
              end.
              
              if i-ord-aux >= truncate(ordem-compra.numero-ordem / 100 , 0) + 1 then
                  assign i-ord-aux = (if i-ord-aux > param-compra.ult-ord-man
                                      or i-ord-aux < param-compra.prim-ord-man
                                     then param-compra.prim-ord-man else i-ord-aux). 
              else assign i-ord-aux = i-ordem.                
          end.
          ELSE assign i-ord-aux = param-compra.prim-ord-man. 
          assign i-ordem = i-ord-aux * 100. */

          ASSIGN i-ord-aux = 0.

            RUN ccp/ccapi333.p (INPUT  0,
                                OUTPUT i-ord-aux).

            assign i-ordem = i-ord-aux.


          /* 21042010 isená∆o fr */
          /************
          IF de-cdo-unit <> 0 OR de-funrural-unit <> 0 OR de-senar-unit <> 0 THEN
                 MESSAGE "Preáo Unit†rio e Total foram ajustados devido ao CDO e SENAR de de Complemento de Preáo!" VIEW-AS ALERT-BOX INFORMATION.

          IF de-cdo-unit = 0 AND de-funrural-unit = 0 AND de-senar-unit = 0 THEN
                 MESSAGE "Preáo Unit†rio e Total n∆o foram ajustados, sem CDO e SENAR, para Complemento de Preáo!" VIEW-AS ALERT-BOX INFORMATION.
          ************/
          IF de-cdo-unit <> 0 THEN
                 MESSAGE "Preáo Unit†rio e Total foram ajustados devido aos impostos!" VIEW-AS ALERT-BOX INFORMATION.

          IF de-cdo-unit = 0 THEN
                 MESSAGE "Preáo Unit†rio e Total n∆o foram ajustados devido aos impostos!" VIEW-AS ALERT-BOX INFORMATION.

          /* 2355743 */
          ASSIGN de-cdo-unit-imp = de-cdo-unit.
 
          FIND FIRST es-contrato-arroz NO-LOCK WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.
          FIND FIRST es-saldo-arroz NO-LOCK WHERE ROWID(es-saldo-arroz) = pRowSaldo NO-ERROR.
          FIND es-param-estab NO-LOCK WHERE es-param-estab.cod-estabel = es-saldo-arroz.cod-estabel NO-ERROR.
          FIND FIRST b-ins-emitente WHERE b-ins-emitente.cod-emitente = INPUT i-fornecedor NO-LOCK NO-ERROR.  

    
          CREATE ordem-compra.
          ASSIGN ordem-compra.numero-ordem = i-ordem
                 ordem-compra.cod-emitente = INPUT i-fornecedor
                 ordem-compra.it-codigo    = IF input rs-tipo = 3 THEN INPUT c-it-codigo ELSE tt-it-nota.it-codigo
                 ordem-compra.natureza     = 1 /* Compra     */
                 ordem-compra.situacao     = 2 /* Confirmada */
                 ordem-compra.cod-estabel  = IF input rs-tipo = 3 THEN es-contrato-arroz.cod-estabel ELSE nota-fiscal.cod-estabel
                 ordem-compra.num-pedido   = i-nr-pedido
                 ordem-compra.data-pedido  = TODAY
                 ordem-compra.qt-solic     = IF input rs-tipo = 3 THEN 1 ELSE tt-it-nota.qt-faturada[1]
                 ordem-compra.aliquota-ipi = 0
                 ordem-compra.preco-fornec = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */ 
                 ordem-compra.preco-orig   = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */ 
                 ordem-compra.preco-unit   = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */ 
                 ordem-compra.pre-unit-for = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                 ordem-compra.cod-comprado = c-seg-usuario
                 ordem-compra.int-1        = es-saldo-arroz.nr-contrato
                 ordem-compra.dep          = es-param-estab.cod-dep-compra
                 ordem-compra.requisitante = c-seg-usuario
                 ordem-compra.tp-despesa   = IF input rs-tipo = 3 THEN 600 ELSE ITEM.tp-desp-padrao.
                  
    
          ASSIGN de-quant-scs    = de-quant-scs + ordem-compra.qt-solic.
          IF c-it-codigo-ped = "" THEN ASSIGN c-it-codigo-ped = ordem-compra.it-codigo.
                                  ELSE ASSIGN c-it-codigo-ped = c-it-codigo-ped + " - " + ordem-compra.it-codigo. 
        
          RUN pi-acompanhar IN h-acomp (INPUT STRING(ordem-compra.numero-ordem)).
        
          FOR EACH unid-neg-fam NO-LOCK
              WHERE unid-neg-fam.fm-codigo = ITEM.fm-codigo:
        
              CREATE unid-neg-ordem.
              ASSIGN unid-neg-ordem.cod_unid_negoc   = unid-neg-fam.cod_unid_negoc
                     unid-neg-ordem.numero-ordem     = ordem-compra.numero-ordem
                     unid-neg-ordem.perc-unid-neg    = unid-neg-fam.perc-unid-neg
                     unid-neg-ordem.qtd-unid-neg     = 0
                     unid-neg-ordem.val-unid-neg[1]  = 0
                     unid-neg-ordem.val-unid-neg[2]  = 0.
          END.
            
          CREATE prazo-compra.
          ASSIGN prazo-compra.numero-ordem = ordem-compra.numero-ordem
                 prazo-compra.situacao     = ordem-compra.situacao
                 prazo-compra.parcela      = 1
                 prazo-compra.it-codigo    = IF input rs-tipo = 3 THEN INPUT c-it-codigo ELSE tt-it-nota.it-codigo
                 prazo-compra.un           = IF input rs-tipo = 3 THEN "kg" ELSE item.un
                 prazo-compra.quantid-orig = ordem-compra.qt-solic
                 prazo-compra.quantidade   = ordem-compra.qt-solic
                 prazo-compra.quant-saldo  = ordem-compra.qt-solic
                 prazo-compra.qtd-do-forn  = ordem-compra.qt-solic
                 prazo-compra.qtd-sal-forn = ordem-compra.qt-solic
                 prazo-compra.data-orig    = TODAY
                 prazo-compra.data-entrega = TODAY.
    
          CREATE cotacao-item.
          ASSIGN cotacao-item.it-codigo    = ordem-compra.it-codigo
                 cotacao-item.cod-emitente = ordem-compra.cod-emitente
                 cotacao-item.numero-ordem = ordem-compra.numero-ordem
                 cotacao-item.seq-cot      = 1
                 cotacao-item.data-cotacao = TODAY
                 cotacao-item.un           = IF input rs-tipo = 3 THEN "kg" ELSE item.un
                 cotacao-item.preco-fornec = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                 cotacao-item.preco-unit   = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                 cotacao-item.pre-unit-for = de-preco /* DEC(de-preco:SCREEN-VALUE) erika - 09/11/12 */
                 cotacao-item.dec-1        = de-unit-ori
                 cotacao-item.cod-comprado = ordem-compra.cod-comprado
                 cotacao-item.aprovador    = c-seg-usuario
                 cotacao-item.usuario      = c-seg-usuario
                 cotacao-item.data-atualiz = TODAY
                 cotacao-item.hora-atualiz = STRING(TIME,"HH:MM:SS")
                 cotacao-item.cot-aprovada = YES
                 cotacao-item.aliquota-ipi = 0.
        
          
          FIND FIRST item NO-LOCK
                   WHERE item.it-codigo = ordem-compra.it-codigo no-error.
          RUN ccp/ccapi001.p (INPUT ROWID(ordem-compra)). /* Calculo do preco para reposicao */          
    

      END. /* Ç complemento */
                
      
   
      /* cria hist¢rico dessa liquidaá∆o, s¢ ser† a primeira vez a criar se for a primeira liquidaá∆o sen∆o ter† entrado no bloco 
      de cima e verificar se a escolha do tipo de rendimento era diferente do escoilhido na vez anterior*/
      CREATE es-hist-rend-ped.
      ASSIGN es-hist-rend-ped.cod-estabel    = ordem-compra.cod-estabel
             es-hist-rend-ped.nr-contrato    = INPUT i-contrato
             es-hist-rend-ped.dt-transacao   = TODAY
             es-hist-rend-ped.hora-transacao = STRING(TIME,"hh:mm:ss")
             es-hist-rend-ped.serie          = IF input rs-tipo = 3 THEN "" ELSE nota-fiscal.serie
             es-hist-rend-ped.nr-nota-fis    = IF input rs-tipo = 3 THEN "9999999" ELSE nota-fiscal.nr-nota-fis
             es-hist-rend-ped.tipo-rendim    = INT(INPUT rs-rendimento)
             es-hist-rend-ped.cod-cond-pag   = int(fl-condicao:SCREEN-VALUE)
             es-hist-rend-ped.dt-vencimento  = INPUT dt-vencimento
             es-hist-rend-ped.num-pedido     = i-nr-pedido
             es-hist-rend-ped.usuario        = c-seg-usuario
             es-hist-rend-ped.rendimento     = IF INT(INPUT rs-rendimento) = 3 THEN string(INPUT c-media)
                                                   ELSE IF INT(INPUT rs-rendimento) = 1 THEN string(INPUT c-media-ent)
                                                       ELSE IF INT(INPUT rs-rendimento) = 2 THEN string(INPUT c-media-liq)
                                                           ELSE "".

 


      CREATE pedido-compr.

      FIND transporte WHERE transporte.cod-transp = b-ordem-compra.cod-transp NO-LOCK NO-ERROR.
      IF AVAIL transporte THEN
          ASSIGN pedido-compr.cod-transp = transporte.cod-transp
                 pedido-compr.via-transp = transporte.via-transp.
          ELSE 
              FIND transporte WHERE transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
              IF AVAIL transporte THEN 
              ASSIGN pedido-compr.cod-transp = transporte.cod-transp
                     pedido-compr.via-transp = transporte.via-transp.

      ASSIGN pedido-compr.num-pedido      = i-nr-pedido
             pedido-compr.emergencial     = NO /* erika 14/09 */
             pedido-compr.cod-cond-pag    = int(fl-condicao:SCREEN-VALUE)
             pedido-compr.cod-emitente    = ordem-compra.cod-emitente
             pedido-compr.cod-mensagem    = 1
             pedido-compr.data-pedido     = TODAY
             pedido-compr.cod-estabel     = ordem-compra.cod-estabel
             pedido-compr.end-cobranca    = ordem-compra.cod-estabel
             pedido-compr.end-entrega     = ordem-compra.cod-estabel
             pedido-compr.frete           = 1
             pedido-compr.natureza        = ordem-compra.natureza
             pedido-compr.responsavel     = c-seg-usuario
             pedido-compr.situacao        = 1
             pedido-compr.nr-processo     = ordem-compra.nr-processo
             c-nat-ext                    = {ininc/i01in295.i 04 pedido-compr.natureza}.

      FIND FIRST repres NO-LOCK WHERE repres.cod-rep = es-contrato-arroz.u-INT-2 NO-ERROR.
      

      IF input rs-tipo <> 3 THEN DO:
          
    
          ASSIGN pedido-compr.c-observacao[1] = "Nf Produtor      : " + c-notas
                 pedido-compr.c-observacao[2] = "Nr Ticket        : " + c-tickets
                 pedido-compr.c-observacao[3] = "Contrato Numero  : " + STRING(INPUT i-contrato,"zzzzzzzz9")
                                                                        + " - Nota Devolucao   : " +
                                                                        nota-fiscal.nr-nota-fis
                 pedido-compr.c-observacao[4] = "Preco Quilo      : " + STRING(INPUT de-preco,"ZZZZZZ9.9999").
    
          IF input rs-tipo = 2 THEN ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "### Pedido Gerado para Cobranáa do Serviáo de Secagem ###" + chr(10) + "Unidade: " + string(pedido-compr.cod-estabel,"99") + chr(10).
                               ELSE ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + chr(10) + "Unidade: " + string(pedido-compr.cod-estabel,"99") + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Nome do Fornecedor: " + b-ins-emitente.nome-emit + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Cidade: " + b-ins-emitente.cidade + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Quantidade Liquidada em Kg: " + STRING(de-quantidade,">>>,>>>,>>9.99") + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Quantidade Liquidada em Scs " + STRING((de-quant-scs / 50),">>>,>>>,>>9.99") + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Tipo: " + string(c-it-codigo-ped) + chr(10). /* 290806 */
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor P/kg Negociado R$: " + STRING(DEC(de-preco-salva),">>>,>>>,>>9.9999") + chr(10).
    
          IF input rs-tipo = 2 THEN ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor P/Saco Negociado R$: " + STRING(DEC(de-preco-salva * 50),">>>,>>>,>>9.9999") + chr(10).
                    ELSE ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor P/Saco Negociado R$: " + STRING(DEC(de-preco-2 * 50) ,">>>,>>>,>>9.9999") + chr(10). 
          
/*           ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor P/kg Bruto (CDO + Funrural) R$: " + STRING(DEC(de-preco),">>>,>>>,>>9.9999") + chr(10). */


          /* 2355743 */
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor CDO R$: " + STRING(DEC(de-cdo-unit-imp),">>>,>>>,>>9.99") + chr(10).

          /* 21042010 isená∆o fr */
          /* senar */
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor P/kg Bruto (CDO + Senar) R$: " + STRING(DEC(de-preco),">>>,>>>,>>9.9999") + chr(10). 
          /*ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor P/kg Bruto (CDO) R$: " + STRING(DEC(de-preco),">>>,>>>,>>9.9999") + chr(10).*/

          /***************
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor Pago ao Produtor R$: " + STRING(DEC(de-quantidade * de-preco-2),">>>,>>>,>>9.9999") + chr(10).
          ****************/
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor Pago ao Produtor R$: " + STRING(DEC(de-preco * de-quantidade) - de-funrural-unit - de-senar-unit,">>>,>>>,>>9.9999") + chr(10).
           
          
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Contrato: " + STRING(es-contrato-arroz.nr-contrato) + chr(10).
          IF AVAIL repres THEN ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Corretor: " + STRING(es-contrato-arroz.u-int-2) + "-" + repres.nome + chr(10).
                          ELSE ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Corretor: " + STRING(es-contrato-arroz.u-int-2) + "- SEM CADASTRO" + chr(10).
    
          
          /* interpretaá∆o de qual tipo de rendimento dever† sair no pedido de compra */
          IF INT(INPUT rs-rendimento) = 3 THEN DO:        
              ASSIGN pedido-compr.c-observacao[5] = "Rendimento MÇdio Saldo : " + INPUT c-media
                     pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Saldo: " + INPUT c-media + "(selecionado pelo operador)" + chr(10)
                     pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Entradas: " + INPUT c-media-ent + chr(10)
                     pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Liquidaá∆o: " + INPUT c-media-liq + chr(10).
          END. /* rendimento saldo como o antigo - sempre foi */

          IF INPUT c-media-ent <> ? AND 
             INPUT c-media     <> ? AND 
             INPUT c-media-liq <> ? THEN DO:
              IF INT(INPUT rs-rendimento) = 1 THEN DO:        
                  ASSIGN pedido-compr.c-observacao[5] = "Rendimento MÇdio Entradas: " + INPUT c-media-ent
                         pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Entradas: " + INPUT c-media-ent + "(selecionado pelo operador)" + chr(10) 
                         pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Saldo: " + INPUT c-media + chr(10) 
                         pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Liquidaá∆o: " + INPUT c-media-liq + chr(10).
              END. /* rendimento pelas entradas como o do es4021 */
              IF INT(INPUT rs-rendimento) = 2 THEN DO:        
                          ASSIGN pedido-compr.c-observacao[5] = "Rendimento MÇdio Liquidaá∆o: " + INPUT c-media-liq
                                 pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Liquidaá∆o: " + INPUT c-media-liq + "(selecionado pelo operador)" + chr(10) 
                                 pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Saldo: " + INPUT c-media + chr(10) 
                                 pedido-compr.Comentario      = pedido-compr.Comentario + "Rendimento MÇdio Entradas: " + INPUT c-media-ent + chr(10).
              END. /* rendimento parcial das liquidaá‰es */
          END. /* s¢ com rendimento */

          /* inicio - se escolheu tipo de rendimento diferente da vez anterior para o dessa liquidaá∆o */
          IF l-conf-rend THEN DO:
            ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + chr(10) + chr(10) + "ATENÄ«O!!! FOI CONFIRMADO PELO OPERADOR " + caps(c-seg-usuario) +
                                             " ESTE TIPO DE RENDIMENTO, APESAR DE NA LIQUIDAÄ«O ANTERIOR, O USUµRIO " + string(c-usr-hist) +
                                             " TER UTILIZADO O RENDIMENTO ".
    
            IF i-rend-hist = 3 THEN ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "SALDO," + CHR(10) .
            IF i-rend-hist = 1 THEN ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "ENTRADAS," + chr(10) .
            IF i-rend-hist = 2 THEN ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "LIQUIDAÄ«O," + chr(10).
    
            ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + " NO VALOR DE " + STRING(c-rend-hist) + chr(10) + CHR(10).
    
          END.
          /* se escolheu tipo de rendimento diferente da vez anterior para o dessa liquidaá∆o */ 
          
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Data de Pagamento: " + string(INPUT dt-vencimento) + chr(10).
          
          IF l-sem-inf = NO THEN do:
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Banco: " + STRING(b-ins-emitente.cod-banco) + "-" + string(banco.nome-banco) + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "AG: "  + string(b-ins-emitente.agencia) + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "C/C: "  + string(b-ins-emitente.conta-corren) + chr(10).
          END.
          ELSE DO:
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Banco: " + "OBS: Usu†rio permitiu gerar pedido SEM Informaá‰es Banc†rias!" + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "AG: "  + "OBS: Usu†rio permitiu gerar pedido SEM Informaá‰es Banc†rias!" + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "C/C: "  + "OBS: Usu†rio permitiu gerar pedido SEM Informaá‰es Banc†rias!" + chr(10).
          END. /* gerou sem informaá‰es banc†rias */
          

      END. /* para secagem e normal */

      /* para complemento de preáo */
      ELSE DO:
          ASSIGN pedido-compr.c-observacao[1] = "Preco Quilo      : " + STRING(INPUT de-preco,"ZZZZZZ9.9999").
          FIND FIRST repres NO-LOCK WHERE repres.cod-rep = es-contrato-arroz.u-INT-2 NO-ERROR.
    
          ASSIGN pedido-compr.Comentario = "### Pedido de Complemento de Preáo ###" + chr(10) + pedido-compr.Comentario + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Motivo do Complemento: " + string(c-motivo-compl,"X(60)") + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Unidade: " + string(pedido-compr.cod-estabel,"99") + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Nome do Fornecedor: " + c-nome-emit-compl  + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Cidade: " + b-ins-emitente.cidade + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor do Complemento R$:" + STRING(DEC(de-preco-salva),">>>,>>>,>>9.9999") + chr(10).    
          /*ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor do Complemento Bruto (+ Funrural) R$:" + STRING(DEC(de-preco),">>>,>>>,>>9.9999") + chr(10). isená∆o FR 21042010*/
          /*ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor do Complemento Bruto R$:" + STRING(DEC(de-preco),">>>,>>>,>>9.9999") + chr(10).*/
          /* senar */
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor do Complemento Bruto (+ Senar) R$:" + STRING(DEC(de-preco),">>>,>>>,>>9.9999") + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Contrato: " + STRING(es-contrato-arroz.nr-contrato) + chr(10).
          IF AVAIL repres THEN ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Corretor: " + STRING(es-contrato-arroz.u-int-2) + "-" + repres.nome + chr(10).
                          ELSE ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Corretor: " + STRING(es-contrato-arroz.u-int-2) + "- SEM CADASTRO" + chr(10).
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Data de Pagamento: 72 horas ap¢s emiss∆o da nfp" + chr(10).
          
          IF AVAIL banco THEN do:
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Banco: " + STRING(b-ins-emitente.cod-banco) + "-" + string(banco.nome-banco) + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "AG: "  + string(b-ins-emitente.agencia) + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "C/C: "  + string(b-ins-emitente.conta-corren) + chr(10).
          END.
          ELSE DO:
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Banco: " + "OBS: Usu†rio permitiu gerar pedido SEM Informaá‰es Banc†rias!" + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "AG: "  + "OBS: Usu†rio permitiu gerar pedido SEM Informaá‰es Banc†rias!" + chr(10).
              ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "C/C: "  + "OBS: Usu†rio permitiu gerar pedido SEM Informaá‰es Banc†rias!" + chr(10).
          END. /* gerou sem informaá‰es banc†rias */

          /* 2355743 */
          ASSIGN pedido-compr.Comentario = pedido-compr.Comentario + "Valor CDO R$: " + STRING(DEC(de-cdo-unit-imp),">>>,>>>,>>9.99") + chr(10).

      END. /* compl preáo */
      
      RUN ccp/ccapi642.p (5, ROWID(pedido-compr), 1). /* Estatisticas de Compras */

      /* cria tabela de descontos e bonificaá∆oes aplicadas por pedido de compra */
      FOR EACH tt-aplic-desc-bon:

          CREATE es-aplic-desc-bon-ped.
          ASSIGN es-aplic-desc-bon-ped.num-pedido = pedido-comp.num-pedido
                 es-aplic-desc-bon-ped.nr-ticket  = tt-aplic-desc-bon.nr-ticket  
                 es-aplic-desc-bon-ped.qt-desc    = tt-aplic-desc-bon.qt-desc    
                 es-aplic-desc-bon-ped.qt-bon     = tt-aplic-desc-bon.qt-bon.
      END. /* tt-aplic-desc-bon */

      FOR EACH tt-aplic-sec-desc-bon:
          CREATE es-aplic-sec-desc-bon-ped.
          ASSIGN es-aplic-sec-desc-bon-ped.num-pedido = pedido-comp.num-pedido
                 es-aplic-sec-desc-bon-ped.nr-ticket  = tt-aplic-sec-desc-bon.nr-ticket  
                 es-aplic-sec-desc-bon-ped.qt-desc    = tt-aplic-sec-desc-bon.qt-desc    
                 es-aplic-sec-desc-bon-ped.qt-bon     = tt-aplic-sec-desc-bon.qt-bon.
      END. /* tt-aplic-sec-desc-bon */


      CREATE mgcam.es-ctrl-rend-ped.
      ASSIGN mgcam.es-ctrl-rend-ped.numero-pedido = i-nr-pedido 
             mgcam.es-ctrl-rend-ped.tp-rendimento = string(INT(INPUT rs-rendimento)).
      
      /*  Aprovacao Eletronica */
      IF param-compra.log-1 AND
          (param-compra.int-1 = 2 OR
           param-compra.int-1 = 3) THEN 
           RUN cdp/cdapi171.p (4,2,ROWID(ordem-compra)).
      
      IF l-modulo-ge = YES THEN DO:
          ASSIGN r-gerencial = ROWID(ordem-compra).
          RUN gep/ge0102a.p (r-gerencial, 16).
      END.         
    
END. /* IF AVAIL es-saldo-arroz... */


RUN pi-finalizar IN h-acomp.

/* diferente de complemento de preáo */
IF input rs-tipo <> 3 THEN DO:

    FIND FIRST nota-fiscal
        WHERE nota-fiscal.cod-estabel = INPUT fl-estabel
          AND nota-fiscal.serie       = INPUT serie
          AND nota-fiscal.nr-nota-fis = INPUT nr-nfd
        NO-ERROR.
        
    FIND b1-emitente NO-LOCK WHERE b1-emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-ERROR.
    
    ASSIGN nota-fiscal.nr-proc-exp = STRING(es-contrato-arroz.nr-contrato).
    
    FOR EACH it-nota-fisc OF nota-fiscal:
        
        FIND FIRST es-ticket 
            WHERE es-ticket.cod-estabel    = it-nota-fisc.cod-estabel
              AND es-ticket.nro-docto      = it-nota-fisc.nr-docum
              AND es-ticket.serie          >= "" /*it-nota-fisc.serie*/
              AND es-ticket.cod-produtor   = b1-emitente.cod-emitente
            NO-ERROR.
        IF AVAIL es-ticket THEN do:
            /* 08/02/2012 - tratando se imprime ou na‰ o rendimento na nota trato no ure1005a */

            ASSIGN it-nota-fisc.conh-frete = STRING(es-ticket.nr-ticket)
                   es-ticket.u-char-4      = INPUT to-rendimento.
        END.
    END.
    
    
    /*** PEDIDO DE COMPRA DE SECAGEM APROVA AUTOMATICAMENTE ***/
    IF  SUBSTR(nota-fiscal.observ-nota,1900,7) = "SECAGEM" THEN DO:
        FIND FIRST doc-pend-aprov USE-INDEX doc-pend-aprov-2
            WHERE (doc-pend-aprov.ind-tip-doc  = 6  /* Pedido Emergencial */
               OR  doc-pend-aprov.ind-tip-doc  = 4) /* Pedido */
              AND doc-pend-aprov.num-pedido   = pedido-compr.num-pedido
              AND doc-pend-aprov.numero-ordem = ordem-compra.numero-ordem
            NO-ERROR.
        IF  AVAIL doc-pend-aprov THEN
            ASSIGN doc-pend-aprov.ind-situacao  = 2 /* Aprovado */
                   doc-pend-aprov.narrativa-apr = "SECAGEM"
                   doc-pend-aprov.aprov-auto    = YES
                   doc-pend-aprov.sit-aprov     = YES.   
                    
    END.
END. /* <> compl preáo */

MESSAGE "Processo de Geraá∆o de Pedido de Compra Conclu°do!" SKIP(1)
        "Pedido: " STRING(i-nr-pedido,"zzzzzzzz9")
        VIEW-AS ALERT-BOX WARNING TITLE "Pedido Compra Gerado!!!".

ASSIGN num-nota = ""
       num-serie = "".
MESSAGE "Efetuar nesse momento a Compra de Item Consignado para a nota " nr-nfd:SCREEN-VALUE "?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-es4027 AS LOG.
IF l-es4027 THEN DO:
   SESSION:SET-WAIT-STATE("general":U).

  UPDATE num-nota LABEL "Ref. a NF de Venda Nr." SKIP
      num-serie LABEL "SÇrie da NF de Venda   "
    WITH FRAME f-num-nota SIDE-LABELS BGCOLOR 8 VIEW-AS DIALOG-BOX.

   IF INPUT num-nota = "" THEN DO: 
       MESSAGE "Nota de Venda deve ser Informada para Processo de Compra Autom†tico!" skip
           "Operaá∆o Cancelada!" VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.
   
   ASSIGN gr-pedido-compr = ROWID(pedido-compr).
  

   RUN esp\es4027a.p(INPUT num-nota,
                     INPUT num-serie).
   
    IF  RETURN-VALUE = 'OK' THEN
        MESSAGE "Recebimento Atualizado ! ! ! " VIEW-AS ALERT-BOX INFO BUTTONS OK.
   SESSION:SET-WAIT-STATE("":U).
END.

END. /* DO TRANSACTION... */

APPLY "CLOSE":U TO THIS-PROCEDURE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-secagem wWindow 
PROCEDURE pi-secagem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* 190306 */
    assign
        de-val-ori-desc = 0
        de-val-ori-bon  = 0.
            
    FOR EACH tt-bonifica:
        DELETE tt-bonifica.
    END. 
    
    FOR EACH tt-qtde.
      DELETE tt-qtde.
    END.

    FIND FIRST es-contrato-arroz NO-LOCK
         WHERE ROWID(es-contrato-arroz) = pRowContrato NO-ERROR.

    FIND es-item NO-LOCK
        WHERE es-item.it-codigo = es-contrato-arroz.it-codigo
        NO-ERROR.

    FIND FIRST es-saldo-arroz NO-LOCK
         WHERE ROWID(es-saldo-arroz) = pRowSaldo NO-ERROR.    

    FOR EACH es-movto-arroz NO-LOCK 
        WHERE es-movto-arroz.nome-matriz  = es-contrato-arroz.nome-matriz
          AND es-movto-arroz.cod-emitente = nota-fiscal.cod-emitente          
          AND es-movto-arroz.nr-contrato  = es-contrato-arroz.nr-contrato
          AND es-movto-arroz.cod-estabel  = es-contrato-arroz.cod-estabel
          AND (es-movto-arroz.esp-docto     = "DSC" OR es-movto-arroz.esp-docto = "BON"),
           FIRST es-ticket NO-LOCK WHERE es-ticket.nr-ticket = es-movto-arroz.nr-ticket
        BREAK BY es-movto-arroz.cod-emitente
              BY es-movto-arroz.esp-docto   
              BY es-movto-arroz.dt-trans
              BY es-movto-arroz.nr-contrato:

        IF es-movto-arroz.tipo-trans = 2 THEN DO:
            FIND es-tipo-desconto WHERE
                 es-tipo-desconto.tp-desconto = es-movto-arroz.tp-desconto NO-LOCK NO-ERROR.
                    
            if es-movto-arroz.tp-desconto <> "implantacao" then do:
                IF NOT AVAIL es-tipo-desconto THEN NEXT.           
                
                IF (es-tipo-desconto.financeiro) THEN do:          

                  FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = es-ticket.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAIL tt-desc-bon THEN DO:                      
                    /* 1 */
                    CREATE tt-desc-bon.
                    ASSIGN tt-desc-bon.nr-ticket = es-ticket.nr-ticket.
                  END.
                  
                  ASSIGN tt-desc-bon.qt-desc = tt-desc-bon.qt-desc + es-movto-arroz.quantidade
                         de-val-ori-desc     = de-val-ori-desc + es-movto-arroz.quantidade.

                END.
            end. /* nao implantacao */
        END. /* saidas */
        
        IF  es-movto-arroz.esp-docto = "DSC" THEN DO:
        
        RUN esp/esapi005.p (INPUT es-ticket.nr-ticket).          

                IF es-ticket.dt-entrada > 11/09/04 THEN DO: 
                    
                    RUN  pi-quantidade.
                    
                    FOR EACH tt-detalhes:
                        DELETE tt-detalhes.
                    END.

                    RUN esp/esapi005b.p (INPUT es-ticket.nr-ticket,
                                         INPUT di-quantidade).
                END. /* interpreta data */

                FOR EACH tt-detalhes WHERE tt-detalhes.c-esp-docto = "BON":

                          FIND FIRST tt-bonifica where
                                     tt-bonifica.nr-ticket   = es-ticket.nr-ticket AND
                                     tt-bonifica.c-tp-desconto = tt-detalhes.c-tp-desconto AND
                                     tt-bonifica.quantidade = tt-detalhes.de-quantidade
                              NO-LOCK NO-ERROR.
                          IF NOT AVAIL tt-bonifica THEN DO:
                              
                            CREATE tt-bonifica.
                            ASSIGN tt-bonifica.nr-ticket     = es-ticket.nr-ticket
                                   tt-bonifica.c-tp-desconto = tt-detalhes.c-tp-desconto
                                   tt-bonifica.quantidade    = tt-bonifica.quantidade + tt-detalhes.de-quantidade /* 101105 */
                                   de-val-ori-bon            = de-val-ori-bon + tt-detalhes.de-quantidade.

                         END.

                END. /* for each tt-detalhes */
          end. /* dsc */
        
        /*** BONIFICACAO ***/
        IF  es-movto-arroz.esp-docto = "BON" THEN DO:
            FIND FIRST tt-desc-bon WHERE
                       tt-desc-bon.nr-ticket = es-ticket.nr-ticket
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL tt-desc-bon THEN DO:
                CREATE tt-desc-bon.
                ASSIGN tt-desc-bon.nr-ticket = es-ticket.nr-ticket.
            END.
            ASSIGN tt-desc-bon.qt-bon = tt-desc-bon.qt-bon + es-movto-arroz.quantidade.
        END. /* se bonificaá∆o */ 
        
        /*** quantidade disponivel ***/    
        FIND FIRST tt-qtde WHERE
                   tt-qtde.nr-ticket = es-movto-arroz.nr-ticket
            NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-qtde THEN DO:
            
            ASSIGN de-a-secar = 0
                   de-entra = 0
                   de-saida = 0
                   de-tot-saldo = 0
                   de-tot-secag = 0
                   de-tot-seco = 0
                   de-a-secar = 0
                   vl-tot-secag = 0
                   vl-tot-seco = 0
                   vl-a-secar = 0
                   de-quantidade2 = 0
                   de-tot-a-secar = 0. 

            RUN esp/esapi007.p (INPUT  es-movto-arroz.nr-contrato,
                                INPUT  es-movto-arroz.nr-ticket,
                                INPUT  es-movto-arroz.nr-ticket,
                                OUTPUT de-entra,    
                                OUTPUT de-saida,    
                                OUTPUT de-tot-saldo,
                                OUTPUT de-tot-secag,
                                OUTPUT de-tot-seco, 
                                OUTPUT de-a-secar,
                                OUTPUT vl-tot-secag,
                                OUTPUT vl-tot-seco, 
                                OUTPUT vl-a-secar).
            
            ASSIGN de-quantidade2 = de-quantidade2 + de-tot-saldo
                   de-tot-a-secar = de-tot-a-secar + de-a-secar
                   de-tot-a-secar = ROUND(de-tot-a-secar,2).

            IF  rs-tipo = 2 THEN de-quantidade2 = de-quantidade2.
            ELSE de-quantidade2 = de-quantidade2 - de-tot-a-secar.
            
            CREATE tt-qtde.
            ASSIGN tt-qtde.nr-ticket = es-movto-arroz.nr-ticket
                   tt-qtde.qt-ori    = de-quantidade2.
            
        END. /* not avail tt-qtde */

    END.
    
     /* se n∆o gerou a tabela de desconto e de bonificaá∆o aqui eu gero a mesma */
    FIND FIRST tt-qtde NO-LOCK no-error.
    IF NOT AVAIL tt-qtde THEN do:

        /*** quantidade disponivel ***/    
        FIND FIRST tt-qtde NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-qtde THEN DO:        
            
            FOR EACH tt-qtde-ger:
                CREATE tt-qtde.
                ASSIGN tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket
                       tt-qtde.qt-ori    = 0.

                FIND FIRST tt-desc-bon WHERE
                           tt-desc-bon.nr-ticket = tt-qtde-ger.nr-ticket
                    NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-desc-bon THEN do:
                    CREATE tt-desc-bon.
                END.
            END.
                                                              
        END. /* not avail tt-qtde */
     END. /* not avail tt-qtde */
    
    
     DO TRANS:

        /* veio do n∆o de secagem pra montar tt-des e tt-bon */
        /* gera valores de desconto e bonificacao parciais aplicados para cada ticket */
        FOR EACH tt-qtde-ger BREAK BY tt-qtde-ger.nr-ticket:

          FIND tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK NO-ERROR.
                            
          /*  interpretacao se n∆o tem desc nem bonificaá∆o */
          FOR EACH tt-qtde WHERE tt-qtde.qt-ori = 0:              
              
              ASSIGN de-a-secar = 0
                  de-entra = 0
                  de-saida = 0
                  de-tot-saldo = 0
                  de-tot-secag = 0
                  de-tot-seco = 0
                  de-a-secar = 0
                  vl-tot-secag = 0
                  vl-tot-seco = 0
                  vl-a-secar = 0
                  de-quantidade2 = 0
                  de-tot-a-secar = 0. /* 080404 */

               RUN esp/esapi007.p (INPUT  i-contrato,
                                   INPUT  tt-qtde.nr-ticket,
                                   INPUT  tt-qtde.nr-ticket,
                                   OUTPUT de-entra,    
                                   OUTPUT de-saida,    
                                   OUTPUT de-tot-saldo,
                                   OUTPUT de-tot-secag,
                                   OUTPUT de-tot-seco, 
                                   OUTPUT de-a-secar,
                                   OUTPUT vl-tot-secag,
                                   OUTPUT vl-tot-seco, 
                                   OUTPUT vl-a-secar).
    
               ASSIGN de-quantidade2 = de-quantidade2 + de-tot-saldo
                      de-tot-a-secar = de-tot-a-secar + de-a-secar
                      de-tot-a-secar = ROUND(de-tot-a-secar,2)   .
    
               IF  rs-tipo = 2 THEN de-quantidade2 = de-quantidade2.
               ELSE de-quantidade2 = de-quantidade2 - de-tot-a-secar.
               ASSIGN tt-qtde.qt-ori    = de-quantidade2.

          END. /* for each tt-qtde pra zeradas */

          FIND FIRST tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK.

              FIND tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

              IF not(tt-qtde.qt-ori = tt-qtde-ger.qt-ori) THEN DO:
               
                   FIND FIRST tt-qtde WHERE tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK. 

                   ASSIGN de-perc = ROUND((tt-qtde-ger.qt-ori * 100) / tt-qtde.qt-ori,4).
            
                    FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.                   

                    IF AVAIL tt-desc-bon THEN
                      ASSIGN tt-desc-bon.qt-des = ROUND((tt-desc-bon.qt-des * de-perc) / 100,4)
                             tt-desc-bon.qt-bon = ROUND((tt-desc-bon.qt-bon * de-perc) / 100,4).
                    
                    FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                    IF AVAIL tt-bonifica THEN ASSIGN tt-bonifica.quantidade = ROUND((tt-bonifica.quantidade * de-perc) / 100,4).

              END. /* qtde original difere da negociada na liq */

        END. /* for each tt-qtde-ger */
        /* ate aqui veio do n∆o secagem pra montar tt-des e tt-bon */

        FOR EACH tt-bonifica:
           ASSIGN de-val-bon-limpo = de-val-bon-limpo + tt-bonifica.quantidade.
        END.

        /* gera valores de desconto e bonificacao parciais aplicados para cada ticket */
        FOR EACH tt-qtde-ger:
            FIND tt-qtde WHERE
                 tt-qtde.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK NO-ERROR.

        FIND FIRST bf3-contrato WHERE
                   bf3-contrato.nr-contrato = es-contrato-arroz.nr-contrato NO-LOCK NO-ERROR.
        
          IF bf3-contrato.u-int-5 > 0 THEN do:
             ASSIGN de-grau-secagem = round((bf3-contrato.u-int-5 / 10000),4).
          END. /* u-int-5 */
          ELSE DO:
             FIND FIRST bf-es-ticket WHERE bf-es-ticket.nr-ticket = es-movto-arroz.nr-ticket NO-LOCK NO-ERROR.  

             RUN esp\esapi009.p (INPUT es-movto-arroz.cod-estabel,
                                 INPUT es-movto-arroz.nr-ticket,
                                 INPUT bf-es-ticket.dt-entrada,
                                 OUTPUT de-valor-formula).  
             
             ASSIGN de-grau-secagem = round((de-valor-formula / 100),4).
             
          END. /* u-log-2 */
        
          IF tt-qtde.qt-ori = tt-qtde-ger.qt-ori THEN DO:
                IF rs-tipo <> 2 THEN DO:
                    IF de-grau-secagem = 0 THEN DO:
                        FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                        FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                        FIND FIRST es-aplic-desc-bon WHERE
                                   es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                        IF NOT AVAIL es-aplic-desc-bon THEN DO:
                          CREATE es-aplic-desc-bon.
                          ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket.
                        END.
                        ASSIGN es-aplic-desc-bon.qt-desc   = es-aplic-desc-bon.qt-desc + IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0
                               es-aplic-desc-bon.qt-bon    = es-aplic-desc-bon.qt-bon  + IF AVAIL tt-bonifica THEN tt-bonifica.quantidade ELSE 0 .

                    END. /* n∆o tem secagem */
                    ELSE DO:
                        FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                        FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                        FIND FIRST es-aplic-desc-bon WHERE
                                   es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket EXCLUSIVE-LOCK NO-ERROR.
                        IF NOT AVAIL es-aplic-desc-bon THEN DO:
                          CREATE es-aplic-desc-bon.
                          ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket.
                        END.
                        ASSIGN de-sec-aplic-desc = 0
                           de-sec-aplic-bon  = 0.

                    FOR EACH es-aplic-sec-desc-bon WHERE 
                             es-aplic-sec-desc-bon.nr-ticket = tt-qtde.nr-ticket:
                        ASSIGN de-sec-aplic-desc = de-sec-aplic-desc + es-aplic-sec-desc-bon.qt-desc  
                               de-sec-aplic-bon  = de-sec-aplic-bon  + es-aplic-sec-desc-bon.qt-bon.
                    END.

                    /* se iguais j† aplicou todo o dsc do ticket */
                    IF round(de-val-ori-desc * de-grau-secagem,4) = de-sec-aplic-desc THEN DO:
                      ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                        IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de desconto para esse ticket */
                        IF de-sec-aplic-desc = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND((tt-desc-bon.qt-des - ROUND((tt-desc-bon.qt-des * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND(tt-desc-bon.qt-des  - de-sec-aplic-desc,4) 
                                                                      ELSE 0. 
                        END.
                    END.
                    /* se iguais j† aplicou toda bon do ticket */
                    IF round(de-val-ori-bon * de-grau-secagem,4) = de-sec-aplic-bon THEN DO:                        
                      ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                                        IF AVAIL tt-bonifica THEN tt-bonifica.quantidad ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de bon para esse ticket */
                        IF de-sec-aplic-bon = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                          IF AVAIL tt-bonifica THEN ROUND((tt-bonifica.quantidad - ROUND((tt-bonifica.quantidad * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                           IF AVAIL tt-bonifica THEN ROUND(tt-bonifica.quantidad - de-sec-aplic-bon,4) 
                                                                      ELSE 0. 
                        END.
                    END.

                    IF es-aplic-desc-bon.qt-desc < 0 THEN ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc * -1. /* para no caso de liquidar tudo que d† negativo */
                    IF es-aplic-desc-bon.qt-bon  < 0 THEN ASSIGN es-aplic-desc-bon.qt-bon  = es-aplic-desc-bon.qt-bon  * -1.
                  END. /* tem secagem */
                  
                 /* nova tabela de bon/desc proporcionais por pedido */
                  CREATE tt-aplic-desc-bon.
                  BUFFER-COPY es-aplic-desc-bon TO tt-aplic-desc-bon.
                END. /* input rs-tipo = 2 */
                ELSE DO:
                    FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                    FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                    CREATE es-aplic-sec-desc-bon.
                    ASSIGN es-aplic-sec-desc-bon.nr-ticket = tt-qtde.nr-ticket
                           es-aplic-sec-desc-bon.qt-desc   = IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0
                           es-aplic-sec-desc-bon.qt-bon    = IF AVAIL tt-bonifica THEN tt-bonifica.quantidade ELSE 0.
                     
                    IF es-aplic-sec-desc-bon.qt-desc < 0 THEN ASSIGN es-aplic-sec-desc-bon.qt-desc = es-aplic-sec-desc-bon.qt-desc * -1. /* para no caso de liquidar tudo que d† negativo */
                    IF es-aplic-sec-desc-bon.qt-bon  < 0 THEN ASSIGN es-aplic-sec-desc-bon.qt-bon  = es-aplic-sec-desc-bon.qt-bon  * -1.

                    CREATE tt-aplic-sec-desc-bon.
                    BUFFER-COPY es-aplic-sec-desc-bon TO tt-aplic-sec-desc-bon.
                END. /* tem secagem */
        
                /******* mÇdia ponderada - cria tabela de percetual do ticket utilizado *******/
                FIND FIRST es-ticket-usado WHERE
                           es-ticket-usado.nr-ticket = tt-qtde.nr-ticket
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAIL es-ticket-usado THEN DO:
                    CREATE es-ticket-usado.
                    ASSIGN es-ticket-usado.nr-ticket = tt-qtde.nr-ticket.
                END.
                ASSIGN es-ticket-usado.perc = 100.
        
          END. /* quantidade original = qtde negociada na liqu */
        
          ELSE DO:
                IF  rs-tipo <> 2 THEN DO:
                    
                    IF de-grau-secagem = 0 THEN DO:
                        FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                        FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
  
                        CREATE es-aplic-desc-bon.
                        ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket
                               es-aplic-desc-bon.qt-desc   = IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0
                               es-aplic-desc-bon.qt-bon    = IF AVAIL tt-bonifica THEN tt-bonifica.quantidade ELSE 0.
                    END. /* n∆o tem secagem */
                    ELSE DO:
                        FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                        FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                        CREATE es-aplic-desc-bon.
                        ASSIGN es-aplic-desc-bon.nr-ticket = tt-qtde.nr-ticket.

                        ASSIGN de-sec-aplic-desc = 0
                           de-sec-aplic-bon  = 0.

                    FOR EACH es-aplic-sec-desc-bon WHERE 
                             es-aplic-sec-desc-bon.nr-ticket = tt-qtde.nr-ticket:
                        ASSIGN de-sec-aplic-desc = de-sec-aplic-desc + es-aplic-sec-desc-bon.qt-desc  
                               de-sec-aplic-bon  = de-sec-aplic-bon  + es-aplic-sec-desc-bon.qt-bon.
                    END.

                    /* se iguais j† aplicou todo o dsc do ticket */
                    IF round(de-val-ori-desc * de-grau-secagem,4) = de-sec-aplic-desc THEN DO:
                      ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                        IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de desconto para esse ticket */
                        IF de-sec-aplic-desc = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND((tt-desc-bon.qt-des - ROUND((tt-desc-bon.qt-des * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc + 
                                                 IF AVAIL tt-desc-bon THEN ROUND(tt-desc-bon.qt-des  - de-sec-aplic-desc,4) 
                                                                      ELSE 0. 
                        END.
                    END.
                    /* se iguais j† aplicou toda bon do ticket */
                    IF round(de-val-ori-bon * de-grau-secagem,4) = de-sec-aplic-bon THEN DO:
                      ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                                        IF AVAIL tt-bonifica THEN tt-bonifica.quantidad ELSE 0.
                    END.
                    ELSE DO:
                        /* n∆o aplicou nada de bon para esse ticket */
                        IF de-sec-aplic-bon = 0 THEN DO:
                          ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                                 IF AVAIL tt-bonifica THEN ROUND((tt-bonifica.quantidad - ROUND((tt-bonifica.quantidad * de-grau-secagem),4)),4) ELSE 0.
                        END.
                        /* aplicou parcial ent∆o desconta o que j† aplicou */
                        ELSE DO:
                              ASSIGN es-aplic-desc-bon.qt-bon = es-aplic-desc-bon.qt-bon + 
                                           IF AVAIL tt-bonifica THEN ROUND(tt-bonifica.quantidad - de-sec-aplic-bon,4) 
                                                                      ELSE 0. 
                        END.
                    END.

                    IF es-aplic-desc-bon.qt-desc < 0 THEN ASSIGN es-aplic-desc-bon.qt-desc = es-aplic-desc-bon.qt-desc * -1. /* para no caso de liquidar tudo que d† negativo */
                    IF es-aplic-desc-bon.qt-bon  < 0 THEN ASSIGN es-aplic-desc-bon.qt-bon  = es-aplic-desc-bon.qt-bon  * -1.

                    END. /* tem secagem */
                 
                 /* nova tabela de bon/desc proporcionais por pedido */
                  CREATE tt-aplic-desc-bon.
                  BUFFER-COPY es-aplic-desc-bon TO tt-aplic-desc-bon.

                END. /* input rs-tipo = 2 */
                ELSE DO:
                     FIND FIRST tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.
                     FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde.nr-ticket NO-LOCK NO-ERROR.

                    CREATE es-aplic-sec-desc-bon.
                    ASSIGN es-aplic-sec-desc-bon.nr-ticket = tt-qtde.nr-ticket
                           es-aplic-sec-desc-bon.qt-desc   = IF AVAIL tt-desc-bon THEN tt-desc-bon.qt-des ELSE 0
                           es-aplic-sec-desc-bon.qt-bon    = IF AVAIL tt-bonifica THEN tt-bonifica.quantidade ELSE 0.
                    
                    IF es-aplic-sec-desc-bon.qt-desc < 0 THEN ASSIGN es-aplic-sec-desc-bon.qt-desc = es-aplic-sec-desc-bon.qt-desc * -1. /* para no caso de liquidar tudo que d† negativo */
                    IF es-aplic-sec-desc-bon.qt-bon  < 0 THEN ASSIGN es-aplic-sec-desc-bon.qt-bon  = es-aplic-sec-desc-bon.qt-bon  * -1.

                    CREATE tt-aplic-sec-desc-bon.
                    BUFFER-COPY es-aplic-sec-desc-bon TO tt-aplic-sec-desc-bon.
                END. /* Ç secagem */
        
                /******* mÇdia ponderada - cria tabela de percetual do ticket utilizado *******/
                FIND FIRST es-ticket-usado WHERE
                           es-ticket-usado.nr-ticket = tt-qtde.nr-ticket
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAIL es-ticket-usado THEN DO:
                    
                    CREATE es-ticket-usado.
                    ASSIGN es-ticket-usado.nr-ticket = tt-qtde.nr-ticket.
                END.

                ASSIGN es-ticket-usado.perc = es-ticket-usado.perc + de-perc.
        
                /****** mÇdia *******/               
          END. /* qtde original difere da negociada na liq */
          /* neste ponto a tabela especifica j† est† gerada assim como o percentual */
          
        END. /* for each tt-qtde-ger */

        /** agora ira calcular o preco unitario */
        FOR EACH tt-qtde-ger: 
        
            if tt-qtde-ger.qt-ori <= 0 then next.
            FIND FIRST tt-desc-bon WHERE tt-desc-bon.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK NO-ERROR.
            IF AVAIL tt-desc-bon THEN DO:
                ASSIGN de-desc = de-desc + tt-desc-bon.qt-desc.
            END.
                                          
            FOR EACH tt-bonifica WHERE tt-bonifica.nr-ticket = tt-qtde-ger.nr-ticket NO-LOCK:
              ASSIGN de-bon  = de-bon  + tt-bonifica.quantidade.
            END.
        END. /* TT-QTDE-GER */
     END. /* do trans */
    
        
     ASSIGN de-val-desc-s  = /* de-desc * */ de-preco
            de-val-bon-s   = /* de-bon  * */ de-preco
            de-val-semdb-s = de-quantidade * de-preco.

     IF es-contrato-arroz.cod-estabel = "04" OR
        es-contrato-arroz.cod-estabel = "06" OR
        es-contrato-arroz.cod-estabel = "08" OR
        es-contrato-arroz.cod-estabel = "09" OR
        es-contrato-arroz.cod-estabel = "12" THEN de-val-liq-s = de-val-semdb-s.
     ELSE de-val-liq-s = de-val-semdb-s - de-val-desc-s + de-val-bon-s.

     ASSIGN de-val-unit-s = de-val-liq-s / de-quantidade
            de-vltotal-3  = de-val-unit-s * de-quantidade
            de-preco-3    = de-val-unit-s.
    
     MESSAGE "Preáo Unit†rio Secagem: " de-preco-3 SKIP
             "Valor Total Calculado Secagem: "  de-vltotal-3 view-as ALERT-BOX INFORMATION.

     APPLY "choose" TO btok IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validacao wWindow 
PROCEDURE pi-validacao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME fPage1:   
    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-quantidade B-table-Win 
PROCEDURE pi-quantidade :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN di-quantidade = 0.

  FOR EACH tt-detalhes:

      FIND es-tipo-desconto WHERE es-tipo-desconto.tp-desconto = tt-detalhes.c-tp-desconto NO-LOCK NO-ERROR.

      FIND FIRST es-param-estab NO-LOCK WHERE cod-estabel = c-estabel.

      IF tt-detalhes.c-esp-docto = "DEP" OR 
         tt-detalhes.c-esp-docto = "CMP" OR 
         tt-detalhes.c-esp-docto = "TRA" OR 
         tt-detalhes.c-esp-docto = "IMP" THEN
         ASSIGN di-quantidade = di-quantidade + tt-detalhes.de-quantidade.
      
      IF tt-detalhes.c-esp-docto = "DSC" THEN DO:
         IF es-tipo-desconto.quantidade THEN
            ASSIGN di-quantidade = di-quantidade - tt-detalhes.de-quantidade.

         IF es-tipo-desconto.financeiro THEN
              IF es-param-estab.base-descto = NO THEN
                 ASSIGN di-quantidade = di-quantidade - tt-detalhes.de-quantidade.
      END.                                                                        
  END.  

END PROCEDURE.

PROCEDURE pi-movto-arroz:

    DEF INPUT PARAM c-esp-docto LIKE es-movto-arroz.esp-docto.
    
    bloco-movto:
    FOR EACH es-movto-arroz NO-LOCK 
        WHERE es-movto-arroz.nr-contrato   = es-contrato-arroz.nr-contrato
          AND es-movto-arroz.esp-docto     = c-esp-docto USE-INDEX ch-esp
        BREAK BY es-movto-arroz.cod-emitente
              BY es-movto-arroz.esp-docto   
              BY es-movto-arroz.dt-trans
              BY es-movto-arroz.nr-contrato:

        IF es-movto-arroz.dt-trans < es-contrato-arroz.dt-contrato THEN NEXT bloco-movto.

        FIND FIRST emitente NO-LOCK
            WHERE emitente.nome-abrev = es-movto-arroz.nome-matriz
            NO-ERROR.
        if NOT avail emitente then
            FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = es-movto-arroz.cod-emitente NO-ERROR.
        
     
        FIND FIRST es-ticket use-index idx-docto
            WHERE es-ticket.serie        = es-movto-arroz.serie-docto
              AND es-ticket.nro-docto    = es-movto-arroz.nro-docto
              AND es-ticket.cod-emitente = es-movto-arroz.cod-emitente
              AND es-ticket.nat-operacao = es-movto-arroz.nat-operacao             
        NO-LOCK NO-ERROR.

        IF  NOT AVAIL es-ticket THEN DO:
            FIND FIRST es-ticket use-index idx-docto
               WHERE es-ticket.serie        = es-movto-arroz.serie-docto
                 AND es-ticket.nro-docto    = es-movto-arroz.nro-docto
                 AND es-ticket.cod-produtor = es-movto-arroz.cod-emitente
                 AND es-ticket.nat-operacao = es-movto-arroz.nat-operacao
               NO-LOCK NO-ERROR.

            /*  para sair transferencia de titularidade */
            FIND FIRST es-transferencia where
                       es-transferencia.nr-contrato-ent = es-movto-arroz.nr-contrato and
                       es-transferencia.nr-ticket       = es-movto-arroz.nr-ticket
                       NO-LOCK no-error.
        END.
            
        IF NOT AVAIL es-ticket THEN NEXT.        

        FIND FIRST docum-est NO-LOCK
            WHERE docum-est.nro-docto    = es-movto-arroz.nro-docto
              AND docum-est.serie        = es-movto-arroz.serie
              AND docum-est.nat-operacao = es-movto-arroz.nat-operacao NO-ERROR.
                                                                               
        IF AVAIL emitente THEN DO:

            FIND FIRST tt-extrato
                WHERE tt-extrato.cod-emitente = emitente.cod-emitente
                  AND tt-extrato.nr-contrato  = es-movto-arroz.nr-contrato
                  AND tt-extrato.nr-ticket    = es-movto-arroz.nr-ticket  
                  AND tt-extrato.serie        = es-movto-arroz.serie
                  AND tt-extrato.nro-docto    = es-movto-arroz.nro-docto
                  AND tt-extrato.esp-docto    = es-movto-arroz.esp-docto
                NO-ERROR.
            IF  NOT AVAIL tt-extrato THEN DO:
                IF  es-movto-arroz.esp-docto <> "BON" AND
                    es-movto-arroz.esp-docto <> "DSC" AND
                    es-movto-arroz.esp-docto <> "SEC" THEN DO:

                    
                    CREATE tt-extrato.
                    ASSIGN tt-extrato.nome-matriz    = es-movto-arroz.nome-matriz
                           tt-extrato.cod-emitente   = emitente.cod-emitente
                           tt-extrato.nr-contrato    = es-movto-arroz.nr-contrato 
                           tt-extrato.nr-ticket      = es-movto-arroz.nr-ticket
                           tt-extrato.serie          = es-movto-arroz.serie
                           tt-extrato.esp-docto      = es-movto-arroz.esp-docto
                           tt-extrato.tipo-trans     = es-movto-arroz.tipo-trans
                           tt-extrato.dt-trans       = es-movto-arroz.dt-trans
                           tt-extrato.nr-placa       = IF AVAIL es-ticket THEN es-ticket.nr-placa-cam   ELSE ""
                           tt-extrato.nr-nota-fornec = IF AVAIL es-ticket THEN es-ticket.nr-nota-fornec ELSE ""
                           tt-extrato.nro-docto      = es-movto-arroz.nro-docto
                           tt-extrato.peso-fornec    = IF AVAIL es-ticket THEN es-ticket.peso-desmem[1] ELSE 0
                           tt-extrato.valor-unit     = es-movto-arroz.valor 
                           tt-extrato.peso-liq       = es-movto-arroz.quantidade
                           tt-extrato.peso-total     = tt-extrato.peso-liq
                           tt-extrato.rendimento     = IF AVAIL es-ticket THEN
                                                       STRING((es-ticket.rend-inteiro),"Z99.9999") + " X " + 
                                                       STRING((es-ticket.rend-quebr),"Z99.9999")
                                                       ELSE es-movto-arroz.u-char-1.

                        /* para coloccar rendimentos das entradas mesmo pra quem seja s¢ dep¢sito, ou seja que n∆o tem nenhum desconto ou bonificaá∆o */
                        FIND FIRST br-es-ticket WHERE
                                   br-es-ticket.nr-ticket = es-movto-arroz.nr-ticket
                                   NO-LOCK NO-ERROR.
                            IF AVAIL br-es-ticket THEN 
                                ASSIGN tt-extrato.rend-inteiro = br-es-ticket.rend-inteiro
                                       tt-extrato.rend-quebr   = br-es-ticket.rend-quebr.
             
                END.
            END.
        END. /*if avail emitente*/
    END.

    bloco-movto2:
    FOR EACH es-movto-arroz NO-LOCK 
        WHERE es-movto-arroz.nr-contrato   = es-contrato-arroz.nr-contrato
          AND (es-movto-arroz.esp-docto     = "DSC" OR es-movto-arroz.esp-docto = "BON" OR es-movto-arroz.esp-docto = "SEC") USE-INDEX ch-esp,
           FIRST es-ticket NO-LOCK
                WHERE es-ticket.nr-ticket = es-movto-arroz.nr-ticket
                BREAK BY es-movto-arroz.cod-emitente
                      BY es-movto-arroz.esp-docto   
                      BY es-movto-arroz.dt-trans
                      BY es-movto-arroz.nr-contrato:

        IF es-movto-arroz.dt-trans < es-contrato-arroz.dt-contrato THEN NEXT bloco-movto2.

        FIND FIRST tt-extrato
            WHERE tt-extrato.cod-emitente = emitente.cod-emitente
              AND tt-extrato.nr-contrato  = es-movto-arroz.nr-contrato
              AND tt-extrato.nr-ticket    = es-movto-arroz.nr-ticket  
              AND tt-extrato.esp-docto    = c-esp-docto
            NO-ERROR.
        
          IF  AVAIL tt-extrato THEN DO:
            
            /*** DESCONTOS ***/
            IF  es-movto-arroz.esp-docto = "DSC" THEN DO:
                
                    /* Umidade */
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*UMIDADE*")
                        NO-ERROR.
                    
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-umidade   = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*UMIDADE*") THEN
                        ASSIGN tt-extrato.desc-umidade = tt-extrato.desc-umidade + es-movto-arroz.quantidade. /*13/01/2004 */ 
                
                    /* Impureza */
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*IMPUREZA*")
                        NO-ERROR.
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-impureza   = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*IMPUREZA*") THEN
                        ASSIGN tt-extrato.desc-impureza = tt-extrato.desc-impureza + es-movto-arroz.quantidade.  /*13/01/2004 */ 
                
                    ASSIGN tt-extrato.rend-inteiro    = es-ticket.rend-inteiro
                           tt-extrato.rend-quebr      = es-ticket.rend-quebr.

            END.
        END.
    END.
END.
