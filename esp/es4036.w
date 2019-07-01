&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
   mgadm            PROGRESS
   mgdis            PROGRESS
   ECCBM - Joca/Waldemar tratamento EFD - PIS/COFINS - 28/03/2011           
*/
&Scoped-define WINDOW-NAME wMasterDetail

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-wt-docto NO-UNDO LIKE wt-docto
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-wt-fat-duplic NO-UNDO LIKE wt-fat-duplic
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-wt-fat-repre NO-UNDO LIKE wt-fat-repre
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-wt-it-docto NO-UNDO LIKE wt-it-docto
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-wt-nota-embal NO-UNDO LIKE wt-nota-embal
       field r-rowid as rowid.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMasterDetail 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
** perfomance (n∆o foi mexido)
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ES4036 2.09.00.001 } /*** "019001" ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
** performance (n∆o mexi)
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program          ES4036
&GLOBAL-DEFINE Version        2.09.00.000

&GLOBAL-DEFINE Folder           YES
&GLOBAL-DEFINE InitialPage      1

&GLOBAL-DEFINE FolderLabels     Itens,Repres,Duplicatas,Embalagens

&GLOBAL-DEFINE First            YES
&GLOBAL-DEFINE Prev             YES
&GLOBAL-DEFINE Next             YES
&GLOBAL-DEFINE Last             YES
&GLOBAL-DEFINE GoTo             YES
&GLOBAL-DEFINE Search           YES

&GLOBAL-DEFINE AddParent        YES
&GLOBAL-DEFINE CopyParent       NO
&GLOBAL-DEFINE UpdateParent     YES
&GLOBAL-DEFINE DeleteParent     YES

&GLOBAL-DEFINE AddSon1          YES
&GLOBAL-DEFINE CopySon1         NO
&GLOBAL-DEFINE UpdateSon1       YES
&GLOBAL-DEFINE DeleteSon1       YES

&GLOBAL-DEFINE AddSon2          YES
&GLOBAL-DEFINE CopySon2         NO
&GLOBAL-DEFINE UpdateSon2       YES
&GLOBAL-DEFINE DeleteSon2       YES

&GLOBAL-DEFINE AddSon3          YES
&GLOBAL-DEFINE CopySon3         NO
&GLOBAL-DEFINE UpdateSon3       YES
&GLOBAL-DEFINE DeleteSon3       YES

&GLOBAL-DEFINE AddSon4          NO
&GLOBAL-DEFINE CopySon4         NO
&GLOBAL-DEFINE UpdateSon4       NO
&GLOBAL-DEFINE DeleteSon4       NO

&GLOBAL-DEFINE ttParent         tt-wt-docto
&GLOBAL-DEFINE hDBOParent       h-bodi317
&GLOBAL-DEFINE DBOParentTable   wt-docto

&GLOBAL-DEFINE ttSon1           tt-wt-it-docto
&GLOBAL-DEFINE hDBOSon1         h-bodi321
&GLOBAL-DEFINE DBOSon1Table     wt-it-docto

&GLOBAL-DEFINE ttSon2           tt-wt-fat-repre
&GLOBAL-DEFINE hDBOSon2         h-bodi319
&GLOBAL-DEFINE DBOSon2Table     wt-fat-repre

&GLOBAL-DEFINE ttSon3           tt-wt-fat-duplic
&GLOBAL-DEFINE hDBOSon3         h-bodi318
&GLOBAL-DEFINE DBOSon3Table     wt-fat-duplic

&GLOBAL-DEFINE ttSon4           tt-wt-nota-embal
&GLOBAL-DEFINE hDBOSon4         h-bodi325
&GLOBAL-DEFINE DBOSon4Table     wt-nota-embal

&GLOBAL-DEFINE page0Fields      tt-wt-docto.seq-wt-docto ~
                                tt-wt-docto.cod-estabel ~
                                tt-wt-docto.serie ~
                                tt-wt-docto.nome-abrev ~
                                tt-wt-docto.nat-operacao ~
                                tt-wt-docto.dt-emis-nota

&GLOBAL-DEFINE page1Browse      brSon1
&GLOBAL-DEFINE page2Browse      brSon2
&GLOBAL-DEFINE page3Browse      brSon3
&GLOBAL-DEFINE page4Browse      brSon4

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var h-bodi317pr                as handle  no-undo.
def var h-bodi317sd                as handle  no-undo.
def var h-bodi317im1bra            as handle  no-undo.
def var h-bodi317va                as handle  no-undo.
def var h-bodi317in                as handle  no-undo.
def var l-proc-ok-aux              as log     no-undo.
def var c-ultimo-metodo-exec       as char    no-undo.
def var i-cont                     as int     no-undo.
def var de-vl-tot-nota             as dec     no-undo.
def var l-possui-duplicatas        as log     no-undo.
def var i-seq-wt-docto-aux-vl-tot-nota as int no-undo.
def var inrocasas                  as int     no-undo.
def var i                          as int     no-undo.
def var moeda                      like wt-docto.mo-codigo no-undo.
def var c-format                   as char    no-undo initial "z".
def var l-nf-man-dev-terc-dif      as log     no-undo.
def var l-recal-apenas-totais      as log     no-undo.
DEF VAR c-mensagem-cfop            AS CHAR    NO-UNDO.

define temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)".

/*** Descontos e Bonificaá‰es ***/
def buffer b-ped-venda for ped-venda.
def var l-retorno            as log    no-undo.
def var c-nome-abrev-bonif   as char   no-undo.
def var c-num-pedido-bonif   as char   no-undo.
def var i-nr-pedcli-venda    as char   no-undo.
def var i-nome-abrev-venda   as char   no-undo.
def var h-ft4002             as handle no-undo.

/* 12/02/04 - passar cod-estabel do tt-wt-docto como parametro para es4036z.w */
DEF NEW GLOBAL SHARED VAR c-cod-estabel-esapi007a-36 LIKE estabelec.cod-estabel.
/* 12/02/04 - passar cod-emitente do tt-wt-docto como parametro para es4036z.w */
DEF NEW GLOBAL SHARED VAR c-cod-emitente-esapi007a-36 LIKE emitente.cod-emitente.
def NEW GLOBAL shared var i-nr-contrato-4036 like es-contrato-arroz.nr-contrato no-undo. /* 280404 */ 
def NEW GLOBAL shared var c-it-codigo-4036 like es-item.it-codigo NO-UNDO. /* 191005 */ 

DEF NEW GLOBAL SHARED VAR dt-trans-esapi007a-36 AS DATE. /* 011206 */ 


{esp/esbodi317ef.i1} /* Definiá∆o da temp-table tt-notas-geradas */
{ftp/ft4015.i1}     /* Definiá∆o da temp-table tt-documentos    */
{scbo/bosc008calc.i} /* Definiá∆o da temp-table tt-ret-calculo (Fretes) */
{cdp/cdcfgdis.i}   


/* Temp-table auxiliar para impress∆o autom†tica, para que n∆o haja nescessidade de se 
   criar um novo programa de impress∆o autom†tica. Ser† utilizado o programa antigo */
def temp-table tt-notas-geradas-impressao no-undo
    field rw-nota-fiscal as   rowid
    field nr-nota        like nota-fiscal.nr-nota-fis.


/* Local Variable Definitions (DBOs Handles) ---                        */
DEFINE VARIABLE {&hDBOParent} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon1}   AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon2}   AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon3}   AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon4}   AS HANDLE NO-UNDO.

/***Esse find Ç necess†rio devido a problemas com o Browse2***/
find first tt-wt-docto no-error.

def buffer b-tt-wt-it-docto for tt-wt-it-docto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MasterDetail
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fPage0
&Scoped-define BROWSE-NAME brSon1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-wt-it-docto tt-wt-fat-repre repres ~
tt-wt-fat-duplic tt-wt-nota-embal embalag

/* Definitions for BROWSE brSon1                                        */
&Scoped-define FIELDS-IN-QUERY-brSon1 (if avail tt-wt-docto and (tt-wt-docto.esp-ped = 1 or tt-wt-it-docto.nr-entrega = 0) then tt-wt-it-docto.nr-sequencia else 0 ) @ tt-wt-it-docto.nr-sequencia (if avail tt-wt-docto and (tt-wt-docto.esp-ped = 1 or tt-wt-it-docto.nr-entrega = 0) then tt-wt-it-docto.it-codigo else "Entrega: " + string(tt-wt-it-docto.nr-entrega)) @ tt-wt-it-docto.it-codigo tt-wt-it-docto.cod-refer tt-wt-it-docto.nat-operacao /*** M£ltiplas Unidades de Medida ***/ &IF DEFINED(BF_DIS_VERSAO_EMS) &THEN &IF '{&BF_DIS_VERSAO_EMS}' >= '2.042' &THEN tt-wt-it-docto.quantidade[2] tt-wt-it-docto.un[2] &ELSE (if not tt-wt-it-docto.fat-qtfam then tt-wt-it-docto.quantidade[1] else tt-wt-it-docto.quantidade[2]) @ tt-wt-it-docto.quantidade[1] (if not tt-wt-it-docto.fat-qtfam then tt-wt-it-docto.un[1] else tt-wt-it-docto.un[2]) @ tt-wt-it-docto.un[1] &ENDIF &ELSE (if not tt-wt-it-docto.fat-qtfam then tt-wt-it-docto.quantidade[1] else tt-wt-it-docto.quantidade[2]) @ tt-wt-it-docto.quantidade[1] (if not tt-wt-it-docto.fat-qtfam then tt-wt-it-docto.un[1] else tt-wt-it-docto.un[2]) @ tt-wt-it-docto.un[1] &ENDIF tt-wt-it-docto.vl-preuni tt-wt-it-docto.vl-merc-liq   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon1   
&Scoped-define SELF-NAME brSon1
&Scoped-define OPEN-QUERY-brSon1 OPEN QUERY {&SELF-NAME} FOR EACH tt-wt-it-docto NO-LOCK     BY tt-wt-it-docto.nr-sequencia     BY tt-wt-it-docto.nr-entrega.
&Scoped-define TABLES-IN-QUERY-brSon1 tt-wt-it-docto
&Scoped-define FIRST-TABLE-IN-QUERY-brSon1 tt-wt-it-docto


/* Definitions for BROWSE brSon2                                        */
&Scoped-define FIELDS-IN-QUERY-brSon2 tt-wt-fat-repre.cod-rep ~
fnNomeRepre() @ tt-wt-fat-repre.char-1 tt-wt-fat-repre.perc-comis ~
tt-wt-fat-repre.comis-emis 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon2 
&Scoped-define OPEN-QUERY-brSon2 OPEN QUERY brSon2 FOR EACH tt-wt-fat-repre NO-LOCK, ~
      EACH repres OF tt-wt-fat-repre NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSon2 tt-wt-fat-repre repres
&Scoped-define FIRST-TABLE-IN-QUERY-brSon2 tt-wt-fat-repre
&Scoped-define SECOND-TABLE-IN-QUERY-brSon2 repres


/* Definitions for BROWSE brSon3                                        */
&Scoped-define FIELDS-IN-QUERY-brSon3 tt-wt-fat-duplic.parcela ~
tt-wt-fat-duplic.cod-vencto tt-wt-fat-duplic.dt-venciment ~
tt-wt-fat-duplic.dt-desconto tt-wt-fat-duplic.vl-desconto ~
tt-wt-fat-duplic.vl-parcela tt-wt-fat-duplic.vl-comis 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon3 
&Scoped-define OPEN-QUERY-brSon3 OPEN QUERY brSon3 FOR EACH tt-wt-fat-duplic NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSon3 tt-wt-fat-duplic
&Scoped-define FIRST-TABLE-IN-QUERY-brSon3 tt-wt-fat-duplic


/* Definitions for BROWSE brSon4                                        */
&Scoped-define FIELDS-IN-QUERY-brSon4 tt-wt-nota-embal.sigla-emb ~
embalag.descricao tt-wt-nota-embal.qt-volumes tt-wt-nota-embal.desc-vol 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon4 
&Scoped-define OPEN-QUERY-brSon4 OPEN QUERY brSon4 FOR EACH tt-wt-nota-embal NO-LOCK, ~
      EACH embalag OF tt-wt-nota-embal NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSon4 tt-wt-nota-embal embalag
&Scoped-define FIRST-TABLE-IN-QUERY-brSon4 tt-wt-nota-embal
&Scoped-define SECOND-TABLE-IN-QUERY-brSon4 embalag


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brSon1}

/* Definitions for FRAME fPage2                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage2 ~
    ~{&OPEN-QUERY-brSon2}

/* Definitions for FRAME fPage3                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage3 ~
    ~{&OPEN-QUERY-brSon3}

/* Definitions for FRAME fPage4                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage4 ~
    ~{&OPEN-QUERY-brSon4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-wt-docto.seq-wt-docto ~
tt-wt-docto.nome-abrev tt-wt-docto.nat-operacao tt-wt-docto.cod-estabel ~
tt-wt-docto.serie tt-wt-docto.dt-emis-nota 
&Scoped-define ENABLED-TABLES tt-wt-docto
&Scoped-define FIRST-ENABLED-TABLE tt-wt-docto
&Scoped-define DISPLAYED-TABLES tt-wt-docto
&Scoped-define FIRST-DISPLAYED-TABLE tt-wt-docto
&Scoped-Define ENABLED-OBJECTS btFirst btPrev btNext btLast btGoTo btSearch ~
btAdd btUpdate btDelete btCalcula BtSimula btRecalcula btQueryJoins ~
btReportsJoins btExit btHelp c-tipo-nota rtParent rtToolBar 
&Scoped-Define DISPLAYED-FIELDS tt-wt-docto.seq-wt-docto ~
tt-wt-docto.nome-abrev tt-wt-docto.nat-operacao tt-wt-docto.cod-estabel ~
tt-wt-docto.serie tt-wt-docto.dt-emis-nota 
&Scoped-Define DISPLAYED-OBJECTS c-tipo-nota 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnNomeRepre wMasterDetail 
FUNCTION fnNomeRepre RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

   
/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMasterDetail AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM miFirst        LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM miPrev         LABEL "&Anterior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM miNext         LABEL "&Pr¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM miLast         LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       RULE
       MENU-ITEM miGoTo         LABEL "&V† Para"       ACCELERATOR "CTRL-T"
       MENU-ITEM miSearch       LABEL "&Pesquisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM miAdd          LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM miUpdate       LABEL "&Alterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM miDelete       LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
       RULE
       MENU-ITEM m_Clculo_da_Nota LABEL "C†lculo da &Nota" ACCELERATOR "CTRL-Q"
       MENU-ITEM m_Simulao_do_Clculo LABEL "Si&mulaá∆o do C†lculo" ACCELERATOR "CTRL-W"
       MENU-ITEM m_Reclculo_de_Impostos LABEL "Rec†lculo de Impostos" ACCELERATOR "CTRL-R"
       RULE
       MENU-ITEM miQueryJoins   LABEL "&Consultas"    
       MENU-ITEM miReportsJoins LABEL "&Relat¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       RULE
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  smFile         LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "&Ajuda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btAdd 
     IMAGE-UP FILE "image\im-add":U
     IMAGE-INSENSITIVE FILE "image\ii-add":U
     LABEL "Add" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btCalcula 
     IMAGE-UP FILE "image\im-calc4":U
     IMAGE-INSENSITIVE FILE "image\ii-calc4":U
     LABEL "C†lculo da Nota" 
     SIZE 4 BY 1.25 TOOLTIP "C†lculo da nota"
     FONT 4.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "image\im-era":U
     IMAGE-INSENSITIVE FILE "image\ii-era":U
     LABEL "Delete" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "image\im-fir":U
     IMAGE-INSENSITIVE FILE "image\ii-fir":U
     LABEL "First":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btGoTo 
     IMAGE-UP FILE "image\im-enter":U
     IMAGE-INSENSITIVE FILE "image\ii-enter":U
     LABEL "Go To" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image\im-hel":U
     IMAGE-INSENSITIVE FILE "image\ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btLast 
     IMAGE-UP FILE "image\im-las":U
     IMAGE-INSENSITIVE FILE "image\ii-las":U
     LABEL "Last":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btNext 
     IMAGE-UP FILE "image\im-nex":U
     IMAGE-INSENSITIVE FILE "image\ii-nex":U
     LABEL "Next":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btPrev 
     IMAGE-UP FILE "image\im-pre":U
     IMAGE-INSENSITIVE FILE "image\ii-pre":U
     LABEL "Prev":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image\im-joi":U
     IMAGE-INSENSITIVE FILE "image\ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btRecalcula 
     IMAGE-UP FILE "image\im-pcust":U
     IMAGE-INSENSITIVE FILE "image\ii-pcust":U
     LABEL "Rec†lculo de Impostos" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image\im-pri":U
     IMAGE-INSENSITIVE FILE "image\ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSearch 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "Search" 
     SIZE 4 BY 1.25.

DEFINE BUTTON BtSimula 
     IMAGE-UP FILE "image\im-orct1":U
     IMAGE-INSENSITIVE FILE "image\ii-orcto":U
     LABEL "Simulaá∆o do C†lculo" 
     SIZE 4 BY 1.25 TOOLTIP "Simulaá∆o do c†lculo da nota."
     FONT 4.

DEFINE BUTTON btUpdate 
     IMAGE-UP FILE "image\im-mod":U
     IMAGE-INSENSITIVE FILE "image\ii-mod":U
     LABEL "Update" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE VARIABLE c-tipo-nota AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo da Nota" 
     VIEW-AS FILL-IN 
     SIZE 22.29 BY .88 NO-UNDO.

DEFINE RECTANGLE rtParent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 3.5.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.5
     BGCOLOR 7 .

DEFINE BUTTON btAddSon1 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON btAtendPed 
     LABEL "Atend &Ped" 
     SIZE 10 BY 1.

DEFINE BUTTON btATendSeq 
     LABEL "Atend &Seq" 
     SIZE 10 BY 1.

DEFINE BUTTON btDeleteSon1 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON btEstrutura 
     LABEL "Es&trutura" 
     SIZE 10 BY 1.

DEFINE BUTTON btGeraItens 
     LABEL "&Gera ÷tens" 
     SIZE 10 BY 1.

DEFINE BUTTON btInfFiscais 
     LABEL "Inf &Fiscais" 
     SIZE 10 BY 1.

DEFINE BUTTON btUpdateSon1 
     LABEL "&Alterar" 
     SIZE 10 BY 1.

DEFINE BUTTON btAddSon2 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON btDeleteSon2 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON btUpdateSon2 
     LABEL "&Alterar" 
     SIZE 10 BY 1.

DEFINE BUTTON btAddSon3 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON btCalcNota 
     LABEL "&Recalcula Total Nota" 
     SIZE 20 BY 1.

DEFINE BUTTON btDeleteSon3 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON btUpdateSon3 
     LABEL "&Alterar" 
     SIZE 10 BY 1.

DEFINE BUTTON btGeraAuto 
     LABEL "&Geraá∆o Autom†tica" 
     SIZE 16 BY 1.

DEFINE BUTTON btManutencaoEmb 
     LABEL "&Manutená∆o" 
     SIZE 16 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brSon1 FOR 
      tt-wt-it-docto SCROLLING.

DEFINE QUERY brSon2 FOR 
      tt-wt-fat-repre, 
      repres SCROLLING.

DEFINE QUERY brSon3 FOR 
      tt-wt-fat-duplic SCROLLING.

DEFINE QUERY brSon4 FOR 
      tt-wt-nota-embal, 
      embalag SCROLLING.
&ANALYZE-RESUME
    
/* Browse definitions                                                   */
DEFINE BROWSE brSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon1 wMasterDetail _FREEFORM
  QUERY brSon1 NO-LOCK DISPLAY
      (if avail tt-wt-docto and
          (tt-wt-docto.esp-ped       = 1
       or tt-wt-it-docto.nr-entrega = 0)
       then tt-wt-it-docto.nr-sequencia
       else 0 ) 
      @ tt-wt-it-docto.nr-sequencia format ">>,>>>":U

      (if avail tt-wt-docto and
         (tt-wt-docto.esp-ped       = 1
       or tt-wt-it-docto.nr-entrega = 0)
       then tt-wt-it-docto.it-codigo
       else "Entrega: " + string(tt-wt-it-docto.nr-entrega)) 
      @ tt-wt-it-docto.it-codigo

      tt-wt-it-docto.cod-refer
      tt-wt-it-docto.nat-operacao

      /*** M£ltiplas Unidades de Medida ***/
      &IF DEFINED(BF_DIS_VERSAO_EMS) &THEN
           &IF '{&BF_DIS_VERSAO_EMS}' >= '2.042' &THEN
           
                tt-wt-it-docto.quantidade[2]
                tt-wt-it-docto.un[2]

           &ELSE

                (if not tt-wt-it-docto.fat-qtfam 
                 then tt-wt-it-docto.quantidade[1]
                 else tt-wt-it-docto.quantidade[2]) 
                @ tt-wt-it-docto.quantidade[1]
          
                (if not tt-wt-it-docto.fat-qtfam 
                 then tt-wt-it-docto.un[1]
                 else tt-wt-it-docto.un[2]) 
                @ tt-wt-it-docto.un[1]

           &ENDIF
       &ELSE

            (if not tt-wt-it-docto.fat-qtfam 
             then tt-wt-it-docto.quantidade[1]
             else tt-wt-it-docto.quantidade[2]) 
            @ tt-wt-it-docto.quantidade[1]
          
            (if not tt-wt-it-docto.fat-qtfam 
             then tt-wt-it-docto.un[1]
             else tt-wt-it-docto.un[2]) 
            @ tt-wt-it-docto.un[1]
       &ENDIF

      tt-wt-it-docto.vl-preuni
      tt-wt-it-docto.vl-merc-liq
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 83.14 BY 7.79
         FONT 2.

DEFINE BROWSE brSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon2 wMasterDetail _STRUCTURED
  QUERY brSon2 NO-LOCK DISPLAY
      tt-wt-fat-repre.cod-rep FORMAT ">>>>9":U
      fnNomeRepre() @ tt-wt-fat-repre.char-1 COLUMN-LABEL "Nome Abreviado" FORMAT "x(50)":U
      tt-wt-fat-repre.perc-comis FORMAT ">>9.9999999999":U
      tt-wt-fat-repre.comis-emis FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83.14 BY 7.79
         FONT 2.

DEFINE BROWSE brSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon3 wMasterDetail _STRUCTURED
  QUERY brSon3 NO-LOCK DISPLAY
      tt-wt-fat-duplic.parcela FORMAT "99":U
      tt-wt-fat-duplic.cod-vencto FORMAT "99":U
      tt-wt-fat-duplic.dt-venciment FORMAT "99/99/9999":U
      tt-wt-fat-duplic.dt-desconto FORMAT "99/99/9999":U
      tt-wt-fat-duplic.vl-desconto FORMAT ">>>,>>>,>>9.99999":U
      tt-wt-fat-duplic.vl-parcela FORMAT ">>>,>>>,>>9.99999":U
      tt-wt-fat-duplic.vl-comis FORMAT ">>>,>>>,>>9.99999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83.14 BY 7.79
         FONT 2.

DEFINE BROWSE brSon4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon4 wMasterDetail _STRUCTURED
  QUERY brSon4 NO-LOCK DISPLAY
      tt-wt-nota-embal.sigla-emb FORMAT "!xx":U
      embalag.descricao FORMAT "x(30)":U
      tt-wt-nota-embal.qt-volumes FORMAT ">>>,>>9":U
      tt-wt-nota-embal.desc-vol FORMAT "x(76)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83.14 BY 7.79
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fPage0
     btFirst AT ROW 1.13 COL 1.57 HELP
          "Primeira ocorrància"
     btPrev AT ROW 1.13 COL 5.57 HELP
          "Ocorrància anterior"
     btNext AT ROW 1.13 COL 9.57 HELP
          "Pr¢xima ocorrància"
     btLast AT ROW 1.13 COL 13.57 HELP
          "Èltima ocorrància"
     btGoTo AT ROW 1.13 COL 17.57 HELP
          "V† Para"
     btSearch AT ROW 1.13 COL 21.57 HELP
          "Pesquisa"
     btAdd AT ROW 1.13 COL 31 HELP
          "Inclui nova ocorrància"
     btUpdate AT ROW 1.13 COL 35 HELP
          "Altera ocorrància corrente"
     btDelete AT ROW 1.13 COL 39 HELP
          "Elimina ocorrància corrente"
     btCalcula AT ROW 1.13 COL 48.43 HELP
          "Elimina ocorrància corrente"
     BtSimula AT ROW 1.13 COL 52.43 HELP
          "Elimina ocorrància corrente"
     btRecalcula AT ROW 1.13 COL 56.43 HELP
          "Rec†lculo dos impostos da nota fiscal"
     btQueryJoins AT ROW 1.13 COL 74.86 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.86 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.86 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.86 HELP
          "Ajuda"
     tt-wt-docto.seq-wt-docto AT ROW 3 COL 18 COLON-ALIGNED
          LABEL "Sequància"
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     tt-wt-docto.nome-abrev AT ROW 3 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     c-tipo-nota AT ROW 4 COL 18 COLON-ALIGNED
     tt-wt-docto.nat-operacao AT ROW 4 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     tt-wt-docto.cod-estabel AT ROW 5 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-wt-docto.serie AT ROW 5 COL 34 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.29 BY .88
     tt-wt-docto.dt-emis-nota AT ROW 5 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.72 BY .88
     rtParent AT ROW 2.67 COL 1
     rtToolBar AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.75
         FONT 1.

DEFINE FRAME fPage3
     brSon3 AT ROW 1.54 COL 1.72
     btAddSon3 AT ROW 9.38 COL 1.86
     btUpdateSon3 AT ROW 9.38 COL 11.86
     btDeleteSon3 AT ROW 9.38 COL 21.86
     btCalcNota AT ROW 9.38 COL 31.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.5
         SIZE 84.43 BY 9.75
         FONT 1.

DEFINE FRAME fPage4
     brSon4 AT ROW 1.54 COL 1.72
     btManutencaoEmb AT ROW 9.38 COL 1.86
     btGeraAuto AT ROW 9.38 COL 17.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.5
         SIZE 84.43 BY 9.75
         FONT 1.

DEFINE FRAME fPage2
     brSon2 AT ROW 1.54 COL 1.72
     btAddSon2 AT ROW 9.38 COL 1.86
     btUpdateSon2 AT ROW 9.38 COL 11.86
     btDeleteSon2 AT ROW 9.38 COL 21.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.5
         SIZE 84.43 BY 9.75
         FONT 1.

DEFINE FRAME fPage1
     brSon1 AT ROW 1.54 COL 1.72
     btAddSon1 AT ROW 9.38 COL 1.86
     btUpdateSon1 AT ROW 9.38 COL 11.86
     btDeleteSon1 AT ROW 9.38 COL 21.86
     btATendSeq AT ROW 9.38 COL 34.72
     btAtendPed AT ROW 9.38 COL 44.72
     btEstrutura AT ROW 9.38 COL 54.72
     btGeraItens AT ROW 9.38 COL 64.72
     btInfFiscais AT ROW 9.38 COL 74.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.5
         SIZE 84.43 BY 9.75
         FONT 1.




/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MasterDetail
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-wt-docto T "?" NO-UNDO mgdis wt-docto
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-wt-fat-duplic T "?" NO-UNDO mgdis wt-fat-duplic
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-wt-fat-repre T "?" NO-UNDO mgdis wt-fat-repre
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-wt-it-docto T "?" NO-UNDO mgdis wt-it-docto
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-wt-nota-embal T "?" NO-UNDO mgdis wt-nota-embal
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wMasterDetail ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 90.29
         MAX-HEIGHT         = 18
         MAX-WIDTH          = 91.29
         VIRTUAL-HEIGHT     = 18
         VIRTUAL-WIDTH      = 91.29
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU mbMain:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wMasterDetail 
/* ************************* Included-Libraries *********************** */

{masterdetail/masterdetail.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wMasterDetail
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fPage0:HANDLE
       FRAME fPage2:FRAME = FRAME fPage0:HANDLE
       FRAME fPage3:FRAME = FRAME fPage0:HANDLE
       FRAME fPage4:FRAME = FRAME fPage0:HANDLE.

/* SETTINGS FOR FRAME fPage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN tt-wt-docto.seq-wt-docto IN FRAME fPage0
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brSon1 1 fPage1 */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* BROWSE-TAB brSon2 1 fPage2 */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* BROWSE-TAB brSon3 1 fPage3 */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* BROWSE-TAB brSon4 1 fPage4 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMasterDetail)
THEN wMasterDetail:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon1
/* Query rebuild information for BROWSE brSon1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-wt-it-docto NO-LOCK
    BY tt-wt-it-docto.nr-sequencia
    BY tt-wt-it-docto.nr-entrega.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tt-wt-it-docto.nr-sequencia|yes,Temp-Tables.tt-wt-it-docto.it-codigo|yes"
     _Query            is OPENED
*/  /* BROWSE brSon1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon2
/* Query rebuild information for BROWSE brSon2
     _TblList          = "Temp-Tables.tt-wt-fat-repre,mgadm.repres OF Temp-Tables.tt-wt-fat-repre"
     _Options          = "NO-LOCK"
     _TblOptList       = ","
     _FldNameList[1]   = Temp-Tables.tt-wt-fat-repre.cod-rep
     _FldNameList[2]   > "_<CALC>"
"fnNomeRepre() @ tt-wt-fat-repre.char-1" "Nome Abreviado" "x(50)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.tt-wt-fat-repre.perc-comis
     _FldNameList[4]   = Temp-Tables.tt-wt-fat-repre.comis-emis
     _Query            is OPENED
*/  /* BROWSE brSon2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon3
/* Query rebuild information for BROWSE brSon3
     _TblList          = "Temp-Tables.tt-wt-fat-duplic"
     _Options          = "NO-LOCK"
     _TblOptList       = ","
     _FldNameList[1]   > Temp-Tables.tt-wt-fat-duplic.parcela
"tt-wt-fat-duplic.parcela" ? "99" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.tt-wt-fat-duplic.cod-vencto
     _FldNameList[3]   = Temp-Tables.tt-wt-fat-duplic.dt-venciment
     _FldNameList[4]   = Temp-Tables.tt-wt-fat-duplic.dt-desconto
     _FldNameList[5]   = Temp-Tables.tt-wt-fat-duplic.vl-desconto
     _FldNameList[6]   = Temp-Tables.tt-wt-fat-duplic.vl-parcela
     _FldNameList[7]   = Temp-Tables.tt-wt-fat-duplic.vl-comis
     _Query            is OPENED
*/  /* BROWSE brSon3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon4
/* Query rebuild information for BROWSE brSon4
     _TblList          = "Temp-Tables.tt-wt-nota-embal,mgdis.embalag OF Temp-Tables.tt-wt-nota-embal"
     _Options          = "NO-LOCK"
     _TblOptList       = ","
     _FldNameList[1]   = Temp-Tables.tt-wt-nota-embal.sigla-emb
     _FldNameList[2]   = mgdis.embalag.descricao
     _FldNameList[3]   = Temp-Tables.tt-wt-nota-embal.qt-volumes
     _FldNameList[4]   = Temp-Tables.tt-wt-nota-embal.desc-vol
     _Query            is OPENED
*/  /* BROWSE brSon4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage0
/* Query rebuild information for FRAME fPage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage3
/* Query rebuild information for FRAME fPage3
     _Query            is NOT OPENED
*/  /* FRAME fPage3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage4
/* Query rebuild information for FRAME fPage4
     _Query            is NOT OPENED
*/  /* FRAME fPage4 */
&ANALYZE-RESUME

 

/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wMasterDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMasterDetail wMasterDetail
ON END-ERROR OF wMasterDetail
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMasterDetail wMasterDetail
ON WINDOW-CLOSE OF wMasterDetail
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brSon1
&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME brSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSon1 wMasterDetail
ON ROW-DISPLAY OF brSon1 IN FRAME fPage1
DO:
  

    if  tt-wt-it-docto.nr-pedcli <> "" then 
        if  tt-wt-it-docto.calcula then 
            assign tt-wt-it-docto.nr-sequencia:fgcolor  in browse brSon1 = 9
                   tt-wt-it-docto.it-codigo:fgcolor     in browse brSon1 = 9
                   tt-wt-it-docto.cod-refer:fgcolor     in browse brSon1 = 9
                   tt-wt-it-docto.nat-operacao:fgcolor  in browse brSon1 = 9
                   &IF DEFINED(BF_DIS_VERSAO_EMS) &THEN
                        &IF '{&BF_DIS_VERSAO_EMS}' >= '2.042' &THEN
                            tt-wt-it-docto.quantidade[2]:fgcolor in browse brSon1 = 9
                            tt-wt-it-docto.un[2]:fgcolor         in browse brSon1 = 9
                        &ELSE
                            tt-wt-it-docto.quantidade[1]:fgcolor in browse brSon1 = 9
                            tt-wt-it-docto.un[1]:fgcolor         in browse brSon1 = 9                        
                        &ENDIF
                    &ELSE
                        tt-wt-it-docto.quantidade[1]:fgcolor in browse brSon1 = 9
                        tt-wt-it-docto.un[1]:fgcolor         in browse brSon1 = 9
                    &ENDIF
                   tt-wt-it-docto.vl-preuni:fgcolor     in browse brSon1 = 9
                   tt-wt-it-docto.vl-merc-liq:fgcolor   in browse brSon1 = 9
                   tt-wt-it-docto.nr-sequencia:font     in browse brSon1 = 6
                   tt-wt-it-docto.it-codigo:font        in browse brSon1 = 6
                   tt-wt-it-docto.cod-refer:font        in browse brSon1 = 6
                   tt-wt-it-docto.nat-operacao:font     in browse brSon1 = 6
                   &IF DEFINED(BF_DIS_VERSAO_EMS) &THEN
                        &IF '{&BF_DIS_VERSAO_EMS}' >= '2.042' &THEN
                            tt-wt-it-docto.quantidade[2]:font    in browse brSon1 = 6
                            tt-wt-it-docto.un[2]:font            in browse brSon1 = 6
                        &ELSE
                            tt-wt-it-docto.quantidade[1]:font    in browse brSon1 = 6
                            tt-wt-it-docto.un[1]:font            in browse brSon1 = 6
                        &ENDIF
                    &ELSE
                        tt-wt-it-docto.quantidade[1]:font    in browse brSon1 = 6
                        tt-wt-it-docto.un[1]:font            in browse brSon1 = 6
                    &ENDIF
                   tt-wt-it-docto.vl-preuni:font        in browse brSon1 = 6
                   tt-wt-it-docto.vl-merc-liq:font      in browse brSon1 = 6.

        else 
            assign tt-wt-it-docto.nr-sequencia:fgcolor  in browse brSon1 = ?
                   tt-wt-it-docto.it-codigo:fgcolor     in browse brSon1 = ?
                   tt-wt-it-docto.cod-refer:fgcolor     in browse brSon1 = ?
                   tt-wt-it-docto.nat-operacao:fgcolor  in browse brSon1 = ?
                   &IF DEFINED(BF_DIS_VERSAO_EMS) &THEN
                        &IF '{&BF_DIS_VERSAO_EMS}' >= '2.042' &THEN                   
                            tt-wt-it-docto.quantidade[2]:fgcolor in browse brSon1 = ?
                            tt-wt-it-docto.un[2]:fgcolor         in browse brSon1 = ?
                        &ELSE
                            tt-wt-it-docto.quantidade[1]:fgcolor in browse brSon1 = ?
                            tt-wt-it-docto.un[1]:fgcolor         in browse brSon1 = ?
                        &ENDIF
                   &ELSE
                        tt-wt-it-docto.quantidade[1]:fgcolor in browse brSon1 = ?
                        tt-wt-it-docto.un[1]:fgcolor         in browse brSon1 = ?
                   &ENDIF
                   tt-wt-it-docto.vl-preuni:fgcolor     in browse brSon1 = ?
                   tt-wt-it-docto.vl-merc-liq:fgcolor   in browse brSon1 = ?
                   tt-wt-it-docto.nr-sequencia:font     in browse brSon1 = ?
                   tt-wt-it-docto.it-codigo:font        in browse brSon1 = ?
                   tt-wt-it-docto.cod-refer:font        in browse brSon1 = ?
                   tt-wt-it-docto.nat-operacao:font     in browse brSon1 = ?
                   &IF DEFINED(BF_DIS_VERSAO_EMS) &THEN
                        &IF '{&BF_DIS_VERSAO_EMS}' >= '2.042' &THEN                   
                            tt-wt-it-docto.quantidade[2]:font    in browse brSon1 = ?
                            tt-wt-it-docto.un[2]:font            in browse brSon1 = ?
                        &ELSE
                            tt-wt-it-docto.quantidade[1]:font    in browse brSon1 = ?
                            tt-wt-it-docto.un[1]:font            in browse brSon1 = ?
                        &ENDIF
                   &ELSE
                        tt-wt-it-docto.quantidade[1]:font    in browse brSon1 = ?
                        tt-wt-it-docto.un[1]:font            in browse brSon1 = ?
                   &ENDIF
                   tt-wt-it-docto.vl-preuni:font        in browse brSon1 = ?
                   tt-wt-it-docto.vl-merc-liq:font      in browse brSon1 = ?.

    /*** Nove casas decimais - Integrada ***/
    if  can-find (funcao where
                  funcao.cd-funcao = "spp-nove-casas-dec" and
                  funcao.ativo     = yes                  and
                  funcao.log-1     = no) then do:

        def var i-nr-casas as int  no-undo.
        def var i-cont     as int  no-undo.
        def var c-formato  as char no-undo.

        find first para-fat no-lock.
        assign i-nr-casas = para-fat.dec-preuni-p.

        assign c-formato = ">>>,>>>,>>9.".
        do i-cont = 1 to i-nr-casas:
           assign c-formato = c-formato + "9".
        end.

        assign tt-wt-it-docto.vl-preuni:format in browse brSon1 = c-formato.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdd wMasterDetail
ON CHOOSE OF btAdd IN FRAME fPage0 /* Add */
OR  CHOOSE OF MENU-ITEM miAdd IN MENU mbMain DO:
    
/* mensagem retirada conforme solicitado no ticket 2355743 */		
	
	
    /* 22355743 */
     /*MESSAGE "Operaá‰es para o Sistema Arroz:" skip(1)
             "5949DC  - Devoluá∆o de Liquidaá∆o de Dep¢sito _ PF" SKIP
             "5949SB  - Devoluá∆o de Liquidaá∆o de Dep¢sito _ PJ" SKIP
             "5949MT  - Devoluá∆o para Mudanáa de Titularidade" SKIP
             "5949AF  - Devoluá∆o F°sica" SKIP
             "5949PJ  - Devolulá∆o Fiscal" SKIP
             "5949AS  - Devoluá∆o para Cobranáa de Serviáo de Secagem"
             VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "********** Naturezas a Utilizar **********".

    ASSIGN c-mensagem-cfop = "Operaá‰es para o Sistema Arroz: " + CHR(13).

    FOR EACH ext-natur-oper-cfop NO-LOCK:

        IF ext-natur-oper-cfop.dev-liq-deposito-pf = YES THEN
            ASSIGN c-mensagem-cfop = c-mensagem-cfop + ext-natur-oper-cfop.nat-operacao + " - Devoluá∆o de Liquidaá∆o de Dep¢sito - PF" + CHR(13).

        IF ext-natur-oper-cfop.dev-liq-deposito-pj = YES THEN
            ASSIGN c-mensagem-cfop = c-mensagem-cfop + ext-natur-oper-cfop.nat-operacao + " - Devoluá∆o de Liquidaá∆o de Dep¢sito - PJ" + CHR(13).

        IF ext-natur-oper-cfop.dev-mud-titularidade = YES THEN
            ASSIGN c-mensagem-cfop = c-mensagem-cfop + ext-natur-oper-cfop.nat-operacao + " - Devoluá∆o para Mudanáa de Titularidade" + CHR(13).

        IF ext-natur-oper-cfop.mudanca-fisica = YES THEN
            ASSIGN c-mensagem-cfop = c-mensagem-cfop + ext-natur-oper-cfop.nat-operacao + " - Devoluá∆o F°sica" + CHR(13).

        IF ext-natur-oper-cfop.devolucao-fiscal = YES THEN
            ASSIGN c-mensagem-cfop = c-mensagem-cfop + ext-natur-oper-cfop.nat-operacao + " - Devoluá∆o Fiscal" + CHR(13).

        IF ext-natur-oper-cfop.dev-cob-secagem = YES THEN
            ASSIGN c-mensagem-cfop = c-mensagem-cfop + ext-natur-oper-cfop.nat-operacao + " - Devoluá∆o para Cobranáa de Serviáo de Secagem" + CHR(13).
    END.

    MESSAGE c-mensagem-cfop
             VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "********** Naturezas a Utilizar **********". */

     RUN addRecord IN THIS-PROCEDURE (INPUT "ftp/ft4003a.w":U). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btAddSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddSon1 wMasterDetail
ON CHOOSE OF btAddSon1 IN FRAME fPage1 /* Incluir */
DO:
    def var h-program-itens as handle no-undo.

    if  not avail tt-wt-docto then
        return no-apply.

    ASSIGN dt-trans-esapi007a-36 = wt-docto.dt-trans.
    
    session:set-wait-state("general":U).
    run ftp/ft4004.w persistent set h-program-itens.
    if  valid-handle(h-program-itens) then do:
        run setParameters in h-program-itens (input tt-wt-docto.seq-wt-docto,
                                              input ?).
        run setHandleDBOs in h-program-itens (input this-procedure,
                                              input h-bodi317in,
                                              input h-bodi317sd,    
                                              input h-bodi317pr,    
                                              input h-bodi317im1bra, 
                                              input h-bodi317va).    
        run initializeInterface in h-program-itens.
        run applyChooseBtAdd in h-program-itens.
        
        session:set-wait-state("":U). 
 
        {&WINDOW-NAME}:sensitive = no.
        wait-for close of h-program-itens focus current-window.
    end.
   
    if  valid-handle(h-program-itens) then
        delete procedure h-program-itens.
    
    assign h-program-itens = ?.

    {&WINDOW-NAME}:sensitive = yes.
    session:set-wait-state("":U).
    
    APPLY 'ENTRY' TO {&WINDOW-NAME}.

    RUN openQueriesSon.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btAddSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddSon2 wMasterDetail
ON CHOOSE OF btAddSon2 IN FRAME fPage2 /* Incluir */
DO:
    {masterdetail/AddSon.i &ProgramSon="ftp/ft4008.w"
                           &PageNumber="2"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btAddSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddSon3 wMasterDetail
ON CHOOSE OF btAddSon3 IN FRAME fPage3 /* Incluir */
DO:
    run calculaTotalNota.
    {masterdetail/AddSon.i &ProgramSon="ftp/ft4006.w"
                           &PageNumber="3"}
    /* Seta o valor total da nota no programa onde s∆o informadas as duplicatas */
    RUN setparameters IN hSonProgram (input de-vl-tot-nota).                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btAtendPed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtendPed wMasterDetail
ON CHOOSE OF btAtendPed IN FRAME fPage1 /* Atend Ped */
DO:
    run emptyRowErrors           in h-bodi317in.
    run inicializaAcompanhamento in h-bodi317pr.
    run atendeTotalPed           in h-bodi317pr (input  tt-wt-docto.seq-wt-docto,
                                                 output l-proc-ok-aux).
    run finalizaAcompanhamento   in h-bodi317pr.
    run devolveErrosbodi317pr    in h-bodi317pr (output c-ultimo-metodo-exec,
                                                 output table RowErrors).
    find first RowErrors no-lock no-error.
    if  avail RowErrors then do:
        {method/ShowMessage.i1}
        {method/ShowMessage.i2 &Modal="yes"}
    end.

    RUN openQueriesSon.

    if  not l-proc-ok-aux then
        return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btATendSeq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btATendSeq wMasterDetail
ON CHOOSE OF btATendSeq IN FRAME fPage1 /* Atend Seq */
DO:
    if  avail tt-wt-it-docto then do:
        run inicializaAcompanhamento in h-bodi317pr.
        do  i-cont = 1 to brSon1:num-selected-rows in frame fPage1:
            if  brSon1:fetch-selected-row(i-cont) in frame fPage1
            and not tt-wt-it-docto.calcula then do:
                run emptyRowErrors           in h-bodi317in.
                run atendeTotalSeq           in h-bodi317pr (input  tt-wt-it-docto.seq-wt-docto,
                                                             input  tt-wt-it-docto.seq-wt-it-docto,
                                                             output l-proc-ok-aux).
                run devolveErrosbodi317pr    in h-bodi317pr (output c-ultimo-metodo-exec,
                                                             output table RowErrors).
                find first RowErrors no-lock no-error.
                if  avail RowErrors then do:
                    run finalizaAcompanhamento   in h-bodi317pr.
                    {method/ShowMessage.i1}
                    {method/ShowMessage.i1 &Modal="yes"}
                end.
            
                if  not l-proc-ok-aux then do:
                    run finalizaAcompanhamento   in h-bodi317pr.
                    brSon1:REFRESH() in frame fPage1.
                    return no-apply.
                end.
                
                run setConstraintDefault in {&hDBOSon1}.
                run openQueryStatic      in {&hDBOSon1} (input "Default":U).
                run goToKey              in {&hDBOSon1} (input tt-wt-it-docto.seq-wt-docto, input tt-wt-it-docto.seq-wt-it-docto).
                if  return-value = "OK":U then do:
                    run getLogField          in {&hDBOSon1} (input "calcula":U,     output tt-wt-it-docto.calcula).
                    run getDecField          in {&hDBOSon1} (input "vl-preuni":U,   output tt-wt-it-docto.vl-preuni).
                    run getDecField          in {&hDBOSon1} (input "vl-merc-liq":U, output tt-wt-it-docto.vl-merc-liq).
                end.
            end.
        end.
        run finalizaAcompanhamento   in h-bodi317pr.
        brSon1:REFRESH() in frame fPage1.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btCalcNota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCalcNota wMasterDetail
ON CHOOSE OF btCalcNota IN FRAME fPage3 /* Recalcula Total Nota */
DO:
    assign de-vl-tot-nota = 0.
    run calculaTotalNota.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btCalcula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCalcula wMasterDetail
ON CHOOSE OF btCalcula IN FRAME fPage0 /* C†lculo da Nota */
DO:
    if trans then do:

        run utp/ut-msgs.p(input "show",
                          input 27976,
                          input "").

        return no-apply.

    end.
    
    run calculaNota(input yes).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete wMasterDetail
ON CHOOSE OF btDelete IN FRAME fPage0 /* Delete */
OR  CHOOSE OF MENU-ITEM miDelete IN MENU mbMain DO:

    SESSION:SET-WAIT-STATE("GENERAL":U).

    if  avail tt-wt-docto then do:
        run utp/ut-msgs.p (input "show", input 550, input ""). /* Mensagem de confirmaá∆o para eliminaá∆o */
        if  return-value = "yes":U then do:
            run eliminaRegistrosWorkTable in h-bodi317sd (input  tt-wt-docto.seq-wt-docto,
                                                          input  yes,
                                                          output l-proc-ok-aux).
            if  btPrev:sensitive in frame fPage0 then 
                apply "choose":U to btPrev in frame fPage0.
            else 
                if  btNext:sensitive in frame fPage0 then
                    apply "choose":U to btNext in frame fPage0.

            if  not avail tt-wt-docto then
                RUN openQueriesSon.
        end.
    end.

    SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btDeleteSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeleteSon1 wMasterDetail
ON CHOOSE OF btDeleteSon1 IN FRAME fPage1 /* Eliminar */
DO:
    SESSION:SET-WAIT-STATE("GENERAL":U).

    if  avail tt-wt-it-docto then do:

        run utp/ut-msgs.p (input "show", input 550, input ""). /* Mensagem de confirmaá∆o para eliminaá∆o */
        if  return-value = "yes":U then do:

            do  i-cont = 1 to brSon1:num-selected-rows in frame fPage1:

                if  brSon1:fetch-selected-row(i-cont) in frame fPage1 then do:

                    run eliminaWtItDoctoImposto in h-bodi317sd (input  tt-wt-it-docto.seq-wt-docto,
                                                                input  tt-wt-it-docto.seq-wt-it-docto,
                                                                output l-proc-ok-aux).

                end.                                                
            end.

            RUN openQueriesSon.

            FIND LAST b-tt-wt-it-docto NO-ERROR.
            IF  AVAILABLE(b-tt-wt-it-docto) THEN
                REPOSITION brSon1 TO ROWID ROWID(b-tt-wt-it-docto).
        end.
    end.
    SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btDeleteSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeleteSon2 wMasterDetail
ON CHOOSE OF btDeleteSon2 IN FRAME fPage2 /* Eliminar */
DO:
    {masterdetail/DeleteSon.i &PageNumber="2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btDeleteSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeleteSon3 wMasterDetail
ON CHOOSE OF btDeleteSon3 IN FRAME fPage3 /* Eliminar */
DO:
    {masterdetail/DeleteSon.i &PageNumber="3"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btEstrutura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEstrutura wMasterDetail
ON CHOOSE OF btEstrutura IN FRAME fPage1 /* Estrutura */
DO:
    if  avail tt-wt-docto then
        RUN updateRecord IN THIS-PROCEDURE (INPUT "ftp/ft4007.w":U).

/*    def var h-ft4007 as handle no-undo.

    if  avail tt-wt-docto then do:
        session:set-wait-state("general":U).
        run ftp/ft4007.w persistent set h-ft4007 (input tt-wt-docto.r-rowid,
                                                  input "",
                                                  input this-procedure).
        session:set-wait-state("":U).
        if  valid-handle(h-ft4007) then do:
            run initializeInterface in h-ft4007.
            {&WINDOW-NAME}:sensitive = no.
    
            wait-for close of h-ft4007 focus current-window.
        end.
        {&WINDOW-NAME}:sensitive = yes.
    end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wMasterDetail
ON CHOOSE OF btExit IN FRAME fPage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst wMasterDetail
ON CHOOSE OF btFirst IN FRAME fPage0 /* First */
OR CHOOSE OF MENU-ITEM miFirst IN MENU mbMain DO:
    RUN getFirst IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME btGeraAuto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGeraAuto wMasterDetail
ON CHOOSE OF btGeraAuto IN FRAME fPage4 /* Geraá∆o Autom†tica */
DO:
    if  avail tt-wt-docto then do:
        run localizaWtDocto          in h-bodi317pr(input  tt-wt-docto.seq-wt-docto,
                                                    output l-proc-ok-aux).
        run inicializaAcompanhamento in h-bodi317pr.
        run criaWtNotaEmbal          in h-bodi317pr(output l-proc-ok-aux).
        run finalizaAcompanhamento   in h-bodi317pr.
        run devolveErrosbodi317pr    in h-bodi317pr(output c-ultimo-metodo-exec,
                                                    output table RowErrors).
        RUN goToRecord2  (input tt-wt-docto.seq-wt-docto).
        find first RowErrors no-error.
        if  avail RowErrors then do:
            {method/ShowMessage.i1}
            {method/ShowMessage.i2 &Modal="yes"}
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btGeraItens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGeraItens wMasterDetail
ON CHOOSE OF btGeraItens IN FRAME fPage1 /* Gera ÷tens */
DO:
    /* 12/02/04 */ 
    ASSIGN 
        c-cod-estabel-esapi007a-36  = tt-wt-docto.cod-estabel
        c-cod-emitente-esapi007a-36 = tt-wt-docto.cod-emitente /* 17/02/04 */.

    if  avail tt-wt-docto then
        if  tt-wt-docto.esp-docto = 20 then /* Nota fiscal de Devoluá∆o */
            RUN updateRecord IN THIS-PROCEDURE (INPUT "ftp/ft4011.w":U).
        else do: /* Notas fiscal de terceiros */
            RUN updateRecord IN THIS-PROCEDURE (INPUT "esp/es4036z.w":U).
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btGoTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGoTo wMasterDetail
ON CHOOSE OF btGoTo IN FRAME fPage0 /* Go To */
OR CHOOSE OF MENU-ITEM miGoTo IN MENU mbMain DO:
    RUN goToRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wMasterDetail
ON CHOOSE OF btHelp IN FRAME fPage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btInfFiscais
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btInfFiscais wMasterDetail
ON CHOOSE OF btInfFiscais IN FRAME fPage1 /* Inf Fiscais */
DO:
    if  avail tt-wt-it-docto then do:
            run retornaVariaveisParaCalculoImpostos in h-bodi317sd (input  tt-wt-docto.seq-wt-docto,
                                                                output l-nf-man-dev-terc-dif,
                                                                output l-recal-apenas-totais,
                                                                output l-proc-ok-aux).
        if  not l-recal-apenas-totais then
            run RecalculaImpostos.

        {masterdetail/UpdateSon.i &ProgramSon="ftp/ft4009.w"
                                   &PageNumber="1"}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast wMasterDetail
ON CHOOSE OF btLast IN FRAME fPage0 /* Last */
OR CHOOSE OF MENU-ITEM miLast IN MENU mbMain DO:
    RUN getLast IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME btManutencaoEmb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btManutencaoEmb wMasterDetail
ON CHOOSE OF btManutencaoEmb IN FRAME fPage4 /* Manutená∆o */
DO:
    if  avail tt-wt-docto then
        RUN updateRecord IN THIS-PROCEDURE (INPUT "ftp/ft4013.w":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext wMasterDetail
ON CHOOSE OF btNext IN FRAME fPage0 /* Next */
OR CHOOSE OF MENU-ITEM miNext IN MENU mbMain DO:
    RUN getNext IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrev wMasterDetail
ON CHOOSE OF btPrev IN FRAME fPage0 /* Prev */
OR CHOOSE OF MENU-ITEM miPrev IN MENU mbMain DO:
    RUN getPrev IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btQueryJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btQueryJoins wMasterDetail
ON CHOOSE OF btQueryJoins IN FRAME fPage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRecalcula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRecalcula wMasterDetail
ON CHOOSE OF btRecalcula IN FRAME fPage0 /* Rec†lculo de Impostos */
DO:
    run utp/ut-msgs.p (input "show",
                      input 15294,
                      input "").
    if return-value = "no" then
       return.

    run RecalculaImpostos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReportsJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReportsJoins wMasterDetail
ON CHOOSE OF btReportsJoins IN FRAME fPage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch wMasterDetail
ON CHOOSE OF btSearch IN FRAME fPage0 /* Search */
OR CHOOSE OF MENU-ITEM miSearch IN MENU mbMain DO:
    {method/ZoomReposition.i &ProgramZoom="dizoom/z01di317.w"}
    /* Passa o nome do programa para o Zoom. Desta forma, somente 
       ir† mostrar registros criados por esse programa. */
    run setProgram in hProgramZoom (input 4003).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtSimula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtSimula wMasterDetail
ON CHOOSE OF BtSimula IN FRAME fPage0 /* Simulaá∆o do C†lculo */
DO:
    /*run calculaNota(input no).*/
    
    run ftp/ft4003c.p (input  no,
                       input  tt-wt-docto.seq-wt-docto,
                       output table rowerrors).
    
    find first RowErrors no-error.
    
    if  avail RowErrors then do:
        {method/ShowMessage.i1}
        {method/ShowMessage.i2 &Modal="yes"}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate wMasterDetail
ON CHOOSE OF btUpdate IN FRAME fPage0 /* Update */
OR CHOOSE OF MENU-ITEM miUpdate IN MENU mbMain DO:
    if  avail tt-wt-docto THEN DO:
     
       
/*         /* para barrar faturamento pelo ft4003 */                           */
/*         FIND FIRST wt-docto WHERE                                           */
/*                    wt-docto.seq-wt-docto = tt-wt-docto.seq-wt-docto         */
/*             EXCLUSIVE-LOCK NO-ERROR.                                        */
/*         IF AVAIL wt-docto THEN ASSIGN wt-docto.char-2 = "sistema de arroz". */
           

        RUN updateRecord IN THIS-PROCEDURE (INPUT "ftp/ft4003b.w":U).
        
/*         /* erika 06/08 para barrar faturamento pelo ft4003 */ */
/*         ASSIGN wt-docto.char-2 = "".                          */
/*         /* erika 06/08 */                                     */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btUpdateSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateSon1 wMasterDetail
ON CHOOSE OF btUpdateSon1 IN FRAME fPage1 /* Alterar */
DO:
    def var h-program-itens as handle no-undo.

    if  not avail tt-wt-docto then
        return no-apply.
    if  not avail tt-wt-it-docto then
        return no-apply.
    session:set-wait-state("general":U).
    
    run ftp/ft4004.w persistent set h-program-itens.
    if  valid-handle(h-program-itens) then do:
        run setParameters in h-program-itens (input tt-wt-docto.seq-wt-docto,
                                              input tt-wt-it-docto.seq-wt-it-docto).
        run setHandleDBOs in h-program-itens (input this-procedure,
                                              input h-bodi317in,
                                              input h-bodi317sd,    
                                              input h-bodi317pr,    
                                              input h-bodi317im1bra, 
                                              input h-bodi317va).    
        run initializeInterface in h-program-itens.
        run applyChooseBtUpdate in h-program-itens.
        session:set-wait-state("":U). 
        {&WINDOW-NAME}:sensitive = no.
 
        wait-for close of h-program-itens focus current-window.
    end.
   
    {&WINDOW-NAME}:sensitive = yes.
    session:set-wait-state("":U).

    APPLY 'ENTRY' TO {&WINDOW-NAME}.

    RUN openQueriesSon.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btUpdateSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateSon2 wMasterDetail
ON CHOOSE OF btUpdateSon2 IN FRAME fPage2 /* Alterar */
DO:
    {masterdetail/UpdateSon.i &ProgramSon="ftp/ft4008.w"
                              &PageNumber="2"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btUpdateSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateSon3 wMasterDetail
ON CHOOSE OF btUpdateSon3 IN FRAME fPage3 /* Alterar */
DO:
    run calculaTotalNota.
    {masterdetail/UpdateSon.i &ProgramSon="ftp/ft4006.w"
                              &PageNumber="3"} 
    /* Seta o valor total da nota no programa onde s∆o informadas as duplicatas */
    RUN setparameters IN hSonProgram (input de-vl-tot-nota).                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Clculo_da_Nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Clculo_da_Nota wMasterDetail
ON CHOOSE OF MENU-ITEM m_Clculo_da_Nota /* C†lculo da Nota */
DO:
  apply "Choose":U to btCalcula in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reclculo_de_Impostos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reclculo_de_Impostos wMasterDetail
ON CHOOSE OF MENU-ITEM m_Reclculo_de_Impostos /* Rec†lculo de Impostos */
DO:
  apply "Choose":U to btRecalcula in frame fPage0.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Simulao_do_Clculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Simulao_do_Clculo wMasterDetail
ON CHOOSE OF MENU-ITEM m_Simulao_do_Clculo /* Simulaá∆o do C†lculo */
DO:
  apply "Choose":U to btSimula in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMasterDetail 


/*--- L¢gica para inicializaá∆o do programam ---*/
{masterdetail/MainBlock.i}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wMasterDetail 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {method/ShowMessage.i3}

    if  valid-handle(h-bodi317in) then do:
        run finalizaBOS in h-bodi317in.
        assign h-bodi317in = ?.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wMasterDetail 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    assign c-tipo-nota:screen-value in frame fPage0 = 
               if  avail tt-wt-docto 
               then if  tt-wt-docto.ind-tip-nota = 2
                    then "Nota Manual":U
                    else if  tt-wt-docto.ind-tip-nota = 4
                         then "Complementar de Mercadoria":U
                         else if  tt-wt-docto.ind-tip-nota = 3
                              or  tt-wt-docto.ind-tip-nota = 51
                              or  tt-wt-docto.ind-tip-nota = 52
                              or  tt-wt-docto.ind-tip-nota = 54 /* Dif Cambial a Menor */
                              then "Diferenáa de Preáo":U
                              else "Complementar de Imposto":U
               else ""
           btCalcula:sensitive in frame fPage0 = avail tt-wt-docto
           BtSimula:sensitive  in frame fPage0 = avail tt-wt-docto.
    
    /* Se o if abaixo for verdadeiro, significa que foi selecionada outra nota fiscal.
       Dessa forma, dever† ser zerada a vari†vel para que n∆o contenha o total da nota incorreto.    
       A v†ri†vel abaixo Ç auxiliar para saber de qual seq-wt-docto est† totalizado na var de-vl-tot-nota */
    if  i-seq-wt-docto-aux-vl-tot-nota <> input frame fPage0 tt-wt-docto.seq-wt-docto then 
        assign de-vl-tot-nota = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wMasterDetail 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyChooseBtIncluiItem wMasterDetail 
PROCEDURE applyChooseBtIncluiItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    if  not avail tt-wt-it-docto then 
        if  btGeraItens:sensitive in frame fPage1 then 
            apply "CHOOSE":U to btGeraItens in frame fPage1.
        else 
            if  btAddSon1:sensitive in frame fPage1 then 
                apply "CHOOSE":U to btAddSon1 in frame fPage1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyChooseBtUpdate wMasterDetail 
PROCEDURE applyChooseBtUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    apply "CHOOSE":U to btUpdate  in frame fPage0.
    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyChooseBtUpdateDiferenca wMasterDetail 
PROCEDURE applyChooseBtUpdateDiferenca :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Chama a tela para solicitar a nota de diferenáa de preáo quando Ç nota de diferenáa */ 
    if  avail tt-wt-docto 
    and tt-wt-docto.ind-tip-nota = 3 then
        RUN updateRecord IN THIS-PROCEDURE (INPUT "ftp/ft4010.w":U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscaHandleBos wMasterDetail 
PROCEDURE buscaHandleBos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def output param p-h-bodi317in     as handle no-undo.
    def output param p-h-bodi317pr     as handle no-undo.
    def output param p-h-bodi317sd     as handle no-undo.
    def output param p-h-bodi317im1bra as handle no-undo.
    def output param p-h-bodi317va     as handle no-undo.

    assign p-h-bodi317in     = h-bodi317in
           p-h-bodi317pr     = h-bodi317pr
           p-h-bodi317sd     = h-bodi317sd  
           p-h-bodi317im1bra = h-bodi317im1bra
           p-h-bodi317va     = h-bodi317va.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculaNota wMasterDetail 
PROCEDURE calculaNota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    def input param p-l-calculo       as log form "Calculo/Simulaá∆o" no-undo.

    def var h-bodi317ef            as handle no-undo.
    def var h-ft4015               as handle no-undo.
    def var h-ft4005               as handle no-undo.
    def var l-possui-impressao-aut as log    no-undo.
    DEF VAR l-transp-branco        AS LOG     no-undo.
    
    def buffer b-tt-notas-geradas for tt-notas-geradas.

        
        /** Simulaá∆o Fretes **/
    FOR EACH tt-ret-calculo:
        DELETE tt-ret-calculo.
    END.
    

    if  avail tt-wt-docto then do:
        run emptyRowErrors           in h-bodi317in.
        
        do  trans: 
            run inicializaAcompanhamento in h-bodi317pr.
            run retornaVariaveisParaCalculoImpostos in h-bodi317sd (input  tt-wt-docto.seq-wt-docto,
                                                                    output l-nf-man-dev-terc-dif,
                                                                    output l-recal-apenas-totais,
                                                                    output l-proc-ok-aux).
            run recebeVariavelTipoCalculoImpostos   in h-bodi317im1bra (input if l-recal-apenas-totais 
                                                                              then 1
                                                                              else 0,
                                                                              output l-proc-ok-aux).
            run confirmaCalculo          in h-bodi317pr(input  tt-wt-docto.seq-wt-docto,
                                                        output l-proc-ok-aux).

            run finalizaAcompanhamento   in h-bodi317pr.
            run devolveErrosbodi317pr    in h-bodi317pr(output c-ultimo-metodo-exec,
                                                        output table RowErrors).
            find first RowErrors no-error.

            if  avail RowErrors then do:
                {method/ShowMessage.i1}
                {method/ShowMessage.i2 &Modal="yes"}
            end.
/*                message "antes" view-as alert-box.    */
            if  not l-proc-ok-aux then 
                undo, return no-apply.
/*                message "depois" view-as alert-box.*/

            if  p-l-calculo then do: /* N∆o Ç simulaá∆o */
                RUN goToRecord2  (input tt-wt-docto.seq-wt-docto).

                /* trata consistencia data ft0114 nf */
                FIND FIRST wt-docto NO-LOCK
                     WHERE wt-docto.seq-wt-docto = tt-wt-docto.seq-wt-docto
                     USE-INDEX seq-tabela NO-ERROR.
                IF AVAIL wt-docto THEN DO:   
            /*         MESSAGE "wt" VIEW-AS ALERT-BOX. */
            
                   FIND ex-ser-estab where
                        ex-ser-estab.serie       = wt-docto.serie       AND
                        ex-ser-estab.cod-estabel = wt-docto.cod-estabel
                        NO-LOCK NO-ERROR.
            
                   IF AVAILABLE ex-ser-estab THEN DO:
            
                      IF ex-ser-estab.data-1 <> ? AND
                         ex-ser-estab.data-1 <> 01/01/0001 THEN DO:
            
            /*               MESSAGE "dentro <> and" ex-ser-estab.data-1 */
            /*                   wt-docto.dt-emis-nota                   */
            /*                   VIEW-AS ALERT-BOX.                      */
                          IF ex-ser-estab.data-1 < wt-docto.dt-emis-nota THEN DO:
            
                              MESSAGE "ES4036-Data de Emiss∆o da nota " wt-docto.dt-emis-nota 
                                    " Ç maior que a Èltima Data do Màs " ex-ser-estab.data-1
                                    ", parametrizada no FT0114!"
                                    VIEW-AS ALERT-BOX ERROR TITLE "Data Màs n∆o Fechado".
                              RETURN ERROR.
                            END.      
                            
                          END.                                  
                   END.
                END.
                    
                IF NOT VALID-HANDLE(h-ft4005) THEN
                    run ftp/ft4005.w PERSISTENT SET h-ft4005 (input tt-wt-docto.seq-wt-docto).

                RUN recebeHandle        IN h-ft4005 (INPUT THIS-PROCEDURE).
                RUN InitializeInterface IN h-ft4005.
                
                session:set-wait-state( "":U ).
                WAIT-FOR CLOSE OF h-ft4005 FOCUS CURRENT-WINDOW.
                {&window-name}:sensitive = yes.

                if  return-value <> "Efetiva":U then
                    undo, return no-apply.
                
                if valid-handle(h-ft4005) then do:
                   delete procedure h-ft4005.
                   assign h-ft4005 = ?.
                end.   
            end.
            else do:
                for each tt-documentos:
                    delete tt-documentos.
                end.
                 
                create tt-documentos. 
                assign tt-documentos.seq-wt-docto = tt-wt-docto.seq-wt-docto.

/*                run ftp/ft4015.w persistent set h-ft4015 (input table tt-documentos).
                run initializeRun in h-ft4015.

                if  valid-handle(h-ft4015) then
                    delete procedure h-ft4015.

                assign h-ft4015 = ?.*/

                for each tt-param:
                    delete tt-param.
                end.

                create tt-param.
                assign tt-param.usuario   = c-seg-usuario
                       tt-param.destino   = 3
                       tt-param.data-exec = today
                       tt-param.hora-exec = time.
                       tt-param.arquivo   = session:temp-directory + "FT4015" + ".tmp".

                run ftp/ft4015rp.p(input i-pais-impto-usuario,
                                   input table tt-param,
                                   input table tt-documentos).

                OS-COMMAND SILENT VALUE('notepad ' + tt-param.arquivo).

                undo, return no-apply.
            end.
        end.
            
        if  p-l-calculo then do: /* N∆o Ç simulaá∆o */
            run dibo/bodi317ef.p persistent set h-bodi317ef.

            run emptyRowErrors           in h-bodi317in.
            run inicializaAcompanhamento in h-bodi317ef.
            run setaHandlesBOS           in h-bodi317ef(h-bodi317pr,     h-bodi317sd, 
                                                        h-bodi317im1bra, h-bodi317va).

            assign i-nr-pedcli-venda  = tt-wt-docto.nr-pedcli
                   i-nome-abrev-venda = tt-wt-docto.nome-abrev. 
            
            RUN inputTableFretes         IN h-bodi317ef(INPUT TABLE tt-ret-calculo,
                                                        INPUT l-transp-branco).
                                                        
            run efetivaNota              in h-bodi317ef(input  tt-wt-docto.seq-wt-docto,
                                                        input  if tt-wt-docto.ind-tip-nota = 2 
                                                               then no 
                                                               else yes,
                                                        output l-proc-ok-aux).
            run finalizaAcompanhamento   in h-bodi317ef.
            run devolveErrosbodi317ef    in h-bodi317ef(output c-ultimo-metodo-exec,
                                                        output table RowErrors).

            find first RowErrors
                 where RowErrors.ErrorSubType = "ERROR":U no-error.
            if  avail RowErrors then do:
                {method/ShowMessage.i1}
                {method/ShowMessage.i2 &Modal="yes"}
            end.
        
            if  not l-proc-ok-aux then do:
                delete procedure h-bodi317ef.
                ASSIGN h-bodi317ef = ?.
                return no-apply.
            end.

            run buscaTTNotasGeradas in h-bodi317ef(output l-proc-ok-aux,
                                                   output table tt-notas-geradas).

            /* Se houver impressao automatica imprime as notas fiscais */
            run possuiImpressaoAutomatica in h-bodi317ef (output l-possui-impressao-aut). 
            if  l-possui-impressao-aut then do:
                /* Temp-table auxiliar para impress∆o autom†tica, para que n∆o haja nescessidade de se 
                   criar um novo programa de impress∆o autom†tica. Ser† utilizado o programa antigo */
                for each tt-notas-geradas-impressao:
                    delete tt-notas-geradas-impressao.
                end.
                for each tt-notas-geradas:
                    create tt-notas-geradas-impressao.
                    assign tt-notas-geradas-impressao.rw-nota-fiscal = tt-notas-geradas.rw-nota-fiscal
                           tt-notas-geradas-impressao.nr-nota        = tt-notas-geradas.nr-nota.
                end.
                if  i-pais-impto-usuario = 1 then
                    run ftp/ft2019.w(input table tt-notas-geradas-impressao).
                else
                    run ftp/ft2018.w(input table tt-notas-geradas-impressao).
            end.

            delete procedure h-bodi317ef.
            ASSIGN h-bodi317ef = ?.

            bell.
            find first tt-notas-geradas.
            find last b-tt-notas-geradas.
            find first nota-fiscal 
                 where rowid(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal no-lock no-error.
            if  tt-notas-geradas.nr-nota = b-tt-notas-geradas.nr-nota then
                run utp/ut-msgs.p(input "show",
                                  input 15263,
                                  input string(tt-notas-geradas.nr-nota) + "~~" +
                                        string(nota-fiscal.cod-estabel)  + "~~" +
                                        string(nota-fiscal.serie)).
            else
                run utp/ut-msgs.p(input "show",
                                  input 15264,
                                  input string(tt-notas-geradas.nr-nota)   + "~~" +
                                        string(b-tt-notas-geradas.nr-nota) + "~~" +
                                        string(nota-fiscal.cod-estabel)    + "~~" +
                                        string(nota-fiscal.serie)).

            /*** Descontos e Bonificaá‰es ***/

            run  trataBonif in h-bodi317pr (input  i-nr-pedcli-venda,
                                            input  i-nome-abrev-venda,
                                            output l-retorno,
                                            output c-nome-abrev-bonif,
                                            output c-num-pedido-bonif,
                                            output l-proc-ok-aux).

            run devolveErrosbodi317pr    in h-bodi317pr(output c-ultimo-metodo-exec,
                                                        output table RowErrors).

            FIND FIRST RowErrors NO-ERROR.
            IF  AVAIL RowErrors THEN DO:

                {method/ShowMessage.i1}
                {method/ShowMessage.i2 &Modal="yes"}
            END.

            if  not l-proc-ok-aux then do:
                delete procedure h-bodi317ef.
                ASSIGN h-bodi317ef = ?.
                return no-apply.
            end.

            if  l-retorno then do:

                run ftp/ft4002.r persistent set h-ft4002.
                run faturaPedidoAutomatico in h-ft4002 (input 4,
                                                        INPUT c-nome-abrev-bonif,
                                                        input c-num-pedido-bonif).
                if valid-handle(h-ft4002) then do:
                   delete procedure h-ft4002.
                   assign h-ft4002 = ?.
                end.   
            end.

            if  btFirst:sensitive in frame fPage0 then 
                apply "choose":U to btFirst in frame fPage0.
            else 
                if  btPrev:sensitive in frame fPage0 then 
                    apply "choose":U to btPrev in frame fPage0.
                else 
                    if  btNext:sensitive in frame fPage0 then
                        apply "choose":U to btNext in frame fPage0.

            if  not avail tt-wt-docto then
                RUN openQueriesSon.
            
            /* Finaliza as BOS e inicializa novamente */
            run finalizaBOS in h-bodi317in.
            assign h-bodi317in = ?.
            run dibo/bodi317in.p persistent set h-bodi317in.
            run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                             output h-bodi317sd,     
                                             output h-bodi317im1bra,
                                             output h-bodi317va).
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculaTotalNota wMasterDetail 
PROCEDURE calculaTotalNota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* A procedure abaixo verificar† se existe ou n∆o duplicatas para um determinado tt-wt-docto */
    run possuiDuplicatas in h-bodi317pr (input  tt-wt-docto.seq-wt-docto,
                                         output l-possui-duplicatas,
                                         output l-proc-ok-aux).

    /* Caso n∆o existe duplciatas ou o total da nota estiver zerado, calcula o valor total da nota
       para que o usu†rio possa informar as duplicatas conforme o valor calculado. */
    if  de-vl-tot-nota = 0
    or  not l-possui-duplicatas then do trans:
        run emptyRowErrors           in h-bodi317in.
        run disableMessage15299      in h-bodi317va.
        run inicializaAcompanhamento in h-bodi317pr.
        run calculaWtDocto in h-bodi317pr (input  tt-wt-docto.seq-wt-docto,
                                           output l-proc-ok-aux).
        run finalizaAcompanhamento   in h-bodi317pr.
        run devolveErrosbodi317pr    in h-bodi317pr(output c-ultimo-metodo-exec,
                                                    output table RowErrors).
        find first RowErrors
             where RowErrors.ErrorSubType = "ERROR":U 
             and   RowErrors.ErrorNumber <> 15299 no-error.
        /* MENSAGEM: 15299 - Duplicatas n∆o foram geradas para a nota, n∆o dever† serr considerada. */
    
        if  avail RowErrors then do:
            {method/ShowMessage.i1}
            {method/ShowMessage.i2 &Modal="yes"}
        end.

        if  not l-proc-ok-aux then 
            undo, return no-apply.

        run totalNota in h-bodi317pr (input  tt-wt-docto.seq-wt-docto,
                                      output de-vl-tot-nota,
                                      output l-proc-ok-aux).
        /* V†ri†vel auxiliar para saber de qual seq-wt-docto est† totalizado na var de-vl-tot-nota */
        assign i-seq-wt-docto-aux-vl-tot-nota = tt-wt-docto.seq-wt-docto.
        
        /* A transaá∆o Ç desfeita devido a n∆o ser aqui o c†lculo da nota */
        undo, next.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToRecord wMasterDetail 
PROCEDURE goToRecord :
/*------------------------------------------------------------------------------
  Purpose:     Exibe dialog de V† Para
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUTTON btGoToCancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON btGoToOK AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE rtGoToButton
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 43 BY 1.42
         BGCOLOR 7.
    
    DEFINE VARIABLE rGoTo AS ROWID NO-UNDO.
    
    DEFINE VARIABLE i-seq-wt-docto LIKE {&ttParent}.seq-wt-docto NO-UNDO.
    
    DEFINE FRAME fGoToRecord
        i-seq-wt-docto  AT ROW 1.71 COL 17.72 COLON-ALIGNED LABEL "Sequància"
        btGoToOK        AT ROW 3.63 COL 2.14
        btGoToCancel    AT ROW 3.63 COL 13
        rtGoToButton    AT ROW 3.38 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "V† Para da Tabela Tempor†ria da Nota" FONT 1
             DEFAULT-BUTTON btGoToOK CANCEL-BUTTON btGoToCancel.
    
    ON "CHOOSE":U OF btGoToOK IN FRAME fGoToRecord DO:
        ASSIGN i-seq-wt-docto.
        
        /* Posiciona query, do DBO, atravÇs dos valores do °ndice £nico */
        RUN goToKey IN {&hDBOParent} (INPUT i-seq-wt-docto).
        IF  RETURN-VALUE = "NOK":U THEN DO:
            RUN utp/ut-msgs.p (INPUT "SHOW", INPUT 2, INPUT "Tabela Tempor†ria da Nota").
            RETURN NO-APPLY.
        END.
        
        /* Retorna rowid do registro corrente do DBO */
        RUN getRowid IN {&hDBOParent} (OUTPUT rGoTo).
        
        /* Reposiciona registro com base em um rowid */
        RUN repositionRecord IN THIS-PROCEDURE (INPUT rGoTo).
        
        APPLY "GO":U TO FRAME fGoToRecord.
    END.
    
    ENABLE i-seq-wt-docto btGoToOK btGoToCancel 
        WITH FRAME fGoToRecord. 
    
    WAIT-FOR "GO":U OF FRAME fGoToRecord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToRecord2 wMasterDetail 
PROCEDURE goToRecord2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-i-seq-wt-docto LIKE {&ttParent}.seq-wt-docto NO-UNDO.

    DEFINE VARIABLE rGoTo AS ROWID NO-UNDO.

    /* Reabre a query */
    RUN openQueryStatic IN {&hDBOParent} (INPUT "DefaultProg4003":U).

    /* Posiciona query, do DBO, atravÇs dos valores do °ndice £nico */
    RUN goToKey IN {&hDBOParent} (INPUT p-i-seq-wt-docto).

    IF  RETURN-VALUE = "NOK":U THEN DO:
        RETURN NO-APPLY.
    END.
        
    /* Retorna rowid do registro corrente do DBO */
    RUN getRowid IN {&hDBOParent} (OUTPUT rGoTo).
    
    /* Reposiciona registro com base em um rowid */
    RUN repositionRecord IN THIS-PROCEDURE (INPUT rGoTo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wMasterDetail 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOParent}) THEN DO:
        {btb/btb008za.i1 dibo/bodi317.p}
        {btb/btb008za.i2 dibo/bodi317.p '' {&hDBOParent}} 
    END.
    
    RUN setConstraintDefault IN {&hDBOParent} (input c-seg-usuario).
    RUN openQueryStatic IN {&hDBOParent} (INPUT "DefaultProg4003":U).
    
    /*--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon1}) THEN DO:
        {btb/btb008za.i1 dibo/bodi321.p}
        {btb/btb008za.i2 dibo/bodi321.p '' {&hDBOSon1}} 
    END.
    
    /*--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon2}) THEN DO:
        {btb/btb008za.i1 dibo/bodi319.p}
        {btb/btb008za.i2 dibo/bodi319.p '' {&hDBOSon2}}
    END.

    /*--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon3}) THEN DO:
        {btb/btb008za.i1 dibo/bodi318.p}
        {btb/btb008za.i2 dibo/bodi318.p '' {&hDBOSon3}}
    END.

    /*--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon4}) THEN DO:
        {btb/btb008za.i1 dibo/bodi325.p}
        {btb/btb008za.i2 dibo/bodi325.p '' {&hDBOSon4}}
    END.

    IF NOT VALID-HANDLE(h-bodi317in) THEN DO:
        run dibo/bodi317in.p persistent set h-bodi317in.
        run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                         output h-bodi317sd,     
                                         output h-bodi317im1bra,
                                         output h-bodi317va).
    END.


    assign brSon1:NUM-LOCKED-COLUMNS in frame fPage1 = 3.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueriesSon wMasterDetail 
PROCEDURE openQueriesSon :
/*------------------------------------------------------------------------------
  Purpose:     Atualiza browsers filhos
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    def var l-inclui      as log no-undo.
    def var l-altera      as log no-undo.
    def var l-elimina     as log no-undo.
    def var l-atend-seq   as log no-undo.
    def var l-atend-ped   as log no-undo.
    def var l-estrutura   as log no-undo.
    def var l-gera-itens  as log no-undo.
    def var l-duplicatas  as log no-undo.
    def var l-represent   as log no-undo.
    def var l-inf-fiscais as log no-undo.

    {masterdetail/OpenQueriesSon.i &Parent="wt-docto"
                                   &Query="ofWtDocto"
                                   &PageNumber="1"}

    {masterdetail/OpenQueriesSon.i &Parent="wt-docto"
                                   &Query="bySeqWtDocto"
                                   &PageNumber="2"}

    {masterdetail/OpenQueriesSon.i &Parent="wt-docto"
                                   &Query="ofWtDocto"
                                   &PageNumber="3"}

    {masterdetail/OpenQueriesSon.i &Parent="wt-docto"
                                   &Query="ofWtDocto"
                                   &PageNumber="4"}
    if  avail tt-wt-docto then do:
        run getEnableFieldButtons in h-bodi317sd (input  tt-wt-docto.seq-wt-docto,
                                                  output l-inclui,
                                                  output l-altera,
                                                  output l-elimina,
                                                  output l-atend-seq,
                                                  output l-atend-ped,
                                                  output l-estrutura,
                                                  output l-gera-itens,
                                                  output l-inf-fiscais,
                                                  output l-proc-ok-aux).   
        run retornaVariaveisParaCalculoImpostos in h-bodi317sd (input  tt-wt-docto.seq-wt-docto,
                                                                output l-nf-man-dev-terc-dif,
                                                                output l-recal-apenas-totais,
                                                                output l-proc-ok-aux).
                                                                         
        assign btAddSon1:sensitive    in frame fPage1 = btAddSon1:sensitive    in frame fPage1 and l-inclui
               btUpdateSon1:sensitive in frame fPage1 = btUpdateSon1:sensitive in frame fPage1 and l-elimina
               btDeleteSon1:sensitive in frame fPage1 = btDeleteSon1:sensitive in frame fPage1 and l-altera 
               btAtendPed:sensitive   in frame fPage1 = l-atend-seq
               btATendSeq:sensitive   in frame fPage1 = l-atend-ped
               btEstrutura:sensitive  in frame fPage1 = l-estrutura
               btGeraItens:sensitive  in frame fPage1 = l-gera-itens
               btInfFiscais:sensitive in frame fPage1 = l-inf-fiscais or l-nf-man-dev-terc-dif
               btRecalcula:sensitive  in frame fPage0 = l-inf-fiscais or l-nf-man-dev-terc-dif.
        run getEnableDuplicatasRepresentantes in h-bodi317sd (input  tt-wt-docto.seq-wt-docto,
                                                              output l-duplicatas,
                                                              output l-represent,
                                                              output l-proc-ok-aux).
        assign btAddSon2:sensitive       in frame fPage2 = btAddSon2:sensitive    in frame fPage2 and l-represent
               btDeleteSon2:sensitive    in frame fPage2 = btDeleteSon2:sensitive in frame fPage2 and l-represent
               btUpdateSon2:sensitive    in frame fPage2 = btUpdateSon2:sensitive in frame fPage2 and l-represent
               btAddSon3:sensitive       in frame fPage3 = btAddSon3:sensitive    in frame fPage3 and l-duplicatas
               btDeleteSon3:sensitive    in frame fPage3 = btDeleteSon3:sensitive in frame fPage3 and l-duplicatas
               btUpdateSon3:sensitive    in frame fPage3 = btUpdateSon3:sensitive in frame fPage3 and l-duplicatas
               btCalcNota:sensitive      in frame fPage3 = btAddSon3:sensitive    in frame fPage3 and l-duplicatas
               btManutencaoEmb:sensitive in frame fPage4 = yes
               btGeraAuto:sensitive      in frame fPage4 = yes.
    end.
    else do:
        /* Os for each e as open querys abaixo s∆o feitas devido a problemas nas templates */
        for each tt-wt-it-docto:   delete tt-wt-it-docto.   end.
        for each tt-wt-fat-duplic: delete tt-wt-fat-duplic. end.
        for each tt-wt-fat-repre:  delete tt-wt-fat-repre.  end.
        for each tt-wt-nota-embal: delete tt-wt-nota-embal. end.
        {&OPEN-QUERY-brSon1}
        {&OPEN-QUERY-brSon2}
        {&OPEN-QUERY-brSon3}
        {&OPEN-QUERY-brSon4} 

        assign btAddSon1:sensitive       in frame fPage1 = no
               btUpdateSon1:sensitive    in frame fPage1 = no
               btDeleteSon1:sensitive    in frame fPage1 = no
               btAtendPed:sensitive      in frame fPage1 = no
               btATendSeq:sensitive      in frame fPage1 = no
               btEstrutura:sensitive     in frame fPage1 = no
               btGeraItens:sensitive     in frame fPage1 = no
               btInfFiscais:sensitive    in frame fPage1 = no
               btRecalcula:sensitive     in frame fPage0 = no
               btAddSon2:sensitive       in frame fPage2 = no
               btDeleteSon2:sensitive    in frame fPage2 = no
               btUpdateSon2:sensitive    in frame fPage2 = no
               btAddSon3:sensitive       in frame fPage3 = no
               btDeleteSon3:sensitive    in frame fPage3 = no
               btUpdateSon3:sensitive    in frame fPage3 = no
               btCalcNota:sensitive      in frame fPage3 = no
               btManutencaoEmb:sensitive in frame fPage4 = no
               btGeraAuto:sensitive      in frame fPage4 = no.
    end.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputTableFretes wMasterDetail 
PROCEDURE OutputTableFretes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER TABLE FOR tt-ret-calculo.
    DEFINE INPUT PARAMETER l-transp-branco AS LOG NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RecalculaImpostos wMasterDetail 
PROCEDURE RecalculaImpostos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
    
    if  avail tt-wt-docto then do:
        run emptyRowErrors           in h-bodi317in.
        do  trans: 
            run inicializaAcompanhamento          in h-bodi317im1bra.
            run recebeVariavelTipoCalculoImpostos in h-bodi317im1bra(input  2,
                                                                     output l-proc-ok-aux).
            run calculaImpostosBrasil             in h-bodi317im1bra(input  tt-wt-docto.seq-wt-docto,
                                                                     output l-proc-ok-aux).
            run finalizaAcompanhamento            in h-bodi317im1bra.
            run devolveErrosbodi317im1br          in h-bodi317im1bra(output c-ultimo-metodo-exec,
                                                                     output table RowErrors).
            find first RowErrors no-error.
            
            if  avail RowErrors then do:
                {method/ShowMessage.i1}
                {method/ShowMessage.i2 &Modal="yes"}
            end.
    
            if  not l-proc-ok-aux then 
                undo, return no-apply.
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnNomeRepre wMasterDetail 
FUNCTION fnNomeRepre RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   
   def var c-nome-repre as char no-undo.
    
   run retornaNomeRepre in h-bodi319 (  input  tt-wt-fat-repre.cod-rep,
                                        output c-nome-repre ).
  
   RETURN c-nome-repre.   /* Function return value. */
   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

