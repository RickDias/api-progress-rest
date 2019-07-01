/*****************************************************************************************
**       Programa: cm02007.p
**       Data....: 28/02/2018
**       Autor...: DKP 
**       Objetivo: Informacoes de pagamentos de Fretes - Arquivo disponibilizado para a Deloitte
**       Versío..: TOTVS12
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
**
**********************************************************************************************/
{include/i-prgvrs.i CM0200RP 2.09.00.001} /*** "019001" ***/

/* DEFINE STREAM s-teste.                                     */
/* OUTPUT STREAM s-teste TO D:\especificos\DSC\esp\TESTE.TXT. */

DEFINE VARIABLE c-ini AS CHARACTER   NO-UNDO.
ASSIGN c-ini = STRING(NOW).


DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEFINE INPUT  PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR tt-raw-digita.

DEFINE VARIABLE h_table-handle          AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-tabela                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoSQLca                AS HANDLE      NO-UNDO.
DEFINE VARIABLE I-GW1_TPFRET            AS INTEGER     NO-UNDO.
DEFINE VARIABLE I-GW3_SIT               AS INTEGER     NO-UNDO.
DEFINE VARIABLE I-GW3_EMIFAT            AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-emis-nf-carga         AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE c-dt-entrada-doc        AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE c-dt-emis-doc           AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE c-prog-gerado           AS CHARACTER   NO-UNDO  INITIAL "cm02007".
DEFINE VARIABLE c-where                 AS CHARACTER   NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE c-arquivo-log AS CHARACTER NO-UNDO FORMAT "x(60)" .
/* DEFINE NEW GLOBAL SHARED VARIABLE c-prg-vrs     AS CHARACTER NO-UNDO.  */
/* DEFINE NEW GLOBAL SHARED VARIABLE c-prg-obj     AS CHARACTER NO-UNDO.  */
 
{include/i-rpvar.i}

/* Include */
{dibo/boSQLca.i}  

/* Mensagem */
{method/dbotterr.i}

/* Tabelas Tempor†rias  */
{esp/cm0200rp.i}

DEFINE TEMP-TABLE tt-cons-GW3 LIKE TT_GW3.
DEFINE TEMP-TABLE B-TT_GW8    LIKE TT_GW8.

DEFINE TEMP-TABLE bf-TT_GWH   LIKE TT_GWH.
DEFINE TEMP-TABLE bf-TT_GW8   LIKE TT_GW8.
DEFINE TEMP-TABLE TT_GWM_CALC LIKE TT_GWM.
DEFINE TEMP-TABLE TT_GWM-gfe  LIKE TT_GWM.
DEFINE TEMP-TABLE TT_GWM-con  LIKE TT_GWM.

DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario          as char FORMAT "x(12)"            NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-num-ped-exec-rpw     as integer                        NO-UNDO.   
DEFINE NEW GLOBAL SHARED VARIABLE i-pais-impto-usuario   as integer FORMAT ">>9"           NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-rpc                  as LOGICAL                        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE r-registro-atual       as rowid                          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-num-ped              as integer                        NO-UNDO.         
DEFINE NEW GLOBAL SHARED VARIABLE v_cod_usuar_corren     AS CHAR                           NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h_prog_segur_estab     as handle                         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_cod_grp_usuar_lst    as char                           NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_num_tip_aces_usuar   as int                            NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE rw-log-exec            as rowid                          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-dir-spool-servid-exec as CHAR NO-UNDO.
 
/****************** Definiªao de Variˇveis de Seleá∆o do RelatΩrio *********************/ 
DEFINE VARIABLE c-cod-estabel-ini       LIKE nota-fiscal.cod-estabel  FORMAT "X(3)"        NO-UNDO.
DEFINE VARIABLE c-cod-estabel-fim       LIKE nota-fiscal.cod-estabel  FORMAT "X(3)"        NO-UNDO.
DEFINE VARIABLE da-dt-entrada-doc-ini   LIKE nota-fiscal.dt-emis-nota FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE da-dt-entrada-doc-fim   LIKE nota-fiscal.dt-emis-nota FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE da-dt-emis-doc-ini      LIKE nota-fiscal.dt-emis-nota FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE da-dt-emis-doc-fim      LIKE nota-fiscal.dt-emis-nota FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE da-dt-emis-nf-carga-ini LIKE nota-fiscal.dt-emis-nota FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE da-dt-emis-nf-carga-fim LIKE nota-fiscal.dt-emis-nota FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE rs-tipo-selecao         AS INTEGER NO-UNDO.

/***************** Definiªao de Variˇveis de Processamento do RelatΩrio *********************/
DEFINE VARIABLE h-acomp                    AS HANDLE                          NO-UNDO.
DEFINE VARIABLE v-vl-pedagio               AS DEC                             NO-UNDO.
/* DEFINE VARIABLE v-vl-frete                 AS DECIMAL                         NO-UNDO. */
DEFINE VARIABLE v-rateio-conhec            AS DEC                             NO-UNDO.
DEFINE VARIABLE v-rateio-calc-gfe          AS DEC                             NO-UNDO.
DEFINE VARIABLE v-gfe-conhec               AS DECIMAL                         NO-UNDO.
DEFINE VARIABLE v-nr-fatura                AS CHAR                            NO-UNDO.
DEFINE VARIABLE v-serie-fat                AS CHAR                            NO-UNDO. 
DEFINE VARIABLE v-tabela-frete             AS INT                             NO-UNDO.
DEFINE VARIABLE v-cgc-tabela               AS CHAR                            NO-UNDO.
/*DEFINE VARIABLE v-peso-bru-doc             LIKE nota-fiscal.peso-bru-tot      NO-UNDO. */
/*DEFINE VARIABLE v-peso-liq-doc             LIKE nota-fiscal.peso-bru-tot      NO-UNDO. */
/*DEFINE VARIABLE v-peso-bru-calc            LIKE nota-fiscal.peso-bru-tot      NO-UNDO. */
/*DEFINE VARIABLE v-peso-liq-calc            LIKE nota-fiscal.peso-bru-tot      NO-UNDO. */
DEFINE VARIABLE v-esp-docto-nf             AS CHAR                            NO-UNDO.
DEFINE VARIABLE v-esp-docto-frete          AS CHAR                            NO-UNDO.
DEFINE VARIABLE v-situacao-conhec          AS CHAR    FORMAT "x(50)"          NO-UNDO.
DEFINE VARIABLE v-tp-nf                    AS CHAR    FORMAT "x(50)"          NO-UNDO.
DEFINE VARIABLE v-tp-frete-con             AS CHAR    FORMAT "x(50)"          NO-UNDO.
DEFINE VARIABLE v-cnpj-emit                AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE v-cod-gr-forn              LIKE emitente.cod-gr-forn          NO-UNDO.
DEFINE VARIABLE v-gr-forn-descricao        LIKE grupo-fornec.descricao        NO-UNDO.
DEFINE VARIABLE v-nome-emit-transp         LIKE emitente.nome-emit            NO-UNDO.
DEFINE VARIABLE v-cod-fornec               LIKE emitente.cod-emitente         NO-UNDO. 
DEFINE VARIABLE v-nr-pedcli                LIKE ped-venda.nr-pedcli           NO-UNDO.
DEFINE VARIABLE v-desc-estoq               LIKE grup-estoq.descricao          NO-UNDO. 
DEFINE VARIABLE v-denominacao              LIKE natur-oper.denominacao        NO-UNDO.
DEFINE VARIABLE v-via-transp               AS   CHAR     FORMAT "x(50)"       NO-UNDO.
DEFINE VARIABLE v-tem-tab-frete            AS LOGICAL                         NO-UNDO.
DEFINE VARIABLE v-cidade-ori               LIKE emitente.cidade               NO-UNDO.
DEFINE VARIABLE v-uf-ori                   LIKE emitente.estado               NO-UNDO.
DEFINE VARIABLE v-estabelec-cidade         LIKE estabelec.cidade              NO-UNDO.
DEFINE VARIABLE v-estabelec-estado         LIKE estabelec.estado              NO-UNDO.
DEFINE VARIABLE v-nome-emit                LIKE  emitente.nome-emit           NO-UNDO.
DEFINE VARIABLE v-desc-sit-ped             AS CHAR      FORMAT "x(30)"        NO-UNDO.
DEFINE VARIABLE v-cod-emitente             LIKE emitente.cod-emitente         NO-UNDO.
DEFINE VARIABLE v-desc-item                LIKE item.desc-item                NO-UNDO.
DEFINE VARIABLE v-dt-aprov                 AS DATE                            NO-UNDO.
DEFINE VARIABLE v-usuario-aprov            AS CHAR                            NO-UNDO.
DEFINE VARIABLE v-observacao               AS CHAR                            NO-UNDO.
DEFINE VARIABLE v-dt-integr-ap             AS DATE                            NO-UNDO.
DEFINE VARIABLE v-user-integr-ap           AS CHAR                            NO-UNDO.
DEFINE VARIABLE c-path                     AS CHAR                            NO-UNDO.
DEFINE VARIABLE v-conta-contabil-gfe       AS CHAR NO-UNDO.
DEFINE VARIABLE v-desc-conta-contabil-gfe  AS CHAR NO-UNDO.
DEFINE VARIABLE v-nr-embarque-gfe          AS INTEGER NO-UNDO.
DEFINE VARIABLE v-placa                    AS CHAR NO-UNDO.
DEFINE VARIABLE v-tipo-veiculo             AS CHAR NO-UNDO.
DEFINE VARIABLE v-produto-frete            AS CHAR NO-UNDO.
DEFINE VARIABLE v-nr-regiao                AS INTEGER NO-UNDO.
DEFINE VARIABLE v-data-contabil            AS DATE NO-UNDO .
DEFINE VARIABLE v-valor-despesa            AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-valor-icms               AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-valor-pis                AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-valor-iss                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE v-valor-cofins             AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-conta-despesa            AS CHAR NO-UNDO.
DEFINE VARIABLE c-estab-docto-frete        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-docto-frete              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-serie                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE d-criacao-docto-frete      AS DATE        NO-UNDO.
DEFINE VARIABLE c-natureza-docto-frete     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE d-aliq-icms                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-aliq-iss                 AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c-usuario-cria-doc-frt     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-NF-Frete                 AS CHAR         NO-UNDO.
DEFINE VARIABLE c-serie-NF                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-qt-faturada              AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-especie-NF               AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-tipo-frete               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-nr-pre-con               AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-estab-pre-con            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-emis-nota               AS DATE   NO-UNDO.
DEFINE VARIABLE I-GW1_CDTPDC               AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-GWM_TPDOC                AS CHARACTER   NO-UNDO. /* rateio-nf-contabil.id-tp-docto */
DEFINE VARIABLE c-GWM_GRP3                 AS CHARACTER   NO-UNDO. /* rateio-nf-contabil.cod-grp-3 */
DEFINE VARIABLE c-tp-docto                 AS CHARACTER   NO-UNDO. /* movct-tr.tp-docto */
DEFINE VARIABLE d-dtTransacao              AS DATE    NO-UNDO.
DEFINE VARIABLE d-lib-docto-frete          AS DATE    NO-UNDO.
DEFINE VARIABLE c_GW1_CDTPDC               as char no-undo. 
DEFINE VARIABLE c_GW1_EMISDC               as char no-undo.
DEFINE VARIABLE v-des-tit-ctbl             AS CHAR NO-UNDO.

/* Para calculo do rateio */
DEFINE VARIABLE c-cod-estabel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-serie-docto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nr-nota-fis AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-nr-seq-fat  AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-it-codigo   AS CHARACTER   NO-UNDO.


/* ************************  Function Prototypes ********************** */
/*FUNCTION fnRetiraAcentos RETURNS CHARACTER (INPUT p-string      AS CHARACTER) FORWARD. */
FUNCTION fnConvDataChar  RETURNS CHARACTER (INPUT p-data        AS DATE     ) FORWARD.
FUNCTION fnConvCharData  RETURNS DATE      (INPUT p-character   AS CHARACTER) FORWARD.
FUNCTION f-modalidade    RETURNS LOGICAL   (INPUT ipch-cgc      AS CHARACTER) FORWARD.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/*
DEFINE VARIABLE c-empresa       AS CHARACTER FORMAT "x(40)"             NO-UNDO.   */
/*DEFINE VARIABLE c-titulo-relat  AS CHARACTER FORMAT "x(50)"             NO-UNDO. */ 
/* DEFINE VARIABLE i-numper-x      AS INTEGER   FORMAT "ZZ"                NO-UNDO.   */
/*DEFINE VARIABLE da-iniper-x     AS DATE      FORMAT "99/99/9999"        NO-UNDO.  */
/*DEFINE VARIABLE da-fimper-x     AS DATE      FORMAT "99/99/9999"        NO-UNDO.  */
/*DEFINE VARIABLE c-programa      AS CHARACTER FORMAT "x(08)"             NO-UNDO.  */
/*DEFINE VARIABLE c-versao        AS CHARACTER FORMAT "x(04)"             NO-UNDO.  */
/* DEFINE VARIABLE c-revisao       AS CHARACTER FORMAT "999"               NO-UNDO. */ 

/* DEFINE NEW SHARED VAR c-impressora    AS CHARACTER                 NO-UNDO.      */
/* DEFINE NEW SHARED VAR c-layout        AS CHARACTER                 NO-UNDO.      */
/* DEFINE NEW SHARED VAR v_num_count     as integer                   NO-UNDO.      */
/* DEFINE NEW SHARED VAR c-arq-control   AS CHARACTER                 NO-UNDO.      */
/* DEFINE NEW SHARED VAR c-sistema       AS CHARACTER FORMAT "x(25)"  NO-UNDO.      */
/* DEFINE NEW SHARED VAR c-rodape        AS CHARACTER                 NO-UNDO.      */

assign c-programa     = "cm0200rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Valores de Frete"
       c-sistema      = "MCE".

assign c-cod-estabel-ini       = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim       = tt-param.c-cod-estabel-fim
       da-dt-entrada-doc-ini   = tt-param.da-dt-entrada-doc-ini
       da-dt-entrada-doc-fim   = tt-param.da-dt-entrada-doc-fim
       da-dt-emis-doc-ini      = tt-param.da-dt-emis-doc-ini
       da-dt-emis-doc-fim      = tt-param.da-dt-emis-doc-fim
       da-dt-emis-nf-carga-ini = tt-param.da-dt-emis-nf-carga-ini
       da-dt-emis-nf-carga-fim = tt-param.da-dt-emis-nf-carga-fim
       rs-tipo-selecao         = tt-param.i-tp-selecao
       c-path                  = tt-param.arquivo
    NO-ERROR.



run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

ASSIGN
    c-dt-entrada-doc[1] = fnConvDataChar(da-dt-entrada-doc-ini  )
    c-dt-entrada-doc[2] = fnConvDataChar(da-dt-entrada-doc-fim  )
    c-dt-emis-doc   [1] = fnConvDataChar(da-dt-emis-doc-ini     )
    c-dt-emis-doc   [2] = fnConvDataChar(da-dt-emis-doc-fim     )
    c-emis-nf-carga [1] = fnConvDataChar(da-dt-emis-nf-carga-ini)
    c-emis-nf-carga [2] = fnConvDataChar(da-dt-emis-nf-carga-fim)
NO-ERROR.

DOS SILENT DEL VALUE(c-path).

/*
OUTPUT TO value(c-path) CONVERT TARGET 'iso8859-1'.*/
{include/i-rpout.i &tofile=c-path &pagesize=0}

RUN p-cabec.

RUN pi-conecta.
RUN p-preselect("GV5"). 
RUN p-preselect("GVT").
RUN p-preselect("GUE").
RUN p-preselect("GUB").

IF rs-tipo-selecao = 1 THEN DO: /* Documento de Frete */
    
    
    RUN pi-acompanhar in h-acomp(INPUT "PESQUISANDO DOCUMENTOS ORIGEM CTE (GFE) ").

    RUN p-preselect("GW3"). /* movtrp.docto-frete. */
    FOR EACH TT_GW3 BREAK BY TT_GW3.GW3_DTFIS:
 
        
        RUN pi-acompanhar IN h-acomp(INPUT "Lendo Conhec: "                  + 
                                           STRING(fnConvCharData(GW3_DTFIS)) +
                                           "  /  "                           +
                                           TT_GW3.GW3_FILIAL                 +
                                           "/"                               +
                                           TT_GW3.GW3_NRDF                   +
                                           "/"                               +
                                           TT_GW3.GW3_SERDF).                
        
        RUN pi-preenche-dados-ctrc.
                           
        ASSIGN v-denominacao = "".

        FIND FIRST natur-oper
             WHERE natur-oper.nat-operacao = TT_GW3.GW3_CFOP NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper THEN DO:
            FIND FIRST CFOP-NATUR 
                 WHERE CFOP-NATUR.cod-cfop = TT_GW3.GW3_CFOP NO-LOCK NO-ERROR.
            IF AVAIL CFOP-NATUR THEN v-denominacao = DES-CFOP.
         END.
         ELSE v-denominacao = natur-oper.denominacao.

        ASSIGN  v-tem-tab-frete             = FALSE
                c-estab-docto-frete         = TT_GW3.GW3_FILIAL
                c-docto-frete               = TT_GW3.GW3_NRDF
                c-serie                     = TT_GW3.GW3_SERDF
                d-dtTransacao               = fnConvCharData(TT_GW3.GW3_DTFIS)
                d-criacao-docto-frete       = fnConvCharData(TT_GW3.GW3_DTEMIS)
                c-natureza-docto-frete      = TT_GW3.GW3_CFOP
                v-esp-docto-frete           = TT_GW3.GW3_CDESP
                c-usuario-cria-doc-frt      = TT_GW3.GW3_USUIMP
                c-NF-Frete                  = " "
                c-serie-NF                  = " "
                i-especie-NF                = 0
                d-lib-docto-frete           = fnConvCharData(TT_GW3.GW3_DTAPR) NO-ERROR.

        ASSIGN d-aliq-icms = 0
               d-aliq-iss  = 0.

        FIND TT_GVT WHERE TT_GVT.GVT_CDESP = TT_GW3.GW3_CDESP NO-ERROR.
        IF AVAILABLE TT_GVT THEN
            ASSIGN d-aliq-icms = TT_GW3.GW3_PCIMP WHEN GVT_TPIMP = "1"
                   d-aliq-iss  = TT_GW3.GW3_PCIMP WHEN GVT_TPIMP = "2" NO-ERROR.
        
        ASSIGN I-GW3_EMIFAT  = INTEGER(TT_GW3.GW3_EMIFAT)
                v-via-transp = "".

        FIND FIRST emitente NO-LOCK
             WHERE emitente.cod-emitente = I-GW3_EMIFAT NO-ERROR. 
        IF AVAILABLE emitente THEN f-modalidade(emitente.cgc). /* retorna  v-via-transp */.
        
        I-GW3_SIT = INTEGER(TT_GW3.GW3_SIT) NO-ERROR.
        CASE I-GW3_SIT > 0:                          
            WHEN I-GW3_SIT = 1 THEN v-situacao-conhec = "RECEBIDO".
            WHEN I-GW3_SIT = 2 THEN v-situacao-conhec = "BLOQUEADO".
            WHEN I-GW3_SIT = 3 THEN v-situacao-conhec = "APROVADO SISTEMA".
            WHEN I-GW3_SIT = 4 THEN v-situacao-conhec = "APROVADO USUARIO".
            OTHERWISE               v-situacao-conhec = "NAO ENCONTRADO". 
        END CASE.
        
        RUN p-preselect("GW4-1").
        FOR EACH  TT_GW4:
            
            /* Leitura da NF de Origem */
            RUN p-preselect("GW1-2").
            FOR EACH  TT_GW1:
                
                ASSIGN
                    v-tabela-frete    = 0
                    v-cgc-tabela      = " "
                    /* v-vl-frete        = 0 */
                    c-NF-Frete        = GW1_NRDC
                    c-serie-NF        = GW1_SERDC
                    i-qt-faturada     = GW1_QTVOL
                    v-tp-nf           = " "
                    dt-emis-nota      = fnConvCharData(GW1_DTEMIS)
                    v-tp-frete-con    = IF TT_GW1.GW1_TPFRET <> "2" THEN "CIF"
                                                                    ELSE "FOB".

                I-GW1_CDTPDC = INTEGER(TT_GW1.GW1_CDTPDC) NO-ERROR.
                CASE I-GW1_CDTPDC >= 0:
                     WHEN I-GW1_CDTPDC = 1 THEN v-tp-nf = "ENTREGA".
                     WHEN I-GW1_CDTPDC = 2 THEN v-tp-nf = "COMPRAS".
                     WHEN I-GW1_CDTPDC = 3 THEN v-tp-nf = "DEVOLUCAO".
                     WHEN I-GW1_CDTPDC = 4 THEN v-tp-nf = "ANULACAO".
                     OTHERWISE                  v-tp-nf = "OUTROS".
                END CASE.
                
                RUN p-preselect("GW6").
                FIND LAST TT_GW6 NO-ERROR.
                IF AVAILABLE TT_GW6 THEN DO:
                    ASSIGN v-nr-fatura      = TT_GW6.GW6_NRFAT
                           v-serie-fat      = TT_GW6.GW6_SERFAT
                           v-dt-aprov       = fnConvCharData(TT_GW6.GW6_DTAPR)
                           v-dt-integr-ap   = fnConvCharData(TT_GW6.GW6_DTFIN)
                           v-user-integr-ap = TT_GW6.GW6_USUFIN NO-ERROR.
                END.
                
                /* --- Busca a tabela GWH pela chave da nota fiscal (GW1) --- */
                RUN p-preselect("GWH").
                FOR EACH TT_GWH:
                    
                    /*Calculo Frete Nota Fiscal */
                    ASSIGN 
                       /* v-vl-frete      = 0 */
                        v-tabela-frete  = 0 
                        v-cgc-tabela    = " "
                        i-nr-pre-con    = 0
                        c-estab-pre-con = ""
                    NO-ERROR.
                    
                    RUN p-preselect("GWF").
                    FIND FIRST TT_GWF NO-LOCK NO-ERROR.
                    IF NOT AVAIL TT_GWF THEN NEXT.
                    
                    RUN pi-preenche-valores-calculo.
                    
                    /* UTILIZADO NA LEITURA DO CµLCULO DE v-peso-bru-calc / v-peso-liq-calc */
                    ASSIGN i-nr-pre-con    = INTEGER(TT_GWF.GWF_NRCALC)     /* pre-con.nr-calculo */
                           c-estab-pre-con = TT_GWF.GWF_FILIAL NO-ERROR.    /* pre-con.cod-estabel */
                         
                END.

                /*
                RUN p-preselect("GWH-2").
                FOR EACH bf-TT_GWH:
                    
                    RUN p-preselect("GW8-2").
                    FOR EACH bf-TT_GW8:

                        v-peso-bru-calc = v-peso-bru-calc + bf-TT_GW8.GW8_PESOR /* b-itens-cliente-tr.qt-peso */.

                        FOR EACH it-nota-fisc NO-LOCK 
                           WHERE it-nota-fisc.cod-estabel = TT_GW1.GW1_FILIAL                        
                             AND it-nota-fisc.serie       = bf-TT_GW8.GW8_SERDC   
                             AND it-nota-fisc.nr-nota-fis = STRING(bf-TT_GW8.GW8_SERDC, "9999999")   
                             AND it-nota-fisc.it-codigo   = bf-TT_GW8.GW8_ITEM:
    
                            ASSIGN v-peso-liq-calc = v-peso-liq-calc + it-nota-fisc.peso-liq-fat.
                        END.
                    END.
                END.*/

                FIND FIRST nota-fiscal NO-LOCK 
                     WHERE nota-fiscal.cod-estabel = GW1_FILIAL 
                       AND nota-fiscal.serie       = GW1_SERDC  
                       AND nota-fiscal.nr-nota-fis = STRING(  INTEGER(GW1_NRDC) , "9999999") NO-ERROR.
                IF NOT AVAIL nota-fiscal THEN NEXT.
                
                RUN pi-preenche-dados-romaneio. 
                                          
                RUN pi-rateio-display (INPUT TT_GW1.GW1_FILIAL, /* cod-estabel */
                                       INPUT TT_GW1.GW1_SERDC,  /* serie       */
                                       INPUT TT_GW1.GW1_NRDC,   /* nr-nota-fis */
                                       INPUT TT_GW1.GW1_CDTPDC  /* cod-tipo-nf */
                                       ). 
            
                /*
                ASSIGN v-peso-bru-calc = 0
                       v-peso-liq-calc = 0. */

            END. /* TT_GW1 */ 

        END. /* TT_GW4 (docto-frete-nf) */ 
    END. /* for each TT_GW3 (docto-frete) */
                                                                       

    RUN pi-acompanhar in h-acomp(INPUT "PESQUISANDO FRETES DO RECEBIMENTO ").

    FOR EACH docum-est NO-LOCK
        WHERE (docum-est.cod-estabel >= c-cod-estabel-ini
          AND  docum-est.cod-estabel <= c-cod-estabel-fim)
          AND (docum-est.dt-trans    >= da-dt-entrada-doc-ini
          AND  docum-est.dt-trans    <= da-dt-entrada-doc-fim)
          AND (docum-est.dt-emissao  >= da-dt-emis-doc-ini
          AND  docum-est.dt-emissao  <= da-dt-emis-doc-fim)
     /*     AND DOCUM-est.nro-docto = '0000015' */ :

        FIND FIRST emitente                                                 
             WHERE emitente.cod-emitente = docum-est.cod-emitente NO-ERROR. 
        IF NOT AVAIL emitente THEN NEXT.
        
        FIND natur-oper
       WHERE natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.

        /*
        IF INT(SUBSTRING(docum-est.nat-operacao,1,4)) <> 1352 AND
           INT(SUBSTRING(docum-est.nat-operacao,1,4)) <> 2352 AND
           INT(SUBSTRING(docum-est.nat-operacao,1,4)) <> 1949 AND
           INT(SUBSTRING(docum-est.nat-operacao,1,4)) <> 2949
        THEN DO:
            RUN pi-acompanhar IN h-acomp(input "Desprezando Docto: "    +
                                               docum-est.cod-estabel    +
                                               "/"                      +
                                               docum-est.serie-docto    +
                                               "/"                      +
                                               docum-est.nro-docto).
            NEXT.
        END.*/

        IF docum-est.ce-atual = NO THEN NEXT.
        IF docum-est.cod-observa <> 4 /* sΩ entra servicos */ THEN NEXT.

        /* 2355743 */
         /*IF INT(natur-oper.cod-cfop) <> 1352 AND
            INT(natur-oper.cod-cfop) <> 2352 AND
            INT(natur-oper.cod-cfop) <> 1949 AND
            INT(natur-oper.cod-cfop) <> 2949 THEN 
             NEXT.*/
         
         /* 2355743 */
         IF INT(natur-oper.cod-cfop) <> 1352 AND
            INT(natur-oper.cod-cfop) <> 2352 AND
            INT(natur-oper.cod-cfop) <> 1949 AND
            INT(natur-oper.cod-cfop) <> 1905 AND
            INT(natur-oper.cod-cfop) <> 2949 THEN DO:

            FIND FIRST ext-natur-oper-cfop NO-LOCK
                 WHERE ext-natur-oper-cfop.nat-operacao = natur-oper.nat-operacao NO-ERROR.

            IF NOT AVAIL ext-natur-oper-cfop THEN
                NEXT.

            IF  NOT ext-natur-oper-cfop.mudanca-titularidade
            AND NOT ext-natur-oper-cfop.entrada-produtor
            AND NOT ext-natur-oper-cfop.entrada-deposito
            AND NOT ext-natur-oper-cfop.entrada-deposito-pj THEN
                NEXT.
         END.
        
        /* Desconsiderar documentos existentes no gfe */
        EMPTY TEMP-TABLE tt-cons-GW3.
        ASSIGN
            h_table-handle = TEMP-TABLE tt-cons-GW3:HANDLE
            c-tabela       = "GW3010"
            c-where        = "WHERE GW3_EMISDF = " + "'" + STRING(emitente.cgc)        + "'" +
                              " AND GW3_NRDF   = " + "'" + STRING(docum-est.nro-docto) + "'" + 
                              " AND GW3_SERDF  = " + "'" + docum-est.serie-docto       + "'" +
                             /* " AND GW3_CFOP   = " + "'" + docum-est.nat-operacao      + "'" + */
                              " AND GW3_DTEMIS = " + "'" + fnConvDataChar(docum-est.dt-emissao)  + "'" +
                              " AND D_E_L_E_T_   <> '*'".
        RUN ExecuteSelect IN hBoSQLCa (INPUT h_table-handle, INPUT c-where, INPUT c-tabela). 
        IF CAN-FIND(FIRST tt-cons-GW3) THEN NEXT.

        
        
        /*
        EMPTY TEMP-TABLE tt-cons-GW3.
        ASSIGN
            h_table-handle = TEMP-TABLE tt-cons-GW3:HANDLE
            c-tabela       = "GW3010"
            c-where        = "WHERE GW3_TPDF   = '5' "                                       + /* nota fiscal de servico */
                              " AND GW3_EMISDF = " + "'" + STRING(emitente.cgc)        + "'" +
                              " AND GW3_NRDF   = " + "'" + STRING(docum-est.nro-docto) + "'" + 
                              " AND GW3_SERDF  = " + "'" + docum-est.serie-docto       + "'" +
                       /*       " AND GW3_CFOP   = " + "'" + docum-est.nat-operacao      + "'" +  */
                              " AND GW3_DTEMIS = " + "'" + fnConvDataChar(docum-est.dt-emissao)  + "'".
        RUN ExecuteSelect IN hBoSQLCa (INPUT h_table-handle, INPUT c-where, INPUT c-tabela).
        IF CAN-FIND(FIRST tt-cons-GW3) THEN NEXT.*/

          
        FOR EACH ITEM-doc-est OF docum-est NO-LOCK: 
                                                                                         
            
            ASSIGN v-denominacao = IF AVAILABLE natur-oper THEN natur-oper.denominacao ELSE ""
                   v-nome-emit   = emitente.nome-emit.
            
            f-modalidade(emitente.cgc). /* retorna  v-via-transp */
            
            FIND FIRST grupo-fornec 
                 WHERE grupo-fornec.cod-gr-forn = emitente.cod-gr-forn NO-ERROR.
            IF AVAIL grupo-fornec THEN
                ASSIGN  v-gr-forn-descricao = grupo-fornec.descricao
                        v-cod-gr-forn       = emitente.cod-gr-forn.
            ELSE
                ASSIGN  v-gr-forn-descricao = " "
                        v-cod-gr-forn       = 0.
             
            FIND FIRST estabelec
                 WHERE estabelec.cod-estabel = docum-est.cod-estabel NO-ERROR.
            
            FIND FIRST ITEM                                                                                
                WHERE ITEM.it-codigo = ITEM-doc-est.it-codigo NO-ERROR.                                         
            
            ASSIGN v-desc-item  = " "
                   v-desc-estoq = " ".
            
            IF AVAIL ITEM THEN DO:
                v-desc-item = item.desc-item.                               
            
                FIND FIRST grup-estoque                                            
                     WHERE grup-estoque.ge-codigo = item.ge-codigo NO-ERROR.       
            
                IF AVAIL grup-estoq THEN
                   ASSIGN v-desc-estoq = grup-estoq.descricao.
            END.
             

            FIND cta_ctbl
           WHERE cta_ctbl.cod_cta_ctbl = item-doc-est.ct-codigo NO-LOCK NO-ERROR.
            IF AVAIL cta_ctbl THEN
                ASSIGN v-des-tit-ctbl = cta_ctbl.des_tit_ctbl.
            ELSE
                ASSIGN v-des-tit-ctbl = "".
         
         PUT UNFORMAT
                docum-est.cod-estabel                         "#"  /*  A */
                docum-est.nro-docto                            "#"  /*  B */
                docum-est.serie-docto                          "#"  /*  C */
                docum-est.dt-trans                                              "#"  /*  D */
                dt-emissao                                                      "#"  /*  E */
                docum-est.nat-operacao                         "#"  /*  F */
                natur-oper.denominacao                         "#"  /*  G */
                "DOCUMENTO FISCAL "                                             "#"  /*  H */
                " "                                                             "#"  /*  I */
                emitente.cgc                                   "#"  /*  J */
                docum-est.cod-emitente                                          "#"  /*  K */
                v-nome-emit                    FORMAT "x(60)"  "#"  /*  L */
                v-cod-gr-forn                                                   "#"  /*  M */
                v-gr-forn-descricao            FORMAT "x(60)"  "#"  /*  N */
                v-via-transp                                   "#"  /*  O */
                " "                                                             "#"  /*  P */
                " "                                                             "#"  /*  Q */
                " "                                                             "#"  /*  R */
                item-doc-est.preco-total[1]        FORMAT "->>>,>>>,>>>,>>9.99" "#"  /*  S */
                "0"                                                             "#"  /*  T */
                " "                                                             "#"  /*  U */
                item-doc-est.aliquota-icm                                       "#"  /*  V */
                item-doc-est.aliquota-iss          FORMAT "->>>,>>>,>>>,>>9.99" "#"  /*  W */
                docum-est.usuario                              "#"  /*  X */
                " "                                                             "#"  /*  Y */
                " "                                                             "#"  /*  Z */
                " "                                                             "#"  /* AA */
                " "                                                             "#"  /* AB */
                " "                                                             "#"  /* AC */
                " "                                                             "#"  /* AD */
                " "                                                             "#"  /* AE */
                " "                                                             "#"  /* AF */
                estabelec.cidade                               "#"  /* AG */
                estabelec.estado                               "#"  /* AH */
                " "                                                             "#"  /* AI */
                " "                                                             "#"  /* AJ */
                item-doc-est.num-pedido                                         "#"  /* AK */
                " "                                                             "#"  /* AL */
                item.it-codigo                                 "#"  /* AM */
                item.desc-item                                 "#"  /* AN */
                ITEM.ge-codigo                                                  "#"  /* AO */
                v-desc-estoq                                   "#"  /* AP */
                ITEM.un                                        "#"  /* AQ */
                item-doc-est.quantidade                                         "#"  /* AR */
                docum-est.tot-valor        FORMAT "->>>,>>>,>>>,>>9.99"         "#"  /* AS */
                docum-est.peso-bruto-tot   FORMAT "->>>,>>>,>>>,>>>,>>9.99999"  "#"  /* AT */
                docum-est.peso-liquido-tot FORMAT "->>>,>>>,>>>,>>>,>>9.99999"  "#"  /* AU */
                " "                                                             "#"  /* AV */
                " "                                                             "#"  /* AW */
                " "                                                             "#"  /* AX */
                " "                                                             "#"  /* AY */
                item-doc-est.ct-codigo                                          "#"  /* AZ */
                v-des-tit-ctbl                                                  "#"                          /* BA */    
                " "                                                                                     "#"  /* BB */    
                " "                                                                                     "#"  /* BC */    
                " "                                                                                     "#"  /* BD */    
                " "                                                                                     "#"  /* BE */    
                "RECEBIMENTO"                                                                           "#"  /* BF */    
                item-doc-est.preco-total[1] /*v-valor-despesa*/  FORMAT "->>>,>>>,>>>,>>>,>>9.99999"    "#"  /* BG */    
                0               FORMAT "->>>,>>>,>>>,>>>,>>9.99999"                                     "#"  /* BH */    
                0               FORMAT "->>>,>>>,>>>,>>>,>>9.99999"                                     "#"  /* BI */    
                0               FORMAT "->>>,>>>,>>>,>>>,>>9.99999"                                     "#"  /* BJ */    
                0               FORMAT "->>>,>>>,>>>,>>>,>>9.99999"                                     "#"  /* BK */    
                " "                                                                                     "#"  /* BL */
                SKIP.                                                                                  

        END.
    END.  /* for each docum-est */
END.      /* rs-tipo-selecao = 1 */                                                       
ELSE DO:  /* Selecao por Nota Fiscal */
    RUN p-preselect("GW1").
    FOR EACH TT_GW1:

        RUN pi-acompanhar IN h-acomp(INPUT "Nota Fiscal: "  +
                                     GW1_DTEMIS             +
                                     "  /  "                +
                                     GW1_FILIAL             +
                                     "/"                    +
                                     STRING(GW1_NRDC)       +
                                     "/"                    +
                                     GW1_SERDC).
        
        FIND FIRST nota-fiscal NO-LOCK WHERE
            nota-fiscal.cod-estabel = GW1_FILIAL AND
            nota-fiscal.serie       = GW1_SERDC  AND
            nota-fiscal.nr-nota-fis = STRING(  INTEGER(GW1_NRDC) , "9999999") NO-ERROR.
        IF NOT AVAIL nota-fiscal THEN NEXT.
        
        IF  nota-fiscal.esp-docto <> 22 AND   /* NF Saida */
            nota-fiscal.esp-docto <> 23       /* NF Transferencia */
        THEN NEXT.

        ASSIGN
            c-estab-docto-frete      = GW1_FILIAL
            c-docto-frete            = GW1_CDTPDC
            c-serie                  = GW1_SERDC
            d-criacao-docto-frete    = nota-fiscal.dt-emis-nota
        NO-ERROR.

        IF GW1_TPFRET <> "2" THEN v-tp-frete-con = "CIF".
                             ELSE v-tp-frete-con = "FOB".
        ASSIGN
            c-NF-Frete               = GW1_NRDC
            c-serie-NF               = GW1_SERDC
            i-especie-NF             = nota-fiscal.esp-docto
            dt-emis-nota             = fnConvCharData(GW1_DTEMIS)
        NO-ERROR.
        
        I-GW1_CDTPDC = INTEGER(GW1_CDTPDC) NO-ERROR.
        CASE I-GW1_CDTPDC >= 0:
             WHEN I-GW1_CDTPDC = 1 THEN v-tp-nf = "ENTREGA".
             WHEN I-GW1_CDTPDC = 2 THEN v-tp-nf = "COMPRAS".
             WHEN I-GW1_CDTPDC = 3 THEN v-tp-nf = "DEVOLUCAO".
             WHEN I-GW1_CDTPDC = 4 THEN v-tp-nf = "ANULACAO".
             OTHERWISE                  v-tp-nf = "OUTROS".
        END CASE.
        
        RUN p-preselect("GW4-2").
        FOR EACH TT_GW4:
            
            /*Calculo Frete Nota Fiscal */
            ASSIGN 
                /*v-vl-frete           = 0   */
                   v-tabela-frete       = 0 
                   v-cgc-tabela         = " " 
                   v-vl-pedagio         = 0. 
            
            FIND FIRST TT_GW3 WHERE
                    GW3_EMISDF = GW4_EMISDF 
                AND GW3_NRDF   = GW4_NRDF
                AND GW3_SERDF  = GW4_SERDF 
                AND GW3_DTEMIS = GW4_DTEMIS
                NO-ERROR.
            IF NOT AVAIL TT_GW3 THEN NEXT.
            
            ASSIGN 
                d-dtTransacao          = fnConvCharData(TT_GW3.GW3_DTFIS)
                v-esp-docto-frete      = TT_GW3.GW3_CDESP
                c-usuario-cria-doc-frt = TT_GW3.GW3_USUIMP
                d-lib-docto-frete      = fnConvCharData(TT_GW3.GW3_DTAPR)
            NO-ERROR.

            ASSIGN
                d-aliq-icms = 0
                d-aliq-iss  = 0.
    
            FIND TT_GVT WHERE TT_GVT.GVT_CDESP = TT_GW3.GW3_CDESP NO-ERROR.
            IF AVAILABLE TT_GVT THEN
                ASSIGN
                    d-aliq-icms = TT_GW3.GW3_PCIMP WHEN GVT_TPIMP = "1"
                    d-aliq-iss  = TT_GW3.GW3_PCIMP WHEN GVT_TPIMP = "2"
                NO-ERROR.
            
            RUN pi-preenche-dados-ctrc.

            v-via-transp = "".
            FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = INTEGER(GW4_EMISDF) NO-ERROR.
            IF AVAILABLE emitente THEN f-modalidade(emitente.cgc). /* retorna  v-via-transp */.
            
            ASSIGN
                v-nr-fatura      = ""
                v-serie-fat      = ""
                v-dt-aprov       = ?
                v-dt-integr-ap   = ?
                v-user-integr-ap = ""
            NO-ERROR.
            
            RUN p-preselect("GW6").
            FIND LAST TT_GW6 NO-ERROR.
            IF AVAILABLE TT_GW6 THEN DO:
                ASSIGN 
                    v-nr-fatura      = TT_GW6.GW6_NRFAT
                    v-serie-fat      = TT_GW6.GW6_SERFAT
                    v-dt-aprov       = fnConvCharData(TT_GW6.GW6_DTAPR)
                    v-dt-integr-ap   = fnConvCharData(TT_GW6.GW6_DTFIN)
                    v-user-integr-ap = TT_GW6.GW6_USUFIN
                NO-ERROR.
            END.

            RUN p-preselect("GWH").
            FOR EACH TT_GWH:
                
                /*Calculo Frete Nota Fiscal */
                ASSIGN
                   /* v-vl-frete      = 0 */
                    v-tabela-frete  = 0 
                    v-cgc-tabela    = " "
                    i-nr-pre-con    = 0
                    c-estab-pre-con = ""
                NO-ERROR.

                RUN p-preselect("GWF").
                FIND FIRST TT_GWF NO-LOCK NO-ERROR.
                IF NOT AVAIL TT_GWF THEN NEXT.
                
                RUN pi-preenche-valores-calculo.
                
                /* UTILIZADO NA LEITURA DO CµLCULO DE v-peso-bru-calc / v-peso-liq-calc */
                ASSIGN i-nr-pre-con    = INTEGER(TT_GWF.GWF_NRCALC)     /* pre-con.nr-calculo */
                       c-estab-pre-con = TT_GWF.GWF_FILIAL              /* pre-con.cod-estabel */
                NO-ERROR.
            END.
            
            /*
            ASSIGN 
                v-peso-bru-calc = 0
                v-peso-liq-calc = 0. */
            
            /*
            RUN p-preselect("GWH-2").
            FOR EACH bf-TT_GWH
                :
                
                RUN p-preselect("GW8-2").
                FOR EACH bf-TT_GW8
                    :

                    v-peso-bru-calc = v-peso-bru-calc + bf-TT_GW8.GW8_PESOR /* b-itens-cliente-tr.qt-peso */.

                    FOR EACH it-nota-fisc NO-LOCK WHERE
                        it-nota-fisc.cod-estabel = TT_GW1.GW1_FILIAL                        AND
                        it-nota-fisc.nr-nota-fis = STRING(bf-TT_GW8.GW8_SERDC, "9999999")   AND   
                        it-nota-fisc.serie       = bf-TT_GW8.GW8_SERDC                      AND
                        it-nota-fisc.it-codigo   = bf-TT_GW8.GW8_ITEM
                        :

                       ASSIGN v-peso-liq-calc = v-peso-liq-calc + it-nota-fisc.peso-liq-fat.
                    END.
                END.
            END.*/
            
            RUN pi-preenche-dados-romaneio.
            
            RUN pi-rateio-display (INPUT TT_GW1.GW1_FILIAL, /* cod-estabel */
                                   INPUT TT_GW1.GW1_SERDC,  /* serie       */
                                   INPUT TT_GW1.GW1_NRDC,   /* nr-nota-fis */
                                   INPUT TT_GW1.GW1_CDTPDC  /* cod-tipo-nf */
                                   ).

        
        END. /* FOR EACH docto-frete-nf */
    END. /* FOR EACH GW1 (nota-fiscal-tr) */
END. /* rs-tipo-selecao = 2 */

RUN pi-desconectar.
/*fechamento do output do relat´rio*/
{include/i-rpclo.i}

IF VALID-HANDLE(h-acomp) THEN 
    RUN pi-finalizar IN h-acomp NO-ERROR.

RETURN 'OK'.

/* **********************  Internal Procedures  *********************** */
PROCEDURE pi-preenche-dados-ctrc:
    
    /* RUN p-preselect("GVA"). */
    
    ASSIGN 
        v-estabelec-cidade = ""
        v-estabelec-estado = ""
        v-cnpj-emit         = " "
        v-gr-forn-descricao = " "
        v-cod-gr-forn       = 0
        v-nome-emit-transp  = " "
        v-cod-fornec        = 0.
     /*   v-tem-tab-frete     = CAN-FIND(FIRST TT_GVA) */
 
    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = TT_GW3.GW3_FILIAL NO-LOCK NO-ERROR.

    IF AVAIL estabelec THEN
        ASSIGN v-estabelec-cidade = estabelec.cidade
               v-estabelec-estado = estabelec.estado.

    /*RUN p-preselect("GU3").
    FIND FIRST TT_GU3 NO-ERROR.
    IF AVAILABLE TT_GU3 THEN DO:
    */    
      /*  v-cnpj-emit = TT_GU3.GU3_IDFED. */


        FIND FIRST emitente 
            WHERE emitente.cod-emitente = INT(TT_GW3.GW3_EMISDF) NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN DO:
            ASSIGN 
                v-nome-emit-transp  = emitente.nome-emit
                v-cod-fornec        = emitente.cod-emitente
                v-cnpj-emit         = emitente.cgc.
          
            FIND FIRST grupo-fornec 
                 WHERE grupo-fornec.cod-gr-forn = emitente.cod-gr-forn NO-ERROR.
            IF AVAIL grupo-fornec THEN
                ASSIGN
                    v-gr-forn-descricao = grupo-fornec.descricao
                    v-cod-gr-forn       = emitente.cod-gr-forn.
        END.
    /*END. */
    
    /*
    /* Busca peso bruto do docto frete para o rateio */
    ASSIGN v-peso-bru-doc = 0
           v-peso-liq-doc = 0.
    
    RUN p-preselect("GW4").
    FOR EACH TT_GW4:
        
        RUN p-preselect("GW8-3").
        FOR EACH B-TT_GW8:
             
            v-peso-bru-doc = v-peso-bru-doc + B-TT_GW8.GW8_PESOR. /* itens-cliente-tr.qt-peso.  */
            
            FOR EACH it-nota-fisc NO-LOCK WHERE
                it-nota-fisc.cod-estabel = TT_GW3.GW3_FILIAL                    AND
                it-nota-fisc.serie       = B-TT_GW8.GW8_SERDC                   AND
                it-nota-fisc.nr-nota-fis = string(B-TT_GW8.GW8_NRDC, "9999999") AND
                it-nota-fisc.it-codigo   = B-TT_GW8.GW8_ITEM
                :
                
                v-peso-liq-doc = v-peso-liq-doc + it-nota-fisc.peso-liq-fat.
            END.
        END.
    END. */ /* FOR EACH docto-frete-nf */
    
END PROCEDURE.


PROCEDURE pi-preenche-valores-calculo:
    
    /* faz a leitura do componente do calculo (GWG) pela chave do calculo que encontrou na tabela GWH */
    RUN p-preselect("GWG").
    FOR EACH TT_GWG:
        ASSIGN v-tabela-frete     = INTEGER(TT_GWG.GWG_NRTAB) /* pre-con-calc.nr-tabela-frete - CAMPO 16 */
               v-cgc-tabela       =         TT_GWG.GWG_CDEMIT /* pre-con-calc.cgc-tabela      - CAMPO 17 */
               v-tem-tab-frete    = TRUE.
    END.

    /*
    RUN p-preselect("GWM-CALC").
    FOR EACH TT_GWM_CALC NO-LOCK:
        ASSIGN v-vl-frete = v-vl-frete + DEC(TT_GWM_CALC.GWM_VLFRET).
    END. */
    
END PROCEDURE.


PROCEDURE pi-preenche-dados-romaneio:
    
    ASSIGN v-placa           = ""
           v-tipo-veiculo    = ""
           v-produto-frete   = ""
    NO-ERROR.
    
    RUN p-preselect("GWN").
    FOR FIRST TT_GWN:
        
        ASSIGN v-placa = TT_GWN.GWN_PLACAD.

        RUN p-preselect("GV3").
        FOR FIRST TT_GV3:
            ASSIGN v-tipo-veiculo = TT_GV3.GV3_DSTPVC.
        END.

        FOR FIRST TT_GUB WHERE TT_GUB.GUB_CDCLFR = TT_GWN.GWN_CDCLFR:
            ASSIGN v-produto-frete = TT_GUB.GUB_DSCLFR.
        END.
        
    END.

END PROCEDURE.


PROCEDURE pi-rateio-display:
    DEFINE INPUT  PARAMETER ipch-cod-estabel AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-serie       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-nr-nota-fis AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-cdtpdc      AS CHARACTER   NO-UNDO. /* GW1_CDTPDC */
    
    ASSIGN v-cod-emitente = 0
           v-nome-emit    = " ".
    
    FIND FIRST nota-fiscal
         WHERE nota-fiscal.cod-estabel = ipch-cod-estabel 
           AND nota-fiscal.serie       = ipch-serie     
           AND nota-fiscal.nr-nota-fis = STRING(ipch-nr-nota-fis,"9999999") NO-LOCK NO-ERROR.


/*     MESSAGE                                    */
/*         ipch-cod-estabel                  SKIP */
/*         ipch-serie                        SKIP */
/*         STRING(ipch-nr-nota-fis,"9999999")     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.         */

    
    FIND FIRST TT_GV5 WHERE TT_GV5.GV5_CDTPDC = ipch-cdtpdc NO-ERROR.
    IF NOT AVAILABLE TT_GV5 THEN NEXT.    

    IF GV5_SENTID = "2" AND AVAIL nota-fiscal THEN DO:
        
        ASSIGN
            v-esp-docto-nf = "   "
            v-nr-pedcli    = " "
            v-desc-sit-ped = " "
            v-cod-emitente = 0
            v-nome-emit    = " ".
        
        CASE nota-fiscal.esp-docto:
            WHEN 20 THEN v-esp-docto-nf = "NFD".
            WHEN 21 THEN v-esp-docto-nf = "NFE".
            WHEN 22 THEN v-esp-docto-nf = "NFS".
            WHEN 23 THEN v-esp-docto-nf = "NFT".
        END CASE.
        
        FIND FIRST emitente
             WHERE emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN
           ASSIGN v-cod-emitente = emitente.cod-emitente
                  v-nome-emit    = emitente.nome-emit.

        FIND FIRST ped-venda
             WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
                   ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
            NO-LOCK NO-ERROR.
        IF AVAIL ped-venda THEN DO:
            v-nr-pedcli = ped-venda.nr-pedcli.

            CASE ped-venda.cod-sit-ped:
                WHEN 1 THEN v-desc-sit-ped =  "ABERTO".
                WHEN 2 THEN v-desc-sit-ped =  "ATENDIDO PARCIAL".
                WHEN 3 THEN v-desc-sit-ped =  "ATENDIDO TOTAL".
                WHEN 5 THEN v-desc-sit-ped =  "SUSPENSO".
                WHEN 6 THEN v-desc-sit-ped =  "CANCELADO".
            END CASE.
        END.
                             
        RUN p-preselect("GWM-GERAL").

        
        FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
                       
            /*
            RUN pi-acompanhar IN h-acomp(INPUT /*"Lendo Conhec: "                   + */
                                           STRING(fnConvCharData(TT_GW3.GW3_DTFIS)) +
                                           "  /  "                           +
                                           TT_GW3.GW3_FILIAL                 +
                                           "/"                               +
                                           TT_GW3.GW3_NRDF                   +
                                           "/"                               +
                                           TT_GW3.GW3_SERDF                  + 
                                           "  /  "                           +
                                           STRING(it-nota-fisc.nr-seq-fat)).  */  
             
            
            
            RUN p-limpa-var.
            
            FIND ITEM                                                                                
           WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN DO:

                ASSIGN v-desc-item = item.desc-item.                               

                FIND grup-estoque
               WHERE grup-estoque.ge-codigo = item.ge-codigo NO-LOCK NO-ERROR.
                IF AVAIL grup-estoq THEN                                           
                    ASSIGN v-desc-estoq = grup-estoq.descricao.                     

                ASSIGN
                    c-cod-estabel = it-nota-fisc.cod-estabel       
                    c-serie-docto = it-nota-fisc.serie             
                    c-nr-nota-fis = it-nota-fisc.nr-nota-fis       
                    i-nr-seq-fat  = it-nota-fisc.nr-seq-fat
                    c-it-codigo   = it-nota-fisc.it-codigo
                    c_GW1_CDTPDC  = TT_GW1.GW1_CDTPDC
                    c_GW1_EMISDC  = TT_GW1.GW1_EMISDC.


                /* RUN p-preselect("GWM-gfe"). */
                FOR EACH TT_GWM
                   WHERE TT_GWM.GWM_FILIAL =  c-cod-estabel       
                     AND TT_GWM.GWM_EMISDC =  c_GW1_EMISDC        
                     AND TT_GWM.GWM_CDTPDC =  c_GW1_CDTPDC        
                     AND TT_GWM.GWM_SERDC  =  c-serie-docto       
                     AND TT_GWM.GWM_NRDC   =  c-nr-nota-fis       
                     AND TT_GWM.GWM_TPDOC  =  '1'                 
                     AND TT_GWM.GWM_SEQGW8 =  STRING(i-nr-seq-fat)
                     AND TT_GWM.GWM_ITEM   =  c-it-codigo:
                                        
                    ASSIGN v-rateio-calc-gfe = TT_GWM.GWM_VLFRET.
                END.


                /* RUN p-preselect("GWM-conhec"). */
                FOR EACH TT_GWM
                   WHERE TT_GWM.GWM_FILIAL  = TT_GW3.GW3_FILIAL             
                     AND TT_GWM.GWM_EMISDC  = c_GW1_EMISDC       
                     AND TT_GWM.GWM_CDTPDC  = c_GW1_CDTPDC       
                     AND TT_GWM.GWM_SERDC   = c-serie-docto      
                     AND TT_GWM.GWM_NRDC    = c-nr-nota-fis      
                     AND TT_GWM.GWM_CDESP   = GW3_CDESP          
                     AND TT_GWM.GWM_CDTRP   = GW3_EMISDF         
                     AND TT_GWM.GWM_SERDOC  = GW3_SERDF          
                     AND TT_GWM.GWM_NRDOC   = GW3_NRDF           
                     AND TT_GWM.GWM_DTEMIS  = GW3_DTEMIS         
               /*      AND (TT_GWM.GWM_TPDOC   = '2' OR TT_GWM.GWM_TPDOC = '5')   */
                     AND TT_GWM.GWM_SEQGW8  = STRING(i-nr-seq-fat)    
                     AND TT_GWM.GWM_ITEM    = c-it-codigo:
                                                   
                    IF LOOKUP(TT_GWM.GWM_TPDOC,"2,5") = 0 THEN
                            NEXT.

                    ASSIGN v-rateio-conhec      = TT_GWM.GWM_VLFRET
                           v-conta-contabil-gfe = TT_GWM.GWM_CTFRET                          
                           v-data-contabil      = fnConvCharData(GWM_DTEMIS)                 
                           v-valor-despesa      = TT_GWM.GWM_VLFRET               
                           v-valor-icms         = TT_GWM.GWM_VLICMS                                 
                           v-conta-despesa      = TT_GWM.GWM_CTFRET
                           v-valor-iss          = TT_GWM.GWM_VLISS.

                    IF AVAILABLE TT_GW3 THEN DO:
                         
                        IF TT_GW3.GW3_VLPIS > 0 THEN    
                            ASSIGN v-valor-pis     = TT_GWM.GWM_VLPIS.

                        IF TT_GW3.GW3_VLCOF > 0 THEN    
                            ASSIGN v-valor-cofins  = TT_GWM.GWM_VLCOFI.

                    END.
                END.   
            END. /* AVAIL ITEM */

            FOR FIRST TT_GUE WHERE TT_GUE.GUE_CTACTB = v-conta-contabil-gfe:
                ASSIGN v-desc-conta-contabil-gfe = TT_GUE.GUE_TITULO.
            END.
            
            IF v-rateio-calc-gfe = ? THEN v-rateio-calc-gfe = 0.
            IF v-rateio-conhec   = ? THEN v-rateio-conhec   = 0.
            IF v-gfe-conhec      = ? THEN v-gfe-conhec      = 0.

            ASSIGN v-gfe-conhec = v-rateio-calc-gfe - v-rateio-conhec.
            

            PUT UNFORMAT
                   c-estab-docto-frete                                                 "#"  /*  A */ 
                   c-docto-frete                                                       "#"  /*  B */
                   c-serie                                                             "#"  /*  C */
                   d-dtTransacao                                                                        "#"  /*  D */
                   d-criacao-docto-frete                                                                "#"  /*  E */
                   c-natureza-docto-frete                                              "#"  /*  F */
                   v-denominacao                        FORMAT "x(40)"                "#"  /*  G */
                   v-esp-docto-frete                    FORMAT "x(30)"                "#"  /*  H */
                   v-tp-frete-con                        FORMAT "x(30)"                "#"  /*  I */
                   v-cnpj-emit                                                         "#"  /*  J */
                   v-cod-fornec                                                                         "#"  /*  K */
                   v-nome-emit-transp                    FORMAT "x(60)"                "#"  /*  L */
                   v-cod-gr-forn                                                                        "#"  /*  M */
                   v-gr-forn-descricao                   FORMAT "x(60)"                "#"  /*  N */
                   v-via-transp                          FORMAT "x(40)"                "#"  /*  O */
                   v-tem-tab-frete                                                                      "#"  /*  P */
                   v-tabela-frete                                                                       "#"  /*  Q */
                   v-cgc-tabela                                                        "#"  /*  R */
                   v-rateio-conhec                       FORMAT ">>>,>>>,>>9.99"       "#"  /*  S */
                   v-rateio-calc-gfe                                       FORMAT ">>>,>>>,>>9.99"       "#"  /*  T */
                   v-gfe-conhec                                            FORMAT "->>>,>>>,>>9.99"      "#"  /*  U */
                   d-aliq-icms                                                                          "#"  /*  V */
                   d-aliq-iss                                                                           "#"  /*  W */
                   c-usuario-cria-doc-frt                FORMAT "x(15)"                "#"  /*  X */
                   c-NF-Frete                                             FORMAT "x(07)"                "#"  /*  Y */
                   c-serie-NF                            FORMAT "x(05)"                "#"  /*  Z */
                   v-esp-docto-nf                        FORMAT "x(03)"                "#"  /* AA */
                   dt-emis-nota                                                                         "#"  /* AB */
                   nota-fiscal.cod-emitente                                                             "#"  /* AC */
                   v-nome-emit                           FORMAT "x(60)"                "#"  /* AD */
                   nota-fiscal.placa                     FORMAT "x(50)"                "#"  /* AE */                                 
                   nota-fiscal.nr-embarque                                                              "#"  /* AF */                                     
                   v-estabelec-cidade                      FORMAT "x(30)"              "#"  /* AG */                      
                   v-estabelec-estado                                                  "#"  /* AH */                      
                   nota-fiscal.cidade                    FORMAT "x(30)"                "#"  /* AI */                     
                   nota-fiscal.estado                                                  "#"  /* AJ */                     
                   v-nr-pedcli                                                         "#"  /* AK */
                   v-desc-sit-ped                        FORMAT "x(50)"                "#"  /* AL */                                       
                   it-nota-fisc.it-codigo                                                               "#"  /* AM */
                   v-desc-item                           FORMAT "x(60)"                "#"  /* AN */
                   ITEM.ge-codigo                                                                       "#"  /* AO */
                   v-desc-estoq                          FORMAT "x(30)"                "#"  /* AP */
                   ITEM.un                                                             "#"  /* AQ */                     
                   it-nota-fisc.qt-faturada[1]                                                          "#"  /* AR */
                   it-nota-fisc.vl-tot-item                               FORMAT ">>>,>>>,>>9.99999"    "#"  /* AS */                    
                   it-nota-fisc.peso-bruto                                FORMAT ">>>,>>>,>>9.99999"    "#"  /* AT */                     
                   it-nota-fisc.peso-liq-fat                              FORMAT ">>>,>>>,>>9.99999"    "#"  /* AU */                     
                   v-situacao-conhec                     FORMAT "x(50)"                "#"  /* AV */
                   v-nr-fatura                                                                          "#"  /* AW */
                   v-serie-fat                                                         "#"  /* AX */
                   v-dt-aprov                                                                           "#"  /* AY */
                   "  "                                                                                 "#"  /* AZ */
                   "  "                                                                                 "#"  /* BA */         
                   v-conta-contabil-gfe                                                                 "#"  /* BB */         
                   v-desc-conta-contabil-gfe            FORMAT "x(32)"                                  "#"  /* BC */         
                   v-tipo-veiculo                                                                       "#"  /* BD */         
                   v-produto-frete                       FORMAT "x(13)"                                 "#"  /* BE */         
                   "GFE"                                                                                "#"  /* BF */         
                   v-rateio-conhec                                        FORMAT "->>>,>>>,>>9.99"      "#"  /* BG */         
                   v-valor-icms                                           FORMAT "->>>,>>>,>>9.99"      "#"  /* BH */         
                   v-valor-iss                                            FORMAT "->>>,>>>,>>9.99"      "#"  /* BI */         
                   v-valor-pis                                            FORMAT "->>>,>>>,>>9.99"      "#"  /* BJ */         
                   v-valor-cofins                                         FORMAT "->>>,>>>,>>9.99"      "#"  /* BK */         
                   v-conta-despesa                                                                      "#"  /* BL */
                   SKIP.
                
       END. /* FOR EACH it-nota-fisc */ 
    END. /* AVAIL nota-fiscal */
    ELSE DO:

        FIND FIRST docum-est NO-LOCK WHERE
                   docum-est.cod-emitente = INTEGER(TT_GW1.GW1_EMISDC) AND
                   docum-est.nro-docto    = TT_GW1.GW1_NRDC     AND
                   docum-est.serie-docto  = TT_GW1.GW1_SERDC    AND
                   docum-est.nat-operacao = TT_GW3.GW3_CFOP
            NO-ERROR.

        IF AVAIL docum-est THEN DO:
            
            CASE docum-est.esp-docto:
                WHEN 2  THEN ASSIGN v-esp-docto-nf = "ACT".       
                WHEN 3  THEN ASSIGN v-esp-docto-nf = "CAC".       
                WHEN 4  THEN ASSIGN v-esp-docto-nf = "DD".        
                WHEN 5  THEN ASSIGN v-esp-docto-nf = "DEV".       
                WHEN 6  THEN ASSIGN v-esp-docto-nf = "DIV".       
                WHEN 7  THEN ASSIGN v-esp-docto-nf = "DRM".       
                WHEN 8  THEN ASSIGN v-esp-docto-nf = "EAC".       
                WHEN 9  THEN ASSIGN v-esp-docto-nf = "EGF".       
                WHEN 10 THEN ASSIGN v-esp-docto-nf = "BEM".       
                WHEN 11 THEN ASSIGN v-esp-docto-nf = "ESP".         
                WHEN 12 THEN ASSIGN v-esp-docto-nf = "GRN".       
                WHEN 13 THEN ASSIGN v-esp-docto-nf = "GTN".       
                WHEN 14 THEN ASSIGN v-esp-docto-nf = "ICM".       
                WHEN 15 THEN ASSIGN v-esp-docto-nf = "INV".       
                WHEN 16 THEN ASSIGN v-esp-docto-nf = "IPL".       
                WHEN 17 THEN ASSIGN v-esp-docto-nf = "MOB".       
                WHEN 18 THEN ASSIGN v-esp-docto-nf = "NC".        
                WHEN 19 THEN ASSIGN v-esp-docto-nf = "NF".        
                WHEN 20 THEN ASSIGN v-esp-docto-nf = "NFD".        
                WHEN 21 THEN ASSIGN v-esp-docto-nf = "NFE".        
                WHEN 22 THEN ASSIGN v-esp-docto-nf = "NFS".        
                WHEN 23 THEN ASSIGN v-esp-docto-nf = "NFT".        
                WHEN 24 THEN ASSIGN v-esp-docto-nf = "PRA".        
                WHEN 25 THEN ASSIGN v-esp-docto-nf = "REF".        
                WHEN 26 THEN ASSIGN v-esp-docto-nf = "RCS".        
                WHEN 27 THEN ASSIGN v-esp-docto-nf = "RDD".        
                WHEN 28 THEN ASSIGN v-esp-docto-nf = "REQ".        
                WHEN 29 THEN ASSIGN v-esp-docto-nf = "RFS".        
                WHEN 30 THEN ASSIGN v-esp-docto-nf = "RM".        
                WHEN 31 THEN ASSIGN v-esp-docto-nf = "RRQ".        
                WHEN 32 THEN ASSIGN v-esp-docto-nf = "STR".        
                WHEN 33 THEN ASSIGN v-esp-docto-nf = "TRA".        
                WHEN 34 THEN ASSIGN v-esp-docto-nf = "ZZZ".        
                WHEN 35 THEN ASSIGN v-esp-docto-nf = "SOB".        
                WHEN 36 THEN ASSIGN v-esp-docto-nf = "EDD".        
                WHEN 37 THEN ASSIGN v-esp-docto-nf = "VAR".        
                OTHERWISE    ASSIGN v-esp-docto-nf = "   ".        
            END CASE.
                                                                                                                          
            FIND FIRST emitente                                                                                           
                WHERE emitente.cod-emit = docum-est.cod-emitente NO-LOCK NO-ERROR.                                    
                                                                                                                          
            IF AVAIL emitente THEN                                                                                        
                ASSIGN v-nome-emit  = emitente.nome-emit
                       v-cidade-ori = emitente.cidade
                       v-uf-ori     = emitente.estado.                                                                
            ELSE                                                                                                          
                ASSIGN v-nome-emit  = " "
                       v-cidade-ori = " "
                       v-uf-ori     = " ".

            ASSIGN v-cod-emitente = docum-est.cod-emitente.

            RUN p-preselect("GWM-GERAL").

            FOR EACH item-doc-est OF docum-est NO-LOCK:                                    
                
                RUN p-preselect("GW8").
                FIND FIRST TT_GW8 NO-ERROR.
                IF NOT AVAIL TT_GW8 THEN NEXT.
                
                RUN p-limpa-var.
                
                FIND FIRST ITEM WHERE ITEM.it-codigo = TT_GW8.GW8_ITEM NO-LOCK NO-ERROR.
                IF AVAIL ITEM THEN DO:
     
                    ASSIGN v-desc-item = item.desc-item.
    
                    FIND FIRST grup-estoque
                        WHERE grup-estoque.ge-codigo = item.ge-codigo NO-LOCK NO-ERROR.
    
                    IF AVAIL grup-estoq THEN
                        ASSIGN v-desc-estoq = grup-estoq.descricao.                     
   
                    ASSIGN                                      
                        c-cod-estabel = item-doc-est.cod-estab-compon
                        c-serie-docto = item-doc-est.serie-docto
                        c-nr-nota-fis = item-doc-est.nro-docto  
                        i-nr-seq-fat  = item-doc-est.sequencia  
                        c-it-codigo   = item-doc-est.it-codigo  
                        c_GW1_CDTPDC  = TT_GW1.GW1_CDTPDC
                        c_GW1_EMISDC  = TT_GW1.GW1_EMISDC.
                    
                    /* RUN p-preselect("GWM-gfe"). */
                    FOR EACH TT_GWM
                       WHERE TT_GWM.GWM_FILIAL =  c-cod-estabel       
                         AND TT_GWM.GWM_EMISDC =  c_GW1_EMISDC        
                         AND TT_GWM.GWM_CDTPDC =  c_GW1_CDTPDC        
                         AND TT_GWM.GWM_SERDC  =  c-serie-docto       
                         AND TT_GWM.GWM_NRDC   =  c-nr-nota-fis       
                         AND TT_GWM.GWM_TPDOC  =  '1'                 
                         AND TT_GWM.GWM_SEQGW8 =  STRING(i-nr-seq-fat)
                         AND TT_GWM.GWM_ITEM   =  c-it-codigo:
                        ASSIGN v-rateio-calc-gfe = TT_GWM.GWM_VLFRET.
                    END.
                    
                    
                    /* RUN p-preselect("GWM-conhec"). */
                    FOR EACH TT_GWM
                       WHERE TT_GWM.GWM_FILIAL  = TT_GW3.GW3_FILIAL             
                         AND TT_GWM.GWM_EMISDC  = c_GW1_EMISDC       
                         AND TT_GWM.GWM_CDTPDC  = c_GW1_CDTPDC       
                         AND TT_GWM.GWM_SERDC   = c-serie-docto      
                         AND TT_GWM.GWM_NRDC    = c-nr-nota-fis      
                         AND TT_GWM.GWM_CDESP   = GW3_CDESP          
                         AND TT_GWM.GWM_CDTRP   = GW3_EMISDF         
                         AND TT_GWM.GWM_SERDOC  = GW3_SERDF          
                         AND TT_GWM.GWM_NRDOC   = GW3_NRDF           
                         AND TT_GWM.GWM_DTEMIS  = GW3_DTEMIS         
                    /*     AND (TT_GWM.GWM_TPDOC   = '2' OR TT_GWM.GWM_TPDOC = '5')  */
                         AND TT_GWM.GWM_SEQGW8  = STRING(i-nr-seq-fat)    
                         AND TT_GWM.GWM_ITEM    = c-it-codigo:
                                                       
                        IF LOOKUP(TT_GWM.GWM_TPDOC,"2,5") = 0 THEN
                            NEXT.

                        ASSIGN v-rateio-conhec      = TT_GWM.GWM_VLFRET
                               v-conta-contabil-gfe = TT_GWM.GWM_CTFRET                          
                               v-data-contabil      = fnConvCharData(GWM_DTEMIS)                 
                               v-valor-despesa      = TT_GWM.GWM_VLFRET               
                               v-valor-icms         = TT_GWM.GWM_VLICMS                                 
                               v-conta-despesa      = TT_GWM.GWM_CTFRET
                               v-valor-iss          = TT_GWM.GWM_VLISS.
                        
                        IF AVAILABLE TT_GW3 THEN DO:
                             
                            IF TT_GW3.GW3_VLPIS > 0 THEN    
                                ASSIGN v-valor-pis     = TT_GWM.GWM_VLPIS.
                        
                            IF TT_GW3.GW3_VLCOF > 0 THEN    
                                ASSIGN v-valor-cofins  = TT_GWM.GWM_VLCOFI.
                        
                        END.
                        

                    END.   

                    /*
                    RUN p-preselect("GWM-gfe").
                    FOR EACH TT_GWM-gfe:
                        v-rateio-calc-gfe = TT_GWM-gfe.GWM_VLFRET.
                    END.
                    
                    RUN p-preselect("GWM-conhec").
                    FOR EACH TT_GWM-con:
                        v-rateio-conhec = TT_GWM-con.GWM_VLFRET.
                    END.*/

                END. /* AVAIL ITEM */

                IF v-conta-contabil-gfe = "" THEN
                    ASSIGN v-conta-contabil-gfe = item-doc-est.conta-contabil.
                
                IF v-rateio-calc-gfe = ? THEN v-rateio-calc-gfe = 0.
                IF v-rateio-conhec   = ? THEN v-rateio-conhec   = 0.
                IF v-gfe-conhec      = ? THEN v-gfe-conhec      = 0.

                v-gfe-conhec      = v-rateio-calc-gfe - v-rateio-conhec.
                
                PUT UNFORMAT
                       c-estab-docto-frete                                                 "#"  /*  A */
                       c-docto-frete                                                      "#"  /*  B */
                       c-serie                                                             "#"  /*  C */
                       d-dtTransacao                                                                        "#"  /*  D */
                       d-criacao-docto-frete                                                                "#"  /*  E */
                       c-natureza-docto-frete                                              "#"  /*  F */
                       v-denominacao                         FORMAT "x(40)"                "#"  /*  G */
                       v-esp-docto-frete                     FORMAT "x(30)"                "#"  /*  H */
                       v-tp-frete-con                       FORMAT "x(30)"                "#"  /*  I */
                       v-cnpj-emit                                                         "#"  /*  J */
                       v-cod-fornec                                                                         "#"  /*  K */
                       v-nome-emit-transp                   FORMAT "x(60)"                "#"  /*  L */                                               
                       v-cod-gr-forn                                                                        "#"  /*  M */
                       v-gr-forn-descricao                  FORMAT "x(60)"                "#"  /*  N */
                       v-via-transp                         FORMAT "x(40)"                "#"  /*  O */
                       v-tem-tab-frete                                                                      "#"  /*  P */
                       v-tabela-frete                                                                       "#"  /*  Q */
                       v-cgc-tabela                                                        "#"  /*  R */
                       v-rateio-conhec                                        FORMAT ">>>,>>>,>>9.99"       "#"  /*  S */
                       v-rateio-calc-gfe                                      FORMAT ">>>,>>>,>>9.99"       "#"  /*  T */
                       v-gfe-conhec                                           FORMAT "->>>,>>>,>>9.99"      "#"  /*  U */
                       d-aliq-icms                                                                          "#"  /*  V */
                       d-aliq-iss                                                                           "#"  /*  W */
                       c-usuario-cria-doc-frt                FORMAT "x(15)"                "#"  /*  X */
                       c-NF-Frete                                             FORMAT "x(07)"                "#"  /*  Y */
                       c-serie-NF                            FORMAT "x(05)"                "#"  /*  Z */
                       v-esp-docto-nf                      FORMAT "x(03)"                "#"  /* AA */
                       dt-emis-nota                                                                         "#"  /* AB */
                       docum-est.cod-emitente                                                               "#"  /* AC */
                       v-nome-emit                           FORMAT "x(60)"                "#"  /* AD */
                       " "                                                                                  "#"  /* AE */                                          
                       " "                                                                                  "#"  /* AF */                                              
                       v-cidade-ori                         FORMAT "x(30)"                "#"  /* AG */                              
                       v-uf-ori                                                          "#"  /* AH */                              
                       v-estabelec-cidade                    FORMAT "x(30)"                "#"  /* AI */                              
                       v-estabelec-estado                                                  "#"  /* AJ */                              
                       " "                                                                                  "#"  /* AK */
                       " "                                                                                  "#"  /* AL */                                                
                       item-doc-est.it-codigo                                                               "#"  /* AM */
                       v-desc-item                           FORMAT "x(60)"                "#"  /* AN */
                       ITEM.ge-codigo                                                                       "#"  /* AO */
                       v-desc-estoq                          FORMAT "x(30)"                "#"  /* AP */
                       ITEM.un                                                             "#"  /* AQ */                              
                       TT_GW8.GW8_QTDE                                                                      "#"  /* AR */
                       TT_GW8.GW8_VALOR                                       FORMAT ">>>,>>>,>>9.99999"    "#"  /* AS */
                       TT_GW8.GW8_PESOR                                       FORMAT ">>>,>>>,>>9.99999"    "#"  /* AT */
                       " "                                                                                  "#"  /* AU */
                       v-situacao-conhec                     FORMAT "x(50)"                "#"  /* AV */
                       v-nr-fatura                                                                          "#"  /* AW */
                       v-serie-fat                                                         "#"  /* AX */
                       v-dt-aprov                                                                           "#"  /* AY */
                       "  "                                                                                 "#"  /* AZ */
                       "  "                                                                                 "#"  /* BA */             
                       v-conta-contabil-gfe                                                                 "#"  /* BB */             
                       v-desc-conta-contabil-gfe             FORMAT "x(32)"                                 "#"  /* BC */             
                       v-tipo-veiculo                                                                       "#"  /* BD */             
                       v-produto-frete                         FORMAT "x(13)"                               "#"  /* BE */             
                       "GFE"                                                                                "#"  /* BF */             
                       v-rateio-conhec                                                                      "#"  /* BG */             
                       v-valor-icms                                           FORMAT "->>>,>>>,>>9.99"      "#"  /* BH */             
                       v-valor-iss                                            FORMAT "->>>,>>>,>>9.99"      "#"  /* BI */             
                       v-valor-pis                                            FORMAT "->>>,>>>,>>9.99"      "#"  /* BJ */             
                       v-valor-cofins                                         FORMAT "->>>,>>>,>>9.99"      "#"  /* BK */             
                       v-conta-despesa                                                                      "#"  /* BL */
                       SKIP.

           END. /* FOR EACH itens-cliente-tr */
        END. /* AVAIL docum-est */
    END. /* not avail nota-fiscal */
    
END PROCEDURE.


PROCEDURE p-preselect:
    DEFINE INPUT  PARAMETER ipch-tipo AS CHARACTER   NO-UNDO.

    
    FIND FIRST tt-param NO-ERROR.
    
    CASE ipch-tipo:
        
        WHEN "GV5" THEN DO:
            EMPTY TEMP-TABLE TT_GV5.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GV5:HANDLE
                c-tabela       = "GV5010".
        END.
        
        WHEN "GW1" THEN DO:
            EMPTY TEMP-TABLE TT_GW1.
            ASSIGN  
                h_table-handle = TEMP-TABLE TT_GW1:HANDLE
                c-tabela       = "GW1010"
                c-where        = "WHERE GW1_FILIAL >= " + "'" + c-cod-estabel-ini  + "'" + 
                                  " AND GW1_FILIAL <= " + "'" + c-cod-estabel-fim  + "'" +
                                  " AND GW1_CDTPDC  = " + "'" + 'NFD'              + "'" + 
                                  " AND GW1_DTEMIS >= " + "'" + c-emis-nf-carga[1] + "'" +
                                  " AND GW1_DTEMIS <= " + "'" + c-emis-nf-carga[2] + "'".
        END.

        WHEN "GW1-2" THEN DO:
            EMPTY TEMP-TABLE TT_GW1.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GW1:HANDLE
                c-tabela       = "GW1010"
                c-where        = "WHERE GW1_FILIAL = " + "'" + TT_GW4.GW4_FILIAL  + "'" +
                                  " AND GW1_CDTPDC = " + "'" + TT_GW4.GW4_TPDC    + "'" +
                                  " AND GW1_EMISDC = " + "'" + TT_GW4.GW4_EMISDC  + "'" +
                                  " AND GW1_SERDC  = " + "'" + TT_GW4.GW4_SERDC   + "'" +
                                  " AND GW1_NRDC   = " + "'" + TT_GW4.GW4_NRDC    + "'".



        END.
        
        WHEN "GW3" THEN DO:
            EMPTY TEMP-TABLE TT_GW3.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GW3:HANDLE
                c-tabela       = "GW3010"
                c-where        = "WHERE GW3_FILIAL >= " + "'" + c-cod-estabel-ini   + "'" +
                                  " AND GW3_FILIAL <= " + "'" + c-cod-estabel-fim   + "'" +
                                  " AND GW3_DTFIS  >= " + "'" + c-dt-entrada-doc[1] + "'" +
                                  " AND GW3_DTFIS  <= " + "'" + c-dt-entrada-doc[2] + "'" +
                                  " AND GW3_DTEMIS >= " + "'" + c-dt-emis-doc   [1] + "'" +
                                  " AND GW3_DTEMIS <= " + "'" + c-dt-emis-doc   [2] + "'".
        END.

   /*      WHEN "GW3" THEN DO:                                                                   */
   /*          EMPTY TEMP-TABLE TT_GW3.                                                          */
   /*          ASSIGN                                                                            */
   /*              h_table-handle = TEMP-TABLE TT_GW3:HANDLE                                     */
   /*              c-tabela       = "GW3010"                                                     */
   /*              c-where        = "WHERE GW3_FILIAL >= " + "'" + '01'       + "'" +            */
   /*                                " AND GW3_FILIAL <= " + "'" + '01'       + "'" +            */
   /*                                " AND GW3_NRDF    = " + "'" +  '0000649' + "'"   .          */
   /*      END.                                                                                  */

        WHEN "GW4-1" THEN DO:
            EMPTY TEMP-TABLE TT_GW4.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GW4:HANDLE
                c-tabela       = "GW4010"
                c-where        = "WHERE GW4_FILIAL = " + "'" + GW3_FILIAL + "'" +
                                  " AND GW4_EMISDF = " + "'" + GW3_EMISDF + "'" +
                                  " AND GW4_CDESP  = " + "'" + GW3_CDESP  + "'" +
                                  " AND GW4_SERDF  = " + "'" + GW3_SERDF  + "'" +
                                  " AND GW4_NRDF   = " + "'" + GW3_NRDF   + "'" +
                                  " AND GW4_DTEMIS = " + "'" + GW3_DTEMIS + "'".
        END.
        
        WHEN "GW4-2" THEN DO:
            EMPTY TEMP-TABLE TT_GW4.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GW4:HANDLE
                c-tabela       = "GW4010"
                c-where        = "WHERE GW4_FILIAL = " + "'" + GW1_FILIAL + "'" +
                                  " AND GW4_EMISDC = " + "'" + GW1_EMISDC + "'" +
                                  " AND GW4_TPDC   = " + "'" + GW1_CDTPDC + "'" +
                                  " AND GW4_SERDC  = " + "'" + GW1_SERDC  + "'" +
                                  " AND GW4_NRDC   = " + "'" + GW1_NRDC   + "'".
        END.
        
        WHEN "GW6" THEN DO:
            EMPTY TEMP-TABLE TT_GW6.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GW6:HANDLE
                c-tabela       = "GW6010"
                c-where        = "WHERE GW6_FILIAL = " + "'" +        GW1_FILIAL + "'" + 
                                  " AND GW6_EMIFAT = " + "'" +        GW1_EMISDC + "'" +
                                  " AND GW6_SERFAT = " + "'" +        GW1_SERDC  + "'" + 
                                  " AND GW6_NRFAT  = " + "'" + STRING(GW1_NRDC)  + "'" +
                                  " AND GW6_DTEMIS = " + "'" +        GW1_DTEMIS + "'".
        END.

        WHEN "GWM-GERAL" THEN DO: /* rateio-nf-contabil */
            EMPTY TEMP-TABLE TT_GWM.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWM:HANDLE
                c-tabela       = "GWM010"
                c-where        =  "WHERE GWM_FILIAL = " + "'" + TT_GW4.GW4_FILIAL + "'" +
                                  " AND  GWM_EMISDC = " + "'" + TT_GW4.GW4_EMISDC + "'" +
                                  " AND  GWM_SERDC  = " + "'" + TT_GW4.GW4_SERDC  + "'" +
                                  " AND  GWM_NRDC   = " + "'" + TT_GW4.GW4_NRDC   + "'" .
                                 /* " AND  GWM_NRDOC  = " + "'" + TT_GW4.GW4_NRDF   + "'" +
                                  " AND  GWM_SERDOC = " + "'" + TT_GW4.GW4_SERDF  + "'" +
                                  " AND  GWM_CDTRP  = " + "'" + TT_GW4.GW4_EMISDF + "'" +
                                  " AND  GWM_GRP3   = " + "'" + c-GWM_GRP3        + "'"*/ .
                                  /*
                                  GWM_TPDOC  = " + "'" + c-GWM_TPDOC       + "'" +
                                  */

    

        END.
        /*
        WHEN "GWM" THEN DO: /* rateio-nf-contabil */
            EMPTY TEMP-TABLE TT_GWM.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWM:HANDLE
                c-tabela       = "GWM010"
                c-where        =  "WHERE GWM_FILIAL = " + "'" + TT_GW4.GW4_FILIAL + "'" +
                                  " AND  GWM_EMISDC = " + "'" + TT_GW4.GW4_EMISDC + "'" +
                                  " AND  GWM_SERDC  = " + "'" + TT_GW4.GW4_SERDC  + "'" +
                                  " AND  GWM_NRDC   = " + "'" + TT_GW4.GW4_NRDC   + "'" +   
                                  " AND  GWM_NRDOC  = " + "'" + TT_GW4.GW4_NRDF   + "'" +
                                  " AND  GWM_SERDOC = " + "'" + TT_GW4.GW4_SERDF  + "'" +
                                  " AND  GWM_CDTRP  = " + "'" + TT_GW4.GW4_EMISDF + "'" +
                                  " AND  GWM_GRP3   = " + "'" + c-GWM_GRP3        + "'".
                                  /*
                                  GWM_TPDOC  = " + "'" + c-GWM_TPDOC       + "'" +
                                  */

    

        END.
        
        /* Calculo - v-rateio-calc-gfe */
        WHEN "GWM-gfe" THEN DO: 
            EMPTY TEMP-TABLE TT_GWM-gfe.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWM-gfe:HANDLE
                c-tabela       = "GWM010"
                c-where        =  "WHERE GWM_FILIAL = " + "'" + c-cod-estabel        + "'" +
                                  " AND  GWM_EMISDC = " + "'" + c_GW1_EMISDC         + "'" +
                                  " AND  GWM_CDTPDC = " + "'" + c_GW1_CDTPDC         + "'" +
                                  " AND  GWM_SERDC  = " + "'" + c-serie-docto        + "'" +
                                  " AND  GWM_NRDC   = " + "'" + c-nr-nota-fis        + "'" +
                                  " AND  GWM_TPDOC  = '1'"                                 +
                                  " AND  GWM_SEQGW8 = " + "'" + STRING(i-nr-seq-fat) + "'" +
                                  " AND  GWM_ITEM   = " + "'" + c-it-codigo          + "'".
            
/*             PUT STREAM S-TESTE UNFORMATTED */
/*                 ipch-tipo SKIP             */
/*                 c-where   SKIP(2).         */


        END.

        /* Calculo - v-rateio-conhec */
        WHEN "GWM-conhec" THEN DO:
            EMPTY TEMP-TABLE TT_GWM-con.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWM-con:HANDLE
                c-tabela       = "GWM010"
                c-where        =  "WHERE     GWM_FILIAL  = " + "'" + GW3_FILIAL              + "'" +
                                       " AND GWM_EMISDC  = " + "'" + c_GW1_EMISDC            + "'" +
                                       " AND GWM_CDTPDC  = " + "'" + c_GW1_CDTPDC            + "'" +
                                       " AND GWM_SERDC   = " + "'" + c-serie-docto           + "'" +
                                       " AND GWM_NRDC    = " + "'" + c-nr-nota-fis           + "'" +
                                       " AND GWM_CDESP   = " + "'" + GW3_CDESP               + "'" +
                                       " AND GWM_CDTRP   = " + "'" + GW3_EMISDF              + "'" +
                                       " AND GWM_SERDOC  = " + "'" + GW3_SERDF               + "'" +
                                       " AND GWM_NRDOC   = " + "'" + GW3_NRDF                + "'" +
                                       " AND GWM_DTEMIS  = " + "'" + GW3_DTEMIS              + "'" +  
                                       " AND (GWM_TPDOC   = '2' OR GWM_TPDOC = '5') "        + /*+ "'" + */
                
                                       " AND GWM_SEQGW8  = " + "'" + STRING(i-nr-seq-fat)    + "'" +
                                       " AND GWM_ITEM    = " + "'" + it-nota-fisc.it-codigo  + "'".

/*             PUT STREAM S-TESTE UNFORMATTED */
/*                 ipch-tipo SKIP             */
/*                 c-where   SKIP(2).         */

        END.
*/        
        WHEN "GWM-CALC" THEN DO:
            EMPTY TEMP-TABLE TT_GWM_CALC.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWM_CALC:HANDLE
                c-tabela       = "GWM010"
                c-where        = "WHERE GWM_FILIAL = " + "'" + TT_GWF.GWF_FILIAL + "'" +
                                  " AND GWM_TPDOC  = " + "'" +      "1"          + "'" +
                                  " AND GWM_NRDOC  = " + "'" + TT_GWF.GWF_NRCALC + "'" +
                                  " AND GWM_SERDC  = " + "'" + TT_GW1.GW1_SERDC  + "'" +
                                  " AND GWM_NRDC   = " + "'" + TT_GW1.GW1_NRDC   .
                         /*         " AND GWM_EMISDC = " + "'" + TT_GW4.GW4_EMISDC + "'" +
                                  " AND GWM_CDTRP  = " + "'" + TT_GW4.GW4_EMISDF + "'" +
                                  " AND GWM_GRP3   = " + "'" + c-GWM_GRP3        + "'"*/ .
        END.
        
        WHEN "GVT" THEN DO:
            EMPTY TEMP-TABLE TT_GVT.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GVT:HANDLE
                c-tabela       = "GVT010".
        END.
        
        WHEN "GWA" THEN DO:
            EMPTY TEMP-TABLE TT_GWA.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWA:HANDLE
                c-tabela       = "GWA010"
                c-where        = "WHERE GWA_FILIAL = " + "'" + TT_GWM.GWM_FILIAL + "'" +
                                  " AND GWA_TPDOC  = " + "'" + c-tp-docto + "'" +
                                  " AND GWA_NRDOC  = " + "'" + TT_GWM.GWM_NRDOC  + "'" +
                                  " AND GWA_CDEMIT = " + "'" + TT_GWM.GWM_CDTRP  + "'" + 
                                  " AND GWA_SERIE  = " + "'" + TT_GWM.GWM_SERDOC + "'" +
                                  " AND GWA_DTEMIS = " + "'" + GW3_DTEMIS + "'".
        END.
        
        WHEN "GUE" THEN DO:
            EMPTY TEMP-TABLE TT_GUE.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GUE:HANDLE
                c-tabela       = "GUE010".
        END.
        
        WHEN "GU3" THEN DO:
            EMPTY TEMP-TABLE TT_GU3.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GU3:HANDLE
                c-tabela       = "GU3010"
                c-where        = "WHERE GU3_CDEMIT = " + "'" + TT_GW3.GW3_EMISDF + "'".
        END.
        
        WHEN "GVA" THEN DO:
            EMPTY TEMP-TABLE TT_GVA.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GVA:HANDLE
                c-tabela       = "GVA010"
                c-where        = "WHERE GVA_CDEMIT = " + "'" + GW3_EMISDF + "'".
        END.

        WHEN "GWG" THEN DO:
            EMPTY TEMP-TABLE TT_GWG.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWG:HANDLE
                c-tabela       = "GWG010"
                c-where        = "WHERE GWG_FILIAL = " + "'" + TT_GWF.GWF_FILIAL + "'" +
                                 "  AND GWG_NRCALC = " + "'" + TT_GWF.GWF_NRCALC + "'".
        END.
        
        WHEN "GWF" THEN DO:
            EMPTY TEMP-TABLE TT_GWF.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWF:HANDLE
                c-tabela       = "GWF010".
                c-where        = "WHERE GWF_FILIAL = " + "'" + TT_GWH.GWH_FILIAL + "'" +
                                 "  AND GWF_NRCALC = " + "'" + TT_GWH.GWH_NRCALC + "'" + 
                                 "  AND GWF_TPCALC = " + "'" + GW3_TPDF          + "'".
                                 
        END.

        WHEN "GWH" THEN DO:
            EMPTY TEMP-TABLE TT_GWH.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWH:HANDLE
                c-tabela       = "GWH010"
                c-where        = "WHERE GWH_FILIAL = " + "'" + TT_GW1.GW1_FILIAL + "'" +
                                 "  and GWH_CDTPDC = " + "'" + TT_GW1.GW1_CDTPDC + "'" +   
                                 "  AND GWH_EMISDC = " + "'" + TT_GW1.GW1_EMISDC + "'" +
                                 "  AND GWH_SERDC  = " + "'" + TT_GW1.GW1_SERDC  + "'" +
                                 "  AND GWH_NRDC   = " + "'" + TT_GW1.GW1_NRDC   + "'" .
        END.

        WHEN "GW8" THEN DO:
            EMPTY TEMP-TABLE TT_GW8.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GW8:HANDLE
                c-tabela       = "GW8010"
                c-where        = "WHERE GW8_SERDC = " + "'" + item-doc-est.serie-docto + "'" +
                                 "  AND GW8_NRDC  = " + "'" + item-doc-est.nro-docto   + "'" +
                                 "  AND GW8_ITEM  = " + "'" + item-doc-est.it-codigo   + "'".
        END.

        WHEN "GWH-2" THEN DO:
            EMPTY TEMP-TABLE bf-TT_GWH.
            ASSIGN
                h_table-handle = TEMP-TABLE bf-TT_GWH:HANDLE
                c-tabela       = "GWH010"
                c-where        = "WHERE GWH_FILIAL = " + "'" + c-estab-pre-con      + "'" +
                                 "  AND GWH_NRCALC = " + "'" + STRING(i-nr-pre-con) + "'".
        END.
        
        WHEN "GW8-2" THEN DO:
            EMPTY TEMP-TABLE bf-TT_GW8.
            ASSIGN
                h_table-handle = TEMP-TABLE bf-TT_GW8:HANDLE
                c-tabela       = "GW8010"
                c-where        = "WHERE GW8_NRDC  = " + "'" + bf-TT_GWH.GWH_NRDC  + "'" +
                                 "  AND GW8_SERDC = " + "'" + bf-TT_GWH.GWH_SERDC + "'".
        END.

        WHEN "GWN" THEN DO:
            EMPTY TEMP-TABLE TT_GWN.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GWN:HANDLE
                c-tabela       = "GWN010"
                c-where        = "WHERE GWN_FILIAL = " + "'" + nota-fiscal.cod-estabel         + "'" +
                                 "  AND GWN_NRROM  = " + "'" + STRING(nota-fiscal.nr-embarque) + "'".



        END.
        
        WHEN "GV3" THEN DO:
            EMPTY TEMP-TABLE TT_GV3.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GV3:HANDLE
                c-tabela       = "GV3010"
                c-where        = "WHERE GV3_CDTPVC  = " + "'" + TT_GWN.GWN_CDTPVC + "'".

        END.

        WHEN "GUB" THEN DO:
            EMPTY TEMP-TABLE TT_GUB.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GUB:HANDLE
                c-tabela       = "GUB010".
        END.

        WHEN "GW4" THEN DO:
            EMPTY TEMP-TABLE TT_GW4.
            ASSIGN
                h_table-handle = TEMP-TABLE TT_GW4:HANDLE
                c-tabela       = "GW4010"
                c-where        = "WHERE GW4_EMISDF = " + "'" + GW3_EMISDF + "'" +
                                 "  AND GW4_SERDF  = " + "'" + GW3_SERDF  + "'" +
                                 "  AND GW4_NRDF   = " + "'" + GW3_NRDF   + "'" +
                                 "  AND GW4_DTEMIS = " + "'" + GW3_DTEMIS + "'".
        END.

        WHEN "GW8-3" THEN DO:
            EMPTY TEMP-TABLE B-TT_GW8.
            ASSIGN
                h_table-handle = TEMP-TABLE B-TT_GW8:HANDLE
                c-tabela       = "GW8010"
                c-where        = "WHERE GW8_EMISDC = " + "'" + GW4_EMISDC + "'" +
                                 "  AND GW8_SERDC  = " + "'" + GW4_SERDC  + "'" +
                                 "  AND GW8_NRDC   = " + "'" + GW4_NRDC   + "'"
                                 .
        END. 
        
    END CASE.

    IF c-where <> "" THEN c-where = c-where + " AND D_E_L_E_T_ <> '*'".

    RUN ExecuteSelect IN hBoSQLCa (INPUT h_table-handle, INPUT c-where, INPUT c-tabela). 

END PROCEDURE.    

PROCEDURE pi-conecta:
    
    IF NOT VALID-HANDLE(hBOSQLCa) THEN DO:
        RUN dibo/boSQLca.p PERSISTENT SET hBoSQLca.

        RUN ConectaAplicacaoExterna IN hBoSQLCa (INPUT 'GFE', OUTPUT TABLE rowErrors).
    END.
    
END PROCEDURE.
                  
PROCEDURE pi-desconectar:   
    
    /* Desconecta SQL */
    RUN desconectaAplicacaoExterna IN hBoSQLca.
        
    RUN getRowErrors IN hBoSQLCa (OUTPUT TABLE rowErrors).
    
    DELETE OBJECT hBoSQLca. 

END PROCEDURE.

/*

PROCEDURE p-rateio-nf-contabil:
    DEFINE INPUT  PARAMETER ipch-tipo-pesq AS CHARACTER   NO-UNDO.
    
    /* docto-frete-nf */
    /*RUN p-preselect("GWM").*/
    IF ipch-tipo-pesq = "E" THEN /* GV5_SENTID = "2" AND AVAIL nota-fiscal */
        FOR EACH TT_GWM
           WHERE TT_GWM.GWM_FILIAL = TT_GW4.GW4_FILIAL  
             AND TT_GWM.GWM_EMISDC = TT_GW4.GW4_EMISDC  
             AND TT_GWM.GWM_SERDC  = TT_GW4.GW4_SERDC   
             AND TT_GWM.GWM_NRDC   = TT_GW4.GW4_NRDC    
             AND TT_GWM.GWM_NRDOC  = TT_GW4.GW4_NRDF    
             AND TT_GWM.GWM_SERDOC = TT_GW4.GW4_SERDF   
             AND TT_GWM.GWM_CDTRP  = TT_GW4.GW4_EMISDF  
             AND TT_GWM.GWM_GRP3   = c-GWM_GRP3.        

            CASE TT_GWM.GWM_TPDOC:
                WHEN "2" OR WHEN "5" THEN RUN pi-processa.
            END CASE.
        END.
    ELSE
        FOR FIRST TT_GWM
            WHERE TT_GWM.GWM_FILIAL = TT_GW4.GW4_FILIAL  
              AND TT_GWM.GWM_EMISDC = TT_GW4.GW4_EMISDC  
              AND TT_GWM.GWM_SERDC  = TT_GW4.GW4_SERDC   
              AND TT_GWM.GWM_NRDC   = TT_GW4.GW4_NRDC    
              AND TT_GWM.GWM_NRDOC  = TT_GW4.GW4_NRDF    
              AND TT_GWM.GWM_SERDOC = TT_GW4.GW4_SERDF   
              AND TT_GWM.GWM_CDTRP  = TT_GW4.GW4_EMISDF  
              AND TT_GWM.GWM_GRP3   = c-GWM_GRP3.      

            CASE TT_GWM.GWM_TPDOC:
                WHEN "2" OR WHEN "5" THEN RUN pi-processa.
            END CASE.
        END.
    
END PROCEDURE.*/


/*
PROCEDURE pi-processa.

    ASSIGN
        v-conta-contabil-gfe = TT_GWM.GWM_CTFRET                            /* rateio-nf-contabil.conta-despesa */
        v-data-contabil      = fnConvCharData(GWM_DTEMIS)                   /* rateio-nf-contabil.data-1        */
      /*  v-valor-despesa      = v-valor-despesa + GWM_VLFRET   */               /* rateio-nf-contabil.val-despesa   */
        v-valor-icms         = GWM_VLICMS                                   /* rateio-nf-contabil.val-icms      */
        v-conta-despesa      = GWM_CTFRET                                   /* rateio-nf-contabil.conta-despesa */
    NO-ERROR.
    
    IF AVAILABLE TT_GW3 AND TT_GW3.GW3_VLPIS > 0 THEN                       /* DEC(SUBSTRING(docto-frete.char-2,25,14)) */
        ASSIGN v-valor-pis     = TT_GWM.GWM_VLPIS          /* rateio-nf-contabil.val-pis */

/*     IF AVAILABLE TT_GW3 AND TT_GW3.GW3_VLISS > 0 THEN */
        v-valor-iss     = GWM_VLISS.

    IF AVAILABLE TT_GW3 AND TT_GW3.GW3_VLCOF > 0 THEN                       /* DEC(SUBSTRING(docto-frete.char-2,53,14)) */
        ASSIGN v-valor-cofins  = TT_GWM.GWM_VLCOFI.         /* rateio-nf-contabil.val-cofins. */
    
   /* RUN p-preselect("GWA"). 
    
    FIND FIRST TT_GWA NO-ERROR.
    IF AVAILABLE TT_GWA THEN
        ASSIGN v-data-contabil = fnConvCharData(TT_GWA.GWA_DTMOV). /* movct-tr.dt-movimento. */ */

END PROCEDURE.*/


PROCEDURE p-limpa-var:

    ASSIGN
        v-conta-contabil-gfe      = " "
        v-desc-conta-contabil-gfe = " "
        v-data-contabil           = ?
        v-desc-item               = " "
        v-desc-estoq              = " "
        v-rateio-calc-gfe         = 0
        v-rateio-conhec           = 0
        v-valor-icms              = 0
        v-valor-despesa           = 0
        v-valor-pis               = 0
        v-valor-iss               = 0
        v-valor-cofins            = 0
        v-conta-despesa           = " "
        .
END PROCEDURE.


PROCEDURE p-cabec:
    PUT "Filial / Origem#"                 /*  A */ 
        "docto frete#"                     /*  B */ 
        "serie#"                           /*  C */ 
        "Data da transacao#"               /*  D */ 
        "criacao docto frete#"             /*  E */ 
        "natureza docto frete#"            /*  F */ 
        "Descricao#"                       /*  G */ 
        "especie#"                         /*  H */ 
        "tipo frete#"                      /*  I */ 
        "cnpj docto frete#"                /*  J */ 
        "emitente transp#"                 /*  K */ 
        "nome-transportador#"              /*  L */ 
        "grupo-forn#"                      /*  M */ 
        "grupo-forn-desc#"                 /*  N */ 
        "Modalidade#"                      /*  O */ 
        "Tab Frete#"                       /*  P */ 
        "tab frete Calc#"                  /*  Q */ 
        "cgc tab frete Calc#"              /*  R */ 
        "rateio-valor-conhec#"             /*  S */ 
        "rateio-valor-calc-gfe#"           /*  T */ 
        "Diferenca Tabela x CTE#"          /*  U */ 
        "aliq-icms#"                       /*  V */ 
        "aliq-iss#"                        /*  W */ 
        "usuario-criacao docto frete#"     /*  X */ 
        "NF Origem Frete#"                 /*  Y */ 
        "serie NF#"                        /*  Z */ 
        "especie NF#"                      /* AA */ 
        "emis-nota#"                       /* AB */ 
        "Destinatario#"                    /* AC */ 
        "Nome Cliente/Fornecedor#"         /* AD */ 
        "placa#"                           /* AE */ 
        "nr-embarque-faturamento#"         /* AF */ 
        "cidade origem#"                   /* AG */ 
        "UF origem#"                       /* AH */ 
        "cidade destino#"                  /* AI */ 
        "UF destino#"                      /* AJ */ 
        "nr-pedido#"                       /* AK */ 
        "situacao Pedido#"                 /* AL */ 
        "item#"                            /* AM */ 
        "descricao item#"                  /* AN */ 
        "grupo estoq#"                     /* AO */  
        "descricao grupo #"                /* AP */ 
        "Unid Medida#"                     /* AQ */ 
        "quantidade do item#"              /* AR */ 
        "vl-tot-item#"                     /* AS */ 
        "peso-bruto item#"                 /* AT */ 
        "peso-liquido item#"               /* AU */ 
        "situacao docto frete#"            /* AV */ 
        "nr-fatura Frete#"                 /* AW */ 
        "serie Fatura#"                    /* AX */ 
        "aprov Fatura Frete#"              /* AY */         
        "Conta Contabil RECEBIMENTO#"      /* AZ */ 
        "Desc Conta Contabil RECEBIMENTO#" /* BA */    
        "Conta Contabil gfe#"              /* BB */  
        "Desc Conta Contabil gfe#"         /* BC */  
        "Tipo Veiculo#"                    /* BD */  
        "Produto Frete#"                   /* BE */  
        "ORIGEM INFORMACAO#"               /* BF */  
        "Valor Movimento Bruto#"           /* BG */  
        "Valor ICMS#"                      /* BH */  
        "Valor ISS#"                       /* BI */  
        "Valor PIS#"                       /* BJ */  
        "Valor COFINS#"                    /* BK */  
        "Conta Despesa#"                   /* BL */
        SKIP.

END PROCEDURE.


/* ************************  Function Implementations ***************** */
FUNCTION fnConvDataChar      RETURNS CHARACTER (INPUT p-data        AS DATE).
    DEFINE VARIABLE c-retval AS CHARACTER   NO-UNDO.

    IF p-data = ? THEN RETURN "".

    c-retval =  STRING(YEAR (p-data), "9999") + 
                STRING(MONTH(p-data),   "99") + 
                STRING(DAY  (p-data),   "99").
    
    RETURN c-retval.

END FUNCTION.

FUNCTION fnConvCharData      RETURNS DATE      (INPUT p-character   AS CHARACTER).
    DEFINE VARIABLE d-retval AS DATE        NO-UNDO.

    IF p-character = "" OR p-character = ? THEN RETURN ?.
    
    d-retval =  DATE ( INTEGER(SUBSTRING(p-character,5,2)), 
                       INTEGER(SUBSTRING(p-character,7,2)), 
                       INTEGER(SUBSTRING(p-character,1,4))
                       ).
    RETURN d-retval.
    
END FUNCTION.

FUNCTION f-modalidade    RETURNS LOGICAL (INPUT ipch-cgc AS CHARACTER).

    v-via-transp = "".

    FIND FIRST transporte NO-LOCK WHERE transporte.cgc = ipch-cgc NO-ERROR.
    IF AVAIL transporte THEN DO:
        CASE transporte.natureza > 0:
             WHEN transporte.via-transp = 1 THEN v-via-transp = "RODOVIARIO".
             WHEN transporte.via-transp = 2 THEN v-via-transp = "AEREO".
             WHEN transporte.via-transp = 3 THEN v-via-transp = "MARITIMO".
             WHEN transporte.via-transp = 4 THEN v-via-transp = "FERROVIARIO".
             WHEN transporte.via-transp = 5 THEN v-via-transp = "RODOFERROVIARIO".
             WHEN transporte.via-transp = 6 THEN v-via-transp = "RODOFLUVIAL".
             WHEN transporte.via-transp = 7 THEN v-via-transp = "RODOAEROVIARIO".
             WHEN transporte.via-transp = 8 THEN v-via-transp = "OUTROS".
             OTHERWISE                           v-via-transp = "NAO ENCONTRADO".
        END CASE.
    END.
    ELSE v-via-transp = "TRANSPORTADOR ENCONTRADO".
    
    RETURN TRUE.
    
END FUNCTION.

/*
FUNCTION fnRetiraAcentos RETURNS char (INPUT p-string AS char ).
    DEFINE VARIABLE c-free-accent AS CHARACTER CASE-SENSITIVE NO-UNDO.

    ASSIGN 
        c-free-accent = p-string
        c-free-accent =  REPLACE(c-free-accent, '˙', 'A')
        c-free-accent =  REPLACE(c-free-accent, 'Ê', 'A')
        c-free-accent =  REPLACE(c-free-accent, 'Ù', 'A')
        c-free-accent =  REPLACE(c-free-accent, 'Ä', 'A')
        c-free-accent =  REPLACE(c-free-accent, 'ƒ', 'A')
        c-free-accent =  REPLACE(c-free-accent, '‚', 'E')
        c-free-accent =  REPLACE(c-free-accent, '»', 'E')
        c-free-accent =  REPLACE(c-free-accent, '„', 'E')
        c-free-accent =  REPLACE(c-free-accent, '‡', 'E')
        c-free-accent =  REPLACE(c-free-accent, 'Ë', 'I')
        c-free-accent =  REPLACE(c-free-accent, 'ô', 'I')
        c-free-accent =  REPLACE(c-free-accent, 'û', 'I')
        c-free-accent =  REPLACE(c-free-accent, 'ù', 'I')
        c-free-accent =  REPLACE(c-free-accent, '∆', 'O')
        c-free-accent =  REPLACE(c-free-accent, 'Ö', 'O')
        c-free-accent =  REPLACE(c-free-accent, 'É', 'O')
        c-free-accent =  REPLACE(c-free-accent, 'Ü', 'O')
        c-free-accent =  REPLACE(c-free-accent, '⁄', 'O')
        c-free-accent =  REPLACE(c-free-accent, 'â', 'U')
        c-free-accent =  REPLACE(c-free-accent, 'Ç', 'U')
        c-free-accent =  REPLACE(c-free-accent, 'à', 'U')
        c-free-accent =  REPLACE(c-free-accent, '€', 'U')
        c-free-accent =  REPLACE(c-free-accent, '°', 'Y')
        c-free-accent =  REPLACE(c-free-accent, 'ü', 'Y')
        c-free-accent =  REPLACE(c-free-accent, '∞', 'C')
        c-free-accent =  REPLACE(c-free-accent, 'æ', 'N')
        c-free-accent =  REPLACE(c-free-accent, 'π', 'a')
        c-free-accent =  REPLACE(c-free-accent, 'ˇ', 'a')
        c-free-accent =  REPLACE(c-free-accent, '≥', 'a')
        c-free-accent =  REPLACE(c-free-accent, 'í', 'a')
        c-free-accent =  REPLACE(c-free-accent, '¥', 'a')
        c-free-accent =  REPLACE(c-free-accent, '¿', 'e')
        c-free-accent =  REPLACE(c-free-accent, '≤', 'e')
        c-free-accent =  REPLACE(c-free-accent, 'º', 'e')
        c-free-accent =  REPLACE(c-free-accent, 'ø', 'e')
        c-free-accent =  REPLACE(c-free-accent, '√', 'i')
        c-free-accent =  REPLACE(c-free-accent, '≠', 'i')
        c-free-accent =  REPLACE(c-free-accent, '¬', 'i')
        c-free-accent =  REPLACE(c-free-accent, '¡', 'i')
        c-free-accent =  REPLACE(c-free-accent, 'Õ', 'o')
        c-free-accent =  REPLACE(c-free-accent, 'Ω', 'o')
        c-free-accent =  REPLACE(c-free-accent, 'À', 'o')
        c-free-accent =  REPLACE(c-free-accent, 'Ñ', 'o')
        c-free-accent =  REPLACE(c-free-accent, 'Ã', 'o')
        c-free-accent =  REPLACE(c-free-accent, '’', 'u')
        c-free-accent =  REPLACE(c-free-accent, 'ú', 'u')
        c-free-accent =  REPLACE(c-free-accent, 'Œ', 'u')
        c-free-accent =  REPLACE(c-free-accent, '±', 'u')
        c-free-accent =  REPLACE(c-free-accent, 'ç', 'y')
        c-free-accent =  REPLACE(c-free-accent, 'Ÿ', 'y')
        c-free-accent =  REPLACE(c-free-accent, 'ª', 'c')
        c-free-accent =  REPLACE(c-free-accent, 'œ', 'n')
        c-free-accent =  REPLACE(c-free-accent, '›', 'a')
        c-free-accent =  REPLACE(c-free-accent, 'ı', 'o')
        c-free-accent =  REPLACE(c-free-accent, '&', 'E')
        c-free-accent =  REPLACE(c-free-accent, CHR(10),'')
        c-free-accent =  REPLACE(c-free-accent, CHR(13),'')
        c-free-accent =  REPLACE(c-free-accent, CHR(9), ''). /* retira enter */

    RETURN c-free-accent.
    
END FUNCTION.
*/
/* fim do programa */
