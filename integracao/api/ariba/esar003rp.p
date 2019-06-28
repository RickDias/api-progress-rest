&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ESAR003RP.p
    Purpose     : Carga Inicial para o ARIBA

    Syntax      :

    Description :

    Author(s)   : TOTVS
    Created     : 04/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table tt-param no-undo
    field destino              as integer
    field arquivo              as char format "x(35)"
    field usuario              as char format "x(12)"
    field data-exec            as date
    field hora-exec            as integer
    FIELD diretorio            AS CHAR
    FIELD condicao-pagamento   AS LOG  
    FIELD centro-custo         AS LOG  
    FIELD conta-contabil       AS LOG  
    FIELD tipos-conta          AS LOG  
    FIELD combinacao-contabil  AS LOG  
    FIELD estabelec-endereco   AS LOG  
    FIELD unidade-negocio      AS LOG  
    FIELD metodo-pagamento     AS LOG  
    FIELD unidade-medida       AS LOG  
    FIELD fornecedor           AS LOG  
    FIELD usuarios             AS LOG
    FIELD ITEM                 AS LOG
    FIELD modelo-rtf           as char format "x(35)"
    FIELD l-habilitaRtf        as LOG.

DEFINE TEMP-TABLE tt-emitente NO-UNDO
    FIELD cod-emitente AS INT
    FIELD total-pedido AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" decimals 8
    FIELD cont-pedido  AS INT.


DEFINE TEMP-TABLE ttErrosRet
    FIELD cod-erro  AS INTEGER 
    FIELD desc-erro AS CHARACTER FORMAT "X (256)"
    FIELD desc-arq  AS CHARACTER.


define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* DefiniÓ“o de VariŸveis Globais ******************************************/ 
DEF NEW GLOBAL SHARED VAR c-seg-usuario                 AS CHAR NO-UNDO.

def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.


CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padr∆o para vari†veis para o log  */
/* {include/i-rpvar.i} */

DEF VAR h-prog              AS HANDLE                                  NO-UNDO.
    
DEF VAR c-dt-cotacao         AS CHAR  FORMAT "x(30)"    NO-UNDO.
DEF VAR i-num-mes            AS INT                     NO-UNDO.
DEF VAR i-cont               AS INT                     NO-UNDO.
DEF VAR c-siglas             AS CHAR                    NO-UNDO.
DEF VAR de-cotacao           AS DECIMAL                 NO-UNDO.

DEFINE VARIABLE lista-mes AS CHARACTER FORMAT "x(20)"
    INITIAL "Janeiro,Fevereiro,Maráo,Abril,Maio,Junho,Julho,Agosto,Setembro,Outubro,Novembro,Dezembro".


DEF STREAM str-rp.

RUN utp/ut-acomp.p PERSISTENT SET h-prog.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.42
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN utp/ut-acomp.p PERSISTENT SET h-prog.

find first tt-param no-lock no-error.

RUN pi-inicializar IN h-prog (INPUT "Inicializando Processo de Geraá∆o de Arquivos Cargas Iniciais!").

IF condicao-pagamento THEN
    RUN pi-condicao-pagamento.

IF centro-custo THEN
    RUN pi-centro-custo.

IF conta-contabil THEN
    RUN pi-conta-contabil.

IF tipos-conta THEN
    RUN pi-tipos-conta.

IF combinacao-contabil THEN
    RUN pi-combinacao-contabil.

IF estabelec-endereco THEN
    RUN pi-estabelec-endereco.

IF unidade-negocio THEN
    RUN pi-unidade-negocio.

IF metodo-pagamento THEN
    RUN pi-metodo-pagamento.

IF unidade-medida THEN
    RUN pi-unidade-medida.

IF fornecedor THEN
    RUN pi-fornecedor.

IF usuarios THEN
    RUN pi-usuarios.

IF tt-param.item THEN
    RUN pi-item.

RUN pi-finalizar IN h-prog.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-centro-custo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-centro-custo Procedure 
PROCEDURE pi-centro-custo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "CostCenterExport" + ".csv" )  NO-CONVERT.
DEF VAR c-desc-tit-ctbl AS CHAR FORMAT "x(40)" NO-UNDO.

/* CABEÄALHO */
PUT STREAM str-rp
    "BusinessUnit"    ","
    "UniqueName"      ","
    "Name"            ","
    "Description"     ","
    "PurchasingUnit"  ","
    "PurchasingUnits" 
    SKIP. 

FOR EACH ems5.ccusto NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Centro Custo: " + ccusto.cod_ccusto).

    ASSIGN c-desc-tit-ctbl = REPLACE(ccusto.des_tit_ctbl,",","-").

    PUT STREAM str-rp
        ""                                      ","
        ccusto.cod_ccusto                       ","
        c-desc-tit-ctbl                         ","
        c-desc-tit-ctbl                         ","   
        "All"                                   ","
        "All"                            
        SKIP.
    

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-combinacao-contabil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-combinacao-contabil Procedure 
PROCEDURE pi-combinacao-contabil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "AccountingCombinationExport" + ".csv" )  NO-CONVERT.
DEF VAR c-cod-combinacao AS CHAR FORMAT "x(50)" NO-UNDO.

/* CABEÄALHO */
PUT STREAM str-rp
    "Account"           ","
    "BusinessUnit"      ","
    "CombinationGroup"  ","
    "Company"           ","
    "CostCenter"        ","
    "Product"           ","
    "Region"            ","
    "SubAccount"        ","
    "UniqueName"        
    SKIP. 



FOR EACH criter_distrib_cta_ctbl NO-LOCK:

    FIND FIRST cta_ctbl WHERE
               cta_ctbl.cod_grp_cta_ctbl = "1" OR
               cta_ctbl.cod_grp_cta_ctbl = "3" OR 
               cta_ctbl.cod_grp_cta_ctbl = "5" NO-LOCK NO-ERROR.
    IF NOT AVAIL cta_ctbl THEN
        NEXT.

    IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "N∆o utiliza" THEN DO:

        RUN pi-acompanhar IN h-prog("Gerando Arquivo Combinaá∆o Cont†bil: " + STRING(criter_distrib_cta_ctbl.cod_cta_ctbl)).

        ASSIGN c-cod-combinacao = criter_distrib_cta_ctbl.cod_cta_ctbl + "-"  + criter_distrib_cta_ctbl.cod_estab.
    
        PUT STREAM str-rp
            criter_distrib_cta_ctbl.cod_cta_ctbl          ","   /*CONTA CONTABIL*/
            ""                                            ","   /*UNIDADE DE NEGOCIO*/
            ""                                            ","   /*BRANCO*/
            criter_distrib_cta_ctbl.cod_estab             ","   /*ESTABELECIMENTO*/
            ""                                            ","   /*CENTRO CUSTO*/
            ""                                            ","   /*BRANCO*/
            ""                                            ","   /*BRANCO*/
            ""                                            ","   /*BRANCO*/
            c-cod-combinacao                                    /*CODIGO COMBINAÄ«O*/
            SKIP.

    END.

    IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Utiliza Todos" THEN DO:
    
        FOR EACH ems5.ccusto WHERE 
                 ems5.ccusto.cod_empresa      = criter_distrib_cta_ctbl.cod_empresa     /*AND
                 ems5.ccusto.cod_plano_ccusto = criter_distrib_cta_ctbl.cod_plano_cta_ctbl*/ NO-LOCK:

            FIND FIRST restric_ccusto WHERE 
                       restric_ccusto.cod_estab  = criter_distrib_cta_ctbl.cod_estab AND
                       restric_ccusto.cod_ccusto = ccusto.cod_ccusto                 NO-LOCK NO-ERROR.
            IF AVAIL restric_ccusto THEN
                NEXT.


            FOR EACH  ccusto_unid_negoc WHERE 
                      ccusto_unid_negoc.cod_ccusto = ems5.ccusto.cod_ccusto NO-LOCK:

                RUN pi-acompanhar IN h-prog("Gerando Arquivo Combinaá∆o Cont†bil: " + STRING(criter_distrib_cta_ctbl.cod_cta_ctbl) + "-" + STRING(ccusto.cod_ccusto) + "-" + STRING(ccusto_unid_negoc.cod_unid_negoc)).

                ASSIGN c-cod-combinacao = criter_distrib_cta_ctbl.cod_cta_ctbl + "-" + ccusto_unid_negoc.cod_unid_negoc + "-" + criter_distrib_cta_ctbl.cod_estab + "-" + ccusto.cod_ccusto.
            
                PUT STREAM str-rp
                    criter_distrib_cta_ctbl.cod_cta_ctbl          ","   /*CONTA CONTABIL*/
                    ccusto_unid_negoc.cod_unid_negoc              ","   /*UNIDADE DE NEGOCIO*/
                    ""                                            ","   /*BRANCO*/
                    criter_distrib_cta_ctbl.cod_estab             ","   /*ESTABELECIMENTO*/
                    ccusto.cod_ccusto                             ","   /*CENTRO CUSTO*/
                    ""                                            ","   /*BRANCO*/
                    ""                                            ","   /*BRANCO*/
                    ""                                            ","   /*BRANCO*/
                    c-cod-combinacao                                    /*CODIGO COMBINAÄ«O*/
                    SKIP.

            END.

        END.

    END.

    IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Definidos" THEN DO:

        FOR EACH item_distrib_ccusto WHERE
                 item_distrib_ccusto.cod_mapa_distrib_ccusto = item_distrib_ccusto.cod_mapa_distrib_ccusto NO-LOCK:

            RUN pi-acompanhar IN h-prog("Gerando Arquivo Combinaá∆o Cont†bil: " + STRING(criter_distrib_cta_ctbl.cod_cta_ctbl)).

            ASSIGN c-cod-combinacao = criter_distrib_cta_ctbl.cod_cta_ctbl + "-" + ccusto_unid_negoc.cod_unid_negoc + "-" + criter_distrib_cta_ctbl.cod_estab + "-" + ccusto.cod_ccusto.
        
            PUT STREAM str-rp
                criter_distrib_cta_ctbl.cod_cta_ctbl          ","   /*CONTA CONTABIL*/
                ""                                            ","   /*UNIDADE DE NEGOCIO*/
                ""                                            ","   /*BRANCO*/
                criter_distrib_cta_ctbl.cod_estab             ","   /*ESTABELECIMENTO*/
                item_distrib_ccusto.cod_ccusto                ","   /*CENTRO CUSTO*/
                ""                                            ","   /*BRANCO*/
                ""                                            ","   /*BRANCO*/
                ""                                            ","   /*BRANCO*/
                c-cod-combinacao                                    /*CODIGO COMBINAÄ«O*/
                SKIP.

        END.

    END.



END.

OUTPUT CLOSE.



/*FOR EACH criter_distrib_cta_ctbl NO-LOCK:

    IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "1" THEN DO:

        RUN pi-acompanhar IN h-prog("Gerando Arquivo Combinaá∆o Cont†bil: " + STRING(criter_distrib_cta_ctbl.cod_cta_ctbl)).
    
        PUT STREAM str-rp
            criter_distrib_cta_ctbl.cod_cta_ctbl          ","   /*CONTA CONTABIL*/
            ""                                            ","   /*UNIDADE DE NEGOCIO*/
            ""                                            ","   /*BRANCO*/
            criter_distrib_cta_ctbl.cod_estab             ","   /*ESTABELECIMENTO*/
            ""                                            ","   /*CENTRO CUSTO*/
            ""                                            ","   /*BRANCO*/
            ""                                            ","   /*BRANCO*/
            ""                                            ","   /*BRANCO*/
            criter_distrib_cta_ctbl.cod_plano_cta_ctbl          /*CODIGO COMBINAÄ«O*/
            SKIP.

    END.

    IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "2" THEN DO:
    
        FOR EACH ems5.ccusto NO-LOCK:

            RUN pi-acompanhar IN h-prog("Gerando Arquivo Combinaá∆o Cont†bil: " + STRING(criter_distrib_cta_ctbl.cod_cta_ctbl)).
        
            PUT STREAM str-rp
                criter_distrib_cta_ctbl.cod_cta_ctbl          ","   /*CONTA CONTABIL*/
                ""                                            ","   /*UNIDADE DE NEGOCIO*/
                ""                                            ","   /*BRANCO*/
                criter_distrib_cta_ctbl.cod_estab             ","   /*ESTABELECIMENTO*/
                ccusto.cod_ccusto                             ","   /*CENTRO CUSTO*/
                ""                                            ","   /*BRANCO*/
                ""                                            ","   /*BRANCO*/
                ""                                            ","   /*BRANCO*/
                criter_distrib_cta_ctbl.cod_plano_cta_ctbl          /*CODIGO COMBINAÄ«O*/
                SKIP.

        END.

    END.

    IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "3" THEN DO:

        MESSAGE "aqui"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        FOR EACH item_distrib_ccusto WHERE
                 item_distrib_ccusto.cod_mapa_distrib_ccusto = item_distrib_ccusto.cod_mapa_distrib_ccusto NO-LOCK:

            RUN pi-acompanhar IN h-prog("Gerando Arquivo Combinaá∆o Cont†bil: " + STRING(criter_distrib_cta_ctbl.cod_cta_ctbl)).
        
            PUT STREAM str-rp
                criter_distrib_cta_ctbl.cod_cta_ctbl          ","   /*CONTA CONTABIL*/
                ""                                            ","   /*UNIDADE DE NEGOCIO*/
                ""                                            ","   /*BRANCO*/
                criter_distrib_cta_ctbl.cod_estab             ","   /*ESTABELECIMENTO*/
                item_distrib_ccusto.cod_ccusto                ","   /*CENTRO CUSTO*/
                ""                                            ","   /*BRANCO*/
                ""                                            ","   /*BRANCO*/
                ""                                            ","   /*BRANCO*/
                criter_distrib_cta_ctbl.cod_plano_cta_ctbl          /*CODIGO COMBINAÄ«O*/
                SKIP.

        END.

    END.



END.

OUTPUT CLOSE.
*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-condicao-pagamento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-condicao-pagamento Procedure 
PROCEDURE pi-condicao-pagamento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "PaymentTermsConsolidatedExport" + ".csv" )  NO-CONVERT.
DEF VAR c-desc-cond-pagto AS CHAR  FORMAT "x(40)".

/* CABEÄALHO */
PUT STREAM str-rp
    "UniqueName"         ","
    "Name"               ","
    "Default"            ","
    "Description"        ","
    "PurchasingUnits"    ","
    "InstallmentPercent" ","
    "InstallmentNumber"  ","
    "Discount"           ","
    "DiscountType"       ","
    "PayInDays"          
    SKIP. 

FOR EACH cond-pagto NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Cond Pgto: " + STRING(cond-pagto.cod-cond-pag)).

    ASSIGN c-desc-cond-pagto = REPLACE(cond-pagto.descricao,",","").

    PUT STREAM str-rp
        cond-pagto.cod-cond-pag          ","
        c-desc-cond-pagto                ","
        "No"                             ","   
        c-desc-cond-pagto                ","
        ""                               ","
        "0"                              ","
        ""                               ","
        ""                               ","
        ""                               ","
        ""                               
        SKIP.

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-conta-contabil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-conta-contabil Procedure 
PROCEDURE pi-conta-contabil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "AccountExport" + ".csv" )  NO-CONVERT.
DEF VAR c-des-ct-contabil AS CHAR  FORMAT "x(40)" NO-UNDO.

/* CABEÄALHO */
PUT STREAM str-rp
    "UniqueName"         ","
    "Name"               ","
    "Description"        ","
    "PurchasingUnits"    
    SKIP. 

FOR EACH cta_ctbl NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Cta Contabil: " + cta_ctbl.cod_cta_ctbl).

    ASSIGN c-des-ct-contabil = REPLACE(cta_ctbl.des_tit_ctbl,",","-").

    PUT STREAM str-rp
        cta_ctbl.cod_cta_ctbl                     ","
        c-des-ct-contabil                         ","
        c-des-ct-contabil                         ","
        "All"                            
        SKIP.
    

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-estabelec-endereco) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-estabelec-endereco Procedure 
PROCEDURE pi-estabelec-endereco :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "CompanyExport" + ".csv" )  NO-CONVERT.
DEF VAR c-endereco AS CHAR FORMAT "X(40)" NO-UNDO.

/* CABEÄALHO */
PUT STREAM str-rp
    "UniqueName"         ","
    "Name"               ","
    "Description"        ","
    "PurchasingUnit"     ","
    "PurchasingUnits"    
    SKIP. 

FOR EACH estabelec NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Estabelec: " + estabelec.cod-estabel).

    PUT STREAM str-rp
        estabelec.cod-estabel    ","
        estabelec.nome           ","
        estabelec.nome           ","
        "All"                    ","
        ""                       
        SKIP.
    

END.

OUTPUT CLOSE.

OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "AddressExport" + ".csv" )  NO-CONVERT.

/* CABEÄALHO */
PUT STREAM str-rp
    "UniqueName"              ","
    "Name"                    ","
    "Lines"                   ","
    "Line2"                   ","
    "Line3"                   ","
    "City"                    ","
    "State"                   ","
    "StateCode"               ","
    "PostalCode"              ","
    "Country"                 ","
    "Phone"                   ","
    "Fax"                     ","
    "Email"                   ","
    "URL"                     ","
    "BillTo"                  ","
    "ShipTo"                  ","
    "PurchasingUnit"          ","
    "Creator.UniqueName"      ","
    "Creator.PasswordAdapter" 
    SKIP. 

FOR EACH estabelec NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo End. Estabelec: " + estabelec.cod-estabel).

    c-endereco = REPLACE(estabelec.endereco,",","-").

    PUT STREAM str-rp
        estabelec.cod-estabel                 ","   
        estabelec.nome                        ","   
        c-endereco                            ","   
        ""                                    ","   
        ""                                    ","   
        estabelec.cidade                      ","   
        estabelec.estado                      ","   
        ""                                    ","   
        estabelec.cep                         ","   
        estabelec.pais                        ","   
        ""                                    ","   
        ""                                    ","   
        ""                                    ","   
        ""                                    ","   
        ""                                    ","   
        ""                                    ","   
        ""                                    ","   
        ""                                    ","   
        ""                       
        SKIP.
    

END.

OUTPUT CLOSE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-fornecedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fornecedor Procedure 
PROCEDURE pi-fornecedor :
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "Fornecedor" + ".csv" )  NO-CONVERT.
DEF VAR c-cpf  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR c-cnpj AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR c-emitente AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR d-total-pedido AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" decimals 8 no-undo.
DEF VAR i-cont-pedido  AS INT     NO-UNDO.

/* CABEÄALHO */
PUT STREAM str-rp
    "ID Fornecedor ERP"         "," 
    "CNPJ"                      "," 
    "CPF"                       "," 
    "Raz∆o Social / Nome"       "," 
    "Nome 2"                    "," 
    "Nome 3"                    "," 
    "Nome 4"                    "," 
    "Telefone"                  "," 
    "FAX"                       "," 
    "Rua"                       "," 
    "N£mero"                    "," 
    "Complemento"               "," 
    "Cidade"                    "," 
    "Estado"                    "," 
    "C¢digo do Estado"          "," 
    "C¢digo Postal/CEP"         "," 
    "Caixa Postal"              "," 
    "Pa°s"                      "," 
    "C¢digo do Pa°s"            "," 
    "Inscriá∆o Estadual"        "," 
    "Inscriá∆o Municipal"       "," 
    "e-mail contato Camil"      "," 
    "e-mail contato Camil 2"    "," 
    "Status Registro"           "," 
    "Status Qualificaá∆o"       "," 
    "Categoria"                 "," 
    "Data de requalificaá∆o"    "," 
    SKIP. 

/*FOR EACH pedido-compr WHERE 
         pedido-compr.data-pedido >= (TODAY - 600) AND 
         pedido-compr.situacao    <> 3                                NO-LOCK
    BREAK BY pedido-compr.cod-emitente:

    FOR EACH ordem-compra WHERE
             ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK:

        RUN pi-acompanhar IN h-prog("Pedido de Compra: " + STRING(pedido-compr.data-pedido) + " Ordem Compra: " + STRING(ordem-compra.numero-ordem)).

        ASSIGN d-total-pedido = d-total-pedido + ordem-compra.preco-orig
               i-cont-pedido  = i-cont-pedido + 1.

    END.

    IF LAST-OF (pedido-compr.cod-emitente) THEN DO:

        CREATE tt-emitente.
        ASSIGN tt-emitente.cod-emitente = pedido-compr.cod-emitente
               tt-emitente.total-pedido = d-total-pedido
               tt-emitente.cont-pedido  = i-cont-pedido.

        ASSIGN d-total-pedido = 0
               i-cont-pedido  = 0.

    END.

    
END.*/


/*FOR EACH tt-emitente WHERE
         tt-emitente.total-pedido  >= 2000 AND 
         tt-emitente.cont-pedido   >= 2    NO-LOCK:*/


FOR EACH emitente WHERE 
         emitente.identific = 2 OR 
         emitente.identific = 3 NO-LOCK:

    ASSIGN c-cpf  = ""
           c-cnpj = "".

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Fornecedor: " + STRING(emitente.nome-abrev)).

    IF LENGTH(emitente.cgc) = 11 THEN 
        ASSIGN c-cpf  = emitente.cgc.
    ELSE
        ASSIGN c-cnpj = emitente.cgc.

    ASSIGN c-emitente = REPLACE(emitente.nome-emit,",","-")
           c-emitente = REPLACE(c-emitente,"""","")
           c-emitente = REPLACE(c-emitente, ";","-").


    PUT STREAM str-rp
        emitente.cod-emitente                                          ","  /*"ID Fornecedor ERP"                */           
        c-cnpj                                FORMAT "X(20)"           ","  /*"CNPJ"                             */
        c-cpf                                 FORMAT "X(20)"           ","  /*"CPF"                              */
        c-emitente                            FORMAT "X(40)"           ","  /*"Raz∆o Social / Nome"              */
        REPLACE(emitente.nome-abrev,",","-")  FORMAT "X(40)"           ","  /*"Nome 2"                           */
        ""                                                             ","  /*"Nome 3"                           */
        ""                                                             ","  /*"Nome 4"                           */
        emitente.telefone[1]                                           ","  /*"Telefone"                         */
        emitente.telefone[2]                                           ","  /*"FAX"                              */
        REPLACE(emitente.endereco,",","-")   FORMAT "X(40)"            ","  /*"Rua"                              */
        ""                                                             ","  /*"N£mero"                           */
        REPLACE(emitente.endereco2,",","-")  FORMAT "X(40)"            ","  /*"Complemento"                      */
        emitente.cidade                                                ","  /*"Cidade"                           */
        emitente.estado                                                ","  /*"Estado"                           */
        ""                                                             ","  /*"C¢digo do Estado"                 */
        emitente.cep                         FORMAT "99999-999"        ","  /*"C¢digo Postal/CEP"                */
        ""                                                             ","  /*"Caixa Postal"                     */
        emitente.pais                                                  ","  /*"Pa°s"                             */
        ""                                                             ","  /*"C¢digo do Pa°s"                   */
        emitente.ins-estadual                                          ","  /*"Inscriá∆o Estadual"               */
        emitente.ins-municipal                                         ","  /*"Inscriá∆o Municipal"              */
        emitente.e-mail                                                ","  /*"e-mail contato Camil"             */
        ""                                                             ","  /*"e-mail contato Camil 2"           */
        ""                                                             ","  /*"Status Registro"                  */
        ""                                                             ","  /*"Status Qualificaá∆o"              */
        ""                                                             ","  /*"Categoria"                        */
        ""                                                             ","  /*"Data de requalificaá∆o"           */
        SKIP.
    

END.

OUTPUT CLOSE.


OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "PedidosFornecedores" + ".csv" )  NO-CONVERT.

FOR EACH tt-emitente NO-LOCK:

    PUT STREAM str-rp
        tt-emitente.cod-emitente   ","
        tt-emitente.total-pedido   ","
        tt-emitente.cont-pedido
        SKIP.   

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-item Procedure 
PROCEDURE pi-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "Familia-Item" + ".csv" )  NO-CONVERT.

DEF VAR c-narrativa AS CHAR NO-UNDO.
DEF VAR c-desc-item AS CHAR NO-UNDO.


/* CABEÄALHO */
PUT STREAM str-rp
    "Cod Item"           ";"
    "Desc Item"          ";"
    "Narrativa"          ";"
    "Familia Item"       ";"
    "Unidade Medida"        
    SKIP. 

FOR EACH ITEM WHERE 
         item.cod-obsoleto = 1 NO-LOCK:

    ASSIGN c-narrativa = REPLACE(ITEM.narrativa,CHR(10), " ")
           c-narrativa = REPLACE(c-narrativa,";", " ")
           c-desc-item = REPLACE(ITEM.desc-item,";","").

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Item.: " + ITEM.it-codigo).

    PUT STREAM str-rp
        ITEM.it-codigo                  ";"
        c-desc-item    FORMAT "x(60)"   ";"
        c-narrativa    FORMAT "x(2000)" ";"
        item.fm-codigo                  ";"
        ITEM.un         
        SKIP.
    

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-metodo-pagamento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-metodo-pagamento Procedure 
PROCEDURE pi-metodo-pagamento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "PaymentMethodTypeExport" + ".csv" )  NO-CONVERT.
DEF VAR c-desc-forma-pgto AS CHAR FORMAT "X(40)" NO-UNDO.

/* CABEÄALHO */
PUT STREAM str-rp
    "Name"               ","
    "UniqueName"         ","
    "Description"        ","
    "Rank"               ","
    "Electronic"         ","
    "ClearancePeriod"    ","
    "CanonicalName"      
    SKIP. 

FOR EACH forma_pagto NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Metodo Pgto: " + forma_pagto.des_forma_pagto).

    ASSIGN c-desc-forma-pgto = REPLACE(forma_pagto.des_forma_pagto,",","-").

    PUT STREAM str-rp
        c-desc-forma-pgto                               ","
        forma_pagto.cod_forma_pagto                     ","     
        c-desc-forma-pgto                               ","
        "1"                                             ","     
        "Yes"                                           ","     
        "2"                                             ","     
        "other"                               
        SKIP.
    

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-tipos-conta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tipos-conta Procedure 
PROCEDURE pi-tipos-conta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "AccountTypeExport" + ".csv" )  NO-CONVERT.

/* CABEÄALHO */
PUT STREAM str-rp
    "UniqueName"         ","
    "Name      "         ","
    "Description"        
    SKIP. 

FOR EACH grp_cta_ctbl NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Tp Contas: " + grp_cta_ctbl.cod_grp_cta_ctbl).

    PUT STREAM str-rp
        grp_cta_ctbl.cod_grp_cta_ctbl         ","
        grp_cta_ctbl.des_grp_cta_ctbl         ","
        grp_cta_ctbl.des_grp_cta_ctbl         
        SKIP.
    

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-unidade-medida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-unidade-medida Procedure 
PROCEDURE pi-unidade-medida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "UnitOfMeasureExport" + ".csv" )  NO-CONVERT.

/* CABEÄALHO */
PUT STREAM str-rp
    "UniqueName"         ","
    "Name"               ","
    "AllowNonWhole"      ","
    "Category"           ","
    "Description"        
    SKIP. 

FOR EACH tab-unidade NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Unidade Med.: " + tab-unidade.un).

    PUT STREAM str-rp
        tab-unidade.un                ","
        tab-unidade.descricao         ","
        "Yes"                         ","
        "0"                           ","
        tab-unidade.descricao         
        SKIP.
    

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-unidade-negocio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-unidade-negocio Procedure 
PROCEDURE pi-unidade-negocio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "BusinessUnitExport" + ".csv" )  NO-CONVERT.

/* CABEÄALHO */
PUT STREAM str-rp
    "Company"         ","
    "UniqueName"      ","
    "Name"            ","
    "Description"     ","
    "PurchasingUnit"  ","
    "PurchasingUnits" 
    SKIP. 

FOR EACH unid_negoc NO-LOCK:

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Unidade Negocio: " + unid_negoc.cod_unid_negoc).

    PUT STREAM str-rp
        "40"                          ","
        unid_negoc.cod_unid_negoc     ","
        unid_negoc.des_unid_negoc     ","
        unid_negoc.des_unid_negoc     ","
        "All"                         ","
        ""                            
        SKIP.
    

END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-usuarios) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-usuarios Procedure 
PROCEDURE pi-usuarios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM str-rp TO VALUE(tt-param.diretorio + "\" + "UserConsolidatedExport" + ".csv" )  NO-CONVERT.


DEF VAR c-cod_usuario_aprov AS CHAR NO-UNDO.
DEF VAR c-cpf_usuario       AS CHAR NO-UNDO.
DEF VAR c-cc_codigo         AS CHAR NO-UNDO.
DEF VAR c-desl_defin        AS CHAR NO-UNDO.
DEF VAR t-usuar_terc        AS CHAR NO-UNDO.
DEF VAR c-tp-usuario        AS CHAR NO-UNDO.
DEF VAR c-estabel-usar      AS CHAR NO-UNDO.


/* CABEÄALHO */
PUT STREAM str-rp
    "UniqueName"                        ","
    "PasswordAdapter"                   ","
    "Name"                              ","
    "EmailAddress"                      ","
    "DefaultCurrency.UniqueName"        ","
    "JobId"                             ","
    "LocaleID.UniqueName"               ","
    "TimeZoneID"                        ","
    "Phone"                             ","
    "Fax"                               ","
    "FailedLoginAttemptAfterLastLogin"  ","
    "LoginDate"                         ","
    "LastLoginDate"                     ","
    "Supervisor.UniqueName"             ","
    "Supervisor.PasswordAdapter"        ","
    "AlternateEmailAddresses"           ","
    "VanillaDeliverTo"                  ","
    "GenericBillingAddress"             ","
    "GenericShipTo"                     ","
    "ApprovalLimit"                     ","    
    "ExpenseApprovalLimit"              ","    
    "Company"                           ","    
    "BusinessUnit"                      ","    
    "CostCenter"                        ","    
    "Product"                           ","    
    "Project"                           ","    
    "Account"                           ","    
    "SubAccount"                        ","    
    "Region"                            ","
    "ImportCtrl" 
    SKIP. 

FOR EACH usuar_mestre WHERE
         usuar_mestre.dat_fim_valid >= TODAY NO-LOCK:

    ASSIGN c-tp-usuario = "ThirdPartyUser".

    FIND FIRST usuar-mater WHERE
               usuar-mater.cod-usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
    IF AVAIL usuar-mater THEN
        ASSIGN c-estabel-usar = SUBSTRING(usuar-mater.char-1,16,2).

    /*

    FIND FIRST ext_usuar_mestre_ems2 WHERE 
               ext_usuar_mestre_ems2.cod_usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
    IF AVAIL ext_usuar_mestre_ems2 THEN DO:

        ASSIGN c-cod_usuario_aprov = ext_usuar_mestre_ems2.cod_usuario_gu 
               c-cpf_usuario       = ext_usuar_mestre_ems2.cpf 
               c-cc_codigo         = ext_usuar_mestre_ems2.cod_rh_ccusto.

        IF ext_usuar_mestre_ems2.usuar_terc THEN
            c-tp-usuario = "ThirdPartyUser".
        ELSE
            c-tp-usuario = "PasswordAdapter1".

    END.
    
    */

    RUN pi-acompanhar IN h-prog("Gerando Arquivo Usuarios: " + usuar_mestre.cod_usuario).

    PUT STREAM str-rp
        usuar_mestre.cod_usuario             ","  /*"UniqueName"                             */
        c-tp-usuario          FORMAT "x(30)" ","  /*"PasswordAdapter"                        */
        usuar_mestre.nom_usuario              ","  /*"Name"                                   */
        usuar_mestre.cod_e_mail_local        ","  /*"EmailAddress"                           */
        ""                                   ","  /*"DefaultCurrency.UniqueName"             */
        ""                                   ","  /*"JobId"                                  */
        ""                                   ","  /*"LocaleID.UniqueName"                    */
        ""                                   ","  /*"TimeZoneID"                             */
        ""                                   ","  /*"Phone"                                  */
        ""                                   ","  /*"Fax"                                    */
        ""                                   ","  /*"FailedLoginAttemptAfterLastLogin"       */
        ""                                   ","  /*"LoginDate"                              */
        ""                                   ","  /*"LastLoginDate"                          */
        c-cod_usuario_aprov                  ","  /*"Supervisor.UniqueName"                  */
        c-tp-usuario       FORMAT "x(30)"    ","  /*"Supervisor.PasswordAdapter"             */
        ""                                   ","  /*"AlternateEmailAddresses"                */
        ""                                   ","  /*"VanillaDeliverTo"                       */
        ""                                   ","  /*"GenericBillingAddress"                  */
        ""                                   ","  /*"GenericShipTo"                          */
        ""                                   ","  /*"ApprovalLimit"                          */
        ""                                   ","  /*"ExpenseApprovalLimit"                   */
        c-estabel-usar                       ","  /*"Company"                                */
        ""                                   ","  /*"BusinessUnit"                           */
        c-cc_codigo                          ","  /*"CostCenter"                             */
        ""                                   ","  /*"Product"                                */
        ""                                   ","  /*"Project"                                */
        ""                                   ","  /*"Account"                                */
        ""                                   ","  /*"SubAccount"                             */
        ""                                   ","  /*"Region"                                 */
        "Both"                                    /*"ImportCtrl"                             */
        SKIP.
    

END.

OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

