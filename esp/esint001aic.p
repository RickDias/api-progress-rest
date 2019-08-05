
{esp/alias-table.i}

DEFINE BUFFER empresa FOR mgcad.empresa.
DEFINE BUFFER portador FOR mgcad.portador.

/* ----------------------------------------------- */
{esp/esint001aic.i}
{esp/esint001a.i}

DEFINE INPUT  PARAMETER TABLE FOR tt_emitente.
DEFINE INPUT  PARAM     TABLE FOR tt_ContatoList.
DEFINE INPUT  PARAM     TABLE FOR tt_CondicaoPagamentoList.
DEFINE OUTPUT PARAMETER TABLE FOR tt-erros-integracao. 
DEFINE OUTPUT PARAMETER pCodEmitenteReturn as INTEGER.

/* DEF VAR i-count AS INTEGER NO-UNDO.  */

DEFINE VARIABLE c-destino  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-filename AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v_cod_erro               AS CHARACTER FORMAT "x(10)":U                     COLUMN-LABEL "Cod Erro"    NO-UNDO.
DEFINE VARIABLE v_cod_id_estad_jurid_sai AS CHARACTER FORMAT "x(20)":U LABEL "ID Estadual" COLUMN-LABEL "ID Estadual" NO-UNDO.
DEFINE VARIABLE execute_evoluida_2  AS HANDLE            NO-UNDO.
DEFINE VARIABLE c-arquivo-saida     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE iNumEmit            AS INTEGER           NO-UNDO INITIAL 0.
DEFINE VARIABLE r-emitente          AS RECID             NO-UNDO.
DEFINE VARIABLE row-id-emitente     AS ROWID             NO-UNDO.
DEFINE VARIABLE i-cod-rep-ant       AS INTEGER           NO-UNDO.
DEFINE VARIABLE cont                AS INTEGER INITIAL 1 NO-UNDO.
DEF VAR v_tipo_movto AS CHARACTER NO-UNDO.
/*Cadastro de campos especificos*/
DEFINE VARIABLE h-cadastraEmitenteEsp AS HANDLE          NO-UNDO.

DEFINE VARIABLE nr-telefone-finan AS CHARACTER   NO-UNDO.
DEFINE VARIABLE email-nfe         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE email-financ      AS CHARACTER   NO-UNDO.

/* Variaveis usadas na procedure pi-grandes-red */
DEFINE VARIABLE c-nome-abrev-rede  LIKE emitente.nome-abrev no-undo.
DEFINE VARIABLE c-nome-abrev-raiz  LIKE emitente.nome-abrev no-undo.
DEFINE VARIABLE c-sequencia        AS CHARACTER              NO-UNDO.
DEFINE VARIABLE i-sequencia        AS INTEGER                NO-UNDO.
DEFINE VARIABLE i-cont             AS INTEGER                NO-UNDO.
DEFINE VARIABLE c-nome-matriz-rede LIKE emitente.nome-matriz no-undo.
DEFINE VARIABLE i-portador-rede    LIKE emitente.portador    no-undo.
DEFINE VARIABLE i-modalidade-rede  LIKE emitente.modalidade  no-undo.
DEFINE VARIABLE i-port-prefer-rede LIKE emitente.port-prefer no-undo.
DEFINE VARIABLE i-mod-prefer-rede  LIKE emitente.mod-prefer  no-undo.
DEFINE VARIABLE i-cod-gr-cli-rede  LIKE emitente.cod-gr-cli  no-undo.
DEFINE VARIABLE i-instr-banc       AS INTEGER NO-UNDO.
DEFINE VARIABLE i-portador-ap      LIKE emitente.portador-ap   NO-UNDO.
DEFINE VARIABLE i-modalidade-ap    LIKE emitente.modalidade-ap NO-UNDO.
DEFINE VARIABLE i-int1             AS INTEGER                  NO-UNDO.
DEFINE BUFFER bf-es-gp-emit-canal FOR es-gp-emit-canal.
DEFINE BUFFER bf-es-emitente      FOR es-emitente.

ASSIGN c-destino = "Arquivo"
       c-filename = SESSION:TEMP-DIRECTORY + "/atualizaems5-"
                    + STRING(DAY(TODAY),"99")
                    + STRING(MONTH(TODAY),"99")
                    + STRING(YEAR(TODAY),"9999")
                    + STRING(TIME)
                    + "tmp".
DEF STREAM S1.
DEF VAR C-TESTE AS CHARACTER.
ASSIGN      c-TESTE = SESSION:TEMP-DIRECTORY + "/atualizaems5-"
                    + STRING(DAY(TODAY),"99")
                    + STRING(MONTH(TODAY),"99")
                    + STRING(YEAR(TODAY),"9999")
                    + STRING(TIME)
                    + "c.tmp".
OUTPUT STREAM S1 TO VALUE(C-TESTE).
    
FUNCTION fnc-proximo-emit RETURNS INTEGER() FORWARD.

define temp-table tt-erros NO-UNDO
    field cod-erro  as INTEGER
    field desc-erro as character format "x(256)"
    field desc-arq  as character.


FIND FIRST param-global NO-LOCK NO-ERROR.

DEF BUFFER usuar_mestre FOR usuar_mestre.




/* Begins  ===================================================================================== */
RUN btb/btapi910za.p (INPUT "geosales",
                      INPUT "camil2016",
                      OUTPUT TABLE tt-erros).
/* End    ===================================================================================== */

{utp\ut-glob.i}

find empresa no-lock
    where empresa.ep-codigo = i-ep-codigo-usuario no-error.
FIND FIRST tt_emitente NO-LOCK NO-ERROR.
EMPTY TEMP-TABLE tt-emitente.
FIND FIRST emitente WHERE emitente.cgc = trim(tt_emitente.cnpj) NO-LOCK NO-ERROR.
IF NOT AVAIL emitente THEN DO:
    ASSIGN v_tipo_movto = "Incluir".
END.
ELSE DO:
    IF emitente.identific = 2 THEN 
        ASSIGN v_tipo_movto = "Alterar".
    IF emitente.identific <> 2 THEN
    DO:
       CREATE tt-erros-integracao.
       ASSIGN tt-erros-integracao.erro  = "17006"
              tt-erros-integracao.descricao = "Cliente ja existe no cadastro." + STRING(emitente.cod-emitente).
       RETURN "NOK". 
    END.
END.

RUN piImportParam.

FIND FIRST tt-emitente EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL tt-emitente THEN DO:
    CREATE tt-erros-integracao.
    ASSIGN tt-erros-integracao.erro  = "17006"
           tt-erros-integracao.descricao = "Dados da tt-emitente n?o foram enviados.".
    RETURN "NOK".
END.

DO TRANSACTION:
IF tt-emitente.tipo-movimento = "Incluir" THEN DO:

    ASSIGN iNumEmit = fnc-proximo-emit().

    ASSIGN i-cod-rep-ant = 0.

    
END.
ELSE DO: /* alteraùˇo */

    FIND emitente WHERE emitente.cgc = tt-emitente.cpfcnpj NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:
/*         ASSIGN i-count = i-count + 1. */

        ASSIGN iNumEmit         = integer(emitente.cod-emitente)
               i-cod-rep-ant    = emitente.cod-rep
               i-portador-ap    = emitente.portador-ap
               i-modalidade-ap  = emitente.modalidade-ap.
  END.
END.                           
END.

ASSIGN pCodEmitenteReturn = iNumEmit.

FIND FIRST es-api-param-cliente NO-LOCK NO-ERROR.

CREATE tt_emitente_integr_old_2.
ASSIGN tt_emitente_integr_old_2.cod_versao_integracao = 1
       tt_emitente_integr_old_2.cod_emitente          = iNumEmit
       tt_emitente_integr_old_2.identific             = integer(tt-emitente.identific)
       tt_emitente_integr_old_2.cod_portador          = integer(tt-emitente.portador)
       tt_emitente_integr_old_2.modalidade            = integer(tt-emitente.Modalidade)
       tt_emitente_integr_old_2.port_prefer           = integer(tt-emitente.portador_pref)
       tt_emitente_integr_old_2.mod_prefer            = integer(tt-emitente.modalidade_pref)
       tt_emitente_integr_old_2.tp_rec_padrao         = integer(tt-emitente.cod_receita)
       tt_emitente_integr_old_2.data_implant          = TODAY
       tt_emitente_integr_old_2.cod_gr_forn           = es-api-param-cliente.cod-gr-for WHEN AVAILABLE es-api-param-cliente.

IF  tt-emitente.tipo-movimento <> "Incluir" THEN
DO:
   IF NOT AVAILABLE emitente THEN
      FIND FIRST emitente WHERE emitente.cgc = tt-emitente.cpfcnpj NO-LOCK NO-ERROR.
   ASSIGN tt_emitente_integr_old_2.identific             = 2 
          tt_emitente_integr_old_2.cod_gr_forn           = emitente.cod-gr-for
          tt_emitente_integr_old_2.tp_desp_padrao        = emitente.tp-desp-padrao.
   IF AVAILABLE es-api-param-cliente THEN
      ASSIGN tt_emitente_integr_old_2.cod_portador          = integer(es-api-param-cliente.portador)  
             /* tt_emitente_integr_old_2.cod_portador          = emitente.portador     */
             tt_emitente_integr_old_2.modalidade            = integer(es-api-param-cliente.Modalidade)
             tt_emitente_integr_old_2.port_prefer           = integer(es-api-param-cliente.port-prefer)
             tt_emitente_integr_old_2.mod_prefer            = integer(es-api-param-cliente.mod-prefer).
END.
FOR FIRST UNID-FEDER NO-LOCK WHERE
          UNID-FEDER.estado = tt-emitente.cod_estado.
    ASSIGN tt-emitente.nome_pais = unid-feder.pais.
END.

FOR FIRST UNID-FEDER NO-LOCK WHERE
          UNID-FEDER.estado = tt-emitente.Estado_cob.
    ASSIGN tt-emitente.nome_pais_cob = unid-feder.pais.
END.
IF tt-emitente.nome_pais_cob = "" THEN
    FOR FIRST UNID-FEDER NO-LOCK WHERE
              UNID-FEDER.estado = tt-emitente.cod_estado.
        ASSIGN tt-emitente.nome_pais_cob = unid-feder.pais.
    END.

FOR FIRST mgdis.cidade NO-LOCK 
    WHERE mgdis.cidade.cidade   = tt-emitente.nome_cidade 
      AND mgdis.cidade.estado   = tt-emitente.cod_estado  
      AND mgdis.cidade.pais     = tt-emitente.nome_pais:
END.


ASSIGN tt_emitente_integr_old_2.cod_pais             = tt-emitente.nome_pais
       tt_emitente_integr_old_2.pais_cob             = tt-emitente.nome_pais_cob
       tt_emitente_integr_old_2.ven_sabado           = 1
       tt_emitente_integr_old_2.ven_domingo          = 1
       tt_emitente_integr_old_2.ven_feriado          = 1
       tt_emitente_integr_old_2.ep_codigo            = empresa.ep-codigo
       tt_emitente_integr_old_2.ep_codigo_principal  = empresa.ep-codigo
       tt_emitente_integr_old_2.num_tip_operac       = 1
       tt_emitente_integr_old_2.forn_exp             = NO.

IF integer(tt-emitente.identific) = 1 THEN DO:
    ASSIGN tt_emitente_integr_old_2.tp_desp_padrao       = 1.
END.


ASSIGN tt_emitente_integr_old_2.nome_emit    = tt-emitente.Nome_emit
       tt_emitente_integr_old_2.cod_gr_cli   = integer(tt-emitente.cod_grupo)
       tt_emitente_integr_old_2.nome_matriz  = tt-emitente.nome_cliente
       tt_emitente_integr_old_2.nome_abrev   = tt-emitente.nome_abreviado
       tt_emitente_integr_old_2.cod_rep      = integer(tt-emitente.cod_representante)
       tt_emitente_integr_old_2.endereco     = tt-emitente.endereco
       tt_emitente_integr_old_2.cod_pais     = tt-emitente.nome_pais
       tt_emitente_integr_old_2.bairro       = tt-emitente.bairro
       tt_emitente_integr_old_2.cep          = tt-emitente.Cep.

ASSIGN tt_emitente_integr_old_2.estado       = tt-emitente.cod_estado
       tt_emitente_integr_old_2.nom_cidade   = tt-emitente.nome_cidade
       tt_emitente_integr_old_2.endereco_cob = tt-emitente.endereco_cob
       tt_emitente_integr_old_2.bairro_cob   = tt-emitente.bairro_cob
       tt_emitente_integr_old_2.cep_cob      = replace(tt-emitente.Cep_cob, "-","")
       tt_emitente_integr_old_2.estado_cob   = tt-emitente.Estado_cob.
       
ASSIGN tt_emitente_integr_old_2.cidade_cob   = tt-emitente.nome_cidade2
       tt_emitente_integr_old_2.pais_cob     = tt-emitente.nome_pais_cob
       tt_emitente_integr_old_2.telefone[1]  = tt-emitente.Telefone
       tt_emitente_integr_old_2.ramal[1]     = tt-emitente.Ramal
       tt_emitente_integr_old_2.telefone[2]  = tt-emitente.Telefone2.
/*                        */
/* i-count = i-count + 1. */

ASSIGN tt_emitente_integr_old_2.ramal[2]     = tt-emitente.Ramal2
       tt_emitente_integr_old_2.telefax      = tt-emitente.Telefax
       tt_emitente_integr_old_2.ramal_fax    = tt-emitente.Ramal_fax
       tt_emitente_integr_old_2.ramal_modem  = tt-emitente.Ramal_modem.

ASSIGN tt_emitente_integr_old_2.e_mail       = IF tt-emitente.email <> ? THEN tt-emitente.email ELSE ""
       tt_emitente_integr_old_2.natureza     = integer(tt-emitente.nat_cliente)
       tt_emitente_integr_old_2.cgc          = tt-emitente.cpfcnpj
       tt_emitente_integr_old_2.ins_municipal= IF tt-emitente.Ins_municipal <> ? THEN tt-emitente.Ins_municipal ELSE ""
       .

ASSIGN tt_emitente_integr_old_2.emite_bloq   = IF AVAIL es-api-param-cliente THEN es-api-param-cliente.emite-bloq ELSE NO
       tt_emitente_integr_old_2.nome_mic_reg = IF AVAIL mgdis.cidade THEN mgdis.cidade.nome-mic-reg ELSE ""
       tt_emitente_integr_old_2.data_implant = date(tt-emitente.dt_implantacao) 
       tt_emitente_integr_old_2.ins_banc[1]  = integer(tt-emitente.cd_instrucao_bancaria)         NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:

    CREATE tt-erros-integracao.
    ASSIGN tt-erros-integracao.erro  = "17006"
          tt-erros-integracao.descricao = "ERRO NO FORMATO DOS DADOS ENVIADOS."                                + CHR(10) +
                                          "###tt-emitente.Nome_emit                                          " + STRING(tt-emitente.Nome_emit                                        ) + CHR(10) + 
                                          "###integer(tt-emitente.cod_grupo)                                 " + STRING(tt-emitente.cod_grupo                                        ) + CHR(10) + 
                                          "###tt-emitente.nome_cliente                                       " + STRING(tt-emitente.nome_cliente                                     ) + CHR(10) + 
                                          "###tt-emitente.nome_abreviado                                     " + STRING(tt-emitente.nome_abreviado                                   ) + CHR(10) + 
                                          "###integer(tt-emitente.cod_representante)                         " + STRING(tt-emitente.cod_representante                                ) + CHR(10) + 
                                          "###tt-emitente.endereco                                           " + STRING(tt-emitente.endereco                                         ) + CHR(10) + 
                                          "###tt-emitente.nome_pais                                          " + STRING(tt-emitente.nome_pais                                        ) + CHR(10) + 
                                          "###tt-emitente.bairro                                             " + STRING(tt-emitente.bairro                                           ) + CHR(10) + 
                                          "###tt-emitente.Cep                                                " + STRING(tt-emitente.Cep                                              ) + CHR(10) + 
                                          "###tt-emitente.cod_estado                                         " + STRING(tt-emitente.cod_estado                                       ) + CHR(10) + 
                                          "###tt-emitente.nome_cidade                                        " + STRING(tt-emitente.nome_cidade                                      ) + CHR(10) + 
                                          "###tt-emitente.endereco_cob                                       " + STRING(tt-emitente.endereco_cob                                     ) + CHR(10) + 
                                          "###tt-emitente.bairro_cob                                         " + STRING(tt-emitente.bairro_cob                                       ) + CHR(10) + 
                                          "###replace(tt-emitente.Cep_cob                                    " + STRING(tt-emitente.Cep_cob                                          ) + CHR(10) + 
                                          "###tt-emitente.Estado_cob                                         " + STRING(tt-emitente.Estado_cob                                       ) + CHR(10) + 
                                          "###tt-emitente.nome_cidade2                                       " + STRING(tt-emitente.nome_cidade2                                     ) + CHR(10) + 
                                          "###tt-emitente.nome_pais_cob                                      " + STRING(tt-emitente.nome_pais_cob                                    ) + CHR(10) + 
                                          "###tt-emitente.Telefone                                           " + STRING(tt-emitente.Telefone                                         ) + CHR(10) + 
                                          "###tt-emitente.Ramal                                              " + STRING(tt-emitente.Ramal                                            ) + CHR(10) + 
                                          "###tt-emitente.Telefone2                                          " + STRING(tt-emitente.Telefone2                                        ) + CHR(10) + 
                                          "###tt-emitente.Ramal2                                             " + STRING(tt-emitente.Ramal2                                           ) + CHR(10) + 
                                          "###tt-emitente.Telefax                                            " + STRING(tt-emitente.Telefax                                          ) + CHR(10) + 
                                          "###tt-emitente.Ramal_fax                                          " + STRING(tt-emitente.Ramal_fax                                        ) + CHR(10) + 
                                          "###tt-emitente.Ramal_modem                                        " + STRING(tt-emitente.Ramal_modem                                      ) + CHR(10) + 
                                          "###tt-emitente.email                                              " + STRING(tt-emitente.email                                            ) + CHR(10) + 
                                          "###integer(tt-emitente.nat_cliente)                               " + STRING(tt-emitente.nat_cliente                                      ) + CHR(10) + 
                                          "###tt-emitente.cpfcnpj                                            " + STRING(tt-emitente.cpfcnpj                                          ) + CHR(10) + 
                                          "###tt-emitente.Ins_municipal                                      " + STRING(tt-emitente.Ins_municipal                                    ) + CHR(10) + 
                                          "###integer(tt-emitente.portador)                                  " + STRING(tt-emitente.portador                                         ) + CHR(10) + 
                                          "###integer(tt-emitente.portador_pref)                             " + STRING(tt-emitente.portador_pref                                    ) + CHR(10) + 
                                          "###integer(tt-emitente.modalidade_pref)                           " + STRING(tt-emitente.modalidade_pref                                  ) + CHR(10) + 
                                          "###IF logical(tt-emitente.emit_boleto) THEN YES ELSE NO           " + STRING(tt-emitente.emit_boleto                                      ) + CHR(10) + 
                                          "###date(tt-emitente.dt_implantacao)                               " + STRING(tt-emitente.dt_implantacao                                   ) + CHR(10) + 
                                          "###integer(tt-emitente.cd_instrucao_bancaria)                     " + STRING(tt-emitente.cd_instrucao_bancaria                            ) + CHR(10).

   RETURN "NOK".    
END.

ASSIGN tt_emitente_integr_old_2.nome_matriz      = IF tt-emitente.nome_cliente = "" THEN tt-emitente.nome_abreviado ELSE tt-emitente.nome_cliente
       tt_emitente_integr_old_2.ins_estadual     = IF tt-emitente.Ins_estadual = "" THEN "ISENTO" ELSE UPPER(STRING(tt-emitente.Ins_estadual,"x(20)"))
       tt_emitente_integr_old_2.Ins_est_cob      = IF tt-emitente.Ins_est_cob  = "" THEN "ISENTO" ELSE UPPER(STRING(tt-emitente.Ins_est_cob,"x(20)")) NO-ERROR.

IF i-portador-rede <> 0 THEN
   ASSIGN tt_emitente_integr_old_2.cod_portador = i-portador-rede
          tt_emitente_integr_old_2.modalidade   = i-modalidade-rede.
IF c-nome-matriz-rede <> "" THEN
   ASSIGN tt_emitente_integr_old_2.nome_matriz  = c-nome-matriz-rede. 
IF c-nome-abrev-rede <> "" THEN
   ASSIGN tt_emitente_integr_old_2.nome_abrev   = c-nome-abrev-rede. 
/*           i-count = i-count + 1.  */
IF ERROR-STATUS:ERROR THEN DO:
  CREATE tt-erros-integracao.
  ASSIGN tt-erros-integracao.erro  = "17006"
         tt-erros-integracao.descricao = "ERRO NO FORMATO DOS DADOS ENVIADOS."                                + CHR(10) +
                                         "###tt-emitente.nome_cliente    " + STRING(tt-emitente.nome_cliente) + CHR(10) +
                                         "###tt-emitente.Ins_estadual    " + STRING(tt-emitente.Ins_estadual) + CHR(10) +
                                         "###tt-emitente.Ins_est_cob     " + STRING(tt-emitente.Ins_est_cob ).

   RETURN "NOK". 
END.
/* --------------------------------------------------------------- */
CREATE tt_cont_emit_integr_new.
ASSIGN tt_cont_emit_integr_new.cod_emitente          = iNumEmit
       tt_cont_emit_integr_new.sequencia	         = integer(tt-emitente.cd_sequencia_contato_emitente)
       tt_cont_emit_integr_new.nome                  = tt-emitente.nm_contato_emitente
       tt_cont_emit_integr_new.area                  = tt-emitente.nm_area_contato_emitente.

ASSIGN tt_cont_emit_integr_new.des_cargo             = tt-emitente.nm_cargo_contato_emitente
       tt_cont_emit_integr_new.telefone		         = tt-emitente.nr_telefone_contato_emitente
       tt_cont_emit_integr_new.ramal                 = tt-emitente.nr_ramal_contato_emitente              
       tt_cont_emit_integr_new.e_mail                = tt-emitente.nm_email_contato_emitente
       overlay(tt_cont_emit_integr_new.char-2,1,20)  = tt-emitente.nr_cpf_cnpj_contato_emitente
       tt_cont_emit_integr_new.cod_versao_integracao = 1
       tt_cont_emit_integr_new.num_tip_operac        = 1 NO-ERROR.

/* --------------------------------------------------------------- */
ASSIGN nr-telefone-finan = tt-emitente.nr_telefone_contato_emitente
       email-nfe         = tt-emitente.nm_internet_email
       email-financ      = tt-emitente.nr_inscricao_estadual.

IF ERROR-STATUS:ERROR THEN DO:
    CREATE tt-erros-integracao.
    ASSIGN tt-erros-integracao.erro  = "17006"
         tt-erros-integracao.descricao = "ERRO NO FORMATO DOS DADOS ENVIADOS."                        + CHR(10) +
                                         "### iNumEmit                                            " + string(iNumEmit                                            ) + chr(10) + 
                                         "### integer(tt-emitente.cd_sequencia_contato_emitente)  " + string(integer(tt-emitente.cd_sequencia_contato_emitente)  ) + chr(10) + 
                                         "### tt-emitente.nm_contato_emitente                     " + string(tt-emitente.nm_contato_emitente                     ) + chr(10) + 
                                         "### tt-emitente.nm_area_contato_emitente                " + string(tt-emitente.nm_area_contato_emitente                ) + chr(10) + 
                                         "### tt-emitente.nm_cargo_contato_emitente               " + string(tt-emitente.nm_cargo_contato_emitente               ) + chr(10) + 
                                         "### tt-emitente.nr_telefone_contato_emitente            " + string(tt-emitente.nr_telefone_contato_emitente            ) + chr(10) + 
                                         "### tt-emitente.nr_ramal_contato_emitente               " + string(tt-emitente.nr_ramal_contato_emitente               ) + chr(10) +                                                                                   
                                         "### tt-emitente.nm_email_contato_emitente               " + string(tt-emitente.nm_email_contato_emitente               ) + chr(10) +  
                                         "### tt-emitente.nr_cpf_cnpj_contato_emitente            " + string(tt-emitente.nr_cpf_cnpj_contato_emitente            ). 

   RETURN "NOK". 
END.

blk_insert-emitente:
DO TRANSACTION ON ERROR UNDO, LEAVE:
   
    IF c-nome-abrev-rede  = "" AND 
       LENGTH(tt-emitente.nome_abreviado) <> 12 THEN
    DO:
        CREATE tt-erros-integracao.
        ASSIGN tt-erros-integracao.erro  = "17006"
               tt-erros-integracao.descricao = "Nome Abreviado tem que possuir 12 posicoes.".        
    END.
    

    {esp/esint001aic.i1}

    IF CAN-FIND(FIRST tt-erros-integracao) THEN DO:
        UNDO blk_insert-emitente, LEAVE blk_insert-emitente.
    END.
    ELSE DO:

        FIND LAST emitente WHERE emitente.cod-emitente = iNumEmit EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL emitente THEN DO:
            ASSIGN r-emitente                                   = RECID(emitente)
                   row-id-emitente                              = ROWID(emitente)
                   /* Alteracao Tiago DSC 20151023 - Fixar cod-emitente para cliente cobranca
                   emitente.end-cobranca                        = integer(tt-emitente.cod_id_cliente_cobranca)*/
                   emitente.end-cobranca                        = integer(iNumEmit)
                   /*******/
                   emitente.cgc-cob                             = tt-emitente.cpfcnpjCobranca
                   emitente.cod-canal-venda                     = integer(tt-emitente.cod_canal_venda)
                   emitente.nat-operacao                        = tt-emitente.natureza_operacao
                   emitente.nat-ope-ext                         = tt-emitente.natureza_interestadual.
            IF v_tipo_movto <> "Alterar" THEN
               ASSIGN emitente.cod-cond-pag                        = integer(tt-emitente.cond_pag).
            ASSIGN emitente.cod-transp                          = integer(tt-emitente.cod_transportadora)
                   overlay(emitente.char-1,21,1)                = tt-emitente.suspensao_ipi
                   emitente.telef-fac                           = tt-emitente.Telefax2
                   emitente.ins-banc[1]                         = int(tt-emitente.cd_instrucao_bancaria)
                   emitente.pais                                = tt-emitente.nome_pais
                   emitente.estado                              = tt-emitente.cod_estado
                   emitente.endereco                            = tt-emitente.endereco.

            FOR FIRST UNID-FEDER NO-LOCK WHERE UNID-FEDER.estado = EMITENTE.estado.
                ASSIGN emitente.pais = unid-feder.pais.
            END.
            /*habilitado no inovar-auto                                 */
            ASSIGN emitente.log-nf-eletro                       = logical(tt-emitente.nfe)
                   overlay(emitente.char-1,21,1)                = IF logical(tt-emitente.suspensao_ipi) THEN "2" ELSE "1"
                   emitente.endereco-cob                        = IF tt-emitente.endereco_cob <> ? THEN tt-emitente.endereco_cob ELSE ""
                   emitente.estado-cob                          = IF tt-emitente.estado_cob <> ? THEN UPPER(tt-emitente.estado_cob) ELSE ""
                   emitente.pais-cob                            = IF tt-emitente.nome_pais_cob <> ? THEN UPPER(tt-emitente.nome_pais_cob) ELSE ""
                   /* 24/07/2019 Cristiane */
                   emitente.cidade-cob                          = IF tt-emitente.cidade_cob <> ? THEN UPPER(tt-emitente.cidade_cob) ELSE ""
                   emitente.bairro-cob                          = IF tt-emitente.bairro_cob <> ? THEN UPPER(tt-emitente.bairro_cob) ELSE ""
                   emitente.cep-cob                             = IF tt-emitente.cep_cob <> ? THEN UPPER(tt-emitente.cep_cob) ELSE ""
                   emitente.nome-mic-reg                        = IF AVAIL mgdis.cidade THEN mgdis.cidade.nome-mic-reg ELSE "".

            IF tt-emitente.tipo-movimento <> "Incluir" THEN
            DO:
               ASSIGN emitente.identific = 3.
               IF emitente.cod-gr-cli = 0 THEN
                  ASSIGN emitente.cod-gr-cli = IF AVAIL es-api-param-cliente THEN (es-api-param-cliente.cod-gr-cli) ELSE 10.
               IF AVAIL es-api-param-cliente THEN 
                   ASSIGN emitente.portador    = es-api-param-cliente.portador   
                          emitente.modalidade  = es-api-param-cliente.modalidade
                          emitente.port-prefer = es-api-param-cliente.port-pref 
                          emitente.mod-prefer  = es-api-param-cliente.mod-prefer.      
               ASSIGN emitente.portador-ap   = i-portador-ap
                      emitente.modalidade-ap = i-modalidade-ap
                      /* Solicitado por Cicero em 25/07/2019 */
                      emitente.data-implant  = TODAY.
            END.
            /* 25/07/2019 */
            IF emitente.bairro-cob = "" OR emitente.bairro-cob = ? THEN
               ASSIGN emitente.bairro-cob = emitente.bairro.
            IF emitente.cep-cob = ""    OR emitente.cep-cob = ? THEN
               ASSIGN emitente.cep-cob = emitente.cep.
            IF emitente.cidade-cob = "" OR emitente.cidade-cob = ? THEN
               ASSIGN emitente.cidade-cob = emitente.cidade.
            IF emitente.endereco-cob = "" OR emitente.endereco-cob = ? THEN
               ASSIGN emitente.endereco-cob = emitente.endereco.
            IF emitente.estado-cob = "" OR emitente.estado-cob = ? THEN
               ASSIGN emitente.estado-cob = emitente.estado.
            IF emitente.pais-cob   = "" OR emitente.pais-cob   = ? THEN
               ASSIGN emitente.pais-cob   = emitente.pais.
            IF emitente.zip-cob-code = "" OR emitente.zip-cob-code = ? THEN
               ASSIGN emitente.zip-cob-code = emitente.zip-code.

            find first loc-entr use-index ch-entrega EXCLUSIVE-LOCK
                where loc-entr.nome-abrev  = emitente.nome-abrev
                and   loc-entr.cod-entrega = emitente.cod-entrega no-error.

            IF AVAIL loc-entr THEN DO:

                ASSIGN loc-entr.nom-cidad-cif = nm_cidade_cif.

                FIND FIRST transporte NO-LOCK WHERE transporte.cod-transp = integer(tt-emitente.cod_transportadora) NO-ERROR.
                IF AVAIL transporte THEN DO:
                    ASSIGN loc-entr.nome-transp = transporte.nome-abrev.

                END.
            END.

            FOR FIRST UNID-FEDER NO-LOCK WHERE
                      UNID-FEDER.estado = EMITENTE.estado-cob.
                ASSIGN emitente.pais-cob = unid-feder.pais.
            END.

            /*faturamento parcial*/
            ASSIGN emitente.ind-fat-par                             = IF LOGICAL(tt-emitente.ck_faturamento_parcial) THEN YES ELSE NO
                   emitente.esp-pd-venda                            = decimal(tt-emitente.sl_esp_padrao_ped_venda)
                   emitente.atividade                               = tt-emitente.nm_ramo_atividade
                   emitente.cod-entrega                             = "Padr∆o" /* 24/11/2015-Claudio-DCS, tt-emitente.nm_entrega_padrao */
                   .

            IF tt-emitente.nome_cliente = "" THEN
                ASSIGN emitente.nome-matriz      = tt-emitente.nome_abreviado.
            ELSE
                ASSIGN emitente.nome-matriz      = tt-emitente.nome_cliente.
            IF c-nome-matriz-rede <> "" THEN
                ASSIGN emitente.nome-matriz      = c-nome-matriz-rede. 

            find first dist-emitente
                where dist-emitente.cod-emitente = emitente.cod-emitente exclusive-lock no-error.

            /*Inicio Rafael 22/07/2015*/
            /*Cadastro de campos especificos*/
/*             FOR FIRST tt-emitente-esp: */

            /*
                ASSIGN tt-emitente.cod-emitente = STRING(emitente.cod-emitente)
                       tt-emitente.cidade       = emitente.cidade
                       tt-emitente.estado       = emitente.estado
                       tt-emitente.pais         = emitente.pais.
                       */

                /*Logica para salvar os campos da tabela emitente*/
                ASSIGN emitente.cod-rep     = INT(tt-emitente.cod-rep)
                       emitente.ind-cre-cli = INT(tt-emitente.ind-cre-cli)
                       emitente.lim-credito = DEC(tt-emitente.lim-credito)
                       emitente.dt-lim-cred = DATE(tt-emitente.dt-lim-cred).

                MESSAGE ">>> gravou ind-cre-cli: " emitente.ind-cre-cli.

                IF integer(tt-emitente.cod-suframa) > 0 THEN
                    ASSIGN emitente.cod-suframa       = tt-emitente.cod-suframa
                           emitente.dat-valid-suframa = date(tt-emitente.dat_valid_suframa).

                /*Fim logica para salvar os campos da tabela emitente*/

                /*Logica para prencher campos es-emitente.exige-laudo-cq, es-emitente.exige-loteu, es-emitente.rejeita-prox-ven, es-emitente.regime-trib*/
                FIND FIRST es-emitente EXCLUSIVE-LOCK
                    WHERE es-emitente.cod-emitente = emitente.cod-emitente NO-ERROR.

                IF NOT AVAIL es-emitente THEN DO:

                    CREATE es-emitente.
                    ASSIGN es-emitente.cod-emitente = emitente.cod-emitente .
                END.

/*                 ASSIGN es-emitente.exige-laudo-cq   = LOGICAL(tt-emitente.exige-laudo) 
                          es-emitente.exige-loteu      = LOGICAL(tt-emitente.exige-loteu)*/
                   ASSIGN es-emitente.exige-laudo-cq   = LOGICAL(tt-emitente.exige-laudo).
                   IF tt-emitente.exige-loteu BEGINS "N" THEN
                      ASSIGN es-emitente.exige-loteu      = NO.
                   ELSE 
                      ASSIGN es-emitente.exige-loteu      = YES.
                   IF tt-emitente.rejeita-prox-ven BEGINS "N" THEN 
                      ASSIGN es-emitente.rejeita-prox-ven = NO.
                   ELSE 
                       ASSIGN es-emitente.rejeita-prox-ven = YES.
                   /*    es-emitente.rejeita-prox-ven = LOGICAL(tt-emitente.rejeita-prox-ven) */
                    ASSIGN es-emitente.regime-trib      = tt-emitente.regime-trib.


                RELEASE es-emitente.
                /*Fim logica para prencher campos es-emitente.exige-laudo-cq, es-emitente.exige-loteu, es-emitente.rejeita-prox-ven, es-emitente.regime-trib*/

                /* Begins: 24/06/2017 Inclusao de Campos - Willians Ambrosio - DSC Praxis */
                ASSIGN emitente.agente-retencao              = logical(tt-emitente.agente-retencao            ) 
                       emitente.contrib-icms                 = logical(tt-emitente.contrib-icms               ) 
                       emitente.log-calcula-pis-cofins-unid  = logical(tt-emitente.log-calcula-pis-cofins-unid). 
                IF emitente.log-calcula-pis-cofins-unid  = ?
                THEN ASSIGN
                   emitente.log-calcula-pis-cofins-unid  = YES.
                
                /* End: 24/06/2017 Inclusao de Campos - Willians Ambrosio - DSC Praxis */
                
                /* 24/07/2019 */
                IF emitente.ins-est-cob = "" THEN
                   ASSIGN emitente.ins-est-cob = emitente.ins-estadual.

                /*Logica para prencher campos es-gp-emit-coord.latitude e es-gp-emit-coord.longitude*/
                FIND FIRST es-gp-emit-coord EXCLUSIVE-LOCK
                    WHERE es-gp-emit-coord.cod-emitente = INT(tt-emitente.cod-emitente) NO-ERROR.

                IF NOT AVAIL es-gp-emit-coord THEN DO:
                    CREATE es-gp-emit-coord.
                    ASSIGN es-gp-emit-coord.cod-emitente = INT(tt-emitente.cod-emitente).
                END.

                ASSIGN es-gp-emit-coord.latitude  = DEC(tt-emitente.latitude)
                       es-gp-emit-coord.longitude = DEC(tt-emitente.longitude).

                RELEASE es-gp-emit-coord.
                /*Fim logica para prencher campos es-gp-emit-coord.latitude e es-gp-emit-coord.longitude*/

                /*Logica para prencher campos es-gp-emit-atend.cod-atend e es-gp-emit-atend.cod-quem*/
                FIND FIRST es-gp-emit-atend EXCLUSIVE-LOCK
                    WHERE es-gp-emit-atend.cod-emitente = INT(tt-emitente.cod-emitente) NO-ERROR.

                IF NOT AVAIL es-gp-emit-atend THEN DO:
                    CREATE es-gp-emit-atend.
                    ASSIGN es-gp-emit-atend.cod-emitente = INT(tt-emitente.cod-emitente).
                END.

                ASSIGN es-gp-emit-atend.cod-atend = tt-emitente.cod-atend
                       es-gp-emit-atend.cod-quem  = tt-emitente.cod-quem.

                RELEASE es-gp-emit-atend.
                /*Fim logica para prencher campos es-gp-emit-atend.cod-atend e es-gp-emit-atend.cod-quem*/

                /*
                /*Logica para salvar o endereco da visita*/
                IF LOGICAL(tt-emitente.cadastra-endereco) THEN DO:

                    IF NOT CAN-FIND(FIRST loc-entr WHERE loc-entr.nome-abrev = tt-emitente.nome_abreviado
                                                     AND loc-entr.cod-entrega = "Visita" ) THEN DO:
                        CREATE loc-entr.
                        ASSIGN loc-entr.nome-abrev    = emitente.nome-abrev
                               loc-entr.cod-entrega   = "Visita"
                               loc-entr.endereco      = tt-emitente.endereco
                               loc-entr.bairro        = tt-emitente.bairro
                               loc-entr.cidade        = tt-emitente.cidade
                               loc-entr.estado        = tt-emitente.estado
                               loc-entr.pais          = tt-emitente.pais
                               loc-entr.cep           = tt-emitente.cep
                               loc-entr.endereco_text = tt-emitente.endereco_text.
    
                        RELEASE loc-entr.
                    END.
                END.
                */

                /*Fim logica para salvar o endereco da visita*/

                /*Logica para salvar o cod-canal-venda2*/
                FIND FIRST es-gp-emit-canal EXCLUSIVE-LOCK
                    WHERE es-gp-emit-canal.cod-emitente = emitente.cod-emitente NO-ERROR.

                IF NOT AVAIL es-gp-emit-canal THEN DO:
                    CREATE es-gp-emit-canal.
                    ASSIGN es-gp-emit-canal.cod-emitente = emitente.cod-emitente.
                END.

                ASSIGN es-gp-emit-canal.cod-canal-venda2 = INT(tt-emitente.cod-canal-venda2).
                /*Logica para salvar o cod-canal-venda2*/

                FOR EACH tt-erro:
                    CREATE tt-erros-integracao.
                    ASSIGN tt-erros-integracao.erro      = tt-erro.cod-erro
                           tt-erros-integracao.descricao = tt-erro.des-erro.                    
                END.
/*             END. */


            /*Tiago - 20151029 - Atualizaùˇo de contato do emitente + atualizaùˇo EMS5*/
            ASSIGN i-int1 = 0.
            FOR EACH tt_ContatoList 
                /* 27/04/2019 */
                WHERE  tt_ContatoList.fld-rel <> 0:
               FIND FIRST cont-emit
                    WHERE cont-emit.cod-emitente = emitente.cod-emitente
                      AND cont-emit.sequencia    = tt_ContatoList.fld-rel
                    NO-ERROR.
               IF NOT AVAIL cont-emit
               THEN DO:
                  CREATE cont-emit.
                  ASSIGN
                      cont-emit.cod-emitente = emitente.cod-emitente  
                      cont-emit.sequencia    = tt_ContatoList.fld-rel.
               END.
               ASSIGN i-int1 = i-int1 + 1.

               ASSIGN
                   cont-emit.identific = emitente.identific
                   cont-emit.nome      = tt_ContatoList.nome
                   cont-emit.area      = tt_ContatoList.areacontato
                   cont-emit.cargo     = tt_ContatoList.cargo
                   cont-emit.e-mail    = tt_ContatoList.email
                   cont-emit.ramal     = tt_ContatoList.ramal
                   cont-emit.ramal-fax = tt_ContatoList.ramalfax
                   cont-emit.telefax   = tt_ContatoList.fax
                   cont-emit.telefone  = tt_ContatoList.telefone
                   cont-emit.int-1     = i-int1.
               /* 25/07/2019 */
               IF tt_ContatoList.email = ""    OR tt_ContatoList.email = ? THEN 
                  ASSIGN cont-emit.e-mail   = IF tt-emitente.email_financ <> ? THEN tt-emitente.email_financ ELSE "".
               IF tt_ContatoList.telefone = "" OR tt_ContatoList.telefone = ? THEN
                  ASSIGN cont-emit.telefone = tt-emitente.telefone.
               IF tt_ContatoList.nome = ""     OR tt_ContatoList.nome = ? THEN
                  ASSIGN cont-emit.nome     = "CONTATO".
            END.
            
            /* Cria contato NF-e */
            find first cont-emit exclusive-lock
                 where cont-emit.cod-emitente = emitente.cod-emitente
                 and   cont-emit.sequencia    = 98 no-error.

            if not avail cont-emit then
            do:
               ASSIGN i-int1 = i-int1 + 1.
               create cont-emit.
               assign cont-emit.cod-emitente = emitente.cod-emitente
                      cont-emit.sequencia    = 98
                      cont-emit.identific    = emitente.identific
                      cont-emit.nome         = "DESTINATARIO XML NF-e"
                      cont-emit.telefone     = tt-emitente.nr_celular
                      cont-emit.e-mail       = IF tt-emitente.email_nfe <> ? THEN tt-emitente.email_nfe ELSE ""
                      cont-emit.int-1        = i-int1.
            end.

            /* Cria contato Financeiro */
            find first cont-emit exclusive-lock
                 where cont-emit.cod-emitente = emitente.cod-emitente
                 and   cont-emit.sequencia    = 99 no-error.

            if not avail cont-emit then do:

                ASSIGN i-int1 = i-int1 + 1.
                create cont-emit.
                assign cont-emit.cod-emitente = emitente.cod-emitente
                       cont-emit.sequencia    = 99
                       cont-emit.identific    = emitente.identific 
                       cont-emit.nome         = "CONTATO FINANCEIRO"
                       cont-emit.telefone     = tt-emitente.nr_telefone_finan
                       cont-emit.e-mail       = IF tt-emitente.email_financ <> ? THEN tt-emitente.email_financ ELSE ""
                       cont-emit.int-1        = i-int1.
            end.

            FIND CURRENT emitente NO-ERROR.
           
            if  can-find(funcao where funcao.cd-funcao = "adm-cdc-ems-5.00"
                and funcao.ativo = yes
                and funcao.log-1 = yes) then do:
                find first param-global NO-LOCK no-error.
                if  param-global.log-2 = yes THEN DO:
                    validate emitente no-error.
                    run cdp/cd1608.p (input emitente.cod-emitente,
                                      input emitente.cod-emitente,
                                      input emitente.identific,
                                      input yes,
                                      input 1,
                                      input 0,
                                      input c-filename, /*"\\192.168.51.98\d$\Especificos\bravaecm\teste.tmp",*/
                                      input "Arquivo":U,
                                      input "") NO-ERROR.  

                END.
            end.           
            

            /*FIM******* Tiago - 20151029 - Atualizaùˇo de contato do emitente + atualizacao EMS5*/
        END.
    END.


    /*Tiago - DSC - Replicar canal de vendas 2 - 07/01/2016*/
    FIND FIRST es-gp-emit-canal WHERE
               es-gp-emit-canal.cod-emitente = pCodEmitenteReturn EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL es-gp-emit-canal THEN DO:
        CREATE es-gp-emit-canal.
        ASSIGN es-gp-emit-canal.cod-emitente = pCodEmitenteReturn.
    END.

    IF tt-emitente.cod_canal_venda = "" THEN
       ASSIGN tt-emitente.cod_canal_venda = "0".

    ASSIGN es-gp-emit-canal.cod-canal-venda2  = INT(tt-emitente.cod_canal_venda).

    /*FIM Tiago - DSC - Replicar canal de vendas 2 - 07/01/2016*/

END.


FUNCTION fnc-proximo-emit RETURNS INTEGER():
/*------------------------------------------------------------------------------
  Purpose: Retorna o numero do proximo emitente a ser cadastrado
------------------------------------------------------------------------------*/

  DEF VAR i-emitente AS INT NO-UNDO.

  run cdp/cd9960.p (OUTPUT i-emitente).

  RETURN i-emitente.   /* Function return value. */


END FUNCTION.

/* ------------------------------------------------ */
RETURN "OK".


PROCEDURE piImportParam:
                
    FIND FIRST tt_emitente NO-LOCK NO-ERROR.
    FIND FIRST tt_CondicaoPagamentoList  NO-LOCK NO-ERROR.
    FIND FIRST es-api-param-cliente NO-LOCK NO-ERROR.
    FIND FIRST portador WHERE portador.cod-portador = es-api-param-cliente.portador NO-LOCK NO-ERROR.
    FIND FIRST transporte WHERE transporte.cod-transp = es-api-param-cliente.cod-transp NO-LOCK NO-ERROR.
    CREATE tt-emitente.  
    IF v_tipo_movto = "Incluir"  THEN
        ASSIGN tt-emitente.identific                      = "1".
    ELSE 
        ASSIGN tt-emitente.identific                      = "2".

    ASSIGN tt-emitente.tipo-movimento                 = v_tipo_movto 
           tt-emitente.nome_abreviado                 = substring(replace(replace(tt_emitente.CNPJ,"-",""),"/",""),1,12)
           tt-emitente.Nome_emit                      = tt_emitente.RazaoSocial  
           tt-emitente.cod_grupo                      = IF AVAIL es-api-param-cliente THEN string(es-api-param-cliente.cod-gr-cli) ELSE "0".
           
    ASSIGN /*tt-emitente.descricao                      = cFieldsTT[006]        */    
           tt-emitente.cod_cliente                    = STRING(tt_emitente.CodigoCliente)   
           tt-emitente.nome_cliente                   = tt-emitente.nome_abreviado
           tt-emitente.cod_representante              = string(tt_emitente.Representante)
           tt-emitente.cond_pag                       = STRING(tt_CondicaoPagamentoList.codigoCondicao) 
           tt-emitente.nome_pag                       = tt_CondicaoPagamentoList.descricaoCondicao
           tt-emitente.endereco                       = tt_emitente.LogradouroEntrega.

    ASSIGN tt-emitente.cidade                         = tt_emitente.CidadeEntrega
           tt-emitente.estado                         = tt_emitente.EstadoEntrega
           tt-emitente.pais                           = tt_emitente.PaisEntrega
           tt-emitente.cod_pais                       = ""
           tt-emitente.nome_pais                      = tt-emitente.pais
           tt-emitente.bairro                         = tt_emitente.BairroEntrega
           tt-emitente.Cep                            = tt_emitente.CepEntrega    
           tt-emitente.nome_estado                    = ""
           tt-emitente.cod_estado                     = tt-emitente.estado
           tt-emitente.nome_cidade                    = tt-emitente.cidade
           tt-emitente.cod_cidade                     = ""
           tt-emitente.cod_id_cliente_cobranca        = ""
           tt-emitente.id_cliente_cobranca            = "".


    ASSIGN tt-emitente.endereco_cob                   = tt_emitente.LogradouroCobranca  
           tt-emitente.Pais_cob                       = tt_emitente.PaisCobranca.
      
    ASSIGN  tt-emitente.bairro_cob                     = tt_emitente.BairroCobranca
            tt-emitente.Cep_cob                        = tt_emitente.CepCobranca
            tt-emitente.Estado_cob                     = tt_emitente.EstadoCobranca 
            tt-emitente.nome_pais_cob                  = "".
           
    ASSIGN tt-emitente.Cidade_cob                     = tt_emitente.CidadeCobranca
           tt-emitente.nome_cidade2                   = tt_emitente.CidadeCobranca 
           tt-emitente.Telefone                       = tt_emitente.Telefone
           tt-emitente.nome_estado_cob                = "".

    ASSIGN tt-emitente.Ramal                          = ""   
           tt-emitente.Telefone2                      = ""   
           tt-emitente.Ramal2                         = ""   
           tt-emitente.Telefax                        = ""   
           tt-emitente.Ramal_fax                      = ""   
           tt-emitente.Telefax2                       = ""   
           tt-emitente.Ramal_fax2                     = ""   
           tt-emitente.Ramal_modem                    = ""  .

   ASSIGN  tt-emitente.email                          = IF tt_emitente.Email <> ? THEN tt_emitente.Email ELSE ""
           tt-emitente.nat_cliente                    = ""
           tt-emitente.cpfcnpj                        = tt_emitente.CNPJ                
           tt-emitente.Ins_estadual                   = tt_emitente.IE
           tt-emitente.Ins_municipal                  = ""
           tt-emitente.cpfcnpjCobranca                = tt_emitente.CNPJCobranca
           tt-emitente.Ins_est_cob                    = tt_emitente.IE
           tt-emitente.cod_canal_venda                = string(tt_emitente.TipoClienteCanal).

    ASSIGN tt-emitente.canal_venda                    = ""
           tt-emitente.libera_venda_sem_bonif         = ""  
           tt-emitente.natureza_operacao              = tt_emitente.NaturezaOperacao
           tt-emitente.natureza_interestadual         = IF AVAIL es-api-param-cliente THEN es-api-param-cliente.nat-ope-ext ELSE ""   .

    ASSIGN tt-emitente.suspensao_ipi                  = "NO"
           tt-emitente.nfe                            = IF AVAIL es-api-param-cliente THEN string(es-api-param-cliente.log-nf-eletro) ELSE ""
           tt-emitente.portador                       = string(es-api-param-cliente.portador  )
           tt-emitente.portador_selecionado           = '1'
           tt-emitente.nome_portador                  = IF AVAIL portador THEN portador.nome ELSE ""  
           tt-emitente.Modalidade                     = string(es-api-param-cliente.modalidade)
           tt-emitente.portador_pref                  = string(es-api-param-cliente.port-pref )                  
           tt-emitente.portador_pref_selecionado      = '1'                    
           tt-emitente.modalidade_pref                = string(es-api-param-cliente.mod-prefer)                    
           tt-emitente.Mod_prefer                     = string(es-api-param-cliente.mod-prefer)                                       
           tt-emitente.cod_receita                    = '10'    .
IF tt-emitente.nfe = ? THEN
        ASSIGN tt-emitente.nfe = "NO".


ASSIGN  tt-emitente.nome_receita                   = ""
        tt-emitente.emit_boleto                    = "NO"
        tt-emitente.regiao                         = ""
        tt-emitente.cod_transportadora             = string(es-api-param-cliente.cod-transp)
        tt-emitente.nome_transportadora            = IF AVAIL transporte THEN transporte.nome    ELSE ""
        tt-emitente.dt_implantacao                 = ""
        tt-emitente.nr_cgcmf                       = ""
        tt-emitente.cd_cidade_cif                  = ""
        tt-emitente.nm_cidade_cif                  = tt-emitente.cidade
        tt-emitente.cd_modalidade_frete            = ""
        tt-emitente.nm_modalidade_frete            = ""
        tt-emitente.ck_faturamento_parcial         = IF AVAIL es-api-param-cliente THEN string(es-api-param-cliente.ind-fat-par) ELSE ""    
        tt-emitente.sl_esp_padrao_ped_venda        = IF AVAIL es-api-param-cliente THEN string(es-api-param-cliente.esp-pd-venda) ELSE ""
        tt-emitente.nm_ramo_atividade              = tt_emitente.RamoAtividade    .
  ASSIGN   tt-emitente.cd_entrega_padrao              = ""
           tt-emitente.nm_entrega_padrao              = ""
           tt-emitente.cd_sequencia_contato_emitente  = ""
           tt-emitente.nr_cpf_cnpj_contato_emitente   = ""
           tt-emitente.sl_aplicacao_contato_emitente  = "1"
           tt-emitente.cod-emitente                   = ""  
           tt-emitente.exige-laudo                    = string(tt_emitente.ExigeCertifAnalise     )
           tt-emitente.exige-loteu                    = string(tt_emitente.ClienteExigeLoteUnico  )
           tt-emitente.rejeita-prox-ven               = string(tt_emitente.NaoRecebeLoteProxVencto)
           tt-emitente.regime-trib                    = string(tt_emitente.RegimeTributario       )
           tt-emitente.cod-canal-venda2               = string(tt_emitente.CanalCliente           )  .

  ASSIGN 
           tt-emitente.latitude                       = "0"                          
           tt-emitente.longitude                      = "0"
           tt-emitente.cod-atend                      = "0"   
           tt-emitente.cod-quem                       = "0"  . 
           
  ASSIGN
           tt-emitente.endereco_text                  = ""   
           tt-emitente.cadastra-endereco              = "YES"   
           tt-emitente.cod-rep                        = STRING(tt_emitente.Representante)
           tt-emitente.nr_telefone_contato_emitente   = "" 
           tt-emitente.nm_internet_email              = "" 
           tt-emitente.nr_inscricao_estadual          = "" 
           tt-emitente.nm_contato_emitente            = "" 
           tt-emitente.nm_area_contato_emitente       = "" 
           tt-emitente.nm_cargo_contato_emitente      = "" 
           tt-emitente.nr_ramal_contato_emitente      = ""               
           tt-emitente.nr_fax_contato_emitente        = "" 
           tt-emitente.nr_ramal_fax_contato_emitente  = "" 
           tt-emitente.nm_email_contato_emitente      = "" 
           tt-emitente.nr_telefone_finan              = tt_emitente.TelefoneFinanceiro    
           tt-emitente.nr_celular                     = "" 
           tt-emitente.nr_telefone_fixo               = "" 
           tt-emitente.email_financ                   = IF tt_emitente.EmailFinanceiro <> ? THEN tt_emitente.EmailFinanceiro ELSE ""
           tt-emitente.email_nfe                      = IF tt_emitente.EmailXML <> ? THEN  tt_emitente.EmailXML ELSE "" .


    MESSAGE "gravou antes tipo credito: " tt_emitente.TipoCredito.

    ASSIGN tt-emitente.email_com                      = "" 
           tt-emitente.agente-retencao                = IF AVAIL es-api-param-cliente THEN string(es-api-param-cliente.agente-retencao) ELSE ""
           tt-emitente.contrib-icms                   = STRING(tt_emitente.ContribuinteICMS)   
           tt-emitente.log-calcula-pis-cofins-unid    = IF AVAIL es-api-param-cliente THEN STRING(es-api-param-cliente.log-calcula-pis-cofins-unid) ELSE "YES"
           tt-emitente.lim-credito                    = string(tt_emitente.LimiteCredito    )
           tt-emitente.ind-aval                       = string(tt_emitente.AvaliacaoCredito )
           tt-emitente.ind-cre-cli                    = string(tt_emitente.TipoCredito      )
           tt-emitente.dt-lim-credito                 = string(tt_emitente.DataLimiteCredito).


    IF integer(tt_emitente.Suframa) > 0 THEN 
        ASSIGN tt-emitente.cod-suframa                    = string(tt_emitente.Suframa)
               tt-emitente.dat_valid_suframa              = string(tt_emitente.DataValidadeSuframa).


    ASSIGN tt-emitente.cod-banco                      = tt_emitente.Banco
           tt-emitente.agencia                        = tt_emitente.Agencia
           tt-emitente.conta-corren                   = tt_emitente.Conta
           tt-emitente.nat_cliente                    = tt_emitente.NaturezaCliente
           tt-emitente.nome_mic_reg                   = tt_emitente.Microrregiao
           tt-emitente.end-complemento                = tt_emitente.ComplementoEntrega
           tt-emitente.end-completo-cob               = tt_emitente.ComplementoCobranca.   


           IF tt-emitente.log-calcula-pis-cofins-unid = ? THEN ASSIGN tt-emitente.log-calcula-pis-cofins-unid = "yes".
    
           ASSIGN tt-emitente.exige-laudo      = IF tt-emitente.exige-laudo      = "" THEN "NO" ELSE tt-emitente.exige-laudo     . 
           ASSIGN tt-emitente.exige-loteu      = IF tt-emitente.exige-loteu      = "" THEN "NO" ELSE tt-emitente.exige-loteu     . 
           ASSIGN tt-emitente.rejeita-prox-ven = IF tt-emitente.rejeita-prox-ven = "" THEN "NO" ELSE tt-emitente.rejeita-prox-ven. 
           ASSIGN tt-emitente.lim-credito      = IF tt-emitente.lim-credito      = ?  THEN "0" ELSE tt-emitente.lim-credito.

    ASSIGN i-instr-banc = es-api-param-cliente.ins-banc WHEN AVAILABLE es-api-param-cliente.
    /* Solicitado em 24/07/2019  IF v_tipo_movto = "Incluir" THEN  */
       RUN pi-grandes-red.  

    IF i-portador-rede <> 0 THEN
    DO:
       FIND FIRST portador WHERE portador.cod-portador = i-portador-rede
                           NO-LOCK NO-ERROR.
       ASSIGN tt-emitente.nome_portador             = IF AVAILABLE portador THEN portador.nome      ELSE ""
              /* tt_emitente_integr_old_2.cod_portador = i-portador-rede   */
              tt-emitente.portador                  = STRING(i-portador-rede).
    END. /* i-portador-rede <> 0 */
    IF i-modalidade-rede <> 0 THEN
       ASSIGN tt-emitente.Modalidade                = STRING(i-modalidade-rede).
    IF i-port-prefer-rede <> 0 THEN
       ASSIGN tt-emitente.portador_pref             = STRING(i-port-prefer-rede). 
    IF i-mod-prefer-rede <> 0 THEN
       ASSIGN tt-emitente.modalidade_pref           = STRING(i-mod-prefer-rede)
              tt-emitente.Mod_prefer                = STRING(i-mod-prefer-rede).
/*     IF c-nome-matriz-rede <> "" THEN
       ASSIGN tt_emitente_integr_old_2.nome_matriz  = c-nome-matriz-rede. */
    IF c-nome-abrev-rede <> "" THEN
       ASSIGN tt-emitente.nome_abreviado            = c-nome-abrev-rede.
/*               tt_emitente_integr_old_2.nome_abrev   = c-nome-abrev-rede. */
    IF i-cod-gr-cli-rede <> 0 THEN
       ASSIGN tt-emitente.cod_grupo                 = STRING(i-cod-gr-cli-rede).

    ASSIGN tt-emitente.cd_instrucao_bancaria          = string(i-instr-banc)
           tt-emitente.nm_instrucao_bancaria          = "".

END PROCEDURE.

/* Procedure para alterar os campos de nome abreviado, matriz, portador e modalidade para clientes de grandes redes
Informacao obtida atravÇs dos codigos de grupos de clientes de exceá∆o da tabela es-api-param-cliente */
PROCEDURE pi-grandes-red:
   DEFINE BUFFER b1-emitente         FOR  emitente.
   DEFINE BUFFER b2-emitente         FOR  emitente.
   DEFINE BUFFER b3-emitente         FOR  emitente.
   DEFINE BUFFER b4-emitente         FOR  emitente.

   DEFINE VARIABLE c-nome-abrev-raiz LIKE emitente.nome-abrev.
   DEFINE VARIABLE i-sequencia       AS INTEGER.
   DEFINE VARIABLE i-cont            AS INTEGER.
   DEFINE VARIABLE c-sequencia       AS CHARACTER.

   ASSIGN c-nome-abrev-rede  = ""
          c-nome-abrev-raiz  = ""
          c-nome-matriz-rede = ""
          i-modalidade-rede  = 0 
          i-portador-rede    = 0 
          i-mod-prefer-rede  = 0 
          i-port-prefer-rede = 0 
          i-cod-gr-cli-rede  = 0 
          c-sequencia        = ""
          i-sequencia        = 0 
          i-cont             = 0.

   FIND FIRST es-api-param-cliente NO-LOCK NO-ERROR.
   FIND FIRST b1-emitente WHERE SUBSTRING(b1-emitente.cgc, 01, 08) =  SUBSTRING(tt_emitente.cnpj, 01,08)
                          AND   b1-emitente.cgc                    <> tt_emitente.cnpj
                          NO-LOCK NO-ERROR.
   /* emitente integrado faz parte de uma grande rede */
   IF AVAILABLE b1-emitente THEN
   DO:
      
      IF AVAILABLE es-api-param-cliente THEN
      DO:
         IF LOOKUP(STRING(b1-emitente.cod-gr-cli), es-api-param-cliente.cod-gr-c-e) = 0 THEN 
         DO:
             ASSIGN i-instr-banc = 0.

            /* Localizando o ultimo emitente pertencente a rede - alteraá∆o p¢s solicitaá∆o de novo tipo de composiá∆o do nome abreviado - chamado aberto pelo Cicero 
            FIND LAST b3-emitente WHERE SUBSTRING(b3-emitente.cgc, 1, 8) =  SUBSTRING(b1-emitente.cgc, 1, 8)
                                  AND   b3-emitente.nome-abrev           <> b3-emitente.nome-matriz
                                  AND   LOOKUP(SUBSTRING(b3-emitente.nome-abrev,12,1), "0,1,2,3,4,5,6,7,8,9") <> 0
                                  AND   LOOKUP(SUBSTRING(b3-emitente.nome-abrev,11,1), "0,1,2,3,4,5,6,7,8,9") <> 0
                                  USE-INDEX nome 
                                  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE b3-emitente THEN
               ASSIGN i-sequencia = 1.
            IF AVAILABLE b3-emitente THEN
            DO:
               ASSIGN i-sequencia = 0 
                      c-sequencia = "".
               DO i-cont = 1 TO 12:
                  IF LOOKUP(SUBSTRING(b3-emitente.nome-abrev, i-cont, 01), "0,1,2,3,4,5,6,7,8,9") <> 0 THEN 
                     ASSIGN c-sequencia = c-sequencia + SUBSTRING(b3-emitente.nome-abrev, i-cont, 01).
               END. /* i-cont = 1 TO 12 */
               IF i-sequencia = 0 THEN
                  ASSIGN i-sequencia = 1.
               ELSE 
                  ASSIGN i-sequencia = DEC(c-sequencia).
            END. /* AVAILABLE b3-emitente */    */
            ASSIGN i-sequencia = INTEGER(SUBSTRING(tt_emitente.cnpj,09,04)). 
            /* Localizando a matriz */
            FIND b2-emitente WHERE b2-emitente.nome-abrev = b1-emitente.nome-matriz
                             AND   b2-emitente.nome-abrev = b2-emitente.nome-matriz
                             NO-LOCK NO-ERROR.
            IF AVAILABLE b2-emitente THEN 
            DO:
               ASSIGN i-modalidade-rede  = b2-emitente.modalidade
                      i-portador-rede    = b2-emitente.portador
                      i-mod-prefer-rede  = b2-emitente.modalidade
                      i-port-prefer-rede = b2-emitente.portador
                      c-nome-matriz-rede = b2-emitente.nome-matriz
                      i-cod-gr-cli-rede  = b2-emitente.cod-gr-cli.
               IF LENGTH(b2-emitente.nome-abrev) <= 3 THEN
                  ASSIGN c-nome-abrev-raiz = b2-emitente.nome-abrev + "-" + b2-emitente.nome-abrev + "-".
               IF LENGTH(b2-emitente.nome-abrev) > 3  AND 
                  LENGTH(b2-emitente.nome-abrev) < 10 THEN
                   ASSIGN c-nome-abrev-raiz = b2-emitente.nome-abrev + "-".
               IF LENGTH(b2-emitente.nome-abrev) >= 10 THEN
                   ASSIGN c-nome-abrev-raiz = SUBSTRING(b2-emitente.nome-abrev,01,09) + "-".

            END. /* AVAILABLE b2-emitente - Matriz */
            
         /*   MESSAGE "c-nome-matriz-rede" c-nome-matriz-rede. */
            bl-ver-reg:
            REPEAT:
               ASSIGN c-nome-abrev-rede = c-nome-abrev-raiz + STRING(i-sequencia, "9999").
               FIND FIRST b4-emitente WHERE b4-emitente.nome-abrev = c-nome-abrev-rede
                                      NO-LOCK NO-ERROR.       
               IF NOT AVAILABLE b4-emitente THEN LEAVE bl-ver-reg.
               IF AVAILABLE b4-emitente AND b4-emitente.cgc = tt_emitente.cnpj THEN
                   LEAVE bl-ver-reg.
               ASSIGN i-sequencia       = i-sequencia + 1.
            END. /* bl-ver-reg */
         END. /* b1-emitente.cod-gr-cli indica se Ç grande rede ou n∆o */
      END. /* AVAILABLE es-api-param-cliente */
   END. /* AVAILABLE b1-emitente */
END PROCEDURE.
