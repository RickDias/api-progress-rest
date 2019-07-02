/*----------------------------------------------------------------------------------------------/
 Programa..: esint020aif.p
 Objetivo..: Interface Integra‡Æo Callback Fornecedores B2E
 Data......: 27/05/2019
 Autor.....: Marcelo Brasil
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{utp\ut-glob.i}
{cdp/cdapi366b.i}

/*603833*/
DEF VAR l-debug AS LOG INIT NO NO-UNDO.

FUNCTION fnc-proximo-emit RETURNS INTEGER() FORWARD.

DEF TEMP-TABLE ttx-emitente NO-UNDO 
          LIKE emitente.

DEF BUFFER empresa     FOR mgcad.empresa.
DEF BUFFER bf-emitente FOR emitente.

//{esp/esint001aic.i}

DEF  INPUT PARAM c-acao      AS c         NO-UNDO.
DEF  INPUT PARAM rw-registro AS ROWID     NO-UNDO.
DEF OUTPUT PARAM c-erro      AS c         NO-UNDO.

DEF VAR c-cgc                AS c         NO-UNDO.
DEF VAR c-pais               AS c         NO-UNDO.

DEF VAR c-nom-estrangeiro    AS c         NO-UNDO.
DEF VAR c-bus-estrangeiro    AS c         NO-UNDO.
DEF VAR i-nom-estrangeiro    AS i         NO-UNDO.

DEF VAR c-arquivo-saida      AS CHARACTER NO-UNDO.
DEF VAR execute_evoluida_2   AS HANDLE    NO-UNDO.
DEF VAR hApi                 AS HANDLE    NO-UNDO.
DEF VAR i-oper               AS i         NO-UNDO.
DEF VAR l-ambos              AS l         NO-UNDO.
DEF VAR iNumEmit             AS i         NO-UNDO.

DEF VAR c-destino            AS c         NO-UNDO. 
DEF VAR c-filename           AS c         NO-UNDO. 

DO TRANS:

   FIND empresa NO-LOCK
        WHERE empresa.ep-codigo = i-ep-codigo-usuario 
        NO-ERROR.
   
   RUN cdp/cdapi366b.r PERSISTENT SET hApi.
   
   FIND FIRST es-fornecedor-ariba EXCLUSIVE-LOCK
        WHERE ROWID(es-fornecedor-ariba) = rw-registro
        NO-ERROR.

   IF NOT AVAIL es-fornecedor-ariba
   THEN DO:
       ASSIGN
          c-erro = "es-fonrecedor-ariba nÆo encontrado".
       RETURN "NOK".
   END.

   FIND FIRST es-ariba-b2e-param NO-LOCK
        NO-ERROR.
   
   IF NOT AVAIL es-ariba-b2e-param
   THEN DO:
       c-Erro = "Parƒmetros de integra‡Æo Ariba/B2E nÆo cadastrados".
       ASSIGN
          es-fornecedor-ariba.erro = c-erro.
       RELEASE es-fornecedor-ariba.
       RETURN "NOK".
   END.


   IF es-fornecedor-ariba.cnpj > ""
   THEN ASSIGN
      c-cgc = es-fornecedor-ariba.cnpj.
   IF es-fornecedor-ariba.cpf > ""
   THEN ASSIGN
      c-cgc = es-fornecedor-ariba.cpf.
   ASSIGN
      c-cgc = REPLACE(c-cgc,".","")
      c-cgc = REPLACE(c-cgc,"-","")
      c-cgc = REPLACE(c-cgc,"/","").
   
   IF es-fornecedor-ariba.cpf + es-fornecedor-ariba.cnpj > ""
   THEN FIND FIRST emitente NO-LOCK
        WHERE emitente.cgc          = c-cgc
/*          AND emitente.ins-estadual = es-fornecedor-ariba.ie *************/
        NO-ERROR.
   ELSE DO:
      IF es-fornecedor-ariba.cod-emitente > 0
      THEN FIND FIRST emitente NO-LOCK
           WHERE emitente.cod-emitente = es-fornecedor-ariba.cod-emitente
           NO-ERROR.
   END.

   IF l-debug THEN DO:
      MESSAGE "**** esint020aif AVAIL emitente " AVAIL emitente VIEW-AS ALERT-BOX.
      IF AVAIL emitente
      THEN MESSAGE "**** emitente " emitente.cgc emitente.cod-emitente VIEW-AS ALERT-BOX.
   END.

   IF NOT AVAIL emitente
   THEN DO ON ERROR UNDO, LEAVE:
      RUN cdp/cd9960.p (OUTPUT iNumEmit).
      IF l-debug THEN MESSAGE "iNumEmit " + STRING(iNumEmit) VIEW-AS ALERT-BOX.
      ASSIGN 
         i-oper   = 1.
   END.
   ELSE DO:
       ASSIGN
         iNumEmit = emitente.cod-emitente
         i-oper   = 2.
      IF emitente.identific = 1
      THEN ASSIGN
         l-ambos  = YES.
      
      find first dist-emitente EXCLUSIVE-LOCK
           where dist-emitente.cod-emitente = emitente.cod-emitente 
           no-error. 

      if NOT avail dist-emitente 
      then do:
         create dist-emitente.
         assign 
            dist-emitente.cod-emitente = emitente.cod-emitente.
      end.

      IF es-fornecedor-ariba.ind-inativado > 0
      THEN DO:
         ASSIGN
            dist-emitente.dat-vigenc-inicial  = TODAY
            dist-emitente.dat-vigenc-final    = 12/31/9999.

         IF es-fornecedor-ariba.ind-inativado = 1
         THEN ASSIGN
            dist-emitente.idi-sit-fornec      = 1.
         ELSE ASSIGN
            dist-emitente.idi-sit-fornec      = 2.
      END.
      ELSE DO:
         IF dist-emitente.idi-sit-fornec      > 1
         THEN ASSIGN
            dist-emitente.idi-sit-fornec      = 1
            dist-emitente.dat-vigenc-inicial  = TODAY
            dist-emitente.dat-vigenc-final    = 12/31/9999.
      END.

      FIND CURRENT disp-emitente NO-LOCK NO-ERROR.

   END.

   CREATE tt_emitente_integr_new.
   
   ASSIGN 
       tt_emitente_integr_new.num_tip_operac        = 1
       tt_emitente_integr_new.cod_emitente          = iNumEmit
       tt_emitente_integr_new.cod_versao_integracao = 1.
   
   IF i-oper = 1
   THEN ASSIGN
      tt_emitente_integr_new.identific              = 2.
   ELSE ASSIGN                                 
      tt_emitente_integr_new.identific              = integer(emitente.identific)
      tt_emitente_integr_new.cgc                    = emitente.cgc
      tt_emitente_integr_new.nome_abrev             = emitente.nome-abrev
      tt_emitente_integr_new.nome_matriz            = IF   emitente.nome-matriz = "" 
                                                        THEN emitente.nome-abrev
                                                        ELSE emitente.nome-matriz.

   ASSIGN
      tt_emitente_integr_new.nome_emit              = es-fornecedor-ariba.Corporate-Name
      .

   IF i-oper = 1
   THEN DO: 
      IF es-fornecedor-ariba.cpf > ""
      THEN ASSIGN
         tt_emitente_integr_new.natureza            = 1.
      IF es-fornecedor-ariba.cnpj > ""
      THEN ASSIGN
         tt_emitente_integr_new.natureza            = 2.

      IF tt_emitente_integr_new.natureza            = 1
      OR tt_emitente_integr_new.natureza            = 2
      THEN ASSIGN
         tt_emitente_integr_new.cgc                 = c-cgc
         tt_emitente_integr_new.nome_abrev          = SUBSTR(c-cgc,1,12)
         tt_emitente_integr_new.nome_matriz         = tt_emitente_integr_new.nome_abrev.
      ELSE DO:
         ASSIGN
            c-bus-estrangeiro = SUBSTR(es-fornecedor-ariba.corporate-name,1,INDEX(es-fornecedor-ariba.corporate-name," ") - 1)
            c-nom-estrangeiro = c-bus-estrangeiro.
         REPEAT:
             FIND FIRST bf-emitente NO-LOCK
                  WHERE bf-emitente.nome-abrev = c-nom-estrangeiro
                  NO-ERROR.
             IF NOT AVAIL bf-emitente
             THEN LEAVE.
             ASSIGN
                i-nom-estrangeiro = i-nom-estrangeiro + 1
                c-nom-estrangeiro = c-bus-estrangeiro + STRING(i-nom-estrangeiro).
         END.
         ASSIGN
            tt_emitente_integr_new.nome_abrev       = c-nom-estrangeiro
            tt_emitente_integr_new.natureza         = 3
            .
      END.
   END.
   ELSE ASSIGN
      tt_emitente_integr_new.natureza             = emitente.natureza.

   ASSIGN
      tt_emitente_integr_new.conta_corren         = es-fornecedor-ariba.conta + es-fornecedor-ariba.dig-conta-corrente
      tt_emitente_integr_new.agencia              = es-fornecedor-ariba.agencia + es-fornecedor-ariba.dig-agencia
      tt_emitente_integr_new.cod_banco            = INT(SUBSTR(es-fornecedor-ariba.banco,1,3))

      tt_emitente_integr_new.data_implant         = IF i-oper = 1 THEN TODAY ELSE emitente.data-implant
      
      tt_emitente_integr_new.ins_estadual         = es-fornecedor-ariba.ie
      tt_emitente_integr_new.ins_municipal        = IF i-oper = 1 THEN "" ELSE emitente.ins-municipal
      
      tt_emitente_integr_new.estado               = es-fornecedor-ariba.state
      tt_emitente_integr_new.endereco             = REPLACE(es-fornecedor-ariba.street,"-"," ").

    
   IF l-debug 
   THEN MESSAGE "**** nome-abrev " tt_emitente_integr_new.nome_abrev  SKIP
                " natureza " tt_emitente_integr_new.natureza          SKIP
                " operacao " ENTRY(i-oper,"cria,altera")                SKIP
                " endereco " tt_emitente_integr_new.endereco
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

   ASSIGN
      tt_emitente_integr_new.telefone[1]          = STRING(es-fornecedor-ariba.Numero-Telefone).
   
   ASSIGN
      tt_emitente_integr_new.endereco2            = es-fornecedor-ariba.complement
      tt_emitente_integr_new.bairro               = es-fornecedor-ariba.district
      tt_emitente_integr_new.cep                  = REPLACE(es-fornecedor-ariba.zip,"-","")
      tt_emitente_integr_new.Nom_cidade           = es-fornecedor-ariba.Municipality
      tt_emitente_integr_new.E_mail               = es-fornecedor-ariba.e-mail
      .

   IF tt_emitente_integr_new.natureza             = 3
   THEN DO:

      ASSIGN
         tt_emitente_integr_new.estado            = "EX".

      FIND FIRST es-pais-ariba NO-LOCK
           WHERE es-pais-ariba.cod-pais-ariba = es-fornecedor-ariba.country
           NO-ERROR.
      IF AVAIL es-pais-ariba
      THEN ASSIGN
         tt_emitente_integr_new.cod_pais          = es-pais-ariba.nome-pais
         tt_emitente_integr_new.pais              = es-pais-ariba.nome-pais
         tt_emitente_integr_new.Pais_cob          = es-pais-ariba.nome-pais.
   END.
   ELSE DO:
      ASSIGN
         tt_emitente_integr_new.cod_pais          = "Brasil"
         tt_emitente_integr_new.pais              = "Brasil"
         tt_emitente_integr_new.Pais_cob          = "Brasil".
   END.


   IF l-ambos = YES
   THEN ASSIGN
      tt_emitente_integr_new.identific            = 3.

   IF i-oper  = 1
   OR l-ambos = YES
   THEN ASSIGN
      tt_emitente_integr_new.cod_gr_cli           = es-ariba-b2e-param.cod-gr-cli 
      tt_emitente_integr_new.cod_gr_for           = es-ariba-b2e-param.cod-gr-for 
      tt_emitente_integr_new.cod_portador         = es-ariba-b2e-param.portador
      tt_emitente_integr_new.modalidade           = es-ariba-b2e-param.modalidade
      tt_emitente_integr_new.Ven_sabado           = es-ariba-b2e-param.ven-sabado
      tt_emitente_integr_new.Ven_Domingo          = es-ariba-b2e-param.ven-domingo
      tt_emitente_integr_new.Ven_feriado          = es-ariba-b2e-param.ven-feriado
      tt_emitente_integr_new.Tp_rec_padrao        = es-ariba-b2e-param.tp-rec-padrao
      tt_emitente_integr_new.Tp_desp_padrao       = es-ariba-b2e-param.tp-desp-padrao
      tt_emitente_integr_new.cod_rep              = es-ariba-b2e-param.cod-rep
                                                    
      tt_emitente_integr_new.Num_tip_operac       = 1
      .
   
   IF i-oper  = 2 THEN 
       ASSIGN tt_emitente_integr_new.cod_gr_cli           = emitente.cod-gr-cli 
              tt_emitente_integr_new.cod_gr_for           = emitente.cod-gr-for 
              tt_emitente_integr_new.cod_portador         = emitente.portador
              tt_emitente_integr_new.modalidade           = emitente.modalidade
              tt_emitente_integr_new.Ven_sabado           = emitente.Ven-sabado     
              tt_emitente_integr_new.Ven_Domingo          = emitente.Ven-Domingo    
              tt_emitente_integr_new.Ven_feriado          = emitente.Ven-feriado    
              tt_emitente_integr_new.Tp_rec_padrao        = emitente.Tp-rec-padrao  
              tt_emitente_integr_new.Tp_desp_padrao       = emitente.Tp-desp-padrao 
              tt_emitente_integr_new.cod_rep              = emitente.cod-rep
              tt_emitente_integr_new.observacoes          = emitente.observacoes.

   IF tt_emitente_integr_new.cod_portador         = 0
   THEN ASSIGN
      tt_emitente_integr_new.cod_portador         = es-ariba-b2e-param.portador.
   IF tt_emitente_integr_new.modalidade           = 0
   THEN ASSIGN
      tt_emitente_integr_new.modalidade           = es-ariba-b2e-param.modalidade.


   IF i-oper = 2 THEN 
       ASSIGN
      tt_emitente_integr_new.caixa_postal           = emitente.caixa-postal                 
      tt_emitente_integr_new.zip_code               = emitente.zip-code                     
      tt_emitente_integr_new.Endereco_cob           = emitente.endereco-cob                 
      tt_emitente_integr_new.Bairro_cob             = emitente.bairro-cob                   
      tt_emitente_integr_new.Cidade_cob             = emitente.cidade-cob                   
      tt_emitente_integr_new.Estado_cob             = emitente.estado-cob                   
      tt_emitente_integr_new.Cep_cob                = emitente.cep-cob                      
      tt_emitente_integr_new.Cgc_cob                = emitente.cgc-cob                      
      tt_emitente_integr_new.Cx_post_cob            = emitente.cx-post-cob                  
      tt_emitente_integr_new.Zip_cob_code           = emitente.zip-cob-code                 
      tt_emitente_integr_new.Ins_est_cob            = emitente.Ins-est-cob                  
      tt_emitente_integr_new.Gera_ad                = emitente.gera-ad                      
      tt_emitente_integr_new.tp_pagto               = emitente.tp-pagto                     
                                                                                              
      tt_emitente_integr_new.telefone[1]            = emitente.Telefone[1]                  
      tt_emitente_integr_new.ramal[1]               = emitente.Ramal[1]                     
      tt_emitente_integr_new.telefone[2]            = emitente.Telefone[2]                  
      tt_emitente_integr_new.ramal[2]               = emitente.Ramal[2]                     
      tt_emitente_integr_new.telefax                = emitente.Telefax                      
      tt_emitente_integr_new.ramal_fax              = emitente.Ramal-fax                    
      tt_emitente_integr_new.telex                  = emitente.Telex                        
      tt_emitente_integr_new.telef_modem            = emitente.Telef-modem                  
      tt_emitente_integr_new.ramal_modem            = emitente.Ramal-modem                  
      tt_emitente_integr_new.port_prefer            = emitente.port-prefer                  
      tt_emitente_integr_new.mod_prefer             = emitente.mod-prefer                   
      tt_emitente_integr_new.emite_bloq             = emitente.emite-bloq                   
      tt_emitente_integr_new.nome_mic_reg           = emitente.nome-mic-reg                 
      tt_emitente_integr_new.ins_banc[1]            = emitente.ins-banc[1]                  
      tt_emitente_integr_new.ins_banc[2]            = emitente.ins-banc[2]                  
      tt_emitente_integr_new.tp_rec_padrao          = integer(emitente.tp-rec-padrao)       
      tt_emitente_integr_new.forn_exp               = emitente.forn-exp                     
      tt_emitente_integr_new.agente_retencao        = emitente.agente-retencao              
      tt_emitente_integr_new.Ramo_atividade         = emitente.atividade                    
      tt_emitente_integr_new.Recebe_inf_sci         = emitente.Recebe-inf-sci               
      tt_emitente_integr_new.Vencto_dia_nao_util    = emitente.Vencto-dia-nao-util          
      tt_emitente_integr_new.Bonificacao            = emitente.Bonificacao                  
      tt_emitente_integr_new.Ind_rendiment          = emitente.Ind-rendiment                
      tt_emitente_integr_new.Dias_comp              = emitente.Dias-comp                    
      tt_emitente_integr_new.Rendto_tribut          = emitente.rend-tribut                  
      tt_emitente_integr_new.Home_page              = emitente.Home-page                    
      tt_emitente_integr_new.Utiliza_verba          = emitente.Utiliza-verba                
      tt_emitente_integr_new.Percent_verba          = emitente.Percent-verba                
      tt_emitente_integr_new.Valor_minimo           = emitente.Valor-minimo                 
      tt_emitente_integr_new.Dias_atraso            = emitente.nr-Dias-atraso               
      tt_emitente_integr_new.Calcula_multa          = emitente.Calcula-multa                
      tt_emitente_integr_new.Flag_pag               = IF emitente.Flag-pag = 2 THEN YES ELSE NO                    
      tt_emitente_integr_new.Ender_text             = emitente.endereco_text                
      tt_emitente_integr_new.Ender_cobr_text        = emitente.Endereco-cob-text            
      tt_emitente_integr_new.Log_cr_pis             = IF emitente.idi-tributac-pis = 1 THEN YES ELSE NO            
      /*tt_emitente_integr_new.cod_id_munic_fisic     = emitente.cod-id-munic-fisic    */   
      tt_emitente_integr_new.cod_id_previd_social   = emitente.cod-inscr-inss               
      /*tt_emitente_integr_new.dat_vencto_id_munic    = emitente.dat-vencto-id-munic   */   
      tt_emitente_integr_new.log_control_inss       = emitente.log-controla-val-max-inss    
      /*tt_emitente_integr_new.log_cr_cofins          = emitente.log-cr-cofins         */        /*???*/
      /*tt_emitente_integr_new.log_retenc_impto_pagto = emitente.log_retenc-impto-pagto*/   
      /*tt_emitente_integr_new.log_cooperativa        = SUBSTR(emitente.char-2,103,1) = "S" */ .  /*???*/
      /*tt_emitente_integr_new.ind_tip_fornecto       = SUBSTR(emitente.char-2,104,8)  */    /*???*/
      /*tt_emitente_integr_new.log_assoc_desportiva   = emitente.log-assoc-desportiva   */   /*???*/
      .                                                                                       
                                                        
   ASSIGN 
      tt_emitente_integr_new.ep_codigo             = empresa.ep-codigo
      tt_emitente_integr_new.ep_codigo_principal   = empresa.ep-codigo
      tt_emitente_integr_new.ins_estadual          = IF tt_emitente_integr_new.ins_estadual = "" THEN "ISENTO" ELSE UPPER(STRING(tt_emitente_integr_new.ins_estadual,"x(20)"))
      tt_emitente_integr_new.Ins_est_cob           = IF tt_emitente_integr_new.Ins_est_cob  = "" THEN "ISENTO" ELSE UPPER(STRING(tt_emitente_integr_new.Ins_est_cob ,"x(20)")) 
      NO-ERROR.
   
   
   /*
   CREATE tt_cont_emit_integr.
   ASSIGN tt_cont_emit_integr.cod_emitente          = emitente.cod-emitente
          tt_cont_emit_integr.sequencia	            = 1
          tt_cont_emit_integr.nome                  = emitente.nome-emit
          tt_cont_emit_integr.area                  = ""
          tt_cont_emit_integr.des_cargo             = ""
          tt_cont_emit_integr.telefone		        = emitente.telefone[1]
          tt_cont_emit_integr.ramal                 = emitente.ramal[1]
          tt_cont_emit_integr.e_mail                = emitente.e-mail
          overlay(tt_cont_emit_integr.char-2,1,20)  = emitente.cgc
          tt_cont_emit_integr.cod_versao_integracao = 1
          tt_cont_emit_integr.num_tip_operac        = 1 NO-ERROR.
   */
   
   /**/


   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_new.nome_emit " tt_emitente_integr_new.nome_emit  SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_new.nome_abrev" tt_emitente_integr_new.nome_abrev SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_new.cgc       " tt_emitente_integr_new.cgc        SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_new.natureza  " tt_emitente_integr_new.natureza   SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_new.endereco  " tt_emitente_integr_new.endereco   SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** portador  " STRING(tt_emitente_integr_new.cod_portador)
             + " - " 
             + STRING(tt_emitente_integr_new.modalidade)  SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug 
   THEN DO:
      IF AVAIL emitente
      THEN MESSAGE 
      "***** portador emitente " STRING(emitente.portador)
             + " - " 
             + STRING(emitente.modalidade)  SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.


   RUN execute_evoluida_3 in hApi  (INPUT        TABLE tt_emitente_integr_new,
                                    INPUT        TABLE tt_cont_emit_integr,
                                    INPUT-OUTPUT TABLE tt_retorno_clien_fornec,
                                    INPUT        0,
                                    INPUT-OUTPUT c-arquivo-saida).
   /**/
   FOR EACH tt_retorno_clien_fornec:
       CREATE tt-erros-integracao.
       ASSIGN tt-erros-integracao.erro      = tt_retorno_clien_fornec.ttv_des_mensagem
              tt-erros-integracao.descricao = tt_retorno_clien_fornec.ttv_des_ajuda.
       ASSIGN
          c-erro = c-erro
                 + STRING(tt_retorno_clien_fornec.ttv_num_mensagem) + " - " 
                 + tt_retorno_clien_fornec.ttv_des_mensagem + " - " 
                 + tt_retorno_clien_fornec.ttv_des_ajuda
                 + "/"
                 .
       IF l-debug THEN MESSAGE 
           STRING(tt_retorno_clien_fornec.ttv_num_mensagem) + " - " 
                 + tt_retorno_clien_fornec.ttv_des_mensagem + " - " 
                 + tt_retorno_clien_fornec.ttv_des_ajuda
           VIEW-AS ALERT-BOX.
   END.

   /*
   IF AVAIL tt_emitente_integr_new
   THEN ASSIGN
      c-erro = c-erro 
             + " Portador "
             + STRING(tt_emitente_integr_new.cod_portador)
             + " - " 
             + STRING(tt_emitente_integr_new.modalidade).
   */

   DO:
      ASSIGN
          es-fornecedor-ariba.cod-emitente       = iNumEmit
          es-fornecedor-ariba.ind-atualizado-ems = 1.

      FIND FIRST emitente EXCLUSIVE-LOCK
           WHERE emitente.cod-emitente = iNumEmit
           NO-ERROR.
      IF AVAIL emitente 
      THEN DO:
          

          /* Atualiza EMS5 */
          IF l-debug THEN MESSAGE "***** Atualizando EMS5 " es-fornecedor-ariba.cod-emitente VIEW-AS ALERT-BOX.
          if  can-find(funcao where funcao.cd-funcao = "adm-cdc-ems-5.00"
              and funcao.ativo = yes
              and funcao.log-1 = yes) 
          then do:
              ASSIGN 
                 c-destino  = "Arquivo"
                 c-filename = SESSION:TEMP-DIRECTORY + "/AtualizaEMS5-"
                            + STRING(DAY(TODAY),"99")
                            + STRING(MONTH(TODAY),"99")
                            + STRING(YEAR(TODAY),"9999")
                            + STRING(TIME)
                            + "tmp".

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
                  IF RETURN-VALUE = "NOK" 
                  THEN ASSIGN
                     c-erro = c-erro
                            + "Erro na atualiza‡Æo do EMS5, verificar o arquivo - " + c-filename + "/".
              END.
          end.           
          /* Retorna c¢digo para o Ariba */
          IF l-debug THEN MESSAGE "***** Enviando c¢digo para o Ariba" es-fornecedor-ariba.cod-emitente es-fornecedor-ariba.number VIEW-AS ALERT-BOX.
          RUN integracao/api/ariba/alterarfornecedorariba.p (emitente.cod-emitente).
          
      END.
      ELSE ASSIGN
         c-erro = c-erro
                + "Fornecedor nÆo foi criado. Atualiza‡Æo no EMS5 nÆo ‚ poss¡vel./".
   END.
   
   IF c-erro > ""
   THEN DO: 
       ASSIGN
         es-fornecedor-ariba.erro               = c-erro
         es-fornecedor-ariba.ind-atualizado-ems = 2.   
       RELEASE es-fornecedor-ariba.
       RETURN "NOK".
   END.
   IF AVAIL es-fornecedor-ariba 
   THEN RELEASE es-fornecedor-ariba.
   IF AVAIL emitente
   THEN RELEASE emitente.
END.


