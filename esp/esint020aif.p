/*----------------------------------------------------------------------------------------------/
 Programa..: esint020aif.p
 Objetivo..: Interface Integra‡Æo Callback Fornecedores B2E
 Data......: 27/05/2019
 Autor.....: Marcelo Brasil
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{utp\ut-glob.i}

/*603833*/
DEF VAR l-debug AS l INIT YES NO-UNDO.

FUNCTION fnc-proximo-emit RETURNS INTEGER() FORWARD.

DEF TEMP-TABLE ttx-emitente NO-UNDO 
          LIKE emitente.

DEF BUFFER empresa     FOR mgcad.empresa.
DEF BUFFER bf-emitente FOR emitente.

{esp/esint001aic.i}

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
DEF VAR i-oper               AS i         NO-UNDO.
DEF VAR l-ambos              AS l         NO-UNDO.
DEF VAR iNumEmit             AS i         NO-UNDO.

DEF VAR c-destino            AS c         NO-UNDO. 
DEF VAR c-filename           AS c         NO-UNDO. 

DO TRANS:

   FIND empresa NO-LOCK
        WHERE empresa.ep-codigo = i-ep-codigo-usuario 
        NO-ERROR.
   
   RUN cdp/cdapi366b.r PERSISTENT SET execute_evoluida_2.
   
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
      
      find first dist-emitente 
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

   END.

   CREATE tt_emitente_integr_old_2.
   
   ASSIGN 
       tt_emitente_integr_old_2.num_tip_operac        = 1
       tt_emitente_integr_old_2.cod_emitente          = iNumEmit
       tt_emitente_integr_old_2.cod_versao_integracao = 1.
   
   IF i-oper = 1
   THEN ASSIGN
      tt_emitente_integr_old_2.identific              = 2.
   ELSE ASSIGN                                 
      tt_emitente_integr_old_2.identific              = integer(emitente.identific)
      tt_emitente_integr_old_2.cgc                    = emitente.cgc
      tt_emitente_integr_old_2.nome_abrev             = emitente.nome-abrev
      tt_emitente_integr_old_2.nome_matriz            = IF   emitente.nome-matriz = "" 
                                                        THEN emitente.nome-abrev
                                                        ELSE emitente.nome-matriz.

   ASSIGN
      tt_emitente_integr_old_2.nome_emit              = es-fornecedor-ariba.Corporate-Name
      .

   IF i-oper = 1
   THEN DO: 
      IF es-fornecedor-ariba.cpf > ""
      THEN ASSIGN
         tt_emitente_integr_old_2.natureza            = 1.
      IF es-fornecedor-ariba.cnpj > ""
      THEN ASSIGN
         tt_emitente_integr_old_2.natureza            = 2.

      IF tt_emitente_integr_old_2.natureza            = 1
      OR tt_emitente_integr_old_2.natureza            = 2
      THEN ASSIGN
         tt_emitente_integr_old_2.cgc                 = c-cgc
         tt_emitente_integr_old_2.nome_abrev          = SUBSTR(c-cgc,1,12)
         tt_emitente_integr_old_2.nome_matriz         = tt_emitente_integr_old_2.nome_abrev.
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
            tt_emitente_integr_old_2.nome_abrev       = c-nom-estrangeiro
            tt_emitente_integr_old_2.natureza         = 3
            .
      END.
   END.
   ELSE ASSIGN
      tt_emitente_integr_old_2.natureza             = emitente.natureza.

   ASSIGN
      tt_emitente_integr_old_2.conta_corren         = es-fornecedor-ariba.conta + es-fornecedor-ariba.dig-conta-corrente
      tt_emitente_integr_old_2.agencia              = es-fornecedor-ariba.agencia + es-fornecedor-ariba.dig-agencia
      tt_emitente_integr_old_2.cod_banco            = INT(SUBSTR(es-fornecedor-ariba.banco,1,3))

      tt_emitente_integr_old_2.data_implant         = IF i-oper = 1 THEN TODAY ELSE emitente.data-implant
      
      tt_emitente_integr_old_2.ins_estadual         = es-fornecedor-ariba.ie
      tt_emitente_integr_old_2.ins_municipal        = IF i-oper = 1 THEN "" ELSE emitente.ins-municipal
      
      tt_emitente_integr_old_2.estado               = es-fornecedor-ariba.state
      tt_emitente_integr_old_2.endereco             = REPLACE(es-fornecedor-ariba.street,"-"," ").

    
   IF l-debug 
   THEN MESSAGE "**** nome-abrev " tt_emitente_integr_old_2.nome_abrev  SKIP
                " natureza " tt_emitente_integr_old_2.natureza          SKIP
                " operacao " ENTRY(i-oper,"cria,altera")                SKIP
                " endereco " tt_emitente_integr_old_2.endereco
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

   ASSIGN
      tt_emitente_integr_old_2.telefone[1]          = STRING(es-fornecedor-ariba.Numero-Telefone).
   
   ASSIGN
      tt_emitente_integr_old_2.endereco2            = es-fornecedor-ariba.complement
      tt_emitente_integr_old_2.bairro               = es-fornecedor-ariba.district
      tt_emitente_integr_old_2.cep                  = REPLACE(es-fornecedor-ariba.zip,"-","")
      tt_emitente_integr_old_2.Nom_cidade           = es-fornecedor-ariba.Municipality
      tt_emitente_integr_old_2.E_mail               = es-fornecedor-ariba.e-mail
      .

   IF tt_emitente_integr_old_2.natureza             = 3
   THEN DO:

      ASSIGN
         tt_emitente_integr_old_2.estado            = "EX".

      FIND FIRST es-pais-ariba NO-LOCK
           WHERE es-pais-ariba.cod-pais-ariba = es-fornecedor-ariba.country
           NO-ERROR.
      IF AVAIL es-pais-ariba
      THEN ASSIGN
         tt_emitente_integr_old_2.cod_pais          = es-pais-ariba.nome-pais
         tt_emitente_integr_old_2.pais              = es-pais-ariba.nome-pais
         tt_emitente_integr_old_2.Pais_cob          = es-pais-ariba.nome-pais.
   END.
   ELSE DO:
      ASSIGN
         tt_emitente_integr_old_2.cod_pais          = "Brasil"
         tt_emitente_integr_old_2.pais              = "Brasil"
         tt_emitente_integr_old_2.Pais_cob          = "Brasil".
   END.


   IF l-ambos = YES
   THEN ASSIGN
      tt_emitente_integr_old_2.identific            = 3.

   IF i-oper  = 1
   OR l-ambos = YES
   THEN ASSIGN
      tt_emitente_integr_old_2.cod_gr_cli           = es-ariba-b2e-param.cod-gr-cli 
      tt_emitente_integr_old_2.cod_gr_for           = es-ariba-b2e-param.cod-gr-for 
      tt_emitente_integr_old_2.cod_portador         = es-ariba-b2e-param.portador
      tt_emitente_integr_old_2.modalidade           = es-ariba-b2e-param.modalidade
      tt_emitente_integr_old_2.Ven_sabado           = es-ariba-b2e-param.ven-sabado
      tt_emitente_integr_old_2.Ven_Domingo          = es-ariba-b2e-param.ven-domingo
      tt_emitente_integr_old_2.Ven_feriado          = es-ariba-b2e-param.ven-feriado
      tt_emitente_integr_old_2.Tp_rec_padrao        = es-ariba-b2e-param.tp-rec-padrao
      tt_emitente_integr_old_2.Tp_desp_padrao       = es-ariba-b2e-param.tp-desp-padrao
      tt_emitente_integr_old_2.cod_rep              = es-ariba-b2e-param.cod-rep
                                                    
      tt_emitente_integr_old_2.Num_tip_operac       = 1
      .
   
   IF i-oper  = 2
   THEN ASSIGN
      tt_emitente_integr_old_2.cod_gr_cli           = emitente.cod-gr-cli 
      tt_emitente_integr_old_2.cod_gr_for           = emitente.cod-gr-for 
      tt_emitente_integr_old_2.cod_portador         = emitente.portador
      tt_emitente_integr_old_2.modalidade           = emitente.modalidade
      tt_emitente_integr_old_2.Ven_sabado           = emitente.Ven-sabado     
      tt_emitente_integr_old_2.Ven_Domingo          = emitente.Ven-Domingo    
      tt_emitente_integr_old_2.Ven_feriado          = emitente.Ven-feriado    
      tt_emitente_integr_old_2.Tp_rec_padrao        = emitente.Tp-rec-padrao  
      tt_emitente_integr_old_2.Tp_desp_padrao       = emitente.Tp-desp-padrao 
   
      tt_emitente_integr_old_2.cod_rep              = emitente.cod-rep
      
      tt_emitente_integr_old_2.observacoes          = emitente.observacoes
      .
   IF tt_emitente_integr_old_2.cod_portador         = 0
   THEN ASSIGN
      tt_emitente_integr_old_2.cod_portador         = es-ariba-b2e-param.portador.
   IF tt_emitente_integr_old_2.modalidade           = 0
   THEN ASSIGN
      tt_emitente_integr_old_2.modalidade           = es-ariba-b2e-param.modalidade.

   IF l-debug
   THEN MESSAGE "**** identific " tt_emitente_integr_old_2.identific
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   IF i-oper = 2
   THEN ASSIGN
      tt_emitente_integr_old_2.caixa_postal           = emitente.caixa-postal                 
      tt_emitente_integr_old_2.zip_code               = emitente.zip-code                     
      tt_emitente_integr_old_2.Endereco_cob           = emitente.endereco-cob                 
      tt_emitente_integr_old_2.Bairro_cob             = emitente.bairro-cob                   
      tt_emitente_integr_old_2.Cidade_cob             = emitente.cidade-cob                   
      tt_emitente_integr_old_2.Estado_cob             = emitente.estado-cob                   
      tt_emitente_integr_old_2.Cep_cob                = emitente.cep-cob                      
      tt_emitente_integr_old_2.Cgc_cob                = emitente.cgc-cob                      
      tt_emitente_integr_old_2.Cx_post_cob            = emitente.cx-post-cob                  
      tt_emitente_integr_old_2.Zip_cob_code           = emitente.zip-cob-code                 
      tt_emitente_integr_old_2.Ins_est_cob            = emitente.Ins-est-cob                  
      tt_emitente_integr_old_2.Gera_ad                = emitente.gera-ad                      
      tt_emitente_integr_old_2.tp_pagto               = emitente.tp-pagto                     
                                                                                              
      tt_emitente_integr_old_2.telefone[1]            = emitente.Telefone[1]                  
      tt_emitente_integr_old_2.ramal[1]               = emitente.Ramal[1]                     
      tt_emitente_integr_old_2.telefone[2]            = emitente.Telefone[2]                  
      tt_emitente_integr_old_2.ramal[2]               = emitente.Ramal[2]                     
      tt_emitente_integr_old_2.telefax                = emitente.Telefax                      
      tt_emitente_integr_old_2.ramal_fax              = emitente.Ramal-fax                    
      tt_emitente_integr_old_2.telex                  = emitente.Telex                        
      tt_emitente_integr_old_2.telef_modem            = emitente.Telef-modem                  
      tt_emitente_integr_old_2.ramal_modem            = emitente.Ramal-modem                  
      tt_emitente_integr_old_2.port_prefer            = emitente.port-prefer                  
      tt_emitente_integr_old_2.mod_prefer             = emitente.mod-prefer                   
      tt_emitente_integr_old_2.emite_bloq             = emitente.emite-bloq                   
      tt_emitente_integr_old_2.nome_mic_reg           = emitente.nome-mic-reg                 
      tt_emitente_integr_old_2.ins_banc[1]            = emitente.ins-banc[1]                  
      tt_emitente_integr_old_2.ins_banc[2]            = emitente.ins-banc[2]                  
      tt_emitente_integr_old_2.tp_rec_padrao          = integer(emitente.tp-rec-padrao)       
      tt_emitente_integr_old_2.forn_exp               = emitente.forn-exp                     
      tt_emitente_integr_old_2.agente_retencao        = emitente.agente-retencao              
      tt_emitente_integr_old_2.Ramo_atividade         = emitente.atividade                    
      tt_emitente_integr_old_2.Recebe_inf_sci         = emitente.Recebe-inf-sci               
      tt_emitente_integr_old_2.Vencto_dia_nao_util    = emitente.Vencto-dia-nao-util          
      tt_emitente_integr_old_2.Bonificacao            = emitente.Bonificacao                  
      tt_emitente_integr_old_2.Ind_rendiment          = emitente.Ind-rendiment                
      tt_emitente_integr_old_2.Dias_comp              = emitente.Dias-comp                    
      tt_emitente_integr_old_2.Rendto_tribut          = emitente.rend-tribut                  
      tt_emitente_integr_old_2.Home_page              = emitente.Home-page                    
      tt_emitente_integr_old_2.Utiliza_verba          = emitente.Utiliza-verba                
      tt_emitente_integr_old_2.Percent_verba          = emitente.Percent-verba                
      tt_emitente_integr_old_2.Valor_minimo           = emitente.Valor-minimo                 
      tt_emitente_integr_old_2.Dias_atraso            = emitente.nr-Dias-atraso               
      tt_emitente_integr_old_2.Calcula_multa          = emitente.Calcula-multa                
      tt_emitente_integr_old_2.Flag_pag               = IF emitente.Flag-pag = 2 THEN YES ELSE NO                    
      tt_emitente_integr_old_2.Ender_text             = emitente.endereco_text                
      tt_emitente_integr_old_2.Ender_cobr_text        = emitente.Endereco-cob-text            
      tt_emitente_integr_old_2.Log_cr_pis             = IF emitente.idi-tributac-pis = 1 THEN YES ELSE NO            
      /*tt_emitente_integr_old_2.cod_id_munic_fisic     = emitente.cod-id-munic-fisic    */   
      tt_emitente_integr_old_2.cod_id_previd_social   = emitente.cod-inscr-inss               
      /*tt_emitente_integr_old_2.dat_vencto_id_munic    = emitente.dat-vencto-id-munic   */   
      tt_emitente_integr_old_2.log_control_inss       = emitente.log-controla-val-max-inss    
      /*tt_emitente_integr_old_2.log_cr_cofins          = emitente.log-cr-cofins         */        /*???*/
      /*tt_emitente_integr_old_2.log_retenc_impto_pagto = emitente.log_retenc-impto-pagto*/   
      /*tt_emitente_integr_old_2.log_cooperativa        = SUBSTR(emitente.char-2,103,1) = "S" */ .  /*???*/
      /*tt_emitente_integr_old_2.ind_tip_fornecto       = SUBSTR(emitente.char-2,104,8)  */    /*???*/
      /*tt_emitente_integr_old_2.log_assoc_desportiva   = emitente.log-assoc-desportiva   */   /*???*/
      .                                                                                       
                                                        
   ASSIGN 
      tt_emitente_integr_old_2.ep_codigo             = empresa.ep-codigo
      tt_emitente_integr_old_2.ep_codigo_principal   = empresa.ep-codigo
      tt_emitente_integr_old_2.ins_estadual          = IF tt_emitente_integr_old_2.ins_estadual = "" THEN "ISENTO" ELSE UPPER(STRING(tt_emitente_integr_old_2.ins_estadual,"x(20)"))
      tt_emitente_integr_old_2.Ins_est_cob           = IF tt_emitente_integr_old_2.Ins_est_cob  = "" THEN "ISENTO" ELSE UPPER(STRING(tt_emitente_integr_old_2.Ins_est_cob ,"x(20)")) 
      NO-ERROR.
   
   
   /*
   CREATE tt_cont_emit_integr_new.
   ASSIGN tt_cont_emit_integr_new.cod_emitente          = emitente.cod-emitente
          tt_cont_emit_integr_new.sequencia	            = 1
          tt_cont_emit_integr_new.nome                  = emitente.nome-emit
          tt_cont_emit_integr_new.area                  = ""
          tt_cont_emit_integr_new.des_cargo             = ""
          tt_cont_emit_integr_new.telefone		        = emitente.telefone[1]
          tt_cont_emit_integr_new.ramal                 = emitente.ramal[1]
          tt_cont_emit_integr_new.e_mail                = emitente.e-mail
          overlay(tt_cont_emit_integr_new.char-2,1,20)  = emitente.cgc
          tt_cont_emit_integr_new.cod_versao_integracao = 1
          tt_cont_emit_integr_new.num_tip_operac        = 1 NO-ERROR.
   */
   
   /**/


   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_old_2.nome_emit " tt_emitente_integr_old_2.nome_emit  SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_old_2.nome_abrev" tt_emitente_integr_old_2.nome_abrev SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_old_2.cgc       " tt_emitente_integr_old_2.cgc        SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_old_2.natureza  " tt_emitente_integr_old_2.natureza   SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** tt_emitente_integr_old_2.endereco  " tt_emitente_integr_old_2.endereco   SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF l-debug THEN MESSAGE 
      "***** portador  " STRING(tt_emitente_integr_old_2.cod_portador)
             + " - " 
             + STRING(tt_emitente_integr_old_2.modalidade)  SKIP
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


   RUN execute_evoluida_2 in execute_evoluida_2 (INPUT        TABLE tt_emitente_integr_old_2,
                                                 INPUT        TABLE tt_cont_emit_integr_new,
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
   IF AVAIL tt_emitente_integr_old_2
   THEN ASSIGN
      c-erro = c-erro 
             + " Portador "
             + STRING(tt_emitente_integr_old_2.cod_portador)
             + " - " 
             + STRING(tt_emitente_integr_old_2.modalidade).
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
          /*Completa parƒmetros*/
          
/*******************          
              ASSIGN r-emitente                                   = RECID(emitente)
                     row-id-emitente                              = ROWID(emitente)
                     /* Alteracao Tiago DSC 20151023 - Fixar cod-emitente para cliente cobranca
                     emitente.end-cobranca                        = integer(tt-emitente.cod_id_cliente_cobranca)*/
                     emitente.end-cobranca                        = integer(iNumEmit)
                     /*******/
                     emitente.cgc-cob                             = tt-emitente.cpfcnpjCobranca
                     emitente.cod-cond-pag                        = integer(tt-emitente.cond_pag)
                     emitente.cod-canal-venda                     = integer(tt-emitente.cod_canal_venda)
                     emitente.nat-operacao                        = tt-emitente.natureza_operacao
                     emitente.nat-ope-ext                         = tt-emitente.natureza_interestadual.

              ASSIGN emitente.cod-transp                          = integer(tt-emitente.cod_transportadora)
                     overlay(emitente.char-1,21,1)                = tt-emitente.suspensao_ipi
                     emitente.telef-fac                           = tt-emitente.Telefax2
                     emitente.ins-banc[1]                         = int(tt-emitente.nm_instrucao_bancaria)
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
                     emitente.nome-mic-reg                        = IF AVAIL mgdis.cidade THEN mgdis.cidade.nome-mic-reg ELSE "".

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
                     emitente.cod-entrega                             = "PadrÆo" /* 24/11/2015-Claudio-DCS, tt-emitente.nm_entrega_padrao */
                     .

              IF tt-emitente.nome_cliente = "" THEN
                  ASSIGN emitente.nome-matriz      = tt-emitente.nome_abreviado.
              ELSE
                  ASSIGN emitente.nome-matriz      = tt-emitente.nome_cliente.

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
                      WHERE es-emitente.cod-emitente = INT(tt-emitente.cod-emitente) NO-ERROR.

                  IF NOT AVAIL es-emitente THEN DO:

                      CREATE es-emitente.
                      ASSIGN es-emitente.cod-emitente = INT(tt-emitente.cod-emitente).
                  END.

                  ASSIGN es-emitente.exige-laudo-cq   = LOGICAL(tt-emitente.exige-laudo)
                         es-emitente.exige-loteu      = LOGICAL(tt-emitente.exige-loteu)
                         es-emitente.rejeita-prox-ven = LOGICAL(tt-emitente.rejeita-prox-ven)
                         es-emitente.regime-trib      = tt-emitente.regime-trib.

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


*******************/


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


