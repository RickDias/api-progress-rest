/********************************************************************************
**
** cdp/cdapi366b.i - Include de Defini‡Æo das temp-tables evolu¡das.
**
********************************************************************************/
/*
   Para que nÆo fosse necess rio criar um novo programa em fun‡Æo dos novos campos
   na temp-table de integra‡Æo, fizemos o seguinte tratamento:

   EMS5                                   EMS2
   ------------------------               ----------------------
   tt_emitente_integr_new(Antiga TT)  =>  tt_emitente_integr_OLD
   tt_emitente_integr_new_1(Nova TT)  =>  tt_emitente_integr_new

   Dessa maneira nÆo foi preciso criar um novo programa e muito menos alterar todo
   o c¢digo do atual para substituir o nome da temp-table.

*/

def temp-table tt_emitente_integr_OLD NO-UNDO
    field cod_versao_integracao        as integer   format "999" 
    field cod_emitente                 as integer   format ">>>>>>9" 
    field identific                    as integer   format ">9" 
    field nome_abrev                   as character format "x(12)" 
    field nome_matriz                  as character format "x(12)" 
    field natureza                     as integer   format ">9" 
    field cgc                          as character format "x(19)" 
    field cod_portador                 as integer   format ">>>>9" 
    field modalidade                   as integer   format "9" 
    field conta_corren                 as character format "x(20)" 
    field agencia                      as character format "x(08)" 
    field cod_banco                    as integer   format "999" 
    field forn_exp                     as logical   format "Sim/NÆo" 
    field data_implant                 as date      format "99/99/9999" 
    field cod_gr_cli                   as integer   format ">>>9" 
    field cod_gr_forn                  as integer   format ">9" 
    field ins_estadual                 as character format "x(19)" 
    field ins_municipal                as character format "x(19)"  
    field estado                       as character format "x(04)" 
    field endereco                     as character format "x(40)" 
    field endereco2                    as character format "x(40)" 
    field bairro                       as character format "x(30)" 
    field cep                          as character format "x(12)" 
    field cod_pais                     as character format "x(20)" 
    field nome_mic_reg                 as character format "x(12)" 
    field nom_cidade                   as character format "x(25)" 
    field caixa_postal                 as character format "x(10)" 
    field telefax                      as character format "x(15)" 
    field ramal_fax                    as character format "x(05)" 
    field telex                        as character format "x(15)" 
    field telefone                     as character format "x(15)" extent 2 
    field ramal                        as character format "x(05)" extent 2 
    field telef_modem                  as character format "x(15)" 
    field ramal_modem                  as character format "x(05)" 
    field zip_code                     as character format "x(12)" 
    field tp_pagto                     as integer   format "99" 
    field emite_bloq                   as logical   format "Sim/NÆo" 
    field ins_banc                     as integer   format ">>9"   extent 2 
    field ven_sabado                   as integer   format "9" 
    field ven_domingo                  as integer   format "9" 
    field ven_feriado                  as integer   format "9" 
    field e_mail                       as character format "x(40)" 
    field end_cobranca                 as integer   format ">>>>>9" 
    field cod_rep                      as integer   format ">>>>9" 
    field observacoes                  as character format "x(2000)" 
    field nome_emit                    as character format "x(40)" 
    field endereco_cob                 as character format "x(40)" 
    field bairro_cob                   as character format "x(30)" 
    field cidade_cob                   as character format "x(25)" 
    field estado_cob                   as character format "x(04)" 
    field cep_cob                      as character format "x(12)" 
    field cgc_cob                      as character format "x(19)" 
    field cx_post_cob                  as character format "x(10)" 
    field zip_cob_code                 as character format "x(12)" 
    field ins_est_cob                  as character format "x(19)" 
    field pais_cob                     as character format "x(20)" 
    field gera_ad                      as logical   format "Sim/NÆo" 
    field port_prefer                  as integer   format ">>>>9" 
    field mod_prefer                   as integer   format "9" 
    &IF "{&mguni_version}" >= "2.07A" &THEN
    field ep_codigo                    LIKE empresa.ep-codigo
    field ep_codigo_principal          LIKE empresa.ep-codigo
    &ELSE
    field ep_codigo                    as integer   format ">>9" 
    field ep_codigo_principal          as integer   format ">>9" 
    &ENDIF
    field num_tip_operac               as integer   format "9" 
    field agente_retencao              as logical   format "Sim/NÆo" 
    field ramo_atividade               as character format "x(08)" 
    field recebe_inf_sci               as logical   format "Sim/NÆo"
    field vencto_dia_nao_util          as log       format "Sim/NÆo"
    field tp_desp_padrao               as int       format "99"
    field bonificacao                  as decimal   format ">>9.99"
    field ind_rendiment                as log       format "Sim/NÆo"
    field dias_comp                    as int       format ">>9"
    field rendto_tribut                as int       format "999"
    field home_page                    as char      format "x(40)"
    field utiliza_verba                as log       format "Sim/NÆo"
    field percent_verba                as dec       format ">>>9.99"
    field valor_minimo                 as dec       format ">>,>>>,>>>,>>9.99"
    field dias_atraso                  as int       format "999"
    field tp_rec_padrao                as int       format ">>9"
    field calcula_multa                as log       format "Sim/NÆo"
    field flag_pag                     as log       format "Sim/NÆo"
    field ender_text                   as char      format "x(2000)"
    field ender_cobr_text              as char      format "x(2000)"
    FIELD LOG_cr_pis                   AS LOG       FORMAT "Sim/NÆo" INITIAL NO
    index codigo                       is primary unique
          cod_emitente                 ascending.

/* Temp-table para API evoluida */
DEF TEMP-TABLE tt_emitente_integr_old_1 NO-UNDO LIKE tt_emitente_integr_OLD
    FIELD cod_id_munic_fisic   AS CHARACTER FORMAT "x(20)"
    FIELD cod_id_previd_social AS CHARACTER FORMAT "x(20)"
    FIELD dat_vencto_id_munic  AS DATE      FORMAT "99/99/9999".

/* Temp-table para API evoluida 1 */
DEF TEMP-TABLE tt_emitente_integr_old_2 NO-UNDO LIKE tt_emitente_integr_old_1
    FIELD log_control_inss     AS LOGICAL   FORMAT "Sim/NÆo" INITIAL NO.

/* Temp-table para API evoluida 2 */
DEF TEMP-TABLE tt_emitente_integr_old_3 NO-UNDO LIKE tt_emitente_integr_old_2
    FIELD log_cr_cofins        AS LOGICAL   FORMAT "Sim/NÆo" INITIAL NO.

/* Temp-table para API evoluida 3 */
DEF TEMP-TABLE tt_emitente_integr_old_4 NO-UNDO LIKE tt_emitente_integr_old_3
    FIELD log_retenc_impto_pagto      AS LOGICAL   FORMAT "Sim/NÆo" INITIAL NO.

def temp-table tt_emitente_integr_new_old no-undo
    field cod_versao_integracao     as integer   format "999"
    field cod_emitente              as integer   format ">>>>>>9"
    field identific                 as integer   format ">9"
    field nome_abrev                as character format "x(12)"
    field nome_matriz               as character format "x(12)"
    field natureza                  as integer   format ">9"
    field cgc                       as character format "x(19)"
    field cod_portador              as integer   format ">>>>9"
    field modalidade                as integer   format "9"
    field conta_corren              as character format "x(20)"
    field agencia                   as character format "x(08)"
    field cod_banco                 as integer   format "999"
    field forn_exp                  as logical   format "Sim/NÆo"
    field data_implant              as date      format "99/99/9999"
    field cod_gr_cli                as integer   format ">>>9"
    field cod_gr_forn               as integer   format ">9"
    field ins_estadual              as character format "x(19)"
    field ins_municipal             as character format "x(19)" 
    field estado                    as character format "x(04)"
    field endereco                  as character format "x(40)"
    field endereco2                 as character format "x(40)"
    field bairro                    as character format "x(30)"
    field cep                       as character format "x(12)"
    field cod_pais                  as character format "x(20)"
    field nome_mic_reg              as character format "x(12)"
    field nom_cidade                as character format "x(25)"
    field caixa_postal              as character format "x(10)"
    field telefax                   as character format "x(15)"
    field ramal_fax                 as character format "x(05)"
    field telex                     as character format "x(15)"
    field telefone                  as character format "x(15)" extent 2
    field ramal                     as character format "x(05)" extent 2
    field telef_modem               as character format "x(15)"
    field ramal_modem               as character format "x(05)"
    field zip_code                  as character format "x(12)"
    field tp_pagto                  as integer   format "99"
    field emite_bloq                as logical   format "Sim/NÆo"
    field ins_banc                  as integer   format ">>9"   extent 2
    field ven_sabado                as integer   format "9"
    field ven_domingo               as integer   format "9"
    field ven_feriado               as integer   format "9"
    field e_mail                    as character format "x(40)"
    field end_cobranca              as integer   format ">>>>>9"
    field cod_rep                   as integer   format ">>>>9"
    field observacoes               as character format "x(2000)"
    field nome_emit                 as character format "x(40)"
    field endereco_cob              as character format "x(40)"
    field bairro_cob                as character format "x(30)"
    field cidade_cob                as character format "x(25)"
    field estado_cob                as character format "x(04)"
    field cep_cob                   as character format "x(12)"
    field cgc_cob                   as character format "x(19)"
    field cx_post_cob               as character format "x(10)"
    field zip_cob_code              as character format "x(12)"
    field ins_est_cob               as character format "x(19)"
    field pais_cob                  as character format "x(20)"
    field gera_ad                   as logical   format "Sim/NÆo"
    field port_prefer               as integer   format ">>>>9"
    field mod_prefer                as integer   format "9"
    &IF "{&mguni_version}" >= "2.07A" &THEN
    field ep_codigo                 LIKE empresa.ep-codigo
    field ep_codigo_principal       LIKE empresa.ep-codigo
    &ELSE
    field ep_codigo                 as integer   format ">>9" 
    field ep_codigo_principal       as integer   format ">>9" 
    &ENDIF
    field num_tip_operac            as integer   format "9"
    field agente_retencao           as logical   format "Sim/NÆo"
    field ramo_atividade            as character format "x(08)"
    field recebe_inf_sci            as logical   format "Sim/NÆo"
    field vencto_dia_nao_util       as logical   format "Sim/NÆo"
    field tp_desp_padrao            as integer   format "99"
    field bonificacao               as decimal   format ">>9.99"
    field ind_rendiment             as logical   format "Sim/NÆo"
    field dias_comp                 as integer   format ">>9"
    field rendto_tribut             as integer   format "999"
    field home_page                 as character format "x(40)"
    field utiliza_verba             as logical   format "Sim/NÆo"	 
    field percent_verba             as decimal   format ">>>9.99"
    field valor_minimo              as decimal   format ">>,>>>,>>>,>>9.99"
    field dias_atraso               as integer   format "999" 
    field tp_rec_padrao             as integer   format ">>9"
    field calcula_multa             as logical   format "Sim/NÆo"
    field flag_pag                  as logical   format "Sim/NÆo"
    field ender_text                as char      format "x(2000)"
    field ender_cobr_text           as char      format "x(2000)"
    field log_cr_pis                as log       format "Sim/NÆo" INITIAL NO
    field cod_num_ender             as char      format "x(06)"
    field cod_num_ender_cob         as char      format "x(06)"
    field compl_ender_cob	        as char      format "x(10)"
    field cod_id_munic_fisic        as char      format "x(20)"
    field cod_id_previd_social      as char      format "x(20)"
    field dat_vencto_id_munic       as date      format "99/99/9999"
    field log_control_inss          as logical   format "Sim/NÆo" initial no
    field log_cr_cofins             as logical   format "Sim/NÆo" initial no
    field log_retenc_impto_pagto    as logical   format "Sim/NÆo" initial no
    index codigo                    is primary unique
          cod_emitente              ascending.


def temp-table tt_emitente_integr_new no-undo
    field cod_versao_integracao     as integer   format "999"
    field cod_emitente              as integer   format ">>>>>>9"
    field identific                 as integer   format ">9"
    field nome_abrev                as character format "x(12)"
    field nome_matriz               as character format "x(12)"
    field natureza                  as integer   format ">9"
    field cgc                       as character format "x(19)"
    field cod_portador              as integer   format ">>>>9"
    field modalidade                as integer   format "9"
    field conta_corren              as character format "x(20)"
    field agencia                   as character format "x(08)"
    field cod_banco                 as integer   format "999"
    field forn_exp                  as logical   format "Sim/NÆo"
    field data_implant              as date      format "99/99/9999"
    field cod_gr_cli                as integer   format ">>>9"
    field cod_gr_forn               as integer   format ">9"
    field ins_estadual              as character format "x(19)"
    field ins_municipal             as character format "x(19)" 
    field estado                    as character format "x(04)"
    field endereco                  as character format "x(40)"
    field endereco2                 as character format "x(40)"
    field bairro                    as character format "x(30)"
    field cep                       as character format "x(12)"
    field cod_pais                  as character format "x(20)"
    field nome_mic_reg              as character format "x(12)"
    field nom_cidade                as character format "x(25)"
    field caixa_postal              as character format "x(10)"
    field telefax                   as character format "x(15)"
    field ramal_fax                 as character format "x(05)"
    field telex                     as character format "x(15)"
    field telefone                  as character format "x(15)" extent 2
    field ramal                     as character format "x(05)" extent 2
    field telef_modem               as character format "x(15)"
    field ramal_modem               as character format "x(05)"
    field zip_code                  as character format "x(12)"
    field tp_pagto                  as integer   format "99"
    field emite_bloq                as logical   format "Sim/NÆo"
    field ins_banc                  as integer   format ">>9"   extent 2
    field ven_sabado                as integer   format "9"
    field ven_domingo               as integer   format "9"
    field ven_feriado               as integer   format "9"
    field e_mail                    as character format "x(40)"
    field end_cobranca              as integer   format ">>>>>9"
    field cod_rep                   as integer   format ">>>>9"
    field observacoes               as character format "x(2000)"
    field nome_emit                 as character format "x(40)"
    field endereco_cob              as character format "x(40)"
    field bairro_cob                as character format "x(30)"
    field cidade_cob                as character format "x(25)"
    field estado_cob                as character format "x(04)"
    field cep_cob                   as character format "x(12)"
    field cgc_cob                   as character format "x(19)"
    field cx_post_cob               as character format "x(10)"
    field zip_cob_code              as character format "x(12)"
    field ins_est_cob               as character format "x(19)"
    field pais_cob                  as character format "x(20)"
    field gera_ad                   as logical   format "Sim/NÆo"
    field port_prefer               as integer   format ">>>>9"
    field mod_prefer                as integer   format "9"
    &IF "{&mguni_version}" >= "2.07A" &THEN
    field ep_codigo                 LIKE empresa.ep-codigo
    field ep_codigo_principal       LIKE empresa.ep-codigo
    &ELSE
    field ep_codigo                 as integer   format ">>9"
    field ep_codigo_principal       as integer   format ">>9"
    &ENDIF
    field num_tip_operac            as integer   format "9"
    field agente_retencao           as logical   format "Sim/NÆo"
    field ramo_atividade            as character format "x(08)"
    field recebe_inf_sci            as logical   format "Sim/NÆo"
    field vencto_dia_nao_util       as logical   format "Sim/NÆo"
    field tp_desp_padrao            as integer   format "99"
    field bonificacao               as decimal   format ">>9.99"
    field ind_rendiment             as logical   format "Sim/NÆo"
    field dias_comp                 as integer   format ">>9"
    field rendto_tribut             as integer   format "999"
    field home_page                 as character format "x(40)"
    field utiliza_verba             as logical   format "Sim/NÆo"	 
    field percent_verba             as decimal   format ">>>9.99"
    field valor_minimo              as decimal   format ">>,>>>,>>>,>>9.99"
    field dias_atraso               as integer   format "999" 
    field tp_rec_padrao             as integer   format ">>9"
    field calcula_multa             as logical   format "Sim/NÆo"
    field flag_pag                  as logical   format "Sim/NÆo"
    field ender_text                as char      format "x(2000)"
    field ender_cobr_text           as char      format "x(2000)"
    field log_cr_pis                as log       format "Sim/NÆo" INITIAL NO
    field cod_id_munic_fisic        as char      format "x(20)"
    field cod_id_previd_social      as char      format "x(20)"
    field dat_vencto_id_munic       as date      format "99/99/9999"
    field log_control_inss          as logical   format "Sim/NÆo" initial no
    field log_cr_cofins             as logical   format "Sim/NÆo" initial no
    field log_retenc_impto_pagto    as logical   format "Sim/NÆo" initial no
    field log_cooperativa           as logical   format "Sim/NÆo" initial no
    field ind_tip_fornecto          as character format 'x(08)'
    field log_assoc_desportiva      as logical   format 'Sim/NÆo' initial no
    index codigo                    is primary unique
          cod_emitente              ascending.


def temp-table tt_cont_emit_integr no-undo
    field cod_versao_integracao        as integer   format "999"
    field cod_emitente                 as integer   format ">>>>>>9"
    field sequencia                    as integer   format ">>9"
    field nome                         as character format "x(40)"
    field des_cargo                    as character format "x(20)"
    field area                         as character format "x(18)"
    field telefone                     as character format "x(15)"
    field ramal                        as character format "x(05)"
    field telefax                      as character format "x(15)"
    field ramal_fax                    as character format "x(05)"
    field e_mail                       as character format "x(25)"
    field observacao                   as character format "x(2000)"
    &IF "{&mguni_version}" >= "2.07A" &THEN
    field ep_codigo_principal          LIKE empresa.ep-codigo
    &ELSE
    field ep_codigo_principal          as integer   format ">>9"
    &ENDIF
    field num_tip_operac               as integer   format "9"
    index codigo                       is primary unique 
          cod_emitente                 ascending
          sequencia                    ascending.

DEF TEMP-TABLE tt_cont_emit_integr_new NO-UNDO LIKE tt_cont_emit_integr
    field num-pessoa-fisic AS INTEGER    FORMAT ">>>,>>>,>>9"
    field nome-abrev       AS CHARACTER  FORMAT "x(12)"
    field char-1     AS  CHAR FORMAT "x(255)"
    field char-2     AS  CHAR FORMAT "x(255)"
    field dec-1      AS  DEC  FORMAT "->>>>>>>>>>>9.99999999"
    field dec-2      AS  DEC  FORMAT "->>>>>>>>>>>9.99999999"
    field log-1      AS  LOGICAL FORMAT "Sim/Nao"
    field log-2      AS  LOGICAL FORMAT "Sim/Nao".

def temp-table tt_retorno_clien_fornec NO-UNDO
    field ttv_cod_parameters               as character format "x(256)"
    field ttv_num_mensagem                 as integer format ">>>>,>>9"
    field ttv_des_mensagem                 as character format "x(52)" label "Mensagem" column-label "Mensagem"
    field ttv_des_ajuda                    as character format "x(256)"
                                           view-as editor max-chars 2000 scrollbar-vertical size 40 by 4
                                           label "Ajuda" column-label "Ajuda"
    field ttv_cod_parameters_clien         as character format "x(2000)"
    field ttv_cod_parameters_fornec        as character format "x(2000)"
    field ttv_log_envdo                    as logical format "Sim/NÆo" initial NO
    field ttv_cod_parameters_clien_financ  as character format "x(2000)"
    field ttv_cod_parameters_fornec_financ as character format "x(2000)"
    field ttv_cod_parameters_pessoa_fisic  as character format "x(2000)"
    field ttv_cod_parameters_pessoa_jurid  as character format "x(2000)"
    field ttv_cod_parameters_estrut_clien  as character format "x(2000)"
    field ttv_cod_parameters_estrut_fornec as character format "x(2000)"
    field ttv_cod_parameters_contat        as character format "x(2000)"
    field ttv_cod_parameters_repres        as character format "x(2000)"
    field ttv_cod_parameters_ender_entreg  as character format "x(2000)"
    field ttv_cod_parameters_pessoa_ativid as character format "x(2000)"
    field ttv_cod_parameters_ramo_negoc    as character format "x(2000)"
    field ttv_cod_parameters_porte_pessoa  as character format "x(2000)"
    field ttv_cod_parameters_idiom_pessoa  as character format "x(2000)"
    field ttv_cod_parameters_clas_contat   as character format "x(2000)"
    field ttv_cod_parameters_idiom_contat  as character format "x(2000)"
    field ttv_cod_parameters_telef         as character format "x(2000)"
    field ttv_cod_parameters_telef_pessoa  as character format "x(2000)"
    field ttv_cod_parameters_histor_clien  as character format "x(4000)"
    field ttv_cod_parameters_histor_fornec as character format "x(4000)".


def temp-table tt_cta_emitente no-undo
    field cod_emitente     as      int  format ">>>>>>>>9"
    field cod_banco        as      int  format "999"
    field agencia          as      char format "x(8)"
    field conta_corrente   as      char format "x(20)"
    field descricao        as      char format "x(30)"
    field preferencial     as      log  format "Sim/NÆo"
    field char-1           as      char format "x(100)"
    field char-2           as      char format "x(100)"
    field dec-1            as      dec  format "->>>>>>>>>>>9.9"
    field dec-2            as      dec  format "->>>>>>>>>>>9.9"
    field int-1            as      int  format "->>>>>>>>>9"
    field int-2            as      int  format "->>>>>>>>>9"
    field log-1            as      log  format "Sim/NÆo"
    field log-2            as      log  format "Sim/NÆo"
    field data-1           as      date format "99/99/9999"
    field data-2           as      date format "99/99/9999"
    field check_sum        as      char format "x(20)"
    index conta_corrente is primary unique
          cod_emitente
          cod_banco
          agencia
          conta_corrente.
