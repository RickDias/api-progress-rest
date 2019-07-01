DEF TEMP-TABLE tt-emitente NO-UNDO
   FIELD i-cod-emitente            like emitente.cod-emitente           
   FIELD c-nome-abrev              like emitente.nome-abrev             
   FIELD c-cgc-cli                 like emitente.cgc                    
   FIELD c-identific               as char format "x(2)"                
   FIELD c-natureza                as char format "x(2)"                
   FIELD c-nome-emit               like emitente.nome-emit              
   FIELD c-endereco                like emitente.endereco               
   FIELD c-cidade                  like emitente.cidade                 
   FIELD c-bairro                  like emitente.bairro                 
   FIELD c-cep                     like emitente.cep                    
   FIELD c-agencia                 like emitente.agencia                
   FIELD c-estado                  like emitente.estado                 
   FIELD c-caixa-postal            like emitente.caixa-postal           
   FIELD c-telefax                 like emitente.telefax                
   FIELD c-ramal-fax               like emitente.ramal-fax              
   FIELD c-telex                   like emitente.telex                  
   FIELD de-compr-period           like emitente.compr-period           
   FIELD c-pais                    like emitente.pais                   
   FIELD c-categoria               like emitente.categoria              
   FIELD c-nat-operacao            like emitente.nat-operacao           
   FIELD c-observacoes             like emitente.observacoes            
   FIELD c-nat-ope-ext             like emitente.nat-ope-ext            
   FIELD c-matriz                  like emitente.nome-matriz            
   FIELD c-telef-modem             like emitente.telef-modem            
   FIELD c-ramal-modem             like emitente.ramal-modem            
   FIELD c-telef-fac               like emitente.telef-fac              
   FIELD c-ramal-fac               like emitente.ramal-fac              
   FIELD c-cod-suframa             like emitente.cod-suframa            
   FIELD c-cod-cacex               like emitente.cod-cacex              
   FIELD c-conta-corren            like emitente.conta-corren           
   FIELD c-nr-tabpre               like emitente.nr-tabpre              
   FIELD c-user-libcre             like emitente.user-libcre            
   FIELD c-estado-cob              like emitente.estado-cob             
   FIELD c-pais-cob                like emitente.pais-cob               
   FIELD c-cidade-cob              like emitente.cidade-cob             
   FIELD c-bairro-cob              like emitente.bairro-cob             
   FIELD c-endereco-cob            like emitente.endereco-cob           
   FIELD c-cx-post-cob             like emitente.cx-post-cob            
   FIELD c-ins-est-cob             like emitente.ins-est-cob            
   FIELD c-ins-estadual            like emitente.ins-est-cob            
   FIELD c-cgc-cob                 like emitente.cgc-cob                
   FIELD c-fone                    like emitente.telefone               
   FIELD c-atividade               like emitente.atividade              
   FIELD c-mic-reg                 like emitente.nome-mic-reg           
   FIELD c-ins-municipal           like emitente.ins-municipal          
   FIELD c-zip-code                like emitente.zip-code               
   FIELD c-data-taxa               as   char format "x(08)"             
   FIELD c-data-implant            as   char format "x(08)"             
   FIELD c-data-lim-cred           as   char format "x(08)"             
   FIELD c-data-ult-nf-e           as   char format "x(08)"             
                                   
   FIELD c-linha-produt            like emitente.linha-produt           
                                                                 
   FIELD de-taxa-financ            like emitente.taxa-financ            
   FIELD de-lim-credito            like emitente.lim-credito            
   FIELD de-per-minfat             like emitente.per-minfat             
   FIELD de-per-max-canc           like emitente.per-max-canc           
   FIELD de-bonificacao            like emitente.bonificacao            
   FIELD c-utiliza-verba           as char format "x(03)"               
   FIELD de-percent-verba          as decimal format ">>>9.99"          
                                                                 
   FIELD i-cod-aux-emit            like emitente.cod-emitente           
   FIELD i-cod-cond-pag            like emitente.cod-cond-pag           
   FIELD i-cod-transp              like emitente.cod-transp             
   FIELD i-cod-gr-forn             like emitente.cod-gr-forn            
   FIELD i-peratr                  like emitente.nr-peratr              
   FIELD i-end-cobranca            like emitente.end-cobranca           
   FIELD i-cod-rep                 like emitente.cod-rep                
   FIELD i-cod-gr-cli              like emitente.cod-gr-cli             
   FIELD i-perc-fat-ped            like emitente.perc-fat-ped           
   FIELD i-portador                like emitente.portador               
   FIELD i-modalidade              like emitente.modalidade             
   FIELD i-port-prefer             like emitente.port-prefer            
   FIELD i-mod-prefer              like emitente.mod-prefer             
   FIELD i-bx-acatada              like emitente.bx-acatada             
   FIELD i-nr-copias-ped           like emitente.nr-copias-ped          
   FIELD i-gera-difer              like emitente.gera-difer             
   FIELD i-ind-aval                like emitente.ind-aval               
   FIELD i-ven-domingo             like emitente.ven-domingo            
   FIELD i-ven-feriado             like emitente.ven-feriado            
   FIELD i-ven-sabado              like emitente.ven-sabado             
   FIELD c-cep-cob                 like emitente.cep-cob                
   FIELD i-ind-credito             like emitente.ind-cre-cli            
   FIELD i-nr-titulo               like emitente.nr-titulo              
   FIELD i-emissao-ped             like emitente.emissao-ped            
   FIELD i-emit-bloq               like emitente.emissao-ped            
   FIELD i-mesina                  like emitente.nr-mesina              
   FIELD i-prox-ad                 like emitente.prox-ad                
   FIELD i-tp-desp                 like emitente.tp-desp-padrao         
   FIELD i-tp-rec                  like emitente.tp-rec-padrao          
   FIELD c-e-mail                  like emitente.e-mail                 
   FIELD i-emit-etiq               as int format "99"      
   FIELD i-vencto-dt-fluxo         AS INT FORMAT "99"      
   FIELD i-ind-fat-par             as int format "99"                   
   FIELD i-cod-banco               as int format "999"                  
   FIELD i-tipo-pag                as int format "9"              initial 1    
   FIELD i-tr-ar-valor             like emitente.tr-ar-valor            
   FIELD i-nr-dias                 like emitente.nr-dias                
   FIELD i-tip-reg                 as int format "99"      
   FIELD i-tip-cob-desp            as int format "9"                    
   FIELD i-abrange-aval            as int format "9"                    
   FIELD i-ind-aval-embar          as int format "9"                    
   FIELD l-contrib-icms            like emitente.contrib-icms 
   FIELD c-end-comp                as char format "x(2000)" 
   FIELD c-end-cob-comp            as char format "x(2000)" 
   FIELD i-canal-venda             like emitente.cod-canal-venda        
   FIELD c-retem-pagto             as char format "x(01)"               
   FIELD i-portador-ap             like emitente.portador              
   FIELD i-modalidade-ap           like emitente.modalidade            
   FIELD l-ind-contr-subst-interm  AS LOG                              
   FIELD c-endereco-aux            LIKE emitente.endereco              
                                   
   FIELD l-gera-ad                 AS l
   FIELD l-funcao-portador-ap      AS l
                                   
   FIELD i-instr-banc-1            AS i
   FIELD i-instr-banc-2            AS i
   FIELD i-trib-pis                AS i
   FIELD c-inscr-inss              AS c
   FIELD i-trib-cofins             AS i
   FIELD c-calcula-pis-cofins-unid AS c
   FIELD i-situacao                AS i
   FIELD c-data-vigencia-ini       AS c
   FIELD c-data-vigencia-fim       AS c

   INDEX i1 i-cod-emitente.


