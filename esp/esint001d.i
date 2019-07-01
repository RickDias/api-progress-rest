/*----------------------------------------------------------------------------------------------/
 Programa..: esint001d.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 28/04/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/



DEFINE TEMP-TABLE tt_nota_fiscal NO-UNDO
    field nr-nota-fis              LIKE nota-fiscal.nr-nota-fis               serialize-name "CodigoNotaFiscal"		
    field nr-nota-fis2             LIKE nota-fiscal.nr-nota-fis               serialize-name "NumeroNotaFiscal"		
    field nat-operacao             LIKE nota-fiscal.nat-operacao              serialize-name "NaturezaOperacao"		
    field nr-pedcli                LIKE nota-fiscal.nr-pedcli                 serialize-name "CodigoPedido"		    
    field cod-emitente             LIKE nota-fiscal.cod-emitente              serialize-name "CodigoEmitente"		
    field pais                     LIKE nota-fiscal.pais                      serialize-name "Pais"		            
    field cod-estabel              LIKE nota-fiscal.cod-estabel               serialize-name "CodigoEstabelecimento"
    field peso-bru-tot             LIKE nota-fiscal.peso-bru-tot              serialize-name "PesoBruto"		    
    field peso-liq-tot             LIKE nota-fiscal.peso-liq-tot              serialize-name "PesoLiquido"	        
    field serie                    LIKE nota-fiscal.serie                     serialize-name "Serie"		        
    field vl-frete                 LIKE nota-fiscal.vl-frete                  serialize-name "ValorFrete"		    
    field vl-mercad                LIKE nota-fiscal.vl-mercad                 serialize-name "ValorMercadoria"		
    field cgc                      LIKE nota-fiscal.cgc                       serialize-name "CNPJCliente"		    
    field dt-emis                  LIKE nota-fiscal.dt-emis                   serialize-name "DataEmissao"		    
    field endereco                 LIKE nota-fiscal.endereco                  serialize-name "Endereco"		        
    field nr-fatura                LIKE nota-fiscal.nr-fatura                 serialize-name "NumeroFatura"		    
    field dt-saida                 LIKE nota-fiscal.dt-saida                  serialize-name "DataSaidaMercadoria"	
    field obs-gerada               LIKE nota-fiscal.obs-gerada                serialize-name "Observacoes"		    
    field dt-cancela               LIKE nota-fiscal.dt-cancel                 serialize-name "DataCancelamento"		
    field desc-cancela             LIKE nota-fiscal.desc-cancela              serialize-name "MotivoCancelamento"	
    field val-desconto             LIKE nota-fiscal.val-desconto              serialize-name "ValorDescontoTotal"	
    field cod-chave-aces-nf-eletro LIKE nota-fiscal.cod-chave-aces-nf-eletro  serialize-name "ChaveNFE"		        
    field idi-sit-nf-eletro        LIKE nota-fiscal.idi-sit-nf-eletro         serialize-name "SituacaoNFE"		    
    field cod-protoc               LIKE Nota-fiscal.cod-protoc                serialize-name "Protocolo"		    
    field modalidadeFrete          AS CHARACTER                               serialize-name "ModalidadeFrete"		
    field c-cod-cond-pag           AS c                                       serialize-name "CondicaoPagamento"	.

    /*field cod-tip-cte              LIKE nota-fiscal.cod-tip-cte               serialize-name "TipoCTE"		        */
    /*field serialize-name "AliquotaICMSSN"		*/
    /*field pais                     LIKE nota-fiscal.pais                      serialize-name "NumeroNotaFiscal"		*/  
    /*field serialize-name "TipoEmissaoNFE"		*/
    /*field serialize-name "Modalidade"		    */


DEFINE TEMP-TABLE tt_item_nota_fisc NO-UNDO
    FIELD codigo-item          AS c                                 serialize-name "CodigoItemNF"		     FORMAT "x(30)"
    FIELD class-fiscal         LIKE item.class-fiscal               serialize-name "ClassificacaoFiscal"	 
    FIELD cod-refer            LIKE it-nota-fisc.cod-refer          serialize-name "Referencia"		         
    FIELD nat-operacao         LIKE it-nota-fisc.nat-operacao       serialize-name "NaturezaOperacao"	     
    FIELD nr-nota-fis          LIKE It-nota-fisc.nr-nota-fis        serialize-name "NumeroNF"		         
    FIELD it-codigo            LIKE it-nota-fisc.it-codigo          serialize-name "Sequencia"		     
    FIELD qt-faturada          LIKE it-nota-fisc.qt-faturada        serialize-name "Quantidade"		     
    FIELD un-fatur             LIKE it-nota-fisc.un-fatur           serialize-name "UnidadeFaturada"		 
    FIELD vl-preori            LIKE it-nota-fisc.vl-preori          serialize-name "PrecoOriginal"		 
    FIELD peso-bruto           LIKE it-nota-fisc.peso-bruto         serialize-name "PesoBruto"		     
    FIELD val-desconto-total   LIKE it-nota-fisc.val-desconto-total serialize-name "ValorDesconto"		 
    FIELD vl-icms-it           LIKE it-nota-fisc.vl-icms-it         serialize-name "ICMS"		             
    FIELD vl-ipi-it            LIKE it-nota-fisc.vl-ipi-it          serialize-name "IPI"		             
    FIELD vl-irf-it            LIKE it-nota-fisc.vl-irf-it          serialize-name "IRRF"		                            
    FIELD vl-desconto          LIKE it-nota-fisc.vl-desconto        serialize-name "DescontoTotal"	     
    FIELD vl-pis               LIKE it-nota-fisc.vl-pis             serialize-name "PIS"		             
    FIELD vl-tot-item          LIKE it-nota-fisc.vl-tot-item        serialize-name "ValorTotalItem"  .  	

/*FIELD vl-cofins            LIKE         serialize-name "COFINS"		         */
/*FIELD serialize-name "DescontoZonaFranca"	 */   
/*FIELD serialize-name "CodigoProduto"		 */               
