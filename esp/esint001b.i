/*----------------------------------------------------------------------------------------------/
 Programa..: esint001b.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 22/04/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


DEFINE TEMP-TABLE tt_PedVenda NO-UNDO
      FIELD l-status               AS LOGICAL                            SERIALIZE-NAME "Status"
      FIELD c-descricao            AS CHARACTER                          SERIALIZE-NAME "Descricao"
      field estab-destino          LIKE ped-venda.estab-destino          SERIALIZE-NAME "EstabelecimentoDestino"       format "X(5)"
      field cod-rep                LIKE repres.cod-rep                   serialize-name "Representante"                
      field dt-entrega             LIKE ped-venda.dt-entrega             serialize-name "DataEntregaPrevista"          
      field cod-entrega            LIKE ped-venda.cod-entrega            serialize-name "LocalEntrega"                 format "X(12)" 
      field nome-abrev             LIKE emitente.nome-abrev              serialize-name "NomeAbreviadoCliente"         format "X(12)"
      field dt-emissao             LIKE ped-venda.dt-emissao             serialize-name "DataEmissaoPedido"            
      field moeda                  AS CHARACTER                          serialize-name "MoedaFaturamento"             
      field nr-pedcli              LIKE ped-venda.nr-pedcli              serialize-name "NumeroPedidoCliente"          format "X(12)"
      field num-pedido-bonif       LIKE ped-venda.num-pedido-bonif       serialize-name "NumeroPedidoBonificacao"      format "X(12)"
      field mensagem               AS CHARACTER                          serialize-name "Mensagem"                     
      field nome-transp            LIKE ped-venda.nome-transp            serialize-name "Transportador"                format "X(12)"
      field nr-pedido              LIKE ped-venda.nr-pedido              serialize-name "CodigoPedido"                 
      FIELD modalidade-frete       AS c                                  SERIALIZE-NAME "ModalidadeFrete"              
      field cgc                    LIKE Emitente.cgc                     serialize-name "CNPJCliente"                  format "X(14)"
      field tp-pedido              like ped-venda.tp-pedido              serialize-name "TipoPedido"                   format "X(2)"
      field nat-operacao           like ped-venda.nat-oper               serialize-name "NaturezaOperacao"             format "X(6)"
      field dt-entorig             like ped-venda.dt-entorig             serialize-name "DataEntrega"                 
      field cidade-cif             like ped-venda.cidade-cif             serialize-name "CidadeCIF"                    format "X(25)"
      field nr-tabpre              like ped-venda.nr-tabpre              serialize-name "TabelaPrecos"                 format "X(8)"
      field tp-preco               like ped-venda.tp-preco               serialize-name "TipoPreco"                    
      field nome-tr-red            like ped-venda.nome-tr-red            serialize-name "NomeTransportadoraRedespacho" format "X(12)"
      field esp-ped                like ped-venda.esp-ped                serialize-name "EspeciePedido"                
      field num-pedido-origem      like ped-venda.num-pedido-origem      serialize-name "NumeroPedidoOrigem"           format "X(12)"
      field destinomercadoria      AS CHARACTER                          serialize-name "DestinoMercadoria"            
      field log-ped-bonif-pendente like ped-venda.log-ped-bonif-pendente serialize-name "ConcedeBonificacao"           
      field cod-estabel            like ped-venda.cod-estabel            serialize-name "CodigoEstabelecimento"        format "X(5)"
      field cod-priori             like ped-venda.cod-priori             serialize-name "Prioridade"                   
      field cod-emitente           like ped-venda.cod-emitente           serialize-name "CodigoEmitente"               
      field codcondpag             AS c                                  serialize-name "CondicaoPagamento"            
      field nr-pedrep              like ped-venda.nr-pedrep              serialize-name "NumeroPedidoRepresentante"    format "X(12)"
      FIELD atendido               LIKE ped-venda.atendido               SERIALIZE-NAME "TipoAtendimento"              
      FIELD cod-ped-clien-mp       LIKE ped-venda.cod-ped-clien-mp       SERIALIZE-NAME "CodigoSalesforce" 
      FIELD sit-ped                AS CHARACTER                          SERIALIZE-NAME "SituacaoPedido".

DEF TEMP-TABLE tt_PedItem NO-UNDO
     field nr-tabpre                  like ped-item.nr-tabpre                  SERIALIZE-NAME "TabelaPreco"                  format "X(12)"     
     field per-des-item               like ped-item.per-des-item               SERIALIZE-NAME "PercDescInformado"                 
     field per-des-icms               like ped-item.per-des-icms               SERIALIZE-NAME "PercentualDescontoICMS"           
     field vl-preuni                  like ped-item.vl-preuni                  SERIALIZE-NAME "PrecoLiquido"                     
     field nr-sequencia               like ped-item.nr-sequencia               SERIALIZE-NAME "CodigoItem"                       
     field it-codigo                  like ped-item.it-codigo                  SERIALIZE-NAME "CodigoProduto"                    
     field nr-pedcli                  like ped-item.nr-pedcli                  SERIALIZE-NAME "PedidoCliente"                    
     field nat-operacao               like ped-item.nat-operacao               SERIALIZE-NAME "NaturezaOperacao"                 
     field cod-un                     like ped-item.cod-un                     SERIALIZE-NAME "UnidadeFaturamento"               
     field val-icm-ret                AS de                                    SERIALIZE-NAME "ICMSRetidonaFonte"                
     field val-pct-desconto-tab-preco like ped-item.val-pct-desconto-tab-preco SERIALIZE-NAME "PercDescTabelaPrecos"             
     field per-minfat                 like ped-item.per-minfat                 SERIALIZE-NAME "PercentualMinimoFaturamentoParcial"                 
     field dt-entrega                 like ped-item.dt-entrega                 SERIALIZE-NAME "DataEntregaPrevista"              
     field cod-ord-compra             like ped-item.cod-ord-compra             SERIALIZE-NAME "OrdemCompra"                      
     field val-desconto-inform        like ped-item.val-desconto-inform        SERIALIZE-NAME "ValorDescontoInformado"           
     field dt-entorig                 like ped-item.dt-entorig                 SERIALIZE-NAME "DataEntregaOriginal"              
     field vl-preori                  like ped-item.vl-preori                  SERIALIZE-NAME "PrecoOriginal"                    
     field qt-un-fat                  like ped-item.qt-un-fat                  SERIALIZE-NAME "QuantidadeUnidadeFaturamento"  
     .     
   
