/*----------------------------------------------------------------------------------------------/
 Programa..: esint001a.i
 Objetivo..: Include com Definiá∆o das Tabelas Tempor†rias
 Data......: 26/02/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

DEFINE TEMP-TABLE tt_emitente SERIALIZE-NAME "req" 
    FIELD l-status                      AS LOGICAL                   COLUMN-LABEL "Status"                                            SERIALIZE-NAME "Status"
    FIELD c-descricao                   AS CHARACTER                                                                                  SERIALIZE-NAME "Descricao"
    field RazaoSocial	                as character                 column-label "Raz∆o Social"               format "X(80)"         SERIALIZE-NAME "RazaoSocial"                   
    field CNPJ	                        as character                 column-label "CNPJ"                       format "X(14)"         SERIALIZE-NAME "CNPJ"                          
    field IE                            as character                 column-label "Inscriá∆o Estadual"         format "X(14)"         SERIALIZE-NAME "IE"                            
    field Email	                        as character                 column-label "Email"                      format "x(40)"         SERIALIZE-NAME "Email"                         
    field Telefone	                    as character                 column-label "Telefone"                   format "x(15)"	      SERIALIZE-NAME "Telefone"                      
    field CodigoCliente         	    as INTEGER                   column-label "C¢digo Cliente Datasul"     format ">,>>>,>>9"     SERIALIZE-NAME "CodigoCliente"                 
    field TipoClienteCanal	            as CHARACTER                 column-label "Tipo De Cliente"            format "9"             SERIALIZE-NAME "TipoClienteCanal"              
    FIELD EmailXML                      AS CHARACTER                 COLUMN-LABEL "E-mail XML"                 FORMAT "X(255)"        SERIALIZE-NAME "EmailXML"                      
    field CanalCliente	                as INTEGER                   column-label "Canal Cliente"                                     SERIALIZE-NAME "CanalCliente"                  
    field GrupoEconomico	            as CHARACTER                 column-label "Grupo Econìmico"                                   SERIALIZE-NAME "GrupoEconomico"                
    field EmailFinanceiro	            as character                 column-label "Email Financeiro"           format "x(170)"        SERIALIZE-NAME "EmailFinanceiro"               
    field TelefoneFinanceiro	        as character                 column-label "Telefone Financeiro"        format "x(40)"         SERIALIZE-NAME "TelefoneFinanceiro"            
    field RegimeTributario	            AS CHARACTER                 column-label "Regime Tribut†rio"                                 SERIALIZE-NAME "RegimeTributario"              
    field ContribuinteICMS      	    as LOGICAL                   column-label "Contribuinte ICMS"                                 SERIALIZE-NAME "ContribuinteICMS"              
    field Suframa	                    as CHARACTER                 column-label "Suframa"                    format "X(12)"         SERIALIZE-NAME "Suframa"                       
    field DataValidadeSuframa           as DATE                      column-label "Data de Vencimento Suframa" format "99/99/9999"    SERIALIZE-NAME "DataValidadeSuframa"           
    field NomeAbreviado	                as character                 column-label "Nome Abreviado"             format "X(12)"         SERIALIZE-NAME "NomeAbreviado"                 
    field LimiteCredito	                as INTEGER                   column-label "Limite De CrÇdito"                                 SERIALIZE-NAME "LimiteCredito"                 
    field AvaliacaoCredito	            like emitente.ind-aval       column-label "Avaliaá∆o de CrÇdito"                              SERIALIZE-NAME "AvaliacaoCredito"              
    field TipoCredito	                AS CHARACTER                 column-label "Tipo De CrÇdito"    FORMAT 'X(10)'                 SERIALIZE-NAME "TipoCredito"                   
    field DataLimiteCredito	            as DATE                      column-label "Data Limite De CrÇdito "    format "99/99/9999"    SERIALIZE-NAME "DataLimiteCredito"       
    /*field DataLimiteCredito	            as DATE                      column-label "Data Limite De CrÇdito "    format "99/99/9999"    SERIALIZE-NAME "DataDividaMaisLonga"*/
    field SaldoCredito	                as DECIMAL                   column-label "Saldo De CrÇdito"                                  SERIALIZE-NAME "SaldoCredito"                  
    field Banco	                        as CHARACTER                 column-label "Banco"                      format ">>9"           SERIALIZE-NAME "Banco"                         
    field Agencia	                    as character                 column-label "Agància"                    FORMAT "X(4)"          SERIALIZE-NAME "Agencia"                       
    field Conta	                        as character                 column-label "Conta"                      format "X(10)"         SERIALIZE-NAME "Conta"                         
    field IM                            as character                 column-label "Inscriá∆o Municipal"        format "X(14)"         SERIALIZE-NAME "IM"                            
    field NaturezaCliente               AS CHARACTER                 column-label "Natureza Cliente"           FORMAT "X(15)"         SERIALIZE-NAME "NaturezaCliente"               
    field NaturezaOperacao	            as character                 column-label "Natureza de Operaá∆o"       format "X(6)"          SERIALIZE-NAME "NaturezaOperacao"              
    field Matriz	                    as character                 column-label "Matriz"                     format "X(12)"         SERIALIZE-NAME "Matriz"                        
    field Representante	                as INTEGER                   column-label "Representante"              format ">>>>9"         SERIALIZE-NAME "Representante"                 
    field Microrregiao	                as character                 column-label "Microrregi∆o"               format "x(12)"         SERIALIZE-NAME "Microrregiao"                  
    field ClienteExigeLoteUnico	        as LOGICAL                   column-label "Cliente Exige Lote Ènico"                          SERIALIZE-NAME "ClienteExigeLoteUnico"         
    field ExigeCertifAnalise	        as LOGICAL                   column-label "Exige Certif Analise"                              SERIALIZE-NAME "ExigeCertifAnalise"            
    field NaoRecebeLoteProxVencto   	as LOGICAL                   column-label "N∆o Recebe LoteProx Vencto"                        SERIALIZE-NAME "NaoRecebeLoteProxVencto"       
    field RamoAtividade	                as character                 column-label "Ramo De Atividade"          format "X(12)"         SERIALIZE-NAME "RamoAtividade"                 
    field CNPJCobranca	                as character                 column-label "CNPJ de Cobranáa"           format "X(14)"         SERIALIZE-NAME "CNPJCobranca"                  
    field PaisEntrega	                as character                 column-label "Pais Entrega"               format "x(60)"         SERIALIZE-NAME "PaisEntrega"                   
    field ComplementoEntrega	        as character                 column-label "Complemento Entrega"        format "X(80)"         SERIALIZE-NAME "ComplementoEntrega"            
    field CidadeEntrega	                as character                 column-label "Cidade Entrega"             format "x(25)"         SERIALIZE-NAME "CidadeEntrega"                 
    field BairroEntrega	                as character                 column-label "Bairro Entrega"             format "X(80)"         SERIALIZE-NAME "BairroEntrega"                 
    field EstadoEntrega	                as character                 column-label "Estado Entrega"             format "x(2)"          SERIALIZE-NAME "EstadoEntrega"                 
    field LogradouroEntrega	            as character                 column-label "Logradouro Entrega"         format "X(80)"         SERIALIZE-NAME "LogradouroEntrega"             
    field CepEntrega	                as character                 column-label "Cep Entrega"                format "X(8)"          SERIALIZE-NAME "CepEntrega"                    
    field PaisCobranca	                as character                 column-label "Pais Cobranáa"              format "x(60)"         SERIALIZE-NAME "PaisCobranca"                  
    field ComplementoCobranca	        as character                 column-label "Complemento Cobranáa"       format "X(80)"         SERIALIZE-NAME "ComplementoCobranca"           
    field CidadeCobranca	            as character                 column-label "Cidade Cobranáa"            format "x(25)"         SERIALIZE-NAME "CidadeCobranca"               
    field BairroCobranca	            as character                 column-label "Bairro Cobranáa"            format "X(80)"         SERIALIZE-NAME "BairroCobranca"               
    field EstadoCobranca	            as character                 column-label "Estado Cobranáa"            format "x(2)"          SERIALIZE-NAME "EstadoCobranca"               
    field LogradouroCobranca	        as character                 column-label "Logradouro Cobranáa"        format "X(80)"         SERIALIZE-NAME "LogradouroCobranca"            
    field CepCobranca	                as character                 column-label "Cep Cobranáa"               format "X(8)"          SERIALIZE-NAME "CepCobranca".                   
      

DEFINE TEMP-TABLE tt_EnderecoList SERIALIZE-NAME "EnderecoList"                                                                   
    FIELD fld-rel       AS INTEGER      
    FIELD codEntrega    AS CHARACTER COLUMN-LABEL "C¢digo Entregas" FORMAT "X(10)"          SERIALIZE-NAME "CodEntrega"
    field Pais	        as character column-label "Pais"            format "X(60)"          SERIALIZE-NAME "Pais"       
    field Logradouro 	as character column-label "Logradouro"      format "X(80)"          SERIALIZE-NAME "Logradouro" 
    field Complemento	as character column-label "Complemento"     format "X(40)"          SERIALIZE-NAME "Complemento"
    field Cidade	    as character column-label "Cidade"          format "X(25)"          SERIALIZE-NAME "Cidade"     
    field Bairro	    as character column-label "Bairro"          format "X(30)"          SERIALIZE-NAME "Bairro"     
    field Estado	    as character column-label "Estado"          format "X(2) "          SERIALIZE-NAME "Estado"     
    field Cep           as character column-label "Cep"             format "X(10)"          SERIALIZE-NAME "Cep"
    FIELD padrao        AS LOGICAL   COLUMN-LABEL "Padr∆o"          FORMAT "TRUE/FALSE"     SERIALIZE-NAME "Padrao".


DEFINE TEMP-TABLE tt_CondicaoPagamentoList SERIALIZE-NAME "CondicaoPagamentoList"
    FIELD fld-rel                       AS INTEGER                   
    FIELD condicaoPadrao    AS LOGICAL   COLUMN-LABEL "Padr∆o"                                    SERIALIZE-NAME "Padrao"
    field codigoCondicao    AS CHARACTER COLUMN-LABEL "C¢digo Condiá∆o Pagamento" FORMAT "X(10)"  SERIALIZE-NAME "codigoCondicao"
    field descricaoCondicao AS CHARACTER COLUMN-LABEL "Descriá∆o"                 FORMAT "X(255)" SERIALIZE-NAME "descricaoCondicao".     


DEFINE TEMP-TABLE tt_ContatoList SERIALIZE-NAME "ContactList"
    FIELD fld-rel           AS INTEGER                   
    field Nome	            as character column-label "Nome"            format "X(30)"  SERIALIZE-NAME "Nome"          
    field CodigoContato     as INTEGER   column-label "Sequància"                       SERIALIZE-NAME "CodigoContato" 
    field Sobrenome	        as character column-label "Sobrenome"       format "X(40)"  SERIALIZE-NAME "Sobrenome"     
    field AreaContato	    as character column-label "µrea do Contato" format "X(80)"  SERIALIZE-NAME "AreaContato"   
    field Cargo        	    as character column-label "Cargo"           format "X(128)" SERIALIZE-NAME "Cargo"         
    field Email	            as character column-label "Email"           format "X(60)"  SERIALIZE-NAME "Email"         
    field Telefone	        as character column-label "Telefone"                        SERIALIZE-NAME "Telefone"      
    field Aplicacao	        as character column-label "Aplicaá∆o"                       SERIALIZE-NAME "Aplicacao"     
    field Descricao	        as character column-label "Descriá∆o"       format "X(255)" SERIALIZE-NAME "Descricao"     
    field CNPJ_CPF           as character column-label "CNPJ / CPF"      format "X(14)" SERIALIZE-NAME "CNPJ_CPF"      
    field Fax	            as character column-label "Fax"             format "X(15)"  SERIALIZE-NAME "Fax"           
    field Ramal	            as character column-label "Ramal"           format "X(5)"   SERIALIZE-NAME "RamalTelefone"         
    field RamalFAX	        as character column-label "Ramal FAX "      format "X(5)"   SERIALIZE-NAME "RamalFAX".     

DEFINE TEMP-TABLE tt_ExcecaoProdutoList SERIALIZE-NAME "ExcecaoProdutoList"
    FIELD fld-rel                       AS INTEGER                   
    field codigoProduto    AS CHARACTER COLUMN-LABEL "Codigo do Produto" FORMAT "X(10)" SERIALIZE-NAME "codigoProduto".

