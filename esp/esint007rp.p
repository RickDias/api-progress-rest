/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{include/i_fnctrad.i}
{utp/ut-glob.i}
{cdp/cd9900.i1}
{include/i_dbvers.i}                
{cdp/cdcfgfin.i}
{cdp/cdapi090def.i} /* Variaveis utilizadas na integra»’o EMS2/NEOLOG */

{cdp/cd7300.i1}
{cdp/cdapi270.i}
def var i-tipo-movto as integer.
assign c-transacao = "ADM111".
{cdp/cd7300.i2}
{cdp/cd7300.i3 "001" c-transacao}
{cdp/cdcfgmat.i} 
{cdp/cdcfgdis.i}
&if defined (bf_dis_fat_moeda) &then  /* pre-processador */   
    def var h-bo as handle no-undo.
    def var r-chave as rowid no-undo.
    {dibo/bodi275.i "rowObject" }
    {include/boerrtab.i}
&endif /* preprocessador */
{cdp/cd6667.i}
{include/i-sysvar.i}



DEF TEMP-TABLE tt-param
    FIELD destino          AS INTEGER
    FIELD arq-destino      AS CHAR
    FIELD arq-entrada      AS CHAR
    FIELD todos            AS INTEGER
    FIELD usuario          AS CHAR
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER.       


DEF VAR raw-param          AS RAW NO-UNDO.
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita       AS RAW.


{method/dbotterr.i}

{esp/esapi007.i}



DEF INPUT  PARAM TABLE FOR tt-emitente.
DEF OUTPUT PARAM TABLE FOR RowErrors.


DEF VAR c-tmp-arq-imp AS c NO-UNDO.
DEF VAR c-tmp-arq-sai AS c NO-UNDO.
DEF VAR c-dados       AS c NO-UNDO.
DEF VAR c-erros       AS c NO-UNDO.

DEF STREAM tmp-arq-imp.
DEF STREAM tmp-arq-exp.

DEF VAR iErro         AS i NO-UNDO.
DEF VAR c-err-codigo  AS c NO-UNDO.



/* ***************************  Main Block  *************************** */

ASSIGN
   c-tmp-arq-sai = SESSION:TEMP-DIRECTORY + "exemit-" + STRING(TIME) + ".imp"
   c-tmp-arq-imp = SESSION:TEMP-DIRECTORY + "imemit-" + STRING(TIME) + ".imp". 

OUTPUT STREAM tmp-arq-exp TO VALUE(c-tmp-arq-imp) PAGE-SIZE 0.
FOR EACH tt-emitente:
    ASSIGN c-dados                            = "".

    ASSIGN 
       substr(c-dados, 001, 6)            = ""
       substr(c-dados, 007, 12)           = c-nome-abrev            
       substr(c-dados, 019, 19)           = c-cgc-cli               
       substr(c-dados, 038, 01)           = CAPS(c-identific)       
       substr(c-dados, 039, 01)           = CAPS(c-natureza)        
       substr(c-dados, 080, 40)           = c-endereco              
       substr(c-dados, 120,30)            = c-bairro                
       substr(c-dados, 150, 25)           = c-cidade                
       substr(c-dados, 175, 04)           = CAPS(c-estado)          
       substr(c-dados, 179, 12)           = c-cep                   
       substr(c-dados, 191,10)            = c-caixa-postal          
       substr(c-dados, 201, 20)           = c-pais                  
       substr(c-dados, 221, 19)           = c-ins-estadual          
       substr(c-dados, 240, 2)            = ""
       substr(c-dados, 242, 05)           = STRING(INT64 (de-taxa-financ) * 100)       
       substr(c-dados, 247, 8)            = c-data-taxa           
       substr(c-dados, 255, 05)           = STRING(i-cod-transp)
       substr(c-dados, 260, 02)           = STRING(i-cod-gr-forn)
       substr(c-dados, 262, 08)           = c-linha-produt        
       substr(c-dados, 270, 12)           = c-atividade           
       substr(c-dados, 282, 15)           = c-telefax             
       substr(c-dados, 297, 5)            = c-ramal-fax           
       substr(c-dados, 302, 15)           = c-telex               
       substr(c-dados, 317, 8)            = c-data-implant        
       substr(c-dados, 325, 14)           = STRING(INT( de-compr-period) * 100) 
       substr(c-dados, 339,01)            = STRING(l-contrib-icms,"1/2")
       substr(c-dados, 340, 02)           = ""
       substr(c-dados, 342, 03)           = c-categoria
       substr(c-dados, 345, 05)           = STRING(i-cod-rep)
       substr(c-dados, 350, 02)           = ""

       substr(c-dados, 352, 05)           = STRING(INT64( de-bonificacao) * 100)
       substr(c-dados, 357, 01)           = STRING(i-abrange-aval)                 
       substr(c-dados, 358, 02)           = STRING(i-cod-gr-cli)                   
       substr(c-dados, 360, 11)           = STRING(INT64 (de-lim-credito) * 100)
       substr(c-dados, 371, 8)            = c-data-lim-cred
       substr(c-dados, 379, 03)           = STRING(i-perc-fat-ped)                 
       substr(c-dados, 382, 05)           = STRING(i-portador)                     
       substr(c-dados, 387, 02)           = STRING(i-modalidade)                   
       substr(c-dados, 389, 01)           = STRING(i-ind-fat-par)                  
       substr(c-dados, 390, 01)           = STRING(i-ind-credito)
       .
    assign 
       substr(c-dados, 391, 01)           = ""
       substr(c-dados, 392, 06)           = c-nat-operacao
       substr(c-dados, 5329, 2000)        = c-observacoes
       substr(c-dados, 548, 04)           = STRING(INT64 (de-per-minfat) * 100)
       substr(c-dados, 552, 01)           = STRING(i-emissao-ped) 

       substr(c-dados, 553, 12)           = c-matriz                
       substr(c-dados, 565, 15)           = c-telef-modem           
       substr(c-dados, 580, 05)           = c-ramal-modem           
       substr(c-dados, 585, 15)           = c-telef-fac             
       substr(c-dados, 600, 05)           = c-ramal-fac             
       substr(c-dados, 605, 07)           = c-agencia                                               
       substr(c-dados, 613, 08)           = STRING(i-nr-titulo)
       substr(c-dados, 621, 08)           = STRING(i-nr-dias)
       substr(c-dados, 629, 04)           = STRING(INT64 (de-per-max-canc) / 100)
       substr(c-dados, 633, 08)           = c-data-ult-nf-e         
       substr(c-dados, 641, 01)           = STRING(i-emit-bloq)
       substr(c-dados, 642, 01)           = STRING(i-emit-etiq)
       substr(c-dados, 643, 01)           = STRING(i-tr-ar-valor)

       substr(c-dados, 644, 01)           = STRING(l-gera-ad,"1/2")
       substr(c-dados, 645, 05)           = STRING(i-port-prefer)
       substr(c-dados, 650, 02)           = STRING(i-mod-prefer)
       substr(c-dados, 652, 03)           = STRING(i-bx-acatada)               
       substr(c-dados, 655, 12)           = c-conta-corren   
       substr(c-dados, 675, 02)           = STRING(i-nr-copias-ped)
       substr(c-dados, 677, 20)           = c-cod-suframa    
       substr(c-dados, 697, 20)           = c-cod-cacex      
       substr(c-dados, 717, 01)           = STRING(i-gera-difer)
       substr(c-dados, 5125,01)           = STRING(i-vencto-dt-fluxo).

    IF i-pais-impto-usuario = 2 
    then do:
       IF l-funcao-portador-ap 
       THEN ASSIGN 
          substr(c-dados,5148,80) = c-nome-emit.
       ELSE ASSIGN 
          substr(c-dados,5126,80) = c-nome-emit.
    end.
    else do:         
       IF  l-funcao-portador-ap 
       THEN ASSIGN 
          substr(c-dados,5149,80) = c-nome-emit.
       ELSE ASSIGN 
          substr(c-dados,5142,80) = c-nome-emit.
    END.
        
        
    &if defined (bf_dis_adm_preco) 
    &then  /* pre-processador */
        assign 
           substr(c-dados, 718 , 08)  =  STRING(c-nr-tabpre    )
           substr(c-dados, 726 , 01)  =  STRING(i-ind-aval     )
           substr(c-dados, 727 , 12)  =  STRING(c-user-libcre  )
           substr(c-dados, 739 , 01)  =  STRING(i-ven-domingo  )
           substr(c-dados, 740 , 01)  =  STRING(i-ven-sabado   )
           substr(c-dados, 741 , 19)  =  STRING(c-cgc-cob      )
           substr(c-dados, 760 , 12)  =  STRING(c-cep-cob      )
           substr(c-dados, 772 , 04)  =  STRING(c-estado-cob   )
           substr(c-dados, 776 , 25)  =  STRING(c-cidade-cob   )
           substr(c-dados, 801 , 30)  =  STRING(c-bairro-cob   )
           substr(c-dados, 831 , 40)  =  STRING(c-endereco-cob )
           substr(c-dados, 871 , 10)  =  STRING(c-cx-post-cob  )
           substr(c-dados, 881 , 19)  =  STRING(c-ins-est-cob  )
           substr(c-dados, 900 , 03)  =  STRING(i-cod-banco    )
           substr(c-dados, 903 , 06)  =  STRING(i-prox-ad      )
           substr(c-dados, 909 , 01)  =  STRING(i-tip-reg      )
           substr(c-dados, 910 , 01)  =  STRING(i-ven-feriado  )
           substr(c-dados, 911 , 02)  =  STRING(i-tipo-pag     )
           substr(c-dados, 913 , 01)  =  STRING(i-tip-cob-desp )
           substr(c-dados, 914 , 19)  =  STRING(c-ins-municipal)
           substr(c-dados, 933 , 03)  =  STRING(i-tp-desp      )
           substr(c-dados, 936 , 03)  =  STRING(i-tp-rec       )
           substr(c-dados, 939 , 12)  =  STRING(c-zip-code     )
           substr(c-dados, 951 , 12)  =  STRING(c-mic-reg      )
       &if "{&bf_dis_versao_ems}" >= "2.062" &then 
           substr(c-dados, 667,  04)  =  STRING(i-cod-cond-pag )
       &else
           substr(c-dados, 963 , 03)  =  STRING(i-cod-cond-pag )
       &endif
           substr(c-dados, 966 , 15)  = STRING(c-fone[1]       )
           substr(c-dados, 981 , 15)  = STRING(c-fone[2]       )
           substr(c-dados, 996 , 02)  = STRING(i-mesina        )
           substr(c-dados, 998 , 03)  = STRING(i-instr-banc-1  )
           substr(c-dados, 1001, 03)  = STRING(i-instr-banc-2  )
           substr(c-dados, 1004, 06)  = STRING(c-nat-ope-ext   )
           substr(c-dados, 1010, 09)  = STRING(i-cod-emitente  )
           substr(c-dados, 1019, 09)  = STRING(i-end-cobranca  )
           substr(c-dados, 1028, 01)  = STRING(c-utiliza-verba )
           substr(c-dados, 1029, 06)  = STRING(INT64(DE-PERCENT-VERBA) * 100)
           substr(c-dados, 5079, 20)  = STRING(c-pais-cob      ).
        if i-pais-impto-usuario = 1 
        then do:
            &if  "{&mgadm_version}" < "2.05" 
            &then
                assign 
                    substr(c-dados, 5099, 01) = STRING(i-trib-pis  )
                    substr(c-dados, 5101, 20) = STRING(c-inscr-inss).
            &else                            
                 assign 
                    substr(c-dados, 5137, 01) = STRING(i-trib-pis  )
                    substr(c-dados, 5116, 20) = STRING(c-inscr-inss).
            &endif
        end.

    &else

        assign 
           substr(c-dados, 718, 05)  = STRING(c-nr-tabpre    )
           substr(c-dados, 723, 01)  = STRING(i-ind-aval     )
           substr(c-dados, 724, 12)  = STRING(c-user-libcre  )
           substr(c-dados, 736, 01)  = STRING(i-ven-domingo  )
           substr(c-dados, 737, 01)  = STRING(i-ven-sabado   )
           substr(c-dados, 738, 19)  = STRING(c-cgc-cob      )
           substr(c-dados, 757, 12)  = STRING(c-cep-cob      )
           substr(c-dados, 769, 04)  = STRING(c-estado-cob   )
           substr(c-dados, 773, 25)  = STRING(c-cidade-cob   )
           substr(c-dados, 798, 30)  = STRING(c-bairro-cob   )
           substr(c-dados, 828, 40)  = STRING(c-endereco-cob )
           substr(c-dados, 868, 10)  = STRING(c-cx-post-cob  )
           substr(c-dados, 878, 19)  = STRING(c-ins-est-cob  )
           substr(c-dados, 897, 03)  = STRING(i-cod-banco    )
           substr(c-dados, 900, 06)  = STRING(i-prox-ad      )
           substr(c-dados, 906, 01)  = STRING(i-tip-reg      )
           substr(c-dados, 907, 01)  = STRING(i-ven-feriado  )
           substr(c-dados, 908, 02)  = STRING(i-tipo-pag     )
           substr(c-dados, 910, 01)  = STRING(i-tip-cob-desp )
           substr(c-dados, 911, 19)  = STRING(c-ins-municipal)
           substr(c-dados, 930, 03)  = STRING(i-tp-desp      )
           substr(c-dados, 933, 03)  = STRING(i-tp-rec       )
           substr(c-dados, 936, 12)  = STRING(c-zip-code     )
           substr(c-dados, 948, 12)  = STRING(c-mic-reg      )
           substr(c-dados, 960, 03)  = STRING(i-cod-cond-pag )
           substr(c-dados, 963, 15)  = STRING(c-fone[1]      )
           substr(c-dados, 978, 15)  = STRING(c-fone[2]      )
           substr(c-dados, 993, 02)  = STRING(i-mesina       )
           substr(c-dados, 995, 03)  = STRING(i-instr-banc-1 )
           substr(c-dados, 998, 03)  = STRING(i-instr-banc-2 )
           substr(c-dados, 1001, 06  = STRING(c-nat-ope-ext  )
           substr(c-dados, 1007, 09  = STRING(i-cod-emitente )
           substr(c-dados, 1016, 09  = STRING(i-end-cobranca )
           substr(c-dados, 1080, 20  = STRING(c-pais-cob     )
           substr(c-dados, 1025, 10) = ""
           .
        if i-pais-impto-usuario = 1 
        then do:
            &if  "{&mgadm_version}" < "2.05" 
            &then
                assign substr(c-dados, 1079, 01) string(i-trib-pis  )
                       substr(c-dados, 1102, 20) string(c-inscr-inss). 
            &else                               
                assign substr(c-dados, 5137, 01) string(i-trib-pis  )
                       substr(c-dados, 5116, 20) string(c-inscr-inss).
            &endif
        end.           
    &endif /* pre-processador */
    IF i-pais-impto-usuario = 1 THEN DO:
        &if  "{&mgadm_version}" >= "2.05" &then
            assign substr(c-dados,5136,1) = string(i-trib-cofins).
        &else
            &if "{&bf_mat_versao_ems}" = "2.04" &then
                assign substr(c-dados,5121,1) = string(i-trib-cofins).
            &else
                assign substr(c-dados,1100,1) = string(i-trib-cofins).
            &endif
        &endif 
    END.
    ELSE DO:
         &if "{&bf_mat_versao_ems}" >= "2.04" &then
             assign substr(c-dados,5121,1) = string(i-trib-cofins).
         &else
             assign substr(c-dados,1100,1) = string(i-trib-cofins).
         &endif
    END.
    IF l-funcao-portador-ap 
    THEN DO:
        ASSIGN substr(c-dados, 5141, 05) = string(i-portador-ap  )
               substr(c-dados, 5146, 02) = string(i-modalidade-ap).
    END.
    IF i-pais-impto-usuario = 1 
    THEN DO:
        &IF "{&bf_mat_versao_ems}" >= "2.05" 
        &THEN
            ASSIGN c-calcula-pis-cofins-unid = SUBSTRING(c-dados,5139,1).
        &ELSE
            &IF "{&bf_mat_versao_ems}" = "2.04" &THEN
                ASSIGN c-calcula-pis-cofins-unid = SUBSTRING(c-dados,5122,1).
            &ELSE
                ASSIGN c-calcula-pis-cofins-unid = SUBSTRING(c-dados,1122,1).
            &ENDIF
        &ENDIF
        if c-natureza = "2" then do: 
            &if  "{&bf_mat_versao_ems}" >= "2.06" &then
                assign substr(c-dados, 5140, 01) = c-retem-pagto.
            &else
                &if  "{&bf_mat_versao_ems}"  = "2.04" &then
                    assign substr(c-dados, 5123, 01) = c-retem-pagto.
                &else
                    assign substr(c-dados, 1123, 01) = c-retem-pagto.
                &endif
            &endif 
        end.

        if i-pais-impto-usuario = 1 then do:
           &if "{&bf_mat_versao_ems}" < "2.04" &then
               IF l-funcao-portador-ap THEN 
                  ASSIGN SUBSTR(c-dados,5148,1) = STRING(l-ind-contr-subst-interm,"S/N"). 
               ELSE
                  ASSIGN substr(c-dados,1124,1) = STRING(l-ind-contr-subst-interm,"S/N").
           &else
                &if "{&bf_mat_versao_ems}" = "2.04" &then
                    ASSIGN SUBSTR(c-dados,5124,1) = STRING(l-ind-contr-subst-interm,"S/N").
                &else
                    IF l-funcao-portador-ap THEN 
                       ASSIGN SUBSTR(c-dados,5148,1) = STRING(l-ind-contr-subst-interm,"S/N"). 
                    ELSE
                       ASSIGN substr(c-dados,5141,1) = STRING(l-ind-contr-subst-interm,"S/N").    
                &endif
           &endif
        END.

        assign  
           SUBSTR(c-dados, 1035, 40)   = STRING(c-e-mail           )  
           SUBSTR(c-dados, 1075, 01)   = STRING(i-ind-aval-embar   )  
           SUBSTR(c-dados, 1076, 03)   = STRING(i-canal-venda      )  
           SUBSTR(c-dados, 1079, 2000) = STRING(c-end-cob-comp     )  
           SUBSTR(c-dados, 3079, 2000) = STRING(c-end-comp         )  
           SUBSTR(c-dados, 5099, 1)    = STRING(i-situacao         )  
           SUBSTR(c-dados, 5100, 8)    = STRING(c-data-vigencia-ini)  
           SUBSTR(c-dados, 5108, 8)    = STRING(c-data-vigencia-fim)  
           .

    END.
   
    /*
    c-dados = FILL(STRING(RANDOM(1,9)),6000). 
    */

    PUT STREAM tmp-arq-exp UNFORMATTED
        c-dados
        SKIP.

END.
OUTPUT STREAM tmp-arq-exp CLOSE.

CREATE tt-param.
ASSIGN tt-param.destino        = 2
       tt-param.arq-destino    = c-tmp-arq-sai
       tt-param.arq-entrada    = c-tmp-arq-imp
       tt-param.todos          = 1      
       tt-param.usuario        = c-seg-usuario
       tt-param.data-exec      = TODAY
       tt-param.hora-exec      = TIME.

RAW-TRANSFER tt-param TO raw-param.

RUN cdp/cd1302rp.p (INPUT raw-param,
                    INPUT TABLE tt-raw-digita).

INPUT STREAM tmp-arq-imp FROM VALUE(c-tmp-arq-sai) NO-ECHO.
REPEAT:

   IMPORT STREAM tmp-arq-imp UNFORMATTED 
       c-erros.
   
   IF c-erros MATCHES "*-----*"
   OR c-erros MATCHES "*de Cliente/Fornecedor*"
   OR c-erros MATCHES "*CNPJ / CPF*"
   OR c-erros MATCHES "*Registros*"
   OR TRIM(c-erros) = ""
   THEN NEXT.

   IF SUBSTR(c-erros,1,9) <> "" THEN ASSIGN c-err-codigo = SUBSTR(c-erros,1,9).
   
   CREATE RowErrors.
   ASSIGN iErro                      = iErro + 1
          RowErrors.ErrorSequence    = iErro
          RowErrors.ErrorNumber      = 17006
          RowErrors.ErrorType        = "Error"
          RowErrors.ErrorDescription = SUBSTR(c-erros,46,65)
          RowErrors.ErrorHelp        = "Codigo do Emitente: " + c-err-codigo.
END.

INPUT STREAM tmp-arq-imp CLOSE.

OS-DELETE VALUE(c-tmp-arq-sai).
OS-DELETE VALUE(c-tmp-arq-imp).
