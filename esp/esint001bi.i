/*----------------------------------------------------------------------------------------------/
 Programa..: esint001bi.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 22/04/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{esp\esint001b.i}
                    
DEF TEMP-TABLE tt-param-ped-venda NO-UNDO
    FIELD nome-abrev   LIKE ped-venda.nome-abrev  
    FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
    FIELD cod-estabel  LIKE ped-venda.cod-estabel 
    FIELD nr-pedrep    LIKE ped-venda.nr-pedrep   
    FIELD no-ab-rep    LIKE ped-venda.no-ab-reppri   
    FIELD nat-operacao LIKE ped-venda.nat-operacao
    FIELD dt-emissao   LIKE ped-venda.dt-emissao  
    FIELD dt-entrega   LIKE ped-venda.dt-entrega  
    FIELD dt-entorig   LIKE ped-venda.dt-entorig   
    FIELD ind-fat-par  LIKE ped-venda.ind-fat-par 
    FIELD mo-codigo    LIKE ped-venda.mo-codigo   
    FIELD ind-lib-nota LIKE ped-venda.ind-lib-nota
    FIELD ind-tp-frete LIKE ped-venda.ind-tp-frete
    FIELD nr-ind-finan LIKE ped-venda.nr-ind-finan
    FIELD observacoes  LIKE ped-venda.observacoes 
    FIELD cod-cond-pag LIKE ped-venda.cod-cond-pag.


DEF TEMP-TABLE tt-param-ped-item NO-UNDO
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD qt-pedida    LIKE ped-item.qt-pedida
    FIELD qt-un-fat    LIKE ped-item.qt-un-fat
    FIELD vl-preori    LIKE ped-item.vl-preori.

