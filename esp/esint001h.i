/*----------------------------------------------------------------------------------------------/
 Programa..: esint001h.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 18/05/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


DEFINE TEMP-TABLE tt_fam_comerc NO-UNDO
    field fm-cod-com LIKE fam-comerc.fm-cod-com  serialize-name "FamiliaComercial"
    field descricao  like fam-comerc.descricao   serialize-name "DescrFamiliaComercial".

         
