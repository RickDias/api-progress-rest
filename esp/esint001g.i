/*----------------------------------------------------------------------------------------------/
 Programa..: esint001g.i
 Objetivo..: Include com Defini»’o das Tabelas Temporÿrias
 Data......: 18/05/2019
 Autor.....: Rog²rio Dias
 Vers’o....: 1.000.000
-----------------------------------------------------------------------------------------------*/


DEFINE TEMP-TABLE tt_gr_cli NO-UNDO
    field cod-gr-cli     LIKE gr-cli.cod-gr-cli     serialize-name "GrupoEconomico"
    field descricao      like gr-cli.descricao      serialize-name "DescrGrupoEconomico".

