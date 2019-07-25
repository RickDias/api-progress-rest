
/*----------------------------------------------------------------------------------------------/
 Programa..: esint029a.i
 Objetivo..: Include com Definiá∆o das Tabelas Tempor†rias
 Data......: 26/02/2019
 Autor.....: Cleberson Silva
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

DEF TEMP-TABLE de-para-tipo NO-UNDO
    FIELD doc-totvs         AS INT
    FIELD tp-ariba          AS CHAR.


DEFINE TEMP-TABLE tt_csv NO-UNDO
    field conteudo AS CHARACTER serialize-name "conteudo".  
