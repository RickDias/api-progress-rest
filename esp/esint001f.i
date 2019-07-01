/*----------------------------------------------------------------------------------------------/
 Programa..: esint001f.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 16/05/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


DEFINE TEMP-TABLE tt_repres NO-UNDO
    field cod-rep     LIKE repres.cod-rep     serialize-name "CodigoRepresentante"
    field nome        like repres.nome        serialize-name "NomeRepresentante"
    field nome-ab-reg like repres.nome-ab-reg serialize-name "RegiaoRepresentante"
    field e-mail      like repres.e-mail      serialize-name "EmailRepresentante"
    field nome-abrev  like repres.nome-abrev  serialize-name "NomeAbreviadoRepresentante"
    FIELD situacao    AS LOGICAL              SERIALIZE-NAME "Inativo".
 
DEFINE TEMP-TABLE tt_repres_familia NO-UNDO
    FIELD fm-cod-com LIKE ITEM.fm-cod-com SERIALIZE-NAME "CodigoFamilia"
    FIELD it-codigo  LIKE ITEM.it-codigo  SERIALIZE-NAME "CodigoProduto".

DEFINE TEMP-TABLE tt_repres_cliente NO-UNDO
    FIELD cgc LIKE emitente.cgc SERIALIZE-NAME "CNPJ".
