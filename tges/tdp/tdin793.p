/***********************************************************************************                                                       
** Programa : tdp/tdin791
** Objetivo : Ao Eliminar um registro no MLA0112, indentificar na tabela especifica
**            para exportar os dados ao ARIBA
** Versao   : 2.00.00.001
** Autor    : Cleberson Silva - TOTVS Private
** Data     : 24/07/2019
** Historico ---------------------------------------------------------------------- 
** Versao   :
** Objetivo :
** Autor    :
** Data     : 
***********************************************************************************/

DEFINE PARAM BUFFER b-mla-hierarquia-faixa       FOR mla-hierarquia-faixa.

IF AVAIL b-mla-hierarquia-faixa THEN 
DO:
    FIND FIRST es-exporta-mla EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL es-exporta-mla THEN
        CREATE es-exporta-mla.

    ASSIGN es-exporta-mla.ind-exportar = YES.

    FIND CURRENT es-exporta-mla NO-LOCK NO-ERROR.

END.


