/************************************************************************                                                       
** Programa : twp/twin791
** Objetivo : Ao incluir/alterar registro marcar flag para exportar os dados
**            Ao Sistema ARIBA
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
DEFINE PARAM BUFFER b-mla-hierarquia-faixa-old   FOR mla-hierarquia-faixa.

DEFINE VARIABLE l-igual AS LOGICAL     NO-UNDO.



BUFFER-COMPARE b-mla-hierarquia-faixa USING cod-usuar TO b-emitente-old SAVE RESULT IN l-igual.


IF NOT l-igual THEN
DO:
    FIND FIRST es-exporta-mla EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL es-exporta-mla THEN
        CREATE es-exporta-mla.
    

    ASSIGN es-exporta-mla.ind-exportar = YES.

    FIND CURRENT es-exporta-mla NO-LOCK NO-ERROR.

END.


