/*----------------------------------------------------------------------------------------------/
 Programa..: esint001ai.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 26/02/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{esp\esint001a.i}
                    
DEFINE DATASET dsEmitente
    FOR tt_emitente
        /*
        , tt_endereco_list, tt_condicao_pagto, tt_contato
        DATA-RELATION drEndEmit  FOR tt_emitente, tt_endereco_list
            RELATION-FIELDS(fld-rel, fld-rel) NESTED FOREIGN-KEY-HIDDEN

        DATA-RELATION drCodPg   FOR tt_emitente, tt_condicao_pagto
            RELATION-FIELDS(fld-rel, fld-rel) NESTED FOREIGN-KEY-HIDDEN

        DATA-RELATION drContato FOR tt_emitente, tt_contato
            RELATION-FIELDS(fld-rel, fld-rel) NESTED FOREIGN-KEY-HIDDEN*/.
