/***********************************************
 Defini‡Æo de temp-tables
************************************************/

DEFINE TEMP-TABLE tt-erro NO-UNDO
    FIELD cod-erro AS CHAR
    FIELD des-erro AS CHAR
    INDEX erro cod-erro.
/*Fim Temp-tables do Cadastro de campos especificos*/

DEFINE TEMP-TABLE tt-erros-integracao NO-UNDO
    FIELD erro AS CHAR
    FIELD descricao AS CHAR.
