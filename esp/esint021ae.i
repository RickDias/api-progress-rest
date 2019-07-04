DEF TEMP-TABLE consulta-fornecedor NO-UNDO SERIALIZE-NAME "Consulta_Fornecedor"
    //FIELD CreationDateTime                    AS CHAR SERIALIZE-NAME "CreationDateTime"
    //FIELD PollingRequestDetails               AS CHAR SERIALIZE-NAME "PollingRequestDetails timestamp"
    FIELD PollingMessage                      AS INT64 SERIALIZE-NAME "PollingRequestDetails timestamp"
    FIELD InboundServiceName                  AS CHAR SERIALIZE-NAME "InboundServiceName".

DEF TEMP-TABLE cadastro-fornecedor NO-UNDO SERIALIZE-NAME "Cadastro_Fornecedor"
    FIELD Corporate-Name                      AS CHAR
    FIELD Trading-name                        AS CHAR
    FIELD Number                              AS CHAR
    FIELD CNPJ                                AS CHAR
    FIELD CPF                                 AS CHAR
    FIELD PIS-Number                          AS CHAR
    FIELD NIS-Number                          AS CHAR
    FIELD IE                                  AS CHAR
    FIELD State                               AS CHAR
    FIELD Street                              AS CHAR
    FIELD Complement                          AS CHAR
    FIELD District                            AS CHAR
    FIELD Zip-Code                            AS CHAR
    FIELD Country                             AS CHAR
    FIELD Pais                                AS CHAR
    FIELD CNAE-principal                      AS CHAR
    FIELD E-mail                              AS CHAR
    FIELD Simples-Nacional                    AS CHAR
    FIELD Municipality                        AS CHAR
    FIELD Nome-Responsavel                    AS CHAR
    FIELD Date-Birth                          AS CHAR
    FIELD Codigo-Pais                         AS CHAR
    FIELD Codigo-area                         AS CHAR
    FIELD Numero-Telefone                     AS CHAR
    FIELD Banco                               AS CHAR
    FIELD Agencia                             AS CHAR
    FIELD Dig-Agencia                         AS CHAR
    FIELD Conta-corrente                      AS CHAR
    FIELD Dig-conta-corrente                  AS CHAR
    FIELD DeletedIndicator                    AS CHAR
    FIELD BlockedIndicator                    AS CHAR
    FIELD BuildingID                          AS CHAR
    FIELD POBoxDeviatingCityName              AS CHAR
    FIELD ID                                  AS CHAR
    FIELD UUID                                AS CHAR
    FIELD ID_1                                AS CHAR
    FIELD UUID_1                              AS CHAR
    FIELD UUID_2                              AS CHAR
    FIELD ReceiverUUID                        AS CHAR
    FIELD ReceiverInternalID                  AS CHAR
    FIELD TaxGroupCode                        AS CHAR
    FIELD PollingMessage                      AS CHAR.
                                              
