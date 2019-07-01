DEF TEMP-TABLE tt_fornecedor_b2e NO-UNDO
    FIELD CNPJ                   AS c serialize-name "CNPJ"
    FIELD Corporate-Name         AS c serialize-name "Raz∆o Social"
    FIELD State                  AS c serialize-name "UF"
    FIELD CPF                    AS c serialize-name "CPF"
    FIELD NOME                   AS c serialize-name "NOME"
    FIELD nascimento             AS c serialize-name "Data de nascimento"
    FIELD tipo-fornecedor        AS c serialize-name "Descriá∆o tipo Fornecedor".

/*
{
  "CodigoPropostaCliente":"0005",
  "CodigoInstituicao":"8084A540-3ADE-4C4F-A8E7-39B80E95306C",
  "Proponente":{
     "RazaoSocial":"ESTADOS UNIDADO DA AMERICA",
     "CNPJ":"22482512000174",
     "Enderecos":[
        {
           "UF":"SP"
        }
     ]
  },
  "InformacoesAdicionais": [
    {
      "Grupo": null,
        "Nome": "tipo_fornecedor",
        "Valor": "descricao"
    }
  ]
}
*/
