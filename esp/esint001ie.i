/*----------------------------------------------------------------------------------------------/
 Programa..: esint00ie.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 02/06/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


DEFINE TEMP-TABLE tt_lead_time NO-UNDO
    fields lead-time          like es-gp-lead-time.lead-time              serialize-name "DiasEnvio"     
    fields cidade-dest        like es-gp-lead-time.cidade-dest            serialize-name "CidadeDestino"    
    fields cod-estabel-orig   like es-gp-lead-time.cod-estabel-orig       serialize-name "EstabelecimentoOrigem"      
    fields estado-dest        like es-gp-lead-time.estado-dest            serialize-name "EstadoDestino"
    fields peso-fim           like es-gp-lead-time.peso-fim               serialize-name "PesoFinal"
    fields peso-ini           like es-gp-lead-time.peso-ini               serialize-name "PesoInicial"
    fields cep-ini            like es-gp-lead-time.cep-ini                serialize-name "FaixaCEPde"       
    fields cep-fim            like es-gp-lead-time.cep-fim                serialize-name "FaixaCEPate"
    FIELDS cod-lead-time      AS CHARACTER                                SERIALIZE-NAME "CodigoLeadTime" .
