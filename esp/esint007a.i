
{include/i_fnctrad.i}
{utp/ut-glob.i}
{cdp/cd9900.i1}
{include/i_dbvers.i}                
{cdp/cdcfgfin.i}
{cdp/cdapi090def.i} /* Variaveis utilizadas na integra»’o EMS2/NEOLOG */
&if  "{&mgadm_version}" >= "2.02" 
&then
    {cdp/cd7300.i1}
    {cdp/cdapi270.i}
    def var i-tipo-movto as integer.
    assign c-transacao = "ADM111".
    {cdp/cd7300.i2}
    {cdp/cd7300.i3 "001" c-transacao}
&endif
{cdp/cdcfgmat.i} 
{cdp/cdcfgdis.i}
&if defined (bf_dis_fat_moeda) &then  /* pre-processador */   
    def var h-bo as handle no-undo.
    def var r-chave as rowid no-undo.
    {dibo/bodi275.i "rowObject" }
    {include/boerrtab.i}
&endif /* preprocessador */
{cdp/cd6667.i}
{include/i-sysvar.i}

DEF TEMP-TABLE tt-param
    FIELD destino          AS INTEGER
    FIELD arq-destino      AS CHAR
    FIELD arq-entrada      AS CHAR
    FIELD todos            AS INTEGER
    FIELD usuario          AS CHAR
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER.       


DEF VAR raw-param          AS RAW NO-UNDO.
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita       AS RAW.
