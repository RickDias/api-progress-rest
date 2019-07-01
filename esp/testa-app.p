
OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "teste-app.txt").

REPEAT:
    
    IF NOT CAN-FIND (FIRST es-api-exec WHERE es-api-exec.data-fim = ? ) THEN LEAVE.

    FOR FIRST ped-venda WHERE cod-estabel = '01'
                         AND nome-abrev MATCHES "XU*":

        DISP ped-venda.nome-abrev ped-venda.nr-pedcli.

    END.
END.

RETURN "OK".
