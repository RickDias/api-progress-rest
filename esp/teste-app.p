DEF VAR h-esint002 AS HANDLE no-undo.
DEF VAR p-token AS CHARACTER NO-UNDO.

COMPILE esp/esint002.p SAVE.

run esp/esint002.p PERSISTEN SET h-esint002.

RUN piGeraTokenApigee IN h-esint002 (OUTPUT p-token).

MESSAGE ">>>>TOKKEENN".
