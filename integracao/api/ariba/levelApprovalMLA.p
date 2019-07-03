/*------------------------------------------------------------------------
    File        : levelApprovalMLA.p
    Purpose     : API REST para exportar n¡vei de aprova‡Æo
    Syntax      :
    Description : Niveis de Hierarquia de Aprova‡Æo
    Author(s)   : Cleberson Silva
    Created     : 02/07/2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i levelApprovalMLA 2.00.00.000} /*** 010000 ***/

{utp/ut-api-action.i pi-create POST /~*}
{utp/ut-api-action.i pi-update PUT /~*}
{utp/ut-api-action.i pi-delete DELETE /~*/~*}
{utp/ut-api-action.i pi-get GET /~*/~*}
{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-notfound.i}

PROCEDURE pi-create:
END PROCEDURE.


PROCEDURE pi-update:
END PROCEDURE. 

PROCEDURE pi-delete:
END PROCEDURE. 

PROCEDURE pi-get:
END PROCEDURE. 

PROCEDURE pi-getAll:
END PROCEDURE.
