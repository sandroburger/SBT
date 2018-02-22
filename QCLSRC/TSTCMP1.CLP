 /*   *$$\CRTOM CRTCLPGM    */
 /*   *$$ USRPRF(*OWNER)    */
 /*   *$$ REPLACE(*YES)     */
 /*   *$$\ENDOM             */

 /* ---------------------------------------------------------------- */
 /* Initialization.                                                  */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Program start                                           */
          /* ------------------------------------------------------- */
             PGM        PARM(&ROUT &TDIR)
             DCLF       FILE(ABCD1234)

          /* ------------------------------------------------------- */
          /* Declare parameters                                      */
          /* ------------------------------------------------------- */
             DCL        VAR(&ROUT) TYPE(*CHAR) LEN(50)
             DCL        VAR(&TDIR) TYPE(*CHAR) LEN(50)
             DCL        VAR(&STR) TYPE(*CHAR) LEN(100)
             CHGVAR     VAR(&STR) VALUE(&ROUT *CAT &TDIR)
             DSPPGMVAR  PGMVAR((&ROUT ()))
             DSPPGMVAR  PGMVAR((&TDIR ()))
          /* ------------------------------------------------------- */
          /* Install the global error monitor.                       */
          /* CPF0000 for system escape messages                      */
          /* ------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDPGM))

 /* ---------------------------------------------------------------- */
 /* Processing starts here.                                          */
 /* ---------------------------------------------------------------- */
 ENDPGM:
             ENDPGM
