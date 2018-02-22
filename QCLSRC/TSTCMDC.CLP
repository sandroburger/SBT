 /*   *$$\CRTOM CRTCLPGM    */
 /*   *$$ USRPRF(*OWNER)    */
 /*   *$$ REPLACE(*YES)     */
 /*   *$$\ENDOM             */

 /* ---------------------------------------------------------------- */
 /* Initialization                                                   */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Program start                                           */
          /* ------------------------------------------------------- */
             PGM

          /* ------------------------------------------------------- */
          /* Declare parameters                                      */
          /* ------------------------------------------------------- */
             DCL        VAR(&ROUT) TYPE(*CHAR) LEN(50)
             DCL        VAR(&TDIR) TYPE(*CHAR) LEN(50)
             DCL        VAR(&STR) TYPE(*CHAR) LEN(100)
             CHGVAR     VAR(&STR) VALUE(&ROUT *CAT &TDIR)

             TSTCMD     USER(test)

             SBMJOB     CMD(PRTASPGM PGM(TEST))
             SBMJOB     CMD(CALL PGM(PGM05))




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
