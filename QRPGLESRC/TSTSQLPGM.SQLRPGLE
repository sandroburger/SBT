     HDEBUG
     H*VARCHAR
      *$$\CRTOM CRTSQLRPGI
      *$$ REPLACE(*YES)
      *SS DBGVIEW(*SOURCE)
      *$$ USRPRF(*OWNER)
      *$$ COMMIT(*NONE)
      *$$ TGTRLS(V5R3M0)
      *$$\ENDOM
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: Test programmaatje                            *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ------------------------------------------------------------ *
     d S#NAME          s            128    Varying
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *

     c/exec sql
     c+                  declare Tables cursor for
     c+                   select KLANTNUMMER,
     c+                           KLANTNAAM
     c+                     from KLANTGEGEVENS
     c/end-exec

      * ----------------------------------------------------------- *
      * Process tables and fill OMQADSP                             *
      * ----------------------------------------------------------- *
     c/exec sql
     c+                  open Tables
     c/end-exec

     c                   exsr      GetSql
     c/exec sql
     c+                  close Tables
     c/end-exec
     c                   move      *on           *inlr
     c                   return

      * ----------------------------------------------------------- *

     c     GetSql        begsr
     c
     c                   exsr      FetchRow
     c                   dow       SQLCOD = 0 or SQLCOD = 30
      *
      *
      *
     c                   exsr      FetchRow
     c                   enddo
     c                   endsr

      * ----------------------------------------------------------- *

     c     FetchRow      begsr
     c/exec sql
     c+                  fetch Tables
     c+                   into :S#NUM,
     c+                         :S#NAME
     c/end-exec
     c                   endsr

      * ----------------------------------------------------------- *

     c     *inzsr        begsr

      *
     c                   MOVE      *BLANKS       S#NUM            10 0
     c                   MOVE      *BLANKS       S#TYPE           30
     c                   endsr
