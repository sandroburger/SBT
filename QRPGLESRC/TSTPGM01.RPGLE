     H*VARCHAR
      *$$\CRTOM CRTBNDRPG
      *$$ REPLACE(*YES)
      *$$ DBGVIEW(*SOURCE)
      *$$ TGTRLS(V5R2M0)
      *$$ USRPRF(*OWNER)
      *$$\ENDOM
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *  Test programmatje tbv DB2 sql                              *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FKLANT00002UF   E           K DISK    Rename(KLANT00001:KLANTR)
     F                                     Prefix($)
      * ----------------------------------------------------------- *
      *
     C     *LOVAL        SETLL     KLANT00002
     C                   READ      KLANT00002
      *
     C                   IF        $KLANT00003 = 'N'
     C                   DELETE    KLANTR
     C                   ENDIF
      *
     C                   READ      KLANT00002
      *
     C                   SETON                                        LR
     C                   RETURN
      * ----------------------------------------------------------- *
      /EJECT
