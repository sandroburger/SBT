      * Changed on 20130327 1437 --------------------------
     FOMLOGL2   IF   E           K DISK
     FOMMBRL2   IF   E           K DISK
     FOMMRRL1   IF   E           K DISK
     FOMOBJL1   IF   E           K DISK
     FOMPDLL1   IF   E           K DISK
     FOMRSRJ6   IF   E           K DISK
     FDSP05     CF   E             WORKSTN
      *          ------------------------------------------
      *          Java remain.nl.Testjava
      *          ------------------------------------------
     DSourceRel        PR              O   CLASS(*JAVA:'nl.remain.td.Testjava')
     D                                     Extproc(*JAVA:'nl.remain.td.Testjava'+
     D                                     :*CONSTRUCTOR)
     C*
     C                   READ      OMLOGL2                                99
     C                   READ      OMMBRL2                                99
     C                   READ      OMMRRL1                                99
     C                   READ      OMOBJL1                                99
     C                   READ      OMPDLL1                                99
     C                   READ      OMRSRJ6                                99
     C*
     C                   EXFMT     DSP
     C*
     C                   SETON                                        LR
