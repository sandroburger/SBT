      *$$\CRTOM CRTSQLRPGI
      *$$ COMMIT(*NONE)
      *$$ REPLACE(*YES)
      *$$ USRPRF(*OWNER)
      *$$ DYNUSRPRF(*OWNER)
      *$$ TGTRLS(V5R3M0)
      *$$ DBGVIEW(*SOURCE)
      *$$ COMPILEOPT('DFTACTGRP(*NO) ACTGRP(*NEW)')
      *$$\ENDOM
      * ------------------------------------------------------------- *
      * Description                                                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * Program ....: OMC018                                          *
      * Function ...: Type action selection                           *
      * Copyright...: REMAIN bv.                                      *
      * Startdate ..: 06-10-2011                                      *
      * Parameters .: P#TYPI - Type code indicator                    *
      *               W3XBUF - Exception buffer                       *
      *               P#TYPC - Type code                              *
      *               P#AKYT - Action key type                        *
      *               P#AKY# - Action key number                      *
      *               P#IRPC - Impact report code                     *
      *               P#STAT - Return status                          *
      *                        *TERM - Error                          *
      *                        *NORM - Normal                         *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Remarks on the use of some indicator fields                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * I2PROC - Process generic object code (used in SRST01)         *
      *                                                               *
      * ------------------------------------------------------------- *
      * File specifications                                           *
      * ------------------------------------------------------------- *
      *
     FOMACTL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(ACTL1)
     FOMASLL3   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(ASLL3)
     FOMASVL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(ASVL1)
     FOMTYPL2   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(TYPL2)
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Extension specifications                                      *
      * ------------------------------------------------------------- *
      *                                                               *
      *                ----------------------------------------------
      *                Arrays for *LIST field search and data access
      *                ----------------------------------------------
      *
     D SX              S              3S 0 DIM(9)
     D LSTI            S             11    DIM(9) CTDATA
     D SLPI            S              3S 0 DIM(9) CTDATA PERRCD(9)
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Input specifications                                          *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFAM)
     D               E DS                  EXTNAME(OMSREFNZ)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D ACTL1           DS            57
     D AGSL1           DS            57
     D AGOL1           DS            57
     D ASLL3           DS            57
     D ASVL1           DS            57
     D TYPL2           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  D#PARM           *PARMS
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Error buffer for API call
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Exception buffer version V5R1M00
      *          ------------------------------------------
      /COPY QCPYLESRC,XW3V5R1M00
      *
      *          ------------------------------------------
      *          Exception buffer without further structure
      *          ------------------------------------------
     D W3XBUF          DS          1024
     D P#XBUF          DS          1024
     D S#XBUF          DS          1024
      *
      *          ------------------------------------------
      *          Host structure
      *          ------------------------------------------
     D D#ASL         E DS                  EXTNAME(OMASL)
      *
      *          ------------------------------------------
      *          formatted positions of LSTI array element
      *          ------------------------------------------
     D D#LSTI          DS
     D  LSTP                   1      2
     D  SLSP                   3      5S 0
     D  W3SP                   6      8S 0
     D  LSTL                   9     11S 0
      *
      *          ------------------------------------------
      *          DS to format fields with +/-   content
      *          according to the sequence of LSTI array
      *          ------------------------------------------
     D D#SLPN          DS
     D  #LPNOB                 1      1
     D  #LPNTP                 2      2
     D  #LPNAT                 3      3
     D  #LPNMB                 4      4
     D  #LPNLB                 5      5
     D  #LPNAP                 6      6
     D  #LPNPD                 7      7
     D  #LPNMA                 8      8
     D  #LPNFX                 9      9
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#WARN          C                   CONST('*WARN')
      *
     D C#ALL           C                   CONST('*ALL')
     D C#ASTR          C                   CONST('*')
     D C#LIST          C                   CONST('*LIST')
      *
     D C#NN            C                   CONST('*N')
      *
     D C#MSGQ          C                   CONST('OMSMSG    *LIBL     ')
      *
     D C#V300          C                   CONST('*V3R0M0 ')
     D C#V510          C                   CONST('*V5R1M0 ')
      *
      * Selection attribute value
     D C#APPCV         C                   CONST(1)
     D C#PDLCV         C                   CONST(2)
     D C#OBJLV         C                   CONST(4)
     D C#FIX#V         C                   CONST(8)
      *
     D C#OCLSV         C                   CONST(16)
     D C#OBJTV         C                   CONST(32)
     D C#OBJAV         C                   CONST(64)
     D C#OBJCV         C                   CONST(128)
     D C#MBRCV         C                   CONST(256)
     D C#MBRAV         C                   CONST(512)
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Main line                                                     *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    P#TYPI
     C                   PARM                    W3XBUF
     C                   PARM                    P#TYPC
     C                   PARM                    P#AKYT
     C                   PARM                    P#AKY#
     C                   PARM                    P#IRPC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Exit if no parameters
      *          ------------------------------------------
     C     D#PARM        IFEQ      *ZEROS                                       B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRVERW - Main program processing routine                      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Initialize return status to *NORM
      *          ------------------------------------------
     C                   MOVE      C#NORM        P#STAT
     C                   MOVE      *BLANKS       P#TYPC
      *
      *          ------------------------------------------
      *          Put buffer in data structure
      *          ------------------------------------------
     C                   MOVEL     W3XBUF        X5R1M0
      *
      *          ------------------------------------------
      *          Declare cursor when necessary
      *          Processing step is not evaluated for
      *          Type selections
      *          ------------------------------------------
     C                   EXSR      SRCRDC
      *
      *          ------------------------------------------
      *          Process
      *          ------------------------------------------
      *
     C                   EXSR      SRST01
      *
      *          ------------------------------------------
      *          Check if type is found and exists
      *          ------------------------------------------
      *
     C                   IF        W#AKY# > 0
     C                   EVAL      K#AKYT = P#TYPI
     C                   EVAL      K#AKY# = W#AKY#
      *
     C     $YP2K1        CHAIN     OMTYPL2
     C                   IF        %FOUND(OMTYPL2)
     C                   EVAL      P#TYPC = YPTYPC
     C                   EVAL      P#AKYT = YPAKYT
     C                   EVAL      P#AKY# = YPAKY#
     C                   EVAL      P#IRPC = YPIRPC
     C                   EVAL      P#STAT = C#NORM
      *          ------------------------------------------
      *          Type code does not exist
      *          ------------------------------------------
     C                   ELSE
     C                   EVAL      P#STAT = C#TERM
     C                   ENDIF
      *          ------------------------------------------
      *          No selection found
      *          ------------------------------------------
     C                   ELSE
     C                   EVAL      P#STAT = C#TERM
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCRDC - Declare cursor for processing steps *N               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCRDC        BEGSR
      *
      *          ------------------------------------------
      *          Close cursor when a cursor is still open
      *          ------------------------------------------
     C     I#CROP        IFEQ      *ON                                          B01
     C                   EXSR      SRCRCL
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Declare cursor
      *          ------------------------------------------
     C/EXEC SQL
      *
     C+ DECLARE C01 DYNAMIC SCROLL CURSOR FOR
      *
     C+    SELECT *
      *
     C+    FROM OMASL
      *
     C+    WHERE (SLPRCS = '*N') AND
      *
     C+          (SLAWHN = '0') AND
      *
     C+          (SLAWHR = '0') AND
      *
     C+          (:P#TYPI = SLAKYT) AND
      *
     C+          ((:#5OBJC = SLOBJC AND SLPNOB = '+') OR
     C+           (:#5OBJC <> SLOBJC AND SLPNOB = '-') OR
     C+           (SLOBJC LIKE '%*%')) AND
      *
     C+          ((:#5OBJT = SLOBJT AND SLPNTP = '+') OR
     C+           (:#5OBJT <> SLOBJT AND SLPNTP = '-') OR
     C+           (SLOBJT = '*LIST') OR
     C+           (SLOBJT = '*ALL')) AND
      *
     C+          ((:#5OBJA = SLOBJA AND SLPNAT = '+') OR
     C+           (:#5OBJA <> SLOBJA AND SLPNAT = '-') OR
     C+           (SLOBJA = '*LIST') OR
     C+           (SLOBJA = '*ALL')) AND
      *
     C+          ((:#5MBRC = SLMBRC AND SLPNMB = '+') OR
     C+           (:#5MBRC <> SLMBRC AND SLPNMB = '-') OR
     C+           (SLMBRC = '*LIST') OR
     C+           (SLMBRC = '*ALL')) AND
      *
     C+          ((:#5TOOL = SLOBJL AND SLPNLB = '+') OR
     C+           (:#5TOOL <> SLOBJL AND SLPNLB = '-') OR
     C+           (SLOBJL = '*LIST') OR
     C+           (SLOBJL = '*ALL')) AND
      *
     C+          ((:#5OCLS = SLOCLS AND SLPNCL = '+') OR
     C+           (:#5OCLS <> SLOCLS AND SLPNCL = '-') OR
     C+           (SLOCLS = '*')) AND
      *
     C+          ((:#5VRS# = SLVRS# AND SLPNVR = '+') OR
     C+           (:#5VRS# <> SLVRS# AND SLPNVR = '-') OR
     C+           (SLVRS# = 0)) AND
      *
     C+          ((:#5APPC = SLAPPC AND SLPNAP = '+') OR
     C+           (:#5APPC <> SLAPPC AND SLPNAP = '-') OR
     C+           (SLAPPC = '*LIST') OR
     C+           (SLAPPC = '*ALL')) AND
      *
     C+          ((:#5TOPL = SLPDLC AND SLPNPD = '+') OR
     C+           (:#5TOPL <> SLPDLC AND SLPNPD = '-') OR
     C+           (SLPDLC = '*LIST') OR
     C+           (SLPDLC = '*ALL')) AND
      *
     C+          ((:P#DIRC = SLDIRC AND SLPNDR = '+') OR
     C+           (:P#DIRC <> SLDIRC AND SLPNDR = '-') OR
     C+           (SLDIRC = '*')) AND
      *
     C+          ((:#5MBRA = SLMBRA AND SLPNMA = '+') OR
     C+           (:#5MBRA <> SLMBRA AND SLPNMA = '-') OR
     C+           (SLMBRA = '*LIST') OR
     C+           (SLMBRA = '*ALL')) AND
      *
     C+          ((:#5FIX# = SLFIX# AND SLPNFX = '+') OR
     C+           (:#5FIX# <> SLFIX# AND SLPNFX = '-') OR
     C+           (SLFIX# = '*LIST') OR
     C+           (SLFIX# = '*ALL')) AND
      *
     C+          ((:#5SOLT = SLSOLT AND SLPNSO = '+') OR
     C+           (:#5SOLT <> SLSOLT AND SLPNSO = '-') OR
     C+           (SLSOLT = '1'))
      *
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Open the cursor
      *          ------------------------------------------
     C/EXEC SQL
     C+ OPEN C01
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Indicate that a cursor has been opened
      *          ------------------------------------------
     C                   MOVE      *ON           I#CROP
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCRCL - Close cursor C01                                     *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCRCL        BEGSR
      *
      *          ------------------------------------------
      *          Close cursor
      *          ------------------------------------------
     C/EXEC SQL
     C+ CLOSE C01
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Indicate that cursor has been closed
      *          ------------------------------------------
     C                   MOVE      *OFF          I#CROP
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRST01 - Process cursor - steps *N Type code selection        *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRST01        BEGSR
      *
      *          ------------------------------------------
      *          Fetch first record into data structure
      *          ------------------------------------------
     C                   EVAL      W#AKY# = 0
     C                   EVAL      W#SVAL = 0
      *
     C/EXEC SQL
     C+ FETCH FIRST FROM C01 INTO :D#ASL
     C/END-EXEC
      *
      *          ------------------------------------------
      *          If return code is negative then call the
      *          subroutine to log a message and close
      *          the program
      *          ------------------------------------------
     C     SQLCOD        IFLT      *ZEROS                                       B01
     C                   EXSR      SRSQLE
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do while end of file not yet reached
      *          ------------------------------------------
     C     SQLCOD        DOWNE     100                                          B01
      *
      *          ------------------------------------------
      *          If this action is for the current step
      *          ------------------------------------------
     C     SLPRCS        IFEQ      '*N'                                         B02
      *
     C                   EVAL      I#QUIT = *OFF
      *          ------------------------------------------
      *          Check if object code is a generic code
      *          Start on position 2 to skip value *ALL
      *          ------------------------------------------
     C     '*'           SCAN      SLOBJC:2                               99
      *
      *          ------------------------------------------
      *          If generic code, then evaluate
      *          ------------------------------------------
     C     *IN99         IFEQ      *ON                                          B03
     C                   EXSR      SRGNR1
     C     I2PROC        IFEQ      *ON                                          B04
      *          ------------------------------------------
      *          If object code is generic and valid
      *          *LIST values have to be tested next
      *          ------------------------------------------
     C                   EXSR      SRLSTV1
     C                   ELSE                                                   X04
     C                   EVAL      I#QUIT = *ON
     C                   ENDIF                                                  E04
      *          ------------------------------------------
      *          If object code is not generic, all possible
      *          *LIST containing fields have to be tested
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EVAL      W#GENV = 12
     C                   EXSR      SRLSTV1
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          if evaluation has turned out that selection
      *          should be processed, then continue
      *          ------------------------------------------
     C     I#QUIT        IFEQ      *OFF                                         B03
      *
      *          ------------------------------------------
      *          Calculate the selection value:
      *          The value each entity represent a given value
      *          ------------------------------------------
     C                   EVAL      W#SVAL = 0
      *          ------------------------------------------
      *          Application
      *          ------------------------------------------
     C                   IF        SLAPPC <> '*ALL' AND SLPNAP = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#APPCV
     C                   ENDIF
      *          ------------------------------------------
      *          Environment
      *          ------------------------------------------
     C                   IF        SLPDLC <> '*ALL' AND SLPNPD = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#PDLCV
     C                   ENDIF
      *          ------------------------------------------
      *          Library
      *          ------------------------------------------
     C                   IF        SLOBJL <> '*ALL' AND SLPNLB = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#OBJLV
     C                   ENDIF
      *          ------------------------------------------
      *          Fix
      *          ------------------------------------------
     C                   IF        SLFIX# <> '*ALL' AND SLPNFX = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#FIX#V
     C                   ENDIF
      *          ------------------------------------------
      *          Object class
      *          ------------------------------------------
     C                   IF        SLOCLS <> '*' AND SLPNCL = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#OCLSV
     C                   ENDIF
      *          ------------------------------------------
      *          Object type
      *          ------------------------------------------
     C                   IF        SLOBJT <> '*ALL' AND SLPNTP = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#OBJTV
     C                   ENDIF
      *          ------------------------------------------
      *          Object attribute
      *          ------------------------------------------
     C                   IF        SLOBJA <> '*ALL' AND SLPNAT = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#OBJAV
     C                   ENDIF
      *          ------------------------------------------
      *          Object code
      *          ------------------------------------------
     C                   IF        SLOBJC <> '*ALL' AND SLPNOB = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#OBJCV
     C                   ENDIF
      *          ------------------------------------------
      *          Object detail
      *          ------------------------------------------
     C                   IF        SLMBRC <> '*ALL' AND SLPNMB = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#MBRCV
     C                   ENDIF
      *          ------------------------------------------
      *          Object detail attribute
      *          ------------------------------------------
     C                   IF        SLMBRA <> '*ALL' AND SLPNMA = '+'
     C                   EVAL      W#SVAL = W#SVAL + C#MBRCV
     C                   ENDIF
      *          ------------------------------------------
      *          Save the action key field if action is higher
      *          as the previously saved action
      *          ------------------------------------------
     C                   IF        (W#SVAL > S#SVAL OR W#AKY# = 0)
     C                               OR (W#SVAL = S#SVAL AND
     C                               W#GENV > S#GENV)
     C                   EVAL      S#SVAL = W#SVAL
     C                   EVAL      S#GENV = W#GENV
     C                   EVAL      W#AKY# = SLAKY#
     C                   ENDIF
      *
     C                   ENDIF                                                  E03
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fetch next record into data structure
      *          ------------------------------------------
     C/EXEC SQL
     C+ FETCH NEXT FROM C01 INTO :D#ASL
     C/END-EXEC
      *
      *          ------------------------------------------
      *          End of fetch loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRGNR1 - Evaluate generic object code                         *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRGNR1        BEGSR
      *
      *          ------------------------------------------
      *          Initialize boundaries
      *          ------------------------------------------
     C                   MOVE      *LOVAL        W#OBCL
     C                   MOVE      *HIVAL        W#OBCH
      *
      *          ------------------------------------------
      *          Initialize 'Process Y/N' to *OFF
      *          ------------------------------------------
     C                   MOVE      *OFF          I2PROC
      *
      *          ------------------------------------------
      *          Initialize generic selection weight value
      *          ------------------------------------------
     C                   EVAL      W#GENV = 0
      *
      *          ------------------------------------------
      *          Locate the generic symbol, starting on
      *          position 2 of the object code
      *          ------------------------------------------
     C     '*'           SCAN      SLOBJC:2      P                        99
      *
      *          ------------------------------------------
      *          Subtract 1 to get the length of the
      *          string that should be substringed
      *          ------------------------------------------
     C     P             SUB       1             LEN
     C                   EVAL      W#GENV = LEN
      *
      *          ------------------------------------------
      *          Substring the relevant positions
      *          ------------------------------------------
     C     LEN           SUBST     SLOBJC        W#OBCL
     C     LEN           SUBST     SLOBJC        W#OBCH
      *
      *          ------------------------------------------
      *          Evaluate
      *          ------------------------------------------
     C     W#OBCL        IFLE      #5OBJC                                       B01
     C     W#OBCH        ANDGE     #5OBJC
     C     SLPNOB        ANDEQ     '+'
     C     W#OBCL        ORGT      #5OBJC                                       B03
     C     SLPNOB        ANDEQ     '-'
     C     W#OBCH        ORLT      #5OBJC
     C     SLPNOB        ANDEQ     '-'
     C                   MOVE      *ON           I2PROC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      *
      * ----------------------------------------------------------- *
      * SRLSTV1 - Check only for *LIST containing fields            *
      *           multiple detail values against single             *
      *           XW3 buffer value                                  *
      * ----------------------------------------------------------- *
     C     SRLSTV1       BEGSR
      *
     C                   MOVE      SLPNOB        #LPNOB
     C                   MOVE      SLPNTP        #LPNTP
     C                   MOVE      SLPNAT        #LPNAT
     C                   MOVE      SLPNLB        #LPNLB
     C                   MOVE      SLPNMB        #LPNMB
     C                   MOVE      SLPNMA        #LPNMA
     C                   MOVE      SLPNAP        #LPNAP
     C                   MOVE      SLPNPD        #LPNPD
     C                   MOVE      SLPNFX        #LPNFX
      *          ------------------------------------------
      *          scan DS with *LIST containing fields
      *          determining current *LIST values in array SX
      *          ------------------------------------------
     C                   MOVEA     *ZEROS        SX
     C     C#LIST        SCAN      D#ASL         SX                       98
      *          ------------------------------------------
      *        looping only *LIST containing field and extracting
      *         the start and end positions from LSTP array
      *          ------------------------------------------
     C                   EVAL      S  = 1
     C     SX(S)         DOWNE     *ZERO                                        B01
     C     S             ANDLT     10                                           B01
      *
     C                   EVAL      SP = SX(S)
     C                   EVAL      L =  %LOOKUP(SP:SLPI:1)
     C                   IF        (L  > 0)                                     B02
     C                   MOVEL     LSTI(L)       D#LSTI
      *
     C                   EVAL      W#SLPN =  %SUBST(D#SLPN:L:1)
      *          ------------------------------------------
      *          extract content of corresponding field in X5R1M0
      *          ------------------------------------------
     C                   EVAL      K#SELS =  %SUBST(X5R1M0:W3SP:LSTL)
     C                   EVAL      K#SELT = LSTP
      *          ------------------------------------------
      *          Set pointer to first action selection *LIST value
      *          for current type of property
      *          ------------------------------------------
     C     $SV1K1        SETLL     OMASVL1                                98
      *          ------------------------------------------
      *          the MISMATCH is now checked
      *          if so, leave the loop and quit selection
      *          ------------------------------------------
     C                   IF        (*IN98 = *OFF) AND                           B03
     C                             (W#SLPN = '+') OR
     C                             (*IN98 = *ON) AND
     C                             (W#SLPN = '-')
     C                   EVAL      I#QUIT = *ON
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *
     C                   ENDIF                                                  E02
      *
     C                   EVAL      S  = S + 1
     C                   ENDDO                                                  E01
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSPGM - Send program message                                * /
      *                                                              * /
      * This routine calls the send program message API (QMHSNDPM).  * /
      *                                                              * /
      * Input      - P#MIDC  Message id code                         *
      *              P#MSGQ  Message file qualifier                  *
      *              P#MDT   Message data or impromptu text buffer   *
      *              P#MDL#  Length of buffer                        *
      *              P#MSTC  Message type code                       *
      *              P#PMQC  Program message queue qualifier         *
      *              P#PSC#  Program stack counter                   *
      *              P#ERR   Error buffer                            *
      *                                                              *
      * Output       P#MSKC  Message key code                        *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSPGM        BEGSR
      *
      *          ------------------------------------------
      *          Send program message
      *          ------------------------------------------
     C                   CALL      'QMHSNDPM'
     C                   PARM                    P#MIDC
     C                   PARM                    P#MSGQ
     C                   PARM                    P#MDT
     C                   PARM                    P#MDL#
     C                   PARM                    P#MSTC
     C                   PARM                    P#PMQC
     C                   PARM                    P#PSC#
     C                   PARM                    P#MSKC
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------- *
      * SRSQLE - Handle SQL error                                     *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRSQLE        BEGSR
      *
      *          ------------------------------------------
      *          Send message by calling API
      *          ------------------------------------------
     C                   MOVE      'SQL0001'     P#MIDC
     C                   MOVE      C#MSGQ        P#MSGQ
     C                   MOVEL(P)  PGMQ          P#MDT
     C                   Z-ADD     10            P#MDL#
     C                   MOVEL(P)  '*INFO'       P#MSTC
     C                   MOVEL     '*     '      P#PMQC
     C                   Z-ADD     0             P#PSC#
     C                   EXSR      SRSPGM
      *
      *          ------------------------------------------
      *          Log the message
      *          ------------------------------------------
     C                   CALL      'OMX928C'
      *
      *          ------------------------------------------
      *          Close the program
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------- *
      * *INZSR - Program initialization                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Explicit numeric field definitions
      *          ------------------------------------------
     C                   Z-ADD     *ZEROS        LEN               2 0
     C                   Z-ADD     *ZEROS        P                 2 0
     C                   Z-ADD     *ZEROS        L                 2 0
     C                   Z-ADD     *ZEROS        S                 2 0
     C                   Z-ADD     *ZEROS        SP                3 0
     C                   Z-ADD     *ZEROS        W#SVAL            5 0
     C                   Z-ADD     *ZEROS        S#SVAL            5 0
     C                   Z-ADD     *ZEROS        W#GENV            2 0
     C                   Z-ADD     *ZEROS        S#GENV            2 0
      *
      *          ------------------------------------------
      *          Explicit character field definitions
      *          ------------------------------------------
     C                   MOVE      *BLANKS       P#MDT            50
      *
      *          ------------------------------------------
      *          Reserved field definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
     C     *LIKE         DEFINE    DIRC          P#DIRC
     C     *LIKE         DEFINE    INDI          P#MBRP
     C     *LIKE         DEFINE    PGML          P#PGML
     C     *LIKE         DEFINE    PGMN          P#PGMN
     C     *LIKE         DEFINE    PRCS          P#PRCS
     C     *LIKE         DEFINE    TYPC          P#TYPC
     C     *LIKE         DEFINE    TYPI          P#TYPI
     C     *LIKE         DEFINE    AKYT          P#AKYT
     C     *LIKE         DEFINE    AKY#          P#AKY#
     C     *LIKE         DEFINE    IRPC          P#IRPC
     C     *LIKE         DEFINE    STAT          P#STAT
     C     *LIKE         DEFINE    STAT          P1STAT
      *
      *          ------------------------------------------
      *          Save field definition
      *          ------------------------------------------
     C     *LIKE         DEFINE    APS#          S#APS#
      *
      *          ------------------------------------------
      *          Key field definition
      *          ------------------------------------------
     C     *LIKE         DEFINE    ACT#          K#ACT#
     C     *LIKE         DEFINE    AGR#          K#AGR#
     C     *LIKE         DEFINE    SELT          K#SELT
     C     *LIKE         DEFINE    SELS          K#SELS
     C     *LIKE         DEFINE    AKYT          K#AKYT
     C     *LIKE         DEFINE    AKY#          K#AKY#
      *
      *          ------------------------------------------
      *          Indicator definitions
      *          ------------------------------------------
     C     *LIKE         DEFINE    INDI          I#QUIT
     C     *LIKE         DEFINE    INDI          I#CROP
     C     *LIKE         DEFINE    INDI          I2PROC
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C     *LIKE         DEFINE    PRCS          W#CRSR
     C     *LIKE         DEFINE    OBJC          W#OBCL
     C     *LIKE         DEFINE    OBJC          W#OBCH
     C     *LIKE         DEFINE    OBJC          W#SLFD
     C     *LIKE         DEFINE    PNOB          W#SLPN
     C     *LIKE         DEFINE    AKY#          W#AKY#
      *
      *          ------------------------------------------
      *          Data area definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          OMASVL1 - Action selection *LIST values
      *          ------------------------------------------
     C     $SV1K1        KLIST
     C                   KFLD                    SLASL#
     C                   KFLD                    K#SELT
     C                   KFLD                    K#SELS
      *
      *          ------------------------------------------
      *          OMTYPL2 - Type code
      *          ------------------------------------------
     C     $YP2K1        KLIST
     C                   KFLD                    K#AKY#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *PSSR  - Error handler                                        *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine
      *          ------------------------------------------
     C     I#PSSR        IFNE      *ON                                          B01
      *
      *          ------------------------------------------
      *          Indicate that this block was processed
      *          ------------------------------------------
     C                   MOVE      *ON           I#PSSR            1
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
      *
     C     E#FILE        WHENEQ    'OMACTL1 '                                   X02
     C                   MOVEL     ACTL1         P#FILE
      *
     C     E#FILE        WHENEQ    'OMASLL3 '                                   X02
     C                   MOVEL     ASLL3         P#FILE
      *
     C     E#FILE        WHENEQ    'OMASVL1 '                                   X02
     C                   MOVEL     ASVL1         P#FILE
      *
     C     E#FILE        WHENEQ    'OMTYPL2 '                                   X02
     C                   MOVEL     TYPL2         P#FILE
      *
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALL      'OMX961'
     C                   PARM                    PGM
     C                   PARM                    P#FILE           57
      *
      *          ------------------------------------------
      *          Move the messages to the queue of the caller
      *          ------------------------------------------
     C                   MOVEL     '*COMP'       P#MSTA           40
     C     P#MSTA        CAT       '*DIAG':5     P#MSTA
     C     P#MSTA        CAT       '*ESCA':5     P#MSTA
     C     P#MSTA        CAT       'PE':0        P#MSTA
     C     P#MSTA        CAT       '*INFO':3     P#MSTA
     C                   CALL      'QMHMOVPM'
     C                   PARM      *BLANKS       P#MSKC
     C                   PARM                    P#MSTA
     C                   PARM      4             P#LOB#
     C                   PARM      '*'           P#PMQC
     C                   PARM      1             P#PSC#
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Indicate abnormal ending
      *          ------------------------------------------
     C                   MOVE      C#TERM        P#STAT
      *
      *          ------------------------------------------
      *          Force 'closing call' processing in SRTTLR
      *          ------------------------------------------
     C                   Z-ADD     *ZEROS        D#PARM
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRTTLR - Total last record processing                         *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          If this was a closing call
      *          ------------------------------------------
     C     D#PARM        IFEQ      *ZEROS                                       B01
      *
      *          ------------------------------------------
      *          Close cursor
      *          ------------------------------------------
     C/EXEC SQL
     C+ CLOSE C01
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Close programs if necessary
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Seton last record
      *          ------------------------------------------
     C                   SETON                                        LR
      *
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to calling program
      *          ------------------------------------------
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * Compile-time arrays                                          *
      * ------------------------------------------------------------ *
**
OB011050012
OT024062009
OA034071010
MB045081010
OL056154010
AP073015005
PD079143005
MA093348010
FX104040010
**
011024034045056073079093104
