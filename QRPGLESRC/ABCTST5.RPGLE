      *$$\CRTOM CRTBNDRPG
      *$$ CVTOPT(*DATETIME *VARCHAR)
      *$$ REPLACE(*YES)
      *$$ TGTRLS(*PRV)
      *$$ USRPRF(*OWNER)
      *$$ DBGVIEW(*NONE)
      *$$ OPTIMIZE(*BASIC)
      *$$ DFTACTGRP(*NO)
      *$$ ACTGRP(*CALLER)
      *$$\ENDOM
      * ------------------------------------------------------------- *
      * Description                                                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * Program ....: OMX025                                          *
      * Function ...: Retrieve constraints                            *
      * Author .....: Jan de Zeeuw                                    *
      * Startdate ..: 27-06-1996.                                     *
      * Create CMD .: CRTRPGPGM CVTOPT(*VARCHAR)                      *
      *                                                               *
      * Input ......: P#OBJC - Object code                            *
      *               P#OBJL - Object library                         *
      *               P#TOOL - Object library target                  *
      *               P#CMDT - Command type                           *
      *  ·························································    *
      *  Command type: 'R' = remove                                   *
      *                'A' = add                                      *
      *  ·························································    *
      *                                                               *
      * Output .....: P#RELI - Relation indicator                     *
      *  ·························································    *
      *  Relation indicator: 'D' = dependent file                     *
      *                      'P' = parent file                        *
      *  ·························································    *
      *               P#ROBC - Related object code                    *
      *               P#ROBL - Related object library                 *
      *               P#BUF  - Command                                *
      *               P#LOB# - Length of command                      *
      *               P#STAT - Status                                 *
      *  ·························································    *
      *  Status: *MORE = more constraints than this one exist for     *
      *                  file and can be retrieved                    *
      *          *NOTF = no constraints found for this file           *
      *          *NORM = this constraint is the last one              *
      *          *TERM = program ended in error                       *
      *                                                               *
      *   The statusfield can also be used as input:                  *
      *          *NEXT = give information about the next constraint   *
      *                  (only possible after returnvalue *MORE)      *
      *          *SAME = give information about the same constraint   *
      *                  (only possible after returnvalue *MORE       *
      *                  or *NORM)                                    *
      *  ·························································    *
      *                                                               *
      * This program retrieves for a specified file (PF) in a         *
      * specified library any related constraints, and returns        *
      * the appropriate command.                                      *
      *                                                               *
      * Status can be filled with *MORE. The program will not be      *
      * closed and the next call with status *NEXT will return        *
      * the next command.                                             *
      *                                                               *
      * The S#MORE field inidicates that a *MORE was returned.        *
      * The program should be closed by being invoked without         *
      * parameters.                                                   *
      *                                                               *
      * NOTE: because this program works with input fields of         *
      *       variable length, it must be created with the            *
      *       *VARCHAR value at the CVTOPT parameter in the           *
      *       CRTRPGPGM command.                                      *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Used files                                                    *
      * ------------------------------------------------------------- *
      *                                                               *
      * QAFDCST - System supplied file for DSPFD (I/O)                *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Used indicators.  0=false  1=true                             *
      * ------------------------------------------------------------- *
      *                                                               *
      * 99 - General purpose indicator.                               *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * File specifications                                           *
      * ------------------------------------------------------------- *
     FQAFDCST   IF   E             DISK    INFDS(FD)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     F                                     RECNO(F#RRN#)
      /EJECT
      * ------------------------------------------------------------- *
      * Array specifications.                                         *
      *                                                               *
      * OBJ   Contains returned related objects                       *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Input specifications                                          *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#PGM                  1     10
     D  E#FILE               201    208
     D  D#JOB#               264    269  0
     D FD              DS            57
      *
      *          ------------------------------------------
      *          Structure for buffer
      *          ------------------------------------------
     D P#BUF           DS          3036
      *
      *          ------------------------------------------
      *          Structure for retrieved related objects
      *          ------------------------------------------
     D P#OBJA          DS           640
     D  OBJ                    1    640
     D                                     DIM(64)
      *
      *
      *          ------------------------------------------
      *          Error buffer for API call.
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Definition RRN field
      *          ------------------------------------------
     D                 DS
     D  F#RRN#                 1      5  0
      *
      *          ------------------------------------------
      *          Check-Constraint
      *          ------------------------------------------
     D W#CKXP          DS          2000
      *
      *          ------------------------------------------
      *          Named constants for status field
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#NOTF          C                   CONST('*NOTF')
     D C#MORE          C                   CONST('*MORE')
     D C#NEXT          C                   CONST('*NEXT')
     D C#SAME          C                   CONST('*SAME')
      *
      *          ------------------------------------------
      *          Named constants constraint commands
      *          ------------------------------------------
     D C#ADD           C                   CONST('ADDPFCST')
     D C#RMV           C                   CONST('RMVPFCST')
      *
     D C#FILE          C                   CONST('FILE')
     D C#TYPE          C                   CONST('TYPE')
     D C#TYPR          C                   CONST('(*REFCST)')
     D C#TYPU          C                   CONST('(*UNQCST)')
     D C#TYPP          C                   CONST('(*PRIKEY)')
     D C#TYPC          C                   CONST('(*CHKCST)')
     D C#KEY           C                   CONST('KEY')
     D C#CST           C                   CONST('CST')
     D C#CHKC          C                   CONST('CHKCST')
     D C#PFIL          C                   CONST('PRNFILE')
     D C#PKEY          C                   CONST('PRNKEY')
     D C#DLTR          C                   CONST('DLTRULE')
     D C#UPDR          C                   CONST('UPDRULE')
     D C#NACT          C                   CONST('(*NOACTION)')
     D C#CASC          C                   CONST('(*CASCADE)')
     D C#SETD          C                   CONST('(*SETDFT)')
     D C#SETN          C                   CONST('(*SETNULL)')
     D C#REST          C                   CONST('(*RESTRICT)')
      *
     D C#RMVC          C                   CONST('RMVCST')
     D C#RMVE          C                   CONST('(*REMOVE)')
      *
      *          ------------------------------------------
      *          Named constants DSPFFD
      *          ------------------------------------------
     D C#FD01          C                   CONST('DSPFD FILE(')
     D C#FD02          C                   CONST(') TYPE(*CST)')
     D C#FD03          C                   CONST('OUTPUT(*OUTFILE)')
     D C#FD04          C                   CONST('FILEATR(*PF)')
     D C#FD05          C                   CONST('OUTFILE(QTEMP/QAFDCS-
     D                                     T)')
     D C#FD06          C                   CONST('OUTMBR(*FIRST *REPLA-
     D                                     CE)')
     D C#FD07          C                   CONST('OUTMBR(*FIRST *ADD)')
      *
      *          ------------------------------------------
      *          Named constants OVRDBF
      *          ------------------------------------------
     D C#OVRF          C                   CONST('OVRDBF FILE(QAFDCST)-
     D                                      TOFILE(QTEMP/QAFDCS-
     D                                     T)')
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      *          «««  mainline  »»»                                   *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    P#OBJC           10
     C                   PARM                    P#OBJL           10
     C                   PARM                    P#TOOL           10
     C                   PARM                    P#CMDT            1
     C                   PARM                    P#RELI            1
     C                   PARM                    P#ROBC           10
     C                   PARM                    P#ROBL           10
     C                   PARM                    P#BUF
     C                   PARM                    P#LOC#            4 0
     C                   PARM                    P#STAT            5
      *
      *          ------------------------------------------
      *          Exit if no parameters passed
      *          ------------------------------------------
     C     D#PARM        IFEQ      *ZEROS                                       B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check if status is allowed
      *          ------------------------------------------
     C     P#STAT        IFEQ      C#NEXT                                       B01
     C     S#STAT        ANDNE     C#MORE
     C     P#STAT        OREQ      C#SAME
     C     S#STAT        ANDNE     C#MORE
     C     S#STAT        ANDNE     C#NORM
     C                   MOVEL     C#TERM        P#STAT
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check if we have to refill the outfile
      *          ------------------------------------------
     C     P#OBJC        IFNE      S#OBJC                                       B01
     C     P#OBJL        ORNE      S#OBJL
      *
     C     P#STAT        IFEQ      '*NEXT'                                      B02
     C     P#STAT        OREQ      '*SAME'
     C                   MOVEL     C#TERM        P#STAT
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X02
     C                   EXSR      SROUTF
     C                   ENDIF                                                  E02
     C*
     C                   ELSE                                                   X01
     C     S#STAT        IFEQ      '*NOTF'                                      B02
     C                   MOVEL     C#TERM        P#STAT
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Position or reposition
      *          ------------------------------------------
     C     P#STAT        IFNE      '*NEXT'                                      B01
     C     P#STAT        ANDNE     '*SAME'
     C     1             SETLL     QWHFDCST
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Process request
      *          ------------------------------------------
     C     S#MORE        IFEQ      *ON                                          B01
     C                   EXSR      SRVERW
     C                   ELSE                                                   X01
     C                   MOVEL     '*NOTF'       P#STAT
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return, saving object and status
      *          ------------------------------------------
     C                   MOVEL     P#OBJC        S#OBJC
     C                   MOVEL     P#OBJL        S#OBJL
     C                   MOVEL     P#STAT        S#STAT
     C                   RETURN
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SROUTF - Create outfile                                       *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SROUTF        BEGSR
      *
      *          ------------------------------------------
      *          Close file, if needed
      *          ------------------------------------------
     C                   CLOSE     QAFDCST                              99
      *
      *          ------------------------------------------
      *          Direct,
      *          ------------------------------------------
     C                   CLEAR                   CMD
     C                   MOVEL     C#FD01        CMD
     C                   CAT       P#OBJL:0      CMD
     C                   CAT       '/':0         CMD
     C                   CAT       P#OBJC:0      CMD
     C                   CAT       C#FD02:0      CMD
     C                   CAT       C#FD03:1      CMD
     C                   CAT       C#FD04:1      CMD
     C                   CAT       C#FD05:1      CMD
     C                   CAT       C#FD06:1      CMD
     C                   CALL      'QCMDEXC'     $MDEXC                 99
      *
      *          ------------------------------------------
      *          and based on constraints.
      *          ------------------------------------------
     C                   MOVEL     '*MORE'       P1STAT
     C                   MOVEL     P#OBJC        P1OBJC
     C                   MOVEL     P#OBJL        P1OBJL
     C                   CLEAR                   P#OBJA
     C     P1STAT        DOWEQ     '*MORE'                                      B01
     C                   CALL      'OMX026'      $MX026
     C                   Z-ADD     2             IY                3 0
     C     OBJ(IY)       DOWNE     *BLANKS                                      B02
     C     IY            ANDLE     640
     C                   CLEAR                   CMD
     C                   MOVEL     C#FD01        CMD
     C                   CAT       OBJ(IY):0     CMD
     C                   CAT       '/':0         CMD
     C                   SUB       1             IY
     C                   CAT       OBJ(IY):0     CMD
     C                   ADD       3             IY
     C                   CAT       C#FD02:0      CMD
     C                   CAT       C#FD03:1      CMD
     C                   CAT       C#FD04:1      CMD
     C                   CAT       C#FD05:1      CMD
     C                   CAT       C#FD07:1      CMD
     C                   CALL      'QCMDEXC'     $MDEXC                 99
     C                   ENDDO                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          File override and open
      *          ------------------------------------------
     C                   CLEAR                   CMD
     C                   MOVEL     C#OVRF        CMD
     C                   CALL      'QCMDEXC'     $MDEXC                 99
     C                   OPEN      QAFDCST
      *
      *          ------------------------------------------
      *          Determine last valid record
      *          ------------------------------------------
     C                   EXSR      SRMORE
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRMORE - Retrieve RRN last valid record                       *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRMORE        BEGSR
      *
      *          ------------------------------------------
      *          Set pointer, read back till valid
      *          ------------------------------------------
     C                   Z-ADD     99999         W#RRN#
     C     W#RRN#        SETGT     QWHFDCST
     C                   SETOFF                                         99
     C     *IN99         DOWEQ     *OFF                                         B01
     C                   READP     QWHFDCST                               99
     C     P#OBJC        IFEQ      CSFILE                                       B02
     C     P#OBJL        ANDEQ     CSLIB
     C     P#OBJC        OREQ      PHCPFN
     C     P#OBJL        ANDEQ     PHCPLN
     C                   Z-ADD     F#RRN#        W#RRN#
     C                   SETON                                          99
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          No constraints for this file
      *          ------------------------------------------
     C     W#RRN#        IFEQ      99999                                        B01
     C                   MOVE      *OFF          S#MORE
     C                   MOVEL     '*NOTF'       P#STAT
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Set the *MORE indicator
      *          ------------------------------------------
     C                   MOVE      *ON           S#MORE            1
      *
      *          ------------------------------------------
      *          Reposition file
      *          ------------------------------------------
     C     1             SETLL     QWHFDCST
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRVERW - Processing (no read if same record is required)      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRVERW        BEGSR
     C*
     C     P#STAT        IFNE      '*SAME'                                      B01
     C                   MOVEL     *BLANKS       P#RELI
     C     P#RELI        DOWEQ     *BLANKS                                      B02
     C                   READ      QAFDCST                                99
      *
      *          ------------------------------------------
      *          Parent or dependent, remove or add
      *          ------------------------------------------
     C                   SELECT                                                 B03
     C     P#OBJC        WHENEQ    CSFILE                                       X03
     C     P#OBJL        ANDEQ     CSLIB
     C                   MOVEL     'D'           P#RELI
     C                   MOVE      *BLANKS       P#ROBC
     C                   MOVE      *BLANKS       P#ROBL
     C     P#OBJC        WHENEQ    PHCPFN                                       X03
     C     P#OBJL        ANDEQ     PHCPLN
     C                   MOVEL     'P'           P#RELI
     C                   MOVEL     CSFILE        P#ROBC
     C                   MOVEL     CSLIB         P#ROBL
     C                   ENDSL                                                  E03
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          More, or last one
      *          ------------------------------------------
     C     F#RRN#        IFEQ      W#RRN#                                       B01
     C                   MOVEL     '*NORM'       P#STAT
     C                   ELSE                                                   X01
     C                   MOVEL     '*MORE'       P#STAT
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Remove or add command
      *          ------------------------------------------
     C     P#RELI        IFNE      *BLANKS                                      B01
     C     P#CMDT        CASEQ     'R'           SRRMV
     C     P#CMDT        CASEQ     'A'           SRADD
     C                   ENDCS
     C     ' '           CHECKR    P#BUF         P#LOC#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRADD - Create apply command for constraint                   *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRADD         BEGSR
      *
      *          ------------------------------------------
      *          Fill P#BUFF
      *          ------------------------------------------
     C                   MOVE      *BLANKS       P#BUF
     C     C#ADD         CAT       C#FILE:1      P#BUF
     C                   CAT       '(':0         P#BUF
     C     P#TOOL        IFEQ      *BLANKS                                      B01
     C                   CAT       CSLIB:0       P#BUF
     C                   ELSE                                                   X01
     C                   CAT       P#TOOL:0      P#BUF
     C                   ENDIF                                                  E01
     C                   CAT       '/':0         P#BUF
     C                   CAT       CSFILE:0      P#BUF
     C                   CAT       ')':0         P#BUF
     C                   CAT       C#TYPE:1      P#BUF
     C                   SELECT                                                 B01
     C     PHCSTT        WHENEQ    'C '                                         X01
     C                   CAT       C#TYPC:0      P#BUF
     C     PHCSTT        WHENEQ    'P '                                         X01
     C                   CAT       C#TYPP:0      P#BUF
     C     PHCSTT        WHENEQ    'U '                                         X01
     C                   CAT       C#TYPU:0      P#BUF
     C     PHCSTT        WHENEQ    'R '                                         X01
     C                   CAT       C#TYPR:0      P#BUF
     C                   ENDSL                                                  E01
     C                   CAT       C#KEY:1       P#BUF
     C                   CAT       '(':0         P#BUF
     C     PHCSTT        IFNE      'R '                                         B01
     C                   Z-ADD     3             IX                3 0
     C                   DO        PHNMKY                                       B02
     C     10            SUBST     PHPRKY:IX     W#KEYC           10
     C     IX            IFEQ      3                                            B03
     C                   CAT       W#KEYC:0      P#BUF
     C                   ELSE                                                   X03
     C                   CAT       W#KEYC:1      P#BUF
     C                   ENDIF                                                  E03
     C                   ADD       10            IX
     C                   ENDDO                                                  E02
     C                   ELSE                                                   X01
     C                   Z-ADD     3             IX
     C                   DO        120                                          B02
     C     10            SUBST     PHFRKY:IX     W#KEYC
     C     W#KEYC        IFEQ      *BLANKS                                      B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C     IX            IFEQ      3                                            B04
     C                   CAT       W#KEYC:0      P#BUF
     C                   ELSE                                                   X04
     C                   CAT       W#KEYC:1      P#BUF
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ADD       10            IX
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
     C                   CAT       ')':0         P#BUF
     C                   CAT       C#CST:1       P#BUF
     C                   CAT       '(':0         P#BUF
      *
      *          ------------------------------------------
      *          One correction is enough
      *          ------------------------------------------
     C     P#STAT        IFNE      '*SAME'                                      B01
     C                   SUBST     PHCSTN:3      PHCSTN
     C                   ENDIF                                                  E01
      *
     C                   CAT       PHCSTN:0      P#BUF
     C                   CAT       ')':0         P#BUF
     C     PHCSTT        IFEQ      'R '                                         B01
     C                   CAT       C#PFIL:1      P#BUF
     C                   CAT       '(':0         P#BUF
     C     PHCPLN        IFEQ      P#OBJL                                       B02
     C     P#TOOL        ANDNE     *BLANKS
     C                   CAT       P#TOOL:0      P#BUF
     C                   ELSE                                                   X02
     C                   CAT       PHCPLN:0      P#BUF
     C                   ENDIF                                                  E02
     C                   CAT       '/':0         P#BUF
     C                   CAT       PHCPFN:0      P#BUF
     C                   CAT       ')':0         P#BUF
     C                   CAT       C#PKEY:1      P#BUF
     C                   CAT       '(':0         P#BUF
     C                   Z-ADD     3             IX
     C                   DO        120                                          B02
     C     10            SUBST     PHPRKY:IX     W#KEYC
     C     W#KEYC        IFEQ      *BLANKS                                      B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C     IX            IFEQ      3                                            B04
     C                   CAT       W#KEYC:0      P#BUF
     C                   ELSE                                                   X04
     C                   CAT       W#KEYC:1      P#BUF
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ADD       10            IX
     C                   ENDDO                                                  E02
     C                   CAT       ')':0         P#BUF
     C                   CAT       C#DLTR:1      P#BUF
     C                   SELECT                                                 B02
     C     PHDLTR        WHENEQ    'NA'                                         X02
     C                   CAT       C#NACT:0      P#BUF
     C     PHDLTR        WHENEQ    'CC'                                         X02
     C                   CAT       C#CASC:0      P#BUF
     C     PHDLTR        WHENEQ    'RS'                                         X02
     C                   CAT       C#REST:0      P#BUF
     C     PHDLTR        WHENEQ    'SN'                                         X02
     C                   CAT       C#SETN:0      P#BUF
     C     PHDLTR        WHENEQ    'SD'                                         X02
     C                   CAT       C#SETD:0      P#BUF
     C                   ENDSL                                                  E02
     C                   CAT       C#UPDR:1      P#BUF
     C                   SELECT                                                 B02
     C     PHULTR        WHENEQ    'NA'                                         X02
     C                   CAT       C#NACT:0      P#BUF
     C     PHULTR        WHENEQ    'RS'                                         X02
     C                   CAT       C#REST:0      P#BUF
     C                   ENDSL                                                  E02
     C                   ENDIF                                                  E01
      *          ------------------------------------------
      *          Create Check-Constraint
      *          ------------------------------------------
     C     PHCSTT        IFEQ      'C '                                         B01
     C                   CAT       C#CHKC:1      P#BUF
     C                   CAT       '(''':0       P#BUF
     C                   SUBST(P)  PHCKXP:3      W#CKXP
     C                   CAT       W#CKXP:0      P#BUF
     C                   CAT       ''')':0       P#BUF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRRMV - Create remove command for constraint                  *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRRMV         BEGSR
      *
      *          ------------------------------------------
      *          Fill P#BUFF
      *          ------------------------------------------
     C                   MOVE      *BLANKS       P#BUF
     C     C#RMV         CAT       C#FILE:1      P#BUF
     C                   CAT       '(':0         P#BUF
     C                   CAT       CSLIB:0       P#BUF
     C                   CAT       '/':0         P#BUF
     C                   CAT       CSFILE:0      P#BUF
     C                   CAT       ')':0         P#BUF
     C                   CAT       C#CST:1       P#BUF
     C                   CAT       '(':0         P#BUF
      *
      *          ------------------------------------------
      *          One correction is enough
      *          ------------------------------------------
     C     P#STAT        IFNE      '*SAME'                                      B01
     C                   SUBST     PHCSTN:3      PHCSTN
     C                   ENDIF                                                  E01
      *
     C                   CAT       PHCSTN:0      P#BUF
     C                   CAT       ')':0         P#BUF
     C                   CAT       C#TYPE:1      P#BUF
     C                   SELECT                                                 B01
     C     PHCSTT        WHENEQ    'C '
     C                   CAT       C#TYPC:0      P#BUF
     C     PHCSTT        WHENEQ    'P '                                         X01
     C                   CAT       C#TYPP:0      P#BUF
     C     PHCSTT        WHENEQ    'U '                                         X01
     C                   CAT       C#TYPU:0      P#BUF
     C     PHCSTT        WHENEQ    'R '                                         X01
     C                   CAT       C#TYPR:0      P#BUF
     C                   ENDSL                                                  E01
     C     PHCSTT        IFNE      'R '                                         B01
     C     PHCSTT        ANDNE     'C '                                         B01
     C                   CAT       C#RMVC:1      P#BUF
     C                   CAT       C#RMVE:0      P#BUF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *PSSR  - Error handler                                        *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine.
      *          ------------------------------------------
     C     I#PSSR        IFNE      *ON                                          B01
     C                   MOVE      *ON           I#PSSR            1
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C     E#FILE        WHENEQ    'QAFDCST '                                   X02
     C                   MOVEL     FD            P#FILE
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Set status
      *          ------------------------------------------
     C                   MOVEL     C#TERM        P#STAT
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
     C                   PARM      *BLANKS       P#MSKC            4
     C                   PARM                    P#MSTA
     C                   PARM      4             P#LOB#
     C                   PARM      '*'           P#PMQC           10
     C                   PARM      1             P#PSC#
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Else exit immediately
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   CLOSE     QAFDCST                              99
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit.
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRTTLR - Total last record processing                         *
      *                                                               *
      * This routine sets the last record indicator on.               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Seton last record, close user handled file
      *          ------------------------------------------
     C                   CALL      'OMX026'
     C                   CLOSE     QAFDCST                              99
     C                   SETON                                        LR
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *INZSR - Initialization program                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Work field definition.
      *          ------------------------------------------
     C     *LIKE         DEFINE    F#RRN#        W#RRN#
     C     *LIKE         DEFINE    P#OBJC        S#OBJC
     C     *LIKE         DEFINE    P#OBJL        S#OBJL
     C     *LIKE         DEFINE    P#STAT        S#STAT
     C                   CLEAR                   S#OBJC
     C                   CLEAR                   S#OBJL
     C                   CLEAR                   S#STAT
      *
      *          ------------------------------------------
      *          Parameter list for QCMDEXEC
      *          ------------------------------------------
     C     $MDEXC        PLIST
     C                   PARM                    CMD             256
     C                   PARM                    P#CMD#           15 5
     C                   PARM                    P#ERR
     C                   Z-ADD     256           P#CMD#
      *
      *          ------------------------------------------
      *          Parameter list for OMX026
      *          ------------------------------------------
     C     $MX026        PLIST
     C                   PARM                    P1OBJC           10
     C                   PARM                    P1OBJL           10
     C                   PARM                    P#OBJA
     C                   PARM                    P1STAT            5
      *
      *          ------------------------------------------
      *          Fill outfile
      *          ------------------------------------------
     C                   EXSR      SROUTF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
