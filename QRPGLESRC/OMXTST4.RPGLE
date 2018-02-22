      *$$\CRTOM CRTRPGPGM
      *$$ CVTOPT(*DATETIME *VARCHAR)
      *$$ REPLACE(*YES)
      *$$ TGTRLS(V5R2M0)
      *$$ USRPRF(*OWNER)
      *$$\ENDOM
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX020                                        *
      * Function ...: Retrieve formatted message                    *
      * Author .....: W.S. Jongman.                                 *
      * Startdate ..: 30-08-1993.                                   *
      * Parameters .: *NONE                                         *
      *                                                             *
      * This program retrieves a messsage, formats it and returns   *
      * the formatted message text.                                 *
      *                                                             *
      * Use the FREE RPG operation code or any related command      *
      * to deactivate the program.                                  *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      *                                                             *
      * *NONE                                                       *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Sub programs                                                *
      * ----------------------------------------------------------- *
      *                                                             *
      * *NONE                                                       *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * MT    Message text from QMHRTVM API                          *
      * FT    Formatted message text                                 *
      * ------------------------------------------------------------ *
     D FT              S              1    DIM(8000)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input pecifications                                         *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFAM)
     D               E DS                  EXTNAME(OMSREFNZ)
      *
      *          ------------------------------------------
      *          P#MSI - RTVM0200 Format
      *
      *          The following table lists the fields in the RTVM0200
      *          format.
      *          Since the length of the message text, message help
      *          (second level text) and the default replay vary,
      *          these buffers are not included in this member
      *          This buffer must be copied into the program
      *          and adjusted to suit your specific needs.
      *
      *          DM2BR# (B4)   Bytes returned to buffer
      *          DM2BA# (B4)   Bytes available for buffer
      *          DM2MS# (B4)   Message severity
      *          DM2AI# (B4)   Alert index
      *          DM2AOC (C9)   Alert option code
      *          DM2LIC (C1)   Log indicator
      *          DM2R1  ( 2)   Reserved
      *          DM2RR# (B4)   Bytes of default reply returned
      *          DM2RA# (B4)   Bytes available for default reply
      *          DM2MR# (B4)   Length of message returned
      *          DM2MA# (B4)   Lentgh available for message
      *          DM2HR# (B4)   Length of message help returned
      *          DM2HA# (B4)   Length available for message help
      *          DM2DR  (*)    Default reply
      *          DM2MT  (*)    Message text buffer
      *          DM2MH  (*)    Message help buffer
      *          ------------------------------------------
     D P#MSI           DS
     D  DM2BR#                 1      4B 0
     D  DM2BA#                 5      8B 0
     D  DM2MS#                 9     12B 0
     D  DM2AI#                13     16B 0
     D  DM2AOC                17     25
     D  DM2LIC                26     26
     D  DM2R1                 27     28
     D  DM2RR#                29     32B 0
     D  DM2RA#                33     36B 0
     D  DM2MR#                37     40B 0
     D  DM2MA#                41     44B 0
     D  DM2HR#                45     48B 0
     D  DM2HA#                49     52B 0
     D  MT                    53   8052
     D                                     DIM(8000)
      *
      *          ------------------------------------------
      *          Error buffer for API call.
      *
      *          The number of bytes provided is an input parameter
      *          indicating the length of the error buffer in the
      *          calling program. This field is set to 266 thus
      *          providing a 250 byte message data field.
      *
      *          D#NBP# (B4)   Number of bytes provided
      *          D#NBA# (B4)   Number of bytes available
      *          D#MSID (A7)   Message id
      *                 (A1)   Reserved
      *          D#MSGD (A250) Message data
      *          ------------------------------------------
     D P#ERR           DS
     D  D#NBP#                 1      4B 0 INZ(272)
     D  D#NBA#                 5      8B 0
     D  D#MSID                 9     15
     D  D#MSGD                17    272
      *
      *          ------------------------------------------
      *          Line size.
      *          ------------------------------------------
     D P#LSZ#          DS
     D  W#LSZ#                 1      4B 0
      *
      *          ------------------------------------------
      *          Number of lines
      *          ------------------------------------------
     D P#NOL#          DS
     D  W#NOL#                 1      4B 0
      *
      *          ------------------------------------------
      *          Number of first level lines
      *          ------------------------------------------
     D P#NFL#          DS
     D  W#NFL#                 1      4B 0
      *
      *          ------------------------------------------
      *          Number of second level lines
      *          ------------------------------------------
     D P#NSL#          DS
     D  W#NSL#                 1      4B 0
      *
      *          ------------------------------------------
      *          Severity level
      *          ------------------------------------------
     D P#SEVL          DS
     D  W#SEVL                 1      4B 0
      *
      *
      *          ------------------------------------------
      *          API binaray parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Mainline                                                    *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Parameters
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    P#MSGN
     C                   PARM                    P#MSGF
     C                   PARM                    P#MSGL
     C                   PARM                    P#MSGD          256
     C                   PARM                    P#LSZ#
     C                   PARM                    P#SEVL
     C                   PARM                    FT
     C                   PARM                    P#NOL#
     C                   PARM                    P#NFL#
     C                   PARM                    P#NSL#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Start of normal
      *          ------------------------------------------
     C                   MOVE      C#NORM        P#STAT
     C                   Z-ADD     *ZEROS        W#NOL#
     C                   Z-ADD     *ZEROS        W#NFL#
     C                   Z-ADD     *ZEROS        W#NSL#
      *
      *          ------------------------------------------
      *          Retrieve the message.
      *          ------------------------------------------
     C                   Z-ADD     8052          P#LOB#
     C                   MOVE      'RTVM0200'    P#FMTC
     C                   MOVE      P#MSGN        P#MIDC
     C                   MOVE      P#MSGL        P#MSGQ
     C                   MOVEL     P#MSGF        P#MSGQ
     C                   MOVE      P#MSGD        P#MSD           256
     C                   Z-ADD     256           P#MDL#
     C                   MOVEL     '*YES    '    P#RSVC
     C                   MOVEL     '*YES    '    P#RFCC
     C                   EXSR      SRRTVM
      *
      *          ------------------------------------------
      *          If error reset fields
      *          ------------------------------------------
     C     D#NBA#        IFNE      *ZEROS                                       B01
     C                   Z-ADD     99            W#SEVL
     C                   MOVE      C#TERM        P#STAT
      *
      *          ------------------------------------------
      *          Format the message.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   Z-ADD     DM2MS#        W#SEVL
     C                   Z-ADD     DM2RR#        W#OMT#
     C     DM2RR#        ADD       DM2MR#        W#OMH#
     C                   EXSR      SRFMTM
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Return to caller.
      *          ------------------------------------------
     C                   RETURN
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFMTM - Format message                                     *
      *                                                             *
      * This standard routine formats the message text in MT to     *
      * a formatted (displayable) message and places the result in  *
      * array FT.                                                   *
      *                                                             *
      * When a &N symbol is found in the text, then the text        *
      * continues on column 1 of the next line. If the text is      *
      * longer then one line, the next lines are indented 2 posi-   *
      * tions, starting in column 3.                                *
      *                                                             *
      * When a &P symbol is found in the text, then the text        *
      * continues on column 5 of the next line. If the text is      *
      * longer then one line, the next lines are indented 2 posi-   *
      * tions, starting in column 3.                                *
      *                                                             *
      * When a &B symbol is found in the text, then the text        *
      * continues on column 3 of the next line. If the text is      *
      * longer then one line, the next lines are indented 2 posi-   *
      * tions, starting in column 5.                                *
      *                                                             *
      * Input      - W#LSZ#  Line size of output device             *
      *              MT      Unformatted message text    (8000 1)   *
      *              W#OMT#  Offset to message text                 *
      *              W#OMH#  Offset to message help                 *
      *              DM2MR#  Length of the first level text         *
      *              DM2HR#  Length of the second level text        *
      *                                                             *
      * Output     - FT      Formatted message text      (8000 1)   *
      *              W#NOL#  Number of lines of length W#LSZ#       *
      *              W#NFL#  Number of first level lines            *
      *              W#NSL#  Number of second level lines           *
      *                                                             *
      * Destroys   - IX      Marks the end of a line                *
      *              IY      Pointer in the formatted text          *
      *              TP      Pointer in the unformatted text        *
      *              IS      Begin position of a line               *
      *              W#IND#  Indentation value                      *
      *              I#LIND  Last indentation code                  *
      *              NL      Line counter                           *
      *              TS      Work pointer                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFMTM        BEGSR
      *
      *          ------------------------------------------
      *          Reset the array indexes.
      *          ------------------------------------------
     C                   Z-ADD     *ZEROS        IX
     C                   Z-ADD     *ZEROS        IY
     C                   Z-ADD     *ZEROS        IS
     C                   Z-ADD     *ZEROS        TP
      *
      *          ------------------------------------------
      *          Set the last indentation character to 'N'.
      *          Reset the indentation workfield
      *          ------------------------------------------
     C                   MOVE      'N'           I#LIND
     C                   Z-ADD     3             W#IND#
      *
      *          ------------------------------------------
      *          Reset the number of lines workfield.
      *          ------------------------------------------
     C                   Z-ADD     *ZEROS        NL
      *
      *          ------------------------------------------
      *          Clear the formatted array and the work array.
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FT
      *
      *          ------------------------------------------
      *          Fill the formatted array with the first text.
      *          ------------------------------------------
     C                   MOVEA     W#5A09        FT(1)
     C                   ADD       21            IS
     C                   ADD       W#OMT#        IS
      *
      *          ------------------------------------------
      *          Format the next line.
      *          ------------------------------------------
     C     FMTM10        TAG
     C                   ADD       1             NL
      *
      *          ------------------------------------------
      *          Calculate the end of this line.
      *          ------------------------------------------
     C     NL            MULT      W#LSZ#        IX
      *
      *          ------------------------------------------
      *          Do until end of the line is reached.
      *          ------------------------------------------
     C     IS            DO        IX            IY                             B01
      *
      *          ------------------------------------------
      *          Get the charachter from the message text if the
      *          text position is greater then the number of bytes
      *          returned from the first level text then jump to
      *          the second level text format part.
      *          ------------------------------------------
     C                   ADD       1             TP
     C     TP            IFGT      DM2MR#                                       B02
     C                   SUB       1             TP
     C                   GOTO      FMTM50
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if this is the start of a special code.
      *          ------------------------------------------
     C     MT(TP)        IFEQ      '&'                                          B02
     C                   ADD       1             TP
      *
     C     MT(TP)        IFEQ      'N'                                          B03
     C                   Z-ADD     *ZEROS        W#IND#
     C     1             ADD       IX            IY
     C                   MOVE      'N'           I#LIND
     C                   ENDIF                                                  E03
      *
     C     MT(TP)        IFEQ      'P'                                          B03
     C                   Z-ADD     5             W#IND#
     C     1             ADD       IX            IY
     C                   MOVE      'P'           I#LIND
     C                   ENDIF                                                  E03
      *
     C     MT(TP)        IFEQ      'B'                                          B03
     C                   Z-ADD     3             W#IND#
     C     1             ADD       IX            IY
     C                   MOVE      'B'           I#LIND
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If this is a special code then search the first
      *          non blank after the code and leave the line loop.
      *          ------------------------------------------
     C     MT(TP)        IFEQ      'N'                                          B03
     C     MT(TP)        OREQ      'P'
     C     MT(TP)        OREQ      'B'
     C     1             DOWEQ     1                                            B04
     C                   ADD       1             TP
     C     MT(TP)        IFNE      *BLANKS                                      B05
     C                   SUB       1             TP
     C                   LEAVE
     C                   ENDIF                                                  E05
     C                   ENDDO                                                  E04
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
     C                   SUB       1             TP
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          No special charachters, check if this word fits on
      *          this line.
      *          ------------------------------------------
     C     TP            IFGT      1                                            B02
     C                   SUB       1             TP
     C     MT(TP)        IFEQ      *BLANKS                                      B03
     C                   ADD       1             TP
     C     MT(TP)        IFNE      *BLANKS                                      B04
     C                   Z-ADD     TP            TS
     C     *BLANK        LOOKUP    MT(TS)                                 99
     C     *IN99         IFEQ      *ON                                          B05
     C     TS            SUB       TP            TS
     C                   ADD       IY            TS
     C     TS            IFGT      IX                                           B06
     C                   SUB       1             TP
     C                   LEAVE
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   ADD       1             TP
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the character
      *          ------------------------------------------
     C                   MOVE      MT(TP)        FT(IY)
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          A next line was reached, add the identation value
      *          into the next line.
      *          ------------------------------------------
     C     1             ADD       IX            IS
     C                   ADD       W#IND#        IS
      *
      *          ------------------------------------------
      *          Set the new indentation value.
      *          ------------------------------------------
     C                   Z-ADD     *ZEROS        W#IND#
     C     I#LIND        IFEQ      'N'                                          B01
     C     I#LIND        OREQ      'P'
     C                   Z-ADD     3             W#IND#
     C                   ELSE                                                   X01
     C     I#LIND        IFEQ      'B'                                          B02
     C                   Z-ADD     5             W#IND#
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Process the next line.
      *          ------------------------------------------
     C                   GOTO      FMTM10
      *
      *          ------------------------------------------
      *          Format the second level help
      *          Calculate the total length of the message text.
      *          Set the last indention code.
      *          Set the number of first level lines.
      *          ------------------------------------------
     C     FMTM50        TAG
     C                   ADD       DM2HR#        DM2MR#
     C                   MOVE      'N'           I#LIND
     C                   Z-ADD     NL            W#NFL#
      *
      *          ------------------------------------------
      *          If no second level text then exit.
      *          ------------------------------------------
     C     DM2HR#        CABEQ     *ZEROS        FMTM90
      *
      *          ------------------------------------------
      *          Skip to the next line if there is not a formatting
      *          code coming.
      *          ------------------------------------------
     C                   ADD       1             TP
     C                   ADD       1             NL
     C                   Z-ADD     TP            TS
     C     MT(TS)        DOWEQ     *BLANKS                                      B01
     C     TS            CABGT     DM2MR#        FMTM55
     C                   ADD       1             TS
     C                   ENDDO                                                  E01
     C     MT(TS)        IFEQ      '&'                                          B01
     C                   ADD       1             TS
     C     MT(TS)        IFEQ      'N'                                          B02
     C     MT(TS)        OREQ      'P'
     C     MT(TS)        OREQ      'B'
     C                   SUB       1             NL
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
     C                   SUB       1             TP
      *
      *          ------------------------------------------
      *          Calulate the first position of the next line
      *          ------------------------------------------
     C     NL            MULT      W#LSZ#        IS
     C     IS            SUB       W#LSZ#        IS
     C                   ADD       1             IS
      *
      *          ------------------------------------------
      *          Format the next line.
      *          ------------------------------------------
     C     FMTM55        TAG
      *
      *          ------------------------------------------
      *          Calculate the end of this line.
      *          ------------------------------------------
     C     NL            MULT      W#LSZ#        IX
      *
      *          ------------------------------------------
      *          Do until end of the line is reached.
      *          ------------------------------------------
     C     IS            DO        IX            IY                             B01
      *
      *          ------------------------------------------
      *          Get the charachter from the message text if the
      *          text position is greater then the number of bytes
      *          returned from the first and second level text then
      *          jump to the end of subroutine.
      *          ------------------------------------------
     C                   ADD       1             TP
     C     TP            IFGT      DM2MR#                                       B02
     C                   GOTO      FMTM90
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if this is the start of a special code.
      *          ------------------------------------------
     C     MT(TP)        IFEQ      '&'                                          B02
     C                   ADD       1             TP
      *
     C     MT(TP)        IFEQ      'N'                                          B03
     C                   Z-ADD     *ZEROS        W#IND#
     C     1             ADD       IX            IY
     C                   MOVE      'N'           I#LIND
     C                   ENDIF                                                  E03
      *
     C     MT(TP)        IFEQ      'P'                                          B03
     C                   Z-ADD     5             W#IND#
     C     1             ADD       IX            IY
     C                   MOVE      'P'           I#LIND
     C                   ENDIF                                                  E03
      *
     C     MT(TP)        IFEQ      'B'                                          B03
     C                   Z-ADD     3             W#IND#
     C     1             ADD       IX            IY
     C                   MOVE      'B'           I#LIND
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If this is a special code then search the first
      *          non blank after the code and leave the line loop.
      *          ------------------------------------------
     C     MT(TP)        IFEQ      'N'                                          B03
     C     MT(TP)        OREQ      'P'                                          B03
     C     MT(TP)        OREQ      'B'                                          B03
     C     1             DOWEQ     1                                            B04
     C                   ADD       1             TP
     C     MT(TP)        IFNE      *BLANKS                                      B05
     C                   SUB       1             TP
     C                   LEAVE
     C                   ENDIF                                                  E05
     C                   ENDDO                                                  E04
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
     C                   SUB       1             TP
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          No special charachters, check if this word fits on
      *          this line.
      *          ------------------------------------------
     C     TP            IFGT      1                                            B02
     C                   SUB       1             TP
     C     MT(TP)        IFEQ      *BLANKS                                      B03
     C                   ADD       1             TP
     C     MT(TP)        IFNE      *BLANKS                                      B04
     C                   Z-ADD     TP            TS
     C     *BLANK        LOOKUP    MT(TS)                                 99
     C     *IN99         IFEQ      *ON                                          B05
     C     TS            SUB       TP            TS
     C                   ADD       IY            TS
     C     TS            IFGT      IX                                           B06
     C                   SUB       1             TP
     C                   LEAVE
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   ADD       1             TP
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          No special charachters, fill this character
      *          in the formatted array.
      *          ------------------------------------------
     C                   MOVE      MT(TP)        FT(IY)
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          A next line was reached, add the identation value
      *          into the next line.
      *          ------------------------------------------
     C     1             ADD       IX            IS
     C                   ADD       W#IND#        IS
      *
      *          ------------------------------------------
      *          Set the new indentation value.
      *          ------------------------------------------
     C                   Z-ADD     *ZEROS        W#IND#
     C     I#LIND        IFEQ      'N'                                          B01
     C     I#LIND        OREQ      'P'
     C                   Z-ADD     3             W#IND#
     C                   ELSE                                                   X01
     C     I#LIND        IFEQ      'B'                                          B02
     C                   Z-ADD     5             W#IND#
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Process the next line.
      *          ------------------------------------------
     C                   ADD       1             NL
     C                   GOTO      FMTM55
      *
      *          ------------------------------------------
      *          Calculate the total number of lines and the number
      *          of second level lines.
      *          ------------------------------------------
     C     FMTM90        TAG
     C                   Z-ADD     NL            W#NOL#
     C     NL            SUB       W#NFL#        W#NSL#
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRTVM - Retrieve message                                    * /
      *                                                              * /
      * The retrieve message (QMHRTVM) API retrieves the message     * /
      * definition of a predefined message. It consists of the text  * /
      * of the message and other information such as the second      * /
      * level text and the default reply for the message. You can    * /
      * use this API to copy the text of predefined messages into a  * /
      * program.                                                     * /
      *                                                              * /
      * Authorities  *USE for the message file                       * /
      *              *USE for the message file library               * /
      *                                                              * /
      * Input      - P#LOB#  Buffer length of message information    *
      *              P#FMTC  Format name                             *
      *              P#MIDC  Message id                              *
      *              P#MSGQ  Message file qualifier                  *
      *              P#MSD   Message data buffer                     *
      *              P#MDL#  Message data lenght                     *
      *              P#RSVC  Replace substition variables            *
      *                      *NO  - Do not use the message data      *
      *                             but return the &x fields.        *
      *                      *YES - Substitute the message data      *
      *              P#RFCC  Return format control characters        *
      *                      *NO  - Do not return the characters     *
      *                             &B, &N and &P.                   *
      *                      *YES - Return them into the text.       *
      *                                                              *
      * Output     - P#MSI   Message information buffer see the      *
      *                      RTVM0100 and RTVM0200 members for       *
      *                      details.                                *
      *              P#ERR   Error buffer                            *
      *                                                              *
      * Destroys   -                                                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRTVM        BEGSR
      *
      *          ------------------------------------------
      *          Clear the message data array to prevent trailing
      *          text of the previous message
      *          ------------------------------------------
     C                   MOVEA     *BLANKS       MT
      *          ------------------------------------------
      *          Retrieve message
      *          ------------------------------------------
     C                   CALL      'QMHRTVM'
     C                   PARM                    P#MSI
     C                   PARM                    P#LOB#
     C                   PARM                    P#FMTC
     C                   PARM                    P#MIDC
     C                   PARM                    P#MSGQ
     C                   PARM                    P#MSD
     C                   PARM                    P#MDL#
     C                   PARM                    P#RSVC
     C                   PARM                    P#RFCC
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
     C                   MOVE      *BLANKS       I#LIND            1
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
     C     *LIKE         DEFINE    MSGN          P#MSGN
     C     *LIKE         DEFINE    MSGF          P#MSGF
     C     *LIKE         DEFINE    OBJL          P#MSGL
     C     *LIKE         DEFINE    STAT          P#STAT
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   Z-ADD     *ZEROS        W#IND#            3 0
     C                   Z-ADD     *ZEROS        W#OMH#            5 0
     C                   Z-ADD     *ZEROS        W#OMT#            5 0
     C                   Z-ADD     *ZEROS        IX                5 0
     C                   Z-ADD     *ZEROS        IY                5 0
     C                   Z-ADD     *ZEROS        IS                5 0
     C                   Z-ADD     *ZEROS        NL                5 0
     C                   Z-ADD     *ZEROS        TP                5 0
     C                   Z-ADD     *ZEROS        TS                5 0
      *
      *          ------------------------------------------
      *          API parameter definition.
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMX904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID             7
     C                   PARM      'QCPFMSG'     MSGF             10
     C                   PARM      '*LIBL'       MSGLIB           10
     C                   PARM                    MSGTXT          132
      *
      *          ------------------------------------------
      *          Get constant messages.
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * retrieve the data wich is filled in message texts.           *
      * It is called one time per program execution. If the system   *
      * choose to do so, it can be paged out. Thats why it is here.  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get CPX5A09 message.
      *          ------------------------------------------
     C                   MOVE      'CPX5A09'     MSGID
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C     MSGID         IFNE      *BLANKS                                      B02
     C                   CALL      'OMX904C'     $X904C
     C                   END                                                    E02
      *
      *          ------------------------------------------
      *          Fill message array
      *          ------------------------------------------
     C                   MOVEL     MSGTXT        W#5A09           17
     C                   ENDSR
