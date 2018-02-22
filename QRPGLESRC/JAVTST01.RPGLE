      *$$\CRTOM CRTBNDRPG
      *$$ CVTOPT(*DATETIME)
      *$$ DFTACTGRP(*NO)
      *$$ ACTGRP(*CALLER)
      *$$ TGTRLS(V5R3M0)
      *$$ OPTION(*NOSHOWCPY *NOEXT *NOEXPDDS)
      *$$ DBGVIEW(*NONE)
      *$$ OPTIMIZE(*FULL)
      *$$ REPLACE(*YES)
      *$$ USRPRF(*OWNER)
      *$$\ENDOM
      * -----------------------------------------------------------
      * Description
      * -----------------------------------------------------------
      *
      * Program ....: OMX0162
      * Function ...: Retrieve source relations
      * Author .....: W.S. Jongman.
      * Startdate ..: 23-11-2004.
      * Parameters .: Object      (12) I
      *               Library     (10) I
      *               Type        ( 9) I
      *               ScanPhase   ( 2) I
      *               Buffer      (45) O DIM(32)
      *               Status      ( 5) I/O
      *
      * This program retrieves information for this object. If the
      * object happens to be an iSeries native object and sources
      * exists for this object, we extract the source and pass
      * it to the source scanner OMX163.
      *
      * iSeries source logic:
      * When the program is called with Status '*NORM' the sources
      * are scanned. The scanner returns a DIM(32K) array of
      * possible relationships. These relationships are processed
      * in this program. The entries that are returned are raw. This
      * means that, for example, all changed fields are passed for
      * a given program. This program reconstructs the fields that
      * are actually information for the configuration database.
      *
      * After the retrieved source relations are refactored they are
      * passed back to the caller in the <Buffer>. This buffer can
      * hold 32 items. If there are more to process then the status
      * is filled with '*MORE' to indicate this. If the caller
      * wants more items, this program is called again with status
      * '*NEXT' indicating the next bunch of 32 items.
      *
      * -----------------------------------------------------------
      /EJECT

      * ----------------------------------------------------------- *
      * Files                                                       *
      *-------------------------------------------------------------*
     FOMAGPL1   IF   E           K DISK
     FOMRELL2   IF   E           K DISK
     FOMRELL4   IF   E           K DISK
     F                                     RENAME(OMRELR:OMRELR2)
     FOMRELL6   IF   E           K DISK
     F                                     RENAME(OMRELR:OMRELR6)
     FOMIOPL2   IF   E           K DISK
     FOMOBJL8   IF   E           K DISK

      * ----------------------------------------------------------- *
      * Data specifications                                         *
      * ----------------------------------------------------------- *

      *          ------------------------------------------
      *          Reference file is used for field definition
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREF)

      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#FILE               201    208
     D  D#JOB#               264    269  0
     D  D#USID               254    263

      *          ------------------------------------------
      *          Error buffer for API call.
      *          ------------------------------------------
      /INCLUDE QAPILESRC,ERR_BUFFER

      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY

      *          ------------------------------------------
      *          Java remain.nl.td.OMiSeriesSourceRelations
      *          Constructor returns an OMiSeriesSourceRelations
      *          ------------------------------------------
     DSourceRel        PR              O   CLASS(*JAVA:'nl.remain.td.Testjava')
     D                                     Extproc(*JAVA:'nl.remain.td.Testjava'+
     D                                     :*CONSTRUCTOR)

      *          ------------------------------------------
      *          Java remain.nl.td.OMiSeriesSourceRelations
      *          Constructor returns an OMiSeriesSourceRelations
      *          and expects: Path to the patterns
      *                       AS400 object
      *          ------------------------------------------
     DSourceRel2       PR              O   CLASS(*JAVA:'nl.remain.td.+
     D                                     OMiSeriesSourceRelations')
     D                                     Extproc(*JAVA:'nl.remain.sc' : 'd' )
     D                                 O   CLASS(*JAVA:'java.lang.String') CONST
     D                                 O   CLASS(*JAVA: C#AS400OBJ)        CONST

      *          ------------------------------------------
      *          Java remain.nl.td.OMiSeriesSourceRelations.getSystem
      *          It gets the system object so it is created only once.
      *          ------------------------------------------
     D  getSystem      PR              O   CLASS(*JAVA: C#AS400OBJ)
     D                                     EXTPROC(*JAVA : 'nl.remain.td.OMiS+
     D                                     eriesSourceRelations' : 'getSystem+
     D                                     ' )

      * ---------------------------------------------------
      * Define string objects
      * ---------------------------------------------------
     D sourcePath      S               O   CLASS(*JAVA:'java.lang.String')
     D sourceType      S               O   CLASS(*JAVA:'java.lang.String')
     D patternRoot     S               O   CLASS(*JAVA:'java.lang.String')
     D value           S               O   DIM(4)
     D                                     CLASS(*JAVA:'java.lang.String')

      * ---------------------------------------------------
      * Define JTOpen objects
      * ---------------------------------------------------
     D AS400Object     S               O   CLASS(*JAVA:C#AS400OBJ)

      * ---------------------------------------------------
      * Define constants to nl.remain.* objects
      * ---------------------------------------------------
     D C#SrcRel        C                   const('nl.remain.td.+
     D                                     OMiSeriesSourceRelations')
     D C#AS400OBJ      C                   const('com.ibm.as400.access.AS400')
     D C#QMQRY         C                   const('*QMQRY')
     D C#QRYDFN        C                   const('*QRYDFN')

      * ---------------------------------------------------
      * Define nl.remain.td.* objects
      * ---------------------------------------------------
     D SrcComponent    S               O   CLASS(*JAVA:
     D                                     'nl.remain.td.OMSourceComponent')
     D SourceRelation  S               O   CLASS(*JAVA:C#SrcRel)

      * ---------------------------------------------------
      * Arrays
      * ---------------------------------------------------
     D char            S          65535A   VARYING DIM(4)

      * ---------------------------------------------------
      * Object data structure
      * ---------------------------------------------------
     D  D#OBJ          DS
     D    D#OBJC2              1     12
     D    D#OBJL2             13     22
     D    D#OBJT2             23     31
     D    D#MBRC2             32     41
     D    D#RELT2             42     44
     D    D#OCLS2             45     45
     D    D#READYI            46     46

      * ---------------------------------------------------
      * Other parameter fields
      * ---------------------------------------------------
     D P#MBRA          S                   Like(MBRA)

      * ---------------------------------------------------
      * Program specific constants
      * ---------------------------------------------------
     DEofString        C                   const(X'00')

      * ---------------------------------------------------
      * Various counters
      * ---------------------------------------------------
     D count           S             10I 0
     D from            S             10I 0
     D numberOfItems   S             10I 0
     D numberOf...
     D Batches         S             10I 0
     D currentBatch    S             10I 0
     D overFlow        S             10I 0
     D to              S             10I 0
     D ix              S             10I 0
     D iy              S             10I 0

      * ---------------------------------------------------
      * Arrays
      * ---------------------------------------------------
     D RES             S             46A   DIM(32000)                           Result array 1
     D RES02           S             45A   DIM(32000)                           Result array 2

      * ---------------------------------------------------
      * Parameter fields
      * ---------------------------------------------------
     D P#OBJC          S                   Like(OBJC)
     D P#OBJL          S                   Like(OBJL)
     D P#OBJT          S                   Like(OBJT)

     D P#PHSE          S              2A
     D P#STAT          S                   Like(STAT)

      * ---------------------------------------------------
      * Workfields
      * ---------------------------------------------------
     D W#SRCF          S                   Like(SRCF)
     D W#SRCL          S                   Like(SRCL)
     D W#SRCM          S                   Like(SRCM)
     D I#OMX0162C      S              1A

     D P#SRC           DS
     D  OBJ2                   1   1440    DIM(32)

      * ---------------------------------------------------
      * Key fields
      * ---------------------------------------------------
     D K#IDRC          S            640

      *    ------------------------------------------------
      *    Entry parameter list
      *    ------------------------------------------------
      /COPY QAPILESRC,DEFOTHER
     C     *Entry        Plist
     C                   Parm                    P#OBJC
     C                   Parm                    P#OBJL
     C                   Parm                    P#OBJT
     C                   Parm                    P#PHSE
     C                   Parm                    P#SRC
     C                   Parm                    P#STAT
      /Free

        //**
        //*   Start of processing
        //**

         // Exit if no parameters where passed
            if D#PARM = 0;
               exsr SRTTLR;
            endIf;

         // Do main
            Exsr main;

       // ----------------------------------------------------------
       // Main
       // ----------------------------------------------------------
            begsr main;

         // Start scanning if not *NEXT
            if P#STAT <> '*NEXT';

            // Scan main source
               exsr srScanMainSource;

         // Else return the next Batch of related objects
            else;
               if P#STAT = '*NEXT';
                 exsr srReturnBatch;
               ENDIF;
            endIf;

         // Exit
            Exsr srTTLR;

            Endsr;

       // ----------------------------------------------------------
       // srScanMainSource
       //
       //  This subroutine scans the main source of the
       //  component.
       //
       // ----------------------------------------------------------

          begsr srScanMainSource;

         // Blank the result array.
            RES = *BLANKS;

         // Get the source loction.
            chain (P#OBJC: P#OBJT: P#OBJL) OMOBJL8;

         // Process the iSeries relations.
            if (%found) and (RJSRCL <> *blanks) and (RJOCLS = '0');
              W#SRCF = RJSRCF;
              W#SRCL = RJSRCL;
              W#SRCM = RJSRCM;
            else;

              if (%found) and (RJOCLS = '0');
              endif;
            ENDIF;

         // Process source if found
         if (W#SRCF <> *blanks);
           Exsr SriSeriesSource;

            // Process the results
           exsr srProcessResult;
         endif;


          endsr;

       // ----------------------------------------------------------
       // SRiSeriesSource
       //
       // This subroutine determines if the source of the component
       // is procesable.
       // ----------------------------------------------------------
          begsr sriSeriesSource;

       // Retrieve the sourcetype of this source
       // The sourcetype retriever does not have a status
       // It returns a blank object description in case of
       // errors. Provide dummy info to do some ugly checking.
          MBRD = 'XX$%';
          P#MBRA = *ALL'*';
          OMX942C(W#SRCF: W#SRCL: W#SRCM: P#MBRA: MBRD: P#STAT);

       // No source found if MBRD is blanks and MBRA equals the dummy
          if MBRD = *BLANKS and P#MBRA = *ALL'*';
             P#STAT = '*TERM';
             exsr SRTTLR;
          endif;

       // Scan the source to get a list of possible
       // relationship objects.
          Exsr srScanSource;

          endsr;

       // ----------------------------------------------------------
       // SRScanSource
       //
       // This subroutine scans the source. This is done by calling the
       // nl.remain.td.OMiSeriesSourceRelations.getRelations() method
       // with the root to the java.util.regex.Pattern objects
       //
       // The class will scan the root directory for Pattern objects
       // that can be used to parse the source data for useful relationships
       // and applies each Pattern to the source.
       // ----------------------------------------------------------
          begsr srScanSource;

       // Set the CLASSPATH if not already done.
          if (I#OMX0162C <> '1');
             OMX0162C(Stat);
             I#OMX0162C = '1';
          Endif;

       // Get the root directory with the patterns.
          Chain (*BLANKS: *BLANKS: *BLANKS: *BLANKS: 'OMPATTERNROOT') OMAGPL1;

          if (%found);
          patternRoot = toString(%trim(GPKEYV) + '/ScanPhase' + P#PHSE);

             // Purge the sources if this is a roundtrip
             if (SourceRelation <> *NULL AND P#PHSE = '01');
                purgeSource(SourceRelation);
             Endif;

             // Set the patternroot in an existing object.
             if (SourceRelation <> *NULL);
                setPatRoot(SourceRelation: patternRoot);

             else;

                // Create an OMiSeriesSourceRelations object
                if (AS400Object = *NULL);
                   SourceRelation = SourceRel();
                   setPatRoot(SourceRelation: patternRoot);
                else;
                   SourceRelation = SourceRel2(patternRoot: AS400Object);
                   setPatRoot(SourceRelation: patternRoot);
                Endif;

                // Get the AS/400 object for later use.
                if (AS400Object = *NULL);
                   AS400Object = getSystem(SourceRelation);
                Endif;
             Endif;

          // Add this source to the list of sources to scan
             sourcePath = toString('/qsys.lib/' +
                                    %trim(W#SRCL) + '.lib/' +
                                    %trim(W#SRCF) + '.file/' +
                                    %trim(W#SRCM) + '.mbr');
             addSource(SourceRelation: sourcePath);

          // Now read all the source relationships of this object
          // This is relation type 900 and 901
             Setll (P#OBJC: P#OBJT: P#OBJL: 900) OMRELL6;
             Reade (P#OBJC: P#OBJT: P#OBJL) OMRELL6;

             DOW NOT %EOF AND (ELRELT <= 901);

             // If this is a file member, then add it.
             // Blank library is unresolved.
                If ELRMBC <> *BLANKS and ELROBT = '*FILE'
                                     and ELROBL <> *Blanks;
                   sourcePath = toString('/qsys.lib/' +
                                       %trim(ELROBL) + '.lib/' +
                                       %trim(ELROBC) + '.file/' +
                                       %trim(ELRMBC) + '.mbr');
                   addSource(SourceRelation: sourcePath);
                Endif;

                // Read next relationship
                   Reade (P#OBJC: P#OBJT: P#OBJL) OMRELL6;
             Enddo;

          // Scan the sources
             sourceType = toString(%TRIM(P#MBRA));
             getSourceRel(SourceRelation: sourceType);

          endif;
          endsr;

       // ----------------------------------------------------------
       // srProcessResult
       //
       //  This subroutine parses the result. The result sets are placed
       //  back in arrays OBJ, LIB, TYP and DTL. These must be further
       //  processed to get correct relationships.
       // ----------------------------------------------------------
          begsr srProcessResult;

       // Part 1: Retrieve the information from the Java object

       // Retrieve the number of objects from the object
          count    = getCount(SourceRelation);

       // Loop count
          for iy = 1 TO count BY 1;

           // Get the OMSourceComponent object
              SrcComponent = getRelation(SourceRelation: iy - 1);

           // Now load the LIB, OBJ, DTL and TYP arrays into array RES
                D#OBJC2     = getBytes(getCode(SrcComponent));
                D#OBJL2     = getBytes(getLibrary(SrcComponent));
                D#MBRC2     = getBytes(getDetail(SrcComponent));
                D#OBJT2     = getBytes(getType(SrcComponent));
                D#OCLS2     = getBytes(getClassCode(SrcComponent));
                D#RELT2     = getBytes(getRelationType(SrcComponent));
                D#READYI    = getBytes(getObjectReady(SrcComponent));

                RES(iy) = D#OBJ;     // Store in array.

          ENDFOR;

       // Part 2: Process this information to get actual relationships

            // Init routine
               ix = 1;
               numberOfItems = 0;

            // Loop through result until no more data.
               dow RES(ix) <> *BLANKS;

                 D#OBJ = RES(ix);

                 if D#READYI = '1';
                    numberOfItems = numberOfItems + 1;
                    RES02(numberOfItems) = D#OBJ;
                 else;
                    if D#OCLS2 = 'D';
                       exsr srAttrFactor;
                    Endif;
                 Endif;

                 ix = ix + 1;
               ENDDO;

            // Process the first batch of items and transfer them to the caller
               currentBatch  = 0;
               numberOfBatches = %DIV(numberOfItems:32);
               overFlow        = %REM(numberOfItems:32);
               exsr srReturnBatch;

          endsr;

       // ----------------------------------------------------------
       // srAttrFactor
       //
       //  This routine reads the parents of this attribute. For each
       //  parent we investigate if this parent is directly related to
       //  the supplied object.
       //  If it is, we have found a good relation and it will be
       //  supplied to OMX016.
       //
       //  Else we discard this attribute.
       // ----------------------------------------------------------
          begsr srAttrFactor;

            // Read all USING relations of this object.
               Setll (P#OBJC: P#OBJT: P#OBJL) OMRELL2;
               Reade (P#OBJC: P#OBJT: P#OBJL) OMRELL2;

               DOW NOT %EOF;

               // Get the object.
                  Chain (ELROBC: ELROBT: ELROBL) OMOBJL8;

               // Only if found and attributes
                  if %found and RJOCLS <> 'D';

                  // Does this object own this attribute
                     K#IDRC = '/QSYS.LIB/' + %trim(RJOBJL) + '.LIB/' +
                              %trim(RJOBJC) + '.' +
                              %trim(%subst(RJOBJT: 2: 8)) + '/';
                     Chain (K#IDRC) OMIOPL2;

                     if %found;
                        Setll (D#OBJC2: D#OBJT2: OPLIBC) OMOBJL8;

                     // YES!!
                        If %equal;
                           numberOfItems        = numberOfItems + 1;
                           D#OBJL2              = OPLIBC;
                           RES02(numberOfItems) = D#OBJ;
                        Endif;
                     Endif;
                  Endif;

                  Reade (P#OBJC: P#OBJT: P#OBJL) OMRELL2;
               Enddo;


            // Read all USED-BY relations of this object.
               Setll (P#OBJC: P#OBJT: P#OBJL) OMRELL4;
               Reade (P#OBJC: P#OBJT: P#OBJL) OMRELL4;

               DOW NOT %EOF;

               // Get the object.
                  Chain (ELOBJC: ELOBJT: ELOBJL) OMOBJL8;
                  if %found and RJOCLS <> 'D';  // not for attributes

                  // Does this object own this attribute?
                     K#IDRC = '/QSYS.LIB/' + %trim(RJOBJL) + '.LIB/' +
                              %trim(RJOBJC) + '.' +
                              %trim(%subst(RJOBJT: 2: 8)) + '/';
                     Chain (K#IDRC) OMIOPL2;

                     if %found;
                        Setll (D#OBJC2: D#OBJT2: OPLIBC) OMOBJL8;

                     // YES!!
                        If %equal;
                           numberOfItems        = numberOfItems + 1;
                           D#OBJL2              = OPLIBC;
                           RES02(numberOfItems) = D#OBJ;
                        Endif;
                     Endif;
                  Endif;

                  reade (P#OBJC: P#OBJT: P#OBJL) OMRELL4;
               Enddo;
          endsr;

       // ----------------------------------------------------------
       // srReturnBatch
       //
       //  This subroutine transfers the next batch of 32 to the
       //  Caller
       // ----------------------------------------------------------
          begsr srReturnBatch;

            // init
               P#SRC = *BLANKS;

            // If current batch is less then the total number of batches.
               if currentBatch < numberOfBatches;

                  ix           = currentBatch * 32;
                  currentBatch = currentBatch + 1;

                  for iy = 1 to 32;
                    ix = ix + 1;
                    OBJ2(iy) = RES02(ix);
                  ENDfor;

               // If more then indicate more items
                  if overflow > 0 or currentBatch < numberOfBatches;
                    P#STAT = '*MORE';
                  ENDIF;

               else;

               // If current batch is equal to the total number of batches.
                  if currentBatch = numberOfBatches;

                     ix           = currentBatch * 32;
                     currentBatch = currentBatch + 1;

                     for iy = 1 to overFlow;
                       ix = ix + 1;
                       OBJ2(iy) = RES02(ix);
                     ENDfor;

                  // Everything processed, return normal;
                     P#STAT = '*NORM';

                  else;

                  // Error if current batch is greater then the total number of batches.
                     if currentBatch > numberOfBatches;

                     P#STAT = '*TERM';

                     ENDIF;
                  ENDIF;
               ENDIF;
          endsr;

      /END-FREE
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *PSSR  - Error handler                                      *
      *                                                             *
      * Standard RPG error handler.                                 *
      * Make sure the data structure PGM is defined as the          *
      * program status data structure (SDS) with a length of 429.   *
      * The file information data structure is also passed, but     *
      * not the complete data structure, only the relevant info.    *
      * After the program is called, the messages in the queue      *
      * of this program are passes back to the queue of the         *
      * calling program and the *CANCL procedure is invoked.        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine.
      *          ------------------------------------------
     C     I#PSSR        IFNE      *ON                                          B01
     C                   MOVE      *ON           I#PSSR            1
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALL      'OMX961'
     C                   PARM                    PGM
     C                   PARM      *BLANKS       P#FILE           57
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
      *          Else exit immediately
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit.
      *          ------------------------------------------
     C                   Z-ADD     0             D#PARM
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTTLR - Total last record processing                       *
      *                                                             *
      * This routine sets the last record indicator on.             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Seton last record if no parameters where passed.
      *          ------------------------------------------
     C     D#PARM        IFEQ      0                                            B01
     C                   SETON                                        LR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Close programs
      *          ------------------------------------------
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
