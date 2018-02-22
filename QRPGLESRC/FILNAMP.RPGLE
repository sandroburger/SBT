      *
      * Field prefix exmaple
      *
     FFILNAM3   IF A E           K DISK
     FFILNAM1   UF   E           K DISK    PREFIX(A)
     FFILNAM2   IF A E           K DISK    PREFIX(B)
      *          ------------------------------------------
     C*
     C                   READ      FILNAM1                                99
     C                   EVAL      BMPFIX# = AMPFIX#
     C                   eval      mpfix# = *blanks
     C                   WRITE     FILNA2
     C                   EVAL      AMPFIX# = *BLANKS
     C                   UPDATE    FILNA1
      *
     C                   SETON                                        LR
