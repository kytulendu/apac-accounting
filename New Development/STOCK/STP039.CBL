      $set LINKCOUNT"468" GNT"STP039.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******   ****    ******    ******    *
      *   ** **     **     **    **  **  **   ** **  **   **   *
      *   **        **     **    ** **    ** **  **   **   *
      *    ******      **     ******* **    **      ***    *******   *
      *  **     **     ** **    ** **   **   *
      *   ** **     **     **  **  **   ** **  **   **   *
      *    ******      **     **   ****    ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     T R A N S F E R   S T O C K   I N  (BRANCH TO BRANCH)    *
      *               *
      *          M A N U A L          *
      *               *
      *     VERSION 8.13.07 - March 2008          *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP039.
       AUTHOR.        J.W.LEMMON (APAC).
       DATE-WRITTEN.   JANUARY 1991.

     COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2016
         by James William Lemmon.
           (Id No. 4412165050082).

     All rights reserved.

     e-mail jwlemmon@gmail.com.

       SECURITY.
     This program is free software; you can redistribute
     it and/or modify it under the terms of the GNU General
     Public License as published by the Free Software
     Foundation; either version 2 of the License, or (at
     your option) any later version.

     This program is distributed in the hope that it will
     be useful, but WITHOUT ANY WARRANTY; without even the
     implied warranty of MERCHANTABILITY or FITNESS FOR A
     PARTICULAR PURPOSE. See the GNU General Public License
     for more details.

     You should have received a copy of the GNU General
     Public License along with this program; if not, write
     to the Free Software Foundation, Inc., 59 Temple Place
     - Suite 330, Boston, MA 02111-1307, USA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               CURSOR IS CSTART
                       CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY DEPART.SL.

       COPY PARAM.SL.

       COPY RECOVER.SL.

       COPY SHARED.SL.

       COPY STOCK.SL.

       COPY STKTRF.SL.

           SELECT PRNREP ASSIGN W02-PRINTER
                         ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

       COPY RECOVER.FD.

       COPY PARAM.FDE.

       COPY SHARED.FDE.

       COPY STOCK.FDE.

       COPY STKTRF.FD.

      /
       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  REP-LINE1.
           03  REP-DETAIL1     PIC X(96).
       01  REP-LINE2.
           03  REP-DETAIL2     PIC X(96).
       01  REP-LINE3.
           03  REP-DATE        PIC X(06).
          03  REP-DTE        PIC Z9/99/9999.
          03  FILLER        PIC X(12).
           03  REP-COMPANY     PIC X(40).
           03  FILLER          PIC X(18).
           03  REP-PAGE        PIC X(06).
           03  REP-P-NO        PIC Z(03)9.
       01  REP-LINE7.
           03  FILLER          PIC X(40).
           03  REP-HEAD10      PIC X(16).
           03  FILLER          PIC X(40).
       01  ENQ-L1.
           03  ENQ-H1          PIC X(15).
           03  ENQ-H2          PIC X(36).
           03  ENQ-H3          PIC X(11).
           03  ENQ-H4          PIC X(12).
           03  ENQ-H5          PIC X(16).
           03  ENQ-H6          PIC X(06).
       01  ENQ-L2.
           03  ENQ-CODE        PIC X(14).
           03  FILLER          PIC X(01).
           03  ENQ-DESC        PIC X(30).
           03  FILLER          PIC X(02).
           03  ENQ-QUANT       PIC Z(06)9.9999-.
           03  FILLER          PIC X(02).
           03  ENQ-COST        PIC Z(05)9.999 BLANK WHEN ZERO.
           03  FILLER          PIC X(02).
           03  ENQ-WSALE       PIC Z(06)9.99.
           03  FILLER          PIC X(02).
           03  ENQ-SELL        PIC Z(06)9.99.
       01  ORD-LINE1.
           03  ORD-H1          PIC X(15).
           03  ORD-H2          PIC X(36).
           03  ORD-H3          PIC X(12).
           03  ORD-H4          PIC X(12).
           03  ORD-H5          PIC X(11).
           03  ORD-H6          PIC X(10).
       01  ORD-LINE2.
           03  ORD-ITEM        PIC X(14).
           03  FILLER          PIC X(01).
           03  ORD-DESC        PIC X(30).
           03  FILLER          PIC X(02).
           03  ORD-QUANT       PIC Z(06)9.9999.
           03  FILLER          PIC X(02).
           03  ORD-COST        PIC Z(06)9.999.
           03  FILLER          PIC X(02).
           03  ORD-SELL        PIC Z(06)9.99.
           03  FILLER          PIC X(02).
           03  ORD-VALUE       PIC Z(06)9.99.
      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-DISC     PIC 999V99   COMP-3.
       77  WS-SUB           PIC 9(04)     COMP-5.
       77  WS-S1            PIC 9(04)     COMP-5.
       77  WS-S2            PIC 9(04)     COMP-5.
       77  WS-S3            PIC 9(04)     COMP-5.
       77  WS-S4            PIC 9(04)     COMP-5.
       77  WS-S5            PIC 9(04)     COMP-5.
       77  WS-S6            PIC 9(04)     COMP-5.
       77  WS-S7            PIC 9(04)     COMP-5.
       77  WS-S8            PIC 9(04)     COMP-5.
       77  WS-IXS           PIC 9(04)     COMP-5.
       77  WS-IXS1          PIC 9(04)     COMP-5.
       77  WS-IXS2          PIC 9(04)     COMP-5.
       77  WS-IXS3          PIC 9(04)     COMP-5.
       77  WS-IXS4          PIC 9(04)     COMP-5.
       77  WS-TRFKEY        PIC 9(06)     COMP-5.
       77  WS-PRCKEY     PIC 9(06)   COMP-5.
       77  WS-PARKEY        PIC 9(06)     COMP-5.
       77  WS-NETKEY     PIC 9(06)   COMP-5.
       77  WS-RECKEY        PIC 9(06)     COMP-5.
       77  WS-RECOVER       PIC 9(06)     COMP-5.
       77  WS-RECORDS       PIC 9(06)     COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-KEYSTR     PIC 9(04)   COMP-5.
       77  WS-MONTHS     PIC S9(01)V99 COMP-3.
       77  WS-KEY1          PIC 9(04)     COMP-5.
       77  WS-ITM           PIC X(14).
       77  WS-ETYPE         PIC X(01).
       77  WS-PAGE          PIC 9(04)     COMP-5.
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-COND    PIC X(06).
       77  WS-NORM    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-OPTION     PIC X(01).
       77  WS-USE-ITM     PIC  X(01).
       77  WS-EXT-STK     PIC  X(01).
       77  WS-SKIP     PIC X(01).
       77  WS-ADJ           PIC X(01).
       77  WS-ERROR         PIC 9(01).
       77  WS-AMEND     PIC 9(01)   VALUE 0.
       77  WS-CONREC        PIC 9(01) VALUE ZERO.
       77  WS-LEDG     PIC X(04).
       77  WS-LEDG1     PIC X(04).
       77  WS-COMP     PIC 9(01).
       77  WS-WEEK          PIC 9(01).
       77  WS-TYPE          PIC X(01).
       77  WS-BATCH         PIC 9(05).
       77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
       77  WS-LABCDE        PIC X(08).
       77  WS-PASS          PIC X(08).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-NODEP     PIC  X(30) VALUE
                            "NO SUCH   D E P A R T M E N T".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      /
       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LNE.
               05  WS-MCHR PIC X(01) OCCURS 80.
    03  WS-MST-LNE REDEFINES WS-MID-LNE.
        05  WS-MST1     PIC  X(01).
        05  WS-MST78    PIC  X(78).
        05  WS-MST80    PIC  X(01).
          03  WS-MID-LNE2.
               05  WS-MCH  PIC X(01) OCCURS 80.
    03  WS-MID78 REDEFINES WS-MID-LNE2.
        05  FILLER      PIC  X(01).
        05  WS-BLNK78   PIC  X(78).
        05  FILLER      PIC  X(01).
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

       COPY WS.WS.

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY STOCK.ID.

       COPY STKTRF.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "STP031.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.VAT.

       01  W05-EXTRA.
    03  W05-ROUND       PIC  X(01).
           03  W05-ROUND-VAL   PIC  9V9(02).
           03  W05-RVAL REDEFINES W05-ROUND-VAL.
               05  W05-RAND    PIC  9(01).
               05  W05-CENTS   PIC  9(02).
           03  W05-RVAL2 REDEFINES W05-ROUND-VAL.
               05  FILLER      PIC  9(01).
               05  W05-RVC1    PIC  9(01).
               05  W05-RVC2    PIC  9(01).
           03  W05-HLF-VAL     PIC  9V99.
           03  W05-TQTR-VAL    PIC  9V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.
       01  W09-STOCK.
          03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
          03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
          03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

       COPY W15.STK.

       COPY W20.WS.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
           03  W25-SCOST      PIC S9(09)V99 COMP-3.
           03  W25-PCOST      PIC S9(09)V99 COMP-3.
           03  W25-PCST       PIC S9(09)V99 COMP-3.
           03  W25-PTOT       PIC S9(09)V99 COMP-3.
           03  W25-TSELL      PIC S9(09)V99 COMP-3.
           03  W25-TOTAL      PIC S9(09)V99 COMP-3.
           03  W25-VALUE      PIC S9(09)V99 COMP-3.
           03  W25-ADJUST     PIC S9(09)V99 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
               05  W80-S7V2   PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
               05  W80-DATE   PIC Z9/99/99.
           03  W80-QNT.
               05  W80-5      PIC Z(04)9.99.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).
       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.
       01  W90-BAL             PIC S9(07)V99 COMP-3.
       01  W95-COMP            PIC X(40).
       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
       SCREEN SECTION.

       COPY BLANK.CRT.

       COPY S99.CRT.

       COPY ERROR.CRT.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-STLEV < 3
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM BI000.
      CLOSE RECOVER.

       AA49.
      EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

       COPY STOCK.RD.

       COPY STKTRF.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

       COPY STKTRF.WR.

       COPY STOCK.1ST.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY01.
             MOVE 01             TO REC-FILE.
             MOVE WS-PARKEY      TO REC-KEY.
             MOVE PAR-RECORD1    TO REC-PARAM.
             GO TO AY50.
       AY05.
             MOVE 05             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
    IF WS-ACTION = 9
        MOVE 0   TO REC-TYPE
    ELSE
        MOVE WS-ACTION  TO REC-TYPE.
             MOVE STK-RECORD1    TO REC-STOCKF.
             GO TO AY50.
       AY37.
            MOVE 37   TO REC-FILE.
            MOVE WS-SHARED  TO REC-KEY.
            MOVE SHR-RECORD  TO REC-NETWORK.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.
       AY50.
            ADD 1   TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
                        WITH FOREGROUND-COLOR 14 WS-STATUS
                        WITH FOREGROUND-COLOR 15
               STOP RUN.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
            DISPLAY "WAITING" AT 2551
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 2551
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "ST"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
    IF WS-STOCK = "Y"
              MOVE 3   TO WS-SHARED
              PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
        PERFORM AY37 THRU AY59
        MOVE SHR-RECORD  TO NET-RECORD
    ELSE
              MOVE 3   TO WS-NETKEY
              PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
              PERFORM AY39 THRU AY59.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
      *
     IF WS-WAIT = ZERO
         GO TO AY63
     ELSE
         GO TO AY62-WAIT
     END-IF
        ELSE
     GO TO AY62-WAIT
        END-IF
    END-IF.
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 2572
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 2572
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        GO TO AY60.
            DISPLAY SPACE AT 2572
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
    IF WS-STOCK = "Y"
        MOVE NET-RECORD  TO SHR-RECORD
              PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
    ELSE
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        IF WS-USUB < 24
     ADD 1  TO WS-USUB
     GO TO AY72
        ELSE
     GO TO AY85.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
    IF LS0-POS NOT = 2
               CLOSE RECOVER
               OPEN I-O RECOVER.
             COMMIT.

       AY999.
             EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *    ****  T R A N S F E R   S T O C K   I N
      *
       BI000         SECTION 5.
       BI0.
      MOVE WS-OPTION  TO WS-TYPE.
       BI00.
             PERFORM ERASE-SCREEN.
      DISPLAY "TRANSFER STOCK IN" AT 0232
        WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      DISPLAY "Key in the file location and name :"
                      AT 0412.
       BI05.
      ACCEPT W02-FILE33 AT 0449
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BI999
   WHEN OTHER
                     CALL X"E5"
       GO TO BI05
        END-EVALUATE.
    IF W02-FILE33 = SPACES
        GO TO BI05.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BI05.
      OPEN I-O STKTRF.
           IF WS-STATUS NOT = "00"
        MOVE "Invalid drive Specification"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO BI05.
      PERFORM CLEAR-L50.
             MOVE ZERO           TO W25-PCOST WS-PAGE
                                    W25-TSELL W25-TOTAL.
      OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
             MOVE WS-12CPI       TO REP-DETAIL2.
             WRITE REP-LINE1 BEFORE 0 LINES.
             DISPLAY "Stock Code:" AT 0612.
       BI10.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "PAGE:"        TO REP-PAGE.
             MOVE "DATE:"        TO REP-DATE.
             MOVE W95-COMP       TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-NO.
             MOVE W12-TODAY      TO REP-DTE.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "TRANSFER IN"  TO REP-HEAD10.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "ITEM CODE"    TO ORD-H1.
             MOVE "DESCRIPTION"  TO ORD-H2.
             MOVE "QUANTITY"     TO ORD-H3.
             MOVE "UNIT COST"    TO ORD-H4.
             MOVE "UNIT SELL"    TO ORD-H5.
             MOVE "TOTAL SELL"   TO ORD-H6.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
       BI15.
            MOVE SPACES  TO W10-EXT-ITEM.
             MOVE 1              TO W10-QUANT.
             MOVE ZERO           TO W25-SCOST W10-COST.
             MOVE 1              TO WS-TRFKEY.
       BI18.
             PERFORM READ-STKTRF THRU READ-STKTRF-EXIT.
           IF WS-F-ERROR = 33
               GO TO BI65.
      DISPLAY TRF-CODE AT 0624
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE TRF-CODE  TO W10-EXT-ITEM.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               GO TO BI20.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             COMPUTE W10-COST ROUNDED = TRF-COST * TRF-QUANT.
    IF TRF-QUANT = ZERO
        MOVE TRF-COST  TO STK-COST
        MOVE TRF-LEDG  TO STK-LDG
               MOVE TRF-ACODE    TO STK-ACODE
               MOVE TRF-XREF     TO STK-XREF
        MOVE TRF-USE-CASES
     TO STK-USE-CASES
        MOVE TRF-USE-PACKS
     TO STK-USE-PACKS
        MOVE 1   TO WS-S4
        PERFORM BI35 THRU BI40
        PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT
        GO TO BI28.
           IF STK-QUANT < 0.01
               MOVE TRF-COST     TO STK-AVGE
           ELSE
               MULTIPLY STK-QUANT BY STK-AVGE GIVING W10-MCOST
               ADD W10-COST      TO W10-MCOST
               COMPUTE STK-AVGE ROUNDED = W10-MCOST /
                                          (STK-QUANT + TRF-QUANT).
             ADD TRF-QUANT       TO STK-QUANT.
      MOVE TRF-COST  TO STK-COST.
      MOVE 1   TO WS-S4.
      PERFORM BI35 THRU BI40.
      PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             GO TO BI25.

       BI20.
             PERFORM AY60 THRU AY999.
             INITIALIZE STK-RECORD1.
             MOVE TRF-CODE       TO STK-CODE.
             MOVE TRF-ACODE      TO STK-ACODE.
             MOVE TRF-BIN        TO STK-BIN.
      MOVE TRF-LEDG  TO STK-LDG.
             MOVE TRF-DESC       TO STK-DESC.
             MOVE TRF-QUANT      TO STK-QUANT.
             MOVE TRF-LEVEL      TO STK-LEVEL.
             MOVE TRF-COST       TO STK-COST.
      MOVE 1   TO WS-S4.
      PERFORM BI35 THRU BI40.
      MOVE TRF-COST  TO STK-AVGE.
             MOVE TRF-ORD        TO STK-ORD.
             MOVE TRF-LTIME      TO STK-LTIME.
      MOVE 1   TO WS-S4.
      PERFORM BI45 THRU BI50.
      MOVE TRF-SUPP  TO STK-SUPP.
             MOVE TRF-GUAR       TO STK-GUAR.
             MOVE TRF-TAX        TO STK-TAX.
             MOVE TRF-XREF       TO STK-XREF.
      MOVE TRF-USE-PACKS  TO TRF-USE-PACKS.
      MOVE TRF-USE-CASES  TO STK-USE-CASES.
             PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
           IF TRF-QUANT = ZERO
               GO TO BI28.
             COMPUTE W10-COST ROUNDED = TRF-COST * TRF-QUANT.

       BI25.
             ADD W10-QUANT       TO W25-TSELL.
             ADD W10-COST        TO W25-TOTAL.
             MOVE SPACES         TO REP-LINE1.
             MOVE W10-COST       TO ORD-VALUE.
             MOVE TRF-QUANT      TO ORD-QUANT.
             MOVE TRF-DESC       TO ORD-DESC.
             MOVE TRF-CODE       TO ORD-ITEM.
      MOVE TRF-SL(1)  TO ORD-SELL.
             MOVE TRF-COST       TO ORD-COST.
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE REP-LINE1 BEFORE 1 LINES
           ELSE
               WRITE REP-LINE1 BEFORE PAGE
               PERFORM BI10.
             MOVE SPACES         TO REP-DETAIL2.
             ADD W10-COST        TO STK-TOTAL.

       BI28.
             PERFORM AY70 THRU AY999.

       BI30.
             ADD 1               TO WS-TRFKEY.
             GO TO BI18.

       BI35.
      MOVE TRF-UNT(WS-S4) TO STK-UNT(WS-S4).
      MOVE TRF-PR(WS-S4)  TO STK-PR(WS-S4).
      MOVE TRF-SL(WS-S4)  TO STK-SL(WS-S4).
      MOVE TRF-CSH(WS-S4) TO STK-CSH(WS-S4).
      MOVE TRF-WSL(WS-S4) TO STK-WSL(WS-S4).
    IF WS-S4 < 4
        ADD 1   TO WS-S4
        GO TO BI35.

       BI40.
      EXIT.

       BI45.
      MOVE TRF-MKUP(WS-S4)
     TO STK-MKUP(WS-S4).
      MOVE TRF-CMKUP(WS-S4)
     TO STK-CMKUP(WS-S4).
      MOVE TRF-WMKUP(WS-S4)
     TO STK-WMKUP(WS-S4).
    IF WS-S4 < 4
        ADD 1   TO WS-S4
        GO TO BI45.

       BI50.
      EXIT.

       BI65.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE "TOTAL"        TO ORD-DESC.
             MOVE W25-TSELL      TO ORD-QUANT.
             MOVE W25-TOTAL      TO ORD-VALUE.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE "***** END REPORT *****" TO REP-DETAIL2.
             WRITE REP-LINE1 BEFORE PAGE.
             MOVE WS-10CPI       TO REP-DETAIL1.
             WRITE REP-LINE1 BEFORE 0 LINES.
             MOVE SPACES         TO REP-DETAIL1.
             CLOSE PRNREP.
             CLOSE STKTRF.
      GO TO BI999.
       BI999.
             EXIT.

      /
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
      MOVE 1   TO USER-ACTION
        USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
      MOVE ZERO   TO USER-NUMBER.
      MOVE 11   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
      MOVE 53   TO USER-NUMBER.
      MOVE 2   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
      MOVE 64   TO MOUSE-FUNC.
      MOVE 1   TO MOUSE-PARAM.
      CALL X"AF" USING MOUSE-FUNC
         MOUSE-PARAM.
    IF MOUSE-FUNC NOT = 255
        MOVE "Y"   TO MOUSE-ATTACHED
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM
    ELSE
        GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *     A S  F U N C T I O N   K E Y
      *
      MOVE 3   TO USER-ACTION.
      MOVE 27   TO USER-NUMBER.
      MOVE 2   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *           MOUSE-BUTTONS
      *     RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"   TO MOUSE-ATTACHED.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
      MOVE "PARAM"  TO AFID-KEY.

       ZA00-READ-APACFIDS.
            READ APACFIDS WITH IGNORE LOCK
        KEY IS AFID-KEY.
    IF WS-STATUS = "00"
        GO TO ZA00-READ-APACFIDS-EXIT.
            STRING "Missing " DELIMITED SIZE
       AFID-KEY DELIMITED BY " "
       " file ID - Status " DELIMITED SIZE
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "RECOVER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-RECOVER.
      MOVE "SHARED"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SHARED.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
             MOVE TRM-STDP (WS-S6)
                                 TO WS-PRINT.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.
       ZA01B.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE 3              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
             MOVE STK-PASSWORD   TO WS-PASSWORD.
      MOVE 1   TO W05-V-RATE.

       ZA05-VAT.
      MOVE W05-VAT-CODE  TO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      MOVE DPT-R-DATE  TO WS-VAT-DATE(W05-V-RATE).
      MOVE DPT-RATE  TO W05-VAT(W05-V-RATE).
      ADD 6 W05-V-RATE  GIVING WS-S1.
      MOVE DPT-P-RATE  TO W05-VAT(WS-S1).
    IF W05-V-RATE < 6
        ADD 1   TO W05-V-RATE
        GO TO ZA05-VAT.
      GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      ACCEPT WS-OPTION AT 1238
      WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
             GO TO ZA205.
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE WS-MID-LNE2.
       ZA60.
             MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
                                    WS-TCHR (60) WS-TCHR (70).
             MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
                                    WS-MCHR (60) WS-MCHR (70)
                                    WS-MCHR (1)  WS-MCHR (80)
                                    WS-MCH  (1)  WS-MCH  (80).
             MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
                                    WS-BCHR (60) WS-BCHR (70).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
             DISPLAY "Press any key" AT 2512
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 2526
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-ERROR        SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK  TO WS-FILE
               MOVE WS-NETKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 7
              MOVE W02-DEPART  TO WS-FILE
              MOVE ZERO  TO WS-KEY
              MOVE DPT-CODE  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
               MOVE W02-RECOVER  TO WS-FILE
               MOVE WS-RECKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 33
               MOVE W02-STKTRF   TO WS-FILE
              MOVE WS-TRFKEY  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY .
           IF WS-STATUS = "10"
               MOVE "End of FILE" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked" TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
            STOP RUN.
