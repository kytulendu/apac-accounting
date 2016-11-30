      $set LINKCOUNT"512" GNT"STP026.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **           **     **    **  **    **       **   **         *
      *    ******      **     *******   **    **     **     *******    *
      *         **     **     **        **    **   **       **    **   *
      *   **    **     **     **         **  **   **        **    **   *
      *    ******      **     **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     A M E N D   I T E M   T O   N O T   P R O D U C E D        *
      *                                                                *
      *       Version 9.00.81 - November 2015                          *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STP026.
       AUTHOR.        J.W.LEMMON (APAC).
       DATE-WRITTEN.   JANUARY 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2016
                                  by James William Lemmon.
                                    (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.

       SECURITY.
                This program is free software; you can redistribute
                it and/or modify it under the terms of the GNU General
                Public License as published by the Free Software
                Foundation; either version 3 of the License, or (at
                your option) any later version.

                This program is distributed in the hope that it will
                be useful, but WITHOUT ANY WARRANTY; without even the
                implied warranty of MERCHANTABILITY or FITNESS FOR A
                PARTICULAR PURPOSE. See the GNU General Public License
                for more details.

                You should have received a COPY of the GNU General
                Public License along with this program. If not, see
                <http://www.gnu.org/licenses/>.

       INSTALLATION.          APAC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               CURSOR IS CSTART
                       CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "APACFIDS.SL".

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "SPARTS.SL".

       COPY "STKMEM.SL".

       COPY "STOCK.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "APACFIDS.FDE".

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "RECOVER.FD".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "SPARTS.FDE".

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

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
       77  WS-NETKEY        PIC 9(06)     COMP-5.
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
       77  WS-OPTION     PIC X(01).
       77  WS-SKIP          PIC X(01).
       77  WS-USE-ITM     PIC  X(01).
       77  WS-EXT-STK     PIC  X(01).
       77  WS-ADJ     PIC X(01).
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
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
       77  WS-NODEP     PIC  X(30) VALUE
                            "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
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
        05  FILLER      PIC  X(01) VALUE "�".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
        05  FILLER      PIC  X(01) VALUE "�".
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

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "STP".
    03  WS-PROG        PIC  X(03) VALUE "026".

       COPY "LEDTRF.WS".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY "APACFIDS.ID".

       COPY "CONTROL.ID".

       COPY "DEPART.ID".

       COPY "LEDTRF.ID".

       COPY "PARAM.ID".

       COPY "RECOVER.ID".

       COPY "SHARED.ID".

       COPY "SPARTS.ID".

       COPY "STKMEM.ID".

       COPY "STOCK.ID".

       COPY "W05.VAT".

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

       COPY "W10.STK".

       COPY "W11.STK".

       COPY "W12.WS".

       COPY "W15.STK".

       COPY "W20.WS".

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

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

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
       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
       SCREEN SECTION.

       COPY "BLANK.CRT".

       01  S04G.
          03  LINE  2 COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "AMEND ITEM TO NON PRODUCED".

       COPY "STOCK.CRT".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey VALUE
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S   ".
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY "S99.CRT".

       COPY "ERROR.CRT".

       COPY "OPT.CRT".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-STLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BS000.
             CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50.LUP".

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

       COPY "AA300.STK"|.

      *
      *    ****    D I S P L A Y   D E P A R T M E N T  N A M E
      *
       AA400              SECTION.
       AA401.
      MOVE W10-LEDG  TO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF (WS-F-ERROR = 7) OR
       (DPT-DESC = SPACES)
        MOVE WS-NODEP  TO W10-DEPT
    ELSE
        MOVE DPT-DESC  TO W10-DEPT.
      DISPLAY W10-DEPT AT 0623
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       AA405.
             EXIT.
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
      PERFORM SAVE-SCREEN.
      MOVE 6   TO SHADE-ROW.
      MOVE 28   TO SHADE-COL.
      MOVE 27   TO SHADE-WIDTH.
      MOVE 3   TO SHADE-LINES.
      DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴" AT 0526
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "�"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
            DISPLAY "�" AT 0626
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       " Insufficient security     "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "�"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
            DISPLAY "�" AT 0726
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       " Press ENTER to continue   "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "�"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      DISPLAY "�" AT 0826
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "컴컴컴컴컴컴컴컴컴컴컴컴컴켸"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      PERFORM SCREEN-SHADOW.
            ACCEPT WS-OPTION AT 0752
      WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey AUTO.
      PERFORM RESTORE-SCREEN.

       AA505.
             EXIT.


       COPY "DATE.CHK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STKMEM.WR".

       COPY "STOCK.WR".

       COPY "SPARTS.WR".

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

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
       AY38.
      MOVE 38   TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-RECORD     TO REC-DEPART.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY40.
             MOVE 40             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
             MOVE XFR-REC        TO REC-LEDTRF.
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
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
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
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 2551
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
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
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 2572
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 2572
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

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

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

       COPY "ROUND.PRC".

       COPY "LEDTRF.UPD".

      /
      *    ****  A M E N D   E X S I S T I N G   S T O C K
      *
       BS000        SECTION 6.
       BS000-AMEND.
             PERFORM ERASE-SCREEN.
             DISPLAY S04G.
             DISPLAY S04-N.
            MOVE SPACE   TO W10-EXT-ITEM.
    IF W10-STCK = "N"
        IF W10-SLNGTH = 3
     MOVE ZERO  TO W10-I3
        ELSE
        IF W10-SLNGTH = 4
     MOVE ZERO  TO W10-I4
        ELSE
        IF W10-SLNGTH = 5
     MOVE ZERO  TO W10-I5
        ELSE
        IF W10-SLNGTH = 6
     MOVE ZERO  TO W10-I6
        ELSE
        IF W10-SLNGTH = 7
     MOVE ZERO  TO W10-I7
        ELSE
        IF W10-SLNGTH = 8
     MOVE ZERO  TO W10-I8
        ELSE
        IF W10-SLNGTH = 9
     MOVE ZERO  TO W10-I9
        ELSE
        IF W10-SLNGTH = 10
     MOVE ZERO  TO W10-I10
        ELSE
        IF W10-SLNGTH = 11
     MOVE ZERO  TO W10-I11
        ELSE
        IF W10-SLNGTH = 12
     MOVE ZERO  TO W10-I12
        ELSE
        IF W10-SLNGTH = 13
     MOVE ZERO  TO W10-I13
        ELSE
     MOVE ZERO  TO W10-I14.
       BS005.
             PERFORM AA300.
    IF W10-STCK = "A"
              ACCEPT S05
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S05-3
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S05-4
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S05-5
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S05-6
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S05-7
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S05-8
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S05-9
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S05-10
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S05-11
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S05-12
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S05-13
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S05-14.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BS999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BS005
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BS005
        END-EVALUATE
        PERFORM AA50-LOOKUP
               DISPLAY S05
        IF W10-EXT-ITEM = SPACES
                   GO TO BS005.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO BS999.
       BS05.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
               DISPLAY S05
              IF W10-EXT-ITEM = SPACES
                   GO TO BS005
               ELSE
                   GO TO BS05.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BS005.

       BS012-DET.
            MOVE STK-CODE  TO W10-EXT-ITEM.
            MOVE STK-XREF  TO W10-EXT-CODE2.
             MOVE STK-BIN        TO W10-BIN.
             MOVE STK-DESC       TO W10-DESC.
            MOVE STK-DESC2  TO W10-DESC2.
             MOVE STK-TAX        TO W10-TAX.
             MOVE STK-MARKUP     TO W10-MARKUP.
             MOVE STK-CMARKUP    TO W10-CMARKUP.
             MOVE STK-WMARKUP    TO W10-WMARKUP.
             MOVE STK-IND        TO W10-IND.
             MOVE STK-QUANT      TO W10-QUANT.
             MOVE STK-LEVEL      TO W10-REORD.
             MOVE STK-LTIME      TO W10-LTIME.
             MOVE STK-COST       TO W10-COST.
             MOVE STK-SELL       TO W10-SELL.
             MOVE STK-CASH       TO W10-CASH.
             MOVE STK-WSALE      TO W10-WSALE.
            MOVE STK-LDG  TO W10-LEDG.
             MOVE STK-GUAR       TO W10-GUAR.
             MOVE STK-ORD        TO W10-ORDER.
             MOVE STK-DTE        TO W22-DTE2.
             MOVE W22-DD2        TO W22-DD3.
             MOVE W22-MM2        TO W22-MM3.
             MOVE W22-YY2        TO W22-YY3.
             MOVE W22-DTE3       TO W10-ODTE.
             MOVE STK-PUR        TO W10-PURCH.
             MOVE STK-AVGE       TO W10-AVRG.
             MOVE STK-ACODE      TO W10-ACODE.
      PERFORM AA400.
             DISPLAY S06A.
             DISPLAY S06C.
            MOVE STK-CODE  TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
    IF WS-F-ERROR = 21
              MOVE "This is a non produced item"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BS000-AMEND.
      PERFORM DELETE-SPARTS-REC THRU WRITE-SPARTS-EXIT.

       BS015-AMEND-DET.
      PERFORM AA400.
             ACCEPT S06A.
      PERFORM AA400.
      MOVE ZERO   TO W05-RATE.
           IF W10-TAX > 0
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED
     = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED
     = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED
     = W10-SELL + (W10-SELL * W05-RTE).
             DISPLAY S06C.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BS015-AMEND-DET.
          IF ((W10-IND < 3) OR (W10-IND = 8 OR 9)) AND
               (W10-DESC = SPACES)
              MOVE "Invalid description"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BS015-AMEND-DET.
      MOVE W10-LEDG  TO DPT-CODE.
          IF (W10-LEDG = SPACES OR "0000" OR "XXXX" OR "YYYY") OR
       (DPT-SCDE = "VAT")
              MOVE "Invalid ledger code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BS015-AMEND-DET.
           IF W10-BIN = SPACES
              MOVE "Invalid bin number"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BS015-AMEND-DET.
    IF W10-TAX > 3
        MOVE "Invalid VAT code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BS015-AMEND-DET.
           IF W10-ODTE = ZERO
               GO TO BS18.
             MOVE W10-ODTE       TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Invalid date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BS015-AMEND-DET.
             MOVE W22-DTE1       TO W22-DTE2.
       BS18.
           IF W10-PURCH = ZERO
               GO TO BS020-AMEND.
             MOVE W10-PURCH TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Invalid purchase date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BS015-AMEND-DET.
       BS020-AMEND.
            MOVE W10-EXT-ITEM  TO WS-ITM.
       BS20A.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE ZERO           TO W25-TOTAL W25-VALUE.
           IF STK-IND = 0
               MULTIPLY STK-QUANT BY STK-AVGE
                                 GIVING W25-VALUE.
           IF W10-IND = 0 OR 8 OR 9
               MULTIPLY W10-QUANT BY W10-AVRG
                                 GIVING W25-TOTAL.
             SUBTRACT W25-VALUE FROM W25-TOTAL
                                 GIVING W25-ADJUST.
           IF W25-ADJUST NOT = ZERO
        MOVE W10-LEDG  TO DPT-CODE
               PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT
               PERFORM AY38 THRU AY59
               ADD W25-ADJUST    TO DPT-ADJ-DAY DPT-ADJ-MTD
                                    DPT-ADJ-YTD STK-ADJMTD
                                    STK-ADJYTD
               PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT
               IF WX-INT = "Y"
                   MOVE W12-T-YMD
                                 TO WX-DATE
                   MOVE 28       TO WX-TYPE
                   MOVE W10-LEDG TO WX-LEDG
                   MOVE DPT-PUR-GL
                                 TO WX-AC
                   MOVE "Stock Amended"
                                 TO WX-NAR
                   MOVE W25-ADJUST
                                 TO WX-VALUE
                   PERFORM GL-TRF-UPDATE
                   MOVE 27       TO WX-TYPE
                   MOVE WX-ADJGL TO WX-AC
                   MULTIPLY -1   BY WX-VALUE
                   PERFORM GL-TRF-UPDATE.
       BS21.
            MOVE W10-EXT-ITEM  TO STK-CODE WS-ITM.
             MOVE W10-BIN        TO STK-BIN.
           IF W10-DESC = SPACES
        GO TO BS21B.
             MOVE 30             TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W10-DESC
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BS21A.
             MOVE W10-DESC       TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-29       TO W50-30
        GO TO BS21A.

       BS21B.
             MOVE W10-DESC       TO STK-DESC.
             MOVE 10             TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W10-ACODE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
             MOVE 14             TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-CODE2
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             MOVE W10-TAX        TO STK-TAX.
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF W10-MARKUP NOT > ZERO
           PERFORM BS50.
             MOVE W10-MARKUP     TO STK-MARKUP.
             MOVE W10-QUANT      TO STK-QUANT.
             MOVE W10-REORD      TO STK-LEVEL.
             MOVE W10-LTIME      TO STK-LTIME.
             MOVE W10-COST       TO W10-CCOST.
             MOVE W10-SELL       TO W10-CSELL.
           IF W10-COST = ZERO
              IF (W10-SELL NOT = ZERO) AND (W10-MARKUP NOT = ZERO)
           COMPUTE W10-CCOST = (W10-CSELL /
                           (100 + W10-MARKUP) * 100) + .005.
           IF W10-SELL = ZERO
               IF (W10-COST NOT = ZERO) AND (W10-MARKUP NOT = ZERO)
           COMPUTE W10-CSELL = (W10-CCOST +
                           ((W10-CCOST * W10-MARKUP) / 100)) + .005.
             MOVE W10-CCOST      TO W10-COST.
             MOVE W10-CSELL      TO W10-SELL.
             MOVE W10-COST       TO STK-COST.
            MOVE W10-LEDG  TO STK-LDG.
             MOVE W10-GUAR       TO STK-GUAR.
             MOVE W10-ORDER      TO STK-ORD.
             MOVE W22-DTE2       TO STK-DTE.
             MOVE W10-PURCH      TO STK-PUR.
           IF W10-AVRG = ZERO
               MOVE W10-COST     TO STK-AVGE
           ELSE
               MOVE W10-AVRG     TO STK-AVGE.
             MOVE W10-IND        TO STK-IND.
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF W10-CMARKUP NOT > ZERO
           PERFORM BS52.
           IF (W10-CMARKUP = ZERO) AND
              (W10-CASH = ZERO)
               MOVE W10-MARKUP   TO W10-CMARKUP
               MOVE W10-SELL     TO W10-CASH
              GO TO BS21C.
           IF W10-CMARKUP = ZERO
               MOVE W10-CASH TO W10-CSELL
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST)
                                      / W10-CCOST) * 100) + .005.
           IF W10-CASH = ZERO
               COMPUTE W10-CSELL = (W10-CCOST + ((W10-CCOST *
                                    W10-CMARKUP) / 100)) + .005
               MOVE W10-CSELL TO W10-CASH.
       BS21C.
             MOVE W10-CMARKUP TO STK-CMARKUP.
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF W10-WMARKUP NOT > ZERO
           PERFORM BS54.
           IF (W10-WMARKUP = ZERO) AND
              (W10-WSALE = ZERO)
               MOVE W10-CMARKUP TO W10-WMARKUP
               MOVE W10-CASH TO W10-WSALE
              GO TO BS21D.
           IF W10-WMARKUP = ZERO
               MOVE W10-WSALE TO W10-CSELL
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST)
                                      / W10-CCOST) * 100) + .005.
           IF W10-WSALE = ZERO
               COMPUTE W10-CSELL = (W10-CCOST + ((W10-CCOST *
                                    W10-WMARKUP) / 100)) + .005
               MOVE W10-CSELL TO W10-WSALE.

       BS21D.
           IF W05-ROUND = "E"
               MOVE W10-SELL     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-SELL
               MOVE W10-WSALE    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-WSALE
               MOVE W10-CASH     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-CASH.
           IF STK-TAX NOT = ZERO
        MOVE W05-VAT (STK-TAX)
     TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL +
                                           (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE +
                                            (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH +
                                           (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE    TO W10-VWSALE
               MOVE W10-CASH     TO W10-VCASH
               MOVE W10-SELL     TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VCASH
               PERFORM CB200.
      MOVE ZERO   TO W05-RATE.
           IF W10-TAX > 0
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED
     = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED
     = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED
     = W10-SELL + (W10-SELL * W05-RTE).
             MOVE W10-WMARKUP    TO STK-WMARKUP.
             MOVE W10-SELL       TO STK-SELL.
             MOVE W10-CASH       TO STK-CASH.
             MOVE W10-WSALE      TO STK-WSALE.
             MOVE W10-CODE2      TO STK-XREF.
             MOVE W10-ACODE      TO STK-ACODE.
    IF STK-IND = 2
               MOVE ZERO         TO STK-COST
                                    STK-MARKUP
                                    STK-CMARKUP
                                    STK-WMARKUP.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE STK-RECORD1    TO W15-RECORD1.
             PERFORM AY70 THRU AY999.
             INITIALIZE W200-MEM-FLDS.
             MOVE STK-CODE       TO SME-CODE.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO BS22A.
       BS22.
             MOVE SME-SEQ        TO WS-S1.
             MOVE SME-DATA       TO W200-MEM (WS-S1).
             PERFORM DELETE-STKMEM-REC THRU WRITE-STKMEM-EXIT.
             PERFORM READ-STKMEM-NEXT-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF SME-CODE = STK-CODE
                   GO TO BS22.
             PERFORM BS23A.
             GO TO BS23C.
       BS22A.
      MOVE "Memo/Remarks/Specifications required (Y/N)"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
       BS23.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "Y" OR "N")
               GO TO BS23.
          IF WS-OPTION = "N"
               GO TO BS24C.
       BS23A.
      PERFORM SAVE-SCREEN-2.
      MOVE 7   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 60   TO SHADE-WIDTH.
      MOVE 15   TO SHADE-LINES.
       BS23B.
             INITIALIZE W200-MEM-FLDS.
       BS23C.
             DISPLAY S-MEM.
      PERFORM SCREEN-SHADOW.
       BS24.
             ACCEPT SA-MEM.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BS24.
             MOVE 1              TO WS-S1.
             INITIALIZE SME-RECORD1.
             MOVE WS-ITM         TO SME-CODE.
             MOVE 1              TO SME-SEQ.
       BS24A.
           IF W200-MEM (WS-S1) NOT = SPACES
               MOVE W200-MEM (WS-S1)
                                 TO SME-DATA
               PERFORM WRITE-STKMEM THRU WRITE-STKMEM-EXIT
               ADD 1             TO SME-SEQ.
           IF WS-S1 < 12
               ADD 1             TO WS-S1
               GO TO BS24A.
      PERFORM RESTORE-SCREEN-2.
       BS24C.
             MOVE ZERO           TO WS-AMEND.

       BS40.
             SUBTRACT W25-VALUE  FROM STK-TOTAL.
             ADD W25-TOTAL       TO STK-TOTAL.
             PERFORM AY70 THRU AY999.
             GO TO BS000-AMEND.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       BS50.
           IF W10-SELL > ZERO AND W10-COST > ZERO
               MOVE W10-SELL     TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-MARKUP = (((W10-CSELL - W10-CCOST)
                                     / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-MARKUP   TO W10-MARKUP.
       BS51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BS52.
           IF W10-CASH > ZERO AND W10-COST > ZERO
               MOVE W10-CASH     TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST)
                                     / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-CMRKUP   TO W10-CMARKUP.
       BS53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BS54.
           IF W10-WSALE > ZERO AND W10-COST > ZERO
               MOVE W10-WSALE    TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST)
                                     / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-WMRKUP   TO W10-WMARKUP.
       BS55.
             EXIT.

       BS100.
             MOVE 0301           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY S04G.
             DISPLAY S04-N.
             PERFORM BS012-DET.
             MOVE STK-MARKUP     TO W10-MARKUP.
             MOVE STK-CMARKUP    TO W10-CMARKUP.
             MOVE STK-WMARKUP    TO W10-WMARKUP.
             MOVE STK-GUAR       TO W10-GUAR.
             MOVE STK-COST       TO W10-COST.
             MOVE STK-SELL       TO W10-SELL.
             MOVE STK-CASH       TO W10-CASH.
             MOVE STK-WSALE      TO W10-WSALE.
             MOVE STK-QUANT      TO W10-QUANT.
             MOVE STK-AVGE       TO W10-AVRG.
             PERFORM AA400.
       BS105.
      MOVE ZERO   TO W05-RATE.
           IF W10-TAX > 0
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE.
       BS110.
             COMPUTE W10-VCASH ROUNDED
     = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED
     = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED
     = W10-SELL + (W10-SELL * W05-RTE).
             DISPLAY S06A.
             DISPLAY S06C.
       BS115.
             ACCEPT S06D.
             MOVE W10-COST       TO W10-MCOST W10-CCOST.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
                   ((W10-CCOST * STK-MARKUP) / 100)).
             MOVE W10-CSELL      TO W10-SELL.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
                   ((W10-CCOST * STK-CMARKUP) / 100)).
             MOVE W10-CSELL      TO W10-CASH.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
                   ((W10-CCOST * STK-WMARKUP) / 100)).
             MOVE W10-CSELL      TO W10-WSALE.
             DISPLAY S06E.
             PERFORM BS105 THRU BS110.
             ACCEPT S06E.
      PERFORM CHECK-CORRECT.
    IF WS-OPTION = "N"
               GO TO BS115.
             MOVE W10-COST       TO STK-COST.
             MOVE W10-SELL       TO STK-SELL.
             MOVE W10-CASH       TO STK-CASH.
             MOVE W10-WSALE      TO STK-WSALE.
       BS199.
             EXIT.
       BS999.
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
      MOVE "LEDTRF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-LEDTRF.
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
      MOVE "SPARTS"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SPARTS.
      MOVE "STKMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKMEM.
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
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-ROUND  TO W05-ROUND.
             MOVE PAR-ROUND-VAL  TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO         TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO         TO W05-TQTR-VAL.
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
            MOVE 8   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-WS-HD          TO WS-WS-HD.
             MOVE PAR-CS-HD          TO WS-CS-HD.
             MOVE PAR-RT-HD          TO WS-RT-HD.
            MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
             GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      ACCEPT WS-OPTION AT 1238
      WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             GO TO ZA205.
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
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
        WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
                                    WS-BCHR (60) WS-BCHR (70).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "Supervisor�"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Press any key" AT 2512
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 2526
      WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-ERROR        SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
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
           IF WS-F-ERROR = 21
               MOVE W02-SPARTS   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE PRT-EXT-ITEM TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 25
               MOVE W02-STKMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE SME-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
          ELSE
           IF WS-F-ERROR = 40
               MOVE W02-LEDTRF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE XFR-KEY      TO WS-KEYX.
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
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                        " " WS-KEYX WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
            STOP RUN.
