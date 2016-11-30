      $set LINKCOUNT"578" GNT"STP022.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **           **     **    **  **    **       **        **    *
      *    ******      **     *******   **    **     **        **      *
      *         **     **     **        **    **   **         **       *
      *   **    **     **     **         **  **   **         **        *
      *    ******      **     **          ****   ********   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   P R O D U C T I O N   -   R E C O R D E D      *
      *                                                                *
      *       Version 9.00.70 - May 2015                               *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STP022.
       AUTHOR.         J.W.LEMMON (CSS).
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

                You should have received a copy of the GNU General
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

       COPY APACFIDS.SL.

       COPY AUDIT.SL.

       COPY CONTROL.SL.

       COPY DEPART.SL.

       COPY PARAM.SL.

       COPY RECOVER.SL.

       COPY SHARED.SL.

       COPY SPARTS.SL.

       COPY STKDEX.SL.

       COPY STKMEM.SL.

       COPY STOCK.SL.

       DATA DIVISION.
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY AUDIT.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

       COPY RECOVER.FD.

       COPY PARAM.FDE.

       COPY SHARED.FDE.

       COPY SPARTS.FDE.

       COPY STKDEX.FDE.

       COPY STKMEM.FDE.

       COPY STOCK.FDE.

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-DISC    PIC 999V99  COMP-3.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6    PIC 9(04)  COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-IXS    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-IXS2    PIC 9(04)  COMP-5.
       77  WS-IXS3    PIC 9(04)  COMP-5.
       77  WS-IXS4    PIC 9(04)  COMP-5.
       77  WS-AUDKEY    PIC 9(04)  COMP-5.
       77  WS-TRFKEY    PIC 9(06)  COMP-5.
       77  WS-PRCKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-RECORDS    PIC 9(06)  COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-ITM    PIC X(18).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-CARDEX    PIC X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ADJ    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-AMEND    PIC 9(01)  VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-LEDG    PIC X(04).
       77  WS-LEDG1    PIC X(04).
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-PASSWORD    PIC X(08).
       77  WS-PRINT    PIC 9(01).
       77  WS-LABCDE    PIC X(08).
       77  WS-PASS    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
       77  WS-DRAW    PIC 9(02).
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
              05  WS-TCH   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
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
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
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

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "STP".
    03  WS-PROG        PIC  X(03) VALUE "022".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY APACFIDS.ID.

       COPY AUDIT.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY SPARTS.ID.

       COPY STKDEX.ID.

       COPY STKMEM.ID.

       COPY STOCK.ID.

       01  W02-PRINTER-DETAILS.
    03  W02-PRINTER     PIC  X(12).
          03  W02-PGE-LENGTH  PIC  9(02).
          03  W02-PRN-LENGTH  PIC  9(02).
    03  W02-LINAGE      PIC  9(02).
    03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      *     C  =  Condensed print
      *     E  =  Expanded print
      *     H  =  Header line
      *     X  =  Cancel expanded print
      *     1  =  10 Characters per inch
      *     2  =  12 Characters per inch
      *     3  =  17 Characters per inch
      *     6  =  6 Lines per inch
      *     8  =  8 Lines per inch
      *
    03  W02-PRN-TYPE    PIC  X(01).
    03  W02-PRN-LINE    PIC  X(136).
    03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-SLA       PIC  X(78).
        05  FILLER      PIC  X(58).
    03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(58).
        05  R-SLB       PIC  X(78).
          03  REP-LINE1 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL1 PIC  X(140).
          03  REP-LINE2 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL2 PIC  X(80).
          03  REP-LINE3 REDEFINES W02-PRN-LINE.
              05  REP-DATE    PIC  X(06).
              05  REP-DTE     PIC  Z9/99/9999.
              05  FILLER      PIC  X(03).
        05  REP-PRC-HD.
     07  FILLER  PIC  X(01).
           07  REP-COMPANY  PIC X(40).
     07  FILLER  PIC  X(02).
              05  FILLER      PIC  X(08).
              05  REP-PAGE    PIC  X(06).
              05  REP-P-NO    PIC  Z(03)9.
          03  REP-LINE4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(30).
              05  REP-HEAD1   PIC  X(20).
              05  FILLER      PIC  X(30).
          03  REP-LINE5 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(09).
              05  REP-DSC     PIC  X(30).
              05  FILLER      PIC  X(93).
          03  REP-LINE6 REDEFINES W02-PRN-LINE.
              05  REP-HEAD2   PIC  X(10).
              05  REP-CNO     PIC  X(04).
              05  REP-CATNO REDEFINES REP-CNO PIC X(04).
              05  FILLER      PIC  X(01).
              05  REP-CAT     PIC  X(30).
              05  FILLER      PIC  X(35).
          03  REP-LINE4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(32).
              05  REP-HEAD10  PIC  X(16).
              05  FILLER      PIC  X(32).
          03  REP-LINE9 REDEFINES W02-PRN-LINE.
              05  REP-PG      PIC  X(06).
              05  REP-P-N     PIC  Z(03)9.
              05  FILLER      PIC  X(21).
              05  REP-COMP    PIC  X(40).
              05  REP-REPORT  PIC  X(34).
              05  FILLER      PIC  X(11).
              05  REP-DAT     PIC  X(06).
              05  REP-DT      PIC  Z9/99/9999.
          03  ORD-LINE1 REDEFINES W02-PRN-LINE.
              05  ORD-H1    PIC X(19).
              05  ORD-H2    PIC X(35).
              05  ORD-H3    PIC X(11).
              05  ORD-H4    PIC X(11).
              05  ORD-H5    PIC X(10).
              05  ORD-H6    PIC X(10).
          03  ORD-LINE2 REDEFINES W02-PRN-LINE.
              05  ORD-EXT-ITEM.
     07  ORD-ITEM    PIC X(14).
     07  ORD-SL    PIC X(01).
     07  ORD-ITM    PIC X(03).
              05  FILLER    PIC X(01).
              05  ORD-DESC    PIC X(30).
              05  FILLER    PIC X(01).
              05  ORD-QUANT    PIC Z(06)9.9999.
              05  FILLER    PIC X(01).
              05  ORD-COST    PIC Z(06)9.999.
              05  FILLER    PIC X(01).
              05  ORD-SELL    PIC Z(06)9.99.
              05  FILLER    PIC X(01).
              05  ORD-VALUE    PIC Z(06)9.99.

       COPY W05.VAT.

       COPY W05.RND.

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
          03  W25-SCOST      PIC S9(08)V999 COMP-3.
          03  W25-PCOST      PIC S9(08)V999 COMP-3.
          03  W25-PCST       PIC S9(08)V999 COMP-3.
          03  W25-PTOT       PIC S9(08)V999 COMP-3.
          03  W25-TSELL      PIC S9(08)V999 COMP-3.
          03  W25-TOTAL      PIC S9(08)V999 COMP-3.
          03  W25-VALUE      PIC S9(08)V999 COMP-3.
          03  W25-ADJUST     PIC S9(08)V999 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

       COPY W70DEBT.IWS.

       COPY W75.STK.

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

       01  S05.
    03  LINE  6 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(14) USING W10-EXT-ITEM AUTO.

       01  S05-ITM.
    03  LINE  6 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(14)/X(03)
     USING W10-EXT-ITEM AUTO.

       01  S05-EXT.
    03  LINE  6 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING W10-EXT-ITEM AUTO.

       01  S15.
           03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
                                 VALUE "STOCK PRODUCTION".
    03  LINE  3 COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
     VALUE "Use recorded details".
           03  LINE  4 COLUMN  2 VALUE "Item Code/Description".
           03          COLUMN 36 VALUE "Quantity".
           03          COLUMN 47 VALUE "Unit cost".
           03          COLUMN 59 VALUE "Unit sell".
           03          COLUMN 70 VALUE "Total cost".

       01  S35.
    03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(05)9.9999 USING W10-QUANT AUTO.

       01  S36.
    03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(06)9.999 USING W10-SELL AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-STLEV < 2
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      MOVE ZERO   TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
            PERFORM BQ000.
      PERFORM AB35.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      CLOSE AUDIT
     RECOVER.

       AA49.
      EXIT PROGRAM.

       AB000        SECTION.
      *
      *    ****   1 2 C P I ( E L I T E   P R I N T )
      *
       AB25.
      MOVE 0   TO WS-ADVANCE.
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.

       AB30.
             EXIT.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
       AB35.
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.

       AB40.
             EXIT.

       COPY AA50.LUP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AS3.

       COPY AA300.STK.

       COPY DATE.CHK.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ        SECTION.

       COPY AUDIT.RD.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

       COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   & W R I T E   F I L E S R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE        SECTION.

       COPY AUDIT.WR.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY STKDEX.WR.

       COPY STKMEM.WR.

       COPY STOCK.WR.

       COPY SPARTS.WR.

       COPY STOCK.1ST.

       COPY APAC.HLP.

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

       AY16.
             MOVE 16             TO REC-FILE.
             MOVE WS-AUDKEY      TO REC-KEY.
             MOVE AUD-REC1       TO REC-AUDITF.
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
      MOVE DPT-RECORD  TO REC-DEPART.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

       AY46.
            MOVE 46   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE STX-REC1  TO REC-STKDEX.
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
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             MOVE NET-DEBTOR     TO W70-DEBT.
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
            MOVE 1   TO WS-NETKEY.
             MOVE W70-DEBT       TO NET-DEBTOR.
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

      /
       BG00-TRAN               SECTION.
       BG80.
             MOVE W70-AUDIT      TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
             PERFORM AY16 THRU AY59.
            MOVE STK-CODE  TO AUD-EXT-CODE.
             MOVE 2              TO AUD-TYPE.
             MOVE WS-DRAW        TO AUD-SUB.
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
             ADD 1               TO W70-AUDIT.
             MOVE 1              TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
             MOVE 1              TO AUD-REP2.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
       BG85.
             EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY ROUND.PRC.

       CB300      SECTION.
       CB301.
    IF W10-TAX NOT = ZERO
        COMPUTE W10-SL(WS-S1) ROUNDED = (W10-VSL(WS-S1) /
    (100.00000 + W05-RATE)) * 100.00
        COMPUTE W10-WSL(WS-S1) ROUNDED = (W10-VWSL(WS-S1) /
     (100.00000 + W05-RATE)) * 100.00
        COMPUTE W10-CSH(WS-S1) ROUNDED = (W10-VCSH(WS-S1) /
    (100.00000 + W05-RATE)) * 100.00
           ELSE
        MOVE W10-VWSL(WS-S1)
     TO W10-WSALE
        MOVE W10-VCSH(WS-S1)
     TO W10-CASH
        MOVE W10-VSL(WS-S1)
     TO W10-SELL.
    IF W10-TAX NOT = ZERO
        COMPUTE W10-CHECK ROUNDED
     = W10-SL(WS-S1) + (W10-SL(WS-S1) * W05-RTE)
        IF W10-CHECK > W10-VSL(WS-S1)
     SUBTRACT 0.01 FROM W10-SL(WS-S1)
        ELSE
        IF W10-CHECK < W10-VSL(WS-S1)
     ADD 0.01  TO W10-SL(WS-S1)
        END-IF
        COMPUTE W10-CHECK ROUNDED
     = W10-CSH(WS-S1) + (W10-CSH(WS-S1) * W05-RTE)
        IF W10-CHECK > W10-VCSH(WS-S1)
     SUBTRACT 0.01 FROM W10-CSH(WS-S1)
        ELSE
        IF W10-CHECK < W10-VCSH(WS-S1)
     ADD 0.01  TO W10-CSH(WS-S1)
        END-IF
        COMPUTE W10-CHECK ROUNDED
     = W10-WSL(WS-S1) + (W10-WSL(WS-S1) * W05-RTE)
        IF W10-CHECK > W10-VWSL(WS-S1)
     SUBTRACT 0.01 FROM W10-WSL(WS-S1)
        ELSE
        IF W10-CHECK < W10-VWSL(WS-S1)
     ADD 0.01  TO W10-WSL(WS-S1).

       CB399.
             EXIT.

      /
      *    ****    PRODUCTION PROCESSING
      *
       BQ000         SECTION 6.
       BQ0.
             PERFORM ERASE-SCREEN.
      DISPLAY "PRODUCTION PROCESSING" AT 0230
        WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      MOVE "S"   TO WS-TYPE.
    IF LS0-STLEV < 3
        GO TO BQ0D.
      DISPLAY "Print the " AT 0412 WITH FOREGROUND-COLOR 7
                     "C" WITH FOREGROUND-COLOR 14
                     "ost, " WITH FOREGROUND-COLOR 7
                     "S" WITH FOREGROUND-COLOR 14
                     "elling or " WITH FOREGROUND-COLOR 7
                     "B" WITH FOREGROUND-COLOR 14
       "oth prices" WITH FOREGROUND-COLOR 7.

       BQ0C.
      ACCEPT WS-TYPE AT 0452
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-TYPE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-TYPE = "B" OR "C" OR "S")
               GO TO BQ0C.

       BQ0D.
             MOVE ZERO           TO W25-PCOST WS-PAGE
                                    W25-TSELL W25-TOTAL.
      DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.

       BQ00.
             PERFORM ERASE-SCREEN.
             DISPLAY S15.

       BQ05.
      PERFORM AB35.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
             MOVE "PAGE:"        TO REP-PAGE.
             MOVE "DATE:"        TO REP-DATE.
             MOVE W95-COMP       TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-NO.
             MOVE W12-TODAY      TO REP-DTE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
             MOVE "PRODUCTION"   TO REP-HEAD10.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      PERFORM AB25.
            MOVE "Item Code"  TO ORD-H1.
            MOVE WS-DESC-H1  TO ORD-H2.
            MOVE "Quantity"  TO ORD-H3.
           IF WS-TYPE = "C" OR "B"
              MOVE "Unit Cost"  TO ORD-H4.
           IF WS-TYPE = "S" OR "B"
              MOVE "Unit Sell"  TO ORD-H5.
           IF WS-TYPE = "C" OR "B"
              MOVE "Total Cost" TO ORD-H6
           ELSE
              MOVE "Total Sell" TO ORD-H6.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       BQ10.
            MOVE SPACES  TO W10-EXT-ITEM.
             MOVE ZERO           TO W10-QUANT W25-SCOST.
             MOVE 0701           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
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
       BQ15.
             PERFORM AA300.
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S05-EXT
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S05-ITM
        ELSE
           ACCEPT S05
        END-IF
    ELSE
    IF W10-SLNGTH = 3
              ACCEPT W10-I3 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 4
              ACCEPT W10-I4 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 5
              ACCEPT W10-I5 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 6
              ACCEPT W10-I6 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 7
              ACCEPT W10-I7 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 8
              ACCEPT W10-I8 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 9
              ACCEPT W10-I9 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 10
              ACCEPT W10-I10 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 11
              ACCEPT W10-I11 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 12
              ACCEPT W10-I12 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 13
              ACCEPT W10-I13 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 14
              ACCEPT W10-I14 AT 0602
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BQ35
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BQ15
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
       GO TO BQ15
        END-EVALUATE
        PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S05-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S05-ITM
     ELSE
         DISPLAY S05
     END-IF
        ELSE
     DISPLAY S05
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO BQ15.
      PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BQ35.

       BQ15A.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S05-EXT
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S05-ITM
        ELSE
           DISPLAY S05
        END-IF
    ELSE
        DISPLAY S05.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S05-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S05-ITM
     ELSE
         DISPLAY S05
     END-IF
        ELSE
     DISPLAY S05
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BQ15
               ELSE
                   GO TO BQ15A.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BQ15.

       BQ16.
      MOVE STK-CODE  TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
              MOVE "Item not manufactured or assembled"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BQ15.
            DISPLAY STK-DESC AT 0702
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
    IF NOT (STK-DESC2 = SPACES)
        DISPLAY STK-DESC2 AT 0802
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BQ15.
      *
      *    ****    Q U A N T I T Y
      *
       BQ20.
             ACCEPT S35 AT 0733.
           IF W10-QUANT = ZERO
               GO TO BQ15.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BQ15.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
      *
      *    ****    S A V E   S T O C K  R E C O R D
      *
             MOVE STK-RECORD1    TO W15-RECORD1.
            MOVE 1   TO WS-S6.
             DISPLAY "Busy calculating cost price" AT 2312
                      WITH FOREGROUND-COLOR 14.
             MOVE ZERO           TO W10-MCOST W10-MSELL
                                    W25-PCST  W25-PTOT.
      *
      *    ****    E N D   O F  P R O D U C T I O N   D E T A I L S ???
      *
       BQ25.
          IF PRT-EXT-CODE(WS-S6) = SPACES
               GO TO BQ30.
            MOVE PRT-EXT-CODE(WS-S6)
                                 TO STK-CODE.
             MOVE 0              TO WS-ACTION.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        MOVE STK-CODE  TO ORD-EXT-ITEM
        MOVE "Invalid code"
     TO ORD-DESC
        IF W02-LINAGE < W02-PRN-LENGTH
     MOVE 1  TO WS-ADVANCE
     PERFORM CALL-PRINTUTL
              ELSE
     MOVE 99  TO WS-ADVANCE
     PERFORM CALL-PRINTUTL
           PERFORM BQ05
        END-IF
              MOVE SPACES  TO REP-DETAIL1
        GO TO BQ28.
             PERFORM AY05 THRU AY59.
           IF STK-COST > STK-AVGE
              COMPUTE W10-COST = (PRT-QUANT(WS-S6) * STK-COST)
           ELSE
              COMPUTE W10-COST = (PRT-QUANT(WS-S6) * STK-AVGE).
      COMPUTE W10-SELL = (PRT-QUANT(WS-S6) * STK-SELL).
          IF STK-IND = 0 OR 1
        COMPUTE W25-SCOST = W25-SCOST + (W10-COST * W10-QUANT)
           ELSE
              ADD W10-COST TO W25-SCOST.
      COMPUTE W10-MQUANT = PRT-QUANT(WS-S6) * W10-QUANT.
             COMPUTE W10-MCOST = W10-MCOST + W10-COST.
          IF STK-IND = 0
              SUBTRACT W10-MQUANT
     FROM STK-QUANT
        IF WS-CARDEX = "Y"
     INITIALIZE STX-REC1
     MOVE STK-CODE TO STX-EXT-CODE
     MOVE W12-T-YMD
     TO STX-DTE
     MOVE ZERO  TO STX-SEQ
     MOVE LS0-NO  TO STX-USER
     MOVE "PRODCT" TO STX-PROG
     MOVE LS-USER  TO STX-WS
     MOVE "Used for Production"
     TO STX-REMARKS
     MULTIPLY -1 BY W10-MQUANT
     GIVING STX-QNT
     MULTIPLY -1 BY W10-COST
     GIVING STX-COST
     COMPUTE STX-SELL ROUNDED = (W10-COST +
      ((W10-COST * STK-MARKUP) / 100)) * -1
     PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
     MOVE 1  TO WS-ACTION
     PERFORM AY46 THRU AY59
        END-IF
              IF NOT(STK-DISC > ZERO)
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
           IF STK-QUANT NOT > STK-LEVEL
               MOVE 1  TO STK-DISC
               PERFORM BG80.
             ADD W10-MQUANT      TO STK-MTD STK-YTD.
            COMPUTE W10-CCOST = (PRT-QUANT(WS-S6) * STK-AVGE).
             ADD W10-CCOST       TO STK-MTDC STK-MTDV STK-YTDC STK-YTDV.
             MOVE SPACES         TO REP-LINE1.
            MOVE PRT-QUANT(WS-S6)
                                 TO ORD-QUANT.
             MOVE STK-DESC       TO ORD-DESC.
    IF WS-USE-ITM = "Y"
        MOVE STK-ITEM  TO ORD-ITEM
        MOVE "/"   TO ORD-SL
        MOVE STK-ITM  TO ORD-ITM
    ELSE
              MOVE STK-CODE  TO ORD-EXT-ITEM.
    IF WS-TYPE = "B" OR "C"
        COMPUTE ORD-COST = (W10-COST / PRT-QUANT(WS-S6))
               MOVE W10-COST     TO ORD-VALUE.
           IF WS-TYPE = "B" OR "S"
               MOVE STK-SELL     TO ORD-SELL.
           IF WS-TYPE = "S"
               MOVE W10-SELL     TO ORD-VALUE.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BQ05.
            MOVE SPACES  TO REP-DETAIL1.
    IF (WS-USE-PACKS = "Y") AND
       (STK-USE-PACKS = "Y")
        PERFORM BQ90 THRU BQ105.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

       BQ28.
           IF WS-S6 < 30
               ADD 1             TO WS-S6
               GO TO BQ25.

       BQ30.
           IF WS-S6 = 1
               GO TO BQ40.
             MOVE W15-RECORD1    TO STK-RECORD1.
      COMPUTE W10-COST ROUNDED = W10-MCOST * 1.
      MOVE W10-COST  TO W80-5.

       BQ32.
           IF WS-TYPE = "B" OR "C"
              DISPLAY W80-5 AT 0749
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
            COMPUTE W10-SELL ROUNDED = (W10-COST +
             ((W10-COST * STK-MARKUP) / 100)).
             MOVE W10-SELL       TO W10-MSELL.
           IF STK-CMARKUP NOT > ZERO
               MOVE STK-MARKUP   TO STK-CMARKUP.
            COMPUTE W10-CASH ROUNDED = (W10-COST +
             ((W10-COST * STK-CMARKUP) / 100)).
           IF STK-WMARKUP NOT > ZERO
               MOVE STK-CMARKUP  TO STK-WMARKUP.
            COMPUTE W10-WSALE ROUNDED = (W10-COST +
             ((W10-COST * STK-WMARKUP) / 100)).
             MULTIPLY W10-COST BY W10-QUANT
                                 GIVING W25-VALUE.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
           IF W05-ROUND = "E"
               MOVE W10-MSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-SELL W10-MSELL
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

       BQ33.
             ACCEPT S36 AT 0759.
    IF W10-SELL < W10-WSALE
        MOVE "Selling price margin too small"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
        MOVE W10-WSALE  TO W10-SELL
               GO TO BQ33.
           IF WS-TYPE = "B" OR "C"
               MOVE W25-VALUE    TO ORD-VALUE
              DISPLAY ORD-VALUE AT 0769
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
        MOVE ZERO  TO W90-BAL.
    IF STK-IND = 0
        IF STK-QUANT > ZERO
           MULTIPLY STK-QUANT BY STK-AVGE
     GIVING W90-BAL.
             ADD W25-SCOST       TO W25-PCOST W25-PCST.
             ADD W25-VALUE       TO W25-TOTAL W25-PTOT.
             COMPUTE W25-TSELL = W25-TSELL + (W10-MSELL * W10-QUANT).
             ADD W25-VALUE       TO W90-BAL.
    IF STK-IND = 0
              ADD W10-QUANT  TO STK-QUANT
        IF WS-CARDEX = "Y"
     INITIALIZE STX-REC1
     MOVE STK-CODE TO STX-EXT-CODE
     MOVE W12-T-YMD
     TO STX-DTE
     MOVE ZERO  TO STX-SEQ
     MOVE LS0-NO  TO STX-USER
     MOVE "PRODCT" TO STX-PROG
     MOVE LS-USER  TO STX-WS
     MOVE "Production"
     TO STX-REMARKS
     MOVE W10-QUANT
     TO STX-QNT
     MOVE W25-VALUE
     TO STX-COST
     COMPUTE STX-SELL ROUNDED = (W25-VALUE +
      ((W25-VALUE * STK-MARKUP) / 100))
     PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
     MOVE 1  TO WS-ACTION
     PERFORM AY46 THRU AY59.
             MOVE W10-QUANT      TO ORD-QUANT.
    IF (STK-IND = 0) AND
       (STK-QUANT > W10-QUANT)
              DIVIDE W90-BAL BY STK-QUANT
     GIVING STK-AVGE
    ELSE
        MOVE W10-COST  TO STK-AVGE.
           IF STK-AVGE < ZERO
               COMPUTE STK-AVGE = STK-AVGE * -1.
             MOVE W10-COST       TO STK-COST.
           IF WS-TYPE = "B" OR "C"
               MOVE W10-COST     TO ORD-COST.
           IF W10-MSELL > STK-SELL
               MOVE W10-MSELL    TO STK-SELL.
           IF W10-CASH > STK-CASH
               MOVE W10-CASH     TO STK-CASH.
           IF W10-WSALE > STK-WSALE
               MOVE W10-WSALE    TO STK-WSALE.
             MOVE STK-DESC       TO ORD-DESC.
    IF WS-USE-ITM = "Y"
        MOVE STK-ITEM  TO ORD-ITEM
        MOVE "/"   TO ORD-SL
        MOVE STK-ITM  TO ORD-ITM
    ELSE
              MOVE STK-CODE  TO ORD-EXT-ITEM.
    IF WS-TYPE = "B" OR "S"
               MOVE W10-MSELL    TO ORD-SELL
               COMPUTE ORD-VALUE = W10-MSELL * W10-QUANT.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BQ05.
            MOVE SPACES  TO REP-DETAIL1.
    IF (WS-USE-PACKS = "Y") AND
       (STK-USE-PACKS = "Y")
        PERFORM BQ70 THRU BQ85.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             SUBTRACT W25-PCST   FROM STK-TOTAL.
             ADD W25-PTOT        TO STK-TOTAL.
             PERFORM AY70 THRU AY999.

       BQ34.
             GO TO BQ40.

       BQ35.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "TOTAL"  TO ORD-DESC.
           IF WS-TYPE = "S"
               MOVE W25-TSELL    TO ORD-VALUE
           ELSE
               MOVE W25-TOTAL    TO ORD-VALUE.
      PERFORM CALL-PRINTUTL.
            MOVE "***** END REPORT *****" TO REP-DETAIL1.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
      MOVE SPACES  TO REP-DETAIL1.
            GO TO BQ999.
       BQ40.
      MOVE "ENTER to continue - E to exit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF WS-OPTION = "E"
               GO TO BQ35.
             GO TO BQ10.

       BQ70.
      MOVE 2   TO WS-S1.

       BQ75.
    IF STK-UNT(WS-S1) = ZERO
        GO TO BQ80.
            MULTIPLY STK-UNT(WS-S1) BY STK-COST
     GIVING W10-CCOST.
            MOVE STK-SL(WS-S1)  TO W10-CSELL.
          IF STK-SL(WS-S1) < W10-CCOST
              MOVE ZERO  TO STK-MKUP(WS-S1)
    ELSE
              COMPUTE STK-MKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
            MOVE STK-CSH(WS-S1) TO W10-CSELL.
          IF STK-CSH(WS-S1) < W10-CCOST
              MOVE ZERO  TO STK-CMKUP(WS-S1)
    ELSE
              COMPUTE STK-CMKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
            MOVE STK-WSL(WS-S1) TO W10-CSELL.
          IF STK-WSL(WS-S1) < W10-CCOST
        MOVE ZERO  TO STK-WMKUP(WS-S1)
    ELSE
              COMPUTE STK-WMKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
      *
      *    ****    C H E C K   I F   E X C L U S I V E  R O U N D I N G
      *     R E Q U I R E D
      *
           IF W05-ROUND = "E"
        MOVE STK-SL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-SL(WS-S1)
        MOVE STK-WSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-WSL(WS-S1)
        MOVE STK-CSH(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-CSH(WS-S1).
    IF STK-TAX NOT = ZERO
        MOVE STK-TAX  TO W10-TAX
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE
        COMPUTE W10-VSL(WS-S1) ROUNDED = W10-SL(WS-S1) +
     (W10-SL(WS-S1) * W05-RTE)
        COMPUTE W10-VWSL(WS-S1) ROUNDED = W10-WSL(WS-S1) +
     (W10-WSL(WS-S1) * W05-RTE)
        COMPUTE W10-VCSH(WS-S1) ROUNDED = W10-CSH(WS-S1) +
     (W10-CSH(WS-S1) * W05-RTE)
           ELSE
        MOVE W10-WSL(WS-S1)
     TO W10-VWSL(WS-S1)
        MOVE W10-CSH(WS-S1)
     TO W10-VCSH(WS-S1)
        MOVE W10-SL(WS-S1)
     TO W10-VSL(WS-S1).
           IF W05-ROUND = "I"
        MOVE W10-VSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VSL(WS-S1)
        MOVE W10-VWSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VWSL(WS-S1)
        MOVE W10-VCSH(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VCSH(WS-S1)
        PERFORM CB300.
      MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
      MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
      MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).

       BQ80.
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BQ75.

       BQ85.
      EXIT.

       BQ90.
      MOVE 2   TO WS-S1.

       BQ95.
    IF STK-UNT(WS-S1) = ZERO
        GO TO BQ100.
            MULTIPLY STK-UNT(WS-S1) BY STK-COST
     GIVING W10-CCOST.
            MOVE STK-SL(WS-S1)  TO W10-CSELL.
          IF STK-SL(WS-S1) < W10-CCOST
              MOVE ZERO  TO STK-MKUP(WS-S1)
    ELSE
              COMPUTE STK-MKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
            MOVE STK-CSH(WS-S1) TO W10-CSELL.
          IF STK-CSH(WS-S1) < W10-CCOST
              MOVE ZERO  TO STK-CMKUP(WS-S1)
    ELSE
              COMPUTE STK-CMKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
            MOVE STK-WSL(WS-S1) TO W10-CSELL.
          IF STK-WSL(WS-S1) < W10-CCOST
        MOVE ZERO  TO STK-WMKUP(WS-S1)
    ELSE
              COMPUTE STK-WMKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
      *
      *    ****    C H E C K   I F   E X C L U S I V E  R O U N D I N G
      *     R E Q U I R E D
      *
           IF W05-ROUND = "E"
        MOVE STK-SL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-SL(WS-S1)
        MOVE STK-WSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-WSL(WS-S1)
        MOVE STK-CSH(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-CSH(WS-S1).
    IF STK-TAX NOT = ZERO
        MOVE STK-TAX  TO W10-TAX
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE
        COMPUTE W10-VSL(WS-S1) ROUNDED = W10-SL(WS-S1) +
     (W10-SL(WS-S1) * W05-RTE)
        COMPUTE W10-VWSL(WS-S1) ROUNDED = W10-WSL(WS-S1) +
     (W10-WSL(WS-S1) * W05-RTE)
        COMPUTE W10-VCSH(WS-S1) ROUNDED = W10-CSH(WS-S1) +
     (W10-CSH(WS-S1) * W05-RTE)
           ELSE
        MOVE W10-WSL(WS-S1)
     TO W10-VWSL(WS-S1)
        MOVE W10-CSH(WS-S1)
     TO W10-VCSH(WS-S1)
        MOVE W10-SL(WS-S1)
     TO W10-VSL(WS-S1).
           IF W05-ROUND = "I"
        MOVE W10-VSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VSL(WS-S1)
        MOVE W10-VWSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VWSL(WS-S1)
        MOVE W10-VCSH(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VCSH(WS-S1)
        PERFORM CB300.
      MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
      MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
      MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).

       BQ100.
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BQ95.

       BQ105.
      EXIT.

       BQ999.
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
      MOVE LS-USUB  TO WS-USUB
        WS-DRAW.
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
      MOVE "AUDITF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-AUDITF.
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
      MOVE "SPARTS"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SPARTS.
      MOVE "STKDEX"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKDEX.
      MOVE "STKMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKMEM.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
      MOVE PAR-CRDX  TO WS-CARDEX.
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.

       ZA01B.
            OPEN I-O AUDIT.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
        IF NOT (WS-STATUS = "41")
           MOVE 1  TO WS-F-ERROR
           PERFORM OPEN-ERROR.
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
    IF NOT (PAR-DESC-H1 = X"0000000000000000000000000000")
        MOVE PAR-DESC-H1  TO WS-DESC-H1.
    IF NOT (PAR-DESC-H2 = X"0000000000000000000000000000")
        MOVE PAR-DESC-H2  TO WS-DESC-H2.
             MOVE PAR-WS-HD          TO WS-WS-HD.
             MOVE PAR-CS-HD          TO WS-CS-HD.
             MOVE PAR-RT-HD          TO WS-RT-HD.
      GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
       ZA60.
            MOVE WS-G1   TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
      MOVE WS-G8   TO WS-BCH(WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
      MOVE WS-G11  TO WS-BCH(1).
      MOVE WS-G12  TO WS-BCH(80).
      MOVE WS-G14  TO WS-TCHR(1) WS-BCHR(1).
      MOVE WS-G13  TO WS-TCHR(80) WS-BCHR(80).
            MOVE WS-G2   TO WS-TCHR(18) WS-TCHR(49)
              WS-TCHR(60) WS-TCHR(70).
            MOVE WS-G3   TO WS-MCHR(18) WS-MCHR(49)
              WS-MCHR(60) WS-MCHR(70)
        WS-MCHR(1) WS-MCHR(80).
            MOVE WS-G4   TO WS-BCHR(18) WS-BCHR(49)
              WS-BCHR(60) WS-BCHR(70).
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
          IF WS-F-ERROR = 1
              MOVE W02-AUDITF  TO WS-FILE
              MOVE WS-AUDKEY  TO WS-KEY
           ELSE
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
               MOVE PRT-ITEM     TO WS-KEYX
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
          IF WS-F-ERROR = 46
              MOVE W02-STKDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STX-KEY  TO WS-KEYX.
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
