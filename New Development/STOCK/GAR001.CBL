      $SET LINKCOUNT"256" GNT"GAR001.GNT"
      ******************************************************************
      *                                                                *
      *    ******      **     *******     ****      ****       **      *
      *   **    **    ****    **    **   **  **    **  **     ***      *
      *   **         **  **   **   **   **    **  **    **     **      *
      *   **        **    **  ******    **    **  **    **     **      *
      *   **   ***  ********  **   **   **    **  **    **     **      *
      *   **    **  **    **  **    **   **  **    **  **      **      *
      *    ******   **    **  **    **    ****      ****     ******    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   G U A R A N T E E   M O D U L E          *
      *                                                                *
      *     VERSION 8.15.00 - April 2011          *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GAR001.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1991.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2016
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

       COPY CONTROL.SL.

       COPY GARTEE.SL.

       COPY PARAM.SL.

      /
       DATA DIVISION.
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FD.

       COPY GARTEE.FD.

       COPY PARAM.FD.

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-IXS2         PIC  9(04)    COMP-5.
       77  WS-PARKEY       PIC  9(04)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5.
       77  WS-ETYPE    PIC X(01).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-CARDEX    PIC X(01).
       77  WS-LINES    PIC 9(02) VALUE 0.
       77  WS-PAGE         PIC  9(02)    COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-NUM          PIC  Z9.
       77  WS-ERROR        PIC  9(01).
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      /
       01  WS-DB-LINE.
           03  WS-TOP-LINE.
               05  WS-TCHAR PIC X(01) OCCURS 80.
           03  WS-TOP-LNE2.
               05  WS-TCR  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LINE.
               05  WS-MCHAR PIC X(01) OCCURS 80.
           03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LINE.
               05  WS-BCHAR PIC X(01) OCCURS 80.
           03  WS-BOT-LNE2.
               05  WS-BCR  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-MID-LNE.
               05  WS-MCHR PIC X(01) OCCURS 80.
    03  WS-MGA-LNE REDEFINES WS-MID-LNE.
        05  WS-MGA1     PIC  X(01).
        05  WS-MGA78    PIC  X(78).
        05  WS-MGA80    PIC  X(01).
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-TP-LNE.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-MD-LNE.
               05  WS-MCH  PIC X(01) OCCURS 80.
           03  WS-BT-LNE.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-MM-LINE.
        05  FILLER    PIC  X(01) VALUE X"B3".
        05  WS-SPACES PIC  X(60) VALUE SPACES.
        05  FILLER    PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

       COPY WS.WS.

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "GAR".
    03  WS-PROG        PIC  X(03) VALUE "001".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY GARTEE.ID.

       COPY PARAM.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "GAR001.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       01  W10-GUARANTEE.
    03  W10-EXT-ITEM.
              05  W10-ITEM    PIC  X(14).
        05  W10-ITM     PIC  X(03).
        05  FILLER      PIC  X(01).
    03  W10-SERNO       PIC  X(18).
           03  W10-DESC.
               05  W10-SDSC.
   07  W10-DLUP  PIC  X(20).
   07  FILLER    PIC  X(10).
           03  W10-NAME.
        05  W10-NLUP.
     07  W10-NMELUP
          PIC  X(10).
     07  FILLER  PIC  X(10).
               05  FILLER      PIC  X(20).
    03  W10-DATE1       PIC  9(08)    COMP-3.
    03  W10-DATE2       PIC  9(08)    COMP-3.
           03  W10-PERIOD      PIC  9(02)    COMP-5.
           03  W10-INV         PIC  X(08).

       01  W10-SPARES.
          03  W10-DATE        PIC X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       COPY W20.WS.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(03).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).

       01  W30-GUAR.
    03  W30-KEY.
        05  W30-EXT-CODE.
         07  W30-CODE  PIC  X(14).
   07  W30-ITM   PIC  X(03).
   07  FILLER    PIC  X(01).
              05  W30-SERIAL  PIC  X(18).
          03  W30-DESC        PIC  X(30).
    03  W30-NAME.
        05  W30-NKEY    PIC  X(12).
               05  FILLER      PIC  X(28).
    03  W30-DATE1       PIC  9(08)  COMP.
    03  W30-DATE2       PIC  9(08)  COMP.
    03  W30-PERIOD      PIC  9(02)  COMP-5.
    03  W30-INV        PIC  X(08).
    03  FILLER        PIC  X(15).

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-ITEMS OCCURS 18.
        05  W75-CODE   PIC X(18).
        05  W75-SERNO  PIC X(18).

       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
       SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Exit".

       01  S-PROG.
          03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "GUARANTEE PROGRAM CONTROL".

       01  S-MENU.
    03  LINE  6 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "¿".
    03  LINE  7 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³        ".
    03        COLUMN 32 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "GUARANTEE OPTIONS".
    03        COLUMN 49 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 0
     VALUE "       ³".
    03  LINE  8 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3
     VALUE " Amend Guarantee Record     ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "³".
    03  LINE  9 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3
     VALUE " Enquiry                 ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "³".
          03  LINE 10 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3
     VALUE " Delete a Guarantee Record  ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "³".
          03  LINE 11 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "4".
    03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3
     VALUE " Amend a Serial Number      ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "³".
          03  LINE 12 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "5".
    03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3
     VALUE " Replace an Item            ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "³".
          03  LINE 13 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "6".
    03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3
     VALUE " Delete Expired Records     ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "³".
          03  LINE 14 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3
     VALUE " Exit                       ".
    03        COLUMN 56 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
     VALUE "³".
    03  LINE 15 COLUMN 25 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 5 HIGHLIGHT
     VALUE "À".
    03        COLUMN 26 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 0 VALUE
     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S02.
           03  LINE 12 COLUMN 12 VALUE "Enter Password >      <".

       01  S03.
          03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5 PIC X(06)
                                 USING W40-CSHPASS AUTO SECURE.
       01  S04.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "AMEND GUARANTEE RECORD".
       01  S05.
    03  LINE  4 COLUMN  6 VALUE "Item Code       :".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(14) USING WS-SPACES.
    03  LINE  5 COLUMN  6 VALUE "Serial Number   :".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING WS-SPACES.
    03  LINE  6 COLUMN  6 VALUE "Item Description:".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(30) USING WS-SPACES.
    03  LINE  7 COLUMN  6 VALUE "Client Name     :".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(40) USING WS-SPACES.
    03  LINE  8 COLUMN  6 VALUE "Date Purchased  :".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(10) USING WS-SPACES.
    03  LINE  9 COLUMN  6 VALUE "Expiry Date     :".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(10) USING WS-SPACES.
    03  LINE 10 COLUMN  6 VALUE "Guarantee Period:".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(02) USING WS-SPACES.
    03        COLUMN 27 VALUE "(Months)".
    03  LINE 11 COLUMN  6 VALUE "Invoice Number  :".
    03        COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(08) USING WS-SPACES.

       01  S05-ITM.
    03  LINE  4 COLUMN 38 FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
     VALUE "/".
    03        COLUMN 39 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(03) USING WS-SPACES.

       01  S05-EXT.
    03  LINE  4 COLUMN 38 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(04) USING WS-SPACES.

       01  S06.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "GUARANTEE ENQUIRY".

       01  S07.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "DELETE GUARANTEE RECORD".

       01  S08.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "AMEND SERIAL NUMBER".

       01  S09.
          03  LINE  2 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "REPLACE AN ITEM UNDER WARRANTY".

       01  S10.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "DELETE EXPIRED RECORDS".
       01  S11.
           02  S11A.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(14) USING W10-ITEM AUTO.
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING W10-SERNO AUTO.
           02  S11B.
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC  X(30) USING W10-SDSC AUTO.
           03  S11C.
    05  LINE  7 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC  X(40) USING W10-NAME AUTO.
    05  LINE  8 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  99/99/9999 USING W10-DATE1 AUTO.
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  99/99/9999 USING W10-DATE2 AUTO.
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC  Z9 USING W10-PERIOD AUTO.
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC  X(08) USING W10-INV AUTO.

       01  S11-ITM.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(14) USING W10-ITEM AUTO.
    03        COLUMN 38 FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
     VALUE "/".
    03        COLUMN 39 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(03) USING W10-ITM AUTO.
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING W10-SERNO AUTO.

       01  S11-EXT.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING W10-EXT-ITEM AUTO.
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING W10-SERNO AUTO.

       01  S12.
    03  LINE 15 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING W10-EXT-ITEM AUTO.
    03  LINE 16 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(18) USING W10-SERNO AUTO.

       COPY GARLUP.CRT.

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-SALEV < 1
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM BA000.
            GO TO AZ000-EOJ.

       AA49.
      EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             PERFORM GUAR-LOOKUP.
             PERFORM RESTORE-SCREEN.
       AA99.
      EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
      DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Item code & Ser.No./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Item code/ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Serial No./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Client name"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA305.
             EXIT.
      *
      *    ****   G E T   G U A R A N T E E   R E C O R D   U S I N G
      *           I T E M   C O D E   &   S E R I A L   N U M B E R
      *
       AA500         SECTION.
       AA501.
            MOVE 18   TO W50-LENGTH.
    IF W10-EXT-ITEM = SPACES
        MOVE "!"   TO W10-EXT-ITEM.
       AA505.
            MOVE W10-EXT-ITEM   TO W50-DATA.
      *
      *    ****    R E M O V E  L E A D I N G  S P A C E S
      *
           IF W50-1 = SPACE
        MOVE W50-17  TO W10-EXT-ITEM
               GO TO AA505.
      *
      *    ****    C O N V E R T   I T E M   C O D E   T O   U P P E R
      *     C A S E   C H A R A C T E R S
      *
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
      MOVE W50-EXT-ITEM  TO W10-EXT-ITEM.

       AA510.
            MOVE W50-EXT-ITEM  TO GUA-EXT-CODE.

       AA512.
            MOVE 18   TO W50-LENGTH.
           IF W10-SERNO = SPACE
               MOVE "!"          TO W10-SERNO.

       AA515.
             MOVE W10-SERNO      TO W50-DATA.
      *
      *    ****    R E M O V E  L E A D I N G  S P A C E S
      *
    IF W50-1 = SPACE
        MOVE W50-17  TO W10-SERNO
               GO TO AA515.
      *
      *    ****    C O N V E R T   S E R I A L  N O   T O   U P P E R
      *     C A S E   C H A R A C T E R S
      *
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-EX-CDE  TO W10-SERNO.

       AA520.
             MOVE W10-SERNO      TO GUA-SERIAL.

       AA525.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR NOT = 0
               GO TO AA599.
             MOVE GUA-DESC       TO W10-DESC.
             MOVE GUA-NAME       TO W10-NAME.
             MOVE GUA-DATE1      TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
      MOVE W22-DTE3  TO W10-DATE1.
             MOVE GUA-DATE2      TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
      MOVE W22-DTE3  TO W10-DATE2.
             MOVE GUA-PERIOD     TO W10-PERIOD.
             MOVE GUA-INV        TO W10-INV.

       AA599.
             EXIT.

       COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

       COPY DATE.CNV.

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY CONTROL.RD.

       COPY GARTEE.RD.

       COPY PARAM.RD.

      /
       AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY GARTEE.WR.

       COPY PARAM.WR.

       COPY GARTEE.LUP.

       COPY APAC.HLP.

      /
       AZ000-END               SECTION.
       AZ000-EOJ.
            CLOSE GARTEE
                   NETWORK
                   PARAM.
       AZ100.
             EXIT PROGRAM.

      /
       BA000           SECTION.
       BA00.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      MOVE 7   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 9   TO SHADE-LINES.
      DISPLAY S-MENU.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.
    IF LS-DAYS > 65
        COMPUTE WS-NUM = 15 - (LS-DAYS - 65)
        DISPLAY "This package must be registered within " AT 2302
   WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12
   WS-NUM WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 15 BLINK
         " days" WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 12.
       BA02.
      MOVE 7   TO TOP-ROW.
      MOVE 13   TO BOTTOM-ROW.
      MOVE 6   TO SCREEN-LIN.
      MOVE 27   TO SCREEN-COL.
      MOVE 28   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       BA05.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO BA15
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO BA10
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO BA15
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BA05
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO BA18
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF WS-OPTION < 0 OR > 6
     CALL X"E5"
     GO TO BA05
        END-IF
        GO TO BA16
    ELSE
        CALL X"E5"
        GO TO BA05.

       BA15.
    IF SCREEN-LIN < 7
        CALL X"E5"
        GO TO BA05
    ELSE
    IF SCREEN-LIN = 7
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 8
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 9
        MOVE "3"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 10
        MOVE "4"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 11
        MOVE "5"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 12
        MOVE "6"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 13
        MOVE "0"   TO WS-OPTION.
    GO TO BA18.

       BA16.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 13   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "1"
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "2"
        MOVE 8   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "3"
        MOVE 9   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "4"
        MOVE 10   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "5"
        MOVE 11   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "6"
        MOVE 12   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       BA18.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
          IF WS-OPTION = "0"
               GO TO BA999.
           IF WS-OPTION = "1"
              PERFORM CA000
          ELSE
           IF WS-OPTION = "2"
              PERFORM DA000
          ELSE
           IF WS-OPTION = "3"
              PERFORM EA000
          ELSE
           IF WS-OPTION = "4"
        PERFORM FA000
          ELSE
    IF WS-OPTION = "5"
        PERFORM GA000
          ELSE
    IF WS-OPTION = "6"
        PERFORM HA000.
      GO TO BA00.
       BA999.
             EXIT.

      /
      *    ****    A M E N D   E X I S T I N G   G U A R A N T E E
      *
       CA000        SECTION 5.
       CA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S04.
             DISPLAY S02.

       CA01.
             ACCEPT S03.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
           IF NOT (W40-CSHPASS = W85-SUPER (1) OR
                                 W85-SUPER (4) OR
                                 ")&^%$!")
              MOVE "Invalid password"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO CA999.

       CA02.
             PERFORM ERASE-SCREEN.
             DISPLAY S04.
      DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
             INITIALIZE W10-GUARANTEE.

       CA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO CA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
                     MOVE "K"    TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO CA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO CA05.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO CA999.

       CA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
               PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO CA05
               ELSE
                   GO TO CA10.
             DISPLAY S11B.

       CA15.
             ACCEPT S11C.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO CA15.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
             MOVE W10-DATE1      TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Invalid date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO CA15.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
             MOVE W10-NAME       TO GUA-NAME.
           IF W22-DTE1 NOT = GUA-DATE1
               PERFORM CA20 THRU CA25.
             PERFORM REWRITE-GARTEE-UNLOCK THRU WRITE-GARTEE-EXIT.
      GO TO CA02.

       CA20.
      MOVE W10-DATE1  TO W22-DTE3.
      PERFORM SWITCH-DATE-TO-CYMD THRU SWITCH-DATE-EXIT.
      MOVE W22-DTE2  TO GUA-DATE1 W20-DTE.
             ADD GUA-PERIOD      TO W20-MONTH.

       CA25.
           IF W20-MONTH > 12
               SUBTRACT 12       FROM W20-MONTH
               IF W20-YEAR = 99
     MOVE ZERO  TO W20-YEAR
     ADD 1  TO W20-CENT
                   GO TO CA25
               ELSE
                   ADD 1         TO W20-YEAR
                   GO TO CA25.
      MOVE W20-DTE  TO GUA-DATE2.

       CA999.
             EXIT.
      /
      *    ****    G U A R A N T E E   E N Q U I R Y
      *
       DA000        SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S06.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.

       DA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO DA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO DA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO DA05.
             DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO DA999.
       DA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
              PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO DA05
               ELSE
                   GO TO DA10.
             DISPLAY S11B.
            MOVE "ENTER to continue"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO DA999
               END-EVALUATE.
      GO TO DA00.
       DA999.
             EXIT.
      /
      *    ****    D E L E T E   E X I S T I N G   G U A R A N T E E
      *
       EA000        SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S07.
             DISPLAY S02.
       EA01.
             ACCEPT S03.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
           IF NOT (W40-CSHPASS = W85-SUPER (1) OR
                                 W85-SUPER (4) OR
                                 ")&^%$!")
              MOVE "Invalid password"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO EA999.
       EA02.
             PERFORM ERASE-SCREEN.
             DISPLAY S07.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.
       EA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO EA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO EA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO EA05.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO EA999.
       EA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
               PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO EA05
               ELSE
                   GO TO EA10.
             DISPLAY S11B.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA02.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
             DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
      PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO EA02.
       EA999.
             EXIT.
      /
      *    ****    A M E N D   A   S E R I A L   N U M B E R
      *
       FA000        SECTION 5.
       FA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S08.
             DISPLAY S02.
       FA01.
             ACCEPT S03.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
           IF NOT (W40-CSHPASS = W85-SUPER (1) OR
                                 W85-SUPER (4) OR
                                 ")&^%$!")
              MOVE "Invalid password"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO FA999.
       FA02.
             PERFORM ERASE-SCREEN.
      DISPLAY S08.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.
       FA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO FA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO FA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO FA05.
             DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO FA999.
       FA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
              PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO FA05
               ELSE
                   GO TO FA10.
             DISPLAY S11B.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA02.
             MOVE GUA-RECORD1    TO W30-GUAR.
      DISPLAY "New Serial No.  :" AT 1306.
             MOVE SPACES         TO W10-SERNO.
       FA15.
             ACCEPT W10-SERNO AT 1324
      WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
      PERFORM CLEAR-L50.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO FA02
               END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA15.
             MOVE W10-SERNO      TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
        MOVE "Serial number duplicated"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO FA15.
      MOVE ZERO   TO WS-F-ERROR.
             PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
      MOVE W30-GUAR  TO GUA-RECORD1.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
             PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO FA02.
       FA999.
             EXIT.
      /
      *    ****   R E P L A C E   A N   I T E M
      *
       GA000        SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S09.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.
       GA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO GA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO GA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO GA05.
             DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO GA999.
       GA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
              PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO GA05
               ELSE
                   GO TO GA10.
             DISPLAY S11B.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA00.
             MOVE GUA-RECORD1    TO W30-GUAR.
             DISPLAY "REPLACEMENT DETAILS" AT 1306
                      WITH FOREGROUND-COLOR 14.
      DISPLAY "Item Code        :" AT 1506.
      DISPLAY "Serial Number    :" AT 1606.
      MOVE SPACES  TO W10-EXT-ITEM W10-SERNO.
       GA15.
             ACCEPT S12.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO GA00
               END-EVALUATE.
      PERFORM CLEAR-L50.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA15.
    IF W10-EXT-ITEM = SPACES OR
       W10-SERNO = SPACES
        MOVE "Data Incomplete"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
        GO TO GA15.
      MOVE W10-EXT-ITEM  TO GUA-EXT-CODE.
             MOVE W10-SERNO      TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
        MOVE "Code & Serial number duplicated"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO GA15.
             PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
             MOVE W30-GUAR       TO GUA-RECORD1.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
             PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO GA00.
       GA999.
             EXIT.
      /
      *    ****   D E L E T E   E X P I R E D   R E C O R D S
      *
       HA000        SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S10.
             DISPLAY "Are you sure Y/N?" AT 0506
                      WITH FOREGROUND-COLOR 14.

       HA05.
            ACCEPT WS-OPTION AT 0524
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "Y" OR "N")
               GO TO HA05.
           IF WS-OPTION = "N"
               GO TO HA999.
             MOVE W12-T-YMD      TO W22-DTE2.
           IF W22-MM2 < 4
                ADD 12           TO W22-MM2
  SUBTRACT 1  FROM W22-CY2.
             SUBTRACT 3          FROM W22-MM2.
             DISPLAY "Reading record :" AT 0706.
      MOVE "!"   TO GUA-EXT-CODE GUA-SERIAL.
             PERFORM START-AT-GARTEE-KEY THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = 9
               GO TO HA999.

       HA10.
             PERFORM READ-GARTEE-NEXT THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = 9
               GO TO HA999.
             DISPLAY GUA-KEY AT 0723 WITH FOREGROUND-COLOR 11.
           IF GUA-DATE2 < W22-DTE2
               PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO HA10.

       HA999.
             EXIT.
      /
       ZA000-INIT        SECTION 9.
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
             MOVE 1              TO USER-ACTION
                                    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
             MOVE ZERO           TO USER-NUMBER.
             MOVE 11             TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
             MOVE 53             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
             MOVE 64             TO MOUSE-FUNC.
             MOVE 1              TO MOUSE-PARAM.
             CALL X"AF" USING MOUSE-FUNC
                              MOUSE-PARAM.
           IF MOUSE-FUNC NOT = 255
               MOVE "Y"          TO MOUSE-ATTACHED
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC
                                MOUSE-PARAM
           ELSE
               GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *            A S   F U N C T I O N   K E Y
      *
             MOVE 3              TO USER-ACTION.
             MOVE 27             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
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
      MOVE "GARTEE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-GARTEE.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
            OPEN I-O PARAM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 15           TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 3              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-CRDX  TO WS-CARDEX.
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN I-O NETWORK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 2            TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN I-O GARTEE.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 9        TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 9            TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
            GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
           IF WS-F-ERROR = 9
               MOVE "GUARANTEE file"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MID-LINE WS-MID-LNE.
       ZA60.
             MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
             MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
             MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
             MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
           IF WS-S1 < 80
              ADD 1   TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCR   (1).
             MOVE WS-G10         TO WS-TCR   (80).
             MOVE WS-G11         TO WS-BCR   (1).
             MOVE WS-G12         TO WS-BCR   (80).
             MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
                                    WS-TCHR  (1)  WS-BCHR  (1)
                                    WS-TCH   (1)  WS-BCH   (1).
             MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
                                    WS-TCHR  (80) WS-BCHR  (80)
                                    WS-TCH   (80) WS-BCH   (80).
             MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
                                    WS-TCHAR (50) WS-TCHAR (59)
                                    WS-TCHAR (69)
                                    WS-TCHR  (16) WS-TCHR  (27)
                                    WS-TCHR  (50) WS-TCHR  (71)
                                    WS-TCH   (8)  WS-TCH   (49)
                                    WS-TCH   (61) WS-TCH   (72).
             MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19)
                                    WS-MCHAR (50) WS-MCHAR (59)
                                    WS-MCHAR (69) WS-MCHAR (80)
                                    WS-MCHAR (1)
                                    WS-MCH   (8)  WS-MCH   (49)
                                    WS-MCH   (61) WS-MCH   (72)
              WS-MCHR  (20) WS-MCHR  (39)
              WS-MCHR  (60) WS-MCHR  (71)
                                    WS-MCHR  (1)  WS-MCHR  (80)
                                    WS-MCH   (1)  WS-MCH   (80).
             MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19)
                                    WS-BCHAR (50) WS-BCHAR (59)
                                    WS-BCHAR (69)
                                    WS-BCHR  (16) WS-BCHR  (27)
                                    WS-BCHR  (50) WS-BCHR  (71)
                                    WS-BCH   (8)  WS-BCH   (49)
                                    WS-BCH   (61) WS-BCH   (72).
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
       ZA999.
             EXIT.

       I-O-ERRORS      SECTION 9.
       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      

       DISPLAY-FILE-NAME.
    IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY.
           IF WS-STATUS = "10"
              MOVE "End of FILE"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
              MOVE "Duplicate record number"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
              MOVE "Invalid record number"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
              MOVE "DISK full"  TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
              MOVE "FILE locked"
     TO WS-STAT-MESSAGE.
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
