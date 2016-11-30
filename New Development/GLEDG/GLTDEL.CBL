      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *     ******   **       ********  *******   ********  **         *
      *    **    **  **          **     **    **  **        **         *
      *    **        **          **     **    **  **        **         *
      *    **        **          **     **    **  *****     **         *
      *    **   ***  **          **     **    **  **        **         *
      *    **    **  **          **     **    **  **        **         *
      *     ******   ********    **     *******   ********  ********   *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   G / L   D E L E T E   T R A N S A C T I O N / S              *
      *                                                                *
      *       Version 9.04.02 - October 2016                           *
      *                                                                *
      ******************************************************************
      * NB -- To do list:                                              *
      *       Include a print of the transaction record that is to be  *
      *       deleted.                                                 *
      *ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GLTDEL.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   SEPTEMBER 1992.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2016
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

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

      /
       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

      *
      *         **        **    *****    ******    **   **
      *         **        **   **   **   **   **   **  **
      *         **        **   **   **   **  **    ** **
      *         **   **   **   **   **   *****     ****
      *          ** **** **    **   **   **  **    ** **
      *           ***  ***     **   **   **   **   **  **
      *            *    *       *****    **   **   **   **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-S9    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-OPEN    PIC 9(02)  VALUE ZERO.
       77  WS-NETKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-ERROR    PIC 9(01)  VALUE ZERO.
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
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-PROG    PIC X(02).
       77  WS-INDG         PIC  9(01) VALUE ZERO.
       77  WS-OPTION    PIC X(01).
       77  WS-ER1    PIC X(07) VALUE "Account".
       77  WS-ER3          PIC  X(04) VALUE "Date".
       77  WS-ER4          PIC  X(05) VALUE "Value".
       77  WS-ER5          PIC  X(13) VALUE "Cheque Number".
       77  WS-ER6          PIC  X(09) VALUE "No Record".
       77  WS-ER7          PIC  X(13) VALUE "Group Heading".
       77  WS-DATE-CHK    PIC 9(08) COMP-5.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      /    *************************************************************
      *    ****     G / L   A C C O U N T   T E M P   S T O R E     ****
      *    *************************************************************
       01  WG-REC.
          03  WG-AC        PIC  9(06).
          03  WG-ACR        REDEFINES WG-AC.
              05  WG-GR       PIC  9(02).
              05  WG-SR       PIC  9(04).
              05  WG-SER      REDEFINES WG-SR.
           07  WG-SGR  PIC  9(02).
           07  WG-ASER PIC  9(02).
          03  WG-ACR2        REDEFINES WG-AC.
              05  WG-GRP      PIC  9(01).
               05  FILLER      PIC  9(05).
          03  WG-AC2        REDEFINES WG-AC.
              05  WG-ACNO     PIC  9(04).
               05  FILLER      PIC  9(02).
          03  WG-NAME.
        05  WG-NKEY     PIC  X(12).
        05  FILLER      PIC  X(18).
      *
      *    ****   This indicator informs the system of the type of
      *           budget on this account.
      *           0 = Manual figures entered.
      *           1 = Percentage of Total income.
      *           2 = Percentage of Group income.
      *           3 = Percentage of Group/Subgroup income.
      *
          03  WG-IND        PIC  9(01).
      *
      *    ****   This is the percentage for calculating the budget
      *    figures based on WG-IND.
      *
          03  WG-BPER        PIC S9(02)V99  COMP-3.
      *
      *    ****   Budget figures for 12 months (Jan to Dec).
      *
          03  WG-BUD.
              05  WG-BVAL     PIC S9(09)    COMP-3 OCCURS 12.
      *
      *    ****   Monthly Balance for each month of the current year.
      *
          03  WG-MOVE.
              05  WG-VAL      PIC S9(09)V99 COMP-3 OCCURS 12.
          03  WG-DT        PIC S9(09)V99 COMP-3.
          03  WG-CR        PIC S9(09)V99 COMP-3.
          03  WG-BAL        PIC S9(09)V99 COMP-3.
    03  WG-OBAL        PIC S9(09)V99 COMP-3.
      *
      *    ****   Monthly Balance for each month of the previous year.
      *
          03  WG-LSTY.
              05  WG-LVAL     PIC S9(09)V99 COMP-3 OCCURS 12.
      *
      *    ****   1 = Income,  2 = Costs,  3 = Expenses.
      *    4 = Capital, 5 = Assets, 6 = Liabilities.
      *
    03  WG-TYPE        PIC  X(01).
    03  WG-SEC        PIC  X(01).
      /    *************************************************************
      *    **** G / L T R A N S A C T I O N T E M P   S T O R E ****
      *    *************************************************************
       01  WT-REC.
    03  WT-KEY.
              05  WT-AC       PIC  9(06).
              05  WT-DATE     PIC  9(08).
              05  WT-DTE REDEFINES WT-DATE.
           07  WT-CC   PIC  9(02).
           07  WT-YY   PIC  9(02).
           07  WT-MM   PIC  9(02).
           07  WT-DD   PIC  9(02).
        05  WT-AKEY.
      *
      *     01 OR 02 - Journal Entries
      *     03 OR 04 - Cheques
      *     06 OR 07 - Deposits
      *     08 OR 09 - Electronic Funds
      *
           07  WT-CODE PIC  9(02).
     07  WT-REFER.
               09  WT-REF
                               PIC  9(07).
     07  WT-SEQ  PIC  9(02).
        05  WT-SUB      PIC  9(01).
          03  WT-NAR        PIC  X(40).
          03  WT-DESC REDEFINES WT-NAR.
              05  WT-NARATIVE PIC  X(36).
               05  FILLER      PIC  9(07)    COMP-3.
          03  WT-VAL        PIC S9(09)V99 COMP-3.
          03  WT-FLAG        PIC  X(01).
    03  WT-BATCH        PIC  9(06)    COMP-3.
    03  FILLER        PIC  X(03).

       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LNE.
               05  WS-MCHR PIC  X(01) OCCURS 80.
    03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
        05  WS-MGL1     PIC X(01).
        05  WS-MGL78    PIC X(78).
        05  WS-MGL80    PIC X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "≥".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "∞".
        05  FILLER      PIC  X(01) VALUE "≥".
          03  WS-BOT-LNE.
               05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH  PIC  X(01) OCCURS 80.

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "GLT".
    03  WS-PROG        PIC  X(03) VALUE "DEL".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "GLPTPT.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY "W05.GL".

       01  W10-DATE.
    03  W10-DTE        PIC 99/99/9999.

       COPY "W15.GL".

       01  W20-INPUT.
           03  W20-ACCOUNT PIC  9(06).
           03  W20-ACNO1   REDEFINES W20-ACCOUNT.
               05  W20-ACNO
                           PIC 9(04).
               05  W20-SER PIC  9(02).
           03  W20-ACNO2   REDEFINES W20-ACCOUNT.
               05  W20-GR  PIC  9(02).
               05  W20-SGRP
                           PIC 9(02).
               05  W20-ASER
                           PIC 9(02).
           03  W20-ACNO3   REDEFINES W20-ACCOUNT.
               05  W20-GROUP
                           PIC 9(02).
               05  W20-REST
                           PIC 9(04).

       COPY "W20.WS".

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).

       01  W25-EDIT.
           03  W25-7.
               05  W25-1   PIC  ZZZZZZZZ9-.
           03  W25-S7V2.
               05  W25-V1  PIC  Z(07)9.99CR.
          03  W25-S8V2.
              05  W25-VG1     PIC Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE PIC Z9/99/9999.
           03  W25-REF.
               05  W25-REFER
                           PIC Z(07).
           03  W25-RECKEY  PIC  Z(04)9.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-GNO       PIC 9(06) COMP-5
                              OCCURS 18.

       01  W90-COMP.
           03  W90-CNAME   PIC  X(40).

      *
      *    **       ******  **    **  **   **    ***     *****   ******
      *    **         **    ***   **  **  **    ** **   **   **  **
      *    **         **    ****  **  ** **    **   **  **       ** 
      *    **         **    ** ** **  ****     *******  **       *****
      *    **         **    **  ****  ** **    **   **  **  ***  **  
      *    **         **    **   ***  **  **   **   **  **   **  **
      *    *******  ******  **    **  **   **  **   **   *****   ******
      *
       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-GENL.IDS".

      * 
      *   *****    *****   ******   ******  ******  **    **
      *  **   **  **   **  **   **  **      **      ***   **
      *  **       **       **  **   **      **      ****  **
      *   *****   **       *****    *****   *****   ** ** **
      *       **  **       **  **   **      **      **  ****
      *  **   **  **   **  **   **  **      **      **   ***
      *   *****    *****   **   **  ******  ******  **    **
      *
       SCREEN SECTION.

       01  S00.
    03  LINE  6 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕª".
    03  LINE  7 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ W A R N I N G  -  W A R N I N G  -  W A R N I N G  -  W A
      -    "R N I N G ∫".
    03  LINE  8 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕπ".
    03  LINE  9 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ This program must be used with caution. The program will a
      -    "llow you  ∫".
    03  LINE 10 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ to delete any General Ledger transaction, without updating
      -    " the      ∫".
    03  LINE 11 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ account balance. The index will be updated, but you must r
      -    "un a re-  ∫".
    03  LINE 12 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ calculate balances after you exit this program, if you dele
      -    "ted any   ∫".
    03  LINE 13 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ transactions.
      -    "          ∫".
    03  LINE 14 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº".
      *
      * …ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
      * ∫ W A R N I N G  -  W A R N I N G  -  W A R N I N G  - W A R N I N G ∫
      * ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ
      * ∫ This program must be used with caution. The program will allow you  ∫
      * ∫ to delete any General Ledger transaction, without updating the      ∫
      * ∫ account balance. The index will be updated, but you must run a re-  ∫
      * ∫ calculate balances after you exit this program, if you deleted any  ∫
      * ∫ transactions.             ∫
      * »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
      *
       01  S09.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "DELETE TRANSACTION".
          03  LINE  4 COLUMN  4 VALUE "Account:".
          03        COLUMN 21 VALUE "Name:".
           03  LINE  6 COLUMN  5 VALUE "O/Balance".
           03          COLUMN 24 VALUE "Debits".
           03          COLUMN 39 VALUE "Credits".
           03          COLUMN 53 VALUE "C/Balance".
           03          COLUMN 69 VALUE "Budget".
           03  LINE  8 COLUMN 34 VALUE "Transactions".
          03  LINE  9 COLUMN  5 VALUE "Date".
          03        COLUMN 13 VALUE "Type".
           03          COLUMN 18 VALUE "Reference".
           03          COLUMN 29 VALUE "Narrative/Payee".
           03          COLUMN 72 VALUE "Value".

       01  S10.
    03  S10-ACNO.
        05  LINE  8 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC X(06) USING W15-ACCOUNT AUTO.
    03  S10-DATE.
        05  LINE  9 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC 99/99/9999 USING W15-DATE AUTO.
    03  S10-TYPE.
        05  LINE 10 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC 99 USING W15-TYPE AUTO.
    03  S10-REF.
        05  LINE 11 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
           PIC X(08) USING W15-REF AUTO.
    03  S10-NAR.
        05  LINE 12 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC X(36) USING W15-NAR AUTO.
    03  S10-VALUE.
        05  LINE 13 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC ZZZZZZZ9.99CR USING W15-VAL AUTO.

       01  S11.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "DELETE TRANSACTION".
          03  LINE  4 COLUMN 12 VALUE "Press ".
    03        COLUMN 18 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "C".
    03        COLUMN 19 VALUE "ontinue or ".
    03        COLUMN 30 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 33 VALUE "ape to Exit".

       01  S17.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC 9(06) USING W15-ACCOUNT AUTO.

       01  S18.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC 99/99/9999 USING W15-DATE AUTO.

       COPY "LEDLUP.CRT".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000        SECTION.
       AA00.
          IF LS0-GLLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000.
      PERFORM EA000.

      *
      *    ****    P R I N T I N G   T O   B E  I N C L U D E D
      *     L A T E R   -   U S E   P R I N T   U T I L I T Y
      *

      *      CLOSE PRNREP.

       AA49.
      EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥     SCREEN-SHADOW          ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ To routine is used to display a shadow down the right and ≥
      *    ≥ along the bottom of a pop-up box. The parameters that are ≥
      *    ≥ required:             ≥
      *    ≥        SHADE-ROW   - Top line of the box + 1.        ≥
      *    ≥        SHADE-COL   - Left line of box + 2.        ≥
      *    ≥        SHADE-WIDTH - Width of the box - 2.        ≥
      *    ≥        SHADE-LINES - Hight of box - 1.         ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥     CHECK-CORRECT          ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ This routine displays a pop-up window with the message    ≥
      *    ≥       "Correct ENTER - N if incorrect"         ≥
      *    ≥              ≥
      *    ≥ The response is returned in WS-OPTION (in upper case).    ≥
      *    ≥              ≥
      *    ≥ To start with the pop-up window higher or lower than row  ≥
      *    ≥ 13 (default); move another value to SLIN and PERFORM      ≥
      *    ≥ CHECK-SETUP THRU CHECK-EXIT.          ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥     ERROR-MESSAGE          ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ To display an error message with " - Press ANY key" at    ≥
      *    ≥ the end; use PERFORM ERROR-MESSAGE.         ≥
      *    ≥              ≥
      *    ≥ To display just the error message; use PERFORM        ≥
      *    ≥ ERROR-LENGTH THRU ERROR-EXIT.          ≥
      *    ≥              ≥
      *    ≥ To display the error message higher or lower (default is  ≥
      *    ≥ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ≥
      *    ≥ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ≥
      *    ≥ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ≥
      *    ≥ THRU ERROR-EXIT.            ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY "FUNCTION.CRT".

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥                        OPT-MESSAGE                        ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ This routine is used to allow the OPERATOR to respond to  ≥
      *    ≥ a request for an option, so that the correct action can   ≥
      *    ≥ be performed by the program. The routine will display the ≥
      *    ≥ message in a pop-up window and allow the OPERATOR to      ≥
      *    ≥ respond to the 'question'.                                ≥
      *    ≥                                                           ≥
      *    ≥ The option request must be placed in WS-OPT-MES and may   ≥
      *    ≥ not exceed 48 characters. The message will be centred in  ≥
      *    ≥ the window. An example of a message request follows:      ≥
      *    ≥                                                           ≥
      *    ≥   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ≥
      *    ≥   MOVE Instruction    TO WS-INSTR.                        ≥
      *    ≥       [see Accptopt for instruction values]               ≥
      *    ≥   PERFORM OPT-MESSAGE.                                    ≥
      *    ≥                                                           ≥
      *    ≥ This would be displayed as:                               ≥
      *    ≥      ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø   ≥
      *    ≥      ≥          Print transactions (Y/N) [ ]          ≥∞∞ ≥
      *    ≥      ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ∞∞ ≥
      *    ≥        ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞ ≥
      *    ≥                                                           ≥
      *    ≥ The response is returned in WS-OPTION (in upper case).    ≥
      *    ≥                                                           ≥
      *    ≥ The system will display the message box with the top line ≥
      *    ≥ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ≥
      *    ≥ default will be 20.                                       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥           ERASE SCREEN FROM SPECIFIED LOCATION            ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ Uses CRT-START as starting line and increases and CRT-END ≥
      *    ≥ as the ending line. The screen is cleared with Column 1   ≥
      *    ≥ and 80 containing "≥" and columns 2 to 79 containing      ≥
      *    ≥ spaces.                                                   ≥
      *    ≥                                                           ≥
      *    ≥  eg.                                                      ≥
      *    ≥   MOVE 8         TO CRT-START.                            ≥
      *    ≥   MOVE 28        TO CRT-END.                              ≥
      *    ≥   PERFORM ERASE-SCREEN.                                   ≥
      *    ≥                                                           ≥
      *    ≥ To clear the entire screen and Display a new screen with  ≥
      *    ≥ headings (program/date/time/company):                     ≥
      *    ≥                                                           ≥
      *    ≥    CRT-PROGRAM (calling program)                          ≥
      *    ≥    CRT-HEADING (Screen heading)                           ≥
      *    ≥    CRT-COMPANY (Company name)                             ≥
      *    ≥    CRT-TERMINAL (consists of two fields:                  ≥
      *    ≥                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ≥
      *    ≥    These fields should be populated at the start of the   ≥
      *    ≥    program.                                               ≥
      *    ≥    PERFORM HEADING-AND-CLEAR-SCREEN.                      ≥
      *    ≥                                                           ≥
      *    ≥ To change the screen heading:                             ≥
      *    ≥    MOVE "The new heading" TO CRT-HEADING.                 ≥
      *    ≥    PERFORM CHANGE-HEADING.                                ≥
      *    ≥                                                           ≥
      *    ≥ To change the time on the screen:                         ≥
      *    ≥    PERFORM CHANGE-TIME                                    ≥
      *    ≥                                                           ≥
      *    ≥ To clear line 50:                                         ≥
      *    ≥    PERFORM CLEAR-L50                                      ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY "CLEAR.CRT".

       AA350-LOOKUP    SECTION.
       AA350.
            PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM GACCNT-LOOKUP.
            PERFORM RESTORE-SCREEN.
       AA399.
             EXIT.

      /    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥        R E A D  F I L E S   R O U T I N E S        ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
       AB000        SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "GLTRAN.RD".

      /    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥R E W R I T E   & W R I T E   F I L E S R O U T I N E S≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
       AC000        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

       COPY "DATE.CHK".

      /
       EA000      SECTION 50.
       EA00.
             PERFORM ERASE-SCREEN.
      MOVE 7   TO SHADE-ROW.
      MOVE 7   TO SHADE-COL.
      MOVE 69   TO SHADE-WIDTH.
      MOVE 8   TO SHADE-LINES.
      DISPLAY S00.
      PERFORM SCREEN-SHADOW.
      DISPLAY "Press " AT 1715
       "Esc" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "ape key to exit or "
       "any OTHER " WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "key to continue".

       EA02.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO EA999
   WHEN OTHER
       CALL X"E5"
       GO TO EA02
        END-EVALUATE.

       EA05.
            PERFORM ERASE-SCREEN.
            DISPLAY S09.
            MOVE ZERO   TO W15-ACCOUNT.

       EA10.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
            ACCEPT S17 AT 0413.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO EA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       CALL X"E5"
       GO TO EA05
        END-EVALUATE
        DISPLAY S17 AT 0413
               IF W15-ACCOUNT = ZERO
           GO TO EA05.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO EA999.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO EA05.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥     Move G/L account details to temporary store.       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      MOVE G-REC   TO WG-REC.
      MOVE W15-ACCOUNT  TO W15-ACNT.
            DISPLAY WG-NAME AT 0426
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       EA15.
            MOVE W05-MM  TO WS-S1.
            MOVE WG-VAL (WS-S1) TO W25-V1.
            DISPLAY W25-S7V2 AT 0704
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE WG-DT   TO W25-V1.
            DISPLAY W25-S7V2 AT 0720
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE WG-CR   TO W25-V1.
            DISPLAY W25-S7V2 AT 0736
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE WG-BAL  TO W25-V1.
            DISPLAY W25-S7V2 AT 0752
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE WG-BVAL(WS-S1) TO W25-1.
            DISPLAY W25-7 AT 0768
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             INITIALIZE T-REC.
      MOVE WG-AC   TO T-AC.
      ACCEPT S18 AT 1002.
      MOVE W15-CC  TO W15-CN.
      MOVE W15-YY  TO W15-YR.
      MOVE W15-MM  TO W15-MT.
      MOVE W15-DD  TO W15-DY.
            MOVE W15-YMD  TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
        GO TO EA190.

       EA20.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
           IF (WS-F-ERROR = 11) OR
       (T-AC NOT = WG-AC)
        GO TO EA05.

       EA20A.
            MOVE T-DATE  TO W15-YMD W15-SYMD.
      MOVE W15-CN  TO W15-CC.
      MOVE W15-YR  TO W15-YY.
      MOVE W15-MT  TO W15-MM.
      MOVE W15-DY  TO W15-DD.
      MOVE W15-DATE  TO W25-DTE WS-DATE-CHK.
            DISPLAY W25-DATE AT 1002
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
          IF T-CODE = 01 OR 02
              DISPLAY "Jnl" AT 1013
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
           ELSE
           IF T-CODE = 03 OR 04
              DISPLAY "Chq" AT 1013
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
          ELSE
          IF T-CODE = 05 OR 10
              DISPLAY "Bnk" AT 1013
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
          ELSE
           IF T-CODE = 06 OR 07
              DISPLAY "Dep" AT 1013
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
           ELSE
          IF T-CODE = 08 OR 09
              DISPLAY "Eft" AT 1013
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
          ELSE
          IF T-CODE = 11 OR 12
              DISPLAY "Pmt" AT 1013
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
          ELSE
          IF T-CODE = 13 OR 14
              DISPLAY "Rec" AT 1013
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
          ELSE
               GO TO EA25.
    IF T-REFER NUMERIC
              MOVE T-REF  TO W25-REFER
    ELSE
        MOVE T-REFER  TO W25-REF.
            DISPLAY W25-REF AT 1019
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       EA25.
            DISPLAY T-NARATIVE AT 1029
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
          IF T-SUB NOT = 5
               MOVE T-VAL TO W25-V1
              DISPLAY W25-S7V2 AT 1067
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.

       EA30.
            MOVE "'D'elete,'P'revious,'N'ext or 'Esc'ape to exit" TO WS-OPT-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.

       EA35.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO EA05
   WHEN OTHER
       CALL X"E5"
       GO TO EA35
        END-EVALUATE.
    IF NOT(WS-OPTION = "D" OR "N" OR "P")
        CALL X"E5"
        GO TO EA30.
    IF WS-OPTION = "N"
        GO TO EA20.
    IF WS-OPTION = "P"
        GO TO EA180.

       EA40.
      MOVE T-AC   TO W15-ACCOUNT.
      MOVE T-CODE  TO W15-TYPE.
      MOVE T-REFER  TO W15-REF.
      MOVE T-NARATIVE  TO W15-NAR.
      MOVE T-VAL   TO W15-VAL.
      PERFORM SAVE-SCREEN-3.
      *    *************************************************************
      *    ****       DISPLAY THE TRANSACTION DELETE SCREEN     ****
      *    *************************************************************
      *  ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *  ≥ Ac No 999999         ≥
      *  ≥ Date 99/99/9999        ≥
      *  ≥ Code 99         ≥
      *  ≥ Ref XXXXXXXX        ≥
      *  ≥ Nar XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ≥
      *  ≥ Value ZZZZZZZ9.99CR        ≥
      *  ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      *
      DISPLAY "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
        AT 0717 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "ø" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Ac No                                      "
        AT 0817 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Date                                       "
        AT 0917 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Code                                       "
        AT 1017 WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Ref                                        "
        AT 1117 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Nar                                        "
        AT 1217 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Value                                      "
        AT 1317 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "¿" AT 1417 WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey
       "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      MOVE 8   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 44   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      DISPLAY "WARNING - Deleting a transaction requires that the other side of the entry " AT 1804
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red.
      DISPLAY "must also be deleted. Be sure that you delete both sides of the entry to   " AT 1904
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red.
      DISPLAY "ensure that the books remain in balance.                                   " AT 2004
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red.
      DISPLAY S10.

       EA45.
      MOVE "Delete transaction Y/N  [ ]" TO WS-OPT-MES.
      MOVE 17   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT(WS-OPTION = "N" OR "Y")
        GO TO EA45.
      PERFORM RESTORE-SCREEN-3.
    IF WS-OPTION = "N"
        GO TO EA20.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Store the transaction record before deleting, as a restart≥
      *    ≥ pointer after deleting the transaction.         ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      MOVE T-REC   TO WT-REC.
      MOVE 1   TO WS-INDG.
      PERFORM DELETE-GLTRAN-REC THRU WRITE-GLTRAN-EXIT.
      MOVE WT-REC  TO T-REC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
      GO TO EA20.

       EA180.
             PERFORM READ-GLTRAN-PREV THRU READ-GLTRAN-EXIT.
    IF (WS-F-ERROR = 11) OR
       (T-AC NOT = WG-AC)
        GO TO EA05.
            PERFORM CLEAR-L50.
             GO TO EA20A.

       EA190.
      MOVE "No transactions on file" TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO EA05.

       EA999.
             EXIT.


       ZA000      SECTION 90.
       ZA00.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥  Setup various keys and the mouse for use with menus and  ≥
      *    ≥  other required user interface functions and procedures.  ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

      COPY "FUNCTION.PRO".

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
        MOVE ZERO  TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
        MOVE 1   TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
        MOVE 2   TO WS-PRN-NO
    ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
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
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
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
       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W90-CNAME.
            MOVE PAR-DMY  TO W05-TODAY.
            MOVE PAR-YMD  TO W05-TYMD.
      *
      *    ****    Printing of an audit trail will be included later.
      *     U S E   P R I N T   U T I L I T Y
      *

            GO TO ZA999.

       COPY "ZA49.PRO".

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 91.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
          ELSE
           IF WS-F-ERROR = 8
              MOVE W02-GACCNT  TO WS-FILE
              MOVE ZERO  TO WS-KEY
        MOVE G-AC  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 11
               MOVE W02-GLTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
              MOVE W02-PARAM  TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY.
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
