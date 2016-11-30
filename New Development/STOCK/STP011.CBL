      $set LINKCOUNT"468" GNT"STP011.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****       **        **      *
      *   **    **     **     **    **   **  **     ***       ***      *
      *   **           **     **    **  **    **     **        **      *
      *    ******      **     *******   **    **     **        **      *
      *         **     **     **        **    **     **        **      *
      *   **    **     **     **         **  **      **        **      *
      *    ******      **     **          ****    ********  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        S T O C K   P R I C E   C H A N G E S                   *
      *                                                                *
      *   Version 9.00.70 - July 2015                                  *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STP011.
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

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY STKMEM.SL.

       COPY "STOCK.SL".

       DATA DIVISION.
      *
      *          ********  ********  **        ******** 
      *          **           **     **        **
      *          **           **     **        ** 
      *          *****        **     **        *****
      *          **           **     **        **  
      *          **           **     **        **
      *          **        ********  ********  ********
      *
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "RECOVER.FD".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY STKMEM.FDE.

       COPY "STOCK.FDE".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-DISC                 PIC  999V99   COMP-3.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-P1                   PIC  9(02)    COMP-5.
       77  WS-TRFKEY               PIC  9(06)    COMP-5.
       77  WS-PRCKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-DEPKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-RECORDS              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-GST-RATE             PIC S9(03)V99 COMP-3.
       77  WS-MONTHS               PIC S9(01)V99 COMP-3.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-ITM                  PIC  X(18).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-ETYPE                PIC  X(01).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-NORM                 PIC  X(04).
       77  WS-COND                 PIC  X(04).
       77  WS-EXPP                 PIC  X(04).
       77  WS-ECAN                 PIC  X(04).
       77  WS-10CPI                PIC  X(04).
       77  WS-12CPI                PIC  X(04).
       77  WS-17CPI                PIC  X(04).
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-IND                  PIC  9(01)    VALUE 0.
       77  WS-INDB                 PIC  9(01)    VALUE 0.
       77  WS-INDP                 PIC  9(01)    VALUE 0.
       77  WS-INDS                 PIC  9(01)    VALUE 0.
       77  WS-AMEND                PIC  9(01)    VALUE 0.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-LEDG                 PIC  X(04).
       77  WS-LEDG1                PIC  X(04).
       77  WS-COMP                 PIC  9(01).
       77  WS-WEEK                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-BATCH                PIC  9(05).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PPASSWORD            PIC  X(06).
       77  WS-PRINT                PIC  9(01).
       77  WS-LABCDE               PIC  X(08).
       77  WS-PASS                 PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)    VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)    VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)    VALUE "-Cash sale".
       77  WS-NODEP                PIC  X(30)    VALUE "NO SUCH - D E P A R T M E N T".
       77  PRG-NAME                PIC  X(12)    VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      /
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP011".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY CHANGE PRICES".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "STP".
           03  WS-PROG             PIC  X(03) VALUE "011".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.VAT".

       COPY "W05.RND".

       COPY "W10.STK".

       COPY "W11.STK".

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables.CPY' now includes the
      *    'W12.WS'and 'W20.WS' variables and is passed to DateCheck
      *    in the CALL statement.
      *
       COPY "DateVariables.cpy".

       COPY "W15.STK".

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-SCOST           PIC S9(09)V99 COMP-3.
           03  W25-PCOST           PIC S9(09)V99 COMP-3.
           03  W25-PCST            PIC S9(09)V99 COMP-3.
           03  W25-PTOT            PIC S9(09)V99 COMP-3.
           03  W25-TSELL           PIC S9(09)V99 COMP-3.
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.
           03  W25-ADJUST          PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

       01  W80-EDIT.
           03  W80-CNO             PIC 9(04).
           03  W80-V1.
               05  W80-S7V2        PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2        PIC Z(04)9.99-.
           03  W80-DTE.
               05  W80-DATE        PIC Z9/99/9999.
           03  W80-QNT.
               05  W80-5           PIC Z(04)9.99.
           03  W80-QNT1                          REDEFINES W80-QNT.
               05  W80-3V2         PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N          PIC 9(05).
               05  W80-REC                       REDEFINES W80-5N
                                   PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N          PIC 9(02).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W90-BAL                 PIC S9(07)V99 COMP-3.

       01  W95-COMP                PIC X(40).

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2       PIC Z(05)9.99-.
           03  W100-V3                           REDEFINES W100-V2.
               05  W100-PRICE      PIC X(09).
               05  FILLER          PIC X(01).
           03  W100-V5                           REDEFINES W100-V2.
               05  W100-QNT        PIC Z(05)9.99-.

       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-MEM1       PIC  X(60).
               05  W200-MEM2       PIC  X(60).
               05  W200-MEM3       PIC  X(60).
               05  W200-MEM4       PIC  X(60).
               05  W200-MEM5       PIC  X(60).
               05  W200-MEM6       PIC  X(60).
               05  W200-MEM7       PIC  X(60).
               05  W200-MEM8       PIC  X(60).
               05  W200-MEM9       PIC  X(60).
               05  W200-MEM10      PIC  X(60).
               05  W200-MEM11      PIC  X(60).
               05  W200-MEM12      PIC  X(60).
           03  W200-MEM-FOC                      REDEFINES W200-MEM-FLDS.
               05  W200-MEM        PIC  X(60)    OCCURS 12.

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

       COPY "FILE.IDS".

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

       01  S05.
           03  BACKGROUND-COLOR Grey.
               05  LINE  7 COLUMN 11 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Black.
               05          COLUMN 70 VALUE                                                            "¿" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  8 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "          P A C K S :   S A L E S   D E T A I L S         "  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  9 COLUMN 11 VALUE "Ã"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Pack 1ÃÄÄÄÄÄ´Pack 2ÃÄÄÄÄÄ´Pack 3ÃÄÄÄ"  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "´" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 10 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Priced per        "                                         FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 11 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Units per pack    "                                         FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Markup "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-RT-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 13 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-CS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-WS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 15 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Price  "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-RT-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 16 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-CS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 17 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-WS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 18 COLUMN 11 VALUE "À"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S06.
      *
      *    ****   Set the colours to be used for these screens 
      * 
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S06-P1.
                   07  LINE 10 COLUMN 31 PIC X(08)        FROM W10-PER-2.
                   07  LINE 11 COLUMN 31 PIC ZZZ9.999     FROM W10-UNIT-2.
                   07  LINE 12 COLUMN 31 PIC ZZ9.9999     USING W10-MARKUP-2 AUTO.
                   07  LINE 13 COLUMN 31 PIC ZZ9.9999     USING W10-CMARKUP-2 AUTO.
                   07  LINE 14 COLUMN 31 PIC ZZ9.9999     USING W10-WMARKUP-2 AUTO.
                   07  LINE 15 COLUMN 31 PIC ZZZZZZZ9.999 USING W10-SELL-2 AUTO.
                   07  LINE 16 COLUMN 31 PIC ZZZZZZZ9.999 USING W10-CASH-2 AUTO.
                   07  LINE 17 COLUMN 31 PIC ZZZZZZZ9.999 USING W10-WSALE-2 AUTO.
               05  S06-P2.
                   07  LINE 10 COLUMN 44 PIC X(08)        FROM W10-PER-3.
                   07  LINE 11 COLUMN 44 PIC ZZZ9.999     FROM W10-UNIT-3.
                   07  LINE 12 COLUMN 44 PIC ZZ9.9999     USING W10-MARKUP-3 AUTO.
                   07  LINE 13 COLUMN 44 PIC ZZ9.9999     USING W10-CMARKUP-3 AUTO.
                   07  LINE 14 COLUMN 44 PIC ZZ9.9999     USING W10-WMARKUP-3 AUTO.
                   07  LINE 15 COLUMN 44 PIC ZZZZZZZ9.999 USING W10-SELL-3 AUTO.
                   07  LINE 16 COLUMN 44 PIC ZZZZZZZ9.999 USING W10-CASH-3 AUTO.
                   07  LINE 17 COLUMN 44 PIC ZZZZZZZ9.999 USING W10-WSALE-3 AUTO.
               05  S06-P3.
                   07  LINE 10 COLUMN 57 PIC X(08)        FROM W10-PER-4.
                   07  LINE 11 COLUMN 57 PIC ZZZ9.999     FROM W10-UNIT-4.
                   07  LINE 12 COLUMN 57 PIC ZZ9.9999     USING W10-MARKUP-4 AUTO.
                   07  LINE 13 COLUMN 57 PIC ZZ9.9999     USING W10-CMARKUP-4 AUTO.
                   07  LINE 14 COLUMN 57 PIC ZZ9.9999     USING W10-WMARKUP-4 AUTO.
                   07  LINE 15 COLUMN 57 PIC ZZZZZZZ9.999 USING W10-SELL-4 AUTO.
                   07  LINE 16 COLUMN 57 PIC ZZZZZZZ9.999 USING W10-CASH-4 AUTO.
                   07  LINE 17 COLUMN 57 PIC ZZZZZZZ9.999 USING W10-WSALE-4 AUTO.

       01  S07-ITEM-CODE.
      *
      *    ****   Set the colours to be used for these screens 
      * 
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S07.
                   07  PIC X(14) USING W10-ITEM AUTO.
               
               05  S07-ITM.
                   07  LINE  9 COLUMN 20 PIC X(14) USING W10-ITEM AUTO.
                   07          COLUMN 35 PIC X(03) USING W10-ITM AUTO.
               
               05  S07-EXT.
                   07  PIC X(18) USING W10-EXT-ITEM AUTO.
               
               05  S07-3.
                   07  PIC Z(03) USING W10-I4 AUTO.
               
               05  S07-4.
                   07  PIC Z(04) USING W10-I4 AUTO.
               
               05  S07-5.
                   07  PIC Z(05) USING W10-I5 AUTO.
               
               05  S07-6.
                   07  PIC Z(06) USING W10-I6 AUTO.
               
               05  S07-7.
                   07  PIC Z(07) USING W10-I7 AUTO.
               
               05  S07-8.
                   07  PIC Z(08) USING W10-I8 AUTO.
               
               05  S07-9.
                   07  PIC Z(09) USING W10-I9 AUTO.
               
               05  S07-10.
                   07  PIC Z(10) USING W10-I10 AUTO.
               
               05  S07-11.
                   07  PIC Z(11) USING W10-I11 AUTO.
               
               05  S07-12.
                   07  PIC Z(12) USING W10-I12 AUTO.
               
               05  S07-13.
                   07  PIC Z(13) USING W10-I13 AUTO.
               
               05  S07-14.
                   07  PIC Z(14) USING W10-I14 AUTO.

       01  S20.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 20 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE 12 COLUMN 20 VALUE "³ Enter price change - Password        ³".
               05  LINE 13 COLUMN 20 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 52 PIC  X(06) USING WS-PPASSWORD AUTO SECURE.

      *
      *    ****    PRICE CHANGE DISPLAY SCREEN
      *
       01  S25.
           03  S25A.
               05  LINE  2 COLUMN 31 VALUE                              "STOCK PRICE CHANGES"                             FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  4 COLUMN  2 VALUE "This option allows you to change the prices on a range of items starting from".
               05  LINE  5 COLUMN  2 VALUE "the  item  code indexed and ending at your request or after the last code  in".
               05  LINE  6 COLUMN  2 VALUE "the file has been read.".
               05  LINE  7 COLUMN  2 VALUE "The system requires that at least one character is keyed in.".
               05  LINE  9 COLUMN  3 VALUE "Start Item Code".
               05  LINE 11 COLUMN  3 VALUE "Item code".
               05          COLUMN 22 VALUE "Description".
           03  S25B.
               05  LINE 13 COLUMN 53 VALUE "Cost Price  ".
               05  LINE 14 COLUMN 53 VALUE "W/sale Mrkup".
               05  LINE 15 COLUMN 53 VALUE "W/sale Price".
               05  LINE 16 COLUMN 53 VALUE "Cash Markup ".
               05  LINE 17 COLUMN 53 VALUE "Cash Price  ".
               05  LINE 18 COLUMN 53 VALUE "Ret. Markup ".
               05  LINE 19 COLUMN 53 VALUE "Retail Price".
      *
      *    ****    PRICE CHANGE INPUT SCREEN
      *
       01  S26.
           03  S26A.
               05  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
                   07  LINE 13 COLUMN  3 PIC X(14) FROM W10-EXT-ITEM.
                   07          COLUMN 22 PIC X(30) FROM W10-DESC.

           03  S26B.
               05  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
                   07  LINE 13 COLUMN 66 PIC Z(07)9.999 USING W10-COST AUTO.
                   07  LINE 14 COLUMN 71 PIC ZZ9.9999   USING W10-WMARKUP AUTO.
                   07  LINE 15 COLUMN 66 PIC Z(07)9.999 USING W10-WSALE AUTO.
                   07  LINE 16 COLUMN 71 PIC ZZ9.9999   USING W10-CMARKUP AUTO.
                   07  LINE 17 COLUMN 66 PIC Z(07)9.999 USING W10-CASH AUTO.
                   07  LINE 18 COLUMN 71 PIC ZZ9.9999   USING W10-MARKUP AUTO.
                   07  LINE 19 COLUMN 66 PIC Z(07)9.999 USING W10-SELL AUTO.

       01  S26-DESC2.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 14 COLUMN 22 PIC X(30) FROM W10-DESC2.

       01  S26-ITM.
           03  BACKGROUND-COLOR Magenta.
               05  LINE 13 COLUMN  3 PIC X(14) FROM W10-ITEM FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 17 VALUE   "/"             FOREGROUND-COLOR Black.
               05          COLUMN 18 PIC X(03) FROM W10-ITM  FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S26-EXT.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 13 COLUMN  3 PIC X(18) FROM W10-EXT-ITEM.

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *         Check the security level of the USER.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
           IF LS0-STLEV < 3
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
           IF LS-OK = "N"
               PERFORM AA500
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BL000.
             CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50.LUP".

      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
             PERFORM SAVE-SCREEN.
             MOVE 7                 TO SHADE-ROW.
             MOVE 26                TO SHADE-COL.
             MOVE 29                TO SHADE-WIDTH.
             MOVE 4                 TO SHADE-LINES.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0726 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
                                                 "¿"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
             DISPLAY "³"                            AT 0826 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
                      " Insufficient security     "         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
                                                 "³"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
             DISPLAY "³"                            AT 0926 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
                      " Press ENTER to continue   "         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
                                                 "³"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
             DISPLAY "À"                            AT 1026 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
                      "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
             PERFORM SCREEN-SHADOW.
             ACCEPT WS-OPTION AT 0752 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey AUTO.
             PERFORM RESTORE-SCREEN.
       
       AA505.
             EXIT.

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box.               ³
      *    ³          SHADE-COL   - Left line of box.                  ³
      *    ³          SHADE-WIDTH - Width of the box.                  ³
      *    ³          SHADE-LINES - Height of box.                     ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 20 (default); move another value to WS-MES-LINE.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

       COPY "AA300.STK".

       COPY "DATE.CHK".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STKMEM.WR".

       COPY "STOCK.WR".

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
      MOVE WS-ACTION  TO REC-TYPE.
             MOVE STK-RECORD1    TO REC-STOCKF.
             GO TO AY50.
       AY37.
            MOVE 37   TO REC-FILE.
            MOVE WS-SHARED  TO REC-KEY.
            MOVE SHR-RECORD  TO REC-NETWORK.
             GO TO AY50.
       AY38.
             MOVE 38             TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
      MOVE DPT-RECORD  TO REC-DEPART.
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
    IF NOT(PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT(PAR-PROG(WS-SUB) = SPACES)
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
    IF NOT(PAR-USR(WS-SUB) = LS-USER)
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
    IF NOT(PAR-USR(WS-USUB) = LS-USER)
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
    IF NOT(PAR-PROG(WS-SUB) = SPACES)
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

      /
      *    ****  P R I C E   C H A N G E S
      *
       BL000        SECTION 50.
       BL0.
             PERFORM ERASE-SCREEN.
            DISPLAY S25.
      PERFORM SAVE-SCREEN-2.
            DISPLAY S20.
      MOVE 12   TO SHADE-ROW.
      MOVE 22   TO SHADE-COL.
      MOVE 38   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
            ACCEPT S20A.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-PPASSWORD
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
            MOVE 5   TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-PW  TO W85-PASS.
      PERFORM RESTORE-SCREEN-2.
          IF NOT(WS-PPASSWORD = W85-SUPER (1) OR
            W85-SUPER (9) OR
           "CECIRP")
              MOVE "Invalid password"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BL999.

       BL00.
             PERFORM ERASE-SCREEN.
             DISPLAY S25.
             MOVE SPACES             TO W10-EXT-ITEM.
           IF W10-STCK = "N"
               EVALUATE W10-SLNGTH
                 WHEN 3   MOVE ZERO  TO W10-I3            
                 WHEN 4   MOVE ZERO  TO W10-I4            
                 WHEN 5   MOVE ZERO  TO W10-I5            
                 WHEN 6   MOVE ZERO  TO W10-I6            
                 WHEN 7   MOVE ZERO  TO W10-I7            
                 WHEN 8   MOVE ZERO  TO W10-I8            
                 WHEN 9   MOVE ZERO  TO W10-I9            
                 WHEN 10  MOVE ZERO  TO W10-I10            
                 WHEN 11  MOVE ZERO  TO W10-I11            
                 WHEN 12  MOVE ZERO  TO W10-I12            
                 WHEN 13  MOVE ZERO  TO W10-I13            
                 WHEN 14  MOVE ZERO  TO W10-I14
               END-EVALUATE.
       BL05.
             PERFORM AA300.
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S07-EXT AT 0920
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM
        ELSE
           ACCEPT S07 AT 0920
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT 0920
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT 0920
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT 0920
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT 0920
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT 0920
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT 0920
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT 0920
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT 0920
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT 0920
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT 0920
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT 0920
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT 0920.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BL999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BL05
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
       GO TO BL05
        END-EVALUATE
        PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S07-EXT AT 0920
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM
     ELSE
         DISPLAY S07 AT 0920
     END-IF
        ELSE
     DISPLAY S07 AT 0920
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO BL05.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BL999.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BL05.
             PERFORM CA00 THRU CA06.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
        MOVE "No record"  TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO BL05.
       BL10.
            PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
        GO TO BL45.
    IF STK-IND = 4
        GO TO BL10.
            MOVE STK-CODE  TO W10-EXT-ITEM.
             MOVE STK-DESC       TO W10-DESC.
            MOVE STK-DESC2  TO W10-DESC2.
             MOVE STK-COST       TO W10-COST.
             MOVE STK-SELL       TO W10-SELL.
             MOVE STK-MARKUP     TO W10-MARKUP.
             MOVE STK-CMARKUP    TO W10-CMARKUP.
             MOVE STK-WMARKUP    TO W10-WMARKUP.
             MOVE STK-CASH       TO W10-CASH.
             MOVE STK-WSALE      TO W10-WSALE.
             DISPLAY S26.
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S26-EXT
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S26-ITM.
    IF NOT(W10-DESC2 = SPACES)
        DISPLAY S26-DESC2.

       BL15.
             ACCEPT S26B.
      MOVE 7   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
               GO TO BL15.
            PERFORM AY60 THRU AY999.
      MOVE 0   TO WS-ACTION.
             PERFORM AY05 THRU AY59.
      MOVE STK-LDG  TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        MOVE "Invalid Sales Ledger Code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BL44.
           IF W10-MARKUP = ZERO
               PERFORM BL50.

       BL40.
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
             MOVE W10-MARKUP     TO STK-MARKUP.
           IF W10-CMARKUP = ZERO
               PERFORM BL52.
           IF (W10-CMARKUP = ZERO) AND
              (W10-CASH = ZERO)
               MOVE W10-MARKUP   TO W10-CMARKUP
               MOVE W10-SELL     TO W10-CASH
               GO TO BL42.
           IF W10-CMARKUP = ZERO
               MOVE W10-CASH     TO W10-CSELL
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST)
                                      / W10-CCOST) * 100) + .005.
           IF W10-CASH = ZERO
               COMPUTE W10-CSELL = (W10-CCOST + ((W10-CCOST *
                                    W10-CMARKUP) / 100)) + .005
               MOVE W10-CSELL    TO W10-CASH.

       BL42.
             MOVE W10-CMARKUP    TO STK-CMARKUP.
           IF W10-WMARKUP = ZERO
               PERFORM BL54.
           IF (W10-WMARKUP = ZERO) AND
              (W10-WSALE = ZERO)
               MOVE W10-CMARKUP  TO W10-WMARKUP
               MOVE W10-CASH     TO W10-WSALE
               GO TO BL43.
           IF W10-WMARKUP = ZERO
               MOVE W10-WSALE    TO W10-CSELL
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST)
                                      / W10-CCOST) * 100) + .005.
           IF W10-WSALE = ZERO
               COMPUTE W10-CSELL = (W10-CCOST + ((W10-CCOST *
                                    W10-WMARKUP) / 100)) + .005
               MOVE W10-CSELL    TO W10-WSALE.

       BL43.
             MOVE W10-WMARKUP    TO STK-WMARKUP.
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
             MOVE W10-SELL       TO STK-SELL.
             MOVE W10-CASH       TO STK-CASH.
             MOVE W10-WSALE      TO STK-WSALE.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             PERFORM AY70 THRU AY999.
      *
      *    ****    P A C K   P R I C E S
      *
    IF STK-USE-PACKS = "Y"
        PERFORM BM000
      *
      *    ****   R E - S T A R T   R E C O V E R Y   L O G I N G
      *
              PERFORM AY60 THRU AY999
        MOVE 0   TO WS-ACTION
              PERFORM AY05 THRU AY59
        PERFORM BL60 THRU BL70
        PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT
        PERFORM AY70 THRU AY999.

       BL44.
            MOVE "ENTER to continue - 'E' to end  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      MOVE 1301   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
           IF WS-OPTION = "E"
               GO TO BL00.
             DISPLAY S25B.
             GO TO BL10.

       BL45.
            MOVE "End of range - ENTER for next range - 'E' to end"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF WS-OPTION = "E"
               GO TO BL999.
             GO TO BL00.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       BL50.
           IF W10-SELL > ZERO AND W10-COST > ZERO
               MOVE W10-SELL     TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-MARKUP = (((W10-CSELL - W10-CCOST)
                                     / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-MARKUP   TO W10-MARKUP.
       BL51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BL52.
           IF W10-CASH > ZERO AND W10-COST > ZERO
               MOVE W10-CASH     TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST)
                                     / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-CMRKUP   TO W10-CMARKUP.
       BL53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BL54.
           IF W10-WSALE > ZERO AND W10-COST > ZERO
               MOVE W10-WSALE    TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST)
                                     / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-CMRKUP   TO W10-WMARKUP.
       BL55.
            EXIT.

       BL60.
      MOVE 2   TO WS-S1.
      *
      *    ****   P A C K   D E T A I L S
      *
       BL65.
      MOVE W10-MKUP(WS-S1)
     TO STK-MKUP(WS-S1).
      MOVE W10-CMKUP(WS-S1)
     TO STK-CMKUP(WS-S1).
      MOVE W10-WMKUP(WS-S1)
     TO STK-WMKUP(WS-S1).
      MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
      MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
      MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BL65.

       BL70.
            EXIT.

       BL999.
            EXIT.
      /
       BM000        SECTION 50.
       BM00.
      PERFORM SAVE-SCREEN-3.
      *
      *    ****    U N I T S   A N D   M A R K U P S
      *
      MOVE 1   TO WS-S1.

       BM05.
      MOVE STK-UNT(WS-S1) TO W10-UNT(WS-S1).
      MOVE STK-PR(WS-S1)  TO W10-PR(WS-S1).
      MOVE STK-MKUP(WS-S1)
     TO W10-MKUP(WS-S1).
      MOVE STK-CMKUP(WS-S1)
     TO W10-CMKUP(WS-S1).
      MOVE STK-WMKUP(WS-S1)
     TO W10-WMKUP(WS-S1).
      MOVE STK-SL(WS-S1)  TO W10-SL(WS-S1).
      MOVE STK-CSH(WS-S1) TO W10-CSH(WS-S1).
      MOVE STK-WSL(WS-S1) TO W10-WSL(WS-S1).
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BM05.
      MOVE 8   TO SHADE-ROW.
      MOVE 13   TO SHADE-COL.
      MOVE 58   TO SHADE-WIDTH.
      MOVE 11   TO SHADE-LINES.
      DISPLAY S05.
      PERFORM SCREEN-SHADOW.
      DISPLAY "Esc" AT 2512
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "ape to cancel"
        WITH FOREGROUND-COLOR Blue HIGHLIGHT BACKGROUND-COLOR Cyan.
      DISPLAY S06.

       BM10.
      ACCEPT S06.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BM990
   WHEN OTHER
                     CALL X"E5"
       GO TO BM10
        END-EVALUATE.

       BM15.
      PERFORM CLEAR-L50.
      MOVE 20   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
    IF WS-OPTION = "N"
        GO TO BM25.
      MOVE 2   TO WS-P1.

       BM20.
    IF W10-UNT(WS-P1) = ZERO
        GO TO BM22.
            MULTIPLY W10-UNT(WS-P1) BY W10-COST
     GIVING W10-CCOST.
          IF W10-MKUP(WS-P1) NOT > ZERO
              PERFORM BM50.
            MOVE W10-SL(WS-P1)  TO W10-CSELL.
          IF W10-SL(WS-P1) = ZERO
              IF (W10-CCOST NOT = ZERO) AND
    (W10-MKUP(WS-P1) NOT = ZERO)
           COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
            ((W10-CCOST * W10-MKUP(WS-P1)) / 100)).
    IF W10-CSELL = ZERO
        MOVE W10-CCOST  TO W10-CSELL.
            MOVE W10-CSELL  TO W10-SL(WS-P1).
          IF W10-CMKUP(WS-P1) NOT > ZERO
              PERFORM BM52.
            MOVE W10-CSH(WS-P1) TO W10-CSELL.
          IF W10-CSH(WS-P1) = ZERO
              IF (W10-CCOST NOT = ZERO) AND
    (W10-CMKUP(WS-P1) NOT = ZERO)
           COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
            ((W10-CCOST * W10-CMKUP(WS-P1)) / 100)).
    IF W10-CSELL = ZERO
        MOVE W10-CCOST  TO W10-CSELL.
            MOVE W10-CSELL  TO W10-CSH(WS-P1).
          IF W10-WMKUP(WS-P1) NOT > ZERO
              PERFORM BM54.
            MOVE W10-WSL(WS-P1) TO W10-CSELL.
          IF W10-WSL(WS-P1) = ZERO
              IF (W10-CCOST NOT = ZERO) AND
    (W10-WMKUP(WS-P1) NOT = ZERO)
           COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
            ((W10-CCOST * W10-WMKUP(WS-P1)) / 100)).
    IF W10-CSELL = ZERO
        MOVE W10-CCOST  TO W10-CSELL.
            MOVE W10-CSELL  TO W10-WSL(WS-P1).
      *
      *    ****    C H E C K   I F   E X C L U S I V E  R O U N D I N G
      *     R E Q U I R E D
      *
           IF W05-ROUND = "E"
        MOVE W10-SL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-SL(WS-P1)
        MOVE W10-WSL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-WSL(WS-P1)
        MOVE W10-CSH(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-CSH(WS-P1).
    IF W10-TAX NOT = ZERO
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE
        COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
     (W10-SL(WS-P1) * W05-RTE)
        COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
     (W10-WSL(WS-P1) * W05-RTE)
        COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
     (W10-CSH(WS-P1) * W05-RTE)
           ELSE
        MOVE W10-WSL(WS-P1)
     TO W10-VWSL(WS-P1)
        MOVE W10-CSH(WS-P1)
     TO W10-VCSH(WS-P1)
        MOVE W10-SL(WS-P1)
     TO W10-VSL(WS-P1).
           IF W05-ROUND = "I"
        MOVE W10-VSL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VSL(WS-P1)
        MOVE W10-VWSL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VWSL(WS-P1)
        MOVE W10-VCSH(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VCSH(WS-P1)
               PERFORM CB200.
      MOVE ZERO   TO W05-RATE.
           IF W10-TAX > 0
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE.
      COMPUTE W10-VCSH(WS-P1) ROUNDED
     = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE).
      COMPUTE W10-VWSL(WS-P1) ROUNDED
     = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE).
      COMPUTE W10-VSL(WS-P1) ROUNDED
     = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE).

       BM22.
    IF WS-P1 < 4
        ADD 1   TO WS-P1
        GO TO BM20.
      DISPLAY S06.
      MOVE 20   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
    IF NOT(WS-OPTION = "N")
        GO TO BM990.

       BM25.
      MOVE "Re-key: 'A'll,pack '1','2','3' [A]"
     TO WS-ERR-MES.
      MOVE "A"   TO WS-OPTION.
      MOVE 20   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
          IF NOT(WS-OPTION = "A" OR "1" OR "2" OR "3")
        GO TO BM25.
    IF WS-OPTION = "A"
        PERFORM RESTORE-SCREEN-3
        GO TO BM00.
      DISPLAY "Esc" AT 2512
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "ape to cancel"
        WITH FOREGROUND-COLOR Blue HIGHLIGHT BACKGROUND-COLOR Cyan.
    IF WS-OPTION = "2"
        GO TO BM35
    ELSE
    IF WS-OPTION = "3"
        GO TO BM40.
      *
      *    ****    P A C K   1
      *
       BM30.
      ACCEPT S06-P1.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BM990
   WHEN OTHER
                     CALL X"E5"
       GO TO BM30
        END-EVALUATE.
      GO TO BM15.
      *
      *    ****    P A C K   2
      *
       BM35.
      ACCEPT S06-P2.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BM990
   WHEN OTHER
                     CALL X"E5"
       GO TO BM35
        END-EVALUATE.
      GO TO BM15.
      *
      *    ****    P A C K   3
      *
       BM40.
      ACCEPT S06-P3.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BM990
   WHEN OTHER
                     CALL X"E5"
       GO TO BM40
        END-EVALUATE.
      GO TO BM15.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       BM50.
          IF (W10-SL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
              MOVE W10-SL(WS-P1)
     TO W10-CSELL
              COMPUTE W10-MKUP(WS-P1) ROUNDED =
      (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
              MOVE DPT-MKUP(WS-P1)
      TO W10-MKUP(WS-P1).

       BM51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BM52.
          IF (W10-CSH(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
              MOVE W10-CSH(WS-P1)
     TO W10-CSELL
              COMPUTE W10-CMKUP(WS-P1) ROUNDED =
      (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
              MOVE DPT-CMKUP(WS-P1)
     TO W10-CMKUP(WS-P1).

       BM53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BM54.
          IF (W10-WSL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
              MOVE W10-WSL(WS-P1)
     TO W10-CSELL
              COMPUTE W10-WMKUP(WS-P1) ROUNDED =
      (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
              MOVE DPT-WMKUP(WS-P1)
     TO W10-WMKUP(WS-P1).

       BM55.
             EXIT.

       BM990.
      PERFORM RESTORE-SCREEN-3.

       BM999.
      EXIT.
      /
       ZA000-INIT        SECTION 81.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

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
      MOVE "STKMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKMEM.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT(PAR-STOCK = "A" OR "N")
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
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.

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
             MOVE PAR-PW         TO W85-PASS.
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
             MOVE 8                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-WS-HD          TO WS-WS-HD.
             MOVE PAR-CS-HD          TO WS-CS-HD.
             MOVE PAR-RT-HD          TO WS-RT-HD.
      GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK TO WS-FILE
                               MOVE WS-NETKEY   TO WS-KEY
               WHEN 7          MOVE W02-DEPART  TO WS-FILE
                               MOVE DPT-CODE    TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 22         MOVE W02-STOCKF  TO WS-FILE
                               MOVE STK-CODE    TO WS-KEYX
               WHEN 25         MOVE W02-STKMEM  TO WS-FILE
                               MOVE SME-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
