      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****     ****     ******    *
      *   ** **  **       **    **  **  **    **  **   **   **   *
      *   **     **       **    ** **    **  ** **   **   *
      *   **     **       ******* **    **  ** **     ****    *
      *   **   ***  **       ** **    **  ** **   **   *
      *   ** **  **       **  **  **    **  **   **   **   *
      *    ******   ********  **   ****     ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   B U D G E T T I N G         *
      *                                                                *
      *       Version 9.00.70 - August 2014                            *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP003.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           JUNE 1983.

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

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

       COPY "GLJRN.SL".

       COPY "LEDTRN.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       COPY "GLJRN.FD".

       COPY "LEDTRN.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-LTRKEY    PIC 9(06)  COMP-5.
       77  WS-ENQPOS       PIC  9(06)    COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-KSTORE       PIC  9(06)    COMP-5.
       77  WS-SKSTORE      PIC  9(06)    COMP-5.
       77  WS-INKEY        PIC  9(06)    COMP-5.
       77  WS-COKEY        PIC  9(06)    COMP-5.
       77  WS-GLJSTR       PIC  9(04)    COMP-5.
       77  WS-GLJKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(04)    COMP-5.
       77  WS-RECOVER      PIC  9(04)    COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-CHEQUE       PIC  9(04)    COMP-5.
       77  WS-PAGE         PIC  9(04)    COMP-5.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-S9    PIC 9(04)   COMP-5.
       77  WS-Y-END        PIC  9(02) VALUE ZERO.
       77  WS-L-END    PIC 9(04) VALUE ZERO.
       77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-MS1          PIC  9(04)    COMP-5.
       77  WS-MS2          PIC  9(04)    COMP-5.
       77  WS-VALUE        PIC S9(09)V99 COMP-3.
       77  WS-EXS          PIC  9(01).
       77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-INDG         PIC  9(01) VALUE ZERO.
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-FIRST        PIC  9(02) VALUE ZERO.
       77  WS-LAST         PIC  9(02) VALUE ZERO.
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-NUM          PIC  Z9.
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
       77  WS-BATCH    PIC 9(05)  COMP-3.
       77  WS-TNEXT        PIC  9(04)    COMP-5.
       77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-H1    PIC X(05) VALUE "DATE:".
       77  WS-H3           PIC  X(06) VALUE "BATCH:".
       77  WS-H4           PIC  X(05) VALUE "PAGE:".
       77  WS-ER1          PIC  X(07) VALUE "Account".
       77  WS-ER3          PIC  X(04) VALUE "Date".
       77  WS-ER4          PIC  X(05) VALUE "Value".
       77  WS-ER5          PIC  X(13) VALUE "Cheque Number".
       77  WS-ER6          PIC  X(09) VALUE "No Record".
       77  WS-ER7          PIC  X(13) VALUE "Group Heading".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      /
       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
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
    03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
        05  WS-MGL1     PIC X(01).
        05  WS-MGL78    PIC X(78).
        05  WS-MGL80    PIC X(01).
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

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "GLP".
    03  WS-PROG        PIC  X(03) VALUE "003".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
    03  W02-PRINTER     PIC  X(12).
          03  W02-PGE-LENGTH  PIC  9(03).
          03  W02-PRN-LENGTH  PIC  9(03).
    03  W02-LINAGE      PIC  9(03).
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

          03  R-L1 REDEFINES W02-PRN-LINE.
              05  R-DET       PIC  X(132).

          03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-1ST       PIC  X(78).
        05  FILLER      PIC  X(56).

          03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(56).
        05  R-2ND       PIC  X(78).

          03  R-L2 REDEFINES W02-PRN-LINE.
              05  R-H1        PIC  X(06).
              05  R-DTE1      PIC  Z9/99/9999.
              05  FILLER      PIC  X(23).
              05  R-H2        PIC  X(40).
              05  FILLER      PIC  X(15).
              05  R-H3        PIC  X(07).
              05  R-BTCH      PIC  ZZZZ9.
              05  FILLER      PIC  X(17).
              05  R-H4        PIC  X(06).
              05  R-PGE       PIC  ZZ9.

          03  R-L3 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(03).
              05  R-H5        PIC  X(11).
              05  R-H6        PIC  X(05).
              05  FILLER      PIC  X(01).
              05  R-H7        PIC  X(13).
              05  R-H8        PIC  X(05).
              05  FILLER      PIC  X(06).
              05  R-H9        PIC  X(09).
              05  R-H10       PIC  X(12).
              05  R-H11       PIC  X(13).
              05  FILLER      PIC  X(01).
              05  R-H12       PIC  X(09).
              05  R-H13       PIC  X(13).
              05  FILLER      PIC  X(08).
              05  R-H14       PIC  X(14).
              05  R-H15       PIC  X(09).

          03  R-L4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(03).
              05  R-ACC       PIC  9(04)/99.
              05  FILLER      PIC  X(04).
              05  R-ANME      PIC  X(24).
              05  FILLER      PIC  X(03).
              05  R-DTE2      PIC  Z9/99/9999.
              05  FILLER      PIC  X(03).
              05  R-REF       PIC  Z(07).
              05  FILLER      PIC  X(04).
              05  R-NAR       PIC  X(36).
              05  FILLER      PIC  X(02).
              05  R-DBT       PIC  Z(08)9.99.
              05  FILLER      PIC  X(02).
              05  R-CRD       PIC  Z(08)9.99-.
              05  FILLER      PIC  X(02).

          03  R-L5 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(65).
              05  R-H16       PIC  X(10).
              05  FILLER      PIC  X(01).
              05  R-H17       PIC  X(15).
              05  R-H18       PIC  X(10).
              05  FILLER      PIC  X(31).

          03  R-L6 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(08).
              05  R-H19       PIC  X(11).
              05  R-H20       PIC  X(05).
              05  FILLER      PIC  X(01).
              05  R-H21       PIC  X(13).
              05  R-H22       PIC  X(05).
              05  FILLER      PIC  X(04).
              05  R-H23       PIC  X(06).
              05  FILLER      PIC  X(01).
              05  R-H24       PIC  X(12).
              05  R-H25       PIC  X(05).
              05  FILLER      PIC  X(06).
              05  R-H26       PIC  X(14).
              05  R-H27       PIC  X(10).
              05  R-H28       PIC  X(15).
              05  R-H29       PIC  X(16).

          03  R-L7 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(08).
              05  R-DAC       PIC  9(04)/99.
              05  FILLER      PIC  X(04).
              05  R-ANAME     PIC  X(24).
              05  FILLER      PIC  X(04).
              05  R-BRANCH    PIC  X(24).
              05  FILLER      PIC  X(02).
              05  R-DTE3      PIC  Z9/99/9999.
              05  FILLER      PIC  X(02).
              05  R-DEBT      PIC  Z(08)9.99.
              05  FILLER      PIC  X(04).
              05  R-BNKAC     PIC  9(04)/99.
              05  FILLER      PIC  X(02).
              05  R-CRED      PIC  Z(08)9.99-.
              05  FILLER      PIC  X(09).

          03  R-BL2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(20).
              05  R-BH1       PIC  X(40).
              05  R-BH2       PIC  X(30).
              05  R-BH3       PIC  X(42).

          03  R-BL3 REDEFINES W02-PRN-LINE.
              05  R-BH4.
           07  R-BAC   PIC  9(06).
           07  FILLER  PIC  X(01).
           07  R-BDESC PIC  X(30).
              05  FILLER      PIC  X(95).

          03  R-BL3A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(06).
              05  R-BH5       PIC  X(10).
              05  R-BH6       PIC  X(10).
              05  R-BH7       PIC  X(10).
              05  R-BH8       PIC  X(10).
              05  R-BH9       PIC  X(10).
              05  R-BH10      PIC  X(10).
              05  R-BH11      PIC  X(10).
              05  R-BH12      PIC  X(10).
              05  R-BH13      PIC  X(10).
              05  R-BH14      PIC  X(10).
              05  R-BH15      PIC  X(10).
              05  R-BH16      PIC  X(10).
              05  R-BH17      PIC  X(06).

          03  R-BL4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(02).
              05  R-BH18      PIC  X(08).
              05  R-BH19      PIC  X(18).
              05  FILLER      PIC  X(104).

          03  R-BL4A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(02).
              05  R-BVAL      PIC  Z(08)9- OCCURS 13.

          03  R-BL5 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(02).
              05  R-BH20      OCCURS 13.
           07  R-BH21  PIC  X(09).
           07  FILLER  PIC  X(01).

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER      PIC  X(27).
           03  W10-HEAD    PIC  X(13).

       COPY "W15.GL".

       01  W20-TOTALS.
           03  W20-DEBIT   PIC S9(09)V99 COMP-3.
           03  W20-CREDIT  PIC S9(09)V99 COMP-3.
           03  W20-RESULT  PIC S9(09)V99.
           03  W20-RES1    REDEFINES W20-RESULT.
               05  W20-WHOLE
                           PIC  9(09).
               05  W20-DEC PIC S9(02).
           03  W20-TOTAL   PIC S9(09)V99 COMP-3.
           03  W20-STOTAL  PIC S9(09)V99 COMP-3.
           03  W20-BTOTAL  PIC S9(09)    COMP-3.
           03  W20-INTOT   PIC S9(09)V99 COMP-3.
           03  W20-COTOT   PIC S9(09)V99 COMP-3.
           03  W20-GTOT.
               05  W20-GRPTOT
                           PIC S9(09)V99 COMP-3 OCCURS 12.
           03  W20-STOT.
               05  W20-SUBTOT
                           PIC S9(09)V99 COMP-3 OCCURS 12.
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
       01  W30-MONTH-NAMES.
           03  W30-MONTHS.
               05  FILLER  PIC  X(24) VALUE
                           "Jan:Feb:Mar:Apr:May:Jun:".
               05  FILLER  PIC X(24) VALUE
                           "Jul:Aug:Sep:Oct:Nov:Dec:".
           03  W30-MNTHS REDEFINES W30-MONTHS.
               05  W30-MONTH OCCURS 12.
                   07  W30-MTH PIC X(03).
                   07  FILLER  PIC X(01).

       01  W35-MONTH-NAMES.
           03  W35-MONTHS.
               05  W35-MONTH OCCURS 12.
                   07  W35-MTH PIC X(03).
                   07  FILLER  PIC X(01).
           03  W35-MTHS REDEFINES W35-MONTHS.
               05  W35-MTH1.
                   07  W35-M1  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH2.
                   07  W35-M2  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH3.
                   07  W35-M3  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH4.
                   07  W35-M4  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH5.
                   07  W35-M5  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH6.
                   07  W35-M6  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH7.
                   07  W35-M7  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH8.
                   07  W35-M8  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH9.
                   07  W35-M9  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH10.
                   07  W35-M10 PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH11.
                   07  W35-M11 PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH12.
                   07  W35-M12 PIC X(03).
                   07  FILLER  PIC X(01).

       01  W40-CUR-YEAR.
          03  W40-YEAR    PIC 9(04) COMP-5 OCCURS 12.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-CALC.
           05  W50-ACNTS   OCCURS 30.
               07  W50-ANO PIC  9(06)    COMP-3.
               07  W50-PER PIC S9(03)V99 COMP-3.

       01  W55-TOTALS.
           03  W55-VAL     PIC S9(09)    COMP-3 OCCURS 12.

       01  W55-TOT         REDEFINES W55-TOTALS.
           03  W55-1       PIC S9(09)    COMP-3.
           03  W55-2       PIC S9(09)    COMP-3.
           03  W55-3       PIC S9(09)    COMP-3.
           03  W55-4       PIC S9(09)    COMP-3.
           03  W55-5       PIC S9(09)    COMP-3.
           03  W55-6       PIC S9(09)    COMP-3.
           03  W55-7       PIC S9(09)    COMP-3.
           03  W55-8       PIC S9(09)    COMP-3.
           03  W55-9       PIC S9(09)    COMP-3.
           03  W55-10      PIC S9(09)    COMP-3.
           03  W55-11      PIC S9(09)    COMP-3.
           03  W55-12      PIC S9(09)    COMP-3.

       01  W55-TOTA        REDEFINES W55-TOTALS.
          03  W55-1A        PIC S9(07)V99 COMP-3.
          03  W55-2A        PIC S9(07)V99 COMP-3.
          03  W55-3A        PIC S9(07)V99 COMP-3.
          03  W55-4A        PIC S9(07)V99 COMP-3.
          03  W55-5A        PIC S9(07)V99 COMP-3.
          03  W55-6A        PIC S9(07)V99 COMP-3.
          03  W55-7A        PIC S9(07)V99 COMP-3.
          03  W55-8A        PIC S9(07)V99 COMP-3.
          03  W55-9A        PIC S9(07)V99 COMP-3.
          03  W55-10A        PIC S9(07)V99 COMP-3.
          03  W55-11A        PIC S9(07)V99 COMP-3.
          03  W55-12A        PIC S9(07)V99 COMP-3.

       01  W55-TOTB        REDEFINES W55-TOTALS.
          03  W55-VALA    PIC S9(07)V99 COMP-3 OCCURS 12.

       01  W60-TOTALS.
           03  W60-VAL     PIC S9(09)    COMP-3 OCCURS 12.

       01  W65-TOTALS.
           03  W65-VAL     PIC S9(09)    COMP-3 OCCURS 12.

       01  W70-ACTS.
           03  W70-BRANCH  PIC  9(04)    COMP-5.

       COPY "W70.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-GNO       PIC 9(06) COMP-5
                              OCCURS 18.
       01  W80-H3.
           03  FILLER      PIC  X(13)      VALUE "BUDGET AS AT ".
          03  W80-DTE    PIC 99/99/9999.

       01  W90-COMP.
           03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-GENL.IDS".

      /
       SCREEN SECTION.

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

       01  S03.
          03  LINE  3 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "GENERAL LEDGER BUDGET".
           03  LINE  5 COLUMN 12 VALUE "Account Number".
           03          COLUMN 35 VALUE "Account Name".
           03  LINE  7 COLUMN  9 PIC X(04) USING W35-MTH1.
           03          COLUMN 25 PIC X(04) USING W35-MTH2.
           03          COLUMN 41 PIC X(04) USING W35-MTH3.
           03          COLUMN 57 PIC X(04) USING W35-MTH4.
           03  LINE  8 COLUMN  9 PIC X(04) USING W35-MTH5.
           03          COLUMN 25 PIC X(04) USING W35-MTH6.
           03          COLUMN 41 PIC X(04) USING W35-MTH7.
           03          COLUMN 57 PIC X(04) USING W35-MTH8.
           03  LINE  9 COLUMN  9 PIC X(04) USING W35-MTH9.
           03          COLUMN 25 PIC X(04) USING W35-MTH10.
           03          COLUMN 41 PIC X(04) USING W35-MTH11.
           03          COLUMN 57 PIC X(04) USING W35-MTH12.
           03  LINE 11 COLUMN  8 VALUE "Total".

       01  S04.
          03  LINE  7 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-1 AUTO.
          03        COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-2 AUTO.
          03        COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-3 AUTO.
          03        COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-4 AUTO.
          03  LINE  8 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-5 AUTO.
          03        COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-6 AUTO.
          03        COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-7 AUTO.
          03        COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-8 AUTO.
          03  LINE  9 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-9 AUTO.
          03        COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-10 AUTO.
          03        COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-11 AUTO.
          03        COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9- USING W55-12 AUTO.

       01  S-PROG.
          03  LINE  2 COLUMN 26 FOREGROUND-COLOR 7
     HIGHLIGHT
                                 VALUE "GENERAL LEDGER BUDGET CONTROL".

       01  S-MENU.
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³    ".
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "BUDGET OPTIONS".
    03        COLUMN 48 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "    ³".
    03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Create/Amend Budget³".
    03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Calculate Budgets  ³".
          03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Budget Report      ³".
          03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "4".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Clear Budgets      ³".
          03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Exit               ³".
    03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S13.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-DATE AUTO.
       01  S14.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-VAL AUTO.
       01  S15.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC ZZ9.99 USING W15-PER AUTO.
       01  S16.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(06)9 USING W15-REFER AUTO.
       01  S17.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC 9(06) USING W15-ACCOUNT AUTO.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000        SECTION.
       AA00.
          IF LS0-GLLEV < 2
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000.
      PERFORM IA000.
      CLOSE RECOVER.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       AA49.
      EXIT PROGRAM.

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP6".

       AA350-LOOKUP    SECTION.
       AA350.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM GACCNT-LOOKUP.
            PERFORM RESTORE-SCREEN.

       AA399.
             EXIT.

       COPY "APAC.HLP".

       AB000        SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "GLTRAN.RD".

       COPY "GLJRN.RD".

       COPY "LEDTRN.RD".

       COPY "SHARED.RD".

       COPY "TXTRAN.RD".

      /
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GLJRN.WR".

       COPY "LEDTRN.WR".

       COPY "TXTRAN.WR".

       COPY "GACCNT.LUP".

      /
      *       ****   *****   ***    ***   *   *  ***** ****   *   *
      *       *   *  *     * *  *   *  *   *  * *   *  *   *
      *       ****   ***    *    *   *  *   *  *** **** * *
      *       *   *  *     * *  *   *   * *  * *   *  *
      *       *   *  *****   ***    ***     *  ***** *   *  *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *   THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED      ÄÄ     (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED      ÄÄ     (DELETE)    ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)    ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.
       AY01.
            MOVE 01   TO REC-FILE.
            MOVE WS-PARKEY  TO REC-KEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-RECORD1  TO REC-PARAM.
             GO TO AY50.

       AY12.
            MOVE 12   TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
      MOVE WS-ACTION  TO REC-TYPE.
           IF WS-SKIP = "N"
               PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT
           ELSE
               MOVE "N"          TO WS-SKIP.
             MOVE G-REC          TO REC-GACCNT.
             GO TO AY50.

       AY13.
            MOVE 13   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE T-REC          TO REC-GLTRAN.
             GO TO AY50.

       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

       AY40.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.

       AY50.
             ADD 1               TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2312
   WITH FOREGROUND-COLOR 6 HIGHLIGHT
   WS-STATUS
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
               STOP RUN.

       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
    IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK  TO WS-STOCK.
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
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 4 HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "GL"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 4              TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
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
           IF PAR-PROG(WS-SUB) = "CG" OR "GL"
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
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *     NETWORK record 2.
      *
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY40 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        ADD 1   TO WS-USUB
        GO TO AY72.

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
            COMMIT.

       AY999.
             EXIT.

      /
       IA000      SECTION 5.
       IA00.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      MOVE 7   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 22   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S-MENU.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.

       IA02.
      MOVE 7   TO TOP-ROW.
      MOVE 11   TO BOTTOM-ROW.
      MOVE 6   TO SCREEN-LIN.
      MOVE 31   TO SCREEN-COL.
      MOVE 20   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       IA05.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       IA10.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO IA15
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO IA10
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO IA10
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO IA10
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO IA10
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO IA15
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO IA05
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO IA18
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO IA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF WS-OPTION < 0 OR > 4
     CALL X"E5"
     GO TO IA05
        END-IF
        GO TO IA16
    ELSE
        CALL X"E5"
        GO TO IA05.

       IA15.
    IF SCREEN-LIN < 7
        CALL X"E5"
        GO TO IA05
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
        MOVE "0"   TO WS-OPTION.
    GO TO IA18.

       IA16.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 11   TO SCREEN-LIN
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
        MOVE 10   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       IA18.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
    IF WS-OPTION = "0"
        GO TO IA999.
    IF WS-OPTION = "1"
        PERFORM IB000
    ELSE
    IF WS-OPTION = "2"
        PERFORM IC000
    ELSE
    IF WS-OPTION = "3"
        PERFORM ID000
    ELSE
    IF WS-OPTION = "4"
        PERFORM IE000.
      GO TO IA00.

       IA999.
             EXIT.
      /
       IB000      SECTION 5.
       IB00.
             PERFORM ERASE-SCREEN.
             DISPLAY S03.

       IB05.
            MOVE ZERO   TO W15-ACCOUNT.

       IB15.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3
    FOREGROUND-COLOR 6 HIGHLIGHT
       "ape to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             ACCEPT S17 AT 0527.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO IB999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       CALL X"E5"
       GO TO IB15
        END-EVALUATE
        DISPLAY S17 AT 0527
               IF W15-ACCOUNT = ZERO
           GO TO IB15.
      PERFORM CLEAR-L50.
             MOVE 1401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.
           IF W15-ACCOUNT = ZERO
               GO TO IB999.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
        PERFORM ERROR-MESSAGE
              GO TO IB15.

       IB25.
      MOVE W15-ACCOUNT  TO G-AC.
            PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
        PERFORM ERROR-MESSAGE
              GO TO IB15.
            MOVE G-NAME  TO W15-ACNME.
            DISPLAY W15-ACNME AT 0548
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO IB00.
           IF G-IND = 1
               DISPLAY "Figures for this A/C are calculated" AT 1429
         WITH FOREGROUND-COLOR 6 HIGHLIGHT
               GO TO IB15.
             MOVE 1401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.
            MOVE 1   TO WS-S1.
            ADD 1 WS-Y-END  GIVING WS-S2.

       IB26.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
             MOVE G-BVAL (WS-S2) TO W55-VAL (WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO IB26.

       IB30.
             ACCEPT S04.
            MOVE 1   TO WS-S1.
            ADD 1 WS-Y-END  GIVING WS-S2.

       IB32.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
            MOVE W55-VAL (WS-S1)
     TO G-BVAL (WS-S2).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO IB32.
             COMPUTE W20-TOTAL = G-BVAL (1) + G-BVAL (2) + G-BVAL (3)
                                + G-BVAL (4) + G-BVAL (5) + G-BVAL (6)
                                + G-BVAL (7) + G-BVAL (8) + G-BVAL (9)
                                + G-BVAL (10) + G-BVAL (11)
                                + G-BVAL (12).
            MOVE W20-TOTAL  TO W25-1.
            DISPLAY W25-7 AT 1114
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO IB30.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
             GO TO IB00.

       IB999.
             EXIT.
      /
       IC000      SECTION 5.
       IC00.
             PERFORM ERASE-SCREEN.
            DISPLAY "Busy with calcs - Please be patient" AT 0612
              WITH FOREGROUND-COLOR 6 HIGHLIGHT
            MOVE 1   TO WS-S2.

       IC03.
            MOVE ZERO   TO W55-VAL (WS-S2).
           IF WS-S2 < 12
              ADD 1   TO WS-S2
               GO TO IC03.
            MOVE 1   TO WS-S2.
      MOVE 100000  TO G-AC.
      PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        GO TO IC999.
      DISPLAY "Account :" AT 0812.

       IC05.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              GO TO IC999.
      DISPLAY G-AC AT 0822 WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5.
            MOVE G-AC   TO W15-ACCOUNT.
           IF W15-ASER = ZERO
              GO TO IC05.
          IF W15-GRP = 1
               GO TO IC25.
           IF G-IND NOT = 1
              GO TO IC05.

       IC10.
             MOVE G-BPER         TO W15-PER.
            MOVE 1   TO WS-S1.

       IC15.
             COMPUTE G-BVAL (WS-S1) = (W55-VAL (WS-S1) * W15-PER)
                                        / 100.
           IF WS-S1 < 12
              ADD 1   TO WS-S1
               GO TO IC15.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
             GO TO IC05.

       IC25.
            ADD G-BVAL (1)  TO W55-VAL (1).
            ADD G-BVAL (2)  TO W55-VAL (2).
            ADD G-BVAL (3)  TO W55-VAL (3).
            ADD G-BVAL (4)  TO W55-VAL (4).
            ADD G-BVAL (5)  TO W55-VAL (5).
            ADD G-BVAL (6)  TO W55-VAL (6).
            ADD G-BVAL (7)  TO W55-VAL (7).
            ADD G-BVAL (8)  TO W55-VAL (8).
            ADD G-BVAL (9)  TO W55-VAL (9).
            ADD G-BVAL (10)  TO W55-VAL (10).
            ADD G-BVAL (11)  TO W55-VAL (11).
            ADD G-BVAL (12)  TO W55-VAL (12).
            GO TO IC05.
       IC999.
             EXIT.
      /
       ID000      SECTION 5.
       ID0.
      MOVE 100000  TO G-AC.
      PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        GO TO ID999.
      MOVE "P"   TO WS-COMMAND.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.

       ID00.
             PERFORM ERASE-SCREEN.
            DISPLAY "BUDGET REPORT" AT 0233
        WITH FOREGROUND-COLOR 7 HIGHLIGHT.
            DISPLAY "Busy printing Budget report" AT 0612
        WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      DISPLAY "Account :" AT 0812.

       ID05.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
            MOVE W90-CNAME  TO R-BH1.
            MOVE W05-TODAY  TO W80-DTE.
            MOVE W80-H3  TO R-BH3.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE "ACC"   TO R-BH18.
            MOVE "DESCRIPTION"  TO R-BH19.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W35-M1  TO R-BH5.
            MOVE W35-M2  TO R-BH6.
            MOVE W35-M3  TO R-BH7.
            MOVE W35-M4  TO R-BH8.
            MOVE W35-M5  TO R-BH9.
            MOVE W35-M6  TO R-BH10.
            MOVE W35-M7  TO R-BH11.
            MOVE W35-M8  TO R-BH12.
            MOVE W35-M9  TO R-BH13.
            MOVE W35-M10  TO R-BH14.
            MOVE W35-M11  TO R-BH15.
            MOVE W35-M12  TO R-BH16.
            MOVE "TOTAL"  TO R-BH17.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       ID06.
            MOVE 1   TO WS-S2.

       ID08.
            MOVE ZERO   TO W55-VAL (WS-S2) W60-VAL (WS-S2)
              W65-VAL (WS-S2).
           IF WS-S2 < 12
              ADD 1   TO WS-S2
               GO TO ID08.
            MOVE ZERO   TO W15-MONTH.

       ID10.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
               GO TO ID50.
      DISPLAY G-AC AT 0822 WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5.
          IF W15-MONTH = G-GRP
               GO TO ID45.
           IF G-GRP > 3
               GO TO ID50.
           IF G-GRP = 1
              MOVE "**** INCOME ****"
     TO R-BH19
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              MOVE SPACES  TO R-DET
               GO TO ID45.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
          IF W15-MONTH = 1
               GO TO ID55.
           IF W15-MONTH = ZERO
               GO TO ID40.
           IF W15-MONTH > 2
               GO TO ID45.

       ID15.
             MOVE ALL "-" TO R-BH21 (1) R-BH21 (2) R-BH21 (3) R-BH21 (4)
                             R-BH21 (5) R-BH21 (6) R-BH21 (7) R-BH21 (8)
                             R-BH21 (9) R-BH21 (10) R-BH21 (11)
                             R-BH21 (12) R-BH21 (13).
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       ID20.
            MOVE "TOTAL COST OF SALES"
     TO R-BH4.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
            MOVE 1   TO WS-S1.

       ID25.
            SUBTRACT W60-VAL (WS-S1)
     FROM W55-VAL (WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1
               GO TO ID25.

       ID30.
            ADD 1 WS-Y-END  GIVING WS-S2.
            MOVE 1   TO WS-S1.

       ID32.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
            MOVE W60-VAL (WS-S2)
     TO R-BVAL (WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO ID32.
            MOVE W60-VAL (1)  TO W20-TOTAL.
            ADD W60-VAL (2)  TO W20-TOTAL.
            ADD W60-VAL (3)  TO W20-TOTAL.
            ADD W60-VAL (4)  TO W20-TOTAL.
            ADD W60-VAL (5)  TO W20-TOTAL.
            ADD W60-VAL (6)  TO W20-TOTAL.
            ADD W60-VAL (7)  TO W20-TOTAL.
            ADD W60-VAL (8)  TO W20-TOTAL.
            ADD W60-VAL (9)  TO W20-TOTAL.
            ADD W60-VAL (10)  TO W20-TOTAL.
            ADD W60-VAL (11)  TO W20-TOTAL.
            ADD W60-VAL (12)  TO W20-TOTAL.
            MOVE W20-TOTAL  TO R-BVAL (13).
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            PERFORM ID15.

       ID35.
             PERFORM ID15.
            MOVE W55-TOTALS  TO W60-TOTALS.
            MOVE "GROSS PROFIT/LOSS "
     TO R-BH4.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             PERFORM ID30 THRU ID32.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.

       ID40.
            MOVE "**** EXPENSES ****"
     TO R-BH19.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.

       ID45.
            MOVE G-GRP   TO W15-MONTH.
            MOVE G-NAME  TO R-BDESC.
            MOVE G-AC   TO R-BAC.
           IF G-ASER = ZERO
               GO TO ID46.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
            ADD 1 WS-Y-END  GIVING WS-S2.
            MOVE 1   TO WS-S1.

       ID45A.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
             MOVE G-BVAL (WS-S2) TO R-BVAL (WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO ID45A.
            MOVE G-BVAL (1)  TO W20-TOTAL.
            ADD G-BVAL (2)  TO W20-TOTAL.
            ADD G-BVAL (3)  TO W20-TOTAL.
            ADD G-BVAL (4)  TO W20-TOTAL.
            ADD G-BVAL (5)  TO W20-TOTAL.
            ADD G-BVAL (6)  TO W20-TOTAL.
            ADD G-BVAL (7)  TO W20-TOTAL.
            ADD G-BVAL (8)  TO W20-TOTAL.
            ADD G-BVAL (9)  TO W20-TOTAL.
            ADD G-BVAL (10)  TO W20-TOTAL.
            ADD G-BVAL (11)  TO W20-TOTAL.
            ADD G-BVAL (12)  TO W20-TOTAL.
            MOVE W20-TOTAL  TO R-BVAL (13).
           IF G-GRP = 1
              ADD G-BVAL (1)  TO W55-VAL (1)
              ADD G-BVAL (2)  TO W55-VAL (2)
              ADD G-BVAL (3)  TO W55-VAL (3)
              ADD G-BVAL (4)  TO W55-VAL (4)
              ADD G-BVAL (5)  TO W55-VAL (5)
              ADD G-BVAL (6)  TO W55-VAL (6)
              ADD G-BVAL (7)  TO W55-VAL (7)
              ADD G-BVAL (8)  TO W55-VAL (8)
              ADD G-BVAL (9)  TO W55-VAL (9)
              ADD G-BVAL (10)  TO W55-VAL (10)
              ADD G-BVAL (11)  TO W55-VAL (11)
              ADD G-BVAL (12)  TO W55-VAL (12)
               GO TO ID46.
           IF G-GRP = 2
              ADD G-BVAL (1)  TO W60-VAL (1)
              ADD G-BVAL (2)  TO W60-VAL (2)
              ADD G-BVAL (3)  TO W60-VAL (3)
              ADD G-BVAL (4)  TO W60-VAL (4)
              ADD G-BVAL (5)  TO W60-VAL (5)
              ADD G-BVAL (6)  TO W60-VAL (6)
              ADD G-BVAL (7)  TO W60-VAL (7)
              ADD G-BVAL (8)  TO W60-VAL (8)
              ADD G-BVAL (9)  TO W60-VAL (9)
              ADD G-BVAL (10)  TO W60-VAL (10)
              ADD G-BVAL (11)  TO W60-VAL (11)
              ADD G-BVAL (12)  TO W60-VAL (12)
               GO TO ID46.
            ADD G-BVAL (1)  TO W65-VAL (1).
            ADD G-BVAL (2)  TO W65-VAL (2).
            ADD G-BVAL (3)  TO W65-VAL (3).
            ADD G-BVAL (4)  TO W65-VAL (4).
            ADD G-BVAL (5)  TO W65-VAL (5).
            ADD G-BVAL (6)  TO W65-VAL (6).
            ADD G-BVAL (7)  TO W65-VAL (7).
            ADD G-BVAL (8)  TO W65-VAL (8).
            ADD G-BVAL (9)  TO W65-VAL (9).
            ADD G-BVAL (10)  TO W65-VAL (10).
            ADD G-BVAL (11)  TO W65-VAL (11).
            ADD G-BVAL (12)  TO W65-VAL (12).

       ID46.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM ID05.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
            MOVE SPACES  TO R-DET.
             GO TO ID10.

       ID50.
            MOVE W65-TOTALS  TO W60-TOTALS.
             PERFORM ID15.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
            MOVE "TOTAL EXPENDITURE ACCOUNTS"
     TO R-BH4.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
            MOVE SPACES  TO R-DET.
            MOVE 1   TO WS-S1.
             PERFORM ID25 THRU ID32.
             PERFORM ID15.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
            MOVE W55-TOTALS  TO W60-TOTALS.
            MOVE "TOTAL PROFIT/LOSS"
     TO R-BH4.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO ID54.
            MOVE SPACES  TO R-DET.
             PERFORM ID30 THRU ID32.

       ID52.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF NOT (WS-PRINTER = "S")
        MOVE 1   TO W02-PRN-TYPE
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
            GO TO ID999.

       ID54.
            MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      GO TO ID52.

       ID55.
            MOVE W55-TOTALS  TO W60-TOTALS.
             PERFORM ID15.
             MOVE "TOTAL INCOME" TO R-BH4.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             PERFORM ID30 THRU ID32.
            MOVE 1   TO WS-S2.

       ID56.
            MOVE ZERO   TO W60-VAL (WS-S2).
           IF WS-S2 < 12
              ADD 1   TO WS-S2
               GO TO ID56.

       ID60.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            GO TO ID45.

       ID999.
             EXIT.
      /
       IE000      SECTION 5.
       IE00.
             PERFORM ERASE-SCREEN.
            DISPLAY "Busy clearing bugets - Please be patient" AT 0612
              WITH FOREGROUND-COLOR 6 HIGHLIGHT
      MOVE 100000  TO G-AC.
      PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        GO TO IE999.
      DISPLAY "Account :" AT 0812.
       IE05.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              GO TO IE999.
      DISPLAY G-AC AT 0822 WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5.
            MOVE G-AC   TO W15-ACCOUNT.
           IF W15-ASER = ZERO
              GO TO IE05.
       IE10.
            MOVE 1   TO WS-S1.
       IE15.
            MOVE ZERO   TO G-BVAL (WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1
              GO TO IE15.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
            GO TO IE05.
       IE999.
             EXIT.
      /
       ZA000      SECTION 9.
       ZA00.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      COPY "FUNCTION.PRO".

    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
     GO TO ZA02.
             MOVE 110            TO WS-USUB.

       ZA02.
            OPEN OUTPUT RECOVER GLJRN.
             CLOSE RECOVER GLJRN.
             OPEN I-O RECOVER GLJRN.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W90-CNAME.
            MOVE PAR-DMY  TO W05-TODAY.
            MOVE PAR-YMD  TO W05-TYMD.
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DEBGL      TO W70-DEBGL.
             MOVE PAR-CREGL      TO W70-CREGL.
             MOVE PAR-INTGL      TO W70-INTGL.
             MOVE PAR-BNKGL      TO W70-BNKGL.
             MOVE PAR-UNPROF     TO W70-UNPROF.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            MOVE G-BATCH  TO WS-BATCH.
           IF G-YEAR-END = ZERO
              MOVE 2   TO G-YEAR-END
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
            MOVE 1   TO WS-S1.
            MOVE G-YEAR-END  TO WS-S2 WS-Y-END.
            MOVE G-LAST-YE  TO WS-L-END.
      MOVE G-OPEN  TO WS-OPEN.
            ADD 1   TO WS-S2.

       ZA15.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
            MOVE W30-MONTH (WS-S2)
     TO W35-MONTH (WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO ZA15.
            MOVE ZERO   TO WS-ERROR.
           IF W70-BNKGL = ZERO
              DISPLAY "Parameters not set up - " AT 2020
         "ENTER" WITH FOREGROUND-COLOR 6 HIGHLIGHT
              ACCEPT WS-OPTION AT 2050
        WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO
              MOVE 1   TO WS-ERROR.
            MOVE WS-Y-END  TO WS-S1.
            MOVE W05-CY  TO WS-S2.
           IF W05-MM < WS-Y-END
              SUBTRACT 1  FROM WS-S2.

       ZA20.
            ADD 1   TO WS-S1.
           IF WS-S1 > 12
              MOVE 1   TO WS-S1
              ADD 1   TO WS-S2.
            MOVE WS-S2   TO W40-YEAR (WS-S1).
           IF WS-S1 NOT = WS-Y-END
               GO TO ZA20.
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
            GO TO ZA999.

       COPY "ZA49.PRO".

       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.

       ZA60.
            MOVE WS-G1   TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
      MOVE WS-G8   TO WS-TCH(WS-S1) WS-BCH(WS-S1).
           IF WS-S1 < 80
              ADD 1   TO WS-S1
               GO TO ZA60.
      MOVE WS-G9   TO WS-TCH(1).
      MOVE WS-G10  TO WS-TCH(80).
      MOVE WS-G11  TO WS-BCH(1).
      MOVE WS-G12  TO WS-BCH(80).
      MOVE WS-G14  TO WS-TCHR(1) WS-BCHR(1).
      MOVE WS-G13  TO WS-TCHR(80) WS-BCHR(80).
            MOVE WS-G2   TO WS-TCHR(11) WS-TCHR(45)
              WS-TCHR(63).
            MOVE WS-G3   TO WS-MCHR(11) WS-MCHR(45)
              WS-MCHR(63)
        WS-MCHR(1) WS-MCHR(80).
            MOVE WS-G4   TO WS-BCHR(11) WS-BCHR(45)
              WS-BCHR(63).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"
     TO WS-WRKHD
    ELSE
        MOVE "Workstation"
     TO WS-WRKHD
        MOVE LS-USER  TO WS-WRKST.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999.
             EXIT.
      /
       ZB000      SECTION 9.

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
              MOVE WS-PARID  TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
              MOVE W02-RECOVER  TO WS-FILE
              MOVE WS-RECKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 26
              MOVE W02-GLJRN  TO WS-FILE
              MOVE WS-GLJKEY  TO WS-KEY
          ELSE
           IF WS-F-ERROR = 32
               MOVE W02-TXTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE TAX-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 39
               MOVE W02-LEDTRN   TO WS-FILE
               MOVE WS-LTRKEY    TO WS-KEY.

       COPY "DISPERR.PRO".
