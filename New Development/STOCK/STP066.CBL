      $set LINKCOUNT"512" GNT"STP066.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******   ****    ******    ******    *
      *   ** **     **     **   **  **  **   ** **  **   **   *
      *   **        **     **   ** **    **  **     **        *
      *    ******      **     ****** **    **  ********  ********   *
      *  **     **     ** **    **  ** **  **   **   *
      *   ** **     **     **  **  **   ** **  **   **   *
      *    ******      **     **   ****    ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    S T O C K   L O C A T I O N  R E P O R T   P R O G R A M   *
      *                                                                *
      *     VERSION 8.13.07 - March 2008          *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STP066.
       AUTHOR.        J.W.LEMMON SOFTWARE CC.
       DATE-WRITTEN.   APRIL 1996.

     COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2016
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

       COPY SPARTS.SL.

       COPY STOCK.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

      /
       DATA DIVISION.
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

       COPY PARAM.FDE.

       COPY SPARTS.FDE.

       COPY STOCK.FDE.

       COPY WARHSE.FDE.

       COPY WSTOCK.FDE.

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
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
       77  WS-PARKEY    PIC 9(04)  COMP-5.
       77  WS-NETKEY    PIC 9(04)  COMP-5.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-PAGE    PIC 9(04)   COMP-5.
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
       77  WS-PQNT    PIC X(01).
       77  WS-WS    PIC X(01) VALUE "N".
       77  WS-MEMO    PIC X(01) VALUE "N".
       77  WS-PRT    PIC X(01) VALUE "Y".
       77  WS-BIN    PIC X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-OPTION    PIC X(01).
       77  WS-ZLT    PIC X(01) VALUE "Y".
       77  WS-ZQT    PIC X(01) VALUE "Y".
       77  WS-NUM    PIC ZZ9.
       77  WS-ERROR    PIC 9(01).
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-KSTORE    PIC 9(04)  COMP-5.
       77  WS-INVF    PIC X(01)  COMP-X.
       77  WS-ORD    PIC S9(05)  COMP-3.
       77  WS-TOTAL    PIC S9(09)V99 COMP-3.
       77  WS-VALUE    PIC S9(09)V99 COMP-3.
       77  WS-CCOST    PIC S9(09)V999 COMP-3.
       77  WS-CSELL    PIC S9(09)V99 COMP-3.
       77  WS-SCOST    PIC S9(09)V99 COMP-3.
       77  WS-SSELL    PIC S9(09)V99 COMP-3.
       77  WS-TCOST    PIC S9(09)V999 COMP-3.
       77  WS-TSELL    PIC S9(09)V99 COMP-3.
       77  WS-DEPT    PIC X(04).
       77  WS-CAT    PIC X(04).
       77  WS-SCAT    PIC X(04).
       77  WS-WEEK    PIC 9(01).
       77  WS-LABCDE    PIC X(08).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
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
    03  WS-PROG        PIC  X(03) VALUE "066".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

       COPY SPARTS.ID.

       COPY STOCK.ID.

       COPY WARHSE.ID.

       COPY WSTOCK.ID.

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
              05  REP-DTE     PIC  Z9/99/99.
              05  FILLER      PIC  X(05).
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
              05  REP-CNO     PIC  9(03).
              05  REP-CATNO REDEFINES REP-CNO PIC 9(03).
              05  FILLER      PIC  X(01).
              05  REP-CAT     PIC  X(30).
              05  FILLER      PIC  X(36).
          03  REP-LINE9 REDEFINES W02-PRN-LINE.
              05  REP-PG      PIC  X(06).
              05  REP-P-N     PIC  Z(03)9.
              05  FILLER      PIC  X(22).
              05  REP-COMP    PIC  X(40).
              05  REP-REPORT  PIC  X(34).
              05  FILLER      PIC  X(12).
              05  REP-DAT     PIC  X(06).
              05  REP-DT      PIC  Z9/99/99.
          03  REP-LINE9A REDEFINES W02-PRN-LINE.
              05  REP-HEAD6   PIC  X(13).
              05  REP-HEAD6A  PIC  X(04).
              05  REP-HEAD7   PIC  X(25).
              05  REP-HEAD8   PIC  X(12).
              05  REP-HEAD9   PIC  X(08).
              05  REP-HEAD10  PIC  X(08).
              05  REP-HEAD11  PIC  X(09).
              05  REP-HEAD12  PIC  X(09).
              05  REP-HEAD14  PIC  X(07).
              05  REP-HEAD15  PIC  X(10).
              05  REP-HEAD16  PIC  X(07).
              05  REP-HEAD17  PIC  X(08).
              05  REP-HEAD18  PIC  X(12).
          03  REP-LINE9B REDEFINES W02-PRN-LINE.
              05  S-H1        PIC  X(15).
              05  S-H2        PIC  X(28).
              05  S-L1        PIC  X(01).
              05  S-H3        PIC  X(03).
              05  S-L2        PIC  X(01).
              05  S-H4        PIC  X(07).
              05  S-L3        PIC  X(01).
              05  S-H5        PIC  X(07).
              05  S-L4        PIC  X(01).
              05  S-H6        PIC  X(07).
              05  S-L5        PIC  X(01).
              05  S-H7        PIC  X(07).
              05  S-L6        PIC  X(01).
          03  REP-LINE9C REDEFINES W02-PRN-LINE.
              05  S-CDE       PIC  X(14).
              05  FILLER      PIC  X(01).
              05  S-DSC       PIC  X(28).
              05  FILLER      PIC  X(01).
              05  S-BN        PIC  X(03).
              05  FILLER      PIC  X(01).
              05  S-QNT       PIC  Z(04)9.9.
        05  S-QNTN REDEFINES S-QNT
          PIC  -(04)9.9.
              05  FILLER      PIC  X(25).
          03  REP-LINE11 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(13).
              05  REP-HEAD13  PIC  X(26).
              05  FILLER      PIC  X(02).
              05  REP-DCOST   PIC  Z(08)9.99.
              05  FILLER      PIC  X(02).
              05  REP-DSELL   PIC  Z(08)9.99.
              05  FILLER      PIC  X(13).
          03  INV-L1 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(04).
              05  INV-H1      PIC  X(17).
              05  INV-H2      PIC  X(33).
              05  INV-H3      PIC  X(13).
              05  INV-H4      PIC  X(12).
              05  INV-H5      PIC  X(13).
              05  INV-H6      PIC  X(14).
              05  INV-H7      PIC  X(13).
              05  INV-H8      PIC  X(10).
        05  FILLER      PIC  X(03).
          03  INV-L2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(04).
              05  INV-ITM     PIC  X(14).
              05  FILLER      PIC  X(03).
              05  INV-DESC    PIC  X(30).
              05  FILLER      PIC  X(03).
        05  INV-REF     PIC  X(10).
        05  FILLER      PIC  X(04).
        05  INV-PER     PIC  X(08).
        05  FILLER      PIC  X(02).
              05  INV-COST    PIC  Z(07)9.999.
              05  FILLER      PIC  X(02).
              05  INV-SELL    PIC  Z(07)9.99.
              05  FILLER      PIC  X(02).
              05  INV-CASH    PIC  Z(07)9.99.
              05  FILLER      PIC  X(02).
              05  INV-WSALE   PIC  Z(07)9.99.
        05  FILLER      PIC  X(03).
          03  INV-L3 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(04).
              05  INV-HEAD2   PIC  X(10).
              05  INV-CNO     PIC  9(03).
              05  INV-CATNO REDEFINES INV-CNO PIC 9(03).
              05  FILLER      PIC  X(01).
              05  INV-CAT     PIC  X(30).
              05  FILLER      PIC  X(84).
    03  PRC-L1 REDEFINES W02-PRN-LINE.
        05  PRC-H1      PIC  X(15).
        05  PRC-H2      PIC  X(06).
        05  PRC-H3      PIC  X(09).
        05  PRC-H4      PIC  X(32).
        05  PRC-H5.
     07  FILLER  PIC  X(01).
     07  PRC-H5A PIC  X(12).
        05  PRC-H6.
     07  FILLER  PIC  X(05).
     07  PRC-H6A PIC  X(04).
        05  PRC-H7.
     07  FILLER  PIC  X(06).
     07  PRC-H7A PIC  X(06).
        05  FILLER      PIC  X(02).
        05  PRC-H8.
     07  FILLER  PIC  X(01).
     07  PRC-H8A PIC  X(12).
        05  PRC-H9.
     07  FILLER  PIC  X(05).
     07  PRC-H9A PIC  X(04).
        05  PRC-H10.
     07  FILLER  PIC  X(06).
     07  PRC-H10A PIC X(06).
          03  PRC-L2 REDEFINES W02-PRN-LINE.
              05  PRC-ITM     PIC  X(14).
              05  FILLER      PIC  X(01).
              05  PRC-BIN     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  PRC-CCODE   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  PRC-DESC    PIC  X(30).
              05  FILLER      PIC  X(01).
        05  PRC-E-WSALE PIC  Z(07)9.99.
        05  FILLER      PIC  X(01).
              05  PRC-E-CASH  PIC  Z(07)9.99.
              05  FILLER      PIC  X(01).
        05  PRC-E-RET   PIC  Z(07)9.99.
        05  FILLER      PIC  X(01).
              05  PRC-I-WSALE PIC  Z(07)9.99.
              05  FILLER      PIC  X(01).
        05  PRC-I-CASH  PIC  Z(07)9.99.
        05  FILLER      PIC  X(01).
              05  PRC-I-RET   PIC  Z(07)9.99.
    03  PRI-L1 REDEFINES W02-PRN-LINE.
        05  PRI-H1      PIC  X(15).
        05  PRI-H2      PIC  X(06).
        05  PRI-H3      PIC  X(08).
        05  PRI-H4      PIC  X(33).
        05  PRI-H5      PIC  X(15).
        05  PRI-H6      PIC  X(09).
        05  PRI-H7      PIC  X(09).

          03  PRC-L2 REDEFINES W02-PRN-LINE.
              05  PRI-ITM     PIC  X(14).
              05  FILLER      PIC  X(01).
              05  PRI-BIN     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  PRI-CCODE   PIC  X(09).
              05  FILLER      PIC  X(01).
              05  PRI-DESC    PIC  X(31).
              05  FILLER      PIC  X(01).
        05  PRI-EXCL    PIC  Z(07)9.99.
        05  FILLER      PIC  X(01).
              05  PRI-VAT     PIC  Z(07)9.99.
              05  FILLER      PIC  X(01).
              05  PRI-INCL    PIC  Z(07)9.99.

    03  PRI-L3 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(42).
        05  PRI-MEMO    PIC  X(60).

          03  MOV-LINE1 REDEFINES W02-PRN-LINE.
              05  MOV-H1      PIC  X(15).
              05  MOV-H2      PIC  X(04).
              05  MOV-H3      PIC  X(23).
              05  MOV-MONTH.
           07  MOV-H4  PIC  X(16).
           07  MOV-H5  PIC  X(08).
           07  MOV-H6  PIC  X(16).
              05  MOV-RMNTH REDEFINES MOV-MONTH.
           07  MOV-H7  PIC  X(09).
           07  MOV-H8  PIC  X(11).
           07  MOV-H9  PIC  X(14).
           07  MOV-H10 PIC  X(06).
              05  FILLER      PIC  X(02).
              05  MOV-YEAR.
           07  MOV-H11 PIC  X(20).
           07  MOV-H12 PIC  X(08).
           07  MOV-H13 PIC  X(20).
              05  MOV-RYR REDEFINES MOV-YEAR.
           07  MOV-H14 PIC  X(09).
           07  MOV-H15 PIC  X(11).
           07  MOV-H16 PIC  X(15).
           07  MOV-H17 PIC  X(07).
           07  MOV-H18 PIC  X(06).

          03  MOV-LINE2 REDEFINES W02-PRN-LINE.
              05  MOV-ITEM    PIC  X(14).
              05  FILLER      PIC  X(01).
              05  MOV-BIN     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  MOV-DESC    PIC  X(22).
              05  FILLER      PIC  X(01).
              05  MOV-MQTY    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-MCOST   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-MSELL   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-MPROF   PIC  Z(05)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YQTY    PIC  Z(05)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YCOST   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YSELL   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YPROF   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-GP      PIC  Z(02)9.99.

          03  ORD-LINE1 REDEFINES W02-PRN-LINE.
              05  ORD-H1      PIC  X(15).
              05  ORD-H2      PIC  X(04).
              05  ORD-H3      PIC  X(23).
              05  ORD-H4      PIC  X(11).
              05  ORD-H5      PIC  X(09).
              05  ORD-MOVE.
           07  ORD-H6  PIC  X(22).
           07  ORD-H7  PIC  X(10).
           07  ORD-H8  PIC  X(22).
           07  FILLER  PIC  X(01).
           07  ORD-H9  PIC  X(15).
              05  ORD-MOV REDEFINES ORD-MOVE.
           07  ORD-H10 PIC  X(09).
           07  ORD-H11 PIC  X(09).
           07  ORD-H12 PIC  X(08).
           07  ORD-H13 PIC  X(11).
           07  ORD-H14 PIC  X(11).
           07  ORD-H15 PIC  X(07).
           07  ORD-H16 PIC  X(11).
           07  ORD-H17 PIC  X(04).
          03  ORD-LINE2 REDEFINES W02-PRN-LINE.
              05  ORD-ITM     PIC  X(14).
              05  FILLER      PIC  X(01).
              05  ORD-BIN     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  ORD-DESC    PIC  X(22).
              05  FILLER      PIC  X(01).
              05  ORD-QUANT   PIC  Z(04)9.99-.
              05  ORD-REORD   PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTH1    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTH2    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTH3    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MAVG    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTD     PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-YTD     PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-ORDER   PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-COST    PIC  Z(05)9.99.
    03  QTY-L1 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(03).
        05  QTY-H1      PIC  X(18).
        05  QTY-H2      PIC  X(35).
        05  QTY-H3      PIC  X(14).
        05  QTY-H4      PIC  X(10).
    03  QTY-L2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(03).
        05  QTY-ITM     PIC  X(14).
        05  FILLER      PIC  X(04).
        05  QTY-DESC    PIC  X(30).
        05  FILLER      PIC  X(04).
        05  QTY-QUANT   PIC  Z(05)9.99-.
        05  FILLER      PIC  X(03).
        05  QTY-PRC.
     07  QTY-PRICE PIC Z(05)9.99.
        05  QTY-DTE REDEFINES QTY-PRC.
     07  QTY-DATE PIC Z9/99/99.
     07  FILLER  PIC  X(01).
        05  FILLER      PIC  X(03).

    03  DEP-L1 REDEFINES W02-PRN-LINE.
        05  DEP-H1      PIC  X(15).
        05  DEP-H2      PIC  X(04).
        05  DEP-H3      PIC  X(34).
        05  DEP-H4      PIC  X(10).
        05  DEP-H5      PIC  X(14).
        05  DEP-H6      PIC  X(12).
        05  DEP-H7      PIC  X(07).
        05  DEP-H8.
     07  FILLER  PIC  X(01).
     07  DEP-H8A PIC  X(06).
        05  DEP-H9.
     07  FILLER  PIC  X(02).
     07  DEP-H9A PIC  X(04).
        05  DEP-H10.
     07  FILLER  PIC  X(01).
     07  DEP-H10A
          PIC  X(06).
        05  FILLER      PIC  X(02).
        05  DEP-H11     PIC  X(09).
        05  DEP-H12     PIC  X(05).

    03  DEP-L2 REDEFINES W02-PRN-LINE.
        05  DEP-ITM     PIC  X(14).
        05  FILLER      PIC  X(01).
        05  DEP-BIN     PIC  X(03).
        05  FILLER      PIC  X(01).
        05  DEP-DESC    PIC  X(30).
        05  FILLER      PIC  X(01).
        05  DEP-QNT     PIC  Z(06)9.999-.
        05  DEP-COST    PIC  Z(06)9.999.
        05  FILLER      PIC  X(01).
        05  DEP-AVG     PIC  Z(05)9.999.
        05  FILLER      PIC  X(01).
        05  DEP-RET     PIC  Z(06)9.99.
        05  FILLER      PIC  X(01).
        05  DEP-MWS     PIC  ZZ9.99.
        05  FILLER      PIC  X(01).
        05  DEP-MCSH    PIC  ZZ9.99.
        05  FILLER      PIC  X(01).
        05  DEP-MRET    PIC  ZZ9.99.
        05  FILLER      PIC  X(01).
        05  DEP-ACT     PIC  Z9/99/99.
        05  FILLER      PIC  X(01).
        05  DEP-GP      PIC  ZZ9.99.

    03  DEP-L3 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(19).
        05  DEP-T-HEAD  PIC  X(30).
        05  FILLER      PIC  X(11).
        05  DEP-TCOST   PIC  Z(08)9.999.
        05  FILLER      PIC  X(10).
        05  DEP-TSELL   PIC  Z(08)9.99.
        05  FILLER      PIC  X(37).

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
           03  W25-VALUE          PIC S9(07)V9(04) COMP-3.
           03  W25-TOTAL          PIC S9(07)V9(04) COMP-3.

       01  W30-COST-CODE.
           03  W30-CODE           PIC X(01) OCCURS 10.
       01  W35-COST.
           03  W35-CST            PIC 9(08)V99.
           03  W35-WHOLE  REDEFINES W35-CST
                                  PIC 9(10).
           03  W35-CPRINT.
               05  W35-CEDIT      PIC ZZZZZZ9999.
           03  W35-EOC REDEFINES W35-CPRINT.
               05  W35-CHAR       PIC X(01) OCCURS 10.
           03  W35-EON REDEFINES W35-CPRINT.
               05  W35-NUM        PIC 9(01) OCCURS 10.
           03  W35-CPRN REDEFINES W35-CPRINT.
               05  FILLER         PIC  X(01).
               05  W35-CSTPRN     PIC  X(09).
           03  W35-S1             PIC 9(02) COMP-5.
           03  W35-S2             PIC 9(02) COMP-5.

       01  W40-DATES.
           03  W40-DATE1          PIC 9(06).
           03  W40-DTE1 REDEFINES W40-DATE1.
               05  W40-Y1         PIC 9(02).
               05  W40-M1         PIC 9(02).
               05  W40-D1         PIC 9(02).
           03  W40-DATE2          PIC 9(06).
           03  W40-DTE2 REDEFINES W40-DATE2.
               05  W40-Y2         PIC 9(02).
               05  W40-M2         PIC 9(02).
               05  W40-D2         PIC 9(02).

       COPY W40.WS.

       01  W40-WAREHOUSE.
           03  W40-NAME1.
              05  W40-NAME    PIC  X(40).
          03  W40-NAME2 REDEFINES W40-NAME1.
              05  W40-NCHAR OCCURS 40 PIC X(01).
           03  W40-CONTACT     PIC  X(30).
    03  W40-CODE        PIC  X(06).
          03  W40-ADD.
              05  W40-ADDRESS1.
           07  W40-ADD1 PIC X(30).
           07  W40-ADD2 PIC X(30).
           07  W40-ADD3 PIC X(30).
           07  W40-ADD4 PIC X(30).
              05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
           07  W40-ACHAR OCCURS 120 PIC X(01).
           03  W40-POST        PIC 9(04).
           03  W40-TEL         PIC X(14).
           03  W40-FAX         PIC X(14).

       COPY FUNCTION.WS.

       COPY W50.WS.

       COPY W70STOCK.WS.

       01  W70-KEYS.
           03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       01  W75-KEYS.
    03  W75-S       PIC  9(02) COMP-5.
    03  W75-S1       PIC  9(02) COMP-5.
    03  W75-S2       PIC  9(02) COMP-5.
    03  W75-WHOUSE OCCURS 18.
        05  W75-WHCDE  PIC  X(06).

       01  W95-COMP        PIC X(40).
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

       COPY CHAIN.LS.

      /
       SCREEN SECTION.

       COPY BLANK.CRT.

       01  S03.
          03  LINE 18 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(14) USING STK-CODE.
       01  S04.
          03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
                                 VALUE "Busy loading stock departments".
       01  S07.
          03  LINE  8 COLUMN 20 FOREGROUND-COLOR 14 VALUE
                                 "Busy printing departmental report".

       01  S08.
          03  LINE 10 COLUMN 20 VALUE "Select department".

       01  S15.
          03  LINE 18 COLUMN 12 VALUE "Stock record:".

       COPY WARLUP.CRT.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA00M   SECTION.
       AA00.
          IF LS0-STLEV < 3
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM BE000.
    IF WS-ERROR = 9
        MOVE "No details in Department file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      PERFORM BE00S.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
      PERFORM SAVE-SCREEN.
            MOVE 4   TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM WARHSE-LOOKUP.
      PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
       AB000              SECTION.
       AB15.
      MOVE "H"   TO W02-PRN-TYPE.
             MOVE SPACES         TO REP-DETAIL1.
            MOVE "Page:"  TO REP-PG.
            MOVE "Date:"  TO REP-DAT.
             MOVE W95-COMP       TO REP-COMP.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-N.
             MOVE W12-TODAY      TO REP-DT.
            MOVE "STOCK LOCATION REPORT"
                                 TO REP-REPORT.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "Location:"  TO REP-HEAD2.
      MOVE WAR-NAME  TO REP-CAT.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "Category:"  TO REP-HEAD2.
             MOVE DPT-SALES-NO   TO REP-CATNO.
             MOVE DPT-DESC       TO REP-CAT.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "Item Code"  TO DEP-H1.
            MOVE "Bin"   TO DEP-H2.
            MOVE "Description"  TO DEP-H3.
            MOVE "Quantity"  TO DEP-H4.
            MOVE "Cost-Price"  TO DEP-H5.
            MOVE "Average"  TO DEP-H6.
            MOVE "Retail"  TO DEP-H7.
             MOVE ALL "_"        TO DEP-H8 DEP-H10.
            MOVE "Markup"  TO DEP-H9.
            MOVE "Active"  TO DEP-H11.
             MOVE "GP-%"         TO DEP-H12.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE WS-WS-HD  TO DEP-H8.
            MOVE WS-CS-HD  TO DEP-H9.
            MOVE "Retail"  TO DEP-H10A.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       AB20.
             EXIT.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
       AB25.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.

       AB30.
             EXIT.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
       AB35.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
              MOVE SPACES  TO REP-DETAIL1.
      MOVE ZERO   TO WS-PAGE.

       AB40.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

       COPY STOCK.1ST.

       COPY WARHSE.LUP.

       COPY APAC.HLP.

      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
       CA000         SECTION.
       CA00.
            MOVE 6   TO W50-LENGTH.

       CA05.
            MOVE W40-CODE  TO W50-DATA.
           IF W50-1 = SPACE
        MOVE W50-5  TO W40-CODE
               GO TO CA05.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-DENT  TO W40-CODE.

       CA06.
            MOVE W50-DENT  TO WAR-CODE.

       CA10.
      PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA99.
             EXIT.

      /
       BE000  SECTION.
       BE00.
       BE00.
      MOVE "!"   TO DPT-CODE.
      PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        MOVE 9   TO WS-ERROR
    ELSE
        MOVE ZERO  TO WS-ERROR.

       BE09.
      EXIT.
      /
       BE00S            SECTION.
       BE00.
             PERFORM ERASE-SCREEN.
      DISPLAY "LOCATION STOCK REPORT" AT 0230
       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      MOVE SPACES  TO W40-CODE.
      DISPLAY "Warehouse Code:" AT 0404.

       BE02.
      DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to do Warehouse Lookup,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      ACCEPT W40-CODE AT 0420
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO BE999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO BE02
        END-EVALUATE
        DISPLAY W40-CODE AT 0420
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5
        GO TO BE02.
            PERFORM CLEAR-L50.
          IF W40-CODE = SPACE
              GO TO BE999.

       BE03.
      PERFORM CA000.
          IF WS-F-ERROR = 51
              MOVE "Z"   TO WS-ETYPE
              PERFORM AA50
        DISPLAY W40-CODE AT 0420
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5
              IF W40-CODE = SPACES
           GO TO BE02
               ELSE
     GO TO BE03.
      DISPLAY WAR-NAME AT 0427
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BE02.
      DISPLAY "Print stock with Lead Time = 0.00" AT 0604.
       BE05.
      ACCEPT WS-ZLT AT 0639
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-ZLT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
    IF NOT (WS-ZLT = "Y" OR "N")
        GO TO BE05.
      DISPLAY "Print stock with zero quantity" AT 0804.
       BE10.
      ACCEPT WS-ZQT AT 0835
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-ZQT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
    IF NOT (WS-ZQT = "Y" OR "N")
        GO TO BE10.
       BE50.
      DISPLAY "A" AT 1004 WITH FOREGROUND-COLOR 6 HIGHLIGHT
       "ll or "
       "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT
       "elected Departments".
      MOVE SPACE   TO WS-OPTION.

       BE55.
      ACCEPT WS-OPTION AT 1033
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "S")
        GO TO BE55.
    IF WS-OPTION = "S"
        GO TO BE110.

       BE85.
             DISPLAY S07.
      MOVE "'C'ontinue or 'E'xit  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BE86.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "E" OR "C")
        GO TO BE85.
           IF WS-OPTION = "E"
        GO TO BE999.
            DISPLAY S15.
      MOVE "!"   TO DPT-CODE.
      PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
             PERFORM AB25.

       BE90.
      PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        GO TO BE92.
    IF NOT (DPT-CODE = "XXXX" OR "YYYY")
        IF NOT (DPT-SCDE = "VAT")
     GO TO BE95.
      GO TO BE90.

       BE92.
            MOVE "STOCK TOTAL"  TO DEP-T-HEAD.
            MOVE WS-TCOST  TO DEP-TCOST.
            MOVE WS-TSELL  TO DEP-TSELL.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
            MOVE SPACES  TO REP-DETAIL1.
             PERFORM AB35.

       BE93.
            GO TO BE999.

       BE95.
            MOVE 0   TO WS-S1.
      MOVE "Y"   TO WS-PRT.
    IF NOT (WS-PRINTER = "S")
        DISPLAY DPT-DESC AT 1412
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
      MOVE WS-CAT  TO STK-LDG WS-SCAT.
      MOVE "!"   TO STK-CODE.
             PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
        MOVE "X"   TO WS-OPTION
               GO TO BE106.

       BE100.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF (WS-F-ERROR = 22) OR (STK-LDG NOT = WS-SCAT)
               GO TO BE105.
    IF WS-ZLT = "N"
        IF STK-LTIME = ZERO
     GO TO BE100.
      MOVE W40-CODE  TO WST-WAR.
      MOVE STK-CODE  TO WST-EXT-CODE.
      PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO BE100.
    IF WS-ZQT = "N"
        IF WST-QUANT = ZERO
     GO TO BE100.
    IF WS-PRT = "Y"
              IF WS-PAGE > ZERO
     MOVE "S"  TO WS-COMMAND
     PERFORM CALL-PRINTUTL
     MOVE "P"  TO WS-COMMAND
        END-IF
              PERFORM AB15
        MOVE "N"   TO WS-PRT.
    IF STK-IND > 2
        MOVE ZERO  TO WST-QUANT.
    IF STK-IND = 1
        IF NOT (WS-INVF = 1)
     MOVE ZERO  TO WST-QUANT.
    IF NOT (WS-PRINTER = "S")
              DISPLAY S03.
      PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE "X"   TO WS-OPTION
              GO TO BE106.
            MOVE STK-CODE  TO DEP-ITM.
             MOVE STK-DESC       TO DEP-DESC.
    IF STK-IND = 4
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO BE101.
             MOVE STK-BIN        TO DEP-BIN.
            MOVE WST-QUANT  TO DEP-QNT.
             MOVE STK-COST       TO DEP-COST.
             MOVE STK-SELL       TO DEP-RET.
      *    ****    A V E R A G E   C O S T
    IF WST-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
    IF STK-IND < 2
              IF STK-AVGE = ZERO
           MOVE STK-COST TO STK-AVGE.
            MOVE STK-AVGE  TO DEP-AVG.
             COMPUTE DEP-MWS ROUNDED = ((STK-WSALE - STK-COST)
                                       * 100.000) / STK-COST.
             COMPUTE DEP-MRET ROUNDED = ((STK-SELL - STK-COST)
                                        * 100.000) / STK-COST.
             COMPUTE DEP-MCSH ROUNDED = ((STK-CASH - STK-COST)
                                        * 100.000) / STK-COST.
            MOVE STK-MTDV  TO WS-SSELL.
            MOVE STK-MTDC  TO WS-SCOST.
           IF WS-SSELL = ZERO OR WS-SCOST = ZERO
              MOVE STK-COST  TO WS-SCOST
               COMPUTE WS-SSELL = (STK-SELL + STK-CASH) / 2.000 + .005.
             COMPUTE DEP-GP ROUNDED = ((WS-SSELL - WS-SCOST)
                                      * 100.000) / WS-SSELL.
            MOVE STK-ACT  TO DEP-ACT.
            MULTIPLY STK-AVGE BY WST-QUANT
     GIVING WS-VALUE.
            MULTIPLY STK-SELL BY WST-QUANT
     GIVING WS-TOTAL.
            ADD WS-VALUE  TO WS-CCOST.
            ADD WS-TOTAL  TO WS-CSELL.

       BE101.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM AB15.
            MOVE SPACES  TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE "X"   TO WS-OPTION
              GO TO BE106.

       BE102.
             GO TO BE100.

       BE105.
    IF WS-PRT = "Y"
        GO TO BE106.
          IF W02-LINAGE > (W02-PRN-LENGTH - 4)
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM AB15.
             MOVE SPACES         TO REP-DETAIL1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "DEPARTMENT TOTAL COST/SELL"
     TO DEP-T-HEAD.
            MOVE WS-CCOST  TO DEP-TCOST.
            MOVE WS-CSELL  TO DEP-TSELL.
             ADD WS-CCOST        TO WS-TCOST.
             ADD WS-CSELL        TO WS-TSELL.
             MOVE ZERO           TO WS-CCOST WS-CSELL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       BE106.
             EXIT.

       BE108.
    IF WS-OPTION = "X"
        GO TO BE999.
            GO TO BE90.

       BE110.
             PERFORM ERASE-SCREEN.
             DISPLAY "SELECTIVE DEPARTMENTAL REPORT" AT 0226
                     WITH FOREGROUND-COLOR 15.
             DISPLAY S08.
             DISPLAY S15.
            ACCEPT WS-DEPT AT 1038
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
          IF WS-DEPT = SPACES
              GO TO BE999.
            MOVE WS-DEPT  TO DPT-CODE WS-CAT.
      PERFORM CLEAR-L50.
          IF (DPT-CODE = "XXXX" OR "YYYY") OR
       (DPT-SCDE = "VAT")
        MOVE "Invalid code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BE110.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        MOVE "Invalid department code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BE110.
      DISPLAY DPT-DESC AT 1412
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       BE115.
      MOVE "'C'ontinue or 'R'ekey  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "R" OR "C")
               GO TO BE115.
           IF WS-OPTION = "R"
               GO TO BE110.
      PERFORM AB25.
            PERFORM BE95 THRU BE106.
    IF WS-OPTION = "X"
        GO TO BE999.
      PERFORM AB35
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
            MOVE ZERO   TO WS-PAGE.
             GO TO BE110.

       BE999.
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
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "SPARTS"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SPARTS.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
      MOVE "WARHSE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-WARHSE.
      MOVE "WSTOCK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-WSTOCK.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-INV  TO WS-INVF.

       ZA01B.
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-WHOLESALE  TO WS-WS.
            MOVE PAR-COST-CODE  TO W30-COST-CODE.
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
            MOVE 3   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            MOVE STK-WEEK  TO WS-WEEK.
             MOVE STK-LABCDE     TO WS-LABCDE.
            MOVE 8   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-WS-HD = X"00000000000000000000")
        MOVE PAR-WS-HD  TO WS-WS-HD.
    IF NOT (PAR-CS-HD = X"00000000000000000000")
        MOVE PAR-CS-HD  TO WS-CS-HD.
      INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
      INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W40-DATE1 W40-DATE2.
           IF W40-M2 > 3
               SUBTRACT 3        FROM W40-M2
           ELSE
               SUBTRACT 1        FROM W40-Y2
               ADD 9             TO W40-M2.
            MOVE PAR-COMPANY  TO W95-COMP.

       ZA05.
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
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
             GO TO DISPLAY-FILE-NAME.

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
           IF WS-F-ERROR = 21
               MOVE W02-SPARTS   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STK-CODE  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 51
              MOVE W02-WARHSE TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE WAR-CODE  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 52
              MOVE W02-WSTOCK TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE WST-KEY  TO WS-KEYX.
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
