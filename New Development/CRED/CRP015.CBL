      $set LINKCOUNT"596" GNT"CRP015.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****       **     ********   *
      *   **    **  **    **  **    **   **  **     ***     **         *
      *   **        **    **  **    **  **    **     **     *******    *
      *   **        ******    *******   **    **     **           **   *
      *   **        **   **   **        **    **     **           **   *
      *   **    **  **    **  **         **  **      **     **    **   *
      *    ******   **    **  **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   O R D E R S                              *
      *                                                                *
      *       Version 9.00.80 - August 2015                            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CRP015.
       AUTHOR.         J.W.LEMMON (CSS).
       DATE-WRITTEN.        JUNE, 1983.


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

       COPY "CREDIT.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "PORDER.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "STKMEM.SL".

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

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "PORDER.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

      *
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-IXS2         PIC  9(04)    COMP-5.
       77  WS-IXS3         PIC  9(04)    COMP-5.
       77  WS-IXS4         PIC  9(04)    COMP-5.
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-RECKEY       PIC  9(04)    COMP-5.
       77  WS-NETKEY    PIC 9(04)  COMP-5.
       77  WS-PURKEY       PIC  9(04)    COMP-5.
       77  WS-KEYSTR       PIC  9(04)    COMP-5.
       77  WS-RECOVER      PIC  9(04)    COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-PURLAST      PIC  9(04)    COMP-5.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR        PIC  9(01)    VALUE 0.
       77  WS-ETYPE        PIC  X(01).
       77  WS-DEL    PIC X(01).
       77  WS-ENT          PIC  X(08).
       77  WS-ENT1         PIC  X(08).
       77  WS-INDS         PIC  9(01)    VALUE 0.
       77  WS-INDI         PIC  9(01)    VALUE 0.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-ITM    PIC X(18).
       77  WS-ITM1    PIC X(18).
       77  WS-ORD          PIC  X(01).
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "Y".
       77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-CHAR1    PIC X(01).
       77  WS-IND    PIC  9(01) VALUE 0.
       77  WS-SKEY         PIC  9(05).
       77  WS-KEY1         PIC  9(04)     COMP-5.
       77  WS-LINES        PIC  9(04)     COMP-5 VALUE 0.
       77  WS-PAGE         PIC  9(04)     COMP-5 VALUE 0.
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-ER1    PIC X(09)  VALUE "No record".
       77  WS-ER2          PIC X(25)     VALUE
                           "May not be less than cost".
       77  WS-ER8          PIC  X(09) VALUE "No record".
       77  WS-HDTE         PIC X(04)     VALUE "DATE".
       77  WS-HACNO        PIC X(04)     VALUE "ACNO".
       77  WS-HCRED        PIC X(09)     VALUE "CREDITOR:".
       77  WS-HQNT         PIC X(05)     VALUE "QUANT".
       77  WS-HDESC        PIC X(11)     VALUE "DESCRIPTION".
       77  WS-HCOST        PIC X(10)     VALUE "COST PRICE".
       77  WS-HSELL        PIC X(10)     VALUE "SELL PRICE".
       77  WS-HNAME        PIC X(04)     VALUE "NAME".
       77  WS-HAGE         PIC X(12)     VALUE "AGE 30 - 180".
       77  WS-HCHQ         PIC X(06)     VALUE "CHEQUE".
       77  WS-HSETT        PIC X(22)     VALUE
                           "30  60  90 120 150 180".
       77  WS-SDTE         PIC X(04)     VALUE "Date".
       77  WS-SACNO        PIC X(04)     VALUE "Acno".
       77  WS-SCRED        PIC X(09)     VALUE "Creditor:".
       77  WS-SQNT         PIC X(05)     VALUE "Quant".
       77  WS-SDESC        PIC X(11)     VALUE "Description".
       77  WS-SCOST        PIC X(10)     VALUE "Cost price".
       77  WS-SSELL        PIC X(10)     VALUE "Sell price".
       77  WS-SNAME        PIC X(04)     VALUE "Name".
       77  WS-SAGE         PIC X(12)     VALUE "Age 30 - 180".
       77  WS-SCHQ         PIC X(06)     VALUE "Cheque".
       77  WS-SSETT        PIC X(22)     VALUE
                           "30  60  90 120 150 180".
       77  WS-H1           PIC X(05)     VALUE "DATE:".
       77  WS-H3           PIC X(05)     VALUE "PAGE:".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      /
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END               PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "CRP015".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "CREDITOR ORDERS (STOCK)".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).


       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "CRP".
    03  WS-PROG        PIC  X(03) VALUE "015".

       01  W00-TRAN-CODES.
           03  W00-C-ABR   PIC  X(03).
           03  W00-C-DESC  PIC  X(15).
           03  W00-C-ACT   PIC  9(01).
           03  W00-C-CODE  PIC  9(02)    COMP-5.
           03  W00-C-GL    PIC  9(06)    COMP-3.
           03  W00-C-DAY   PIC  9(06)    COMP-5.
           03  W00-C-VAL   PIC S9(09)V99 COMP-3.
           03  W00-C-MTD   PIC  9(06)    COMP-5.
           03  W00-C-MTDV  PIC S9(09)V99 COMP-3.
           03  W00-C-YTD   PIC  9(06)    COMP-5.
           03  W00-C-YTDV  PIC S9(09)V99 COMP-3.

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

    03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-SLA       PIC  X(78).
        05  FILLER      PIC  X(58).

    03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(58).
        05  R-SLB       PIC  X(78).

    03  R-L1 REDEFINES W02-PRN-LINE.
        05  R-DET    PIC X(140).

    03  R-L2 REDEFINES W02-PRN-LINE.
        05  R-H1     PIC X(06).
        05  R-DTE1    PIC Z9/99/9999.
        05  FILLER    PIC X(14).
        05  R-H2     PIC X(40).
        05  FILLER    PIC X(17).
        05  R-H3     PIC X(06).
        05  R-PGE    PIC ZZ9.

    03  R-L2A REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(38).
        05  R-HEAD    PIC X(20).
        05  FILLER    PIC X(38).

    03  R-L3 REDEFINES W02-PRN-LINE.
        05  R-ORD    PIC Z(07).
        05  FILLER    PIC X(01).
        05  R-ITM    PIC X(14).
        05  FILLER    PIC X(01).
        05  R-DESC    PIC X(18).
        05  FILLER    PIC X(01).
        05  R-QNT    PIC Z(04)9.99.
        05  FILLER    PIC X(01).
        05  R-COST    PIC Z(04)9.999.
        05  FILLER    PIC X(01).
        05  R-SELL    PIC Z(04)9.99-.
        05  FILLER    PIC X(01).
        05  R-EXT    PIC Z(06)9.99-.

    03  R-L3A REDEFINES W02-PRN-LINE.
        05  I-H1     PIC X(08).
        05  I-H2     PIC X(15).
        05  I-H3     PIC X(22).
        05  I-H4     PIC X(07).
        05  I-H5     PIC X(10).
        05  I-H6     PIC X(11).
        05  I-H7     PIC X(09).

    03  R-L4 REDEFINES W02-PRN-LINE.
        05  G-H1     PIC X(10).
        05  G-SUPP    PIC X(06).
        05  FILLER    PIC X(01).
        05  G-NAME    PIC X(40).
        05  FILLER    PIC X(23).

    03  R-L4A REDEFINES W02-PRN-LINE.
        05  G-H3     PIC X(05).
        05  G-INVN    PIC Z(06)9.
        05  FILLER    PIC X(01).
        05  G-H4     PIC X(06).
        05  FILLER    PIC X(01).
        05  G-INVD    PIC Z9/99/9999.
        05  FILLER    PIC X(01).
        05  G-H5     PIC X(06).
        05  G-RAIL    PIC Z(04)9.99-.
        05  FILLER    PIC X(01).
        05  G-H6     PIC X(06).
        05  G-DISC    PIC Z(04)9.99-.
        05  FILLER    PIC X(01).
        05  G-H6A    PIC X(05).
        05  G-VAT    PIC Z(05)9.99-.
        05  FILLER    PIC X(01).
        05  G-H7     PIC X(06).
        05  G-TOTAL    PIC Z(06)9.99-.

    03  R-L5 REDEFINES W02-PRN-LINE.
        05  J-H1     PIC X(07).
        05  J-H2     PIC X(39).
        05  J-H3     PIC X(10).
        05  J-H4     PIC X(15).
        05  J-H5     PIC X(13).
        05  J-H6     PIC X(12).

    03  R-L5A REDEFINES W02-PRN-LINE.
        05  J-ACNO    PIC X(06).
        05  FILLER    PIC X(01).
        05  J-NAME    PIC X(35).
        05  FILLER    PIC X(01).
        05  J-DATE    PIC Z9/99/9999.
        05  FILLER    PIC X(01).
        05  J-COST    PIC Z(08)9.99-.
        05  FILLER    PIC X(02).
        05  J-SELL    PIC Z(08)9.99-.
        05  FILLER    PIC X(02).
        05  J-30     PIC X(02).
        05  J-60     PIC X(02).
        05  J-90     PIC X(02).
        05  J-120    PIC X(02).
        05  J-150    PIC X(02).
        05  J-180    PIC X(02).

           03  R-L8 REDEFINES W02-PRN-LINE.
               05  OR-H1           PIC  X(12).
               05  OR-H2.
                   07  FILLER      PIC  X(04).
                   07  OR-H2A      PIC  X(27).
               05  OR-H3           PIC  X(06).
               05  OR-H4           PIC  X(12).
               05  OR-H5           PIC  X(10).
               05  OR-H6           PIC  X(09).
               05  OR-H7           PIC  X(22).
               05  OR-H8           PIC  X(13).
               05  OR-H9           PIC  X(09).
               05  OR-H10          PIC  X(08).

           03  R-L8A REDEFINES W02-PRN-LINE.
               05  OR-ORD          PIC  Z(07)9/99.
               05  FILLER          PIC  X(01).
               05  OR-DESC.
                   07  OR-ITEM     PIC  X(14).
                   07  OR-SL       PIC  X(01).
                   07  OR-ITM      PIC  X(03).
                   07  FILLER      PIC  X(12).
               05  FILLER          PIC  X(01).
               05  OR-QUANT        PIC  Z(04)9.
               05  FILLER          PIC  X(01).
               05  OR-CURRENCY.
                   07  FILLER      PIC  X(01).
                   07  OR-RATE     PIC  Z(02)9.99.
                   07  FILLER      PIC  X(03).
               05  FILLER          PIC  X(01).
               05  OR-COST         PIC  Z(06)9.99.
               05  FILLER          PIC  X(01).
               05  OR-PMT.
                   07  FILLER      PIC  X(01).
                   07  OR-DISC     PIC  Z9.99    BLANK WHEN ZERO.
                   07  FILLER      PIC  X(02).
               05  FILLER          PIC  X(01).
               05  OR-NAME.
                   07  OR-CONTACT.
                       09  OR-ACNO PIC  X(06).
                       09  FILLER  PIC  X(15).
                   07  FILLER      PIC  X(01).
                   07  OR-TEL      PIC  X(12).
               05  FILLER          PIC  X(01).
               05  OR-DEL          PIC  Z9/99/99.
               05  FILLER          PIC  X(01).
               05  OR-RMKS.
                   07  OR-PSLIP    PIC  Z(08).

           03  R-L9 REDEFINES W02-PRN-LINE.
               05  REP-PG          PIC  X(06).
               05  REP-P-N         PIC  Z(03)9.
               05  FILLER          PIC  X(16).
               05  REP-COMP        PIC  X(40).
               05  FILLER          PIC  X(02).
               05  REP-REPORT      PIC  X(40).
               05  FILLER          PIC  X(08).
               05  REP-DAT         PIC  X(06).
               05  REP-DT          PIC  Z9/99/9999.

       COPY "W05.WS".

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
           03  W05-HLF-VAL     PIC  9(01)V99.
           03  W05-TQTR-VAL    PIC  9(01)V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.

       COPY "W10.CRP".

       01  W15-INPUT-AREA.
          03  W15-DATE        PIC 9(08).
    03  W15-DTE REDEFINES W15-DATE.
              05  W15-DATE-CHAR
                PIC  X(01)     OCCURS 8.
          03  W15-DATE1       REDEFINES W15-DATE.
               05  W15-DD      PIC 9(02).
               05  W15-MM      PIC 9(02).
        05  W15-CC      PIC 9(02).
               05  W15-YY      PIC 9(02).
    03  W15-DATE2       REDEFINES W15-DATE.
        05  FILLER      PIC  9(04).
        05  W15-CCYY    PIC  9(04).
          03  W15-ACNO        PIC  X(06).
           03  W15-SNO         REDEFINES W15-ACNO.
              05  W15-APREF   PIC  X(01).
              05  W15-SERIAL  PIC  X(05).
          03  W15-NAME.
              05  W15-NCHAR   PIC  X(01) OCCURS 40.
           03  W15-N           REDEFINES W15-NAME.
               05  W15-NIX.
                   07  W15-ENTRY PIC X(08).
           07  FILLER  PIC  X(16).
              05  FILLER      PIC  X(16).
          03  W15-ADDRESS     PIC  X(120).
           03  W15-ADD  REDEFINES W15-ADDRESS.
              05  W15-ADDR1   PIC  X(30).
              05  W15-ADDR2   PIC  X(30).
              05  W15-ADDR3   PIC  X(30).
              05  W15-ADDR4   PIC  X(30).
           03  W15-ADD2 REDEFINES W15-ADDRESS.
              05  W15-ACHAR   PIC  X(01) OCCURS 120.
          03  W15-PCODE       PIC  9(04).
    03  W15-CONTACT     PIC  X(30).
    03  W15-TEL        PIC  X(14).
    03  W15-FAX        PIC  X(14).
          03  W15-UN        PIC  9(05)V99.
          03  W15-UNIT REDEFINES W15-UN.
              05  FILLER      PIC  9(01).
              05  W15-UNT     PIC  9(04).
        05  W15-UDEC    PIC  9(02).
          03  W15-ORD        PIC  9(10).
           03  W15-ONO         REDEFINES W15-ORD.
              05  W15-OPREF   PIC  9(08).
              05  W15-OSUB    PIC  9(02).
    03  W15-CURRENCY    PIC  X(10).
           03  W15-OCOST       PIC S9(09)V999 COMP-3.
           03  W15-RATE        PIC S9(03)V999 COMP-3.
          03  W15-COST        PIC S9(09)V999 COMP-3.
          03  W15-SELL        PIC S9(09)V999 COMP-3.
          03  W15-SETT        PIC  9(06).
           03  W15-SET         REDEFINES W15-SETT.
              05  W15-30      PIC  9(01).
              05  W15-60      PIC  9(01).
              05  W15-90      PIC  9(01).
              05  W15-120     PIC  9(01).
              05  W15-150     PIC  9(01).
              05  W15-180     PIC  9(01).
           03  W15-PAY         REDEFINES W15-SETT.
              05  W15-MONTH   PIC  9(01) OCCURS 6.
          03  W15-DEL        PIC 9(08).
           03  W15-DEL-DMY REDEFINES W15-DEL.
               05  W15-DEL-DD  PIC 9(02).
               05  W15-DEL-MM  PIC 9(02).
        05  W15-DEL-CC  PIC 9(02).
               05  W15-DEL-YY  PIC 9(02).
          03  W15-CAT        PIC  9(02).
          03  W15-GRNN        PIC X(08).
          03  W15-GRND        PIC 9(08)     COMP-3.
          03  W15-INVN        PIC X(08).
          03  W15-INVD        PIC 9(08)     COMP-3.
          03  W15-TCOST       PIC S9(09)V99 COMP-3.
           03  W15-BAL         PIC S9(09)V99 COMP-3.
           03  W15-TSELL       PIC S9(09)V99 COMP-3.
           03  W15-QUANT       PIC S9(05)V9999 COMP-3.
          03  W15-XQNT        PIC S9(09)V99 COMP-3.
          03  W15-UCOST       PIC S9(09)V999 COMP-3.
          03  W15-USELL       PIC S9(09)V999 COMP-3.
          03  W15-UCASH       PIC S9(09)V999 COMP-3.
          03  W15-UWSALE      PIC S9(09)V999 COMP-3.
           03  W15-RSELL       PIC S9(09)V99.
    03  W15-RSR1 REDEFINES W15-RSELL.
               05  W15-RSR     PIC S9(09).
               05  W15-RSRANDS REDEFINES W15-RSR.
                   07  W15-RSR8
                               PIC S9(08).
                   07  W15-RSR9
                               PIC  9(01).
               05  W15-RSC     PIC  9(02).
           03  W15-RSRC2 REDEFINES W15-RSELL.
               05  FILLER      PIC S9(08).
               05  W15-RSRCC   PIC  9V99.
          03  W15-CHECK       PIC S9(07)V99  COMP-3.
           03  W15-RAIL        PIC S9(09)V99 COMP-3.
           03  W15-DISCOUNT    PIC S9(09)V99 COMP-3.
          03  W15-DISC1       PIC  99V99.
    03  W15-DSC1 REDEFINES W15-DISC1
          PIC  V9(04).
          03  W15-DISC2       PIC  99V99.
    03  W15-DSC2 REDEFINES W15-DISC2
          PIC  V9(04).
    03  W15-TAXABLE OCCURS 12.
              05  W15-TAXVAL  PIC S9(09)V99 COMP-3.
        05  W15-VATVAL  PIC S9(09)V99 COMP-3.
    03  W15-TXBL        PIC S9(09)V99 COMP-3.
           03  W15-INCVAL      PIC S9(09)V99 COMP-3.
           03  W15-IVAT        PIC S9(07)V99 COMP-3.
    03  W15-U-VAT       PIC S9(07)V999 COMP-3.
    03  W15-U-NETT      PIC S9(07)V999 COMP-3.
    03  W15-U-TXBL      PIC S9(07)V999 COMP-3.
    03  W15-U-NTXBL     PIC S9(07)V999 COMP-3.
    03  W15-U-INCVAL    PIC S9(07)V999 COMP-3.
    03  W15-VAT        PIC S9(07)V99  COMP-3.
           03  W15-TOTVAT      PIC S9(07)V99 COMP-3.
           03  W15-TOTAL       PIC S9(09)V99 COMP-3.
           03  W15-IUN         PIC S9(09)V99 COMP-3.
          03  W15-AVRG        PIC S9(08)V999 COMP-3.
           03  W15-ICOST       PIC S9(09)V999 COMP-3.
           03  W15-INCOST      PIC S9(09)V999 COMP-3.
           03  W15-ILCOST      PIC S9(09)V999 COMP-3.
           03  W15-RMKS        PIC  X(08) VALUE "Account".
           03  W15-PMT         PIC  X(08) VALUE "Deliver".
           03  W15-PSLIP       PIC  X(08).
    03  W15-PMNT        PIC  X(12).

       01  W16-TOTALS.
           03  FILLER          PIC  X(12) VALUE "TOTAL COST: ".
           03  W16-COST        PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(12) VALUE " TOTAL VAT: ".
           03  W16-VAT         PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(08) VALUE " TOTAL: ".
           03  W16-TOTAL       PIC  Z(08)9.99DB.

       01  W20-CALCS.
          03  W20-RESULT      PIC  9(09)V99.
           03  W20-RESULT-1    REDEFINES W20-RESULT.
              05  W20-WHOLE   PIC  9(09).
              05  W20-DECIMAL PIC  9(02).
          03  W20-ORD        PIC  9(07)    COMP-3.
    03  W20-DATE.
        05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
          PIC 9(08).
    03  W21-DATE.
        05  W21-CC      PIC  99.
        05  W21-YY      PIC  99.
        05  W21-MM      PIC  99.
        05  W21-DD      PIC  99.
    03  W21-DTE REDEFINES W21-DATE
          PIC  9(08).

       01  W25-CALCS.
          03  W25-RESULT      PIC  9(05)V99.
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY     PIC  9(04).
        05  W25-SUB     PIC  9(01).
        05  FILLER      PIC  9(02).

       01  W25-VALUES.
           03  W25-CFLOW OCCURS 6 PIC S9(09)V99  COMP-3.
           03  W25-V1          PIC S9(09)V99 COMP-3.
           03  W25-V2          PIC S9(09)V99 COMP-3.
       01  W25-VALUES-2.
           03  W25-CFLW2 OCCURS 6 PIC S9(09)V99  COMP-3.
           03  W25-V1-2        PIC S9(09)V99 COMP-3.
           03  W25-V2-2        PIC S9(09)V99 COMP-3.

       01  W25-EDIT.
           03  W25-5.
              05  W25-1       PIC  ZZZZ9.
           03  W25-3V2.
              05  W25-2       PIC  ZZ9.99.
           03  W25-7V2.
              05  W25-3       PIC  Z(06)9.99.
           03  W25-6V2.
              05  W25-3A      PIC  Z(06)9.99.
           03  W25-3N.
              05  W25-4       PIC  ZZ9.
           03  W25-S7V2A.
              05  W25-CVAL    PIC  Z(06)9.99-.
           03  W25-DTE.
              05  W25-DATE    PIC  99/99/9999.
           03  W25-6.
              05  W25-NO      PIC  Z(07)9.
           03  W25-S5V2.
              05  W25-SVAL    PIC  Z(07)9.99DB.
           03  W25-S7V2.
              05  W25-VAL2    PIC  Z(06)9.99DB.
           03  W25-5V2 REDEFINES W25-S7V2.
              05  W25-VAL     PIC  Z(07)9.99-.
           03  W25-72.
               05  W25-VAL3    PIC S9(07)V99.
           03  W25-PRC.
              05  W25-IPRC    PIC  Z(06)9.999.

       01  W30-AGE.
           03  W30-CUR         PIC S9(09)V99 COMP-3.
           03  W30-30          PIC S9(09)V99 COMP-3.
           03  W30-60          PIC S9(09)V99 COMP-3.
           03  W30-90          PIC S9(09)V99 COMP-3.
           03  W30-120         PIC S9(09)V99 COMP-3.
           03  W30-150         PIC S9(09)V99 COMP-3.
           03  W30-180         PIC S9(09)V99 COMP-3.

       01  W40-NADD.
           03  W40-ADD.
              05  W40-ACHAR   PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS   PIC  X(60).
               05  FILLER      PIC  X(10).
           03  W40-NAME.
               05  W40-NCHAR   PIC  X(01) OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
          03  W50-HEAD        PIC  X(20).

       01  W60-H2.
          03  W60-COMPANY     PIC  X(40).
          03  W60-DESC        PIC  X(21)     VALUE
                               "CREDITORS AUDIT TRAIL".

       01  W65-CREDITOR.
           03  W65-CFLOW       PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W65-V1          PIC S9(09)V99 COMP-3.
           03  W65-V2          PIC S9(09)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W75-KEYS.
    03  W75-S       PIC  9(02) COMP-5.
    03  W75-S1       PIC  9(02) COMP-5.
    03  W75-S2       PIC  9(02) COMP-5.
    03  W75-CNO       PIC  X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
        05  W75-CODE   PIC  X(18).
        05  W75-SELL   PIC  9(07)V99 COMP-3.
        05  W75-VSELL  PIC  9(07)V99 COMP-3.
        05  W75-CASH   PIC  9(07)V99 COMP-3.
        05  W75-VCASH  PIC  9(07)V99 COMP-3.
        05  W75-WSALE  PIC  9(07)V99 COMP-3.
        05  W75-VWSALE PIC  9(07)V99 COMP-3.

       01  W90-TRAN.
           03  W90-CODE        PIC  9(02).
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-TRNKEY      PIC  9(04)    COMP-5.
           03  W90-TS          PIC  9(02)    COMP-5.
       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC  Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
        05  W100-PRICE  PIC  X(09).
        05  FILLER      PIC  X(01).
           03  W100-V4 REDEFINES W100-V2.
        05  W100-EXT    PIC  Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
        05  W100-QNT    PIC  Z(05)9.99-.
    03  W100-S8V2       PIC  Z(07)9.99DB.
      *
      *    **        ********  **    **  **    **    ****     ******   ********
      *    **           **     ***   **  **   **    **  **   **    **  **
      *    **           **     ****  **  **  **    **    **  **        ** 
      *    **           **     ** ** **  *****     ********  **        *****
      *    **           **     **  ****  **  **    **    **  **   ***  **  
      *    **           **     **   ***  **   **   **    **  **    **  **
      *    ********  ********  **    **  **    **  **    **   ******   ********
      *
       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      * 
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       01  S08.
          03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "PURCHASE ORDERS".
          03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "C".
          03        COLUMN 13 VALUE "reate, ".
          03        COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "D".
          03        COLUMN 21 VALUE "elete Order - ".
          03        COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "E".
          03        COLUMN 36 VALUE "xit or ".
    03        COLUMN 43 FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 46 VALUE "ape".

       01  S09.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "STOCK PURCHASE ORDERS".

       01  S10.
           02  S10A.
          03  LINE  4 COLUMN  6 VALUE "Order number   ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(11) FROM WS-SPC.
          03  LINE  5 COLUMN  6 VALUE "Item code      ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(14) FROM WS-SPC.
    03        COLUMN 41 BACKGROUND-COLOR 5
     PIC  X(30) FROM WS-SPC.
          03  LINE  6 COLUMN  6 VALUE "Creditor number".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 38 BACKGROUND-COLOR 5
     PIC  X(40) FROM WS-SPC.
    03  LINE  7 COLUMN  6 VALUE "Contact        ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(30) FROM WS-SPC.
    03        COLUMN 53 VALUE "Telephone".
    03        COLUMN 63 BACKGROUND-COLOR 5
     PIC  X(14) FROM WS-SPC.
          03  LINE  8 COLUMN  6 VALUE "Quantity       ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(11) FROM WS-SPC.
          03  LINE  9 COLUMN  6 VALUE "Cost price     ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(12) FROM WS-SPC.
    03        COLUMN 36 VALUE "Rate".
    03        COLUMN 43 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPC.
    03        COLUMN 53 VALUE "Currency".
    03        COLUMN 63 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPC.
    03  LINE 10 COLUMN  6 VALUE "Rand price     ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(11) FROM WS-SPC.
          03  LINE 11 COLUMN  6 VALUE "Selling -W/Sale".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(11) FROM WS-SPC.
    03        COLUMN 36 VALUE "-Cash".
    03        COLUMN 43 BACKGROUND-COLOR 5
     PIC  X(10) FROM WS-SPC.
    03        COLUMN 54 VALUE "-Retail".
    03        COLUMN 63 BACKGROUND-COLOR 5
     PIC  X(10) FROM WS-SPC.
    03  LINE 12 COLUMN  6 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "  (Inclusive)  ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(11) FROM WS-SPC.
    03        COLUMN 43 BACKGROUND-COLOR 5
     PIC  X(10) FROM WS-SPC.
    03        COLUMN 63 BACKGROUND-COLOR 5
     PIC  X(10) FROM WS-SPC.
    03  LINE 13 COLUMN  6 VALUE "Payment method ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPC.
    03  LINE 14 COLUMN  6 VALUE "Discount %     ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(11) FROM WS-SPC.
    03  LINE 15 COLUMN  6 VALUE "Total cost     ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(11) FROM WS-SPC.
    03  LINE 16 COLUMN  6 VALUE "Remarks        ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPC.
          03  LINE 17 COLUMN  6 VALUE "Delivery date  ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(10) FROM WS-SPC.
          02  S10B.
          03  LINE 18 COLUMN  6 VALUE "Settlement     ".
          03  LINE 18 COLUMN 22 VALUE "30".
    03        COLUMN 25 BACKGROUND-COLOR 5
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 27 VALUE "60".
    03        COLUMN 30 BACKGROUND-COLOR 5
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 32 VALUE "90".
    03        COLUMN 35 BACKGROUND-COLOR 5
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 37 VALUE "120".
    03        COLUMN 41 BACKGROUND-COLOR 5
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 43 VALUE "150".
    03        COLUMN 47 BACKGROUND-COLOR 5
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 49 VALUE "180".
    03        COLUMN 53 BACKGROUND-COLOR 5
     PIC  X(01) FROM WS-SPC.
    03  LINE 19 COLUMN  6 VALUE "Picking slip   ".
    03        COLUMN 22 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPC.

       01  S11.
          03  LINE  2 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "DELETE STOCK ORDER".

       01  S12.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(14) USING W10-ITEM AUTO.

       01  S12-ITM.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC X(14) USING W10-ITEM AUTO.
    03        COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC X(03) USING W10-ITM AUTO.

       01  S12-EXT.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S12-3.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(03) USING W10-I4 AUTO.

       01  S12-4.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(04) USING W10-I4 AUTO.

       01  S12-5.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(05) USING W10-I5 AUTO.

       01  S12-6.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(06) USING W10-I6 AUTO.

       01  S12-7.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(07) USING W10-I7 AUTO.

       01  S12-8.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(08) USING W10-I8 AUTO.

       01  S12-9.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(09) USING W10-I9 AUTO.

       01  S12-10.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(10) USING W10-I10 AUTO.

       01  S12-11.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(11) USING W10-I11 AUTO.

       01  S12-12.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(12) USING W10-I12 AUTO.

       01  S12-13.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(13) USING W10-I13 AUTO.

       01  S12-14.
          03        COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(14) USING W10-I14 AUTO.

       01  S19.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W15-ACNO AUTO.

       01  S20.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(04)9.99 USING W15-UN AUTO.

       01  S21.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9 USING W15-OPREF AUTO.

       01  S22.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC 99 USING W15-OSUB AUTO.

       01  S23.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(06)9.99 USING W15-COST AUTO.

       01  S23C.
    03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(08) USING W15-CURRENCY AUTO.

       01  S23O.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.999 USING W15-OCOST AUTO.

       01  S23R.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(02)9.999 USING W15-RATE AUTO.

       01  S25.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-DEL AUTO.

       01  S31.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99 USING W15-TCOST AUTO.

       01  S35.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9.99 USING W15-DISC1 AUTO.

       01  S49.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(08) USING W15-PSLIP AUTO.

       01  S50.
    03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(30) USING W15-CONTACT AUTO.

       01  S50A.
    03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(14) USING W15-TEL AUTO.

       01  S51.
           02  S51A.
    03  LINE 11 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W15-UWSALE AUTO.
           02  S51B.
    03  LINE 11 COLUMN 43 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W15-UCASH AUTO.
           02  S51C.
    03  LINE 11 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W15-USELL AUTO.
           02  S51D.
    03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W10-VWSALE.
    03        COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W10-VCASH.
    03        COLUMN 63 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W10-VSELL.

       COPY "CRDLUP.CRT".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    ".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-START-UP  SECTION.
       AA000.
          IF LS0-CRLEV < 1
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      MOVE 0   TO WS-ADVANCE.
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
            PERFORM GA000.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
            CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50CS.LUP".

      *
      *    ****    C A L L   S T O C K   C R E A T I O N
      *
       AA100.
      PERFORM SAVE-SCREEN.
            CALL "STP\STP010"
     USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
             CANCEL "STP010".
      OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE ZERO           TO WS-RECOVER.
      PERFORM RESTORE-SCREEN.

       AA199.
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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        OPT-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           ERASE SCREEN FROM SPECIFIED LOCATION            ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CRT-START as starting line and increases and CRT-END ³
      *    ³ as the ending line. The screen is cleared with Column 1   ³
      *    ³ and 80 containing "³" and columns 2 to 79 containing      ³
      *    ³ spaces.                                                   ³
      *    ³                                                           ³
      *    ³  eg.                                                      ³
      *    ³   MOVE 8         TO CRT-START.                            ³
      *    ³   MOVE 28        TO CRT-END.                              ³
      *    ³   PERFORM ERASE-SCREEN.                                   ³
      *    ³                                                           ³
      *    ³ To clear the entire screen and Display a new screen with  ³
      *    ³ headings (program/date/time/company):                     ³
      *    ³                                                           ³
      *    ³    CRT-PROGRAM (calling program)                          ³
      *    ³    CRT-HEADING (Screen heading)                           ³
      *    ³    CRT-COMPANY (Company name)                             ³
      *    ³    CRT-TERMINAL (consists of two fields:                  ³
      *    ³                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ³
      *    ³    These fields should be populated at the start of the   ³
      *    ³    program.                                               ³
      *    ³    PERFORM HEADING-AND-CLEAR-SCREEN.                      ³
      *    ³                                                           ³
      *    ³ To change the screen heading:                             ³
      *    ³    MOVE "The new heading" TO CRT-HEADING.                 ³
      *    ³    PERFORM CHANGE-HEADING.                                ³
      *    ³                                                           ³
      *    ³ To change the time on the screen:                         ³
      *    ³    PERFORM CHANGE-TIME                                    ³
      *    ³                                                           ³
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "PRINTUTL.AS4".

       COPY "CREDITOR.TRN".

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " Item code/ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " Alt code/ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " Description/ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " X/Ref Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12
       " Create"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA305.
             EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "PORDER.RD".

       COPY "SHARED.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   & W R I T E   F I L E S R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-WRITE   SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "PORDER.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

      /
       AD000         SECTION.
       AD00.
             MOVE ZERO           TO WS-ERROR.
           IF W15-DD < 01 OR > 31
               GO TO AD15.
           IF W15-MM < 01 OR > 12
               GO TO AD15.
           IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
               GO TO AD999.
           IF W15-MM = 02 AND W15-DD > 28
               GO TO AD05.
           IF W15-DD > 30
               GO TO AD15.
             GO TO AD999.
       AD05.
           IF W15-DD > 29
               GO TO AD15.
            DIVIDE W15-CCYY BY 4
     GIVING W20-RESULT.
           IF W20-DECIMAL > ZERO
               GO TO AD15.
             GO TO AD999.
       AD15.
             MOVE 1              TO WS-ERROR.
       AD999.
             EXIT.

       COPY "CREDIT.ILU".

       COPY "APAC.HLP".

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
             MOVE 01             TO REC-FILE.
             MOVE WS-PARKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD1    TO REC-PARAM.
             GO TO AY50.

       AY05.
             MOVE 05             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
              PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-RECORD1    TO REC-STOCKF.
             GO TO AY50.

       AY07.
             MOVE 07 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
             MOVE P-REC          TO REC-CREDIT.
             GO TO AY50.
       AY10.
             MOVE 10 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
        PERFORM READ-PORDER THRU READ-PORDER-EXIT.
             MOVE ORD-RECORD1 TO REC-PORDER.
             GO TO AY50.

       AY37.
            MOVE 37   TO REC-FILE.
            MOVE WS-SHARED  TO REC-KEY.
            MOVE SHR-RECORD  TO REC-NETWORK.
             GO TO AY50.

       AY38.
             MOVE 38             TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
      MOVE ZERO   TO REC-TYPE.
            PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-RECORD     TO REC-DEPART.
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
                        WITH FOREGROUND-COLOR 14
                        WS-STATUS WITH FOREGROUND-COLOR 15
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
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "CS"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 2              TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
      MOVE NET-CREDITOR  TO W70-CREDCON.
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
           IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
       OR "ST" OR "DS"
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
        FOREGROUND-COLOR 14 BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
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
    IF WS-STOCK = "Y"
        MOVE NET-RECORD  TO SHR-RECORD
              PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
    ELSE
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            MOVE 2   TO WS-NETKEY.
             MOVE W70-CREDCON    TO NET-CREDITOR.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
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

       COPY "CA000.STK".

       COPY "CA100.STK".

      /
       CA155-GET-CREDITOR      SECTION.
       CA155-ACCOUNT.
             MOVE W15-ACNO       TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               MOVE 1 TO WS-ERROR
           ELSE
           IF P-NUMBER NOT = W15-ACNO
               MOVE 2 TO WS-ERROR
           ELSE
               MOVE 0 TO WS-ERROR.
       CA160-EXIT.
             EXIT.

       COPY "CA200.STK".

       COPY "CA500.STK".

      /
      *    R O U N D I N G   O F   S E L L I N G   P R I C E S
      *
      *    This routine will round the selling price as follows:
      *
      *    If the rounding factor is less than 50 cents or greater
      *    than R1.00 but less than five rand, then the selling price
      *    (.CC or R.CC portion) will be rounded to a multiple of the
      *    rounding factor.
      *
      *    If the rounding factor is R1.00, then if the selling price
      *    is greater than 79 cents it will be rounded to the next rand.
      *
      *    If the rounding factor is greater than 49 cents and less than
      *    R1.00 or greater than R4.99 then the selling price (.CC or
      *    R.CC portion) will be changed to the rounding value. If the
      *    rounding portion of the selling price is greater than the
      *    factor then the system will amend these figures and change
      *    the adjoining value.
      *
       CB100         SECTION.
       CB101.
      *
      *    ROUND TO THE NEXT RAND
      *
           IF W05-ROUND-VAL = 1.00
        IF W15-RSC = ZERO
                   GO TO CB199
               ELSE
      *
      *    SELLING PRICE LESS THAN ONE RAND
      *
        IF W15-RSELL < 1.00
      *
      *    SELLING PRICE IS MORE THAN 79 CENTS - CHANGE TO R1.00
      *
     IF W15-RSC > 79
         MOVE 1.00 TO W15-RSELL
                       GO TO CB199
                   ELSE
      *
      *    IF THE SELLING PRICE IS LESS THAN 80 CENTS DO NOT ROUND UP
      *
                       GO TO CB199
      *
      *    SELLING PRICE MORE THAN ONE RAND
      *
               ELSE
      *
      *    CENTS PORTION OF PRICE IS LESS THAN TEN CENTS - DROP CENTS
      *
     IF W15-RSC < 10
         MOVE ZERO TO W15-RSC
                       GO TO CB199
                   ELSE
      *
      *    ROUND UP TO NEXT R1.00
      *
         MOVE ZERO TO W15-RSC
         ADD 1  TO W15-RSR
                       GO TO CB199.
      *
      *    ROUNDING CENTS ONLY
      *
           IF W05-ROUND-VAL < 1.00
      *
      *    ROUNDING VALUE AND CENTS PORTION ARE EQUAL - EXIT
      *
        IF W15-RSC = W05-CENTS
                   GO TO CB199
               ELSE
      *
      *    ROUNDING VALUE IS 50 CENTS OR MORE
      *
               IF W05-ROUND-VAL > .50
      *
      *    IF CENTS PORTION TO BE ROUNDED IS LESS THAN ROUNDING VALUE
      *
     IF W15-RSC < W05-CENTS
                       MOVE W05-CENTS
     TO W15-RSC
                       GO TO CB199
      *
      *    IF CENTS PORTION TO BE ROUNDED IS MORE THAN ROUNDING VALUE
      *
                   ELSE
                       MOVE W05-CENTS
     TO W15-RSC
         ADD 1  TO W15-RSR
                       GO TO CB199
      *
      *    ROUNDING VALUE IS LESS THAN 50 CENTS
      *
               ELSE
                   GO TO CB120.
      *
      *    IF ROUNDING VALUE EQUALS THE R.CC OF VALUE TO BE ROUNDED - EXIT
      *
    IF W15-RSRCC = W05-ROUND-VAL
               GO TO CB199.
      *
      *    SELLING PRICE IS NOT MORE THAN HALF THE ROUNDING FACTOR - EXIT
      *
    IF W15-RSELL NOT > W05-HLF-VAL
               GO TO CB199.
           IF W05-ROUND-VAL > 5.00
               GO TO CB110.
      *
      *    ROUNDING FACTOR IS LESS THAN FIVE RAND
      *
      *    R.CC OF VALUE TO BE ROUNDED IS LESS THAN ROUNDING FACTOR
      *
    IF W15-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL
     TO W15-RSRCC
               GO TO CB199.
             MOVE W05-ROUND-VAL  TO W05-CALC-VAL.
       CB105.
    IF W15-RSRCC = W05-CALC-VAL
               GO TO CB199.
    IF W15-RSRCC < W05-CALC-VAL
        MOVE W05-CALC-VAL TO W15-RSRCC
               GO TO CB199.
             ADD W05-ROUND-VAL   TO W05-CALC-VAL.
           IF W05-CALC-VAL < 10.00
               GO TO CB105.
      ADD 1   TO W15-RSR8.
      MOVE W05-RVRCC  TO W15-RSRCC.
             GO TO CB199.
      *
      *    ROUNDING FACTOR GREATER THAN R5.00
      *
      *    SELLING PRICE IS NOT MORE THAN 75% OF ROUNDING FACTOR - EXIT
      *
       CB110.
    IF W15-RSELL NOT > W05-TQTR-VAL
               GO TO CB199.
       CB115.
    IF W15-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL
     TO W15-RSRCC
               GO TO CB199.
      ADD 1   TO W15-RSR8.
      MOVE W05-ROUND-VAL  TO W15-RSRCC.
             GO TO CB199.
      *
      *    ROUND UP VALUES WHERE ROUNDING FACTOR IS LESS THAN 50 CENTS
      *
       CB120.
    IF W05-CENTS = 2 OR 4 OR 5 OR 10 OR 20 OR 25
        MOVE ZERO  TO WS-SUB
    ELSE
        MOVE W05-CENTS  TO WS-SUB.
       CB125.
    IF W15-RSC = WS-SUB
               GO TO CB199.
    IF W15-RSC < WS-SUB
        MOVE WS-SUB  TO W15-RSC
               GO TO CB199.
             ADD W05-CENTS       TO WS-SUB.
           IF WS-SUB < 100
               GO TO CB125.
      ADD 1   TO W15-RSR.
             SUBTRACT 100 FROM WS-SUB
     GIVING W15-RSC.
       CB199.
             EXIT.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   P R I C E
      *              F R O M   T H E   I N C L U S I V E   P R I C E
      *
       CB200         SECTION.
       CB201.
           IF STK-TAX NOT = ZERO
        COMPUTE W15-USELL ROUNDED= (W10-VSELL /
       (100.00000 + W05-RATE)) * 100.00
        COMPUTE W15-UWSALE ROUNDED = (W10-VWSALE /
        (100.00000 + W05-RATE)) * 100.00
        COMPUTE W15-UCASH ROUNDED = (W10-VCASH /
       (100.00000 + W05-RATE)) * 100.00
           ELSE
        MOVE W10-VWSALE  TO W15-UWSALE
        MOVE W10-VCASH  TO W15-UCASH
        MOVE W10-VSELL  TO W15-USELL.
    IF STK-TAX NOT = ZERO
        COMPUTE W15-CHECK
     = W15-USELL + (W15-USELL * W05-RTE)
        IF W15-CHECK > W10-VSELL
     SUBTRACT 0.01 FROM W15-USELL
        ELSE
        IF W15-CHECK < W10-VSELL
     ADD 0.01  TO W15-USELL
        END-IF
        COMPUTE W15-CHECK
     = W15-UCASH + (W15-UCASH * W05-RTE)
        IF W15-CHECK > W10-VCASH
     SUBTRACT 0.01 FROM W15-UCASH
        ELSE
        IF W15-CHECK < W10-VCASH
     ADD 0.01  TO W15-UCASH
        END-IF
        COMPUTE W15-CHECK
     = W15-UWSALE + (W15-UWSALE * W05-RTE)
        IF W15-CHECK > W10-VWSALE
     SUBTRACT 0.01 FROM W15-UWSALE
        ELSE
        IF W15-CHECK < W10-VWSALE
     ADD 0.01  TO W15-UWSALE.
       CB299.
             EXIT.

      /
       GA000      SECTION 50.
       GA00A.
          IF LS0-STP = 2
               GO TO GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "Stock not installed - 'ENTER'" AT 1212
                      WITH FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 1242
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
             MOVE ZERO           TO WS-PAGE.
             GO TO GA999.

       GA00B.
      *
      *    ****    Print the audit trail HEADING
      *
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
             MOVE "Page:"        TO REP-PG.
             MOVE W60-COMPANY    TO REP-COMP.
             MOVE "Date:"        TO REP-DAT.
            ADD 1   TO WS-PAGE.
             MOVE W05-DATE       TO REP-DT.
             MOVE WS-PAGE        TO REP-P-N.
           IF WS-DEL = "D"
               MOVE "STOCK PURCHASE ORDERS DELETED"
                                 TO REP-REPORT
           ELSE
               MOVE "STOCK PURCHASE ORDERS"
                                 TO REP-REPORT.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE "ORDER.NO"     TO OR-H1.
             MOVE "ITEM CODE"    TO OR-H2.
             MOVE "QUANT"        TO OR-H3.
             MOVE "CURRENCY"     TO OR-H4.
             MOVE "UNIT COST"    TO OR-H5.
             MOVE "PAYMENT"      TO OR-H6.
             MOVE "CREDITOR"     TO OR-H7.
             MOVE "FAX NO"       TO OR-H8.
             MOVE "DELIVERY"     TO OR-H9.
             MOVE "PCK-SLIP"     TO OR-H10.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE "/DESCRIPTION" TO OR-H2A.
             MOVE " /RATE"       TO OR-H4.
             MOVE "  /RAND"      TO OR-H5.
             MOVE " /DISC"       TO OR-H6.
             MOVE "/NAME/CONTACT"
                                 TO OR-H7.
             MOVE "/TELEPHONE"   TO OR-H8.
             MOVE "/REMARKS"     TO OR-H10.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S08.

       GA005.
            ACCEPT WS-OPTION AT 0650
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO GA999
   WHEN OTHER
       CALL X"E5"
       GO TO GA005
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "E"
               GO TO GA999.
          IF WS-OPTION = "C"
               MOVE ZERO         TO W15-ORD
               MOVE "O"          TO WS-DEL
               PERFORM GA10
        PERFORM GB000-TRAN
              GO TO GA00.
          IF WS-OPTION = "D"
               MOVE ZERO         TO W15-ORD
               MOVE "D"          TO WS-DEL
               PERFORM GA10
        PERFORM GD000-TRAN
              GO TO GA00.
             GO TO GA005.

       GA10.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 3   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
      MOVE SPACES  TO R-DET.
             PERFORM GA00B.

       GA15.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.

       GA999.
             EXIT.
      /
       GB000-TRAN    SECTION 51.
       GB00.
            MOVE ZERO   TO W15-30 W15-120
                                    W15-60 W15-150 W15-DEL
                                    W15-90 W15-180.
      MOVE SPACES  TO W15-CONTACT W15-TEL P-NUMBER
        W15-FAX W15-ACNO.
             MOVE SPACES         TO W15-CURRENCY.
             MOVE 1              TO W15-RATE.
             MOVE "Account"      TO W15-PMT.
             MOVE "Deliver"      TO W15-RMKS.

       GB000.
             PERFORM ERASE-SCREEN.
             DISPLAY S09.
             DISPLAY S10.
    IF WS-EXT-STK = "Y"
        DISPLAY "    " AT 0536
   WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
    ELSE
    IF WS-USE-ITM = "Y"
        DISPLAY "/" AT 0536
   WITH FOREGROUND-COLOR 6 HIGHLIGHT
        BACKGROUND-COLOR 5
         "   " WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
            MOVE SPACES  TO W10-DESC.
             MOVE ZERO           TO W15-UN W15-SELL W15-ICOST
                                           W15-COST.
            MOVE SPACES  TO W10-EXT-ITEM W15-PSLIP.
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

       GB02.
      MOVE "'E'xisting,'N'ew order number 'ESC' to exit  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       GB03.
            PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-ORD.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM GA15
       GO TO GB999
   WHEN OTHER
       CALL X"E5"
       GO TO GB03
        END-EVALUATE.
          IF NOT (WS-ORD = "E" OR "N")
              GO TO GB02.
    IF WS-ORD = "N"
               PERFORM GB00
               GO TO GB15.

       GB05.
             ACCEPT S21 AT 0422.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-OPREF
   WHEN OTHER
       CALL X"E5"
       GO TO GB05
        END-EVALUATE.
    IF W15-OPREF = ZERO
               PERFORM GA15
              GO TO GB999.
             MOVE W15-OPREF      TO ORD-ORD.
             MOVE ZERO           TO ORD-LINE.
             PERFORM START-AT-ORDER-NO THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               GO TO GB08.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF W15-OPREF NOT = ORD-ORD
               GO TO GB08.
             GO TO GB010.

       GB08.
      MOVE "Order number invalid"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
             GO TO GB02.

       GB010.
             MOVE ORD-LINE       TO W15-OSUB.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = ZERO
               IF W15-OPREF = ORD-ORD
                   GO TO GB010.
             ADD 1               TO W15-OSUB.
            DISPLAY "/" AT 0430
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
             DISPLAY S22 AT 0431.
      MOVE ORD-CRED  TO W15-ACNO.
             GO TO GB015.

       GB15.
             MOVE 2              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE NET-CREDITOR   TO W70-CREDCON.
           IF W70-PORDER = 99999999
               MOVE 1            TO W70-PORDER
           ELSE
               ADD 1             TO W70-PORDER
               MOVE W70-PORDER   TO W15-OPREF
               MOVE ZERO         TO W15-OSUB.
             DISPLAY S21 AT 0422.
            DISPLAY "/" AT 0430
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
             DISPLAY S22 AT 0431.

       GB015.
             PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF WS-EXT-STK = "Y"
        ACCEPT S12-EXT AT LINE 5
    ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S12-ITM AT LINE 5
        ELSE
           ACCEPT S12 AT LINE 5
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S12-3 AT LINE 5
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S12-4 AT LINE 5
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S12-5 AT LINE 5
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S12-6 AT LINE 5
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S12-7 AT LINE 5
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S12-8 AT LINE 5
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S12-9 AT LINE 5
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S12-10 AT LINE 5
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S12-11 AT LINE 5
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S12-12 AT LINE 5
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S12-13 AT LINE 5
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S12-14 AT LINE 5.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO GB15E
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F8-KEY
       PERFORM AA100
       GO TO GB015
   WHEN OTHER
       CALL X"E5"
       GO TO GB015
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S12-EXT AT LINE 5
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S12-ITM AT LINE 5
     ELSE
               DISPLAY S12 AT LINE 5.
              IF W10-EXT-ITEM = SPACES
           GO TO GB015.
       GB15E.
      PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO GB000.
       GB015A.
             PERFORM CA000.
           IF WS-F-ERROR NOT = ZERO
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S12-EXT AT LINE 5
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S12-ITM AT LINE 5
     ELSE
               DISPLAY S12 AT LINE 5
     END-IF
        END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO GB015
               ELSE
     GO TO GB015A.
            MOVE STK-DESC  TO W10-DESC.
            DISPLAY W10-DESC AT 0541
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO GB015.

            MOVE STK-COST  TO W15-COST.
           IF W15-OSUB NOT = ZERO
               IF W15-ACNO NOT = SPACES
                   PERFORM CA155-GET-CREDITOR
                   DISPLAY S19 AT 0622
     DISPLAY P-NAME AT 0637
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
    BACKGROUND-COLOR 5
                   MOVE ORD-CONTACT
                                 TO W15-CONTACT
                   MOVE ORD-TEL  TO W15-TEL
                   MOVE ORD-FAX  TO W15-FAX
                   MOVE ORD-TERMS
                                 TO W15-SETT
                   GO TO GB16A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       GB16.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to do Creditor Lookup "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            ACCEPT S19 AT 0622.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W15-ACNO
       GO TO GB000
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50C THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO GB16
        END-EVALUATE
              DISPLAY S19 AT 0622
              IF W15-ACNO = SPACES
           GO TO GB16.
      PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO GB000.
            PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
              MOVE WS-ER1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO GB16.
      DISPLAY P-NAME AT 0637
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MOVE P-TERMS        TO W15-SETT.
             MOVE P-CONTACT      TO W15-CONTACT.
             MOVE P-TEL          TO W15-TEL.
             MOVE P-FAX          TO W15-FAX.
       GB16A.
      DISPLAY "Press " AT 5001
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       GB16AA.
             ACCEPT S50 AT 0722.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16AA
        END-EVALUATE.
       GB16AB.
             ACCEPT S50A AT 0763.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16AB
        END-EVALUATE.
       GB16B.
            ACCEPT S20 AT 0825.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16B
        END-EVALUATE.
          IF W15-UN = ZERO
               GO TO GB16B.
             MOVE STK-COST       TO W15-OCOST.
       GB16C.
      ACCEPT S23O AT 0922.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16C
        END-EVALUATE.
          IF W15-OCOST = ZERO
               GO TO GB16C.
       GB16CA.
      ACCEPT S23R AT 0944.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16CA
        END-EVALUATE.
          IF W15-RATE = ZERO
               GO TO GB16CA.
       GB16CB.
           IF W15-RATE = 1.00
        MOVE W15-OCOST  TO W15-COST
               MOVE "Rand"       TO W15-CURRENCY
        DISPLAY S23C AT 0963
               GO TO GB16CD.
       GB16CC.
      ACCEPT S23C AT 0963.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16CC
        END-EVALUATE.
    IF W15-CURRENCY = SPACES
               GO TO GB16CC.
      COMPUTE W15-COST ROUNDED = W15-OCOST * W15-RATE.
       GB16CD.
            DISPLAY S23 AT 1023.
            COMPUTE W15-USELL ROUNDED = (W15-COST +
         (W15-COST * STK-MARKUP / 100.00000)).
            COMPUTE W15-UWSALE ROUNDED = (W15-COST +
         (W15-COST * STK-WMARKUP / 100.00000)).
            COMPUTE W15-UCASH ROUNDED = (W15-COST +
         (W15-COST * STK-CMARKUP / 100.00000)).
           IF W05-ROUND = "E"
               MOVE W15-USELL    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W15-USELL
               MOVE W15-UWSALE   TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W15-UWSALE
               MOVE W15-UCASH    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W15-UCASH.
           IF STK-TAX NOT = ZERO
        MOVE W05-VAT (1)  TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W15-USELL +
                                           (W15-USELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W15-UWSALE +
                                            (W15-UWSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W15-UCASH +
                                           (W15-UCASH * W05-RTE)
           ELSE
               MOVE W15-UWSALE   TO W10-VWSALE
               MOVE W15-UCASH    TO W10-VCASH
               MOVE W15-USELL    TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W10-VCASH
               PERFORM CB200.
             MOVE W15-USELL      TO W15-SELL.
             DISPLAY S51.
       GB16D.
             ACCEPT W15-PMT AT 1322
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16D
        END-EVALUATE.
       GB16E.
      ACCEPT S35 AT 1428.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16E
        END-EVALUATE.
      COMPUTE W15-TCOST ROUNDED = W15-COST * W15-UN.
             COMPUTE W15-TCOST = W15-TCOST -
                                (W15-TCOST * W15-DISC1 /100).
      DISPLAY S31 AT 1522.
       GB16F.
             ACCEPT W15-RMKS AT 1622
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16F
        END-EVALUATE.
       GB16G.
             ACCEPT S25 AT 1722.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16G
        END-EVALUATE.
      MOVE W15-DEL  TO W15-DATE.
             PERFORM AD000.
           IF WS-ERROR NOT = ZERO
               GO TO GB16G.

       GB16H.
            ACCEPT W15-30 AT 1825
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16H
        END-EVALUATE.
            ACCEPT W15-60 AT 1830
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16H
        END-EVALUATE.
            ACCEPT W15-90 AT 1835
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16H
        END-EVALUATE.
            ACCEPT W15-120 AT 1841
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16H
        END-EVALUATE.
            ACCEPT W15-150 AT 1847
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16H
        END-EVALUATE.
            ACCEPT W15-180 AT 1853
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GB000
   WHEN OTHER
       CALL X"E5"
       GO TO GB16H
        END-EVALUATE.
            PERFORM CLEAR-L50.
             ACCEPT S49 AT 1922.
       GB017.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               IF WS-ORD = "N"
                   GO TO GB015
               ELSE
                   GO TO GB05.
       GB050.
             MOVE ZERO           TO WS-S.
           IF W15-30 > ZERO
               ADD 1             TO WS-S.
           IF W15-60 > ZERO
               ADD 1             TO WS-S
           IF W15-90 > ZERO
               ADD 1             TO WS-S.
           IF W15-120 > ZERO
               ADD 1             TO WS-S.
           IF W15-150 > ZERO
               ADD 1             TO WS-S.
           IF W15-180 > ZERO
               ADD 1             TO WS-S.
       GB065.
      *
      *    ****    Start Recovery Logging.
      *
            PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY07 THRU AY59.
             PERFORM AY05 THRU AY59.
           IF WS-ORD = "N"
               IF W70-PORDER = 99999999
                   MOVE 1        TO W70-PORDER
               ELSE
                   ADD 1         TO W70-PORDER
                   MOVE W70-PORDER
                                 TO W15-OPREF
                   MOVE ZERO     TO W15-OSUB.
             DISPLAY S21 AT 0422.
            DISPLAY "/" AT 0430
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
             DISPLAY S22 AT 0431.
             INITIALIZE ORD-RECORD1.
             MOVE W05-DATE-YMD   TO ORD-DATE STK-DTE.
             MOVE W15-ACNO       TO ORD-CRED.
      MOVE W10-EXT-ITEM  TO ORD-EXT-ITEM.
            MOVE W15-OPREF  TO ORD-ORD.
             MOVE W15-OSUB       TO ORD-LINE.
            MOVE W15-UN  TO ORD-QUANT.
            MOVE W15-COST  TO ORD-UCOST.
             MOVE W15-OCOST      TO ORD-OCOST.
             MOVE W15-RATE       TO ORD-DRATE.
             MOVE W15-CURRENCY   TO ORD-CURRENCY.
             MOVE W15-TCOST      TO ORD-COST.
            MOVE W15-SETT  TO ORD-TERMS.
      *
      *    ****    S W I T C H  D A T E   T O CCYYMMDD
      *
      MOVE W15-DEL-DD  TO W21-DD.
      MOVE W15-DEL-MM  TO W21-MM.
      MOVE W15-DEL-CC  TO W21-CC.
      MOVE W15-DEL-YY  TO W21-YY.
      MOVE W21-DTE  TO ORD-DELIV.
             MOVE W15-SELL       TO ORD-SELL.
             MOVE W15-PSLIP      TO ORD-PSLIP.
             MOVE W15-PMT        TO ORD-PMT.
             MOVE W15-RMKS       TO ORD-INST.
             MOVE W15-DISC1      TO ORD-DISC.
             MOVE W15-CONTACT    TO ORD-CONTACT.
             MOVE W15-TEL        TO ORD-TEL.
             MOVE W15-FAX        TO ORD-FAX.
             ADD W15-UN          TO STK-ORD.
             PERFORM WRITE-PORDER THRU WRITE-PORDER-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY10 THRU AY59.

       GB070.
             MOVE W15-DEL-MM     TO WS-S2.
             MOVE ZERO           TO W25-CFLOW (1) W25-CFLOW (2)
                                    W25-CFLOW (3) W25-CFLOW (4)
                                    W25-CFLOW (5) W25-CFLOW (6)
                                    W25-V1        W25-V2.
             MOVE 1              TO WS-S3.
           IF WS-S = 1
               MOVE W15-TCOST    TO W25-V2
               GO TO GB075.
             DIVIDE W15-TCOST BY WS-S
                                 GIVING W20-RESULT ROUNDED.
             MOVE W20-RESULT     TO W25-V1.
             SUBTRACT 1          FROM WS-S.
             MULTIPLY WS-S       BY W20-RESULT.
             SUBTRACT W20-RESULT FROM W15-TCOST
                                 GIVING W25-V2.
             ADD 1               TO WS-S.
       GB075.
           IF W15-MONTH (WS-S3) = ZERO
               ADD 1             TO WS-S3
               GO TO GB075.
           IF WS-S > 1
               MOVE W25-V1       TO W25-CFLOW (WS-S3)
               SUBTRACT 1        FROM WS-S
               ADD 1             TO WS-S3
               GO TO GB075.
             MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE 1              TO WS-S2.
       GB080.
           IF W25-CFLOW (WS-S2) = ZERO
               GO TO GB085.
             ADD W15-DEL-MM WS-S2
                                 GIVING WS-S3.
           IF WS-S3 > 12
               SUBTRACT 12       FROM WS-S3.
       GB085.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO GB080.
       GB090.
             MOVE W15-ORD        TO OR-ORD.
    IF WS-EXT-STK = "Y"
        MOVE W10-EXT-ITEM TO OR-EXT-ITEM
    ELSE
    IF WS-USE-ITM = "Y"
        MOVE W10-ITEM  TO OR-ITEM
        MOVE "/"   TO OR-SL
        MOVE W10-ITM  TO OR-ITM
    ELSE
              MOVE W10-ITEM  TO OR-ITEM.
            MOVE W15-UN  TO OR-QUANT.
             MOVE W15-CURRENCY   TO OR-CURRENCY.
             MOVE W15-OCOST      TO OR-COST.
             MOVE W15-PMT        TO OR-PMT.
             MOVE W15-ACNO       TO OR-ACNO.
             MOVE W15-FAX        TO OR-TEL.
             MOVE W15-DEL        TO OR-DEL.
             MOVE W15-PSLIP      TO OR-PSLIP.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W10-DESC  TO OR-DESC.
             MOVE W15-RATE       TO OR-RATE.
            MOVE W15-COST  TO OR-COST.
             MOVE W15-DISC1      TO OR-DISC.
             MOVE P-NAME         TO OR-NAME.
             MOVE W15-RMKS       TO OR-RMKS.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE W15-CONTACT    TO OR-CONTACT.
             MOVE W15-TEL        TO OR-TEL.

       GB100.
          IF W02-LINAGE < (W02-PRN-LENGTH - 2)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM GA00B.
             MOVE SPACES         TO R-DET.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
      *
      *    ****    End Recovery Logging.
      *
      CLOSE CREDIT.
      OPEN I-O CREDIT.
            PERFORM AY70 THRU AY999.
       GB105.
      MOVE "'C'ontinue or 'E'xit  [ ]"
     TO WS-ERR-MES.
             MOVE "C"            TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "C" OR "E")
               GO TO GB105.
    IF WS-OPTION = "E"
               PERFORM GA15
              GO TO GB999.
             GO TO GB000.
       GB999.
      EXIT.
      /
       GD000-TRAN    SECTION 52.
       GD000.
             PERFORM ERASE-SCREEN.
             DISPLAY S11.
             DISPLAY S10.
             MOVE ZERO           TO W15-ORD.
       GD005.
             ACCEPT S21 AT 0422.
           IF W15-OPREF = ZERO
              GO TO GD999.
            DISPLAY "/" AT 0430
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
             ACCEPT S22 AT 0431.
      MOVE W15-ORD  TO ORD-KEY.
             PERFORM READ-PORDER THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
              MOVE WS-ER1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO GD005.
             MOVE ORD-CRED       TO W15-ACNO.
            MOVE ORD-EXT-ITEM  TO W10-EXT-ITEM.
             PERFORM CA155-GET-CREDITOR.
             PERFORM CA000.
    IF NOT (WS-F-ERROR = ZERO)
        MOVE "Item not on file"
     TO W10-DESC
        MOVE 1   TO WS-IND
    ELSE
        MOVE ZERO  TO WS-IND
              MOVE STK-DESC  TO W10-DESC.
            MOVE ORD-QUANT  TO W15-UN.
      *
      *    ****    S W I T C H  D A T E   T O DDMMCCYY
      *
      MOVE ORD-DELIV  TO W21-DTE.
      MOVE W21-DD  TO W15-DEL-DD.
      MOVE W21-MM  TO W15-DEL-MM.
      MOVE W21-CC  TO W15-DEL-CC.
      MOVE W21-YY  TO W15-DEL-YY.
            MOVE ORD-UCOST  TO W15-COST.
             MOVE ORD-SELL       TO W15-SELL.
             MOVE ORD-OCOST      TO W15-OCOST.
             MOVE ORD-COST       TO W15-TCOST.
             MOVE ORD-FAX        TO W15-FAX.
             MOVE ORD-TEL        TO W15-TEL.
             MOVE ORD-DRATE      TO W15-RATE.
             MOVE ORD-CONTACT    TO W15-CONTACT.
             MOVE ORD-PMT        TO W15-PMT.
             MOVE ORD-INST       TO W15-RMKS.
             MOVE ORD-DISC       TO W15-DISC1.
             MOVE ORD-PSLIP      TO W15-PSLIP.
             MOVE ORD-CURRENCY   TO W15-CURRENCY.
             MOVE ORD-TERMS      TO W15-SETT.
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S12-EXT AT LINE 5
    ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S12-ITM AT LINE 5
        ELSE
           DISPLAY S12 AT LINE 5.
            DISPLAY W10-DESC AT 0541
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             DISPLAY S19 AT 0622.
      DISPLAY P-NAME AT 0637
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             DISPLAY S50 AT 0722.
             DISPLAY S50A AT 0763.
            DISPLAY S20 AT 0825.
            DISPLAY S23O AT 0922.
            DISPLAY S23R AT 0944.
            DISPLAY S23C AT 0963.
             PERFORM GB16CD.
      DISPLAY W15-PMT AT 1322
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY S35 AT 1428.
            DISPLAY S31 AT 1522.
      DISPLAY W15-RMKS AT 1622
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
             DISPLAY S25 AT 1722.
            DISPLAY W15-30 AT 1825
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W15-60 AT 1830
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W15-90 AT 1835
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W15-120 AT 1841
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W15-150 AT 1847
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W15-180 AT 1853
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
             DISPLAY S49 AT 1922.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GD000.
      *
      *    ****    Start Recovery Logging.
      *
             MOVE "N"            TO WS-SKIP.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE 2              TO WS-ACTION.
             PERFORM AY10 THRU AY59.
           IF ORD-TERMS NOT = ZERO
               GO TO GD08.
             MOVE P-TERMS        TO ORD-TERMS.

       GD08.
    IF WS-IND = ZERO
              IF STK-ORD NOT > ORD-QUANT
           MOVE ZERO  TO STK-ORD STK-DTE STK-DISC
              ELSE
           SUBTRACT ORD-QUANT
     FROM STK-ORD.
      *
      *    ****    S W I T C H  D A T E   T O DDMMCCYY
      *
      MOVE ORD-DELIV  TO W21-DTE.
      MOVE W21-DD  TO W15-DEL-DD.
      MOVE W21-MM  TO W15-DEL-MM.
      MOVE W21-CC  TO W15-DEL-CC.
      MOVE W21-YY  TO W15-DEL-YY.
            MOVE W15-DEL-MM  TO WS-S.
             MOVE ORD-TERMS      TO W15-SETT.
             MOVE 0              TO WS-S.
             MOVE 1              TO WS-S2.

       GD010.
             MOVE ZERO           TO W25-CFLOW (WS-S2).
           IF W15-MONTH (WS-S2) > 0
               ADD 1             TO WS-S.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO GD010.
             MOVE ZERO           TO W25-V1 W25-V2.
             MOVE 1              TO WS-S3.
           IF WS-S = 1 OR 0
               MOVE ORD-COST     TO W25-V2
               GO TO GD015.
             DIVIDE ORD-COST BY WS-S
                                 GIVING W25-V1 ROUNDED.
             SUBTRACT 1          FROM WS-S.
             COMPUTE W25-V2 = ORD-COST - (W25-V1 * WS-S).
             ADD 1               TO WS-S.

       GD015.
           IF W15-MONTH (WS-S3) = ZERO
               ADD 1             TO WS-S3
               GO TO GD015.
           IF WS-S > 1
               MOVE W25-V1       TO W25-CFLOW (WS-S3)
               SUBTRACT 1        FROM WS-S
               ADD 1             TO WS-S3
               GO TO GD015.
             MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE 1              TO WS-S2.

       GD020.
           IF W25-CFLOW (WS-S2) = ZERO
               GO TO GD025.
             ADD W15-DEL-MM WS-S2
                                 GIVING WS-S3.
           IF WS-S3 > 12
               SUBTRACT 12       FROM WS-S3.
       GD025.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO GD020.
             PERFORM GB090.
             MOVE "DELETION"     TO OR-RMKS.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM GA00B.
             MOVE SPACES         TO R-DET.
             PERFORM DELETE-PORDER-REC THRU WRITE-PORDER-EXIT.
    IF WS-IND = ZERO
              PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    End Recovery Logging.
      *
      CLOSE CREDIT.
      OPEN I-O CREDIT.
            PERFORM AY70 THRU AY999.
             GO TO GD000.
       GD999.
      EXIT.
      /    *************************************************************
      *   I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 90.
       ZA000-START.
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

             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-STOCK = "A" OR "N")
               MOVE "A"              TO PAR-STOCK.
               MOVE PAR-STOCK        TO W10-STCK.
           IF PAR-STOCK = "N"
               MOVE PAR-SLNGTH       TO W10-SLNGTH
           ELSE
               MOVE 14               TO W10-SLNGTH.
           IF W10-SLNGTH < 3
               MOVE 3                TO W10-SLNGTH
           ELSE
           IF W10-SLNGTH > 14
               MOVE 14               TO W10-SLNGTH.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W60-COMPANY.
             MOVE PAR-DMY        TO W05-DATE.
             MOVE PAR-YMD        TO W05-DATE-YMD.
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
            PERFORM ERASE-SCREEN.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MD-LNE WS-MID-LNE.
       ZA60.
             MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCR  (WS-S1) WS-BCR  (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
                                    WS-TCHR (60) WS-TCHR (70)
                                    WS-TCR  (8)  WS-TCR  (50)
                                    WS-TCR  (66).
             MOVE WS-G3          TO WS-MCHR (1)  WS-MCHR (80)
                                    WS-MCHR (18) WS-MCHR (49)
                                    WS-MCHR (60) WS-MCHR (70)
                                    WS-MCR  (8)  WS-MCR  (50)
                                    WS-MCR  (66) WS-MCR  (1)
        WS-MCR  (80).
             MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
                                    WS-BCHR (60) WS-BCHR (70)
                                    WS-BCR  (8)  WS-BCR  (50)
                                    WS-BCR  (66).
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCR  (1)  WS-BCR  (1)
                                    WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCR  (80) WS-BCR  (80)
                                    WS-TCHR (80) WS-BCHR (80).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-I-O-ERROR SECTION 91.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK TO WS-FILE
                               MOVE WS-NETKEY   TO WS-KEY
               WHEN 3          MOVE W02-CREDIT  TO WS-FILE
                               MOVE P-NUMBER    TO WS-KEYX
               WHEN 4          MOVE W02-CRTRAN  TO WS-FILE
                               MOVE CT-KEY      TO WS-KEYX
               WHEN 7          MOVE W02-DEPART  TO WS-FILE
                               MOVE DPT-CODE    TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 16         MOVE W02-PORDER  TO WS-FILE
                               MOVE ZERO        TO WS-KEY
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 22         MOVE W02-STOCKF  TO WS-FILE
                               MOVE STK-CODE    TO WS-KEYX
               WHEN 32         MOVE W02-TXTRAN  TO WS-FILE
                               MOVE TAX-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
