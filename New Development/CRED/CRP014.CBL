      $set LINKCOUNT"384" GNT"CRP014.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****       **         **     *
      *   **    **  **    **  **    **   **  **     ***        ***     *
      *   **        **    **  **    **  **    **     **       * **     *
      *   **        ******    *******   **    **     **      *  **     *
      *   **        **   **   **        **    **     **     *******    *
      *   **    **  **    **  **         **  **      **         **     *
      *    ******   **    **  **          ****     ******      ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   T R A N S F E R S                        *
      *                                                                *
      *       Version 9.00.80 - August 2015                            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CRP014.
       AUTHOR.            J.W.LEMMON (CSS).
       DATE-WRITTEN.      JUNE, 1983.


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

       COPY "CRTRAN.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

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

       COPY "CRTRAN.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

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
       77  WS-ITM          PIC  X(14).
       77  WS-ITM1         PIC  X(14).
       77  WS-ORD          PIC  X(01).
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "Y".
       77  WS-LOOK         PIC  X(01) VALUE "S".
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
       77  WS-H1    PIC X(05)   VALUE "DATE:".
       77  WS-H3    PIC X(05)   VALUE "PAGE:".
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
           03  CRT-PROGRAM           PIC  X(15) VALUE "CRP014".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "CREDITOR INTERNAL TRANSFER".
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
    03  WS-PROG        PIC  X(03) VALUE "001".

       COPY "LEDTRF.WS".

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

    03  R-L9 REDEFINES W02-PRN-LINE.
        05  REP-PG    PIC X(06).
        05  REP-P-N    PIC Z(03)9.
        05  FILLER    PIC X(16).
        05  REP-COMP    PIC X(40).
        05  FILLER    PIC X(02).
        05  REP-REPORT    PIC X(40).
        05  FILLER    PIC X(08).
        05  REP-DAT    PIC X(06).
        05  REP-DT    PIC Z9/99/9999.

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

       01  W10-STOCK.
           03  W10-VSELL       PIC S9(09)V99 COMP-3.
           03  W10-VCASH       PIC S9(09)V99 COMP-3.
           03  W10-VWSALE      PIC S9(09)V99 COMP-3.

       COPY "W15.CRP".

       01  W16-TOTALS.
           03  FILLER          PIC  X(12) VALUE "TOTAL COST: ".
           03  W16-COST        PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(12) VALUE " TOTAL VAT: ".
           03  W16-VAT         PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(08) VALUE " TOTAL: ".
           03  W16-TOTAL       PIC  Z(08)9.99DB.

       01  W20-CALCS.
           03  W20-RESULT      PIC 9(09)V99.
           03  W20-RESULT-1    REDEFINES W20-RESULT.
               05  W20-WHOLE   PIC 9(09).
               05  W20-DECIMAL PIC 9(02).
           03  W20-ORD         PIC 9(07)     COMP-3.
    03  W20-DATE.
        05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
          PIC 9(08).

    03  W21-DATE.
        05  W21-CC      PIC 99.
        05  W21-YY      PIC 99.
        05  W21-MM      PIC 99.
        05  W21-DD      PIC 99.
    03  W21-DTE REDEFINES W21-DATE
          PIC 9(08).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
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
               05  W25-1       PIC ZZZZ9.
           03  W25-3V2.
               05  W25-2       PIC ZZ9.99.
           03  W25-7V2.
               05  W25-3       PIC Z(06)9.99.
           03  W25-6V2.
               05  W25-3A      PIC Z(06)9.99.
           03  W25-3N.
               05  W25-4       PIC ZZ9.
           03  W25-S7V2A.
               05  W25-CVAL  PIC  Z(06)9.99-.
           03  W25-DTE.
              05  W25-DATE    PIC 99/99/9999.
           03  W25-6.
               05  W25-NO      PIC Z(07)9.
           03  W25-S5V2.
               05  W25-SVAL    PIC Z(07)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2    PIC Z(06)9.99DB.
           03  W25-5V2 REDEFINES W25-S7V2.
               05  W25-VAL     PIC Z(07)9.99-.
           03  W25-72.
               05  W25-VAL3    PIC S9(07)V99.
           03  W25-PRC.
               05  W25-IPRC    PIC Z(06)9.999.

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
               05  W40-ACHAR PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS   PIC  X(60).
               05  FILLER      PIC  X(10).
           03  W40-NAME.
               05  W40-NCHAR   PIC  X(01) OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
           03  W50-HEAD        PIC X(20).

       01  W60-H2.
           03  W60-COMPANY     PIC X(40).
           03  W60-DESC        PIC X(21)     VALUE
                               "CREDITORS AUDIT TRAIL".
       01  W65-CREDITOR.
           03  W65-CFLOW       PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W65-V1          PIC S9(09)V99 COMP-3.
           03  W65-V2          PIC S9(09)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
    03  W75-CNO       PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.

       01  W90-TRAN.
           03  W90-CODE        PIC  9(02).
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-TRNKEY      PIC  9(04)    COMP-5.
           03  W90-TS          PIC  9(02)    COMP-5.

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC  Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
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

       01  S05.
    02  S05A.
          03  LINE  2 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "TRANSFERS".
          03  LINE  4 COLUMN  2 VALUE "Creditor Ac. :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 24 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(40) FROM WS-SPC.
          03  LINE  6 COLUMN 11 VALUE "  Date  ".
          03        COLUMN 28 VALUE "  Cost price  ".
          03        COLUMN 49 VALUE "30  60  90 120 150 180".
    02  S05B.
          03  LINE  8 COLUMN 11 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(08) FROM WS-SPC.
          03        COLUMN 28 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(14) FROM WS-SPC.
          03        COLUMN 49 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(01) FROM WS-SPC.
          03        COLUMN 53 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(01) FROM WS-SPC.
          03        COLUMN 57 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(01) FROM WS-SPC.
          03        COLUMN 61 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(01) FROM WS-SPC.
          03        COLUMN 65 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(01) FROM WS-SPC.
          03        COLUMN 69 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(01) FROM WS-SPC.

       01  S19.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W15-ACNO AUTO.

       01  S28.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-GRND AUTO.

       01  S34.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99DB
                                       USING W15-DISCOUNT AUTO.

       COPY "CRDLUP.CRT".

       01  S-CRED.
    03  LINE 23 COLUMN  2 VALUE "Balance :".
    03        COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-BAL.
           03          COLUMN 28 VALUE "Due :".
    03        COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-OUT.

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
            PERFORM DA000.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
            CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
            PERFORM CREDITOR-LOOKUP.
            PERFORM RESTORE-SCREEN.
       AA99.
            EXIT.

       AA050-HEADINGS  SECTION.
       AA050.
      *
      *    ****    Print the audit trail HEADING
      *
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
             MOVE WS-H1          TO R-H1.
             MOVE W60-COMPANY    TO R-H2.
             MOVE WS-H3          TO R-H3.
            ADD 1   TO WS-PAGE.
             MOVE W05-DATE       TO R-DTE1.
             MOVE WS-PAGE        TO R-PGE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE W60-DESC       TO R-H2.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       AA060.
             MOVE W50-H4         TO R-HEAD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

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

      /
      *    ****    READ FILES ROUTINES
      *
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CRTRAN.RD".

       COPY "CREDIT.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "TXTRAN.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AC000-WRITE   SECTION.

       COPY "CONTROL.WR".

       COPY "CRTRAN.WR".

       COPY "CREDIT.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "TXTRAN.WR".

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
       AY07.
             MOVE 07 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
             MOVE P-REC          TO REC-CREDIT.
             GO TO AY50.
       AY08.
             MOVE 08             TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE CT-REC  TO REC-CRTRAN.
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
             MOVE 40             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
             MOVE XFR-REC        TO REC-LEDTRF.
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
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "CR"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 2              TO WS-NETKEY.
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
           IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
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

       CA600         SECTION.
       CA601.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-RECORD.
             SUBTRACT CT-COST    FROM W00-C-VAL
                                      W00-C-MTDV
                                      W00-C-YTDV.
             ADD 1               TO W00-C-DAY
                                    W00-C-MTD
                                    W00-C-YTD.
             PERFORM UPDATE-TRAN-RECORD.

       CA699.
             EXIT.

       COPY "LEDTRF.UPD".

      /
       DA000      SECTION 5.
       DA00A.
             MOVE "ACCOUNT TRANSFERS" TO W50-HEAD.
            MOVE ZERO   TO WS-PAGE.

       DA00B.
             PERFORM AA050 THRU AA060.
             MOVE WS-HACNO       TO J-H1.
             MOVE WS-HNAME       TO J-H2.
             MOVE WS-HDTE        TO J-H3.
             MOVE WS-HCOST       TO J-H4.
             MOVE WS-HSELL       TO J-H5.
             MOVE WS-HAGE        TO J-H6.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       DA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S05.
             MOVE SPACES         TO W15-ACNO.

       DA02.
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
            ACCEPT S19 AT 0417.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W15-ACNO
       GO TO DA06
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO DA02
        END-EVALUATE
              DISPLAY S19 AT 0417
              IF W15-ACNO = SPACES
           GO TO DA02.

       DA06.
      PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
               GO TO DA999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
            PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
              MOVE WS-ER1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO DA02.
             MOVE P-NAME         TO W15-NAME.
            DISPLAY P-NAME AT 0424
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY S-CRED.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO DA02.
             MOVE ZERO           TO W15-GRND W15-DISCOUNT W15-SETT.

       DA10.
      DISPLAY S05B.
      MOVE 0901   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.

       DA12.
      DISPLAY "Key in date as " AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "DDMMCCYY" WITH BACKGROUND-COLOR 3
         FOREGROUND-COLOR 6 HIGHLIGHT.
      DISPLAY "Press " AT 5041
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3
    FOREGROUND-COLOR 6 HIGHLIGHT
       " to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            ACCEPT S28 AT 0811.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO DA00
   WHEN OTHER
       CALL X"E5"
       GO TO DA12
        END-EVALUATE.
            MOVE W15-GRND  TO W15-DATE.
             PERFORM AD000.
            PERFORM CLEAR-L50.
           IF WS-ERROR = 1
              MOVE "Invalid Date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO DA12.
      MOVE W15-DD  TO W20-DD.
      MOVE W15-MM  TO W20-MM.
      MOVE W15-CC  TO W20-CC.
      MOVE W15-YY  TO W20-YY.

       DA15.
      DISPLAY "Press '-' key for debit amount" AT 5001
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
      DISPLAY "Press " AT 5041
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             ACCEPT S34 AT 0831.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO DA00
   WHEN OTHER
       CALL X"E5"
       GO TO DA15
        END-EVALUATE.
           IF W15-DISCOUNT = ZERO
               GO TO DA15.
            PERFORM CLEAR-L50.
       DA20.
            ACCEPT W15-30 AT 0849
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
            ACCEPT W15-60 AT 0853
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
            ACCEPT W15-90 AT 0857
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
            ACCEPT W15-120 AT 0861
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
            ACCEPT W15-150 AT 0865
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
            ACCEPT W15-180 AT 0869
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO DA10.
             MOVE ZERO           TO WS-S.
             MOVE 1              TO WS-S2.
       DA25.
             MOVE ZERO           TO W25-CFLOW (WS-S2).
           IF W15-MONTH (WS-S2) > ZERO
               ADD 1             TO WS-S.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO DA25.
             MOVE ZERO           TO W25-V1 W25-V2.
             MOVE 1              TO WS-S3.
           IF WS-S < 2
               MOVE W15-DISCOUNT TO W25-V2
               GO TO DA30.
           IF W15-DISCOUNT < ZERO
               MULTIPLY -1 BY W15-DISCOUNT
                                 GIVING W20-RESULT
           ELSE
               MOVE W15-DISCOUNT TO W20-RESULT.
             DIVIDE W20-RESULT BY WS-S
                                 GIVING W25-V1 ROUNDED.
             SUBTRACT 1          FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1               TO WS-S.
           IF W15-DISCOUNT < ZERO
               MULTIPLY -1       BY W25-V1
               MULTIPLY -1       BY W25-V2.
       DA30.
           IF WS-S = ZERO
               GO TO DA40.
       DA35.
           IF W15-MONTH (WS-S3) = ZERO
               ADD 1             TO WS-S3
               GO TO DA35.
           IF WS-S > 1
               MOVE W25-V1       TO W25-CFLOW (WS-S3)
               SUBTRACT 1        FROM WS-S
               ADD 1             TO WS-S3
               GO TO DA35.
             MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE ZERO           TO W25-V1 W25-V2.
       DA40.
      *
      *    ****    Start Recovery Logging.
      *
             PERFORM AY60 THRU AY999.
             PERFORM AY07 THRU AY59.

       DA45.
            MOVE W20-DTE  TO CT-DTE CT-SDTE.
            MOVE ZERO   TO CT-TAX CT-TAXFREE
        CT-SEQ CT-PAID.
      MOVE SPACES  TO CT-SINV CT-REF
        CT-ORD CT-DREF.
             MOVE W15-DISCOUNT   TO CT-COST.
             MOVE W15-ACNO       TO CT-CRED.
           IF W15-DISCOUNT < ZERO
               MOVE 05           TO CT-CODE
           ELSE
               MOVE 06           TO CT-CODE.
            MOVE ZERO   TO CT-AGE CT-SELL.
      MOVE "U"   TO CT-TYPE.
      MOVE "T"   TO CT-PJ.
      PERFORM CA600.
           IF WX-INT = "Y"
               MOVE W20-DTE      TO WX-DATE
        MOVE "  CT"  TO WX-LEDG
        MOVE W90-CODE  TO WX-TRN
               MOVE W00-C-DESC   TO WX-DESC
               MOVE "- CREDITORS"
                                 TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL     TO WX-AC
               MOVE 02           TO WX-TYPE
               PERFORM GL-TRF-UPDATE
               MOVE 25           TO WX-TYPE
               MOVE W00-C-GL     TO WX-AC
               MOVE CT-COST      TO WX-VALUE
               PERFORM GL-TRF-UPDATE.
             MOVE 1              TO WS-S WS-S2.
           IF W25-V2 NOT = ZERO
               GO TO DA55.

       DA50.
          IF W15-MONTH (WS-S) NOT = ZERO
               ADD WS-S2         TO CT-AGE.
           IF WS-S < 6
               ADD 1             TO WS-S
               MULTIPLY 2        BY WS-S2
               GO TO DA50.

       DA55.
             ADD W15-DISCOUNT    TO P-BAL.
           IF W15-DISCOUNT > ZERO
               ADD W15-DISCOUNT  TO P-CR
           ELSE
               ADD W15-DISCOUNT  TO P-DT.
             ADD W25-CFLOW (6)   TO P-180D.
           IF P-180D < ZERO
               SUBTRACT P-180D   FROM W25-CFLOW (6)
               ADD P-180D        TO W25-CFLOW (5)
               MOVE ZERO         TO P-180D.
             ADD W25-CFLOW (5)   TO P-150D.
           IF P-150D < ZERO
               SUBTRACT P-150D   FROM W25-CFLOW (5)
               ADD P-150D        TO W25-CFLOW (4)
               MOVE ZERO         TO P-150D.
             ADD W25-CFLOW (4)   TO P-120D.
           IF P-120D < ZERO
               SUBTRACT P-120D   FROM W25-CFLOW (4)
               ADD P-120D        TO W25-CFLOW (3)
               MOVE ZERO         TO P-120D.
             ADD W25-CFLOW (3)   TO P-90D.
           IF P-90D < ZERO
               SUBTRACT P-90D    FROM W25-CFLOW (3)
               ADD P-90D         TO W25-CFLOW (2)
               MOVE ZERO         TO P-90D.
             ADD W25-CFLOW (2)   TO P-60D.
           IF P-60D < ZERO
               SUBTRACT P-60D    FROM W25-CFLOW (2)
               ADD P-60D         TO W25-CFLOW (1)
               MOVE ZERO         TO P-60D.
             ADD W25-CFLOW (1)   TO P-30D.
           IF P-30D < ZERO
               SUBTRACT P-30D    FROM W25-CFLOW (1)
               ADD P-30D         TO W25-V2
               MOVE ZERO         TO P-30D.
             ADD W25-V2          TO P-OUT.
            PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
      MOVE 1   TO WS-ACTION.
      PERFORM AY08 THRU AY59.
            PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
           IF W15-DISCOUNT < ZERO
               ADD W15-DISCOUNT  TO W70-DEBT
           ELSE
               ADD W15-DISCOUNT  TO W70-CRED.
             ADD W15-DISCOUNT    TO W70-CLOSE.
             ADD W25-CFLOW (6)   TO W70-DUE-180.
             ADD W25-CFLOW (5)   TO W70-DUE-150.
             ADD W25-CFLOW (4)   TO W70-DUE-120.
             ADD W25-CFLOW (3)   TO W70-DUE-90.
             ADD W25-CFLOW (2)   TO W70-DUE-60.
             ADD W25-CFLOW (1)   TO W70-DUE-30.
             ADD W25-V2          TO W70-DUE-NOW.
      *
      *    ****    End Recovery Logging.
      *
             PERFORM AY70 THRU AY999.
             MOVE W15-ACNO       TO J-ACNO.
             MOVE W15-NAME       TO J-NAME.
             MOVE W15-GRND       TO J-DATE.
             MOVE W15-DISCOUNT   TO J-COST.
             MOVE "N"            TO J-30 J-60 J-90 J-120 J-150 J-180.
           IF W15-MONTH (1) > ZERO
               MOVE "Y"          TO J-30.
           IF W15-MONTH (2) > ZERO
               MOVE "Y"          TO J-60.
           IF W15-MONTH (3) > ZERO
               MOVE "Y"          TO J-90.
           IF W15-MONTH (4) > ZERO
               MOVE "Y"          TO J-120.
           IF W15-MONTH (5) > ZERO
               MOVE "Y"          TO J-150.
           IF W15-MONTH (6) > ZERO
               MOVE "Y"          TO J-180.
          IF W02-LINAGE < (W02-PRN-LENGTH - 1)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM DA00B.
             MOVE SPACES         TO R-DET.
             GO TO DA00.

       DA999.
             EXIT.

      /
       ZA000-INIT    SECTION 8.
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

            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W60-COMPANY.
             MOVE PAR-DMY        TO W05-DATE.
             MOVE PAR-YMD        TO W05-DATE-YMD.
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
             MOVE WS-G1          TO WS-TCR  (WS-S1) WS-BCR  (WS-S1).
             MOVE WS-G8          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
             MOVE WS-G2          TO WS-TCR  (8)  WS-TCR (50)
                                    WS-TCR  (66).
             MOVE WS-G3          TO WS-MCR  (8)  WS-MCR (50)
                                    WS-MCR  (66) WS-MCR (1)
        WS-MCR  (80).
             MOVE WS-G4          TO WS-BCR  (8)  WS-BCR (50)
                                    WS-BCR  (66).
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCR  (1)  WS-BCR (1).
             MOVE WS-G13         TO WS-TCR (80)  WS-BCR (80).
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
       ZB000-I-O-ERROR SECTION 9.

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
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 32         MOVE W02-TXTRAN  TO WS-FILE
                               MOVE TAX-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF  TO WS-FILE
                               MOVE XFR-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
