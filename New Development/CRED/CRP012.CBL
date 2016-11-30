      $set LINKCOUNT"596" GNT"CRP012.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****       **      ******    *
      *   **    **  **    **  **    **   **  **     ***     **    **   *
      *   **        **    **  **    **  **    **     **          **    *
      *   **        ******    *******   **    **     **        **      *
      *   **        **   **   **        **    **     **      **        *
      *   **    **  **    **  **         **  **      **     **         *
      *    ******   **    **  **          ****     ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   D E B I T   N O T E S   ( S T O C K )    *
      *                                                                *
      *       Version 9.00.80 - August 2015                            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CRP012.
       AUTHOR.            James William Lemmon.
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

       COPY "CRETRN.SL".

       COPY "CRTRAN.SL".

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "PURCH.SL".

       COPY "PURDEX.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "STOCK.SL".

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

       COPY "CRETRN.FDE".

       COPY "CRTRAN.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "PURCH.FD".

       COPY "PURDEX.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "STOCK.FDE".

       COPY "TXTRAN.FDE".

      /
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
       77  WS-S     PIC 9(04)  COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S9    PIC 9(04)  COMP-5.
       77  WS-CRETRN    PIC 9(04)  COMP-5.
       77  WS-VAT    PIC S9(07)V99 COMP-3.
       77  WS-IVAT    PIC S9(07)V99 COMP-3.
       77  WS-ORD    PIC X(01).
       77  WS-OPTION       PIC  X(01).
       77  WS-ALLOCATE    PIC X(01).
       77  WS-SKIP    PIC X(01)  VALUE "Y".
       77  WS-LOOK    PIC X(01)  VALUE "S".
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-CHAR1    PIC X(01).
       77  WS-SKEY    PIC 9(05).
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-LINES    PIC 9(04)  COMP-5 VALUE 0.
       77  WS-PAGE    PIC 9(04)  COMP-5 VALUE 0.
       77  WS-ER1    PIC X(09)  VALUE "No record".
       77  WS-HDTE    PIC X(04)  VALUE "DATE".
       77  WS-HACNO        PIC X(04)     VALUE "ACNO".
       77  WS-HCRED        PIC X(09)     VALUE "CREDITOR:".
       77  WS-HQNT         PIC X(05)     VALUE "QUANT".
       77  WS-HDESC        PIC X(11)     VALUE "DESCRIPTION".
       77  WS-HCOST        PIC X(10)     VALUE "COST PRICE".
       77  WS-HSELL        PIC X(10)     VALUE "SELL PRICE".
       77  WS-HNAME        PIC X(04)     VALUE "NAME".
       77  WS-HAGE         PIC X(12)     VALUE "AGE 30 - 180".
       77  WS-HSETT    PIC X(22)  VALUE
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
       77  WS-SSETT    PIC X(22)  VALUE
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
           03  CRT-PROGRAM           PIC  X(15) VALUE "CRP012".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "CREDITOR DEBIT NOTE (STOCK)".
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
    03  WS-PROG        PIC  X(03) VALUE "012".

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
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            X  =  Cancel expanded print
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
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
        05  G-H3     PIC X(04).
        05  FILLER    PIC X(01).
        05  G-INVN    PIC X(08).
        05  FILLER    PIC X(01).
        05  G-H4     PIC X(04).
        05  FILLER    PIC X(01).
        05  G-INVD    PIC Z9/99/9999.
        05  FILLER    PIC X(01).
        05  G-H5     PIC X(05).
        05  FILLER    PIC X(01).
        05  G-RAIL    PIC Z(04)9.99-.
        05  FILLER    PIC X(01).
        05  G-H6     PIC X(04).
        05  FILLER    PIC X(01).
        05  G-DISC    PIC Z(04)9.99-.
        05  FILLER    PIC X(01).
        05  G-H6A    PIC X(04).
        05  FILLER    PIC X(01).
        05  G-VAT    PIC Z(05)9.99-.
        05  FILLER    PIC X(01).
        05  G-H7     PIC X(06).
        05  G-TOTAL    PIC Z(07)9.99-.

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

    03  DS-L1 REDEFINES W02-PRN-LINE.
        05  DS-H1    PIC X(06).
        05  DS-DATE    PIC Z9/99/9999.
        05  FILLER    PIC X(25).
        05  DS-COMP    PIC X(41).
        05  DS-H2    PIC X(40).
        05  DS-H3    PIC X(06).
        05  DS-PAGE    PIC ZZZ9.

    03  DS-L2 REDEFINES W02-PRN-LINE.
        05  DS-H4    PIC X(09).
        05  DS-ACNO    PIC X(06).
        05  FILLER    PIC X(01).
        05  DS-NAME    PIC X(40).
        05  FILLER    PIC X(02).
        05  DS-H5    PIC X(11).
        05  DS-DNO    PIC X(08).
        05  FILLER    PIC X(02).
        05  DS-H6    PIC X(05).
        05  DS-DNDTE    PIC Z9/99/9999.
        05  FILLER    PIC X(02).
        05  DS-H7    PIC X(11).
        05  DS-INV    PIC X(08).
        05  FILLER    PIC X(01).
        05  DS-H8    PIC X(06).
        05  DS-IDTE    PIC Z9/99/9999.

    03  DS-L3 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(01).
        05  DS-H9    PIC X(16).
        05  DS-H10    PIC X(29).
        05  DS-H11    PIC X(13).
        05  DS-H12    PIC X(13).
        05  DS-H13    PIC X(12).
        05  DS-H14    PIC X(15).
        05  DS-H15    PIC X(11).
        05  DS-H16    PIC X(22).

    03  DS-L4 REDEFINES W02-PRN-LINE.
        05  DS-ITEM    PIC X(14).
        05  FILLER    PIC X(03).
        05  DS-DESC    PIC X(24).
        05  FILLER    PIC X(04).
        05  DS-UCOST    PIC Z(05)9.999.
        05  FILLER    PIC X(04).
        05  DS-USELL    PIC Z(05)9.99.
        05  FILLER    PIC X(02).
        05  DS-QNT    PIC Z(05)9.99.
        05  FILLER    PIC X(03).
        05  DS-COST    PIC Z(06)9.999.
        05  FILLER    PIC X(04).
        05  DS-SELL    PIC Z(07)9.99.
        05  FILLER    PIC X(02).
        05  DS-30    PIC X(01).
        05  FILLER    PIC X(03).
        05  DS-60    PIC X(01).
        05  FILLER    PIC X(03).
        05  DS-90    PIC X(01).
        05  FILLER    PIC X(03).
        05  DS-120    PIC X(01).
        05  FILLER    PIC X(03).
        05  DS-150    PIC X(01).
        05  FILLER    PIC X(03).
        05  DS-180    PIC X(01).
        05  FILLER    PIC X(01).

       COPY "W05.WS".

       COPY "W05.VAT".

       COPY "W10.CRP".

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
    03  W21-DTE2 REDEFINES W21-DATE.
        05  W21-CY      PIC 9(04).
        05  FILLER      PIC 9(04).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
    03  W25-INVD        PIC 9(08).
    03  W25-PDTE REDEFINES W25-INVD.
        05  W25-DD      PIC 9(02).
        05  W25-MM      PIC 9(02).
        05  W25-CC      PIC 9(02).
        05  W25-YY      PIC 9(02).

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
              05  W25-SVAL    PIC Z(07)9.99.
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

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
           03  W50-HEAD        PIC X(20).

       01  W55-DEL.
           03  W55-MNTH        PIC X(03).
           03  FILLER          PIC X(01)     VALUE ",".
           03  W55-YY          PIC 9(02).

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
    03  W75-S       PIC  9(02)    COMP-5.
    03  W75-S1       PIC  9(02)    COMP-5.
    03  W75-S2       PIC  9(02)    COMP-5.
    03  W75-KEY       PIC  9(06)    COMP-5.
    03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
    03  W75-CNO       PIC  X(06)    OCCURS 18.

       01  W90-TRAN.
           03  W90-CODE        PIC  9(02).
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-TRNKEY      PIC  9(04)    COMP-5.
           03  W90-TS          PIC  9(02)    COMP-5.
          03  W90-DB        PIC S9(09)V99 COMP-3.
           03  W90-CR          PIC S9(09)V99 COMP-3.
           03  W90-ALLOCATED   PIC S9(09)V99 COMP-3.
          03  W90-DISCOUNT    PIC S9(09)V99 COMP-3.
          03  W90-DEBIT       PIC S9(09)V99 COMP-3.
           03  W90-CREDIT      PIC S9(09)V99 COMP-3.

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

       01  S06.
           02  S06A.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "CREDITOR DEBIT NOTE".
          03  LINE  4 COLUMN  2 VALUE "Creditor Ac. :".
          03  LINE  5 COLUMN  2 VALUE "D/N no:".
          03        COLUMN 20 VALUE "D/N date:".
          03        COLUMN 42 VALUE "Inv no:".
          03        COLUMN 60 VALUE "Inv date:".
           03  LINE  7 COLUMN  2 VALUE "Item code    :".
           03  LINE  8 COLUMN  2 VALUE "Description  :".
           03  LINE  9 COLUMN  2 VALUE "Quantity     :".
           03  LINE 10 COLUMN  2 VALUE "Unit cost    :".
           03  LINE 11 COLUMN  2 VALUE "Discount-1   :".
           03  LINE 12 COLUMN  2 VALUE "Discount-2   :".
           03  LINE 13 COLUMN  2 VALUE "Unit nett    :".
           03  LINE 14 COLUMN  2 VALUE "Total cost   :".
           03  LINE 16 COLUMN  2 VALUE "Unit retail  :".
           03  LINE 17 COLUMN  2 VALUE "Total retail :".
           02  S06B.
          03  LINE 19 COLUMN  2 VALUE "Allocation - 30:".
    03        COLUMN 22 VALUE "60:".
    03        COLUMN 29 VALUE "90:".
    03        COLUMN 36 VALUE "120:".
    03        COLUMN 44 VALUE "150:".
    03        COLUMN 52 VALUE "180:".
           03  LINE 20 COLUMN 54 VALUE "Sub total :".
           03  LINE 21 COLUMN 54 VALUE "VAT       :".
           03  LINE 22 COLUMN 54 VALUE "Totals    :".

       01  S07.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
      PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
            PIC X(14) USING W10-ITEM AUTO.
    03  COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
            PIC X(03) USING W10-ITM AUTO.

       01  S07-EXT.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
            PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S07-3.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(03) USING W10-I4 AUTO.

       01  S07-4.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(04) USING W10-I4 AUTO.

       01  S07-5.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(05) USING W10-I5 AUTO.

       01  S07-6.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(06) USING W10-I6 AUTO.

       01  S07-7.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(07) USING W10-I7 AUTO.

       01  S07-8.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(08) USING W10-I8 AUTO.

       01  S07-9.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(09) USING W10-I9 AUTO.

       01  S07-10.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(10) USING W10-I10 AUTO.

       01  S07-11.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(11) USING W10-I11 AUTO.

       01  S07-12.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(12) USING W10-I12 AUTO.

       01  S07-13.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(13) USING W10-I13 AUTO.

       01  S07-14.
          03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           PIC Z(14) USING W10-I14 AUTO.

       01  S19.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC X(06) USING W15-ACNO AUTO.

       01  S27.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC X(08) USING W15-GRNN AUTO.

       01  S28.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z9/99/9999 USING W15-GRND AUTO.

       01  S29.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC X(08) USING W15-INVN AUTO.

       01  S30.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z9/99/9999 USING W15-INVD AUTO.

       01  S32.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-USELL AUTO.

       01  S35.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z9.99 USING W15-DISC1 AUTO.

       01  S36.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z9.99 USING W15-DISC2 AUTO.

       01  S37.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-IUN AUTO.

       01  S38.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z(07)9.999 USING W15-ICOST AUTO.

       01  S40.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-COST AUTO.

       01  S42.
    02  S42A.
    03  LINE  4 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 24 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(40) FROM WS-SPC.
    03  LINE  5 COLUMN 10 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(08) FROM WS-SPC.
    03        COLUMN 30 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(10) FROM WS-SPC.
    03        COLUMN 50 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(08) FROM WS-SPC.
    03        COLUMN 70 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(10) FROM WS-SPC.
    02  S42B.
    03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(14) FROM WS-SPC.
    03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(30) FROM WS-SPC.
    03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(11) FROM WS-SPC.
    03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(12) FROM WS-SPC.
    03  LINE 11 COLUMN 23 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(05) FROM WS-SPC.
    03  LINE 12 COLUMN 23 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(05) FROM WS-SPC.
    03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(12) FROM WS-SPC.
    03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(11) FROM WS-SPC.
    03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(11) FROM WS-SPC.
    03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(11) FROM WS-SPC.
    03  LINE 19 COLUMN 19 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 26 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 33 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 41 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 49 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(01) FROM WS-SPC.
    03        COLUMN 57 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(01) FROM WS-SPC.
    02  S42C.
    03  LINE 20 COLUMN 66 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(13) FROM WS-SPC.
    03  LINE 21 COLUMN 66 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(13) FROM WS-SPC.
    03  LINE 22 COLUMN 66 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC  X(13) FROM WS-SPC.

       01  S43.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z(07)9.99DB USING W15-VAT AUTO.

       01  S44.
          03  FOREGROUND-COLOR 0
        BACKGROUND-COLOR 7 PIC Z(07)9.99DB USING W15-TCOST AUTO.

       COPY "CRDLUP.CRT".

       01  S-CRED.
           03  LINE 23 COLUMN  2 VALUE "Balance :".
    03        COLUMN 12 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
                                 PIC Z(08)9.99DB USING P-BAL.
           03          COLUMN 28 VALUE "Due :".
    03        COLUMN 34 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
                                 PIC Z(08)9.99DB USING P-OUT.

       01  S-ALLOC.
    03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
    03        COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC Z(08)9.99 FROM W75-BALANCE.
    03        COLUMN 51 VALUE "Allocated : ".
    03        COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC Z(08)9.99 FROM W75-ALLOCATED.

       01  S-CTRN.
    03  COLUMN  6 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
       PIC X(08) FROM CRT-REF.
    03  COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
       PIC 9999/99/99 FROM CRT-SDTE.
    03  COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
       PIC X(12) FROM W00-C-DESC.
    03  COLUMN 39 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
       PIC Z(08)9.99 FROM CRT-VALUE.
    03  COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
       PIC Z(08)9.99 FROM CRT-OUTSTD.
    03  COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
       PIC Z(08)9.99 USING CRT-ALLOC AUTO.

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
      MOVE 3   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
            PERFORM EA000.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
            CLOSE PURCH
     RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50CS.LUP".

       AA050-UTILS     SECTION.
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

       COPY "PRINTUTL.AS4".

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
             "=Item/"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Alt/"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Desc/"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Desc2/"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=X/Ref Find,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12
       " Create"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA305.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CRETRN.RD".

       COPY "CRTRAN.RD".

       COPY "CREDIT.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "PURCH.RD".

       COPY "SHARED.RD".

       COPY "STOCK.RD".

       COPY "TXTRAN.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AC000-WRITE   SECTION.

       COPY "CONTROL.WR".

       COPY "CRETRN.WR".

       COPY "CRTRAN.WR".

       COPY "CREDIT.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "PURCH.WR".

       COPY "PURDEX.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

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

       AY37.
            MOVE 37   TO REC-FILE.
            MOVE WS-SHARED  TO REC-KEY.
            MOVE SHR-RECORD  TO REC-NETWORK.
             GO TO AY50.

       AY38.
      MOVE 38   TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
      MOVE ZERO   TO REC-TYPE.
            PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        MOVE "XXXX"  TO DPT-CODE
        GO TO AY38.
            MOVE DPT-RECORD  TO REC-DEPART.
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

       AY056.
            MOVE 56   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE PRD-REC1  TO REC-PURDEX.
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

      /
       CA155-GET-CREDITOR      SECTION.
       CA155-ACCOUNT.
             MOVE W15-ACNO       TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
              MOVE 1   TO WS-ERROR
           ELSE
           IF P-NUMBER NOT = W15-ACNO
              MOVE 2   TO WS-ERROR
           ELSE
              MOVE 0   TO WS-ERROR.

       CA160-EXIT.
             EXIT.

       CA600      SECTION.
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

       CA700         SECTION.
       CA701.
             MOVE 3              TO TAX-ACTYPE.
             MOVE CT-CODE        TO TAX-CODE.
             MOVE W20-DTE        TO TAX-DATE.
             MOVE CT-REF         TO TAX-REF.
             MULTIPLY -1 BY CT-COST
                                 GIVING TAX-VALUE.
             MULTIPLY -1 BY CT-TAXFREE
                                 GIVING TAX-TAXFREE.
      MOVE CT-CRED  TO TAX-AC.
    IF W00-C-ACT = 1
               IF CT-CODE = 03
                   MOVE "3"      TO TAX-TYPE
               ELSE
                   MOVE "2"      TO TAX-TYPE
           ELSE
               MOVE "1"          TO TAX-TYPE.
      MOVE ZERO   TO TAX-SEQ.
      MOVE 1   TO WS-S9.

       CA705.
      MOVE W15-TAXVAL(WS-S9)
     TO TAX-VAL(WS-S9).
      MOVE W15-VATVAL(WS-S9)
     TO TAX-VAT(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO CA705.
            PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
            MOVE 1   TO WS-ACTION.
            PERFORM AY19 THRU AY59.

       CA799.
             EXIT.

       COPY "LEDTRF.UPD".

       COPY "CREDIT.ALL".

      /
       EA000      SECTION 50.
       EA00.
          IF LS0-STP NOT = 2
               PERFORM ERASE-SCREEN
              MOVE "Stock not installed"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO EA999.
             MOVE ZERO           TO W15-TOTAL WS-PAGE W15-TOTVAT.
      *
      *    ****    Print the audit trail HEADING
      *
       EA05.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
             MOVE WS-H1          TO DS-H1.
             MOVE W60-COMPANY    TO DS-COMP.
             MOVE "- DEBIT NOTES"
                                 TO DS-H2.
             MOVE WS-H3          TO DS-H3.
            ADD 1   TO WS-PAGE.
             MOVE W05-DATE       TO DS-DATE.
             MOVE WS-PAGE        TO DS-PAGE.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.
      *
      *    ****    S C R E E N  L A Y O U T .
      *
       EA10.
             PERFORM ERASE-SCREEN.
             DISPLAY S06.
      DISPLAY S42.
             MOVE 1              TO WS-PURLAST.
      *
      *                                ENTER CREDIT ACCOUNT NUMBER.
      *
             MOVE SPACES         TO W15-ACNO.

       EA15.
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
       GO TO EA16
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50C THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO EA15
        END-EVALUATE
              DISPLAY S19 AT 0417
              IF W15-ACNO = SPACES
           GO TO EA15.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       EA16.
      PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              MOVE W15-TOTAL  TO W16-COST
               MOVE W15-TOTVAT   TO W16-VAT
        ADD W15-TOTAL W15-TOTVAT
                                 GIVING W16-TOTAL
               MOVE W16-TOTALS   TO DS-L1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              MOVE SPACES  TO R-DET
              GO TO EA999.
            PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
              MOVE WS-ER1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO EA15.
             MOVE P-NAME         TO W15-NAME.
            DISPLAY P-NAME AT 0424
       WITH FOREGROUND-COLOR 1
     BACKGROUND-COLOR 7.
           IF P-TAX = "E"
        DISPLAY "(Exclusive)" AT 0465
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
           ELSE
           IF P-TAX = "I"
        DISPLAY "(Inclusive)" AT 0465
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
           ELSE
        DISPLAY "(No VAT)" AT 0465
   WITH FOREGROUND-COLOR 3 HIGHLIGHT.
             DISPLAY S-CRED.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA15.

       EA19.
             MOVE ZERO           TO W30-CUR W30-30
                                    W30-120 W30-60
                                    W30-150 W30-90
                                    W30-180 W15-TXBL.
            MOVE ZERO   TO W15-GRND W15-INVD.
      MOVE SPACES  TO W15-GRNN W15-INVN.
          IF W02-LINAGE NOT < (W02-PRN-LENGTH - 8)
               PERFORM EA05.

       EA20.
            ACCEPT S27 AT 0510.
          IF W15-GRNN = SPACES
               GO TO EA20.
      MOVE 8   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-GRNN
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       EA25.
             ACCEPT S28 AT 0530.
             MOVE W15-GRND       TO W15-DATE.
             PERFORM AD000.
           IF WS-ERROR = 1
               GO TO EA25.
      MOVE W15-DD  TO W20-DD W25-DD.
      MOVE W15-MM  TO W20-MM W25-MM.
      MOVE W15-CC  TO W20-CC W25-CC.
      MOVE W15-YY  TO W20-YY W25-YY.

       EA30.
            ACCEPT S29 AT 0550.
          IF W15-INVN = SPACES
               GO TO EA30.
      MOVE 8   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-INVN
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       EA35.
            ACCEPT S30 AT 0570.
             MOVE W15-INVD       TO W15-DATE.
             PERFORM AD000.
           IF WS-ERROR = 1
               GO TO EA35.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA20.
            PERFORM CLEAR-L50.
      MOVE W15-DD  TO W21-DD.
      MOVE W15-MM  TO W21-MM.
      MOVE W15-CC  TO W21-CC.
      MOVE W15-YY  TO W21-YY.
            MOVE ZERO   TO W15-QUANT W15-TCOST W15-DISCOUNT
                                    W15-DISC1 W15-DISC2 W15-INCVAL
                                    W15-TSELL W15-TXBL  W15-VAT
        W15-IVAT  W15-RAIL.
      MOVE 1   TO WS-S9.

       EA36.
            MOVE ZERO   TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO EA36.

       EA38.
            MOVE SPACES  TO W10-EXT-ITEM W15-DESCR.
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

       EA40.
             PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF WS-EXT-STK = "Y"
        ACCEPT S07-EXT AT 0717
    ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM AT 0717
        ELSE
           ACCEPT S07 AT 0717
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT 0717
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT 0717
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT 0717
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT 0717
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT 0717
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT 0717
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT 0717
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT 0717
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT 0717
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT 0717
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT 0717
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT 0717.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO EA40A
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
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
   WHEN F8-KEY
       PERFORM AA100
       GO TO EA40
   WHEN OTHER
       CALL X"E5"
       GO TO EA38
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT 0717
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT 0717
     ELSE
               DISPLAY S07 AT 0717
     END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO EA38.

       EA40A.
          IF W10-EXT-ITEM = SPACES
               SUBTRACT 1        FROM WS-PURLAST
               GO TO EA72.

       EA41.
             PERFORM CA000.
          IF WS-F-ERROR NOT = ZERO
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT 0717
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT 0717
     ELSE
               DISPLAY S07 AT 0717
     END-IF
        END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO EA40
               ELSE
                   GO TO EA41.
            PERFORM CLEAR-L50.
      *
      *    ****    GET THE APPLICABLE VAT RATE (W05-RATE, WS-VAT-SUB)
      *
      MOVE STK-TAX  TO WS-VAT-SUB.
      *
      *    ****    DOES THE PREVIOUS RATE APPLY
      *
    IF (WS-VAT-SUB > 0 AND < 7)
        IF W21-DTE < WS-VAT-DATE(WS-VAT-SUB)
     ADD 6  TO WS-VAT-SUB.
    IF STK-TAX NOT = ZERO
        MOVE W05-VAT(WS-VAT-SUB)
     TO W05-RATE
    ELSE
        MOVE ZERO  TO W05-RATE.
          IF STK-IND = 3
               MOVE ZERO         TO W15-ICOST W15-USELL
           ELSE
               MOVE STK-COST     TO W15-ICOST
               MOVE STK-SELL     TO W15-USELL.
      *
      *    ****    I S  I T   A   T E X T   I T E M
      *
          IF STK-IND = 3
              ACCEPT W15-DESCR AT 0817
        WITH FOREGROUND-COLOR 0
      BACKGROUND-COLOR 7 AUTO
           ELSE
               MOVE STK-DESC     TO W15-DESCR
              DISPLAY STK-DESC AT 0817
         WITH FOREGROUND-COLOR 1
       BACKGROUND-COLOR 7.
             MOVE ZERO           TO W15-IUN.
      *
      *    ****    Q U A N T I T Y
      *
       EA45.
             ACCEPT S37 AT 0917.
           IF W15-IUN = ZERO
               GO TO EA40.
      *
      *    ****    C A L C U L A T E   I N C L U S I V E   C O S T
      *
    IF (P-TAX = "I") AND (STK-TAX > ZERO)
       COMPUTE W15-ICOST ROUNDED = W15-ICOST +
       (W15-ICOST * W05-RTE).
      *
      *    ****    U N I T   C O S T
      *
       EA50.
             ACCEPT S38 AT 1017.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO EA45
   WHEN OTHER
       CALL X"E5"
       GO TO EA50
        END-EVALUATE.
          IF W15-ICOST = ZERO
              GO TO EA50.
             DISPLAY S38 AT 1317.
      MOVE ZERO   TO W15-U-TXBL
        W15-U-NTXBL
        W15-U-VAT.
    IF STK-TAX NOT = ZERO
               IF P-TAX = "N"
     MOVE W15-ICOST
     TO W15-U-NTXBL
                                    W15-U-INCVAL
        W15-U-NETT
               ELSE
               IF P-TAX = "I"
      *
      *    ****    I N C L U S I V E   V A L U E
      *
     MOVE W15-ICOST
     TO W15-U-INCVAL
      *
      *    ****    E X C L U S I V E   V A L U E
      *
     COMPUTE W15-U-TXBL ROUNDED = W15-U-INCVAL -
                       ((W15-U-INCVAL / (100.00000 + W05-RATE))
                       * W05-RATE)
                   MOVE W15-U-TXBL
     TO W15-U-NETT
      *
      *    ****    V A T   V A L U E
      *
     SUBTRACT W15-U-TXBL FROM W15-U-INCVAL
                                 GIVING W15-U-VAT
               ELSE
      *
      *    ****    E X C L U S I V E   V A L U E
      *
     MOVE W15-ICOST
     TO W15-U-TXBL
        W15-U-NETT
      *
      *    ****    V A T   V A L U E
      *
     COMPUTE W15-U-VAT ROUNDED = W15-U-TXBL * W05-RTE
      *
      *    ****    I N C L U S I V E   V A L U E
      *
     ADD W15-U-TXBL W15-U-VAT
                                 GIVING W15-U-INCVAL
           ELSE
      *
      *    ****    N O N   T A X A B L E
      *
        MOVE W15-ICOST  TO W15-U-NTXBL
        W15-U-INCVAL
        W15-U-NETT.
      *
      *    ****    T O T A L   C O S T  ( L I N E )
      *     (W15-COST)
      *
            MULTIPLY W15-IUN BY W15-ICOST
     GIVING W15-COST ROUNDED.
             DISPLAY S40 AT 1417.
      *
      *      ***** CALCULATE COST (IN/EXCLUSIVE) PRICE AFTER DISCOUNT
      *     (W15-ICOST)
      *
             ACCEPT S35 AT 1123.
           IF W15-DISC1 = ZERO
               GO TO EA55.
             COMPUTE W25-V1 ROUNDED = (W15-COST -
             (W15-COST * W15-DSC1)).
            COMPUTE W15-ICOST ROUNDED = W25-V1 / W15-IUN.
            DISPLAY S38 AT 1317.
             MOVE W25-V1         TO W15-COST.
             DISPLAY S40 AT 1417.
      *
      *            2nd DISCOUNT
      *
            ACCEPT S36 AT 1223.
           IF W15-DISC2 = ZERO
               GO TO EA55.
             COMPUTE W25-V1 ROUNDED = (W15-COST -
             (W15-COST * W15-DSC2)).
            COMPUTE W15-ICOST ROUNDED = W25-V1 / W15-IUN.
            DISPLAY S38 AT 1317.
             MOVE W25-V1         TO W15-COST.
             DISPLAY S40 AT 1417.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   C O S T
      *     (W15-UCOST)
      *
       EA55.
    IF (P-TAX = "I") AND (STK-TAX > ZERO)
        COMPUTE W15-UCOST ROUNDED = W15-ICOST -
        (W15-ICOST / (100.0000 + W05-RATE) * W05-RATE)
    ELSE
        MOVE W15-ICOST  TO W15-UCOST.

       EA55A.
            ACCEPT S32 AT 1617.
          IF W15-USELL = ZERO OR W15-USELL < W15-UCOST
        MOVE "Retail price < cost price"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO EA55A.
             MULTIPLY W15-IUN BY W15-USELL
                                 GIVING W15-SELL.
             MOVE W15-SELL       TO W25-SVAL.
            DISPLAY W25-S5V2 AT 1717
       WITH FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7.

       EA65.
      MOVE P-TERMS  TO W15-SET.
    IF NOT (W21-MM = W05-MM)
        IF W21-CY < W05-CY
     ADD 12 W05-MM GIVING WS-IXS4
     SUBTRACT W21-MM
     FROM WS-IXS4
        ELSE
     SUBTRACT W21-MM FROM W05-MM
     GIVING WS-IXS4
        END-IF
        PERFORM EA71 THRU EA71E.
            ACCEPT W15-30 AT 1919
      WITH FOREGROUND-COLOR 0
    BACKGROUND-COLOR 7 UPDATE AUTO.
            ACCEPT W15-60 AT 1926
      WITH FOREGROUND-COLOR 0
    BACKGROUND-COLOR 7 UPDATE AUTO.
            ACCEPT W15-90 AT 1933
      WITH FOREGROUND-COLOR 0
    BACKGROUND-COLOR 7 UPDATE AUTO.
            ACCEPT W15-120 AT 1941
      WITH FOREGROUND-COLOR 0
    BACKGROUND-COLOR 7 UPDATE AUTO.
            ACCEPT W15-150 AT 1949
      WITH FOREGROUND-COLOR 0
    BACKGROUND-COLOR 7 UPDATE AUTO.
            ACCEPT W15-180 AT 1957
      WITH FOREGROUND-COLOR 0
    BACKGROUND-COLOR 7 UPDATE AUTO.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA40.
      MOVE ZERO   TO W15-IVAT.
    IF STK-TAX > ZERO
        IF P-TAX = "I"
      *
      *    ****    I N C L U S I V E
      *     (W15-INCVAL)
      *
     ADD W15-COST  TO W15-INCVAL
     COMPUTE W15-IVAT ROUNDED = W15-COST / (100.0000
     + W05-RATE) * W05-RATE
     COMPUTE W15-TXBL ROUNDED = W15-TXBL +
     (W15-COST - W15-IVAT)
        ELSE
      *
      *    ****    E X C L U S I V E
      *     (W15-TXBL)
      *
     COMPUTE W15-IVAT ROUNDED = W15-COST * W05-RTE
     ADD W15-COST  TO W15-TXBL.
      *
      *    ****    T O T A L   C O S T  ( D / N O T E )
      *     (W15-TCOST)
      *
    IF NOT (WS-VAT-SUB = ZERO)
        ADD W15-TXBL  TO W15-TAXVAL(WS-VAT-SUB)
        ADD W15-IVAT  TO W15-VATVAL(WS-VAT-SUB).
      ADD W15-COST  TO W15-TCOST.
             MOVE WS-PURLAST     TO WS-PURKEY.
             INITIALIZE PUR-RECORD1.
             MOVE W15-SETT       TO PUR-TERM.
            MOVE W10-EXT-ITEM  TO PUR-EXT-STOCK.
             MOVE W15-IUN        TO PUR-IUN.
      *      MOVE W15-ICOST  TO PUR-ICOST.
            MOVE W15-UCOST  TO PUR-ICOST.
            MOVE W15-ICOST  TO PUR-INCVAL.
             MOVE W15-USELL      TO PUR-USELL.
             MOVE W15-COST       TO PUR-COST.
             MOVE W15-SELL       TO PUR-SELL.
            PERFORM WRITE-PURCH THRU WRITE-PURCH-EXIT.
      *                                DISPLAY RUNNING TOTAL.
             DISPLAY S44 AT 2066.

       EA70.
            DISPLAY S42B.

       EA70-CHK.
            MOVE "'N'ext Item or 'E'nd  [N]"
     TO WS-ERR-MES.
      MOVE "N"   TO WS-OPTION.
       EA70A.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "N" OR "E")
        GO TO EA70-CHK.
    IF NOT (WS-OPTION = "N" OR "E")
               GO TO EA70A.
           IF WS-OPTION = "E"
               GO TO EA72.
             ADD 1               TO WS-PURLAST.
             GO TO EA38.
       EA71.
      MOVE W15-L5  TO W15-F5.
      MOVE ZERO   TO W15-180.
      SUBTRACT 1   FROM WS-IXS4.
    IF WS-IXS4 > ZERO
        GO TO EA71.
       EA71E.
      EXIT.
       EA72.
      PERFORM CLEAR-L50.
           IF WS-PURLAST = ZERO
               GO TO EA10.
       EA73.
            MOVE "P"      TO WS-OPTION.
      MOVE SPACES     TO WS-ERR-MES.
            STRING "'P'rocess, 'C'ancel or 'M'ore items  ["
      DELIMITED SIZE
      WS-OPTION DELIMITED SIZE
      "]" DELIMITED SIZE
        INTO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "C" OR "P" OR "M")
        GO TO EA73.
           IF WS-OPTION = "C"
               GO TO EA10.
           IF WS-OPTION = "M"
               GO TO EA70-CHK.
       EA74.
      MOVE 1   TO WS-S9.
      MOVE ZERO   TO W15-VAT.

       EA74-TAX.
      ADD W15-VATVAL(WS-S9)
     TO W15-VAT.
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO EA74-TAX.
      MOVE W15-VAT  TO WS-VAT.
      *      COMPUTE W15-VAT ROUNDED = W15-TXBL * W05-RTE.
    IF P-TAX = "E"
        ACCEPT S43 AT 2166.
    IF P-TAX = "E"
        ADD W15-TCOST W15-VAT
     GIVING W25-SVAL
    ELSE
        MOVE W15-TCOST  TO W25-SVAL.
      DISPLAY W25-S5V2 AT 2266
       WITH FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA74.
             MOVE ZERO           TO WS-PURKEY.
            DISPLAY "Processing Debit Note" AT 5012 WITH
              BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
      *
      *    ****    Start Recovery Logging.
      *
            PERFORM AY60 THRU AY999.
      *
      *    ****    Lock Creditor record and Log to Recovery file
      *
             MOVE 0              TO WS-ACTION.
             PERFORM AY07 THRU AY59.
      *                                PRINT DEBIT NOTE HEADING.
       EA75.
             MOVE WS-HCRED       TO DS-H4.
             MOVE "D/NOTE NO:"   TO DS-H5.
             MOVE WS-H1          TO DS-H6 DS-H8.
             MOVE "INVOICE NO:"  TO DS-H7.
             MOVE W15-ACNO       TO DS-ACNO.
             MOVE W15-NAME       TO DS-NAME.
             MOVE W15-GRNN       TO DS-DNO.
             MOVE W15-GRND       TO DS-DNDTE.
             MOVE W15-INVN       TO DS-INV.
             MOVE W15-INVD       TO DS-IDTE.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE "ITEM CODE"    TO DS-H9.
             MOVE "DESCRIPTION"  TO DS-H10.
             MOVE "UNT COST"     TO DS-H11.
             MOVE "UNT-SELL"     TO DS-H12.
             MOVE "QUANT"        TO DS-H13.
             MOVE "TOT COST"     TO DS-H14.
             MOVE "TOT SELL"     TO DS-H15.
             MOVE "30  60  90 120 150 180" TO DS-H16.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       EA80.
           IF WS-PURKEY NOT < WS-PURLAST
               GO TO EA125.
             ADD 1               TO WS-PURKEY.
             PERFORM READ-PURCH THRU READ-PURCH-EXIT.
      *
      *    ****    Lock Stock record and Log to Recovery file
      *
            MOVE PUR-EXT-STOCK  TO STK-CODE.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE PUR-IUN        TO W15-IUN.
            MOVE PUR-INCVAL  TO W15-ICOST.
            MOVE PUR-ICOST  TO W15-UCOST.
            MOVE PUR-USELL  TO W15-USELL.
             MOVE PUR-COST       TO W15-COST.
             MOVE PUR-SELL       TO W15-SELL.
            MOVE STK-CODE  TO W10-EXT-ITEM.
            MOVE STK-DESC  TO W15-DESC.
             MOVE PUR-TERM       TO W15-SETT.
      *
      *    ****    U P D A T E   S T O C K   R E C O R D
      *
            MULTIPLY W15-IUN BY W15-UCOST
                                 GIVING W25-V2 ROUNDED.
          IF STK-IND > 1
               GO TO EA85.
             MULTIPLY STK-QUANT BY STK-AVGE
                                 GIVING W25-V1.
             SUBTRACT W15-IUN    FROM STK-QUANT.
           IF STK-QUANT > ZERO
               SUBTRACT W25-V2   FROM W25-V1
               DIVIDE W25-V1 BY STK-QUANT
                                 GIVING STK-AVGE.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      MOVE P-NUMBER  TO PRD-SUPP.
      MOVE W10-EXT-ITEM  TO PRD-EXT-CODE.
      MOVE W05-DATE-YMD  TO PRD-DTE.
      ADD 1   TO PRD-SEQ.
      MOVE "D"   TO PRD-TRAN.
    IF W15-GRNN NOT = SPACES
              MOVE W15-GRNN  TO PRD-REF
           ELSE
        MOVE W15-INVN  TO PRD-REF.
            MULTIPLY -1 BY W15-IUN
     GIVING PRD-QNT.
      MULTIPLY -1 BY W15-COST
     GIVING PRD-COST.
      MOVE W15-NAME  TO PRD-NAME.
      *      MOVE W15-WHS  TO PRD-WHSE.
      PERFORM WRITE-PURDEX THRU WRITE-PURDEX-EXIT
            MOVE 1   TO WS-ACTION
      PERFORM AY056 THRU AY59.
      *
      *    ****    U P D A T E   D E P A R T M E N T   C O S T
      *
       EA85.
            MOVE STK-LDG  TO DPT-CODE.
             PERFORM AY38 THRU AY59.
             ADD W25-V2          TO DPT-PRET-MTD
                                    DPT-PRET-YTD
                                    DPT-PRET-DAY.
           IF WX-INT = "Y"
        MOVE W20-DTE  TO WX-DATE
        MOVE STK-LDG  TO WX-LEDG
               MOVE "RETURNS - CREDITORS"
                                 TO WX-NAR
               COMPUTE WX-VALUE = W25-V2 * -1
               MOVE DPT-PUR-GL   TO WX-AC
               MOVE 07           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
            PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *
      *    ****    U P D A T E   S T O C K   C O N T R O L
      *
             SUBTRACT W25-V2     FROM STK-TOTAL.
             MOVE ZERO           TO WS-S.
             MOVE 1              TO WS-S2.
      *                                CALCULATE AGEING.
       EA90.
            MOVE ZERO   TO W25-CFLOW(WS-S2).
          IF W15-MONTH(WS-S2) > ZERO
               ADD 1             TO WS-S.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO EA90.
             MOVE ZERO           TO W25-V1 W25-V2.
             MOVE 1              TO WS-S3.
           IF WS-S < 2
               MOVE W15-COST     TO W25-V2
               GO TO EA95.
             MOVE W15-COST       TO W20-RESULT.
             DIVIDE W20-RESULT BY WS-S
                                 GIVING W25-V1 ROUNDED.
             SUBTRACT 1          FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1               TO WS-S.
       EA95.
             MULTIPLY -1         BY W25-V1.
             MULTIPLY -1         BY W25-V2.
           IF WS-S = ZERO
               GO TO EA120.
       EA100.
          IF W15-MONTH(WS-S3) = ZERO
               ADD 1             TO WS-S3
               GO TO EA100.
           IF WS-S > 1
              MOVE W25-V1  TO W25-CFLOW(WS-S3)
               SUBTRACT 1        FROM WS-S
               ADD 1             TO WS-S3
               GO TO EA100.
            MOVE W25-V2  TO W25-CFLOW(WS-S3).
             MOVE ZERO           TO W25-V1 W25-V2.
             MOVE 1              TO WS-S2.
       EA105.
          IF W25-CFLOW(WS-S2) = ZERO
               GO TO EA110.
             ADD W05-MM WS-S2    GIVING WS-S3.
           IF WS-S3 > 12
               SUBTRACT 12       FROM WS-S3.

       EA110.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO EA105.
           IF W25-V2 NOT = ZERO
               GO TO EA120.
             MOVE 1              TO WS-S WS-S2.

       EA115.
          IF W15-MONTH(WS-S) NOT = ZERO
               ADD WS-S2         TO CT-AGE.
           IF WS-S < 6
               MULTIPLY 2        BY WS-S2
               ADD 1             TO WS-S
               GO TO EA115.
      *                                ACCUMULATE TOTALS.
       EA120.
             ADD W15-SELL        TO W15-TSELL.
            ADD W25-CFLOW(1)  TO W30-30.
            ADD W25-CFLOW(2)  TO W30-60.
            ADD W25-CFLOW(3)  TO W30-90.
            ADD W25-CFLOW(4)  TO W30-120.
            ADD W25-CFLOW(5)  TO W30-150.
            ADD W25-CFLOW(6)  TO W30-180.
             ADD W25-V2          TO W30-CUR.
      *
      *      ****    P R I N T   I T E M   D E T A I L S
      *
            MOVE W10-EXT-ITEM  TO DS-ITEM.
            MOVE W15-DESC  TO DS-DESC.
            MOVE W15-ICOST  TO DS-UCOST.
             MOVE W15-USELL      TO DS-USELL.
             MOVE W15-IUN        TO DS-QNT.
             MOVE W15-COST       TO DS-COST.
             MOVE W15-SELL       TO DS-SELL.
          IF W15-MONTH(1) > ZERO
               MOVE "Y"          TO DS-30
           ELSE
               MOVE "N"          TO DS-30.
          IF W15-MONTH(2) > ZERO
               MOVE "Y"          TO DS-60
           ELSE
               MOVE "N"          TO DS-60.
          IF W15-MONTH(3) > ZERO
               MOVE "Y"          TO DS-90
           ELSE
               MOVE "N"          TO DS-90.
          IF W15-MONTH(4) > ZERO
               MOVE "Y"          TO DS-120
           ELSE
               MOVE "N"          TO DS-120.
          IF W15-MONTH(5) > ZERO
               MOVE "Y"          TO DS-150
           ELSE
               MOVE "N"          TO DS-150.
          IF W15-MONTH(6) > ZERO
               MOVE "Y"          TO DS-180
           ELSE
               MOVE "N"          TO DS-180.
          IF W02-LINAGE < (W02-PRN-LENGTH - 2)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM EA05
               PERFORM EA75.
             MOVE SPACES         TO R-DET.
             GO TO EA80.
      *                                DEBIT NOTE TOTAL.
       EA125.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "SUB-TOTAL"  TO DS-H13.
             MOVE W15-TCOST      TO DS-COST.
             MOVE W15-TSELL      TO DS-SELL.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE "VAT"          TO DS-H13.
             MOVE W15-VAT        TO DS-COST.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             ADD W15-TCOST       TO W15-TOTAL.
      ADD W15-VAT  TO W15-TOTVAT.
    IF P-TAX = "I"
       SUBTRACT W15-VAT  FROM W15-TOTAL.
    IF P-TAX = "E"
        ADD W15-VAT  TO W15-TCOST.
             MOVE "TOTAL"        TO DS-H13.
             MOVE W15-TCOST      TO DS-COST.
          IF W02-LINAGE < (W02-PRN-LENGTH - 10)
        MOVE 3   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
             MOVE SPACES         TO R-DET.
      *
      *    ****    G E N E R A T E   T R A N S A C T I O N
      *
       EA130.
      INITIALIZE CT-REC.
            MOVE W15-GRND  TO W15-DEL.
      MOVE W15-DEL-DD  TO W21-DD.
      MOVE W15-DEL-MM  TO W21-MM.
      MOVE W15-DEL-CC  TO W21-CC.
      MOVE W15-DEL-YY  TO W21-YY.
      MOVE W21-DTE  TO CT-DTE.
    IF W15-GRNN NOT = SPACES
               MOVE W15-GRNN     TO CT-REF
           ELSE
               MOVE W15-INVN     TO CT-REF.
             MOVE W15-INVN       TO CT-SINV.
            MOVE W15-INVD  TO W15-DEL.
      MOVE W15-DEL-DD  TO W21-DD.
      MOVE W15-DEL-MM  TO W21-MM.
      MOVE W15-DEL-CC  TO W21-CC.
      MOVE W15-DEL-YY  TO W21-YY.
      MOVE W21-DTE  TO CT-SDTE.
      MOVE "U"   TO CT-TYPE.
      MOVE "I"   TO CT-PJ.
            MOVE W15-ACNO  TO CT-CRED.
      MOVE SPACES  TO CT-ORD.
            MOVE ZERO   TO CT-PAID
        CT-SEQ.
             MULTIPLY -1 BY W15-TCOST
                                 GIVING CT-COST.
             MULTIPLY -1 BY W15-TSELL
                                 GIVING CT-SELL.
             MULTIPLY -1 BY W15-VAT
                                 GIVING CT-TAX
             MOVE 02             TO CT-CODE.
            MOVE ZERO   TO CT-AGE W15-SETT.
      COMPUTE CT-TAXFREE = CT-COST -
   (CT-TAX + (W15-TXBL * -1)).
      MOVE SPACES  TO CT-DREF.
      PERFORM CA600.
           IF WX-INT = "Y"
               MOVE W20-DTE      TO WX-DATE
        MOVE "  CR"  TO WX-LEDG
        MOVE W90-CODE  TO WX-TRN
               MOVE W00-C-DESC   TO WX-DESC
               MOVE "- CREDITORS"
                                 TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL     TO WX-AC
               MOVE 02           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM CA700.
           IF W30-30 NOT = ZERO
               MOVE 1            TO W15-30.
           IF W30-60 NOT = ZERO
               MOVE 1            TO W15-60.
           IF W30-90 NOT = ZERO
               MOVE 1            TO W15-90.
           IF W30-120 NOT = ZERO
               MOVE 1            TO W15-120.
           IF W30-150 NOT = ZERO
               MOVE 1            TO W15-150.
           IF W30-180 NOT = ZERO
               MOVE 1            TO W15-180.
             MOVE 1              TO WS-S WS-S2.
       EA140.
          IF W15-MONTH(WS-S) NOT = ZERO
               ADD WS-S2         TO CT-AGE.
           IF WS-S < 6
               ADD 1             TO WS-S
               MULTIPLY 2        BY WS-S2
               GO TO EA140.
       EA145.
             ADD CT-COST         TO P-BAL.
             ADD CT-COST         TO P-DT.
             ADD W30-180         TO P-180D.
           IF P-180D < ZERO
               SUBTRACT P-180D   FROM W30-180
               ADD P-180D        TO W30-150
               MOVE ZERO         TO P-180D.
             ADD W30-150         TO P-150D.
           IF P-150D < ZERO
               SUBTRACT P-150D   FROM W30-150
               ADD P-150D        TO W30-120
               MOVE ZERO         TO P-150D.
             ADD W30-120         TO P-120D.
           IF P-120D < ZERO
               SUBTRACT P-120D   FROM W30-120
               ADD P-120D        TO W30-90
               MOVE ZERO         TO P-120D.
             ADD W30-90          TO P-90D.
           IF P-90D < ZERO
               SUBTRACT P-90D    FROM W30-90
               ADD P-90D         TO W30-60
               MOVE ZERO         TO P-90D.
             ADD W30-60          TO P-60D.
           IF P-60D < ZERO
               SUBTRACT P-60D    FROM W30-60
               ADD P-60D         TO W30-30
               MOVE ZERO         TO P-60D.
             ADD W30-30          TO P-30D.
           IF P-30D < ZERO
               SUBTRACT P-30D    FROM W30-30
               ADD P-30D         TO W30-CUR
               MOVE ZERO         TO P-30D.
             ADD W30-CUR         TO P-OUT.
             PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
      MOVE 1   TO WS-ACTION.
      PERFORM AY08 THRU AY59.
            PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
      *
      *    ****    U P D A T E   C R E D I T O R   C O N T R O L
      *
             MULTIPLY -1         BY W15-TCOST.
             ADD W15-TCOST       TO W70-CLOSE.
             ADD W15-TCOST       TO W70-DEBT.
             ADD W30-180         TO W70-DUE-180.
             ADD W30-150         TO W70-DUE-150.
             ADD W30-120         TO W70-DUE-120.
             ADD W30-90          TO W70-DUE-90.
             ADD W30-60          TO W70-DUE-60.
             ADD W30-30          TO W70-DUE-30.
             ADD W30-CUR         TO W70-DUE-NOW.
      *
      *    ****    VAT Control
      *
      MOVE 1   TO W05-V-RATE.

       EA146.
      ADD 6 W05-V-RATE  GIVING WS-VAT-SUB.
    IF (W15-VATVAL(W05-V-RATE) = ZERO) AND
       (W15-VATVAL(WS-VAT-SUB) = ZERO)
        GO TO EA147.
      MOVE W05-VAT-CODE  TO DPT-CODE.
      MOVE 0   TO WS-ACTION.
             PERFORM AY38 THRU AY59.
      ADD W15-VATVAL(W05-V-RATE)
     TO DPT-I-VRET-DAY
        DPT-I-VRET-MTD
        DPT-I-VRET-YTD.
      ADD W15-VATVAL(WS-VAT-SUB)
     TO DPT-I-VRET-DAY
        DPT-I-VRET-MTD
        DPT-I-VRET-YTD.
      ADD W15-TAXVAL(W05-V-RATE)
     TO DPT-V-PRET-DAY
        DPT-V-PRET-MTD
        DPT-V-PRET-YTD.
      ADD W15-TAXVAL(WS-VAT-SUB)
     TO DPT-V-PRET-DAY
        DPT-V-PRET-MTD
        DPT-V-PRET-YTD.
            PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       EA147.
    IF W05-V-RATE < 6
        ADD 1   TO W05-V-RATE
        GO TO EA146.
    IF (WX-INT = "Y") AND (W15-VAT NOT = ZERO)
        COMPUTE WX-VALUE = W15-VAT * -1
        MOVE DPT-I-VAT-GL TO WX-AC
        MOVE 12   TO WX-TYPE
        PERFORM GL-TRF-UPDATE.
      *
      *    ****    End Recovery Logging.
      *
       EA148.
      CLOSE CREDIT.
      OPEN I-O CREDIT.
            PERFORM AY70 THRU AY999.
            CLOSE PURCH.
             OPEN OUTPUT PURCH.
             CLOSE PURCH.
            OPEN I-O PURCH.
             GO TO EA10.

       EA999.
             EXIT.
      /
       ZA000-INIT    SECTION 89.
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
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             OPEN OUTPUT PURCH.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
             CLOSE PURCH.
             OPEN I-O PURCH.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W60-COMPANY.
             MOVE PAR-DMY        TO W05-DATE.
             MOVE PAR-YMD        TO W05-DATE-YMD.
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
            MOVE SPACES  TO WS-MD-LNE WS-MID-LNE.
       ZA60.
            MOVE WS-G1   TO WS-TCHR(WS-S1) WS-BCHR(WS-S1)
        WS-TCR(WS-S1) WS-BCR(WS-S1).
            MOVE WS-G8   TO WS-TCH(WS-S1) WS-BCH(WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
            MOVE WS-G2   TO WS-TCHR(18) WS-TCHR(49)
              WS-TCHR(60) WS-TCHR(70)
              WS-TCR(8) WS-TCR(50)
              WS-TCR(66).
            MOVE WS-G3   TO WS-MCHR(1) WS-MCHR(80)
        WS-MCHR(18) WS-MCHR(49)
              WS-MCHR(60) WS-MCHR(70)
              WS-MCR(8) WS-MCR(50)
              WS-MCR(66) WS-MCR(1)
        WS-MCR(80).
            MOVE WS-G4   TO WS-BCHR(18) WS-BCHR(49)
              WS-BCHR(60) WS-BCHR(70)
              WS-BCR(8) WS-BCR(50)
              WS-BCR(66).
            MOVE WS-G9   TO WS-TCH(1).
            MOVE WS-G10  TO WS-TCH(80).
            MOVE WS-G11  TO WS-BCH(1).
            MOVE WS-G12  TO WS-BCH(80).
            MOVE WS-G14  TO WS-TCR(1) WS-BCR(1)
        WS-TCHR(1) WS-BCHR(1).
            MOVE WS-G13  TO WS-TCR(80) WS-BCR(80)
        WS-TCHR(80) WS-BCHR(80).
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
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-I-O-ERROR SECTION 90.

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
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 22         MOVE W02-STOCKF  TO WS-FILE
                               MOVE STK-CODE    TO WS-KEYX
               WHEN 32         MOVE W02-TXTRAN  TO WS-FILE
                               MOVE TAX-KEY     TO WS-KEYX
               WHEN 35         MOVE W02-CRETRN  TO WS-FILE
                               MOVE CRT-KEY  TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF  TO WS-FILE
                               MOVE XFR-KEY     TO WS-KEYX
               WHEN 56         MOVE W02-PURDEX  TO WS-FILE
                               MOVE PRD-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
