      $set LINKCOUNT"256" GNT"CRPEXP.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   ******* ********  ** **  *******    *
      *   ** **  **   **  **    ** **    **  **   **   **   *
      *   **     **   **  **    ** **     ****    **   **   *
      *   **     *******   ******* *****      **     *******    *
      *   **     **  **   ** **     ****    **        *
      *   ** **  **   **  ** **    **  **   **        *
      *    ******   **   **  ** ********  ** **  **        *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   E X P O R T   P R O G R A M        *
      *                                                                *
      *     VERSION 9.00.00 - August 2012          *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CRPEXP.
       AUTHOR.         J W LEMMON (CSS).
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
                       CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "PARAM.SL".

       COPY "CREDIT.SL".

           SELECT CRDDET  ASSIGN W02-CRDDET
                          STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

       COPY "CRDMEM.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "PARAM.FDE".

       COPY "CREDIT.FDE".

       FD  CRDDET      LABEL RECORD OMITTED.
       01  EXP-REC1.
           03  EXP-DATA     PIC  X(450).

       COPY "CRDMEM.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-SUB     PIC 9(04)   COMP-5.
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
       77  WS-SELKEY        PIC 9(06)     COMP-5.
       77  WS-SPAKEY        PIC 9(06)     COMP-5.
       77  WS-PARKEY        PIC 9(06)     COMP-5.
       77  WS-DEPKEY        PIC 9(06)     COMP-5.
       77  WS-NETKEY        PIC 9(06)     COMP-5.
       77  WS-SINKEY        PIC 9(06)     COMP-5.
       77  WS-RECKEY        PIC 9(06)     COMP-5.
       77  WS-RECOVER       PIC 9(06)     COMP-5.
       77  WS-TRANS         PIC 9(04)     COMP-5 VALUE 1.
       77  WS-KEYSTR        PIC 9(04)     COMP-5.
       77  WS-GST-RATE      PIC S9(03)V99 COMP-3.
       77  WS-MONTHS        PIC S9(01)V99 COMP-3.
       77  WS-KEY1          PIC 9(04)     COMP-5.
       77  WS-KSTORE        PIC 9(04)     COMP-5.
       77  WS-ITM           PIC X(14).
       77  WS-ETYPE         PIC X(01).
       77  WS-VAT-GST       PIC X(01).
       77  WS-PAGE          PIC 9(04)     COMP-5.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-8LPI         PIC  X(04).
       77  WS-6LPI         PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
       77  WS-OPTION        PIC X(01).
       77  WS-SKIP          PIC X(01).
       77  WS-ADJ           PIC X(01).
       77  WS-ERROR         PIC 9(01).
       77  WS-IND           PIC 9(01)     VALUE 0.
       77  WS-INDB          PIC 9(01)     VALUE 0.
       77  WS-INDP          PIC 9(01)     VALUE 0.
       77  WS-INDS          PIC 9(01)     VALUE 0.
       77  WS-AMEND         PIC 9(01)     VALUE 0.
       77  WS-CONREC        PIC 9(01) VALUE ZERO.
       77  WS-LEDG          PIC 9(03).
       77  WS-LEDG1         PIC 9(03).
       77  WS-SELECT        PIC X(13)     VALUE "Select Option".
       77  WS-CHAR1         PIC X(01).
       77  WS-CHAR2         PIC X(01).
       77  WS-COMP          PIC 9(01).
       77  WS-WEEK          PIC 9(01).
       77  WS-TYPE          PIC X(01).
       77  WS-INDEX-STORE   PIC X(14).
       77  WS-INDEX-NAME    PIC X(14).
       77  WS-BATCH         PIC 9(05).
       77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
       77  WS-PASS          PIC X(08).
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
          03  WS-B-LNE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
          03  WS-TP-LNE.
               05  WS-TCR  PIC X(01) OCCURS 80.
    03  WS-TPCR-LNE REDEFINES WS-TP-LNE.
        05  WS-TCR1     PIC  X(01).
        05  WS-TCR78    PIC  X(78).
        05  WS-TCR80    PIC  X(01).
          03  WS-MD-LNE.
               05  WS-MCR  PIC X(01) OCCURS 80.
    03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
        05  WS-MCR1     PIC  X(01).
        05  WS-MCR78    PIC  X(78).
        05  WS-MCR80    PIC  X(01).
          03  WS-BT-LNE.
               05  WS-BCR  PIC X(01) OCCURS 80.
    03  WS-BTCR-LNE REDEFINES WS-BT-LNE.
        05  WS-BCR1     PIC  X(01).
        05  WS-BCR78    PIC  X(78).
        05  WS-BCR80    PIC  X(01).
    03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
    03  WS-HME-LINE PIC X(60) VALUE ALL X"CD".

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "CRP".
    03  WS-PROG        PIC  X(03) VALUE "EXP".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
    03  W02-CRDDET.
        05  W02-EXPORT PIC X(64) VALUE "CREDIT".

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables".

       01  W15-INPUT-AREA.
           03  W15-DATE      PIC  X(08).
           03  W15-DATE-CHAR REDEFINES W15-DATE
                             PIC  X(01)     OCCURS 8.
          03  W15-DTE        PIC 9(06).
          03  W15-RDATE       REDEFINES W15-DTE.
               05  W15-DD      PIC 9(02).
               05  W15-MM      PIC 9(02).
               05  W15-YY      PIC 9(02).
          03  W15-ACNO      PIC  X(06).
           03  W15-SNO       REDEFINES W15-ACNO.
               05  W15-APREF PIC  X(01).
               05  W15-SERIAL
        PIC  X(06).
           03  W15-ITEM      PIC  9(05).
           03  W15-I-CODE    REDEFINES W15-ITEM.
               05  W15-PREFIX
                             PIC  9(03).
               05  W15-TYPE  PIC  9(02).
           03  W15-NAME.
               05  W15-NCHAR PIC  X(01) OCCURS 40.
           03  W15-N         REDEFINES W15-NAME.
               05  W15-DESC.
                   07  W15-ENTRY
                             PIC  X(08).
                   07  FILLER
                             PIC  X(16).
               05  FILLER    PIC  X(16).
           03  W15-UN        PIC  9(05).
           03  W15-VALUE     PIC  9(07)V99.
           03  W15-VAL2      PIC  9(07)V99.
           03  W15-ORD         PIC 9(10).
           03  W15-ONO         REDEFINES W15-ORD.
               05  W15-OPREF   PIC 9(08).
               05  W15-OSUB    PIC 9(02).
           03  W15-DEL         PIC 9(06).
           03  W15-DEL-DMY REDEFINES W15-DEL.
               05  W15-DEL-DD  PIC 9(02).
               05  W15-DEL-MM  PIC 9(02).
               05  W15-DEL-YY  PIC 9(02).
           03  W15-STYLE     PIC  X(08).
           03  W15-COST      PIC S9(07)V99 COMP-3.
           03  W15-SELL      PIC S9(07)V99 COMP-3.
           03  W15-SETT      PIC  9(06).
           03  W15-SET       REDEFINES W15-SETT.
               05  W15-30    PIC  9(01).
               05  W15-60    PIC  9(01).
               05  W15-90    PIC  9(01).
               05  W15-120   PIC  9(01).
               05  W15-150   PIC  9(01).
               05  W15-180   PIC  9(01).
           03  W15-PAY       REDEFINES W15-SETT.
               05  W15-MONTH PIC  9(01)     OCCURS 6.
           03  W15-ADDRESS   PIC  X(120).
           03  W15-ADD  REDEFINES W15-ADDRESS.
               05  W15-ADDR1 PIC  X(30).
               05  W15-ADDR2 PIC  X(30).
               05  W15-ADDR3 PIC  X(30).
               05  W15-ADDR4 PIC  X(30).
           03  W15-ADD2 REDEFINES W15-ADDRESS.
               05  W15-ACHAR PIC  X(01) OCCURS 120.
           03  W15-PCODE     PIC  9(04).
           03  W15-CONTACT   PIC  X(30).
           03  W15-TEL       PIC  X(14).
           03  W15-FAX       PIC  X(14).
           03  W15-TAX       PIC  X(01).
           03  W15-TERMS     PIC  X(01).
           03  W15-PMNT      PIC  X(12).

       01  W40-WORK.
          03  W40-NAME1.
               05  W40-NAME.
                   07  W40-ENTRY PIC X(08).
                   07  FILLER  PIC X(32).
          02  W40-ADDRESS1.
               03  W40-ADD1    PIC X(30).
               03  W40-ADD2    PIC X(30).
               03  W40-ADD3    PIC X(30).
               03  W40-ADD4    PIC X(30).
           02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
               03  W40-ACHAR OCCURS 120 PIC X(01).
           02  W40-POST        PIC 9(04).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W55-EXPORT.
           03  W55-S1          PIC  9(04) COMP-5.
           03  W55-EXP-DATA.
               05  W55-EXP     PIC  X(01) OCCURS 450.

       01  W60-FIELDS.
           03  W60-S           PIC  9(02) COMP-5.
          03  W60-FNO        PIC  9(02) COMP-5 OCCURS 30.

       01  W65-FIELDS.
           03  W65-S           PIC  9(02) COMP-5.
           03  W65-FIELD       PIC  X(22).
           03  W65-FLDS.
               05  FILLER      PIC X(21) VALUE "Account Number".
              05  FILLER      PIC X(21) VALUE "Creditor Name".
              05  FILLER      PIC X(21) VALUE "Address".
              05  FILLER      PIC X(21) VALUE "Terms".
              05  FILLER      PIC X(21) VALUE "Telephone".
              05  FILLER      PIC X(21) VALUE "Fax Number".
              05  FILLER      PIC X(21) VALUE "Contact Person".
              05  FILLER      PIC X(21) VALUE "Outstanding Balance".
              05  FILLER      PIC X(21) VALUE "M.T.D. Debits".
              05  FILLER      PIC X(21) VALUE "M.T.D. Credits".
              05  FILLER      PIC X(21) VALUE "Previous Balance".
        05  FILLER      PIC X(21) VALUE "Amount Due".
              05  FILLER      PIC X(21) VALUE "Cash Flow 30 Days".
              05  FILLER      PIC X(21) VALUE "Cash Flow 60 Days".
              05  FILLER      PIC X(21) VALUE "Cash Flow 90 Days".
              05  FILLER      PIC X(21) VALUE "Cash Flow 120 Days".
              05  FILLER      PIC X(21) VALUE "Cash Flow 150 Days".
              05  FILLER      PIC X(21) VALUE "Cash Flow 180 Days".
              05  FILLER      PIC X(21) VALUE "V.A.T. Type".
              05  FILLER      PIC X(21) VALUE "Payment Method".
           03  W65-RFLDS REDEFINES W65-FLDS.
        05  W65-FLD     PIC  X(21) OCCURS 20.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
    03  W75-CNO       PIC  X(05) OCCURS 18.

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
               05  W80-S7V2   PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
               05  W80-DATE   PIC Z9/99/99.
           03  W80-QNT.
               05  W80-5      PIC Z(04)9.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).
       01  W90-BAL             PIC S9(07)V99 COMP-3.
       01  W95-COMP            PIC X(40).
       01  W100-EDIT.
           03  W100-1.
               05  W100-N      PIC  9(01).
           03  W100-2.
               05  W100-N2     PIC  9(02).
           03  W100-3.
               05  W100-NO1    PIC  9(03).
    03  W100-4.
        05  W100-N4     PIC  9(04).
    03  W100-9.
               05  W100-NO2    PIC  Z(08)9.
           03  W100-S9.
        05  W100-NO3    PIC  -(09)9.
           03  W100-S9M REDEFINES W100-S9.
        05  W100-QNT    PIC  -(09)9.
           03  W100-PRICE.
               05  W100-S6V2   PIC  Z(05)9.99.
           03  W100-S9V2.
        05  W100-V1     PIC  -(09)9.99.
           03  W100-9V2.
               05  W100-V2     PIC  Z(08)9.99.
           03  W100-9V3.
               05  W100-V3     PIC  Z(08)9.999.
           03  W100-3V2.
               05  W100-V4     PIC  Z(02)9.99.
           03  W100-DATE.
               05  W100-DTE    PIC  9(06).
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

       COPY "FILE.IDS".

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

       01  S-PROG.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "CREDITOR EXPORT PROGRAM".

       01  S-MENU.
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 52 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
     VALUE "¿".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³    ".
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "EXPORT OPTIONS".
    03        COLUMN 48 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "    ³".
    03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " No, Name & Address ³".
    03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " All Fields      ³".
          03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "3".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Selected Fields    ³".
          03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "4".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Field Separator    ³".
          03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Exit               ³".
    03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "À".
    03        COLUMN 30 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
     VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03.
           03  LINE  4 COLUMN  2 VALUE
        "Export: Account No., Name and Address".

       01  S04.
           03  LINE  4 COLUMN  2 VALUE "Export all the fields".

       01  S05.
           03  LINE  4 COLUMN  2 VALUE "Select the fields required".
           03  LINE  6 COLUMN  2 VALUE "Field :".

       01  S06.
           03  LINE  8 COLUMN  2 VALUE "Fields:".
    03        COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (1).
    03        COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (2).
    03        COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (3).
    03        COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (4).
    03        COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (5).
    03        COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (6).
    03        COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (7).
    03        COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (8).
    03        COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (9).
    03        COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (10).
    03        COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (11).
    03        COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (12).
    03        COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (13).
    03        COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (14).
    03        COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (15).
    03        COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (16).
    03        COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (17).
    03        COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (18).
    03        COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (19).
    03        COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (20).
    03        COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (21).
    03        COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (22).
    03  LINE 10 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (23).
    03        COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (24).
    03        COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (25).
    03        COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (26).
    03        COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (27).
    03        COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (28).
    03        COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (29).
    03        COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (30).

       01  S07.
           03  LINE 16 COLUMN  2 VALUE "Exporting:".

       01  S08.
    03  LINE 16 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(05) FROM P-NUMBER.
    03        COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(40) FROM P-NAME.

       01  S09.
    03  LINE  6 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC 99 FROM W65-S.
    03        COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC  X(21) FROM W65-FIELD.
       01  S10.
    03  LINE  4 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
     VALUE "Change Field Separator".
    03  LINE  6 COLUMN  2 VALUE "Character Used:".

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
    "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".
    03        COLUMN 78 FOREGROUND-COLOR 0
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
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-CRLEV < 9
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
           IF WS-STATUS NOT = "00"
               PERFORM OPEN-ERROR.
      PERFORM BA000.
             GO TO AZ000-EOJ.
       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
      PERFORM SAVE-SCREEN.
            MOVE 4   TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM CREDITOR-LOOKUP.
      PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY "FUNCTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

      *
      *    ****    G E T   T H E   E X P O R T   F I L E   I D
      *
       AA100           SECTION.
       AA101.
      DISPLAY "Keyin the <" AT 2202
       "pathname" WITH FOREGROUND-COLOR 6 HIGHLIGHT ">/<"
       "filename" WITH FOREGROUND-COLOR 6 HIGHLIGHT ">".
             DISPLAY "EXPORT file :" AT 2302.
             MOVE ZERO           TO WS-ERROR.
       AA105.
             ACCEPT W02-EXPORT AT 2316
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO AA199
   WHEN OTHER
       CALL X"E5"
       GO TO AA105
        END-EVALUATE.
      MOVE 64   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W02-EXPORT
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
             MOVE 1              TO WS-LENGTH.
             DISPLAY W02-EXPORT AT 2316
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CLEAR-L50.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO AA105.
      OPEN OUTPUT CRDDET.
           IF WS-STATUS = "00"
        PERFORM AA600
               GO TO AA199.
      MOVE "Invalid file name"
     TO WS-ERR-STRING.
            PERFORM ERROR-MESSAGE.
             GO TO AA105.
       AA199.
             EXIT.
      *
      *    ****    M O V E   F I E L D   T O   E X P O R T   A R E A
      *
      *            W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
           IF W50-DATA = SPACES
               GO TO AA225.
             MOVE W50-LENGTH     TO W50-S1.
      *
      *    ****    D E T E R M I N E   E N D   O F   F I E L D
      *
      *            THIS LOOP EXCLUDES THE TRAILING SPACES
      *
       AA205.
           IF W50-CCHAR (W50-S1) = SPACE
               SUBTRACT 1        FROM W50-S1
               GO TO AA205.
      *
      *    ****    R E M O V E   L E A D I N G   S P A C E S
      *
       AA210.
           IF W50-1 = SPACE
               MOVE W50-D39      TO W50-DATA
               SUBTRACT 1        FROM W50-S1
               GO TO AA210.
             MOVE W50-S1         TO W50-S2.
             MOVE 1              TO W50-S1.
       AA220.
             MOVE W50-CCHAR (W50-S1)
                                 TO W55-EXP (W55-S1).
             ADD 1               TO W50-S1
                                    W55-S1.
           IF W50-S1 NOT > W50-S2
               GO TO AA220.
       AA225.
             MOVE W50-EXP        TO W55-EXP (W55-S1).
             ADD 1               TO W55-S1.
       AA299.
             EXIT.
      *
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *
       AA300              SECTION.
       AA301.
    IF W50-BIN < 01 OR > 20
               GO TO AA399.
      *
           IF W50-BIN = 01
        MOVE P-NUMBER  TO W50-DATA
        MOVE 6   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 02
        MOVE P-NAME  TO W50-DATA
        MOVE 40   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 03
              UNSTRING P-ADDR DELIMITED BY "*"
         INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
        MOVE W40-ADD1  TO W50-DATA
        MOVE 30   TO W50-LENGTH
               PERFORM AA200
        MOVE W40-ADD2  TO W50-DATA
        MOVE 30   TO W50-LENGTH
               PERFORM AA200
        MOVE W40-ADD3  TO W50-DATA
        MOVE 30   TO W50-LENGTH
               PERFORM AA200
        MOVE W40-ADD4  TO W50-DATA
        MOVE 30   TO W50-LENGTH
               PERFORM AA200
        MOVE P-PC  TO W100-N4
        MOVE W100-4  TO W50-DATA
        MOVE 4   TO W50-LENGTH
               PERFORM AA200
        GO TO AA399.
      *
           IF W50-BIN = 04
        MOVE P-TERMS  TO W50-DATA
        MOVE 6   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 05
        MOVE P-TEL  TO W50-DATA
        MOVE 14   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 06
        MOVE P-FAX  TO W50-DATA
        MOVE 14   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 07
               PERFORM AA200
        MOVE P-CONTACT  TO W50-DATA
        MOVE 30   TO W50-LENGTH
               PERFORM AA200
        GO TO AA399.
      *
           IF W50-BIN = 08
        MOVE P-BAL  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 09
        MOVE P-DT  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 10
        MOVE P-CR  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 11
        MOVE P-PBAL  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 12
        MOVE P-OUT  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 13
        MOVE P-30D  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 14
        MOVE P-60D  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 15
        MOVE P-90D  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 16
        MOVE P-120D  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 17
        MOVE P-150D  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 18
        MOVE P-180D  TO W100-V1
        MOVE W100-S9V2  TO W50-DATA
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 19
        MOVE P-TAX  TO W50-DATA
        MOVE 1   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 20
        MOVE P-PMNT  TO W50-DATA
        MOVE 12   TO W50-LENGTH
        PERFORM AA200.

       AA399.
               EXIT.
      *
      *    ****    P R O C E S S   T H E   S E L E C T E D   F I E L D S
      *
       AA400           SECTION.
       AA401.
             MOVE 1              TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
       AA405.
           IF W60-FNO (W60-S) = ZERO
               GO TO AA499.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
             PERFORM AA300.
    IF W60-S < 20
               ADD 1             TO W60-S
               GO TO AA405.
       AA499.
             EXIT.
      *
      *    ****   H E A D E R S
      *
       AA600    SECTION.
       AA601.
      MOVE 1   TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
       AA605.
           IF W60-FNO (W60-S) = ZERO
        MOVE W55-EXP-DATA TO EXP-REC1
        PERFORM WRITE-CRDDET THRU WRITE-CRDDET-EXIT
        GO TO AA699.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
      MOVE 21   TO W50-LENGTH.
      MOVE W65-FLD (W50-BIN)
     TO W50-DATA.
      PERFORM AA200.
    IF W60-S < 43
               ADD 1             TO W60-S
        GO TO AA605.
       AA699.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "PARAM.RD".

       COPY "CRDMEM.RD".

       COPY "CREDIT.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

       WRITE-CRDDET.
             WRITE EXP-REC1.
           IF WS-STATUS NOT = "00"
               MOVE 99           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
      
       WRITE-CRDDET-EXIT.
             EXIT.

       COPY "CREDIT.LUP".

       COPY "APAC.HLP".

      /
       AZ000-END               SECTION.
       AZ000-EOJ.
      MOVE ZERO   TO WS-S1.

       AZ010.
             EXIT PROGRAM.
      /
      *    ****    P R O G R A M   M E N U / C O N T R O L
      *
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
      MOVE 31   TO SHADE-COL.
      MOVE 22   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S-MENU.
    IF LS0-LEV < 3
        DISPLAY "(N/A)" AT 1049
   WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.
       BA02.
      MOVE 7   TO TOP-ROW.
      MOVE 11   TO BOTTOM-ROW.
      MOVE 6   TO SCREEN-LIN.
      MOVE 31   TO SCREEN-COL.
      MOVE 20   TO STRING-LENGTH.
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
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO BA18
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF WS-OPTION < 0 OR > 4
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
        MOVE "0"   TO WS-OPTION.
    GO TO BA18.

       BA16.
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

       BA18.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
    IF WS-OPTION = "0"
        GO TO BA999.
    IF WS-OPTION = "1"
        PERFORM DA000
    ELSE
    IF WS-OPTION = "2"
        PERFORM EA000
    ELSE
    IF WS-OPTION = "3" AND
       LS0-LEV > 2
        PERFORM FA000
    ELSE
    IF WS-OPTION = "4"
        PERFORM GA000.
      GO TO BA00.
       BA999.
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
      /
      *    ****    E X P O R T :
      *
      *         C L I E N T   C O D E
      *         N A M E
      *         P O S T A L   A D D R E S S
      *         L I N E 1
      *         L I N E 2
      *         L I N E 3
      *         L I N E 4
      *         P O S T A L   C O D E
      *
       DA000        SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
             DISPLAY S03.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO DA999.
      INITIALIZE W60-FIELDS.
             MOVE 01             TO W60-FNO (1).
      MOVE 02   TO W60-FNO (2).
      MOVE 03   TO W60-FNO (3).
             DISPLAY S06.
             PERFORM AA100.
       DA02.
             DISPLAY S07.
      MOVE "!"   TO P-NUMBER.
      PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
        GO TO DA900.
       DA05.
      PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
        GO TO DA900.
             DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
      PERFORM WRITE-CRDDET THRU WRITE-CRDDET-EXIT.
             GO TO DA05.
       DA900.
      CLOSE CRDDET.
       DA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                A L L   T H E   F I E L D S
      *
       EA000        SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
             DISPLAY S04.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA999.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
       EA05.
             MOVE W60-S          TO W60-FNO (W60-S).
    IF W60-S < 20
               ADD 1             TO W60-S
               GO TO EA05.
             DISPLAY S06.
             PERFORM AA100.
      DISPLAY S07.
      MOVE "!"   TO P-NUMBER.
      PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
        GO TO EA900.
       EA10.
      PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
        GO TO EA900.
             DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
      PERFORM WRITE-CRDDET THRU WRITE-CRDDET-EXIT.
             GO TO EA10.
       EA900.
      CLOSE CRDDET.
       EA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                S E L E C T E D   F I E L D S
      *
       FA000        SECTION 5.
       FA05.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
             DISPLAY S05.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA999.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
001020      DISPLAY " " AT 1803 WITH FOREGROUND-COLOR 14
                     " to view the fields available "
                      WITH FOREGROUND-COLOR 7
                     "S" WITH FOREGROUND-COLOR 14
                     "elect/" WITH FOREGROUND-COLOR 7
                     "A" WITH FOREGROUND-COLOR 14
       "ccept - " WITH FOREGROUND-COLOR 7
                     "Esc" WITH FOREGROUND-COLOR 14
                     "ape" WITH FOREGROUND-COLOR 7.
             MOVE 1              TO W65-S.
       FA10.
             MOVE W65-FLD (W65-S)
                                 TO W65-FIELD.
             DISPLAY S09.
       FA15.
      DISPLAY "°" AT 1859.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      DISPLAY SPACE AT 5041
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF W65-S = 1
    GO TO FA15
       ELSE
    SUBTRACT 1  FROM W65-S
    GO TO FA10
   WHEN DOWN-KEY
       IF W65-S NOT < 41
    GO TO FA15
       ELSE
    ADD 1  TO W65-S
    GO TO FA10
   WHEN OTHER
       CALL X"E5"
        END-EVALUATE
        GO TO FA15
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO FA05
   WHEN OTHER
       CALL X"E5"
        END-EVALUATE
        GO TO FA15
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
    ELSE
        CALL X"35"
        GO TO FA15.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
        CALL X"E5"
               GO TO FA15.
           IF WS-OPTION = "A"
               GO TO FA30.
       FA18.
             MOVE 1              TO WS-S1.
       FA20.
           IF WS-S1 = W60-S
               GO TO FA25.
           IF W60-FNO (WS-S1) = W65-S
               GO TO FA15.
             ADD 1               TO WS-S1.
             GO TO FA20.
       FA25.
             MOVE W65-S          TO W60-FNO (W60-S).
             DISPLAY S06.
             ADD 1               TO W60-S.
    IF W65-S < 41
               ADD 1             TO W65-S.
             GO TO FA10.
       FA30.
             PERFORM AA100.
             DISPLAY S07.
       FA32.
      MOVE "!"   TO P-NUMBER.
      PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
               GO TO FA900.
       FA35.
      PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
               GO TO FA900.
             DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
      PERFORM WRITE-CRDDET THRU WRITE-CRDDET-EXIT.
             GO TO FA35.
       FA900.
      CLOSE CRDDET.
       FA999.
             EXIT.
      /
      *    ****    F I E L D   S E P A R A T O R
      *
       GA000        SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      DISPLAY S10.
       GA05.
      ACCEPT W50-EXP AT 0618
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
       GA999.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
            MOVE PAR-COMPANY  TO W95-COMP.

       ZA05.
             MOVE ","            TO W50-EXP.
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
          IF WS-F-ERROR = 3
              MOVE W02-CREDIT TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE P-NUMBER  TO WS-KEYX
           ELSE
41850   IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 99
              MOVE W02-CRDDET  TO WS-FILE
               MOVE ZERO         TO WS-KEY.
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
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
