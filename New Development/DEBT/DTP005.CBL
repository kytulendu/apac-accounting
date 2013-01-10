      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****    ********   *
      *   **    **     **     **    **   **  **    **  **   **         *
      *   **    **     **     **    **  **    **  **    **  *******    *
      *   **    **     **     *******   **    **  **    **        **   *
      *   **    **     **     **        **    **  **    **        **   *
      *   **    **     **     **         **  **    **  **   **    **   *
      *   *******      **     **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *            A C C O U N T S   R E C E I V A B L E   /           *
      *                                                                *
      *       D E B T O R   M E M O / R E M A R K S   M A I N T        *
      *                                                                *
      *     VERSION 9.00.20 - December 2012                            *
      *                                                                *
      ******************************************************************
      *                                                                *
      * May 2006 - New field included in Accounts Receivable for       *
      * 	       recording number of outstanding Jobs linked to the  *
      * 	       account. Additional filler added for future         *
      * 	       expansion.                                          *
      *                                                                *
      * Dec 2012 - Increased the number of Memo entries, of 60 bytes,  *
      *            allowed from 100 to 200 per account.                *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP005.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   February 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2013
                                 by James William Lemmon.
                                 (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
                         CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000120 COPY "CONTROL.SL".

000140 COPY "DEBTOR.ISL".

000140 COPY "DEBMEM.SL".

000140 COPY "MEMDEB.SL".

000120 COPY "PARAM.SL".

000260 DATA DIVISION.
000270 FILE SECTION.

000120 COPY "CONTROL.FDE".

000310 COPY "DEBTOR.FDE".

000140 COPY "DEBMEM.FDE".

       COPY "MEMDEB.FDE".

000290 COPY "PARAM.FDE".

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
001560 77  WS-SUB                  PIC  9(04)    COMP-5.
001560 77  WS-S                    PIC  9(04)    COMP-5.
001570 77  WS-S1                   PIC  9(04)    COMP-5.
001580 77  WS-S2                   PIC  9(04)    COMP-5.
001590 77  WS-S3                   PIC  9(04)    COMP-5.
001600 77  WS-S4                   PIC  9(04)    COMP-5.
001610 77  WS-S5                   PIC  9(04)    COMP-5.
001620 77  WS-S6                   PIC  9(04)    COMP-5.
001630 77  WS-S7                   PIC  9(04)    COMP-5.
001640 77  WS-S8                   PIC  9(04)    COMP-5.
001650 77  WS-S9                   PIC  9(04)    COMP-5.
002850 77  WS-IXS1                 PIC  9(04)    COMP-5.
002860 77  WS-IXS2                 PIC  9(04)    COMP-5.
002870 77  WS-IXS3                 PIC  9(04)    COMP-5.
002880 77  WS-IXS4                 PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS               PIC  9(06)    COMP-5.
001700 77  WS-ENQEND               PIC  9(06)    COMP-5.
001670 77  WS-NETKEY               PIC  9(06)    COMP-5.
001660 77  WS-PARKEY               PIC  9(06)    COMP-5.
001710 77  WS-RECKEY               PIC  9(06)    COMP-5.
001730 77  WS-TRANS	           PIC  9(04)	 COMP-5 VALUE 1.
001730 77  WS-FIRST	           PIC  9(04)	 COMP-5 VALUE 0.
001730 77  WS-LAST	           PIC  9(04)	 COMP-5 VALUE 0.
       77  WS-MEMPOS	           PIC  9(04)	 COMP-5 VALUE 0.
001750 77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD               PIC  9(02).
001770 77  WS-NTP                  PIC  X(01).
001810 77  WS-KEYSTR               PIC  9(06)    COMP-5.
001820 77  WS-OPTION               PIC  X(01).
001820 77  WS-SKIP                 PIC  X(01)    VALUE "N".
001820 77  WS-TAX                  PIC  X(01).
001820 77  WS-TYPE                 PIC  X(01).
001850 77  WS-ERROR                PIC  9(01)    VALUE ZERO.
001870 77  WS-EKEY                 PIC  9(06)    COMP-5.
001900 77  WS-COMP	           PIC  9(01).
       77  WS-RT-HD	           PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD	           PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD	           PIC  X(10)    VALUE "-Cash sale".
002010 77  WS-ERR1	           PIC  X(22)    VALUE "Invalid Account Number".
002020 77  WS-ERR2                 PIC  X(09)    VALUE "No Record".
002110 77  WS-E    	           PIC  X(15)    VALUE "**END** - ENTER".
002130 77  WS-AC                   PIC  X(07)    VALUE "ACCOUNT".
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY	           PIC  9(08)    COMP-5.
       77  WS-USUB	           PIC  9(04)    COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.	  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *	     C = Clear comment line (50)
      *	     E = Clear lines (any line or lines between 5 and 46)
      * 	     H = Change heading
      * 	     S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP005".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - MEMO MAINTENANCE".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(14).
003280         05  FILLER  PIC X(15).

002230 COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE             PIC  X(03) VALUE "DTP".
           03  WS-PROG               PIC  X(03) VALUE "005".

000290 01  WS-PARID.
000020     03  WS-SYS-ID             PIC  X(03).

002500 01  W10-SPARES.
002510     03  W10-DATE	             PIC X(10).
002520     03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM               PIC  9(08).

       COPY "W12.WS".

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

003530 COPY "W50.WS".

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC  X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC  X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD           PIC  X(60).
               05  FILLER            PIC  X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD           PIC  X(60).
               05  FILLER            PIC  X(10).

       01  W75-KEYS.
           03  W75-S                 PIC  9(02)    COMP-5.
           03  W75-S1                PIC  9(02)    COMP-5.
           03  W75-KEY               PIC  9(06)    COMP-5.
           03  W75-ALLOCATED         PIC S9(09)V99 COMP-3.
           03  W75-BALANCE           PIC S9(09)V99 COMP-3.
           03  W75-DNO               PIC  X(06)    OCCURS 18.

003750 01  W75-TITLES.
003760     03  W75-DESCRIP.
003770         05  FILLER            PIC  X(28)    VALUE "Mr     Mnr    Mrs    Mev    ".
003790         05  FILLER            PIC  X(28)    VALUE "Miss   Mej    Ms     Ms     ".
003810         05  FILLER            PIC  X(28)    VALUE "Dr     Dr     Rev    Ds     ".
003830         05  FILLER            PIC  X(28)    VALUE "Prof   Prof   The HonSy Edel".
003850         05  FILLER            PIC  X(28)    VALUE "                            ".
003870     03  W75-CODES REDEFINES W75-DESCRIP.
003880         05  W75-TITLE  OCCURS 10.
003890             07  W75-ETITLE    PIC  X(07).
003900             07  W75-ATITLE    PIC  X(07).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO              PIC  X(06).
003940     03  W80-TYPE              PIC  9(01).

006270 01  W85-PASS.
006280     03  W85-SUPER             PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY             PIC  9(02)    COMP-3.
006300     03  W85-MARG              PIC S9(02)V99.

004390 01  W95-STM.
004400     03  W95-COMP              PIC  X(40).
004410     03  W95-V1.
004420         05  W95-T             PIC  Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B             PIC  Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE       PIC  Z9/99/9999.
004470     03  W95-POST              PIC  9(04).
004480     03  W95-TEL               PIC  X(08).

004490 01  W100-EDIT.
004500     03  W100-LBAL             PIC  Z(06)9.99-.
004500     03  W100-LARR             PIC  Z(06)9.99.
004500     03  W100-BAL              PIC  Z(08)9.99-.
004510     03  W100-CUR              PIC  Z(08)9.99-.
004520     03  W100-30               PIC  Z(08)9.99-.
004530     03  W100-60               PIC  Z(08)9.99-.
004540     03  W100-90               PIC  Z(08)9.99-.
004540     03  W100-120              PIC  Z(08)9.99-.
004550     03  W100-INT              PIC  Z(08)9.99.
004560     03  W100-CRL              PIC  Z(06)9.
004570     03  W100-KEY              PIC  Z(05)9.
      *
      *    W200-MEMO has been included to make the displaying and data
      *    capture (Screen Handling) of the memo data easier. The data
      *    to be displayed, amended or added will be in this RECORD in
      *    WORKING storage.
      *
       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-SEQ1         PIC  9(04).
               05  W200-MEM1         PIC  X(60).
               05  W200-SEQ2         PIC  9(04).
               05  W200-MEM2         PIC  X(60).
               05  W200-SEQ3         PIC  9(04).
               05  W200-MEM3         PIC  X(60).
               05  W200-SEQ4         PIC  9(04).
               05  W200-MEM4         PIC  X(60).
               05  W200-SEQ5         PIC  9(04).
               05  W200-MEM5         PIC  X(60).
               05  W200-SEQ6         PIC  9(04).
               05  W200-MEM6         PIC  X(60).
               05  W200-SEQ7         PIC  9(04).
               05  W200-MEM7         PIC  X(60).
               05  W200-SEQ8         PIC  9(04).
               05  W200-MEM8         PIC  X(60).
               05  W200-SEQ9         PIC  9(04).
               05  W200-MEM9         PIC  X(60).
               05  W200-SEQ10        PIC  9(04).
               05  W200-MEM10        PIC  X(60).
               05  W200-SEQ11        PIC  9(04).
               05  W200-MEM11        PIC  X(60).
               05  W200-SEQ12        PIC  9(04).
               05  W200-MEM12        PIC  X(60).
               05  W200-SEQ13        PIC  9(04).
               05  W200-MEM13        PIC  X(60).
               05  W200-SEQ14        PIC  9(04).
               05  W200-MEM14        PIC  X(60).
               05  W200-SEQ15        PIC  9(04).
               05  W200-MEM15        PIC  X(60).
               05  W200-SEQ16        PIC  9(04).
               05  W200-MEM16        PIC  X(60).
               05  W200-SEQ17        PIC  9(04).
               05  W200-MEM17        PIC  X(60).
               05  W200-SEQ18        PIC  9(04).
               05  W200-MEM18        PIC  X(60).
               05  W200-SEQ19        PIC  9(04).
               05  W200-MEM19        PIC  X(60).
               05  W200-SEQ20        PIC  9(04).
               05  W200-MEM20        PIC  X(60).
               05  W200-SEQ21        PIC  9(04).
               05  W200-MEM21        PIC  X(60).
               05  W200-SEQ22        PIC  9(04).
               05  W200-MEM22        PIC  X(60).
               05  W200-SEQ23        PIC  9(04).
               05  W200-MEM23        PIC  X(60).
               05  W200-SEQ24        PIC  9(04).
               05  W200-MEM24        PIC  X(60).
               05  W200-SEQ25        PIC  9(04).
               05  W200-MEM25        PIC  X(60).
               05  W200-SEQ26        PIC  9(04).
               05  W200-MEM26        PIC  X(60).
               05  W200-SEQ27        PIC  9(04).
               05  W200-MEM27        PIC  X(60).
               05  W200-SEQ28        PIC  9(04).
               05  W200-MEM28        PIC  X(60).
               05  W200-SEQ29        PIC  9(04).
               05  W200-MEM29        PIC  X(60).
               05  W200-SEQ30        PIC  9(04).
               05  W200-MEM30        PIC  X(60).
               05  W200-SEQ31        PIC  9(04).
               05  W200-MEM31        PIC  X(60).
               05  W200-SEQ32        PIC  9(04).
               05  W200-MEM32        PIC  X(60).
               05  W200-SEQ33        PIC  9(04).
               05  W200-MEM33        PIC  X(60).
               05  W200-SEQ34        PIC  9(04).
               05  W200-MEM34        PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM-DET      OCCURS 34.
                   07  W200-MSQ      PIC  9(04).
                   07  W200-MEM      PIC  X(60).
      *
      *    All current keys for MEMO data for the account that is
      *    being worked on will be in this RECORD W200-KEYS while
      *    the data will be in RECORD W205-MEMO.
      *
       01  W200-KEYS.
           03  W200-KEY              OCCURS 200.
               05  W200-AC           PIC  X(06).
               05  W200-SEQ          PIC  9(04).

       01  W205-MEMO.
           03  W205-MEM-DET.
               05  W205-MEM          PIC  X(60) OCCURS 200.
           03  W205-S 	             PIC  9(04) COMP-5.
           03  W205-S1	             PIC  9(04) COMP-5.
           03  W205-S2	             PIC  9(04) COMP-5.
           03  W205-LAST             PIC  9(04) COMP-5.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-DEBT.IDS".

      /
004580 SCREEN SECTION.

005020 01  S06.
005030     03  LINE  3 COLUMN 37 VALUE "INTERNAL" FOREGROUND-COLOR Grey HIGHLIGHT.

       COPY "DEBTOR.CRT".

007910 01  S36.
007920     03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(06) USING W80-ACNO AUTO.


       01  S-MEM.
           02  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      *
      *                              1         2         3         4         5         6         7         8
      *                     12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
           03  LINE  8 COLUMN 13 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           03  LINE  9 COLUMN 13 VALUE "³Lin    M E M O / R E M A R K S / C O R R E S P O N D E N C E    ³".
           03  LINE 10 COLUMN 13 VALUE "ÆÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ".
           03  LINE 11 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 12 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 13 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 14 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 15 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 16 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 17 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 18 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 19 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 20 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 21 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 22 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 23 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 24 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 25 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 26 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 27 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 28 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 29 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 30 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 31 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 32 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 33 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 34 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 35 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 36 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 37 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 38 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 39 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 40 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 41 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 42 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 43 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 44 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 45 COLUMN 13 VALUE "ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
      *
      *    U S E D   T O   D I S P L A Y   M E M O   D A T A
      *
       01  SD-MEM.
           02  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
           03  LINE 11 COLUMN 14 PIC ZZ9   USING W200-SEQ1  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM1.
           03  LINE 12 COLUMN 14 PIC ZZ9   USING W200-SEQ2  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM2.
           03  LINE 13 COLUMN 14 PIC ZZ9   USING W200-SEQ3  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM3.
           03  LINE 14 COLUMN 14 PIC ZZ9   USING W200-SEQ4  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM4.
           03  LINE 15 COLUMN 14 PIC ZZ9   USING W200-SEQ5  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM5.
           03  LINE 16 COLUMN 14 PIC ZZ9   USING W200-SEQ6  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM6.
           03  LINE 17 COLUMN 14 PIC ZZ9   USING W200-SEQ7  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM7.
           03  LINE 18 COLUMN 14 PIC ZZ9   USING W200-SEQ8  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM8.
           03  LINE 19 COLUMN 14 PIC ZZ9   USING W200-SEQ9  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM9.
           03  LINE 20 COLUMN 14 PIC ZZ9   USING W200-SEQ10 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM10.
           03  LINE 21 COLUMN 14 PIC ZZ9   USING W200-SEQ11 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM11.
           03  LINE 22 COLUMN 14 PIC ZZ9   USING W200-SEQ12 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM12.
           03  LINE 23 COLUMN 14 PIC ZZ9   USING W200-SEQ13 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM13.
           03  LINE 24 COLUMN 14 PIC ZZ9   USING W200-SEQ14 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM14.
           03  LINE 25 COLUMN 14 PIC ZZ9   USING W200-SEQ15 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM15.
           03  LINE 26 COLUMN 14 PIC ZZ9   USING W200-SEQ16 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM16.
           03  LINE 27 COLUMN 14 PIC ZZ9   USING W200-SEQ17 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM17.
           03  LINE 28 COLUMN 14 PIC ZZ9   USING W200-SEQ18 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM18.
           03  LINE 29 COLUMN 14 PIC ZZ9   USING W200-SEQ19 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM19.
           03  LINE 30 COLUMN 14 PIC ZZ9   USING W200-SEQ20 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM20.
           03  LINE 31 COLUMN 14 PIC ZZ9   USING W200-SEQ21 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM21.
           03  LINE 32 COLUMN 14 PIC ZZ9   USING W200-SEQ22 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM22.
           03  LINE 33 COLUMN 14 PIC ZZ9   USING W200-SEQ23 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM23.
           03  LINE 34 COLUMN 14 PIC ZZ9   USING W200-SEQ24 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM24.
           03  LINE 35 COLUMN 14 PIC ZZ9   USING W200-SEQ25 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM25.
           03  LINE 36 COLUMN 14 PIC ZZ9   USING W200-SEQ26 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM26.
           03  LINE 37 COLUMN 14 PIC ZZ9   USING W200-SEQ27 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM27.
           03  LINE 38 COLUMN 14 PIC ZZ9   USING W200-SEQ28 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM28.
           03  LINE 39 COLUMN 14 PIC ZZ9   USING W200-SEQ29 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM29.
           03  LINE 40 COLUMN 14 PIC ZZ9   USING W200-SEQ30 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM30.
           03  LINE 41 COLUMN 14 PIC ZZ9   USING W200-SEQ31 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM31.
           03  LINE 42 COLUMN 14 PIC ZZ9   USING W200-SEQ32 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM32.
           03  LINE 43 COLUMN 14 PIC ZZ9   USING W200-SEQ33 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM33.
           03  LINE 44 COLUMN 14 PIC ZZ9   USING W200-SEQ34 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM34.
      *
      *    U S E D   T O   C A P T U R E   O R   A M E N D   M E M O   D A T A
      *
       01  SA-MEM.
           03  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM(WS-S2) AUTO.

       01  SD-SEQ.
           03  FOREGROUND-COLOR Green BACKGROUND-COLOR Grey PIC ZZ9 USING W200-MSQ(WS-S2) AUTO.



      /
001230 PROCEDURE DIVISION  USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DEBT-FILE-IDS.
008230 AA000		       SECTION.
008240 AA00.
003240     IF LS0-DBLEV < 2
               MOVE "Not Authorised" TO WS-ERR-MES
003260         PERFORM ERROR-MESSAGE
               GO TO AA49.
008250       PERFORM ZA000-INIT.
             PERFORM CJ000.

013140 AA49.
013150       EXIT PROGRAM.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call the Debtor / Ac's Receivable ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W40-COMPANY which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA50-LOOKUP     SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
011080       MOVE 4                  TO W44-FUNCTION.
011090       PERFORM SCREEN-CONTENTS.
	       CALL PRG-DEBT-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DEBT-FILE-IDS W40-COMPANY
	            ON EXCEPTION GO TO AA55
	       END-CALL.
             MOVE W40-ACNO           TO W80-ACNO.
	       CANCEL PRG-DEBT-LUP.
	       GO TO AA58.

       AA55.
             MOVE SPACE            TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-DEBT-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3    ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box + 1.           ³
      *    ³          SHADE-COL   - Left line of box + 2.              ³
      *    ³          SHADE-WIDTH - Width of the box - 2.              ³
      *    ³          SHADE-LINES - Hight of box - 1.                  ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.	                             ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	       D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
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

       COPY "CLEAR.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

000140 COPY "DEBMEM.RD".

000140 COPY "MEMDEB.RD".

       COPY "PARAM.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBTOR.WR".

       COPY "DEBMEM.WR".

000140 COPY "MEMDEB.WR".

       COPY "PARAM.WR".

       COPY "APAC.HLP".

      *
      *    ****    Read debtor record - using account number.
      *
014260 CA155-GET-DEBTOR  SECTION 2.
014270 CA155.
012010       MOVE W80-ACNO       TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420         MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
014430 CA160-EXIT.
014440       EXIT.
      /
      *    ****    D E B T O R   M E M O   M A I N T E N A N C E
      *
037050 CJ000	       SECTION 50.
037060 CJ00.
             MOVE "S"                TO CRT-TYPE.
007750       CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
020350       DISPLAY "Account :" AT 0602.
020370       MOVE SPACES             TO W80-ACNO.

020360 CJ05.
             DISPLAY "F1"                        AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Help,"                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F2"                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to do Account Lookup,"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F9"                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " Debtor Lookup (Any Word),"        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "Esc"                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to exit"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
020380       ACCEPT S36 AT 0612.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                      MOVE SPACES    TO W80-ACNO
                      GO TO CJ999
                 WHEN F1-KEY
                      PERFORM HELP-ROUTINE
                 WHEN F2-KEY
                      MOVE SPACE     TO W40-LUP
016560                PERFORM AA50
                 WHEN F9-KEY
                      MOVE "A"       TO W40-LUP
                      PERFORM AA50
                 WHEN OTHER
                      CALL X"E5"
                      GO TO CJ05
               END-EVALUATE
016570         DISPLAY S36 AT 0612
016580         IF W80-ACNO = SPACES
016590             GO TO CJ05.
             MOVE 12                 TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
             PERFORM CLEAR-L50.
020390     IF W80-ACNO = SPACES
020400         GO TO CJ999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
020410       PERFORM CA155-GET-DEBTOR.
020420     IF WS-ERROR = 1
020430         MOVE WS-ERR2          TO WS-ERR-MES
003260         PERFORM ERROR-MESSAGE
020440         GO TO CJ05.
020450     IF WS-ERROR = 2
020460         MOVE WS-ERR1          TO WS-ERR-MES
003260         PERFORM ERROR-MESSAGE
020470         GO TO CJ05.
             MOVE 6                  TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
           IF NOT VALID-PRICE
               MOVE "R"              TO DEB-PRICE.
             MOVE DEB-RECORD1        TO W40-DEBTREC.
             MOVE 6                  TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
020710     IF W80-TYPE NOT = 1
020720         GO TO CJ15.
020760       DISPLAY S22.
             DISPLAY S36 AT 0615.

020770 CJ10.
020780       DISPLAY S25.
             PERFORM CHECK-CORRECT.
020860     IF WS-OPTION = "N"
020870         GO TO CJ00.
             GO TO CJ25.

021360 CJ15.
021410       DISPLAY S23.
019110     IF W80-TYPE = 3
019120         DISPLAY S06.
             DISPLAY S36 AT 0615.

021450 CJ20.
021460       DISPLAY S24.
             PERFORM CHECK-CORRECT.
021540     IF WS-OPTION = "N"
021550         GO TO CJ00.
      *
      *    ****   M E M O   W I N D O W
      *
       CJ25.
             PERFORM SAVE-SCREEN-3.
             MOVE 9                  TO SHADE-ROW.
             MOVE 15                 TO SHADE-COL.
             MOVE 64                 TO SHADE-WIDTH.
             MOVE 37                 TO SHADE-LINES.
             DISPLAY S-MEM.
             PERFORM SCREEN-SHADOW.
      *
      *        S U B S C R I P T S   A N D   P O I N T E R S
      *
      *        WS-S1   - Used for moving data to and from the Screen area
      *                  W200 to Work W205 or Work File MEM-DATA.
      *        WS-S2   - Current MEMO line on screen.
      *        WS-S6   - Used when moving date up or down in the work area.
      *        WS-S7   - Used to store W205-S1 when inserting or deleting
      *                  lines of data.
      *        WS-S8   - Used to store W205-S2 when inserting or deleting
      *                  lines of data.
      *        WS-S9   - Pointer for Work Area and Keys
      *        W205-S  - Pointing to the W205-MEM-DET entry that is the
      *                  first entry in the memo window on the screen.
      *        W205-S1 - Used as a pointer when moving data from or to
      *                  W205-MEM-DET and W200-MEM-FLDS (Screen).
      *        W205-S2 - Pointer to the current line in W205-MEM-DET
      *        W205-LAST - The last entry in the Work Area W205-MEM-DET
      *
      *
      *    ****   C H E C K   I F   T H E R E	I S   A N Y   D A T A
      *
             INITIALIZE W200-MEM-FLDS.
             MOVE ZERO               TO W205-LAST W205-S1 W205-S2.
             MOVE W80-ACNO           TO DME-AC.
             MOVE 1                  TO DME-SEQ.
018690       MOVE 1                  TO WS-FIRST WS-MEMPOS WS-LAST.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
      *
      *    ****    I F	 T H E R E   I S   N O   D A T A   B Y P A S S
      *
           IF WS-F-ERROR NOT = ZERO
               OPEN OUTPUT MEMDEB
               CLOSE MEMDEB
               OPEN I-O MEMDEB
               MOVE ZERO             TO WS-S9
               MOVE SPACES           TO W205-MEM-DET W200-KEYS
               MOVE 1                TO W205-S W205-S2 W200-SEQ1
               DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
               GO TO CJ40.
      *
      *    ****   G E T   E X I S T I N G   M E M O   D A T A
      *
             PERFORM GET-DEBMEM-DETAILS.
             MOVE 1                  TO W205-S.
      *
      *    ****   S E T   S T A R T   P O S I T I O N	  F O R
      *           D I S P L A Y   A N D   A C C E P T
      *
       CJ28.
           IF W205-S2 > W205-LAST
               MOVE W205-S2          TO W205-LAST.
             MOVE 1                  TO WS-S1.
             MOVE W205-S             TO W205-S1 W205-S2.
             INITIALIZE W200-MEM-FLDS.
      *
      *    ****   S T O R E   M E M O   D E T A I L S   I N
      *           D I S P L A Y   A N D   A C C E P T	  A R E A
      *
       CJ30.
             MOVE W205-MEM(W205-S1)  TO W200-MEM(WS-S1).
             MOVE W200-SEQ(W205-S1)  TO W200-MSQ(WS-S1).
           IF WS-S1 > 33
               GO TO CJ40.
      *
      *    ****   C H E C K   F O R   E N D   O F   M E M O   D A T A
      *
       CJ35.
           IF W205-S1 < W205-LAST
               ADD 1                 TO WS-S1 W205-S1
               GO TO CJ30.

       CJ40.
             DISPLAY SD-MEM.
001020       DISPLAY " "           AT 5003 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
001030               ","                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "PgUp"                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "/"                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "PgDn"                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     ","                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
001170               "Esc"                   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "ape,"                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F2"                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Insert line,"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F3"                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Delete line,"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F10"                   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Update"               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       CJ45.
      *
      *      CLIN = 11  CCOL = 18
      *
             MOVE 1118               TO CPOS.
             MOVE 1                  TO WS-S2.

       CJ50.
             ACCEPT SA-MEM AT CPOS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
      *
      *        M O V E   U P   O N E   L I N E   (SCREEN)
      *
                 WHEN UP-KEY
                      IF W205-S = 1
                          GO TO CJ45
                      ELSE
                          SUBTRACT 1 FROM W205-S WS-S2
                      END-IF
001320                GO TO CJ28
      *
      *       M O V E   D O W N   O N E   L I N E   (SCREEN)
      *
                 WHEN DOWN-KEY
                      IF W205-S2 < 200
                          ADD 1              TO W205-S2
                          IF W205-S2 > W205-LAST
                              MOVE W205-S2   TO W205-LAST
                              MOVE W80-ACNO  TO W200-AC(W205-S2)
                              MOVE W205-S2   TO W200-SEQ(W205-S2)
                              DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
                          END-IF
                      END-IF
                      IF WS-S2 < 34
                          ADD 1              TO WS-S2 CLIN
                      ELSE
      *
      *     On the bottom line of the screen window: Move the data up
      *     and get the next line from the work area (W205)
      *
                          PERFORM CP000
                      END-IF
001340                GO TO CJ50
      *
      *       I N V A L I D   K E Y   S T R O K E
      *
                 WHEN OTHER CALL X"E5"
	         END-EVALUATE
               GO TO CJ45
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
      *
      *                   E X I T
      *
                 WHEN ESC-KEY
                      GO TO CJ110
      *
      *              P A G E   D O W N
      *
                 WHEN PAGE-DOWN
                      PERFORM CK000
                      IF W205-S > 165
                          MOVE 167   TO W205-S
                      ELSE
                          ADD 34     TO W205-S
                      END-IF
      *
      *                P A G E   U P
      *
                 WHEN PAGE-UP
                      PERFORM CK000
                      IF W205-S < 35
                          MOVE 1       TO W205-S
                      ELSE
                          SUBTRACT 34  FROM W205-S
                      END-IF
      *
      *	   I N S E R T   A   L I N E
      *
                 WHEN F2-KEY
                 	    PERFORM CK000
                      IF WS-LAST < 200
	                    PERFORM CL000
                      END-IF
      *
      *	   D E L E T E   A   L I N E
      *
                 WHEN F3-KEY
                      MOVE SPACES    TO W200-MEM1
	                PERFORM CK000
	                MOVE W205-S2   TO WS-S6
	                PERFORM CM000
	                PERFORM CJ28 THRU CJ40
	                MOVE WS-S6     TO W205-S2
                      GO TO CJ45
      *
      *            E N D   I N P U T
      *
                 WHEN F10-KEY
                      PERFORM CK000
                      ADD 33 W205-S	   GIVING W205-S2
                      IF W205-S2 > W205-LAST
                          MOVE W205-S2 TO W205-LAST
                      END-IF
                      PERFORM CHECK-CORRECT
015980                IF WS-OPTION = "N"
                          GO TO CJ45
                      END-IF
                      PERFORM CN000
                      GO TO CJ110
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
                 WHEN OTHER CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
               END-EVALUATE
               GO TO CJ28.
      *
      *    U P D A T E   W 2 0 5   W I T H   A D D E D / A M E N D E D
      *           D A T A   F R O M   S C R E E N   A R E A
      *
             MOVE W200-MEM(WS-S2)    TO W205-MEM(W205-S2).
           IF WS-S2 < 34
               IF W205-S2 > W205-LAST
                   MOVE W205-S2      TO W205-LAST
               END-IF
               ADD 1                 TO W205-S2 WS-S2 CLIN
               IF W205-S2 > W205-LAST
                   MOVE W80-ACNO     TO W200-AC(W205-S2)
                   MOVE W205-S2      TO W200-SEQ(W205-S2) W200-MSQ(WS-S2)
                   DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
               END-IF
           ELSE
               PERFORM CP000
           END-IF.
             GO TO CJ50.

       CJ110.
	     PERFORM RESTORE-SCREEN-3.
	     CLOSE MEMDEB.
	     GO TO CJ00.

       CJ999.
	     EXIT.
      *
      *    ****   M O V E   D A T A   F R O M	A C C E P T   A R E A
      *           T O   W 2 0 5   A N D   W R I T E   T H E S E
      *           D E T A I L S   T O   T H E   W O R K   F I L E
      *
       CK000	       SECTION 51.
       CK00.
             MOVE W205-S             TO W205-S1.
             MOVE 1                  TO WS-S1.
             MOVE DEB-ACNO           TO MEM-AC.

       CK05.
             MOVE W205-S1            TO MEM-SEQ.
             MOVE W200-MEM(WS-S1)    TO W205-MEM(W205-S1) MEM-DATA.
             PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
           IF WS-F-ERROR = 62
               MOVE ZERO             TO WS-F-ERROR
               IF NOT(MEM-DATA = SPACES)
                   PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
           IF WS-S1 < 12
               ADD 1                 TO WS-S1 W205-S1
               GO TO CK05.

       CK999.
	     EXIT.
      *
      *    ****   I N S E R T	  A   B L A N K   L I N E
      *
       CL000	       SECTION 5.
       CL00.
             MOVE W205-S1            TO WS-S7.
             MOVE W205-S2            TO WS-S8.
             MOVE W205-LAST          TO W205-S1.
             ADD 1                   TO W205-LAST.
             MOVE W205-LAST          TO W205-S2.
             MOVE DEB-ACNO           TO MEM-AC.
      *
      *    M O V E   D A T A   U P   I N   W O R K   A R E A   A N D
      *               I N   T H E   W O R K   F I L E
      *
       CL05.
             MOVE W205-S2            TO MEM-SEQ.
             MOVE W205-MEM(W205-S1)  TO W205-MEM(W205-S2) MEM-DATA.
             MOVE MEM-KEY            TO W200-KEY(W205-S2).
           IF W205-S2 = W205-LAST
               PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT
               IF WS-F-ERROR = 62
        	       PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT
                   MOVE ZERO         TO WS-F-ERROR
               END-IF
           ELSE
               PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
           IF W205-S1 > WS-S8
               SUBTRACT 1            FROM W205-S1 W205-S2
               GO TO CL05.
             MOVE SPACES             TO W205-MEM(W205-S1) MEM-DATA.
             MOVE W205-S1            TO MEM-SEQ.

       CL10.
             PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
             MOVE WS-S7              TO W205-S1.
             MOVE WS-S8              TO W205-S2.

       CL999.
	     EXIT.
      *
      *    ****   D E L E T E   T H E   L I N E   B E I N G
      *                        E D I T E D
      *
       CM000	       SECTION 5.
       CM00.
             MOVE W205-S1            TO WS-S7.
             MOVE W205-S2            TO WS-S8.
             MOVE DEB-ACNO           TO MEM-AC.

       CM02.
           IF W205-MEM(W205-LAST) = SPACES
               IF W205-S2 = W205-LAST
                    GO TO CM999
               ELSE
               IF W205-S2 < W205-LAST
                   SUBTRACT 1        FROM W205-LAST
                   GO TO CM02.
           IF W205-S2 = W205-LAST
               MOVE SPACES           TO W205-MEM(W205-S2)
               GO TO CM10.
             ADD 1 W205-S2           GIVING W205-S1.
      *
      *    M O V E   D A T A   D O W N   I N   W O R K   A R E A   A N D
      *                 I N   T H E   W O R K   F I L E
      *
       CM05.
             MOVE W205-S2            TO MEM-SEQ.
             MOVE W205-MEM(W205-S1)	 TO W205-MEM(W205-S2) MEM-DATA.
             MOVE MEM-KEY            TO W200-KEY(W205-S2).
             PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
             MOVE SPACES             TO W205-MEM(W205-S1).
           IF W205-S1 < W205-LAST
               ADD 1                 TO W205-S1 W205-S2
               GO TO CM05.

       CM10.
             MOVE SPACES             TO MEM-DATA.
             MOVE W205-LAST          TO MEM-SEQ.
             PERFORM READ-MEMDEB THRU READ-MEMDEB-EXIT.
             PERFORM DELETE-MEMDEB-REC THRU WRITE-MEMDEB-EXIT.
             SUBTRACT 1              FROM W205-LAST.
             MOVE WS-S7              TO W205-S1.
             MOVE WS-S8              TO W205-S2.
           IF W205-S2 > W205-LAST
               MOVE W205-LAST        TO W205-S2 WS-S6.

       CM999.
	     EXIT.
      *
      *    ****   U P D A T E   T H E   M E M O   F I L E
      *
       CN000	       SECTION 5.
       CN00.
	     MOVE 1		 TO W205-S1 WS-S6.
      *
      *    F I N D   L A S T   L I N E   W I T H   D A T A
      *
       CN02.
	   IF W205-MEM(W205-LAST) = SPACES
	       SUBTRACT 1	 FROM W205-LAST
	       GO TO CN02.
      *
      *         R E M O V E   B L A N K   L I N E S
      *
       CN05.
	   IF W205-MEM(W205-S1) = SPACES
	       PERFORM CO000
	       GO TO CN05.
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO W205-S1
	       GO TO CN05.
	     MOVE 1		 TO W205-S1.
	     MOVE DEB-ACNO	 TO DME-AC.
      *
      *          U P D A T E   M E M O   F I L E
      *
       CN10.
	     MOVE W205-S1	 TO DME-SEQ.
	     MOVE W205-MEM(W205-S1)
				 TO DME-DATA.
      *
      *      F I N D   K E Y   I N   K E Y   T A B L E
      *
       CN15.
	   IF DME-KEY < W200-KEY(WS-S6)
	       GO TO CN20.
	   IF DME-KEY = W200-KEY(WS-S6)
	       GO TO CN25.
	   IF WS-S6 < W205-LAST
	       ADD 1		 TO WS-S6
	       GO TO CN15.
      *
      *        A D D   N E W   M E M O   R E C O R D
      *
       CN20.
	     PERFORM WRITE-DEBMEM THRU WRITE-DEBMEM-EXIT.
	     GO TO CN30.
      *
      *    U P D A T E	 E X I S T I N G   M E M O   R E C O R D
      *
       CN25.
	     PERFORM REWRITE-DEBMEM THRU WRITE-DEBMEM-EXIT.
      *
      *     C L E A R   K E Y S   O F   U S E D   R E C O R D S
      *
             MOVE SPACES             TO W200-KEY(WS-S6).

       CN30.
           IF W205-S1 < W205-LAST
               ADD 1                 TO W205-S1
               GO TO CN10.
      *
      *    D E L E T E	 R E D U N D A N T   R E C O R D S   F R O M
      *                      M E M O   F I L E
      *
             MOVE 1                  TO WS-S6.
      *
      *              F I N D   R E D U N D A N T   K E Y S
      *
       CN35.
           IF NOT(W200-KEY(WS-S6) = SPACES)
               MOVE W200-KEY(WS-S6)  TO DME-KEY
               PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT
               PERFORM DELETE-DEBMEM-REC THRU WRITE-DEBMEM-EXIT.
           IF WS-S6 < WS-S9
               ADD 1                 TO WS-S6
               GO TO CN35.

       CN999.
	     EXIT.
      *
      *    M O V E   D A T A   D O W N   I N   W O R K   A R E A
      *
       CO000	       SECTION 5.
       CO00.
             MOVE W205-S1            TO WS-S7.
             ADD 1 WS-S7             GIVING WS-S8.

       CO05.
             MOVE W205-MEM(WS-S8)    TO W205-MEM(WS-S7).
             MOVE SPACES             TO W205-MEM(WS-S8).
           IF WS-S8 < W205-LAST
               ADD 1                 TO WS-S8 WS-S7
               GO TO CO05.
             SUBTRACT 1              FROM W205-LAST.

       CO999.
	     EXIT.
      *
      *    M O V E   D A T A   U P   1   L I N E   O N   T H E
      *    S C R E E N   A N D   G E T   T H E   N E X T   L I N E
      *    F R O M   T H E   W O R K   A R E A   W 2 0 5
      *
       CP000	       SECTION 5.
       CP00.
           IF W205-S > 165
               MOVE 167              TO W205-S
           ELSE
               ADD 1                 TO W205-S.
             INITIALIZE W200-MEM-FLDS.
             MOVE 1                  TO WS-S1.
             MOVE W205-S             TO W205-S1 W205-S2.
             PERFORM TEST AFTER UNTIL WS-S1 > 34
                 MOVE W205-MEM(W205-S1)  TO W200-MEM(WS-S1)
                 MOVE W200-SEQ(W205-S1)  TO W200-MSQ(WS-S1)
                 IF W205-S1 < W205-LAST
                     ADD 1           TO W205-S1 WS-S1
                 ELSE
                     MOVE 99         TO WS-S1
                 END-IF
             END-PERFORM.
      *
      *    I S   T H E   L A S T   L I N E   I N   T H E   M E M O
      *    W I N D O W   A   B L A N K   L I N E ?
      *
           IF W200-MSQ(34) = ZERO
               SUBTRACT 1 FROM W205-S1 GIVING W200-MSQ(34).
             DISPLAY SD-MEM.

       CP999.
             EXIT.

       GET-DEBMEM-DETAILS SECTION 5.
       OPEN-MEMDEB.
             OPEN OUTPUT MEMDEB.
             CLOSE MEMDEB.
             OPEN I-O MEMDEB.
             MOVE ZERO               TO WS-S9.
             MOVE SPACES             TO W205-MEM-DET W200-KEYS.
             MOVE DME-AC             TO MEM-AC.

       WRITE-MEMO-DETAILS.
             MOVE DME-SEQ            TO MEM-SEQ WS-LAST.
             ADD 1                   TO WS-S9.
             MOVE DME-DATA           TO MEM-DATA W205-MEM(WS-S9).
             MOVE DME-KEY            TO W200-KEY(WS-S9).
             PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
                   GO TO WRITE-MEMO-DETAILS.
             MOVE WS-S9              TO W205-LAST.

       GET-DEBMEM-DETAILS-EXIT.
	     EXIT.

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
044780	     GO TO ZA999-EXIT.
       ZA55.
041220       MOVE 1                  TO WS-S1.
021870       MOVE SPACES             TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1              TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8              TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1                 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9              TO WS-TCH(1).
             MOVE WS-G10             TO WS-TCH(80).
             MOVE WS-G11             TO WS-BCH(1).
             MOVE WS-G12             TO WS-BCH(80).
             MOVE WS-G14             TO WS-TCHR(1)  WS-BCHR(1).
             MOVE WS-G13             TO WS-TCHR(80) WS-BCHR(80).
021930       MOVE WS-G2              TO WS-TCHR(8)  WS-TCHR(49) WS-TCHR(61) WS-TCHR(72).
021950       MOVE WS-G3              TO WS-MCHR(8)  WS-MCHR(49) WS-MCHR(61) WS-MCHR(72) WS-MCHR(1)  WS-MCHR(80).
021970       MOVE WS-G4              TO WS-BCHR(8)  WS-BCHR(49) WS-BCHR(61) WS-BCHR(72).
             MOVE WS-G13             TO WS-TCHR(80) WS-BCHR(80).
             MOVE WS-G14             TO WS-TCHR(1)  WS-BCHR(1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
042900 ZB000-ERROR	     SECTION 8.

       COPY "ERRORS.PRO".

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 62
041860	       MOVE W02-MEMDEB	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE MEM-KEY	 TO WS-KEYX.

       COPY "DISPERR.PRO".
