      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****       **      *
      *   **    **     **     **    **   **  **    **  **     ***      *
      *   **    **     **     **    **  **    **  **    **     **      *
      *   **    **     **     *******   **    **  **    **     **      *
      *   **    **     **     **        **    **  **    **     **      *
      *   **    **     **     **         **  **    **  **      **      *
      *   *******      **     **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *          A C C O U N T S   R E C E I V A B L E   /             *
      *                                                                *
      *       D E B T O R   C R E A T I O N S	P R O G R A M          *
      *                                                                *
      *     VERSION 9.00.20 - December 2012                            *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006	- New field included in Accounts Receivable for      *
      * 		  recording number of outstanding Jobs linked to the *
      *             account. Additional filler added for future        *
      *             expansion.                                         *
      *                                                                *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales       *
      * 		  restrictions file has been included which will     *
      *             allow for departments to be excluded or only       *
      *             selected departments allowed. This will allow the  *
      *             user the abillity to allow or bar sales of select- *
      *             ed departments.                                    *
      *                                                                *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index includ- *
      *             ed for lookups, using any word contained in the    *
      *             Debtor name.                                       *
      *                                                                *
      *  Nov 2012 - Changed DEB-ADDRESS and DEB-PADDR from group       *
      *             fields with 70 characters to group fields          *
      *             containing DEB-ADD-L1/2/3/4 and DEB-PADD-L1/2/3/4  *
      *             respectively. Each of these new fields are defined *
      *             as PIC X(30). As new PC's and Servers contain huge *
      *             amounts of disk space this has been changed, which *
      *             also results in simplifying the code for handling  *
      *             addresses in Data capture, enquiries, reports etc. *
      *             This also removes the restriction on the number of *
      *             characters that an address may contain.            *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP001.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   March 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2013
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

000140 COPY "DEBALT.SL".

000140 COPY "DEBTOR.ISL".

000140 COPY "DEBMEM.SL".

000120 COPY "PARAM.SL".

000220 COPY "RECOVER.SL".

       COPY "SHARED.SL".

000260 DATA DIVISION.
000270 FILE SECTION.

000120 COPY "CONTROL.FDE".

000310 COPY "DEBALT.FDE".

000310 COPY "DEBTOR.FDE".

000140 COPY "DEBMEM.FDE".

000290 COPY "PARAM.FDE".

000390 COPY "RECOVER.GFD".

       COPY "SHARED.FDE".

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
001720 77  WS-RECOVER              PIC  9(06)    COMP-5.
000820 77  WS-SHARED	           PIC  9(04)    COMP-5 VALUE 1.
001730 77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
001750 77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD               PIC  9(02).
001770 77  WS-NTP                  PIC  X(01).
001810 77  WS-KEYSTR               PIC  9(06)    COMP-5.
001820 77  WS-OPTION               PIC  X(01).
001820 77  WS-ASSIGN	           PIC  X(01)    VALUE "N".
001820 77  WS-SKIP                 PIC  X(01)    VALUE "N".
001820 77  WS-TAX                  PIC  X(01).
001820 77  WS-TYPE                 PIC  X(01).
001850 77  WS-ERROR                PIC  9(01)    VALUE ZERO.
001870 77  WS-EKEY                 PIC  9(06)    COMP-5.
001880 77  WS-CHAR1                PIC  X(01).
001890 77  WS-CHAR2                PIC  X(01).
001900 77  WS-COMP                 PIC  9(01).
       77  WS-RT-HD	           PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD	           PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD	           PIC  X(10)    VALUE "-Cash sale".
002010 77  WS-ERR1                 PIC  X(22)    VALUE "Invalid Account Number".
002020 77  WS-ERR2                 PIC  X(09)    VALUE "No Record".
002030 77  WS-ERR3                 PIC  X(15)    VALUE "Data incomplete".
002040 77  WS-ERR4                 PIC  X(13)    VALUE "Invalid title".
002090 77  WS-ERR8                 PIC  X(16)    VALUE "Invalid category".
002130 77  WS-AC                   PIC  X(07)    VALUE "ACCOUNT".
       77  PRG-NAME	           PIC  X(12)    VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY	           PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - ADD ACCOUNT".
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
002450     03  WS-MID-LNE.
002460         05  WS-MCHR         PIC  X(01) OCCURS 80.
           03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
               05  WS-MDT1         PIC  X(01).
               05  WS-MDT78        PIC  X(78).
               05  WS-MDT80        PIC  X(01).
002450     03  WS-MID-LNE2.
               05  FILLER          PIC  X(01) VALUE "³".
               05  WS-BLNK78       PIC  X(78) VALUE ALL "°".
               05  FILLER          PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR         PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER          PIC  X(01).
               05  WS-F-LINE       PIC  X(78).
               05  FILLER          PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH          PIC  X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER          PIC  X(01).
               05  WS-MEM-LINE.
003260             07  WS-SRT-LINE PIC  X(50).
                   07  FILLER      PIC  X(10).
003280         05  FILLER          PIC  X(19).
           03  WS-MM-LINE.
               05  FILLER          PIC  X(01) VALUE X"B3".
               05  FILLER          PIC  X(60) VALUE SPACES.
               05  FILLER          PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE         PIC  X(60) VALUE ALL X"CD".

002230 COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "DTP".
           03  WS-PROG             PIC  X(03) VALUE "001".

000290 01  WS-PARID.
000020     03  WS-SYS-ID           PIC  X(03).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.	  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE -NEW ACCOUNT".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03) VALUE "ÄÄÄ".

002500 01  W10-SPARES.
002510     03  W10-DATE	           PIC  X(08).
002520     03  W10-EDTE REDEFINES W10-DATE PIC 99/99/99.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

       COPY "W12.WS".

       COPY "W20.WS".

002850 01  W25-CALCS.
002860     03  W25-RESULT          PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE       PIC  9(05).
002890         05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
002900     03  W25-TOTAL           PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE           PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08) COMP-3.
           03  W30-90              PIC  9(08) COMP-3.
           03  W30-60              PIC  9(08) COMP-3.
           03  W30-30              PIC  9(08) COMP-3.
           03  W30-CUR             PIC  9(08) COMP-3.

       COPY "W40.DBT".

       01  W40-BALANCES.
           03  W40-A-MTD           PIC S9(09)V99 COMP-3.
003390     03  W40-A-CUR           PIC S9(09)V99 COMP-3.
003400     03  W40-A-30            PIC S9(09)V99 COMP-3.
003410     03  W40-A-60            PIC S9(09)V99 COMP-3.
003420     03  W40-A-90            PIC S9(09)V99 COMP-3.
003420     03  W40-A-120           PIC S9(09)V99 COMP-3.
003430     03  W40-A-INT           PIC S9(09)V99 COMP-3.
           03  W40-D-MTD           PIC S9(09)V99 COMP-3.
003390     03  W40-D-CUR           PIC S9(09)V99 COMP-3.
003400     03  W40-D-30            PIC S9(09)V99 COMP-3.
003410     03  W40-D-60            PIC S9(09)V99 COMP-3.
003420     03  W40-D-90            PIC S9(09)V99 COMP-3.
003420     03  W40-D-120           PIC S9(09)V99 COMP-3.
003430     03  W40-D-INT           PIC S9(09)V99 COMP-3.
           03  W40-T-MTD           PIC S9(09)V99 COMP-3.
003390     03  W40-T-CUR           PIC S9(09)V99 COMP-3.
003400     03  W40-T-30            PIC S9(09)V99 COMP-3.
003410     03  W40-T-60            PIC S9(09)V99 COMP-3.
003420     03  W40-T-90            PIC S9(09)V99 COMP-3.
003420     03  W40-T-120           PIC S9(09)V99 COMP-3.
003430     03  W40-T-INT           PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

003530 COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02) COMP-5.
           03  W75-S1              PIC  9(02) COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06) OCCURS 18.
003750 01  W75-TITLES.
003760     03  W75-DESCRIP.
003770         05  FILLER          PIC  X(28)      VALUE "Mr     Mnr    Mrs    Mev    ".
003790         05  FILLER          PIC  X(28)      VALUE "Miss   Mej    Ms     Ms     ".
003810         05  FILLER          PIC  X(28)      VALUE "Dr     Dr     Rev    Ds     ".
003830         05  FILLER          PIC  X(28)      VALUE "Prof   Prof   The HonSy Edel".
003850         05  FILLER          PIC  X(28)      VALUE "                            ".
003870     03  W75-CODES REDEFINES W75-DESCRIP.
003880         05  W75-TITLE  OCCURS 10.
003890             07  W75-ETITLE  PIC  X(07).
003900             07  W75-ATITLE  PIC  X(07).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO            PIC  X(06).
003940     03  W80-TYPE            PIC  9(01).

006270 01  W85-PASS.
006280     03  W85-SUPER           PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY           PIC  9(02)    COMP-3.
006300     03  W85-MARG            PIC S9(02)V99.

004390 01  W95-STM.
004400     03  W95-COMP            PIC  X(40).
004410     03  W95-V1.
004420         05  W95-T           PIC  Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B           PIC  Z(06)9.99-.
004450     03  W95-D.
004460         05  W95-DTE         PIC  Z9/99/9999.
004470     03  W95-POST            PIC  X(08).
004480     03  W95-TEL             PIC  X(08).

004490 01  W100-EDIT.
004500     03  W100-LBAL           PIC  Z(06)9.99-.
004500     03  W100-LARR           PIC  Z(06)9.99.
004500     03  W100-BAL            PIC  Z(08)9.99-.
004510     03  W100-CUR            PIC  Z(08)9.99-.
004520     03  W100-30             PIC  Z(08)9.99-.
004530     03  W100-60             PIC  Z(08)9.99-.
004540     03  W100-90             PIC  Z(08)9.99-.
004540     03  W100-120            PIC  Z(08)9.99-.
004550     03  W100-INT            PIC  Z(08)9.99.
004560     03  W100-CRL            PIC  Z(06)9.
004570     03  W100-KEY            PIC  Z(05)9.

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
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM        PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-DEBT.IDS".

      /
004580 SCREEN SECTION.

005020 01  S06.
005030     03  LINE  5 COLUMN 37 FOREGROUND-COLOR Grey HIGHLIGHT VALUE "INTERNAL".

       COPY "DEBLUP.CRT".

       01  S-ART11.
	     03  LINE 11 COLUMN 16 FOREGROUND-COLOR Black BACKGROUND-COLOR Green           VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	     03          COLUMN 65 FOREGROUND-COLOR Green HIGHLIGHT BACKGROUND-COLOR Green VALUE "¿".
	     03  LINE 12 COLUMN 16 FOREGROUND-COLOR Black BACKGROUND-COLOR Green           VALUE "³".
	     03          COLUMN 17 FOREGROUND-COLOR Grey  BACKGROUND-COLOR Green           VALUE " Does this A/C qualify for VAT article 11/1g:   ".
	     03          COLUMN 65 FOREGROUND-COLOR Green HIGHLIGHT BACKGROUND-COLOR Green VALUE "³".
	     03  LINE 13 COLUMN 16 FOREGROUND-COLOR Black BACKGROUND-COLOR Green           VALUE "À".
	     03          COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT BACKGROUND-COLOR Green VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

005740 01  S20.
005770     03  LINE  6 COLUMN 12                                  VALUE "Account Type".
005780     03          COLUMN 29                                  VALUE "(".
	     03          COLUMN 30 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "C".
	     03          COLUMN 31                                  VALUE "ompany, ".
	     03          COLUMN 39 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "I".
	     03          COLUMN 40                                  VALUE "ndividual, i".
	     03          COLUMN 52 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "N".
	     03          COLUMN 53                                  VALUE "ternal)".

005800 01  S21.
005810     03  LINE  6 COLUMN 26 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(01) USING WS-TYPE AUTO.

       COPY "DEBTOR.CRT".

007910 01  S36.
007920     03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(06) USING W80-ACNO AUTO.

       COPY "UPDATE.CRT".

      /
001230 PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DEBT-FILE-IDS.
008230 AA000		       SECTION.
008240 AA00.
003240     IF LS0-DBLEV < 1
               MOVE "Not Authorised" TO WS-ERR-MES
003260         PERFORM ERROR-MESSAGE
               GO TO AA49.
008250       PERFORM ZA000-INIT.

       AA05.
             PERFORM CB000.
           IF NOT(WS-ERROR = 9)
               MOVE "Generating Alternate Name Index" AT 5018 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               PERFORM DEBTOR-NAME-INDEX
020150         PERFORM AY70 THRU AY999
               MOVE 23               TO CRT-START CRT-END
018380         PERFORM ERASE-SCREEN
002100         MOVE "Any key to continue"  TO WS-ERR-MES
               PERFORM DISPLAY-MESSAGE
020230         GO TO AA05.
013040       CLOSE RECOVER.
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
             MOVE PRG-DEBT-LUP       TO PRG-NAME.
	       CALL PRG-NAME USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DEBT-FILE-IDS W40-COMPANY
	            ON EXCEPTION GO TO AA55
	       END-CALL.
	       CANCEL PRG-NAME.
	       GO TO AA58.

       AA55.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-NAME DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      * 	  D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-ERR-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´

       COPY "DISPMESS.SCR".

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

       COPY "DATE.CHK".

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

000140 COPY "DEBMEM.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBALT.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

       COPY "APAC.HLP".

      /
      *       ****   *****   ***    ***   *   *  *****  ****   *   *
      *       *   *  *	    *	  *  *   *  *   *  *	  *   *  *   *
      *       ****   ***    *	     *   *  *   *  ***	  ****    * *
      *       *   *  *	    *	  *  *   *   * *	 *	  *   *    *
      *       *   *  *****   ***    ***     *	 *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED	         ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010910 AY000	       SECTION.

010970 AY02.
010980       MOVE 02               TO REC-FILE.
008020       MOVE ZERO             TO REC-KEY.
             MOVE WS-ACTION        TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011000       MOVE DEB-RECORD1      TO REC-DEBTOR.
011010       GO TO AY50.

       AY39.
             MOVE 39               TO REC-FILE.
             MOVE WS-NETKEY        TO REC-KEY.
             MOVE NET-RECORD       TO REC-NETWORK.
             GO TO AY50.

010970 AY47.
010980       MOVE 47               TO REC-FILE.
008020       MOVE ZERO             TO REC-KEY.
             MOVE WS-ACTION        TO REC-TYPE
011000       MOVE DAL-RECORD1      TO REC-DEBALT.
011010       GO TO AY50.

011120 AY49.
011130       MOVE 99               TO REC-FILE.
011140       MOVE ZERO             TO REC-KEY.
011150       MOVE SPACES           TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1                 TO WS-TRANS.
011180     IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO           TO WS-RECOVER.
011240       GO TO AY59.

011250 AY50.
011260       ADD 1                 TO WS-RECOVER.
011270       MOVE WS-RECOVER       TO WS-RECKEY.
011280       MOVE WS-TRANS         TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
               MOVE SPACES         TO WS-ERR-MES
               STRING "Write error Recovery file - Status " DELIMITED SIZE WS-STATUS DELIMITED SIZE INTO WS-ERR-MES
011330         PERFORM ERROR-MESSAGE
011340         STOP RUN.
011370 AY59.
011380       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690       MOVE 1                  TO WS-COUNT.
012690       MOVE 5                  TO WS-SHARED.
012700       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK          TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500       MOVE 4                  TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550       DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
012560       COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
012580       PERFORM LOCK-REC-LOOP.
012590       DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
012600       GO TO AY60.

012610 AY61.
012620       MOVE "DB"               TO PAR-PROG(WS-USUB).
012630       MOVE LS-USER            TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 1                  TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012740       GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR
               PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
           IF NOT(PAR-PROG(WS-SUB) = SPACES)
012760         IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
                   GO TO AY62-WAIT
               ELSE
                   ADD 1             TO WS-SUB
                   GO TO AY62
               END-IF
           ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
               IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
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
             MOVE LS-USER            TO PAR-USR(WS-SUB).
             MOVE WS-SUB             TO PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
           IF NOT(PAR-USR(WS-SUB) = LS-USER)
               IF WS-SUB < 24
                   ADD 1             TO WS-SUB
                   GO TO AY62.

       AY62-CHECK.
           IF WS-WAIT > ZERO
               COMMIT
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               ACCEPT WS-STIME FROM TIME
               PERFORM LOCK-REC-LOOP
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               GO TO AY60.
012880       DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

012900 AY63.
012920       MOVE WS-SUB             TO WS-USUB.
012940       GO TO AY61.

012970 AY70.
013030       MOVE 4                  TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
013010       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1	 TO WS-USUB
                   GO TO AY72
	         ELSE
                   GO TO AY85.

       AY75.
013050       MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB         GIVING WS-SUB
           ELSE
               GO TO AY80.
           IF NOT(PAR-PROG(WS-SUB) = SPACES)
               MOVE PAR-PROG(WS-SUB) TO PAR-PROG(WS-USUB)
               MOVE PAR-USR(WS-SUB)  TO PAR-USR(WS-USUB)
               ADD 1                 TO WS-USUB
               GO TO AY75.

       AY80.
013070       SUBTRACT 1 FROM WS-USUB GIVING PAR-USERS.
013080       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
013090       COMMIT.

011860 AY999.
011870       EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
014260 CA155-GET-DEBTOR  SECTION 2.
014270 CA155.
012010       MOVE W80-ACNO           TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1                TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2                TO WS-ERROR
014410     ELSE
014420         MOVE 0                TO WS-ERROR
               MOVE DEB-TYPE         TO W80-TYPE.

014430 CA160-EXIT.
014440       EXIT.
      /
      *    ****   C R E A T E   N E W   D E B T O R   R E CO R D
      *
018130 CB000		 SECTION 3.
       CB00.
             MOVE "S"                TO CRT-TYPE.
007750       CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
018200       DISPLAY S20.
018210       MOVE SPACE              TO WS-TYPE.
018220 CB05.
018230       ACCEPT S21.
             CALL "CBL_TOUPPER" USING WS-TYPE BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                      MOVE 9         TO WS-ERROR
                      GO TO CB999
                 WHEN OTHER
                      CALL X"E5"
                      GO TO CB05
               END-EVALUATE.
018290     IF WS-TYPE = SPACE
               MOVE 9                TO WS-ERROR
018300         GO TO CB999.
018310     IF NOT(WS-TYPE = "O" OR "N" OR "C" OR "I")
018320         GO TO CB05.
             MOVE "N"                TO WS-ASSIGN.
018330       MOVE SPACES             TO W40-HTEL W40-NAME1 W40-ADDRESS1 W40-TEL W40-PPOST W40-PADDRESS W80-ACNO
                                        W40-POST W40-CONAC W40-CONTACT  W40-TAX W40-FAX   W40-ID.
             MOVE "E"                TO W40-LANG W40-INV-TYPE.
018350       MOVE "Y"                TO W40-FIN  W40-LAB.
             MOVE "O"                TO W40-OI.
             MOVE "1"                TO W40-PRICE.
             MOVE 01                 TO W40-CAT.
             MOVE "N"                TO W40-EXEMPT.
018360       MOVE ZERO               TO W40-TITLE W40-TERMS W40-CRL W40-DISC W40-SDISC W40-SMAN.
             MOVE 6                  TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
018390     IF WS-TYPE NOT = "I"
018400         GO TO CB15 .
018410       MOVE SPACES             TO W40-HTEL.
018420       DISPLAY S22.
018440       DISPLAY S25.
       CB08.
             DISPLAY "F1"                     AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         "=Help,"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
		         "F2"                             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         " to do Account Lookup,"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
		         "F3"                             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         " allocate account No. "         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F9"                             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " Debtor Lookup (Any Word),"     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
		         "Esc"                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         " to exit"                       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
020380       ACCEPT S36 AT 0615.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
	           WHEN ESC-KEY
                      MOVE SPACES    TO W80-ACNO
                      MOVE 9         TO WS-ERROR
	                GO TO CB999
                 WHEN F1-KEY
                      PERFORM HELP-ROUTINE
                 WHEN F2-KEY
                      MOVE SPACE     TO W40-LUP
016560                PERFORM AA50
                 WHEN F3-KEY
                      MOVE "Y"       TO WS-ASSIGN
                      PERFORM CLEAR-L50
                      GO TO CB10
                 WHEN F9-KEY
                      MOVE "A"       TO W40-LUP
                      PERFORM AA50
                 WHEN OTHER
                      CALL X"E5"
                      GO TO CB08
               END-EVALUATE
016570         DISPLAY S36 AT 0615
016580         IF W80-ACNO = SPACES
016590             GO TO CB08.
           IF W80-ACNO = SPACES
                GO TO CB00.
             PERFORM CLEAR-L50.
             MOVE 23                 TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 0
               MOVE "Existing A/C number"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CB08.
018430 CB10.
018440       ACCEPT S25.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
	           WHEN ESC-KEY
                      MOVE "N"       TO WS-ASSIGN
                      GO TO CB08
               END-EVALUATE.
             PERFORM CHECK-CORRECT.
019260     IF WS-OPTION = "N"
               IF WS-ASSIGN = "N"
019270             GO TO CB08
               ELSE
                   GO TO CB10.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CONAC    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-POST     BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PPOST    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-FIN      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-INV-TYPE BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-OI       BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-LAB      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PRICE    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO W50-S1.
             MOVE W40-SURNAME        TO W50-SURNAME.
      *
      *    Remove any INVALID characters from the name as these effect the
      *    sequence in the Alphabetic Index.
      *
       CB12.
             MOVE W50-CCHAR(W50-S1)  TO W50-ASCII.
           IF W50-BIN < 32 OR W50-BIN = 127
               MOVE SPACE            TO W50-CCHAR(W50-S1).
           IF W50-S1 < 24
               ADD 1                 TO W50-S1
               GO TO CB12.
             MOVE W50-SURNAME        TO W40-SURNAME.
018640     IF W40-SURNAME = SPACE
               MOVE 9                TO WS-ERROR
018650         GO TO CB999.
             MOVE 24                 TO WS-LENGTH.
	       CALL "CBL_TOUPPER" USING W40-SURNAME  BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-FNAME    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-SNAME    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-INITIALS BY VALUE WS-LENGTH RETURNING WS-STATUS.
	       MOVE 1                  TO WS-LENGTH.
             MOVE "I"                TO W40-INSTR.
             CALL "DTP\DTPNAME" USING W40-COMPANY.
             PERFORM CLEAR-L50.
018670       DISPLAY "Checking Input data" AT 5018 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
018680     IF  W40-ADD1 = SPACES OR
018690          W40-POST = SPACES OR W40-TITLE = ZERO
018710          MOVE WS-ERR3         TO WS-ERR-MES
                PERFORM ERROR-MESSAGE
018720          GO TO CB10.
018730     IF W40-TITLE < 0 OR > 10
018740         MOVE WS-ERR4          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018750         GO TO CB10.
           IF W40-CAT < 1
               MOVE 1                TO W40-CAT.
018930     IF W40-CAT > 80
018940         MOVE WS-ERR8          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018950         GO TO CB10.
018960     IF W40-TERMS > "3"
018970         MOVE "Terms"          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018980         GO TO CB10.
018990     IF W40-SMAN > 50
019000         MOVE "Salesperson"    TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019010         GO TO CB10.
019020     IF W40-DISC > 10 AND < 60
019030         MOVE "Discount"       TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019040         GO TO CB10.
019020     IF W40-SDISC > 10
019030         MOVE "Settlement Discount"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019040         GO TO CB10.
018760     IF NOT(W40-INV-TYPE = "I" OR "E" OR "X")
018770         MOVE "Invoice type"   TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018780         GO TO CB10.
018760     IF NOT(W40-OI = "O" OR "B")
018770         MOVE "Statement type" TO WS-ERR-MES
	       PERFORM ERROR-MESSAGE
018780	       GO TO CB10.
018760     IF NOT (W40-LAB = "Y" OR "N")
018770         MOVE "Label code"     TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018780         GO TO CB10.
018760     IF NOT(W40-PRICE = "1" OR "2" OR "3")
018770         MOVE "Price code"     TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018780         GO TO CB10.
           IF W40-CONAC NOT = SPACES
               MOVE W40-CONAC        TO DEB-ACNO
012020         PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               IF WS-F-ERROR = 6
                   MOVE "Consolidation Account"  TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   GO TO CB10.
019090       GO TO CB24.
      *
      *    ****   C O M P A N Y  /  I N T E R N A L   A / C
      *
019100 CB15.
019110     IF WS-TYPE = "O" OR "N"
019120         DISPLAY S06.
019160       DISPLAY S23.
019160       DISPLAY S24.

       CB16.
             DISPLAY "F1" AT 5002             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         "=Help,"                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
		         "F2"                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         " to do Account Lookup," WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
		         "F3"                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         " allocate account No. " WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
		         "Esc"                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
		         " to exit"               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
020380       ACCEPT S36 AT 0615.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                      MOVE SPACES    TO W80-ACNO
                      MOVE 9         TO WS-ERROR
                      GO TO CB999
                 WHEN F1-KEY
                      PERFORM HELP-ROUTINE
                 WHEN F2-KEY
016560                PERFORM AA50
                 WHEN F3-KEY
                      MOVE "Y"       TO WS-ASSIGN
                      PERFORM CLEAR-L50
                      GO TO CB20
                 WHEN OTHER
                      CALL X"E5"
                      GO TO CB16
                END-EVALUATE
016570          DISPLAY S36 AT 0615
016580          IF W80-ACNO = SPACES
016590              GO TO CB16.
           IF W80-ACNO = SPACES
               GO TO CB00.
             PERFORM CLEAR-L50.
             MOVE 23                 TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO   BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 0
               MOVE "Existing A/C number"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CB16.
019170 CB20.
019180       ACCEPT S24.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
	           WHEN ESC-KEY
	                MOVE "N"            TO WS-ASSIGN
	                GO TO CB16
               END-EVALUATE.
             PERFORM CHECK-CORRECT.
019260     IF WS-OPTION = "N"
               IF WS-ASSIGN = "N"
019270             GO TO CB16
               ELSE
                   GO TO CB20.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CONAC    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-POST     BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PPOST    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-FIN      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-INV-TYPE BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-OI       BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-LAB      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PRICE    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO W50-S1.
             MOVE W40-NAME           TO W50-NAME.

       CB22.
             MOVE W50-CCHAR(W50-S1)  TO W50-ASCII.
           IF W50-BIN < 32 OR W50-BIN = 127
               MOVE SPACE            TO W50-CCHAR (W50-S1).
           IF W50-S1 < 40
               ADD 1                 TO W50-S1
               GO TO CB22.
             MOVE W50-NAME           TO W40-NAME.
019380     IF W40-NAME = SPACE
               MOVE 9                TO WS-ERROR
019390         GO TO CB999.
             MOVE 40                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-NAME     BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE "C"                TO W40-INSTR.
             CALL "DTP\DTPNAME" USING W40-COMPANY.
             MOVE 23                 TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
019410       DISPLAY "Checking Input data" AT 2318 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
019420     IF W40-ADD1 = SPACES OR W40-POST = SPACES
019440         MOVE WS-ERR3          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019450         GO TO CB20.
           IF W40-CAT < 1
               MOVE 1                TO W40-CAT.
018930     IF W40-CAT > 80
018940         MOVE WS-ERR8          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019620         GO TO CB20.
019630     IF W40-TERMS < "0" OR > "3"
019640         MOVE "Terms"          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019650         GO TO CB20.
019660     IF W40-SMAN > 50
019670         MOVE "Salesperson"    TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019680         GO TO CB20.
019020     IF W40-DISC > 10 AND < 60
019030         MOVE "Discount"       TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019040         GO TO CB20.
019020     IF W40-SDISC > 10
019030         MOVE "Settlement Discount"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
019040         GO TO CB20.
018760     IF NOT(W40-INV-TYPE = "I" OR "E" OR "X")
018770         MOVE "Invoice type"   TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018780         GO TO CB20.
018760     IF NOT(W40-OI = "O" OR "B")
018770         MOVE "Statement type" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018780         GO TO CB20.
018760     IF NOT(W40-LAB = "Y" OR "N")
018770         MOVE "Label code"     TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018780         GO TO CB20.
018760     IF NOT(W40-PRICE = "1" OR "2" OR "3")
018770         MOVE "Price code"     TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
018780         GO TO CB20.
           IF W40-CONAC NOT = SPACES
               MOVE W40-CONAC	       TO DEB-ACNO
012020         PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               IF WS-F-ERROR = 6
                   MOVE "Consolidation Account"  TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   GO TO CB20.

       CB24.
019690       DISPLAY "Generating Account" AT 5018 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
           IF W40-ART11 = "N"
               GO TO CB25.

       CB24-ART11.
             PERFORM SAVE-SCREEN-3.
             MOVE 12	                TO SHADE-ROW.
             MOVE 18	                TO SHADE-COL.
             MOVE 48	                TO SHADE-WIDTH.
             MOVE 2	                TO SHADE-LINES.
             DISPLAY S-ART11.
             PERFORM SCREEN-SHADOW.

       CB24-EXEMPT.
             ACCEPT W40-EXEMPT AT 1263 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING W40-EXEMPT Y VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W40-EXEMPT = "N" OR "Y")
               GO TO CB24-EXEMPT.
             PERFORM RESTORE-SCREEN-3.

019730 CB25.
             PERFORM AY60 THRU AY999.

       CB25A.
           IF WS-ASSIGN = "Y"
               IF DEB-AC-NO = ZERO
                   ADD 1             TO DEB-AC-NO
               END-IF
               MOVE DEB-AC-NO        TO DEB-ACNO
012020         PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               IF WS-F-ERROR = 0
                   IF DEB-AC-NO = 999999
                       MOVE 1        TO DEB-AC-NO
                       GO TO CB25A
                   ELSE
                       ADD 1         TO DEB-AC-NO
                       GO TO CB25A
                   END-IF
               ELSE
                   MOVE DEB-ACNO     TO W80-ACNO
016570             DISPLAY S36 AT 0615.
             INITIALIZE DEB-RECORD1.
018040     IF WS-TYPE = "I"
018050         MOVE 1                TO W80-TYPE
018060     ELSE
018070     IF WS-TYPE = "C"
018080         MOVE 2                TO W80-TYPE
018090     ELSE
018100         MOVE 3                TO W80-TYPE.
             MOVE 23                 TO CRT-START CRT-END.
018380       PERFORM ERASE-SCREEN.
             MOVE W80-ACNO           TO DEB-ACNO LS-ACNO.
019840       MOVE W40-NAME1          TO DEB-NAME.
             MOVE W40-CONTACT        TO DEB-CONTACT.
019850       MOVE W40-ADDRESS1       TO DEB-ADDRESS.
019860       MOVE W40-ADDRESS        TO DEB-PADDR.
019870       MOVE W40-POST           TO DEB-POST.
019880       MOVE W40-PPOST          TO DEB-PPOST.
019890       MOVE W40-LANG           TO DEB-LANG.
019900       MOVE W40-TEL            TO DEB-B-TEL.
019910       MOVE W40-HTEL           TO DEB-H-TEL.
019900       MOVE W40-FAX            TO DEB-FAX.
019920       MOVE W40-ID             TO DEB-ID.
019900       MOVE W40-TAX            TO DEB-TAX.
019930       MOVE W40-CAT            TO DEB-CAT.
019940       MOVE W40-TITLE          TO DEB-TITLE.
             MOVE W40-INV-TYPE       TO DEB-INV-TYPE.
019950       MOVE W80-TYPE           TO DEB-TYPE.
019960       MOVE W40-CRL            TO DEB-CRL.
019960       MOVE W40-OI             TO DEB-OI.
019960       MOVE W40-LAB            TO DEB-LAB.
             MOVE W40-PRICE          TO DEB-PRICE.
019970       MOVE W40-FIN            TO DEB-FIN.
020010       MOVE W40-TERMS          TO DEB-TERMS.
020020       MOVE W40-DISC           TO DEB-DISC.
020020       MOVE W40-SDISC          TO DEB-SDISC.
020030       MOVE W40-SMAN           TO DEB-SMAN.
             MOVE W40-CONAC          TO DEB-CONAC.
             MOVE W40-EXEMPT         TO DEB-ART11.
             MOVE "N"                TO DEB-RESTRICT.
020100       PERFORM WRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY02 THRU AY59.
018040     IF WS-TYPE = "I"
018050         MOVE 1                TO W80-TYPE
018060     ELSE
018070     IF WS-TYPE = "C"
018080         MOVE 2                TO W80-TYPE
018090     ELSE
018100         MOVE 3                TO W80-TYPE.
             ADD 1                   TO DEB-ACCOUNTS.

020280 CB999.
020290       EXIT.
      /
      *    ****  A L T E R N A T E   N A M E   I N D E X
      *

	     COPY "DEBTOR.NIX".

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-L-OR-N          TO W02-L-OR-N.
             MOVE WS-SYS-ID          TO W02-SYSID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
042000       DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
042080       MOVE 3              TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-ART11 = "Y" OR "N")
	       MOVE "N" 	 TO PAR-ART11.
	     MOVE PAR-ART11	 TO W40-ART11.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
042720       MOVE 1              TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
044780	     GO TO ZA999-EXIT.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1          TO  WS-BCHR(WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-BCHR (1).
             MOVE WS-G13         TO WS-BCHR (80).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE LS-COMPANY         TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO CRT-WRKHD
	   ELSE
	       MOVE "Workstation"  TO CRT-WRKHD
	       MOVE LS-USER	   TO CRT-WRKST.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
042900 ZB000-ERROR	     SECTION 8.

       COPY "ERRORS.PRO".

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK    TO WS-FILE
041630         MOVE WS-NETKEY      TO WS-KEY
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR     TO WS-FILE
041790         MOVE ZERO           TO WS-KEY
               MOVE DEB-ACNO       TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID       TO WS-FILE
041870         MOVE WS-PARKEY      TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER    TO WS-FILE
041870         MOVE WS-RECKEY      TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM     TO WS-FILE
041870         MOVE ZERO           TO WS-KEY
               MOVE DME-KEY	     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 37
041860         MOVE W02-SHARED     TO WS-FILE
041870         MOVE WS-SHARED      TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 47
041860         MOVE W02-DEBALT     TO WS-FILE
041870         MOVE ZERO           TO WS-KEY
               MOVE DAL-KEY        TO WS-KEYX.

       COPY "DISPERR.PRO".
