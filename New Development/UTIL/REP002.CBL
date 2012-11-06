      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****	    ****     ******    *
      *   **    **  **	      **    **   **  **    **  **   **	**   *
      *   **    **  **	      **    **  **    **  **	  **	      **   *
      *   ******    *****     *******   **    **  **	  **      **     *
      *   **   **   **	      **	    **    **  **	  **    **       *
      *   **    **  **	      **	     **  **    **  **    **	     *
      *   **    **  ********  **	      ****	    ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *       D A Y - E N D   R E P O R T S   P R O G R A M	           *
      *                                                                *
      *     VERSION 9.00.00 - August 2012	                             *
      * 					                                   *
      ******************************************************************
      /
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     REP002.
000030 AUTHOR.	     James William Lemmon.
000040 DATE-WRITTEN.   AUGUST 1982.

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

000060 ENVIRONMENT DIVISION.
000070 CONFIGURATION SECTION.
000080 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		             CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY "APACFIDS.SL".

000420	   SELECT AUDIT   ASSIGN DISK
000430                        LOCK EXCLUSIVE
000440                        STATUS WS-STATUS
000450                        ACCESS RANDOM
000460                        ORGANIZATION RELATIVE
000470                        RELATIVE WS-AUDKEY.
000110	   SELECT NETWORK ASSIGN DISK
                              LOCK EXCLUSIVE
000120                        STATUS WS-STATUS
000130                        ACCESS RANDOM
000140                        ORGANIZATION RELATIVE
000150                        RELATIVE WS-NETKEY.
      $SET KEYCOMPRESS "7"
000170	   SELECT DEBTOR  ASSIGN DISK
000190                        LOCK EXCLUSIVE
000180                        STATUS WS-STATUS
000190                        ACCESS DYNAMIC
000200                        ORGANIZATION INDEXED
000210                        RECORD DEB-ACNO
                              ALTERNATE RECORD DEB-NKEY DUPLICATES.
000110	   SELECT DEPART  ASSIGN DISK
                              LOCK EXCLUSIVE
000040			      STATUS WS-STATUS
000190                        ACCESS DYNAMIC
000200                        ORGANIZATION INDEXED
000210			      RECORD DPT-CODE
			            ALTERNATE RECORD DPT-NKEY DUPLICATES.
      $SET NOKEYCOMPRESS
000120	   SELECT PARAM   ASSIGN DISK
000130                        LOCK EXCLUSIVE
000140                        STATUS WS-STATUS
000150                        ACCESS RANDOM
000160                        ORGANIZATION RELATIVE
000170                        RELATIVE WS-PARKEY.
      $SET KEYCOMPRESS "7"
000300	   SELECT STOCK   ASSIGN DISK
000310                        LOCK EXCLUSIVE
000320                        STATUS WS-STATUS
000130                        ACCESS DYNAMIC
000140                        ORGANIZATION INDEXED
000070			      RECORD STK-CODE
000080                        ALTERNATE RECORD STK-ACODE DUPLICATES
000090                        ALTERNATE RECORD STK-BSEQ = STK-BIN STK-CODE
000110                        ALTERNATE RECORD STK-LSEQ = STK-LDG STK-CODE
000130                        ALTERNATE RECORD STK-DKEY DUPLICATES
000130			      ALTERNATE RECORD STK-DKEY2 DUPLICATES
000140                        ALTERNATE RECORD STK-XREF DUPLICATES.
      $SET NOKEYCOMPRESS

000490 DATA DIVISION.
000500 FILE SECTION.

       COPY "APACFIDS.FDE".

000620 COPY "AUDIT.IFD".

000520 COPY "CONTROL.IFD".

000540 COPY "DEBTOR.GFD".

000520 COPY "DEPART.GFD".

000520 COPY "PARAM.IFD".

000560 COPY "STOCK.GFD".

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
003220 WORKING-STORAGE SECTION.
       77  WS-CHECK      PIC  X(18)	 VALUE "aeWlimemnomLalismJ".
003230 77  WS-CLEAR      PIC  9(01)	 VALUE ZERO.
       77  WS-AUD	       PIC  9(06)	 COMP-5 VALUE ZERO.
003240 77  WS-S1	       PIC  9(04)	 COMP-5.
003250 77  WS-S2	       PIC  9(04)	 COMP-5.
003260 77  WS-S3	       PIC  9(04)	 COMP-5.
003270 77  WS-S4	       PIC  9(04)	 COMP-5.
003280 77  WS-S5	       PIC  9(04)	 COMP-5.
003290 77  WS-S6	       PIC  9(04)	 COMP-5.
003290 77  WS-S7	       PIC  9(04)	 COMP-5.
003300 77  WS-NO	       PIC  9(04)	 COMP-5.
003320 77  WS-PARKEY     PIC  9(04)	 COMP-5.
003330 77  WS-NETKEY     PIC  9(04)	 COMP-5.
003360 77  WS-AUDKEY     PIC  9(06)	 COMP-5.
       77  WS-USE-ITM    PIC  X(01).
003400 77  WS-PAGE       PIC  9(04)	 COMP-5.
003440 77  WS-OPTION     PIC  X(01).
       77  WS-STATOP     PIC  X(01).
003470 77  WS-ERROR      PIC  9(01).
003620 77  WS-REP1       PIC  9(01)	 VALUE 0.
003630 77  WS-REP2       PIC  9(01)	 VALUE 0.
003640 77  WS-REP3       PIC  9(01)	 VALUE 0.
003650 77  WS-REP4       PIC  9(01)	 VALUE 0.
003660 77  WS-REP5       PIC  9(01)	 VALUE 0.
003670 77  WS-REP6       PIC  9(01)	 VALUE 0.
003680 77  WS-REP7       PIC  9(01)	 VALUE 0.
003690 77  WS-REP8       PIC  9(01)	 VALUE 0.
003700 77  WS-REP9       PIC  9(01)	 VALUE 0.
003710 77  WS-REP10      PIC  9(01)	 VALUE 0.
003710 77  WS-REP11      PIC  9(01)	 VALUE 0.
003720 77  WS-H1	       PIC  X(18)	 VALUE "*** END REPORT ***".
003730 77  WS-H2	       PIC  X(06)	 VALUE "M.T.D.".
003740 77  WS-H3	       PIC  X(06)	 VALUE "Y.T.D.".
003750 77  WS-H4	       PIC  X(05)	 VALUE "TOTAL".
003760 77  WS-H5	       PIC  X(06)	 VALUE "CREDIT".
003770 77  WS-H6	       PIC  X(05)	 VALUE "DEBIT".
003780 77  WS-H7	       PIC  X(09)	 VALUE "ITEM CODE".
003790 77  WS-H8	       PIC  X(11)	 VALUE "DESCRIPTION".
003800 77  WS-H9	       PIC  X(05)	 VALUE "QUANT".
003810 77  WS-H10	       PIC  X(04)	 VALUE "CASH".
003820 77  WS-H11	       PIC  X(05)	 VALUE "VALUE".
003830 77  WS-H12	       PIC  X(03)	 VALUE "NO.".
003840 77  WS-H13	       PIC  X(03)	 VALUE "MTD".
003850 77  WS-H14	       PIC  X(03)	 VALUE "YTD".
       77  TODAY-DDMMYY	 PIC  9(08)	 COMP-5.
       77  WS-USUB	 PIC  9(04)	 COMP-5.

003880 COPY "WS.WS".

000290 01  WS-PARID.
000020     03  WS-SYS-ID       PIC  X(03).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.	  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      * 	 C =  Clear comment line (50)
      *	 E =  Clear lines (any line or lines between 5 and 46)
      * 	 H =  Change heading
      * 	 S =  Clear the screen
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END	             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "REP002".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "DAY-END REPORTS".
      *
      *    Company Name
      *
           03  CRT-COMP              PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD             PIC  X(11).
           03  CRT-WRKST             PIC  X(03) VALUE "ÄÄÄ".

003900 01  W02-FID.

       COPY "APACFIDS.ID".

       COPY "AUDIT.ID".

       COPY "CONTROL.ID".

       COPY "DEPART.ID".

       COPY "DEBTOR.ID".

       COPY "PARAM.ID".

       COPY "STOCK.ID".

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER     PIC  X(12).
002570     03  W02-PGE-LENGTH  PIC  9(03).
001870     03  W02-PRN-LENGTH  PIC  9(03).
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

000660     03  R-L1 REDEFINES W02-PRN-LINE.
000670         05  R-DETAIL	 PIC  X(136).

000680     03  R-L2 REDEFINES W02-PRN-LINE.
000690         05  R-PGE       PIC  X(06).
000700         05  R-PNO       PIC  Z(03)9.
000710         05  FILLER      PIC  X(09).
000720         05  R-CPY       PIC  X(40).
000730         05  FILLER      PIC  X(05).
000740         05  R-DATE      PIC  X(06).
000750         05  R-DTE       PIC  Z9/99/9999.

000840     03  R-L3 REDEFINES W02-PRN-LINE.
000850         05  FILLER      PIC	X(30).
000860         05  R-HEAD      PIC	X(20).
000870         05  FILLER      PIC	X(30).

000920     03  R-L4 REDEFINES W02-PRN-LINE.
000930         05  FILLER      PIC	X(02).
000940         05  R-H1        PIC	X(17).
000950         05  R-H2        PIC	X(27).
000960         05  R-H3        PIC	X(09).
000970         05  R-H4        PIC	X(17).
000980         05  R-H5        PIC	X(08).

000990     03  R-L5 REDEFINES W02-PRN-LINE.
001000         05  FILLER      PIC	X(02).
001010         05  R-I1        PIC	X(15).
001020         05  FILLER      PIC	X(02).
001030         05  R-DESC1     PIC	X(24).
001040         05  FILLER      PIC	X(01).
001050         05  R-Q1        PIC	Z(06)9.
001060         05  FILLER      PIC	X(02).
001070         05  R-UP1       PIC	Z(07)9.99.
001080         05  FILLER      PIC	X(02).
001090         05  R-V1        PIC	Z(08)9.99.
001100         05  FILLER      PIC	X(02).

001110     03  R-L6 REDEFINES W02-PRN-LINE.
001120         05  R-H6        PIC	X(19).
001130         05  R-H7        PIC	X(31).
001140         05  R-H8        PIC	X(07).
001150         05  R-H9        PIC	X(09).
001160         05  R-H10       PIC	X(08).
001170         05  R-H11       PIC	X(11).
001180         05  R-H12       PIC	X(07).
001190         05  R-H13       PIC	X(04).

001200     03  R-L7 REDEFINES W02-PRN-LINE.
001210         05  R-I2.
001780             07  R-ITM   PIC	X(14).
                   07  R-SL    PIC	X(01).
                   07  R-EXT   PIC	X(03).
001220         05  FILLER      PIC	X(01).
001230         05  R-DESC2     PIC	X(29).
001270         05  FILLER      PIC	X(01).
001240         05  R-LEV       PIC	Z(05)9.
001250         05  FILLER      PIC	X(01).
001260         05  R-Q2        PIC	Z(05)9.
001270         05  FILLER      PIC	X(01).
001280         05  R-COST1     PIC	Z(05)9.99.
001290         05  FILLER      PIC	X(01).
001300         05  R-AVGE      PIC	Z(05)9.99.
001310         05  FILLER      PIC	X(01).
001320         05  R-NMTD      PIC	Z(05)9.
001330         05  FILLER      PIC	X(01).
001340         05  R-NYTD      PIC	Z(05)9.

001350     03  R-L8 REDEFINES W02-PRN-LINE.
001360         05  R-H14       PIC	X(07).
001370         05  R-H14A      PIC	X(41).
001380         05  R-H15       PIC	X(11).
001390         05  R-H16       PIC	X(13).
001400         05  R-H17       PIC	X(08).

001410     03  R-L9 REDEFINES W02-PRN-LINE.
001420         05  R-AC        PIC	X(06).
001430         05  FILLER      PIC	X(01).
001440         05  R-DNME      PIC	X(39).
001450         05  FILLER      PIC	X(01).
001460         05  R-REF       PIC	X(08).
001470         05  FILLER      PIC	X(01).
001480         05  R-DEBT      PIC	Z(06)9.99-.
001490         05  FILLER      PIC	X(02).
001500         05  R-CASH      PIC	Z(06)9.99-.

001510     03  R-L10 REDEFINES W02-PRN-LINE.
001520         05  R-H18       PIC	X(06).
001530         05  R-H19       PIC	X(32).
001540         05  R-H20       PIC	X(12).
001550         05  R-H21       PIC	X(12).
001560         05  R-H22       PIC	X(12).
001570         05  R-H23       PIC	X(06).

001790     03  R-L11 REDEFINES W02-PRN-LINE.
001800         05  FILLER      PIC	X(46).
001810         05  R-H24       PIC	X(21).
001820         05  R-H25       PIC	X(13).

001940     03  R-L12 REDEFINES W02-PRN-LINE.
001950         05  FILLER      PIC	X(01).
001960         05  R-LEDG      PIC	9(03).
001970         05  FILLER      PIC	X(01).
001980         05  R-SDESC     PIC	X(27).
001970         05  FILLER      PIC	X(01).
001990         05  R-DMTD      PIC	-(07)9.99.
002000         05  FILLER      PIC	X(01).
002010         05  R-DYTD      PIC	-(07)9.99.
002020         05  FILLER      PIC	X(01).
002030         05  R-CMTD      PIC	-(07)9.99.
002040         05  FILLER      PIC	X(01).
002050         05  R-CYTD      PIC	-(07)9.99.

002420     03  R-L13 REDEFINES W02-PRN-LINE.
002430         05  R-H26       PIC	X(20).
002440         05  FILLER      PIC	X(02).
002450         05  R-H27       PIC	X(05).
002460         05  R-H28       PIC	X(06).
002470         05  R-H29       PIC	X(09).
002480         05  R-H30       PIC	X(06).
002490         05  R-H31       PIC	X(05).
002500         05  R-H32       PIC	X(10).
002510         05  R-H33       PIC	X(05).
002520         05  R-H34       PIC	X(06).
002530         05  R-H35       PIC	X(06).

002540     03  R-L14 REDEFINES W02-PRN-LINE.
002550         05  FILLER      PIC	X(05).
002560         05  R-H36       PIC	X(15).
002570         05  R-N1        PIC	Z(04)9.
	         05  R-VAT REDEFINES R-N1 PIC  Z9.99.
	         05  R-PERC      PIC	X(01).
002580         05  FILLER      PIC	X(01).
002590         05  R-V5        PIC	Z(08)9.99-.
002600         05  FILLER      PIC	X(01).
002610         05  R-N2        PIC	Z(04)9.
002620         05  FILLER      PIC	X(01).
002630         05  R-V6        PIC	Z(08)9.99-.
002640         05  FILLER      PIC	X(01).
002650         05  R-N3        PIC	Z(04)9.
002660         05  FILLER      PIC	X(01).
002670         05  R-V7        PIC	Z(08)9.99-.

000760     03  REP-LINE3 REDEFINES W02-PRN-LINE.
000790         05  REP-COMPANY PIC	X(40).
000800         05  REP-DATE    PIC	X(06).
000810         05  REP-DTE     PIC	Z9/99/9999.
000820         05  FILLER      PIC	X(16).

000830     03  REP-LINE4 REDEFINES W02-PRN-LINE.
000870         05  REP-REMK    PIC	X(20).
000880         05  FILLER      PIC	X(01).
	         05  REP-VAL-NO.
000890             07  REP-VAL PIC	Z(06)9.99-.
	         05  REP-VAL-NO-R REDEFINES REP-VAL-NO.
                   07  REP-NO  PIC	Z(06)9.
                   07  FILLER  PIC	X(04).
000900         05  FILLER      PIC	X(24).

002680     03  SAL-L1 REDEFINES W02-PRN-LINE.
002690         05  SAL-H1      PIC	X(10).
002700         05  SAL-NO      PIC	ZZ9.
002710         05  FILLER      PIC	X(01).
002720         05  SAL-SMAN    PIC	X(30).
002730         05  FILLER      PIC	X(36).

002740     03  SAL-L2 REDEFINES W02-PRN-LINE.
002760         05  SAL-H2      PIC	X(15).
002770         05  SAL-H3      PIC	X(36).
002780         05  SAL-H4      PIC	X(09).
002790         05  SAL-H5      PIC	X(11).
002800         05  SAL-H6      PIC	X(08).
	         05  SAL-H6A     PIC	X(09).
	         05  SAL-H6B     PIC	X(06).

002810     03  SAL-L3 REDEFINES W02-PRN-LINE.
002830         05  SAL-ITEM    PIC	X(14).
002840         05  FILLER      PIC	X(01).
002850         05  SAL-DESC    PIC	X(28).
002860         05  FILLER      PIC	X(01).
002870         05  SAL-COST    PIC	Z(07)9.99-.
002880         05  FILLER      PIC	X(01).
002890         05  SAL-SELL    PIC	Z(07)9.99-.
002900         05  FILLER      PIC	X(01).
002910         05  SAL-GP      PIC	ZZ9.99-.
002920         05  FILLER      PIC	X(02).
               05  SAL-MARKUP  PIC	ZZ9.99-.
               05  FILLER      PIC	X(01).
               05  SAL-NOCHARGE.
	             07  FILLER	   PIC  X(01).
	             07  SAL-SUPER PIC  ZZ.
	         05  FILLER      PIC	X(01).
	         05  SAL-PAY     PIC	X(03).

002930     03  SAL-L4 REDEFINES W02-PRN-LINE.
002940         05  FILLER      PIC	X(02).
002950         05  SAL-H7      PIC	X(14).
002960         05  SAL-REF     PIC	X(08).
002970         05  FILLER      PIC	X(02).
002980         05  SAL-H8      PIC	X(22).
002990         05  FILLER      PIC	X(30).

003000     03  SAL-L5 REDEFINES W02-PRN-LINE.
003010         05  FILLER      PIC	X(02).
003020         05  SAL-H9      PIC	X(10).
003030         05  SAL-H10     PIC	X(20).
003040         05  SAL-H11     PIC	X(12).
003050         05  SAL-H12     PIC	X(16).
003060         05  SAL-H13     PIC	X(13).
003070         05  SAL-H14     PIC	X(07).

003080     03  SAL-L6 REDEFINES W02-PRN-LINE.
003090         05  FILLER      PIC	X(01).
003100         05  SAL-RF      PIC	X(08).
003110         05  FILLER      PIC	X(03).
003120         05  SAL-DSC     PIC	X(11).
003130         05  FILLER      PIC	X(02).
003140         05  SAL-CST     PIC	Z(08)9.99-.
003150         05  FILLER      PIC	X(02).
003160         05  SAL-SEL     PIC	Z(08)9.99-.
003170         05  FILLER      PIC	X(02).
003180         05  SAL-PRF     PIC	Z(08)9.99-.
003190         05  FILLER      PIC	X(04).
003200         05  SAL-GPR     PIC	ZZ9.99.
003210         05  FILLER      PIC	X(02).

       COPY "W12.WS".

       COPY "W15.WS".

004310 01  W25-CALCS.
           03  W25-CALCULATIONS.
005040         05  W25-RESULT	 PIC  9(05)V99.
004330         05  W25-RESULT1 REDEFINES W25-RESULT.
004340	       07  W25-WHOLE   PIC  9(05).
004350	       07  W25-DECIMAL PIC  9(02).
	         05  W25-RESULT2 REDEFINES W25-RESULT.
                   07  W25-KEY PIC  9(04).
                   07  W25-SUB PIC  9(01).
                   07  FILLER	 PIC 9(02).
004360         05  W25-TOTAL	 PIC S9(09)V99  COMP-3.
004370         05  W25-VALUE	 PIC S9(09)V99  COMP-3.
004380         05  W25-COST1	 PIC S9(09)V99  COMP-3.
004390         05  W25-COST2	 PIC S9(09)V99  COMP-3.
004400         05  W25-COST3	 PIC S9(09)V99  COMP-3.
004410         05  W25-SELL1	 PIC S9(09)V99  COMP-3.
004420         05  W25-SELL2	 PIC S9(09)V99  COMP-3.
004430         05  W25-SELL3	 PIC S9(09)V99  COMP-3.
004380         05  W25-CASH	 PIC S9(09)V99  COMP-3.
004390         05  W25-REFF	 PIC S9(09)V99  COMP-3.
004400         05  W25-INV	 PIC S9(09)V99  COMP-3.
004410         05  W25-CRN	 PIC S9(09)V99  COMP-3.

004440 01  W40-INDIC.
004450     03  W40-SMAN.
004460         05  W40-SREP    PIC  9(01) COMP-5 OCCURS 50.
004470     03  W40-DRAW.
004480         05  W40-DREP    PIC  9(01) COMP-5 OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

004890 01  W70-WORK.
004900     03  W70-REP         PIC  9(04)     COMP-5.

004910 01  W80-ACCOUNT.
004920     03  W80-ACNO        PIC  9(05).

004960 01  W90-TRAN.
004970     03  W90-DATE        PIC  X(10).
004980     03  W90-DTE REDEFINES W90-DATE PIC 99/99/9999.
005000     03  W90-CODE        PIC  9(02).
005010     03  W90-REF         PIC  X(06).
005020     03  W90-DB          PIC  9(09)V99 COMP-3.
005030     03  W90-CR          PIC  9(09)V99 COMP-3.
005040     03  W90-BDT         PIC S9(09)V99 COMP-3.
005050     03  W90-BCR         PIC S9(09)V99 COMP-3.
005060     03  W90-DESC        PIC  X(12).
005070     03  W90-90          PIC S9(09)V99 COMP-3.
005080     03  W90-60          PIC S9(09)V99 COMP-3.
005090     03  W90-30          PIC S9(09)V99 COMP-3.
005100     03  W90-CUR         PIC S9(09)V99 COMP-3.
005110     03  W90-BAL         PIC S9(09)V99 COMP-3.

005390 01  W200-TRAN.
005400     03  W200-ACCUM.
               05  W200-LEDG   PIC  X(04)	   OCCURS 300.
005410         05  W200-DT     PIC S9(09)V99 COMP-3 OCCURS 300.
005420         05  W200-CR     PIC S9(09)V99 COMP-3 OCCURS 300.
005430         05  W200-DTOT   PIC S9(09)V99 COMP-3.
005440         05  W200-CTOT   PIC S9(09)V99 COMP-3.
005450         05  W200-COST   PIC S9(09)V99 COMP-3.
005460         05  W200-SELL   PIC S9(09)V99 COMP-3.
005470         05  W200-NET    PIC S9(09)V99 COMP-3.
005470         05  W200-ADJ    PIC S9(09)V99 COMP-3.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
005480 SCREEN SECTION.

       01  MENU-INSTRUCT.
	     02  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
	         03  LINE 25 COLUMN  2 VALUE "Use ".
	         03          COLUMN  6 VALUE ""	                         FOREGROUND-COLOR Brown HIGHLIGHT.
	         03          COLUMN  7 VALUE " or ".
	         03          COLUMN 11 VALUE ""	                         FOREGROUND-COLOR Brown HIGHLIGHT..
	         03          COLUMN 12 VALUE " to point to an item, and ".
	         03          COLUMN 38 VALUE "<Enter>"	                   FOREGROUND-COLOR Brown HIGHLIGHT..
	         03          COLUMN 45 VALUE " to Select ".
	         03          COLUMN 56 VALUE "<Esc>"	                   FOREGROUND-COLOR Brown HIGHLIGHT..
	         03          COLUMN 61 VALUE " to Exit".

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 31 VALUE "DAY/MONTH/YEAR END" FOREGROUND-COLOR Grey HIGHLIGHT.

005960 01  S03.
005970	   03  LINE  2 COLUMN 32 VALUE "DAY-END REPORTS"    FOREGROUND-COLOR Grey HIGHLIGHT.

005990 01  S04.
	     02  BACKGROUND-COLOR Green FOREGROUND-COLOR Green.
               03  LINE  6 COLUMN 29 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" HIGHLIGHT.
               03          COLUMN 52 VALUE "¿"                       FOREGROUND-COLOR Black.
006000         03  LINE  7 COLUMN 29 VALUE "³  "                     HIGHLIGHT.
               03          COLUMN 32 VALUE "STATISTICS REPORTS"      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 50 VALUE "  ³"                     FOREGROUND-COLOR Black.
006020         03  LINE  8 COLUMN 29 VALUE "³ "                      HIGHLIGHT.
               03          COLUMN 31 VALUE "1"                       FOREGROUND-COLOR Grey HIGHLIGHT.
006030         03          COLUMN 32 VALUE " Debtors            "    FOREGROUND-COLOR Black.
               03          COLUMN 52 VALUE "³"                       FOREGROUND-COLOR Black.
006040         03  LINE  9 COLUMN 29 VALUE "³ "	                     HIGHLIGHT.
               03          COLUMN 31 VALUE "2"                       FOREGROUND-COLOR Grey HIGHLIGHT.
006050         03          COLUMN 32 VALUE " Creditors          "    FOREGROUND-COLOR Black.
               03          COLUMN 52 VALUE "³"                       FOREGROUND-COLOR Black.
006080         03  LINE 10 COLUMN 29 VALUE "³ "                      HIGHLIGHT.
               03          COLUMN 31 VALUE "3"                       FOREGROUND-COLOR Grey HIGHLIGHT.
006090         03          COLUMN 32 VALUE " Stock              "    FOREGROUND-COLOR Black.
               03          COLUMN 52 VALUE "³"                       FOREGROUND-COLOR Black.
006100         03  LINE 11 COLUMN 29 VALUE "³ "                      HIGHLIGHT.
               03          COLUMN 31 VALUE "4"                       FOREGROUND-COLOR Grey HIGHLIGHT.
006110         03          COLUMN 32 VALUE " All the above      "    FOREGROUND-COLOR Black.
               03          COLUMN 52 VALUE "³"                       FOREGROUND-COLOR Black.
006120         03  LINE 12 COLUMN 29 VALUE "³ "                      HIGHLIGHT.
               03          COLUMN 31 VALUE "0"                       FOREGROUND-COLOR Grey HIGHLIGHT.
006130         03          COLUMN 32 VALUE " Period-End Control "    FOREGROUND-COLOR Black.
               03          COLUMN 52 VALUE "³"                       FOREGROUND-COLOR Black.
006140         03  LINE 13 COLUMN 29 VALUE "À"                       HIGHLIGHT.
               03          COLUMN 30 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Black.

006370 01  S06.
           02  BACKGROUND-COLOR Green FOREGROUND-COLOR Green.
006380         03  LINE  2 COLUMN 30 VALUE "DAY-END REPORT OPTIONS"            FOREGROUND-COLOR Grey HIGHLIGHT.
               03  LINE  8 COLUMN 23 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" HIGHLIGHT.
               03          COLUMN 56 VALUE "¿"                                 FOREGROUND-COLOR Black.
006230         03  LINE  9 COLUMN 23 VALUE "³ "	                               HIGHLIGHT.
               03          COLUMN 25 VALUE "1"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
006410         03          COLUMN 26 VALUE " Stock Re-Order               "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"	                               FOREGROUND-COLOR Black.
006420         03  LINE 10 COLUMN 23 VALUE "³ "	                               HIGHLIGHT.
               03          COLUMN 25 VALUE "2"	                               FOREGROUND-COLOR Grey HIGHLIGHT.
006430         03          COLUMN 26 VALUE " Debtor Transactions          "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"	                               FOREGROUND-COLOR Black.
006440         03  LINE 11 COLUMN 23 VALUE "³ "	                               HIGHLIGHT.
               03          COLUMN 25 VALUE "3"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
006450         03          COLUMN 26 VALUE " Cash Drawers                 "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"                                 FOREGROUND-COLOR Black.
006460         03  LINE 12 COLUMN 23 VALUE "³ "	                               HIGHLIGHT.
               03          COLUMN 25 VALUE "4"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
006470         03          COLUMN 26 VALUE " Salesman                     "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"	                               FOREGROUND-COLOR Black.
006480         03  LINE 13 COLUMN 23 VALUE "³ "                                HIGHLIGHT.
               03          COLUMN 25 VALUE "5"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
006490         03          COLUMN 26 VALUE " Turnover and G.P.            "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"                                 FOREGROUND-COLOR Black.
006500         03  LINE 14 COLUMN 23 VALUE "³ "                                HIGHLIGHT.
               03          COLUMN 25 VALUE "6"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
006510         03          COLUMN 26 VALUE " Turnover and G.P. (Detailed) "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"                                 FOREGROUND-COLOR Black.
               03  LINE 15 COLUMN 23 VALUE "³ "                                HIGHLIGHT.
               03          COLUMN 25 VALUE "7"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE " Debtor Receipts (POS)        "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"                                 FOREGROUND-COLOR Black.
               03  LINE 16 COLUMN 23 VALUE "³ "                                HIGHLIGHT.
               03          COLUMN 25 VALUE "8"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE " Discounted Sales             "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"                                 FOREGROUND-COLOR Black.
006520         03  LINE 17 COLUMN 23 VALUE "³ "                                HIGHLIGHT.
               03          COLUMN 25 VALUE "0"                                 FOREGROUND-COLOR Grey HIGHLIGHT.
006530         03          COLUMN 26 VALUE " Exit from this MENU          "    FOREGROUND-COLOR Black.
               03          COLUMN 56 VALUE "³"                                 FOREGROUND-COLOR Black.
006330         03  LINE 18 COLUMN 23 VALUE "À"                                 HIGHLIGHT.
               03          COLUMN 24 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Black.

      /
      *        *******    *******     ******     ******
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ******
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006570 AA00M           SECTION.
006580 AA00.
006590       PERFORM ZA00-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *	  *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *     *   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	    *   * *** *   *   *   ***** *	*
      *
             CLOSE PARAM.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As the parameter file has been assigned exclusive in this ³
      *    ³ program, it is  necessary to close it.  W02-LINAGE is set ³
      *    ³ to 99 which informs the PRINTUTL program to open the file ³
      *    ³ and close it again on exit.	                             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 99           TO W02-LINAGE.
      *
      *    ****     S E T   T O   8   L I N E S   P E R   I N C H
      *
             MOVE 8            TO W02-PRN-TYPE.
             PERFORM OPEN-PRINTER.
             MOVE ZERO         TO W02-LINAGE.
029050       OPEN I-O PARAM LOCK.
             MOVE "P"          TO WS-COMMAND.
             MOVE "D"          TO W02-PRN-TYPE.
             MOVE SPACES       TO R-L1.

006620 AA01.






007070 AA50.
             MOVE "H"          TO W02-PRN-TYPE.
007080       MOVE SPACES       TO R-DETAIL.
007090       MOVE "PAGE:"      TO R-PGE.
007100       MOVE "DATE:"      TO R-DATE.
007110       ADD 1             TO WS-PAGE.
007120       MOVE WS-PAGE      TO R-PNO.
007130       MOVE W12-TODAY    TO R-DTE.
007140       MOVE CRT-COMP	 TO R-CPY.
             MOVE 2            TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
007160       MOVE SPACES       TO R-DETAIL.

007170 AA55.
007180       EXIT.

       COPY "CLEAR.CRT".

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

       COPY "LOCKED.REC".

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      OPT-MESSAGE                          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 	                             ³
      *    ³	                                                     ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.	                                   ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ at line as the value of WS-MES-LINE. If WS-MES-LINE is    ³
      *    ³ zero the default will be 20.                              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP6".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007310 AN00-READ	       SECTION.
008460 READ-AUDIT.
008470       READ AUDIT.
008480     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008490         GO TO READ-AUDIT-EXIT.
           IF WS-STATUS = 23
               IF (WS-AUDKEY NOT = 1) AND
		  (WS-AUDKEY < W70-REP)
		   ADD 1	 TO WS-AUD WS-AUDKEY
                   GO TO READ-AUDIT.
             MOVE ZERO           TO WS-AUD.
008500     IF WS-STAT1 = "2" OR "3" OR "4"
008510         MOVE 1            TO WS-F-ERROR
008520         PERFORM READ-ERROR.
008530     IF RECORD-LOCKED
008540         PERFORM LOCKED-RECORD
008550         GO TO READ-AUDIT.
008560	     MOVE 1		 TO WS-F-ERROR.
008570       PERFORM READ-ERROR.

008740 READ-AUDIT-EXIT.
008750       EXIT.

008650 READ-CONTROL.
008660       READ NETWORK.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-CONTROL-EXIT.
008690     IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 2            TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008720     IF RECORD-LOCKED
008730         PERFORM LOCKED-RECORD
008740         GO TO READ-CONTROL.
008750       MOVE 2              TO WS-F-ERROR.
008760       PERFORM READ-ERROR.

008930 READ-CONTROL-EXIT.
008940       EXIT.
      *
      *    ****  ***** ****    *   ****  *****	   ***** *** *     *****
      *    *   * *     *   *  * *  *   *   *	   *      *  *     *
      *    *   * *     *   * *   * *   *   *	   *      *  *     *
      *    *   * ***   ****  ***** ****    *	   ***    *  *     ***
      *    *   * *     *   * *   * *  *    *	   *      *  *     *
      *    *   * *     *   * *   * *   *   *	   *      *  *     *
      *    ****  ***** *   * *   * *   *   *	   *     *** ***** *****
      *
008650 READ-DEPART.
008660	     READ DEPART	 WITH IGNORE LOCK
	       KEY IS DPT-CODE.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-DEPART-EXIT.
	   IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
008690	   IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 7            TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008770       GO TO READ-DEPART-EXIT.

006080 READ-DEPART-NEXT.
006090	     READ DEPART NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-DEPART-NEXT.
006200	     GO TO READ-DEPART-EXIT.

006080 START-AT-DEPT-CODE.
006090	     START DEPART
	       KEY >= DPT-CODE.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.

008930 READ-DEPART-EXIT.
008940       EXIT.

      *
      *    ****  ***** ****  *****  ***  ****     ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * ***   ****    *   *   * ****     ***    *  *     ***
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    ****  ***** ****    *    ***  *   *    *     *** ***** *****
      *
000020 READ-DEBTOR.
000030       READ DEBTOR
               KEY IS DEB-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-DEBTOR.
             GO TO READ-DEBTOR-EXIT.

000310 READ-DEBTOR-NEXT.
000320       READ DEBTOR NEXT.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340         GO TO READ-DEBTOR-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360         MOVE 6            TO WS-F-ERROR
000370         GO TO READ-DEBTOR-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390         MOVE 6            TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
000420         PERFORM LOCKED-RECORD
000430         GO TO READ-DEBTOR-NEXT.
             GO TO READ-DEBTOR-EXIT.

000020 START-AT-DEBT-AC.
000030       START DEBTOR
               KEY >= DEB-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000470 READ-DEBTOR-EXIT.
000480       EXIT.

008690 READ-PARAM.
008700       READ PARAM.
008710     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008720         GO TO READ-PARAM-EXIT.
008730     IF WS-STAT1 = "2" OR "3" OR "4"
008740         MOVE 15           TO WS-F-ERROR
008750         PERFORM READ-ERROR.
008760     IF RECORD-LOCKED
008770         PERFORM LOCKED-RECORD
008780         GO TO READ-PARAM.
008790       MOVE 15             TO WS-F-ERROR.
008800       PERFORM READ-ERROR.

008960 READ-PARAM-EXIT.
008970       EXIT.

      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
008190 READ-STOCK.
008200       READ STOCK
               KEY IS STK-CODE.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-STOCK-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 22           TO WS-F-ERROR
009920         GO TO READ-STOCK-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 22           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-STOCK.
008310       GO TO READ-STOCK-EXIT.

008190 READ-STOCK-NEXT.
008200       READ STOCK NEXT.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-STOCK-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 22           TO WS-F-ERROR
009920         GO TO READ-STOCK-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 22           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-STOCK-NEXT.
008310       GO TO READ-STOCK-EXIT.

008190 START-AT-STOCK-CODE.
008200       START STOCK
               KEY >= STK-CODE.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-STOCK-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 22           TO WS-F-ERROR
009920         GO TO READ-STOCK-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 22           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008310       GO TO READ-STOCK-EXIT.

       READ-STOCK-EXIT.
008640       EXIT.
      /
008260 AY000-END              SECTION.
008270 AY000-EOJ.
008280       PERFORM ZB00-EOJ.

       AY005.
             EXIT PROGRAM.
      /
008360 BA000D          SECTION 5.
008370 BA00C.
             MOVE "S"                TO CRT-TYPE.
007750       CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008390       MOVE 1              TO WS-S6.

008400 BA00C-LOOP.
008410	     MOVE ZERO		 TO W40-SREP(WS-S6) W40-DREP(WS-S6).
008420     IF WS-S6 < 40
008430         ADD 1             TO WS-S6
008440         GO TO BA00C-LOOP.
008450       MOVE 41             TO WS-S6.
008460       DISPLAY S03.

008470 BA00C-LP.
008480	     MOVE ZERO		 TO W40-SREP(WS-S6).
008490     IF WS-S6 < 50
008500         ADD 1             TO WS-S6
008510         GO TO BA00C-LP.
008520	     DISPLAY "Do you wish to clear daily statistics after printing Y/N" AT 0404.
008540	     ACCEPT WS-OPTION AT 0461 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
008600	   IF WS-OPTION = "Y"
008610	       MOVE 1		 TO WS-CLEAR.
	     PERFORM SAVE-SCREEN-3.
008650       PERFORM BX000.
008660     IF WS-REP1 = 1
008670         PERFORM BA00R.

008680 BA00C-MENU.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 25		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
021470	     DISPLAY S06.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

021480 BA00D.
	     MOVE 8		 TO TOP-ROW.
	     MOVE 16		 TO BOTTOM-ROW.
	     MOVE 7		 TO SCREEN-LIN.
	     MOVE 25		 TO SCREEN-COL.
	     MOVE 30		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA00D-ACCEPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 BA00D-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA00D-15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA00D-10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA00D-10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA00D-10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA00D-10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA00D-15
	       END-EVALUATE
	       GO TO BA00D-ACCEPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA00DF
	       END-EVALUATE
	       GO TO BA00D-ACCEPT
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < "0" OR > "8"
		   GO TO BA00D-ACCEPT
	       END-IF
	       GO TO BA00D-16
	   ELSE
	       GO TO BA00D-ACCEPT.

       BA00D-15.
	     EVALUATE SCREEN-LIN
	       WHEN 8
		    MOVE "1"	      TO WS-OPTION
	       WHEN 9
		    MOVE "2"	      TO WS-OPTION
	       WHEN 10
		    MOVE "3"	      TO WS-OPTION
	       WHEN 11
		    MOVE "4"	      TO WS-OPTION
	       WHEN 12
		    MOVE "5"	      TO WS-OPTION
	       WHEN 13
		    MOVE "6"	      TO WS-OPTION
	       WHEN 14
		    MOVE "7"	      TO WS-OPTION
	       WHEN 15
		    MOVE "8"	      TO WS-OPTION
	       WHEN 16
		    MOVE "0"	      TO WS-OPTION
	       WHEN OTHER
		    GO TO BA00D-ACCEPT
	     END-EVALUATE.
	     GO TO BA00DF.

       BA00D-16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		    MOVE 16	      TO SCREEN-LIN
	       WHEN "1"
		    MOVE 8	      TO SCREEN-LIN
	       WHEN "2"
		    MOVE 9	      TO SCREEN-LIN
	       WHEN "3"
		    MOVE 10	      TO SCREEN-LIN
	       WHEN "4"
		    MOVE 11	      TO SCREEN-LIN
	       WHEN "5"
		    MOVE 12	      TO SCREEN-LIN
	       WHEN "6"
		    MOVE 13	      TO SCREEN-LIN
	       WHEN "7"
		    MOVE 14	      TO SCREEN-LIN
	       WHEN "8"
		    MOVE 15	      TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA00DF.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     PERFORM CLEAR-L50.
	     EVALUATE WS-OPTION
	       WHEN "0"
		    GO TO BA00E
	       WHEN "1"
008790		    IF WS-REP2 = 1
008800			PERFORM BB00R
008820		    ELSE
008830			MOVE "No re-orders to report"	        TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	       WHEN "2"
008870		    IF WS-REP3 = 1 OR WS-REP4 = 1
008880			PERFORM BC00R
008900		    ELSE
008910			MOVE "No Transactions to report"      TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	       WHEN "3"
008950		    IF WS-REP4 = 1
008960			PERFORM BE00R
008980		    ELSE
008990			MOVE "No Cash draw details to report" TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	       WHEN "4"
009030		    IF WS-REP3 = 1 OR WS-REP4 = 1
009040			PERFORM BF00R
009060		    ELSE
009070			MOVE "No Salesman details to report"  TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	       WHEN "5"
009110		    IF WS-REP3 = 1 OR WS-REP4 = 1
009120			PERFORM BG00R
009140		    ELSE
009150			MOVE "No Turnover to report"	        TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	       WHEN "6"
009190		    IF WS-REP3 = 1 OR WS-REP4 = 1
009200			PERFORM BH00R
009220		    ELSE
009230			MOVE "No Turnover to report"	        TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	       WHEN "7"
009190		    IF WS-REP11 = 1
009200			PERFORM BI00R
009220		    ELSE
009230			MOVE "No Debtor receipts to report"   TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	       WHEN "8"
008870		    IF WS-REP3 = 1
			PERFORM BJ00R
009220		    ELSE
008910			MOVE "No Transactions to report"      TO WS-ERR-MES
003260			PERFORM DISPLAY-MESSAGE
	     END-EVALUATE.
008760	     GO TO BA00C-MENU.

009270 BA00E.
	   IF WS-PRINTER = "S"
	       PERFORM RESTORE-SCREEN-3
	       MOVE "I" 	 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S04.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BA00E-2.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA00E-5.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 BA00E-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA00E-15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA00E-10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA00E-10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA00E-10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA00E-10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA00E-15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA00E-5
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA00F
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA00E-5
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < "0" OR > "4"
		   CALL X"E5"
		   GO TO BA00E-5
	       END-IF
	       GO TO BA00E-16
	   ELSE
	       CALL X"E5"
	       GO TO BA00E-5.

       BA00E-15.
	     EVALUATE SCREEN-LIN
	       WHEN 7
		    MOVE "1"	      TO WS-OPTION
	       WHEN 8
		    MOVE "2"	      TO WS-OPTION
	       WHEN 9
		    MOVE "3"	      TO WS-OPTION
	       WHEN 10
		    MOVE "4"	      TO WS-OPTION
	       WHEN 11
		    MOVE "0"	      TO WS-OPTION
	       WHEN OTHER
		    GO TO BA00E-5
	   GO TO BA00F.

       BA00E-16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		    MOVE 11	      TO SCREEN-LIN
	       WHEN "1"
		    MOVE 7	      TO SCREEN-LIN
	       WHEN "2"
		    MOVE 8	      TO SCREEN-LIN
	       WHEN "3"
		    MOVE 9	      TO SCREEN-LIN
	       WHEN "4"
		    MOVE 10	      TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA00F.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
009320	   IF WS-OPTION = "0"
009330         GO TO BA99.
	     MOVE WS-OPTION	 TO WS-STATOP.
009360       PERFORM BZ00R.
	     PERFORM RESTORE-SCREEN-3.
009370	     GO TO BA00E.

009380 BA99.
009390       EXIT.
      /
009400 BA00R           SECTION 5.
009410 BA00.
             MOVE 05               TO CRT-START.
             MOVE 46               TO CRT-END.
             PERFORM ERASE-SCREEN.




009430	     DISPLAY "Busy printing Lost Sales Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
009450	     MOVE ZERO		 TO W25-TOTAL.
009460	     MOVE 2		 TO WS-AUDKEY.
009470	     MOVE ZERO		 TO WS-PAGE.

009480 BA05.
009490       PERFORM AA50 THRU AA55.
009500	     MOVE "LOST SALES"	 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009520	     MOVE SPACES	 TO R-DETAIL.
009530	     MOVE WS-H7 	 TO R-H1.
009540	     MOVE WS-H8 	 TO R-H2.
009550	     MOVE WS-H9 	 TO R-H3.
009560	     MOVE "UNIT PRICE"	 TO R-H4.
009570	     MOVE WS-H11	 TO R-H5.
	     PERFORM CALL-PRINTUTL.
009590	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

009600 BA10.
009610       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
009650	   IF AUD-TYPE NOT = 1
009660         GO TO BA15.
009670	     MOVE AUD-EXT-CODE	 TO STK-CODE.
009680	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"	   TO STK-DESC.
009690	     MOVE STK-CODE	 TO R-I1.
009700	     MOVE STK-DESC	 TO R-DESC1.
009710	     MOVE AUD-QNT	 TO R-Q1.
009720	     MOVE STK-SELL	 TO R-UP1.
009730	     MULTIPLY STK-SELL BY AUD-QNT
				 GIVING W25-VALUE.
009740	     MOVE W25-VALUE	 TO R-V1.
009750	     ADD W25-VALUE	 TO W25-TOTAL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BA05.
009810	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BA999.
009820 BA15.
009830     IF WS-AUDKEY < W70-REP
009840	       ADD 1		 TO WS-AUDKEY
009850         GO TO BA10.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009870	     MOVE "TOTAL LOST SALES"   TO R-DESC1.
009880	     MOVE W25-TOTAL	 TO R-V1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009900	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
009920	     MOVE SPACES	 TO R-DETAIL.

009930 BA999.
009940       EXIT.
      /
009950 BB00R           SECTION 5.
009960 BB00.
	     PERFORM RESTORE-SCREEN-3.
009980	     DISPLAY "Printing Stock ReOrder Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
010000	     MOVE ZERO		 TO WS-PAGE.
010010	     MOVE 2		 TO WS-AUDKEY.

010020 BB05.
      *
      *    ****    SET PRINTER TO 10 CPI
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
010030	     PERFORM AA50 THRU AA55.
010040	     MOVE "STOCK LOW LEVELS"   TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****    SET PRINTER TO 12 CPI
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
010060	     MOVE SPACES	 TO R-DETAIL.
010070	     MOVE WS-H7 	 TO R-H6.
010080	     MOVE WS-H8 	 TO R-H7.
010090	     MOVE "LEVEL"	 TO R-H8.
010100	     MOVE WS-H9 	 TO R-H9.
010110	     MOVE "COST"	 TO R-H10.
010120	     MOVE "AVERAGE"	 TO R-H11.
010130	     MOVE WS-H13	 TO R-H12.
010140	     MOVE WS-H14	 TO R-H13.
	     PERFORM CALL-PRINTUTL.
010160	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

010170 BB10.
010180       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
010220	   IF AUD-TYPE NOT = 02
010230         GO TO BB15.
010240	     MOVE AUD-EXT-CODE	 TO STK-CODE.
010250       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"
				 TO STK-DESC.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is a 3 character extension code being used in addition to ³
      *    ³ the 14 character stock/inventory item code?	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO R-ITM
	       MOVE "/" 	 TO R-SL
	       MOVE STK-ITM	 TO R-EXT
	   ELSE
010260	       MOVE STK-CODE	 TO R-I2.
010270	     MOVE STK-DESC	 TO R-DESC2.
010280	     MOVE STK-LEVEL	 TO R-LEV.
010290	     MOVE STK-QUANT	 TO R-Q2.
010300	     MOVE STK-COST	 TO R-COST1.
010310	     MOVE STK-AVGE	 TO R-AVGE.
010320	     MOVE STK-MTD	 TO R-NMTD.
010330	     MOVE STK-YTD	 TO R-NYTD.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
010380	       PERFORM BB05.
010390	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BB900.
010400 BB15.
010410     IF WS-AUDKEY < W70-REP
010420	       ADD 1		 TO WS-AUDKEY
010430         GO TO BB10.
010440	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
010460	     MOVE SPACES	 TO R-DETAIL.

       BB900.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.

010470 BB999.
010480       EXIT.
      /
010490 BC00R           SECTION 5.
010500 BC00.
	     PERFORM RESTORE-SCREEN-3.
010520	     DISPLAY "Printing Sales Transactions" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
010540	     MOVE 1		 TO WS-S6.

010550 BC02.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
010560	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
010570	   IF WS-S6 < 300
010580	       ADD 1		 TO WS-S6
010590	       GO TO BC02.
010600	     MOVE ZERO		 TO W200-DTOT W200-CTOT
010610				    W200-COST W200-SELL W200-NET.
010620	     MOVE ZERO		 TO WS-PAGE.
010630	     MOVE 2		 TO WS-AUDKEY.

010640 BC05.
010650       PERFORM AA50 THRU AA55.
010660	     MOVE "DEBTOR TRANSACTIONS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010680	     MOVE SPACES	 TO R-DETAIL.
010690	     MOVE "AC-NO"	 TO R-H14.
010700	     MOVE "DEBTOR NAME"  TO R-H14A.
010710	     MOVE "REF. NO."	 TO R-H15.
010720	     MOVE "INVOICE"	 TO R-H16.
010730	     MOVE "C/NOTE"	 TO R-H17.
	     PERFORM CALL-PRINTUTL.
010750	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

010760 BC10.
010770       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
010810	   IF NOT (AUD-TYPE = 03)
010820         GO TO BC15.
010830       MOVE AUD-AC         TO R-AC W80-ACNO DEB-ACNO.
010850       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
010860	     MOVE DEB-NAME	 TO R-DNME.
010870     IF DEB-TYPE = 1
010880         INSPECT R-DNME REPLACING FIRST "*" BY "."
010890         INSPECT R-DNME REPLACING FIRST "*" BY " "
010900         INSPECT R-DNME REPLACING FIRST "!" BY " ".
010910	     MOVE AUD-REF	 TO R-REF.
010920     IF AUD-VAL < ZERO
010930	       MOVE AUD-VAL	 TO R-CASH
010940	       ADD AUD-VAL	 TO W200-CTOT
010950     ELSE
010960	       ADD AUD-VAL	 TO W200-DTOT
010970	       MOVE AUD-VAL	 TO R-DEBT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011020	       PERFORM BC05.
011030	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BC999.
011040 BC15.
011050     IF WS-AUDKEY < W70-REP
011060	       ADD 1		 TO WS-AUDKEY
011070         GO TO BC10.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011120	       PERFORM BC05.
011130	     MOVE WS-H4 	 TO R-H15.
011140	     MOVE W200-DTOT	 TO R-DEBT.
011150	     MOVE W200-CTOT	 TO R-CASH.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011170	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
011190	     MOVE SPACES	 TO R-DETAIL.

011200 BC999.
011210       EXIT.
      /
011220 BE00R           SECTION 5.
011230 BE00.
	     PERFORM RESTORE-SCREEN-3.
011250	     DISPLAY "Busy printing Cash Draw Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
011270	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3 W25-TOTAL.
      *
      *    ****    SET PRINTER TO 12 CPI
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DETAIL.
011290	     MOVE ZERO		 TO WS-PAGE.
011300	     MOVE 1		 TO WS-NO.
011310 BE05.
011320	   IF W40-DREP(WS-NO) = 1
011330         GO TO BE10.
011340 BE06.
011350     IF WS-NO < 40
011360	       ADD 1		 TO WS-NO
011370         GO TO BE05.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011390	     MOVE "******"	 TO SAL-H7.
011400	     MOVE "TOTAL FOR ALL DRAWS"    TO SAL-H8.
011410	     MOVE W25-COST3	 TO SAL-COST.
011420	     MOVE W25-SELL3	 TO SAL-SELL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011440	     MOVE SPACES	 TO R-DETAIL.
011450	     MOVE "  **  GROSS PROFIT"	   TO SAL-H8.
011460       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
011470	     COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
011490	     MOVE W25-SELL1	 TO SAL-SELL.
011500	     MOVE W25-COST1	 TO SAL-GP.
011470	     COMPUTE W25-COST1 = (W25-SELL1 / W25-COST3) * 100.000 + 0.005.
011500	     MOVE W25-COST1	 TO SAL-MARKUP.
	     PERFORM CALL-PRINTUTL.
011520	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "NETT SALES + TAX"	   TO SAL-H8.
	     MOVE W25-TOTAL	 TO SAL-SELL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
011520	     MOVE SPACES	 TO R-DETAIL.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DETAIL.
011530	     GO TO BE999.

011540 BE10.
	   IF NOT (WS-PRINTER = "S")
011550	       IF WS-PAGE > ZERO
011560		   MOVE SPACES	 TO R-DETAIL.
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL.
011580	     MOVE 2		 TO WS-AUDKEY.

011590 BE15.
011600       PERFORM AA50 THRU AA55.
011610	     MOVE "C A S H   D R A W   R E P O R T"  TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011630	     MOVE SPACES	 TO R-DETAIL.
011640	     MOVE "CASHDRAW:"	 TO SAL-H1.
011650	     MOVE WS-NO 	 TO SAL-NO.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011670	     MOVE SPACES	 TO R-DETAIL.
011680	     MOVE "ITEM CODE"	 TO SAL-H2.
011690	     MOVE "DESCRIPTION"  TO SAL-H3.
011700	     MOVE "COST"	 TO SAL-H4.
011710	     MOVE "SELLING"	 TO SAL-H5.
011720	     MOVE "G.P.%"	 TO SAL-H6.
	     MOVE "MARKUP"	 TO SAL-H6A.
             MOVE "PW"           TO SAL-H6B.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011740	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

011750 BE20.
011760       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
011800	   IF NOT (AUD-TYPE = 4 OR 6)
011810         GO TO BE30.
011820     IF AUD-SUB NOT = WS-NO
011830         GO TO BE30.
011840     IF AUD-TYPE = 4
011850         GO TO BE25.
      *
      *    ****    P R I N T   I N D V I D U A L   I T E M S
      *
011860	     MOVE AUD-EXT-STOCK	 TO STK-CODE.
011870       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"   TO STK-DESC.
011880	     MOVE STK-CODE	 TO SAL-ITEM.
011890	     MOVE STK-DESC	 TO SAL-DESC.
011900	     MOVE AUD-CST	 TO SAL-COST.
011910	     MOVE AUD-SEL	 TO SAL-SELL.
011920	     COMPUTE SAL-GP = ((AUD-SEL - AUD-CST) / AUD-SEL) * 100.000 + 0.005.
011920	     COMPUTE SAL-MARKUP = ((AUD-SEL - AUD-CST) / AUD-CST) * 100.000 + 0.005.
011940	     ADD AUD-CST	 TO W25-COST1 W25-COST2 W25-COST3.
011950	     ADD AUD-SEL	 TO W25-SELL1 W25-SELL2 W25-SELL3.
	   IF AUD-SUPER = 99
	       MOVE "N/C"	 TO SAL-NOCHARGE
	   ELSE
	       MOVE AUD-SUPER	 TO SAL-SUPER.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
012000	       PERFORM BE15.
012010	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BE999.
012020       GO TO BE30.
      *
      *    ****    P R I N T   S A L E   T O T A L
      *
012030 BE25.
012040	     MOVE "REFERENCE NO:"    TO SAL-H7.
012050	     MOVE AUD-REF	 TO SAL-REF.
012060	     MOVE "CASH SALE TOTAL"  TO SAL-H8.
012070	     MOVE W25-COST1	 TO SAL-COST.
012080	     MOVE W25-SELL1	 TO SAL-SELL.
012090	     COMPUTE SAL-GP = ((W25-SELL1 - W25-COST1) / W25-SELL1) * 100.000 + 0.005.
012090	     COMPUTE SAL-MARKUP = ((W25-SELL1 - W25-COST1) / W25-COST1) * 100.000 + 0.005.
	   IF AUD-PAYMENT = "C"
	       MOVE "CSH"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "Q"
	       MOVE "CHQ"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "D"
	       MOVE "CRD"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "S"
	       MOVE "SPD"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "V"
	       MOVE "VCH"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "N"
	       MOVE "CRN"	 TO SAL-PAY.
012110	     MOVE ZERO		 TO W25-SELL1 W25-COST1.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
012160	       PERFORM BE15.
012170	     MOVE SPACES	 TO R-DETAIL.
      *
      *    ****    C H E C K   E N D   O F   A U D I T   F I L E
      *
012180 BE30.
012190     IF WS-AUDKEY < W70-REP
012200	       ADD 1		 TO WS-AUDKEY
012210         GO TO BE20.
029860	     ADD 150 WS-NO	 GIVING WS-PARKEY.
	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     ADD PAR-CSH	 TO W25-TOTAL.
	     ADD PAR-VATVAL	 TO W25-TOTAL.
      *
      *    ****    P R I N T   C A S H   D R A W   T O T A L
      *
012220	   IF W02-LINAGE < (W02-PRN-LENGTH - 6)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
012260	       PERFORM BE15.
012270	     MOVE SPACES	    TO R-DETAIL.
012280	     MOVE "   ***"	    TO SAL-H7.
012290	     MOVE "CASH DRAW TOTAL" TO SAL-H8.
012300	     MOVE W25-COST2	    TO SAL-COST.
012310	     MOVE W25-SELL2	    TO SAL-SELL.
	     MOVE 2		    TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012330	     MOVE SPACES	    TO R-DETAIL.
012340	     MOVE "  GROSS PROFIT"  TO SAL-H8.
012350       COMPUTE W25-SELL1 = W25-SELL2 - W25-COST2.
012360	     COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL2) * 100.000 + 0.005.
012380	     MOVE W25-SELL1	    TO SAL-SELL.
012390	     MOVE W25-COST1	    TO SAL-GP.
012360	     COMPUTE W25-COST1 = (W25-SELL1 / W25-COST2) * 100.000 + 0.005.
012390	     MOVE W25-COST1	    TO SAL-MARKUP.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO SAL-L1
	       MOVE 99		    TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		    TO WS-COMMAND
014990	       GO TO BE999.
012410	     MOVE SPACES	    TO R-DETAIL.
012420	     MOVE ZERO		    TO W25-SELL1 W25-SELL2 W25-COST1 W25-COST2.
012430       GO TO BE06.
012440 BE999.
012450       EXIT.
      /
012460 BF00R           SECTION 5.
012470 BF00.
	     PERFORM RESTORE-SCREEN-3.
012490	     DISPLAY "Busy printing Salesman Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
012510	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3.
012530	     MOVE ZERO		 TO WS-PAGE.
012540	     MOVE 1		 TO WS-NO.

012550 BF05.
012560	   IF W40-SREP(WS-NO) = 1
012570	       GO TO BF10.
012580 BF06.
012590     IF WS-NO < 50
012600	       ADD 1		 TO WS-NO
012610         GO TO BF05.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012630	     MOVE SPACES	 TO R-DETAIL.
012640	     MOVE "  TOTAL - ALL SALESMEN" TO SAL-L5.
012650	     MOVE W25-COST3	 TO SAL-CST.
012660	     MOVE W25-SELL3	 TO SAL-SEL.
012670       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
012680	     COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
012700	     MOVE W25-SELL1	 TO SAL-PRF.
012710	     MOVE W25-COST1	 TO SAL-GPR.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
012730	     MOVE SPACES	 TO R-DETAIL.
012740	     GO TO BF999.
012750 BF10.
	   IF NOT (WS-PRINTER = "S")
012760	       IF WS-PAGE > ZERO
012770		   MOVE SPACES	 TO R-DETAIL
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL.
012790	     MOVE 2		 TO WS-AUDKEY.
012800	     ADD 100 WS-NO	 GIVING WS-PARKEY.
012810       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE ZERO		 TO W25-CASH W25-REFF W25-INV W25-CRN.
012820 BF15.
012830       PERFORM AA50 THRU AA55.
012840	     MOVE "S A L E S M A N   R E P O R T" TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
012870	     MOVE "SALESMAN:"	 TO SAL-H1.
012880	     MOVE WS-NO 	 TO SAL-NO.
012890	     MOVE PAR-SALESMAN	 TO SAL-SMAN.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
012920	     MOVE "REF NO"	 TO SAL-H9.
012930	     MOVE "DESCRIPTION"  TO SAL-H10.
012940	     MOVE " COST"	 TO SAL-H11.
012950	     MOVE " SELLING"	 TO SAL-H12.
012960	     MOVE " PROFIT"	 TO SAL-H13.
012970	     MOVE "G.P.%"	 TO SAL-H14.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

013000 BF20.
013010       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
013050	   IF NOT (AUD-TYPE = 5 OR 7)
013060         GO TO BF30.
013070     IF AUD-SMAN NOT = WS-NO
013080         GO TO BF30.
      *
      *    ****    P R I N T   I N D V I D U A L   S A L E S
      *
013090	     MOVE AUD-CSH	 TO SAL-RF.
013100     IF AUD-TYPE = 5
	       IF AUD-SELL < ZERO
		   MOVE "CASH REFUND"  TO SAL-DSC
		   ADD AUD-SELL  TO W25-REFF
	       ELSE
013110		   MOVE "CASH SALE"    TO SAL-DSC
		   ADD AUD-SELL  TO W25-CASH
	       END-IF
013120     ELSE
013130     IF AUD-SELL < ZERO
013140	       MOVE "CREDIT NOTE"  TO SAL-DSC
	       ADD AUD-SELL	 TO W25-CRN
013150     ELSE
013160	       MOVE "INVOICE"	 TO SAL-DSC
	       ADD AUD-SELL	 TO W25-INV.
013170	     MOVE AUD-COST	 TO SAL-CST.
013180	     MOVE AUD-SELL	 TO SAL-SEL.
013190       COMPUTE SAL-PRF = AUD-SELL - AUD-COST.
013200	     COMPUTE SAL-GPR = ((AUD-SELL - AUD-COST) / AUD-SELL) * 100.000 + 0.005.
013220	     ADD AUD-COST	 TO W25-COST2 W25-COST3.
013230	     ADD AUD-SELL	 TO W25-SELL2 W25-SELL3.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
013280	       PERFORM BF15.
013290	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BF999.
013300 BF30.
013310     IF WS-AUDKEY < W70-REP
013320	       ADD 1		 TO WS-AUDKEY
013330         GO TO BF20.
      *
      *    ****    P R I N T   S A L E S M A N   T O T A L
      *
013340	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
013380	       PERFORM BE15.
013390	     MOVE SPACES	 TO R-DETAIL.
013400	     MOVE "  SALESMAN TOTAL"   TO SAL-L5.
013410	     MOVE W25-COST2	 TO SAL-CST.
013420	     MOVE W25-SELL2	 TO SAL-SEL.
013430       COMPUTE W25-SELL1 = W25-SELL2 - W25-COST2.
013440	     COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL2) * 100.000 + 0.005.
013460	     MOVE W25-SELL1	 TO SAL-PRF.
013470	     MOVE W25-COST1	 TO SAL-GPR.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "SALES"	 TO SAL-H11.
	     MOVE " RETURNS"	 TO SAL-H12.
	     MOVE "   NETT"	 TO SAL-H13.
	     MOVE "RTN %"	 TO SAL-H14.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "CASH SALES"	 TO SAL-DSC.
	     MOVE W25-CASH	 TO SAL-CST.
	     MOVE W25-REFF	 TO SAL-SEL.
	     ADD W25-CASH W25-REFF  GIVING SAL-PRF.
	     COMPUTE SAL-GPR ROUNDED = ((W25-REFF * -1) / W25-CASH) * 100.0000.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "INVOICING"	 TO SAL-DSC.
	     MOVE W25-INV	 TO SAL-CST.
	     MOVE W25-CRN	 TO SAL-SEL.
	     ADD W25-INV W25-CRN GIVING SAL-PRF.
	     COMPUTE SAL-GPR ROUNDED = ((W25-CRN * -1) / W25-INV) * 100.0000.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BF999.
013500	     MOVE ZERO		 TO W25-SELL1 W25-SELL2 W25-COST1 W25-COST2.
013510       GO TO BF06.
013520 BF999.
013530       EXIT.
      /
013540 BG00R           SECTION 5.
013550 BG00.
	     PERFORM RESTORE-SCREEN-3.
013570	     DISPLAY "Busy printing Turnover Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
013590	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3.
013610	     MOVE ZERO		 TO WS-PAGE.
013620	     MOVE 2		 TO WS-AUDKEY.

013630 BG05.
013640       PERFORM AA50 THRU AA55.
013650	     MOVE "T U R N O V E R   R E P O R T"    TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
013670	     MOVE "REF NO"	 TO SAL-H9.
013680	     MOVE "DESCRIPTION"  TO SAL-H10.
013690	     MOVE " COST"	 TO SAL-H11.
013700	     MOVE " SELLING"	 TO SAL-H12.
013710	     MOVE " PROFIT"	 TO SAL-H13.
013720	     MOVE "G.P.%"	 TO SAL-H14.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

013750 BG10.
013760       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
013800	   IF NOT (AUD-TYPE = 5 OR 7)
013810         GO TO BG15.
      *
      *    ****    P R I N T   I N D V I D U A L   S A L E S
      *
013820	     MOVE AUD-CSH	 TO SAL-RF.
013830     IF AUD-TYPE = 5
013840	       MOVE "CASH SALE"  TO SAL-DSC
013850     ELSE
013860     IF AUD-SELL < ZERO
013870	       MOVE "CREDIT NOTE"  TO SAL-DSC
013880     ELSE
013890	       MOVE "INVOICE"	 TO SAL-DSC.
013900	     MOVE AUD-COST	 TO SAL-CST.
013910	     MOVE AUD-SELL	 TO SAL-SEL.
013920       COMPUTE SAL-PRF = AUD-SELL - AUD-COST.
013930	     COMPUTE SAL-GPR = ((AUD-SELL - AUD-COST) / AUD-SELL) * 100.000 + 0.005.
013950	     ADD AUD-COST	 TO W25-COST2 W25-COST3.
013960	     ADD AUD-SELL	 TO W25-SELL2 W25-SELL3.
013970	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014010	       PERFORM BG05.
014020	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BG999.
014030 BG15.
014040     IF WS-AUDKEY < W70-REP
014050	       ADD 1		 TO WS-AUDKEY
014060         GO TO BG10.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
014080	     MOVE "  TOTAL TURNOVER AND GP"
				 TO SAL-L5.
014090	     MOVE W25-COST3	 TO SAL-CST.
014100	     MOVE W25-SELL3	 TO SAL-SEL.
014110       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
014120	     COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
014140	     MOVE W25-SELL1	 TO SAL-PRF.
014150	     MOVE W25-COST1	 TO SAL-GPR.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
014170	     MOVE SPACES	 TO R-DETAIL.

014180 BG999.
014190       EXIT.
      /
014200 BH00R           SECTION 5.
014210 BH00.
	     PERFORM RESTORE-SCREEN-3.
014230	     DISPLAY "Busy printing detailed Turnover Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
014250	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3.
014270	     MOVE ZERO		 TO WS-PAGE.
014280	     MOVE 2		 TO WS-AUDKEY.

014290 BH15.
014300       PERFORM AA50 THRU AA55.
014310	     MOVE "T U R N O V E R   R E P O R T"    TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014330	     MOVE SPACES	 TO R-DETAIL.
014340	     MOVE "ITEM CODE"	 TO SAL-H2.
014350	     MOVE "DESCRIPTION"  TO SAL-H3.
014360	     MOVE "COST"	 TO SAL-H4.
014370	     MOVE "SELLING"	 TO SAL-H5.
014380	     MOVE "G.P.%"	 TO SAL-H6.
	     PERFORM CALL-PRINTUTL.
014400	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

014410 BH20.
014420       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
014460	   IF NOT (AUD-TYPE = 3 OR 4 OR 6 OR 8)
014470         GO TO BH30.
014480     IF AUD-TYPE = 3 OR 4
014490         GO TO BH25.
      *
      *    ****    P R I N T   I N D V I D U A L   I T E M S
      *
014500	     MOVE AUD-EXT-STOCK	 TO STK-CODE.
014510       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"   TO STK-DESC.
014520	     MOVE STK-CODE	 TO SAL-ITEM.
014530	     MOVE STK-DESC	 TO SAL-DESC.
014540	     MOVE AUD-CST	 TO SAL-COST.
014550	     MOVE AUD-SEL	 TO SAL-SELL.
014560	     COMPUTE SAL-GP = ((AUD-SEL - AUD-CST) / AUD-SEL) * 100.000 + 0.005.
014580	     ADD AUD-CST	 TO W25-COST1 W25-COST2 W25-COST3.
014590	     ADD AUD-SEL	 TO W25-SELL1 W25-SELL2 W25-SELL3.
	   IF AUD-SUPER = 99
	       MOVE "N/C"	 TO SAL-NOCHARGE
	   ELSE
	       MOVE AUD-SUPER	 TO SAL-SUPER.
014600	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014640	       PERFORM BH15.
014650	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BH999.
014660       GO TO BH30.
      *
      *    ****    P R I N T   S A L E   T O T A L
      *
014670 BH25.
014700	   IF AUD-TYPE = 4
	       IF W25-SELL1 < ZERO
		   MOVE "CASH REFUND TOTAL"  TO SAL-H8
	       ELSE
014710		   MOVE "CASH SALE TOTAL"    TO SAL-H8
	       END-IF
014720     ELSE
010830	       MOVE AUD-AC	 TO R-AC W80-ACNO DEB-ACNO
010850	       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
010860	       MOVE DEB-NAME	 TO R-DNME
010870	       IF DEB-TYPE = 1
010880		   INSPECT R-DNME REPLACING FIRST "*" BY "."
010890		   INSPECT R-DNME REPLACING FIRST "*" BY " "
010900		   INSPECT R-DNME REPLACING FIRST "!" BY " "
	       END-IF
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014840	       MOVE SPACES	 TO R-DETAIL
	       IF W25-SELL1 < ZERO
		   MOVE "CREDIT NOTE TOTAL"    TO SAL-H8
	       ELSE
014730		   MOVE "INVOICE TOTAL"        TO SAL-H8.
014680	     MOVE "REFERENCE NO:"	       TO SAL-H7.
014690	     MOVE AUD-REF		       TO SAL-REF.
014740	     MOVE W25-COST1		       TO SAL-COST.
014750	     MOVE W25-SELL1		       TO SAL-SELL.
014760	     COMPUTE SAL-GP = ((W25-SELL1 - W25-COST1) / W25-SELL1) * 100.000 + 0.005.
014780	     MOVE ZERO			       TO W25-SELL1 W25-COST1.
	     MOVE ZERO			       TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014790	   IF W02-LINAGE < (W02-PRN-LENGTH - 3)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014830	       PERFORM BH15.
014840	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BH999.
      *
      *    ****    C H E C K   E N D   O F   A U D I T   F I L E
      *
014850 BH30.
014860     IF WS-AUDKEY < W70-REP
014870	       ADD 1		 TO WS-AUDKEY
014880         GO TO BH20.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
014900	     MOVE "******"	 TO SAL-H7.
014910	     MOVE "TOTAL TURNOVER"
				 TO SAL-H8.
014920	     MOVE W25-COST3	 TO SAL-COST.
014930	     MOVE W25-SELL3	 TO SAL-SELL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014950	     MOVE SPACES	 TO R-DETAIL.
014960	     MOVE "** GROSS PROFIT"
				 TO SAL-H8.
014970       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
014980	     COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
015000	     MOVE W25-SELL1	 TO SAL-SELL.
015010	     MOVE W25-COST1	 TO SAL-GP.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
015030	     MOVE SPACES	 TO R-DETAIL.

015050 BH999.
015060       EXIT.
      /
010490 BI00R           SECTION 5.
010500 BI00.
	     PERFORM RESTORE-SCREEN-3.
010520	     DISPLAY "Printing Debtor Receipts" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
010540	     MOVE 1		 TO WS-S6.

010550 BI02.
010560	     MOVE ZERO		 TO W200-CR(WS-S6).
010570	   IF WS-S6 < 300
010580	       ADD 1		 TO WS-S6
010590	       GO TO BI02.
010600	     MOVE ZERO		 TO W200-DTOT W200-CTOT W200-COST W200-SELL W200-NET.
010620	     MOVE ZERO		 TO WS-PAGE.
010630	     MOVE 2		 TO WS-AUDKEY.
010640 BI05.
010650       PERFORM AA50 THRU AA55.
010660	     MOVE "DEBTOR RECEIPTS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010680	     MOVE SPACES	 TO R-DETAIL.
010690	     MOVE "AC-NO"	 TO R-H14.
010700	     MOVE "DEBTOR NAME"  TO R-H14A.
010710	     MOVE "REF. NO."	 TO R-H15.
010720	     MOVE "  VALUE"	 TO R-H16.
	     PERFORM CALL-PRINTUTL.
010750	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

010760 BI10.
010770       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
010810	   IF NOT (AUD-TYPE = 11)
010820	       GO TO BI15.
010830       MOVE AUD-AC         TO R-AC W80-ACNO DEB-ACNO.
010850       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
010860	     MOVE DEB-NAME	 TO R-DNME.
010870     IF DEB-TYPE = 1
010880         INSPECT R-DNME REPLACING FIRST "*" BY "."
010890         INSPECT R-DNME REPLACING FIRST "*" BY " "
010900         INSPECT R-DNME REPLACING FIRST "!" BY " ".
010910	     MOVE AUD-REF	 TO R-REF.
010930	     MOVE AUD-VAL	 TO R-DEBT.
010940	     ADD AUD-VAL	 TO W200-CTOT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011020	       PERFORM BI05.
011030	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BI999.

011040 BI15.
011050     IF WS-AUDKEY < W70-REP
011060	       ADD 1		 TO WS-AUDKEY
011070	       GO TO BI10.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011120	       PERFORM BI05.
011130	     MOVE WS-H4 	 TO R-H15.
011140	     MOVE W200-CTOT	 TO R-DEBT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011170	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
011190	     MOVE SPACES	 TO R-DETAIL.

011200 BI999.
011210       EXIT.
      /
010490 BJ00R           SECTION 5.
010500 BJ00.
	     PERFORM RESTORE-SCREEN-3.
010520	     DISPLAY "Printing Discounted Sales" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       BJ999.
	     EXIT.
      /
015070 BX000           SECTION 5.
015080 BX00.
015090	     DISPLAY "Analyzing audit file " AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
015110     IF W70-REP < 2
015120	       MOVE ZERO	 TO WS-REP1 WS-REP2 WS-REP3
				    WS-REP4 WS-REP5 WS-REP6
				    WS-REP7 WS-REP8 WS-REP9 WS-REP10
015140         GO TO BX999.
015150	     MOVE 2		 TO WS-AUDKEY.

015160 BX05.
015170       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
015210	   IF AUD-TYPE = 5
015220         IF AUD-SUB > 0 AND < 41
015230		   MOVE 1	 TO W40-DREP(AUD-SUB).
015250     IF AUD-TYPE = 5 OR 7
015260         IF AUD-SMAN > 0 AND < 51
015270		   MOVE 1	 TO W40-SREP(AUD-SMAN).

015280 BX10.
015290     IF WS-AUDKEY < W70-REP
015300	       ADD 1		 TO WS-AUDKEY
015310         GO TO BX05.

015320 BX999.
015330       EXIT.
      /
015340 BZ00R           SECTION 5.
015350 BZ00.
	     PERFORM CLEAR-L50.
015360	     MOVE ZERO		 TO WS-PAGE.
015370     IF NOT (WS-STATOP = "1" OR "8")
015380         PERFORM BZ05
015390         GO TO BZ15.
015400	   IF LS0-DTP NOT = 2
015410	       MOVE "Not Installed"    TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
015420         IF WS-STATOP = "1"
015430             GO TO BZ999
015440         ELSE
015450             PERFORM BZ05
015460             GO TO BZ15.
015470	     DISPLAY "Busy printing Debtors Statistics      " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
015490	     MOVE 1		       TO WS-NETKEY.
015500       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.

015510 BZ05.
015520       PERFORM AA50 THRU AA55.
015530	     MOVE "STATISTICS REPORT"  TO R-HEAD.
	     MOVE 2		       TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015550	     MOVE SPACES	       TO R-DETAIL.

015560 BZ10.
015570	     MOVE "DEBTORS"	 TO R-H26.
	     PERFORM CALL-PRINTUTL.
015590	     MOVE SPACES	 TO R-DETAIL.
015600	     MOVE "TODAY"	 TO R-H28.
015610	     MOVE WS-H13	 TO R-H31.
015620	     MOVE WS-H14	 TO R-H34.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015640	     MOVE SPACES	 TO R-DETAIL.
015650       MOVE "TRANSACTIONS" TO R-H26.
015660	     MOVE WS-H12	 TO R-H27 R-H30 R-H33.
015670	     MOVE WS-H11	 TO R-H29 R-H32 R-H35.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015690	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       BZ10A.
             MOVE 11             TO WS-PARKEY.

       BZ11.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE 1		 TO WS-S1.

       BZ12.
	   IF PAR-E-DESC(WS-S1) = SPACES
               GO TO BZ14.
015700	     MOVE PAR-E-DESC(WS-S1)    TO R-H36.
015710	     MOVE PAR-T-DAY(WS-S1)     TO R-N1.
015720	     MOVE PAR-T-VAL(WS-S1)     TO R-V5.
015730	     MOVE PAR-T-MTD(WS-S1)     TO R-N2.
015740	     MOVE PAR-T-MTDV(WS-S1)    TO R-V6.
015750	     MOVE PAR-T-YTD(WS-S1)     TO R-N3.
015760	     MOVE PAR-T-YTDV(WS-S1)    TO R-V7.

       BZ13.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BZ05 THRU BZ10.
	     MOVE SPACES	 TO R-DETAIL.

       BZ14.
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO BZ12.
           IF WS-PARKEY < 60
               ADD 1             TO WS-PARKEY
               GO TO BZ11.
             PERFORM BZ13.
             MOVE 9              TO WS-PARKEY.
016350       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      * 	P R I N T   A L L   V A T   R A T E S
      *
      *    **** INSERT A LOOP TO PRINT EACH VAT RATE
      *
	     MOVE "VAT" 	 TO DPT-SCDE.
	     MOVE 1		 TO DPT-V-RTE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE "VAT SALES"    TO R-H36.
	     MOVE PAR-DVAT-DAY	 TO R-V5.
             MOVE PAR-DVAT-MTD   TO R-V6.
             MOVE PAR-DVAT-YTD   TO R-V7.
             PERFORM BZ13.
             MOVE "VAT"          TO R-H36.
	     MOVE DPT-O-VAT-DAY  TO R-V5.
016380	     MOVE DPT-O-VAT-MTD  TO R-V6.
016390	     MOVE DPT-O-VAT-YTD  TO R-V7.
             PERFORM BZ13.
016410       MOVE "NON VAT SALES"
                                 TO R-H36.
016420       MOVE PAR-DNGS-DAY   TO R-V5.
016430       MOVE PAR-DNGS-MTD   TO R-V6.
016440       MOVE PAR-DNGS-YTD   TO R-V7.
             PERFORM BZ13.
016460       MOVE "VAT RETURNS"  TO R-H36.
016470	     MULTIPLY -1 BY PAR-CVAT-DAY GIVING R-V5.
016480	     MULTIPLY -1 BY PAR-CVAT-MTD GIVING R-V6.
016490	     MULTIPLY -1 BY PAR-CVAT-YTD GIVING R-V7.
	     PERFORM BZ13.
016520       MOVE "VAT REVERSED" TO R-H36.
016530	     MOVE DPT-O-VRET-DAY TO R-V5.
016530	     MOVE DPT-O-VRET-MTD TO R-V6.
016540	     MOVE DPT-O-VRET-YTD TO R-V7.
             PERFORM BZ13.
	     MOVE "NET VAT - DAY"  TO R-H36.
	     COMPUTE R-V5 = DPT-O-VAT-DAY + DPT-O-VRET-DAY.
             PERFORM BZ13.
016560	     MOVE "NON VAT RETURNS"    TO R-H36.
016570	     MULTIPLY -1 BY PAR-CNGS-DAY GIVING R-V5.
016580	     MULTIPLY -1 BY PAR-CNGS-MTD GIVING R-V6.
016590	     MULTIPLY -1 BY PAR-CNGS-YTD GIVING R-V7.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 3		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BZ05 THRU BZ10.
             MOVE SPACES         TO R-DETAIL.
016620	     MOVE "ACCOUNT RECORDS"    TO R-H26.
             PERFORM BZ13.
016650	     MOVE "DEBTOR ACCOUNTS"    TO R-H36.
016660       MOVE DEB-ACCOUNTS   TO R-N2.
             PERFORM BZ13 3 TIMES.
016690       MOVE "THIS MONTH"   TO R-H26.
             PERFORM BZ13.
016720	     MOVE "OPENING BALANCE"  TO R-H36.
016730       MOVE DEB-OPEN       TO R-V6.
             PERFORM BZ13.
016750       MOVE "DEBITS"       TO R-H36.
016760       MOVE DEB-DT         TO R-V6.
             PERFORM BZ13.
016780       MOVE "CREDITS"      TO R-H36.
016790	     MULTIPLY -1 BY DEB-CR GIVING R-V6.
	     PERFORM BZ13.
016810	     MOVE "CLOSING BALANCE"  TO R-H36.
016820       MOVE DEB-OUT        TO R-V6.
             PERFORM BZ13 3 TIMES.
016840       MOVE SPACES         TO R-DETAIL.
016850       MOVE "AGE ANALYSIS" TO R-H26.
             PERFORM BZ13.
016880       MOVE DEB-ONE        TO W25-TOTAL.
016890       ADD DEB-TWO         TO W25-TOTAL.
016900       ADD DEB-THREE       TO W25-TOTAL.
016910       ADD DEB-INTEREST    TO W25-TOTAL.
016920	     SUBTRACT W25-TOTAL FROM DEB-OUT GIVING R-V6.
016930       MOVE "CURRENT"      TO R-H36.
             PERFORM BZ13.
016950       MOVE "30 DAYS"      TO R-H36.
016960       MOVE DEB-ONE        TO R-V6.
             PERFORM BZ13.
016980       MOVE "60 DAYS"      TO R-H36.
016990       MOVE DEB-TWO        TO R-V6.
             PERFORM BZ13.
017010       MOVE "90 DAYS"      TO R-H36.
017020       MOVE DEB-THREE      TO R-V6.
             PERFORM BZ13.
017040       MOVE "INTEREST"     TO R-H36.
017050       MOVE DEB-INTEREST   TO R-V6.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017070	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BZ999.
017080	   IF LS0-STP NOT = 2
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
017160	     DISPLAY "Printed" AT 0935 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

017170 BZ15.
017180	   IF NOT (WS-STATOP = "1" OR "3" OR "4" OR "8")
017190         GO TO BZ20.
017200	   IF LS0-STP NOT = 2
017210	       MOVE "Not Installed"    TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
017220         IF WS-STATOP = "8"
017230             GO TO BZ20
017240         ELSE
017250             GO TO BZ999.
017260	     DISPLAY "Busy printing Stock Statistics       " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
017280	     MOVE 3		 TO WS-NETKEY.
017290       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
017300	     MOVE "STOCK"	 TO R-H26.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017320	   IF (WS-STATOP = "4") OR (LS0-DTP NOT = 2)
017330         MOVE SPACES       TO R-DETAIL
017340         MOVE "TODAY"      TO R-H28
017350         MOVE WS-H13       TO R-H31
017360	       MOVE WS-H14	 TO R-H34
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
017380	     MOVE SPACES	 TO R-DETAIL.
017390       MOVE "PURCHASES"    TO R-H36.
017400       MOVE STK-CMTD       TO R-V6.
017410       MOVE STK-CYTD       TO R-V7.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017430	     MOVE "ADJUSTMENTS"  TO R-H36.
017440       MOVE STK-ADJMTD     TO R-V6.
017450       MOVE STK-ADJYTD     TO R-V7.
	     PERFORM CALL-PRINTUTL.
017430	     MOVE "SALES"	 TO R-H36.
017440       MOVE STK-SMTD       TO R-V6.
017450       MOVE STK-SYTD       TO R-V7.
	     PERFORM CALL-PRINTUTL.
017470	     MOVE SPACES	 TO R-DETAIL.
017480	     MOVE "STOCK ON HAND"
				 TO R-H36.
017490	     MOVE STK-TOTAL	 TO R-V5.
	     PERFORM CALL-PRINTUTL.
017510	     MOVE SPACES	 TO R-DETAIL.
017520	     MOVE "COST OF SALES"
				 TO R-H26.
017530	     MOVE 8		 TO WS-PARKEY.
017540       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017550	     MOVE PAR-CSHT	 TO R-V5.
017560	     MOVE PAR-CMTD	 TO R-V6.
017570	     MOVE PAR-CYTD	 TO R-V7.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
017620	     MOVE SPACES	 TO R-DETAIL.
017630	     DISPLAY "Printed" AT 1235 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BZ999.
017640 BZ20.
017650     IF NOT (WS-STATOP = "2" OR "8")
017660         GO TO BZ999.
017670     IF WS-STATOP = "8"
017680	       IF LS0-DTP = 2 OR LS0-STP = 2
017690		   IF LS0-CRP = 2
017700                 PERFORM BZ05
017710             ELSE
017720                 GO TO BZ999.
017730	   IF LS0-CRP NOT = 2
017740	       MOVE "Not Installed"    TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
017750         GO TO BZ999.
017760	     DISPLAY "Busy printing Creditors Statistics   " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE 2              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
017800       MOVE 6              TO WS-PARKEY.
017810       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BZ999.
017820	     MOVE "CREDITORS"	 TO R-H26.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017840	     MOVE SPACES	 TO R-DETAIL.
017850	     MOVE "TODAY"	 TO R-H28.
017860	     MOVE WS-H13	 TO R-H31.
017870	     MOVE WS-H14	 TO R-H34.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017890	     MOVE SPACES	 TO R-DETAIL.
017900       MOVE "TRANSACTIONS" TO R-H26.
017910	     MOVE WS-H12	 TO R-H27 R-H30 R-H33.
017920	     MOVE WS-H11	 TO R-H29 R-H32 R-H35.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017940	     MOVE SPACES	 TO R-DETAIL.

       BZ20A.
             MOVE 191            TO WS-PARKEY.

       BZ21.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE 1		 TO WS-S1.
       BZ22.
	   IF PAR-C-DESC(WS-S1) = SPACES
               GO TO BZ24.
015700	     MOVE PAR-C-DESC(WS-S1)  TO R-H36.
015710	     MOVE PAR-C-DAY(WS-S1)   TO R-N1.
015720	     MOVE PAR-C-VAL(WS-S1)   TO R-V5.
015730	     MOVE PAR-C-MTD(WS-S1)   TO R-N2.
015740	     MOVE PAR-C-MTDV(WS-S1)  TO R-V6.
015750	     MOVE PAR-C-YTD(WS-S1)   TO R-N3.
015760	     MOVE PAR-C-YTDV(WS-S1)  TO R-V7.
             PERFORM BZ13.
       BZ24.
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO BZ22.
           IF WS-PARKEY < 200
               ADD 1             TO WS-PARKEY
               GO TO BZ21.
             MOVE 6              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM BZ13.
018440       MOVE "RAILAGE"      TO R-H36.
018450       MOVE PAR-RAILD      TO R-V5.
018460       MOVE PAR-RAILM      TO R-V6.
             PERFORM BZ13.
018490       MOVE "DISCOUNT"     TO R-H36.
018500       MOVE PAR-DISCD      TO R-V5.
018510       MOVE PAR-DISCM      TO R-V6.
             PERFORM BZ13 3 TIMES.
018540       MOVE "THIS MONTH"   TO R-H26.
             PERFORM BZ13.
018570       MOVE "OPENING BALANCE"
                                 TO R-H36.
018580       MOVE P-OPEN         TO R-V6.
             PERFORM BZ13.
018600       MOVE "DEBITS"       TO R-H36.
018610       MOVE P-DEBT         TO R-V6.
             PERFORM BZ13.
018630       MOVE "CREDITS"      TO R-H36.
018640       MOVE P-CRED         TO R-V6.
             PERFORM BZ13.
018660       MOVE "CLOSING BALANCE"
                                 TO R-H36.
018670       MOVE P-CLOSE        TO R-V6.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE SPACES	 TO R-DETAIL.
018760	     DISPLAY "Printed" AT 1035 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

020270 BZ999.
020280       EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
029020 ZA00-INIT       SECTION 8.
       ZA00.
041220       MOVE 1                TO WS-S1.
041240 ZA02.
           IF LS-USER = LS-SYS-ID
               MOVE "SupervisorÄ"  TO WS-WRKHD
           ELSE
               MOVE "Workstation"  TO WS-WRKHD
             MOVE LS-USER          TO WS-WRKST.
029030 ZA00-OPEN.
             MOVE LS-PARID         TO WS-PARID.
             MOVE LS-L-OR-N        TO W02-L-OR-N.
             MOVE WS-SYS-ID        TO W02-SYSID.
             MOVE LS-TODAY-DDMMYY  TO TODAY-DDMMYY.
             MOVE LS-USUB          TO WS-USUB.
             MOVE "S"              TO CRT-TYPE.
007750       CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      S e t   u p   t h e   F U N C T I O N   k e y s      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 1                TO USER-ACTION USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                     ESC, F1 to F10 keys                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE ZERO             TO USER-NUMBER.
             MOVE 11               TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               PAGE-UP and PAGE-DOWN keys                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 53               TO USER-NUMBER.
             MOVE 2                TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  A C T I V A T E   M O U S E              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 64               TO MOUSE-FUNC.
             MOVE 1                TO MOUSE-PARAM.
             CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
           IF MOUSE-FUNC NOT = 255
               MOVE "Y"	           TO MOUSE-ATTACHED
               MOVE 66             TO MOUSE-FUNC
               MOVE 0              TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM
           ELSE
               GO TO ZA00A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          Set MOUSE key to act as FUNCTION key             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 3                TO USER-ACTION.
             MOVE 27               TO USER-NUMBER.
             MOVE 2                TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         Setup the file ID's including path names          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030       READ APACFIDS WITH IGNORE LOCK KEY IS AFID-KEY.
           IF WS-STATUS = "00"
               GO TO ZA00-READ-APACFIDS-EXIT.
006260       STRING "Missing " DELIMITED SIZE AFID-KEY DELIMITED BY " " " file ID - Status " DELIMITED SIZE WS-STATUS DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
006370       STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
             EXIT.

       ZA00A-CONTINUE.
             MOVE AFID-PATH        TO W02-PARAM.
      *    *************************************************************
      *    ****        D A I L Y   -   A U D I T   F I L E          ****
      *    *************************************************************
             MOVE "AUDITF"         TO AFID-KEY.
             PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
             MOVE AFID-PATH        TO W02-AUDITF.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
             MOVE "DEBTOR"         TO AFID-KEY.
             PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
             MOVE AFID-PATH        TO W02-DEBTOR.
      *    *************************************************************
      *    ****             D E P A R T M E N T   F I L E           ****
      *    *************************************************************
             MOVE "DEPART"         TO AFID-KEY.
             PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
             MOVE AFID-PATH        TO W02-DEPART.
      *    *************************************************************
      *    ****                C O N T R O L   F I L E              ****
      *    *************************************************************
             MOVE "NETWORK"        TO AFID-KEY.
             PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
             MOVE AFID-PATH        TO W02-NETWORK.
      *    *************************************************************
      *    ****                  S T O C K   F I L E	              ****
      *    *************************************************************
             MOVE "STOCKF"         TO AFID-KEY.
             PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
             MOVE AFID-PATH        TO W02-STOCKF.
029620 ZA10.
029890       MOVE 1                TO WS-PARKEY.
029900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
029910	     MOVE PAR-DMY	     TO W12-TODAY.
029920	     MOVE PAR-YMD	     TO W12-T-YMD.
029930	     MOVE PAR-COMPANY  TO CRT-COMP.
030020     IF LS0-DTP NOT = 2
030030         GO TO ZA999.
030040     IF LS0-STP NOT = 2
030050         GO TO ZA999.
030060       MOVE 1                TO WS-AUDKEY.
030070       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
030080       MOVE AUD-REP1         TO WS-REP1.
030090       MOVE AUD-REP2         TO WS-REP2.
030100       MOVE AUD-REP3         TO WS-REP3.
030110       MOVE AUD-REP4         TO WS-REP4.
030120       MOVE AUD-REP5         TO WS-REP5.
030130       MOVE AUD-REP6         TO WS-REP6.
030140       MOVE AUD-REP7         TO WS-REP7.
030150       MOVE AUD-REP8         TO WS-REP8.
030160       MOVE AUD-REP9         TO WS-REP9.
030170       MOVE AUD-REP10        TO WS-REP10.
030170       MOVE AUD-REP11        TO WS-REP11.
030190       GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN"                  AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue"            AT 1212.
             ACCEPT WS-OPTION                               AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             GO TO ZA205.
       ZA50.
041850     IF WS-F-ERROR = 3
041860         MOVE "CREDITOR file1" TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 6
041860         MOVE "DEBTOR file"    TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE "STOCK FILE"     TO WS-FILE.
             DISPLAY "Rebuild "                  AT 0812 WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION                    AT 1038 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
       ZA51.
             EXIT PROGRAM.
030200 ZA200.
	     DISPLAY "This program can only be run when no other workstations are logged into the" AT 0803 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
	     DISPLAY "accounting system. All other workstations should return to the program con-" AT 0903 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
	     DISPLAY "trol menu or exit from the accounting system before you may continue       " AT 1003 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
030210     DISPLAY "Files locked - Try later"                                                    AT 2312 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                   " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
030230     DISPLAY "Press any key" AT 5012 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
030250     ACCEPT WS-OPTION AT 5026 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

       ZA205.
             EXIT PROGRAM.
030280 ZA999.
030290       EXIT.
      /
030740 ZC00-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

041560 DISPLAY-FILE-NAME.
041570     IF WS-F-ERROR = 1
041580         MOVE W02-AUDITF   TO WS-FILE
041590         MOVE WS-AUDKEY    TO WS-KEY
041600     ELSE
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041600     ELSE
041610     IF WS-F-ERROR = 3
041620         MOVE W02-CREDIT   TO WS-FILE
041630         MOVE ZERO         TO WS-KEY
041760     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041800     ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830         MOVE DPT-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY.

       COPY "DISPERR.PRO".
