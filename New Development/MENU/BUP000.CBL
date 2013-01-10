      $set LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   **    **  *******     ****      ****      ****     *
      *   **    **  **    **  **    **   **  **    **  **    **  **    *
      *   **    **  **    **  **    **  **    **  **    **  **    **   *
      *   *******   **    **  *******   **    **  **    **  **    **   *
      *   **    **  **    **  **        **    **  **    **  **    **   *
      *   **    **  **    **  **         **  **    **  **    **  **    *
      *   *******    ******   **          ****      ****      ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   S T A R T   &   I N I T    P R O G R A M     *
      *                                                                *
      *     VERSION 9.00.00 - November 2012                            *
      * 							                       *
      ******************************************************************
      * 							                       *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales       *
      * 		  restrictions file has been included which          *
      * 		  will allow for departments to de excluded or       *
      * 		  only selected departments allowed. This will       *
      * 		  allow the user the abillity to allow or bar        *
      * 		  sales of selected departments.	                 *
      * 							                       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index         *
      * 		  included for lookups, using any word con-          *
      * 		  tained in the Debtor name.		                 *
      * 							                       *
      *  Aug 2008	- New file (ADJREF) - Stock adjustments file         *
      * 		  included for lookups of references used for        *
      * 		  receiving stock via adjustment program.            *
      * 							                       *
      * 		- New file (STKALT) - Stock alternate index          *
      * 		  included for lookups, using any word con-          *
      * 		  tained in the Stock description.	                 *
      * 							                       *
      *  March 2010	- Include facility to print to a USB printer   *
      * 		  via the Windows print using the relavent           *
      * 		  print API's supplied with MF COBOL. Allow          *
      * 		  BMP files in APACFILE (INVBMP & STMBMP).           *
      * 							                       *
      *  May 2010	- Include facility to print sales by depart-         *
      * 		  ment for Debtors accounts. New work files          *
      * 		  "DEALER" & "DEATOT" included for extracting        *
      * 		  the data from the Cardex file.	                 *
      * 							                       *
      ******************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                VERSIONS OF THE APAC SYSTEM                Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё While developing this system the requirements and budgets Ё
      *    Ё (originally this was a propriety package) for various     Ё
      *    Ё businesses necessitated that at least 4 separate systems  Ё
      *    Ё be produced. As the maintenance and amendments to the 4   Ё
      *    Ё different systems would require man hours that would not  Ё
      *    Ё enable me to do much development, it was decided to use   Ё
      *    Ё one system with the various requirements controlled by    Ё
      *    Ё the various menu programs using a version indicator. This Ё
      *    Ё indicator is located in the second record of the SYSTEM   Ё
      *    Ё file SEC-LEV, which is in the SEC-MODULES. This indicator Ё
      *    Ё together with all the other indicators in SEC-MODULES, is Ё
      *    Ё moved to WS0-MODULES in BUP000 or BUP001 and then passed  Ё
      *    Ё via the LINKAGE SECTION from program to program in  the   Ё
      *    Ё LS0-MODULES group. The other indicators in this group are Ё
      *    Ё used to show which modules of the system are installed.   Ё
      *    фмммммммммммммммммммямммммммммммммммммммяммммммммммммммммммм╣
      *    Ё    SYSTEM FILE    Ё      CHAIN.WS	   Ё     CHAIN.LS      Ё
      *    цдддддддддддддддддддедддддддддддддддддддеддддддддддддддддддд╢
      *    Ё03	SEC-MODULES. Ё03  WS0-MODULES.   Ё03 LS0-MODULES.    Ё
      *    Ё  05  SEC-DTP 9(01)Ё  05  WS0-DTP 9(01)Ё  05  LS0-DTP 9(01)Ё
      *    Ё  05  SEC-STP 9(01)Ё  05  WS0-STP 9(01)Ё  05  LS0-STP 9(01)Ё
      *    Ё  05  SEC-HPD 9(01)Ё  05  WS0-HPD 9(01)Ё  05  LS0-HPD 9(01)Ё
      *    Ё  05  SEC-CRP 9(01)Ё  05  WS0-CRP 9(01)Ё  05  LS0-CRP 9(01)Ё
      *    Ё  05  SEC-GLP 9(01)Ё  05  WS0-GLP 9(01)Ё  05  LS0-GLP 9(01)Ё
      *    Ё  05  SEC-POS 9(01)Ё  05  WS0-POS 9(01)Ё  05  LS0-POS 9(01)Ё
      *    Ё  05  SEC-JCP 9(01)Ё  05  WS0-JCP 9(01)Ё  05  LS0-JCP 9(01)Ё
      *    Ё  05  SEC-VHP 9(01)Ё  05  WS0-VHP 9(01)Ё  05  LS0-VHP 9(01)Ё
      *    Ё  05  SEC-WAG 9(01)Ё  05  WS0-WAG 9(01)Ё  05  LS0-WAG 9(01)Ё
      *    Ё  05  SEC-SAL 9(01)Ё  05  WS0-SAL 9(01)Ё  05  LS0-SAL 9(01)Ё
      *    Ё  05  SEC-LEV 9(01)Ё  05  WS0-LEV 9(01)Ё  05  LS0-LEV 9(01)Ё
      *    Ё  05  SEC-WHS 9(01)Ё  05  WS0-WHS 9(01)Ё  05  LS0-WHS 9(01)Ё
      *    Ё  05  SEC-RTE 9(01)Ё  05  WS0-RTE 9(01)Ё  05  LS0-RTE 9(01)Ё
      *    Ё  05  FILLER  X(07)Ё  05  FILLER  X(07)Ё  05  FILLER  X(07)Ё
      *    юдддддддддддддддддддадддддддддддддддддддаддддддддддддддддддды
      *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     BUP000.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

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

       COPY "APACFIDS.SL".

       COPY "APACFILE.SL".

       COPY "APCCOM.SL".

000140 COPY "PARAM.SL".

000140 COPY "PRNTCONT.SL".

000120 COPY "SYSTEM.SL".

000160     SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY "APACFIDS.FDE".

       COPY "APACFILE.FD".

       COPY "APCCOM.FD".

000220 COPY "PARAM.IFD".

000140 COPY "PRNTCONT.FD".

000200 COPY "SYSTEM.FD".

      /
000240 FD  PRNREP      LABEL RECORD OMITTED
000250                 LINAGE WS-PGE-LENGTH.
000260 01  REP-L1.
000270     03  REP-DETAIL            PIC  X(132).
000280 01  REP-L2.
000290     03  FILLER                PIC  X(28).
000300     03  REP-HEAD              PIC  X(24).
000310     03  FILLER                PIC  X(28).
000320 01  REP-L3.
000330     03  REP-GRPH              OCCURS 4.
000340         05  FILLER            PIC  X(01).
000350         05  REP-H1            PIC  X(06).
000360         05  REP-H2            PIC  X(06).
000370         05  REP-H3            PIC  X(06).
000380         05  REP-DIV           PIC  X(01).
000390 01  REP-L4.
000400     03  REP-ASCII             OCCURS 4.
000410         05  FILLER            PIC  X(01).
000420         05  REP-DEC           PIC  Z99.
000430         05  FILLER            PIC  X(03).
000440         05  REP-HEX           PIC  X(02).
000450         05  FILLER            PIC  X(04).
000460         05  REP-GRAPH.
000470             07  FILLER        PIC  X(02).
000480             07  REP-ALPH      PIC  X(01).
000490             07  FILLER        PIC  X(04).
000500 01  SYS-L1.
000510     03  FILLER                PIC  X(05).
000520     03  SYS-H1                PIC  X(09).
000530     03  FILLER                PIC  X(03).
000540     03  SYS-H2                PIC  X(05).
000550     03  FILLER                PIC  X(03).
000560     03  SYS-H3                PIC  X(09).
000570     03  FILLER                PIC  X(03).
000580     03  SYS-H4                PIC  X(07).
000590     03  FILLER                PIC  X(36).
000600 01  SYS-L2.
000610     03  FILLER                PIC  X(07).
000620     03  SYS-FILE              PIC  X(06).
000630     03  FILLER                PIC  X(06).
000640     03  SYS-DRIVE             PIC  X(01).
000650     03  FILLER                PIC  X(08).
000660     03  SYS-EXT               PIC  X(03).
000670     03  FILLER                PIC  X(07).
000680     03  SYS-SIZE              PIC  Z(04)9.
000690     03  FILLER                PIC  X(37).
      /
000700 WORKING-STORAGE SECTION.
       77  WS-CHECK                  PIC  X(18)         VALUE "aeWlimemnomLalismJ".
000710 77  WS-S1                     PIC  9(04) COMP-5.
000720 77  WS-S2                     PIC  9(04) COMP-5.
000730 77  WS-S3                     PIC  9(04) COMP-5.
000740 77  WS-S4                     PIC  9(04) COMP-5.
000740 77  WS-S6                     PIC  9(04) COMP-5.
000740 77  WS-SUB-6                  PIC  9(04) COMP-5.
000750 77  WS-C1                     PIC  9(04) COMP-5.
000760 77  WS-C2                     PIC  9(04) COMP-5.
000770 77  WS-C3                     PIC  9(04) COMP-5.
000780 77  WS-C4                     PIC  9(04) COMP-5.
000710 77  WS-IXS1                   PIC  9(04) COMP-5.
000790 77  WS-SECKEY                 PIC  9(04) COMP-5.
000800 77  WS-PARKEY                 PIC  9(04) COMP-5.
000800 77  WS-PRNKEY                 PIC  9(04) COMP-5.
       77  WS-DTE                    PIC  9(08) COMP-3.
       77  WS-LEV                    PIC  9(01).
000810 77  WS-SERIAL                 PIC  X(07).
       77  WS-TERMS                  PIC  9(04) COMP-5.
000820 77  WS-NUM                    PIC  Z(04)9.
       77  WS-CONTRACT               PIC  X(01)         VALUE SPACE.
000870 77  WS-OPTION                 PIC  X(01).
000880 77  WS-INIT                   PIC  9(01)         VALUE ZERO.
000890 77  WS-ERROR                  PIC  9(01)         VALUE ZERO.
002570 77  WS-PGE-LENGTH             PIC  9(02)         VALUE 66.
001870 77  WS-PRN-LENGTH             PIC  9(02)         VALUE 62.
       77  WS-RESULT                 PIC  9(02) COMP-X.
       77  WS-FUNCTION               PIC  9(02) COMP-X.
000910 77  WS-CHAR                   PIC  X(01).
000920 77  WS-PAR                    PIC  9(01)         VALUE 0.
000940 77  WS-START                  PIC  9(08).
000950 77  WS-END                    PIC  9(08).
000950 77  WS-XXX                    PIC  X(07)         VALUE "XXXXXXX".
       77  WS-PACK                   PIC  X(08).

000960 01  WS-GRAPHICS.
000970     03  WS-GCHAR              PIC X(01) OCCURS 20.

002060 01  WS-DB-LINE.
002070     03  WS-TOP-LINE.
002080         05  WS-TCHAR          PIC  X(01) OCCURS 80.
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH            PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER            PIC  X(01).
               05  WS-TOP-COMP       PIC  X(40).
001430         05  FILLER            PIC  X(23).
               05  WS-WRKHD          PIC  X(11).
001430         05  FILLER            PIC  X(01).
               05  WS-WRKST          PIC  X(03).
001430         05  FILLER            PIC  X(01).
002450     03  WS-MID-LINE.
002460         05  WS-MCHR           PIC  X(01) OCCURS 80.
           03  WS-MID78 REDEFINES WS-MID-LINE.
               05  FILLER            PIC  X(01).
               05  WS-BLNK78         PIC  X(78).
               05  FILLER            PIC  X(01).
002110     03  WS-BOT-LINE.
002120         05  WS-BCHAR          PIC  X(01) OCCURS 80.
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH            PIC  X(01) OCCURS 80.

000980 01  W00-ALPHA-SER.
000990     03  W00-SERIAL            PIC X(07).

001010 COPY "WS.WS".

001010 COPY "CHAIN.WS".

           03  W02-PRINTER.
               05  W02-REPORT        PIC X(07) VALUE "BUP000.".
               05  W02-USER          PIC X(05) VALUE SPACES.

           03  W02-RENAME-FILE       PIC  X(64).

           03  W02-NEW-FILE          PIC  X(64).

           03  W02-WORK-PTH.
               05  W02-COMP          PIC  X(03).
               05  FILLER            PIC  X(05) VALUE "_WORK".
               05  FILLER            PIC  X(01) VALUE X"00".

           03  W02-CHECK-SYS.
               05  W02-CHK-ID        PIC  X(03) VALUE SPACES.
               05  FILLER            PIC  X(16) VALUE "_DATA\SYSTEM.DAT".
               05  FILLER            PIC  X(01) VALUE X"00".

           03  W02-OFFLINE.
               05  FILLER            PIC  X(07) VALUE "OFFLINE".
               05  FILLER            PIC  X(01) VALUE X"00".

001160 01  W05-COMMAND.
001170     03  W05-CHAR              PIC  X(01) OCCURS 128.

001180 01  W05-DETAIL.
001190     03  W05-DC                PIC  X(01) OCCURS 2.

001200 01  W05-COM                   REDEFINES W05-DETAIL.
001210     03  W05-DRW.
001220         05  W05-DRAW          PIC  9(02).

       01  W10-FILE-PATHS.
         03  W10-CURRENT-PATH        PIC  X(52).

         03  W10-CURR-PATH           REDEFINES W10-CURRENT-PATH.
             05  W10-CURR-CHAR       PIC  X(01) OCCURS 52.
         03  W10-WRK-PATH            PIC  X(52).

         03  W10-WK-PATH             REDEFINES W10-WRK-PATH.
             05  W10-WRK-CHAR        PIC  X(01) OCCURS 52.
         03  W10-PATH                PIC  X(09).

	   03  W10-COM-PATH	       REDEFINES W10-PATH.
             05  W10-COM-CHAR        PIC  X(01) OCCURS 9.

       COPY "W12.WS".

001490 01  W15-HEX.
001500     03  W15-ASCII             PIC X(01) OCCURS 256.

001510 01  W20-DATE.
001520     03  W20-DAY               PIC 9(02).
001530     03  W20-MONTH             PIC 9(02).
           03  W20-CENT              PIC 9(02).
001540     03  W20-YEAR              PIC 9(02).

001550 01  W20-DTE1                  REDEFINES W20-DATE.
001560     03  W20-DTE               PIC 9(08).

001550 01  W20-DTE2                  REDEFINES W20-DATE.
           03  W20-CC                PIC 9(02).
001520     03  W20-YY                PIC 9(02).
001530     03  W20-MM                PIC 9(02).
001540     03  W20-DD                PIC 9(02).

       01  W20-LEAP                  REDEFINES W20-DATE.
           03  FILLER                PIC 9(04).
           03  W20-CY                PIC 9(04).

001570 01  W25-CALCS.
001580     03  W25-RESULT            PIC  9(09)V99.

001590     03  W25-RESULT1           REDEFINES W25-RESULT.
001600         05  W25-DAYS          PIC  9(03).
001610         05  W25-WHOLE         PIC  9(06).
001620         05  W25-DECIMAL       PIC  9(02).

           03  W25-RESULT2           REDEFINES W25-RESULT.
               05  FILLER            PIC  9(02).
               05  W25-KEY           PIC  9(04).
               05  W25-SUB           PIC  9(01).
               05  FILLER            PIC  9(02).
001630     03  W25-PASS.
001640         05  W25-PASS1         PIC  9(03).
001650         05  W25-PASS2         PIC  9(03).
001660     03  W25-TIME              PIC  9(08).

001670     03  W25-TRED              REDEFINES  W25-TIME.
001690         05  W25-FACT          PIC  9(08).

               05  W25-DATE          REDEFINES W25-FACT.
                   07  W25-CC        PIC  9(02).
                   07  W25-YY        PIC  9(02).
                   07  W25-MM        PIC  9(02).
                   07  W25-DD        PIC  9(02).

001700 01  W30-MONTHS.
001710     03  W30-MONTH-NAMES.
001720         05  W30-ENG           PIC X(36)     VALUE "JanFebMarAprMayJunJulAugSepOctNovDec".

001740     03  W30-MONTH             REDEFINES W30-MONTH-NAMES PIC X(03) OCCURS 12.

       01  W31-DAYS.
           03  W31-DAY-NUMBERS.
               05  W31-DAYNO         PIC  X(36)    VALUE "000031059090120151181212243273303334".

               05  W31-DN            REDEFINES W31-DAYNO.
                   07  W31-NO        PIC 9(03) OCCURS 12.
           03  W31-DTE1              PIC 9(06) COMP-5.
           03  W31-DTE2              PIC 9(06) COMP-5.

001760 01  W35-PASS.
001770     03  W35-PASS1             PIC 9(03).
001780     03  W35-PASS2             PIC 9(03).

001790 01  W35-PASSW                 REDEFINES W35-PASS.
001800     03  W35-PNUM              PIC 9(06).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W75-KEYS.
           03  W75-S                 PIC  9(02) COMP-5.
           03  W75-S1                PIC  9(02) COMP-5.
           03  W75-S2                PIC  9(02) COMP-5.
           03  W75-COM               PIC  X(03) OCCURS 18.

001870 01  W85-PASS.
001880     03  W85-PW                PIC  X(02).
001890     03  W85-SUPER             PIC  X(06) OCCURS 9.
001910     03  W85-ENTRY             PIC  9(02)    COMP-0.
001920     03  W85-MARG              PIC  9(02)V99.
001930     03  FILLER                PIC  X(02).

       LINKAGE SECTION.

       COPY "CHAIN.LS".

001160 01  L05-COMMAND.
001170     03  L05-CHAR              PIC  X(128).

       COPY "FILE.IDS".

      /
001940 SCREEN SECTION.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    A P A C   L O G O                      Ё
      *    Ё This is the logo displayed using graphics (ASCII) char-   Ё
      *    Ё acters and will later be changed to use a bitmat when the Ё
      *    Ё menus are changed to GUI.                                 Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "LOGO.CRT".

       COPY "MENUINST.CRT".

002880 01  S01.
           03  LINE  4 COLUMN  6 VALUE "зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд" FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN 74 VALUE "©"                                                                    FOREGROUND-COLOR Black.
002900     03  LINE  5 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "APAC - Business System (Version 2013).                             "  FOREGROUND-COLOR Red  HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"    FOREGROUND-COLOR Black.
002920     03  LINE  6 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "Copyright (C) 1982 - 2013  JW Lemmon.                              "  FOREGROUND-COLOR Grey HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
002940     03  LINE  7 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "This SOFTWARE is used under licence to JWL Software CC.            "  HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
002960     03  LINE  8 COLUMN  6 VALUE "Ё"                                                                    HIGHLIGHT.
           03          COLUMN  7 VALUE "It is a contravention of the copyright law to:                     "  FOREGROUND-COLOR Grey HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
002980     03  LINE  9 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "  USE this SOFTWARE without registering it.                        "  HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
003000     03  LINE 10 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "  HAVE additional copies other than your WORKING & SECURITY disks. "  HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
003030     03  LINE 11 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "  USE the SOFTWARE on more than one computer or for more than one  "  HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
003060     03  LINE 12 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "  Business.                                                        "  HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
003070     03  LINE 13 COLUMN  6 VALUE "Ё"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "The SOFTWARE remains the property of James W Lemmon.               "  FOREGROUND-COLOR Grey HIGHLIGHT.
002920     03          COLUMN 74 VALUE "Ё"                                                                    FOREGROUND-COLOR Black.
           03  LINE 14 COLUMN  6 VALUE "ю"                                                                    FOREGROUND-COLOR Blue HIGHLIGHT.
           03          COLUMN  7 VALUE "ддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды" FOREGROUND-COLOR Black.

003090     03  LINE 16 COLUMN  7 VALUE "Do you accept these conditions (Y/N)  "                               FOREGROUND-COLOR Brown HIGHLIGHT.

000850 01  S-PROG.
003430     03  LINE  2 COLUMN 35 VALUE "SYSTEM MENU" BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S-MENU.
           02  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
               03  LINE  8 COLUMN 30 VALUE "здддддддддддддддддддд".
               03          COLUMN 51 VALUE "©"                      FOREGROUND-COLOR Black.
               03  LINE  9 COLUMN 30 VALUE "Ё   ".
               03          COLUMN 34 VALUE "SYSTEM CONTROL".
               03          COLUMN 48 VALUE "   Ё"                   FOREGROUND-COLOR Black.
               03  LINE 10 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "1".
               03          COLUMN 33 VALUE " Run the System   Ё"    FOREGROUND-COLOR Black.
               03  LINE 11 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "2".
               03          COLUMN 33 VALUE " Special Routines Ё"    FOREGROUND-COLOR Black.
               03  LINE 12 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "9".
               03          COLUMN 33 VALUE " Exit to Op-SystemЁ"    FOREGROUND-COLOR Black.
               03  LINE 13 COLUMN 30 VALUE "ю".
               03          COLUMN 31 VALUE "дддддддддддддддддддды"  FOREGROUND-COLOR Black.

       01  S-MENU2.
           02  BACKGROUND-COLOR Green FOREGROUND-COLOR Green HIGHLIGHT.
               03  LINE  5 COLUMN 30 VALUE "здддддддддддддддддддд".
               03          COLUMN 51 VALUE "©"                      FOREGROUND-COLOR Black.
               03  LINE  6 COLUMN 30 VALUE "Ё  ".
               03          COLUMN 33 VALUE "SPECIAL ROUTINES"       FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 49 VALUE "  Ё"                    FOREGROUND-COLOR Black.
               03  LINE  7 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "1"	                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Offline Location Ё"    FOREGROUND-COLOR Black.
               03  LINE  8 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "2"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Initialize Files Ё"    FOREGROUND-COLOR Black.
001470         03  LINE  9 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "3"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Register System  Ё"    FOREGROUND-COLOR Black.
001570         03  LINE 10 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "4"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Run the System   Ё"    FOREGROUND-COLOR Black.
001500         03  LINE 11 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "5"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Print ASCII TableЁ"    FOREGROUND-COLOR Black.
               03  LINE 12 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "6"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Clear W/Stat. LogЁ"    FOREGROUND-COLOR Black.
               03  LINE 13 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "7"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Amend No of W/St Ё"    FOREGROUND-COLOR Black.
               03  LINE 14 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "8"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Renew Contract   Ё"    FOREGROUND-COLOR Black.
               03  LINE 15 COLUMN 30 VALUE "Ё ".
               03          COLUMN 32 VALUE "9"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 33 VALUE " Exit to Op-SystemЁ"    FOREGROUND-COLOR Black.
               03  LINE 16 COLUMN 30 VALUE "ю".
               03          COLUMN 31 VALUE "дддддддддддддддддддды"  FOREGROUND-COLOR Black.

       01  S-COM.
           02  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey.
               03  LINE  6 COLUMN 28 VALUE "здддддддддддддддддддддддддддддддддддддддддддддд" HIGHLIGHT.
               03          COLUMN 75 VALUE "©".
               03  LINE  7 COLUMN 28 VALUE "Ё"                                               HIGHLIGHT.
               03          COLUMN 29 VALUE "     C O M P A N Y   C O D E   L O O K U P    "  FOREGROUND-COLOR Magenta.
               03          COLUMN 75 VALUE "Ё"                                               FOREGROUND-COLOR Black.
               03  LINE  8 COLUMN 28 VALUE "Ё"                                               HIGHLIGHT.
               03          COLUMN 29 VALUE "здддбдддддддддддддддддддддддддддддддддддддддд©"  FOREGROUND-COLOR Magenta.
               03          COLUMN 75 VALUE "Ё"                                               FOREGROUND-COLOR Black.
               03  LINE 21 COLUMN 28 VALUE "Ё"                                               HIGHLIGHT.
               03          COLUMN 29 VALUE "юдддадддддддддддддддддддддддддддддддддддддддды"  FOREGROUND-COLOR Magenta.
               03          COLUMN 75 VALUE "Ё"                                               FOREGROUND-COLOR Black.
               03  LINE 22 COLUMN 28 VALUE "ю"                                               HIGHLIGHT.
               03          COLUMN 29 VALUE "дддддддддддддддддддддддддддддддддддддддддддддды" FOREGROUND-COLOR Black.
       01  SD-COM.
           03  COLUMN 30 FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey PIC X(03) FROM COM-CODE.
           03  COLUMN 34 FOREGROUND-COLOR Blue BACKGROUND-COLOR Grey PIC X(40) FROM COM-NAME.

003330 01  S03.
003340     03  LINE 18 COLUMN  7 VALUE "Company Identity Code ?" FOREGROUND-COLOR Brown HIGHLIGHT.

003370 01  S04.
003380	   03  LINE 18 COLUMN 31 PIC X(03) USING WS-SYS-ID AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

003490 01  S06.
           02  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
003500         03  LINE  8 COLUMN 12 VALUE "Enter Password >      <".
003510	   03  LINE 16 COLUMN  8 VALUE " NB. ALL terminals MUST have signed off before this routine ".
003550	   03  LINE 17 COLUMN  8 VALUE " can  be run.  If this routine is run  while terminals  are ".
003590	   03  LINE 18 COLUMN  8 VALUE " still  attached, the  results are unpredictable and  could ".
003630	   03  LINE 19 COLUMN  8 VALUE " result in the LOSS OF DATA or worse if a recovery is run.  ".

003670 01  S07.
003680     03  LINE  8 COLUMN 28 PIC  X(06) TO W40-CSHPASS AUTO SECURE FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S08.
           03  LINE 17 COLUMN 32 PIC  X(07) TO W00-SERIAL AUTO SECURE FOREGROUND-COLOR Grey HIGHLIGHT.

003700 01  S10.
003710     03  LINE  5 COLUMN 27 PIC Z9/99/9999 USING SEC-DATE AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

003730 01  S11.
003740     03  LINE 20 COLUMN  7 VALUE "Work Station id " FOREGROUND-COLOR Brown HIGHLIGHT.

003750 01  S12.
003760     03  LINE 20 COLUMN 23 PIC X(03) USING WS-USER AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

003780 01  S13.
003790     02  S13A.
003800	   03  LINE  2 COLUMN 26 VALUE "OFFLINE FILE LOCATIONS"                         FOREGROUND-COLOR Grey HIGHLIGHT.
003820     02  S13B.
               03  LINE 12 COLUMN 18 VALUE "This routine will set up the offline locations" FOREGROUND-COLOR Brown HIGHLIGHT.
               03  LINE 14 COLUMN 18 VALUE "Press ANY key to continue"                      FOREGROUND-COLOR Brown HIGHLIGHT.

004790 01  S17.
004800     03  LINE  3 COLUMN 33 VALUE "INITIALIZE FILES"        FOREGROUND-COLOR Grey HIGHLIGHT.
004820     03  LINE  5 COLUMN  4 VALUE "Are you sure (Y/N)? > <" FOREGROUND-COLOR Brown HIGHLIGHT.

004790 01  S17A.
004800     03  LINE  3 COLUMN 31 VALUE "REGISTER THE SYSTEM"     FOREGROUND-COLOR Grey HIGHLIGHT.
004820     03  LINE  5 COLUMN  4 VALUE "Are you sure (Y/N)? > <" FOREGROUND-COLOR Brown HIGHLIGHT.

004790 01  S17B.
004800     03  LINE  3 COLUMN 28 VALUE "AMEND NO. OF WORK STATIONS" FOREGROUND-COLOR Grey HIGHLIGHT.
004820     03  LINE  5 COLUMN  4 VALUE "Are you sure (Y/N)?"        FOREGROUND-COLOR Brown HIGHLIGHT.

004790 01  S17C.
004800     03  LINE  3 COLUMN 34 VALUE "RENEW CONTRACT"          FOREGROUND-COLOR Grey HIGHLIGHT.
004820     03  LINE  5 COLUMN  4 VALUE "Are you sure (Y/N)? > <" FOREGROUND-COLOR Brown HIGHLIGHT.

004830 01  S18.
       02  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey.
           03  LINE  4 COLUMN 16 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддд" HIGHLIGHT.
           03          COLUMN 65 VALUE "©"                                                 FOREGROUND-COLOR Black.
           03  LINE  5 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "Packaging and Marketing:                        "  HIGHLIGHT.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
004850     03  LINE  6 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "       WORKGROUP DATA SOLUTIONS.                "  FOREGROUND-COLOR Black.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
           03  LINE  7 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "       18 Milford Crest, North Riding, JHB.     "  FOREGROUND-COLOR Black.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
004880     03  LINE  8 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "       PO Box 68416,                            "  FOREGROUND-COLOR Black.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
004890     03  LINE  9 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "       BRYANSTON. 2021                          "  FOREGROUND-COLOR Black.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
004900     03  LINE 10 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "Support:                                        "  HIGHLIGHT.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
004910     03  LINE 11 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "  See the registration card, supplied with the  "  FOREGROUND-COLOR Black.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
           03  LINE 12 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "  system, for details of the telephone and fax  "  FOREGROUND-COLOR Black.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
004910     03  LINE 13 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "  numbers of the APAC support line.             "  FOREGROUND-COLOR Black.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
           03  LINE 14 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "  For local support in your area please contact "  HIGHLIGHT.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
           03  LINE 15 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
004910     03          COLUMN 17 VALUE "  the support line, who will in turn give you   "  HIGHLIGHT.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
           03  LINE 16 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "  details of your local support centre.         "  HIGHLIGHT.
           03          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
           03  LINE 17 COLUMN 16 VALUE "ю"                                                 HIGHLIGHT.
           03          COLUMN 17 VALUE "дддддддддддддддддддддддддддддддддддддддддддддддды" FOREGROUND-COLOR Black.

004970     03  LINE 19 COLUMN 17 VALUE "Please key in todays date (DDMMCCYY)"              FOREGROUND-COLOR Brown HIGHLIGHT.

004990 01  S20.
005000     03  LINE 19 COLUMN 54 PIC Z9/99/9999 USING W20-DTE AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S25.
           03  LINE  8 COLUMN  4 VALUE "Work Stations :" FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 10 COLUMN 10 VALUE "Package: ".
           03          COLUMN 20 PIC  X(08) FROM WS-PACK FOREGROUND-COLOR Cyan HIGHLIGHT.
           03  S25A.
               05  LINE 8 COLUMN 20 PIC ZZ9 USING WS-TERMS AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S30.
           03  LINE 12 COLUMN 50 PIC ZZ9 USING WS-TERMS AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

      /
010570 PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS L05-COMMAND DATA-FILE-IDS.
005070 AA000-MAIN              SECTION.
005080 AA00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё This routine has now been upgraded to the latest COBOL    Ё
      *    Ё syntax regarding date.				                 Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             ACCEPT W25-FACT FROM DATE[YYYYMMDD].
             MOVE W25-FACT           TO W12-T-YMD.
             MOVE W25-CC             TO W12-CENT.
             MOVE W25-YY             TO W12-YEAR.
             MOVE W25-MM             TO W12-MONTH.
             MOVE W25-DD             TO W12-DAY.
             MOVE W12-TODAY          TO TODAY-DDMMYY.
005100       PERFORM AL000.
005110       MOVE W05-DRAW           TO WS-DRAW.

       AA02.
005210       DISPLAY CLR-SCREEN.
             DISPLAY WS-BLNK78 AT 0202 WITH FOREGROUND-COLOR Cyan.
             MOVE 5                  TO SHADE-ROW.
             MOVE 8                  TO SHADE-COL.
             MOVE 67                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S01.
             PERFORM SCREEN-SHADOW.
005130       PERFORM AD000.
005140     IF WS-PAR = 9
005150         PERFORM AK000
               GO TO AA02.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
005210       DISPLAY CLR-SCREEN.
005220       DISPLAY S-PROG.
             MOVE 9                  TO SHADE-ROW.
             MOVE 32                 TO SHADE-COL.
             MOVE 20                 TO SHADE-WIDTH.
             MOVE 5                  TO SHADE-LINES.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       The System Start Menu		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
005240       DISPLAY S-MENU.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 9                   TO TOP-ROW.
             MOVE 11                  TO BOTTOM-ROW.
             MOVE 8                   TO SCREEN-LIN.
             MOVE 32                  TO SCREEN-COL.
             MOVE 18                  TO STRING-LENGTH.
             PERFORM SAVE-ATTR.
006280       MOVE 2                   TO WS-SECKEY.
006300       READ SECUR.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	C H E C K   T O	  S E E	  I F	T H E	A N N U A L    Ё
      *    Ё	C O N T R A C T	  M U S T   B E	  R E N E W E D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF SEC-AGREE = "R"
              IF NOT(SEC-DAYS = 255)
                  DISPLAY "Contract must be renewed" AT 1629 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
                  IF SEC-DAYS > 15
                      MOVE SEC-ANDATE  TO W20-DTE
015070                DISPLAY "By - " AT 1729    WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
                               W20-DD            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
                              " "                WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
015080                         W30-MONTH(W12-MM) WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
                              ","                WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
                               W12-CC            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
                               W12-YY            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red
                              "        "         WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red.

       AA05.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY    PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY PERFORM POSITION-MOUSE
                                IF MOUSE-ENTER = "Y"
                                    MOVE "N" TO MOUSE-ENTER
                                    GO TO AA15
                                END-IF
                                IF MOUSE-Y = 24
                                    IF MOUSE-X = 5
	                                  MOVE 5  TO KEY-CODE-1
	                                  GO TO AA10
                                    ELSE
                                    IF MOUSE-X = 10
                                        MOVE 6  TO KEY-CODE-1
                                        GO TO AA10
                                    ELSE
                                    IF MOUSE-X > 36 AND < 44
                                        MOVE 0  TO KEY-CODE-1
                                        GO TO AA10
                                    ELSE
                                    IF MOUSE-X > 54 AND < 60
	                                  MOVE 0  TO KEY-CODE-1
	                                  MOVE 1  TO KEY-TYPE
	                                  GO TO AA10
                                    END-IF
                                END-IF
                 WHEN ENTER-KEY  GO TO AA15
                 WHEN OTHER      CALL X"E5"
               END-EVALUATE
               GO TO AA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF NOT(WS-OPTION = 1 OR 2 OR 9)
                   CALL X"E5"
                   GO TO AA05
               END-IF
               GO TO AA16
           ELSE
               CALL X"E5"
               GO TO AA05.

       AA15.
             EVALUATE SCREEN-LIN
               WHEN 9
                    MOVE "1"         TO WS-OPTION
               WHEN 10
                    MOVE "2"         TO WS-OPTION
               WHEN 11
                    MOVE "9"         TO WS-OPTION
               WHEN OTHER
                    GO TO AA05
             END-EVALUATE.
             GO TO AA18.

       AA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "9"
                    MOVE 11          TO SCREEN-LIN
               WHEN "1"
                    MOVE 9           TO SCREEN-LIN
               WHEN "2"
                    MOVE 10          TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       AA18.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
005320     IF WS-OPTION = "9"
005330         MOVE 6                TO WS-ERROR
005340         GO TO AZ000-EOJ.
005350     IF WS-OPTION = "1"
005510         PERFORM AI000
005520         IF WS-ERROR = ZERO
005530		 GO TO AZ000-EOJ.
005160	 GO TO AB00.
005170 AA999.
005180       EXIT.

005190 AB000	    SECTION.
005200 AB00.
006280	     MOVE 2		 TO WS-SECKEY.
006300	     READ SECUR.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
005210	     DISPLAY CLR-SCREEN.
005220	     DISPLAY S-PROG.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 32		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 The System Special Routines Menu	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     DISPLAY S-MENU2.
	     DISPLAY MENU-INSTRUCT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If the USER of the system is not on an annual contract    Ё
      *    Ё then change the 'Renew Contract' menu item to Not Applic- Ё
      *    Ё able and set WS-CONTRACT to 'N'.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF NOT (SEC-AGREE = "R") OR
	      (SEC-AGREE = "R" AND SEC-DAYS = 255)
	       MOVE "N"		 TO WS-CONTRACT
	       DISPLAY " (N/A)" AT 1444
			WITH FOREGROUND-COLOR Black
			     BACKGROUND-COLOR Green.
	     PERFORM SCREEN-SHADOW.

       AB02.
	     MOVE 6		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 5		 TO SCREEN-LIN.
	     MOVE 32		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       AB05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AB10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO AB15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO AB10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO AB10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO AB10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO AB10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO AB15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AB05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF (WS-OPTION < 1 OR > 9)
		   CALL X"E5"
		   GO TO AB05
	       END-IF
	       GO TO AB16
	   ELSE
	       CALL X"E5"
	       GO TO AB05.

       AB15.
	     EVALUATE SCREEN-LIN
	       WHEN 6
		    MOVE "1"	 TO WS-OPTION
	       WHEN 7
		    MOVE "2"	 TO WS-OPTION
	       WHEN 8
		    MOVE "3"	 TO WS-OPTION
	       WHEN 9
		    MOVE "4"	 TO WS-OPTION
	       WHEN 10
		    MOVE "5"	 TO WS-OPTION
	       WHEN 11
		    MOVE "6"	 TO WS-OPTION
	       WHEN 12
		    MOVE "7"	 TO WS-OPTION
	       WHEN 13
		    MOVE "8"	 TO WS-OPTION
	       WHEN 14
		    MOVE "9"	 TO WS-OPTION
	       WHEN OTHER
		    GO TO AB05
	     END-EVALUATE.
	     GO TO AB18.

       AB16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "9"
		    MOVE 14	 TO SCREEN-LIN
	       WHEN "1"
		    MOVE 6	 TO SCREEN-LIN
	       WHEN "2"
		    MOVE 7	 TO SCREEN-LIN
	       WHEN "3"
		    MOVE 8	 TO SCREEN-LIN
	       WHEN "4"
		    MOVE 9	 TO SCREEN-LIN
	       WHEN "5"
		    MOVE 10	 TO SCREEN-LIN
	       WHEN "6"
		    MOVE 11	 TO SCREEN-LIN
	       WHEN "7"
		    MOVE 12	 TO SCREEN-LIN
	       WHEN "8"
		    MOVE 13	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       AB18.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     EVALUATE WS-OPTION
	       WHEN "9"
005330		    MOVE 6	 TO WS-ERROR
005340		    GO TO AZ000-EOJ
005350	       WHEN "1"
005360		    CALL "UTP\OFFLFILE" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
		    CANCEL "UTP\OFFLFILE"
005380	       WHEN "2"
005420		    PERFORM AG000
005430		    IF WS-ERROR = 9
005440			GO TO AZ000-EOJ
		    END-IF
005410	       WHEN "3"
005390		    PERFORM AH000
005380	       WHEN "4"
005510		    PERFORM AI000
005520		    IF WS-ERROR = ZERO
005530			GO TO AZ000-EOJ
		    END-IF
005500	       WHEN "5"
		    CALL "UTP\ASCIIREP" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
		    CANCEL "UTP\ASCIIREP"
005560	       WHEN "6"
005600		    PERFORM AM000
005560	       WHEN "7"
005600		    PERFORM AN000
	       WHEN "8"
		    IF NOT (WS-CONTRACT = "N")
			PERFORM AO000
		    END-IF
	       WHEN OTHER
005620		    GO TO AB05
	     END-EVALUATE.
	     GO TO AB00.
      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   D A T E   V A L I D A T I O N   R O U T I N E       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
005630 AB000-DATE              SECTION.
005640 AB000-VALID-DATE.
005650	     MOVE ZERO		 TO WS-ERROR.
005660     IF W20-DAY < 01 OR > 31
005670         GO TO AB010-ERROR.
005680     IF W20-MONTH < 01 OR > 12
005690         GO TO AB010-ERROR.
005700     IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
005710         GO TO AB999-EXIT.
005720     IF W20-MONTH = 02 AND W20-DAY > 28
005730         GO TO AB005-FEB.
005740     IF W20-DAY > 30
005750         GO TO AB010-ERROR.
005760       GO TO AB999-EXIT.

005770 AB005-FEB.
005780     IF W20-DAY > 29
005790         GO TO AB010-ERROR.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	Check for a leap year if February and the day is 29    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
005800	     DIVIDE W20-CY BY 4  GIVING W25-RESULT.
005810	   IF W25-DECIMAL = 0
005820         GO TO AB999-EXIT.

005830 AB010-ERROR.
005840	     MOVE 1		 TO WS-ERROR.

005850 AB999-EXIT.
005860       EXIT.
      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   E N D   O F F   P R O G R A M	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
021980 AZ000-END	       SECTION.
021990 AZ000-EOJ.
	   IF WS-DEM > 2
006280	       MOVE 2		 TO WS-SECKEY
006300	       READ SECUR
	       MOVE 2		 TO SEC-DEM
	       MOVE ZERO	 TO SEC-DATE
008570	       REWRITE SEC-REC2.
022000	   IF WS-INIT NOT = 2
022010         GO TO AZ05.
022090       CLOSE PARAM.

022100 AZ05.
022110       CLOSE SECUR.
022130       DISPLAY CLR-SCREEN.
	     MOVE WS-ERROR	 TO WS-RET.
	     MOVE WS-PARID	 TO LS-PARID.
	     MOVE WS-USER-ID	 TO LS-USER-ID.
	     MOVE WS0-PROGRAMS	 TO LS0-PROGRAMS.
	   IF WS-ERROR = ZERO
	       MOVE "C" 	 TO LS-INSTR
	       MOVE SPACES	 TO LS-NEXT-PRG
	   ELSE
	   IF WS-ERROR = 9
	       MOVE "I" 	 TO LS-INSTR
	       MOVE "UTP\APACINST"
				 TO LS-NEXT-PRG
	   ELSE
	       MOVE "E" 	 TO LS-INSTR.

       AZ99.
	     EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  SCREEN-SHADOW 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:						       Ё
      *	   Ё	       SHADE-ROW   - Top line of the box + 1.	       Ё
      *	   Ё	       SHADE-COL   - Left line of box + 2.	       Ё
      *	   Ё	       SHADE-WIDTH - Width of the box - 2.	       Ё
      *	   Ё	       SHADE-LINES - Hight of box - 1.		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  CHECK-CORRECT 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё	      "Correct ENTER - N if incorrect"		       Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 13 (default); move another value to SLIN and PERFORM      Ё
      *    Ё CHECK-SETUP THRU CHECK-EXIT.			       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.CRT".

      /    *************************************************************
      *    *****	I N I T I A L I Z E   P R O G R A M	   *****
      *    *************************************************************
005870 AD000	       SECTION 50.
005880 AD00.
	   IF LS-INSTR = "X"
	       DISPLAY WS-BLNK78 AT 1602 WITH FOREGROUND-COLOR Cyan
	       MOVE SPACE	 TO LS-INSTR
	       GO TO AD05.
005890	     ACCEPT WS-OPTION AT 1644
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
005950	   IF NOT (WS-OPTION = "Y" OR "N")
005960         GO TO AD00.
005970	   IF NOT (WS-OPTION = "Y")
005980	       STOP RUN.
	     MOVE SPACES	 TO WS-SYS-ID.
      *    *************************************************************
      *    ****       G E T   S Y S T E M   E X T E N S I O N	    ****
      *    *************************************************************
005990 AD05.
006000       DISPLAY S03.
	     DISPLAY "F2" AT 5002
		      WITH BACKGROUND-COLOR Cyan
			   FOREGROUND-COLOR Brown HIGHLIGHT
		     " to view existing codes"
		      WITH BACKGROUND-COLOR Cyan
			   FOREGROUND-COLOR Blue.
006010	     ACCEPT S04.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F2-KEY
		     OPEN INPUT APCCOM
018810		     PERFORM COMPANY-LOOKUP
		     CLOSE APCCOM
		     GO TO AD05
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AD05
	       END-EVALUATE.
	     PERFORM CLEAR-L50.
	   IF WS-SYS-ID = SPACES
	       GO TO AD05.
006020	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-SYS-ID
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
006080	     MOVE 1		 TO WS-LENGTH.

       AD05-APAC.
	     PERFORM APACFILES.
006190	   IF NOT (WS-ERROR = 9)
	       OPEN I-O SECUR.
006200	   IF RUNTIME-ERROR
	       IF FLE-LOCKED
006210		   MOVE "Files locked - try later - Press ENTER"
006220				 TO WS-ERR-MES
006230		   PERFORM ERROR-LENGTH THRU ERROR-EXIT
006240		   STOP RUN.
	   IF (WS-STAT1 = "9") AND
	      (WS-STAT2H = 009 OR 199) AND
	      (WS-DSKDRV = "N")
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       F I L E S   N O T   A V A I L A B L E	       Ё
      *    Ё  Check if this is a Network and if off-line files exist.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       PERFORM AD200
	       IF WS-ERROR = 0
		   PERFORM AD100
		   IF WS-OPTION = "Y"
		       MOVE "OFFLINE\"
				 TO W02-N-OR-L
				    W02-L-OR-N
				    WS-L-OR-N
		       MOVE "L"	 TO WS-DSKDRV
		       GO TO AD05-APAC
		   ELSE
		       MOVE "System Aborted- No network drive"
				 TO WS-ERR-STRING
003260		       PERFORM ERROR-MESSAGE
		       STOP RUN.
006250	   IF WS-STATUS NOT = "00"
006260	       MOVE "Invalid or file not on disk - Exit or 'R'ekey"
				 TO WS-ERR-MES
006230	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       IF WS-OPTION = "R"
		   GO TO AD05
               ELSE
006270             STOP RUN.
006280	     MOVE 2		 TO WS-SECKEY.

006290 AD005.
006300       READ SECUR.
006310     IF WS-STATUS = "00"
006320         GO TO AD005A.
006330     IF RUNTIME-ERROR
006340         IF RECORD-LOCKED
006350             GO TO AD005.
006260	     STRING "Invalid or file not on disk - "
		     DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

006380 AD005A.
	     DISPLAY SEC-COMP AT 2207
		     WITH FOREGROUND-COLOR Brown HIGHLIGHT.
	   IF (SEC-TERMS < 2 OR > 100)
	       DISPLAY " Single USER System" AT 2247
			WITH FOREGROUND-COLOR Grey HIGHLIGHT
	   ELSE
	       ADD 1 SEC-TERMS	 GIVING WS-NUM
	       DISPLAY " " AT 2247
			WS-NUM WITH FOREGROUND-COLOR Grey HIGHLIGHT
		       " WORK Stations"
			WITH FOREGROUND-COLOR Grey HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
006160	   IF WS-OPTION = "N"
               CLOSE SECUR
006170         GO TO AD05.
	     MOVE SEC-COMP	 TO WS-COMPANY.
	     MOVE SEC-SYS	 TO APAC-SYSTEM WS-APAC-SYSTEM.
      *
      *     The following if statement will be removed with the next
      *     release as any systems prior to version 8 will need major
      *     file convertions.
      *
	   IF NOT (SEC-VER = "8.00" OR "8.01" OR "8.02"
			  OR "2001" OR "2002" OR "2003"
			  OR "2004" OR "2005" OR "2006"
			  OR "2007" OR "2008" OR "2009"
			  OR "2010" OR "2011")
	       PERFORM APAC-SETUP-PATHS.
	   IF NOT (SEC-VER = "2011")
	       MOVE "2011"	 TO SEC-VER
008570	       REWRITE SEC-REC2.
	     MOVE SEC-VER	 TO APAC-VERSION WS-APAC-VERSION.
006180	     PERFORM CLEAR-L50.
	     MOVE 40		 TO WS-S1.

       AD005B.
           IF WS-COMPCHR (WS-S1) = SPACE
               MOVE WS-G8        TO WS-COMPCHR (WS-S1)
               IF WS-S1 > 1
                   SUBTRACT 1    FROM WS-S1
                   GO TO AD005B.
             MOVE WS-COMPANY     TO WS-TOP-COMP.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      G E T   O P E R A T O R	N U M B E R	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
006630 AD05B.
006640       DISPLAY S11.

006650 AD05U.
006660	     MOVE WS-SYS-ID	 TO WS-USER.
006670       ACCEPT S12.
006680	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-USER
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
006740	     MOVE 1		 TO WS-LENGTH.
006750	     MOVE SEC-PMODS	 TO WS0-PMODS.
	   IF WS-USER = WS-SYS-ID
	       MOVE "Supervisorд"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE WS-USER	 TO WS-WRKST.
             MOVE SEC-DEM        TO WS-DEM.
006790	   IF (SEC-DTP NOT = 2) AND (SEC-STP NOT = 2) AND
006800        (SEC-CRP NOT = 2) AND (SEC-GLP NOT = 2) AND
006810        (SEC-POS NOT = 2) AND (SEC-HPD NOT = 2) AND
006820        (SEC-JCP NOT = 2) AND (SEC-VHP NOT = 2)
               MOVE "LPT1"       TO W02-PRINTER
006830         GO TO AD08.
006840	     OPEN I-O PARAM.
006850	   IF RUNTIME-ERROR
	       IF FLE-LOCKED
006860		   DISPLAY "Files locked - try later - Press ENTER"
			    AT 2312
006870			    WITH FOREGROUND-COLOR Brown HIGHLIGHT
			   "   " WS-PARID
006880		   ACCEPT WS-OPTION AT 2351
			  WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO
006890		   STOP RUN.
006900	     MOVE 2		 TO WS-INIT.
006910	   IF WS-STATUS NOT = "00"
006920         GO TO AD06.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		CHECK IF LAST RUN ENDED CORRECTLY	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
006930	     MOVE 4		 TO WS-PARKEY.

006940 AD05-PAR.
006950       READ PARAM LOCK.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980             GO TO AD05-PAR.
006990	   IF PAR-USERS = 0
007020	       MOVE 1		 TO WS-PAR PAR-LOGON
007030         GO TO AD07U.
007040	     MOVE 1		 TO WS-S1.
007050	     MOVE 0		 TO WS-S2 WS-S3.

007060 AD05-CHECK.
007070	   IF PAR-USR(WS-S1) = WS-USER
007080	       IF PAR-PROG(WS-S1) = SPACES
007090		   MOVE SPACES	 TO PAR-USR (WS-S1)
		   SUBTRACT 1	 FROM PAR-USERS
007100         ELSE
007120             GO TO AD06.
007160     IF WS-S1 < 24
007170	       ADD 1		 TO WS-S1
007180         GO TO AD05-CHECK.
007270	     MOVE 1		 TO PAR-LOGON.
007280       GO TO AD07U.

007290 AD06.
	     DISPLAY CLR-SCREEN.
007300	     DISPLAY "Last run not ended off correctly" AT 0814
		      WITH FOREGROUND-COLOR Grey HIGHLIGHT.
007320     IF PAR-PROG (WS-S1) = "99"
007330	       DISPLAY "Reload files and redo any updating done since la
      - 	       "st BACKUP" AT 1014 WITH BACKGROUND-COLOR Grey
			FOREGROUND-COLOR Red HIGHLIGHT
007350         STOP RUN.
007360	     DISPLAY "Recovery procedure will now be initiated" AT 1014
		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
007380	     DISPLAY "Press ANY key to continue" AT 1214
		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.

007400 AD07.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
007500	     MOVE 9		 TO WS-PAR.
007510       UNLOCK PARAM.
007520	     GO TO AD99.

007530 AD07U.
007540       REWRITE PAR-RECORD4.
007550       UNLOCK PARAM.

       AD07W.
             MOVE 1              TO WS-PARKEY.
006950       READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980             GO TO AD07W.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё C H E C K	 I F   D E M O N S T R A T I O N   S Y S T E M Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF (SEC-DEM = 1) OR (PAR-STAT = 1)
               IF SEC-COUNT < 75
		   PERFORM AD15
               END-IF
           ELSE
               MOVE ZERO         TO SEC-COUNT.
             MOVE SEC-COUNT      TO WS-DAYS.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 C H E C K   F O R   E X P I R Y	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF SEC-COUNT > 65
               COMPUTE WS-NUM = 15 - (WS-DAYS - 65)
	       DISPLAY "This package must be registered within" AT 5002
			WITH BACKGROUND-COLOR Cyan
			     FOREGROUND-COLOR Red HIGHLIGHT
			WS-NUM WITH
			BACKGROUND-COLOR Cyan
			FOREGROUND-COLOR Grey HIGHLIGHT BLINK
		       " days"
			WITH BACKGROUND-COLOR Cyan
			     FOREGROUND-COLOR Red HIGHLIGHT.

       AD07X.
	     MOVE 3		 TO WS-PARKEY.
006950	     READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980             GO TO AD07X.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  A D D I T I O N A L   S E C U R I T Y   I N C L U D E D  Ё
      *    Ё	      ( ---TRYING TO CUT DOWN ON PIRACY--- )	       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё	    New Systems - already correct		       Ё
      *    Ё	    (This ROUTINE updating older versions)	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF NOT (PAR-VER = "2011")
	       MOVE "2011"	 TO PAR-VER
007540	       REWRITE PAR-RECORD3.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	  S E R I A L	N U M B E R   A N D   S Y S T E M      Ё
      *    Ё	  F R O M   P A R A M E T E R	F I L E 	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE PAR-LEV	 TO WS-LEV.
	     MOVE PAR-SERIAL	 TO WS-SERIAL.
	     MOVE PAR-CUR-YR	 TO WS-CUR-YR.
	     MOVE PAR-CUR-CEN	 TO WS-CUR-CEN.
	     MOVE PAR-PRV-CEN	 TO WS-PRV-CEN.
	     MOVE PAR-NXT-CEN	 TO WS-NXT-CEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	S U P E R V I S O R   P R I N T E R   D E T A I L S    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 110		 TO WS-USUB.
014240	     ADD 2549 WS-USUB	 GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
006950	     READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980             GO TO AD07X.
	     ADD 501 TRM-PRN4 (WS-S6)
                                 GIVING WS-SUB-6.
           IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
	     READ PARAM.
006960	   IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980             GO TO AD07X.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.

007560 AD08.
007570	     MOVE 1		 TO WS-SECKEY.
007580       READ SECUR.
007590     IF RUNTIME-ERROR
007600         IF RECORD-LOCKED
007610             GO TO AD08.
007620     IF WS-STATUS NOT = "00"
007630	       MOVE "Disk error - AD08"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
007640         STOP RUN.
007650	     MOVE 1		 TO WS-S1.

007660 AD10.
007670	     MOVE SEC-ASCII (WS-S1)
				 TO W15-ASCII (WS-S1).
007680     IF WS-S1 < 256
007690	       ADD 1		 TO WS-S1
007700         GO TO AD10.
	     MOVE 5		 TO WS-PARKEY.

       AD12.
006950	     READ PARAM.
006960	   IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980		   GO TO AD12.
	   IF NOT (PAR-BC = "Y")
	       MOVE "Y" 	 TO PAR-BC
	       MOVE 80		 TO PAR-BC-HGHT
	       MOVE 4		 TO PAR-COLUMN
	       MOVE 45		 TO PAR-ROW
	       MOVE 2		 TO PAR-NAR
	       MOVE 2		 TO PAR-WIDE
	       MOVE 2		 TO PAR-FONT
	       MOVE 160 	 TO PAR-F-ROW
	       MOVE ZERO	 TO PAR-F-COLUMN
	       MOVE 125 	 TO PAR-F-ROW2
	       MOVE ZERO	 TO PAR-F-COL2
	       MOVE "Y" 	 TO PAR-SPLIT
007540	       REWRITE PAR-RECORD5.
	     GO TO AD99.
       AD15.
             MOVE W12-T-YMD      TO WS-DTE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  N U M B E R   O F   D A Y S   F R O M   S T A R T   O F  Ё
      *    Ё  P R E V I O U S	C E N T U R Y	-   D A T E   1        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     COMPUTE W31-DTE1 = (W12-CY - (WS-PRV-CEN*100)) * 365.25.
             ADD W31-NO (W12-MM) TO W31-DTE1.
             ADD W12-DD          TO W31-DTE1.
             MOVE WS-DTE         TO W12-T-YMD.
           IF W12-T-YMD < SEC-INITIAL
               MOVE W12-T-YMD    TO SEC-INITIAL.
             MOVE SEC-INITIAL    TO W20-DTE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  N U M B E R   O F   D A Y S   F R O M   S T A R T   O F  Ё
      *    Ё  P R E V I O U S	C E N T U R Y	-   D A T E   2        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     COMPUTE W31-DTE2 = (W20-CY - (WS-PRV-CEN*100)) * 365.25.
             ADD W31-NO (W20-MM) TO W31-DTE2.
             ADD W20-DD          TO W31-DTE2.
           IF (W31-DTE1 - W31-DTE2) < 75
               SUBTRACT W31-DTE2 FROM W31-DTE1
                                 GIVING SEC-COUNT
           ELSE
               MOVE 75           TO SEC-COUNT.
008570       REWRITE SEC-REC2.

       AD99.
	     EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё   N E T W O R K   D R I V E   N O T   A V A I L A B L E   Ё
      *    Ё   M U S T	 S Y S T E M   R U N   O F F - L I N E	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AD100	       SECTION 50.
       AD105.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY "зддддддддддддддддддддддддддддддддддддддд"
		     AT 1120 WITH FOREGROUND-COLOR Black
				  BACKGROUND-COLOR Magenta
		     "©" WITH FOREGROUND-COLOR Magenta HIGHLIGHT
			      BACKGROUND-COLOR Magenta.
	     DISPLAY "Ё" AT 1220 WITH FOREGROUND-COLOR Black
				      BACKGROUND-COLOR Magenta
		     "Network files not available at present!"
		     WITH FOREGROUND-COLOR Cyan HIGHLIGHT
			  BACKGROUND-COLOR Magenta
		     "Ё" WITH FOREGROUND-COLOR Magenta HIGHLIGHT
			      BACKGROUND-COLOR Magenta.
	     DISPLAY "Ё" AT 1320 WITH FOREGROUND-COLOR Black
				      BACKGROUND-COLOR Magenta
		     "Work off-line (Y/N)                    "
		      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
			   BACKGROUND-COLOR Magenta
		     "Ё" WITH FOREGROUND-COLOR Magenta HIGHLIGHT
			      BACKGROUND-COLOR Magenta.
	     DISPLAY "ю" AT 1420 WITH FOREGROUND-COLOR Black
				      BACKGROUND-COLOR Magenta
		     "ддддддддддддддддддддддддддддддддддддддды"
		      WITH FOREGROUND-COLOR Magenta HIGHLIGHT
			   BACKGROUND-COLOR Magenta.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 22		 TO SHADE-COL.
	     MOVE 40		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE "N"		 TO WS-OPTION.

       AD110.
	     ACCEPT WS-OPTION AT 1341
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT
			 BACKGROUND-COLOR Magenta UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO AD110.
	     PERFORM RESTORE-SCREEN-3.

007710 AD199.
007720	     EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       F I L E S   N O T   A V A I L A B L E	       Ё
      *    Ё  Check if this is a Network and if off-line files exist.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AD200	       SECTION 50.
       AD205.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE "OFFLINE\"     TO W02-N-OR-L.
	     MOVE WS-SYS-ID	 TO W02-SYSPTH.
	     CALL "CBL_CHECK_FILE_EXIST" USING W02-APACFILE
					       FILE-DETAILS
				     RETURNING WS-STATUS.
	   IF WS-STAT1H = 1
	       MOVE 9		 TO WS-ERROR.
	     MOVE "SYSTEMS\"     TO W02-N-OR-L.

007710 AD999.
007720       EXIT.
      /    *************************************************************
      *    *****	    INITIALIZE MODULES (FILES)		   *****
      *    *************************************************************
011610 AG000		       SECTION 53.
011620 AG00.
011630       DISPLAY CLR-SCREEN.
011640       DISPLAY S17.
011650	     MOVE "N"		 TO WS-OPTION.
011660 AG02.
011670	     ACCEPT WS-OPTION AT 0525
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT
011680			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
011740	   IF NOT (WS-OPTION = "N" OR "Y")
011750         GO TO AG02.
011760	   IF WS-OPTION = "N"
011770         GO TO AG999.
	     DISPLAY WS-BLNK78 AT 0502 WITH FOREGROUND-COLOR Cyan.
011780	     DISPLAY "Key in date (DDMMCCYY)" AT 0504
		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
011790	     MOVE 2		 TO WS-SECKEY.

011800 AG03.
011810       READ SECUR LOCK.
011820     IF RUNTIME-ERROR
011830         IF RECORD-LOCKED
011840             GO TO AG03.
011850     IF WS-STATUS NOT = "00"
011860	       MOVE "Disk error - AG00"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011870	       STOP RUN.
	     MOVE TODAY-DDMMYY	 TO SEC-DATE.

011880 AG05.
011890       ACCEPT S10.
011900	     MOVE SEC-DATE	 TO W20-DTE.
011910	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
011920       PERFORM AB000-DATE.
011930     IF WS-ERROR = 1
011940	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011950         GO TO AG05.
	   IF WS0-LEV = 0
	       MOVE "JUNIOR"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 1
	       MOVE "ENTRY"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 2
	       MOVE "BASIC"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 3
	       MOVE "STANDARD"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 4
	       MOVE "PREMIUM"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 5
	       MOVE "G O L D"	 TO WS-PACK
	   ELSE
	       MOVE "S.MANAG."	 TO WS-PACK.
	     DISPLAY S25.

       AG06.
	     MOVE SEC-TERMS	 TO WS-TERMS.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       Is this a new system?		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF SEC-NUM = WS-XXX
               PERFORM AG30
               IF SEC-NUM = SPACES
                   MOVE WS-XXX   TO SEC-NUM
               END-IF
               IF SEC-NUM = WS-XXX
                   PERFORM AG16 THRU AG25
		   IF WS-OPTION = "N"
                       MOVE ZERO TO WS-ERROR
                       GO TO AG999
                   ELSE
		       MOVE W12-T-YMD
				 TO SEC-INITIAL
                       GO TO AG08.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Check if additional  modules  have	 been  added  and need Ё
      *    Ё to be  initialized	 or  whether  all  modules  need to be Ё
      *    Ё initialized.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
011960	   IF SEC-DTP = 1 OR SEC-STP = 1 OR SEC-CRP = 1 OR
011970        SEC-GLP = 1 OR SEC-HPD = 1 OR SEC-POS = 1 OR
011980        SEC-JCP = 1 OR SEC-VHP = 1
011990         IF SEC-DTP = 2 OR SEC-STP = 2
012000             OR SEC-CRP = 2 OR SEC-GLP = 2
012010             OR SEC-HPD = 2 OR SEC-POS = 2
012020             OR SEC-JCP = 2 OR SEC-VHP = 2
012030             GO TO AG12.
012040     IF (SEC-DTP = 2) OR (SEC-STP = 2) OR
012050        (SEC-CRP = 2) OR (SEC-GLP = 2) OR
012060        (SEC-HPD = 2) OR (SEC-POS = 2) OR
012070        (SEC-JCP = 2) OR (SEC-VHP = 2)
012080         DIVIDE SEC-DATE BY 2 GIVING W25-RESULT
012090	       ADD 13		 TO W25-RESULT
012100         MULTIPLY 2.136 BY W25-RESULT
012110     ELSE
012120	       MOVE SEC-DATE	 TO W25-RESULT.
012140	     ADD SEC-PASS	 TO W25-RESULT.
012150       ACCEPT W25-TIME FROM TIME.
012160	     ADD W25-TIME	 TO W25-RESULT.
012170	     DISPLAY W25-RESULT AT 1204.
012180	     DIVIDE 1.97	 INTO W25-RESULT.
012190	     MOVE W25-WHOLE	 TO W25-PASS.
012200	     MOVE W25-PASS1	 TO W35-PASS2.
012210	     MOVE W25-PASS2	 TO W35-PASS1.
012220	     MOVE W35-PNUM	 TO W25-RESULT.
012230     IF W25-RESULT < 100000
012240	       ADD 701010	 TO W25-RESULT
012250     ELSE
012260	       ADD 77		 TO W25-RESULT.
012270	     ADD 10112		 TO W25-RESULT.
	     ADD WS-TERMS	 TO W25-RESULT.
	     ADD WS0-LEV	 TO W25-RESULT.
012280	     MOVE W25-WHOLE	 TO SEC-PASS.
      *
      *	     DISPLAY SEC-PASS AT 1513 " " WS0-LEV.
      *
012290	     DISPLAY "Password" AT 1404.
	     DISPLAY S25.
012300	     MOVE ZERO		 TO W25-WHOLE.
012310	     ACCEPT W25-WHOLE AT 1413 WITH FOREGROUND-COLOR Brown HIGHLIGHT
012320                                NO-ECHO AUTO.
             MOVE W12-T-YMD      TO SEC-INITIAL.
             MOVE ZERO           TO SEC-COUNT
                                    SEC-DEM.
      *    *************************************************************
      *    ****      O V E R R I D E   P A S S W O R D
      * 	   F O R   I N I T I A L I Z E	 F I L E S
      * 	       Y O U R	 E Y E S   O N L Y
      * 	  ----- NOT TO BE GIVEN TO ANYONE -----
      *    *************************************************************
012330	   IF W25-WHOLE NOT = 918273
012340	       IF W25-WHOLE NOT = SEC-PASS
		   PERFORM AG15 THRU AG25.
	   IF WS-OPTION = "N"
               MOVE ZERO         TO WS-ERROR
               GO TO AG999.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       UPDATE DETAILS IN THE PARAMETER FILE	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AG07.
	     MOVE 3		 TO WS-PARKEY.
006950	     READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980		   GO TO AG07.
	     MOVE SEC-NUM	 TO PAR-SERIAL WS-SERIAL.
	     MOVE SEC-LEV	 TO PAR-LEV    WS-LEV.
007540	     REWRITE PAR-RECORD3.

       AG08.
012360	     MOVE SPACES	 TO SEC-COMP.
012370	     DISPLAY "Company Name" AT 1604.
012380	     ACCEPT SEC-COMP AT 1618
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT PROMPT AUTO.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Are the modules (files) to be reinitialized?        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
012390 AG10.
012400	     MOVE "Y"		 TO WS-OPTION.
012410     IF (WS0-STP = 2) OR (WS0-DTP = 2) OR
012420        (WS0-CRP = 2) OR (WS0-GLP = 2) OR
012430        (WS0-HPD = 2) OR (WS0-POS = 2) OR
012440        (WS0-JCP = 2) OR (WS0-VHP = 2)
012450         DISPLAY "Initialize again (Y/N)" AT 2312
			WITH FOREGROUND-COLOR Brown HIGHLIGHT
012460	       ACCEPT WS-OPTION AT 2335
		      WITH FOREGROUND-COLOR Grey HIGHLIGHT
012470			   UPDATE AUTO
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
012530	   IF WS-OPTION = "N"
012540         GO TO AG25.
012550     IF SEC-DTP = 2
012560	       MOVE 1		 TO WS0-DTP.
012570     IF SEC-STP = 2
012580	       MOVE 1		 TO WS0-STP.
012590	   IF SEC-CRP = 2
012600	       MOVE 1		 TO WS0-CRP.
012610     IF SEC-GLP = 2
012620	       MOVE 1		 TO WS0-GLP.
012630     IF SEC-HPD = 2
012640	       MOVE 1		 TO WS0-HPD.
012650     IF SEC-POS = 2
012660	       MOVE 1		 TO WS0-POS.
012670     IF SEC-JCP = 2
012680	       MOVE 1		 TO WS0-JCP.
012690     IF SEC-VHP = 2
012700	       MOVE 1		 TO WS0-VHP.

012710 AG12.
012720	   IF WS0-DTP = 1
012730	       MOVE 2		 TO SEC-DTP.
012740     IF WS0-STP = 1
012750	       MOVE 2		 TO SEC-STP.
012760     IF WS0-CRP = 1
012770	       MOVE 2		 TO SEC-CRP.
012780     IF WS0-GLP = 1
012790	       MOVE 2		 TO SEC-GLP.
012800     IF WS0-HPD = 1
012810	       MOVE 2		 TO SEC-HPD.
012820     IF WS0-POS = 1
012830	       MOVE 2		 TO SEC-POS.
012840     IF WS0-JCP = 1
012850	       MOVE 2		 TO SEC-JCP.
012860     IF WS0-VHP = 1
012870	       MOVE 2		 TO SEC-VHP.
012880	     MOVE WS-SYS-ID	 TO SEC-EXT.
	   IF SEC-AGREE = "R"
	       MOVE W12-T-YMD	 TO W20-DTE
	       ADD 1		 TO W20-YY
	       MOVE W20-DTE	 TO SEC-ANDATE
	       MOVE 255 	 TO SEC-DAYS
	       IF ((W20-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12)
		   AND (W20-DD > 16)) OR
		  ((W20-MM = 02) AND (W20-DD > 14)) OR
		  (W20-DD > 15)
		   IF W20-MM < 12
		       ADD 1	 TO W20-MM
		   ELSE
		       MOVE 1	 TO W20-MM
		       ADD 1	 TO W20-YY
		   END-IF
		   IF W20-DD < 16
		       SUBTRACT 14
				 FROM W20-DD
		   ELSE
		       SUBTRACT 15
				 FROM W20-DD
		   END-IF
		   IF W20-MM > 12
		       SUBTRACT 12
				 FROM W20-MM
		       ADD 1	 TO W20-YY
		   END-IF
	       ELSE
	       IF (W20-MM = 02 AND W20-DD < 14)
		   ADD 14	 TO W20-DD
	       ELSE
		   MOVE 28	 TO W20-DD
	       END-IF
	       IF NOT (W20-MM = 02)
		   ADD 15	 TO W20-DD
	       END-IF
	       END-IF
	       MOVE W20-DTE	 TO SEC-EXPDATE
	   END-IF.
012890	     REWRITE SEC-REC2.
012900       UNLOCK SECUR.
012910     IF WS-STATUS NOT = "00"
012920	       MOVE "Disk error - AG10"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012930         STOP RUN.
012940	     MOVE 9		 TO WS-ERROR.
012950       GO TO AG999.

012960 AG15.
012970       DISPLAY "Invalid password" AT 1804
		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       AG16.
012980       DISPLAY "Demonstration system will be created" AT 1904.
012990	     DISPLAY "Do you wish to continue (Y/N) " AT 2004.

013000 AG20.
013010	     ACCEPT WS-OPTION AT 2034
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
013070	   IF NOT (WS-OPTION = "N" OR "Y")
013080         GO TO AG20.
             MOVE 1              TO SEC-DEM.

013130 AG25.
013180       EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Get details for a new system		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AG30.
	     DISPLAY "Registration No: " AT 0704.
             ACCEPT SEC-NUM AT 0721
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
	   IF SEC-POS > ZERO
	       DISPLAY S25.
	     PERFORM CHECK-CORRECT.
006160	   IF WS-OPTION = "N"
               GO TO AG30.

       AG35.
             EXIT.

013190 AG999.
013200       EXIT.
      /    *************************************************************
      *    *****	       REGISTER THE SYSTEM		   *****
      *    *************************************************************
011610 AH000                   SECTION 54.
011620 AH00.
011630       DISPLAY CLR-SCREEN.
011640       DISPLAY S17A.
014560     IF (WS0-DTP NOT = 2) AND (WS0-STP NOT = 2) AND
014570        (WS0-CRP NOT = 2) AND (WS0-GLP NOT = 2) AND
014580        (WS0-HPD NOT = 2) AND (WS0-POS NOT = 2) AND
014590        (WS0-JCP NOT = 2) AND (WS0-VHP NOT = 2)
014600	       MOVE "Files not yet initialized"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014640         GO TO AH999.
011790	     MOVE 2		 TO WS-SECKEY.

011800 AH02.
011810       READ SECUR LOCK.
011820     IF RUNTIME-ERROR
011830         IF RECORD-LOCKED
011840             GO TO AH02.
011850     IF WS-STATUS NOT = "00"
011860	       MOVE "Disk error - AH00"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011870         STOP RUN.
           IF SEC-DEM NOT = 1
014600	       MOVE "The system is registered"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014640         GO TO AH999.
011650	     MOVE "N"		 TO WS-OPTION.

011660 AH03.
011670	     ACCEPT WS-OPTION AT 0525 WITH FOREGROUND-COLOR Grey HIGHLIGHT
011680                               UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
011740	   IF NOT (WS-OPTION = "N" OR "Y")
011750         GO TO AH03.
011760	   IF WS-OPTION = "N"
011770         GO TO AH999.
011780	     DISPLAY "Key in date (DDMMCCYY)" AT 0504.

011880 AH05.
011890       ACCEPT S10.
011900	     MOVE SEC-DATE	 TO W20-DTE.
011910	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
011920       PERFORM AB000-DATE.
011930     IF WS-ERROR = 1
011940	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011950         GO TO AH05.
	     MOVE SEC-TERMS	 TO WS-TERMS.
	   IF WS0-LEV = 0
	       MOVE "JUNIOR"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 1
	       MOVE "ENTRY"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 2
	       MOVE "BASIC"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 3
	       MOVE "STANDARD"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 4
	       MOVE "PREMIUM"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 5
	       MOVE "G O L D"	 TO WS-PACK
	   ELSE
	       MOVE "S.MANAG."	 TO WS-PACK.
	     DISPLAY S25.
	   IF SEC-NUM = WS-XXX
               PERFORM AH20
               IF SEC-NUM = SPACES
                   MOVE WS-XXX   TO SEC-NUM
               END-IF
               IF SEC-NUM = WS-XXX
                   GO TO AH999.
012080       DIVIDE SEC-DATE BY 2 GIVING W25-RESULT.
012090       ADD 13              TO W25-RESULT.
012100       MULTIPLY 2.136      BY W25-RESULT.
012140	     ADD SEC-PASS	 TO W25-RESULT.
012150       ACCEPT W25-TIME     FROM TIME.
012160	     ADD W25-TIME	 TO W25-RESULT.
012170	     DISPLAY W25-RESULT AT 1204.
012180       DIVIDE 1.97         INTO W25-RESULT.
012190       MOVE W25-WHOLE      TO W25-PASS.
012200       MOVE W25-PASS1      TO W35-PASS2.
012210       MOVE W25-PASS2      TO W35-PASS1.
012220       MOVE W35-PNUM       TO W25-RESULT.
012230     IF W25-RESULT < 100000
012240         ADD 701010        TO W25-RESULT
012250     ELSE
012260         ADD 77            TO W25-RESULT.
012270	     ADD 10112		 TO W25-RESULT.
	     ADD WS-TERMS	 TO W25-RESULT.
	     ADD WS0-LEV	 TO W25-RESULT.
012280       MOVE W25-WHOLE      TO SEC-PASS.
012290	     DISPLAY "Password" AT 1404.
	     DISPLAY S25.
012300       MOVE ZERO           TO W25-WHOLE.
      *	     DISPLAY SEC-PASS AT 1513 " " WS0-LEV.
012310	     ACCEPT W25-WHOLE AT 1413
		    WITH FOREGROUND-COLOR Brown HIGHLIGHT NO-ECHO AUTO.
             MOVE W12-T-YMD      TO SEC-INITIAL.
      *    *************************************************************
      *    ****      O V E R R I D E   P A S S W O R D
      * 	   F O R   R E G I S T E R   S Y S T E M
      * 	       Y O U R	 E Y E S   O N L Y
      * 	  ----- NOT TO BE GIVEN TO ANYONE -----
      *    *************************************************************
012330	   IF W25-WHOLE NOT = 232323
012340	       IF W25-WHOLE NOT = SEC-PASS
		   GO TO AH15.

012390 AH10.
	     MOVE 3		 TO WS-PARKEY.
006950	     READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980		   GO TO AH10.
	     MOVE SEC-NUM	 TO PAR-SERIAL WS-SERIAL.
	     MOVE SEC-LEV	 TO PAR-LEV    WS-LEV.
007540	     REWRITE PAR-RECORD3.
	     MOVE ZERO		 TO SEC-COUNT
				    SEC-DEM.
	     MOVE WS-TERMS	 TO SEC-TERMS.
012890       REWRITE SEC-REC2.
012900       UNLOCK SECUR.
012910     IF WS-STATUS NOT = "00"
012920	       MOVE "Disk error - AH10"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012930         STOP RUN.

       AH12.
             MOVE 1              TO WS-PARKEY.
006950       READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980             GO TO AH12.
             MOVE ZERO           TO PAR-STAT.
             REWRITE PAR-RECORD1.
             GO TO AH999.

012960 AH15.
012970	     DISPLAY "Invalid password" AT 1604.
012990	     DISPLAY "Press any key to continue" AT 1804.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     MOVE WS-XXX	 TO SEC-NUM.
             GO TO AH999.

       AH20.
             DISPLAY "Registration No:" AT 0704.
             ACCEPT SEC-NUM AT 0721
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
	   IF SEC-POS > ZERO
	       DISPLAY S25.
	     PERFORM CHECK-CORRECT.
006160	   IF WS-OPTION = "N"
               GO TO AH20.

       AH25.
             EXIT.

013190 AH999.
013200       EXIT.
      /    *************************************************************
      *    *****	      S T A R T   S Y S T E M		   *****
      *    *************************************************************
014530 AI000                   SECTION 55.
       AI0.
014760	     MOVE 2		 TO WS-SECKEY.

       AI0-READ.
014780	     READ SECUR.
014790     IF RUNTIME-ERROR
014800         IF RECORD-LOCKED
014810		   GO TO AI0-READ.
014820     IF WS-STATUS NOT = "00"
014830	       MOVE "Disk error - AI00 (SYSTEM)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014840         STOP RUN.

014540 AI00.
014550	     MOVE 1		 TO WS-ERROR.
014560     IF (WS0-DTP NOT = 2) AND (WS0-STP NOT = 2) AND
014570        (WS0-CRP NOT = 2) AND (WS0-GLP NOT = 2) AND
014580        (WS0-HPD NOT = 2) AND (WS0-POS NOT = 2) AND
014590        (WS0-JCP NOT = 2) AND (WS0-VHP NOT = 2)
014600	       MOVE "Files not yet initialized"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014640         GO TO AI999.
	   IF SEC-AGREE = "R" AND
	      SEC-DAYS = ZERO
	       MOVE "Contract must be renewed!"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014640         GO TO AI999.
014650	   IF WS0-POS = 2
014660	       MOVE "Y" 	 TO WS-CSS.

       AI02.
	     MOVE 3		 TO WS-PARKEY.
006950	     READ PARAM.
006960	   IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980		   GO TO AI02.
	     MOVE PAR-CUR-YR	 TO WS-CUR-YR.
	     MOVE PAR-CUR-CEN	 TO WS-CUR-CEN.
	     MOVE PAR-PRV-CEN	 TO WS-PRV-CEN.
	     MOVE PAR-NXT-CEN	 TO WS-NXT-CEN.
014670	     MOVE 1		 TO WS-PARKEY.

014680 AI05.
014690       READ PARAM.
014700	   IF RUNTIME-ERROR
014710         IF RECORD-LOCKED
014720             GO TO AI05.
014730     IF WS-STATUS NOT = "00"
014740	       MOVE "Disk error - AI00 (PARAM)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014750         STOP RUN.

014770 AI10.
014850	   IF (PAR-COMPANY NOT = SEC-COMP) OR
	      (WS-SERIAL NOT = SEC-NUM) OR
	      (WS-LEV NOT = SEC-LEV)
014860	       MOVE "Contact your supplier"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014870         STOP RUN.
      *    *************************************************************
      *    ****   U S E D   T O   C H E C K   F O R   C L E A R
      * 	  O N	D E M O N S T R A T I O N   S Y S T E M
      *    *************************************************************
	     MOVE SEC-DATE	 TO WS-DTE.

014920 AI25.
014930       DISPLAY CLR-SCREEN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 18		 TO SHADE-COL.
	     MOVE 48		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
	     DISPLAY S18.
	     PERFORM SCREEN-SHADOW.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  DATE returns a six digit result as YYMMDD. W25-FACT is   Ё
      *    Ё  defined as YYMMDD. These details are moved to W12-TODAY  Ё
      *    Ё  and W20-DATE (DDMMCCYY). The current century is inserted Ё
      *    Ё  The system then checks the year and the current year (as Ё
      *    Ё  recorded in the data base) and if the year is less by    Ё
      *    Ё  more than 70, the next century will be inserted. If the  Ё
      *    Ё  year is greater by more than 70, the previous century    Ё
      *    Ё  will be inserted. 				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *      ACCEPT W12-T-YMD FROM DATE.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W20-YEAR  W12-YEAR.
	     MOVE W25-MM	 TO W20-MONTH W12-MONTH.
	     MOVE W25-DD	 TO W20-DAY   W12-DAY.
	     MOVE WS-CUR-CEN	 TO W20-CENT  W12-CENT.
	   IF W25-YY < (WS-CUR-YR - 70)
	       MOVE WS-NXT-CEN	 TO W20-CENT W12-CENT
	   ELSE
	   IF W25-YY > (WS-CUR-YR + 70)
	       MOVE WS-PRV-CEN	 TO W20-CENT W12-CENT.

014970 AI30.
014980       ACCEPT S20.
006180	     PERFORM CLEAR-L50.
014990       PERFORM AB000-DATE.
015000     IF WS-ERROR = 1
015010	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015020         GO TO AI30.
015030	     MOVE W20-DAY	 TO W12-DAY W12-DD.
015040	     MOVE W20-MONTH	 TO W12-MONTH W12-MM.
015050	     MOVE W20-YEAR	 TO W12-YEAR W12-YY.
	     MOVE W20-CENT	 TO W12-CENT W12-CC.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY SEC-DATE.
015060	     DISPLAY WS-BLNK78 AT 2102 WITH FOREGROUND-COLOR Cyan.
015060	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
015070	     DISPLAY "Todays date - " AT 2129 W12-DD " "
015080		      W30-MONTH (W12-MM) "," W12-CC W12-YY.
015090	     DISPLAY "Is date correct (Y/N) ?" AT 2329
		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
	     MOVE "Y"		 TO WS-OPTION.

       AI35.
015100	     ACCEPT WS-OPTION AT 2353 WITH FOREGROUND-COLOR Grey HIGHLIGHT
                                          UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO AI35.
015160	   IF WS-OPTION = "N"
015170         GO TO AI30.
015180	     MOVE W12-TODAY	 TO PAR-DMY.
015190	     MOVE W12-T-YMD	 TO PAR-YMD.
015200       REWRITE PAR-RECORD1.
	   IF SEC-AGREE = "R"
	       IF NOT (W12-T-YMD < SEC-EXPDATE)
		   MOVE ZERO	 TO SEC-DAYS
	       ELSE
		   COMPUTE W31-DTE1 = W12-YY * 365.25
		   ADD W31-NO (W12-MM)
				 TO W31-DTE1
		   ADD W12-DD	 TO W31-DTE1
		   MOVE SEC-EXPDATE
				 TO W20-DTE
		   COMPUTE W31-DTE2 = W20-YY * 365.25
		   ADD W31-NO (W20-MM)
				 TO W31-DTE2
		   ADD W20-DD	 TO W31-DTE2
		   SUBTRACT W31-DTE2 FROM W31-DTE1
				 GIVING W25-DAYS
		   IF W25-DAYS < 46
		       MOVE W25-DAYS
				 TO SEC-DAYS.
	     MOVE SEC-DEM	 TO WS-DEM.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   T E S T   F O R   D E M O N S T R A T I O N	       Ё
      *    Ё	   S Y S T E M					       Ё
      *    Ё							       Ё
      *    Ё	   Is the run date still the same ?		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF SEC-DATE = WS-DTE
      *    *************************************************************
      *    ****   M U S T   I N D I C A T O R	B E   R E S E T   ?
      *    *************************************************************
	       IF WS-DEM > 2
		   MOVE 2	 TO WS-DEM SEC-DEM
	       END-IF
	   ELSE
      *    *************************************************************
      *    ****   M U S T   I N D I C A T O R	B E   S E T    ?
      *    *************************************************************
	   IF WS-DEM = 2
	       MOVE ZERO	 TO W31-DTE1
	       ADD W31-NO (W12-MM)
				 TO W31-DTE1
	       ADD W12-DD	 TO W31-DTE1
	       DIVIDE W31-DTE1 BY 13 GIVING W31-DTE2
				 REMAINDER WS-C1
	       IF WS-C1 = 3 OR 6 OR 9 OR 12
		   DIVIDE 3	 INTO WS-C1
		   ADD 2 WS-C1	 GIVING WS-DEM
		   MOVE WS-DEM	 TO SEC-DEM.
008570	     REWRITE SEC-REC2.
	   IF W12-YY = WS-CUR-YR
	       GO TO AI999.

012390 AI40.
	     MOVE 3		 TO WS-PARKEY.
006950	     READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980		   GO TO AI40.
	     MOVE W12-YY	 TO PAR-CUR-YR.
	   IF W12-YY < (WS-CUR-YR - 70)
	       ADD 1		 TO PAR-CUR-CEN
				    PAR-PRV-CEN
				    PAR-NXT-CEN
	   ELSE
	   IF W12-YY > (WS-CUR-YR + 70)
	       SUBTRACT 1	 FROM PAR-CUR-CEN
				      PAR-PRV-CEN
				      PAR-NXT-CEN.
002380	     REWRITE PAR-RECORD3.

015210 AI999.
015220       EXIT.
      /    *************************************************************
      *    *****	       RECOVERY INITIALIZE		   *****
      *    *************************************************************
020050 AK000                   SECTION 57.
020060 AK00.
022090       CLOSE PARAM.
022110	     CLOSE SECUR.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     OPEN I-O APACFIDS.
020070	   IF PAR-PROG (WS-S1) = "DB" OR "DS"
	       MOVE "UTP\LEDREB" TO WS-NEXT-PRG
020250	       CALL WS-NEXT-PRG
		  USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
	       CANCEL WS-NEXT-PRG
	       MOVE "UTP\TAXREB" TO WS-NEXT-PRG
020250	       CALL WS-NEXT-PRG
		  USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
	       CANCEL WS-NEXT-PRG
020080	       DISPLAY "Loading DEBTOR recovery" AT 1212
	       MOVE "UTP\DTPREC" TO WS-NEXT-PRG
020100     ELSE
020110     IF PAR-PROG (WS-S1) = "CR" OR "CS"
020120	       DISPLAY "Loading CREDITOR recovery" AT 1212
	       MOVE "UTP\CRPREC" TO WS-NEXT-PRG
020140	   ELSE
020150	   IF PAR-PROG (WS-S1) = "JC"
020160	       DISPLAY "Loading JOB COSTING recovery" AT 1212
020170	       MOVE "UTP\JCPREC" TO WS-NEXT-PRG
020180     ELSE
020190     IF PAR-PROG (WS-S1) = "ST"
020200	       DISPLAY "Loading STOCK recovery" AT 1212
	       MOVE "UTP\STPREC" TO WS-NEXT-PRG
020220     ELSE
020230     IF PAR-PROG (WS-S1) = "GL" OR "CG"
020240	       DISPLAY "Loading G/LEDGER recovery" AT 1212
	       MOVE "UTP\GLPREC" TO WS-NEXT-PRG.
020250	     CALL WS-NEXT-PRG
		  USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY.
	     CANCEL WS-NEXT-PRG.
	     CLOSE APACFIDS.

       AK999.
	     EXIT.
      /    *************************************************************
      *    *****	     A P A C   MARKETING SCREEN		    ****
      *    *************************************************************
020260 AL000	     SECTION 58.
020270 AL00.
020280       ACCEPT WS-START FROM TIME.
020290       MOVE 223                TO W40-DEC.
020300       MOVE 1                  TO WS-S1.

020310 AL05.
020320       MOVE W40-CHAR           TO WS-GCHAR(WS-S1).
020330     IF WS-S1 < 20
020340         ADD 1                 TO WS-S1
020350         GO TO AL05.
014170       MOVE 2                  TO WS-S1.

014180 AL05A.
014190       MOVE WS-G1              TO WS-TCHAR(WS-S1) WS-BCHAR(WS-S1).
             MOVE WS-G8              TO WS-TCH(WS-S1)   WS-BCH(WS-S1).
014200     IF WS-S1 < 79
014210         ADD 1                 TO WS-S1
014220         GO TO AL05A.
             MOVE WS-G9	             TO WS-TCH(1).
             MOVE WS-G10             TO WS-TCH(80).
             MOVE WS-G11             TO WS-BCH(1).
             MOVE WS-G12             TO WS-BCH(80).
014230       MOVE WS-G15             TO WS-TCHAR(1).
014240       MOVE WS-G16             TO WS-TCHAR(80).
014250       MOVE WS-G17             TO WS-BCHAR(1).
014260       MOVE WS-G18             TO WS-BCHAR(80).
           IF NOT(LS-INSTR = "X")
020360         DISPLAY APAC.
021870         MOVE ALL "╟"          TO WS-MID-LINE.
             MOVE WS-G3              TO WS-MCHR(1) WS-MCHR(80).
020510       MOVE ZERO               TO WS-S1 WS-S2.
             MOVE L05-COMMAND        TO W05-COMMAND.
020540       MOVE 1                  TO WS-S1 WS-S2.
020550       MOVE ZERO               TO W05-DRAW.

020560 AL10.
020570     IF W05-CHAR(WS-S1) = "*"
020580         GO TO AL15.
020590     IF WS-S1 < 117
020600         ADD 1                 TO WS-S1
020610         GO TO AL10.
020620       GO TO AL30.

020630 AL15.
020640       ADD 2                   TO WS-S1.

020650 AL20.
020660     IF W05-CHAR (WS-S1) = "," OR " "
020670         GO TO AL25.
020680       MOVE W05-CHAR(WS-S1)    TO W05-DC(WS-S2).
           IF WS-S2 < 2
               ADD 1                 TO WS-S2
           ELSE
020690         GO TO AL30.

020700 AL25.
020710     IF WS-S1 < 128
020720         ADD 1                 TO WS-S1
020730         GO TO AL20.

020740 AL30.
020750     IF W05-DRW NOT NUMERIC
020760         MOVE 0                TO W05-DRAW
020770     ELSE
020780     IF W05-DRAW > 39
020790         MOVE 40               TO W05-DRAW.
020800       MOVE ZERO               TO WS-S1.

020810 AL35.
020820       CALL X"D9" USING WS-CONSOLE.
020830     IF WS-CONSOLE NOT = ZERO
020840         ACCEPT WS-OPTION WITH AUTO
020850         GO TO AL40.
020860       ADD 1                   TO WS-S1 WS-S2.
020870       ACCEPT WS-END FROM TIME.
020880       SUBTRACT WS-START       FROM WS-END.
           IF LS-INSTR = "X"
               GO TO AL999.
020890     IF WS-END < 400
020900         GO TO AL35.

020910 AL40.
020920       DISPLAY SPACE AT 0101.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё D O   N O T   D I S P L A Y   E R R O R   M E S S A G E S Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

020930 AL999.
020940     EXIT.
      /    *************************************************************
      *    *****		CLEAR THE USER LOG		   *****
      *    *************************************************************
020950 AM000                   SECTION 59.
020960 AM00.
020970       DISPLAY CLR-SCREEN.
020980     IF WS-INIT NOT = 2
020990         GO TO AM999.
021000	     DISPLAY "Clear the Work Station Log" AT 0329
021010		      WITH FOREGROUND-COLOR Grey HIGHLIGHT .
021020	     MOVE SPACES	 TO W40-CSHPASS.
021030       DISPLAY S06.
021040	     DISPLAY "Do you wish to continue Y/N > <" AT 5012
021050		      WITH BACKGROUND-COLOR Cyan
			   FOREGROUND-COLOR Brown HIGHLIGHT.

021060 AM02.
021070	     ACCEPT WS-OPTION AT 5041
		    WITH BACKGROUND-COLOR Cyan
			 FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
021130	   IF NOT (WS-OPTION = "N" OR "Y")
021140         GO TO AM02.
021150	   IF WS-OPTION = "N"
021160         GO TO AM999.

021170 AM05.
021180       ACCEPT S07.
021190	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
021250	     MOVE 1		 TO WS-LENGTH.
021260	     MOVE 5		 TO WS-PARKEY.

021270 AM06.
021280       READ PARAM.
021290     IF RUNTIME-ERROR
021300         IF RECORD-LOCKED
021310             GO TO AM06.
021320     IF WS-STATUS NOT = "00"
021330	       MOVE "Disk error - AM05 (PARAM)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021340         STOP RUN.
	     MOVE 1		 TO WS-S1.

       AM07.
	     MOVE PAR-SUPER (WS-S1)
				 TO W85-SUPER (WS-S1).
           IF WS-S1 < 9
	       ADD 1		 TO WS-S1
               GO TO AM07.
021360     IF NOT (W40-CSHPASS = W85-SUPER (1) OR
021370                           W85-SUPER (2) OR
021380                           W85-SUPER (3) OR
021390                           W85-SUPER (4) OR
021400                           W85-SUPER (5) OR
021410                           W85-SUPER (6) OR
021420                           W85-SUPER (7) OR
021430                           W85-SUPER (8) OR
021440                           W85-SUPER (9) OR
021450                           "!@#$%^")
021460	       DISPLAY "Invalid password - press any key" AT 1412
021470			WITH FOREGROUND-COLOR Brown HIGHLIGHT
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
021490	       GO TO AM999.
021500	     MOVE 4		 TO WS-PARKEY.

021510 AM08.
021520       READ PARAM LOCK.
021530     IF RUNTIME-ERROR
021540         IF RECORD-LOCKED
021550             GO TO AM08.
021560	     MOVE 1		 TO WS-S1.
021570	     MOVE 0		 TO WS-S2.
021580     IF PAR-USERS = 0
021590         GO TO AM20.
021610	     DISPLAY "Recoveries required for W/St" AT 1212.
021620	     MOVE 1241		 TO CPOS.

021630 AM10.
021640     IF PAR-USR (WS-S1) = WS-USER
021650	       ADD 1		 TO PAR-USERS
021660         GO TO AM15.
021670     IF PAR-USR (WS-S1) = SPACES
021680         GO TO AM15.
021690     IF PAR-PROG (WS-S1) NOT = SPACES
021700	       ADD 1		 TO PAR-USERS
021710	       DISPLAY PAR-USR (WS-S1) AT CPOS
		       WITH FOREGROUND-COLOR Brown HIGHLIGHT
021720	       ADD 4		 TO CCOL
021730     ELSE
021740	       MOVE SPACES	 TO PAR-USR (WS-S1).

021750 AM15.
021760     IF WS-S1 < 24
021770	       ADD 1		 TO WS-S1
021780         GO TO AM10.

       AM18.
021850       DISPLAY "Recoveries must be done before continuing - Press
021860- 	     "ENTER" AT 2312
		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
021880	     ACCEPT WS-OPTION AT 2368
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
021890	     MOVE SPACES	 TO PAR-USR (WS-USUB).
021900	     SUBTRACT 1 	 FROM PAR-USERS.
021910	     MOVE ZERO		 TO PAR-LOGON.
             GO TO AM25.

021790 AM20.
021810	     DISPLAY "W/St Log cleared - Press ENTER" AT 2312
021820		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
021830	     ACCEPT WS-OPTION AT 2344
		    WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.

002192 AM25.
021930       REWRITE PAR-RECORD4.
021940     IF PAR-LOGON = ZERO
021950         STOP RUN.

021960 AM999.
021970       EXIT.
      /    *************************************************************
      *    *****	    AMEND NUMBER OF W/STATIONS		   *****
      *    *************************************************************
011610 AN000		       SECTION 60.
011620 AN00.
011630       DISPLAY CLR-SCREEN.
011640	     DISPLAY S17B.
014560     IF (WS0-DTP NOT = 2) AND (WS0-STP NOT = 2) AND
014570        (WS0-CRP NOT = 2) AND (WS0-GLP NOT = 2) AND
014580        (WS0-HPD NOT = 2) AND (WS0-POS NOT = 2) AND
014590        (WS0-JCP NOT = 2) AND (WS0-VHP NOT = 2)
014600	       MOVE "Files not yet initialized"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014640	       GO TO AN999.
011790	     MOVE 2		 TO WS-SECKEY.

011800 AN02.
011810       READ SECUR LOCK.
011820     IF RUNTIME-ERROR
011830         IF RECORD-LOCKED
011840		   GO TO AN02.
011850     IF WS-STATUS NOT = "00"
011860	       MOVE "Disk error - AN00"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011870         STOP RUN.
	   IF SEC-DEM > ZERO
014600	       MOVE "System is not registered"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014640	       GO TO AN999.
	   IF WS0-LEV < 2
	       MOVE 2		 TO WS-S2
	   ELSE
	   IF WS0-LEV < 3
	       MOVE 4		 TO WS-S2
	   ELSE
	   IF WS0-LEV < 4
	       MOVE 10		 TO WS-S2
	   ELSE
	       MOVE 100		 TO WS-S2.
	   IF NOT (SEC-TERMS < WS-S2)
	       MOVE "Maximum W/Stations already installed - ENTER"
				 TO WS-ERR-MES
	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO AN999.
011650	     MOVE "N"		 TO WS-OPTION.

011660 AN03.
011670	     ACCEPT WS-OPTION AT 0524 WITH FOREGROUND-COLOR Grey HIGHLIGHT
011680                               UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
011740	   IF NOT (WS-OPTION = "N" OR "Y")
011750	       GO TO AN03.
011760	   IF WS-OPTION = "N"
011770	       GO TO AN999.
011780	     DISPLAY "Key in date (DDMMCCYY)" AT 0504
		     WITH FOREGROUND-COLOR Brown HIGHLIGHT.

011880 AN05.
011890       ACCEPT S10.
011900	     MOVE SEC-DATE	 TO W20-DTE.
011910	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
011920       PERFORM AB000-DATE.
011930     IF WS-ERROR = 1
011940	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011950	       GO TO AN05.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 29		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддддддддд©" AT 1124
		      WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
	     DISPLAY "Ё Number of Work Stations     Ё" AT 1224
		      WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
	     DISPLAY "юддддддддддддддддддддддддддддды" AT 1324
		      WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
	     PERFORM SCREEN-SHADOW.
	     MOVE SEC-TERMS	 TO WS-TERMS.

       AN08.
	     ACCEPT S30.
	   IF WS-TERMS > WS-S2
	       MOVE WS-S2	 TO WS-TERMS.
	   IF WS0-LEV = 0
	       MOVE "JUNIOR"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 1
	       MOVE "ENTRY"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 2
	       MOVE "BASIC"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 3
	       MOVE "STANDARD"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 4
	       MOVE "PREMIUM"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 5
	       MOVE "G O L D"	 TO WS-PACK
	   ELSE
	       MOVE "S.MANAG."	 TO WS-PACK.
	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY S25.
012080       DIVIDE SEC-DATE BY 2 GIVING W25-RESULT.
012090       ADD 13              TO W25-RESULT.
012100       MULTIPLY 2.136      BY W25-RESULT.
012140       ADD SEC-PASS        TO W25-RESULT.
012150       ACCEPT W25-TIME     FROM TIME.
012160	     ADD W25-TIME	 TO W25-RESULT.
012170	     DISPLAY W25-RESULT AT 1204.
012180       DIVIDE 1.97         INTO W25-RESULT.
012190       MOVE W25-WHOLE      TO W25-PASS.
012200       MOVE W25-PASS1      TO W35-PASS2.
012210       MOVE W25-PASS2      TO W35-PASS1.
012220       MOVE W35-PNUM       TO W25-RESULT.
012230     IF W25-RESULT < 100000
012240         ADD 701010        TO W25-RESULT
012250     ELSE
012260         ADD 77            TO W25-RESULT.
012270       ADD 10112           TO W25-RESULT.
	     ADD WS-TERMS	 TO W25-RESULT.
	     ADD WS0-LEV	 TO W25-RESULT.
012280       MOVE W25-WHOLE      TO SEC-PASS.
012290	     DISPLAY "Password" AT 1404.
	     DISPLAY S25.
012300       MOVE ZERO           TO W25-WHOLE.
012310	     ACCEPT W25-WHOLE AT 1413
		    WITH FOREGROUND-COLOR Brown HIGHLIGHT NO-ECHO AUTO.
             MOVE W12-T-YMD      TO SEC-INITIAL.
      *    *************************************************************
      *    ****     O V E R R I D E   P A S S W O R D
      * 	   F O R   A D D   F O U R   U S E R S
      * 	       Y O U R	 E Y E S   O N L Y
      * 	  ----- NOT TO BE GIVEN TO ANYONE -----
      *    *************************************************************
	   IF W25-WHOLE NOT = SEC-PASS
		IF W25-WHOLE NOT = 112233
		   GO TO AN15.

012390 AN10.
	     MOVE 3		 TO WS-PARKEY.
006950	     READ PARAM.
006960     IF RUNTIME-ERROR
006970         IF RECORD-LOCKED
006980		   GO TO AN10.
	     MOVE SEC-NUM	 TO PAR-SERIAL WS-SERIAL.
	     MOVE SEC-LEV	 TO PAR-LEV    WS-LEV.
007540	     REWRITE PAR-RECORD3.
	     MOVE WS-TERMS	  TO SEC-TERMS.
012890       REWRITE SEC-REC2.
012900       UNLOCK SECUR.
012910     IF WS-STATUS NOT = "00"
012920	       MOVE "Disk error - AN10"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012930         STOP RUN.
	     GO TO AN999.

012960 AN15.
012970	     DISPLAY "Invalid password" AT 1604.
012990	     DISPLAY "Press any key to continue" AT 1804.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

013190 AN999.
013200       EXIT.
      /    *************************************************************
      *    *****	    R E N E W	C O N T R A C T		   *****
      *    *************************************************************
011610 AO000		       SECTION 61.
011620 AO00.
011630       DISPLAY CLR-SCREEN.
011640	     DISPLAY S17C.
011790	     MOVE 2		 TO WS-SECKEY.

011800 AO02.
011810       READ SECUR LOCK.
011820     IF RUNTIME-ERROR
011830         IF RECORD-LOCKED
011840		   GO TO AO02.
011850     IF WS-STATUS NOT = "00"
011860	       MOVE "Disk error - AO00"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011870         STOP RUN.
011650	     MOVE "N"		 TO WS-OPTION.

011660 AO03.
011670	     ACCEPT WS-OPTION AT 0525 WITH FOREGROUND-COLOR Grey HIGHLIGHT
011680                               UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
011740	   IF NOT (WS-OPTION = "N" OR "Y")
011750	       GO TO AO03.
011760	   IF WS-OPTION = "N"
011770	       GO TO AO999.
011910	     DISPLAY WS-BLNK78 AT 0502 WITH FOREGROUND-COLOR Cyan.
011780	     DISPLAY "Key in date (DDMMCCYY)" AT 0504
		      WITH FOREGROUND-COLOR Brown HIGHLIGHT.

011880 AO05.
011890       ACCEPT S10.
011900	     MOVE SEC-DATE	 TO W20-DTE.
011910	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
011920       PERFORM AB000-DATE.
011930     IF WS-ERROR = 1
011940	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011950	       GO TO AO05.
	   IF WS0-LEV = 0
	       MOVE "JUNIOR"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 1
	       MOVE "ENTRY"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 2
	       MOVE "BASIC"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 3
	       MOVE "STANDARD"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 4
	       MOVE "PREMIUM"	 TO WS-PACK
	   ELSE
	   IF WS0-LEV = 5
	       MOVE "G O L D"	 TO WS-PACK
	   ELSE
	       MOVE "S.MANAG."	 TO WS-PACK.
      *	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY S25.
012080       DIVIDE SEC-DATE BY 2 GIVING W25-RESULT.
012090       ADD 13              TO W25-RESULT.
012100       MULTIPLY 2.136      BY W25-RESULT.
012140       ADD SEC-PASS        TO W25-RESULT.
012150       ACCEPT W25-TIME     FROM TIME.
012160	     ADD W25-TIME	 TO W25-RESULT.
012170	     DISPLAY W25-RESULT AT 1204.
012180       DIVIDE 1.97         INTO W25-RESULT.
012190       MOVE W25-WHOLE      TO W25-PASS.
012200       MOVE W25-PASS1      TO W35-PASS2.
012210       MOVE W25-PASS2      TO W35-PASS1.
012220       MOVE W35-PNUM       TO W25-RESULT.
012230     IF W25-RESULT < 100000
012240         ADD 701010        TO W25-RESULT
012250     ELSE
012260         ADD 77            TO W25-RESULT.
012270       ADD 10112           TO W25-RESULT.
	     ADD WS-TERMS	 TO W25-RESULT.
	     ADD WS0-LEV	 TO W25-RESULT.
012280       MOVE W25-WHOLE      TO SEC-PASS.
012290	     DISPLAY "Password" AT 1404.
	     DISPLAY S25.
012300       MOVE ZERO           TO W25-WHOLE.
012310	     ACCEPT W25-WHOLE AT 1413
		    WITH FOREGROUND-COLOR Brown HIGHLIGHT NO-ECHO AUTO.
      *    *************************************************************
      *    ****     O V E R R I D E   P A S S W O R D
      * 	   F O R   R E N E W   C O N T R A C T
      * 	       Y O U R	 E Y E S   O N L Y
      * 	  ----- NOT TO BE GIVEN TO ANYONE -----
      *    *************************************************************
	   IF W25-WHOLE NOT = SEC-PASS
		IF W25-WHOLE NOT = 112233
		   GO TO AO15.

012390 AO10.
	     MOVE SEC-ANDATE	 TO W20-DTE.
	     ADD 1		 TO W20-YY.
	     MOVE W20-DTE	 TO SEC-ANDATE.
	     MOVE SEC-EXPDATE	 TO W20-DTE.
	     ADD 1		 TO W20-YY.
	     MOVE W20-DTE	 TO SEC-EXPDATE.
	     MOVE 255		 TO SEC-DAYS
012890	     REWRITE SEC-REC2.
012900       UNLOCK SECUR.
012910     IF WS-STATUS NOT = "00"
012920	       MOVE "Disk error - AO10"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012930         STOP RUN.
	     GO TO AO999.

012960 AO15.
012970	     DISPLAY "Invalid password" AT 1604.
012990	     DISPLAY "Press any key to continue" AT 1804.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AO999.
	     EXIT.

       COMPANY-LOOKUP  SECTION 70.
012520 COMPANY-LUP-SCREEN.
011050	     PERFORM SAVE-SCREEN.
	     DISPLAY S-COM.

       COMPANY-LUP-CLR-SCREEN.
	     MOVE 0928		 TO CPOS.
	     PERFORM COMPANY-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 30		 TO SHADE-COL.
	     MOVE 46		 TO SHADE-WIDTH.
	     MOVE 16		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
000340	     PERFORM CLEAR-L50.
000350	     MOVE 9		 TO CLIN.
000360	     MOVE 1		 TO W75-S W75-S1.

       COMPANY-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-COM (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO COMPANY-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.
	     MOVE "  !" 	 TO COM-CODE.
006850	     START APCCOM
	       KEY >= COM-CODE.
006860	   IF NOT (WS-STATUS = "00")
	       GO TO COMPANY-NO-RECORD.

       COMPANY-GET-COM-RECORD.
	   IF WS-OPTION = "J"
	       MOVE SPACE	 TO WS-OPTION
	       GO TO COMPANY-SKIP-COM-READ.
006850	     READ APCCOM NEXT	 WITH IGNORE LOCK.
009900	   IF NOT (WS-STATUS = "00")
	       SUBTRACT 1	 FROM W75-S
	       GO TO COMPANY-FULL-SCREEN.

       COMPANY-SKIP-COM-READ.
	     MOVE COM-CODE	 TO W75-COM (W75-S).
	     DISPLAY SD-COM AT LINE CLIN.
027700	   IF CLIN < 20
027710         ADD 1 TO CLIN W75-S
027720	       GO TO COMPANY-GET-COM-RECORD.

       COMPANY-FULL-SCREEN.
001020	     DISPLAY " " AT 5003 WITH FOREGROUND-COLOR Brown HIGHLIGHT
					BACKGROUND-COLOR Cyan
001030		     " highlight - " WITH FOREGROUND-COLOR Blue
					  BACKGROUND-COLOR Cyan
		     "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
				 BACKGROUND-COLOR Cyan
		     "/" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
		     "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
				 BACKGROUND-COLOR Cyan
		     " - " WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
001170		     "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
				BACKGROUND-COLOR Cyan
		     "ape" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.

       COMPANY-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
	     MOVE 0930		 TO CPOS.

       COMPANY-CRT-ATTRIB.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       COMPANY-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM CLEAR-L50.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO COMPANY-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO COMPANY-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO COMPANY-RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO COMPANY-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO COMPANY-RESTORE-SCREEN
		 WHEN PAGE-DOWN
001340		     GO TO COMPANY-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO COMPANY-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO COMPANY-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO COMPANY-GET-USER-OPT.

001240 COMPANY-GET-NEXT-PAGE.
	   IF W75-S < 12
	       GO TO COMPANY-GET-USER-OPT.

       COMPANY-NEW-PAGE.
	     MOVE 0929		 TO CPOS.
	     PERFORM COMPANY-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 9		 TO CLIN.
	     MOVE LOW-VALUES	 TO W75-KEYS.
	     MOVE 1		 TO W75-S W75-S1.
	     PERFORM COMPANY-LUP-TAB-LOOP.
027830	     GO TO COMPANY-GET-COM-RECORD.

       COMPANY-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		SET OPTION TO BYPASS THE READ NEXT	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE "J"		 TO WS-OPTION.

       COMPANY-GET-PREV-LOOP.
006850	     READ APCCOM PREVIOUS
				 WITH IGNORE LOCK.
009900	   IF NOT (WS-STATUS = "00")
	       MOVE SPACE	 TO WS-OPTION
	       GO TO COMPANY-NEW-PAGE.
	   IF WS-IXS1 < 24
               ADD 1             TO WS-IXS1
	       GO TO COMPANY-GET-PREV-LOOP.
	     GO TO COMPANY-NEW-PAGE.

027970 COMPANY-NO-RECORD.
027730	     MOVE "No company codes"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO COMPANY-RESTORE-SCREEN.

       COMPANY-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
	     GO TO COMPANY-HIGHLIGHT.

       COMPANY-CHK-NO-KEYS.
	   IF W75-S1 = ZERO
	       MOVE 1		 TO W75-S1
	       GO TO COMPANY-HIGHLIGHT.
           IF W75-S1 < W75-S
	       ADD 1		 TO W75-S1 CLIN.

       COMPANY-HIGHLIGHT.
	     PERFORM COMPANY-CRT-ATTRIB.
	     DISPLAY W43-DET3 (CLIN) AT CPOS
		     WITH BACKGROUND-COLOR Magenta
			  FOREGROUND-COLOR Grey.
	     GO TO COMPANY-GET-USER-OPT.

       COMPANY-CRT-EDGE.
	     DISPLAY "Ё" AT CPOS
		      WITH FOREGROUND-COLOR Grey HIGHLIGHT
			   BACKGROUND-COLOR Grey
012890		     "Ё   Ё                                        Ё"
		      WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta
		     "Ё" WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
019260	     ADD 1		 TO CLIN.

       COMPANY-RESTORE-SCREEN.
	   IF NOT (W75-S1 = ZERO)
	       MOVE W75-COM (W75-S1) TO WS-SYS-ID.
011320	     PERFORM RESTORE-SCREEN.

       COMPANY-LOOKUP-EXIT.
             EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Set the DEFAULT file PATHS as if  they are present on the Ё
      *    Ё same hard drive as  the  programs.	 This will allow for a Ё
      *    Ё single USER system  or  for  a  network system, where the Ё
      *    Ё programs are run from  the Network drive.	Where programs Ё
      *    Ё are run from a local  drive  on  network  use the program Ё
      *    Ё FILEUTIL to change the paths.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       APACFILES       SECTION 71.
       APAC-FILES-INIT.
	     MOVE WS-SYS-ID	 TO W02-SYSPTH W02-CHK-ID.
	     MOVE ZERO		 TO WS-ERROR.
	     OPEN I-O APACFILE.
	   IF NOT (WS-STATUS = "00")
	       MOVE W02-CHECK-SYS
				 TO W02-SYSTEM
	       OPEN I-O SECUR
	       IF NOT (WS-STATUS = "00")
		   MOVE 9	 TO WS-ERROR
		   GO TO APACFILES-EXIT
	       ELSE
		   CLOSE SECUR
	       END-IF
	       OPEN OUTPUT APACFILE
	       CLOSE APACFILE
	       PERFORM APAC-CREATE-NEW THRU APAC-CREATE-NEW-EXIT
	       GO TO APAC-FILES-INIT.
	     MOVE "SYSTEM"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       GO TO APAC-ERROR-MESSAGE.
	     MOVE SPACES	 TO W02-SYSTEM.
	     STRING APAC-PATH DELIMITED BY " "
		    APAC-NAME DELIMITED BY " "
		    INTO W02-SYSTEM.
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
	     MOVE "PARAM"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       GO TO APAC-ERROR-MESSAGE.
      *
      * 	Get the path used for the existing files.
      *
	     MOVE APAC-PATH	 TO W10-CURRENT-PATH.
	     MOVE SPACES	 TO W02-PARAM.
	     STRING APAC-PATH DELIMITED BY " "
		    APAC-NAME DELIMITED BY " "
		    INTO W02-PARAM.
      *
      * 	Get the path used for the WORK files.
      *
	     MOVE "CORDER"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	     MOVE APAC-PATH	 TO W10-WRK-PATH.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "SHARED"	 TO APAC-KEY
	       MOVE "SHARED.DAT" TO APAC-NAME
	       MOVE SPACES	 TO APAC-PATH
	       STRING WS-SYS-ID DELIMITED SIZE
		      "_DATA\" DELIMITED SIZE
		      INTO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё CHECK FOR THE PRESENCE OF NEW FILES THAT ARE ADDED TO THE Ё
      *    Ё SYSTEM AND UPDATE THE PATH FILE WITH MISSING FILE NAMES.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    ****   C R E D I T O R S   M E M O	W O R K   F I L E   ****
      * 	       Version 8.15.01
      *    *************************************************************
	     MOVE "MEMCRD"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT(WS-STATUS = "00")
	       MOVE "MEMCRD"	 TO APAC-KEY
	       MOVE "CRDM_XXX.WRK"
				 TO APAC-NAME
	       MOVE ZERO	 TO APAC-FILE
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****        I N V O I C E   B I T M A P   F I L E	    ****
      * 	       Version 8.14.03
      *    *************************************************************
	     MOVE "INVBMP"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "INVBMP"	 TO APAC-KEY
	       MOVE SPACES	 TO APAC-NAME
	       STRING WS-SYS-ID DELIMITED SIZE
		      "_INV.BMP" DELIMITED SIZE
		      INTO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****      S T A T E M E N T	 B I T M A P   F I L E	    ****
      * 	     Version 8.14.03
      *    *************************************************************
	     MOVE "STMBMP"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "STMBMP"	 TO APAC-KEY
	       MOVE SPACES	 TO APAC-NAME
	       STRING WS-SYS-ID DELIMITED SIZE
		      "_STM.BMP" DELIMITED SIZE
		      INTO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****    D E B T O R	 N A M E   I N D E X   F I L E	    ****
      * 	   Version 8.13.8
      *    *************************************************************
	     MOVE "DEBALT"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "DEBALT"	 TO APAC-KEY
	       MOVE "AR_NAME.DAT"
				 TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 8.14.03
      *    *************************************************************
	     MOVE "DEALER"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "DEALER"	 TO APAC-KEY
	       MOVE "DEAL_XXX.WRK"
				 TO APAC-NAME
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
	     MOVE "DEATOT"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "DEATOT"	 TO APAC-KEY
	       MOVE "DEAT_XXX.WRK"
				 TO APAC-NAME
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****    D E B T O R	 R E S T R I C T I O N	 F I L E    ****
      * 	   Version 8.13.4
      *    *************************************************************
	     MOVE "DEBDEP"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "DEBDEP"	 TO APAC-KEY
	       MOVE "AR_REST.DAT"
				 TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 7.08.0
      *    *************************************************************
	     MOVE "LEDTRG"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
		  KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "LEDTRG"	 TO APAC-KEY
	       MOVE "LTRG_XXX.DAT"
				 TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****   S T O C K   D E S C R I P T I O N   INDEX   FILE  ****
      * 	  Version 8.13.5
      *    *************************************************************
	     MOVE "STKALT"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "STKALT"	 TO APAC-KEY
	       MOVE "IN_DESC.DAT"
				 TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 7.11.02
      *    *************************************************************
	     MOVE "TRFOUT"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
		  KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "TRFOUT"	 TO APAC-KEY
	       MOVE "TRFOUT.DAT" TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 7.11.02
      *    *************************************************************
	     MOVE "TRFRIN"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
		  KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "TRFRIN"	 TO APAC-KEY
	       MOVE "TRFRIN.DAT" TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 8.07.01
      *    *************************************************************
	     MOVE "TERMINAL"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "TERMINAL"	 TO APAC-KEY
	       MOVE "TRMNAL.DAT" TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 7.04.09
      *    *************************************************************
	     MOVE "PICSLIP"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "PICSLIP"	 TO APAC-KEY
	       MOVE "PICSLP.DAT" TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 8.05.07
      *    *************************************************************
	     MOVE "DEBPRF"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "DEBPRF"	 TO APAC-KEY
	       MOVE "DEBPRF.DAT" TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 7.12.03
      *    *************************************************************
	     MOVE "RECEIPTS"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "RECEIPTS"	 TO APAC-KEY
	       MOVE "RECP_XXX.WRK"
				 TO APAC-NAME
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
	     MOVE "STXOLD"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "STXOLD"	 TO APAC-KEY
	       MOVE "IN_ADJ.OLD" TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
	     MOVE "ADJREF"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT (WS-STATUS = "00")
	       MOVE "ADJREF"	 TO APAC-KEY
	       MOVE "ADJR_XXX.WRK"
				 TO APAC-NAME
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
	     CLOSE APACFILE.
	     GO TO APACFILES-EXIT.

       APAC-ERROR-MESSAGE.
006260	     STRING "Missing " DELIMITED SIZE
		     APAC-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       APAC-CREATE-NEW.
	     OPEN I-O APACFILE.
	     MOVE "ASSETT"	 TO APAC-KEY.
	     MOVE "ASSETT.DAT"	 TO APAC-NAME.
	     MOVE 11		 TO APAC-FILE.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING WS-SYS-ID DELIMITED SIZE
		    "_DATA\" DELIMITED SIZE
		    INTO APAC-PATH.

       APAC-WRITE-APACFILE.
	     WRITE APAC-RECORD1.

       APAC-DATA-FILES.
	     MOVE "AUDITF"	 TO APAC-KEY.
	     MOVE "AUDIT.DAT"	 TO APAC-NAME.
	     MOVE 16		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "BARCOD"	 TO APAC-KEY.
	     MOVE "BARCODE.DAT"	 TO APAC-NAME.
	     MOVE 57		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PURDEX"	 TO APAC-KEY.
	     MOVE "CARDEXPR.DAT" TO APAC-NAME.
	     MOVE 56		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CARDEX"	 TO APAC-KEY.
	     MOVE "CARDEXSL.DAT" TO APAC-NAME.
	     MOVE 43		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CATITM"	 TO APAC-KEY.
	     MOVE "CATITM.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CATLOG"	 TO APAC-KEY.
	     MOVE "CATLOG.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHEQUE"	 TO APAC-KEY.
	     MOVE "CHEQUE.DAT"	 TO APAC-NAME.
	     MOVE 36		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQBATCH"	 TO APAC-KEY.
	     MOVE "CHQBCH.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQBCH"	 TO APAC-KEY.
	     MOVE "CHQBCH_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "NETWORK"	 TO APAC-KEY.
	     MOVE "CONTROL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRDMEM"	 TO APAC-KEY.
	     MOVE "AP_MEMO.DAT"	 TO APAC-NAME.
	     MOVE 6		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "CRDMEM.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AP_MEMO.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "CRDMEM.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AP_MEMO.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "CREDIT"	 TO APAC-KEY.
	     MOVE "AP_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 7		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "CREDIT.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AP_ACNT.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "CREDIT.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AP_ACNT.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "CRTRAN"	 TO APAC-KEY.
	     MOVE "AP_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 8		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "CRTRAN.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AP_TRAN.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "CRTRAN.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AP_TRAN.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "CSBOOK"	 TO APAC-KEY.
	     MOVE "CSBOOK.DAT"	 TO APAC-NAME.
	     MOVE 29		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CSTRAN"	 TO APAC-KEY.
	     MOVE "CSTRAN.DAT"	 TO APAC-NAME.
	     MOVE 30		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBFOTO"	 TO APAC-KEY.
	     MOVE "AR_CONT.DAT"	 TO APAC-NAME.
	     MOVE 3		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DBFOTO.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_CONT.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DBFOTO.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_CONT.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "DBTRAN"	 TO APAC-KEY.
	     MOVE "AR_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 4		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DBTRAN.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_TRAN.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DBTRAN.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_TRAN.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "DEBCOL"	 TO APAC-KEY.
	     MOVE "AR_COL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBCOL.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_COL.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBCOL.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_COL.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "DEBHIS"	 TO APAC-KEY.
	     MOVE "AR_HIST.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBHIS.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_HIST.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBHIS.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_HIST.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "DEBMEM"	 TO APAC-KEY.
	     MOVE "AR_MEMO.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBMEM.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_MEMO.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBMEM.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_MEMO.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "DEBORD"	 TO APAC-KEY.
	     MOVE "AR_ORD.DAT"	 TO APAC-NAME.
	     MOVE 53		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBORD.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_ORD.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBORD.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_ORD.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "DEBDEP"	 TO APAC-KEY.
	     MOVE "AR_REST.DAT"	 TO APAC-NAME.
	     MOVE 33		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTOR"	 TO APAC-KEY.
	     MOVE "AR_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 2		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBTOR.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_ACNT.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "DEBTOR.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "AR_ACNT.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "DEPART"	 TO APAC-KEY.
	     MOVE "DEPART.DAT"	 TO APAC-NAME.
	     MOVE 38		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "EGGSLE"	 TO APAC-KEY.
	     MOVE "EGGSLE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GACCNT"	 TO APAC-KEY.
	     MOVE "GL_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 12		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GACCNT.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_ACNT.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GACCNT.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_ACNT.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "GARTEE"	 TO APAC-KEY.
	     MOVE "GARTEE.DAT"	 TO APAC-NAME.
	     MOVE 9		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLBHDR"	 TO APAC-KEY.
	     MOVE "GL_BHDR.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GLBHDR.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_BHDR.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GLBHDR.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_BHDR.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "GLBTCH"	 TO APAC-KEY.
	     MOVE "GLBTCH_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLCHEQ"	 TO APAC-KEY.
	     MOVE "GLCHEQ_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLDEPO"	 TO APAC-KEY.
	     MOVE "GLDEPO_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLINST"	 TO APAC-KEY.
	     MOVE "GL_INST.DAT"	 TO APAC-NAME.
	     MOVE 15		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GLINST.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_INST.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GLINST.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_INST.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "GLTRAN"	 TO APAC-KEY.
	     MOVE "GL_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 13		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GLTRAN.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_TRAN.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GLTRAN.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_TRAN.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "GPDCHQ"	 TO APAC-KEY.
	     MOVE "GL_PDCHQ.DAT" TO APAC-NAME.
	     MOVE 45		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GPDCHQ.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_PDCHQ.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GPDCHQ.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "GL_PDCHQ.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "INVOIC"	 TO APAC-KEY.
	     MOVE "INVOICE.DAT"	 TO APAC-NAME.
	     MOVE 22		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "ITEMS"	 TO APAC-KEY.
	     MOVE "ITEMS.DAT"	 TO APAC-NAME.
	     MOVE 57		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JCHIST"	 TO APAC-KEY.
	     MOVE "JCHIST.DAT"	 TO APAC-NAME.
	     MOVE 58		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JCLINE"	 TO APAC-KEY.
	     MOVE "JCLINE.DAT"	 TO APAC-NAME.
	     MOVE 59		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBCST"	 TO APAC-KEY.
	     MOVE "JOBLIN.DAT"	 TO APAC-NAME.
	     MOVE 25		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBAUD"	 TO APAC-KEY.
	     MOVE "JOBAUD.DAT"	 TO APAC-NAME.
	     MOVE 24		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBHDR"	 TO APAC-KEY.
	     MOVE "JOBHDR.DAT"	 TO APAC-NAME.
	     MOVE 21		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBREQ"	 TO APAC-KEY.
	     MOVE "JOBREQ.DAT"	 TO APAC-NAME.
	     MOVE 26		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBRMK"	 TO APAC-KEY.
	     MOVE "JOBRMK.DAT"	 TO APAC-NAME.
	     MOVE 27		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBSTAT"	 TO APAC-KEY.
	     MOVE "JOBSTAT.DAT"	 TO APAC-NAME.
	     MOVE 60		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBTECH"	 TO APAC-KEY.
	     MOVE "JOBTECH.DAT"	 TO APAC-NAME.
	     MOVE 61		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBWAR"	 TO APAC-KEY.
	     MOVE "JOBWAR.DAT"	 TO APAC-NAME.
	     MOVE 28		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDTRF"	 TO APAC-KEY.
	     MOVE "LEDTRF.DAT"	 TO APAC-NAME.
	     MOVE 40		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDTRG"	 TO APAC-KEY.
	     MOVE "LTRG_XXX.DAT" TO APAC-NAME.
	     MOVE 40		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PARAM"	 TO APAC-KEY.
	     MOVE "PARAM.DAT"	 TO APAC-NAME.
	     MOVE 1		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PDCHEQ"	 TO APAC-KEY.
	     MOVE "PDCHEQ.DAT"	 TO APAC-NAME.
	     MOVE 44		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PORDER"	 TO APAC-KEY.
	     MOVE "PORDER.DAT"	 TO APAC-NAME.
	     MOVE 10		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "POSPLU"	 TO APAC-KEY.
	     MOVE "POSPLU.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "ROUTE"	 TO APAC-KEY.
	     MOVE "ROUTE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PSLIPS"	 TO APAC-KEY.
	     MOVE "PSLIPS.DAT"	 TO APAC-NAME.
	     MOVE 35		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SORDER"	 TO APAC-KEY.
	     MOVE "SORDER.DAT"	 TO APAC-NAME.
	     MOVE 17		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SPARTS"	 TO APAC-KEY.
	     MOVE "IN_PARTS.DAT" TO APAC-NAME.
	     MOVE 20		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "SPARTS.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "IN_PARTS.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "SPARTS.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "IN_PARTS.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "STKDEX"	 TO APAC-KEY.
	     MOVE "IN_ADJ.DAT"	 TO APAC-NAME.
	     MOVE 46		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "STOCKADJ.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "IN_ADJ.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "STOCKADJ.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "IN_ADJ.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "STKMEM"	 TO APAC-KEY.
	     MOVE "IN_MEMO.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "STKMEM.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "IN_MEMO.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "STKMEM.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "IN_MEMO.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "STOCKF"	 TO APAC-KEY.
	     MOVE "INVENT.DAT"	 TO APAC-NAME.
	     MOVE 5		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "STOCKF.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "INVENT.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "STOCKF.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "INVENT.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "SYSTEM"	 TO APAC-KEY.
	     MOVE "SYSTEM.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SYSUSE"	 TO APAC-KEY.
	     MOVE "USERS.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "SYSUSE.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "USERS.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "SYSUSE.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "USERS.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "TRFOUT"	 TO APAC-KEY.
	     MOVE "TRF_OUT.DAT"	 TO APAC-NAME.
	     MOVE 14		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TRFRIN"	 TO APAC-KEY.
	     MOVE "TRFR_IN.DAT"	 TO APAC-NAME.
	     MOVE 18		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TXTRAN"	 TO APAC-KEY.
	     MOVE "TX_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 19		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "TXTRAN.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "TX_TRAN.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "TXTRAN.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "TX_TRAN.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "UNTSLE"	 TO APAC-KEY.
	     MOVE "UNTSLE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VEHICLE"	 TO APAC-KEY.
	     MOVE "VEHICLE.DAT"	 TO APAC-NAME.
	     MOVE 54		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VSTOCK"	 TO APAC-KEY.
	     MOVE "V_INVENT.DAT" TO APAC-NAME.
	     MOVE 55		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "VSTOCK.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "V_INVENT.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "VSTOCK.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "V_INVENT.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE "WARHSE"	 TO APAC-KEY.
	     MOVE "WARHSE.DAT"	 TO APAC-NAME.
	     MOVE 51		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "WSTOCK"	 TO APAC-KEY.
	     MOVE "W_INVENT.DAT" TO APAC-NAME.
	     MOVE 52		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "WSTOCK.DAT" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "W_INVENT.DAT" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.
	     MOVE SPACES	 TO W02-RENAME-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "WSTOCK.IDX" DELIMITED BY SIZE
		    INTO W02-RENAME-FILE.
	     MOVE SPACES	 TO W02-NEW-FILE.
	     STRING APAC-PATH DELIMITED BY " "
		    "W_INVENT.IDX" DELIMITED BY SIZE
		    INTO W02-NEW-FILE.
004990	     CALL "CBL_RENAME_FILE" USING W02-RENAME-FILE
005000					  W02-NEW-FILE
005030				    RETURNING WS-STATUS.

       APAC-WORK-FILES.
	     MOVE "BULKST"	 TO APAC-KEY.
	     MOVE "BULK_XXX.WRK" TO APAC-NAME.
	     MOVE 59		 TO APAC-FILE.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING WS-SYS-ID DELIMITED SIZE
		    "_WORK\" DELIMITED SIZE
		    INTO APAC-PATH.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CORDER"	 TO APAC-KEY.
	     MOVE "CORD_XXX.WRK" TO APAC-NAME.
	     MOVE 48		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRETRN"	 TO APAC-KEY.
	     MOVE "CTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBTCON"	 TO APAC-KEY.
	     MOVE "DCON_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DPTSALES"	 TO APAC-KEY.
	     MOVE "DSAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEALER"	 TO APAC-KEY.
	     MOVE "DEAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTEX"	 TO APAC-KEY.
	     MOVE "DBEX_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTOT"	 TO APAC-KEY.
	     MOVE "DTOT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTRN"	 TO APAC-KEY.
	     MOVE "DTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DRBTCH"	 TO APAC-KEY.
	     MOVE "DBCH_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLBATCH"	 TO APAC-KEY.
	     MOVE "GBAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLJRN"	 TO APAC-KEY.
	     MOVE "GJRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLTRNB"	 TO APAC-KEY.
	     MOVE "GTRB_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "INVSAL"	 TO APAC-KEY.
	     MOVE "INVS_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDTRN"	 TO APAC-KEY.
	     MOVE "LTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "MEMDEB"	 TO APAC-KEY.
	     MOVE "DEBM_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "MOVEMT"	 TO APAC-KEY.
	     MOVE "MOVM_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PERFRM"	 TO APAC-KEY.
	     MOVE "PERF_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PRCLAB"	 TO APAC-KEY.
	     MOVE "PLAB_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PRNDISK"	 TO APAC-KEY.
	     MOVE "PRNT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PURCH"	 TO APAC-KEY.
	     MOVE "PURC_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "RECEIPTS"	 TO APAC-KEY.
	     MOVE "RECP_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "RECOVER"	 TO APAC-KEY.
	     MOVE "RECV_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALE"	 TO APAC-KEY.
	     MOVE "SALE_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALEJ"	 TO APAC-KEY.
	     MOVE "SALJ_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALESMAN"	 TO APAC-KEY.
	     MOVE "SMAN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALESTRK"	 TO APAC-KEY.
	     MOVE "STRK_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKORD"	 TO APAC-KEY.
	     MOVE "STKO_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKSEL"	 TO APAC-KEY.
	     MOVE "STKS_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKWRK"	 TO APAC-KEY.
	     MOVE "STOT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.

       APAC-OLD-FILES.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING WS-SYS-ID DELIMITED SIZE
		    "_DATA\" DELIMITED SIZE
		    INTO APAC-PATH.
	     MOVE "ASSOLD"	 TO APAC-KEY.
	     MOVE "ASSETT.OLD"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CAROLD"	 TO APAC-KEY.
	     MOVE "CARDEXSL.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQBHOLD"	 TO APAC-KEY.
	     MOVE "CHQBCH_1.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQOLD"	 TO APAC-KEY.
	     MOVE "CHEQUE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CREOLD"	 TO APAC-KEY.
	     MOVE "AP_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRMOLD"	 TO APAC-KEY.
	     MOVE "AP_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRTOLD"	 TO APAC-KEY.
	     MOVE "AP_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBFOLD"	 TO APAC-KEY.
	     MOVE "AR_CONT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBMOLD"	 TO APAC-KEY.
	     MOVE "AR_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBTOLD"	 TO APAC-KEY.
	     MOVE "AR_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBOLD"	 TO APAC-KEY.
	     MOVE "AR_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEPOLD"	 TO APAC-KEY.
	     MOVE "DEPART.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GACOLD"	 TO APAC-KEY.
	     MOVE "GL_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GAROLD"	 TO APAC-KEY.
	     MOVE "GARTEE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GBHOLD"	 TO APAC-KEY.
	     MOVE "GL_BHDR.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLBOLD"	 TO APAC-KEY.
	     MOVE "GLBTCH_1.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GPDOLD"	 TO APAC-KEY.
	     MOVE "GL_PDCHQ.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GTROLD"	 TO APAC-KEY.
	     MOVE "GL_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "INVOLD"	 TO APAC-KEY.
	     MOVE "INVOICE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "ITMOLD"	 TO APAC-KEY.
	     MOVE "ITEMS.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JAUOLD"	 TO APAC-KEY.
	     MOVE "JOBAUD.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JHDOLD"	 TO APAC-KEY.
	     MOVE "JOBHDR.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JHSOLD"	 TO APAC-KEY.
	     MOVE "JCHIST.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBOLD"	 TO APAC-KEY.
	     MOVE "JOBLIN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JRMOLD"	 TO APAC-KEY.
	     MOVE "JOBRMK.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JRQOLD"	 TO APAC-KEY.
	     MOVE "JOBREQ.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JSTOLD"	 TO APAC-KEY.
	     MOVE "JOBSTAT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JTCOLD"	 TO APAC-KEY.
	     MOVE "JOBTECH.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JWAOLD"	 TO APAC-KEY.
	     MOVE "JOBWAR.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDOLD"	 TO APAC-KEY.
	     MOVE "LEDTRF.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PDCOLD"	 TO APAC-KEY.
	     MOVE "PDCHEQ.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PRDOLD"	 TO APAC-KEY.
	     MOVE "CARDEXPR.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PUROLD"	 TO APAC-KEY.
	     MOVE "PORDER.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SLPOLD"	 TO APAC-KEY.
	     MOVE "PSLIPS.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SOROLD"	 TO APAC-KEY.
	     MOVE "SORDER.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SPAOLD"	 TO APAC-KEY.
	     MOVE "IN_PARTS.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKOLD"	 TO APAC-KEY.
	     MOVE "INVENT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STMOLD"	 TO APAC-KEY.
	     MOVE "IN_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STXOLD"	 TO APAC-KEY.
	     MOVE "IN_ADJ.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TXTOLD"	 TO APAC-KEY.
	     MOVE "TX_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "UNTOLD"	 TO APAC-KEY.
	     MOVE "UNTSLE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VEHOLD"	 TO APAC-KEY.
	     MOVE "VEHICLE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VSTOLD"	 TO APAC-KEY.
	     MOVE "V_INVENT.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "WAROLD"	 TO APAC-KEY.
	     MOVE "WARHSE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "WSTOLD"	 TO APAC-KEY.
	     MOVE "W_INVENT.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     CLOSE APACFILE.

       APAC-CREATE-NEW-EXIT.
	     EXIT.

       APACFILES-EXIT.
	     EXIT.

       APAC-SETUP-PATHS        SECTION 72.
       APAC-SETUP.
	     MOVE WS-SYS-ID	 TO W02-SYSPTH
				    W02-SYSID
				    W02-COMP.
	     CALL "CBL_CREATE_DIR" USING W02-WORK-PTH
				   RETURNING WS-STATUS.
	     OPEN INPUT APACFILE.
	     OPEN OUTPUT APACFIDS.
	     CLOSE APACFIDS.
	     OPEN I-O APACFIDS.
	     MOVE SPACES	 TO APAC-KEY.
000030	     START APACFILE
	       KEY >= APAC-KEY.

       APAC-GET-NEXT-FILE-ID.
000320	     READ APACFILE NEXT WITH IGNORE LOCK.
000350	   IF NOT (WS-STAT1 = "0")
000370	       GO TO APAC-SETUP-END.
	     MOVE APAC-KEY	 TO AFID-KEY.
	     MOVE SPACES	 TO AFID-PATH.
	     STRING APAC-PATH DELIMITED BY " "
		    APAC-NAME DELIMITED BY " "
		    INTO AFID-PATH.
	     WRITE AFID-RECORD1.
	     GO TO APAC-GET-NEXT-FILE-ID.

       APAC-SETUP-END.
	     CLOSE APACFILE
		   APACFIDS.

       APAC-SETUP-EXIT.
	     EXIT.
