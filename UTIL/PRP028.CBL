      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	   ******    ******    *
      *   **	**  **	  **  **    **	 **  **   **	**  **	  **   *
      *   **	**  **	  **  **    **	**    **       **   **	  **   *
      *   *******   ******    *******	**    **     **      ******    *
      *   **	    **	 **   **	**    **   **	    **	  **   *
      *   **	    **	  **  **	 **  **   **	    **	  **   *
      *   **	    **	  **  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   C O M P A N Y   D E T A I L S	       *
      *                                                                *
      *     VERSION 8.14.02 - January 2010			       *
      * 							       *
      ******************************************************************
       IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP028.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MARCH 2009.

	   This program is used to setup the default USB/Network printer
	   for a specific Workstation and report. (Printers that do not
	   allow for inline control characters).

           The program performs the following functions:

	       Allow the User/Installer to select the printer and font
	       that must be used and save these details to the file:
	       PRINTFIL, which is used by the system when printing is
	       to be done for the Workstation and report that has been
	       requested.

	       The program will use the Printer Open command, allowing
	       the User to select a printer and the font to be used.
	       The font that is selected may not be a True Type font
	       and must be a fixed point font such as courier.


		   COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2010
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
                   PARTICULAR PURPOSE.  See the GNU General Public License
                   for more details.

                   You should have received a copy of the GNU General
                   Public License along with this program; if not, write
                   to the Free Software Foundation, Inc., 59 Temple Place
                   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY PARAM.SL.

       COPY PRINTFIL.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000250 COPY PARAM.FDE.

       COPY PRINTFIL.FDE.

      /

       working-storage section.
       77  WS-CHECK        PIC  X(18)    VALUE
                           "aeWlimemnomLalismJ".
003260 77  WS-S1           PIC  9(04)    COMP-5.
003270 77  WS-S2           PIC  9(04)    COMP-5.
003280 77  WS-S3           PIC  9(04)    COMP-5.
003290 77  WS-S4           PIC  9(04)    COMP-5.
003300 77  WS-S5           PIC  9(04)    COMP-5.
003310 77  WS-S6           PIC  9(04)    COMP-5.
003320 77  WS-S7           PIC  9(04)    COMP-5.
003330 77  WS-S8           PIC  9(04)    COMP-5.
003330 77  WS-LEFT         PIC  9(04)    COMP-5.
003330 77  WS-RIGHT        PIC  9(04)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-SINGLE       PIC  X(01).
       77  WS-ERROR        PIC  9(01)    VALUE ZERO.
       77  WS-LINES        PIC  9(02)    VALUE ZERO.
002570 77  WS-PGE-LENGTH   PIC  9(02)    VALUE 66.
001870 77  WS-PRN-LENGTH   PIC  9(02)    VALUE 62.
003650 77  WS-COND         PIC  X(06)    VALUE X"000000000000".
003660 77  WS-NORM         PIC  X(06)    VALUE X"000000000000".
003070 77  WS-EXPP         PIC  X(06)    VALUE X"000000000000".
003080 77  WS-ECAN         PIC  X(06)    VALUE X"000000000000".
003090 77  WS-8LPI         PIC  X(06)    VALUE X"000000000000".
003100 77  WS-6LPI         PIC  X(06)    VALUE X"000000000000".
       77  WS-10CPI        PIC  X(06)    VALUE X"000000000000".
       77  WS-12CPI        PIC  X(06)    VALUE X"000000000000".
       77  WS-17CPI        PIC  X(06)    VALUE X"000000000000".
003150 77  WS-PRN-NO       PIC  X(01)    COMP-X VALUE 9.
003160 77  WS-PRN-STAT     PIC  X(01)    COMP-X.
       77  TODAY-DDMMYY    PIC  9(08) COMP-5.
       77  WS-USUB         PIC  9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
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
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.

001260 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

      *  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *  ³  PRINTER ROUTINES FOR USB & WINDOWS PRINTERS THAT DO NOT  ³
      *  ³  USE STANDARD RAW CODE FOR FORMATING 		     ³
      *  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      ******************************************************************
      * KEY							       *
      * 							       *
      * Descriptions for all of the call-by-name routines appear       *
      * alphabetically. Each description contains the routine name and *
      * function followed by these sections (as appropriate):	       *
      * 							       *
      * SYNTAX:     Shows the CALL statement you could use to call     *
      * 	    the	routine.				       *
      * 							       *
      * 	    The optional RETURNING clause is also shown. Every *
      * 	    routine returns a value showing the result of the  *
      * 	    operation. Unless otherwise indicated, zero        *
      * 	    indicates success, nonzero indicates failure. This *
      * 	    value is left in the data item specified in the    *
      * 	    RETURNING clause, in this reference, status-code.  *
      * 	    If this clause is omitted, the value is left in    *
      * 	    the special register RETURN-CODE. (If call-        *
      * 	    convention bit two is set, RETURN-CODE is not      *
      * 	    changed.)					       *
      * 							       *
      * 	    status-code must be a numeric data item capable of *
      * 			holding positive values from 0 to      *
      * 			65535; for example, PIC X(2) COMP-5.   *
      * 							       *
      * 	    The name of the routine must be coded in upper     *
      * 	    case.					       *
      * 							       *
      * PARAMETERS: Describes any parameters shown in the RETURNING    *
      * 	    and USING clause. A parameter enclosed in brackets,*
      * 	    for example, [parameter1] is optional and might    *
      * 	    not be needed for all forms of the routine.        *
      * 							       *
      * ON ENTRY:   Indicates which of the parameters shown are passed *
      * 	    on entry.					       *
      * 							       *
      * ON EXIT:    Indicates which of the parameters shown are        *
      * 	    returned on exit.				       *
      * 							       *
      * 	    Where bits of one or more bytes are referenced,    *
      * 	    bit 0 is the least significant (rightmost) bit.    *
      * 							       *
      * COMMENTS:   Provides any additional information necessary for  *
      * 	    the successful use of the routine.		       *
      *								       *
      *    PC_PRINT_FILE	Print a file			       *
      *    PC_PRINTER_CLOSE	Close a printer channel 	       *
      *    PC_PRINTER_CONTROL	Send a printer command to a printer    *
      *    PC_PRINTER_FREE_BMP	Free bitmap from memory 	       *
      *    PC_PRINTER_INFO	Get printer information 	       *
      *    PC_PRINTER_LOAD_BMP	Load bitmap into memory 	       *
      *    PC_PRINTER_OPEN	Open a printer channel		       *
      *    PC_PRINTER_SET_COLOR	Set printer color		       *
      *    PC_PRINTER_SET_FONT	Set printer font		       *
      *    PC_PRINTER_WRITE	Write text to a printer 	       *
      *    PC_PRINTER_WRITE_BMP	Write bitmap to a printer	       *
      * 							       *
      *================================================================*
      * 							       *
      *    PC_WIN_CLOSE_PRINTER	   Close a printer channel	       *
      *    PC_WIN_FREE_PBMP	   Free bitmap from memory	       *
      *    PC_WIN_LOAD_PBMP	   Load bitmap into memory	       *
      *    PC_WIN_OPEN_PRINTER	   Open a printer channel	       *
      *    PC_WIN_OPEN_PRINTER_EXT Extended open a printer channel     *
      *    PC_WIN_PRINT_FILE	   Print a file 		       *
      *    PC_WIN_PRINT_FILE_EXT   Extended print a file	       *
      *    PC_WIN_PRINTER_CONTROL  Send a printer control	       *
      *    PC_WIN_PRINTER_INFO	   Get printer information	       *
      *    PC_WIN_SET_PCOLOR	   Set printer color		       *
      *    PC_WIN_SET_PDEFAULT	   Set or reset default printer        *
      *    PC_WIN_SET_PFONT	   Set printer font		       *
      *    PC_WIN_WRITE_PBMP	   Write bitmap to a printer	       *
      *    PC_WIN_WRITE_PRINTER	   Write to a printer		       *
      * 							       *
      * ______________________________________________________________ *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_OPEN	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Printer-Handle                  pic x(04) comp-5.
       01  Document-Title.
	   03  Title-Len		   pic x(02) comp-5.
	   03  Title-Text		   pic x(80).
       01  Printer-Flags                   pic x(04) comp-5.
       01  Window-Handle		   pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_WRITE	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  Print-Buffer                    pic x(80).
       01  Print-Buffer-Len                pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Parameters for PC_PRINTER_CONTROL	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
      * ---print-command:  1 - Abort printing and close printer
      * 		   2 - Throw a page
      * 		   3 - Flush the print buffers
      * 		   4 - Start a new line
      *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_CLOSE	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_INFO	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       78 PrinterNameBufferBegin        value next.
       01 PrinterNameBuffer         pic x(255).
       78 PrinterNameBufferLength       value next
          - PrinterNameBufferBegin.
       78 PrinterTypeBufferBegin        value next.
       01 PrinterTypeBuffer         pic x(255).
       78 PrinterTypeBufferLength       value next
          - PrinterTypeBufferBegin.
       78 PrinterDeviceBufferBegin      value next.
       01 PrinterDeviceBuffer       pic x(255).
       78 PrinterDeviceBufferLength     value next
          - PrinterDeviceBufferBegin.
       78 PrinterLocationBufferBegin    value next.
       01 PrinterLocationBuffer     pic x(255).
       78 PrinterLocationBufferLength   value next
          - PrinterLocationBufferBegin.
       78 PrinterCommentBufferBegin     value next.
       01 PrinterCommentBuffer      pic x(255).
       78 PrinterCommentBufferLength    value next
          - PrinterCommentBufferBegin.
       78 PrinterPaperSizeBufferBegin   value next.
       01 PrinterPaperSizeBuffer    pic x(255).
       78 PrinterPaperSizeBufferLength  value next
          - PrinterPaperSizeBufferBegin.

       01  PrinterInformation.
	   03  PrinterInformationLength  pic x(04) comp-5.
	   03  DeviceContextHandle	 pic x(04) comp-5.
	   03  reserved 		 pic x(04) comp-5.
	   03  Orientation		 pic x(04) comp-5.
	   03  PageRowsCurrentFont	 pic x(04) comp-5.
	   03  PageColsCurrentFont	 pic x(04) comp-5.
	   03  RowsRemainingCurrentFont  pic x(04) comp-5.
	   03  MaxGraphicsHorizontalAxis pic x(04) comp-5.
	   03  MaxGraphicsVerticalAxis	 pic x(04) comp-5.
	   03  MinGraphicsHorizontalAxis pic x(04) comp-5.
	   03  MinGraphicsVerticalAxis	 pic x(04) comp-5.
      *
      *    -------  Horizontal
      *
	   03  CurrentCooridinateX	 pic x(04) comp-5.
      *
      *    -------  Vertical
      *
	   03  CurrentCooridinateY	 pic x(04) comp-5.
	   03  NumberOfCopies		 pic x(02) comp-5.
      *
      *    -------  Draft, Low,Medium, High, Default or DPI
      *
	   03  PrintQuality		 pic x(02) comp-5.
      *
      *    -------  Monochrome or Color
      *
	   03  Color			 pic x(01) comp-5.
	   03  reserved 		 pic x(01) comp-5.
	   03  DriverVersionNumber	 pic x(02) comp-5.
	   03  PrinterName.
	       05  PrinterNameLength	 pic x(04) comp-5
                   value PrinterNameBufferLength.
	       05  p-PrinterNameBuffer			   pointer.
	   03  PrinterType.
	       05  PrinterTypeLength	 pic x(04) comp-5
                   value PrinterTypeBufferLength.
	       05  p-PrinterTypeBuffer			   pointer.
	   03  PrinterDevice.
	       05 PrinterDeviceLength	 pic x(04) comp-5
                  value PrinterDeviceBufferLength.
	       05 p-PrinterDeviceBuffer 		  pointer.
	   03  PrinterLocation.
	       05 PrinterLocationLength  pic x(04) comp-5
                  value PrinterLocationBufferLength.
	       05 p-PrinterLocationBuffer		  pointer.
	   03  PrinterComment.
	       05 PrinterCommentLength	 pic x(04) comp-5
                  value PrinterCommentBufferLength.
	       05 p-PrinterCommentBuffer		  pointer.
	   03  PrinterPaperSize.
	       05 PrinterPaperSizeLength pic x(04) comp-5
                 value PrinterPaperSizeBufferLength.
	       05 p-PrinterPaperSizeBuffer		  pointer.

      *
       01  Operation                       pic x(30).
      *


       01   printer-control.

	   03  Default-Printer-Name.
	       05  Default-Printer-Name-Len  pic x(02) comp-5.
	       05  Default-Printer-Name-Text pic x(80).

	   03  Document-title.
	       05  Title-len   pic x(02) comp-5.
	       05  Title-text  pic x(20).

	   03  Font-family.
	       05  Font-family-namelen	pic x(2) comp-5 value 80.
	       05  Font-family-name	pic x(80).

	   03  FontTypeFace    pic x(13) value "Courier (W1)" & x"00".
	   03  font-style      pic x(4) comp-5 value 0.
	   03  font-size       pic x(4) comp-5 value 0.
	   03  font-size-8     pic x(4) comp-5 value 8.
	   03  font-size-9     pic x(4) comp-5 value 9.
	   03  font-size-10    pic x(4) comp-5 value 10.
	   03  font-size-11    pic x(4) comp-5 value 11.
	   03  font-size-12    pic x(4) comp-5 value 12.
	   03  font-size-13    pic x(4) comp-5 value 13.
	   03  font-size-14    pic x(4) comp-5 value 14.
	   03  font-size-15    pic x(4) comp-5 value 15.
	   03  font-size-16    pic x(4) comp-5 value 16.
	   03  font-size-17    pic x(4) comp-5 value 17.
	   03  font-size-18    pic x(4) comp-5 value 18.

           03  abort           pic x(4) comp-5 value 1.
           03  prn-control     pic x(4) comp-5 value 2.
	   03  prn-flags       pic x(4) comp-5 value 1.
           03  prn-command     pic x(4) comp-5 value 4.
	   03  prn-handle      pic x(4) comp-5.

	   03  prn-line-size   pic x(4) comp-5 value 0.
           03  prn-line-80     pic x(4) comp-5 value 80.
           03  prn-line-136    pic x(4) comp-5 value 136.

       01  W01-Print-Line-80.
           03  W01-Detail-80   pic x(80).

       01  W01-Print-Line-136.
           03  W01-Detail-136  pic x(136).

      *
      *    *****    test data layout
      *
       01  W01-PRINTER-COMP.
           03  FILLER         PIC  X(06) VALUE SPACES.
	   03  W01-COMP-NAME  PIC  X(40) VALUE
			    "TEST COMPANY - APAC/WINDOWS PRINTING USB".

       01  W01-PRINT-HEADER.
	   03  W01-H1.
	       05  W01-DATE-H  PIC  X(06) VALUE "Date:".
	       05  W01-DATE    PIC  Z9/99/9999.
	       05  FILLER      PIC  X(06) VALUE SPACES.
	   03  W01-H2.
	       05  W01-COMPANY PIC  X(40) VALUE SPACES.
	   03  W01-H3.
	       05  FILLER      PIC  X(08) VALUE SPACES.
	       05  W01-PAGE-H  PIC  X(06) VALUE "Page:".
	       05  W01-PAGE    PIC  ZZZ9.

       01  W01-PRINT-RECORD.
	   03  W01-H4.
	       05  W01-ACNO	 PIC  X(06).
	       05  FILLER	 PIC  X(02) VALUE SPACES.
	   03  W01-H5.
	       05  W01-ACNT-NAME PIC  X(40).
	       05  FILLER	 PIC  X(02) VALUE SPACES.
	   03  W01-H6.
	       05  W01-BALANCE	 PIC  Z(07)9.99-.
	       05  FILLER	 PIC  X(02) VALUE SPACES.
	   03  W01-H7.
	       05  W01-ACTIVE	 PIC  Z9/99/9999.
	       05  FILLER	 PIC  X(02) VALUE SPACES.
	   03  W01-H8.
	       05  W01-STATUS	 PIC  X(04).

       COPY WS.WS.

000290 01  WS-PARID.
000020     03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

           03  W02-PRINTER.
               05  W02-PRN     PIC  X(06).

           03  W02-SPOOLER.
               05  W02-WS      PIC  X(02).
               05  W02-US      PIC  X(01).
               05  W02-TERM    PIC  X(03).
               05  W02-US2     PIC  X(01).
               05  W02-REP     PIC  X(01).
               05  W02-P       PIC  X(01).
               05  W02-USER    PIC  X(03).

       COPY W12.WS.

001510 01  W20-DATE.
001520     03  W20-DAY         PIC 9(02).
001530     03  W20-MONTH       PIC 9(02).
	   03  W20-CENT        PIC 9(02).
001540     03  W20-YEAR        PIC 9(02).
001550 01  W20-DTE1 REDEFINES W20-DATE.
001560	   03  W20-DTE	       PIC 9(08).
001550 01  W20-DTE2 REDEFINES W20-DATE.
	   03  W20-CC	       PIC 9(02).
001520     03  W20-YY          PIC 9(02).
001530     03  W20-MM          PIC 9(02).
001540     03  W20-DD          PIC 9(02).
       01  W20-LEAP REDEFINES W20-DATE.
	   03  FILLER	       PIC 9(04).
	   03  W20-CY	       PIC 9(04).

001570 01  W25-CALCS.
001580	   03  W25-RESULT      PIC  9(09)V99.
001590     03  W25-RESULT1 REDEFINES W25-RESULT.
001600	       05  W25-DAYS    PIC  9(03).
001610	       05  W25-WHOLE   PIC  9(06).
001620	       05  W25-DECIMAL PIC  9(02).
	   03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  FILLER      PIC  9(02).
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
001630	   03  W25-PASS.
001640	       05  W25-PASS1   PIC  9(03).
001650	       05  W25-PASS2   PIC  9(03).
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

       LINKAGE SECTION.

       COPY CHAIN.LS.

       01  LS-SCREEN-PRINT.
      *
      *    COMMAND: C = Close printer
      *             E = End of report
      *             O = Open printer
      *             P = Print a line
      *             S = Skip to new page
      *
           03  LS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      *             S = Screen
      *             1 = Default Printer
      *             2 = Allow user to select a printer
      *
           03  LS-PRINTER.
               05  LS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      *             1-3 = Print advance lines specified
      *             4-? = Print advance 1 line, then use a loop
      *                   to advance the remaining lines.
      *              99 = Print and advance to a new page.
      *
           03  LS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      *            2 = Invoices/Credit Notes
      *            3 = General Reports (Stock)
      *            4 = General Reports (Creditors)
      *            5 = General Reports (Debtors)
      *            6 = General Reports (G/Ledger)
      *            7 = Statements
      *            8 = Labels
      *            9 = Quotations
      *
           03  LS-REPORT       PIC  9(01).
           03  LS-SCREEN       PIC  9(01).
           03  LS-SCREEN-LIN   PIC  9(02).
           03  LS-LINAGE       PIC  9(02).
           03  LS-PAPER        PIC  X(01).
           03  FILLER          PIC  X(01).

       01  L02-PRINTER-DETAILS.
           03  L02-PRINTER     PIC  X(12).
002570     03  L02-PGE-LENGTH  PIC  9(02).
001870     03  L02-PRN-LENGTH  PIC  9(02).
           03  L02-LINAGE      PIC  9(02).
           03  L02-PRN-STATUS  PIC  X(01).
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
           03  L02-PRN-TYPE    PIC  X(01).
           03  L02-PRN-LINE    PIC  X(136).

001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS0-PROGRAMS
				LS0-SECURITY.

005070 AA000-MAIN	       SECTION.
005080 AA00.
007760	     PERFORM ZA000-INIT.
003240	   IF LS0-PALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.

	     PERFORM YC000.

       AA49.
             EXIT PROGRAM.

       AA05.
      * ______________________________________________________________ *
      * PC_PRINTER_OPEN 					       *
      * 							       *
      * Opens a printer channel, giving it a document title, and       *
      * optionally displaying the dialog boxes for printer control,    *
      * font selection, and progress indication.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_OPEN" using	 printer-handle        *
      * 					 document-title        *
      * 			       by value  flags		       *
      * 			       by value  window-handle	       *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	document-title	Group item defined as:		       *
      * 	    title-len	pic x(2) comp-5.		       *
      * 	    title-text	pic x.				       *
      * 	    flags	Numeric literal or pic x(4) comp-5.    *
      * 	    window-handle Numeric literal or pic x(4) comp-5.  *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	title-len	The length of the document title.      *
      * 	title-text	The title of the document to be        *
      * 			printed.			       *
      * 	flags	A set of bits to define printer options:       *
      * 	 bit 0	Display a dialog for defining printer	       *
      * 		characteristics (cannot be used with bit 2 or  *
      * 		bit 3)					       *
      * 	 bit 1	Display a font dialog box, so you can select   *
      * 		the font for the document		       *
      * 	 bit 2	Print in portrait orientation (cannot be used  *
      * 		with bit 0 or bit 3)			       *
      * 	 bit 3	Print in landscape orientation (cannot be used *
      * 		with bit 0 or bit 2)			       *
      * 	 bit 4	Display a progress indicator dialog box        *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * 	window-handle  Handle of the parent window. The parent *
      * 		       window is used as a reference for       *
      * 		       positioning dialog boxes on the screen. *
      * 		       If window-handle is 0 positioning is    *
      * 		       system dependent.		       *
      * 							       *
      * ON EXIT:						       *
      * 	printer-handle	Returns a handle to be used for        *
      * 			subsequent printer operations.	       *
      * 	status-code	Printer status code:		       *
      * 							       *
      * 	    0		Successful			       *
      * 	    1		Could not open printer device	       *
      * 	    5		Failed to open file		       *
      * 	    9		No default printer found. Select a     *
      * 			printer from Windows Control Panel.    *
      * 	   24		User pressed Cancel on the printer     *
      * 			setup dialog or the font selection     *
      * 			dialog. 			       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	     move 17		 to title-len.
             move "Printer Info Test"
                                 to title-text.

             call "PC_PRINTER_OPEN" using by reference prn-handle
                                          by reference document-title
                                          by value prn-flags
                                          by value 0
	     end-call.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     Setup the printer font		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move FontTypeFace	 to font-family-name.
	     move 12		 to font-family-namelen.
             move 16             to font-size.
             move 8              to font-style.
	     perform set-printer-font.
	     perform printer-comp.
             move 46             to prn-line-size.
	     perform print-line.
	     move 4		 to prn-command.
	     perform control-printer.
	     move 11		 to font-size.
	     perform set-printer-font.
	     perform printer-header.
	     move 80		 to prn-line-size.
	     perform print-line.
	     move 4		 to prn-command.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     move 4		 to prn-command.
             perform control-printer.
	     MOVE "Acc.No."	 TO W01-H4.
	     MOVE "Account Name" TO W01-H5.
	     MOVE "    Balance"	 TO W01-H6.
	     MOVE " Date Act"	 TO W01-H7.
	     MOVE "Status"	 TO W01-H8.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-80.
	     move 80		 to prn-line-size.
	     perform print-line.
	     move 4		 to prn-command.
             move 0              to font-style.
             perform set-printer-font.
             perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     MOVE "TST001"	 TO W01-ACNO.
	     MOVE "First test account name - Version 9"
				 TO W01-ACNT-NAME.
	     MOVE 185624.23	 TO W01-BALANCE.
	     MOVE 12082009	 TO W01-ACTIVE.
	     MOVE "30 D"	 TO W01-STATUS.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-80.
	     perform print-line.
	     move 4		 to prn-command.
             perform control-printer.
	     MOVE "TST002"	 TO W01-ACNO.
	     MOVE "Second test account name"
				 TO W01-ACNT-NAME.
	     MOVE -567.94	 TO W01-BALANCE.
	     MOVE 23072009	 TO W01-ACTIVE.
	     MOVE "CRED"	 TO W01-STATUS.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-80.
	     perform print-line.
	     move 4		 to prn-command.
             perform control-printer.
	     MOVE "TST003"	 TO W01-ACNO.
	     MOVE "Third test account name"
				 TO W01-ACNT-NAME.
	     MOVE 23947.12	 TO W01-BALANCE.
	     MOVE 19082009	 TO W01-ACTIVE.
	     MOVE "CURR"	 TO W01-STATUS.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-80.
	     perform print-line.
	     move 4		 to prn-command.
             perform control-printer.

	     perform close-down-printer.
             stop run.

       printer-header	   section.
	     MOVE TODAY-DDMMYY	 TO W01-DATE.
	     MOVE 1		 TO W01-PAGE.
	     MOVE W01-PRINT-HEADER
				 TO W01-Print-Line-80.

       printer-comp	   section.
	     MOVE W01-PRINTER-COMP
				 TO W01-Print-Line-80.

      * ______________________________________________________________ *
      * PC_PRINTER_WRITE					       *
      * 							       *
      * Writes a text string to the printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_WRITE" using	  printer-handle       *
      * 					 print-buffer	       *
      * 			       by value  print-buff-len        *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-buffer	pic x(n).			       *
      * 	print-buff-len	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	printer-buffer	The buffer from which the bytes are to *
      * 			be printed			       *
      * 	printer-buff-len  The number of bytes to print	       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
       print-line	  section.
             call "PC_PRINTER_WRITE" using    prn-handle
                                              W01-Print-Line-80
				     by value prn-line-size
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_SET_FONT					       *
      * 							       *
      * Sets the font on the printer.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_SET_FONT" using     printer-handle    *
      * 					     font-family-name  *
      * 				   by value  font-size	       *
      * 				   by value  font-style        *
      * 			   returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	font-family-name	Group item defined as:	       *
      * 	    name-len	    x(2) comp-5.		       *
      * 	    name-text	    pic x().			       *
      * 	font-size	Numeric literal or pic x(4) comp-5.    *
      * 	font-style	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	name-len	The length of the font family name.    *
      * 	name-text	The family name of the font to be used,*
      * 			such as Courier, Times and Symbol      *
      * 	font-size	Point size of the font		       *
      * 	font-style	Set of bits defining the required font *
      * 			style:				       *
      * 	  bit 3	 Bold					       *
      * 	  bit 2	 Strikeout				       *
      * 	  bit 1	 Underline				       *
      * 	  bit 0	 Italic 				       *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	set-printer-font   section.
	     call "PC_PRINTER_SET_FONT"
				     using by reference prn-handle
					   by reference font-family
                                           by value font-size
                                           by value font-style
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CONTROL					       *
      * 							       *
      * Sends a print command to a printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CONTROL" using      printer-handle    *
      * 				  by value   print-command     *
      * 				  returning  status-code       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-command	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	print-command	The command to send to the printer:    *
      * 							       *
      * 	  1   Abort printing the document and close the        *
      * 	      printer					       *
      * 	  2   Throw a page				       *
      * 	  3   Flush the print buffers			       *
      * 	  4   Advance one line				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      *		None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	control-printer    section.
             call "PC_PRINTER_CONTROL" using    prn-handle
                                       by value prn-command
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CLOSE					       *
      * 							       *
      * Closes a previously opened printer channel.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CLOSE" using printer-handle	       *
      * 				returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 							       *
      * ON EXIT:						       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	close-down-printer section.
            call "PC_PRINTER_CLOSE" using by reference prn-handle
            end-call.






      /
044130 ZA000-INIT	       SECTION 80.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
044150	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
      *    *************************************************************
      *    ****     W I N D O W S   P R I N T E R S   F I L E	    ****
      *    *************************************************************
	     MOVE "PRINTFIL"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PRINTFIL.

       ZA01B.
044580	     MOVE 1		 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
	     GO TO ZA999-EXIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup the Screen Graphics Characters: ³ Ä Ú ¿ À Ù Â Á etc ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (16) WS-TCHR (41)
021940                              WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3          TO WS-MCHR (16) WS-MCHR (41)
021960                              WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (16) WS-BCHR (41)
021980                              WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.

044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       ZA205.
             EXIT PROGRAM.

044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 81.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER  TO WS-FILE
041870         MOVE WS-RECKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 33
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 49
041860	       MOVE W02-STKALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STKA-KEY	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170	     STOP RUN.
