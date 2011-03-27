      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *      *******	*******	   ******   **	   **	********       *
      *      **	   **	**    **     **	    ***	   **	   **	       *
      *      **	   **	**    **     **	    ** *   **	   **	       *
      *      *******	*******	     **	    **	*  **	   **	       *
      *      **		**    **     **	    **	 * **	   **	       *
      *      **		**    **     **	    **	  ***	   **	       *
      *      **		**    **   ******   **	   **	   **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * 	  P R I N T   U T I L I T Y   P R O G R A M	       *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *    All code for USB/WINDOWS printing has been written in lower *
      *    case to enable it to be easiliy identified while the new    *
      *    printing routines are being implemented.		       *
      *    This will be changed to the normal standard once everything *
      *    is working correctly.				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRINTUTL.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   January 1997. November 2009 - WIN/USB.

	   This program replaces the previous program, that only allowed
	   for printing to the LPT and COM ports. This program now allows
	   for USB, Network or any printers that need the Windows print
	   process, while still allowing for the previous printers.

	   This program is used to print/spool/display report details.

	   The program performs the following functions:

	       Open the printer.
	       Open the spool file.
	       Set up the necessary parameters if the report is to appear
	       on the screen.
	       Print a line and advance the paper as requested.
	       Spool a line and pass on the advance parameters.
	       Display a line of print as requested (allows for left/right).

	   The calling program/s treat all reports as if they were being
	   printed on a printer and are unaware of the Users request to
	   spool or display the report.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1997 - 2010
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 3 of the License, or (at
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
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.

000100 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY PARAM.SL.

000200	   SELECT PRNSPL  ASSIGN W02-SPOOLER
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000250 COPY PARAM.FDE.

      /
000330 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
       01  R-SL1.
	   03  R-SLA	       PIC  X(78).
	   03  FILLER	       PIC  X(58).
       01  R-SL2.
	   03  FILLER	       PIC  X(58).
	   03  R-SLB	       PIC  X(78).
000350 01  REP-LINE1.
000360	   03  REP-DETAIL1     PIC  X(136).

000330 FD  PRNSPL.
       01  SPL-L1.
	   03  SPL-CONT.
	       05  SPL-ADV     PIC  9(02).
	       05  SPL-LIN     PIC  9(02).
000360	   03  SPL-DETAIL1     PIC  X(136).

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003260 77  WS-S1	   PIC	9(04)	 COMP-5.
003270 77  WS-S2	   PIC	9(04)	 COMP-5.
003280 77  WS-S3	   PIC	9(04)	 COMP-5.
003290 77  WS-S4	   PIC	9(04)	 COMP-5.
003300 77  WS-S5	   PIC	9(04)	 COMP-5.
003310 77  WS-S6	   PIC	9(04)	 COMP-5.
003320 77  WS-S7	   PIC	9(04)	 COMP-5.
003330 77  WS-S8	   PIC	9(04)	 COMP-5.
003330 77  WS-LEFT	   PIC	9(04)	 COMP-5.
003330 77  WS-RIGHT	   PIC	9(04)	 COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001820 77  WS-OPTION	   PIC	X(01).
001820 77  WS-SINGLE	   PIC	X(01).
       77  WS-ERROR	   PIC	9(01)	 VALUE ZERO.
       77  WS-LINES	   PIC	9(02)	 VALUE ZERO.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

003050 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003200	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250	       05  FILLER      PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270			       PIC X(50).
		  07  FILLER   PIC X(10).
003280	       05  FILLER      PIC X(19).

       COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

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
      *    ³	      Parameters for PC_PRINTER_DEFAULT_NAME	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Printer-Flags           pic x(04) comp-5  Value 1.
       01  Default-Printer-Name.
	   03  Default-Printer-Name-Len    pic x(02) comp-5.
	   03  Default-Printer-Name-Text   pic x(80).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      Parameters for PC_PRINTER_DEFAULT_FONT	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Font-Family-Name.
	   03  Default-Font-Name-Len	   pic x(02) comp-5.
	   03  Default-Font-Name-Text	   pic x(80).
       01  Default-Font-Size               pic x(04) comp-5.
       01  Default-Font-Style              pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Parameters for PC_PRINTER_DEFAULT_PROPERTIES       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Properties-Flags      pic x(4) comp-5.
       01  Default-Properties.
	   03 pr-len	       pic x(2) comp-5.
	   03 pr-papersize     pic s9(4) comp-5.
	   03 pr-paperlength   pic s9(4) comp-5.
	   03 pr-paperwidth    pic s9(4) comp-5.
	   03 pr-scale	       pic s9(4) comp-5.
	   03 pr-copies        pic s9(4) comp-5.
	   03 pr-papertray     pic s9(4) comp-5.
	   03 pr-printquality  pic s9(4) comp-5.
	   03 pr-color	       pic s9(4) comp-5.
	   03 pr-duplex        pic s9(4) comp-5.
	   03 pr-orientation   pic s9(4) comp-5.
	   03 pr-yresolution   pic s9(4) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_OPEN	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Printer-Handle     pic x(04) comp-5.
       01  Document-Title.
	   03  Title-Len      pic x(02) comp-5.
	   03  Title-Text     pic x(80).
       01  Printer-Flags      pic x(04) comp-5.
       01  Window-Handle      pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_WRITE	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  Print-Buffer       pic x(136).
       01  Print-Buffer-Len   pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Parameters for PC_PRINTER_LOAD_BMP	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
       01  BmpFilename	      pic x(64).
       01  BmpID	      pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Parameters for PC_PRINTER_WRITE_BMP	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)
       01  WriteBmpReserved   pic x(04) comp-5 value 0.
       01  BmpRow	      pic x(04) comp-5 value 1.
       01  BmpCol	      pic x(04) comp-5 value 1.
       01  BmpWidth	      pic x(04) comp-5 value 92.
       01  BmpHeigth	      pic x(04) comp-5 value 12.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Parameters for PC_PRINTER_WRITE_BMP	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Parameters for PC_PRINTER_SET_COLOR	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  fore-or-back       pic x(2) comp-5.
       01  color-red	      pic x(2) comp-5.
       01  color-green	      pic x(2) comp-5.
       01  color-blue	      pic x(2) comp-5.

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
      *    ³		    Parameters for PC_PRINT_FILE	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Filename.
	   03  Filename-Len		       pic xx comp-5.
	   03  Filename-Text		       pic x(80).

       01  DocumentTitle.
	   03  DocumentTitle-Len	       pic xx comp-5.
	   03  DocumentTitle-Text	       pic x(15).
      * --- Printer-Flags (same as above)
      * --- Window-Handle (Same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_INFO	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       78 PrinterNameBufferBegin	value next.
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
	   03  PrinterInformationLength  pic xxxx comp-5.
	   03  DeviceContextHandle	 pic xxxx comp-5.
	   03  reserved 		 pic xxxx comp-5.
	   03  Orientation		 pic xxxx comp-5.
	   03  PageRowsCurrentFont	 pic xxxx comp-5.
	   03  PageColsCurrentFont	 pic xxxx comp-5.
	   03  RowsRemainingCurrentFont  pic xxxx comp-5.
	   03  MaxGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MaxGraphicsVerticalAxis	 pic xxxx comp-5.
	   03  MinGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MinGraphicsVerticalAxis	 pic xxxx comp-5.
      *
      *    -------  Horizontal
      *
	   03  CurrentCooridinateX	 pic xxxx comp-5.
      *
      *    -------  Vertical
      *
	   03  CurrentCooridinateY	 pic xxxx comp-5.
	   03  NumberOfCopies		 pic xx   comp-5.
      *
      *    -------  Draft, Low,Medium, High, Default or DPI
      *
	   03  PrintQuality		 pic xx   comp-5.
      *
      *    -------  Monochrome or Color
      *
	   03  Color			 pic x	  comp-5.
	   03  reserved 		 pic x	  comp-5.
	   03  DriverVersionNumber	 pic xx   comp-5.
	   03  PrinterName.
	       05  PrinterNameLength	 pic xxxx comp-5
                   value PrinterNameBufferLength.
	       05  p-PrinterNameBuffer	 pointer.
	   03  PrinterType.
	       05  PrinterTypeLength	 pic xxxx comp-5
                   value PrinterTypeBufferLength.
	       05  p-PrinterTypeBuffer	 pointer.
	   03  PrinterDevice.
	       05 PrinterDeviceLength	 pic xxxx comp-5
                  value PrinterDeviceBufferLength.
	       05 p-PrinterDeviceBuffer  pointer.
	   03  PrinterLocation.
	       05 PrinterLocationLength  pic xxxx comp-5
                  value PrinterLocationBufferLength.
	       05 p-PrinterLocationBuffer  pointer.
	   03  PrinterComment.
	       05 PrinterCommentLength	 pic xxxx comp-5
                  value PrinterCommentBufferLength.
	       05 p-PrinterCommentBuffer pointer.
	   03  PrinterPaperSize.
	       05 PrinterPaperSizeLength pic xxxx comp-5
                 value PrinterPaperSizeBufferLength.
	       05 p-PrinterPaperSizeBuffer pointer.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Used to display error messages for the Print Routines.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  Operation			   pic x(30).

       01   printer-control.

	   03  Default-Printer-Name.
	       05  Default-Printer-Name-Len  pic x(02) comp-5.
	       05  Default-Printer-Name-Text pic x(80).

	   03  Font-family.
	       05  Font-family-namelen	pic x(2) comp-5 value 80.
	       05  Font-family-name	pic x(80).

	   03  FontTypeFace    pic x(20) value spaces.
	   03  FontTypeFaceC   pic x(13) value "Courier New" & x"00".
	   03  FontTypeFaceL   pic x(18)
			       value "Letter Gothic Std" & x"00".
	   03  font-style      pic x(4) comp-5 value 0.
	   03  font-size       pic x(4) comp-5 value 0.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 5 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-5-CPI      pic x(4) comp-5 value 24.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 8 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-8-CPI      pic x(4) comp-5 value 14.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 12 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-12-CPI     pic x(4) comp-5 value 10.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 15 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-15-CPI     pic x(4) comp-5 value 8.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 16 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-16-CPI     pic x(4) comp-5 value 7.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 19 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-19-CPI     pic x(4) comp-5 value 5.

	   03  abort	       pic x(4) comp-5 value 1.
           03  prn-control     pic x(4) comp-5 value 2.
	   03  prn-flags       pic x(4) comp-5 value 1.
           03  prn-command     pic x(4) comp-5 value 4.
	   03  prn-handle      pic x(4) comp-5.

	   03  prn-line-size   pic x(4) comp-5 value 0.
	   03  prn-line-94     pic x(4) comp-5 value 94.
           03  prn-line-136    pic x(4) comp-5 value 136.
      *
      *    -------    The buffer used for printing
      * 	      Formatted data is moved to this buffer for
      * 	      printing. This buffer may be redefined to
      * 	      represent different buffer sizes (Font size)
      *
       01  W01-Print-Detail.
      *
      *    -------    Font size 7
      *
	   03  W01-Print-Line-136.
	     05  W01-Detail-136.
      *
      *    -------    Font size 10
      *
	       07  W01-Print-Line-94.
		 09  W01-Detail-94 pic	x(94).
	       07  filler	   pic	x(42).

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

       01  W05-SCREEN.
	   03  W05-PRINT-DETAIL.
	     05  W05-LINE OCCURS 20.
	       07  W05-TYPE    PIC  X(01).
	       07  W05-DETAIL  PIC  X(136).
	   03  W05-PRINT-DETAIL2 REDEFINES W05-PRINT-DETAIL.
	     05  W05-LINE2 OCCURS 20.
	       07  FILLER      PIC  X(01).
	       07  W05-SLA     PIC  X(78).
	       07  FILLER      PIC  X(58).
	   03  W05-PRINT-DETAIL2 REDEFINES W05-PRINT-DETAIL.
	     05  W05-LINE3 OCCURS 20.
	       07  FILLER      PIC  X(01).
	       07  FILLER      PIC  X(58).
	       07  W05-SLB     PIC  X(78).
	   03  W05-PRINT-DETAIL3 REDEFINES W05-PRINT-DETAIL.
	     05  W05-LINE4 OCCURS 20.
	       07  FILLER      PIC  X(01).
	       07  W05-CHAR    PIC  X(01) OCCURS 136.

       01  W06-SCREEN.
	   03  W06-PRINT-DETAIL.
	     05  W06-LINE OCCURS 20.
	       07  W06-CHAR    PIC  X(01) OCCURS 78.

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

003930 01  W25-CALCS.
002860	   03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC 9(04).
	       05  W25-SUB     PIC 9(01).
	       05  FILLER      PIC 9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

       01  LS-SCREEN-PRINT.
      *
      *    COMMAND: C = Close printer
      * 	    E = End of report
      *		    O = Open printer
      * 	    P = Print a line
      * 	    S = Skip to new page
      *
	   03  LS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      * 	    S = Screen
      * 	    1 = Lpt1
      * 	    2 = Lpt2
      * 	    3 = Lpt3
      * 	    4 = Com1
      * 	    5 = Com2
      * 	    8 = USB/WINDOWS
      *
	   03  LS-PRINTER.
	       05  LS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      * 	    1-3 = Print advance lines specified
      * 	    4-? = Print advance 1 line, then use a loop
      * 		  to advance the remaining lines.
      * 	     99 = Print and advance to a new page.
      *
	   03  LS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      * 	   2 = Invoices/Credit Notes
      * 	   3 = General Reports (Stock)
      * 	   4 = General Reports (Creditors)
      * 	   5 = General Reports (Debtors)
      * 	   6 = General Reports (G/Ledger)
      * 	   7 = Statements
      * 	   8 = Labels
      * 	   9 = Quotations
      *
	   03  LS-REPORT       PIC  9(01).
	   03  LS-SCREEN       PIC  9(01).
	   03  LS-SCREEN-LIN   PIC  9(02).
	   03  LS-LINAGE       PIC  9(02).
	   03  LS-PAPER        PIC  X(01).
	   03  FILLER	       PIC  X(01).

       01  L02-PRINTER-DETAILS.
	   03  L02-PRINTER     PIC  X(12).
002570	   03  L02-PGE-LENGTH  PIC  9(02).
001870	   03  L02-PRN-LENGTH  PIC  9(02).
	   03  L02-LINAGE      PIC  9(02).
	   03  L02-PRN-STATUS  PIC  X(01).
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  L02-PRN-TYPE    PIC  X(01).
	   03  L02-PRN-LINE    PIC  X(136).

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

       01  SD-LEFT.
	   03  LINE  3 COLUMN  1 FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (1).
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (2).
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (3).
	   03  LINE  7 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (4).
	   03  LINE  8 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (5).
	   03  LINE  9 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (6).
	   03  LINE 10 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (7).
	   03  LINE 11 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (8).
	   03  LINE 12 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (9).
	   03  LINE 13 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (10).
	   03  LINE 14 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (11).
	   03  LINE 15 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (12).
	   03  LINE 16 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (13).
	   03  LINE 17 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (14).
	   03  LINE 18 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (15).
	   03  LINE 19 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (16).
	   03  LINE 20 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (17).
	   03  LINE 21 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (18).
	   03  LINE 22 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (19).
	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA (20).

       01  SD-RIGHT.
	   03  LINE  3 COLUMN  1 FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (1).
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (2).
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (3).
	   03  LINE  7 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (4).
	   03  LINE  8 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (5).
	   03  LINE  9 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (6).
	   03  LINE 10 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (7).
	   03  LINE 11 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (8).
	   03  LINE 12 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (9).
	   03  LINE 13 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (10).
	   03  LINE 14 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (11).
	   03  LINE 15 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (12).
	   03  LINE 16 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (13).
	   03  LINE 17 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (14).
	   03  LINE 18 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (15).
	   03  LINE 19 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (16).
	   03  LINE 20 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (17).
	   03  LINE 21 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (18).
	   03  LINE 22 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (19).
	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB (20).

       01  SD-RANDOM.
	   03  LINE  3 COLUMN  1 FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (1).
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (2).
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (3).
	   03  LINE  7 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (4).
	   03  LINE  8 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (5).
	   03  LINE  9 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (6).
	   03  LINE 10 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (7).
	   03  LINE 11 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (8).
	   03  LINE 12 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (9).
	   03  LINE 13 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (10).
	   03  LINE 14 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (11).
	   03  LINE 15 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (12).
	   03  LINE 16 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (13).
	   03  LINE 17 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (14).
	   03  LINE 18 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (15).
	   03  LINE 19 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (16).
	   03  LINE 20 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (17).
	   03  LINE 21 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (18).
	   03  LINE 22 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (19).
	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE (20).

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS-SCREEN-PRINT
				L02-PRINTER-DETAILS.
010250 AA000-MAIN      SECTION.
010260 AA000-INIT.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-SCREEN-PRINT
				 TO WS-SCREEN-PRINT.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       AA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       AA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AA00-READ-APACFIDS-EXIT.
	     EXIT.

       AA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	   IF WS-COMMAND = "I"
	       PERFORM OPEN-SCREEN THRU OPEN-SCREEN-EXIT
	       MOVE WS-SCREEN-PRINT
				 TO LS-SCREEN-PRINT
	       GO TO AA999.
	   IF NOT (L02-PRN-STATUS = "O")
	       PERFORM SAVE-SCREEN-3.
	     PERFORM PRINT-SCREEN.
	   IF L02-PRN-STATUS = "C"
	       PERFORM RESTORE-SCREEN-3.
	     MOVE WS-SCREEN-PRINT
				 TO LS-SCREEN-PRINT.

       AA999.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

011750 COPY PARAM.RD.

       PRINT-SCREEN    SECTION.
       PRINT-STATUS-TEST.
      *
      *    ****   O P E N   T H E   P R I N T E R   F I L E
      *
	   IF WS-COMMAND = "O"
	       IF L02-PRN-STATUS = "C"
		   PERFORM OPEN-PRINT-FILE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT
	   ELSE
      *
      *    ****   C L O S E   T H E   P R I N T E R   F I L E
      *
	   IF WS-COMMAND = "C"
	       IF L02-PRN-STATUS = "O"
		   PERFORM CLOSE-PRINT-FILE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT
	   ELSE
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
	   IF WS-COMMAND = "S"
	       IF L02-PRN-STATUS = "O"
		   PERFORM NEW-PAGE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT.
	     MOVE L02-PRN-LINE	TO REP-DETAIL1.
      *
      *    ****	  C H E C K   I F   P R I N T E R   F I L E   O P E N
      *
	   IF L02-PRN-STATUS = "C"
	       MOVE "Printer file not open"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PRINT-SCREEN-EXIT.
      *
      *    ****	  C H E C K   I F   P R I N T E R   O R   S C R E E N
      *
	   IF WS-PRINTER = "S"
	       PERFORM SCREEN-PRINT
	   ELSE
	       PERFORM LINE-PRINT-SPOOL.

       PRINT-SCREEN-EXIT.
	     EXIT.
      *
      *    ****   O P E N   P R I N T E R   /	S P O O L   F I L E
      *
       OPEN-PRINT-FILE	SECTION 50.
       OPEN-PRINTER.
	   IF L02-LINAGE = 99
	       OPEN INPUT PARAM.
	     MOVE "P"		 TO WS-OPTION.

       OPEN-PRINTER-NO.
	     MOVE SPACES	 TO WS-ERR-MES.
	   IF LS-PRINTER = "S"
	       STRING "'P'rinter or 'D'isk  ["
		      DELIMITED SIZE
		      WS-OPTION DELIMITED SIZE
		      "]" DELIMITED SIZE
				 INTO WS-ERR-MES
	   ELSE
	       STRING "'P'rinter,'S'creen or 'D'isk  ["
		      DELIMITED SIZE
		      WS-OPTION DELIMITED SIZE
		      "]" DELIMITED SIZE
				 INTO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	   IF LS-PRINTER = "S"
	       IF NOT (WS-OPTION = "P" OR "D")
		   GO TO OPEN-PRINTER-NO
	       END-IF
	   ELSE
	   IF NOT (WS-OPTION = "P" OR "D" OR "S")
	       GO TO OPEN-PRINTER-NO.
	     MOVE WS-OPTION	 TO WS-PRINTER.
	   IF WS-OPTION = "S"
	       PERFORM OPEN-SCREEN THRU OPEN-SCREEN-EXIT
	       MOVE 62		 TO L02-PRN-LENGTH
	       MOVE 66		 TO L02-PGE-LENGTH
	       GO TO OPEN-PRINTER-STATUS.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
	       END-IF
	   ELSE
	       MOVE 110		 TO WS-USUB.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF WS-REPORT = 1
	       PERFORM OPEN-REPORT-1
	   ELSE
	   IF WS-REPORT = 2
	       PERFORM OPEN-REPORT-2
	   ELSE
	   IF WS-REPORT = 3 OR 4 OR 5 OR 6
	       PERFORM OPEN-REPORT-3
	   ELSE
	   IF WS-REPORT = 7
	       PERFORM OPEN-REPORT-4
	   ELSE
	   IF WS-REPORT = 8
	       PERFORM OPEN-REPORT-5
	   ELSE
	   IF WS-REPORT = 9
	       PERFORM OPEN-REPORT-6.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the printer being used is a USB printer or a printer   ³
      *    ³ being accessed via the WINDOWS spool routine then the in- ³
      *    ³ line printer control characters are not used.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W02-PRINTER = "USB/WINDOWS"
	       GO TO OPEN-WIN-USB-PRINTER.
	     DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
				 TO WS-EXPP W08-EXPD.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH
				    L02-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH
				    L02-PGE-LENGTH.
	     MOVE TRM-SINGLE(WS-S6)
				 TO WS-PAPER.
	   IF TRM-PRN-TYPE (WS-S6) = "H"
	       MOVE 77		 TO WS-SCREEN-LIN
	       MOVE WS-COND	 TO WS-ECAN
	       MOVE 048		 TO W08-DEC (4)
	       MOVE 056		 TO W08-DEC (5)
	       MOVE W08-EXPD	 TO WS-EXPP.
	   IF WS-PRINTER = "D"
	       MOVE "WS"	 TO W02-WS
	       MOVE "_"		 TO W02-US W02-US2
	       MOVE LS-USER	 TO W02-TERM
	       MOVE WS-REPORT	 TO W02-REP
	       MOVE "."		 TO W02-P
	       MOVE LS-SYS-ID	 TO W02-USER
	       MOVE 9		 TO WS-PRN-NO.
	   IF WS-PRN-NO < 3
	       PERFORM CHECK-PRINTER THRU CHECK-PRINTER-EXIT
	       IF WS-ERROR = 1
		   MOVE "D"	TO WS-OPTION
		   GO TO OPEN-PRINTER-NO
	       END-IF
	   ELSE
	       MOVE "Waiting for printer response"
				 TO WS-ERR-MES
	       PERFORM OPT-CURSOR THRU OPT-REPLY.
	   IF L02-PRN-TYPE = 8
      *
      *    ****    SET PRINTER TO 8 LPI
      *
	       COMPUTE WS-PGE-LENGTH = WS-PGE-LENGTH / 6 * 8
	       MOVE WS-PGE-LENGTH
				 TO L02-PGE-LENGTH
	       IF WS-PGE-LENGTH > 88
		   SUBTRACT 7 FROM WS-PGE-LENGTH
				 GIVING WS-PRN-LENGTH
					L02-PRN-LENGTH
	       ELSE
		   SUBTRACT 6 FROM WS-PGE-LENGTH
				 GIVING WS-PRN-LENGTH
					L02-PRN-LENGTH.
	   IF NOT (WS-LINAGE = ZERO OR WS-PGE-LENGTH)
	       COMPUTE WS-PRN-LENGTH = WS-LINAGE -
				      (WS-PGE-LENGTH - WS-PRN-LENGTH)
	       MOVE WS-LINAGE	 TO WS-PGE-LENGTH L02-PGE-LENGTH
	       MOVE WS-PRN-LENGTH
				 TO L02-PRN-LENGTH.
	   IF WS-PRINTER = "D"
	       OPEN EXTEND PRNSPL
	   ELSE
019890	       OPEN OUTPUT PRNREP.
	     PERFORM RESTORE-SCREEN.
	     CALL X"91" USING X91-RES X91-FUN PRNSPL.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
	   IF WS-PRINTER = "D"
	       MOVE SPACES	 TO SPL-DETAIL1
	       MOVE WS-PGE-LENGTH
				 TO SPL-LIN
	       MOVE ZERO	 TO SPL-ADV
	       WRITE SPL-L1 BEFORE 0 LINES
	       MOVE ZERO	 TO SPL-LIN.
	   IF L02-PRN-TYPE = 8
      *
      *    ****    SET PRINTER TO 8 LPI
      *
	       MOVE WS-8LPI	 TO REP-DETAIL1
	       IF WS-PRINTER = "D"
		   MOVE ZERO	 TO SPL-ADV
		   MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		   WRITE SPL-L1
	       ELSE
011380		   WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	   IF (WS-PGE-LENGTH > 66 AND < 88) OR
	      (WS-PGE-LENGTH > 88)
	       IF WS-PRINTER = "D"
		   MOVE 2	 TO SPL-ADV
		   MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		   WRITE SPL-L1
	       ELSE
		   WRITE REP-LINE1 BEFORE 2 LINES
	       END-IF
	   ELSE
	       IF WS-PRINTER = "D"
		   MOVE ZERO	 TO SPL-ADV
		   MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		   WRITE SPL-L1
	       ELSE
		   WRITE REP-LINE1 BEFORE 0 LINES.
	     GO TO OPEN-PRINTER-STATUS.

       OPEN-WIN-USB-PRINTER.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
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
	   IF WS-REPORT = 1
	     move 17		 to title-len.
	     move "INVF12W - Invoice" & x"00"
                                 to title-text.
	     call "PC_PRINTER_OPEN" using by reference prn-handle
                                          by reference document-title
                                          by value prn-flags
                                          by value 0
	     end-call.

       OPEN-REPORT-1.
	   IF TRM-PICP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-PICP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-PICP(WS-S6) = 8
	       MOVE "USB/WINDOWS"
				 TO W02-PRINTER
           ELSE
	   IF TRM-PICP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN1 (WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-2.
	   IF TRM-INVP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-INVP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-INVP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-INVP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-INVP(WS-S6) = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
	   IF TRM-INVP(WS-S6) = 8
	       MOVE "USB/WINDOWS"
				 TO W02-PRINTER
           ELSE
	   IF TRM-INVP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN2 (WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-3.
	   IF TRM-STDP(WS-S6) = 1 OR 9
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-STDP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-STDP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-STDP(WS-S6) = 4
	       MOVE "COM1"	 TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 8
	       MOVE "USB/WINDOWS"
				 TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-4.
	   IF TRM-STMP(WS-S6) = 1
	       MOVE "LPT1"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 2
	       MOVE "LPT2"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 3
	       MOVE "LPT3"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 4
	       MOVE "COM1"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 8
	       MOVE "USB/WINDOWS"
				 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN4 (WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-5.
	   IF TRM-CSHP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-CSHP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-CSHP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-CSHP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-CSHP(WS-S6) = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
	   IF TRM-CSHP(WS-S6) = 8
	       MOVE "USB/WINDOWS"
				 TO W02-PRINTER
           ELSE
	   IF TRM-CSHP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN5 (WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-6.
	   IF TRM-POS(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 8
	       MOVE "USB/WINDOWS"
				 TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN6 (WS-S6)
                                 GIVING W25-RESULT.

       CHECK-PRINTER.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO CHECK-PRINTER-EXIT.

       PRINTER-NOT-READY.
	     MOVE "Check printer! - 'R'etry or 'A'bort  [ ] "
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "A")
	       GO TO PRINTER-NOT-READY.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO CHECK-PRINTER-EXIT.
	       GO TO CHECK-PRINTER.

       CHECK-PRINTER-EXIT.
	     EXIT.

       OPEN-SCREEN.
	     MOVE 1		 TO WS-S8.

       OPEN-SCREEN-LOOP.
	     MOVE SPACES	 TO W05-LINE (WS-S8).
	   IF WS-S8 < 20
	       ADD 1		 TO WS-S8
	       GO TO OPEN-SCREEN-LOOP.
	     MOVE ZERO		 TO WS-SCREEN-LIN.
	     MOVE 1		 TO WS-SCREEN
				    WS-LEFT.
	     MOVE 78		 TO WS-RIGHT.

       OPEN-SCREEN-EXIT.
	     EXIT.

       OPEN-PRINTER-STATUS.
	     MOVE SPACES	 TO L02-PRN-LINE.
	     MOVE "O"		 TO L02-PRN-STATUS.
	   IF L02-LINAGE = 99
	       CLOSE PARAM.

       OPEN-PRINTER-EXIT.
	     EXIT.

       CLOSE-PRINT-FILE  SECTION 51.
       CLOSE-PRINTER.
	   IF WS-PRINTER = "S"
	       GO TO CLOSE-PRINTER-STATUS.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	   IF WS-PRINTER = "D"
	       MOVE ZERO	 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
	       WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE WS-6LPI	 TO REP-DETAIL1.
	   IF WS-PRINTER = "D"
	       MOVE ZERO	 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
	       WRITE REP-LINE1 BEFORE 0 LINES.
	   IF WS-PRINTER = "D"
	       CLOSE PRNSPL
	   ELSE
	       CLOSE PRNREP.

       CLOSE-PRINTER-STATUS.
	     MOVE "C"		 TO L02-PRN-STATUS.

       CLOSE-PRINTER-EXIT.
	     EXIT.
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
       NEW-PAGE        SECTION 52.
       SKIP-TO-NEW-PAGE.
	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   I G N O R E	S K I P   I F	S C R E E N   I S
      * 	  S E L E C T E D
      *
	   IF WS-PRINTER = "S"
	       GO TO NEW-PAGE-EXIT.
	   IF WS-PRINTER = "D"
	       MOVE "PAGE"	 TO SPL-CONT
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	       MOVE ZERO	 TO SPL-LIN
	   ELSE
028700	       WRITE REP-LINE1 BEFORE PAGE.
	   IF WS-COMMAND NOT = "E"
	       IF (WS-PGE-LENGTH > 66 AND < 88) OR
		  (WS-PGE-LENGTH > 88)
		   IF WS-PRINTER = "D"
		       MOVE 2	 TO SPL-ADV
		       MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		       WRITE SPL-L1
		   ELSE
		       WRITE REP-LINE1 BEFORE 2 LINES.

       NEW-PAGE-EXIT.
	    EXIT.

       LINE-PRINT-SPOOL	 SECTION 53.
       PRINT-A-LINE.
	   IF WS-ADVANCE = 0
	       PERFORM PRINT-ZERO
	   ELSE
	   IF WS-ADVANCE = 1
	       PERFORM PRINT-SINGLE
	   ELSE
	   IF WS-ADVANCE = 2
	       PERFORM PRINT-DOUBLE
	   ELSE
	   IF WS-ADVANCE = 99
	       PERFORM PRINT-PAGE
	   ELSE
	       MOVE WS-ADVANCE	 TO WS-LINES
	       PERFORM PRINT-ADVANCE.
	     MOVE LINAGE-COUNTER TO L02-LINAGE.
	     GO TO LINE-PRINT-EXIT.

       PRINT-ZERO.
	   IF L02-PRN-TYPE = "C"
	       MOVE WS-COND	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "E"
	       MOVE WS-EXPP	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "X"
	       MOVE WS-ECAN	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "1"
	       MOVE WS-10CPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "2"
	       MOVE WS-12CPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "3"
	       MOVE WS-17CPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "6"
	       MOVE WS-6LPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "8"
	       MOVE WS-8LPI	 TO REP-DETAIL1.
	   IF WS-PRINTER = "D"
	       MOVE ZERO	 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 0 LINES.

       PRINT-SINGLE.
	   IF WS-PRINTER = "D"
	       MOVE 1		 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 1 LINES.

       PRINT-DOUBLE.
	   IF WS-PRINTER = "D"
	       MOVE 2		 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-PAGE.
	   IF WS-PRINTER = "D"
	       MOVE "PAGE"	 TO SPL-CONT
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	       MOVE ZERO	 TO SPL-LIN
	   ELSE
028700	       WRITE REP-LINE1 BEFORE PAGE.
	   IF WS-COMMAND NOT = "E"
	       IF (WS-PGE-LENGTH > 66 AND < 88) OR
		  (WS-PGE-LENGTH > 88)
		   MOVE SPACES	 TO REP-DETAIL1
		   IF WS-PRINTER = "D"
		       MOVE 2	 TO SPL-ADV
		       MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		       WRITE SPL-L1
		   ELSE
		       WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-ADVANCE.
	   IF WS-PRINTER = "D"
	       MOVE 1		 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     SUBTRACT 1		 FROM WS-LINES.
	   IF WS-LINES > ZERO
	       GO TO PRINT-ADVANCE.

       LINE-PRINT-EXIT.
	     EXIT.

       SCREEN-PRINT    SECTION 54.
       SCREEN-PRINT-INIT.
	     MOVE WS-SCREEN-LIN  TO WS-S8.
	   IF WS-SCREEN-LIN < 20
	       ADD 1		 TO WS-S8
	   ELSE
	       GO TO SCREEN-PRINT-SCROLL.

       DISPLAY-A-PRINT-LINE.
	     MOVE L02-PRN-TYPE	 TO W05-TYPE (WS-S8).
	     MOVE L02-PRN-LINE	 TO W05-DETAIL (WS-S8).
	   IF WS-LEFT > 1 AND < 60
	       PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP
	   ELSE
	   IF WS-SCREEN = 1
	       DISPLAY SD-LEFT
	   ELSE
	       DISPLAY SD-RIGHT.
	     GO TO SCREEN-PRINT-CHECK.

       SCREEN-SHIFT-LEFT.
	   IF WS-LEFT > 1
	       SUBTRACT 1	 FROM WS-LEFT
				      WS-RIGHT.
	       PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-RIGHT.
	   IF WS-RIGHT < 136
	       ADD 1		 TO WS-LEFT
				    WS-RIGHT.
	       PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-DATA.
	     MOVE 1		 TO WS-S1 WS-S2.
	     MOVE WS-LEFT	 TO WS-S3.

       SCREEN-SHIFT-LOOP.
	     MOVE W05-CHAR (WS-S1, WS-S3)
				 TO W06-CHAR (WS-S1, WS-S2).
	   IF WS-S2 < 78
	       ADD 1		 TO WS-S2 WS-S3
	       GO TO SCREEN-SHIFT-LOOP.
	   IF WS-S1 < 20
	       ADD 1		 TO WS-S1
	       MOVE 1		 TO WS-S2
	       MOVE WS-LEFT	 TO WS-S3
	       GO TO SCREEN-SHIFT-LOOP.
	     DISPLAY SD-RANDOM.

       SCREEN-PRINT-SCROLL.
	     MOVE 1		 TO WS-S7.
	     MOVE 2		 TO WS-S8.

       SCREEN-PRINT-LOOP.
	     MOVE W05-LINE (WS-S8)
				 TO W05-LINE (WS-S7).
	   IF WS-S8 < 20
	       ADD 1		 TO WS-S7 WS-S8
	       GO TO SCREEN-PRINT-LOOP.
	     GO TO DISPLAY-A-PRINT-LINE.

       SCREEN-PRINT-CHECK.
	     MOVE WS-S8 	 TO WS-SCREEN-LIN.
	     MOVE SPACES	 TO L02-PRN-LINE.
	   IF WS-COMMAND = "E"
	       GO TO SCREEN-CONTROL.
	   IF (WS-S8 = 20) AND (LS-ADVANCE = WS-ADVANCE)
	       PERFORM SCREEN-CONTROL THRU SCREEN-CONTROL-EXIT
	       IF (WS-COMMAND = "Z") OR (WS-ADVANCE = 1)
		   GO TO SCREEN-PRINT-EXIT.
	   IF WS-ADVANCE > 1
	       SUBTRACT 1	 FROM WS-ADVANCE
	       GO TO SCREEN-PRINT-INIT.
	   IF WS-SCREEN-LIN < 20
	       GO TO SCREEN-PRINT-EXIT.

       SCREEN-CONTROL.
	     DISPLAY "L" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "eft, "
		     "R" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "ight, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     X"1B201A" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "," WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "ext, " WITH FOREGROUND-COLOR 1
				  BACKGROUND-COLOR 3
		     "P" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "age or " WITH FOREGROUND-COLOR 1
				    BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       SCREEN-CONTROL-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "Z"	 TO WS-COMMAND
		     GO TO SCREEN-CONTROL-EXIT
		 WHEN PAGE-DOWN
		     GO TO SCREEN-CLEAR
		 WHEN OTHER	 CALL X"E5"
		     GO TO SCREEN-CONTROL-OPT
	       END-EVALUATE
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN LEFT-KEY
		     PERFORM SCREEN-SHIFT-LEFT
		     GO TO SCREEN-CONTROL-OPT
		 WHEN RIGHT-KEY
		     PERFORM SCREEN-SHIFT-RIGHT
		     GO TO SCREEN-CONTROL-OPT
		 WHEN DOWN-KEY
		     GO TO SCREEN-CONTROL-EXIT
		 WHEN OTHER	 CALL X"E5"
		     GO TO SCREEN-CONTROL-OPT
	   ELSE
	       GO TO SCREEN-CONTROL-OPT.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "L" OR "N" OR "R" OR "P")
	       GO TO SCREEN-CONTROL-OPT.
	   IF WS-OPTION = "R"
	       MOVE 2		 TO WS-SCREEN
	       MOVE 59		 TO WS-LEFT
	       MOVE 136 	 TO WS-RIGHT
	   ELSE
	   IF WS-OPTION = "L"
	       MOVE 1		 TO WS-SCREEN
				    WS-LEFT
	       MOVE 78		 TO WS-RIGHT
	   ELSE
	   IF WS-OPTION = "P"
	       GO TO SCREEN-CLEAR
	   ELSE
	       DISPLAY CLEAR-L25
	       GO TO SCREEN-CONTROL-EXIT.
	   IF WS-SCREEN = 1
	       DISPLAY SD-LEFT
	   ELSE
	       DISPLAY SD-RIGHT.
	     GO TO SCREEN-CONTROL-OPT.

       SCREEN-CLEAR.
	     MOVE 1		 TO WS-S8.

       SCREEN-CLEAR-LOOP.
	     MOVE SPACES	 TO W05-LINE (WS-S8).
	   IF WS-S8 < 20
	       ADD 1		 TO WS-S8
	       GO TO SCREEN-CLEAR-LOOP.
	     MOVE ZERO		 TO WS-SCREEN-LIN.

       SCREEN-CONTROL-EXIT.
	     EXIT.

       SCREEN-PRINT-EXIT.
	     EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 91.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.

       COPY DISPERR.PRO.
