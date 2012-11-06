      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ****    *******    ******   **     **  ********  **	 **  *
      *   **  **   **	 **  **    **  ***   ***  **	      ***    **  *
      *  **    **  **    **  **        ** * * **  **        ** *   **  *
      *  ********  *******   **	   **  *  **  *****     **  *  **  *
      *  **    **  **	     **        **     **  **	      **   * **  *
      *  **    **  **	     **    **  **     **  **	      **    ***  *
      *  **    **  **	      ******   **     **  *******   **	 **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     B U S I N E S S   S Y S T E M   M E N U   P R O G R A M    *
      *                                                                *
      *     VERSION 9.00.00 - August 2012	                             *
      *                                                                *
      ******************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    VERSIONS OF THE APAC SYSTEM	           ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³ While developing this system the requirements and budgets ³
      *    ³ (originally this was a propriety package) for various     ³
      *    ³ businesses necessitated that at least 4 separate systems  ³
      *    ³ be produced. As the maintenance and amendments to the 4   ³
      *    ³ different systems would require man hours that would not  ³
      *    ³ enable me to do much development, it was decided to use   ³
      *    ³ one system with the various requirements controlled by    ³
      *    ³ the various menu programs using a version indicator. This ³
      *    ³ indicator is located in the second record of the SYSTEM   ³
      *    ³ file SEC-LEV, which is in the SEC-MODULES. This indicator ³
      *    ³ together with all the other indicators in SEC-MODULES, is ³
      *    ³ moved to WS0-MODULES in BUP000 or BUP001 and then passed  ³
      *    ³ via the LINKAGE SECTION from program to program in  the   ³
      *    ³ LS0-MODULES group. The other indicators in this group are ³
      *    ³ used to show which modules of the system are installed.   ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³    SYSTEM FILE    ³      CHAIN.WS	   ³	   CHAIN.LS      ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³  03  SEC-MODULES. ³  03  WS0-MODULES. ³  03  LS0-MODULES. ³
      *    ³      05  SEC-DTP  ³      05  WS0-DTP  ³	    05  LS0-DTP  ³
      *    ³      05  SEC-STP  ³      05  WS0-STP  ³	    05  LS0-STP  ³
      *    ³      05  SEC-HPD  ³      05  WS0-HPD  ³	    05  LS0-HPD  ³
      *    ³      05  SEC-CRP  ³      05  WS0-CRP  ³	    05  LS0-CRP  ³
      *    ³      05  SEC-GLP  ³      05  WS0-GLP  ³	    05  LS0-GLP  ³
      *    ³      05  SEC-POS  ³      05  WS0-POS  ³	    05  LS0-POS  ³
      *    ³      05  SEC-JCP  ³      05  WS0-JCP  ³	    05  LS0-JCP  ³
      *    ³      05  SEC-VHP  ³      05  WS0-VHP  ³	    05  LS0-VHP  ³
      *    ³      05  SEC-WAG  ³      05  WS0-WAG  ³	    05  LS0-WAG  ³
      *    ³      05  SEC-SAL  ³      05  WS0-SAL  ³	    05  LS0-SAL  ³
      *    ³      05  SEC-LEV  ³      05  WS0-LEV  ³	    05  LS0-LEV  ³
      *    ³      05  SEC-WHS  ³      05  WS0-WHS  ³	    05  LS0-WHS  ³
      *    ³      05  SEC-RTE  ³      05  WS0-RTE  ³	    05  LS0-RTE  ³
      *    ³      05  FILLER   ³		         ³		           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     APACTERM.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   APRIL 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2012
				     by James William Lemmon.

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

000130 COPY "CONTROL.SL".

000190 COPY "DEPART.SL".

000210 COPY "PARAM.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY "APACFIDS.FDE".

000360 COPY "CONTROL.FDE".

000420 COPY "DEPART.FDE".

000440 COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC  X(18)    VALUE "aeWlimemnomLalismJ".
000610 77  WS-S1           PIC  9(04)    COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
000820 77  WS-NETKEY	   PIC  9(04)    COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TEMP-LIN	   PIC  9(02)    COMP-5.
       77  WS-OPTION	   PIC  X(01).
       77  PRG-NAME	   PIC  X(12)    VALUE SPACES.
       77  WS-NUM          PIC  Z9.
       77  APAC-SYSTEM	   PIC  X(12).
       77  APAC-VERSION    PIC  X(04).

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
	   03  CRT-TYPE      PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
	   03  CRT-START     PIC  9(02).
	   03  CRT-END	   PIC  9(02).
      *
      *    Calling Program
      *
	   03  CRT-PROGRAM   PIC  X(15) VALUE "APACTERM".
      *
      *    Screen Heading
      *
	   03  CRT-HEADING   PIC  X(40) VALUE "WORKSTATION MENU".
      *
      *    Company Name
      *
	   03  CRT-COMPANY   PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
         03  CRT-WRKHD     PIC  X(11).
         03  CRT-WRKST     PIC  X(03).

001350 01  WS-CRT-LINES.
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
001400	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".

001840 COPY "WS.WS".

001860 COPY "CHAIN.WS".

002040 01  W02-FID.

       COPY "APACFIDS.ID".

002060 COPY "CONTROL.ID".

002120 COPY "DEPART.ID".

       COPY "PARAM.ID".

       COPY "SHARED.ID".

       COPY "TXTRAN.ID".

001160 01  W05-COMMAND.
001170	   03  W05-CHAR        PIC  X(128).

       COPY "W12.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

004540 COPY "W50.WS".

      /
000750 SCREEN SECTION.

       01  MENU-INSTRUCT.
	     03  BACKGROUND-COLOR Cyan.
	         05  LINE 25 COLUMN  2 FOREGROUND-COLOR Blue	          VALUE "Use ".
	         05          COLUMN  6 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "".
	         05          COLUMN  7 FOREGROUND-COLOR Blue	          VALUE " or ".
	         05          COLUMN 11 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "".
	         05          COLUMN 12 FOREGROUND-COLOR Blue	          VALUE " to point to an item, and ".
	         05          COLUMN 38 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "<Enter>".
	         05          COLUMN 45 FOREGROUND-COLOR Blue	          VALUE " to Select ".

000850 01  S-PROG.
000860     03  LINE  2 COLUMN 33 BACKGROUND-COLOR Blue FOREGROUND-COLOR Grey HIGHLIGHT VALUE "APAC MAIN MENU".

       01  S-MENU.
           03  BACKGROUND-COLOR Red.
	         05  LINE  3 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	         05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "¿".
	         05  LINE  4 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³  ".
	         05          COLUMN 33 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "PROGRAM  CONTROL".
	         05          COLUMN 49 FOREGROUND-COLOR Black           VALUE "  ³".
	         05  LINE  5 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
	         05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "1".
	         05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Stock      	  ".
	         05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
	         05  LINE  6 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
	         05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "2".
	         05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Creditors  	  ".
	         05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
001470         05  LINE  7 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
	         05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "3".
	         05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Debtors  	  ".
	         05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
001570         05  LINE  8 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "4".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " General Ledger   ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
001500         05  LINE  9 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "5".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Sales / Orders   ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 10 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "6".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Point-of-Sale    ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 11 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "7".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Job Costing      ".
               05          COLUMN 51 FOREGROUND-COLOR Black 	    VALUE "³".
               05  LINE 12 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "8".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Period End/Dates ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 13 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "A".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Utilities        ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 14 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "C".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Switch Companies ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 15 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "D".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Print Spool File ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 16 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "F".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Orders Spec.     ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 17 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "L".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " Login/Logout     ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 18 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "³ ".
               05          COLUMN 32 FOREGROUND-COLOR Grey  HIGHLIGHT VALUE "9".
               05          COLUMN 33 FOREGROUND-COLOR Grey            VALUE " End System       ".
               05          COLUMN 51 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 19 COLUMN 30 FOREGROUND-COLOR Red   HIGHLIGHT VALUE "À".
               05          COLUMN 31 FOREGROUND-COLOR Black           VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

002480 01  S98.
           03  BACKGROUND-COLOR Red.
               05  LINE 11 COLUMN 14 FOREGROUND-COLOR Red	HIGHLIGHT VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
               05          COLUMN 66 FOREGROUND-COLOR Black           VALUE "¿".
002490         05  LINE 12 COLUMN 14 FOREGROUND-COLOR Red	HIGHLIGHT VALUE "³".
               05          COLUMN 15 FOREGROUND-COLOR Grey	HIGHLIGHT VALUE "Do not forget to make security copies of your files".
               05          COLUMN 66 FOREGROUND-COLOR Black           VALUE "³".
               05  LINE 13 COLUMN 14 FOREGROUND-COLOR Red	HIGHLIGHT VALUE "À".
               05	         COLUMN 15 FOREGROUND-COLOR Black           VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
               05  LINE 24 COLUMN 79 BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan VALUE "Ä".

002480 01  S98T.
           03  BACKGROUND-COLOR Red.
               05  LINE 11 COLUMN 22 FOREGROUND-COLOR Red	HIGHLIGHT VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
               05	         COLUMN 59 FOREGROUND-COLOR Black	          VALUE "¿".
002490	   05  LINE 12 COLUMN 22 FOREGROUND-COLOR Red	HIGHLIGHT VALUE "³".
002490	   05	         COLUMN 23 FOREGROUND-COLOR Grey	HIGHLIGHT VALUE "The terminal may now be switched off".
               05	         COLUMN 59 FOREGROUND-COLOR Black	          VALUE "³".
               05  LINE 13 COLUMN 22 FOREGROUND-COLOR Red	HIGHLIGHT VALUE "À".
               05	         COLUMN 23 FOREGROUND-COLOR Black	          VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
               05  LINE 24 COLUMN 79 BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan VALUE "Ä".

      /
010570 PROCEDURE DIVISION.
       AA000	       SECTION.
       AA00.
             ACCEPT W05-COMMAND FROM COMMAND-LINE.
             PERFORM ZA000-INIT.
           IF WS0-CRED = "N" AND WS0-STOCK = "N" AND WS0-DEBT = "N" AND WS0-JOBS = "N"  AND WS0-PARM = "N" AND WS0-WAGES = "N" AND WS0-GL = "N"
               GO TO AA10.
             DISPLAY S-PROG.
             PERFORM BA000.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             MOVE "S"                TO CRT-TYPE.
007750       CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
           IF WS-USER NUMERIC
               MOVE 12           TO SHADE-ROW
               MOVE 24           TO SHADE-COL
               MOVE 36           TO SHADE-WIDTH
               MOVE 2            TO SHADE-LINES
007820         DISPLAY S98T
               PERFORM SCREEN-SHADOW
               STOP RUN.
       AA05.
             OPEN I-O PARAM.
038210     IF RUNTIME-ERROR
038220         IF FLE-LOCKED OR FLE-LIMIT
                   DISPLAY "WAITING" AT 2502 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
038230             GO TO AA05.
038270     IF WS-STATUS NOT = "00"
038280         MOVE 15           TO WS-F-ERROR
038290         PERFORM OPEN-ERROR.
             MOVE 4              TO WS-PARKEY.
007710       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
007780       MOVE ZERO           TO PAR-LOGON.
007790       REWRITE PAR-RECORD1.
007800       UNLOCK PARAM.
007810       CLOSE PARAM.
             MOVE 12             TO SHADE-ROW.
             MOVE 13             TO SHADE-COL.
             MOVE 51             TO SHADE-WIDTH.
             MOVE 2              TO SHADE-LINES.
007820       DISPLAY S98.
             PERFORM SCREEN-SHADOW.
007830       STOP RUN.

       AA10.
             MOVE "MNU\POSMENU"  TO PRG-NAME.
             PERFORM AA100.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
007630       DISPLAY CLR-SCREEN.
             MOVE 12             TO SHADE-ROW.
             MOVE 24             TO SHADE-COL.
             MOVE 36             TO SHADE-WIDTH.
             MOVE 2              TO SHADE-LINES.
007820       DISPLAY S98T.
             PERFORM SCREEN-SHADOW.
             STOP RUN.

       AA100	       SECTION.
       AA101.
             PERFORM ZA00A THRU ZA999.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM SAVE-SCREEN-3.
             CALL PRG-NAME USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
		      ON EXCEPTION GO TO AA110
             END-CALL.
             CANCEL PRG-NAME.

       AA105.
	   IF WS-DEM NOT = 0 AND WS-DAYS > 65
	       CALL "UTP\SYS001" USING WS-PARID WS-USER-ID WS0-PROGRAMS
	       CANCEL "UTP\SYS001".
	     PERFORM RESTORE-SCREEN-3.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
013040	     CLOSE NETWORK
013050             DEPART
		   PARAM
		   SHARED
		   TXTRAN.
	     GO TO AA199.

       AA110.
	     MOVE SPACE 	 TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     GO TO AA105.

       AA199.
	     EXIT.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "LOCKED.RC2".

011433 COPY "CLEAR.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

011750 COPY "PARAM.RD".

       BA000	       SECTION 50.
       BA00.
	     MOVE 4		 TO SHADE-ROW.
	     MOVE 32		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 16		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	   IF WS0-POS = 0
	       DISPLAY "(N/A)" AT 1046 WITH FOREGROUND-COLOR Grey BACKGROUND-COLOR Red.
	   IF (WS0-LEV < 4) AND (WS0-JCP < 2)
	       DISPLAY "(N/A)" AT 1146 WITH FOREGROUND-COLOR Grey BACKGROUND-COLOR Red.
	   IF WS0-LEV < 3
	       DISPLAY " Switch Comp (N/A)" AT 1433 WITH FOREGROUND-COLOR Grey BACKGROUND-COLOR Red
	       DISPLAY "(N/A)" AT 1646 WITH FOREGROUND-COLOR Grey BACKGROUND-COLOR Red.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
             MOVE 4              TO TOP-ROW.
             MOVE 17             TO BOTTOM-ROW.
             MOVE 3              TO SCREEN-LIN.
             MOVE 32             TO SCREEN-COL.
             MOVE 18             TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
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
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < 1 OR > 9) AND
		  (WS-OPTION NOT = "A") AND
		  (WS-OPTION NOT = "C") AND
		  (WS-OPTION NOT = "D") AND
		  (WS-OPTION NOT = "F") AND
		  (WS-OPTION NOT = "L")
		   CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05.

       BA15.
	   IF SCREEN-LIN < 4
	       CALL X"E5"
	       GO TO BA05
	   ELSE
	   IF SCREEN-LIN = 4
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "A"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "C"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "D"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "F"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "L"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 17
	       MOVE "9"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "9"
	       MOVE 17		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 4		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 6		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "C"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "D"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "F"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "L"
	       MOVE 16		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	   IF WS-OPTION = "9"
	       CLOSE APACFIDS
	       GO TO BA999.
	   IF WS-DAYS > 94
	     DISPLAY "This package must be legalised - PHONE JW Lemmon -
      - 	     " 082 566 3095" AT 2302
		      WITH FOREGROUND-COLOR Red HIGHLIGHT
	     GO TO BA05.
	   IF WS-OPTION = "1"
	       MOVE "MNU\STPMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "MNU\CRPMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "MNU\DTPMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "MNU\GLPMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "MNU\SLPMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       IF WS0-POS = 0
		   MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       ELSE
		   MOVE "MNU\SALMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       IF (WS0-LEV < 4) AND (WS0-JCP < 2)
		   MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       ELSE
		   MOVE "MNU\JCPMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "MNU\REPMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE "MNU\UTILMENU"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "D"
	       MOVE "UTP\SPOOLPRN"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "F"
	       IF WS0-LEV < 3
		   MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       ELSE
		   MOVE "SLP\ORDSPC"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "L"
	       MOVE "UTP\USR001" TO PRG-NAME
	   ELSE
	   IF WS0-LEV < 3
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	   IF (WS0-DBLEV < 3) AND (WS0-STLEV < 3) AND
	      (WS0-GLLEV < 3) AND (WS0-CRLEV < 3)
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       GO TO BA20.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO BA05.
       BA20.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
007630	     DISPLAY CLR-SCREEN.

       BA25.
	     PERFORM ZA00A THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     OPEN I-O PARAM.
038210	   IF RUNTIME-ERROR
038220	       IF FLE-LOCKED OR FLE-LIMIT
		   DISPLAY "WAITING" AT 2502
			    WITH BACKGROUND-COLOR Cyan
				 FOREGROUND-COLOR Brown HIGHLIGHT
038230		   GO TO BA25.
038270     IF WS-STATUS NOT = "00"
038280         MOVE 15           TO WS-F-ERROR
038290         PERFORM OPEN-ERROR.
	     MOVE 3		 TO WS-PARKEY.
007710	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
007800	     UNLOCK PARAM.
007810	     CLOSE PARAM.
	     CLOSE APACFIDS.
	     MOVE "ANYBODY"	 TO WS0-NAME.
	     MOVE "X"		 TO WS-INSTR.
	     PERFORM ZA000-INIT.
	     DISPLAY S-PROG.
	     GO TO BA00.

       BA999.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
038140 ZA000-INIT	       SECTION 90.
038150 ZA000-OPEN.
	     CALL "MNU\BUP001"
		   USING WS-PARID WS-USER-ID WS0-PROGRAMS W05-COMMAND.
	     CANCEL "MNU\BUP001".
	     MOVE WS-APAC-VERSION
				 TO APAC-VERSION.
	     MOVE WS-APAC-SYSTEM TO APAC-SYSTEM.
	     MOVE TODAY-DDMMYY	 TO W12-TODAY.
	   IF APAC-END
	       DISPLAY CLR-SCREEN
	       STOP RUN.
	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     OPEN I-O APACFIDS.
038160	     PERFORM ZA60.
	     CALL "UTP\USR001"
		 USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY.
	     CANCEL "UTP\USR001".
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 2		 TO USER-ACTION.
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	   ELSE
	       GO TO ZA00.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.

       ZA00.
             DISPLAY CLR-SCREEN.
           IF (WS-DEM NOT = 0) AND (WS-DAYS > 65)
               COMPUTE WS-NUM = 15 - (WS-DAYS - 65)
               DISPLAY "This package must be registered within " AT 2202 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT
                       WS-NUM WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT BLINK " days" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
	     GO TO ZA999.
      *
      *    ****    P A R A M E T E R   F I L E
      *
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
038200	     OPEN I-O PARAM.
038210     IF RUNTIME-ERROR
038220         IF FLE-LOCKED
038230             GO TO ZA200
038240         ELSE
038250         IF FLE-LIMIT
038260             GO TO ZA49.
038270     IF WS-STATUS NOT = "00"
038280         MOVE 15           TO WS-F-ERROR
038290         PERFORM OPEN-ERROR.
038300       MOVE 3              TO WS-PARKEY.
038310       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      *    ****    G E T   F I L E   P A T H S	 A N D	 I D ' S
      *
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
038470	     OPEN I-O NETWORK.
038480     IF RUNTIME-ERROR
038490         IF FLE-LOCKED
038500             GO TO ZA200
038510         ELSE
038520         IF FLE-LIMIT
038530             GO TO ZA49.
038540     IF WS-STATUS NOT = "00"
038550         MOVE 2            TO WS-F-ERROR
038560         PERFORM OPEN-ERROR.
      *
      *    ****    S H A R E D	 F I L E
      *
038470	     OPEN I-O SHARED.
038480     IF RUNTIME-ERROR
038490         IF FLE-LOCKED
038500             GO TO ZA200
038510         ELSE
038520         IF FLE-LIMIT
038530             GO TO ZA49.
038540     IF WS-STATUS NOT = "00"
038550	       MOVE 37		 TO WS-F-ERROR
038560         PERFORM OPEN-ERROR.
038810	     OPEN I-O DEPART.
038820	   IF RUNTIME-ERROR
038830         IF FLE-LOCKED
038840             GO TO ZA200
038850         ELSE
038860         IF FLE-LIMIT
038870             GO TO ZA49.
038910     IF WS-STATUS NOT = "00"
038920	       MOVE 7		 TO WS-F-ERROR
038930         PERFORM OPEN-ERROR.
038670	     OPEN I-O TXTRAN.
038680     IF RUNTIME-ERROR
038690         IF FLE-LOCKED
038700             GO TO ZA200
038710         ELSE
038720         IF FLE-LIMIT
038730             GO TO ZA49
038740         ELSE
038750         IF IDX-CORRUPT
038760		   MOVE 32	 TO WS-F-ERROR
038770             GO TO ZA50.
038780     IF WS-STATUS NOT = "00"
038790	       MOVE 32		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
038800		   PERFORM OPEN-ERROR.
039770	     GO TO ZA999.

       COPY "ZA49.PRO".

039850 ZA50.
039860	   IF WS-F-ERROR = 32 OR 35
039870	       MOVE "TAX file"	 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990		      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

040020 ZA51.
040030       EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *

040090 ZA60.
040650	     MOVE WS-COMPANY	 TO WS-TOP-COMP.
	   IF WS-USER = WS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE WS-USER	   TO WS-WRKST.

040660 ZA200.

       COPY "LOCKED.PRO".

040730 ZA205.
040740       EXIT PROGRAM.

040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION  91.

       COPY "ERRORS.PRO".

040950 DISPLAY-FILE-NAME.
040960	   IF WS-F-ERROR = 2
040970         MOVE W02-NETWORK TO WS-FILE
040980         MOVE WS-NETKEY TO WS-KEY
041110	   ELSE
040960	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041110	   ELSE
041120	   IF WS-F-ERROR = 37
041130	       MOVE W02-SHARED	 TO WS-FILE
041140	       MOVE WS-SHARED	 TO WS-KEY.

       COPY "DISPERR.PRO".
