      $set LINKCOUNT"512" GNT"CRP006.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****      ****     ******    *
      *   **    **  **    **  **    **   **  **    **  **   **    **   *
      *   **        **    **  **    **  **    **  **    **  **         *
      *   **        ******    *******   **    **  **    **  *******    *
      *   **        **   **   **        **    **  **    **  **    **   *
      *   **    **  **    **  **         **  **    **  **   **    **   *
      *    ******   **    **  **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   M A I N T E N A N C E   P R O G R A M    *
      *                                                                *
      *     A G E   A C C O U N T S                                    *
      *                                                                *
      *       Version 9.00.80 - August 2015                            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CRP006.
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

       COPY CONTROL.SL.

       COPY CRDMEM.SL.

       COPY CREDIT.SL.

       COPY CRTRAN.SL.

           SELECT CRTOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION SEQUENTIAL.

       COPY PARAM.SL.

       COPY RECOVER.SL.

       COPY SHARED.SL.

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

       COPY CONTROL.FDE.

       COPY CRDMEM.FDE.

       COPY CREDIT.FDE.

       COPY CRTRAN.FDE.

       COPY CRTOLD.FD.

       COPY PARAM.FDE.

       COPY RECOVER.FD.

       COPY SHARED.FDE.

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
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-CTOKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-CTRKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CREKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  WS-RECKEY       PIC  9(04)    COMP-5.
       77  WS-RECOVER      PIC  9(04)    COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR        PIC  9(01) VALUE 0.
       77  WS-FC           PIC  9(01) VALUE 0.
       77  WS-SUB          PIC  9(04)    COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-IXS2         PIC  9(04)    COMP-5.
       77  WS-IXS3         PIC  9(04)    COMP-5.
       77  WS-IXS4         PIC  9(04)    COMP-5.
       77  WS-ENT          PIC  X(08).
       77  WS-ENT1         PIC  X(08).
       77  WS-KEYSTR       PIC  9(04)    COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-REST    PIC X(01).
       77  WS-NUM          PIC  Z9.
       77  WS-KEY1         PIC  9(04)    COMP-5.
       77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-INDS         PIC  9(01) VALUE 0.
       77  WS-INDC         PIC  9(01) VALUE 0.
       77  WS-SKIP    PIC X(01) VALUE " ".
       77  WS-CHAR1        PIC  X(01).
       77  WS-LIN          PIC  9(02) VALUE ZERO.
       77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
       77  WS-PAGE         PIC  9(04)    COMP-5.
       77  WS-SEASON       PIC  X(01).
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      /
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *        C =  Clear comment line (50)
      *        E =  Clear lines (any line or lines between 5 and 46)
      *        H =  Change heading
      *        S =  Clear the screen
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP006".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR - AGE ACCOUNTS".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY WS.WS.

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "CRP".
    03  WS-PROG        PIC  X(03) VALUE "006".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY W05.WS.

       COPY W15.CRP.

       01  W20-CALCS.
           03  W20-RESULT    PIC  9(06)V99.
           03  W20-RESULT-1  REDEFINES W20-RESULT.
               05  W20-WHOLE PIC  9(06).
               05  W20-DECIMAL
                             PIC  9(02).
           03  W20-ORD       PIC  9(07)    COMP-3.
    03  W20-DATE.
        05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
          PIC 9(08).

       01  W40-NADD.
           03  W40-ADD.
               05  W40-ACHAR PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS PIC  X(60).
               05  FILLER    PIC  X(10).
           03  W40-NAME.
               05  W40-NCHAR PIC  X(01) OCCURS 40.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

       COPY W70CRED.WS.

       01  W75-KEYS.
           03  W75-S         PIC  9(02) COMP-5.
           03  W75-S1        PIC  9(02) COMP-5.
    03  W75-CNO      PIC  X(06) OCCURS 18.

       01  W95-COMPANY.
           03  W95-COMP      PIC  X(40).

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

      $SET SOURCEFORMAT"VARIABLE"

       01  S12.
          03  LINE  2 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "BUSY WITH MONTH-END AGE".

       01  S39.
          03  LINE  4 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 4 VALUE
           " This routine does not include a recov
      -     "ery procedure. Security ".
          03  LINE  5 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 4 VALUE
           " copies  of the data files should be m
      -     "ade as a power  failure ".
          03  LINE  6 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 4 VALUE
           " or any other problem during this proc
      -     "edure would require the ".
          03  LINE  7 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 4 VALUE
           " DATA FILES from the previous backup t
      -     "o be restored !!!       ".
          03  LINE  9 COLUMN 10 VALUE
     " Do you wish to continue (Y/N)? ".

       01  S-CRE-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".

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
           03          COLUMN 78 FOREGROUND-COLOR 0
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

       COPY CRDLUP.CRT.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-START-UP  SECTION.
       AA000.
          IF LS0-CRLEV < 3
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM HA000.
            CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
      PERFORM SAVE-SCREEN.
            MOVE 4   TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM CREDITOR-LOOKUP
      PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     OPT-MESSAGE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.           ³
      *    ³              ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³              ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³        other keys are to be tested on exit)        ³
      *    ³        OR           ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)            ³
      *    ³              ³
      *    ³ This would be displayed as:          ³
      *    ³ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³ ³    Print transactions (Y/N) [ ]   ³°°   ³
      *    ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³              ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³              ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.         ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.REC.

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

       COPY APAC.HLP.

      /
      *    ****    READ FILES ROUTINES
      *
       AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CRDMEM.RD.

       COPY CREDIT.RD.

       COPY CRTRAN.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AC00-WRITE    SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY CRTRAN.WR.

       COPY PARAM.WR.

       COPY CREDIT.LUP.

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
            MOVE 01   TO REC-FILE.
            MOVE WS-PARKEY  TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-RECORD1  TO REC-PARAM.
             GO TO AY50.
       AY07.
             MOVE 07 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
            MOVE P-REC   TO REC-CREDIT.
             GO TO AY50.
       AY08.
            MOVE 08   TO REC-FILE.
            MOVE WS-CTRKEY  TO REC-KEY.
            MOVE CT-REC  TO REC-CRTRAN.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY40.
            MOVE 99   TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
            MOVE SPACES  TO REC-DETAIL.
             PERFORM AY50.
            ADD 1   TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
              MOVE ZERO  TO WS-RECOVER.
             GO TO AY59.
       AY50.
            ADD 1   TO WS-RECOVER.
            MOVE WS-RECOVER  TO WS-RECKEY.
            MOVE WS-TRANS  TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
               WITH FOREGROUND-COLOR 6 HIGHLIGHT
              DISPLAY WS-STATUS AT 2247
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
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
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 4 HIGHLIGHT.
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
    IF NOT(PAR-PROG(WS-SUB) = SPACES OR
    PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT(PAR-PROG(WS-SUB) = SPACES)
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
    IF NOT(PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT BLINK
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
    IF NOT(PAR-USR(WS-USUB) = LS-USER)
        ADD 1   TO WS-USUB
        GO TO AY72.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT(PAR-PROG(WS-SUB) = SPACES)
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
      /
       HA000      SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
      ADD 1 W05-MM  GIVING WS-S8.
           IF WS-S8 > 12
               MOVE 1            TO WS-S8.
            DISPLAY S12.
      PERFORM SAVE-SCREEN-3.
             DISPLAY S39.

       HA05.
             PERFORM TEST AFTER UNTIL WS-OPTION = "N" OR "Y"
                     ACCEPT WS-OPTION AT 0942 WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5 AUTO
                     CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
          IF WS-OPTION = "N"
               GO TO HA999.
            DISPLAY "Have BACKUP copies of files been made (Y/N)"
        AT 1112.

       HA10.
      PERFORM TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y"
              ACCEPT WS-OPTION AT 1156
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
          IF WS-OPTION = "N"
              GO TO HA999.
            DISPLAY "Have Remittance Advices been printed (Y/N)"
        AT 1312.

       HA15.
      PERFORM TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y"
              ACCEPT WS-OPTION AT 1355
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
          IF WS-OPTION = "N"
              GO TO HA999.
            DISPLAY "Are you sure (Y/N)" AT 1512.

       HA20.
      PERFORM TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y"
              ACCEPT WS-OPTION AT 1531
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
          IF WS-OPTION = "N"
              GO TO HA999.
      DISPLAY "Is this a restart (Y/N)" AT 1712.
      MOVE "N"   TO WS-REST.

       HA22.
      PERFORM TEST AFTER
       UNTIL WS-REST = "N" OR "Y"
              ACCEPT WS-REST AT 1736
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
        CALL "CBL_TOUPPER" USING WS-REST
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
    IF WS-REST = "Y"
        PERFORM HA90 THRU HA100.
      MOVE ZERO   TO W70-DUE-NOW W70-DUE-30
                                    W70-DUE-150 W70-DUE-60
                                    W70-DUE-180 W70-DUE-90
                                    W70-DUE-120 W70-OPEN
                                    W70-CRED    W70-CLOSE
                                    W70-DEBT
            PERFORM AY60 THRU AY999.
            MOVE "  !"   TO P-NUMBER.
            DISPLAY "Account :" AT 1920.
             PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
        GO TO HA40.

       HA25.
             PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
        GO TO HA40.
            DISPLAY P-NUMBER AT 1930
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
       " " P-NAME
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
    IF WS-REST = "Y"
        IF P-NUMBER < W15-ACNO
     ADD P-BAL  TO W70-OPEN
        W70-CLOSE
     ADD P-30D  TO W70-DUE-30
     ADD P-60D  TO W70-DUE-60
     ADD P-90D  TO W70-DUE-90
     ADD P-120D  TO W70-DUE-120
     ADD P-150D  TO W70-DUE-150
     ADD P-OUT  TO W70-DUE-NOW
     GO TO HA35
        ELSE
     MOVE "N"  TO WS-REST.
      *    IF OPEN-ITEM
      *        GO TO HA70.
            ADD P-30D   TO P-OUT.
             MOVE P-60D          TO P-30D.
             MOVE P-90D          TO P-60D.
             MOVE P-120D         TO P-90D.
             MOVE P-150D         TO P-120D.
             MOVE P-180D         TO P-150D.
            MOVE ZERO   TO P-180D P-DT P-CR.
             MOVE P-BAL          TO P-PBAL.
      COMPUTE P-OUT = P-BAL - (P-30D  + P-60D  + P-90D +
          P-120D + P-150D + P-180D).
    IF P-OUT < ZERO
        ADD P-OUT  TO P-30D
        MOVE ZERO  TO P-OUT.
    IF P-30D < ZERO
        ADD P-30D  TO P-60D
        MOVE ZERO  TO P-30D.
    IF P-60D < ZERO
        ADD P-60D  TO P-90D
        MOVE ZERO  TO P-60D.
    IF P-90D < ZERO
        ADD P-90D  TO P-120D
        MOVE ZERO  TO P-90D.
    IF P-120D < ZERO
        ADD P-120D  TO P-150D
        MOVE ZERO  TO P-120D.
    IF P-150D < ZERO
        ADD P-150D  TO P-180D
        MOVE ZERO  TO P-150D.
    IF P-180D < ZERO
        ADD P-180D  TO P-OUT
        MOVE ZERO  TO P-180D.
      ADD P-BAL   TO W70-OPEN
                                    W70-CLOSE.
             ADD P-30D           TO W70-DUE-30.
             ADD P-60D           TO W70-DUE-60.
             ADD P-90D           TO W70-DUE-90.
             ADD P-120D          TO W70-DUE-120.
             ADD P-150D          TO W70-DUE-150.
             ADD P-OUT           TO W70-DUE-NOW.
      MOVE P-NUMBER  TO CT-CRED.
      MOVE ZERO   TO CT-CRED
        CT-SDTE
        CT-CODE
        CT-SEQ.
      MOVE SPACES  TO CT-SINV.
      PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
    IF WS-F-ERROR = 4
        GO TO HA35.

       HA30.
            PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
    IF (WS-F-ERROR = 4) OR
       (CT-CRED NOT = P-NUMBER)
        GO TO HA35.
      PERFORM DELETE-CRTRAN-REC THRU WRITE-CRTRAN-EXIT.
      GO TO HA30.

       HA35.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
      GO TO HA25.

       HA40.
      PERFORM RESTORE-SCREEN-3.
      *       MOVE 1201    TO CPOS.
      *       PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
            DISPLAY "Transaction file will now be rebuilt" AT 0412.
      DISPLAY "NB" AT 0612
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     " - ALL other workstations must EXIT from ".
      DISPLAY "CREDITORS before continuing " AT 0812
       "C" WITH FOREGROUND-COLOR 7 HIGHLIGHT
       "ontinue [ ]".

       HA45.
      PERFORM TEST AFTER
       UNTIL WS-OPTION = "C"
        ACCEPT WS-OPTION AT 0850
        WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
            CLOSE CTRANS.
      DISPLAY "Reading :" AT 1012.
             DISPLAY "Writing :" AT 1212.
      *
      *    ****    R E N A M E  C R T R A N   F I L E
      *
            CALL "CBL_RENAME_FILE" USING W02-CRTRAN
             W02-CRTOLD
              RETURNING WS-STATUS.

       HA50.
            OPEN INPUT CRTOLD.
    IF WS-STAT1 = "9"
        IF FLE-LOCKED
     DISPLAY "Waiting" AT 5062
       WITH BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
     PERFORM LOCK-REC-LOOP THRU LOCK-REC-EXIT
     GO TO HA50.
    IF NOT(WS-STATUS = "00")
        MOVE 4   TO WS-F-ERROR
        PERFORM OPEN-ERROR.
      PERFORM CLEAR-L50.

       HA55.
            OPEN OUTPUT CTRANS.
    IF WS-STAT1 = "9"
        IF FLE-LOCKED
     DISPLAY "Waiting" AT 5062
       WITH BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
     PERFORM LOCK-REC-LOOP THRU LOCK-REC-EXIT
     GO TO HA50.
    IF NOT(WS-STATUS = "00")
        MOVE 4   TO WS-F-ERROR
        PERFORM OPEN-ERROR.
      PERFORM CLEAR-L50.

       HA60.
      READ CRTOLD
    AT END
    GO TO HA65.
      DISPLAY CTO-CRED AT 1022
       WITH FOREGROUND-COLOR 3 HIGHLIGHT " "
       CTO-SDTE
       WITH FOREGROUND-COLOR 3 HIGHLIGHT " "
       CTO-CODE
       WITH FOREGROUND-COLOR 3 HIGHLIGHT " "
       CTO-SINV
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      *
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
            MOVE CTO-REC  TO CT-REC.
      DISPLAY CT-CRED AT 1222
       WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
       CT-SDTE
       WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
       CT-CODE
       WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
       CT-SINV
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
            PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
           IF WS-STAT1 NOT = "0"
        MOVE 4   TO WS-F-ERROR
               PERFORM WRITE-ERROR.
      GO TO HA60.

       HA65.
      CLOSE CTRANS
     CRTOLD.
      *
      *    ****    D E L E T E  O L D  C R T R A N   F I L E
      *
            CALL "CBL_DELETE_FILE" USING W02-CRTOLD
              RETURNING WS-STATUS.
            OPEN I-O CTRANS.
      *
      *    ****    End Recovery Logging.
      *
            PERFORM AY70 THRU AY999.
      GO TO HA999.

      *HA70.
      *      DISPLAY "Transaction :" AT 1912.
      *      MOVE ZERO   TO P-OUT
      *        P-30D
      *        P-60D
      *        P-90D
      *        P-120D
      *        P-150D
      *        P-180D.
      *      MOVE P-NUMBER  TO CT-CRED.
      *      MOVE ZERO   TO CT-CRED
      *        CT-SDTE
      *        CT-CODE
      *        CT-SEQ.
      *      MOVE SPACES  TO CT-SINV.
      *      PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
      *    IF WS-F-ERROR = 4
      *        GO TO HA********

      *HA75.
      *      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
      *    IF (WS-F-ERROR = 4) OR
      *       (CT-CRED NOT = P-NUMBER)
      *        GO TO HA*******
      *      DISPLAY CT-CRED AT 1926
      *       WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
      *       CT-SDTE
      *       WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
      *       CT-CODE
      *       WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
      *       CT-SINV
      *       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      *    IF CT-COST

       HA90.
      DISPLAY "Restart at A/C No." AT 2012.
      MOVE SPACES  TO W15-ACNO.

       HA95.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
       " to do Creditor Lookup "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3
    FOREGROUND-COLOR 6 HIGHLIGHT
       " to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      ACCEPT W15-ACNO AT 2031
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W15-ACNO
       GO TO HA100
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50-LOOKUP
   WHEN OTHER
       CALL X"E5"
       GO TO HA95
        END-EVALUATE
              DISPLAY W15-ACNO AT 2031
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5
              IF W15-ACNO = SPACES
           GO TO HA95.
    IF W15-ACNO = SPACES
        GO TO HA100.
      PERFORM CLEAR-L50.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
        GO TO HA95.
            DISPLAY P-NAME AT 2038
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       HA96.
            MOVE "'C'ontinue or 'R'ekey  [ ]"
     TO WS-ERR-MES.
      MOVE "C"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "C" OR "R".
    IF WS-OPTION = "R"
        GO TO HA95.

       HA100.
      EXIT.

       HA999.
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
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W05-DATE.
             MOVE PAR-YMD        TO W05-DATE-YMD.
      MOVE 2   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

       COPY ZA49.PRO.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MD-LNE WS-MID-LNE.

       ZA60.
            MOVE WS-G1   TO WS-TCR(WS-S1) WS-BCR(WS-S1).
      MOVE WS-G8   TO WS-TCHR(WS-S1) WS-BCHR(WS-S1)
        WS-TCH(WS-S1) WS-BCH(WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
            MOVE WS-G2   TO WS-TCR(8) WS-TCR(50)
              WS-TCR(66).
            MOVE WS-G3   TO WS-MCR(8)  WS-MCR(50)
              WS-MCR(66) WS-MCR(1)
        WS-MCR(80).
            MOVE WS-G4   TO WS-BCR(8) WS-BCR(50)
              WS-BCR(66).
            MOVE WS-G9   TO WS-TCH(1).
            MOVE WS-G10  TO WS-TCH(80).
            MOVE WS-G11  TO WS-BCH(1).
            MOVE WS-G12  TO WS-BCH(80).
            MOVE WS-G14  TO WS-TCR(1)  WS-BCR(1).
            MOVE WS-G13  TO WS-TCR(80) WS-BCR(80).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"
     TO WS-WRKHD
    ELSE
        MOVE "Workstation"
     TO WS-WRKHD
        MOVE LS-USER  TO WS-WRKST.
       ZA200.

       COPY LOCKED.PRO.

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
               WHEN 24         MOVE W02-CRDMEM  TO WS-FILE
                               MOVE CME-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY DISPERR.PRO.
