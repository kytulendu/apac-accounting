      *
      *    ****  *   * ****  ****  ***** *   *	   ***** *** *	   *****
      *    *   * *   * *   * *	 * *	 *   *	   *	  *  *	   *
      *    *   * *   * *   * *	 * *	  * *	   *	  *  *	   *
      *    ****  *   * ****  *	 * ***	   *	   ***	  *  *	   ***
      *    *     *   * *   * *   * *      * *      *      *  *     *
      *    *	 *   * *   * *	 * *	 *   *	   *	  *  *	   *
      *    *	  ***  *   * ****  ***** *   *	   *	 *** ***** *****
      *
006080 READ-PURDEX.
006090	     READ PURDEX
	       KEY IS PRD-KEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-PURDEX.
006200	     GO TO READ-PURDEX-EXIT.

006210 READ-PURDEX-LOCK.
006220	     READ PURDEX WITH KEPT LOCK
	       KEY IS PRD-KEY.
006230     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006240	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006250     IF WS-STAT1 = "2" OR "3" OR "4"
006260	       MOVE 56		 TO WS-F-ERROR
006270         PERFORM READ-ERROR.
006280     IF RECORD-LOCKED
006290         PERFORM LOCKED-RECORD
006300	       GO TO READ-PURDEX-LOCK.
006310     IF TOO-MANY-LOCKS
006320	       UNLOCK PURDEX
006330	       GO TO READ-PURDEX-LOCK.
006360	     GO TO READ-PURDEX-EXIT.

006080 READ-PURDEX-NEXT.
006090	     READ PURDEX NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-PURDEX-NEXT.
006200	     GO TO READ-PURDEX-EXIT.

006080 READ-PURDEX-NEXT-LOCK.
006090	     READ PURDEX NEXT WITH KEPT LOCK.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-PURDEX-NEXT-LOCK.
           IF TOO-MANY-LOCKS
	       UNLOCK PURDEX
	       GO TO READ-PURDEX-NEXT-LOCK.
006200	     GO TO READ-PURDEX-EXIT.

006080 READ-PURDEX-PREV.
006090	     READ PURDEX PREVIOUS.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-PURDEX-PREV.
006200	     GO TO READ-PURDEX-EXIT.

006080 START-AT-PRDX-KEY.
006090	     START PURDEX
	       KEY >= PRD-KEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PURDEX-EXIT.

006080 START-AT-PRDX-STOCK.
006090	     START PURDEX
	       KEY >= PRD-SKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PURDEX-EXIT.

006080 START-AT-PRDX-MOVE.
006090	     START PURDEX
	       KEY >= PRD-MKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PURDEX-EXIT.

006080 START-AT-PRDX-DATE.
006090	     START PURDEX
	       KEY >= PRD-DKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PURDEX-EXIT.

006080 START-AT-PRDX-SDTE.
006090	     START PURDEX
	       KEY >= PRD-DSKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PURDEX-EXIT.

006080 START-AT-PRDX-CDTE.
	     MOVE ZERO		 TO WS-F-ERROR.
006090	     START PURDEX
	       KEY >= PRD-CDKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140	       PERFORM READ-ERROR.
006200	     GO TO READ-PURDEX-EXIT.

006080 START-AT-PRDX-SREF.
006090	     START PURDEX
	       KEY >= PRD-RSKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
	       GO TO READ-PURDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 56		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.

006520 READ-PURDEX-EXIT.
006530       EXIT.