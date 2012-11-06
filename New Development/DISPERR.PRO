             EVALUATE WS-STATUS
               WHEN "10"
041890              MOVE "End of FILE" TO WS-STAT-MESSAGE
               WHEN "22"
041920              MOVE "Duplicate record number" TO WS-STAT-MESSAGE
               WHEN "23"
041950              MOVE "Invalid record number" TO WS-STAT-MESSAGE
               WHEN "24"
041980              MOVE "DISK full" TO WS-STAT-MESSAGE
               WHEN "30"
042010              MOVE "DISK error" TO WS-STAT-MESSAGE
               WHEN "94"
042040              MOVE "FILE locked" TO WS-STAT-MESSAGE
             END-EVALUATE.
             MOVE "S"                TO CRT-TYPE.
007750       CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
             DISPLAY WS-IO-ERROR                                   AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
041520       DISPLAY "File - "                                     AT 1405 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
041530       DISPLAY "Status "                                     AT 1605 WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
041560     IF WS-STATUS NOT = "94"
041570         DISPLAY "Key    "                                   AT 1805 WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT " " WS-KEYX WITH FOREGROUND-COLOR Cyan HIGHLIGHT
041590	   DISPLAY "Reverse backup or contact program Support" AT 2005.
041610       DISPLAY "Please make a note of these details" AT 2205.
041620       STOP RUN.
