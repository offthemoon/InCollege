       VIEW-MY-MESSAGES.
           MOVE 0 TO VM-COUNT
           MOVE 0 TO VM-MSG-EOF

           OPEN INPUT MESSAGES-FILE
           IF MESSAGES-STATUS NOT = "00"
               MOVE "You have no messages at this time." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL VM-MSG-EOF = 1
               READ MESSAGES-FILE
                   AT END
                       MOVE 1 TO VM-MSG-EOF
                   NOT AT END
                       PERFORM VM-PARSE-AND-COLLECT
               END-READ
           END-PERFORM

           CLOSE MESSAGES-FILE

           IF VM-COUNT = 0
               MOVE "You have no messages at this time." TO WS-OUT
               PERFORM PRINT-LINE
           ELSE
               MOVE "--- Your Messages ---" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM VARYING VM-DISPLAY-I FROM 1 BY 1
                       UNTIL VM-DISPLAY-I > VM-COUNT
                   MOVE SPACES TO WS-OUT
                   STRING "From: "
                       FUNCTION TRIM(VM-FROM(VM-DISPLAY-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
                   STRING "Message: "
                       FUNCTION TRIM(VM-BODY(VM-DISPLAY-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
                   STRING "Sent: "
                       FUNCTION TRIM(VM-WHEN(VM-DISPLAY-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
                   IF VM-DISPLAY-I < VM-COUNT
                       MOVE "---" TO WS-OUT
                       PERFORM PRINT-LINE
                   END-IF
               END-PERFORM
               MOVE "---------------------" TO WS-OUT
               PERFORM PRINT-LINE
           END-IF
           .

       VM-PARSE-AND-COLLECT.
           MOVE SPACES TO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS
           INSPECT MESSAGE-RECORD REPLACING ALL X"0D" BY SPACE
           UNSTRING MESSAGE-RECORD
               DELIMITED BY "|"
               INTO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS
           END-UNSTRING
           IF FUNCTION TRIM(VM-TMP-RECV) = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
               IF VM-COUNT < VM-MAX
                   ADD 1 TO VM-COUNT
                   MOVE FUNCTION TRIM(VM-TMP-SENDER) TO VM-FROM(VM-COUNT)
                   MOVE FUNCTION TRIM(VM-TMP-TEXT) TO VM-BODY(VM-COUNT)
                   IF FUNCTION TRIM(VM-TMP-TS) = SPACES
                       MOVE "N/A" TO VM-WHEN(VM-COUNT)
                   ELSE
                       MOVE FUNCTION TRIM(VM-TMP-TS) TO VM-WHEN(VM-COUNT)
                   END-IF
               END-IF
           END-IF
           .
