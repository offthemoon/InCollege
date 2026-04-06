       VIEW-MY-MESSAGES.
           PERFORM LOAD-MESSAGES-INTO-M-REC
           IF M-MSG-TOTAL = 0
               MOVE "You have no messages at this time." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO VM-COUNT
           PERFORM VARYING MSG-LOOP-IDX FROM 1 BY 1 UNTIL MSG-LOOP-IDX > M-MSG-TOTAL
               MOVE M-REC(MSG-LOOP-IDX) TO MESSAGE-RECORD
               PERFORM VM-PARSE-AND-COLLECT
           END-PERFORM

           IF VM-COUNT = 0
               MOVE "You have no messages at this time." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "--- Your Messages ---" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM VARYING VM-DISPLAY-I FROM 1 BY 1 UNTIL VM-DISPLAY-I > VM-COUNT
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

           PERFORM MARK-INCOMING-MESSAGES-READ
           PERFORM SAVE-M-MESSAGES-FROM-MEMORY
           .

       LOAD-MESSAGES-INTO-M-REC.
           MOVE 0 TO M-MSG-TOTAL
           OPEN INPUT MESSAGES-FILE
           IF MESSAGES-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO M-LOAD-EOF
           PERFORM UNTIL M-LOAD-EOF = 1
               READ MESSAGES-FILE
                   AT END
                       MOVE 1 TO M-LOAD-EOF
                   NOT AT END
                       IF M-MSG-TOTAL < 200
                           ADD 1 TO M-MSG-TOTAL
                           MOVE MESSAGE-RECORD TO M-REC(M-MSG-TOTAL)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE MESSAGES-FILE
           .

       MARK-INCOMING-MESSAGES-READ.
           PERFORM VARYING MSG-LOOP-IDX FROM 1 BY 1 UNTIL MSG-LOOP-IDX > M-MSG-TOTAL
               MOVE M-REC(MSG-LOOP-IDX) TO MESSAGE-RECORD
               MOVE SPACES TO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS
               MOVE SPACES TO VM-TMP-READ
               INSPECT MESSAGE-RECORD REPLACING ALL X"0D" BY SPACE
               UNSTRING MESSAGE-RECORD
                   DELIMITED BY "|"
                   INTO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS
                        VM-TMP-READ
               END-UNSTRING
               IF FUNCTION TRIM(VM-TMP-RECV) = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                   MOVE SPACES TO MESSAGE-RECORD
                   STRING
                       FUNCTION TRIM(VM-TMP-SENDER) "|"
                       FUNCTION TRIM(VM-TMP-RECV) "|"
                       FUNCTION TRIM(VM-TMP-TEXT) "|"
                       FUNCTION TRIM(VM-TMP-TS) "|"
                       "1"
                       DELIMITED BY SIZE
                       INTO MESSAGE-RECORD
                   END-STRING
                   MOVE MESSAGE-RECORD TO M-REC(MSG-LOOP-IDX)
               END-IF
           END-PERFORM
           .

       SAVE-M-MESSAGES-FROM-MEMORY.
           OPEN OUTPUT MESSAGES-FILE
           PERFORM VARYING MSG-LOOP-IDX FROM 1 BY 1 UNTIL MSG-LOOP-IDX > M-MSG-TOTAL
               MOVE M-REC(MSG-LOOP-IDX) TO MESSAGE-RECORD
               WRITE MESSAGE-RECORD
           END-PERFORM
           CLOSE MESSAGES-FILE
           .

       VM-PARSE-AND-COLLECT.
           MOVE SPACES TO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS
           MOVE SPACES TO VM-TMP-READ
           INSPECT MESSAGE-RECORD REPLACING ALL X"0D" BY SPACE
           UNSTRING MESSAGE-RECORD
               DELIMITED BY "|"
               INTO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS VM-TMP-READ
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
