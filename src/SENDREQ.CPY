       ENSURE-REQUESTS-FILE.
           MOVE "00" TO REQUESTS-STATUS
           OPEN INPUT REQUESTS-FILE
           IF REQUESTS-STATUS NOT = "00"
               OPEN OUTPUT REQUESTS-FILE
               CLOSE REQUESTS-FILE
           ELSE
               CLOSE REQUESTS-FILE
           END-IF
           .

       LOAD-REQUESTS.
           MOVE 0 TO PEND-COUNT
           OPEN INPUT REQUESTS-FILE
           PERFORM UNTIL 1 = 2
               READ REQUESTS-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF PEND-COUNT < 25
                           ADD 1 TO PEND-COUNT
                           MOVE SPACES TO PEND-FROM(PEND-COUNT)
                           MOVE SPACES TO PEND-TO(PEND-COUNT)
                           UNSTRING REQ-RECORD DELIMITED BY "|"
                               INTO PEND-FROM(PEND-COUNT)
                                    PEND-TO(PEND-COUNT)
                           END-UNSTRING
                       END-IF
               END-READ
           END-PERFORM
           CLOSE REQUESTS-FILE
           .

       SAVE-REQUESTS.
           OPEN OUTPUT REQUESTS-FILE
           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > PEND-COUNT
                   MOVE SPACES TO REQ-RECORD
                   STRING
                       FUNCTION TRIM(PEND-FROM(WS-K)) "|"
                       FUNCTION TRIM(PEND-TO(WS-K))
                       DELIMITED BY SIZE
                       INTO REQ-RECORD
                   END-STRING
                   WRITE REQ-RECORD
               END-PERFORM
           END-IF
           CLOSE REQUESTS-FILE
           .

       VALIDATE-SEND-REQUEST.
           MOVE 1 TO SR-VALID

           IF FUNCTION TRIM(SR-SENDER) = FUNCTION TRIM(SR-RECIPIENT)
               MOVE 0 TO SR-VALID
               MOVE "You cannot send a connection request to yourself."
                   TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > PEND-COUNT
                   IF FUNCTION TRIM(PEND-FROM(WS-K))
                           = FUNCTION TRIM(SR-SENDER)
                       AND FUNCTION TRIM(PEND-TO(WS-K))
                           = FUNCTION TRIM(SR-RECIPIENT)
                       MOVE 0 TO SR-VALID
                       MOVE "You have already sent a connection request to this user."
                           TO WS-OUT
                       PERFORM PRINT-LINE
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION TRIM(PEND-FROM(WS-K))
                           = FUNCTION TRIM(SR-RECIPIENT)
                       AND FUNCTION TRIM(PEND-TO(WS-K))
                           = FUNCTION TRIM(SR-SENDER)
                       MOVE 0 TO SR-VALID
                       MOVE "This user has already sent you a connection request."
                           TO WS-OUT
                       PERFORM PRINT-LINE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF
           .

       SEND-REQUEST.
           PERFORM VALIDATE-SEND-REQUEST

           IF SR-VALID = 1
               IF PEND-COUNT < 25
                   ADD 1 TO PEND-COUNT
                   MOVE FUNCTION TRIM(SR-SENDER)
                       TO PEND-FROM(PEND-COUNT)
                   MOVE FUNCTION TRIM(SR-RECIPIENT)
                       TO PEND-TO(PEND-COUNT)
                   PERFORM SAVE-REQUESTS

                   MOVE SPACES TO WS-OUT
                   STRING "Connection request sent to "
                          FUNCTION TRIM(SR-RECIPIENT) "."
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
               ELSE
                   MOVE "System error: pending request limit reached."
                       TO WS-OUT
                   PERFORM PRINT-LINE
               END-IF
           END-IF
           .
