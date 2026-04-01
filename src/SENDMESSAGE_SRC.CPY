       SEND-MESSAGE.
           MOVE "Enter recipient username:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           MOVE WS-OUT(1:15) TO MSG-RECEIVER

      *> Check if recipient user exists
           MOVE 0 TO FOUND
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
               IF FUNCTION TRIM(U-NAME(WS-I)) = FUNCTION TRIM(MSG-RECEIVER)
                   MOVE 1 TO FOUND
               END-IF
           END-PERFORM

           IF FOUND = 0
               MOVE "Recipient user not found." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

      *> Block messaging while request is still pending (either direction)
           MOVE 0 TO FOUND
           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > PEND-COUNT
                   IF (FUNCTION TRIM(PEND-FROM(WS-K))
                           = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                       AND FUNCTION TRIM(PEND-TO(WS-K))
                           = FUNCTION TRIM(MSG-RECEIVER))
                    OR (FUNCTION TRIM(PEND-FROM(WS-K))
                           = FUNCTION TRIM(MSG-RECEIVER)
                       AND FUNCTION TRIM(PEND-TO(WS-K))
                           = FUNCTION TRIM(U-NAME(CURRENT-USER-ID)))
                       MOVE 1 TO FOUND
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF

           IF FOUND = 1
               MOVE "Cannot message users with pending requests." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

      *> Only allow messages to established connections
           MOVE FUNCTION TRIM(U-NAME(CURRENT-USER-ID)) TO SR-SENDER
           MOVE FUNCTION TRIM(MSG-RECEIVER) TO SR-RECIPIENT
           PERFORM ARE-CONNECTED

           IF VN-FOUND = 0
               MOVE "You can only message users you are connected with." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "Enter your message:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           MOVE WS-OUT TO MSG-TEXT

           MOVE FUNCTION TRIM(U-NAME(CURRENT-USER-ID)) TO MSG-SENDER

      *> Write to file
           OPEN EXTEND MESSAGES-FILE

           MOVE SPACES TO MESSAGE-RECORD

           STRING
               FUNCTION TRIM(MSG-SENDER) "|"
               FUNCTION TRIM(MSG-RECEIVER) "|"
               FUNCTION TRIM(MSG-TEXT)
               DELIMITED BY SIZE
               INTO MESSAGE-RECORD
           END-STRING

           WRITE MESSAGE-RECORD
           CLOSE MESSAGES-FILE

           OPEN EXTEND MESSAGES-FILE

           MOVE "Message sent successfully!" TO WS-OUT
           PERFORM PRINT-LINE
           .
