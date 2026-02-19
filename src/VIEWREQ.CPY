       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE 0 TO VR-FOUND

           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > PEND-COUNT
                   IF FUNCTION TRIM(PEND-TO(WS-K))
                      = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                       MOVE SPACES TO WS-OUT
                       STRING FUNCTION TRIM(PEND-FROM(WS-K))
                              " has sent you a connection request."
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE
                       MOVE 1 TO VR-FOUND
                   END-IF
               END-PERFORM
           END-IF

           IF VR-FOUND = 0
               MOVE "You have no pending connection requests at this time."
                   TO WS-OUT
               PERFORM PRINT-LINE
           END-IF

           MOVE "-----------------------------------" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE "1. Back to Main Menu" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM GET-CHOICE-1DIGIT
           .
