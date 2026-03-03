       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE 0 TO VR-FOUND

           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > PEND-COUNT
                   IF FUNCTION TRIM(PEND-TO(WS-K))
                      = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                       MOVE 1 TO VR-FOUND

                       MOVE SPACES TO WS-OUT
                       STRING FUNCTION TRIM(PEND-FROM(WS-K))
                              " has sent you a connection request."
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE "  1. Accept   2. Reject" TO WS-OUT
                       PERFORM PRINT-LINE
                       MOVE "Enter your choice:" TO WS-OUT
                       PERFORM PRINT-LINE
                       PERFORM GET-CHOICE-1DIGIT

                       IF CHOICE = 1
                           PERFORM ACCEPT-REQUEST
                       ELSE
                       IF CHOICE = 2
                           PERFORM REJECT-REQUEST
                       END-IF
                       END-IF
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

       ACCEPT-REQUEST.
      *> WS-K holds the index of the request being processed.
           IF CONN-COUNT < 25
               ADD 1 TO CONN-COUNT
               MOVE FUNCTION TRIM(PEND-FROM(WS-K))
                   TO CONN-USER1(CONN-COUNT)
               MOVE FUNCTION TRIM(PEND-TO(WS-K))
                   TO CONN-USER2(CONN-COUNT)
               PERFORM SAVE-CONNECTS

               MOVE SPACES TO WS-OUT
               STRING "You are now connected with "
                      FUNCTION TRIM(PEND-FROM(WS-K)) "."
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
           ELSE
               MOVE "System error: connection limit reached." TO WS-OUT
               PERFORM PRINT-LINE
           END-IF

      *> Remove the request from pending list
           PERFORM REMOVE-PENDING-REQUEST
           .

       REJECT-REQUEST.
      *> WS-K holds the index of the request being processed.
           MOVE SPACES TO WS-OUT
           STRING "Connection request from "
                  FUNCTION TRIM(PEND-FROM(WS-K)) " declined."
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           PERFORM REMOVE-PENDING-REQUEST
           .

       REMOVE-PENDING-REQUEST.
      *> Removes entry WS-K from PENDING-REQUESTS by shifting entries down.
           PERFORM VARYING WS-J FROM WS-K BY 1
               UNTIL WS-J >= PEND-COUNT
               MOVE PEND-FROM(WS-J + 1) TO PEND-FROM(WS-J)
               MOVE PEND-TO(WS-J + 1)   TO PEND-TO(WS-J)
           END-PERFORM
           IF PEND-COUNT > 0
               MOVE SPACES TO PEND-FROM(PEND-COUNT)
               MOVE SPACES TO PEND-TO(PEND-COUNT)
               SUBTRACT 1 FROM PEND-COUNT
           END-IF
           SUBTRACT 1 FROM WS-K
           PERFORM SAVE-REQUESTS
           .

