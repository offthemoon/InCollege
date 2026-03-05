       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE 0 TO VR-FOUND

           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > PEND-COUNT
                   IF FUNCTION TRIM(PEND-TO(WS-K))
                      = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
      *>find the requesting user in USERS array
                       PERFORM VARYING WS-J FROM 1 BY 1
                           UNTIL WS-J > 5
                           IF FUNCTION TRIM(U-NAME(WS-J))
                              = FUNCTION TRIM(PEND-FROM(WS-K))
                               MOVE SPACES TO WS-OUT
                               STRING "Request from: "
                                      FUNCTION TRIM(PEND-FROM(WS-K))
                                      " "
                                      FUNCTION TRIM(PEND-FROM-LNAME(WS-K))
                                   DELIMITED BY SIZE
                                   INTO WS-OUT
                               END-STRING
                               PERFORM PRINT-LINE
                           END-IF
                       END-PERFORM

                       MOVE "1. Accept" TO WS-OUT
                       PERFORM PRINT-LINE
                       MOVE "2. Reject" TO WS-OUT
                       PERFORM PRINT-LINE
                       MOVE SPACES TO WS-OUT
                       STRING "Enter your choice for "
                              FUNCTION TRIM(PEND-FROM(WS-K)) ":"
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE

                       PERFORM GET-CHOICE-1DIGIT

                       EVALUATE CHOICE
                           WHEN 1
                               PERFORM ACCEPT-CONNECTION-REQUEST
                           WHEN 2
                               PERFORM REJECT-CONNECTION-REQUEST
                       END-EVALUATE

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
           .

       ACCEPT-CONNECTION-REQUEST.
           MOVE SPACES TO WS-OUT
           STRING "Connection request from "
                  FUNCTION TRIM(PEND-FROM(WS-K)) " accepted!"
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

      *>add bidirectional connections
           IF CONN-COUNT < 24
      *>first direction: Current user -> requester
               ADD 1 TO CONN-COUNT
               MOVE FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                   TO CONN-USER1(CONN-COUNT)
      *>store current user's last name directly (CURRENT-USER-ID is an index)
               MOVE FUNCTION TRIM(U-LNAME(CURRENT-USER-ID))
                   TO CONN-USER1-LNAME(CONN-COUNT)
               MOVE FUNCTION TRIM(PEND-FROM(WS-K))
                   TO CONN-USER2(CONN-COUNT)
               MOVE FUNCTION TRIM(PEND-FROM-LNAME(WS-K))
                   TO CONN-USER2-LNAME(CONN-COUNT)

      *>second direction: requester -> current user
               ADD 1 TO CONN-COUNT
               MOVE FUNCTION TRIM(PEND-FROM(WS-K))
                   TO CONN-USER1(CONN-COUNT)
               MOVE FUNCTION TRIM(PEND-FROM-LNAME(WS-K))
                   TO CONN-USER1-LNAME(CONN-COUNT)
               MOVE FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                   TO CONN-USER2(CONN-COUNT)
      *>store current user's last name directly (CURRENT-USER-ID is an index)
               MOVE FUNCTION TRIM(U-LNAME(CURRENT-USER-ID))
                   TO CONN-USER2-LNAME(CONN-COUNT)

               PERFORM SAVE-CONNECTIONS
           END-IF

      *>remove from pending requests
           PERFORM REMOVE-PENDING-REQUEST
           .

       REJECT-CONNECTION-REQUEST.
           MOVE SPACES TO WS-OUT
           STRING "Connection request from "
                  FUNCTION TRIM(PEND-FROM(WS-K)) " rejected."
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

      *>remove from pending requests
           PERFORM REMOVE-PENDING-REQUEST
           .

       REMOVE-PENDING-REQUEST.
      *>shift all requests after WS-K down by one position
           PERFORM VARYING WS-J FROM WS-K BY 1
               UNTIL WS-J > PEND-COUNT
               IF WS-J < PEND-COUNT
                   MOVE PEND-FROM(WS-J + 1) TO PEND-FROM(WS-J)
                   MOVE PEND-TO(WS-J + 1) TO PEND-TO(WS-J)
               END-IF
           END-PERFORM

      *>clear the last entry
           MOVE SPACES TO PEND-FROM(PEND-COUNT)
           MOVE SPACES TO PEND-TO(PEND-COUNT)

      *>decrement count
           SUBTRACT 1 FROM PEND-COUNT

      *>also decrement WS-K to stay at same position since we shifted
           SUBTRACT 1 FROM WS-K

      *>save updated pending requests
           PERFORM SAVE-REQUESTS
           .
