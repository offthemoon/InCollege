       ARE-CONNECTED.
      *> Sets VN-FOUND=1 if SR-SENDER and SR-RECIPIENT are already connected.
           MOVE 0 TO VN-FOUND
           IF CONN-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > CONN-COUNT
                   IF (FUNCTION TRIM(CONN-USER1(WS-K))
                           = FUNCTION TRIM(SR-SENDER)
                       AND FUNCTION TRIM(CONN-USER2(WS-K))
                           = FUNCTION TRIM(SR-RECIPIENT))
                   OR (FUNCTION TRIM(CONN-USER1(WS-K))
                           = FUNCTION TRIM(SR-RECIPIENT)
                       AND FUNCTION TRIM(CONN-USER2(WS-K))
                           = FUNCTION TRIM(SR-SENDER))
                       MOVE 1 TO VN-FOUND
                   END-IF
               END-PERFORM
           END-IF
           .

       VIEW-NETWORK.
           MOVE "--- Your Network ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE 0 TO VN-FOUND

           IF CONN-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > CONN-COUNT
                   IF FUNCTION TRIM(CONN-USER1(WS-K))
                      = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
      *> Find the connected user in USERS array
                       PERFORM VARYING WS-J FROM 1 BY 1
                           UNTIL WS-J > 5
                           IF FUNCTION TRIM(U-NAME(WS-J))
                              = FUNCTION TRIM(CONN-USER2(WS-K))
                               MOVE SPACES TO WS-OUT
                               STRING "Connected with: "
                                      FUNCTION TRIM(CONN-USER2(WS-K))
                                      " "
                                      FUNCTION TRIM(CONN-USER2-LNAME(WS-K))
                                      " (University: "
                                      FUNCTION TRIM(U-UNIV(WS-J))
                                      ", Major: "
                                      FUNCTION TRIM(U-MAJOR(WS-J))
                                      ")"
                                   DELIMITED BY SIZE
                                   INTO WS-OUT
                               END-STRING
                               PERFORM PRINT-LINE
                           END-IF
                       END-PERFORM
                       MOVE 1 TO VN-FOUND
                   ELSE
                   IF FUNCTION TRIM(CONN-USER2(WS-K))
                      = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
      *> Find the connected user in USERS array
                       PERFORM VARYING WS-J FROM 1 BY 1
                           UNTIL WS-J > 5
                           IF FUNCTION TRIM(U-NAME(WS-J))
                              = FUNCTION TRIM(CONN-USER1(WS-K))
                               MOVE SPACES TO WS-OUT
                               STRING "Connected with: "
                                      FUNCTION TRIM(CONN-USER1(WS-K))
                                      " "
                                      FUNCTION TRIM(CONN-USER1-LNAME(WS-K))
                                      " (University: "
                                      FUNCTION TRIM(U-UNIV(WS-J))
                                      ", Major: "
                                      FUNCTION TRIM(U-MAJOR(WS-J))
                                      ")"
                                   DELIMITED BY SIZE
                                   INTO WS-OUT
                               END-STRING
                               PERFORM PRINT-LINE
                           END-IF
                       END-PERFORM
                       MOVE 1 TO VN-FOUND
                   END-IF
                   END-IF
               END-PERFORM
           END-IF

           IF VN-FOUND = 0
               MOVE "You have no connections in your network yet."
                   TO WS-OUT
               PERFORM PRINT-LINE
           END-IF

           MOVE "--------------------" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE "1. Back to Main Menu" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM GET-CHOICE-1DIGIT
           .
