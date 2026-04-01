       ARE-CONNECTED.
      *>sets VN-FOUND=1 if SR-SENDER and SR-RECIPIENT are connected
      *>supports both one-direction and two-direction stored records
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
      *>direct direction: current user stored as user1
                   IF FUNCTION TRIM(CONN-USER1(WS-K))
                      = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
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
                               MOVE 1 TO VN-FOUND
                           END-IF
                       END-PERFORM
      *>reverse-only direction: current user stored as user2
                   ELSE
                       IF FUNCTION TRIM(CONN-USER2(WS-K))
                          = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
      *>avoid duplicate display if reciprocal row already exists
                           MOVE 0 TO FOUND
                           PERFORM VARYING WS-I FROM 1 BY 1
                               UNTIL WS-I > CONN-COUNT
                               IF FUNCTION TRIM(CONN-USER1(WS-I))
                                  = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                                  AND FUNCTION TRIM(CONN-USER2(WS-I))
                                  = FUNCTION TRIM(CONN-USER1(WS-K))
                                   MOVE 1 TO FOUND
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM

                           IF FOUND = 0
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
                                       MOVE 1 TO VN-FOUND
                                   END-IF
                               END-PERFORM
                           END-IF
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
