       ENSURE-CONNECTS-FILE.
           MOVE "00" TO CONNECTS-STATUS
           OPEN INPUT CONNECTS-FILE
           IF CONNECTS-STATUS NOT = "00"
               OPEN OUTPUT CONNECTS-FILE
               CLOSE CONNECTS-FILE
           ELSE
               CLOSE CONNECTS-FILE
           END-IF
           .

       LOAD-CONNECTS.
           MOVE 0 TO CONN-COUNT
           OPEN INPUT CONNECTS-FILE
           PERFORM UNTIL 1 = 2
               READ CONNECTS-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF CONN-COUNT < 25
                           ADD 1 TO CONN-COUNT
                           MOVE SPACES TO CONN-USER1(CONN-COUNT)
                           MOVE SPACES TO CONN-USER2(CONN-COUNT)
                           UNSTRING CONN-RECORD DELIMITED BY "|"
                               INTO CONN-USER1(CONN-COUNT)
                                    CONN-USER2(CONN-COUNT)
                           END-UNSTRING
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONNECTS-FILE
           .

       SAVE-CONNECTS.
           OPEN OUTPUT CONNECTS-FILE
           IF CONN-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > CONN-COUNT
                   MOVE SPACES TO CONN-RECORD
                   STRING
                       FUNCTION TRIM(CONN-USER1(WS-K)) "|"
                       FUNCTION TRIM(CONN-USER2(WS-K))
                       DELIMITED BY SIZE
                       INTO CONN-RECORD
                   END-STRING
                   WRITE CONN-RECORD
               END-PERFORM
           END-IF
           CLOSE CONNECTS-FILE
           .

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
                       MOVE SPACES TO WS-OUT
                       STRING FUNCTION TRIM(CONN-USER2(WS-K))
                              " is in your network."
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE
                       MOVE 1 TO VN-FOUND
                   ELSE
                   IF FUNCTION TRIM(CONN-USER2(WS-K))
                      = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                       MOVE SPACES TO WS-OUT
                       STRING FUNCTION TRIM(CONN-USER1(WS-K))
                              " is in your network."
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE
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
