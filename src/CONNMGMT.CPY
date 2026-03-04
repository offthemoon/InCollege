       ENSURE-CONNECTIONS-FILE.
           MOVE "00" TO CONNECTIONS-STATUS
           OPEN INPUT CONNECTIONS-FILE
           IF CONNECTIONS-STATUS NOT = "00"
               OPEN OUTPUT CONNECTIONS-FILE
               CLOSE CONNECTIONS-FILE
           ELSE
               CLOSE CONNECTIONS-FILE
           END-IF
           .

       LOAD-CONNECTIONS.
           MOVE 0 TO CONN-COUNT
           OPEN INPUT CONNECTIONS-FILE
           PERFORM UNTIL 1 = 2
               READ CONNECTIONS-FILE
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
           CLOSE CONNECTIONS-FILE
           .

       SAVE-CONNECTIONS.
           OPEN OUTPUT CONNECTIONS-FILE
           IF CONN-COUNT > 0
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > CONN-COUNT
                   MOVE SPACES TO CONN-RECORD
                   STRING
                       FUNCTION TRIM(CONN-USER1(WS-J)) "|"
                       FUNCTION TRIM(CONN-USER2(WS-J))
                       DELIMITED BY SIZE
                       INTO CONN-RECORD
                   END-STRING
                   WRITE CONN-RECORD
               END-PERFORM
           END-IF
           CLOSE CONNECTIONS-FILE
           .

