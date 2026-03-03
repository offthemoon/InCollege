       VIEW-NETWORK.
           DISPLAY "Your Network Connections:".
           IF NETWORK-COUNT = 0
              DISPLAY "You have no connections in your network."
           ELSE
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > NETWORK-COUNT
                 DISPLAY NETWORK-CONNECTIONS(I)
              END-PERFORM
           END-IF.
           DISPLAY "Press Enter to continue.".
           ACCEPT WS-DUMMY.
           EXIT PARAGRAPH.