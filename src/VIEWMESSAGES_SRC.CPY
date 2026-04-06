       VIEW-MY-MESSAGES.
           PERFORM LOAD-MESSAGES-INTO-M-REC
           IF M-MSG-TOTAL = 0
               MOVE "You have no messages at this time." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO VM-COUNT
           PERFORM VARYING MSG-LOOP-IDX FROM 1 BY 1 UNTIL MSG-LOOP-IDX > M-MSG-TOTAL
               MOVE MSG-LOOP-IDX TO VM-CURRENT-LINE-IDX
               MOVE M-REC(MSG-LOOP-IDX) TO MESSAGE-RECORD
               PERFORM VM-PARSE-AND-COLLECT
           END-PERFORM

           IF VM-COUNT = 0
               MOVE "You have no messages at this time." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM VM-BUILD-UNIQUE-SENDERS
           IF VM-US-COUNT = 1
               COMPUTE VM-MENU-MAX = 2
           ELSE
               COMPUTE VM-MENU-MAX = VM-US-COUNT + 2
           END-IF

           PERFORM VM-SHOW-FILTER-SUBMENU
           MOVE "Enter your choice:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM GET-CHOICE-MENU-NUM

           IF VM-MENU-CHOICE < 1 OR VM-MENU-CHOICE > VM-MENU-MAX
               MOVE "Invalid choice." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF VM-MENU-CHOICE = VM-MENU-MAX
               EXIT PARAGRAPH
           END-IF

           PERFORM VM-CLEAR-SHOW-FLAGS
           IF VM-MENU-CHOICE = 1
               PERFORM VM-SET-ALL-SHOW-FLAGS
           ELSE
               SUBTRACT 1 FROM VM-MENU-CHOICE GIVING VM-PICK-SENDER-IX
               PERFORM VARYING VM-DISPLAY-I FROM 1 BY 1 UNTIL VM-DISPLAY-I > VM-COUNT
                   IF FUNCTION TRIM(VM-FROM(VM-DISPLAY-I)) = FUNCTION TRIM(
                           VM-US-NAME(VM-PICK-SENDER-IX))
                       MOVE 1 TO VM-SHOW-ENT(VM-DISPLAY-I)
                   END-IF
               END-PERFORM
           END-IF

           PERFORM VM-DISPLAY-SELECTED-MESSAGES
           PERFORM VM-MARK-SHOWN-AS-READ
           PERFORM SAVE-M-MESSAGES-FROM-MEMORY
           .

       VM-BUILD-UNIQUE-SENDERS.
           MOVE 0 TO VM-US-COUNT
           PERFORM VARYING MSG-LOOP-IDX FROM 1 BY 1 UNTIL MSG-LOOP-IDX > VM-COUNT
               MOVE 0 TO FOUND
               IF VM-US-COUNT > 0
                   PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > VM-US-COUNT
                       IF FUNCTION TRIM(VM-US-NAME(WS-K)) = FUNCTION TRIM(
                               VM-FROM(MSG-LOOP-IDX))
                           MOVE 1 TO FOUND
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
               IF FOUND = 0
                   IF VM-US-COUNT < 15
                       ADD 1 TO VM-US-COUNT
                       MOVE FUNCTION TRIM(VM-FROM(MSG-LOOP-IDX))
                           TO VM-US-NAME(VM-US-COUNT)
                   END-IF
               END-IF
           END-PERFORM
           .

       VM-SHOW-FILTER-SUBMENU.
           MOVE "--- How do you want to view messages? ---" TO WS-OUT
           PERFORM PRINT-LINE
           IF VM-US-COUNT = 1
               MOVE SPACES TO WS-OUT
               STRING "1. View messages from "
                   FUNCTION TRIM(VM-US-NAME(1))
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               MOVE "2. Back (return without viewing)" TO WS-OUT
               PERFORM PRINT-LINE
           ELSE
               MOVE "1. View all messages" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > VM-US-COUNT
                   COMPUTE VM-SUBMENU-OPT-N = WS-K + 1
                   MOVE VM-SUBMENU-OPT-N TO UNREAD-DISP
                   MOVE SPACES TO WS-OUT
                   STRING FUNCTION TRIM(UNREAD-DISP) ". Messages from: "
                       FUNCTION TRIM(VM-US-NAME(WS-K))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
               END-PERFORM
               COMPUTE VM-SUBMENU-OPT-N = VM-US-COUNT + 2
               MOVE VM-SUBMENU-OPT-N TO UNREAD-DISP
               MOVE SPACES TO WS-OUT
               STRING FUNCTION TRIM(UNREAD-DISP)
                   ". Back (return without viewing)"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
           END-IF
           .

       VM-CLEAR-SHOW-FLAGS.
           PERFORM VARYING VM-DISPLAY-I FROM 1 BY 1 UNTIL VM-DISPLAY-I > VM-COUNT
               MOVE 0 TO VM-SHOW-ENT(VM-DISPLAY-I)
           END-PERFORM
           .

       VM-SET-ALL-SHOW-FLAGS.
           PERFORM VARYING VM-DISPLAY-I FROM 1 BY 1 UNTIL VM-DISPLAY-I > VM-COUNT
               MOVE 1 TO VM-SHOW-ENT(VM-DISPLAY-I)
           END-PERFORM
           .

       VM-DISPLAY-SELECTED-MESSAGES.
           MOVE "--- Your Messages ---" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE 0 TO VM-PREV-SHOWN
           PERFORM VARYING VM-DISPLAY-I FROM 1 BY 1 UNTIL VM-DISPLAY-I > VM-COUNT
               IF VM-SHOW-ENT(VM-DISPLAY-I) = 1
                   IF VM-PREV-SHOWN = 1
                       MOVE "---" TO WS-OUT
                       PERFORM PRINT-LINE
                   END-IF
                   MOVE 1 TO VM-PREV-SHOWN
                   MOVE SPACES TO WS-OUT
                   STRING "From: "
                       FUNCTION TRIM(VM-FROM(VM-DISPLAY-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
                   STRING "Message: "
                       FUNCTION TRIM(VM-BODY(VM-DISPLAY-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
                   STRING "Sent: "
                       FUNCTION TRIM(VM-WHEN(VM-DISPLAY-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM
           MOVE "---------------------" TO WS-OUT
           PERFORM PRINT-LINE
           .

       VM-MARK-SHOWN-AS-READ.
           PERFORM VARYING VM-DISPLAY-I FROM 1 BY 1 UNTIL VM-DISPLAY-I > VM-COUNT
               IF VM-SHOW-ENT(VM-DISPLAY-I) = 1
                   MOVE VM-SRC-LINE-ENT(VM-DISPLAY-I) TO MSG-LOOP-IDX
                   PERFORM VM-MARK-ONE-MREC-READ
               END-IF
           END-PERFORM
           .

       VM-MARK-ONE-MREC-READ.
           MOVE M-REC(MSG-LOOP-IDX) TO MESSAGE-RECORD
           MOVE SPACES TO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS
           MOVE SPACES TO VM-TMP-READ
           INSPECT MESSAGE-RECORD REPLACING ALL X"0D" BY SPACE
           UNSTRING MESSAGE-RECORD
               DELIMITED BY "|"
               INTO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS VM-TMP-READ
           END-UNSTRING
           IF FUNCTION TRIM(VM-TMP-RECV) = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
               MOVE SPACES TO MESSAGE-RECORD
               STRING
                   FUNCTION TRIM(VM-TMP-SENDER) "|"
                   FUNCTION TRIM(VM-TMP-RECV) "|"
                   FUNCTION TRIM(VM-TMP-TEXT) "|"
                   FUNCTION TRIM(VM-TMP-TS) "|"
                   "1"
                   DELIMITED BY SIZE
                   INTO MESSAGE-RECORD
               END-STRING
               MOVE MESSAGE-RECORD TO M-REC(MSG-LOOP-IDX)
           END-IF
           .

       LOAD-MESSAGES-INTO-M-REC.
           MOVE 0 TO M-MSG-TOTAL
           OPEN INPUT MESSAGES-FILE
           IF MESSAGES-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO M-LOAD-EOF
           PERFORM UNTIL M-LOAD-EOF = 1
               READ MESSAGES-FILE
                   AT END
                       MOVE 1 TO M-LOAD-EOF
                   NOT AT END
                       IF M-MSG-TOTAL < 200
                           ADD 1 TO M-MSG-TOTAL
                           MOVE MESSAGE-RECORD TO M-REC(M-MSG-TOTAL)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE MESSAGES-FILE
           .

       SAVE-M-MESSAGES-FROM-MEMORY.
           OPEN OUTPUT MESSAGES-FILE
           PERFORM VARYING MSG-LOOP-IDX FROM 1 BY 1 UNTIL MSG-LOOP-IDX > M-MSG-TOTAL
               MOVE M-REC(MSG-LOOP-IDX) TO MESSAGE-RECORD
               WRITE MESSAGE-RECORD
           END-PERFORM
           CLOSE MESSAGES-FILE
           .

       VM-PARSE-AND-COLLECT.
           MOVE SPACES TO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS
           MOVE SPACES TO VM-TMP-READ
           INSPECT MESSAGE-RECORD REPLACING ALL X"0D" BY SPACE
           UNSTRING MESSAGE-RECORD
               DELIMITED BY "|"
               INTO VM-TMP-SENDER VM-TMP-RECV VM-TMP-TEXT VM-TMP-TS VM-TMP-READ
           END-UNSTRING
           IF FUNCTION TRIM(VM-TMP-RECV) = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
               IF VM-COUNT < VM-MAX
                   ADD 1 TO VM-COUNT
                   MOVE VM-CURRENT-LINE-IDX TO VM-SRC-LINE-ENT(VM-COUNT)
                   MOVE FUNCTION TRIM(VM-TMP-SENDER) TO VM-FROM(VM-COUNT)
                   MOVE FUNCTION TRIM(VM-TMP-TEXT) TO VM-BODY(VM-COUNT)
                   IF FUNCTION TRIM(VM-TMP-TS) = SPACES
                       MOVE "N/A" TO VM-WHEN(VM-COUNT)
                   ELSE
                       MOVE FUNCTION TRIM(VM-TMP-TS) TO VM-WHEN(VM-COUNT)
                   END-IF
               END-IF
           END-IF
           .
