      *> ============================================================
      *> VIEWREQ.CPY - Copybook for Viewing Incoming Connection Requests
      *> Handles: displaying all pending requests received by the
      *>          currently logged-in user from requests.dat
      *>
      *> PARAGRAPHS PROVIDED:
      *>   VIEW-PENDING-REQUESTS - displays all incoming requests for
      *>                           the logged-in user, or a message if
      *>                           none exist. Includes Back to Main Menu.
      *>
      *> DEPENDS ON (defined in InCollege.cob or SENDREQ.CPY):
      *>   PENDING-REQUESTS table (PEND-FROM, PEND-TO, PEND-COUNT)
      *>   CURRENT-USER-ID, U-NAME
      *>   WS-OUT, WS-K
      *>   PRINT-LINE, GET-CHOICE-1DIGIT
      *> ============================================================

      *> ============================================================
      *> WORKING-STORAGE variable (COPY into WS SECTION of .cob)
      *> ============================================================

       01 VR-FOUND                     PIC 9 VALUE 0.
      *>   0 = no incoming requests found for this user
      *>   1 = at least one incoming request was displayed

      *> ============================================================
      *> PROCEDURE DIVISION paragraph
      *> ============================================================

      *> ------------------------------------------------------------
      *> VIEW-PENDING-REQUESTS
      *>   Loops through the in-memory PENDING-REQUESTS table and
      *>   displays every entry where PEND-TO = logged-in user.
      *>   If none are found, displays a "no pending requests" message.
      *>   Always shows "1. Back to Main Menu" at the end.
      *> ------------------------------------------------------------
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
