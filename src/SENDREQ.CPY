      *> ============================================================
      *> SENDREQ.CPY - Copybook for Connection Request Logic
      *> Handles: sending requests, validating requests, loading and
      *>          saving pending requests to/from pending_requests.dat
      *>
      *> HOW TO USE IN InCollege.cob:
      *>   1. Add the FILE-CONTROL and FD entries (marked below) to
      *>      the ENVIRONMENT and FILE SECTION of InCollege.cob.
      *>   2. COPY SENDREQ in the WORKING-STORAGE SECTION to bring in
      *>      all shared variables.
      *>   3. COPY SENDREQ in the PROCEDURE DIVISION to bring in all
      *>      paragraphs, then PERFORM them as needed.
      *>
      *> PARAGRAPHS PROVIDED:
      *>   ENSURE-REQUESTS-FILE  - creates file if it does not exist
      *>   LOAD-REQUESTS         - reads all pending requests from file
      *>   SAVE-REQUESTS         - writes all pending requests to file
      *>   SEND-REQUEST          - validates & records a new request
      *>   VALIDATE-SEND-REQUEST - checks for duplicate/already-connected
      *> ============================================================

      *> ------------------------------------------------------------
      *> FILE-CONTROL entry (add to ENVIRONMENT DIVISION in .cob):
      *>
      *>    SELECT REQUESTS-FILE ASSIGN TO "pending_requests.dat"
      *>        ORGANIZATION IS LINE SEQUENTIAL
      *>        FILE STATUS IS REQUESTS-STATUS.
      *> ------------------------------------------------------------

      *> ------------------------------------------------------------
      *> FD entry (add to FILE SECTION in .cob):
      *>
      *>    FD REQUESTS-FILE.
      *>    01 REQ-RECORD   PIC X(32).
      *> ------------------------------------------------------------

      *> ============================================================
      *> WORKING-STORAGE variables (COPY this block into WS SECTION)
      *> ============================================================

       01 REQUESTS-STATUS              PIC XX  VALUE "00".

      *> Maximum pending requests stored in memory at one time
       01 REQ-MAX                      PIC 99  VALUE 25.

      *> In-memory table of pending requests (sender -> recipient)
       01 PENDING-REQUESTS.
           05 PEND-ENTRY OCCURS 25 TIMES.
               10 PEND-FROM            PIC X(15).
               10 PEND-TO              PIC X(15).

       01 PEND-COUNT                   PIC 99  VALUE 0.

      *> Scratch fields used during send-request validation
       01 SR-SENDER                    PIC X(15) VALUE SPACES.
       01 SR-RECIPIENT                 PIC X(15) VALUE SPACES.
       01 SR-VALID                     PIC 9     VALUE 0.
      *>   0 = invalid, 1 = ok to send

      *> ============================================================
      *> PROCEDURE DIVISION paragraphs
      *> ============================================================

      *> ------------------------------------------------------------
      *> ENSURE-REQUESTS-FILE
      *>   Creates pending_requests.dat if it does not already exist.
      *>   Call once at program start, after ENSURE-ACCOUNTS-FILE.
      *> ------------------------------------------------------------
       ENSURE-REQUESTS-FILE.
           MOVE "00" TO REQUESTS-STATUS
           OPEN INPUT REQUESTS-FILE
           IF REQUESTS-STATUS NOT = "00"
               OPEN OUTPUT REQUESTS-FILE
               CLOSE REQUESTS-FILE
           ELSE
               CLOSE REQUESTS-FILE
           END-IF
           .

      *> ------------------------------------------------------------
      *> LOAD-REQUESTS
      *>   Reads pending_requests.dat into the PEND-ENTRY table.
      *>   Each line format:  <sender_username>|<recipient_username>
      *>   Call once at program start, after LOAD-ACCOUNTS.
      *> ------------------------------------------------------------
       LOAD-REQUESTS.
           MOVE 0 TO PEND-COUNT
           OPEN INPUT REQUESTS-FILE
           PERFORM UNTIL 1 = 2
               READ REQUESTS-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF PEND-COUNT < 25
                           ADD 1 TO PEND-COUNT
                           MOVE SPACES TO PEND-FROM(PEND-COUNT)
                           MOVE SPACES TO PEND-TO(PEND-COUNT)
                           UNSTRING REQ-RECORD DELIMITED BY "|"
                               INTO PEND-FROM(PEND-COUNT)
                                    PEND-TO(PEND-COUNT)
                           END-UNSTRING
                       END-IF
               END-READ
           END-PERFORM
           CLOSE REQUESTS-FILE
           .

      *> ------------------------------------------------------------
      *> SAVE-REQUESTS
      *>   Writes the in-memory PEND-ENTRY table back to
      *>   pending_requests.dat, overwriting previous contents.
      *>   Call after any change to the pending-requests table.
      *> ------------------------------------------------------------
       SAVE-REQUESTS.
           OPEN OUTPUT REQUESTS-FILE
           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > PEND-COUNT
                   MOVE SPACES TO REQ-RECORD
                   STRING
                       FUNCTION TRIM(PEND-FROM(WS-K)) "|"
                       FUNCTION TRIM(PEND-TO(WS-K))
                       DELIMITED BY SIZE
                       INTO REQ-RECORD
                   END-STRING
                   WRITE REQ-RECORD
               END-PERFORM
           END-IF
           CLOSE REQUESTS-FILE
           .

      *> ------------------------------------------------------------
      *> VALIDATE-SEND-REQUEST
      *>   Checks whether SR-SENDER is allowed to send a request to
      *>   SR-RECIPIENT.  Sets SR-VALID:
      *>     1  = request is allowed
      *>     0  = blocked (already connected or request already pending)
      *>
      *>   Blocking conditions checked:
      *>     a) SR-SENDER = SR-RECIPIENT (cannot request yourself)
      *>     b) A pending request already exists FROM sender TO recipient
      *>     c) A pending request already exists FROM recipient TO sender
      *>        (they already sent the current user a request)
      *>
      *>   NOTE: Full "already connected" checking requires a
      *>   connections table (Week 5+).  For Week 4 the pending-request
      *>   table is the only persistent store, so conditions (b) and (c)
      *>   cover all required validation per the rubric.
      *> ------------------------------------------------------------
       VALIDATE-SEND-REQUEST.
           MOVE 1 TO SR-VALID

           IF FUNCTION TRIM(SR-SENDER) = FUNCTION TRIM(SR-RECIPIENT)
               MOVE 0 TO SR-VALID
               MOVE "You cannot send a connection request to yourself."
                   TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF PEND-COUNT > 0
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > PEND-COUNT
                   IF FUNCTION TRIM(PEND-FROM(WS-K))
                          = FUNCTION TRIM(SR-SENDER)
                      AND FUNCTION TRIM(PEND-TO(WS-K))
                          = FUNCTION TRIM(SR-RECIPIENT)
                       MOVE 0 TO SR-VALID
                       MOVE "You have already sent a connection request to this user."
                           TO WS-OUT
                       PERFORM PRINT-LINE
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION TRIM(PEND-FROM(WS-K))
                          = FUNCTION TRIM(SR-RECIPIENT)
                      AND FUNCTION TRIM(PEND-TO(WS-K))
                          = FUNCTION TRIM(SR-SENDER)
                       MOVE 0 TO SR-VALID
                       MOVE "This user has already sent you a connection request."
                           TO WS-OUT
                       PERFORM PRINT-LINE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF
           .

      *> ------------------------------------------------------------
      *> SEND-REQUEST
      *>   High-level paragraph called when the logged-in user chooses
      *>   "Send Connection Request" while viewing another user's
      *>   profile.
      *>
      *>   Preconditions (set by caller before PERFORM SEND-REQUEST):
      *>     SR-SENDER    = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
      *>     SR-RECIPIENT = FUNCTION TRIM(U-NAME(WS-SEARCH-ID))
      *>
      *>   Actions:
      *>     1. Validate the request via VALIDATE-SEND-REQUEST.
      *>     2. If valid: add entry to PENDING-REQUESTS, call
      *>        SAVE-REQUESTS, and display a confirmation message.
      *>     3. If invalid: error message was already printed by
      *>        VALIDATE-SEND-REQUEST; no further action needed.
      *> ------------------------------------------------------------
       SEND-REQUEST.
           PERFORM VALIDATE-SEND-REQUEST

           IF SR-VALID = 1
               IF PEND-COUNT < 25
                   ADD 1 TO PEND-COUNT
                   MOVE FUNCTION TRIM(SR-SENDER)
                       TO PEND-FROM(PEND-COUNT)
                   MOVE FUNCTION TRIM(SR-RECIPIENT)
                       TO PEND-TO(PEND-COUNT)
                   PERFORM SAVE-REQUESTS

                   MOVE SPACES TO WS-OUT
                   STRING "Connection request sent to "
                          FUNCTION TRIM(SR-RECIPIENT) "."
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
               ELSE
                   MOVE "System error: pending request limit reached."
                       TO WS-OUT
                   PERFORM PRINT-LINE
               END-IF
           END-IF
           .
