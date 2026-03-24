      *> handles applying for jobs and preventing duplicate applications
       APPLY-FOR-JOB.
           MOVE J-ID(CHOICE) TO SELECTED-JOB-ID

      *> check if already applied
           MOVE 0 TO MATCH-FOUND
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > APP-COUNT
               IF FUNCTION TRIM(APP-USERNAME(WS-I))
                    = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
                  AND APP-JOB-ID(WS-I) = SELECTED-JOB-ID
                   MOVE 1 TO MATCH-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF MATCH-FOUND = 1
               MOVE SPACES TO WS-OUT
               STRING
                   "You have already applied for "
                   FUNCTION TRIM(J-TITLE(CHOICE))
                   " at "
                   FUNCTION TRIM(J-EMPLOYER(CHOICE))
                   "."
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF APP-COUNT >= APP-MAX
               MOVE "Error: Maximum application limit reached." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO APP-COUNT
           MOVE APP-COUNT TO WS-I
           MOVE FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
               TO APP-USERNAME(WS-I)
           MOVE SELECTED-JOB-ID
               TO APP-JOB-ID(WS-I)

           PERFORM SAVE-APPLICATIONS

           MOVE SPACES TO WS-OUT
           STRING
               "Your application for "
               FUNCTION TRIM(J-TITLE(CHOICE))
               " at "
               FUNCTION TRIM(J-EMPLOYER(CHOICE))
               " has been submitted."
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE
           .