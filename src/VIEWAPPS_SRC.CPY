      *> handles the job application summary report
       VIEW-MY-APPLICATIONS.
           MOVE "--- Your Job Applications ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING
               "Application Summary for "
               FUNCTION TRIM(U-NAME(CURRENT-USER-ID))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE "------------------------------" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE 0 TO APP-TOTAL-FOR-USER

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > APP-COUNT
               IF FUNCTION TRIM(APP-USERNAME(WS-J))
                    = FUNCTION TRIM(U-NAME(CURRENT-USER-ID))

                   ADD 1 TO APP-TOTAL-FOR-USER
                   MOVE APP-JOB-ID(WS-J) TO SELECTED-JOB-ID
                   PERFORM FIND-JOB-BY-ID

                   IF MATCH-FOUND = 1
                       MOVE SPACES TO WS-OUT
                       STRING
                           "Job Title: "
                           FUNCTION TRIM(J-TITLE(WS-I))
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE SPACES TO WS-OUT
                       STRING
                           "Employer: "
                           FUNCTION TRIM(J-EMPLOYER(WS-I))
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE SPACES TO WS-OUT
                       STRING
                           "Location: "
                           FUNCTION TRIM(J-LOC(WS-I))
                           DELIMITED BY SIZE
                           INTO WS-OUT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE "---" TO WS-OUT
                       PERFORM PRINT-LINE
                   END-IF
               END-IF
           END-PERFORM

           IF APP-TOTAL-FOR-USER = 0
               MOVE "You have not applied to any jobs yet." TO WS-OUT
               PERFORM PRINT-LINE
           END-IF

           MOVE "------------------------------" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING
               "Total Applications: "
               FUNCTION TRIM(FUNCTION NUMVAL-C(APP-TOTAL-FOR-USER))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING

      *> safer numeric display
           MOVE APP-TOTAL-FOR-USER TO WS-NUM-TXT
           MOVE SPACES TO WS-OUT
           STRING
               "Total Applications: "
               FUNCTION TRIM(WS-NUM-TXT)
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE "------------------------------" TO WS-OUT
           PERFORM PRINT-LINE
           .
