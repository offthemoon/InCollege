      *> contains job search submenu, browse, detail view, apply, report
       POST-BROWSE-JOBS.
           PERFORM UNTIL 1 = 2
               MOVE "--- Job Search/Internship Menu ---" TO WS-OUT
               PERFORM PRINT-LINE

               MOVE "1. Post a Job/Internship" TO WS-OUT
               PERFORM PRINT-LINE

               MOVE "2. Browse Jobs/Internships" TO WS-OUT
               PERFORM PRINT-LINE

               MOVE "3. View My Applications" TO WS-OUT
               PERFORM PRINT-LINE

               MOVE "4. Back to Main Menu" TO WS-OUT
               PERFORM PRINT-LINE

               MOVE "Enter your choice:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-1DIGIT

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM POST-JOB
                   WHEN 2
                       PERFORM BROWSE-JOBS
                   WHEN 3
                       PERFORM VIEW-MY-APPLICATIONS
                   WHEN 4
                       EXIT PARAGRAPH
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-OUT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM
           .

       BROWSE-JOBS.
           MOVE "--- Available Job Listings ---" TO WS-OUT
           PERFORM PRINT-LINE

           IF JOB-COUNT = 0
               MOVE "No jobs/internships are currently available." TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "-----------------------------" TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > JOB-COUNT
               MOVE SPACES TO WS-OUT
               STRING
                   WS-I ". "
                   FUNCTION TRIM(J-TITLE(WS-I))
                   " at "
                   FUNCTION TRIM(J-EMPLOYER(WS-I))
                   " ("
                   FUNCTION TRIM(J-LOC(WS-I))
                   ")"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
           END-PERFORM

           MOVE "-----------------------------" TO WS-OUT
           PERFORM PRINT-LINE

           PERFORM BROWSE-JOBS-DETAIL-LOOP
           .

       BROWSE-JOBS-DETAIL-LOOP.
           PERFORM UNTIL 1 = 2
               MOVE "Enter job number to view details, or 0 to go back:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-2DIGIT

               IF CHOICE = 0
                   EXIT PARAGRAPH
               END-IF

               IF CHOICE < 1 OR CHOICE > JOB-COUNT
                   MOVE "Invalid job number. Please try again." TO WS-OUT
                   PERFORM PRINT-LINE
               ELSE
                   PERFORM SHOW-JOB-DETAILS
               END-IF
           END-PERFORM
           .

       SHOW-JOB-DETAILS.
           MOVE CHOICE TO SELECTED-JOB-INDEX
           MOVE "--- Job Details ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "Title: "
                  FUNCTION TRIM(J-TITLE(SELECTED-JOB-INDEX))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "Description: "
                  FUNCTION TRIM(J-DESC(SELECTED-JOB-INDEX))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "Employer: "
                  FUNCTION TRIM(J-EMPLOYER(SELECTED-JOB-INDEX))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "Location: "
                  FUNCTION TRIM(J-LOC(SELECTED-JOB-INDEX))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           IF FUNCTION TRIM(J-SALARY(SELECTED-JOB-INDEX)) NOT = SPACES
              AND FUNCTION TRIM(J-SALARY(SELECTED-JOB-INDEX)) NOT = "NONE"
               MOVE SPACES TO WS-OUT
               STRING "Salary: "
                      FUNCTION TRIM(J-SALARY(SELECTED-JOB-INDEX))
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
           END-IF

           MOVE "-------------------" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE "1. Apply for this Job" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE "2. Back to Job List" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE "Enter your choice:" TO WS-OUT
           PERFORM PRINT-LINE

           PERFORM GET-CHOICE-1DIGIT

           EVALUATE CHOICE
               WHEN 1
                   PERFORM APPLY-FOR-JOB
               WHEN 2
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice. Returning to job list." TO WS-OUT
                   PERFORM PRINT-LINE
           END-EVALUATE
           .

       GET-CHOICE-2DIGIT.
           PERFORM READ-INPUT
           MOVE 0 TO CHOICE
           IF WS-OUT(1:1) >= "0" AND WS-OUT(1:1) <= "9"
               COMPUTE CHOICE = FUNCTION NUMVAL(WS-OUT(1:2))
           END-IF
           .