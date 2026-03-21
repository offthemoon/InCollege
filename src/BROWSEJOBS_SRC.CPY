      *>contains main menu for job posting and browsing
       POST-BROWSE-JOBS.
           PERFORM UNTIL 1 = 2
               MOVE "1. Post a Job/Internship" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "2. Browse Jobs/Internships" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "3. Back to Main Menu" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-1DIGIT

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM POST-JOB
                   WHEN 2
                       PERFORM BROWSE-JOBS
                   WHEN 3
                       EXIT PARAGRAPH
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .

       BROWSE-JOBS.
        *>    MOVE "Browse Jobs/Internships is under construction."
        *>        TO WS-OUT
        *>    PERFORM PRINT-LINE
               MOVE "Here are the available jobs/internships:" TO WS-OUT
               PERFORM PRINT-LINE

               *>This function here is going to call a function that is going to display all the jobs.
               PERFORM DISPLAY-JOBS
           .


       DISPLAY-JOBS.
           IF JOB-COUNT = 0
               MOVE "--- Available Job Listings ---"
                   TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > JOB-COUNT
               MOVE "----------------------------------" TO WS-OUT
               PERFORM PRINT-LINE

               MOVE SPACES TO WS-OUT
               STRING "Job #" WS-I ": "
                      FUNCTION TRIM(J-TITLE(WS-I))
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE

               MOVE SPACES TO WS-OUT
               STRING "  Employer : "
                      FUNCTION TRIM(J-EMPLOYER(WS-I))
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE

               MOVE SPACES TO WS-OUT
               STRING "  Location : "
                      FUNCTION TRIM(J-LOC(WS-I))
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE

           END-PERFORM

           MOVE "----------------------------------" TO WS-OUT
           PERFORM PRINT-LINE


           MOVE SPACES TO WS-OUT

            MOVE "Enter job number to view details, or 0 to go back:" TO WS-OUT
             PERFORM PRINT-LINE

            PERFORM GET-CHOICE-2DIGIT

             IF CHOICE = 0
                 PERFORM POST-LOGIN-MENU

              ELSE IF CHOICE > 0 AND CHOICE <= JOB-COUNT
                 MOVE SPACES TO WS-OUT

                 MOVE "--- Job Details ---" TO WS-OUT
                 PERFORM PRINT-LINE

                 STRING "Job Title: "
                        FUNCTION TRIM(J-TITLE(CHOICE))
                     DELIMITED BY SIZE
                     INTO WS-OUT
                 END-STRING
                 PERFORM PRINT-LINE

                 MOVE SPACES TO WS-OUT
                 STRING "  Employer   : "
                        FUNCTION TRIM(J-EMPLOYER(CHOICE))
                     DELIMITED BY SIZE
                     INTO WS-OUT
                 END-STRING
                 PERFORM PRINT-LINE

                 MOVE SPACES TO WS-OUT
                 STRING "  Location   : "
                        FUNCTION TRIM(J-LOC(CHOICE))
                     DELIMITED BY SIZE
                     INTO WS-OUT
                 END-STRING
                 PERFORM PRINT-LINE

                 MOVE SPACES TO WS-OUT
                 STRING "  Salary     : "
                        FUNCTION TRIM(J-SALARY(CHOICE))
                     DELIMITED BY SIZE
                     INTO WS-OUT
                 END-STRING
                 PERFORM PRINT-LINE

                 MOVE SPACES TO WS-OUT
                 STRING "  Description: "
                        FUNCTION TRIM(J-DESC(CHOICE))
                     DELIMITED BY SIZE
                     INTO WS-OUT
                 END-STRING
                 PERFORM PRINT-LINE


                 MOVE "-------------------------------" TO WS-OUT
                 PERFORM PRINT-LINE


               *> We are going to have to implmenet a function that allows users to apply.

       GET-CHOICE-2DIGIT.
           PERFORM READ-INPUT
           MOVE 0 TO CHOICE
           IF WS-OUT(1:1) >= "0" AND WS-OUT(1:1) <= "9"
               COMPUTE CHOICE = FUNCTION NUMVAL(WS-OUT(1:2))
           END-IF
           .
