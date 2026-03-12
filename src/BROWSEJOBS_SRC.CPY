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
           MOVE "Browse Jobs/Internships is under construction."
               TO WS-OUT
           PERFORM PRINT-LINE
           .
