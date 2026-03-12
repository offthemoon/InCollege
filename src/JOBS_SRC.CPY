      *>manages job and internship posting, including storing, creating
       ENSURE-JOBS-FILE.
           MOVE "00" TO JOBS-STATUS
           OPEN INPUT JOBS-FILE
           IF JOBS-STATUS NOT = "00"
               OPEN OUTPUT JOBS-FILE
               CLOSE JOBS-FILE
           ELSE
               CLOSE JOBS-FILE
           END-IF
           .

       LOAD-JOBS.
           MOVE 0 TO JOB-COUNT
           MOVE 1 TO NEXT-JOB-ID
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > JOB-MAX
               MOVE 0 TO J-ID(WS-I)
               MOVE SPACES TO J-TITLE(WS-I)
               MOVE SPACES TO J-DESC(WS-I)
               MOVE SPACES TO J-EMPLOYER(WS-I)
               MOVE SPACES TO J-LOC(WS-I)
               MOVE SPACES TO J-SALARY(WS-I)
               MOVE SPACES TO J-POSTED-BY(WS-I)
           END-PERFORM

           OPEN INPUT JOBS-FILE
           PERFORM UNTIL 1 = 2
               READ JOBS-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF JOB-COUNT < JOB-MAX
                           ADD 1 TO JOB-COUNT
                           MOVE JOB-COUNT TO WS-I
                           MOVE 0 TO J-ID(WS-I)
                           MOVE SPACES TO J-TITLE(WS-I)
                           MOVE SPACES TO J-DESC(WS-I)
                           MOVE SPACES TO J-EMPLOYER(WS-I)
                           MOVE SPACES TO J-LOC(WS-I)
                           MOVE SPACES TO J-SALARY(WS-I)
                           MOVE SPACES TO J-POSTED-BY(WS-I)
                           UNSTRING JOB-RECORD DELIMITED BY "|"
                               INTO J-ID(WS-I)
                                    J-TITLE(WS-I)
                                    J-DESC(WS-I)
                                    J-EMPLOYER(WS-I)
                                    J-LOC(WS-I)
                                    J-SALARY(WS-I)
                                    J-POSTED-BY(WS-I)
                           END-UNSTRING
      *>update NEXT-JOB-ID to be higher than any existing job ID
                           IF J-ID(WS-I) >= NEXT-JOB-ID
                               COMPUTE NEXT-JOB-ID = J-ID(WS-I) + 1
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JOBS-FILE
           .

       SAVE-JOBS.
           OPEN OUTPUT JOBS-FILE
           IF JOB-COUNT > 0
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > JOB-COUNT
                   MOVE SPACES TO JOB-RECORD
                   MOVE J-ID(WS-I) TO WS-K
                   STRING
                       WS-K "|"
                       FUNCTION TRIM(J-TITLE(WS-I)) "|"
                       FUNCTION TRIM(J-DESC(WS-I)) "|"
                       FUNCTION TRIM(J-EMPLOYER(WS-I)) "|"
                       FUNCTION TRIM(J-LOC(WS-I)) "|"
                       FUNCTION TRIM(J-SALARY(WS-I)) "|"
                       FUNCTION TRIM(J-POSTED-BY(WS-I))
                       DELIMITED BY SIZE
                       INTO JOB-RECORD
                   END-STRING
                   WRITE JOB-RECORD
               END-PERFORM
           END-IF
           CLOSE JOBS-FILE
           .

       POST-JOB.
           IF JOB-COUNT >= JOB-MAX
               MOVE "Error: Maximum number of job postings reached (10)."
                   TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

      *>increment job count and set index
           ADD 1 TO JOB-COUNT
           MOVE JOB-COUNT TO WS-I

      *>assign unique sequential ID
           MOVE NEXT-JOB-ID TO J-ID(WS-I)
           ADD 1 TO NEXT-JOB-ID

      *>get title (required)
           MOVE SPACES TO WS-OUT
           PERFORM UNTIL WS-OUT NOT = SPACES
               MOVE "Enter Job Title:" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               IF WS-OUT = SPACES
                   MOVE "Job title cannot be empty. Please try again."
                       TO WS-OUT
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
               END-IF
           END-PERFORM
           MOVE WS-OUT TO J-TITLE(WS-I)

      *>get description (required)
           MOVE SPACES TO WS-OUT
           PERFORM UNTIL WS-OUT NOT = SPACES
               MOVE "Enter Description (max 200 chars):" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               IF WS-OUT = SPACES
                   MOVE "Description cannot be empty. Please try again."
                       TO WS-OUT
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
               END-IF
           END-PERFORM
           MOVE WS-OUT TO J-DESC(WS-I)

      *>get employer name (required)
           MOVE SPACES TO WS-OUT
           PERFORM UNTIL WS-OUT NOT = SPACES
               MOVE "Enter Employer Name:" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               IF WS-OUT = SPACES
                   MOVE "Employer name cannot be empty. Please try again."
                       TO WS-OUT
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
               END-IF
           END-PERFORM
           MOVE WS-OUT TO J-EMPLOYER(WS-I)

      *>get location (required)
           MOVE SPACES TO WS-OUT
           PERFORM UNTIL WS-OUT NOT = SPACES
               MOVE "Enter Location:" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               IF WS-OUT = SPACES
                   MOVE "Location cannot be empty. Please try again."
                       TO WS-OUT
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
               END-IF
           END-PERFORM
           MOVE WS-OUT TO J-LOC(WS-I)

      *>get salary (optional)
           MOVE "Enter Salary (optional, enter 'NONE' to skip):" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           MOVE WS-OUT TO J-SALARY(WS-I)

      *>record who posted this job
           MOVE U-NAME(CURRENT-USER-ID) TO J-POSTED-BY(WS-I)

      *>save jobs to file
           PERFORM SAVE-JOBS

           MOVE "Job posted successfully!" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE "----------------------------------" TO WS-OUT
           PERFORM PRINT-LINE
       .
