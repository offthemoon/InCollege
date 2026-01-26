       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts_info.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ACCOUNTS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD                 PIC X(200).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD                PIC X(200).

       FD ACCOUNTS-FILE.
       01 ACCT-RECORD                  PIC X(200).

       WORKING-STORAGE SECTION.

       01 ACCOUNTS-STATUS              PIC XX VALUE "00".
       01 INFILE-EOF                   PIC 9 VALUE 0.

       01 CHOICE                       PIC 9 VALUE 0.
       01 LOGIN-SUCCESS                PIC 9 VALUE 0.
       01 USER-COUNT                   PIC 9 VALUE 0.
       01 FOUND                        PIC 9 VALUE 0.

       01 WS-OUT                       PIC X(200) VALUE SPACES.
       01 USERNAME                     PIC X(15)  VALUE SPACES.
       01 PASSWORD                     PIC X(12)  VALUE SPACES.

       01 WS-I                         PIC 99 VALUE 0.
       01 WS-LEN                       PIC 99 VALUE 0.
       01 WS-CH                        PIC X  VALUE SPACE.
       01 HAS-UPPER                    PIC 9  VALUE 0.
       01 HAS-DIGIT                    PIC 9  VALUE 0.
       01 HAS-SPECIAL                  PIC 9  VALUE 0.

       01 USERS.
           05 USER-ENTRY OCCURS 5 TIMES.
               10 U-NAME               PIC X(15).
               10 U-PASS               PIC X(12).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           PERFORM ENSURE-ACCOUNTS-FILE
           PERFORM LOAD-ACCOUNTS
           PERFORM MAIN-MENU
           PERFORM END-PROGRAM
           .

       END-PROGRAM.
           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO WS-OUT
           PERFORM PRINT-LINE
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN
           .

       PRINT-LINE.
           DISPLAY WS-OUT
           MOVE WS-OUT TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           .

       READ-INPUT.
           READ INPUT-FILE
               AT END
                   MOVE 1 TO INFILE-EOF
           END-READ
           IF INFILE-EOF = 1
               PERFORM END-PROGRAM
           END-IF
           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-OUT
           DISPLAY WS-OUT
           MOVE WS-OUT TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           .

       GET-CHOICE-1DIGIT.
           PERFORM READ-INPUT
           MOVE 0 TO CHOICE
           IF WS-OUT(1:1) >= "0" AND WS-OUT(1:1) <= "9"
               COMPUTE CHOICE = FUNCTION NUMVAL(WS-OUT(1:1))
           END-IF
           .

       ENSURE-ACCOUNTS-FILE.
           OPEN INPUT ACCOUNTS-FILE
           IF ACCOUNTS-STATUS NOT = "00"
               OPEN OUTPUT ACCOUNTS-FILE
               CLOSE ACCOUNTS-FILE
           ELSE
               CLOSE ACCOUNTS-FILE
           END-IF
           .

       LOAD-ACCOUNTS.
           MOVE 0 TO USER-COUNT
           OPEN INPUT ACCOUNTS-FILE
           PERFORM UNTIL 1 = 2
               READ ACCOUNTS-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF USER-COUNT < 5
                           ADD 1 TO USER-COUNT
                           MOVE SPACES TO U-NAME(USER-COUNT)
                           MOVE SPACES TO U-PASS(USER-COUNT)
                           UNSTRING ACCT-RECORD
                               DELIMITED BY "|"
                               INTO U-NAME(USER-COUNT)
                                    U-PASS(USER-COUNT)
                           END-UNSTRING
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS-FILE
           .

       SAVE-ACCOUNTS.
           OPEN OUTPUT ACCOUNTS-FILE
           IF USER-COUNT > 0
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
                   MOVE SPACES TO ACCT-RECORD
                   STRING
                       FUNCTION TRIM(U-NAME(WS-I))
                       "|"
                       FUNCTION TRIM(U-PASS(WS-I))
                       DELIMITED BY SIZE
                       INTO ACCT-RECORD
                   END-STRING
                   WRITE ACCT-RECORD
               END-PERFORM
           END-IF
           CLOSE ACCOUNTS-FILE
           .

       MAIN-MENU.
           PERFORM UNTIL 1 = 2
               MOVE "Welcome to InCollege!" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "1. Log In" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "2. Create New Account" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "3. Exit" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-1DIGIT

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM LOGIN
                   WHEN 2
                       PERFORM CREATE-ACCOUNT
                   WHEN 3
                       PERFORM END-PROGRAM
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .

       CREATE-ACCOUNT.
           IF USER-COUNT >= 5
               MOVE "All permitted accounts have been created, please co"
      -            "me back later" TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "Please enter your username:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           MOVE SPACES TO USERNAME
           MOVE WS-OUT(1:15) TO USERNAME

           PERFORM CHECK-USERNAME
           IF FOUND = 1
               MOVE "Username already exists, please try again"
      -             TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "Please enter your password:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           MOVE SPACES TO PASSWORD
           MOVE WS-OUT(1:12) TO PASSWORD

           PERFORM VALIDATE-PASSWORD

           IF WS-LEN < 8 OR WS-LEN > 12
               MOVE "Password does not meet requirements, please try aga"
      -            "in" TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF HAS-UPPER = 0 OR HAS-DIGIT = 0 OR HAS-SPECIAL = 0
               MOVE "Password does not meet requirements, please try aga"
      -            "in" TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO USER-COUNT
           MOVE USERNAME TO U-NAME(USER-COUNT)
           MOVE PASSWORD TO U-PASS(USER-COUNT)
           PERFORM SAVE-ACCOUNTS

           MOVE "Account created successfully" TO WS-OUT
           PERFORM PRINT-LINE
           .

       CHECK-USERNAME.
           MOVE 0 TO FOUND
           IF USER-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
               IF FUNCTION TRIM(U-NAME(WS-I)) = FUNCTION TRIM(USERNAME)
                   MOVE 1 TO FOUND
               END-IF
           END-PERFORM
           .

       VALIDATE-PASSWORD.
           MOVE 0 TO HAS-UPPER
           MOVE 0 TO HAS-DIGIT
           MOVE 0 TO HAS-SPECIAL

           COMPUTE WS-LEN = FUNCTION LENGTH(FUNCTION TRIM(PASSWORD))

           IF WS-LEN < 8 OR WS-LEN > 12
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE PASSWORD(WS-I:1) TO WS-CH

               IF WS-CH >= "A" AND WS-CH <= "Z"
                   MOVE 1 TO HAS-UPPER
               END-IF

               IF WS-CH >= "0" AND WS-CH <= "9"
                   MOVE 1 TO HAS-DIGIT
               END-IF

               IF NOT ((WS-CH >= "A" AND WS-CH <= "Z")
                   OR (WS-CH >= "a" AND WS-CH <= "z")
                   OR (WS-CH >= "0" AND WS-CH <= "9"))
                   MOVE 1 TO HAS-SPECIAL
               END-IF
           END-PERFORM
           .

       LOGIN.
           MOVE 0 TO LOGIN-SUCCESS
           PERFORM UNTIL LOGIN-SUCCESS = 1
               MOVE "Please enter your username:" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE SPACES TO USERNAME
               MOVE WS-OUT(1:15) TO USERNAME

               MOVE "Please enter your password:" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE SPACES TO PASSWORD
               MOVE WS-OUT(1:12) TO PASSWORD

               PERFORM AUTHENTICATE

               IF LOGIN-SUCCESS = 1
                   MOVE "You have successfully logged in." TO WS-OUT
                   PERFORM PRINT-LINE
               ELSE
                   MOVE "Incorrect username/password, please try again"
                       TO WS-OUT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM

           PERFORM POST-LOGIN-MENU
           .

       AUTHENTICATE.
           MOVE 0 TO LOGIN-SUCCESS
           IF USER-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
               IF FUNCTION TRIM(U-NAME(WS-I)) = FUNCTION TRIM(USERNAME)
      -           AND FUNCTION TRIM(U-PASS(WS-I)) = FUNCTION TRIM(
      -           PASSWORD)
                   MOVE 1 TO LOGIN-SUCCESS
               END-IF
           END-PERFORM
           .

       POST-LOGIN-MENU.
           PERFORM UNTIL 1 = 2
               MOVE "1. Search for a job" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "2. Find someone you know" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "3. Learn a new skill" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "4. Logout" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-1DIGIT

               EVALUATE CHOICE
                   WHEN 1
                       MOVE "Job search/internship is under construction"
      -                    "." TO WS-OUT
                       PERFORM PRINT-LINE
                   WHEN 2
                       MOVE "Find someone you know is under construction"
      -                    "." TO WS-OUT
                       PERFORM PRINT-LINE
                   WHEN 3
                       PERFORM LEARN-SKILL-MENU
                   WHEN 4
                       EXIT PARAGRAPH
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .

       LEARN-SKILL-MENU.
           PERFORM UNTIL 1 = 2
               MOVE "Learn a New Skill:" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Skill 1" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Skill 2" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Skill 3" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Skill 4" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Skill 5" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Go Back" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-1DIGIT

               IF CHOICE >= 1 AND CHOICE <= 5
                   MOVE "This skill is under construction." TO WS-OUT
                   PERFORM PRINT-LINE
               ELSE
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           .