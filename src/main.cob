       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDKBRUH.

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

       FD  INPUT-FILE.
       01  INPUT-RECORD                 PIC X(200).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD                PIC X(200).

       FD  ACCOUNTS-FILE.
       01  ACCT-RECORD                  PIC X(200).

       WORKING-STORAGE SECTION.

       01  ACCOUNTS-STATUS              PIC XX VALUE "00".

       77  INFILE-EOF                   PIC 9 VALUE 0.
       77  ACCTFILE-EOF                 PIC 9 VALUE 0.

       77  WELCOME-CHOICE               PIC 9 VALUE 0.
       77  USER-CHOICE                  PIC 9 VALUE 0.
       77  SKILL-CHOICE                 PIC 9 VALUE 0.

       77  LOGIN-SUCCESS                PIC 9 VALUE 0.
       77  FOUND                        PIC 9 VALUE 0.

       77  WS-OUT                       PIC X(200) VALUE SPACES.

       77  IN-USER                      PIC X(15) VALUE SPACES.
       77  IN-PASS                      PIC X(12) VALUE SPACES.

       77  USER-COUNT                   PIC 9 VALUE 0.

       77  WS-LEN                       PIC 99 VALUE 0.
       77  HAS-UPPER                    PIC 9 VALUE 0.
       77  HAS-DIGIT                    PIC 9 VALUE 0.
       77  HAS-SPECIAL                  PIC 9 VALUE 0.
       77  WS-I                         PIC 99 VALUE 0.
       77  WS-J                         PIC 99 VALUE 0.
       77  WS-CH                        PIC X VALUE SPACE.

       01  ACCT-TABLE.
           05 ACCT-ENTRY OCCURS 5 TIMES.
              10 ACCT-USER              PIC X(15).
              10 ACCT-PASS              PIC X(12).

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

       READ-INPUT-LINE.
           READ INPUT-FILE
               AT END
                   MOVE 1 TO INFILE-EOF
               NOT AT END
                   CONTINUE
           END-READ
           IF INFILE-EOF = 1
               PERFORM END-PROGRAM
           END-IF
           .

       ECHO-INPUT-LINE.
           MOVE INPUT-RECORD TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           .

       GET-CHOICE-1DIGIT.
           PERFORM READ-INPUT-LINE
           PERFORM ECHO-INPUT-LINE
           MOVE 0 TO WELCOME-CHOICE
           IF INPUT-RECORD(1:1) >= "0" AND INPUT-RECORD(1:1) <= "9"
               COMPUTE WELCOME-CHOICE = FUNCTION NUMVAL(INPUT-RECORD(1:1))
           END-IF
           .

       GET-USER-CHOICE-1DIGIT.
           PERFORM READ-INPUT-LINE
           PERFORM ECHO-INPUT-LINE
           MOVE 0 TO USER-CHOICE
           IF INPUT-RECORD(1:1) >= "0" AND INPUT-RECORD(1:1) <= "9"
               COMPUTE USER-CHOICE = FUNCTION NUMVAL(INPUT-RECORD(1:1))
           END-IF
           .

       GET-SKILL-CHOICE-1DIGIT.
           PERFORM READ-INPUT-LINE
           PERFORM ECHO-INPUT-LINE
           MOVE 0 TO SKILL-CHOICE
           IF INPUT-RECORD(1:1) >= "0" AND INPUT-RECORD(1:1) <= "9"
               COMPUTE SKILL-CHOICE = FUNCTION NUMVAL(INPUT-RECORD(1:1))
           END-IF
           .

       GET-USERNAME.
           PERFORM READ-INPUT-LINE
           PERFORM ECHO-INPUT-LINE
           MOVE SPACES TO IN-USER
           MOVE INPUT-RECORD(1:15) TO IN-USER
           .

       GET-PASSWORD.
           PERFORM READ-INPUT-LINE
           PERFORM ECHO-INPUT-LINE
           MOVE SPACES TO IN-PASS
           MOVE INPUT-RECORD(1:12) TO IN-PASS
           .

       ENSURE-ACCOUNTS-FILE.
           MOVE "00" TO ACCOUNTS-STATUS
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
           MOVE 0 TO ACCTFILE-EOF
           OPEN INPUT ACCOUNTS-FILE
           PERFORM UNTIL ACCTFILE-EOF = 1
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 1 TO ACCTFILE-EOF
                   NOT AT END
                       IF USER-COUNT < 5
                           ADD 1 TO USER-COUNT
                           PERFORM PARSE-ACCOUNT-LINE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS-FILE
           .

       PARSE-ACCOUNT-LINE.
           MOVE SPACES TO ACCT-USER(USER-COUNT)
           MOVE SPACES TO ACCT-PASS(USER-COUNT)

           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 200 OR ACCT-RECORD(WS-I:1) = "|"
               IF WS-I <= 15
                   MOVE ACCT-RECORD(WS-I:1)
                     TO ACCT-USER(USER-COUNT)(WS-I:1)
               END-IF
               ADD 1 TO WS-I
           END-PERFORM

           ADD 1 TO WS-I
           MOVE 1 TO WS-J
           PERFORM UNTIL WS-I > 200 OR WS-J > 12
               MOVE ACCT-RECORD(WS-I:1)
                 TO ACCT-PASS(USER-COUNT)(WS-J:1)
               ADD 1 TO WS-I
               ADD 1 TO WS-J
           END-PERFORM
           .

       SAVE-ACCOUNTS.
           OPEN OUTPUT ACCOUNTS-FILE
           IF USER-COUNT > 0
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
                   MOVE SPACES TO ACCT-RECORD
                   STRING
                       FUNCTION TRIM(ACCT-USER(WS-I))
                       "|"
                       FUNCTION TRIM(ACCT-PASS(WS-I))
                       DELIMITED BY SIZE
                       INTO ACCT-RECORD
                   END-STRING
                   WRITE ACCT-RECORD
               END-PERFORM
           END-IF
           CLOSE ACCOUNTS-FILE
           .

       MAIN-MENU.
           MOVE "Welcome to InCollege!" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE "Log In" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE "Create New Account" TO WS-OUT
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUT
           PERFORM PRINT-LINE

           PERFORM GET-CHOICE-1DIGIT

           EVALUATE WELCOME-CHOICE
               WHEN 1
                   PERFORM DO-LOGIN
                   PERFORM POST-LOGIN-MENU
               WHEN 2
                   PERFORM DO-CREATE-ACCOUNT
                   PERFORM MAIN-MENU
               WHEN OTHER
                   PERFORM MAIN-MENU
           END-EVALUATE
           .

       DO-CREATE-ACCOUNT.
           IF USER-COUNT >= 5
               MOVE "All permitted accounts have been created, please come back later"
                 TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "Please enter your username:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM GET-USERNAME

           PERFORM CHECK-USERNAME-UNIQUE
           IF FOUND = 1
               MOVE "Username already exists, please try again" TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "Please enter your password:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM GET-PASSWORD

           PERFORM VALIDATE-PASSWORD
           IF WS-LEN < 8 OR WS-LEN > 12
               MOVE "Password does not meet requirements, please try again" TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF
           IF HAS-UPPER = 0 OR HAS-DIGIT = 0 OR HAS-SPECIAL = 0
               MOVE "Password does not meet requirements, please try again" TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO USER-COUNT
           MOVE IN-USER TO ACCT-USER(USER-COUNT)
           MOVE IN-PASS TO ACCT-PASS(USER-COUNT)
           PERFORM SAVE-ACCOUNTS

           MOVE "Account created successfully" TO WS-OUT
           PERFORM PRINT-LINE
           .

       CHECK-USERNAME-UNIQUE.
           MOVE 0 TO FOUND
           IF USER-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
               IF FUNCTION TRIM(ACCT-USER(WS-I)) = FUNCTION TRIM(IN-USER)
                   MOVE 1 TO FOUND
               END-IF
           END-PERFORM
           .

       VALIDATE-PASSWORD.
           MOVE 0 TO HAS-UPPER
           MOVE 0 TO HAS-DIGIT
           MOVE 0 TO HAS-SPECIAL

           COMPUTE WS-LEN = FUNCTION LENGTH(FUNCTION TRIM(IN-PASS))
           IF WS-LEN < 8 OR WS-LEN > 12
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE IN-PASS(WS-I:1) TO WS-CH

               IF WS-CH >= "A" AND WS-CH <= "Z"
                   MOVE 1 TO HAS-UPPER
               END-IF

               IF WS-CH >= "0" AND WS-CH <= "9"
                   MOVE 1 TO HAS-DIGIT
               END-IF

               IF NOT ((WS-CH >= "A" AND WS-CH <= "Z")
                   OR (WS-CH >= "a" AND WS-CH <= "z")
                   OR (WS-CH >= "0" AND WS-CH <= "9")
                   OR  WS-CH = SPACE)
                   MOVE 1 TO HAS-SPECIAL
               END-IF
           END-PERFORM
           .

       DO-LOGIN.
           MOVE 0 TO LOGIN-SUCCESS
           PERFORM UNTIL LOGIN-SUCCESS = 1
               MOVE "Please enter your username:" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM GET-USERNAME

               MOVE "Please enter your password:" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM GET-PASSWORD

               PERFORM AUTHENTICATE

               IF LOGIN-SUCCESS = 1
                   MOVE "You have successfully logged in." TO WS-OUT
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUT
                   STRING "Welcome, "
                          FUNCTION TRIM(IN-USER)
                          "!"
                          DELIMITED BY SIZE
                          INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
               ELSE
                   MOVE "Incorrect username/password, please try again" TO WS-OUT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM
           .

       AUTHENTICATE.
           MOVE 0 TO LOGIN-SUCCESS
           IF USER-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
               IF FUNCTION TRIM(ACCT-USER(WS-I)) = FUNCTION TRIM(IN-USER)
                  AND FUNCTION TRIM(ACCT-PASS(WS-I)) = FUNCTION TRIM(IN-PASS)
                   MOVE 1 TO LOGIN-SUCCESS
               END-IF
           END-PERFORM
           .

       POST-LOGIN-MENU.
           MOVE 0 TO USER-CHOICE
           PERFORM UNTIL USER-CHOICE = 9
               MOVE "1. Search for a job" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "2. Find someone you know" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "3. Learn a new skill" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "9. Logout" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-USER-CHOICE-1DIGIT

               EVALUATE USER-CHOICE
                   WHEN 1
                       MOVE "Job search/internship is under construction." TO WS-OUT
                       PERFORM PRINT-LINE
                   WHEN 2
                       MOVE "Find someone you know is under construction." TO WS-OUT
                       PERFORM PRINT-LINE
                   WHEN 3
                       PERFORM LEARN-SKILL-MENU
                   WHEN 9
                       PERFORM END-PROGRAM
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .

       LEARN-SKILL-MENU.
           MOVE 0 TO SKILL-CHOICE
           PERFORM UNTIL SKILL-CHOICE = 6
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

               PERFORM GET-SKILL-CHOICE-1DIGIT

               IF SKILL-CHOICE >= 1 AND SKILL-CHOICE <= 5
                   MOVE "This skill is under construction." TO WS-OUT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM

           MOVE 0 TO USER-CHOICE
           .
