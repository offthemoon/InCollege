       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts_info.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ACCOUNTS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD                     PIC X(300).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD                    PIC X(300).

       FD ACCOUNTS-FILE.
       01 ACCT-RECORD                      PIC X(4000).

       WORKING-STORAGE SECTION.

       01 ACCOUNTS-STATUS                  PIC XX VALUE "00".
       01 INFILE-EOF                       PIC 9 VALUE 0.

       01 INPUT-FILENAME                   PIC X(120) VALUE "Wk3-In/t8.txt".
       01 OUTPUT-FILENAME                  PIC X(120) VALUE "Wk3-Out/t8.txt".
       01 CMDLINE                          PIC X(120) VALUE SPACES.
       01 TEST-ID                          PIC X(40)  VALUE SPACES.

       01 CHOICE                           PIC 9 VALUE 0.
       01 LOGIN-SUCCESS                    PIC 9 VALUE 0.
       01 USER-COUNT                       PIC 9 VALUE 0.
       01 FOUND                            PIC 9 VALUE 0.
       01 CURRENT-USER-ID                  PIC 9 VALUE 0.

       01 WS-OUT                           PIC X(300) VALUE SPACES.
       01 USERNAME                         PIC X(15)  VALUE SPACES.
       01 PASSWORD                         PIC X(12)  VALUE SPACES.

       01 WS-I                             PIC 99 VALUE 0.
       01 WS-J                             PIC 99 VALUE 0.
       01 WS-K                             PIC 99 VALUE 0.
       01 WS-LEN                           PIC 99 VALUE 0.
       01 WS-CH                            PIC X  VALUE SPACE.
       01 HAS-UPPER                        PIC 9  VALUE 0.
       01 HAS-DIGIT                        PIC 9  VALUE 0.
       01 HAS-SPECIAL                      PIC 9  VALUE 0.

       01 DIGIT-CHAR                       PIC X VALUE "1".

       01 GRAD-YEAR-TEXT                   PIC X(4) VALUE SPACES.
       01 GRAD-YEAR-NUM                    PIC 9(4) VALUE 0.
       01 YEAR-OK                          PIC 9 VALUE 0.

       01 EXP-COUNT-TXT                    PIC X(2) VALUE SPACES.
       01 EDU-COUNT-TXT                    PIC X(2) VALUE SPACES.

       01 EXP-ALL                          PIC X(1200) VALUE SPACES.
       01 EDU-ALL                          PIC X(600)  VALUE SPACES.
       01 EXP-ITEM                         PIC X(400)  VALUE SPACES.
       01 EDU-ITEM                         PIC X(200)  VALUE SPACES.

       01 EXP-PTR                          PIC 9(4) VALUE 1.
       01 EDU-PTR                          PIC 9(4) VALUE 1.

       01 USERS.
           05 USER-ENTRY OCCURS 5 TIMES.
               10 U-NAME                   PIC X(15).
               10 U-PASS                   PIC X(12).
               10 U-FNAME                  PIC X(15).
               10 U-LNAME                  PIC X(15).
               10 U-UNIV                   PIC X(50).
               10 U-MAJOR                  PIC X(50).
               10 U-GRAD                   PIC X(4).
               10 U-ABOUT                  PIC X(200).
               10 U-EXP-COUNT              PIC 9.
               10 U-EDU-COUNT              PIC 9.
               10 U-EXP.
                  15 U-EXP-ENTRY OCCURS 3 TIMES.
                     20 U-EXP-TITLE        PIC X(50).
                     20 U-EXP-COMP         PIC X(50).
                     20 U-EXP-DATES        PIC X(50).
                     20 U-EXP-DESC         PIC X(100).
               10 U-EDU.
                  15 U-EDU-ENTRY OCCURS 3 TIMES.
                     20 U-EDU-DEGREE       PIC X(50).
                     20 U-EDU-UNIV         PIC X(50).
                     20 U-EDU-YEARS        PIC X(20).

       PROCEDURE DIVISION.
       MAIN.
           PERFORM SETUP-FILENAMES
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           PERFORM ENSURE-ACCOUNTS-FILE
           PERFORM LOAD-ACCOUNTS
           PERFORM MAIN-MENU
           PERFORM END-PROGRAM
           .

       SETUP-FILENAMES.
           MOVE SPACES TO CMDLINE
           ACCEPT CMDLINE FROM COMMAND-LINE
           MOVE FUNCTION TRIM(CMDLINE) TO TEST-ID
           IF TEST-ID NOT = SPACES
               MOVE SPACES TO INPUT-FILENAME
               MOVE SPACES TO OUTPUT-FILENAME
               STRING "InCollege-Input-"  FUNCTION TRIM(TEST-ID) ".txt"
                   DELIMITED BY SIZE
                   INTO INPUT-FILENAME
               END-STRING
               STRING "InCollege-Output-" FUNCTION TRIM(TEST-ID) ".txt"
                   DELIMITED BY SIZE
                   INTO OUTPUT-FILENAME
               END-STRING
           END-IF
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
           MOVE "00" TO ACCOUNTS-STATUS
           OPEN INPUT ACCOUNTS-FILE
           IF ACCOUNTS-STATUS NOT = "00"
               OPEN OUTPUT ACCOUNTS-FILE
               CLOSE ACCOUNTS-FILE
           ELSE
               CLOSE ACCOUNTS-FILE
           END-IF
           .

       CLEAR-USER-ROW.
           MOVE SPACES TO U-NAME(WS-I)
           MOVE SPACES TO U-PASS(WS-I)
           MOVE SPACES TO U-FNAME(WS-I)
           MOVE SPACES TO U-LNAME(WS-I)
           MOVE SPACES TO U-UNIV(WS-I)
           MOVE SPACES TO U-MAJOR(WS-I)
           MOVE SPACES TO U-GRAD(WS-I)
           MOVE SPACES TO U-ABOUT(WS-I)
           MOVE 0 TO U-EXP-COUNT(WS-I)
           MOVE 0 TO U-EDU-COUNT(WS-I)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE SPACES TO U-EXP-TITLE(WS-I, WS-J)
               MOVE SPACES TO U-EXP-COMP(WS-I, WS-J)
               MOVE SPACES TO U-EXP-DATES(WS-I, WS-J)
               MOVE SPACES TO U-EXP-DESC(WS-I, WS-J)
               MOVE SPACES TO U-EDU-DEGREE(WS-I, WS-J)
               MOVE SPACES TO U-EDU-UNIV(WS-I, WS-J)
               MOVE SPACES TO U-EDU-YEARS(WS-I, WS-J)
           END-PERFORM
           .

       LOAD-ACCOUNTS.
           MOVE 0 TO USER-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               PERFORM CLEAR-USER-ROW
           END-PERFORM

           OPEN INPUT ACCOUNTS-FILE
           PERFORM UNTIL 1 = 2
               READ ACCOUNTS-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF USER-COUNT < 5
                           ADD 1 TO USER-COUNT
                           MOVE USER-COUNT TO WS-I
                           PERFORM CLEAR-USER-ROW
                           PERFORM PARSE-ACCOUNT-LINE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS-FILE
           .

       PARSE-ACCOUNT-LINE.
           MOVE SPACES TO EXP-COUNT-TXT
           MOVE SPACES TO EDU-COUNT-TXT
           MOVE SPACES TO EXP-ALL
           MOVE SPACES TO EDU-ALL

           UNSTRING ACCT-RECORD DELIMITED BY "|"
               INTO U-NAME(WS-I)
                    U-PASS(WS-I)
                    U-FNAME(WS-I)
                    U-LNAME(WS-I)
                    U-UNIV(WS-I)
                    U-MAJOR(WS-I)
                    U-GRAD(WS-I)
                    U-ABOUT(WS-I)
                    EXP-COUNT-TXT
                    EXP-ALL
                    EDU-COUNT-TXT
                    EDU-ALL
           END-UNSTRING

           IF EXP-COUNT-TXT(1:1) >= "0" AND EXP-COUNT-TXT(1:1) <= "3"
               COMPUTE U-EXP-COUNT(WS-I) =
                   FUNCTION NUMVAL(EXP-COUNT-TXT(1:1))
           ELSE
               MOVE 0 TO U-EXP-COUNT(WS-I)
           END-IF

           IF EDU-COUNT-TXT(1:1) >= "0" AND EDU-COUNT-TXT(1:1) <= "3"
               COMPUTE U-EDU-COUNT(WS-I) =
                   FUNCTION NUMVAL(EDU-COUNT-TXT(1:1))
           ELSE
               MOVE 0 TO U-EDU-COUNT(WS-I)
           END-IF

           PERFORM PARSE-EXP
           PERFORM PARSE-EDU
           .

       PARSE-EXP.
           MOVE 1 TO EXP-PTR
           IF U-EXP-COUNT(WS-I) = 0
               EXIT PARAGRAPH
           END-IF
           IF EXP-ALL = SPACES
               MOVE 0 TO U-EXP-COUNT(WS-I)
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > U-EXP-COUNT(WS-I)
               MOVE SPACES TO EXP-ITEM
               UNSTRING EXP-ALL DELIMITED BY "~"
                   INTO EXP-ITEM
                   WITH POINTER EXP-PTR
               END-UNSTRING

               UNSTRING EXP-ITEM DELIMITED BY "^"
                   INTO U-EXP-TITLE(WS-I, WS-J)
                        U-EXP-COMP(WS-I, WS-J)
                        U-EXP-DATES(WS-I, WS-J)
                        U-EXP-DESC(WS-I, WS-J)
               END-UNSTRING
           END-PERFORM
           .

       PARSE-EDU.
           MOVE 1 TO EDU-PTR
           IF U-EDU-COUNT(WS-I) = 0
               EXIT PARAGRAPH
           END-IF
           IF EDU-ALL = SPACES
               MOVE 0 TO U-EDU-COUNT(WS-I)
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > U-EDU-COUNT(WS-I)
               MOVE SPACES TO EDU-ITEM
               UNSTRING EDU-ALL DELIMITED BY "~"
                   INTO EDU-ITEM
                   WITH POINTER EDU-PTR
               END-UNSTRING

               UNSTRING EDU-ITEM DELIMITED BY "^"
                   INTO U-EDU-DEGREE(WS-I, WS-J)
                        U-EDU-UNIV(WS-I, WS-J)
                        U-EDU-YEARS(WS-I, WS-J)
               END-UNSTRING
           END-PERFORM
           .

       SAVE-ACCOUNTS.
           OPEN OUTPUT ACCOUNTS-FILE
           IF USER-COUNT > 0
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
                   PERFORM BUILD-EXP-ALL
                   PERFORM BUILD-EDU-ALL
                   MOVE SPACES TO ACCT-RECORD
                   STRING
                       FUNCTION TRIM(U-NAME(WS-I)) "|"
                       FUNCTION TRIM(U-PASS(WS-I)) "|"
                       FUNCTION TRIM(U-FNAME(WS-I)) "|"
                       FUNCTION TRIM(U-LNAME(WS-I)) "|"
                       FUNCTION TRIM(U-UNIV(WS-I)) "|"
                       FUNCTION TRIM(U-MAJOR(WS-I)) "|"
                       FUNCTION TRIM(U-GRAD(WS-I)) "|"
                       FUNCTION TRIM(U-ABOUT(WS-I)) "|"
                       FUNCTION TRIM(EXP-COUNT-TXT) "|"
                       FUNCTION TRIM(EXP-ALL) "|"
                       FUNCTION TRIM(EDU-COUNT-TXT) "|"
                       FUNCTION TRIM(EDU-ALL)
                       DELIMITED BY SIZE
                       INTO ACCT-RECORD
                   END-STRING
                   WRITE ACCT-RECORD
               END-PERFORM
           END-IF
           CLOSE ACCOUNTS-FILE
           .

       BUILD-EXP-ALL.
           MOVE SPACES TO EXP-ALL
           MOVE "0" TO EXP-COUNT-TXT
           IF U-EXP-COUNT(WS-I) = 0
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION CHAR(U-EXP-COUNT(WS-I) + 48)
               TO EXP-COUNT-TXT(1:1)

           MOVE 1 TO EXP-PTR
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > U-EXP-COUNT(WS-I)
               IF WS-J > 1
                   STRING "~" DELIMITED BY SIZE
                       INTO EXP-ALL WITH POINTER EXP-PTR
                   END-STRING
               END-IF

               STRING
                   FUNCTION TRIM(U-EXP-TITLE(WS-I, WS-J)) "^"
                   FUNCTION TRIM(U-EXP-COMP(WS-I, WS-J)) "^"
                   FUNCTION TRIM(U-EXP-DATES(WS-I, WS-J)) "^"
                   FUNCTION TRIM(U-EXP-DESC(WS-I, WS-J))
                   DELIMITED BY SIZE
                   INTO EXP-ALL WITH POINTER EXP-PTR
               END-STRING
           END-PERFORM
           .

       BUILD-EDU-ALL.
           MOVE SPACES TO EDU-ALL
           MOVE "0" TO EDU-COUNT-TXT
           IF U-EDU-COUNT(WS-I) = 0
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION CHAR(U-EDU-COUNT(WS-I) + 48)
               TO EDU-COUNT-TXT(1:1)

           MOVE 1 TO EDU-PTR
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > U-EDU-COUNT(WS-I)
               IF WS-J > 1
                   STRING "~" DELIMITED BY SIZE
                       INTO EDU-ALL WITH POINTER EDU-PTR
                   END-STRING
               END-IF

               STRING
                   FUNCTION TRIM(U-EDU-DEGREE(WS-I, WS-J)) "^"
                   FUNCTION TRIM(U-EDU-UNIV(WS-I, WS-J)) "^"
                   FUNCTION TRIM(U-EDU-YEARS(WS-I, WS-J))
                   DELIMITED BY SIZE
                   INTO EDU-ALL WITH POINTER EDU-PTR
               END-STRING
           END-PERFORM
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
               MOVE "All permitted accounts have been created, please come back later"
                   TO WS-OUT
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
               MOVE "Username already exists, please try again" TO WS-OUT
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
               MOVE "Password does not meet requirements, please try again"
                   TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF HAS-UPPER = 0 OR HAS-DIGIT = 0 OR HAS-SPECIAL = 0
               MOVE "Password does not meet requirements, please try again"
                   TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO USER-COUNT
           MOVE USER-COUNT TO WS-I
           PERFORM CLEAR-USER-ROW
           MOVE USERNAME TO U-NAME(WS-I)
           MOVE PASSWORD TO U-PASS(WS-I)
           PERFORM SAVE-ACCOUNTS
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
           MOVE 0 TO CURRENT-USER-ID
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
           MOVE 0 TO CURRENT-USER-ID
           IF USER-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
               IF FUNCTION TRIM(U-NAME(WS-I)) = FUNCTION TRIM(USERNAME)
                  AND FUNCTION TRIM(U-PASS(WS-I)) = FUNCTION TRIM(PASSWORD)
                   MOVE 1 TO LOGIN-SUCCESS
                   MOVE WS-I TO CURRENT-USER-ID
               END-IF
           END-PERFORM
           .

       POST-LOGIN-MENU.
           PERFORM UNTIL 1 = 2
               MOVE "1. Create/Edit My Profile" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "2. View My Profile" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "3. Search for a job" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "4. Find someone you know" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "5. Learn a new skill" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "6. Logout" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-1DIGIT

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM CREATE-EDIT-PROFILE
                   WHEN 2
                       PERFORM VIEW-MY-PROFILE
                   WHEN 3
                       MOVE "Job search/internship is under construction."
                           TO WS-OUT
                       PERFORM PRINT-LINE
                   WHEN 4
                       PERFORM FIND-SOMEONE
                   WHEN 5
                       PERFORM LEARN-SKILL-MENU
                   WHEN 6
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

       CREATE-EDIT-PROFILE.
           MOVE "--- Create/Edit Profile ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE "Enter First Name:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           PERFORM REQUIRE-NONBLANK
           MOVE WS-OUT(1:15) TO U-FNAME(CURRENT-USER-ID)

           MOVE "Enter Last Name:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           PERFORM REQUIRE-NONBLANK
           MOVE WS-OUT(1:15) TO U-LNAME(CURRENT-USER-ID)

           MOVE "Enter University/College Attended:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           PERFORM REQUIRE-NONBLANK
           MOVE WS-OUT(1:50) TO U-UNIV(CURRENT-USER-ID)

           MOVE "Enter Major:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           PERFORM REQUIRE-NONBLANK
           MOVE WS-OUT(1:50) TO U-MAJOR(CURRENT-USER-ID)

           PERFORM GET-VALID-GRAD-YEAR
           MOVE GRAD-YEAR-TEXT TO U-GRAD(CURRENT-USER-ID)

           MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):"
               TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           IF WS-OUT = SPACES
               MOVE SPACES TO U-ABOUT(CURRENT-USER-ID)
           ELSE
               MOVE WS-OUT(1:200) TO U-ABOUT(CURRENT-USER-ID)
           END-IF

           PERFORM CAPTURE-EXPERIENCE
           PERFORM CAPTURE-EDUCATION
           PERFORM SAVE-ACCOUNTS

           MOVE "Profile saved successfully!" TO WS-OUT
           PERFORM PRINT-LINE
           .

       REQUIRE-NONBLANK.
           PERFORM UNTIL WS-OUT NOT = SPACES
               MOVE "Invalid input. Please try again." TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
           END-PERFORM
           .

       GET-VALID-GRAD-YEAR.
           MOVE 0 TO YEAR-OK
           PERFORM UNTIL YEAR-OK = 1
               MOVE "Enter Graduation Year (YYYY):" TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE SPACES TO GRAD-YEAR-TEXT
               MOVE WS-OUT(1:4) TO GRAD-YEAR-TEXT
               PERFORM VALIDATE-GRAD-YEAR
           END-PERFORM
           .

       VALIDATE-GRAD-YEAR.
           MOVE 0 TO YEAR-OK
           IF GRAD-YEAR-TEXT IS NUMERIC
               MOVE GRAD-YEAR-TEXT TO GRAD-YEAR-NUM
               IF GRAD-YEAR-NUM >= 1950 AND GRAD-YEAR-NUM <= 2099
                   MOVE 1 TO YEAR-OK
               END-IF
           END-IF
           IF YEAR-OK = 0
               MOVE "Invalid graduation year. Please try again." TO WS-OUT
               PERFORM PRINT-LINE
           END-IF
           .

       SET-DIGIT-CHAR.
           EVALUATE WS-I
               WHEN 1 MOVE "1" TO DIGIT-CHAR
               WHEN 2 MOVE "2" TO DIGIT-CHAR
               WHEN 3 MOVE "3" TO DIGIT-CHAR
               WHEN OTHER MOVE "1" TO DIGIT-CHAR
           END-EVALUATE
           .

       CAPTURE-EXPERIENCE.
           MOVE 0 TO U-EXP-COUNT(CURRENT-USER-ID)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE SPACES TO U-EXP-TITLE(CURRENT-USER-ID, WS-I)
               MOVE SPACES TO U-EXP-COMP(CURRENT-USER-ID, WS-I)
               MOVE SPACES TO U-EXP-DATES(CURRENT-USER-ID, WS-I)
               MOVE SPACES TO U-EXP-DESC(CURRENT-USER-ID, WS-I)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):"
                   TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               IF WS-OUT = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO U-EXP-COUNT(CURRENT-USER-ID)
               PERFORM SET-DIGIT-CHAR

               MOVE SPACES TO WS-OUT
               STRING "Experience #" DIGIT-CHAR " - Title:"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE WS-OUT(1:50) TO U-EXP-TITLE(CURRENT-USER-ID, WS-I)

               MOVE SPACES TO WS-OUT
               STRING "Experience #" DIGIT-CHAR " - Company/Organization:"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE WS-OUT(1:50) TO U-EXP-COMP(CURRENT-USER-ID, WS-I)

               MOVE SPACES TO WS-OUT
               STRING "Experience #" DIGIT-CHAR " - Dates (e.g., Summer 2024):"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE WS-OUT(1:50) TO U-EXP-DATES(CURRENT-USER-ID, WS-I)

               MOVE SPACES TO WS-OUT
               STRING "Experience #" DIGIT-CHAR " - Description (optional, max 100 chars, blank to skip):"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               IF WS-OUT = SPACES
                   MOVE SPACES TO U-EXP-DESC(CURRENT-USER-ID, WS-I)
               ELSE
                   MOVE WS-OUT(1:100) TO U-EXP-DESC(CURRENT-USER-ID, WS-I)
               END-IF
           END-PERFORM
           .

       CAPTURE-EDUCATION.
           MOVE 0 TO U-EDU-COUNT(CURRENT-USER-ID)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE SPACES TO U-EDU-DEGREE(CURRENT-USER-ID, WS-I)
               MOVE SPACES TO U-EDU-UNIV(CURRENT-USER-ID, WS-I)
               MOVE SPACES TO U-EDU-YEARS(CURRENT-USER-ID, WS-I)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):"
                   TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               IF WS-OUT = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO U-EDU-COUNT(CURRENT-USER-ID)
               PERFORM SET-DIGIT-CHAR

               MOVE SPACES TO WS-OUT
               STRING "Education #" DIGIT-CHAR " - Degree:"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE WS-OUT(1:50) TO U-EDU-DEGREE(CURRENT-USER-ID, WS-I)

               MOVE SPACES TO WS-OUT
               STRING "Education #" DIGIT-CHAR " - University/College:"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE WS-OUT(1:50) TO U-EDU-UNIV(CURRENT-USER-ID, WS-I)

               MOVE SPACES TO WS-OUT
               STRING "Education #" DIGIT-CHAR " - Years Attended (e.g., 2023-2025):"
                   DELIMITED BY SIZE
                   INTO WS-OUT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE WS-OUT(1:20) TO U-EDU-YEARS(CURRENT-USER-ID, WS-I)
           END-PERFORM
           .

       VIEW-MY-PROFILE.
           IF CURRENT-USER-ID < 1 OR CURRENT-USER-ID > 5
               MOVE "Error: Invalid user session." TO WS-OUT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "--- Profile Information ---" TO WS-OUT
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "Name: "
                  FUNCTION TRIM(U-FNAME(CURRENT-USER-ID)) " "
                  FUNCTION TRIM(U-LNAME(CURRENT-USER-ID))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "University: " FUNCTION TRIM(U-UNIV(CURRENT-USER-ID))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "Major: " FUNCTION TRIM(U-MAJOR(CURRENT-USER-ID))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "Graduation Year: " FUNCTION TRIM(U-GRAD(CURRENT-USER-ID))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUT
           STRING "About Me: " FUNCTION TRIM(U-ABOUT(CURRENT-USER-ID))
               DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING
           PERFORM PRINT-LINE

           MOVE "Experience:" TO WS-OUT
           PERFORM PRINT-LINE

           IF U-EXP-COUNT(CURRENT-USER-ID) > 0
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > U-EXP-COUNT(CURRENT-USER-ID)
                   MOVE SPACES TO WS-OUT
                   STRING " Title: " FUNCTION TRIM(U-EXP-TITLE(CURRENT-USER-ID, WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO WS-OUT
                   STRING " Company: " FUNCTION TRIM(U-EXP-COMP(CURRENT-USER-ID, WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO WS-OUT
                   STRING " Dates: " FUNCTION TRIM(U-EXP-DATES(CURRENT-USER-ID, WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO WS-OUT
                   STRING " Description: " FUNCTION TRIM(U-EXP-DESC(CURRENT-USER-ID, WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
               *> added this to help make readibility easier.
               MOVE "----------------------------" TO WS-OUT
               PERFORM PRINT-LINE
               END-PERFORM
           END-IF

           MOVE "Education:" TO WS-OUT
           PERFORM PRINT-LINE

           IF U-EDU-COUNT(CURRENT-USER-ID) > 0
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > U-EDU-COUNT(CURRENT-USER-ID)
                   MOVE SPACES TO WS-OUT
                   STRING " Degree: " FUNCTION TRIM(U-EDU-DEGREE(CURRENT-USER-ID, WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO WS-OUT
                   STRING " University: " FUNCTION TRIM(U-EDU-UNIV(CURRENT-USER-ID, WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO WS-OUT
                   STRING " Years: " FUNCTION TRIM(U-EDU-YEARS(CURRENT-USER-ID, WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUT
                   END-STRING
                   PERFORM PRINT-LINE
                    *> added this to help make readibility easier.
                   MOVE "----------------------------" TO WS-OUT
                   PERFORM PRINT-LINE
               END-PERFORM
           END-IF
           .

       FIND-SOMEONE.
           MOVE "Enter first name to search:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT

           MOVE WS-OUT(1:15) TO USERNAME    *> reuse buffer as search fname

           MOVE "Enter last name to search:" TO WS-OUT
           PERFORM PRINT-LINE
           PERFORM READ-INPUT
           MOVE WS-OUT(1:15) TO PASSWORD    *> reuse buffer as search lname

           MOVE 0 TO FOUND

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > USER-COUNT
               IF FUNCTION TRIM(U-FNAME(WS-I)) = FUNCTION TRIM(USERNAME)
                  AND FUNCTION TRIM(U-LNAME(WS-I)) = FUNCTION TRIM(PASSWORD)

                   MOVE 1 TO FOUND

                   MOVE "--- Profile Found ---" TO WS-OUT
                   PERFORM PRINT-LINE

                   MOVE WS-I TO CURRENT-USER-ID
                   PERFORM VIEW-MY-PROFILE

                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF FOUND = 0
               MOVE "-----No match found. ----" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Please Try Again. Make sure you use correct capitials and spell the name correctly" TO WS-OUT
               PERFORM PRINT-LINE
           END-IF
           .
