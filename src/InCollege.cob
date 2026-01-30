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
       01 INPUT-RECORD                 PIC X(200).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD                PIC X(200).

       FD ACCOUNTS-FILE.
       01 ACCT-RECORD                  PIC X(200).

       WORKING-STORAGE SECTION.

       01 ACCOUNTS-STATUS              PIC XX VALUE "00".
       01 INFILE-EOF                   PIC 9 VALUE 0.

       01 INPUT-FILENAME               PIC X(120) VALUE "InCollege-Input.txt".
       01 OUTPUT-FILENAME              PIC X(120) VALUE "InCollege-Output.txt".
       01 CMDLINE                      PIC X(120) VALUE SPACES.
       01 TEST-ID                      PIC X(40)  VALUE SPACES.

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



       *> MORE INFROMATIONA ADDED FOR EPIC 2. ------------
       01 WS-VALID-STRING             PIC X(30).
       01 WS-VALID-STRING-LENGTH      PIC 99  VALUE 0.

       01 WS-VALID-STRING-OKAY        PIC 9  VALUE 0.

       01 ITTERATION                  PIC 99   VALUE 0.

       *> ALL OF THESE ARE REQUIRED -------------------------
       01 FIRST_NAME                  PIC X(15).
       01 LAST_NAME                   PIC X(15).
       01 UNIVERSITY_NAME             PIC X(25).
       01 MAJOR                       PIC X(20).

       *> ONLY GOING TO WANT TO STORE 4 DIGITS. FOR THIS.
       01 GRADUATION-YEAR             PIC X(4).

       01 GRADUATION-YEAR-NUM         PIC 9(4).
       *> DONE FOR REQUIRED ---------------------


       *> BOOLEAN TYPE VARIABLES USED TO MAKE SURE THE USERS INFORMATION IS VALID

       01 FIRST-NAME-CHECK             PIC 9  VALUE 1.

       01 LAST-NAME-CHECK              PIC 9  VALUE 1.

       01 UNIVERSITY-NAME-CHECK        PIC 9  VALUE 1.

       01 MAJOR-CHECK                  PIC 9  VALUE 1.

       01 GRADUATION-YEAR-CHECK        PIC 9 VALUE 1.



       *> OPTIONAL INFORMATION --------------------------

       01 ABOUT-ME                 PIC X(200).

       01 EXPERIENCE-RESPONSE      PIC X(20).



       *> we are now going to use the data type occurs since we can have experience occur 3 times

       01 EXPERIENCE
           05 EXPERIENCE-ENTRY OCCURS 3 TIMES.
               10 EXPERIENCE-TITLE PIC X(50).
               10 EXPERIENCE-COMPANY PIC X(50).
               10 EXPERIENCE-DATES   PIC X(50).
               10 EXPERIENCE-DESCRIPTION PIC X(150).


       *> OPTIONAL INFORMATION FLAGS.

       *> 1 = WE ARE GOING TO STORE , 0 = WE ARE GOING TO SKIP.

       01 ABOUT-ME-STORE             PIC 9 VALUE 1.

       01 EXPERIENCE-COUNTER         PIC 9 VALUE 0.






*> end of information i added for epic-2
       01 USERS.
           05 USER-ENTRY OCCURS 5 TIMES.
               10 U-NAME               PIC X(15).
               10 U-PASS               PIC X(12).

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
                  AND FUNCTION TRIM(U-PASS(WS-I)) = FUNCTION TRIM(PASSWORD)
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
               *> ADDED THIS NEW LINE FOR EPIC 2 TASK 1.
               MOVE "5. Create/Edit My Profile" TO WS-OUT
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUT
               PERFORM PRINT-LINE

               PERFORM GET-CHOICE-1DIGIT


               EVALUATE CHOICE
                   WHEN 1
                       MOVE "Job search/internship is under construction."
                           TO WS-OUT
                       PERFORM PRINT-LINE
                   WHEN 2
                       MOVE "Find someone you know is under construction."
                           TO WS-OUT
                       PERFORM PRINT-LINE
                   WHEN 3
                       PERFORM LEARN-SKILL-MENU
                   WHEN 4
                       EXIT PARAGRAPH
                      *> ADDED THIS RIGHT HERE AS WELL.
                   WHEN 5
                       PERFORM EDIT-PROFILE
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

           *> ---------------- NEW CODE ADDED FOR EPIC 2 IS ALL BELOW.

          EDIT-PROFILE.
             MOVE "We are Going to Create/Edit Our Profile " TO WS-OUT
             PERFORM PRINT-LINE
             MOVE "* Means it is Required Information " TO  WS-OUT
             PERFORM PRINT-LINE

             *> RESET JUST IN CASE
             MOVE 0 TO WS-VALID-STRING-OKAY

              *> Will perform until we get the user to enter a proper name.
             PERFORM UNTIL WS-VALID-STRING-OKAY = 1

                   IF FIRST-NAME-CHECK = 0
                       MOVE "Please Enter a Correct First Name (No Digits / Blank )" TO WS-OUT
                       PERFORM PRINT-LINE
                   END-IF

                   MOVE "*Please Enter Your First Name: " TO WS-OUT
                   PERFORM PRINT-LINE
                   PERFORM READ-INPUT
                   MOVE SPACES TO FIRST_NAME
                   MOVE WS-OUT(1:15) TO FIRST_NAME

                   *> going to call function to validate this.

                   MOVE FIRST_NAME TO WS-VALID-STRING
                   MOVE 15 TO WS-VALID-STRING-LENGTH

                   PERFORM GOOD-STRING

                   IF WS-VALID-STRING-OKAY = 0
                       MOVE 0 TO FIRST-NAME-CHECK
                   ELSE
                       MOVE 1 TO FIRST-NAME-CHECK
                   END-IF

              END-PERFORM.

              *>Crucial, needed to reset it.
              MOVE 0 TO WS-VALID-STRING-OKAY
               *> we should add something that verifys that a user name is nothing but characters and no digits for first,last, unviersity name and major.

               *> CHECKING THE LAST-NAME NOW.
              PERFORM UNTIL WS-VALID-STRING-OKAY = 1

                   IF LAST-NAME-CHECK = 0
                       MOVE "Please Retype a valid last name (No Digits / Can't Be Empty " TO WS-OUT
                       PERFORM PRINT-LINE
                   END-IF

                   MOVE "* Please Enter Your Last Name" TO WS-OUT
                   PERFORM PRINT-LINE
                   PERFORM READ-INPUT
                   MOVE SPACES TO LAST_NAME
                   MOVE WS-OUT(1:15) TO LAST_NAME

                   MOVE LAST_NAME TO WS-VALID-STRING
                   MOVE 15 TO WS-VALID-STRING-LENGTH

                   PERFORM GOOD-STRING

                   IF WS-VALID-STRING-OKAY = 0
                       MOVE 0 TO LAST-NAME-CHECK
                   ELSE
                       MOVE 1 TO LAST-NAME-CHECK
                   END-IF

               END-PERFORM.

               *> must reset this back to 0.
               MOVE 0 TO WS-VALID-STRING-OKAY


               *> CHECKING THE UNIVERSITY NOW
               PERFORM UNTIL WS-VALID-STRING-OKAY = 1

                   IF UNIVERSITY-NAME-CHECK = 0
                      MOVE "Please Enter A Correct University (No Numbers/ Not Blank) " WS-OUT
                      PERFORM PRINT-LINE
                   END-IF

                   MOVE "* Please Enter The University You Attended / Currently Atendding) " TO WS-OUT
                   PERFORM PRINT-LINE
                   PERFORM READ-INPUT
                   MOVE SPACES TO UNIVERSITY_NAME
                   MOVE WS-OUT(1:25) TO UNIVERSITY_NAME


                   MOVE UNIVERSITY_NAME TO WS-VALID-STRING
                   MOVE 25 TO WS-VALID-STRING-LENGTH

                   PERFORM GOOD-STRING

                   IF WS-VALID-STRING-OKAY = 0
                      MOVE 0 TO UNIVERSITY-NAME-CHECK
                   ELSE
                       MOVE 1 TO UNIVERSITY-NAME-CHECK
                   END-IF

               END-PERFORM.

               *>RESET AGAIN
               MOVE 0 TO WS-VALID-STRING-OKAY


               *>CHECKING THE MAJOR NOW.
               PERFORM UNTIL WS-VALID-STRING-OKAY = 1

                   IF MAJOR-CHECK = 0
                       MOVE "Please Enter a Correct Major (No Digits and Not Blank)" TO WS-OUT
                       PERFORM PRINT-LINE
                   END-IF


                   MOVE "* Please Enter Your Current Major " TO WS-OUT
                   PERFORM PRINT-LINE
                   PERFORM READ-INPUT
                   MOVE SPACES TO MAJOR
                   MOVE WS-OUT(1:20) TO MAJOR


                   MOVE MAJOR TO WS-VALID-STRING
                   MOVE 20 TO WS-VALID-STRING-LENGTH

                   PERFORM GOOD-STRING

                   IF WS-VALID-STRING-OKAY = 0
                       MOVE 0 TO MAJOR-CHECK
                   ELSE
                       MOVE 1 TO MAJOR-CHECK
                   END-IF

               END-PERFORM.


               *> need to validate if the user entered 4 digits. size needs to be 4 and needs to be digits.

               *>we are going to still use the same flag we used for the strings. (Reset it here)
               MOVE 0 TO WS-VALID-STRING-OKAY


               *> CHECKING THE GRADUATION DATE NOW
               PERFORM UNTIL WS-VALID-STRING-OKAY = 0

                   IF GRADUATION-YEAR-CHECK = 0
                       MOVE "Please Enter a Valid Graduation Date (Between 1950 - 2099) and Not left Blank / Empty " TO WS-OUT
                       PERFORM PRINT-LINE
                   END-IF

                   MOVE "Please Enter The Estimated Graduation Year " TO WS-OUT
                   PERFORM PRINT-LINE
                   PERFORM READ-INPUT
                   MOVE SPACES TO GRADUATION-YEAR
                   MOVE WS-OUT(1:4) TO GRADUATION-YEAR


                   PERFORM VALID-GRADUATION-YEAR

                   IF WS-VALID-STRING-OKAY = 0
                       GRADUATION-YEAR-CHECK = 0
                   ELSE
                       GRADUATION-YEAR-CHECK = 1
                   END-IF

               END-PERFORM.


               *> -------------- all of this now is going to be the optional stuff (About me, Experience and ....)



               *> EVERYTHINIG BELOW THIS I DID NOT FINISH / COMPLETE / REVIEW . THIS IS GOING TO BE THE OPTINAL STUFFF
               *> CODE MAY NOT WORK / COMPILE.

               *> ONLY THING THAT WORKS IS THE PARAGRAHS 'GOOD STRING AND GOOD YEAR' THE VALIDATION CHECKERS.!!!!!!!


                *>IN ORDER TO ALLOW THE USER TO SKIP AND NOT ENTER INFORMATION WE ARE GOING TO USE
                *> KEY LETTER "SKIP" TO SHOW THE USER CHOOSE TO SKIP AND WE WON'T HAVE TO SAVE ANY OF THE INFROMATION.
               MOVE "Please Enter anything you would like to share for your 'About Me' section or type 'SKIP' to skip/leave blank " TO WS-OUT
               PERFORM PRINT-LINE
               PERFORM READ-INPUT
               MOVE SPACES TO ABOUT-ME
               MOVE WS-OUT(1:200) TO ABOUT-ME

               *> WE ARE GOING TO WANT TO CHECK IF THE USER SKIPPED BY ENTERING "SKIP" OR IF THEY ACTUALLY INPUTED VALID INFORMATION WE WANT TO STORE.


               *> DO NOTHING.
               IF ABOUT-ME = "SKIP"
                   *> 0 MEANS WE ARE NOT GOING TO STORE AND 1 MEANS WE ARE GOING TO STORE
                   MOVE 0 TO ABOUT-ME-STORE
               ELSE
                   MOVE 1 TO ABOUT-ME-STORE
               END-IF
                   *>WE ARE GOING TO WANT TO FLAG IT TO KNOW TO STORE ABOUT-ME



               *> this here is going to be the code for the Experience (We can only have 3)

               MOVE 0 TO EXPERIENCE-COUNTER

               PERFORM UNTIL EXPERIENCE-COUNTER >= 3
                   MOVE "Please enter anything to enter experience or type SKIP to skip this step " TO WS-OUT
                   PERFORM PRINT-LINE
                   PERFORM READ-INPUT
                   MOVE SPACES TO EXPERIENCE-RESPONSE
                   MOVE WS-OUT(1:20) TO EXPERIENCE-RESPONSE

                   IF EXPERIENCE-RESPONSE = "SKIP"
                   *> GOING TO SKIP. HOW DO WE EXIT A PERFORM?






               END-PERFORM.

         *> WILL BE USED TO VALIDATE STRINGS AND MAKE SURE THAT THERE IS
       GOOD-STRING.
           *> We are going to want to validate the string.
           MOVE 0 TO ITTERATION

           *> We are going to assume that the string is valid to start off with
           MOVE 1 TO WS-VALID-STRING-OKAY

           PERFORM UNTIL ITTERATION >= WS-VALID-STRING-LENGTH OR WS-VALID-STRING-OKAY = 0

               IF WS-VALID-STRING = SPACES
                   MOVE 0 TO WS-VALID-STRING-OKAY
                   EXIT PARAGRAPH
               END-IF


               ADD 1 TO ITTERATION

               IF WS-VALID-STRING(ITTERATION:1) IS NUMERIC
                   MOVE 0 TO WS-VALID-STRING-OKAY
               END-IF

           END-PERFORM.


          *> ONE THING THIS DOESN'T CHECK FOR IS WHAT IF THE USER ENTERS 2024123123. IT WOULD GET 2024 AND REST DISGARDERD . DO WE WANT TO PREVENT THAT OR DO WE WANT TO LET IT LIVE?

          VALID-GRADUATION-YEAR.
               MOVE 1 TO WS-VALID-STRING-OKAY

               IF GRADUATION-YEAR IS NUMERIC
                   MOVE GRADUATION-YEAR TO GRADUATION-YEAR-NUM
                       IF GRADUATION-YEAR-NUM >= 1950 AND GRADUATION-YEAR-NUM <= 2099
                           CONTINUE
                    ELSE
                        MOVE 0 TO WS-VALID-STRING-OKAY
                     END-IF
               ELSE
                   MOVE 0 TO WS-VALID-STRING-OKAY
               END-IF.



