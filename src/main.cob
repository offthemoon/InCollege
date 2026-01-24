       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDKBRUH.
       AUTHOR. NEVADA.
       DATE-WRITTEN.  01/23/2026.
       DATE-COMPILED. 01/23/2026.
       SECURITY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts_info.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNTS-FILE.
       01 ACCOUNTS-RECORD.
           05 FILE-USER PIC X(15).
           05 FILE-PASS PIC X(15).

       WORKING-STORAGE SECTION.
           01 MAX-USERS PIC 9 VALUE 5.
           01 WELCOME-CHOICE PIC 9.
           01 USER-CHOICE PIC 9.
           01 IN-USER PIC X(15).
           01 IN-PASS PIC X(15).
           01  LOGIN-SUCCESS PIC 9 VALUE 0.
           01  FILE-EOF PIC 9 VALUE 0.


       PROCEDURE DIVISION.
       PERFORM MAIN-MENU.
       STOP RUN.

       MAIN-MENU.
           MOVE 0 TO WELCOME-CHOICE.
           DISPLAY "============================".
           DISPLAY "Welcome to InCollege!"
           DISPLAY "============================".
           DISPLAY "Select an option below to get started:".
           DISPLAY "1. Sign Up".
           DISPLAY "2. Log In".
           DISPLAY "3. Exit".
           ACCEPT WELCOME-CHOICE.
           EVALUATE WELCOME-CHOICE
               WHEN 2
                   PERFORM LOGIN
               WHEN 3
                   STOP RUN
           END-EVALUATE.

       LOGIN.
           DISPLAY     "============================".
           DISPLAY "LOGIN PAGE".
           DISPLAY "Enter Username:".
           ACCEPT IN-USER.
           DISPLAY "Enter Password:".
           ACCEPT IN-PASS.

           OPEN INPUT ACCOUNTS-FILE.
           MOVE 0 TO LOGIN-SUCCESS
           MOVE 0 TO FILE-EOF.

           PERFORM UNTIL FILE-EOF = 1
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 1 TO FILE-EOF
                   NOT AT END
                       PERFORM CHECK-CREDENTIALS
               END-READ
           END-PERFORM.

           CLOSE ACCOUNTS-FILE.

           IF LOGIN-SUCCESS = 1
               DISPLAY "Login Successful!"
               PERFORM USER-OPTIONS
           ELSE
               DISPLAY "Incorrect username/password, please try again."
               PERFORM LOGIN
           END-IF.

       CHECK-CREDENTIALS.
           IF FILE-USER = IN-USER
               IF  FILE-PASS = IN-PASS
                   MOVE 1 TO LOGIN-SUCCESS
                   MOVE 1 TO FILE-EOF
               END-IF
           END-IF.

       USER-OPTIONS.
           MOVE 0 TO USER-CHOICE.
           DISPLAY "============================".
           DISPLAY "1. Job Search".
           DISPLAY "2. Find Someone".
           DISPLAY "3. Learn Skill".
           DISPLAY "4. Exit".

           ACCEPT USER-CHOICE.
           EVALUATE USER-CHOICE
               WHEN 1
                   PERFORM JOB-SEARCH
               WHEN 2
                   PERFORM FIND-SOMEONE
               WHEN 3
                   PERFORM LEARN-SKILL
               WHEN 4
                   STOP RUN
           END-EVALUATE.

       JOB-SEARCH.
           PERFORM CONSTRUCTION.
       FIND-SOMEONE.
           PERFORM CONSTRUCTION.

       LEARN-SKILL.
           MOVE 0 TO USER-CHOICE.
           DISPLAY "============================".
           DISPLAY "Choose a new skill to learn:".
           DISPLAY "1. Programming in COBOL".
           DISPLAY "2. Programming in Python".
           DISPLAY "3. Programming in C++".
           DISPLAY "4. CAD Modeling".
           DISPLAY "5. PCB Design".
           DISPLAY "6. Back to Main Menu".

           ACCEPT USER-CHOICE.
           EVALUATE USER-CHOICE
               WHEN 1
                   PERFORM LEARN-COBOL
               WHEN 2
                   PERFORM LEARN-PYTHON
               WHEN 3
                   PERFORM LEARN-CPP
               WHEN 4
                   PERFORM LEARN-CAD
               WHEN 5
                   PERFORM LEARN-PCB
               WHEN 6
                   PERFORM USER-OPTIONS
           END-EVALUATE.

       LEARN-COBOL.
           PERFORM CONSTRUCTION.
       LEARN-PYTHON.
           PERFORM CONSTRUCTION.
       LEARN-CPP.
           PERFORM CONSTRUCTION.
       LEARN-CAD.
           PERFORM CONSTRUCTION.
       LEARN-PCB.
           PERFORM CONSTRUCTION.

       CONSTRUCTION.
           MOVE 0 TO USER-CHOICE.
           DISPLAY "============================".
           DISPLAY "This feature is under construction. Please check back later!".
           DISPLAY "1. Back to Main Menu".
           ACCEPT USER-CHOICE.
           PERFORM USER-OPTIONS.