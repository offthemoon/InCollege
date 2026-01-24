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
           01 IN-USER PIC X(15).
           01 IN-PASS PIC X(15).
           01  LOGIN-SUCCESS PIC 9 VALUE 0.
           01  FILE-EOF PIC 9 VALUE 0.


       PROCEDURE DIVISION.
       PERFORM MAIN-MENU.
       STOP RUN.

       MAIN-MENU.
           DISPLAY "============================".
           DISPLAY "Welcome to InCollege!"
           DISPLAY "============================".
           DISPLAY "Select an option below to get started:".
           DISPLAY "1. Sign Up".
           DISPLAY "2. Log In".
           DISPLAY "3. Exit".
           ACCEPT WELCOME-CHOICE.
           EVALUATE WELCOME-CHOICE
               WHEN 1
                   PERFORM LOGIN
           END-EVALUATE.

       LOGIN.
           DISPLAY "============================".
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
           ELSE
               DISPLAY "Login Failed! Invalid username or password"
           END-IF.

       CHECK-CREDENTIALS.
           IF FILE-USER = IN-USER
               IF  FILE-PASS = IN-PASS
                   MOVE 1 TO LOGIN-SUCCESS
                   MOVE 1 TO FILE-EOF
               END-IF
           END-IF.
