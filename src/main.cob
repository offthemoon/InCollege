       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDKBRUH.
       AUTHOR. NEVADA.
       DATE-WRITTEN.  01/23/2026.
       DATE-COMPILED. 01/23/2026.
       SECURITY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.



       WORKING-STORAGE SECTION.
           01 MAX-USERS PIC 9 VALUE 5.
           01 WELCOME-CHOICE PIC 9.

           01 USER-DATA.
               10 U-NAME PIC X(15).
               10 U-PASS PIC X(15).

           01 MENU-CHOICE PIC 9.
           01 IN-USER PIC X(15).
           01 IN-PASS PIC X(15).


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
