>>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.
AUTHOR. Kaden and Paola
DATE-WRITTEN. 09/07/2025

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.

                 *> Select INPUT-FILE tells COBOL what the input file is
               SELECT INPUT-FILE ASSIGN TO "InCollege-Test.txt"
                 *> LINE SEQUENTIAL means each line in text is a record
                   ORGANIZATION IS LINE SEQUENTIAL.
                 *> OUTPUT-FILE defines what file will have the output stored
               SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT ACCOUNTS-FILE ASSIGN TO "accounts.txt"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS WS-ACCOUNTS-STATUS.

DATA DIVISION.
       FILE SECTION.
           *> FD describes the structure of the INPUT-FILE
           FD  INPUT-FILE.
           *> Defines each record as a 80 charecter line of text
           01  INPUT-RECORD      PIC X(80).

           *> FD describes the structure of the OUTPUT-FILE
           FD  OUTPUT-FILE.

           *> Defines each record as a 80 charecter line of text
           01  OUTPUT-RECORD     PIC X(80).


           FD  ACCOUNTS-FILE.
           01  ACCOUNTS-RECORD-DATA.
               05  ACCOUNTS-USERNAME    PIC X(20).
               05  ACCOUNTS-PASSWORD    PIC X(20).

             *> Working storeage section is where the variables of the program are stored
       WORKING-STORAGE SECTION.

           *> Used to hold a line of text before displaying it
           01  WS-MESSAGE        PIC X(80).

           *> Space for input line
           01  WS-INPUT          PIC X(80).

           *> Stores the entered username
           01  WS-USERNAME       PIC X(20).

           *> Stores the entered password
           01  WS-PASSWORD       PIC X(20).

           *> Single charecter
           01  WS-CHOICE         PIC 9(1).

           *> Bounds of the choices options
           01  MIN-VALUE-CHOICE       PIC 9(1).
           01  MAX-VALUE-CHOICE       PIC 9(1).

           *> Flag for end of file to then exit program
           01  WS-END-FILE       PIC X VALUE "N".

           01  WS-RETURN-CODE     PIC X.

           01  WS-ACCOUNTS-STATUS     PIC X(2).

PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE.
       OPEN OUTPUT OUTPUT-FILE.

       PERFORM WELCOME-SCREEN
       PERFORM MAIN-MENU

       CLOSE INPUT-FILE, OUTPUT-FILE.
       STOP RUN.

       WELCOME-SCREEN SECTION.
       *> Storing welcome messages into variables
           MOVE "Welcome to InCollege!" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG.
           MOVE "1. Log In" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG.
           MOVE "2. Create New Account" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG.

           MOVE 1 TO MIN-VALUE-CHOICE.
           MOVE 2 TO MAX-VALUE-CHOICE.

           PERFORM CHOICE.

           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM LOGIN-FLOW
               WHEN 2
                   PERFORM CREATE-ACCOUNT-FLOW
           END-EVALUATE.

           INITIALIZE WS-MESSAGE
           STRING 'Welcome, ' DELIMITED BY SIZE
                   WS-USERNAME DELIMITED BY SPACE
                   '!' DELIMITED BY SIZE
                   INTO WS-MESSAGE

           PERFORM DISPLAY-AND-LOG.


       MAIN-MENU SECTION.

           MOVE "1. Search for a job" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "2. Find someone you know" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "3. Learn a new skill" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           MOVE 1 TO MIN-VALUE-CHOICE
           MOVE 3 TO MAX-VALUE-CHOICE

           PERFORM CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM SEARCH-JOB
               WHEN 2
                   PERFORM FIND-SOMEONE
               WHEN 3
                   PERFORM LEARN-SKILL.


       CREATE-ACCOUNT-FLOW SECTION.

           MOVE "Enter username:" TO WS-MESSAGE.
           PERFORM DISPLAY-AND-LOG.

           PERFORM READ-FROM-INPUT-FILE.

           IF WS-END-FILE = 'N'
               MOVE INPUT-RECORD TO WS-USERNAME
           END-IF.

           MOVE "Enter password:" TO WS-MESSAGE.
           PERFORM DISPLAY-AND-LOG.

           PERFORM READ-FROM-INPUT-FILE.

           IF WS-END-FILE = 'N'
               MOVE INPUT-RECORD TO WS-PASSWORD
           END-IF.

           CALL "CREATE-ACCOUNT" USING WS-USERNAME, WS-PASSWORD, WS-RETURN-CODE.

           EVALUATE WS-RETURN-CODE
               WHEN 'S'
                   MOVE "Account created successfully." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
               WHEN 'L'
                   MOVE "All permitted accounts have been created." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   CLOSE INPUT-FILE, OUTPUT-FILE
                   STOP RUN
               WHEN 'E'
                   MOVE "Username already exists." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   CLOSE INPUT-FILE, OUTPUT-FILE
                   STOP RUN
               WHEN 'F'
                   MOVE "Invalid password format." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   CLOSE INPUT-FILE, OUTPUT-FILE
                   STOP RUN
               WHEN OTHER
                   MOVE "An unknown error occurred." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   CLOSE INPUT-FILE, OUTPUT-FILE
                   STOP RUN
           END-EVALUATE.



           EXIT.

       LOGIN-FLOW SECTION.

           PERFORM UNTIL WS-RETURN-CODE = 'S'
               MOVE "Enter username:" TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               PERFORM READ-FROM-INPUT-FILE

               IF WS-END-FILE = 'N'
                   MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-USERNAME
               END-IF

               MOVE "Enter password:" TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               PERFORM READ-FROM-INPUT-FILE

               IF WS-END-FILE = 'N'
                   MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PASSWORD
               END-IF

               CALL "LOGIN" USING WS-USERNAME, WS-PASSWORD, WS-RETURN-CODE

               EVALUATE WS-RETURN-CODE
                   WHEN 'S'
                       MOVE "Login successful" TO WS-MESSAGE
                   WHEN 'F'
                       MOVE "Incorrect username/password. Please try again." TO WS-MESSAGE
                   WHEN 'X'
                       MOVE "Error accessing accounts file." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                       CLOSE INPUT-FILE, OUTPUT-FILE
                       STOP RUN
                   WHEN OTHER
                       MOVE "An unknown error occurred." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                       CLOSE INPUT-FILE, OUTPUT-FILE
                       STOP RUN
               END-EVALUATE

               PERFORM DISPLAY-AND-LOG
           END-PERFORM

           EXIT.


       SEARCH-JOB SECTION.
           MOVE "Job search/internship is under construction." TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG.

           PERFORM MAIN-MENU.


       FIND-SOMEONE SECTION.
           MOVE "Find someone you know is under construction." TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG.

           PERFORM MAIN-MENU.

       LEARN-SKILL SECTION.
           MOVE "Learn a New Skill:" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG.

           MOVE "1. Skill 1" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "2. Skill 2" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "3. Skill 3" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "4. Skill 4" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "5. Skill 5" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "6. Go Back" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           MOVE 1 TO MIN-VALUE-CHOICE.
           MOVE 6 TO MAX-VALUE-CHOICE.

           PERFORM CHOICE.

           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE "This skill is under construction." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   PERFORM LEARN-SKILL
               WHEN 2
                   MOVE "This skill is under construction." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   PERFORM LEARN-SKILL
               WHEN 3
                   MOVE "This skill is under construction." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   PERFORM LEARN-SKILL
               WHEN 4
                   MOVE "This skill is under construction." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   PERFORM LEARN-SKILL
               WHEN 5
                   MOVE "This skill is under construction." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   PERFORM LEARN-SKILL
               WHEN 6
                   PERFORM MAIN-MENU.


       CHOICE SECTION.

           INITIALIZE WS-CHOICE

           MOVE "Enter your choice as a number:" TO WS-MESSAGE.

           PERFORM DISPLAY-AND-LOG.

           PERFORM READ-FROM-INPUT-FILE

           IF WS-END-FILE = 'N'
               MOVE INPUT-RECORD TO WS-CHOICE
           END-IF

           IF WS-END-FILE = 'N'
               PERFORM UNTIL (WS-CHOICE >= MIN-VALUE-CHOICE)
                              AND (WS-CHOICE <= MAX-VALUE-CHOICE)

                   DISPLAY "Not a valid choice. Try again."

                   PERFORM READ-FROM-INPUT-FILE

                   IF WS-END-FILE = 'N'
                       MOVE INPUT-RECORD TO WS-CHOICE
                   END-IF

               END-PERFORM
           END-IF

           IF WS-END-FILE = 'Y'
               MOVE "You quit successfully." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG

               CLOSE INPUT-FILE, OUTPUT-FILE
               STOP RUN
           END-IF.

       DISPLAY-AND-LOG SECTION.
           DISPLAY WS-MESSAGE
           MOVE WS-MESSAGE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.

       READ-FROM-INPUT-FILE SECTION.
           IF WS-END-FILE = 'Y'
               CLOSE INPUT-FILE, OUTPUT-FILE
               STOP RUN
           END-IF

           READ INPUT-FILE
               AT END
                   MOVE 'Y' TO WS-END-FILE
               NOT AT END
                   MOVE 'N' TO WS-END-FILE
           END-READ

           EXIT.


