       >>SOURCE FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.
       AUTHOR. Kaden
       DATE-WRITTEN. 09/08/2025

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *> Select INPUT-FILE tells COBOL what the input file is
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
      *> LINE SEQUENTIAL means each line in text is a record
               ORGANIZATION IS LINE SEQUENTIAL.
      *> OUTPUT-FILE defines what file will have the output stored
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *> FD describes the structure of the INPUT-FILE
       FD  INPUT-FILE.
      *> Defines each record as a 80 charecter line of text
       01  INPUT-RECORD      PIC X(80).

      *> FD describes the structure of the INPUT-FILE
       FD  OUTPUT-FILE.

      *> Defines each record as a 80 charecter line of text
       01  OUTPUT-RECORD     PIC X(80).

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
       01  WS-CHOICE         PIC X.

      *> Flag for end of file to then exit program
       01  WS-END-FILE       PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE

           PERFORM WELCOME-SCREEN

           CLOSE INPUT-FILE OUTPUT-FILE
           STOP RUN.

       WELCOME-SCREEN.
      *> Storing welcome messages into variables
           MOVE "Welcome to InCollege!" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "Log In" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "Create New Account" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "Enter your choice:" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           READ INPUT-FILE
               AT END MOVE "Y" TO WS-END-FILE
               NOT AT END MOVE INPUT-RECORD TO WS-CHOICE
           END-READ

           IF WS-CHOICE = "L"
               PERFORM LOGIN-PROCESS
           ELSE
               MOVE "Feature not yet implemented." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
           END-IF.

       LOGIN-PROCESS.
           MOVE "Please enter your username:" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           READ INPUT-FILE
               AT END MOVE "Y" TO WS-END-FILE
               NOT AT END MOVE INPUT-RECORD TO WS-USERNAME
           END-READ

           MOVE "Please enter your password:" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           READ INPUT-FILE
               AT END MOVE "Y" TO WS-END-FILE
               NOT AT END MOVE INPUT-RECORD TO WS-PASSWORD
           END-READ

           MOVE "You have successfully logged in." TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           STRING "Welcome, " WS-USERNAME "!"
               DELIMITED BY SIZE
               INTO WS-MESSAGE
           END-STRING
           PERFORM DISPLAY-AND-LOG

           PERFORM MAIN-MENU.

       MAIN-MENU.
           PERFORM UNTIL WS-END-FILE = "Y"
               MOVE "Search for a job" TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE "Find someone you know" TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE "Enter your choice: " TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG

               READ INPUT-FILE
                   AT END MOVE "Y" TO WS-END-FILE
                   NOT AT END MOVE INPUT-RECORD TO WS-CHOICE
               END-READ

               EVALUATE WS-CHOICE
                   WHEN "Search for a job"
                       MOVE "Job search/internship is under construction." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                   WHEN "Find someone you know"
                       MOVE "Find someone you know is under construction." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                   WHEN "Learn a new skill"
                       MOVE "Learn a new skill is under construction." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                   WHEN OTHER
                       MOVE "Invalid input, please try again." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                END-EVALUATE
           END-PERFORM.

       DISPLAY-AND-LOG.
           DISPLAY WS-MESSAGE
           MOVE WS-MESSAGE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
