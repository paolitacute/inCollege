>>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.
AUTHOR. Kaden and Paola
DATE-WRITTEN. 09/07/2025

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

               SELECT ACCOUNTS-FILE ASSIGN TO "accounts.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS ACCOUNTS-USERNAME.

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
               05  FILLER               PIC X(10).

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

PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE.
       OPEN OUTPUT OUTPUT-FILE.
       OPEN I-O ACCOUNTS-FILE.

       PERFORM WELCOME-SCREEN

       CLOSE INPUT-FILE, OUTPUT-FILE, ACCOUNTS-FILE.
       STOP RUN.

       WELCOME-SCREEN SECTION.
       *> Storing welcome messages into variables
           MOVE "Welcome to InCollege!" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "Log In" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE "Create New Account" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           MOVE 1 TO MIN-VALUE-CHOICE.
           MOVE 2 TO MAX-VALUE-CHOICE.

           PERFORM CHOICE.

           EVALUATE WS-CHOICE
         *>  WHEN 1
           *>    CALL "LOGIN" USING
               WHEN 2
                   CALL "CREATE-ACCOUNT" USING WS-USERNAME, WS-PASSWORD, WS-RETURN-CODE.



       DISPLAY-AND-LOG SECTION.
           DISPLAY WS-MESSAGE
           MOVE WS-MESSAGE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.



       CHOICE SECTION.
           MOVE "Enter your choice as a number:" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG


           READ INPUT-FILE
               AT END
                   MOVE 'Y' TO WS-END-FILE
               NOT AT END
                   MOVE INPUT-RECORD TO WS-CHOICE
           END-READ


           PERFORM UNTIL (WS-CHOICE >= MIN-VALUE-CHOICE)
                          AND (WS-CHOICE <= MAX-VALUE-CHOICE)
               DISPLAY "Not a valid choice. Try again."

               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-FILE
                   NOT AT END
                       MOVE INPUT-RECORD TO WS-CHOICE
               END-READ
           END-PERFORM.





