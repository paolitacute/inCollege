IDENTIFICATION DIVISION.
       PROGRAM-ID. inCollege.
       AUTHOR. Paola.
       DATE-WRITTEN. 9/7/2025.
ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD-DATA.
           05  IN-USER-CHOICE       PIC X.
           05  IN-USERNAME          PIC X(20).
           05  IN-PASSWORD          PIC X(20).
           05  FILLER               PIC X(9).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD-DATA       PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-USER-CHOICE         PIC X.
       01  WS-USERNAME            PIC X(20).
       01  WS-PASSWORD            PIC X(20).

       01  MIN-VALUE-CHOICE       PIC 9(1).
       01  MAX-VALUE-CHOICE       PIC 9(1).
       01  WS-END-OF-FILE         PIC X         VALUE 'N'.

PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE
           OUTPUT OUTPUT-FILE.

       PERFORM DISPLAY-MAIN-MENU.


       CLOSE INPUT-FILE, OUTPUT-FILE.
       STOP RUN.

DISPLAY-MAIN-MENU SECTION.
       DISPLAY "Welcome to InCollege!".
       MOVE "Welcome to InCollege!" TO OUTPUT-RECORD-DATA.
       WRITE OUTPUT-RECORD-DATA.

       DISPLAY "Log In".
       MOVE "Log In" TO OUTPUT-RECORD-DATA.
       WRITE OUTPUT-RECORD-DATA.

       DISPLAY "Create New Account".
       MOVE "Create New Account" TO OUTPUT-RECORD-DATA.
       WRITE OUTPUT-RECORD-DATA.

       MOVE 1 TO MIN-VALUE-CHOICE.
       MOVE 2 TO MAX-VALUE-CHOICE.

       PERFORM CHOICE.

       *>EVALUATE WS-USER-CHOICE
         *>  WHEN 1
           *>    CALL "LOGIN"
           *>WHEN 2
           *>    CALL "CREATE-ACCOUNT"


CHOICE SECTION.
       DISPLAY "Enter your choice as a number:".
       MOVE "Enter your choice as a number:" TO OUTPUT-RECORD-DATA.
       WRITE OUTPUT-RECORD-DATA.

       PERFORM UNTIL WS-END-OF-FILE = 'Y'
           READ INPUT-FILE
               AT END
                   MOVE 'Y' TO WS-END-OF-FILE
               NOT AT END
                   MOVE IN-USER-CHOICE TO WS-USER-CHOICE
           END-READ

           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE
                   NOT AT END
                       MOVE IN-USER-CHOICE TO WS-USER-CHOICE
               END-READ
           END-PERFORM

           PERFORM UNTIL (WS-USER-CHOICE >= MIN-VALUE-CHOICE)
                          AND (WS-USER-CHOICE <= MAX-VALUE-CHOICE)
               DISPLAY "Not a valid choice. Try again."

               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE
                   NOT AT END
                       MOVE IN-USER-CHOICE TO WS-USER-CHOICE
               END-READ
           END-PERFORM

       END-PERFORM.

