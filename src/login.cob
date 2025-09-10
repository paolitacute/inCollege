       >>SOURCE FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN.
       AUTHOR. Kaden
       DATE-WRITTEN. 09/09/2025

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT ACCOUNTS-FILE ASSIGN TO "accounts.txt"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS WS-ACCOUNTS-STATUS.

       DATA DIVISION.
       FILE SECTION.
           FD  ACCOUNTS-FILE.
           01  ACCOUNTS-RECORD-DATA PIC X(50).  *> entire line from accounts file

       WORKING-STORAGE SECTION.
           01  WS-EOF-FLAG        PIC X(1) VALUE 'N'.
           01  WS-ACCOUNTS-STATUS PIC X(2).
           01  WS-USER-FROM-FILE  PIC X(20).
           01  WS-PASS-FROM-FILE  PIC X(20).

       LINKAGE SECTION.
           01  LS-USERNAME     PIC X(20).
           01  LS-PASSWORD     PIC X(20).
           01  LS-RETURN-CODE  PIC X.

       PROCEDURE DIVISION USING LS-USERNAME, LS-PASSWORD, LS-RETURN-CODE.

           *> Default to failed login
           MOVE 'F' TO LS-RETURN-CODE
           MOVE 'N' TO WS-EOF-FLAG

           *> Trim input username and password to remove extra spaces or newlines
           MOVE FUNCTION TRIM(LS-USERNAME) TO LS-USERNAME
           MOVE FUNCTION TRIM(LS-PASSWORD) TO LS-PASSWORD

           OPEN INPUT ACCOUNTS-FILE

           IF WS-ACCOUNTS-STATUS = "35"
               *> File does not exist, cannot login
               MOVE 'F' TO LS-RETURN-CODE
               CLOSE ACCOUNTS-FILE
               GOBACK
           END-IF

           IF WS-ACCOUNTS-STATUS NOT = "00"
               DISPLAY "Error opening accounts file."
               DISPLAY "File Status: " WS-ACCOUNTS-STATUS
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE ACCOUNTS-FILE
               GOBACK
           END-IF

           *> Search the file for matching username and password
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       *> Split line into username and password
                       UNSTRING ACCOUNTS-RECORD-DATA
                           DELIMITED BY ALL SPACE
                           INTO WS-USER-FROM-FILE
                                WS-PASS-FROM-FILE

                       *> Trim both values
                       MOVE FUNCTION TRIM(WS-USER-FROM-FILE) TO WS-USER-FROM-FILE
                       MOVE FUNCTION TRIM(WS-PASS-FROM-FILE) TO WS-PASS-FROM-FILE

                       *> Debug output
                       *> DISPLAY "USERNAME ENTERED=[" LS-USERNAME "]"
                       *> DISPLAY "USERNAME FILE   =[" WS-USER-FROM-FILE "]"
                       *> DISPLAY "PASSWORD ENTERED=[" LS-PASSWORD "]"
                       *> DISPLAY "PASSWORD FILE   =[" WS-PASS-FROM-FILE "]"

                       *> Compare with input
                       IF WS-USER-FROM-FILE = LS-USERNAME
                          AND WS-PASS-FROM-FILE = LS-PASSWORD
                           MOVE 'S' TO LS-RETURN-CODE
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM

           CLOSE ACCOUNTS-FILE

           GOBACK.
