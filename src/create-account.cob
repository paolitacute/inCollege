>>SOURCE FREE
IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-ACCOUNT.
       AUTHOR. Paola
       DATE-WRITTEN. 09/09/2025

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT ACCOUNTS-FILE ASSIGN TO "accounts.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS ACCOUNTS-USERNAME
                   FILE STATUS IS WS-ACCOUNTS-STATUS.

DATA DIVISION.
       FILE SECTION.
           FD  ACCOUNTS-FILE.
           01  ACCOUNTS-RECORD-DATA.
               05  ACCOUNTS-USERNAME PIC X(20).
               05  ACCOUNTS-PASSWORD PIC X(20).
               05  FILLER            PIC X(10).

       WORKING-STORAGE SECTION.
           01  WS-ACCOUNT-COUNT    PIC 9(1) VALUE 0.
           01  WS-ACCOUNT-LIMIT    PIC 9(1) VALUE 5.
           01  WS-EOF-FLAG         PIC X(1) VALUE 'N'.
           01  WS-USERNAME-EXISTS  PIC X(1) VALUE 'N'.

           01  WS-ACCOUNTS-STATUS PIC X(2).

           01  WS-PASSWORD-FLAGS.
               05  WS-HAS-CAPITAL      PIC X VALUE 'N'.
               05  WS-HAS-DIGIT        PIC X VALUE 'N'.
               05  WS-HAS-SPECIAL      PIC X VALUE 'N'.

           01  WS-PASSWORD-INDEX   PIC 99.

       LINKAGE SECTION.
           01  LS-USERNAME     PIC X(20).
           01  LS-PASSWORD     PIC X(20).
           01  LS-RETURN-CODE  PIC X.


PROCEDURE DIVISION USING LS-USERNAME, LS-PASSWORD, LS-RETURN-CODE.
       MOVE 'S' TO LS-RETURN-CODE.

       OPEN I-O ACCOUNTS-FILE.

       *> Check account limit
       MOVE 0 TO WS-ACCOUNT-COUNT.
       MOVE 'N' TO WS-EOF-FLAG.

       *> ADDED: Initialize key to start reading from the beginning of the file.
       MOVE LOW-VALUES TO ACCOUNTS-USERNAME.

       START ACCOUNTS-FILE KEY IS NOT LESS THAN ACCOUNTS-USERNAME
           INVALID KEY MOVE 'Y' TO WS-EOF-FLAG
       END-START.

       IF WS-EOF-FLAG = 'N'
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNTS-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END ADD 1 TO WS-ACCOUNT-COUNT
               END-READ
           END-PERFORM
       END-IF.

       IF WS-ACCOUNT-COUNT >= WS-ACCOUNT-LIMIT
           MOVE 'L' TO LS-RETURN-CODE
           PERFORM CLOSE-PROGRAM
       END-IF.

       *> Check if username exists (This is a random read)
       MOVE LS-USERNAME TO ACCOUNTS-USERNAME.
       MOVE 'N' TO WS-USERNAME-EXISTS.

       READ ACCOUNTS-FILE KEY IS ACCOUNTS-USERNAME
           INVALID KEY
               MOVE 'N' TO WS-USERNAME-EXISTS
           NOT INVALID KEY
               MOVE 'Y' TO WS-USERNAME-EXISTS
       END-READ.

       IF WS-USERNAME-EXISTS = 'Y'
           MOVE 'E' TO LS-RETURN-CODE
           PERFORM CLOSE-PROGRAM
       END-IF.

       *> Password validation
       PERFORM PASSWORD-VALIDATION.

       IF LS-RETURN-CODE = 'F'
           PERFORM CLOSE-PROGRAM
       END-IF.

       *> Write new account
       MOVE LS-USERNAME TO ACCOUNTS-USERNAME.
       MOVE LS-PASSWORD TO ACCOUNTS-PASSWORD.
       WRITE ACCOUNTS-RECORD-DATA
           INVALID KEY
               DISPLAY "Error writing new account record."
               MOVE 'X' TO LS-RETURN-CODE
       END-WRITE.

       PERFORM CLOSE-PROGRAM.

       PASSWORD-VALIDATION SECTION.
           *> Check for password length
           IF FUNCTION LENGTH(FUNCTION TRIM(LS-PASSWORD)) < 8 OR
              FUNCTION LENGTH(FUNCTION TRIM(LS-PASSWORD)) > 12
               MOVE 'F' TO LS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           *> Reset flags before checking a new password
           MOVE 'N' TO WS-HAS-CAPITAL.
           MOVE 'N' TO WS-HAS-DIGIT.
           MOVE 'N' TO WS-HAS-SPECIAL.

           *> Check for character types
           PERFORM VARYING WS-PASSWORD-INDEX FROM 1 BY 1
               UNTIL WS-PASSWORD-INDEX > FUNCTION LENGTH(FUNCTION TRIM(LS-PASSWORD))

               EVALUATE TRUE
                   WHEN LS-PASSWORD(WS-PASSWORD-INDEX:1) IS ALPHABETIC-UPPER
                       MOVE 'Y' TO WS-HAS-CAPITAL
                   WHEN LS-PASSWORD(WS-PASSWORD-INDEX:1) IS NUMERIC
                       MOVE 'Y' TO WS-HAS-DIGIT
                   WHEN LS-PASSWORD(WS-PASSWORD-INDEX:1) IS NOT ALPHABETIC
                    AND LS-PASSWORD(WS-PASSWORD-INDEX:1) IS NOT NUMERIC
                       MOVE 'Y' TO WS-HAS-SPECIAL
               END-EVALUATE
           END-PERFORM.

           IF WS-HAS-CAPITAL = 'N' OR WS-HAS-DIGIT = 'N' OR WS-HAS-SPECIAL = 'N'
               MOVE 'F' TO LS-RETURN-CODE
           END-IF.
           EXIT.

       CLOsE-PROGRAM SECTION.
           CLOSE ACCOUNTS-FILE.
           GOBACK.


