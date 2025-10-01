       >>SOURCE FREE
       IDENTIFICATION DIVISION.
              PROGRAM-ID. CREATE-ACCOUNT.
              AUTHOR. Paola
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
                  01  ACCOUNTS-RECORD-DATA.
                      05  ACCOUNTS-USERNAME PIC X(20).
                      05  ACCOUNTS-PASSWORD PIC X(20).

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

              *> First, read the file to check limits and if user exists.
              PERFORM VALIDATE-ACCOUNT-DATA.

              IF LS-RETURN-CODE NOT = 'S'
                  GOBACK
              END-IF.



              *> Second, validate the password format.
              PERFORM PASSWORD-VALIDATION.

              IF LS-RETURN-CODE = 'F'
                  GOBACK
              END-IF.


              *> If all checks pass, open the file again to add the record.
              PERFORM WRITE-NEW-ACCOUNT.

              GOBACK.


       VALIDATE-ACCOUNT-DATA SECTION.
              MOVE 0 TO WS-ACCOUNT-COUNT.
              MOVE 'N' TO WS-USERNAME-EXISTS.
              MOVE 'N' TO WS-EOF-FLAG.

              OPEN INPUT ACCOUNTS-FILE.

              *> A status of "35" means the file doesn't exist, which is okay.
              *> We will create it in the WRITE-NEW-ACCOUNT paragraph.
              IF WS-ACCOUNTS-STATUS = "35"
                  CLOSE ACCOUNTS-FILE
                  EXIT PARAGRAPH
              END-IF.

              *> Handle other potential open errors.
              IF WS-ACCOUNTS-STATUS NOT = "00"
                  DISPLAY "Error opening accounts file for validation."
                  DISPLAY "File Status: " WS-ACCOUNTS-STATUS
                  MOVE 'X' TO LS-RETURN-CODE
                  CLOSE ACCOUNTS-FILE
                  EXIT PARAGRAPH
              END-IF.

              *> Read the entire file to count records and check for the username.
              PERFORM UNTIL WS-EOF-FLAG = 'Y'
                  READ ACCOUNTS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-ACCOUNT-COUNT
                       IF ACCOUNTS-USERNAME = LS-USERNAME
                           MOVE 'Y' TO WS-USERNAME-EXISTS
                       END-IF
                  END-READ
              END-PERFORM.

              CLOSE ACCOUNTS-FILE.

              *> Set return codes based on what was found.
              IF WS-ACCOUNT-COUNT >= WS-ACCOUNT-LIMIT
                  MOVE 'L' TO LS-RETURN-CODE
              END-IF.

              IF WS-USERNAME-EXISTS = 'Y'
                  MOVE 'E' TO LS-RETURN-CODE
              END-IF.
              EXIT.


       WRITE-NEW-ACCOUNT SECTION.
           *> Open in EXTEND mode to append to the end of the file.
              OPEN EXTEND ACCOUNTS-FILE.

              IF WS-ACCOUNTS-STATUS NOT = "00"
                  DISPLAY "Error opening accounts file for writing."
                  DISPLAY "File Status: " WS-ACCOUNTS-STATUS
                  MOVE 'X' TO LS-RETURN-CODE
                  GOBACK
              END-IF.

              MOVE LS-USERNAME TO ACCOUNTS-USERNAME.
              MOVE LS-PASSWORD TO ACCOUNTS-PASSWORD.
              WRITE ACCOUNTS-RECORD-DATA.

              CLOSE ACCOUNTS-FILE.
              EXIT.


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


