>>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. View-Profile.
AUTHOR. Jacob
DATE-WRITTEN. 09/12/2025

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROFILE-FILE ASSIGN TO 'profiles.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
       FILE SECTION.
       FD PROFILE-FILE
           RECORD CONTAINS 250 characters
           DATA RECORD IS PROFILE-RECORD
           FILE STATUS IS WS-FILE-STATUS.
       01 PROFILE-RECORD       PIC X(250).
           05 PR-FULL-RECORD   PIC X(250).

       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUS       PIC XX.
       01 WS-EOF-FLAG          PIC X VALUE 'N'.

       *> Parsed profile fields
       01 WS-PROFILE-DATA.
          05 WS-FIRST-NAME     PIC X(50).
          05 WS-LAST-NAME      PIC X(50).
          05 WS-UNIVERSITY     PIC X(100).
          05 WS-MAJOR          PIC X(50).
          05 WS-GRAD-YEAR      PIC X(4).
          05 WS-ABOUT-ME       PIC X(200).

       *> Experience (up to 3)
       01 WS-EXPERIENCE-TABLE.
          05 WS-EXPERIENCE OCCURS 3 TIMES.
             10 WS-EXP-TITLE    PIC X(50).
             10 WS-EXP-COMPANY  PIC X(50).
             10 WS-EXP-DATES    PIC X(50).
             10 WS-EXP-DESC     PIC X(100).
       01 WS-EXP-COUNT          PIC 9.

       *> Education (up to 3)
       01 WS-EDUCATION-TABLE.
          05 WS-EDUCATION OCCURS 3 TIMES.
             10 WS-EDU-DEGREE   PIC X(50).
             10 WS-EDU-UNIV     PIC X(50).
             10 WS-EDU-YEARS    PIC X(50).
       01 WS-EDU-COUNT          PIC 9.

       LINKAGE SECTION.
       01 LS-USERNAME       PIC X(20).
       01 LS-PROFILE-DATA.
          05 LS-FIRST-NAME     PIC X(50).
          05 LS-LAST-NAME      PIC X(50).
          05 LS-UNIVERSITY     PIC X(100).
          05 LS-MAJOR          PIC X(50).
          05 LS-GRAD-YEAR      PIC X(4).
          05 LS-ABOUT-ME       PIC X(200).

       01 LS-EXPERIENCE-TABLE.
          05 LS-EXPERIENCE OCCURS 3 TIMES.
             10 LS-EXP-TITLE    PIC X(50).
             10 LS-EXP-COMPANY  PIC X(50).
             10 LS-EXP-DATES    PIC X(50).
             10 LS-EXP-DESC     PIC X(100).
       01 LS-EXP-COUNT      PIC 9.

       01 LS-EDUCATION-TABLE.
          05 LS-EDUCATION OCCURS 3 TIMES.
             10 LS-EDU-DEGREE   PIC X(50).
             10 LS-EDU-UNIV     PIC X(50).
             10 LS-EDU-YEARS    PIC X(50).
       01 LS-EDU-COUNT      PIC 9.

       01 LS-RETURN-CODE      PIC X.

PROCEDURE DIVISION USING LS-USERNAME, LS-PROFILE-DATA, LS-RETURN-CODE
       MOVE 'F' TO LS-RETURN-CODE
       MOVE 'N' TO WS-EOF-FLAG

       *> Trim input username and password to remove extra spaces or newlines
       MOVE FUNCTION TRIM(LS-USERNAME) TO LS-USERNAME
       MOVE FUNCTION TRIM(LS-PASSWORD) TO LS-PASSWORD

       OPEN INPUT PROFILE-FILE

       IF WS-FILE-STATUS = "35"
           *> File does not exist, cannot login
           MOVE 'F' TO LS-RETURN-CODE
           CLOSE PROFILE-FILE
           GOBACK
        END-IF
       IF WS-FILE-STATUS = "00"
           DISPLAY "Error opening profiles file."
           DISPLAY "File Status: " WS-FILE-STATUS
           MOVE 'X' TO LS-RETURN-CODE
           CLOSE PROFILE-FILE
           GOBACK
       END-IF.

       PERFORM UNTIL WS-EOF-FLAG = 'Y'
           READ PROFILE-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   MOVE 'N' TO WS-EOF-FLAG
           END READ

           IF WS-EOF-FLAG = 'N'
               IF PROFILE-RECORD(1:5) = "USER:"
                   IF PROFILE-RECORD(6:) = LS-USERNAME
                       PERFORM PARSE-PROFILE
                       EXIT PERFORM
                   END-IF
               END-IF
           END-IF
       END-PERFORM


       PARSE-PROFILE.
       PERFORM UNTIL PROFILE-RECORD = "ENDPROFILE"
           READ PROFILE-FILE INTO PROFILE-RECORD
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
           END-READ

           IF PROFILE-RECORD(1:5) = "FNAM:"
               MOVE PROFILE-RECORD(6:) TO LS-FIRST-NAME
           ELSE IF PROFILE-RECORD(1:5) = "LNAM:"
               MOVE PROFILE-RECORD(6:) TO LS-LAST-NAME
           ELSE IF PROFILE-RECORD(1:5) = "UNIV:"
               MOVE PROFILE-RECORD(6:) TO LS-UNIVERSITY
           ELSE IF PROFILE-RECORD(1:5) = "MAJR:"
               MOVE PROFILE-RECORD(6:) TO LS-MAJOR
           ELSE IF PROFILE-RECORD(1:5) = "GRAD:"
               MOVE PROFILE-RECORD(6:) TO LS-GRAD-YEAR
           ELSE IF PROFILE-RECORD(1:5) = "ABOU:"
               MOVE PROFILE-RECORD(6:) TO LS-ABOUT-ME
           ELSE IF PROFILE-RECORD(1:5) = "EXP01:" OR
                   PROFILE-RECORD(1:5) = "EXP02:" OR
                   PROFILE-RECORD(1:5) = "EXP03:"
               *> Split by '~' into LS-EXPERIENCE
               PERFORM PARSE-EXPERIENCE-LINE
           ELSE IF PROFILE-RECORD(1:5) = "EDU01:" OR
                   PROFILE-RECORD(1:5) = "EDU02:" OR
                   PROFILE-RECORD(1:5) = "EDU03:"
               *> Split by '~' into LS-EDUCATION
               PERFORM PARSE-EDUCATION-LINE
           END-IF
       END-PERFORM

       PARSE-EXPERIENCE-LINE.
           UNSTRING PROFILE-RECORD DELIMITED BY "~"
               INTO LS-EXPERIENCE(WS-EXP-COUNT + 1)-LS-EXP-TITLE
                    LS-EXPERIENCE(WS-EXP-COUNT + 1)-LS-EXP-COMPANY
                    LS-EXPERIENCE(WS-EXP-COUNT + 1)-LS-EXP-DATES
                    LS-EXPERIENCE(WS-EXP-COUNT + 1)-LS-EXP-DESC
           END-UNSTRING
           ADD 1 TO WS-EXP-COUNT
