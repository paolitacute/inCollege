>>SOURCE FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BROWSE-JOB.
       AUTHOR. Assistant.
       DATE-WRITTEN. 10/26/2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JOBS-FILE ASSIGN TO "jobs.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOBS-STATUS.
           SELECT APPLICATIONS-FILE ASSIGN TO "applications.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APP-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  JOBS-FILE.
       01  JOB-RECORD        PIC X(500).
       FD  APPLICATIONS-FILE.
       01  APPLICATION-RECORD PIC X(500).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD     PIC X(350).
       WORKING-STORAGE SECTION.
       01  WS-JOBS-STATUS    PIC XX.
       01  WS-APP-STATUS     PIC XX.
       01  WS-EOF            PIC X VALUE 'N'.
       01  WS-COUNT          PIC 9(1) VALUE 0.

       01  WS-DISLPAY-NUMBER PIC Z(3).
       01  WS-MESSAGE        PIC X(200).
       01  WS-TEMP           PIC X(200).
       01  WS-USER           PIC X(20).
       01  WS-LINE           PIC X(500).
       01  WS-JOB-TITLE      PIC X(50).
       01  WS-JOB-DESC       PIC X(200).
       01  WS-JOB-EMPLOYER   PIC X(50).
       01  WS-JOB-LOCATION   PIC X(50).
       01  WS-JOB-SALARY     PIC X(50).
       01  WS-JOB-POSTER     PIC X(50).

       *> --- NEW VARIABLES FOR DUPLICATE CHECK ---
       01  WS-CHECK-VARS.
           05 WS-CHECK-USER      PIC X(20).
           05 WS-CHECK-JOB-NUM   PIC X(200).
           05 WS-CHECK-TITLE     PIC X(50).
           05 WS-CHECK-EMPLOYER  PIC X(50).
           05 WS-CHECK-LOCATION  PIC X(50).
       *> --- END OF NEW VARIABLES ---

       LINKAGE SECTION.
       01  LS-USERNAME       PIC X(20).
       01  LS-ACTION         PIC X(10).
       01  LS-JOB-NUM        PIC 9(1).
       01  LS-RETURN-CODE    PIC X.

       PROCEDURE DIVISION USING LS-USERNAME, LS-ACTION, LS-JOB-NUM, LS-RETURN-CODE.
       *> Default return to failure
       MOVE 'F' TO LS-RETURN-CODE.
       *> Normalize username and action
       MOVE FUNCTION TRIM(LS-USERNAME) TO LS-USERNAME
       MOVE FUNCTION TRIM(LS-ACTION) TO LS-ACTION

       EVALUATE TRUE
           WHEN LS-ACTION = "LIST"
               PERFORM JOB-LIST
           WHEN LS-ACTION = "DETAIL"
               PERFORM JOB-DETAIL
           WHEN LS-ACTION = "APPLY"
               PERFORM JOB-APPLY
           WHEN LS-ACTION = "VIEW"
               PERFORM VIEW-APPLICATIONS
           WHEN OTHER
               MOVE 'X' TO LS-RETURN-CODE
       END-EVALUATE
       GOBACK.
       JOB-LIST.
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-COUNT
           OPEN INPUT JOBS-FILE
           OPEN EXTEND OUTPUT-FILE
           IF WS-JOBS-STATUS NOT = "00"
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE JOBS-FILE
               CLOSE OUTPUT-FILE
               GOBACK
           END-IF

           PERFORM UNTIL WS-EOF = 'Y'
               READ JOBS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE 'N' TO WS-EOF
               END-READ

               IF WS-EOF = 'N'
                   MOVE JOB-RECORD TO WS-LINE
                   UNSTRING WS-LINE DELIMITED BY "~"
                       INTO WS-JOB-POSTER WS-JOB-TITLE WS-JOB-DESC
                            WS-JOB-EMPLOYER WS-JOB-LOCATION WS-JOB-SALARY
                   END-UNSTRING

                   ADD 1 TO WS-COUNT
                   MOVE WS-COUNT TO WS-DISLPAY-NUMBER
                   INITIALIZE WS-MESSAGE
                   STRING FUNCTION TRIM(WS-DISLPAY-NUMBER) ". "
                          FUNCTION TRIM(WS-JOB-TITLE)
                          " at " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-JOB-EMPLOYER)
                          " (" DELIMITED BY SIZE
                          FUNCTION TRIM(WS-JOB-LOCATION)
                          ")" DELIMITED BY SIZE
                          INTO WS-MESSAGE
                   DISPLAY WS-MESSAGE
                   MOVE WS-MESSAGE TO OUTPUT-RECORD
                   WRITE OUTPUT-RECORD
               END-IF
           END-PERFORM

           IF WS-COUNT = 0
               MOVE "No job listings available." TO WS-MESSAGE
               DISPLAY WS-MESSAGE
               MOVE WS-MESSAGE TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
           END-IF

           MOVE 'S' TO LS-RETURN-CODE
           CLOSE JOBS-FILE
           CLOSE OUTPUT-FILE
           GOBACK.

       JOB-DETAIL.
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-COUNT
           OPEN INPUT JOBS-FILE
           OPEN EXTEND OUTPUT-FILE
           IF WS-JOBS-STATUS NOT = "00"
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE JOBS-FILE
               CLOSE OUTPUT-FILE
               GOBACK
           END-IF

           PERFORM UNTIL WS-EOF = 'Y'
               READ JOBS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE 'N' TO WS-EOF
               END-READ

               IF WS-EOF = 'N'
                   ADD 1 TO WS-COUNT
                   MOVE WS-COUNT TO WS-DISLPAY-NUMBER
                   MOVE JOB-RECORD TO WS-LINE
                   UNSTRING WS-LINE DELIMITED BY "~"
                       INTO WS-JOB-POSTER WS-JOB-TITLE WS-JOB-DESC
                            WS-JOB-EMPLOYER WS-JOB-LOCATION WS-JOB-SALARY
                   END-UNSTRING

                   IF WS-COUNT = LS-JOB-NUM
                       *> Display full details
                       INITIALIZE WS-MESSAGE
                       STRING "Job Title: " FUNCTION TRIM(WS-JOB-TITLE)
                              INTO WS-MESSAGE
                       DISPLAY WS-MESSAGE
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD

                       INITIALIZE WS-MESSAGE
                       STRING "Description: " FUNCTION TRIM(WS-JOB-DESC)
                              INTO WS-MESSAGE
                       DISPLAY WS-MESSAGE
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD

                       INITIALIZE WS-MESSAGE
                       STRING "Employer: " FUNCTION TRIM(WS-JOB-EMPLOYER)
                              INTO WS-MESSAGE
                       DISPLAY WS-MESSAGE
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD

                       INITIALIZE WS-MESSAGE
                       STRING "Location: " FUNCTION TRIM(WS-JOB-LOCATION)
                              INTO WS-MESSAGE
                       DISPLAY WS-MESSAGE
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD

                       IF FUNCTION TRIM(WS-JOB-SALARY) > SPACES
                           INITIALIZE WS-MESSAGE
                           STRING "Salary: " FUNCTION TRIM(WS-JOB-SALARY)
                                  INTO WS-MESSAGE
                           DISPLAY WS-MESSAGE
                           MOVE WS-MESSAGE TO OUTPUT-RECORD
                           WRITE OUTPUT-RECORD
                       END-IF

                       MOVE 'S' TO LS-RETURN-CODE
                       MOVE 'Y' TO WS-EOF *> stop loop
                   END-IF
               END-IF
           END-PERFORM

           IF LS-RETURN-CODE NOT = 'S'
               MOVE "Requested job not found." TO WS-MESSAGE
               DISPLAY WS-MESSAGE
               MOVE WS-MESSAGE TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
           END-IF

           CLOSE JOBS-FILE
           CLOSE OUTPUT-FILE
           GOBACK.
       JOB-APPLY.
           *> Find the job details first
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-COUNT
           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STATUS NOT = "00"
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE JOBS-FILE
               GOBACK
           END-IF

           PERFORM UNTIL WS-EOF = 'Y'
               READ JOBS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE 'N' TO WS-EOF
               END-READ
               IF WS-EOF = 'N'
                   ADD 1 TO WS-COUNT
                   MOVE JOB-RECORD TO WS-LINE
                   UNSTRING WS-LINE DELIMITED BY "~"
                       INTO WS-JOB-POSTER WS-JOB-TITLE WS-JOB-DESC
                            WS-JOB-EMPLOYER WS-JOB-LOCATION WS-JOB-SALARY
                   END-UNSTRING

                   IF WS-COUNT = LS-JOB-NUM
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
           CLOSE JOBS-FILE

           IF WS-COUNT < LS-JOB-NUM OR LS-JOB-NUM = 0
               MOVE 'F' TO LS-RETURN-CODE
               MOVE "Job not found; cannot apply." TO WS-MESSAGE
               OPEN EXTEND OUTPUT-FILE
               DISPLAY WS-MESSAGE
               MOVE WS-MESSAGE TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
               CLOSE OUTPUT-FILE
               GOBACK
           END-IF

           *> Append application record
           *> Check if the user already applied to this job
           *> Move numeric job count to a display field for safe string compare
           MOVE WS-COUNT TO WS-DISLPAY-NUMBER
           OPEN INPUT APPLICATIONS-FILE
           IF WS-APP-STATUS = "00"
               MOVE 'N' TO WS-EOF
               PERFORM UNTIL WS-EOF = 'Y'
                   READ APPLICATIONS-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           MOVE 'N' TO WS-EOF
                   END-READ
                   IF WS-EOF = 'N'
                       MOVE APPLICATION-RECORD TO WS-LINE

                       *> --- MODIFIED LINE: Use new check variables ---
                       UNSTRING WS-LINE DELIMITED BY "~"
                           INTO WS-CHECK-USER WS-CHECK-JOB-NUM
                                WS-CHECK-TITLE WS-CHECK-EMPLOYER
                                WS-CHECK-LOCATION
                       END-UNSTRING

                       *> --- MODIFIED LINE: Use new check variables ---
                       IF FUNCTION TRIM(WS-CHECK-USER) = FUNCTION TRIM(LS-USERNAME)
                          AND FUNCTION TRIM(WS-CHECK-JOB-NUM) = FUNCTION TRIM(WS-DISLPAY-NUMBER)
                           *> Already applied — inform user and exit
                           CLOSE APPLICATIONS-FILE
                           OPEN EXTEND OUTPUT-FILE
                           MOVE "You have already applied to this job." TO WS-MESSAGE
                           DISPLAY WS-MESSAGE
                           MOVE WS-MESSAGE TO OUTPUT-RECORD
                           WRITE OUTPUT-RECORD
                           CLOSE OUTPUT-FILE
                           MOVE 'F' TO LS-RETURN-CODE
                           GOBACK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           CLOSE APPLICATIONS-FILE

           *> --- BEGINNING OF COMBINED FIX ---
           INITIALIZE WS-LINE
           STRING FUNCTION TRIM(LS-USERNAME) DELIMITED BY SIZE
                  "~" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-COUNT) DELIMITED BY SIZE
                  "~" DELIMITED BY SIZE
                  *> These variables are now safe and were not overwritten
                  FUNCTION TRIM(WS-JOB-TITLE) DELIMITED BY SPACE
                  "~" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER) DELIMITED BY SIZE
                  "~" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION) DELIMITED BY SIZE
                  INTO WS-LINE

           *> Open for append, or create if it doesn't exist
           OPEN EXTEND APPLICATIONS-FILE
           IF WS-APP-STATUS = "35"
               OPEN OUTPUT APPLICATIONS-FILE
           END-IF

           *> Write the new application record ONCE
           IF WS-APP-STATUS = "00"
               WRITE APPLICATION-RECORD FROM WS-LINE
               *> Set success code ONLY if write is successful
               MOVE 'S' TO LS-RETURN-CODE
           ELSE
               *> Set error code if file couldn't be opened
               MOVE 'X' TO LS-RETURN-CODE
           END-IF

           *> Close the file ONCE
           CLOSE APPLICATIONS-FILE

           *> Log confirmation only if write was successful
           IF LS-RETURN-CODE = 'S'
               OPEN EXTEND OUTPUT-FILE
               INITIALIZE WS-MESSAGE
               STRING "Your application for " FUNCTION TRIM(WS-JOB-TITLE)
                      " at " FUNCTION TRIM(WS-JOB-EMPLOYER)
                      " has been submitted."
                      DELIMITED BY SIZE
                      INTO WS-MESSAGE
               DISPLAY WS-MESSAGE
               MOVE WS-MESSAGE TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
               CLOSE OUTPUT-FILE
           END-IF
           *> --- END OF COMBINED FIX ---

           GOBACK.
       VIEW-APPLICATIONS.
           MOVE 0 TO WS-COUNT
           OPEN INPUT APPLICATIONS-FILE
           OPEN EXTEND OUTPUT-FILE
           IF WS-APP-STATUS NOT = "00"
               *> If file doesn't exist or can't be opened, treat as no applications
               MOVE "Your Job Applications" TO WS-MESSAGE
               DISPLAY WS-MESSAGE
               MOVE WS-MESSAGE TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
               MOVE "Total applications: 0" TO WS-MESSAGE
               DISPLAY WS-MESSAGE
               MOVE WS-MESSAGE TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
               MOVE 'S' TO LS-RETURN-CODE
               CLOSE OUTPUT-FILE
               GOBACK
           END-IF

           MOVE "Your Job Applications" TO WS-MESSAGE
           DISPLAY WS-MESSAGE
           MOVE WS-MESSAGE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ APPLICATIONS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE 'N' to WS-EOF
               END-READ
               IF WS-EOF = 'N'
                   MOVE APPLICATION-RECORD TO WS-LINE
                   UNSTRING WS-LINE DELIMITED BY "~"
                       INTO WS-USER WS-TEMP WS-JOB-TITLE WS-JOB-EMPLOYER WS-JOB-LOCATION
                   END-UNSTRING

                   *> WS-TEMP contains jobnum
                   IF FUNCTION TRIM(WS-USER) = FUNCTION TRIM(LS-USERNAME)
                       ADD 1 TO WS-COUNT
                       INITIALIZE WS-MESSAGE
                       STRING "Job " FUNCTION TRIM(WS-TEMP) ": "
                              FUNCTION TRIM(WS-JOB-TITLE) " at "
                              FUNCTION TRIM(WS-JOB-EMPLOYER) " ("
                              FUNCTION TRIM(WS-JOB-LOCATION) ")"
                              INTO WS-MESSAGE
                       DISPLAY WS-MESSAGE
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
                   END-IF
               END-IF
           END-PERFORM

           INITIALIZE WS-MESSAGE
           MOVE WS-COUNT TO WS-DISLPAY-NUMBER
           STRING "Total applications: " FUNCTION TRIM(WS-DISLPAY-NUMBER)
                  INTO WS-MESSAGE
           DISPLAY WS-MESSAGE
           MOVE WS-MESSAGE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           MOVE 'S' TO LS-RETURN-CODE
           CLOSE APPLICATIONS-FILE
           CLOSE OUTPUT-FILE
           GOBACK.
