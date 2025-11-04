        >>SOURCE FREE
        IDENTIFICATION DIVISION.
        PROGRAM-ID. POST-JOB.
        AUTHOR. Paola
        DATE-WRITTEN. 10/19/2025

        *> This program appends a new job posting to the jobs.txt file,
        *> after checking for duplicates.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT JOBS-FILE ASSIGN TO "jobs.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-JOBS-STATUS.

        DATA DIVISION.
        FILE SECTION.
        FD  JOBS-FILE.
        01  JOB-RECORD           PIC X(500).

        WORKING-STORAGE SECTION.
        01  WS-JOBS-STATUS       PIC XX.
        01  WS-JOB-LINE          PIC X(500).
        01  WS-EOF               PIC X VALUE 'N'.

        *> Fields to read existing job data for duplicate check
        01  WS-READ-JOB-DATA.
            05 WS-JOB-POSTER      PIC X(20).
            05 WS-JOB-TITLE       PIC X(50).
            05 WS-JOB-DESC        PIC X(200).
            05 WS-JOB-EMPLOYER    PIC X(50).
            05 WS-JOB-LOCATION    PIC X(50).
            05 WS-JOB-SALARY      PIC X(50).
            05 WS-FILLER          PIC X(100). *> Handle extra data/line variations

        LINKAGE SECTION.
        01  LS-USERNAME          PIC X(20).
        01  LS-JOB-TITLE         PIC X(50).
        01  LS-JOB-DESC          PIC X(200).
        01  LS-JOB-EMPLOYER      PIC X(50).
        01  LS-JOB-LOCATION      PIC X(50).
        01  LS-JOB-SALARY        PIC X(50).
        01  LS-RETURN-CODE       PIC X.

        PROCEDURE DIVISION USING LS-USERNAME, LS-JOB-TITLE, LS-JOB-DESC,
                                LS-JOB-EMPLOYER, LS-JOB-LOCATION,
                                LS-JOB-SALARY, LS-RETURN-CODE.

            MOVE 'N' TO WS-EOF.
            MOVE SPACES TO LS-RETURN-CODE.

            *> --- 1. CHECK FOR DUPLICATES ---
            OPEN INPUT JOBS-FILE.

            *> If file doesn't exist ("35"), it's not a duplicate.
            IF WS-JOBS-STATUS = "35"
                CLOSE JOBS-FILE
                PERFORM WRITE-NEW-JOB
                MOVE 'S' TO LS-RETURN-CODE
                GOBACK
            END-IF.

            *> Check for other file open errors
            IF WS-JOBS-STATUS NOT = "00"
                MOVE 'F' TO LS-RETURN-CODE
                CLOSE JOBS-FILE
                GOBACK
            END-IF.

            *> Read the file to find duplicates
            PERFORM UNTIL WS-EOF = 'Y'
                READ JOBS-FILE INTO WS-JOB-LINE
                    AT END
                        MOVE 'Y' TO WS-EOF
                    NOT AT END
                        INITIALIZE WS-READ-JOB-DATA
                        UNSTRING WS-JOB-LINE DELIMITED BY "~"
                            INTO WS-JOB-POSTER WS-JOB-TITLE WS-JOB-DESC
                                WS-JOB-EMPLOYER WS-JOB-LOCATION WS-JOB-SALARY
                        END-UNSTRING

                        *> Check if same user posted same title
                        IF FUNCTION TRIM(LS-USERNAME) = FUNCTION TRIM(WS-JOB-POSTER) AND
                        FUNCTION TRIM(LS-JOB-TITLE) = FUNCTION TRIM(WS-JOB-TITLE)
                            MOVE 'D' TO LS-RETURN-CODE *> 'D' for Duplicate
                            MOVE 'Y' TO WS-EOF         *> Stop loop
                        END-IF
                END-READ
            END-PERFORM.

            CLOSE JOBS-FILE.

            *> --- 2. WRITE NEW JOB (if not a duplicate) ---
            IF LS-RETURN-CODE = 'D'
                GOBACK *> Found a duplicate, return code is set
            ELSE
                PERFORM WRITE-NEW-JOB
                IF LS-RETURN-CODE NOT = 'F'
                    MOVE 'S' TO LS-RETURN-CODE
                END-IF
            END-IF.

            GOBACK.

        WRITE-NEW-JOB SECTION.
            *> Open the file in EXTEND mode to add to the end
            OPEN EXTEND JOBS-FILE.

            *> Handle case where file was deleted between read and write
            IF WS-JOBS-STATUS = "35"
                OPEN OUTPUT JOBS-FILE
            END-IF.

            *> Check for open error
            IF WS-JOBS-STATUS NOT = "00"
                MOVE 'F' TO LS-RETURN-CODE
                GOBACK
            END-IF.

            *> Create a single line record, separated by ~
            INITIALIZE WS-JOB-LINE
            STRING FUNCTION TRIM(LS-USERNAME)     DELIMITED BY SIZE
                "~"                           DELIMITED BY SIZE
                FUNCTION TRIM(LS-JOB-TITLE)     DELIMITED BY SIZE
                "~"                           DELIMITED BY SIZE
                FUNCTION TRIM(LS-JOB-DESC)      DELIMITED BY SIZE
                "~"                           DELIMITED BY SIZE
                FUNCTION TRIM(LS-JOB-EMPLOYER)  DELIMITED BY SIZE
                "~"                           DELIMITED BY SIZE
                FUNCTION TRIM(LS-JOB-LOCATION)  DELIMITED BY SIZE
                "~"                           DELIMITED BY SIZE
                FUNCTION TRIM(LS-JOB-SALARY)    DELIMITED BY SIZE
                INTO WS-JOB-LINE.

            *> Write the new job posting to the file
            WRITE JOB-RECORD FROM WS-JOB-LINE.

            CLOSE JOBS-FILE.
            EXIT.
