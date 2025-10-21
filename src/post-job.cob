>>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. POST-JOB.
AUTHOR. Paola
DATE-WRITTEN. 10/19/2025

*> This program appends a new job posting to the jobs.txt file.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    *> Define the new jobs file
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

LINKAGE SECTION.
*> Data passed from incollege.cob
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

    MOVE 'S' TO LS-RETURN-CODE.

    *> Open the file in EXTEND mode to add to the end
    OPEN EXTEND JOBS-FILE.

    *> Check if file opened successfully
    IF WS-JOBS-STATUS NOT = "00" AND WS-JOBS-STATUS NOT = "35"
        MOVE 'F' TO LS-RETURN-CODE
        GOBACK
    END-IF.

    *> Create a single line record, separated by ~
    *> This also links the job to the user who posted it [cite: 152]
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
    GOBACK.
    