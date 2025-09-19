>>SOURCE FREE
*> Program to search for a user by first and last name in profiles file
IDENTIFICATION DIVISION.
PROGRAM-ID. SEARCH.
AUTHOR. Jacob
DATE-WRITTEN. 09/18/2025

*> Define file handling environment
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    *> Input file containing profile data in line sequential format
    SELECT PROFILE-FILE ASSIGN TO 'profiles.txt'
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-FILE-STATUS.

DATA DIVISION.

*> File record structure
FILE SECTION.
*> Record structure for reading profile data (250 chars max)
FD PROFILE-FILE.
01 PROFILE-RECORD       PIC X(250).

*> Working variables for program processing
WORKING-STORAGE SECTION.
01 WS-FILE-STATUS       PIC XX.              *> File operation status codes
01 WS-EOF-FLAG          PIC X VALUE 'N'.     *> End-of-file indicator
01 WS-MESSAGE           PIC X(80).           *> General purpose message buffer
01 WS-TEMP-ONE          PIC X(80).           *> Temporary storage for username
01 WS-CURRENT-FIRST     PIC X(50).           *> Current profile's first name being examined
01 WS-CURRENT-LAST      PIC X(50).           *> Current profile's last name being examined

*> Parameters passed from calling program
LINKAGE SECTION.
*> Search criteria - first and last names to find
01 LS-FIRST             PIC X(20).           *> Target first name to search for
01 LS-LAST              PIC X(20).           *> Target last name to search for

*> Complete profile data structure (not used in this search program)
01 LS-PROFILE-DATA.
   05 LS-FIRST-NAME     PIC X(50).           *> User's first name
   05 LS-LAST-NAME      PIC X(50).           *> User's last name
   05 LS-UNIVERSITY     PIC X(100).          *> University name
   05 LS-MAJOR          PIC X(50).           *> Academic major
   05 LS-GRAD-YEAR      PIC X(4).            *> Graduation year
   05 LS-ABOUT-ME       PIC X(200).          *> Personal description
   *> Work experience table (up to 3 entries)
   05 LS-EXPERIENCE-TABLE.
      10 LS-EXPERIENCE OCCURS 3 TIMES INDEXED BY EXP-IDX.
         15 LS-EXP-TITLE    PIC X(50).       *> Job title
         15 LS-EXP-COMPANY  PIC X(50).       *> Company name
         15 LS-EXP-DATES    PIC X(50).       *> Employment dates
         15 LS-EXP-DESC     PIC X(100).      *> Job description
   05 LS-EXP-COUNT      PIC 9.               *> Number of experience entries
   *> Education table (up to 3 entries)
   05 LS-EDUCATION-TABLE.
      10 LS-EDUCATION OCCURS 3 TIMES INDEXED BY EDU-IDX.
         15 LS-EDU-DEGREE   PIC X(50).       *> Degree type
         15 LS-EDU-UNIV     PIC X(50).       *> University name
         15 LS-EDU-YEARS    PIC X(50).       *> Years attended
   05 LS-EDU-COUNT      PIC 9.               *> Number of education entries

*> Return values to calling program
01 LS-RETURN-CODE       PIC X.               *> Success/failure indicator
01 LS-RETURN-USER       PIC X(20).           *> Username of found profile

*> Main program logic - searches for user by first and last name
PROCEDURE DIVISION USING LS-FIRST, LS-LAST, LS-PROFILE-DATA, LS-RETURN-CODE, LS-RETURN-USER.

    *> Initialize return code to 'F' (failure) and reset EOF flag
    MOVE 'F' TO LS-RETURN-CODE
    MOVE 'N' TO WS-EOF-FLAG

    *> Clean up input names by removing extra spaces/newlines
    MOVE FUNCTION TRIM(LS-FIRST) TO LS-FIRST
    MOVE FUNCTION TRIM(LS-LAST) TO LS-LAST

    *> Open the profiles file for reading
    OPEN INPUT PROFILE-FILE

    *> Check if file opened successfully
    IF WS-FILE-STATUS NOT = "00"
       DISPLAY "Error opening profiles file."
       DISPLAY "File Status: " WS-FILE-STATUS
       MOVE 'X' TO LS-RETURN-CODE            *> Set error code
       CLOSE PROFILE-FILE
       GOBACK
    END-IF

    *> Main search loop - read through entire file looking for matching names and return the username for view-profile
    PERFORM UNTIL WS-EOF-FLAG = 'Y'
       READ PROFILE-FILE
           AT END
               MOVE 'Y' TO WS-EOF-FLAG       *> Set end-of-file flag
           NOT AT END
               *> Process each line based on its prefix
               EVALUATE TRUE
                   *> Found a username line - store it temporarily
                   WHEN PROFILE-RECORD(1:5) = "USER:"
                   MOVE PROFILE-RECORD(6:) TO WS-TEMP-ONE

                   *> Found a first name line - store current profile's first name
                   WHEN PROFILE-RECORD(1:5) = "FNAM:"
                       MOVE PROFILE-RECORD(6:) TO WS-CURRENT-FIRST

                   *> Found a last name line - store current profile's last name
                   WHEN PROFILE-RECORD(1:5) = "LNAM:"
                       MOVE PROFILE-RECORD(6:) TO WS-CURRENT-LAST
               END-EVALUATE

               *> Check if current profile matches search criteria
               IF WS-CURRENT-FIRST = LS-FIRST AND WS-CURRENT-LAST = LS-LAST
                   *> Match found! Return the username and set success flag
                   MOVE WS-TEMP-ONE TO LS-RETURN-USER
                   MOVE 'T' TO LS-RETURN-CODE    *> Set success code ('T' for True/Found)
                   MOVE 'Y' TO WS-EOF-FLAG       *> Stop searching
               END-IF
       END-READ
    END-PERFORM

    *> Clean up - close file and return to calling program
    CLOSE PROFILE-FILE
    GOBACK.
