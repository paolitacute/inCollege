       >>SOURCE FREE
       *> Program to view user profiles from a file
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-PROFILE.
       AUTHOR. Jacob.
       DATE-WRITTEN. 09/12/2025.

       *> Define file handling and I/O operations
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *> Input file containing profile data in line sequential format
       SELECT PROFILE-FILE ASSIGN TO 'profiles.txt'
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS WS-FILE-STATUS.
       *> Output file for writing profile display results
       SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
          ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       *> File record structures
       FILE SECTION.
       *> Record structure for reading profile data (250 chars max)
       FD PROFILE-FILE.
       01 PROFILE-RECORD       PIC X(250).

       *> Record structure for writing output data (350 chars max)
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD        PIC X(350).

       *> Working variables for program processing
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS       PIC XX.              *> File operation status codes
       01 WS-EOF-FLAG          PIC X VALUE 'N'.     *> End-of-file indicator
       01 I                    PIC 9.               *> Loop counter for experience entries
       01 J                    PIC 9.               *> Loop counter for education entries
       01 WS-MESSAGE           PIC X(80).           *> Buffer for formatted output messages
       01 WS-TEMP-ONE          PIC X(80).           *> Temporary work area
       01 WS-TEMP-TWO          PIC X(80).           *> Temporary work area
       *> Parameters passed from calling program
       LINKAGE SECTION.
       *> Username to search for in profiles file
       01 LS-USERNAME          PIC X(20).

       *> Complete profile data structure
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

       *> Return code to indicate success/failure to calling program
       01 LS-RETURN-CODE       PIC X.

       *> Main program logic - searches for and displays user profile
       PROCEDURE DIVISION USING LS-USERNAME LS-PROFILE-DATA LS-RETURN-CODE.

       *> Initialize return code to 'F' (failure) and reset EOF flag
       MOVE 'F' TO LS-RETURN-CODE
       MOVE 'N' TO WS-EOF-FLAG

       *> Clean up input username by removing extra spaces/newlines
       MOVE FUNCTION TRIM(LS-USERNAME) TO LS-USERNAME

       *> Open input file for reading and output file for appending
       OPEN INPUT PROFILE-FILE.
       OPEN EXTEND OUTPUT-FILE.

       *> Check if file opened successfully
       IF WS-FILE-STATUS NOT = "00"
          DISPLAY "Error opening profiles file."
          DISPLAY "File Status: " WS-FILE-STATUS
          MOVE 'X' TO LS-RETURN-CODE          *> Set error code
          CLOSE PROFILE-FILE
          GOBACK
       END-IF

       *> Main search loop - read through file looking for matching username
       PERFORM UNTIL WS-EOF-FLAG = 'Y'
          READ PROFILE-FILE
              AT END
                  MOVE 'Y' TO WS-EOF-FLAG
              NOT AT END
                  MOVE 'N' TO WS-EOF-FLAG
          END-READ

          *> Process record if not at end of file
          IF WS-EOF-FLAG = 'N'
              *> Check if this line contains a username marker
              IF PROFILE-RECORD(1:5) = "USER:"
                  *> Check if username matches what we're looking for
                  IF PROFILE-RECORD(6:) = LS-USERNAME
                      *> Found matching user - parse their profile data
                      PERFORM PARSE-PROFILE
                      MOVE 'S' TO LS-RETURN-CODE  *> Set success code
                      MOVE 'Y' TO WS-EOF-FLAG      *> Stop searching
                  END-IF
              END-IF
          END-IF
       END-PERFORM

       *> If profile was found successfully, display it
       IF LS-RETURN-CODE = 'S'
          PERFORM PROFILE-DISPLAY
       END-IF

       *> Clean up - close files and return to calling program
       CLOSE PROFILE-FILE
       CLOSE OUTPUT-FILE
       GOBACK.

       *> Display formatted profile information to screen and output file
       PROFILE-DISPLAY.
              *> Display full name
              INITIALIZE WS-MESSAGE
              STRING "Name: " FUNCTION TRIM(LS-FIRST-NAME) " " FUNCTION TRIM(LS-LAST-NAME)
                 INTO WS-MESSAGE
              DISPLAY WS-MESSAGE
              MOVE WS-MESSAGE TO OUTPUT-RECORD
              WRITE OUTPUT-RECORD

              *> Display university
              INITIALIZE WS-MESSAGE
              STRING "University: " LS-UNIVERSITY
                 INTO WS-MESSAGE
              DISPLAY WS-MESSAGE
              MOVE WS-MESSAGE TO OUTPUT-RECORD
              WRITE OUTPUT-RECORD

              *> Display major
              INITIALIZE WS-MESSAGE
              STRING "Major: " LS-MAJOR
                 INTO WS-MESSAGE
              DISPLAY WS-MESSAGE
              MOVE WS-MESSAGE TO OUTPUT-RECORD
              WRITE OUTPUT-RECORD

              *> Display graduation year
              INITIALIZE WS-MESSAGE
              STRING "Graduation Year: " LS-GRAD-YEAR
                 INTO WS-MESSAGE
              DISPLAY WS-MESSAGE
              MOVE WS-MESSAGE TO OUTPUT-RECORD
              WRITE OUTPUT-RECORD

              *> Display about me section
              INITIALIZE WS-MESSAGE
              STRING "About Me: " LS-ABOUT-ME
                 INTO WS-MESSAGE
              DISPLAY WS-MESSAGE
              MOVE WS-MESSAGE TO OUTPUT-RECORD
              WRITE OUTPUT-RECORD

              *> Display experience section with None check
              IF LS-EXP-COUNT = 0
                  INITIALIZE WS-MESSAGE
                  MOVE "Experience: None" TO WS-MESSAGE
                  DISPLAY WS-MESSAGE
                  MOVE WS-MESSAGE TO OUTPUT-RECORD
                  WRITE OUTPUT-RECORD
              ELSE
                  INITIALIZE WS-MESSAGE
                  MOVE "Experience:" TO WS-MESSAGE
                  DISPLAY WS-MESSAGE
                  MOVE WS-MESSAGE TO OUTPUT-RECORD
                  WRITE OUTPUT-RECORD

                  *> Loop through and display each experience entry
                  PERFORM VARYING I FROM 1 BY 1 UNTIL I > LS-EXP-COUNT
                     *> Display job title with leading space
                     INITIALIZE WS-MESSAGE
                     STRING " Title: " LS-EXP-TITLE(I)
                         INTO WS-MESSAGE
                     DISPLAY WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD

                     *> Display company name with leading space
                     INITIALIZE WS-MESSAGE
                     STRING " Company: " LS-EXP-COMPANY(I)
                         INTO WS-MESSAGE
                     DISPLAY WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD

                     *> Display employment dates with leading space
                     INITIALIZE WS-MESSAGE
                     STRING " Dates: " LS-EXP-DATES(I)
                         INTO WS-MESSAGE
                     DISPLAY WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD

                     *> Display job description with leading space
                     INITIALIZE WS-MESSAGE
                     STRING " Description: " LS-EXP-DESC(I)
                         INTO WS-MESSAGE
                     DISPLAY WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD
                  END-PERFORM
              END-IF

              *> Display education section with None check
              IF LS-EDU-COUNT = 0
                  INITIALIZE WS-MESSAGE
                  MOVE "Education: None" TO WS-MESSAGE
                  DISPLAY WS-MESSAGE
                  MOVE WS-MESSAGE TO OUTPUT-RECORD
                  WRITE OUTPUT-RECORD
              ELSE
                  INITIALIZE WS-MESSAGE
                  MOVE "Education:" TO WS-MESSAGE
                  DISPLAY WS-MESSAGE
                  MOVE WS-MESSAGE TO OUTPUT-RECORD
                  WRITE OUTPUT-RECORD

                  *> Loop through and display each education entry
                  PERFORM VARYING J FROM 1 BY 1 UNTIL J > LS-EDU-COUNT
                     *> Display degree type with leading space
                     INITIALIZE WS-MESSAGE
                     STRING " Degree: " LS-EDU-DEGREE(J)
                         INTO WS-MESSAGE
                     DISPLAY WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD

                     *> Display university name with leading space
                     INITIALIZE WS-MESSAGE
                     STRING " University: " LS-EDU-UNIV(J)
                         INTO WS-MESSAGE
                     DISPLAY WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD

                     *> Display years attended with leading space
                     INITIALIZE WS-MESSAGE
                     STRING " Years: " LS-EDU-YEARS(J)
                         INTO WS-MESSAGE
                     DISPLAY WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD
                  END-PERFORM
              END-IF

              *> Add separator line after profile
              INITIALIZE WS-MESSAGE
              MOVE "--------------------" TO WS-MESSAGE
              DISPLAY WS-MESSAGE
              MOVE WS-MESSAGE TO OUTPUT-RECORD
              WRITE OUTPUT-RECORD.

       *> Parse profile data from file after finding matching username
       PARSE-PROFILE.
       *> Initialize counters for experience and education entries
       MOVE 0 TO LS-EXP-COUNT
       MOVE 0 TO LS-EDU-COUNT

       *> Continue reading and parsing until we hit the end marker
       PERFORM UNTIL PROFILE-RECORD(1:10) = "ENDPROFILE"
          READ PROFILE-FILE INTO PROFILE-RECORD
              AT END
                  MOVE 'Y' TO WS-EOF-FLAG
                  EXIT PERFORM
          END-READ

          *> Parse different types of profile data based on line prefix
          EVALUATE TRUE
              *> Parse first name
              WHEN PROFILE-RECORD(1:5) = "FNAM:"
                  MOVE PROFILE-RECORD(6:) TO LS-FIRST-NAME
              *> Parse last name
              WHEN PROFILE-RECORD(1:5) = "LNAM:"
                  MOVE PROFILE-RECORD(6:) TO LS-LAST-NAME
              *> Parse university
              WHEN PROFILE-RECORD(1:5) = "UNIV:"
                  MOVE PROFILE-RECORD(6:) TO LS-UNIVERSITY
              *> Parse major
              WHEN PROFILE-RECORD(1:5) = "MAJR:"
                  MOVE PROFILE-RECORD(6:) TO LS-MAJOR
              *> Parse graduation year
              WHEN PROFILE-RECORD(1:5) = "GRAD:"
                  MOVE PROFILE-RECORD(6:) TO LS-GRAD-YEAR
              *> Parse about me section
              WHEN PROFILE-RECORD(1:5) = "ABOU:"
                  MOVE PROFILE-RECORD(6:) TO LS-ABOUT-ME
              *> Parse experience entries (up to 3)
              WHEN PROFILE-RECORD(1:6) = "EXP01:" OR
                   PROFILE-RECORD(1:6) = "EXP02:" OR
                   PROFILE-RECORD(1:6) = "EXP03:"
                  PERFORM PARSE-EXPERIENCE-LINE
              *> Parse education entries (up to 3)
              WHEN PROFILE-RECORD(1:6) = "EDU01:" OR
                   PROFILE-RECORD(1:6) = "EDU02:" OR
                   PROFILE-RECORD(1:6) = "EDU03:"
                  PERFORM PARSE-EDUCATION-LINE
          END-EVALUATE
       END-PERFORM.

       *> Parse a single experience entry line using tilde (~) as delimiter
       PARSE-EXPERIENCE-LINE.
       ADD 1 TO LS-EXP-COUNT                    *> Increment experience counter
       *> Split the line into components: title~company~dates~description
       UNSTRING PROFILE-RECORD(7:) DELIMITED BY "~"
          INTO LS-EXP-TITLE(LS-EXP-COUNT)
               LS-EXP-COMPANY(LS-EXP-COUNT)
               LS-EXP-DATES(LS-EXP-COUNT)
               LS-EXP-DESC(LS-EXP-COUNT)
       END-UNSTRING.

       *> Parse a single education entry line using tilde (~) as delimiter
       PARSE-EDUCATION-LINE.
       ADD 1 TO LS-EDU-COUNT                    *> Increment education counter
       *> Split the line into components: degree~university~years
       UNSTRING PROFILE-RECORD(7:) DELIMITED BY "~"
          INTO LS-EDU-DEGREE(LS-EDU-COUNT)
               LS-EDU-UNIV(LS-EDU-COUNT)
               LS-EDU-YEARS(LS-EDU-COUNT)
       END-UNSTRING.



