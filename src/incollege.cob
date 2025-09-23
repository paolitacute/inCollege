>>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.
AUTHOR. Kaden and Paola
DATE-WRITTEN. 09/07/2025

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
    FILE-CONTROL.

        *> Select INPUT-FILE tells COBOL what the input file is
        SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
        *> LINE SEQUENTIAL means each line in text is a record
            ORGANIZATION IS LINE SEQUENTIAL.
        *> OUTPUT-FILE defines what file will have the output stored
        SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
            ORGANIZATION IS LINE SEQUENTIAL.

        SELECT ACCOUNTS-FILE ASSIGN TO "accounts.txt"
            ORGANIZATION IS LINE SEQUENTIAL
            FILE STATUS IS WS-ACCOUNTS-STATUS.

DATA DIVISION.
FILE SECTION.
    *> FD describes the structure of the INPUT-FILE
    FD  INPUT-FILE.
    *> Defines each record as a 80 charecter line of text
    01  INPUT-RECORD      PIC X(350).

    *> FD describes the structure of the OUTPUT-FILE
    FD  OUTPUT-FILE.

    *> Defines each record as a 80 charecter line of text
    01  OUTPUT-RECORD     PIC X(350).


    FD  ACCOUNTS-FILE.
    01  ACCOUNTS-RECORD-DATA.
        05  ACCOUNTS-USERNAME    PIC X(20).
        05  ACCOUNTS-PASSWORD    PIC X(20).

    *> Working storeage section is where the variables of the program are stored
WORKING-STORAGE SECTION.

    *> Used to hold a line of text before displaying it
    01  WS-MESSAGE        PIC X(80).
    01  WS-TEMP           PIC X(80).

    *> Stores the entered username
    01  WS-USERNAME       PIC X(20).
    01  WS-VIEW-USER      PIC X(20).

    *> Stores the entered password
    01  WS-PASSWORD       PIC X(20).

    *> Single charecter
    01  WS-CHOICE         PIC 9(1).

    *> Bounds of the choices options
    01  MIN-VALUE-CHOICE       PIC 9(1).
    01  MAX-VALUE-CHOICE       PIC 9(1).

    *> Flag for end of file to then exit program
    01  WS-END-FILE       PIC X VALUE "N".
    01  WS-EXIT-FLAG      PIC X VALUE 'N'.
    01  WS-LOOP-FLAG      PIC X.

    01  WS-RETURN-CODE     PIC X.
    01  WS-RETURN-USER     PIC X(20).
    01  WS-ACCOUNTS-STATUS PIC X(2).
    01  WS-INPUT-BUFFER    PIC X(80).
    01  WS-TRIGGER         PIC X VALUE "0".

    01  WS-PROFILE-DATA.
        05 WS-FIRST-NAME     PIC X(50).
        05 WS-LAST-NAME      PIC X(50).
        05 WS-UNIVERSITY     PIC X(100).
        05 WS-MAJOR          PIC X(50).
        05 WS-GRAD-YEAR      PIC X(4).
        05 WS-ABOUT-ME       PIC X(200).
        05 WS-EXPERIENCE-TABLE.
           10 WS-EXPERIENCE OCCURS 3 TIMES INDEXED BY I.
              15 WS-EXP-TITLE    PIC X(50).
              15 WS-EXP-COMPANY  PIC X(50).
              15 WS-EXP-DATES    PIC X(50).
              15 WS-EXP-DESC     PIC X(100).
        05 WS-EXP-COUNT      PIC 9.
        05 WS-EDUCATION-TABLE.
           10 WS-EDUCATION OCCURS 3 TIMES INDEXED BY J.
              15 WS-EDU-DEGREE   PIC X(50).
              15 WS-EDU-UNIV     PIC X(50).
              15 WS-EDU-YEARS    PIC X(50).
        05 WS-EDU-COUNT      PIC 9.

    01  WS-VALIDATION-VARS.
        05 WS-CURRENT-YEAR      PIC 9(4) VALUE 2025.
        05 WS-MIN-GRAD-YEAR     PIC 9(4).
        05 WS-MAX-GRAD-YEAR     PIC 9(4).
        05 WS-GRAD-YEAR-NUM     PIC 9(4).
        05 WS-EXP-DISPLAY-NUM   PIC 9.
        05 WS-EDU-DISPLAY-NUM   PIC 9.

PROCEDURE DIVISION.
    COMPUTE WS-MIN-GRAD-YEAR = WS-CURRENT-YEAR - 2.
    COMPUTE WS-MAX-GRAD-YEAR = WS-CURRENT-YEAR + 10.

    OPEN INPUT INPUT-FILE.
    OPEN OUTPUT OUTPUT-FILE.

    PERFORM WELCOME-SCREEN.

    IF WS-END-FILE = 'N' AND WS-RETURN-CODE NOT = 'L'
       AND WS-RETURN-CODE NOT = 'E' AND WS-RETURN-CODE NOT = 'F'
        PERFORM MAIN-MENU-LOOP UNTIL WS-EXIT-FLAG = 'Y'
    END-IF.

    CLOSE INPUT-FILE, OUTPUT-FILE.
    STOP RUN.

WELCOME-SCREEN SECTION.
    *> Storing welcome messages into variables
    MOVE "Welcome to InCollege!" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "1. Log In" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "2. Create New Account" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.

    MOVE 1 TO MIN-VALUE-CHOICE.
    MOVE 2 TO MAX-VALUE-CHOICE.

    PERFORM CHOICE.

    EVALUATE WS-CHOICE
        WHEN 1
            PERFORM LOGIN-FLOW
        WHEN 2
            PERFORM CREATE-ACCOUNT-FLOW
    END-EVALUATE.

    IF WS-RETURN-CODE = 'S'
        INITIALIZE WS-MESSAGE
        STRING 'Welcome, ' DELIMITED BY SIZE
                FUNCTION TRIM(WS-USERNAME) DELIMITED BY SPACE
                '!' DELIMITED BY SIZE
                INTO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG
    END-IF.
    EXIT.

MAIN-MENU-LOOP SECTION.
    INITIALIZE WS-RETURN-CODE *>clean up for each menu operation
    PERFORM DISPLAY-MAIN-MENU.
    PERFORM PROCESS-MAIN-MENU-CHOICE.
    EXIT.

DISPLAY-MAIN-MENU SECTION.
    MOVE "1. Create/Edit My Profile" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "2. View My Profile" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "3. Search for a job" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "4. Find someone you know" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "5. Learn a new skill" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "6. Exit" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.

    MOVE 1 TO MIN-VALUE-CHOICE.
    MOVE 6 TO MAX-VALUE-CHOICE.

    PERFORM CHOICE.
    EXIT.

PROCESS-MAIN-MENU-CHOICE SECTION.
    EVALUATE WS-CHOICE
        WHEN 1
            PERFORM PROFILE-CREATION-FLOW
        WHEN 2
            PERFORM VIEW-PROFILE
        WHEN 3
            PERFORM SEARCH-JOB
        WHEN 4
            PERFORM FIND-SOMEONE
        WHEN 5
            PERFORM LEARN-SKILL
        WHEN 6
            MOVE 'Y' TO WS-EXIT-FLAG
            MOVE "You quit successfully." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG

            CLOSE INPUT-FILE, OUTPUT-FILE
            STOP RUN
    END-EVALUATE.
    EXIT.


VIEW-PROFILE SECTION.
    IF WS-TRIGGER = '0'
       MOVE "---Your Profile---" TO WS-MESSAGE
       PERFORM DISPLAY-AND-LOG
       MOVE WS-USERNAME TO WS-VIEW-USER
    END-IF
    IF WS-TRIGGER = '1'
       MOVE "---Found User Profile---" TO WS-MESSAGE
       PERFORM DISPLAY-AND-LOG
       MOVE WS-RETURN-USER TO WS-VIEW-USER
    END-IF
    CLOSE OUTPUT-FILE
    CALL "VIEW-PROFILE" USING WS-VIEW-USER, WS-PROFILE-DATA, WS-RETURN-CODE.

    OPEN EXTEND OUTPUT-FILE
    EVALUATE WS-RETURN-CODE
        WHEN 'S'
            *> Profile displayed successfully - no additional message needed
            CONTINUE
        WHEN 'F'
            MOVE "No profile found for this user." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
        WHEN 'X'
            MOVE "Error accessing profile file." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
        WHEN OTHER
            MOVE "Unknown error occurred while viewing profile." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
    END-EVALUATE.

    EXIT.


PROFILE-CREATION-FLOW SECTION.

    INITIALIZE WS-PROFILE-DATA.
    MOVE "--- Create/Edit Profile ---" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.

    *> Get Required Data: First Name
    MOVE "Enter First Name:" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    PERFORM GET-REQUIRED-INPUT.
    IF WS-END-FILE = 'Y'
        PERFORM CLOSE-PROGRAM
    END-IF.
    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-FIRST-NAME.


    *> Get Required Data: Last Name
    MOVE "Enter Last Name:" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    PERFORM GET-REQUIRED-INPUT.
    IF WS-END-FILE = 'Y'
        PERFORM CLOSE-PROGRAM
    END-IF.
    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-LAST-NAME.

    *> Get Required Data: University
    MOVE "Enter University/College Attended:" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    PERFORM GET-REQUIRED-INPUT.
    IF WS-END-FILE = 'Y'
        PERFORM CLOSE-PROGRAM
    END-IF.
    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-UNIVERSITY.

    *> Get Required Data: Major
    MOVE "Enter Major:" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    PERFORM GET-REQUIRED-INPUT.
    IF WS-END-FILE = 'Y'
        PERFORM CLOSE-PROGRAM
    END-IF.
    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MAJOR.

    *> Get Required Data: Graduation Year
    MOVE "Enter Graduation Year (YYYY):" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    PERFORM READ-FROM-INPUT-FILE.

    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-GRAD-YEAR.
    IF WS-GRAD-YEAR IS NUMERIC
        MOVE WS-GRAD-YEAR TO WS-GRAD-YEAR-NUM
    ELSE
        MOVE 0 TO WS-GRAD-YEAR-NUM
    END-IF.

    PERFORM UNTIL FUNCTION TRIM(INPUT-RECORD) > SPACES
              AND WS-GRAD-YEAR IS NUMERIC
              AND FUNCTION LENGTH(WS-GRAD-YEAR) = 4
              AND WS-GRAD-YEAR-NUM >= WS-MIN-GRAD-YEAR
              AND WS-GRAD-YEAR-NUM <= WS-MAX-GRAD-YEAR

        INITIALIZE WS-MESSAGE
        STRING "Invalid year. Enter a year between "
               WS-MIN-GRAD-YEAR DELIMITED BY SIZE
               " and " DELIMITED BY SIZE
               WS-MAX-GRAD-YEAR DELIMITED BY SIZE
               "." DELIMITED BY SIZE
               INTO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG

        MOVE "Enter Graduation Year (YYYY):" TO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG
        PERFORM READ-FROM-INPUT-FILE

        MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-GRAD-YEAR
        IF WS-GRAD-YEAR IS NUMERIC
            MOVE WS-GRAD-YEAR TO WS-GRAD-YEAR-NUM
        ELSE
            MOVE 0 TO WS-GRAD-YEAR-NUM
        END-IF
    END-PERFORM.

    *> Get Optional About Me
    MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):"
        TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    PERFORM READ-FROM-INPUT-FILE.
    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-ABOUT-ME.

    *> Get Experience
    MOVE 'N' TO WS-LOOP-FLAG.
    SET I TO 1.
    MOVE 0 TO WS-EXP-COUNT.
    PERFORM 3 TIMES
        IF WS-LOOP-FLAG = 'N'
            ADD 1 TO WS-EXP-COUNT
            MOVE WS-EXP-COUNT TO WS-EXP-DISPLAY-NUM

            INITIALIZE WS-MESSAGE
            MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            PERFORM READ-FROM-INPUT-FILE
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-INPUT-BUFFER

            IF WS-INPUT-BUFFER = "DONE" OR WS-INPUT-BUFFER = " "
                MOVE 'Y' TO WS-LOOP-FLAG
                SUBTRACT 1 FROM WS-EXP-COUNT
            ELSE
                INITIALIZE WS-MESSAGE
                STRING "Experience #" WS-EXP-DISPLAY-NUM
                       " - Title:"
                       DELIMITED BY SIZE INTO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(WS-INPUT-BUFFER) TO WS-EXP-TITLE(I)

                INITIALIZE WS-MESSAGE
                PERFORM READ-FROM-INPUT-FILE
                STRING "Experience #" WS-EXP-DISPLAY-NUM
                       " - Company/Organization:"
                       DELIMITED BY SIZE INTO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-EXP-COMPANY(I)

                INITIALIZE WS-MESSAGE
                PERFORM READ-FROM-INPUT-FILE
                STRING "Experience #" WS-EXP-DISPLAY-NUM
                       " - Dates (e.g., Summer 2024):"
                       DELIMITED BY SIZE INTO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-EXP-DATES(I)

                INITIALIZE WS-MESSAGE
                PERFORM READ-FROM-INPUT-FILE
                STRING "Experience #" WS-EXP-DISPLAY-NUM
                     " - Description (optional, blank to skip):"
                     DELIMITED BY SIZE INTO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-EXP-DESC(I)
                SET I UP BY 1
            END-IF
        END-IF
    END-PERFORM.

    *> Get Education
    MOVE 'N' TO WS-LOOP-FLAG.
    SET J TO 1.
    MOVE 0 TO WS-EDU-COUNT.
    PERFORM 3 TIMES
        IF WS-LOOP-FLAG = 'N'
            ADD 1 TO WS-EDU-COUNT
            MOVE WS-EDU-COUNT TO WS-EDU-DISPLAY-NUM

            INITIALIZE WS-MESSAGE
            MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            PERFORM READ-FROM-INPUT-FILE
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-INPUT-BUFFER

            IF WS-INPUT-BUFFER = "DONE" OR WS-INPUT-BUFFER = " "
                MOVE 'Y' TO WS-LOOP-FLAG
                SUBTRACT 1 FROM WS-EDU-COUNT
            ELSE

                INITIALIZE WS-MESSAGE
                STRING "Education #" WS-EDU-DISPLAY-NUM
                       " - Degree:"
                       DELIMITED BY SIZE INTO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(WS-INPUT-BUFFER) TO WS-EDU-DEGREE(J)


                INITIALIZE WS-MESSAGE
                PERFORM READ-FROM-INPUT-FILE
                STRING "Education #" WS-EDU-DISPLAY-NUM
                       " - University/College:"
                       DELIMITED BY SIZE INTO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-EDU-UNIV(J)


                INITIALIZE WS-MESSAGE
                PERFORM READ-FROM-INPUT-FILE
                STRING "Education #" WS-EDU-DISPLAY-NUM
                       " - Years Attended (e.g., 2023-2025):"
                       DELIMITED BY SIZE INTO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-EDU-YEARS(J)

                SET J UP BY 1
            END-IF
        END-IF
    END-PERFORM.

    CALL "CREATE-PROFILE" USING WS-USERNAME, WS-PROFILE-DATA, WS-RETURN-CODE.

    EVALUATE WS-RETURN-CODE
        WHEN 'S'
            MOVE "Profile saved successfully!" TO WS-MESSAGE
        WHEN 'F'
            MOVE "Invalid data (Graduation Year). Profile not saved."
                TO WS-MESSAGE
        WHEN 'E'
            MOVE "Error occurred while saving profile." TO WS-MESSAGE
        WHEN OTHER
            MOVE "Unknown error saving profile." TO WS-MESSAGE
    END-EVALUATE.
    PERFORM DISPLAY-AND-LOG.
    EXIT.


CREATE-ACCOUNT-FLOW SECTION.

    MOVE "Enter username:" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.

    PERFORM READ-FROM-INPUT-FILE.

    IF WS-END-FILE = 'N'
        MOVE INPUT-RECORD TO WS-USERNAME
    END-IF.

    MOVE "Enter password:" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.

    PERFORM READ-FROM-INPUT-FILE.

    IF WS-END-FILE = 'N'
        MOVE INPUT-RECORD TO WS-PASSWORD
    END-IF.

    CALL "CREATE-ACCOUNT" USING WS-USERNAME, WS-PASSWORD, WS-RETURN-CODE.

    EVALUATE WS-RETURN-CODE
        WHEN 'S'
            MOVE "Account created successfully." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
        WHEN 'L'
            MOVE "All permitted accounts have been created." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            CLOSE INPUT-FILE, OUTPUT-FILE
            STOP RUN
        WHEN 'E'
            MOVE "Username already exists." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            CLOSE INPUT-FILE, OUTPUT-FILE
            STOP RUN
        WHEN 'F'
            MOVE "Invalid password format." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            CLOSE INPUT-FILE, OUTPUT-FILE
            STOP RUN
        WHEN OTHER
            MOVE "An unknown error occurred." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            CLOSE INPUT-FILE, OUTPUT-FILE
            STOP RUN
    END-EVALUATE.
    EXIT.

LOGIN-FLOW SECTION.

    PERFORM UNTIL WS-RETURN-CODE = 'S'
        MOVE "Enter username:" TO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG
        PERFORM READ-FROM-INPUT-FILE

        IF WS-END-FILE = 'N'
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-USERNAME
        END-IF

        MOVE "Enter password:" TO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG
        PERFORM READ-from-input-file

        IF WS-END-FILE = 'N'
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PASSWORD
        END-IF

        CALL "LOGIN" USING WS-USERNAME, WS-PASSWORD, WS-RETURN-CODE

        EVALUATE WS-RETURN-CODE
            WHEN 'S'
                MOVE "Login successful" TO WS-MESSAGE
            WHEN 'F'
                MOVE "Incorrect username/password. Please try again." TO WS-MESSAGE
            WHEN 'X'
                MOVE "Error accessing accounts file." TO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                CLOSE INPUT-FILE, OUTPUT-FILE
                STOP RUN
            WHEN OTHER
                MOVE "An unknown error occurred." TO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                CLOSE INPUT-FILE, OUTPUT-FILE
                STOP RUN
        END-EVALUATE

        PERFORM DISPLAY-AND-LOG
    END-PERFORM

    EXIT.


SEARCH-JOB SECTION.
    MOVE "Job search/internship is under construction." TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    EXIT.


FIND-SOMEONE SECTION.
    *> Initialize search loop control variable
    MOVE 'N' TO WS-LOOP-FLAG
    
    *> Continue searching until user chooses to stop
    PERFORM UNTIL WS-LOOP-FLAG = 'Y'
      *> Clear any previous name data before starting new search
      INITIALIZE WS-FIRST-NAME
      INITIALIZE WS-LAST-NAME
    
      *> Display the prompt to user first
      MOVE "Enter the full name of the person you are looking for:" TO WS-MESSAGE
      PERFORM DISPLAY-AND-LOG
    
      PERFORM READ-FROM-INPUT-FILE
      IF WS-END-FILE ='Y'
          PERFORM CLOSE-PROGRAM
      END-IF
    
      *> Process the input only if we successfully read from file
      IF WS-END-FILE = 'N'
          MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TEMP
    
          *> Check if user entered a blank line (no name provided)
          IF WS-TEMP = SPACES
              *> Show error message for blank input and continue loop
              MOVE "Please enter a name. Try again:" TO WS-MESSAGE
              PERFORM DISPLAY-AND-LOG
          ELSE
              UNSTRING WS-TEMP
                  DELIMITED BY ALL SPACE
                  INTO WS-FIRST-NAME
                       WS-LAST-NAME
              MOVE FUNCTION TRIM(WS-FIRST-NAME) TO WS-FIRST-NAME
              MOVE FUNCTION TRIM(WS-LAST-NAME) TO WS-LAST-NAME
    
              CALL "SEARCH" USING WS-FIRST-NAME, WS-LAST-NAME, WS-PROFILE-DATA, WS-RETURN-CODE, WS-RETURN-USER
    
              EVALUATE WS-RETURN-CODE
                   WHEN 'T'
                       MOVE "1" TO WS-TRIGGER
                       PERFORM VIEW-PROFILE
                       MOVE "0" TO WS-TRIGGER
                       MOVE 'Y' TO WS-LOOP-FLAG
                   WHEN 'F'
                       MOVE "This user profile does not exist, Try again:" TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                   WHEN 'X'
                       MOVE "Error accessing accounts file." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                       CLOSE INPUT-FILE, OUTPUT-FILE
                       STOP RUN
                   WHEN OTHER
                       MOVE "An unknown error occurred." TO WS-MESSAGE
                       PERFORM DISPLAY-AND-LOG
                       CLOSE INPUT-FILE, OUTPUT-FILE
                       STOP RUN
               END-EVALUATE
          END-IF
      END-IF
    END-PERFORM

   *> Reset return code so it doesn't interfere with main menu
   INITIALIZE WS-RETURN-CODE
   EXIT.

LEARN-SKILL SECTION.
    MOVE "Learn a New Skill:" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.

    MOVE "1. Skill 1" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "2. Skill 2" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "3. Skill 3" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "4. Skill 4" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "5. Skill 5" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.
    MOVE "6. Go Back" TO WS-MESSAGE.
    PERFORM DISPLAY-AND-LOG.

    MOVE 1 TO MIN-VALUE-CHOICE.
    MOVE 6 TO MAX-VALUE-CHOICE.

    PERFORM CHOICE.

    EVALUATE WS-CHOICE
        WHEN 1
            MOVE "This skill is under construction." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            PERFORM LEARN-SKILL
        WHEN 2
            MOVE "This skill is under construction." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            PERFORM LEARN-SKILL
        WHEN 3
            MOVE "This skill is under construction." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            PERFORM LEARN-SKILL
        WHEN 4
            MOVE "This skill is under construction." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            PERFORM LEARN-SKILL
        WHEN 5
            MOVE "This skill is under construction." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            PERFORM LEARN-SKILL
        WHEN 6
            EXIT SECTION
    END-EVALUATE.
    EXIT.


CHOICE SECTION.

    INITIALIZE WS-CHOICE.

    MOVE "Enter your choice as a number:" TO WS-MESSAGE.

    PERFORM DISPLAY-AND-LOG.

    PERFORM READ-FROM-INPUT-FILE.

    IF WS-END-FILE = 'N'
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 1 AND
                       FUNCTION TRIM(INPUT-RECORD) IS NUMERIC
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-CHOICE
        ELSE
            MOVE 0 TO WS-CHOICE *> Force invalid choice
        END-IF
    END-IF.

    IF WS-END-FILE = 'N'
        PERFORM UNTIL (WS-CHOICE >= MIN-VALUE-CHOICE)
                       AND (WS-CHOICE <= MAX-VALUE-CHOICE)

            MOVE "Not a valid choice. Try again." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG

            PERFORM READ-FROM-INPUT-FILE

            IF WS-END-FILE = 'N'
                IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 1 AND
                       FUNCTION TRIM(INPUT-RECORD) IS NUMERIC
                    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-CHOICE
                ELSE
                    MOVE 0 TO WS-CHOICE *> Force invalid choice
                END-IF
            END-IF

        END-PERFORM
    END-IF.

    IF WS-END-FILE = 'Y'
        MOVE "You quit successfully." TO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG

        CLOSE INPUT-FILE, OUTPUT-FILE
        STOP RUN
    END-IF.
    EXIT.


GET-REQUIRED-INPUT SECTION.
    PERFORM READ-FROM-INPUT-FILE.
    PERFORM UNTIL WS-END-FILE = 'Y' OR
                  FUNCTION TRIM(INPUT-RECORD) > SPACES
        MOVE "Input cannot be blank. Please provide a value."
            TO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG
        PERFORM READ-FROM-INPUT-FILE
    END-PERFORM.
    EXIT.



DISPLAY-AND-LOG SECTION.
    DISPLAY WS-MESSAGE.
    MOVE WS-MESSAGE TO OUTPUT-RECORD.
    WRITE OUTPUT-RECORD.
    EXIT.

READ-FROM-INPUT-FILE SECTION.
    IF WS-END-FILE = 'Y'
        PERFORM CLOSE-PROGRAM
    END-IF.

    READ INPUT-FILE
        AT END
            MOVE 'Y' TO WS-END-FILE
        NOT AT END
            MOVE 'N' TO WS-END-FILE
    END-READ.

    EXIT.

CLOSE-PROGRAM SECTION.
       IF WS-END-FILE = 'Y'
           MOVE "Inactivity. You quit successfully." TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           CLOSE INPUT-FILE, OUTPUT-FILE
           STOP RUN
       END-IF.


