       >>SOURCE FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-MESSAGE.
       AUTHOR. Vamsi.
       DATE-WRITTEN. 11/10/2025.
      *
      *This module reads the messages.txt file and displays
      * all messages for the specified recipient.
      * It logs all output to InCollege-Output.txt.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.
           SELECT MESSAGES-FILE ASSIGN TO "messages.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MESSAGES-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD      PIC X(350).

       FD  MESSAGES-FILE.
       01  MESSAGE-RECORD     PIC X(500).

       WORKING-STORAGE SECTION.
       01  WS-OUTPUT-STATUS   PIC X(2).
       01  WS-MESSAGES-STATUS PIC X(2).
       01  WS-EOF-FLAG        PIC X VALUE 'N'.
      * This tracks if we find at least one message
       01  WS-MSG-FOUND-FLAG  PIC X VALUE 'N'.

      * Variables for parsing
       01  WS-STORED-LINE     PIC X(500).
       01  WS-SENDER          PIC X(20).
       01  WS-RECIPIENT       PIC X(20).
       01  WS-MESSAGE-CONTENT PIC X(200).
       01  WS-TIMESTAMP       PIC X(30).
       01  WS-REST-OF-LINE    PIC X(480).

      * Variable for logging output
       01  WS-DISPLAY-LINE    PIC X(350).

       LINKAGE SECTION.
      * The logged-in user, passed from the main program
       01  LS-CURRENT-USER    PIC X(20).
      * 'S' = Success (messages found), 'F' = No messages, 'X' = Error
       01  LS-RETURN-CODE     PIC X.

       PROCEDURE DIVISION USING LS-CURRENT-USER, LS-RETURN-CODE.
      * Default to error until a clear outcome
       MOVE 'X' TO LS-RETURN-CODE
       MOVE 'N' TO WS-MSG-FOUND-FLAG
       MOVE 'N' TO WS-EOF-FLAG
       MOVE FUNCTION TRIM(LS-CURRENT-USER) TO LS-CURRENT-USER.

      * Open the output file first to log all actions.
      * The main program must CLOSE this file
      * before CALLing and OPEN EXTEND it after.
       OPEN EXTEND OUTPUT-FILE
       IF WS-OUTPUT-STATUS NOT = "00"
           MOVE 'X' TO LS-RETURN-CODE
           GOBACK
       END-IF.

      * Try to open the messages file
       OPEN INPUT MESSAGES-FILE
       IF WS-MESSAGES-STATUS NOT = "00"
           IF WS-MESSAGES-STATUS = "35"
      * File not found = No messages exist yet
               MOVE "You have no messages at this time." TO WS-DISPLAY-LINE
               PERFORM DISPLAY-AND-LOG
               MOVE 'F' TO LS-RETURN-CODE
               CLOSE OUTPUT-FILE
               GOBACK
           ELSE
      * Other file error
               MOVE "Error accessing messages file." TO WS-DISPLAY-LINE
               PERFORM DISPLAY-AND-LOG
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE OUTPUT-FILE
               GOBACK
           END-IF
       END-IF.

      * File opened successfully, print header
       MOVE "Your Messages" TO WS-DISPLAY-LINE
       PERFORM DISPLAY-AND-LOG
       MOVE "---" TO WS-DISPLAY-LINE
       PERFORM DISPLAY-AND-LOG.

      * Read through the entire messages file
       PERFORM UNTIL WS-EOF-FLAG = 'Y'
           READ MESSAGES-FILE INTO WS-STORED-LINE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   PERFORM PROCESS-MESSAGE-RECORD
           END-READ
       END-PERFORM.

       CLOSE MESSAGES-FILE.

      * After loop, check if we ever found a message
       IF WS-MSG-FOUND-FLAG = 'Y'
           MOVE 'S' TO LS-RETURN-CODE
       ELSE
      * File was read, but no messages matched the user
           MOVE "You have no messages at this time." TO WS-DISPLAY-LINE
           PERFORM DISPLAY-AND-LOG
           MOVE 'F' TO LS-RETURN-CODE
       END-IF.

       CLOSE OUTPUT-FILE.
       GOBACK.

      * This paragraph parses one line from messages.txt
       PROCESS-MESSAGE-RECORD SECTION.
           INITIALIZE WS-SENDER, WS-RECIPIENT, WS-MESSAGE-CONTENT,
                      WS-TIMESTAMP, WS-REST-OF-LINE.

      * Parse the record based on the format from SEND-MESSAGE
      * Format: Sender:Recipient>>Message>>Timestamp
           UNSTRING WS-STORED-LINE DELIMITED BY ":"
               INTO WS-SENDER, WS-REST-OF-LINE
           END-UNSTRING.

           UNSTRING WS-REST-OF-LINE DELIMITED BY ">>"
               INTO WS-RECIPIENT, WS-MESSAGE-CONTENT, WS-TIMESTAMP
           END-UNSTRING.

      * Check if this message is for the current user
           IF FUNCTION TRIM(WS-RECIPIENT) = FUNCTION TRIM(LS-CURRENT-USER)
      * This is a message for them, set flag
               MOVE 'Y' TO WS-MSG-FOUND-FLAG

      * Display formatted message as per requirements
               MOVE SPACES TO WS-DISPLAY-LINE
               STRING "From: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-SENDER) DELIMITED BY SIZE
                      INTO WS-DISPLAY-LINE
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-DISPLAY-LINE
               STRING "Message: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-MESSAGE-CONTENT) DELIMITED BY SIZE
                      INTO WS-DISPLAY-LINE
               PERFORM DISPLAY-AND-LOG

      * Display timestamp if it exists
               IF WS-TIMESTAMP > SPACES
                   MOVE SPACES TO WS-DISPLAY-LINE
                   STRING "(Sent: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-TIMESTAMP) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                          INTO WS-DISPLAY-LINE
                   PERFORM DISPLAY-AND-LOG
               END-IF

      * Blank linebetween messages
               MOVE " " TO WS-DISPLAY-LINE
               PERFORM DISPLAY-AND-LOG
           END-IF.
           EXIT.

      * This needs its own log routine
      * to write to both screen and file.
       DISPLAY-AND-LOG SECTION.
           DISPLAY WS-DISPLAY-LINE.
           MOVE WS-DISPLAY-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           EXIT.