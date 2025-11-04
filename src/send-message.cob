       >>SOURCE FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEND-MESSAGE.
       AUTHOR. Copilot.
       DATE-WRITTEN. 11/03/2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCOUNTS-STATUS.

           SELECT CONNECTIONS-FILE ASSIGN TO "connections.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONNECTIONS-STATUS.

           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-INPUT-STATUS.

           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.

           SELECT MESSAGES-FILE ASSIGN TO "messages.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MESSAGES-STATUS.

       DATA DIVISION.
       FILE SECTION.
    FD  ACCOUNTS-FILE.
    01  ACCOUNTS-RECORD    PIC X(40).

       FD  CONNECTIONS-FILE.
       01  CONNECTION-RECORD  PIC X(100).

       FD  INPUT-FILE.
       01  INPUT-RECORD       PIC X(350).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD      PIC X(350).

       FD  MESSAGES-FILE.
       01  MESSAGE-RECORD     PIC X(500).

       WORKING-STORAGE SECTION.
    01  WS-ACCOUNTS-STATUS PIC X(2).
       01  WS-CONNECTIONS-STATUS PIC X(2).
       01  WS-INPUT-STATUS PIC X(2).
       01  WS-OUTPUT-STATUS PIC X(2).
       01  WS-MESSAGES-STATUS PIC X(2).

       01  WS-FOUND-FLAG      PIC X VALUE 'N'.
       01  WS-CONNECTED-FLAG  PIC X VALUE 'N'.
       01  WS-EOF-FLAG        PIC X VALUE 'N'.

       01  WS-FROM-USER       PIC X(20).
       01  WS-TO-USER         PIC X(20).
    01  WS-ACCT-USERNAME   PIC X(20).
       01  WS-STATUS-TXT      PIC X(20).

       01  WS-MESSAGE-TEXT    PIC X(200).
       01  WS-STORED-LINE     PIC X(500).
         01  WS-TIMESTAMP       PIC X(30).
     01  WS-CURR-DATE-TXT   PIC X(30).
    01  WS-HOUR-STR        PIC X(2).
    01  WS-MIN-STR         PIC X(2).
    01  WS-HOUR-N          PIC 99.
    01  WS-HOUR-ADJ        PIC 99.
    01  WS-HOUR-12         PIC 99.
    01  WS-HOUR-FMT        PIC XX.
    01  WS-AMPM            PIC XX.
    01  WS-TZ-OFFSET       PIC S9(3) VALUE -5.
    01  WS-LAST-LINE       PIC X(200).
    01  WS-MSG-FOUND       PIC X VALUE 'N'.
       01  WS-MESSAGE         PIC X(80).

       LINKAGE SECTION.
       01  LS-SENDER          PIC X(20).
       01  LS-RECIPIENT       PIC X(20).
       01  LS-RETURN-CODE     PIC X.

       PROCEDURE DIVISION USING LS-SENDER, LS-RECIPIENT, LS-RETURN-CODE.

       *> Default to failure until success conditions met
       MOVE 'F' TO LS-RETURN-CODE

       *> Trim inputs
       MOVE FUNCTION TRIM(LS-SENDER) TO LS-SENDER
       MOVE FUNCTION TRIM(LS-RECIPIENT) TO LS-RECIPIENT

       *> 1) Verify recipient exists in accounts.txt (username present)
       MOVE 'N' TO WS-FOUND-FLAG
       OPEN INPUT ACCOUNTS-FILE
       IF WS-ACCOUNTS-STATUS NOT = "00"
           IF WS-ACCOUNTS-STATUS = "35"
               MOVE "User does not exist." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'F' TO LS-RETURN-CODE
               CLOSE ACCOUNTS-FILE
               GOBACK
           ELSE
               MOVE "Error accessing accounts file." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE ACCOUNTS-FILE
               GOBACK
           END-IF
       END-IF

       MOVE 'N' TO WS-EOF-FLAG
       PERFORM UNTIL WS-EOF-FLAG = 'Y'
           READ ACCOUNTS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                  *> Extract the username token from the accounts record (accounts file may contain username and password)
                  UNSTRING ACCOUNTS-RECORD DELIMITED BY SPACE
                      INTO WS-ACCT-USERNAME
                  END-UNSTRING
                  IF FUNCTION TRIM(WS-ACCT-USERNAME) = FUNCTION TRIM(LS-RECIPIENT)
                       MOVE 'Y' TO WS-FOUND-FLAG
                       MOVE 'Y' TO WS-EOF-FLAG
                  END-IF
           END-READ
       END-PERFORM
       CLOSE ACCOUNTS-FILE

       IF WS-FOUND-FLAG NOT = 'Y'
           MOVE "User does not exist." TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE 'F' TO LS-RETURN-CODE
           GOBACK
       END-IF

       *> 2) Verify sender and recipient are connected in connections.txt
       MOVE 'N' TO WS-CONNECTED-FLAG
       OPEN INPUT CONNECTIONS-FILE
       IF WS-CONNECTIONS-STATUS NOT = "00"
           IF WS-CONNECTIONS-STATUS = "35"
               MOVE "You can only message users you are connected with." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'F' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               GOBACK
           ELSE
               MOVE "Error accessing connections file." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               GOBACK
           END-IF
       END-IF

       MOVE 'N' TO WS-EOF-FLAG
       PERFORM UNTIL WS-EOF-FLAG = 'Y'
           READ CONNECTIONS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   UNSTRING CONNECTION-RECORD DELIMITED BY ":"
                       INTO WS-FROM-USER, WS-TO-USER, WS-STATUS-TXT
                   IF ((FUNCTION TRIM(WS-FROM-USER) = FUNCTION TRIM(LS-SENDER)
                        AND FUNCTION TRIM(WS-TO-USER) = FUNCTION TRIM(LS-RECIPIENT))
                       OR (FUNCTION TRIM(WS-FROM-USER) = FUNCTION TRIM(LS-RECIPIENT)
                           AND FUNCTION TRIM(WS-TO-USER) = FUNCTION TRIM(LS-SENDER)))
                       AND FUNCTION TRIM(WS-STATUS-TXT) = "CONNECTED"
                       MOVE 'Y' TO WS-CONNECTED-FLAG
                       MOVE 'Y' TO WS-EOF-FLAG
                   END-IF
           END-READ
       END-PERFORM
       CLOSE CONNECTIONS-FILE

       IF WS-CONNECTED-FLAG NOT = 'Y'
           MOVE "You can only message users you are connected with." TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG
           MOVE 'F' TO LS-RETURN-CODE
           GOBACK
       END-IF

        *> 3) Prompt for message content and read from input file
        MOVE "Enter your message (max 200 chars):" TO WS-MESSAGE
        PERFORM DISPLAY-AND-LOG

        *> Read the input file and find the recipient's name; the next line after the recipient is the message
        OPEN INPUT INPUT-FILE
        IF WS-INPUT-STATUS NOT = "00"
            IF WS-INPUT-STATUS = "35"
                MOVE "No input available for message." TO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE 'F' TO LS-RETURN-CODE
                CLOSE INPUT-FILE
                GOBACK
            ELSE
                MOVE "Error opening input file." TO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE 'X' TO LS-RETURN-CODE
                CLOSE INPUT-FILE
                GOBACK
            END-IF
        END-IF

        MOVE 'N' TO WS-EOF-FLAG
        MOVE 'N' TO WS-MSG-FOUND
        MOVE SPACES TO WS-LAST-LINE
        PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-MSG-FOUND = 'Y'
            READ INPUT-FILE
                AT END
                    MOVE 'Y' TO WS-EOF-FLAG
                NOT AT END
                    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-LAST-LINE
                    IF FUNCTION TRIM(WS-LAST-LINE) = FUNCTION TRIM(LS-RECIPIENT)
                        *> Next line should be the actual message
                        READ INPUT-FILE
                            AT END
                                MOVE "No message provided." TO WS-MESSAGE
                                PERFORM DISPLAY-AND-LOG
                                MOVE 'F' TO LS-RETURN-CODE
                                MOVE 'Y' TO WS-EOF-FLAG
                                MOVE 'Y' TO WS-MSG-FOUND
                            NOT AT END
                                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MESSAGE-TEXT
                                MOVE 'Y' TO WS-MSG-FOUND
                        END-READ
                    END-IF
            END-READ
        END-PERFORM
        CLOSE INPUT-FILE

        IF WS-MSG-FOUND NOT = 'Y'
            MOVE "No message provided." TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG
            MOVE 'F' TO LS-RETURN-CODE
            GOBACK
        END-IF

        *> Enforce max 200 chars
        IF FUNCTION LENGTH(WS-MESSAGE-TEXT) > 200
            MOVE WS-MESSAGE-TEXT(1:200) TO WS-MESSAGE-TEXT
        END-IF

       *> 4) Persist message to messages.txt as Sender:Recipient>>Message>>Timestamp
       *> Ensure messages file exists and append
       OPEN EXTEND MESSAGES-FILE
       IF WS-MESSAGES-STATUS NOT = "00"
           IF WS-MESSAGES-STATUS = "35"
               *> create file then reopen for append
               OPEN OUTPUT MESSAGES-FILE
               CLOSE MESSAGES-FILE
               OPEN EXTEND MESSAGES-FILE
           ELSE
               MOVE "Error accessing messages file." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'X' TO LS-RETURN-CODE
               GOBACK
           END-IF
       END-IF

     *> Create formatted timestamp (h:mm am/pm-MM/DD)
     MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-TXT
     *> Extract hour and minute strings
     MOVE WS-CURR-DATE-TXT(9:2) TO WS-HOUR-STR
     MOVE WS-CURR-DATE-TXT(11:2) TO WS-MIN-STR
     *> Convert hour to numeric and apply timezone offset (WS-TZ-OFFSET)
     MOVE FUNCTION NUMVAL(WS-HOUR-STR) TO WS-HOUR-N
     COMPUTE WS-HOUR-ADJ = WS-HOUR-N + WS-TZ-OFFSET
     IF WS-HOUR-ADJ <= 0
         ADD 24 TO WS-HOUR-ADJ
     END-IF
     IF WS-HOUR-ADJ >= 24
         SUBTRACT 24 FROM WS-HOUR-ADJ
     END-IF
     *> Compute 12-hour display and am/pm from adjusted hour
     IF WS-HOUR-ADJ = 0
         MOVE 12 TO WS-HOUR-12
         MOVE "am" TO WS-AMPM
     ELSE IF WS-HOUR-ADJ = 12
         MOVE 12 TO WS-HOUR-12
         MOVE "pm" TO WS-AMPM
     ELSE IF WS-HOUR-ADJ > 12
         COMPUTE WS-HOUR-12 = WS-HOUR-ADJ - 12
         MOVE "pm" TO WS-AMPM
     ELSE
         MOVE WS-HOUR-ADJ TO WS-HOUR-12
         MOVE "am" TO WS-AMPM
     END-IF
     *> Prepare hour for display (no leading zero)
     MOVE WS-HOUR-12 TO WS-HOUR-FMT
     *> Build timestamp: h:mm am/pm-MM/DD
     STRING FUNCTION TRIM(WS-HOUR-FMT) ":" WS-MIN-STR " " WS-AMPM "-"
            WS-CURR-DATE-TXT(5:2) "/" WS-CURR-DATE-TXT(7:2)
            INTO WS-TIMESTAMP

       INITIALIZE WS-STORED-LINE
       STRING FUNCTION TRIM(LS-SENDER) DELIMITED BY SIZE
              ":" DELIMITED BY SIZE
              FUNCTION TRIM(LS-RECIPIENT) DELIMITED BY SIZE
              ">>" DELIMITED BY SIZE
              FUNCTION TRIM(WS-MESSAGE-TEXT) DELIMITED BY SIZE
              ">>" DELIMITED BY SIZE
              WS-TIMESTAMP DELIMITED BY SIZE
              INTO WS-STORED-LINE

       MOVE WS-STORED-LINE TO MESSAGE-RECORD
       WRITE MESSAGE-RECORD
       CLOSE MESSAGES-FILE

       *> 5) Confirm to user
       INITIALIZE WS-MESSAGE
       STRING "Message sent to " FUNCTION TRIM(LS-RECIPIENT) " successfully!" DELIMITED BY SIZE
           INTO WS-MESSAGE
       PERFORM DISPLAY-AND-LOG

       MOVE 'S' TO LS-RETURN-CODE
       GOBACK.

       DISPLAY-AND-LOG SECTION.
           DISPLAY WS-MESSAGE.
           MOVE WS-MESSAGE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           EXIT.
