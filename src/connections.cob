       >>SOURCE FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECTIONS.
       AUTHOR. InCollege Team.
       DATE-WRITTEN. 09/28/2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONNECTIONS-FILE ASSIGN TO "connections.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONNECTIONS-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CONNECTIONS-FILE.
       01  CONNECTION-RECORD    PIC X(100).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD        PIC X(350).

       WORKING-STORAGE SECTION.
       01  WS-CONNECTIONS-STATUS PIC X(2).
       01  WS-EOF-FLAG          PIC X VALUE 'N'.
       01  WS-FROM-USER         PIC X(20).
       01  WS-TO-USER           PIC X(20).
       01  WS-STATUS            PIC X(10).
       01  WS-MESSAGE           PIC X(80).
       01  WS-PENDING-COUNT     PIC 9(3) VALUE 0.

       01  I                    PIC 99.
       01  WS-TEMP-TABLE.
           *> Define a table to hold records for file manipulation
           05 WS-TEMP-RECORD OCCURS 200 TIMES PIC X(100).

       01  WS-ALREADY-CONNECTED PIC X VALUE 'N'.
       01  WS-PENDING-EXISTS    PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  LS-ACTION            PIC X(10).
       01  LS-USERNAME          PIC X(20).
       01  LS-TARGET-USERNAME   PIC X(20).
       01  LS-RETURN-CODE       PIC X.

       PROCEDURE DIVISION USING LS-ACTION, LS-USERNAME, LS-TARGET-USERNAME, LS-RETURN-CODE.

           MOVE 'S' TO LS-RETURN-CODE.

           EVALUATE FUNCTION TRIM(LS-ACTION)
               WHEN "SEND"
                   PERFORM SEND-CONNECTION-REQUEST
               WHEN "VIEW"
                   PERFORM VIEW-PENDING-CONNECTIONS
               WHEN "ACCEPT"
                   PERFORM ACCEPT-CONNECTION-REQUEST
               WHEN OTHER
                   MOVE 'E' TO LS-RETURN-CODE
           END-EVALUATE.

           GOBACK.

       SEND-CONNECTION-REQUEST SECTION.
           MOVE 'N' TO WS-ALREADY-CONNECTED.
           MOVE 'N' TO WS-PENDING-EXISTS.

           *> First, check if connection already exists or if there's a pending request
           PERFORM CHECK-EXISTING-CONNECTIONS.

           IF LS-RETURN-CODE NOT = 'S'
               EXIT SECTION
           END-IF.

           *> If validation passes, add the new connection request
           PERFORM ADD-CONNECTION-REQUEST.
           EXIT.

       CHECK-EXISTING-CONNECTIONS SECTION.
           OPEN INPUT CONNECTIONS-FILE.

           *> If file doesn't exist, that's okay - we'll create it later
           IF WS-CONNECTIONS-STATUS = "35"
               CLOSE CONNECTIONS-FILE
               EXIT PARAGRAPH
           END-IF.

           IF WS-CONNECTIONS-STATUS NOT = "00"
               DISPLAY "Error opening connections file for validation."
               DISPLAY "File Status: " WS-CONNECTIONS-STATUS
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               EXIT PARAGRAPH
           END-IF.

           MOVE 'N' TO WS-EOF-FLAG.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CONNECTIONS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       *> Parse the connection record: FROM_USER:TO_USER:STATUS
                       UNSTRING CONNECTION-RECORD DELIMITED BY ":"
                           INTO WS-FROM-USER, WS-TO-USER, WS-STATUS

                       *> Trim spaces from parsed fields
                       MOVE FUNCTION TRIM(WS-FROM-USER) TO WS-FROM-USER
                       MOVE FUNCTION TRIM(WS-TO-USER) TO WS-TO-USER
                       MOVE FUNCTION TRIM(WS-STATUS) TO WS-STATUS

                       *> Check if users are already connected
                       IF ((WS-FROM-USER = LS-USERNAME AND
                            WS-TO-USER = LS-TARGET-USERNAME) OR
                           (WS-FROM-USER = LS-TARGET-USERNAME AND
                            WS-TO-USER = LS-USERNAME))
                            AND WS-STATUS = "CONNECTED"
                           MOVE 'Y' TO WS-ALREADY-CONNECTED
                           MOVE 'C' TO LS-RETURN-CODE
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF

                       *> Check if target user already sent a request to sender
                       IF WS-FROM-USER = LS-TARGET-USERNAME AND
                          WS-TO-USER = LS-USERNAME AND
                          WS-STATUS = "PENDING"
                           MOVE 'Y' TO WS-PENDING-EXISTS
                           MOVE 'P' TO LS-RETURN-CODE
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF

                       *> Check if sender already sent a request to target
                       IF WS-FROM-USER = LS-USERNAME AND
                          WS-TO-USER = LS-TARGET-USERNAME AND
                          WS-STATUS = "PENDING"
                           MOVE 'A' TO LS-RETURN-CODE
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE CONNECTIONS-FILE.
           EXIT.

       ADD-CONNECTION-REQUEST SECTION.
           OPEN EXTEND CONNECTIONS-FILE.

           IF WS-CONNECTIONS-STATUS NOT = "00"
               DISPLAY "Error opening connections file for writing."
               DISPLAY "File Status: " WS-CONNECTIONS-STATUS
               MOVE 'X' TO LS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           *> Create the connection request record
           INITIALIZE CONNECTION-RECORD.
           STRING FUNCTION TRIM(LS-USERNAME) DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  FUNCTION TRIM(LS-TARGET-USERNAME) DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  "PENDING" DELIMITED BY SIZE
                  INTO CONNECTION-RECORD.

           WRITE CONNECTION-RECORD.

           CLOSE CONNECTIONS-FILE.
           EXIT.


       ACCEPT-CONNECTION-REQUEST SECTION.
           MOVE 'F' TO LS-RETURN-CODE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 0 TO WS-PENDING-COUNT. *> WS-PENDING-COUNT now acts as a line counter

           OPEN INPUT CONNECTIONS-FILE.
           IF WS-CONNECTIONS-STATUS NOT = "00" AND WS-CONNECTIONS-STATUS NOT = "35"
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               EXIT SECTION
           END-IF.

           *> Read all records into the memory table (WS-TEMP-RECORD)
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CONNECTIONS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-PENDING-COUNT
                       IF WS-PENDING-COUNT <= 200 *> Ensure we don't overflow the table
                           MOVE CONNECTION-RECORD TO WS-TEMP-RECORD(WS-PENDING-COUNT)
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE CONNECTIONS-FILE.

           *> Only proceed if there are records to process
           IF WS-PENDING-COUNT > 0
               *> Look for the specific pending request to update
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-PENDING-COUNT
                   UNSTRING WS-TEMP-RECORD(I) DELIMITED BY ":"
                       INTO WS-FROM-USER, WS-TO-USER, WS-STATUS

                   *> Check for the connection request TO the current user FROM the target user
                   IF FUNCTION TRIM(WS-FROM-USER) = FUNCTION TRIM(LS-TARGET-USERNAME) AND
                      FUNCTION TRIM(WS-TO-USER) = FUNCTION TRIM(LS-USERNAME) AND
                      FUNCTION TRIM(WS-STATUS) = "PENDING"
                       *> Found it, now update the status and rebuild the record
                       INITIALIZE WS-TEMP-RECORD(I)
                       STRING FUNCTION TRIM(LS-TARGET-USERNAME) DELIMITED BY SIZE
                              ":" DELIMITED BY SIZE
                              FUNCTION TRIM(LS-USERNAME) DELIMITED BY SIZE
                              ":" DELIMITED BY SIZE
                              "CONNECTED" DELIMITED BY SIZE
                              INTO WS-TEMP-RECORD(I)
                       MOVE 'S' TO LS-RETURN-CODE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF.

           *> If the request was found and updated (LS-RETURN-CODE = 'S'), rewrite the file
           IF LS-RETURN-CODE = 'S'
               OPEN OUTPUT CONNECTIONS-FILE
               IF WS-CONNECTIONS-STATUS NOT = "00"
                   MOVE 'X' TO LS-RETURN-CODE
                   CLOSE CONNECTIONS-FILE
                   EXIT SECTION
               END-IF

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-PENDING-COUNT
                   WRITE CONNECTION-RECORD FROM WS-TEMP-RECORD(I)
               END-PERFORM

               CLOSE CONNECTIONS-FILE
           END-IF.

           EXIT.

       VIEW-PENDING-CONNECTIONS SECTION.
           MOVE 0 TO WS-PENDING-COUNT.

           OPEN INPUT CONNECTIONS-FILE.
           OPEN EXTEND OUTPUT-FILE.

           *> If file doesn't exist, no pending requests
           IF WS-CONNECTIONS-STATUS = "35"
               MOVE "You have no pending connection requests." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               CLOSE CONNECTIONS-FILE
               CLOSE OUTPUT-FILE
               EXIT SECTION
           END-IF.

           IF WS-CONNECTIONS-STATUS NOT = "00"
               DISPLAY "Error opening connections file."
               DISPLAY "File Status: " WS-CONNECTIONS-STATUS
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               CLOSE OUTPUT-FILE
               EXIT SECTION
           END-IF.

           MOVE "Your Pending Connection Requests:" TO WS-MESSAGE.
           PERFORM DISPLAY-AND-LOG.

           MOVE 'N' TO WS-EOF-FLAG.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CONNECTIONS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       *> Parse the connection record: FROM_USER:TO_USER:STATUS
                       UNSTRING CONNECTION-RECORD DELIMITED BY ":"
                           INTO WS-FROM-USER, WS-TO-USER, WS-STATUS

                       *> Trim spaces from parsed fields
                       MOVE FUNCTION TRIM(WS-FROM-USER) TO WS-FROM-USER
                       MOVE FUNCTION TRIM(WS-TO-USER) TO WS-TO-USER
                       MOVE FUNCTION TRIM(WS-STATUS) TO WS-STATUS

                       *> Check if this is a pending request TO the current user
                       IF WS-TO-USER = LS-USERNAME AND WS-STATUS = "PENDING"
                           ADD 1 TO WS-PENDING-COUNT
                           INITIALIZE WS-MESSAGE
                           STRING "- " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-FROM-USER) DELIMITED BY SPACE
                                  " wants to connect with you" DELIMITED BY SIZE
                                  INTO WS-MESSAGE
                           PERFORM DISPLAY-AND-LOG
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-PENDING-COUNT = 0
               MOVE "You have no pending connection requests." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
           ELSE
               *> Success code 'S' is returned if requests were found, so main program
               *> knows to prompt for management action.
               MOVE 'S' TO LS-RETURN-CODE
           END-IF.

           CLOSE CONNECTIONS-FILE.
           CLOSE OUTPUT-FILE.
           EXIT.

       DISPLAY-AND-LOG SECTION.
           DISPLAY WS-MESSAGE.
           MOVE WS-MESSAGE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           EXIT.

