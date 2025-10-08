       >>SOURCE FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECTIONS.
       AUTHOR. Vamsi Dandu and Kaden
       DATE-WRITTEN. 09/28/2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONNECTIONS-FILE ASSIGN TO "connections.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONNECTIONS-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.
           SELECT PROFILES-FILE ASSIGN TO "profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PROFILES-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONNECTIONS-FILE.
       01  CONNECTION-RECORD    PIC X(100).

       FD  OUTPUT-FILE EXTERNAL.
       01  OUTPUT-RECORD        PIC X(350).

       FD  PROFILES-FILE.
       01  PROFILES-RECORD          PIC X(350).

       WORKING-STORAGE SECTION.
       01  WS-PROFILES-STATUS       PIC X(2).
       01  WS-NET-COUNT             PIC 9(3) VALUE 0.
       01  WS-NET-LIST.
           05 WS-NET-USER OCCURS 100 TIMES PIC X(20).
       01  WS-FOUND-FLAG            PIC X VALUE 'N'.
       01  WS-IN-PROFILE-BLOCK      PIC X VALUE 'N'.
       01  WS-U                     PIC X(20).
       01  WS-FNAME                 PIC X(50).
       01  WS-LNAME                 PIC X(50).
       01  WS-OUTPUT-STATUS PIC X(2).
       01  WS-CONNECTIONS-STATUS PIC X(2).
       01  WS-EOF-FLAG          PIC X VALUE 'N'.
       01  WS-FROM-USER         PIC X(20).
       01  WS-TO-USER           PIC X(20).
       01  WS-STATUS            PIC X(10).
       01  WS-MESSAGE           PIC X(80).
       01  WS-PENDING-COUNT     PIC 9(3) VALUE 0.
       01  WS-LAST-INDEX        PIC 9(3) VALUE 0.

       01  I                    PIC 9(3).
       01  J                    PIC 9(3).
       01  WS-TEMP-TABLE.
           *> In-memory storage for file read/write operations
           05 WS-TEMP-RECORD OCCURS 200 TIMES PIC X(100).

       01  WS-ALREADY-CONNECTED PIC X VALUE 'N'.
       01  WS-PENDING-EXISTS    PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  LS-ACTION            PIC X(10).  *> Action: SEND, VIEW, ACCEPT
       01  LS-USERNAME          PIC X(20).
       01  LS-TARGET-USERNAME   PIC X(20).
       01  LS-RETURN-CODE       PIC X.     *> S=Success, F=Failure, X=Error

       01  WS-TARGET-FOUND          PIC X VALUE 'N'.


       PROCEDURE DIVISION USING LS-ACTION, LS-USERNAME, LS-TARGET-USERNAME, LS-RETURN-CODE.

           MOVE 'S' TO LS-RETURN-CODE.

           *> Direct control based on the requested action
           EVALUATE FUNCTION TRIM(LS-ACTION)
               WHEN "SEND"
                   PERFORM SEND-CONNECTION-REQUEST
               WHEN "VIEW"
                   PERFORM VIEW-PENDING-CONNECTIONS
               WHEN "ACCEPT"
                   PERFORM ACCEPT-CONNECTION-REQUEST
               WHEN "REJECT"
                   PERFORM REJECT-CONNECTION-REQUEST
               WHEN "NETWORK"
                   PERFORM VIEW-MY-NETWORK
               WHEN OTHER
                   MOVE 'E' TO LS-RETURN-CODE
           END-EVALUATE.

           GOBACK.

       SEND-CONNECTION-REQUEST SECTION.
           MOVE 'N' TO WS-ALREADY-CONNECTED.
           MOVE 'N' TO WS-PENDING-EXISTS.

           *> Verify that the target user exists in profiles.txt
           MOVE 'N' TO WS-TARGET-FOUND.
           OPEN INPUT PROFILES-FILE.

           IF WS-PROFILES-STATUS = "35"
               MOVE "User does not exist." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'F' TO LS-RETURN-CODE
               CLOSE PROFILES-FILE
               EXIT SECTION
           END-IF

           IF WS-PROFILES-STATUS NOT = "00"
               MOVE "Error accessing profiles file." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE PROFILES-FILE
               EXIT SECTION
           END-IF

           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ PROFILES-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF PROFILES-RECORD(1:5) = "USER:" AND
                          FUNCTION TRIM(PROFILES-RECORD(6:20)) = FUNCTION TRIM(LS-TARGET-USERNAME)
                           MOVE 'Y' TO WS-TARGET-FOUND
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PROFILES-FILE

           IF WS-TARGET-FOUND NOT = 'Y'
               MOVE "User does not exist." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'F' TO LS-RETURN-CODE
               EXIT SECTION
           END-IF


           *> Check for existing connections or pending requests
           PERFORM CHECK-EXISTING-CONNECTIONS.

           IF LS-RETURN-CODE NOT = 'S'
               EXIT SECTION
           END-IF.

           *> Add the new connection request record
           PERFORM ADD-CONNECTION-REQUEST.
           EXIT.

       CHECK-EXISTING-CONNECTIONS SECTION.
           OPEN INPUT CONNECTIONS-FILE.

           *> Handle file not found (status 35) or general file error
           IF WS-CONNECTIONS-STATUS = "35"
               CLOSE CONNECTIONS-FILE
               EXIT PARAGRAPH
           END-IF.

           IF WS-CONNECTIONS-STATUS NOT = "00"
               DISPLAY "Error opening connections file for validation."
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

                       *> Check if already connected (status 'C')
                       IF ((WS-FROM-USER = LS-USERNAME AND
                            WS-TO-USER = LS-TARGET-USERNAME) OR
                           (WS-FROM-USER = LS-TARGET-USERNAME AND
                            WS-TO-USER = LS-USERNAME))
                            AND WS-STATUS = "CONNECTED"
                           MOVE 'Y' TO WS-ALREADY-CONNECTED
                           MOVE 'C' TO LS-RETURN-CODE
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF

                       *> Check if target user has already sent a request (status 'P')
                       IF WS-FROM-USER = LS-TARGET-USERNAME AND
                          WS-TO-USER = LS-USERNAME AND
                          WS-STATUS = "PENDING"
                           MOVE 'Y' TO WS-PENDING-EXISTS
                           MOVE 'P' TO LS-RETURN-CODE
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF

                       *> Check if sender already sent a request (status 'A')
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
            MOVE 0 TO I
            MOVE 'N' TO WS-EOF-FLAG
            *> Initialize the temp record table to avoid garbage
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 200
                MOVE SPACES TO WS-TEMP-RECORD(I)
            END-PERFORM
            MOVE 0 TO I
            *> Open for input to read existing records, create if not found
            OPEN INPUT CONNECTIONS-FILE
            IF WS-CONNECTIONS-STATUS = "35"
                CLOSE CONNECTIONS-FILE
                OPEN OUTPUT CONNECTIONS-FILE
                IF WS-CONNECTIONS-STATUS NOT = "00"
                    MOVE 'X' TO LS-RETURN-CODE
                    EXIT PARAGRAPH
                END-IF
                CLOSE CONNECTIONS-FILE
                OPEN INPUT CONNECTIONS-FILE
            END-IF
            IF WS-CONNECTIONS-STATUS NOT = "00"
                MOVE 'X' TO LS-RETURN-CODE
                EXIT PARAGRAPH
            END-IF
            *> Read all existing records into the in-memory table
            PERFORM UNTIL WS-EOF-FLAG = 'Y'
                READ CONNECTIONS-FILE
                    AT END
                        MOVE 'Y' TO WS-EOF-FLAG
                    NOT AT END
                        ADD 1 TO I
                        IF I <= 200
                            MOVE CONNECTION-RECORD TO WS-TEMP-RECORD(I)
                        ELSE
                            MOVE 'Y' TO WS-EOF-FLAG
                        END-IF
                END-READ
            END-PERFORM
            CLOSE CONNECTIONS-FILE
            *> Add the new "PENDING" record
            ADD 1 TO I
            IF I <= 200
                INITIALIZE CONNECTION-RECORD
                STRING FUNCTION TRIM(LS-USERNAME) DELIMITED BY SIZE
                    ":" DELIMITED BY SIZE
                    FUNCTION TRIM(LS-TARGET-USERNAME) DELIMITED BY SIZE
                    ":" DELIMITED BY SIZE
                    "PENDING" DELIMITED BY SIZE
                    INTO CONNECTION-RECORD
                MOVE CONNECTION-RECORD TO WS-TEMP-RECORD(I)
            ELSE
                MOVE 'X' TO LS-RETURN-CODE
                EXIT PARAGRAPH
            END-IF
            *> Write all records back to the file, preserving all lines
            OPEN OUTPUT CONNECTIONS-FILE
            IF WS-CONNECTIONS-STATUS NOT = "00"
                MOVE 'X' TO LS-RETURN-CODE
                EXIT PARAGRAPH
            END-IF
            MOVE I TO WS-LAST-INDEX  *> Store the last valid index
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LAST-INDEX
                IF WS-TEMP-RECORD(I) NOT = SPACES AND WS-TEMP-RECORD(I) NOT = LOW-VALUES
                    WRITE CONNECTION-RECORD FROM WS-TEMP-RECORD(I)
                    IF WS-CONNECTIONS-STATUS NOT = "00"
                        MOVE 'X' TO LS-RETURN-CODE
                        EXIT PERFORM
                    END-IF
                END-IF
            END-PERFORM
            IF LS-RETURN-CODE = 'S'
                MOVE 'S' TO LS-RETURN-CODE
            END-IF
            CLOSE CONNECTIONS-FILE
            EXIT.


       ACCEPT-CONNECTION-REQUEST SECTION.
           MOVE 'F' TO LS-RETURN-CODE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 0 TO WS-PENDING-COUNT.

           OPEN INPUT CONNECTIONS-FILE.
           *> Check for file error during open
           IF WS-CONNECTIONS-STATUS NOT = "00" AND WS-CONNECTIONS-STATUS NOT = "35"
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               EXIT SECTION
           END-IF.

           *> Read all connection records into the in-memory table
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CONNECTIONS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-PENDING-COUNT
                       IF WS-PENDING-COUNT <= 200
                           MOVE CONNECTION-RECORD TO WS-TEMP-RECORD(WS-PENDING-COUNT)
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE CONNECTIONS-FILE.

           *> Search the in-memory table for the specific PENDING request
           IF WS-PENDING-COUNT > 0
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-PENDING-COUNT
                   UNSTRING WS-TEMP-RECORD(I) DELIMITED BY ":"
                       INTO WS-FROM-USER, WS-TO-USER, WS-STATUS

                   *> Check if the record is the request to be accepted
                   IF FUNCTION TRIM(WS-FROM-USER) = FUNCTION TRIM(LS-TARGET-USERNAME) AND
                      FUNCTION TRIM(WS-TO-USER) = FUNCTION TRIM(LS-USERNAME) AND
                      FUNCTION TRIM(WS-STATUS) = "PENDING"
                       *> Update status from PENDING to CONNECTED
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

           *> Rewrite the entire file if a record was successfully updated
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

            *> If file missing, treat as "no pending"
            IF WS-CONNECTIONS-STATUS = "35"
                MOVE "You have no pending connection requests." TO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE 'F' TO LS-RETURN-CODE          *> ensure caller knows there are none
                CLOSE CONNECTIONS-FILE
                EXIT SECTION
            END-IF

            *> Other errors on connections file
            IF WS-CONNECTIONS-STATUS NOT = "00"
                MOVE "Error accessing connections file." TO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE 'X' TO LS-RETURN-CODE
                CLOSE CONNECTIONS-FILE
                EXIT SECTION
            END-IF

            MOVE "Your Pending Connection Requests:" TO WS-MESSAGE
            PERFORM DISPLAY-AND-LOG

            MOVE 'N' TO WS-EOF-FLAG
            PERFORM UNTIL WS-EOF-FLAG = 'Y'
                READ CONNECTIONS-FILE
                    AT END
                        MOVE 'Y' TO WS-EOF-FLAG
                    NOT AT END
                        UNSTRING CONNECTION-RECORD DELIMITED BY ":"
                            INTO WS-FROM-USER WS-TO-USER WS-STATUS
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
            END-PERFORM

            IF WS-PENDING-COUNT = 0
                MOVE "You have no pending connection requests." TO WS-MESSAGE
                PERFORM DISPLAY-AND-LOG
                MOVE 'F' TO LS-RETURN-CODE          *> <<< important
                CLOSE CONNECTIONS-FILE
                EXIT SECTION                         *> <<< return immediately
            ELSE
                MOVE 'S' TO LS-RETURN-CODE
            END-IF

            CLOSE CONNECTIONS-FILE
            EXIT.



       DISPLAY-AND-LOG SECTION.
           DISPLAY WS-MESSAGE.
           MOVE WS-MESSAGE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           EXIT.

       REJECT-CONNECTION-REQUEST SECTION.
           MOVE 'F' TO LS-RETURN-CODE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 0   TO WS-PENDING-COUNT.

           *> Load existing records into memory
           OPEN INPUT CONNECTIONS-FILE.
           IF WS-CONNECTIONS-STATUS NOT = "00" AND WS-CONNECTIONS-STATUS NOT = "35"
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               EXIT SECTION
           END-IF

           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CONNECTIONS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-PENDING-COUNT
                       MOVE CONNECTION-RECORD TO WS-TEMP-RECORD(WS-PENDING-COUNT)
               END-READ
           END-PERFORM
           CLOSE CONNECTIONS-FILE.

           *> Copy all except the matching pending request
           MOVE 0 TO I
           MOVE 0 TO J
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-PENDING-COUNT
               UNSTRING WS-TEMP-RECORD(I) DELIMITED BY ":"
                   INTO WS-FROM-USER WS-TO-USER WS-STATUS
               IF FUNCTION TRIM(WS-FROM-USER) = FUNCTION TRIM(LS-TARGET-USERNAME)
                  AND FUNCTION TRIM(WS-TO-USER) = FUNCTION TRIM(LS-USERNAME)
                  AND FUNCTION TRIM(WS-STATUS) = "PENDING"
                   MOVE 'S' TO LS-RETURN-CODE
               ELSE
                   ADD 1 TO J
                   MOVE WS-TEMP-RECORD(I) TO WS-TEMP-RECORD(J)
               END-IF
           END-PERFORM.

           *> Rewrite file without the deleted line
           IF LS-RETURN-CODE = 'S'
               OPEN OUTPUT CONNECTIONS-FILE
               IF WS-CONNECTIONS-STATUS NOT = "00"
                   MOVE 'X' TO LS-RETURN-CODE
                   CLOSE CONNECTIONS-FILE
                   EXIT SECTION
               END-IF

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
                   IF WS-TEMP-RECORD(I) NOT = SPACES AND WS-TEMP-RECORD(I) NOT = LOW-VALUES
                       WRITE CONNECTION-RECORD FROM WS-TEMP-RECORD(I)
                   END-IF
               END-PERFORM
               CLOSE CONNECTIONS-FILE
           END-IF

           EXIT SECTION.

       VIEW-MY-NETWORK SECTION.
           MOVE 0 TO WS-NET-COUNT.

           *> Read all connections and collect peers connected to LS-USERNAME
           OPEN INPUT CONNECTIONS-FILE
           IF WS-CONNECTIONS-STATUS = "35"
               *> No connections file -> no network
               MOVE "You have no connections yet." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'F' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               EXIT SECTION
           END-IF
           IF WS-CONNECTIONS-STATUS NOT = "00"
               MOVE "Error accessing connections file." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE CONNECTIONS-FILE
               EXIT SECTION
           END-IF

           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CONNECTIONS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       UNSTRING CONNECTION-RECORD DELIMITED BY ":"
                           INTO WS-FROM-USER WS-TO-USER WS-STATUS

                       IF FUNCTION TRIM(WS-STATUS) = "CONNECTED"
                          AND (FUNCTION TRIM(WS-FROM-USER) = FUNCTION TRIM(LS-USERNAME)
                               OR FUNCTION TRIM(WS-TO-USER)   = FUNCTION TRIM(LS-USERNAME))

                           *> Determine the peer username
                           IF FUNCTION TRIM(WS-FROM-USER) = FUNCTION TRIM(LS-USERNAME)
                               MOVE FUNCTION TRIM(WS-TO-USER) TO WS-U
                           ELSE
                               MOVE FUNCTION TRIM(WS-FROM-USER) TO WS-U
                           END-IF

                           *> Deduplicate
                           MOVE 'N' TO WS-FOUND-FLAG
                           PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > WS-NET-COUNT OR WS-FOUND-FLAG = 'Y'
                               IF FUNCTION TRIM(WS-NET-USER(I)) = WS-U
                                   MOVE 'Y' TO WS-FOUND-FLAG
                               END-IF
                           END-PERFORM

                           IF WS-FOUND-FLAG = 'N'
                               ADD 1 TO WS-NET-COUNT
                               IF WS-NET-COUNT <= 100
                                   MOVE WS-U TO WS-NET-USER(WS-NET-COUNT)
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONNECTIONS-FILE

           IF WS-NET-COUNT = 0
               MOVE "You have no connections yet." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'F' TO LS-RETURN-CODE
               EXIT SECTION
           END-IF

           MOVE "Your Network:" TO WS-MESSAGE
           PERFORM DISPLAY-AND-LOG

           *> For each peer, look up FNAM/LNAM in profiles.txt and print
           OPEN INPUT PROFILES-FILE
           IF WS-PROFILES-STATUS NOT = "00" AND WS-PROFILES-STATUS NOT = "35"
               MOVE "Error accessing profiles file." TO WS-MESSAGE
               PERFORM DISPLAY-AND-LOG
               MOVE 'X' TO LS-RETURN-CODE
               CLOSE PROFILES-FILE
               EXIT SECTION
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NET-COUNT
               MOVE FUNCTION TRIM(WS-NET-USER(I)) TO WS-U

               *> Rewind profiles file to start for each user
               CLOSE PROFILES-FILE
               OPEN INPUT PROFILES-FILE
               IF WS-PROFILES-STATUS NOT = "00" AND WS-PROFILES-STATUS NOT = "35"
                   MOVE "Error accessing profiles file." TO WS-MESSAGE
                   PERFORM DISPLAY-AND-LOG
                   MOVE 'X' TO LS-RETURN-CODE
                   CLOSE PROFILES-FILE
                   EXIT SECTION
               END-IF

               MOVE 'N' TO WS-EOF-FLAG
               MOVE 'N' TO WS-IN-PROFILE-BLOCK
               MOVE SPACES TO WS-FNAME
               MOVE SPACES TO WS-LNAME

               PERFORM UNTIL WS-EOF-FLAG = 'Y'
                   READ PROFILES-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF-FLAG
                       NOT AT END
                           *> Detect start of this user’s block
                           IF PROFILES-RECORD(1:5) = "USER:"
                              AND FUNCTION TRIM(PROFILES-RECORD(6:20)) = WS-U
                               MOVE 'Y' TO WS-IN-PROFILE-BLOCK
                               MOVE SPACES TO WS-FNAME
                               MOVE SPACES TO WS-LNAME
                           END-IF

                           IF WS-IN-PROFILE-BLOCK = 'Y'
                               IF PROFILES-RECORD(1:5) = "FNAM:"
                                   MOVE FUNCTION TRIM(PROFILES-RECORD(6:50)) TO WS-FNAME
                               END-IF
                               IF PROFILES-RECORD(1:5) = "LNAM:"
                                   MOVE FUNCTION TRIM(PROFILES-RECORD(6:50)) TO WS-LNAME
                               END-IF
                               IF PROFILES-RECORD(1:10) = "ENDPROFILE"
                                   *> reached end of matching block -> stop reading
                                   MOVE 'Y' TO WS-EOF-FLAG
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM

               *> Print name if found; fallback to username
               INITIALIZE WS-MESSAGE
               IF WS-FNAME > SPACES OR WS-LNAME > SPACES
                   STRING "- " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-FNAME) DELIMITED BY SIZE
                          " " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-LNAME) DELIMITED BY SIZE
                          INTO WS-MESSAGE
               ELSE
                   STRING "- " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-U) DELIMITED BY SIZE
                          INTO WS-MESSAGE
               END-IF
               PERFORM DISPLAY-AND-LOG
           END-PERFORM

           CLOSE PROFILES-FILE

           MOVE 'S' TO LS-RETURN-CODE
           EXIT SECTION.


