*> This is free-form
IDENTIFICATION DIVISION.
      PROGRAM-ID. inCollege.
      AUTHOR. Paola
      DATE-WRITTEN. 9/7/2025
ENVIRONMENT DIVISION.
DATA DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
           ORGANIZATION IS LINE SEQUENTIAL. *> what does organization is line sequential mean?
       SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD-DATA.
           05  IN-USER-CHOICE       PIC X.
           05  IN-USERNAME          PIC X(20).
           05  IN-PASSWORD          PIC X(20).
           05  FILLER               PIC X(9).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD-DATA       PIC X(80).

       WORKING-STORAGE SECTION. *> what does the working section even do??? why do we need it?
       01  WS-USER-CHOICE         PIC X.
       01  WS-USERNAME            PIC X(20).
       01  WS-PASSWORD            PIC X(20).
       01  WS-END-OF-FILE         PIC X       VALUE 'N'.

