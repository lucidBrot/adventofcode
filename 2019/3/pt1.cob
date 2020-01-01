      * Required: Info about the program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. LUCID.
      * INPUT / OUTPUT and such
       ENVIRONMENT DIVISION.
      * Sections are parts of a division:
       CONFIGURATION SECTION.
      * Optional: Specify the OS of the building and executing computer
      ** SOURCE-COMPUTER. XXX.
      ** OBJECT-COMPUTER. XXX
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ONEINPUTFILE ASSIGN TO 'cable1.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT TWOINPUTFILE ASSIGN TO 'cable2.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
      * Constants and Variables are in the Data Division:
      * File, Working-storage for temp variables, Local-Storage for allocated variables, Linkage
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Define a variable at hierarchy 01 (up to 49) or in the case of
      * 01 it can also be a constant. See page 111 (pdf page 125) for
      * more info.
      * Also see
      * https://open-cobol.sourceforge.io/guides/GnuCOBOL%202.2%20NOV2017%20Programmers%20Guide%20(US%20Letter).pdf
      * page 173 Section 5.9.24
      * PIC A(bytecount) is alphabetic, PIC X(bytecount) is alphanumeric
      * "PIC" seems to stand for "picture-string"
       01 WS-CONTENT.
      *    Numeric String of size 5 bytes (zero-initialized)
           05 WS-SOMETHING PIC 9(5).
      *    Alphabetic String of size 25 bytes
           05 WS-SOME-NAME PIC A(25).
      * Alphabetic string of 1 byte length:
       01 WS-EOF PIC A(1).
      * Valid PICTURE characters are A(alphabetic), N(single character -
      * same as X(2)),
      * X(Alphanumeric), Z, 1, 9 and *
      * I don't know yet what all of them do.
       01 GRID.
           05 GRID-ROW OCCURS 3000 TIMES.
               10 GRID-COL OCCURS 3000 TIMES.
                   15 GRID-CHARACTER PIC X(1).

       01 TEMP-CABLE-STEP.
           05 DIRECTION PIC A(1) VALUE 'Z'.
           05 NUM-STEPS PIC 9(4) VALUE 0.

       01 CABLE-ONE.
           02 CONE-STUFF OCCURS 1000 TIMES.
               05 DIRECTION PIC A(1) VALUE 'Z'.
               05 NUM-STEPS PIC 9(4) VALUE 0.

       01 CABLE-TWO.
           02 CTWO-STUFF OCCURS 1000 TIMES.
               05 DIRECTION PIC A(1) VALUE 'Z'.
               05 NUM-STEPS PIC 9(4) VALUE 0.

       01 UNINITIALIZED-DIRECTION PIC A(1) VALUE 'Z'.
       01 UP-DIRECTION PIC A(1) VALUE 'U'.
       01 DOWN-DIRECTION PIC A(1) VALUE 'D'.
       01 LEFT-DIRECTION PIC A(1) VALUE 'L'.
       01 RIGHT-DIRECTION PIC A(1) VALUE 'R'.

       01 LOOP-CTR PIC 9(10) VALUE 1 .
           
       01 CENTEER PIC 9(10) VALUE 500 .
       01 NAVX PIC 9(10).
       01 NAVY PIC 9(10).

      * Executable Code
       PROCEDURE DIVISION.
           SET LOOP-CTR TO 1 .
           MOVE 'N' TO WS-EOF .
      *    Specify Grid Size
           OPEN Input ONEINPUTFILE.
           PERFORM UNTIL WS-EOF='Y'
               READ ONEINPUTFILE INTO TEMP-CABLE-STEP
                   AT END MOVE 'Y' TO WS-EOF
      *                    Invalidly structured data is printed empty when using
      *                    WS-SOME-NAME but is printed entirely when using
      *                    WS-CONTENT
                   NOT AT END 

                   MOVE DIRECTION OF TEMP-CABLE-STEP TO DIRECTION OF
                   CONE-STUFF(LOOP-CTR)
                   MOVE NUM-STEPS OF TEMP-CABLE-STEP TO NUM-STEPS OF
                   CONE-STUFF(LOOP-CTR)
                   ADD 1 TO LOOP-CTR
               END-READ
           END-PERFORM.
           DISPLAY "CABLE: "CABLE-ONE.
           CLOSE ONEINPUTFILE.

           SET LOOP-CTR TO 1 .
           MOVE 'N' TO WS-EOF .
           OPEN Input TWOINPUTFILE.
           PERFORM UNTIL WS-EOF='Y'
               READ TWOINPUTFILE INTO TEMP-CABLE-STEP
                   AT END MOVE 'Y' TO WS-EOF
      *                    Invalidly structured data is printed empty when using
      *                    WS-SOME-NAME but is printed entirely when using
      *                    WS-CONTENT
                   NOT AT END 

                   MOVE DIRECTION OF TEMP-CABLE-STEP TO DIRECTION OF
                   CTWO-STUFF(LOOP-CTR)
                   MOVE NUM-STEPS OF TEMP-CABLE-STEP TO NUM-STEPS OF
                   CTWO-STUFF(LOOP-CTR)
                   ADD 1 TO LOOP-CTR
               END-READ
           END-PERFORM.
           DISPLAY "CABLE: "CABLE-TWO.
           CLOSE TWOINPUTFILE.

      *    Cables parsed.
      *    Now write to grid and when they cross, mark as X
      *    Later find again using a search where DIRECTION is X (and not
      *    Z for uninitialized or U,D,L,R for up down left right)

      * read cable 1 into the grid. X is right/left, Y is up/down. 0,0
      * is at top left corner
           SET LOOP-CTR TO 0 .
           SET NAVX TO CENTEER .
           MOVE CENTEER TO NAVY .
           PERFORM UNTIL LOOP-CTR > 1000
               ADD 1 TO LOOP-CTR
               MOVE CONE-STUFF(LOOP-CTR) TO TEMP-CABLE-STEP
      * TODO: interpret commandsj
               DISPLAY "CABLE 1 DO "TEMP-CABLE-STEP
               IF ( DIRECTION OF TEMP-CABLE-STEP =
                   UNINITIALIZED-DIRECTION )
                   SET LOOP-CTR TO 1001
               END-IF

               IF DIRECTION OF TEMP-CABLE-STEP = RIGHT-DIRECTION
                   ADD NUM-STEPS OF TEMP-CABLE-STEP TO NAVX
               END-IF

               IF DIRECTION OF TEMP-CABLE-STEP = UP-DIRECTION
                   ADD NUM-STEPS OF TEMP-CABLE-STEP TO NAVY
               END-IF
               
               IF DIRECTION OF TEMP-CABLE-STEP = LEFT-DIRECTION
                   SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM NAVX
               END-IF

               IF DIRECTION OF TEMP-CABLE-STEP = DOWN-DIRECTION
                   SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM NAVY
               END-IF

               DISPLAY "(NAVX, NAVY): ("NAVX", "NAVY")"
           END-PERFORM.

           STOP RUN.
