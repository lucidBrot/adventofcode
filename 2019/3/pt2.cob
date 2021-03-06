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
       01 CH1 PIC X(1) VALUE '1'.
       01 CH2 PIC X(1) VALUE '2'.
       01 CHBOTH PIC X(1) VALUE 'B'.
       01 CHEMPTY PIC X(1) VALUE 'E'.

       01 TEMP-CABLE-STEP.
           05 DIRECTION PIC A(1) VALUE 'Z'.
           05 NUM-STEPS PIC S9(9) USAGE IS COMPUTATIONAL VALUE 0.

       01 CABLE-ONE.
           02 CONE-STUFF OCCURS 1000 TIMES.
               05 DIRECTION PIC A(1) VALUE 'Z'.
               05 NUM-STEPS PIC S9(9) USAGE IS COMPUTATIONAL VALUE 0.

       01 CABLE-TWO.
           02 CTWO-STUFF OCCURS 1000 TIMES.
               05 DIRECTION PIC A(1) VALUE 'Z'.
               05 NUM-STEPS PIC S9(9) USAGE IS COMPUTATIONAL VALUE 0.

       01 UNINITIALIZED-DIRECTION PIC A(1) VALUE 'Z'.
       01 UP-DIRECTION PIC A(1) VALUE 'U'.
       01 DOWN-DIRECTION PIC A(1) VALUE 'D'.
       01 LEFT-DIRECTION PIC A(1) VALUE 'L'.
       01 RIGHT-DIRECTION PIC A(1) VALUE 'R'.

       01 LOOP-CTR PIC 9(10) VALUE 1 .
           
       01 CENTEER PIC S9(9) USAGE IS COMPUTATIONAL VALUE 38500 .
       01 NAVX PIC S9(9) USAGE IS COMPUTATIONAL.
       01 NAVX-PREV PIC S9(9) USAGE IS COMPUTATIONAL.
       01 NAVX-POST PIC S9(9) USAGE IS COMPUTATIONAL.
       01 NAVY PIC S9(9) USAGE IS COMPUTATIONAL.
       01 NAVY-POST PIC S9(9) USAGE IS COMPUTATIONAL.
       01 NAVY-PREV PIC S9(9) USAGE IS COMPUTATIONAL.

       01 GRIDSET.
           03 SET-ENTRY OCCURS 100000 TIMES INDEXED BY SEARCHINDEX.
               05 X-COORD PIC 9(9).
               05 Y-COORD PIC 9(9).
               05 CHAR PIC X(1) VALUE 'E'.
           03 LATEST-INSERT PIC 9(9) VALUE 0.

       01 GRIDSET1AND2.
           03 SET-ENTRY2 OCCURS 10000 TIMES INDEXED BY SEARCHINDEX2.
               05 X-COORD PIC 9(9).
               05 Y-COORD PIC 9(9).
               05 CHAR PIC X(1) VALUE 'E'.
           03 LATEST-INSERT2 PIC 9(9) VALUE 0.

       01 INSERTSETENTRY2.
           03 X-COORD PIC 9(9).
           03 Y-COORD PIC 9(9).
           03 CHAR PIC X(1) VALUE 'E'.

       01 TEMP-NUM PIC 9(10) VALUE 0.

       01 VAL-A PIC 9(9) VALUE 0.
       01 VAL-D PIC 9(9) VALUE 0.
       01 VAL-C PIC 9(9) VALUE 0.
       01 VAL-B PIC 9(9) VALUE 0.
       01 STEPS-SUM PIC 9(9) VALUE 0.
       01 VAL-E PIC 9(9) VALUE 0.

       01 SS-INDEX PIC 9(9) VALUE 0.
       01 SS-COUNTER PIC 9(9) VALUE 0.
       01 SS-CROSSES PIC 9(9) VALUE 0.
       01 SS-CURR-X PIC 9(9) VALUE 0.
       01 SS-CURR-Y PIC 9(9) VALUE 0.
       01 SS-VAL-Q PIC 9(9) VALUE 0.
       01 SS-VAL-U PIC 9(9) VALUE 0.
       01 SS-VAL-V PIC 9(9) VALUE 0.
       01 SS-VAL-W PIC 9(9) VALUE 0.
       01 SS-SHOULD-STOP PIC 9(9) VALUE 0.
       01 SS-NUM-STEPS PIC 9(9) VALUE 0.



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
                   MOVE FUNCTION NUMVAL(NUM-STEPS OF TEMP-CABLE-STEP)
                   TO NUM-STEPS OF
                   CONE-STUFF(LOOP-CTR)
                   ADD 1 TO LOOP-CTR
               END-READ
           END-PERFORM.
      *DISPLAY "CABLE: "CABLE-ONE.
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
                   MOVE FUNCTION NUMVAL(NUM-STEPS OF TEMP-CABLE-STEP)
                   TO NUM-STEPS OF
                   CTWO-STUFF(LOOP-CTR)
                   ADD 1 TO LOOP-CTR
               END-READ
           END-PERFORM.
      *DISPLAY "CABLE: "CABLE-TWO.
           CLOSE TWOINPUTFILE.

      *    Cables parsed.
      *    Now write to grid and when they cross, mark as X
      *    Later find again using a search where DIRECTION is X (and not
      *    Z for uninitialized or U,D,L,R for up down left right)

      * read cable 1 into the grid. X is right/left, Y is up/down.
           SET LOOP-CTR TO 0 .
           SET NAVX TO CENTEER .
           SET NAVY TO CENTEER .
           PERFORM UNTIL LOOP-CTR > 1000
               ADD 1 TO LOOP-CTR
               MOVE CONE-STUFF(LOOP-CTR) TO TEMP-CABLE-STEP

      *DISPLAY "CABLE 1 DO "DIRECTION OF
      *TEMP-CABLE-STEP":"NUM-STEPS OF TEMP-CABLE-STEP
               IF ( DIRECTION OF TEMP-CABLE-STEP =
                   UNINITIALIZED-DIRECTION )
                   SET LOOP-CTR TO 1001
               ELSE

      *    set every value from the current position up to (including) the
      *    final position
               SET NAVY-PREV TO NAVY
               SET NAVX-PREV TO NAVX
               IF DIRECTION OF TEMP-CABLE-STEP = RIGHT-DIRECTION
                   OR DIRECTION OF TEMP-CABLE-STEP = UP-DIRECTION
               ADD NUM-STEPS OF TEMP-CABLE-STEP TO NAVX-PREV
                   GIVING NAVX-POST
               ADD NUM-STEPS OF TEMP-CABLE-STEP TO NAVY-PREV
                   GIVING NAVY-POST
               ELSE
               SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM NAVX-PREV
                   GIVING NAVX-POST
               SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM NAVY-PREV
                   GIVING NAVY-POST
               END-IF
      *    only NAVX-POST XOR NAVY-POST are relevant, only NAVY XOR NAVX
      *    shall be modified
               PERFORM UNTIL NAVX = NAVX-POST OR NAVY = NAVY-POST
     
                   IF DIRECTION OF TEMP-CABLE-STEP = RIGHT-DIRECTION
                       ADD 1 TO NAVX
                   END-IF
     
                   IF DIRECTION OF TEMP-CABLE-STEP = UP-DIRECTION
                       ADD 1 TO NAVY
                   END-IF
                    
                   IF DIRECTION OF TEMP-CABLE-STEP = LEFT-DIRECTION
                       SUBTRACT 1 FROM NAVX
                   END-IF
     
                   IF DIRECTION OF TEMP-CABLE-STEP = DOWN-DIRECTION
                       SUBTRACT 1 FROM NAVY
                   END-IF
     
      *    search list for that value and if it is not there, set it
                   SET SEARCHINDEX TO 1
                   SEARCH SET-ENTRY OF GRIDSET
                       VARYING SEARCHINDEX
                       AT END 
                           ADD 1 TO LATEST-INSERT OF GRIDSET
                           MOVE CH1 TO CHAR OF SET-ENTRY(LATEST-INSERT)
                           MOVE NAVX TO 
                           X-COORD OF SET-ENTRY(LATEST-INSERT)
                           MOVE NAVY TO 
                           Y-COORD OF SET-ENTRY(LATEST-INSERT)
                           DISPLAY "["LOOP-CTR"] INSERTED CABLE1: ("
                           NAVX", "NAVY") at ["LATEST-INSERT"]"
                       WHEN ( X-COORD OF SET-ENTRY(SEARCHINDEX) = NAVX )
                           AND
                           ( Y-COORD OF SET-ENTRY(SEARCHINDEX) = NAVY )
                           MOVE CH1 TO CHAR OF SET-ENTRY(SEARCHINDEX)
                           DISPLAY "MODIFIED CABLE1: ("NAVX", "NAVY")"
                   END-SEARCH
                END-PERFORM
               END-IF
            END-PERFORM.

      *TODO: store second cable and intersections
           SET LOOP-CTR TO 0 .
           SET NAVX TO CENTEER .
           SET NAVY TO CENTEER .
           PERFORM UNTIL LOOP-CTR > 1000
               ADD 1 TO LOOP-CTR
               MOVE CTWO-STUFF(LOOP-CTR) TO TEMP-CABLE-STEP

      *DISPLAY "CABLE 2 DO "DIRECTION OF
      *TEMP-CABLE-STEP":"NUM-STEPS OF TEMP-CABLE-STEP
               IF ( DIRECTION OF TEMP-CABLE-STEP =
                   UNINITIALIZED-DIRECTION )
                   SET LOOP-CTR TO 1001
               ELSE


      *    set every value from the current position up to (including) the
      *    final position
               SET NAVY-PREV TO NAVY
               SET NAVX-PREV TO NAVX
               IF DIRECTION OF TEMP-CABLE-STEP = RIGHT-DIRECTION
                   OR DIRECTION OF TEMP-CABLE-STEP = UP-DIRECTION
               ADD NUM-STEPS OF TEMP-CABLE-STEP TO NAVX-PREV
                   GIVING NAVX-POST
               ADD NUM-STEPS OF TEMP-CABLE-STEP TO NAVY-PREV
                   GIVING NAVY-POST
               ELSE
               SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM NAVX-PREV
                   GIVING NAVX-POST
               SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM NAVY-PREV
                   GIVING NAVY-POST
               END-IF
      *    only NAVX-POST XOR NAVY-POST are relevant, only NAVY XOR NAVX
      *    shall be modified
               PERFORM UNTIL NAVX = NAVX-POST OR NAVY = NAVY-POST
     
                   IF DIRECTION OF TEMP-CABLE-STEP = RIGHT-DIRECTION
                       ADD 1 TO NAVX
                   END-IF
     
                   IF DIRECTION OF TEMP-CABLE-STEP = UP-DIRECTION
                       ADD 1 TO NAVY
                   END-IF
                    
                   IF DIRECTION OF TEMP-CABLE-STEP = LEFT-DIRECTION
                       SUBTRACT 1 FROM NAVX
                   END-IF
     
                   IF DIRECTION OF TEMP-CABLE-STEP = DOWN-DIRECTION
                       SUBTRACT 1 FROM NAVY
                   END-IF
     
                   SET SEARCHINDEX TO 1
                   SEARCH SET-ENTRY OF GRIDSET
                       VARYING SEARCHINDEX
                       AT END
      *                This was not crossed by cable 1
                           DISPLAY "NOPE"
                       WHEN ( X-COORD OF SET-ENTRY(SEARCHINDEX) = NAVX )
                           AND
                           ( Y-COORD OF SET-ENTRY(SEARCHINDEX) = NAVY )
      *                This was crossed by cable 1, add it to new list
                           MOVE CH2 TO CHAR OF INSERTSETENTRY2
                           MOVE NAVX TO X-COORD OF INSERTSETENTRY2
                           MOVE NAVY TO Y-COORD OF INSERTSETENTRY2
                           DISPLAY 
                           "FOUND VISITED BY BOTH: ("NAVX", "NAVY")"
                           PERFORM INSERTION
                   END-SEARCH
                END-PERFORM
               END-IF
           END-PERFORM.

       DISPLAY "MATCHES: "GRIDSET1AND2.
      * compute steps-sum of each match, keep the minimum
           SET STEPS-SUM TO 10000000
           PERFORM VARYING LOOP-CTR
               FROM 1 BY 1 UNTIL LOOP-CTR = LATEST-INSERT2
               SET VAL-A TO X-COORD OF SET-ENTRY2(LOOP-CTR)
               SET VAL-B TO Y-COORD OF SET-ENTRY2(LOOP-CTR)
      * VAL-A and VAL-B are inputs to Procedure COUNT-STEPS. 
      * As are the cables stored in CABLE-ONE and CABLE-TWO
      * Output will
      * be in SS-NUM-STEPS
               PERFORM COUNT-STEPS
               SET STEPS-SUM TO FUNCTION MIN ( STEPS-SUM SS-NUM-STEPS )
      * no need to track which intersection it was, because we only need
      * to know the combined sum of steps for the best intersection
           END-PERFORM.

           DISPLAY "MINIMUM STEPS-SUM: "STEPS-SUM.

       STOP RUN.

       INSERTION.
           SET SEARCHINDEX2 TO 1.
           SEARCH SET-ENTRY2 OF GRIDSET1AND2
               AT END
                   ADD 1 TO LATEST-INSERT2 OF GRIDSET1AND2
                   MOVE INSERTSETENTRY2 TO SET-ENTRY2(LATEST-INSERT2)
               WHEN ( X-COORD OF INSERTSETENTRY2 = X-COORD OF
                       SET-ENTRY2(SEARCHINDEX2)
                   AND Y-COORD OF INSERTSETENTRY2 = Y-COORD OF
                       SET-ENTRY2(SEARCHINDEX2))
                   MOVE CHAR OF INSERTSETENTRY2 TO
                       CHAR OF SET-ENTRY2(SEARCHINDEX2)
           END-SEARCH.

       COUNT-STEPS-INNER-COMPUTATION.
           SET SS-CROSSES TO 1.
      *     It crosses here. Compute the Distance Moved
           SUBTRACT VAL-A FROM SS-CURR-X GIVING SS-VAL-U.
           SUBTRACT VAL-B FROM SS-CURR-Y GIVING SS-VAL-V.
      *     One of these is zero anyways
           SET SS-VAL-U TO FUNCTION ABS ( SS-VAL-U ).
           SET SS-VAL-V TO FUNCTION ABS ( SS-VAL-V ).
           ADD SS-VAL-U TO SS-VAL-V GIVING SS-VAL-W.
      *     SS-VAL-W is now the distance moved
           ADD SS-VAL-W TO SS-COUNTER.
      *     Update current position
           SET SS-CURR-X TO VAL-A.
           SET SS-CURR-Y TO VAL-B.

       
       COUNT-STEPS.
      * Inputs are VAL-A as x coord , VAL-B as y coord
      *     CABLE-ONE and CABLE-TWO
           SET SS-NUM-STEPS TO 0.
      * Get next step, and next, and next...
           SET SS-INDEX TO 0.
           SET SS-CURR-X TO CENTEER.
           SET SS-CURR-Y TO CENTEER.
           SET SS-SHOULD-STOP TO 0.
           PERFORM UNTIL LOOP-CTR > 1000 OR SS-SHOULD-STOP = 1
               MOVE CONE-STUFF(SS-INDEX) TO TEMP-CABLE-STEP
               DISPLAY "Lp 1: "LOOP-CTR",  Cur Best Sum: "STEPS-SUM
               PERFORM COUNT-STEPS-ANOTHER-INNER
               ADD SS-COUNTER TO SS-NUM-STEPS
      *     SS-COUNTER WAS SET BY THE PERFORM
               END-PERFORM.
           SET SS-INDEX TO 0.
           SET SS-CURR-X TO CENTEER.
           SET SS-CURR-Y TO CENTEER.
           SET SS-SHOULD-STOP TO 0.
      * Also for the second cable. Sum together.
           PERFORM UNTIL LOOP-CTR > 1000 OR SS-SHOULD-STOP = 1
               MOVE CTWO-STUFF(SS-INDEX) TO TEMP-CABLE-STEP
               DISPLAY "Lp 2: "LOOP-CTR",  Cur Best Sum: "STEPS-SUM
               PERFORM COUNT-STEPS-ANOTHER-INNER
               ADD SS-COUNTER TO SS-NUM-STEPS
      *     SS-COUNTER WAS SET BY THE PERFORM
               END-PERFORM.

       COUNT-STEPS-ANOTHER-INNER.
      * Set Counter to zero
           SET SS-COUNTER TO 0.
      * Check whether the next step crosses the goal field
           SET SS-CROSSES TO 0.

           IF DIRECTION OF TEMP-CABLE-STEP = RIGHT-DIRECTION
               ADD SS-CURR-X TO NUM-STEPS OF TEMP-CABLE-STEP
                   GIVING SS-VAL-Q
               IF VAL-A <= SS-VAL-Q AND VAL-A >= SS-CURR-X AND VAL-B =
                   SS-CURR-Y
                   PERFORM COUNT-STEPS-INNER-COMPUTATION
               ELSE
      *             Update current position
                   SET SS-CURR-X TO SS-VAL-Q
      *             Update Counter
                   ADD NUM-STEPS OF TEMP-CABLE-STEP TO SS-COUNTER
               END-IF
           END-IF.
     
           IF DIRECTION OF TEMP-CABLE-STEP = UP-DIRECTION
               ADD SS-CURR-Y TO NUM-STEPS OF TEMP-CABLE-STEP
                   GIVING SS-VAL-Q
               IF VAL-B <= SS-VAL-Q AND VAL-B >= SS-CURR-Y AND VAL-A =
                   SS-CURR-X
                   PERFORM COUNT-STEPS-INNER-COMPUTATION
               ELSE
                   SET SS-CURR-Y TO SS-VAL-Q
                   ADD NUM-STEPS OF TEMP-CABLE-STEP TO SS-COUNTER
               END-IF
           END-IF.

           IF DIRECTION OF TEMP-CABLE-STEP = LEFT-DIRECTION
               SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM SS-CURR-X
                   GIVING SS-VAL-Q
               IF VAL-A >= SS-VAL-Q AND VAL-A <= SS-CURR-X AND VAL-B =
                   SS-CURR-Y
                   PERFORM COUNT-STEPS-INNER-COMPUTATION
               ELSE
                   SET SS-CURR-X TO SS-VAL-Q
                   ADD NUM-STEPS OF TEMP-CABLE-STEP TO SS-COUNTER
               END-IF
           END-IF.
     
           IF DIRECTION OF TEMP-CABLE-STEP = DOWN-DIRECTION
               SUBTRACT NUM-STEPS OF TEMP-CABLE-STEP FROM SS-CURR-Y
                   GIVING SS-VAL-Q
               IF VAL-B >= SS-VAL-Q AND VAL-B <= SS-CURR-Y AND VAL-A =
                   SS-CURR-X
                   PERFORM COUNT-STEPS-INNER-COMPUTATION
               ELSE
                   SET SS-CURR-Y TO SS-VAL-Q
                   ADD NUM-STEPS OF TEMP-CABLE-STEP TO SS-COUNTER
               END-IF
           END-IF.
      * Increment step
           ADD 1 TO SS-INDEX.
      * Set early-abort condition
           IF VAL-B = SS-CURR-Y AND VAL-A = SS-CURR-X
               SET SS-SHOULD-STOP TO 1
           END-IF.


