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
       SELECT MYINPUTFILE ASSIGN TO 'input.txt'
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

       01 CABLE-ONE.
           02 STUFF OCCURS 1000 TIMES.
               05 DIRECTION PIC A(1) VALUE 'Z'.
               05 NUM-STEPS PIC 9(4) VALUE 0.

       01 CABLE-TWO.
           02 STUFF OCCURS 1000 TIMES.
               05 DIRECTION PIC A(1) VALUE 'Z'.
               05 NUM-STEPS PIC 9(4) VALUE 0.

      * Executable Code
       PROCEDURE DIVISION.
           DISPLAY WS-SOMETHING.
           DISPLAY 'Hello World!'.
      *    Specify Grid Size
           OPEN Input MYINPUTFILE.
           PERFORM UNTIL WS-EOF='Y'
               READ MYINPUTFILE INTO WS-CONTENT
                   AT END MOVE 'Y' TO WS-EOF
      *            Invalidly structured data is printed empty when using
      *            WS-SOME-NAME but is printed entirely when using
      *            WS-CONTENT
                   NOT AT END DISPLAY WS-SOME-NAME
               END-READ
           END-PERFORM.
           CLOSE MYINPUTFILE.
           STOP RUN.
