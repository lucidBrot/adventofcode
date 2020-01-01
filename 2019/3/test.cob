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
      * Constants
      * File, Working-storage for temp variables, Local-Storage for allocated variables, Linkage
      * Executable Code
       PROCEDURE DIVISION.
           DISPLAY 'Hello World!'.
           OPEN Input MYINPUTFILE.
           STOP RUN.
