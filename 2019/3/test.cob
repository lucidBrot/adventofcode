      * Required: Info about the program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. LUCID.
      * INPUT / OUTPUT and such
       ENVIRONMENT DIVISION.
      * Sections are parts of a division:
       CONFIGURATION SECTION.
      ** SOURCE-COMPUTER. XXX.
      ** OBJECT-COMPUTER. XXX
       INPUT OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILENAME 
      * Constants
       DATA DIVISION.
        * File, Working-storage for temp variables, Local-Storage for allocated variables, Linkage
      * Executable Code
       PROCEDURE DIVISION.
           DISPLAY 'Hello World!'.
           STOP RUN.
