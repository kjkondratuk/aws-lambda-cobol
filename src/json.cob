       >>SOURCE FORMAT IS FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-PARSE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
       OBJECT-COMPUTER.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STANDARD-INPUT ASSIGN TO KEYBOARD.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       FD  STANDARD-INPUT.
       01  STDIN-RECORD                         PIC X(32768).
       WORKING-STORAGE SECTION.
      **************************************************************************
      * STRUCTURES FOR WORKING WITH FILES (INPUT)
      **************************************************************************
       01  WS-FILE-STATUS                       PIC X VALUE SPACE.
           88 END-OF-FILE                             VALUE HIGH-VALUE.
           88 NOT-EOF                                 VALUE LOW-VALUE.
       01  WS-REC                               PIC 9(4) VALUE 0.
       01  WS-LINE                              PIC X(2048).

      **************************************************************************
      * STRUCTURES FOR WORKING WITH THE JSON PARSER
      **************************************************************************
       01  WS-JSON-BLOB.
           05  WS-JSON-BLOB-DATA                PIC X(2048).
       01  WS-JSON-ROOT                         USAGE POINTER.
       01  WS-JSON-OBJECT                       USAGE POINTER.
       01  WS-JSON-FIELD                        USAGE POINTER.

       01  WS-JSON                              BASED.
           05  WS-JSON-NEXT                     USAGE POINTER.
           05  WS-JSON-PREV                     USAGE POINTER.
           05  WS-JSON-CHILD                    USAGE POINTER.
           05  WS-JSON-TYPE                     USAGE BINARY-LONG SYNC.
           05  WS-VALUESTRING                   USAGE POINTER SYNC.
           05  WS-VALUEINT                      USAGE BINARY-LONG SYNC.
           05  WS-VALUEDOUBLE                   USAGE FLOAT-LONG SYNC.
           05  WS-JSON-NAME                     USAGE POINTER SYNC.

       01  WS-INPUT-LEN                         PIC 9(4).
       01  WS-RESPONSE-BODY                     PIC X(2048).
      **************************************************************************
      * THE STRUCTURE FOR THE REQUEST THAT'S SENT TO THE LAMBDA
      **************************************************************************
       01  WS-REQUEST.
           05  WS-N1                            PIC 9(4).
           05  WS-N2                            PIC 9(4).

      **************************************************************************
      * THE STRUCTURE FOR THE RESPONSE THAT'S RETURNED FROM THE LAMBDA
      **************************************************************************
       01  WS-RESPONSE.
           05  WS-STATUS-CODE                   PIC 9(3) VALUE 200.
               88 STATUS-OK                     VALUE 200.
               88 STATUS-BAD-REQUEST            VALUE 400.
               88 STATUS-SERVER-ERROR           VALUE 500.
           05  WS-RESULT                        PIC 9(4).

       01  WS-TOTAL                             PIC Z(5) USAGE DISPLAY.

       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       REPORT SECTION.
       SCREEN SECTION.

      *PROCEDURE DIVISION CHAINING WS-JSON-BLOB.
       PROCEDURE DIVISION.
      **************************************************************************
      * 1000-MAIN
      **************************************************************************
      * THE MAIN PARAGRAPH
      **************************************************************************
       1000-MAIN.

           OPEN INPUT STANDARD-INPUT.
           READ STANDARD-INPUT AT END SET END-OF-FILE TO TRUE END-READ.
           PERFORM VARYING WS-REC FROM 1 BY 1 UNTIL END-OF-FILE
               MOVE STDIN-RECORD TO WS-LINE
      *         DISPLAY "DATA: " FUNCTION TRIM(WS-LINE)
               MOVE FUNCTION CONCATENATE(
                   FUNCTION TRIM(WS-JSON-BLOB-DATA)
                   FUNCTION TRIM(WS-LINE)) TO WS-JSON-BLOB-DATA
      *         DISPLAY "BLOB DATA: " FUNCTION TRIM(WS-JSON-BLOB-DATA)
               READ STANDARD-INPUT AT END SET END-OF-FILE
                   TO TRUE END-READ
           END-PERFORM.
           CLOSE STANDARD-INPUT.

      *    PRINT SOME DIAGNOSTIC INFORMATION TO THE LOGS ABOUT THE INPUT
           DISPLAY "LENGTH: " LENGTH OF WS-JSON-BLOB-DATA UPON STDERR.
           INSPECT FUNCTION REVERSE(WS-JSON-BLOB-DATA)
               TALLYING WS-INPUT-LEN FOR LEADING SPACES.
           DISPLAY "SPACES: " WS-INPUT-LEN UPON STDERR.
           DISPLAY "DATA: " FUNCTION TRIM(WS-JSON-BLOB-DATA)
               UPON STDERR.

      *    *********************************************************************
      *    WANTED TO USE JSON PARSE, BUT IT'S NOT IMPLEMENTED YET
      *    *********************************************************************
      *    JSON PARSE WS-JSON-BLOB-DATA INTO WS-REQUEST
      *        NAME WS-N1 'n1'
      *             WS-N2 'n2'
      *        ON EXCEPTION SET STATUS-BAD-REQUEST TO TRUE.
           CALL STATIC "cJSON_Parse" USING WS-JSON-BLOB
               RETURNING WS-JSON-ROOT.

      *    THERE WAS AN ERROR PARSING THE JSON PROVIDED, SO RETURN A
      *    HTTP 400 TO THE REQUESTOR.
           IF WS-JSON-ROOT EQUAL NULL THEN
               SET STATUS-BAD-REQUEST TO TRUE
           ELSE

      * TODO: THE PROBLEM HERE IS THAT WE ARE

      *        Parse the first numeric value out of the request
               CALL STATIC "cJSON_GetObjectItem" USING
                   BY VALUE WS-JSON-ROOT
                   BY REFERENCE "n1"
                   RETURNING WS-JSON-FIELD
               END-CALL
               IF WS-JSON-FIELD NOT EQUAL NULL THEN
                   SET ADDRESS OF WS-JSON TO WS-JSON-FIELD
                   DISPLAY "n1: " WS-VALUEINT UPON STDERR
                   MOVE WS-VALUEINT TO WS-N1
               ELSE
                   SET STATUS-BAD-REQUEST TO TRUE
                   DISPLAY "Could not locate field [n1] on request!"
                       UPON STDERR
               END-IF

               CALL STATIC "cJSON_GetObjectItem" USING
                   BY VALUE WS-JSON-ROOT
                   BY REFERENCE "n2"
                   RETURNING WS-JSON-FIELD
               END-CALL
               IF WS-JSON-FIELD NOT EQUAL NULL THEN
                   SET ADDRESS OF WS-JSON TO WS-JSON-FIELD
                   DISPLAY "n2: " WS-VALUEINT UPON STDERR
                   MOVE WS-VALUEINT TO WS-N2
               ELSE
                   SET STATUS-BAD-REQUEST TO TRUE
                   DISPLAY "Could not locate field [n2] on request!"
                       UPON STDERR
               END-IF
           END-IF.

      *     DISPLAY "ROOT: " WS-JSON-ROOT UPON STDERR.
      *     DISPLAY "VALUE AT ROOT: " FUNCTION TRIM(WS-JSON-BLOB)
      *         UPON STDERR.
           ADD WS-N1 TO WS-N2 GIVING WS-TOTAL.

           PERFORM 2000-PREPARE-RESPONSE
              THRU 2000-PREPARE-RESPONSE-EXIT.

           DISPLAY "Writing out: " FUNCTION TRIM(WS-RESPONSE-BODY)
               UPON STDERR
           DISPLAY FUNCTION TRIM(WS-RESPONSE-BODY) UPON STDOUT.

       1000-MAIN-END. STOP RUN.

      **************************************************************************
      * 2000-PREPARE-RESPONSE
      **************************************************************************
      * GENERATES JSON FOR THE RESPONSE AND TRIMS TOP LEVEL IDENTIFIER INFO
      * TO FORMAT THE RESPONSE PROPERLY FOR THE WEB.
      **************************************************************************
       2000-PREPARE-RESPONSE.
           MOVE WS-TOTAL TO WS-RESULT.
           JSON GENERATE WS-RESPONSE-BODY FROM WS-RESPONSE
               NAME WS-STATUS-CODE 'statusCode'
                    WS-RESULT        'result'
               ON EXCEPTION SET STATUS-SERVER-ERROR TO TRUE.

           MOVE WS-RESPONSE-BODY(16:) TO WS-RESPONSE-BODY.
           MOVE FUNCTION REVERSE(WS-RESPONSE-BODY) TO WS-RESPONSE-BODY.
           MOVE FUNCTION TRIM(WS-RESPONSE-BODY)(2:) TO WS-RESPONSE-BODY.
           MOVE FUNCTION REVERSE(WS-RESPONSE-BODY) TO WS-RESPONSE-BODY.
           MOVE FUNCTION TRIM(WS-RESPONSE-BODY) TO WS-RESPONSE-BODY.

       2000-PREPARE-RESPONSE-EXIT. EXIT.

       END PROGRAM JSON-PARSE.
