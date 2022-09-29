       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALESWITHCOMMISSION.
       AUTHOR.     PEGGY FISHER.
      ***************************************************************
      *  This program reads a file containing sales person yearly   *
      *   sales information and prints a report.                    *
      ***************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
       OBJECT-COMPUTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT SALESFILE ASSIGN TO "SALES.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PRINT-FILE ASSIGN TO "SALESREPORT.DAT".
            SELECT COMM-FILE ASSIGN TO "COMMISION.DAT".


       DATA DIVISION.
       FILE SECTION.
       FD SALESFILE.

       01 SALESDETAILS.
            88 ENDOFSALES VALUE HIGH-VALUES.
            05 SALESPERSON-ID       PIC 9(5).
            05 SALESPERSON-NAME.
                10 LASTNAME         PIC X(20).
                10 FIRSTNAME        PIC X(20).
            05 REGION               PIC X(5).
            05 YEARLYSALES          PIC 9(6).
            05 GENDER               PIC X.

        FD PRINT-FILE.

        01  PRINT-LINE             PIC X(132).

        FD COMM-FILE.

        01  COMM-LINE              PIC X(132).

        WORKING-STORAGE SECTION.
        01  WS-FIELDS.
            05 WS-TOTAL-SALES      PIC 9(10) COMP-3 VALUE ZEROES.
            05 WS-COMMISSION-RATE  PIC V99 VALUE .05.
            05 WS-COMMISSION-AMT   PIC 9(10) COMP-3 VALUES ZEROES.
            05 WS-TOTAL-COMM-AMT   PIC 9(10) COMP-3 VALUES ZEROES.

        01  WS-REGION-SALES.
            05 WS-EAST             PIC 9(7) VALUE ZEROES.
            05 WS-WEST             PIC 9(7) VALUE ZEROES.
            05 WS-NORTH            PIC 9(7) VALUE ZEROES.
            05 WS-SOUTH            PIC 9(7) VALUE ZEROES.

        01  HEADING-LINE.
            05 FILLER              PIC X(5) VALUE SPACES.
            05 FILLER              PIC X(16) VALUE 'SALESPERSON NAME'.
            05 FILLER              PIC X(29) VALUE SPACES.
            05 FILLER              PIC X(6)  VALUE 'REGION'.
            05 FILLER              PIC X(10) VALUE SPACES.
            05 FILLER              PIC X(12) VALUE 'YEARLY SALES'.
            05 FILLER              PIC X(73) VALUE SPACES.

        01  DETAIL-LINE.
            05 FILLER               PIC X(5)  VALUE SPACES.
            05 DET-SALESPERSON-NAME PIC X(40).
            05 FILLER               PIC X(5)  VALUE SPACES.
            05 DET-REGION           PIC X(5).
            05 FILLER               PIC X(10)  VALUE SPACES.
            05 DET-YEARLYSALES      PIC X(12).
            05 FILLER               PIC X(40)  VALUE SPACES.

        01  TOTAL-LINE.
            05 FILLER               PIC X(5)   VALUE SPACES.
            05 FILLER               PIC X(16)  VALUE SPACES.
            05 FILLER               PIC X(10)  VALUE SPACES.
            05 FILLER               PIC X(6)   VALUE SPACES.
            05 FILLER               PIC X(10)  VALUE SPACES.
            05 TOTAL-YRLY-SALES     PIC X(12).
            05 FILLER               PIC X(73)  VALUE SPACES.

        01  WS-CURRENT-DATE-FIELDS.
            05 WS-CURRENT-DATE.
                10  WS-CURRENT-YEAR    PIC  9(4).
                10  WS-CURRENT-MONTH   PIC  9(2).
                10  WS-CURRENT-DAY     PIC  9(2).
            05  WS-CURRENT-TIME.
                10  WS-CURRENT-HOUR    PIC  9(2).
                10  WS-CURRENT-MINUTE  PIC  9(2).
                10  WS-CURRENT-SECOND  PIC  9(2).
                10  WS-CURRENT-MS      PIC  9(2).
            05  WS-DIFF-FROM-GMT       PIC S9(4).

        01  COMM-HEADING-LINE-1.
            05 FILLER              PIC X(25) VALUE SPACES.
            05 FILLER              PIC X(16) VALUE 'COMMISION REPORT'.
            05 FILLER              PIC X(25) VALUE SPACES.
            05 COMM-HEADING-DATE.
               10 COMM-HEADING-DAY   PIC X(2).
               10 FILLER             PIC X(1) VALUE '/'.         
               10 COMM-HEADING-MONTH PIC X(2).
               10 FILLER             PIC X(1) VALUE '/'.
               10 COMM-HEADING-YEAR  PIC X(4).
            05 FILLER                PIC X(56) VALUE SPACES.

        01  COMM-HEADING-LINE-2.
            05 FILLER PIC X(132) VALUE ALL '*'.

        01  COMM-HEADING-LINE-3.
            05 FILLER              PIC X(5)  VALUE SPACES.
            05 FILLER              PIC X(16) VALUE 'SALESPERSON NAME'.
            05 FILLER              PIC X(29) VALUE SPACES.
            05 FILLER              PIC X(17) VALUE 'COMMISION PERCENT'.
            05 FILLER              PIC X(10) VALUE SPACES.
            05 FILLER              PIC X(16) VALUE 'COMMISION AMOUNT'.
            05 FILLER              PIC x(39) VALUE SPACES.

        01  COMM-DETAIL-LINE.
            05 FILLER                PIC X(5)  VALUE SPACES.
            05 COMM-SALESPERSON-NAME PIC X(40).
            05 FILLER                PIC X(5)  VALUE SPACES.
            05 COMM-AMT-PERCT        PIC .99.
            05 FILLER                PIC X(24)  VALUE SPACES.
            05 COMM-AMT              PIC $$$,$$$,$$$,$$$.
            05 FILLER                PIC X(29)  VALUE SPACES. 

        01  COMM-TOTAL-LINE.
            05 FILLER               PIC X(5)   VALUE SPACES.
            05 FILLER               PIC X(16)  VALUE SPACES.
            05 FILLER               PIC X(10)  VALUE SPACES.
            05 FILLER               PIC X(6)   VALUE SPACES.
            05 FILLER               PIC X(10)  VALUE SPACES.
            05 TOTAL-COMM           PIC $$$,$$$,$$$,$$$.
            05 FILLER               PIC X(73)  VALUE SPACES.

        PROCEDURE DIVISION.

        0050-OPEN-FILE.
           OPEN INPUT SALESFILE.
           OPEN OUTPUT PRINT-FILE.
           OPEN OUTPUT COMM-FILE.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0200-STOP-RUN.

        0100-PROCESS-RECORDS.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR TO COMM-HEADING-YEAR.
           MOVE WS-CURRENT-MONTH TO COMM-HEADING-MONTH.
           MOVE WS-CURRENT-DAY TO COMM-HEADING-DAY.
           MOVE WS-COMMISSION-RATE TO COMM-AMT-PERCT.
           PERFORM 0110-WRITE-HEADING-LINE.
           PERFORM 0115-WRITE-COMM-HEADING-LINE.
           READ SALESFILE
                AT END SET ENDOFSALES TO TRUE
                END-READ.
           PERFORM UNTIL ENDOFSALES
            ADD YEARLYSALES TO WS-TOTAL-SALES
            MOVE SALESPERSON-NAME TO DET-SALESPERSON-NAME
                                     COMM-SALESPERSON-NAME
            MOVE REGION TO DET-REGION
            MOVE YEARLYSALES TO DET-YEARLYSALES
            COMPUTE WS-COMMISSION-AMT = WS-COMMISSION-RATE *
              YEARLYSALES
            COMPUTE WS-TOTAL-COMM-AMT = WS-TOTAL-COMM-AMT +
              WS-COMMISSION-AMT
            MOVE WS-COMMISSION-AMT TO COMM-AMT
            PERFORM 0120-WRITE-DETAIL-LINE
            PERFORM 0125-WRITE-COMM-DETAIL-LINE

            READ SALESFILE
            AT END SET ENDOFSALES TO TRUE
            END-READ
           END-PERFORM.
           PERFORM 0130-WRITE-TOTAL-LINE.
           PERFORM 0135-WRITE-COMM-TOTAL-LINE.

        0110-WRITE-HEADING-LINE.
            MOVE HEADING-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
            MOVE SPACES TO PRINT-LINE.
            WRITE PRINT-LINE.

        0115-WRITE-COMM-HEADING-LINE.
            MOVE COMM-HEADING-LINE-1 TO COMM-LINE.
            WRITE COMM-LINE AFTER ADVANCING 1 LINE.
            MOVE COMM-HEADING-LINE-2 TO COMM-LINE.
            WRITE COMM-LINE.
            MOVE COMM-HEADING-LINE-3 TO COMM-LINE.
            WRITE COMM-LINE AFTER ADVANCING 1 LINE.
            MOVE SPACES TO COMM-LINE.
            WRITE COMM-LINE.

        0120-WRITE-DETAIL-LINE.
            MOVE DETAIL-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

        0125-WRITE-COMM-DETAIL-LINE.
            MOVE COMM-DETAIL-LINE TO COMM-LINE.
            WRITE COMM-LINE AFTER ADVANCING 1 LINE.

        0130-WRITE-TOTAL-LINE.
            MOVE WS-TOTAL-SALES TO TOTAL-YRLY-SALES.
            MOVE TOTAL-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

        0135-WRITE-COMM-TOTAL-LINE.
            MOVE WS-TOTAL-COMM-AMT TO TOTAL-COMM.
            MOVE COMM-TOTAL-LINE TO COMM-LINE.
            WRITE COMM-LINE AFTER ADVANCING 1 LINE.

        0200-STOP-RUN.
           CLOSE SALESFILE.
           CLOSE PRINT-FILE.
           CLOSE COMM-FILE.
           STOP RUN.

          END PROGRAM SALESWITHCOMMISSION.
