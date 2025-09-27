*> Use free source format
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DataEntryApp.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS ws-kb-status.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT customer-file ASSIGN TO "data/customers.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS fs.
           SELECT audit-file ASSIGN TO "data/audit.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS fs-audit.
           SELECT temp-file ASSIGN TO "data/customers.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS fs-temp.
           SELECT import-file ASSIGN TO ws-import-path
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS fs-import.
           SELECT idx-file ASSIGN TO "data/customers.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS idx-id
               FILE STATUS IS fs-idx.

       DATA DIVISION.
       FILE SECTION.
       FD  customer-file.
       01  customer-record                 PIC X(256).
       FD  audit-file.
       01  audit-record                    PIC X(256).
       FD  temp-file.
       01  temp-record                     PIC X(256).
       FD  import-file.
       01  import-record                   PIC X(256).
       FD  idx-file.
       01  idx-record.
           05 idx-id                        PIC X(9).
           05 idx-name                      PIC X(50).
           05 idx-email                     PIC X(60).
           05 idx-age                       PIC 9(3).

       WORKING-STORAGE SECTION.
       01  fs                              PIC XX.
       01  file-exists                     PIC X      VALUE "N".
       01  ws-continue                     PIC X      VALUE "Y".

       01  ws-id-text                      PIC X(9).
       01  ws-name                         PIC X(50).
       01  ws-email                        PIC X(60).
       01  ws-age-text                     PIC X(3).

       01  pos-at                          PIC 9(9)   VALUE 0.
       01  pos-dot                         PIC 9(9)   VALUE 0.
       01  trimmed-len                     PIC 9(9)   VALUE 0.
       01  num-id                          PIC 9(9)   VALUE 0.
       01  num-age                         PIC 9(3)   VALUE 0.
       01  ws-id-trim                      PIC X(9).
       01  ws-age-trim                     PIC X(3).
       01  id-digit-count                  PIC 9(9)   VALUE 0.
       01  age-digit-count                 PIC 9(9)   VALUE 0.
       01  len-id                          PIC 9(9)   VALUE 0.
       01  len-age                         PIC 9(9)   VALUE 0.
       01  i                                PIC 9(9)   VALUE 0.
       01  email-len                        PIC 9(9)   VALUE 0.
       01  space-count                      PIC 9(9)   VALUE 0.
       01  comma-count                      PIC 9(9)   VALUE 0.
       01  quote-count                      PIC 9(9)   VALUE 0.
       01  ws-screen-mode                   PIC X      VALUE "N".
       01  ws-status                        PIC X(60)  VALUE SPACES.
       01  ws-valid                         PIC X      VALUE "N".
       01  ws-title-color                   PIC 9      VALUE 7.
       01  ws-title-bg                      PIC 9      VALUE 1.
       01  ws-label-color                   PIC 9      VALUE 7.
       01  ws-input-color                   PIC 9      VALUE 7.
       01  ws-status-color                  PIC 9      VALUE 7.
       01  ws-id-color                      PIC 9      VALUE 7.
       01  ws-name-color                    PIC 9      VALUE 7.
       01  ws-email-color                   PIC 9      VALUE 7.
       01  ws-age-color                     PIC 9      VALUE 7.

       *> Theme support
       01  ws-theme                          PIC X(16)  VALUE SPACES.
       01  ws-theme-label                    PIC X(10)  VALUE "DEFAULT".
       01  ws-batch-mode                    PIC X      VALUE "N".
       01  arg-count                        PIC 9(4)   VALUE 0.
       01  arg1                             PIC X(32).
       01  ws-email-trim                    PIC X(60).
       01  need-header                      PIC X      VALUE "N".

       *> Key and duplicate/audit support
       01  ws-kb-status                     PIC 9(4)   VALUE 0.
       01  ws-dup-found                     PIC X      VALUE "N".
       01  ws-scan-id                       PIC X(32).
       01  ws-scan-id-trim                  PIC X(32).

       *> Audit logging
       01  fs-audit                         PIC XX.
       01  ws-date                          PIC 9(8).
       01  ws-time                          PIC 9(6).
       01  ws-date-a                        PIC X(8).
       01  ws-time-a                        PIC X(6).

       *> Key constants (common ncurses codes; will also show unknown codes in status)
       01  k-esc                            PIC 9(4)   VALUE 27.
       01  k-f1                             PIC 9(4)   VALUE 265.
       01  k-f5                             PIC 9(4)   VALUE 269.
       01  k-f10                            PIC 9(4)   VALUE 274.

       *> Edit/Search support
       01  ws-edit-mode                     PIC X      VALUE "N".
       01  ws-edit-id                       PIC X(32).
       01  ws-edit-screen-mode              PIC X      VALUE "N".
       01  ws-search-id                     PIC X(32).
       01  ws-search-name                   PIC X(60).
       01  ws-found                         PIC X      VALUE "N".

       *> Parsing buffers
       01  r-id                             PIC X(32).
       01  r-name                           PIC X(50).
       01  r-email                          PIC X(60).
       01  r-age                            PIC 9(3).

       *> Name matching helpers
       01  ws-name-up                       PIC X(60).
       01  ws-key-up                        PIC X(60).
       01  l-name                           PIC 9(3)   VALUE 0.
       01  l-key                            PIC 9(3)   VALUE 0.
       01  end-pos                          PIC 9(3)   VALUE 0.
       01  j                                PIC 9(9)   VALUE 0.
       01  ws-match                         PIC X      VALUE "N".
       01  ws-start-key                     PIC X(9).

       *> Temp file status
       01  fs-temp                          PIC XX.

       *> Indexed file status
       01  fs-idx                           PIC XX.

       *> Storage selection
       01  ws-storage                       PIC X(16)  VALUE "CSV".
       01  ws-storage-indexed               PIC X      VALUE "N".
       01  ws-storage-label                 PIC X(10)  VALUE "CSV".

       *> Home menu
       01  ws-home-mode                     PIC X      VALUE "N".
       01  ws-menu-choice                   PIC X      VALUE SPACE.

       *> Import mode
       01  ws-import-mode                   PIC X      VALUE "N".
       01  ws-import-path                   PIC X(256).
       01  fs-import                        PIC XX.
       01  ws-added-count                   PIC 9(9)   VALUE 0.
       01  ws-updated-count                 PIC 9(9)   VALUE 0.
       01  ws-failed-count                  PIC 9(9)   VALUE 0.
       01  ws-line-number                   PIC 9(9)   VALUE 0.

       *> LIST mode support
       01  ws-list-screen-mode               PIC X      VALUE "N".
       01  ws-filter-name                    PIC X(60).
       01  ws-filter-domain                  PIC X(60).
       01  ws-filter-age-min-txt             PIC X(3).
       01  ws-filter-age-max-txt             PIC X(3).
       01  ws-filter-age-min                 PIC 9(3)   VALUE 0.
       01  ws-filter-age-max                 PIC 9(3)   VALUE 0.
       01  ws-list-cmd                       PIC X      VALUE SPACE.
       01  ws-page-size                      PIC 9(4)   VALUE 10.
       01  ws-page                           PIC 9(4)   VALUE 1.
       01  ws-total-pages                    PIC 9(4)   VALUE 1.
       01  ws-total-count                    PIC 9(4)   VALUE 0.
       01  ws-start-index                    PIC 9(4)   VALUE 1.
       01  ws-end-index                      PIC 9(4)   VALUE 0.

       01  ws-row-1                          PIC X(80).
       01  ws-row-2                          PIC X(80).
       01  ws-row-3                          PIC X(80).
       01  ws-row-4                          PIC X(80).
       01  ws-row-5                          PIC X(80).
       01  ws-row-6                          PIC X(80).
       01  ws-row-7                          PIC X(80).
       01  ws-row-8                          PIC X(80).
       01  ws-row-9                          PIC X(80).
       01  ws-row-10                         PIC X(80).

       01  tbl-count                         PIC 9(4)   VALUE 0.
       01  rec-table.
           05 rec-item OCCURS 500 TIMES.
              10 t-id                        PIC X(32).
              10 t-name                      PIC X(50).
              10 t-email                     PIC X(60).
              10 t-age                       PIC 9(3).

       SCREEN SECTION.
       01  entry-screen.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "COBOL Customer Entry"
                            FOREGROUND-COLOR IS ws-title-color
                            BACKGROUND-COLOR IS ws-title-bg
                            HIGHLIGHT.
           05 LINE 4  COLUMN 10 VALUE "ID:"    FOREGROUND-COLOR IS ws-label-color.
           05 LINE 4  COLUMN 20 PIC X(9)  USING ws-id-text
                            FOREGROUND-COLOR IS ws-id-color
                            REVERSE-VIDEO.
           05 LINE 5  COLUMN 10 VALUE "Name:"  FOREGROUND-COLOR IS ws-label-color.
           05 LINE 5  COLUMN 20 PIC X(50) USING ws-name
                            FOREGROUND-COLOR IS ws-name-color
                            REVERSE-VIDEO.
           05 LINE 6  COLUMN 10 VALUE "Email:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 6  COLUMN 20 PIC X(60) USING ws-email
                            FOREGROUND-COLOR IS ws-email-color
                            REVERSE-VIDEO.
           05 LINE 7  COLUMN 10 VALUE "Age:"   FOREGROUND-COLOR IS ws-label-color.
           05 LINE 7  COLUMN 20 PIC X(3)  USING ws-age-text
                            FOREGROUND-COLOR IS ws-age-color
                            REVERSE-VIDEO.
           05 LINE 9  COLUMN 10 VALUE "Enter=Save" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 10 COLUMN 10 VALUE "Status:"     FOREGROUND-COLOR IS ws-label-color.
           05 LINE 10 COLUMN 18 PIC X(60) USING ws-status
                            FOREGROUND-COLOR IS ws-status-color.
       01  cont-screen.
           05 LINE 12 COLUMN 10 VALUE "Add another? (Y/N):"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 12 COLUMN 31 PIC X USING ws-continue
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.

       01  help-screen.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "Help"
                            FOREGROUND-COLOR IS ws-title-color
                            BACKGROUND-COLOR IS ws-title-bg
                            HIGHLIGHT.
           05 LINE 4  COLUMN 10 VALUE "Keys:"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 5  COLUMN 12 VALUE "Enter = Save"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 6  COLUMN 12 VALUE "F5 = Save (same as Enter)"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 7  COLUMN 12 VALUE "ESC or F10 = Exit"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 8  COLUMN 12 VALUE "F1 or ? = Help"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 10 COLUMN 10 VALUE "Press any key to return"
                            FOREGROUND-COLOR IS ws-label-color.

       01  home-screen.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "COBOL Data App"
                            FOREGROUND-COLOR IS ws-title-color
                            BACKGROUND-COLOR IS ws-title-bg
                            HIGHLIGHT.
           05 LINE 3  COLUMN 10 VALUE "Storage:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 3  COLUMN 20 PIC X(10) USING ws-storage-label FOREGROUND-COLOR IS ws-status-color.
           05 LINE 3  COLUMN 35 VALUE "Theme:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 3  COLUMN 43 PIC X(10) USING ws-theme-label FOREGROUND-COLOR IS ws-status-color.
           05 LINE 4  COLUMN 10 VALUE "1) Add new record" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 5  COLUMN 10 VALUE "2) Edit existing record" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 6  COLUMN 10 VALUE "3) List records" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 7  COLUMN 10 VALUE "4) Import from CSV" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 9  COLUMN 10 VALUE "X) Exit" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 11 COLUMN 10 VALUE "Choice:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 11 COLUMN 18 PIC X USING ws-menu-choice
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 13 COLUMN 10 VALUE "Status:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 13 COLUMN 18 PIC X(60) USING ws-status
                            FOREGROUND-COLOR IS ws-status-color.

       01  import-path-screen.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "Import CSV path (Enter for default: data/import.csv)"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 4  COLUMN 10 PIC X(60) USING ws-import-path
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 6  COLUMN 10 VALUE "Enter=Run  ESC/F10=Cancel"
                            FOREGROUND-COLOR IS ws-label-color.

       01  search-screen.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "Search/Edit"
                            FOREGROUND-COLOR IS ws-title-color
                            BACKGROUND-COLOR IS ws-title-bg
                            HIGHLIGHT.
           05 LINE 4  COLUMN 10 VALUE "Search by ID:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 4  COLUMN 25 PIC X(32) USING ws-search-id
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 5  COLUMN 10 VALUE "or Name contains:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 5  COLUMN 28 PIC X(60) USING ws-search-name
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 7  COLUMN 10 VALUE "Enter=Search  ESC=Cancel"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 9  COLUMN 10 VALUE "Status:"     FOREGROUND-COLOR IS ws-label-color.
           05 LINE 9  COLUMN 18 PIC X(60) USING ws-status
                            FOREGROUND-COLOR IS ws-status-color.

       01  list-filter-screen.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "Customer List Filters"
                            FOREGROUND-COLOR IS ws-title-color
                            BACKGROUND-COLOR IS ws-title-bg
                            HIGHLIGHT.
           05 LINE 4  COLUMN 10 VALUE "Name contains:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 4  COLUMN 28 PIC X(60) USING ws-filter-name
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 5  COLUMN 10 VALUE "Email domain (e.g., example.com):"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 5  COLUMN 45 PIC X(60) USING ws-filter-domain
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 6  COLUMN 10 VALUE "Age min:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 6  COLUMN 19 PIC X(3) USING ws-filter-age-min-txt
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 6  COLUMN 26 VALUE "Age max:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 6  COLUMN 35 PIC X(3) USING ws-filter-age-max-txt
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 8  COLUMN 10 VALUE "Enter=Apply  ESC=Cancel"
                            FOREGROUND-COLOR IS ws-label-color.

       01  list-screen.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "Customer List"
                            FOREGROUND-COLOR IS ws-title-color
                            BACKGROUND-COLOR IS ws-title-bg
                            HIGHLIGHT.
           05 LINE 2  COLUMN 40 VALUE "Storage:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 2  COLUMN 49 PIC X(10) USING ws-storage-label FOREGROUND-COLOR IS ws-status-color.
           05 LINE 2  COLUMN 61 VALUE "Theme:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 2  COLUMN 68 PIC X(10) USING ws-theme-label FOREGROUND-COLOR IS ws-status-color.
           05 LINE 3  COLUMN 10 VALUE "Filters: domain=" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 3  COLUMN 28 PIC X(30) USING ws-filter-domain FOREGROUND-COLOR IS ws-status-color.
           05 LINE 3  COLUMN 60 VALUE " age=" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 3  COLUMN 66 PIC 9(3) USING ws-filter-age-min FOREGROUND-COLOR IS ws-status-color.
           05 LINE 3  COLUMN 70 VALUE "-" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 3  COLUMN 72 PIC 9(3) USING ws-filter-age-max FOREGROUND-COLOR IS ws-status-color.
           05 LINE 4  COLUMN 10 VALUE "ID        NAME                                 AGE  EMAIL"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 5  COLUMN 10 PIC X(80) USING ws-row-1  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 6  COLUMN 10 PIC X(80) USING ws-row-2  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 7  COLUMN 10 PIC X(80) USING ws-row-3  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 8  COLUMN 10 PIC X(80) USING ws-row-4  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 9  COLUMN 10 PIC X(80) USING ws-row-5  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 10 COLUMN 10 PIC X(80) USING ws-row-6  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 11 COLUMN 10 PIC X(80) USING ws-row-7  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 12 COLUMN 10 PIC X(80) USING ws-row-8  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 13 COLUMN 10 PIC X(80) USING ws-row-9  FOREGROUND-COLOR IS ws-input-color.
           05 LINE 14 COLUMN 10 PIC X(80) USING ws-row-10 FOREGROUND-COLOR IS ws-input-color.
           05 LINE 16 COLUMN 10 VALUE "N=Next  P=Prev  F=Filter  ESC/F10=Exit"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 17 COLUMN 10 VALUE "Page:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 17 COLUMN 17 PIC 9(4) USING ws-page FOREGROUND-COLOR IS ws-status-color.
           05 LINE 17 COLUMN 22 VALUE "/" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 17 COLUMN 24 PIC 9(4) USING ws-total-pages FOREGROUND-COLOR IS ws-status-color.
           05 LINE 17 COLUMN 32 VALUE "Total:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 17 COLUMN 39 PIC 9(4) USING ws-total-count FOREGROUND-COLOR IS ws-status-color.
           05 LINE 19 COLUMN 10 VALUE "Cmd:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 19 COLUMN 15 PIC X USING ws-list-cmd FOREGROUND-COLOR IS ws-input-color REVERSE-VIDEO.
           05 BLANK SCREEN BACKGROUND-COLOR 1.
           05 LINE 2  COLUMN 10 VALUE "Search/Edit"
                            FOREGROUND-COLOR IS ws-title-color
                            BACKGROUND-COLOR IS ws-title-bg
                            HIGHLIGHT.
           05 LINE 4  COLUMN 10 VALUE "Search by ID:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 4  COLUMN 25 PIC X(32) USING ws-search-id
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 5  COLUMN 10 VALUE "or Name contains:" FOREGROUND-COLOR IS ws-label-color.
           05 LINE 5  COLUMN 28 PIC X(60) USING ws-search-name
                            FOREGROUND-COLOR IS ws-input-color
                            REVERSE-VIDEO.
           05 LINE 7  COLUMN 10 VALUE "Enter=Search  ESC=Cancel"
                            FOREGROUND-COLOR IS ws-label-color.
           05 LINE 9  COLUMN 10 VALUE "Status:"     FOREGROUND-COLOR IS ws-label-color.
           05 LINE 9  COLUMN 18 PIC X(60) USING ws-status
                            FOREGROUND-COLOR IS ws-status-color.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INIT-FILE
           PERFORM READ-ARGS
           PERFORM APPLY-THEME
           IF ws-home-mode = "Y"
               PERFORM HOME-MODE
           ELSE
               IF ws-edit-screen-mode = "Y"
                   PERFORM EDIT-SCREEN-MODE
               ELSE
                   IF ws-list-screen-mode = "Y"
                       PERFORM LIST-MODE
                   ELSE
                       IF ws-screen-mode = "Y"
                           PERFORM SCREEN-MODE
                       ELSE
                           IF ws-import-mode = "Y"
                               PERFORM IMPORT-MODE
                           ELSE
                               IF ws-batch-mode = "Y"
                                   PERFORM GET-VALID-ID
                                   PERFORM GET-VALID-NAME
                                   PERFORM GET-VALID-EMAIL
                                   PERFORM GET-VALID-AGE
                                   PERFORM CHECK-DUPLICATE-ID
                                   IF ws-dup-found = "Y"
                                       DISPLAY "ID already exists."
                                       STOP RUN
                                   END-IF
                                   PERFORM WRITE-RECORD
                               ELSE
                                   PERFORM ENTRY-LOOP UNTIL ws-continue NOT = "Y"
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF ws-storage-indexed = "Y"
               CLOSE idx-file
           ELSE
               CLOSE customer-file
           END-IF
           STOP RUN.

       INIT-FILE.
           IF ws-storage-indexed = "Y"
               *> Open or create indexed file
               OPEN I-O idx-file
               EVALUATE fs-idx
                   WHEN "00"
                       CONTINUE
                   WHEN "35"
                       OPEN OUTPUT idx-file
                       IF fs-idx NOT = "00"
                           DISPLAY "Error creating data/customers.idx, status " fs-idx
                           STOP RUN
                       END-IF
                       CLOSE idx-file
                       OPEN I-O idx-file
                       IF fs-idx NOT = "00"
                           DISPLAY "Error reopening data/customers.idx, status " fs-idx
                           STOP RUN
                       END-IF
                   WHEN OTHER
                       DISPLAY "Error opening data/customers.idx, status " fs-idx
                       STOP RUN
               END-EVALUATE
           ELSE
               MOVE "N" TO need-header
               OPEN INPUT customer-file
               EVALUATE fs
                   WHEN "00"
                       READ customer-file INTO customer-record
                       IF fs = "10"
                           MOVE "Y" TO need-header
                       END-IF
                       CLOSE customer-file
                   WHEN "35"
                       MOVE "Y" TO need-header
                   WHEN OTHER
                       DISPLAY "Error checking data/customers.csv, status " fs
                       STOP RUN
               END-EVALUATE

               IF need-header = "Y"
                   OPEN OUTPUT customer-file
                   IF fs NOT = "00"
                       DISPLAY "Error creating data/customers.csv, status " fs
                       STOP RUN
                   END-IF
                   MOVE SPACES TO customer-record
                   STRING "id,name,email,age" DELIMITED BY SIZE
                       INTO customer-record
                   END-STRING
                   WRITE customer-record
                   CLOSE customer-file
               END-IF

               OPEN EXTEND customer-file
               IF fs NOT = "00"
                   DISPLAY "Error opening data/customers.csv for append, status " fs
                   STOP RUN
               END-IF
           END-IF

           *> Open audit log for append (create if needed)
           OPEN EXTEND audit-file
           IF fs-audit = "35"
               OPEN OUTPUT audit-file
               IF fs-audit NOT = "00"
                   DISPLAY "Error creating data/audit.log, status " fs-audit
                   STOP RUN
               END-IF
               CLOSE audit-file
               OPEN EXTEND audit-file
           END-IF
           IF fs-audit NOT = "00"
               DISPLAY "Error opening data/audit.log, status " fs-audit
               STOP RUN
           END-IF.

       CHECK-EXISTS.
           OPEN INPUT customer-file
           IF fs = "00"
               MOVE "Y" TO file-exists
               CLOSE customer-file
           ELSE
               MOVE "N" TO file-exists
           END-IF.

       READ-ARGS.
           MOVE "N" TO ws-batch-mode
           MOVE "N" TO ws-screen-mode
           MOVE "N" TO ws-edit-screen-mode
           MOVE "N" TO ws-list-screen-mode
           MOVE "N" TO ws-import-mode
           MOVE "N" TO ws-home-mode
           MOVE SPACES TO ws-import-path
           ACCEPT arg1 FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(arg1)) TO arg1
           *> Read THEME from environment (DEFAULT/BLUE/GREEN)
           ACCEPT ws-theme FROM ENVIRONMENT "THEME"
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(ws-theme)) TO ws-theme
           IF FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(ws-theme)) = 0
               MOVE "DEFAULT" TO ws-theme-label
           ELSE
               MOVE ws-theme TO ws-theme-label
           END-IF
           *> Read STORAGE from environment (CSV/INDEXED)
           ACCEPT ws-storage FROM ENVIRONMENT "STORAGE"
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(ws-storage)) TO ws-storage
           IF ws-storage = "INDEXED"
               MOVE "Y" TO ws-storage-indexed
               MOVE "INDEXED" TO ws-storage-label
           ELSE
               MOVE "N" TO ws-storage-indexed
               MOVE "CSV" TO ws-storage-label
           END-IF
           IF arg1 = "BATCH4"
               MOVE "Y" TO ws-batch-mode
           END-IF
           IF arg1 = "SCREEN"
               MOVE "Y" TO ws-screen-mode
           END-IF
           IF arg1 = "EDIT"
               MOVE "Y" TO ws-edit-screen-mode
           END-IF
           IF arg1 = "LIST"
               MOVE "Y" TO ws-list-screen-mode
           END-IF
           IF arg1 = "IMPORT"
               MOVE "Y" TO ws-import-mode
               MOVE SPACES TO ws-import-path
               ACCEPT ws-import-path FROM ENVIRONMENT "IMPORT_FILE"
               IF FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(ws-import-path)) = 0
                   MOVE "data/import.csv" TO ws-import-path
               END-IF
           END-IF
           IF arg1 = "HOME" OR FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(arg1)) = 0
               MOVE "Y" TO ws-home-mode
           END-IF
           IF ws-batch-mode = "N" AND ws-screen-mode = "N" AND ws-edit-screen-mode = "N"
              AND ws-list-screen-mode = "N" AND ws-import-mode = "N" AND ws-home-mode = "N"
               MOVE "Y" TO ws-home-mode
           END-IF.

       ENTRY-LOOP.
           PERFORM GET-VALID-ID
           PERFORM GET-VALID-NAME
           PERFORM GET-VALID-EMAIL
           PERFORM GET-VALID-AGE
           PERFORM WRITE-RECORD
           PERFORM ASK-CONTINUE.

       GET-VALID-ID.
           IF ws-batch-mode = "Y"
               DISPLAY "Enter Customer ID (digits only): " WITH NO ADVANCING
               ACCEPT ws-id-text FROM STDIN
               MOVE FUNCTION TRIM(ws-id-text) TO ws-id-trim
               MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-id-trim)) TO len-id
               IF len-id = 0
                   DISPLAY "ID cannot be empty."
                   STOP RUN
               END-IF
               MOVE 0 TO id-digit-count
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "0"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "1"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "2"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "3"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "4"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "5"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "6"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "7"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "8"
               INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "9"
               IF id-digit-count NOT = len-id
                   DISPLAY "ID must be numeric."
                   STOP RUN
               END-IF
               MOVE FUNCTION NUMVAL(ws-id-trim) TO num-id
               IF num-id <= 0
                   DISPLAY "ID must be greater than 0."
                   STOP RUN
               END-IF
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               DISPLAY "Enter Customer ID (digits only): " WITH NO ADVANCING
               ACCEPT ws-id-text FROM STDIN
               MOVE FUNCTION TRIM(ws-id-text) TO ws-id-trim
               MOVE FUNCTION STORED-CHAR-LENGTH(ws-id-trim) TO len-id
               IF len-id = 0
                   DISPLAY "ID cannot be empty."
               ELSE
                   MOVE 0 TO id-digit-count
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "0"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "1"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "2"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "3"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "4"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "5"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "6"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "7"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "8"
                   INSPECT ws-id-trim TALLYING id-digit-count FOR ALL "9"
                   IF id-digit-count NOT = len-id
                       DISPLAY "ID must be numeric."
                   ELSE
                       MOVE FUNCTION NUMVAL(ws-id-trim) TO num-id
                       IF num-id > 0
                           EXIT PERFORM
                       ELSE
                           DISPLAY "ID must be greater than 0."
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       GET-VALID-NAME.
           IF ws-batch-mode = "Y"
               DISPLAY "Enter Name (no commas or quotes): " WITH NO ADVANCING
               ACCEPT ws-name FROM STDIN
               MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-name)) TO trimmed-len
               IF trimmed-len = 0
                   DISPLAY "Name cannot be empty."
                   STOP RUN
               END-IF
               MOVE 0 TO comma-count quote-count
               INSPECT ws-name(1:trimmed-len) TALLYING comma-count FOR ALL ","
               INSPECT ws-name(1:trimmed-len) TALLYING quote-count FOR ALL QUOTE
               IF comma-count > 0 OR quote-count > 0
                   DISPLAY "Name cannot contain commas or quotes."
                   STOP RUN
               END-IF
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               DISPLAY "Enter Name (no commas or quotes): " WITH NO ADVANCING
               ACCEPT ws-name FROM STDIN
               MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-name)) TO trimmed-len
               IF trimmed-len = 0
                   DISPLAY "Name cannot be empty."
               ELSE
                   MOVE 0 TO comma-count quote-count
                   INSPECT ws-name(1:trimmed-len) TALLYING comma-count FOR ALL ","
                   INSPECT ws-name(1:trimmed-len) TALLYING quote-count FOR ALL QUOTE
                   IF comma-count > 0 OR quote-count > 0
                       DISPLAY "Name cannot contain commas or quotes."
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       GET-VALID-EMAIL.
           IF ws-batch-mode = "Y"
               DISPLAY "Enter Email: " WITH NO ADVANCING
               ACCEPT ws-email FROM STDIN
               MOVE FUNCTION TRIM(ws-email) TO ws-email-trim
               MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-email-trim)) TO trimmed-len
               IF trimmed-len < 3
                   DISPLAY "Email too short."
                   STOP RUN
               END-IF
               MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-email-trim)) TO email-len
               MOVE 0 TO pos-at pos-dot
               MOVE 1 TO i
               PERFORM UNTIL i > email-len
                   IF ws-email-trim(i:1) = "@" AND pos-at = 0
                       MOVE i TO pos-at
                   END-IF
                   IF ws-email-trim(i:1) = "."
                       MOVE i TO pos-dot
                   END-IF
                   ADD 1 TO i
               END-PERFORM
               IF pos-at = 0 OR pos-dot = 0
                   DISPLAY "Email must contain @ and ."
                   STOP RUN
               END-IF
               IF pos-at = 1 OR pos-dot = 1
                   DISPLAY "Email cannot start with @ or ."
                   STOP RUN
               END-IF
               IF pos-at >= pos-dot
                   DISPLAY "Email must have . after @"
                   STOP RUN
               END-IF
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               DISPLAY "Enter Email: " WITH NO ADVANCING
               ACCEPT ws-email FROM STDIN
               MOVE FUNCTION TRIM(ws-email) TO ws-email-trim
               MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-email-trim)) TO trimmed-len
               IF trimmed-len < 3
                   DISPLAY "Email too short."
               ELSE
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-email-trim)) TO email-len
                   MOVE 0 TO pos-at pos-dot
                   MOVE 1 TO i
                   PERFORM UNTIL i > email-len
                       IF ws-email-trim(i:1) = "@" AND pos-at = 0
                           MOVE i TO pos-at
                       END-IF
                       IF ws-email-trim(i:1) = "."
                           MOVE i TO pos-dot
                       END-IF
                       ADD 1 TO i
                   END-PERFORM
                   IF pos-at = 0 OR pos-dot = 0
                       DISPLAY "Email must contain @ and ."
                   ELSE
                       IF pos-at = 1 OR pos-dot = 1
                           DISPLAY "Email cannot start with @ or ."
                       ELSE
                           IF pos-at >= pos-dot
                               DISPLAY "Email must have . after @"
                           ELSE
                               EXIT PERFORM
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       GET-VALID-AGE.
           IF ws-batch-mode = "Y"
               DISPLAY "Enter Age (1-120): " WITH NO ADVANCING
               ACCEPT ws-age-text FROM STDIN
               MOVE FUNCTION TRIM(ws-age-text) TO ws-age-trim
               MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-age-trim)) TO len-age
               IF len-age = 0
                   DISPLAY "Age cannot be empty."
                   STOP RUN
               END-IF
               MOVE 0 TO age-digit-count
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "0"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "1"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "2"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "3"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "4"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "5"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "6"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "7"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "8"
               INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "9"
               IF age-digit-count NOT = len-age
                   DISPLAY "Age must be numeric."
                   STOP RUN
               END-IF
               MOVE FUNCTION NUMVAL(ws-age-trim) TO num-age
               IF num-age < 1 OR num-age > 120
                   DISPLAY "Age must be between 1 and 120."
                   STOP RUN
               END-IF
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               DISPLAY "Enter Age (1-120): " WITH NO ADVANCING
               ACCEPT ws-age-text FROM STDIN
               MOVE FUNCTION TRIM(ws-age-text) TO ws-age-trim
               MOVE FUNCTION STORED-CHAR-LENGTH(ws-age-trim) TO len-age
               IF len-age = 0
                   DISPLAY "Age cannot be empty."
               ELSE
                   MOVE 0 TO age-digit-count
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "0"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "1"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "2"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "3"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "4"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "5"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "6"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "7"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "8"
                   INSPECT ws-age-trim TALLYING age-digit-count FOR ALL "9"
                   IF age-digit-count NOT = len-age
                       DISPLAY "Age must be numeric."
                   ELSE
                       MOVE FUNCTION NUMVAL(ws-age-trim) TO num-age
                       IF num-age >= 1 AND num-age <= 120
                           EXIT PERFORM
                       ELSE
                           DISPLAY "Age must be between 1 and 120."
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       CHECK-DUPLICATE-ID.
           MOVE "N" TO ws-dup-found
           IF ws-storage-indexed = "Y"
               PERFORM CHECK-DUP-ID-INDEXED
               EXIT PARAGRAPH
           END-IF
           *> Re-open file for input to scan, then return to append mode
           CLOSE customer-file
           OPEN INPUT customer-file
           IF fs NOT = "00"
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               READ customer-file INTO customer-record
                   AT END EXIT PERFORM
               END-READ
               UNSTRING customer-record DELIMITED BY ","
                   INTO ws-scan-id
               END-UNSTRING
               MOVE FUNCTION TRIM(ws-scan-id) TO ws-scan-id-trim
               IF FUNCTION UPPER-CASE(ws-scan-id-trim) = "ID"
                   CONTINUE
               ELSE
                   IF ws-scan-id-trim = FUNCTION TRIM(ws-id-trim)
                       MOVE "Y" TO ws-dup-found
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
           CLOSE customer-file
           OPEN EXTEND customer-file.

       WRITE-RECORD.
           IF ws-storage-indexed = "Y"
               PERFORM WRITE-RECORD-INDEXED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO customer-record
           STRING
               FUNCTION TRIM(ws-id-text) DELIMITED BY SIZE
               "," DELIMITED BY SIZE
               FUNCTION TRIM(ws-name) DELIMITED BY SIZE
               "," DELIMITED BY SIZE
               FUNCTION TRIM(ws-email) DELIMITED BY SIZE
               "," DELIMITED BY SIZE
               FUNCTION TRIM(ws-age-text) DELIMITED BY SIZE
               INTO customer-record
           END-STRING
           WRITE customer-record
           IF fs NOT = "00"
               DISPLAY "Error writing record, status " fs
           ELSE
               *> Append audit log: YYYYMMDD-HHMMSS,save,<id>
               ACCEPT ws-date FROM DATE YYYYMMDD
               ACCEPT ws-time FROM TIME
               MOVE ws-date TO ws-date-a
               MOVE ws-time TO ws-time-a
               MOVE SPACES TO audit-record
               STRING
                   ws-date-a DELIMITED BY SIZE
                   '-'       DELIMITED BY SIZE
                   ws-time-a DELIMITED BY SIZE
                   ','       DELIMITED BY SIZE
                   'save'    DELIMITED BY SIZE
                   ','       DELIMITED BY SIZE
                   FUNCTION TRIM(ws-id-text) DELIMITED BY SIZE
                   INTO audit-record
               END-STRING
               WRITE audit-record
               DISPLAY "Record saved."
           END-IF.

       ASK-CONTINUE.
           DISPLAY "Add another? (Y/N): " WITH NO ADVANCING
           ACCEPT ws-continue FROM STDIN
           MOVE FUNCTION UPPER-CASE(ws-continue) TO ws-continue
           IF ws-continue NOT = "Y"
               MOVE "N" TO ws-continue
           END-IF.

       SCREEN-MODE.
           MOVE "Y" TO ws-continue
           PERFORM UNTIL ws-continue NOT = "Y"
               MOVE 7 TO ws-status-color
               MOVE SPACES TO ws-status
               DISPLAY entry-screen
               ACCEPT entry-screen
               IF ws-kb-status NOT = 0
                   PERFORM HANDLE-KEY
                   IF ws-continue = "N"
                       EXIT PERFORM
                   END-IF
                   IF ws-status NOT = SPACES
                       CONTINUE
                   END-IF
               END-IF
               PERFORM VALIDATE-FIELDS
               IF ws-valid = "Y"
                   PERFORM WRITE-RECORD
                   MOVE "Record saved." TO ws-status
                   MOVE 2 TO ws-status-color
                   DISPLAY cont-screen
                   ACCEPT cont-screen
                   MOVE FUNCTION UPPER-CASE(ws-continue) TO ws-continue
                   IF ws-continue = "Y"
                       MOVE SPACES TO ws-id-text ws-name ws-email ws-age-text
                   ELSE
                       MOVE "N" TO ws-continue
                   END-IF
               ELSE
                   CONTINUE
               END-IF
           END-PERFORM.

       VALIDATE-FIELDS.
           MOVE "Y" TO ws-valid
           MOVE 7 TO ws-id-color ws-name-color ws-email-color ws-age-color
           PERFORM V-ID
           IF ws-valid = "N" EXIT PARAGRAPH END-IF
           IF ws-edit-mode = "Y"
               MOVE FUNCTION TRIM(ws-id-text) TO ws-id-trim
               IF FUNCTION TRIM(ws-id-trim) NOT = FUNCTION TRIM(ws-edit-id)
                   PERFORM CHECK-DUPLICATE-ID
                   IF ws-dup-found = "Y"
                       MOVE "ID already exists." TO ws-status
                       MOVE 4 TO ws-status-color
                       MOVE "N" TO ws-valid
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           ELSE
               PERFORM CHECK-DUPLICATE-ID
               IF ws-dup-found = "Y"
                   MOVE "ID already exists." TO ws-status
                   MOVE 4 TO ws-status-color
                   MOVE "N" TO ws-valid
                   EXIT PARAGRAPH
               END-IF
           END-IF
           PERFORM V-NAME
           IF ws-valid = "N" EXIT PARAGRAPH END-IF
           PERFORM V-EMAIL
           IF ws-valid = "N" EXIT PARAGRAPH END-IF
           PERFORM V-AGE.

       V-ID.
           MOVE FUNCTION TRIM(ws-id-text) TO ws-id-trim
           MOVE 7 TO ws-id-color
           MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-id-trim)) TO len-id
           IF len-id = 0
               MOVE "ID cannot be empty." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-id-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO id-digit-count
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "0"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "1"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "2"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "3"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "4"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "5"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "6"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "7"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "8"
           INSPECT ws-id-trim(1:len-id) TALLYING id-digit-count FOR ALL "9"
           IF id-digit-count NOT = len-id
               MOVE "ID must be numeric." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-id-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION NUMVAL(ws-id-trim) TO num-id
           IF num-id <= 0
               MOVE "ID must be greater than 0." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-id-color
               MOVE "N" TO ws-valid
           END-IF.

       V-NAME.
           MOVE 7 TO ws-name-color
           MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-name)) TO trimmed-len
           IF trimmed-len = 0
               MOVE "Name cannot be empty." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-name-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO comma-count quote-count
           INSPECT ws-name(1:trimmed-len) TALLYING comma-count FOR ALL ","
           INSPECT ws-name(1:trimmed-len) TALLYING quote-count FOR ALL QUOTE
           IF comma-count > 0 OR quote-count > 0
               MOVE "Name cannot contain commas or quotes." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-name-color
               MOVE "N" TO ws-valid
           END-IF.

       V-EMAIL.
           MOVE 7 TO ws-email-color
           MOVE FUNCTION TRIM(ws-email) TO ws-email-trim
           MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-email-trim)) TO trimmed-len
           IF trimmed-len < 3
               MOVE "Email too short." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-email-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO pos-at pos-dot
           MOVE 1 TO i
           MOVE trimmed-len TO email-len
           PERFORM UNTIL i > email-len
               IF ws-email-trim(i:1) = "@" AND pos-at = 0
                   MOVE i TO pos-at
               END-IF
               IF ws-email-trim(i:1) = "."
                   MOVE i TO pos-dot
               END-IF
               ADD 1 TO i
           END-PERFORM
           IF pos-at = 0 OR pos-dot = 0
               MOVE "Email must contain @ and ." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-email-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           IF pos-at = 1 OR pos-dot = 1
               MOVE "Email cannot start with @ or ." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-email-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           IF pos-at >= pos-dot
               MOVE "Email must have . after @" TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-email-color
               MOVE "N" TO ws-valid
           END-IF.

       V-AGE.
           MOVE 7 TO ws-age-color
           MOVE FUNCTION TRIM(ws-age-text) TO ws-age-trim
           MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-age-trim)) TO len-age
           IF len-age = 0
               MOVE "Age cannot be empty." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-age-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO age-digit-count
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "0"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "1"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "2"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "3"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "4"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "5"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "6"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "7"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "8"
           INSPECT ws-age-trim(1:len-age) TALLYING age-digit-count FOR ALL "9"
           IF age-digit-count NOT = len-age
               MOVE "Age must be numeric." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-age-color
               MOVE "N" TO ws-valid
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION NUMVAL(ws-age-trim) TO num-age
           IF num-age < 1 OR num-age > 120
               MOVE "Age must be between 1 and 120." TO ws-status
               MOVE 4 TO ws-status-color
               MOVE 4 TO ws-age-color
               MOVE "N" TO ws-valid
           END-IF.

       EDIT-SCREEN-MODE.
           MOVE "Y" TO ws-continue
           PERFORM UNTIL ws-continue NOT = "Y"
               MOVE SPACES TO ws-status
               MOVE 7 TO ws-status-color
               MOVE SPACES TO ws-search-id ws-search-name
               DISPLAY search-screen
               ACCEPT search-screen
               IF ws-kb-status = k-esc OR ws-kb-status = k-f10
                   MOVE "N" TO ws-continue
                   EXIT PERFORM
               END-IF
               PERFORM FIND-RECORD
               IF ws-found = "Y"
                   MOVE "Y" TO ws-edit-mode
                   MOVE FUNCTION TRIM(ws-id-text) TO ws-edit-id
                   MOVE SPACES TO ws-status
                   DISPLAY entry-screen
                   ACCEPT entry-screen
                   IF ws-kb-status NOT = 0
                       PERFORM HANDLE-KEY
                       IF ws-continue = "N"
                           EXIT PERFORM
                       END-IF
                       IF ws-status NOT = SPACES
                           CONTINUE
                       END-IF
                   END-IF
                   PERFORM VALIDATE-FIELDS
                   IF ws-valid = "Y"
                       PERFORM UPDATE-RECORD
                       MOVE "Record updated." TO ws-status
                       MOVE 2 TO ws-status-color
                       DISPLAY cont-screen
                       ACCEPT cont-screen
                       MOVE FUNCTION UPPER-CASE(ws-continue) TO ws-continue
                       IF ws-continue = "Y"
                           MOVE "N" TO ws-edit-mode
                           MOVE SPACES TO ws-id-text ws-name ws-email ws-age-text
                       ELSE
                           MOVE "N" TO ws-continue
                       END-IF
                   END-IF
               ELSE
                   MOVE "Not found." TO ws-status
                   MOVE 4 TO ws-status-color
                   DISPLAY search-screen
                   ACCEPT search-screen
                   MOVE "N" TO ws-continue
               END-IF
           END-PERFORM.

       FIND-RECORD.
           MOVE "N" TO ws-found
           IF ws-storage-indexed = "Y"
               PERFORM FIND-RECORD-INDEXED
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(ws-search-id) TO ws-search-id
           MOVE FUNCTION TRIM(ws-search-name) TO ws-search-name
           CLOSE customer-file
           OPEN INPUT customer-file
           IF fs NOT = "00"
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               READ customer-file INTO customer-record
                   AT END EXIT PERFORM
               END-READ
               UNSTRING customer-record DELIMITED BY ","
                   INTO r-id r-name r-email r-age
               END-UNSTRING
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(r-id)) = "ID"
                   CONTINUE
               END-IF
               IF FUNCTION STORED-CHAR-LENGTH(ws-search-id) > 0
                   IF FUNCTION TRIM(r-id) = FUNCTION TRIM(ws-search-id)
                       MOVE FUNCTION TRIM(r-id) TO ws-id-text ws-edit-id
                       MOVE FUNCTION TRIM(r-name) TO ws-name
                       MOVE FUNCTION TRIM(r-email) TO ws-email
                       MOVE FUNCTION TRIM(r-age) TO ws-age-text
                       MOVE "Y" TO ws-found
                       EXIT PERFORM
                   END-IF
               ELSE
                   IF FUNCTION STORED-CHAR-LENGTH(ws-search-name) > 0
                       MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(r-name)) TO ws-name-up
                       MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(ws-search-name)) TO ws-key-up
                       MOVE FUNCTION STORED-CHAR-LENGTH(ws-name-up) TO l-name
                       MOVE FUNCTION STORED-CHAR-LENGTH(ws-key-up) TO l-key
                       MOVE "N" TO ws-match
                       IF l-key > 0 AND l-name >= l-key
                           COMPUTE end-pos = l-name - l-key + 1
                           MOVE 1 TO j
                           PERFORM UNTIL j > end-pos OR ws-match = "Y"
                               IF ws-name-up(j:l-key) = ws-key-up(1:l-key)
                                   MOVE "Y" TO ws-match
                               ELSE
                                   ADD 1 TO j
                               END-IF
                           END-PERFORM
                       END-IF
                       IF ws-match = "Y"
                           MOVE FUNCTION TRIM(r-id) TO ws-id-text ws-edit-id
                           MOVE FUNCTION TRIM(r-name) TO ws-name
                           MOVE FUNCTION TRIM(r-email) TO ws-email
                           MOVE FUNCTION TRIM(r-age) TO ws-age-text
                           MOVE "Y" TO ws-found
                           EXIT PERFORM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           CLOSE customer-file
           OPEN EXTEND customer-file.

       UPDATE-RECORD.
           IF ws-storage-indexed = "Y"
               PERFORM UPDATE-RECORD-INDEXED
               EXIT PARAGRAPH
           END-IF
           CLOSE customer-file
           OPEN INPUT customer-file
           IF fs NOT = "00"
               MOVE "Error opening CSV for read." TO ws-status
               MOVE 4 TO ws-status-color
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT temp-file
           IF fs-temp NOT = "00"
               MOVE "Error opening temp file." TO ws-status
               MOVE 4 TO ws-status-color
               CLOSE customer-file
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO temp-record
           STRING "id,name,email,age" DELIMITED BY SIZE INTO temp-record END-STRING
           WRITE temp-record
           PERFORM UNTIL 1 = 2
               READ customer-file INTO customer-record
                   AT END EXIT PERFORM
               END-READ
               UNSTRING customer-record DELIMITED BY ","
                   INTO r-id r-name r-email r-age
               END-UNSTRING
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(r-id)) = "ID"
                   CONTINUE
               ELSE
                   IF FUNCTION TRIM(r-id) = FUNCTION TRIM(ws-edit-id)
                       MOVE SPACES TO temp-record
                       STRING
                           FUNCTION TRIM(ws-id-text) DELIMITED BY SIZE
                           "," DELIMITED BY SIZE
                           FUNCTION TRIM(ws-name) DELIMITED BY SIZE
                           "," DELIMITED BY SIZE
                           FUNCTION TRIM(ws-email) DELIMITED BY SIZE
                           "," DELIMITED BY SIZE
                           FUNCTION TRIM(ws-age-text) DELIMITED BY SIZE
                           INTO temp-record
                       END-STRING
                       WRITE temp-record
                   ELSE
                       WRITE customer-record
                   END-IF
               END-IF
           END-PERFORM
           CLOSE customer-file
           CLOSE temp-file

           OPEN INPUT temp-file
           IF fs-temp NOT = "00"
               MOVE "Error reading temp file." TO ws-status
               MOVE 4 TO ws-status-color
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT customer-file
           IF fs NOT = "00"
               MOVE "Error opening CSV for write." TO ws-status
               MOVE 4 TO ws-status-color
               CLOSE temp-file
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               READ temp-file INTO temp-record
                   AT END EXIT PERFORM
               END-READ
               MOVE temp-record TO customer-record
               WRITE customer-record
           END-PERFORM
           CLOSE customer-file
           CLOSE temp-file
           OPEN EXTEND customer-file

           ACCEPT ws-date FROM DATE YYYYMMDD
           ACCEPT ws-time FROM TIME
           MOVE ws-date TO ws-date-a
           MOVE ws-time TO ws-time-a
           MOVE SPACES TO audit-record
           STRING
               ws-date-a DELIMITED BY SIZE
               '-'       DELIMITED BY SIZE
               ws-time-a DELIMITED BY SIZE
               ','       DELIMITED BY SIZE
               'update'  DELIMITED BY SIZE
               ','       DELIMITED BY SIZE
               FUNCTION TRIM(ws-id-text) DELIMITED BY SIZE
               INTO audit-record
           END-STRING
           WRITE audit-record.

       HANDLE-KEY.
           EVALUATE ws-kb-status
               WHEN k-esc
                   MOVE "N" TO ws-continue
               WHEN k-f10
                   MOVE "N" TO ws-continue
               WHEN k-f1
                   PERFORM SHOW-HELP
               WHEN k-f5
                   CONTINUE
               WHEN OTHER
                   MOVE 6 TO ws-status-color
                   MOVE SPACES TO ws-status
                   STRING "Key code: " DELIMITED BY SIZE
                          ws-kb-status DELIMITED BY SIZE
                          INTO ws-status
                   END-STRING
           END-EVALUATE.

       SHOW-HELP.
           MOVE 7 TO ws-status-color
           MOVE SPACES TO ws-status
           DISPLAY help-screen
           ACCEPT help-screen
           MOVE SPACES TO ws-status.

       APPLY-THEME.
           EVALUATE ws-theme
               WHEN "BLUE"
                   MOVE 1 TO ws-title-bg
                   MOVE 7 TO ws-title-color ws-label-color ws-input-color ws-status-color
               WHEN "GREEN"
                   MOVE 2 TO ws-title-bg
                   MOVE 7 TO ws-title-color ws-label-color ws-input-color ws-status-color
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       CHECK-DUP-ID-INDEXED.
           MOVE FUNCTION TRIM(ws-id-text) TO ws-id-trim
           MOVE SPACES TO idx-id
           MOVE FUNCTION TRIM(ws-id-trim) TO idx-id
           READ idx-file KEY IS idx-id
               INVALID KEY MOVE "N" TO ws-dup-found
               NOT INVALID KEY MOVE "Y" TO ws-dup-found
           END-READ.

       WRITE-RECORD-INDEXED.
           MOVE FUNCTION TRIM(ws-id-text) TO idx-id
           MOVE FUNCTION TRIM(ws-name) TO idx-name
           MOVE FUNCTION TRIM(ws-email) TO idx-email
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(ws-age-text)) TO idx-age
           WRITE idx-record
           IF fs-idx NOT = "00"
               DISPLAY "Error writing indexed record, status " fs-idx
           ELSE
               ACCEPT ws-date FROM DATE YYYYMMDD
               ACCEPT ws-time FROM TIME
               MOVE ws-date TO ws-date-a
               MOVE ws-time TO ws-time-a
               MOVE SPACES TO audit-record
               STRING ws-date-a DELIMITED BY SIZE '-' DELIMITED BY SIZE ws-time-a DELIMITED BY SIZE ',' DELIMITED BY SIZE 'save' DELIMITED BY SIZE ',' DELIMITED BY SIZE FUNCTION TRIM(ws-id-text) DELIMITED BY SIZE INTO audit-record END-STRING
               WRITE audit-record
               DISPLAY "Record saved."
           END-IF.

       UPDATE-RECORD-INDEXED.
           MOVE FUNCTION TRIM(ws-edit-id) TO idx-id
           READ idx-file KEY IS idx-id
               INVALID KEY
                   MOVE "Record not found." TO ws-status
                   MOVE 4 TO ws-status-color
                   EXIT PARAGRAPH
           END-READ
           MOVE FUNCTION TRIM(ws-id-text) TO idx-id
           MOVE FUNCTION TRIM(ws-name) TO idx-name
           MOVE FUNCTION TRIM(ws-email) TO idx-email
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(ws-age-text)) TO idx-age
           REWRITE idx-record
           IF fs-idx NOT = "00"
               MOVE "Error updating indexed record." TO ws-status
               MOVE 4 TO ws-status-color
           ELSE
               ACCEPT ws-date FROM DATE YYYYMMDD
               ACCEPT ws-time FROM TIME
               MOVE ws-date TO ws-date-a
               MOVE ws-time TO ws-time-a
               MOVE SPACES TO audit-record
               STRING ws-date-a DELIMITED BY SIZE '-' DELIMITED BY SIZE ws-time-a DELIMITED BY SIZE ',' DELIMITED BY SIZE 'update' DELIMITED BY SIZE ',' DELIMITED BY SIZE FUNCTION TRIM(ws-id-text) DELIMITED BY SIZE INTO audit-record END-STRING
               WRITE audit-record
           END-IF.

       FIND-RECORD-INDEXED.
           MOVE "N" TO ws-found
           MOVE FUNCTION TRIM(ws-search-id) TO ws-search-id
           MOVE FUNCTION TRIM(ws-search-name) TO ws-search-name
           IF FUNCTION STORED-CHAR-LENGTH(ws-search-id) > 0
               MOVE FUNCTION TRIM(ws-search-id) TO idx-id
               READ idx-file KEY IS idx-id
                   INVALID KEY
                       CONTINUE
                   NOT INVALID KEY
                       MOVE idx-id TO ws-id-text ws-edit-id
                       MOVE idx-name TO ws-name
                       MOVE idx-email TO ws-email
                       MOVE idx-age TO num-age
                       MOVE idx-age TO ws-age-text
                       MOVE "Y" TO ws-found
                       EXIT PARAGRAPH
               END-READ
           END-IF
           *> Name contains scan
           MOVE LOW-VALUES TO idx-id
           START idx-file KEY IS NOT LESS THAN idx-id
           PERFORM UNTIL 1 = 2
               READ idx-file NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF FUNCTION STORED-CHAR-LENGTH(ws-search-name) > 0
                   MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(idx-name)) TO ws-name-up
                   MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(ws-search-name)) TO ws-key-up
                   MOVE FUNCTION STORED-CHAR-LENGTH(ws-name-up) TO l-name
                   MOVE FUNCTION STORED-CHAR-LENGTH(ws-key-up) TO l-key
                   MOVE "N" TO ws-match
                   IF l-key > 0 AND l-name >= l-key
                       COMPUTE end-pos = l-name - l-key + 1
                       MOVE 1 TO j
                       PERFORM UNTIL j > end-pos OR ws-match = "Y"
                           IF ws-name-up(j:l-key) = ws-key-up(1:l-key)
                               MOVE "Y" TO ws-match
                           ELSE
                               ADD 1 TO j
                           END-IF
                       END-PERFORM
                   END-IF
                   IF ws-match = "Y"
                       MOVE idx-id TO ws-id-text ws-edit-id
                       MOVE idx-name TO ws-name
                       MOVE idx-email TO ws-email
                       MOVE idx-age TO ws-age-text
                       MOVE "Y" TO ws-found
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       BUILD-LIST-INDEXED.
           MOVE 0 TO tbl-count
           MOVE LOW-VALUES TO idx-id
           START idx-file KEY IS NOT LESS THAN idx-id
           PERFORM UNTIL 1 = 2
               READ idx-file NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               MOVE "Y" TO ws-valid
               *> Name filter
               IF FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(ws-filter-name)) > 0
                   MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(idx-name)) TO ws-name-up
                   MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(ws-filter-name)) TO ws-key-up
                   MOVE FUNCTION STORED-CHAR-LENGTH(ws-name-up) TO l-name
                   MOVE FUNCTION STORED-CHAR-LENGTH(ws-key-up) TO l-key
                   MOVE "N" TO ws-match
                   IF l-key > 0 AND l-name >= l-key
                       COMPUTE end-pos = l-name - l-key + 1
                       MOVE 1 TO j
                       PERFORM UNTIL j > end-pos OR ws-match = "Y"
                           IF ws-name-up(j:l-key) = ws-key-up(1:l-key)
                               MOVE "Y" TO ws-match
                           ELSE
                               ADD 1 TO j
                           END-IF
                       END-PERFORM
                   END-IF
                   IF ws-match NOT = "Y"
                       MOVE "N" TO ws-valid
                   END-IF
               END-IF
               *> Email domain filter
               IF ws-valid = "Y"
                   MOVE FUNCTION STORED-CHAR-LENGTH(idx-email) TO email-len
                   MOVE FUNCTION TRIM(idx-email) TO ws-email-trim
                   MOVE 0 TO pos-at
                   MOVE 1 TO i
                   PERFORM UNTIL i > email-len
                       IF ws-email-trim(i:1) = "@" AND pos-at = 0
                           MOVE i TO pos-at
                       END-IF
                       ADD 1 TO i
                   END-PERFORM
                   IF FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(ws-filter-domain)) > 0
                       IF pos-at > 0
                           IF FUNCTION UPPER-CASE(ws-email-trim(pos-at + 1: email-len - pos-at)) NOT =
                              FUNCTION UPPER-CASE(FUNCTION TRIM(ws-filter-domain))
                               MOVE "N" TO ws-valid
                           END-IF
                       ELSE
                           MOVE "N" TO ws-valid
                       END-IF
                   END-IF
               END-IF
               *> Age filter
               IF ws-valid = "Y"
                   IF ws-filter-age-min > 0 AND idx-age < ws-filter-age-min
                       MOVE "N" TO ws-valid
                   END-IF
                   IF ws-filter-age-max > 0 AND idx-age > ws-filter-age-max
                       MOVE "N" TO ws-valid
                   END-IF
               END-IF
               IF ws-valid = "Y"
                   ADD 1 TO tbl-count
                   IF tbl-count <= 500
                       MOVE FUNCTION TRIM(idx-id) TO t-id(tbl-count)
                       MOVE FUNCTION TRIM(idx-name) TO t-name(tbl-count)
                       MOVE FUNCTION TRIM(idx-email) TO t-email(tbl-count)
                       MOVE idx-age TO t-age(tbl-count)
                   END-IF
               END-IF
           END-PERFORM.

       HOME-MODE.
           MOVE "Y" TO ws-continue
           PERFORM UNTIL ws-continue NOT = "Y"
               MOVE SPACES TO ws-status
               MOVE 7 TO ws-status-color
               MOVE SPACE TO ws-menu-choice
               DISPLAY home-screen
               ACCEPT home-screen
               IF ws-kb-status = k-esc OR ws-kb-status = k-f10
                   MOVE "N" TO ws-continue
                   EXIT PERFORM
               END-IF
               EVALUATE FUNCTION UPPER-CASE(ws-menu-choice)
                   WHEN "1"
                       MOVE "Y" TO ws-screen-mode
                       PERFORM SCREEN-MODE
                       MOVE "N" TO ws-screen-mode
                   WHEN "2"
                       MOVE "Y" TO ws-edit-screen-mode
                       PERFORM EDIT-SCREEN-MODE
                       MOVE "N" TO ws-edit-screen-mode
                   WHEN "3"
                       MOVE "Y" TO ws-list-screen-mode
                       PERFORM LIST-MODE
                       MOVE "N" TO ws-list-screen-mode
                   WHEN "4"
                       MOVE "Y" TO ws-import-mode
                       MOVE SPACES TO ws-import-path
                       DISPLAY import-path-screen
                       ACCEPT import-path-screen
                       IF ws-kb-status = k-esc OR ws-kb-status = k-f10
                           MOVE "N" TO ws-import-mode
                       ELSE
                           IF FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(ws-import-path)) = 0
                               MOVE "data/import.csv" TO ws-import-path
                           END-IF
                           PERFORM IMPORT-MODE
                           MOVE "N" TO ws-import-mode
                       END-IF
                   WHEN "X"
                       MOVE "N" TO ws-continue
                   WHEN OTHER
                       MOVE 4 TO ws-status-color
                       MOVE "Invalid choice (1-4 or X)." TO ws-status
               END-EVALUATE
           END-PERFORM.

       IMPORT-MODE.
           MOVE 0 TO ws-added-count ws-updated-count ws-failed-count ws-line-number
           IF ws-import-mode NOT = "Y"
               EXIT PARAGRAPH
           END-IF
           CLOSE import-file
           OPEN INPUT import-file
           IF fs-import NOT = "00"
               DISPLAY "Error opening import file: " ws-import-path " status " fs-import
               EXIT PARAGRAPH
           END-IF
           *> Optionally skip header if first line looks like header
           PERFORM UNTIL 1 = 2
               READ import-file INTO import-record
                   AT END EXIT PERFORM
               END-READ
               ADD 1 TO ws-line-number
               IF ws-line-number = 1
                   IF FUNCTION UPPER-CASE(import-record(1:2)) = "ID"
                       CONTINUE
                       *> Skip header, continue to next line
                       CONTINUE
                   END-IF
               END-IF
               *> Parse CSV into fields
               MOVE SPACES TO r-id r-name r-email
               MOVE ZEROS TO r-age
               UNSTRING import-record DELIMITED BY ","
                   INTO r-id r-name r-email r-age
               END-UNSTRING
               MOVE FUNCTION TRIM(r-id)    TO ws-id-text
               MOVE FUNCTION TRIM(r-name)  TO ws-name
               MOVE FUNCTION TRIM(r-email) TO ws-email
               MOVE FUNCTION TRIM(r-age)   TO ws-age-text
               MOVE SPACES TO ws-status
               MOVE 7 TO ws-status-color
               MOVE "N" TO ws-edit-mode
               PERFORM VALIDATE-FIELDS
               IF ws-valid = "Y"
                   *> Decide add vs update
                   PERFORM CHECK-DUPLICATE-ID
                   IF ws-dup-found = "Y"
                       MOVE "Y" TO ws-edit-mode
                       MOVE FUNCTION TRIM(ws-id-text) TO ws-edit-id
                       PERFORM UPDATE-RECORD
                       ADD 1 TO ws-updated-count
                   ELSE
                       PERFORM WRITE-RECORD
                       ADD 1 TO ws-added-count
                   END-IF
               ELSE
                   ADD 1 TO ws-failed-count
               END-IF
           END-PERFORM
           CLOSE import-file
           DISPLAY "Import complete: added=" ws-added-count " updated=" ws-updated-count " failed=" ws-failed-count
           MOVE SPACES TO ws-status.

       LIST-MODE.
           MOVE SPACES TO ws-filter-domain ws-filter-age-min-txt ws-filter-age-max-txt
           MOVE 0 TO ws-filter-age-min ws-filter-age-max
           PERFORM UNTIL 1 = 2
               DISPLAY list-filter-screen
               ACCEPT list-filter-screen
               IF ws-kb-status = k-esc OR ws-kb-status = k-f10
                   EXIT PERFORM
               END-IF
               PERFORM BUILD-LIST
               PERFORM SHOW-LIST
               EXIT PERFORM
           END-PERFORM.

       SHOW-LIST.
           MOVE 1 TO ws-page
           PERFORM UNTIL 1 = 2
               PERFORM REFRESH-LIST-SCREEN
               DISPLAY list-screen
               ACCEPT list-screen
               MOVE FUNCTION UPPER-CASE(ws-list-cmd) TO ws-list-cmd
               IF ws-kb-status = k-esc OR ws-kb-status = k-f10
                   EXIT PERFORM
               END-IF
               EVALUATE TRUE
                   WHEN ws-list-cmd = "N"
                       IF ws-page < ws-total-pages
                           ADD 1 TO ws-page
                       END-IF
                   WHEN ws-list-cmd = "P"
                       IF ws-page > 1
                           SUBTRACT 1 FROM ws-page
                       END-IF
                   WHEN ws-list-cmd = "F"
                       EXIT PERFORM
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               MOVE SPACE TO ws-list-cmd
           END-PERFORM.

       REFRESH-LIST-SCREEN.
           MOVE 0 TO ws-start-index ws-end-index
           MOVE 0 TO ws-total-pages
           IF tbl-count = 0
               MOVE 1 TO ws-total-pages
               MOVE 1 TO ws-page
           ELSE
               MOVE 0 TO ws-total-pages
               MOVE 0 TO i
               PERFORM UNTIL i >= tbl-count
                   ADD 1 TO ws-total-pages
                   ADD ws-page-size TO i
               END-PERFORM
               IF ws-page > ws-total-pages
                   MOVE ws-total-pages TO ws-page
               END-IF
               COMPUTE ws-start-index = (ws-page - 1) * ws-page-size + 1
               COMPUTE ws-end-index   = ws-page * ws-page-size
               IF ws-end-index > tbl-count
                   MOVE tbl-count TO ws-end-index
               END-IF
           END-IF
           MOVE SPACES TO ws-row-1 ws-row-2 ws-row-3 ws-row-4 ws-row-5
                          ws-row-6 ws-row-7 ws-row-8 ws-row-9 ws-row-10
           PERFORM VARYING i FROM ws-start-index BY 1 UNTIL i > ws-end-index
               PERFORM SET-ROW
           END-PERFORM
           MOVE tbl-count TO ws-total-count.

       SET-ROW.
           *> Build display line for item i into proper ws-row-N
           MOVE SPACES TO customer-record
           STRING
               FUNCTION TRIM(t-id(i)) DELIMITED BY SIZE
               SPACE DELIMITED BY SIZE
               FUNCTION TRIM(t-name(i)) DELIMITED BY SIZE
               SPACE DELIMITED BY SIZE
               FUNCTION TRIM(t-age(i)) DELIMITED BY SIZE
               SPACE DELIMITED BY SIZE
               FUNCTION TRIM(t-email(i)) DELIMITED BY SIZE
               INTO customer-record
           END-STRING
           EVALUATE i - ws-start-index + 1
               WHEN 1  MOVE customer-record TO ws-row-1
               WHEN 2  MOVE customer-record TO ws-row-2
               WHEN 3  MOVE customer-record TO ws-row-3
               WHEN 4  MOVE customer-record TO ws-row-4
               WHEN 5  MOVE customer-record TO ws-row-5
               WHEN 6  MOVE customer-record TO ws-row-6
               WHEN 7  MOVE customer-record TO ws-row-7
               WHEN 8  MOVE customer-record TO ws-row-8
               WHEN 9  MOVE customer-record TO ws-row-9
               WHEN 10 MOVE customer-record TO ws-row-10
               WHEN OTHER CONTINUE
           END-EVALUATE.

       BUILD-LIST.
           IF ws-storage-indexed = "Y"
               PERFORM BUILD-LIST-INDEXED
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO tbl-count
           MOVE FUNCTION TRIM(ws-filter-age-min-txt) TO ws-age-trim
           IF FUNCTION STORED-CHAR-LENGTH(ws-age-trim) > 0
               MOVE FUNCTION NUMVAL(ws-age-trim) TO ws-filter-age-min
           ELSE
               MOVE 0 TO ws-filter-age-min
           END-IF
           MOVE FUNCTION TRIM(ws-filter-age-max-txt) TO ws-age-trim
           IF FUNCTION STORED-CHAR-LENGTH(ws-age-trim) > 0
               MOVE FUNCTION NUMVAL(ws-age-trim) TO ws-filter-age-max
           ELSE
               MOVE 0 TO ws-filter-age-max
           END-IF
           CLOSE customer-file
           OPEN INPUT customer-file
           IF fs NOT = "00"
               MOVE "Error opening CSV for list." TO ws-status
               MOVE 4 TO ws-status-color
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL 1 = 2
               READ customer-file INTO customer-record
                   AT END EXIT PERFORM
               END-READ
               UNSTRING customer-record DELIMITED BY ","
                   INTO r-id r-name r-email r-age
               END-UNSTRING
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(r-id)) = "ID"
                   CONTINUE
               END-IF
               MOVE "Y" TO ws-valid
               *> Name contains filter
               IF FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(ws-filter-name)) > 0
                   MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(r-name)) TO ws-name-up
                   MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(ws-filter-name)) TO ws-key-up
                   MOVE FUNCTION STORED-CHAR-LENGTH(ws-name-up) TO l-name
                   MOVE FUNCTION STORED-CHAR-LENGTH(ws-key-up) TO l-key
                   MOVE "N" TO ws-match
                   IF l-key > 0 AND l-name >= l-key
                       COMPUTE end-pos = l-name - l-key + 1
                       MOVE 1 TO j
                       PERFORM UNTIL j > end-pos OR ws-match = "Y"
                           IF ws-name-up(j:l-key) = ws-key-up(1:l-key)
                               MOVE "Y" TO ws-match
                           ELSE
                               ADD 1 TO j
                           END-IF
                       END-PERFORM
                   END-IF
                   IF ws-match NOT = "Y"
                       MOVE "N" TO ws-valid
                   END-IF
               END-IF
               *> Email domain filter
               MOVE FUNCTION TRIM(r-email) TO ws-email-trim
               MOVE FUNCTION STORED-CHAR-LENGTH(ws-email-trim) TO email-len
               MOVE 0 TO pos-at
               MOVE 1 TO i
               PERFORM UNTIL i > email-len
                   IF ws-email-trim(i:1) = "@" AND pos-at = 0
                       MOVE i TO pos-at
                   END-IF
                   ADD 1 TO i
               END-PERFORM
               MOVE "Y" TO ws-valid
               IF FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(ws-filter-domain)) > 0
                   IF pos-at > 0
                       IF FUNCTION UPPER-CASE(ws-email-trim(pos-at + 1: email-len - pos-at)) NOT =
                          FUNCTION UPPER-CASE(FUNCTION TRIM(ws-filter-domain))
                           MOVE "N" TO ws-valid
                       END-IF
                   ELSE
                       MOVE "N" TO ws-valid
                   END-IF
               END-IF
               *> Age filter
               IF ws-valid = "Y"
                   MOVE FUNCTION NUMVAL(FUNCTION TRIM(r-age)) TO num-age
                   IF ws-filter-age-min > 0 AND num-age < ws-filter-age-min
                       MOVE "N" TO ws-valid
                   END-IF
                   IF ws-filter-age-max > 0 AND num-age > ws-filter-age-max
                       MOVE "N" TO ws-valid
                   END-IF
               END-IF
               IF ws-valid = "Y"
                   ADD 1 TO tbl-count
                   IF tbl-count <= 500
                       MOVE FUNCTION TRIM(r-id) TO t-id(tbl-count)
                       MOVE FUNCTION TRIM(r-name) TO t-name(tbl-count)
                       MOVE FUNCTION TRIM(r-email) TO t-email(tbl-count)
                       MOVE FUNCTION NUMVAL(FUNCTION TRIM(r-age)) TO t-age(tbl-count)
                   END-IF
               END-IF
           END-PERFORM
           CLOSE customer-file
           OPEN EXTEND customer-file.
