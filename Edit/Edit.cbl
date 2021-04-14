       identification division.
       program-id. Edit.
       author. Raje Singh.
       date-written. 2021-04-14.
      * Program Description:
      * Reads records produced by the POS in the POS data file and 
      * splits them into two different data files. One with valid 
      * records and one with invalid ones An error report is also 
      * produced.
      *
       environment division.
       input-output section.
       file-control.
      *
           select file-01-pos-input
               assign
                   to '../../../../datafiles/data/file-01-pos-data.dat'
               organization is line sequential.
      *
           select file-02-error-output
               assign
                   to 
                   '../../../../datafiles/data/file-02-error-data.dat'
               organization is line sequential.
      *
           select file-03-valid-output
               assign
                   to 
                   '../../../../datafiles/data/file-03-valid-data.dat'
               organization is line sequential.
      *
           select file-04-error-report
               assign
                   to 
                   '../../../../datafiles/data/file-04-error-report.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd file-01-pos-input
           data record is pos-rec.
      *
       01 pos-rec.
         05 pr-trans-code      pic x.
           88 pr-trans-code-valid value 'S', 'R', 'L'.
         05 pr-trans-amt       pic 9(5)V99.
         05 pr-pay-type        pic XX.
           88 pr-pay-type-valid value 'CA', 'CR', 'DB'.
         05 pr-store-num       pic XX.
           88 pr-store-num-valid value '01', '02', '03', '04', '05',
                                 '12'.
         05 pr-invoice-num.
           10 pr-invoice-X1    pic X.
             88 invoice-X1-valid value 'A', 'B', 'C', 'D', 'E'.
           10 pr-invoice-X2    pic X.
             88 invoice-X2-valid value 'A', 'B', 'C', 'D', 'E'.
           10 pr-invoice-dash  pic X.
             88 invoice-dash-valid value '-'.
           10 pr-invoice-000000 pic X(6).
         05 pr-sku-code        pic X(15).
      *
       fd file-02-error-output
           data record is error-out-data.
      *
       01 error-out-data.
         05 eo-trans-code      pic x.
         05 eo-trans-amt       pic 9(5)V99.
         05 eo-pay-type        pic XX.
         05 eo-store-num       pic XX.
         05 eo-invoice-num     pic X(9).
         05 eo-sku-code        pic X(15).
      *
       fd file-03-valid-output
           data record is valid-out-data.
      *
       01 valid-out-data.
         05 vo-trans-code      pic x.
         05 vo-trans-amt       pic 9(5)V99.
         05 vo-pay-type        pic XX.
         05 vo-store-num       pic XX.
         05 vo-invoice-num     pic X(9).
         05 vo-sku-code        pic X(15).
      *
       fd file-04-error-report
           data record is report-print-line.
      *
       01 report-print-line    pic x(330).
      *
       working-storage section.
      *
       01 ws-error-report-name-line.
         05 filler             pic x(26) value
                               "Raje Singh, Final Project".
      *
       01 ws-error-report-title-line.
         05 filler             pic x(10) value spaces.
         05 filler             pic x(12) value "ERROR REPORT".
      *
       01 ws-error-report-headings.
      *
         05 filler             pic x(8) value "RECORD #".
         05 filler             pic x(5) value spaces.
         05 filler             pic x(8) value "RAW DATA".
         05 filler             pic x(30) value spaces.
         05 filler             pic x(14) value "ERROR MESSAGES".
      *
       01 ws-error-detail-line.
         05 ws-edl-record-num  pic z(7)9.
         05 filler             pic x(5) value spaces.
         05 ws-edl-raw-data    pic x(36).
         05 filler             pic x(2) value spaces.
         05 ws-edl-error-list  occurs 10 times.
           10 ws-edl-error     pic x(24).
           10 filler           pic xxx value spaces.
      *
       01 ws-error-records-read-line.
         05 filler             pic x(26) value
                               "NUMBER OF RECORDS READ:   ".
         05 ws-err-rec-read    pic z(7)9.
      *
       01 ws-error-records-valid-line.
         05 filler             pic x(26) value
                               "NUMBER OF VALID RECORDS:  ".
         05 ws-erv-valid-rec   pic z(7)9.
      *
       01 ws-error-records-invalid-line.
      *
         05 filler             pic x(26) value
                               "NUMBER OF INVALID RECORDS: ".
         05 ws-eri-invalid-rec pic z(7)9.
      *
       01 ws-error-messages.
         05 ws-trans-code-invalid  pic x(24) value
                                  "TRANSACTION CODE INVALID".
         05 ws-trans-amt-invalid   pic x(23) value
                                 "TRANSACTION AMT INVALID".
         05 ws-pay-type-invalid pic x(16) value "PAY TYPE INVALID".
         05 ws-store-num-invalid   pic x(17) value "STORE NUM INVALID".
         05 ws-invoice-format-inv  pic x(22) value
                                  "INVOICE FORMAT INVALID".
         05 ws-invoice-letters-inv pic x(23) value
                                  "INVOICE LETTERS INVALID".
         05 ws-invoice-two-same    pic x(20) value 
         "INVOICE LETTERS SAME".
         05 ws-invoice-number-invalid pic x(21) value
                                      "INVOICE RANGE INVALID".
         05 ws-invoice-dash-invalid    pic x(20) value
                                    "INVOICE DASH INVALID".
         05 ws-sku-code-empty      pic x(14) value "SKU CODE EMPTY".
      *
       77 ws-total-records     pic 9(8) value 0.
       77 ws-valid-records     pic 9(8) value 0.
       77 ws-invalid-records   pic 9(8) value 0.
       77 ws-current-error     pic 99 value 1.
       77 ws-current-record    pic 9(8) value 0.
       77 ws-has-errors        pic x value 'n'.
       77 ws-eof-flag          pic x value 'n'.
       77 ws-is-true           pic x value 'y'.
       77 ws-is-false          pic x value 'n'.
       77 ws-1                 pic 9 value 1.
       77 ws-900000            pic 9(6) value 900000.
       77 ws-100000            pic 9(6) value 100000.
      *
       procedure division.
      *
       0000-Main.
      *
           perform 1000-open-files.
           perform 2000-print-report-headings.
           perform 3000-read-file.
           perform 4000-process-records
             until ws-eof-flag = ws-is-true.
           perform 5000-prepare-summary.
           perform 6000-print-report-footers.
           perform 7000-close-files.
           goback.
      *
       1000-open-files.
      *
           open input file-01-pos-input.
           open output file-02-error-output, file-03-valid-output,
             file-04-error-report.
      *
       2000-print-report-headings.
      *
           write report-print-line from spaces.
           write report-print-line from ws-error-report-name-line.
           write report-print-line from spaces.
           write report-print-line from ws-error-report-title-line.
           write report-print-line from spaces.
           write report-print-line from ws-error-report-headings.
      *
       3000-read-file.
      *
           read file-01-pos-input
               at end
                   move ws-is-true to ws-eof-flag.
      *
       4000-process-records.
      *
           add ws-1 to ws-current-record.
           perform 4001-reset-accumulators.
      *
           perform 4100-check-errors.
      *
           if ws-has-errors equals ws-is-true then
               add ws-1 to ws-invalid-records
               move ws-current-record to ws-edl-record-num
               move pos-rec to ws-edl-raw-data
               write error-out-data from pos-rec
               write report-print-line from spaces
               write report-print-line from ws-error-detail-line
           else
               add ws-1 to ws-valid-records
               write valid-out-data from pos-rec
           end-if.
      *
           add ws-1 to ws-total-records.
      *
           perform 3000-read-file.
      *
       4001-reset-accumulators.
      *
           move ws-1        to ws-current-error.
           move ws-is-false to ws-has-errors.
           move spaces      to ws-error-detail-line.
      *
       4100-check-errors.
      *
           perform 4110-check-trans-code.
           perform 4120-check-trans-amt.
           perform 4130-check-pay-type.
           perform 4140-check-store-num.
           perform 4150-check-invoice-num.
           perform 4160-check-sku-code.
      *
       4110-check-trans-code.
      *
           if not pr-trans-code-valid then
               move ws-is-true to ws-has-errors
               move ws-trans-code-invalid to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
       4120-check-trans-amt.
      *
           if pr-trans-amt is not numeric then
               move ws-is-true to ws-has-errors
               move ws-trans-amt-invalid to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
       4130-check-pay-type.
      *
           if not pr-pay-type-valid then
               move ws-is-true to ws-has-errors
               move ws-pay-type-invalid to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
       4140-check-store-num.
      *
           if not pr-store-num-valid then
               move ws-is-true to ws-has-errors
               move ws-store-num-invalid to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
       4150-check-invoice-num.
      *
           if pr-invoice-X1 is not alphabetic or pr-invoice-X2 is not
             alphabetic or pr-invoice-000000 is not numeric then
               move ws-is-true to ws-has-errors
               move ws-invoice-format-inv to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
           if not invoice-X1-valid or not invoice-X2-valid then
               move ws-is-true to ws-has-errors
               move ws-invoice-letters-inv to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
           if pr-invoice-X1 is equal to pr-invoice-X2 then
               move ws-is-true to ws-has-errors
               move ws-invoice-two-same to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
           if pr-invoice-000000 is numeric then
               if pr-invoice-000000 > ws-900000 or pr-invoice-000000 <
                 ws-100000 then
                   move ws-is-true to ws-has-errors
                   move ws-invoice-number-invalid to ws-edl-error(
                       ws-current-error)
                   add ws-1 to ws-current-error
               end-if
           end-if.
      *
           if not invoice-dash-valid then
               move ws-is-true to ws-has-errors
               move ws-invoice-dash-invalid to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
       4160-check-sku-code.
      *
           if pr-sku-code is equal to spaces then
               move ws-is-true to ws-has-errors
               move ws-sku-code-empty to ws-edl-error(
                   ws-current-error)
               add ws-1 to ws-current-error
           end-if.
      *
       5000-prepare-summary.
      *
           move ws-total-records to ws-err-rec-read.
           move ws-valid-records to ws-erv-valid-rec.
           move ws-invalid-records to ws-eri-invalid-rec.
      *
       6000-print-report-footers.
      *
           write report-print-line from spaces.
           write report-print-line from ws-error-records-read-line.
           write report-print-line from spaces.
           write report-print-line from ws-error-records-valid-line.
           write report-print-line from ws-error-records-invalid-line.
      *
       7000-close-files.
      *
           close file-01-pos-input, file-02-error-output,
             file-03-valid-output, file-04-error-report.
      *
       end program Edit.