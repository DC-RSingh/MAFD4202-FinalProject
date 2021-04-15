       identification division.
       program-id. ReturnsProcessing.
       author. Raje Singh.
       date-written. 2021-04-15.
      * Program Description:
      * Performs analysis on the Returns records produced by
      * the Data Split and Count program.  Calculates tax owed to us 
      * for each returns record then outputs a report.
      *
       environment division.
       input-output section.
       file-control.
      *
          select file-06-returns-data
               assign
                   to 
                   '../../../../datafiles/data/file-06-returns-data.dat'
               organization is line sequential.
      *
           select file-09-r-report
               assign
                   to '../../../../datafiles/data/file-09-r-report.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd file-06-returns-data
           data record is in-returns-data.
      *
       01 in-returns-data.
         05 in-trans-code      pic x.
         05 in-trans-amt       pic 9(5)V99.
         05 in-pay-type        pic XX.
         05 in-store-num       pic XX.
         05 in-invoice-num     pic X(9).
         05 in-sku-code        pic X(15).
      *
       fd file-09-r-report
           data record is report-print-line.
      *
       01 report-print-line    pic x(100).
      *
       working-storage section.
      *
       01 ws-report-name-line.
         05 filler             pic x(26) value
                               "Raje Singh, Final Project".
      *
       01 ws-report-title-line.
         05 filler             pic x(20) value spaces.
         05 filler             pic x(14) value
                               "RETURNS REPORT".
         05 filler             pic x(10) value spaces.
         05 filler             pic x(4) value "PAGE".
         05 ws-title-page-num  pic zz9.
      *
       01 ws-page-heading.
         05 filler             pic x(10) value "TRANS CODE".
         05 filler             pic x(2) value spaces.
         05 filler             pic x(12) value "TRANS AMOUNT".
         05 filler             pic x(2) value spaces.
         05 filler             pic x(8) value "PAY TYPE".
         05 filler             pic x(2) value spaces.
         05 filler             pic x(7) value "STORE #".
         05 filler             pic x(2) value spaces.
         05 filler             pic x(9) value "INVOICE #".
         05 filler             pic x(2) value spaces.
         05 filler             pic x(8) value "SKU CODE".
         05 filler             pic x(9) value spaces.
         05 filler             pic x(9) value "TAX OWED".
      *
       01 ws-detail-line.
         05 filler             pic x(5) value spaces.
         05 ws-dl-trans-code   pic x.
         05 filler             pic xxxxxxx value spaces.
         05 ws-dl-trans-amt    pic $z(4)9.99.
         05 filler             pic xxxx value spaces.
         05 ws-dl-pay-type     pic xx.
         05 filler             pic x(8) value spaces.
         05 ws-dl-store-num    pic xx.
         05 filler             pic x(7) value spaces.
         05 ws-dl-invoice-num  pic x(9).
         05 filler             pic x(2).
         05 ws-dl-sku-code     pic x(15).
         05 filler             pic xx value spaces.
         05 ws-dl-tax-owed     pic $z(4)9.99.
      *
       01 ws-summary-return-for-each-store occurs 6 times.
         05 filler             pic x(41) value
                            "TOTAL NUMBER OF RETURN RECORDS FOR STORE ".
         05 ws-rfes-r-store    pic xx.
         05 filler             pic x(2) value ": ".
         05 ws-rfes-r-total    pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-rfes-r-amount   pic $z(10)9.99.
      *
       01 ws-summary-return-line.
         05 filler             pic x(32) value
                               "TOTAL NUMBER OF RETURN RECORDS: ".
         05 ws-rl-return-total pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-rl-return-amount pic $z(11)9.99.
      *
       01 ws-summary-total-tax-owed-line.
         05 filler             pic x(22) value "TOTAL TAX OWED TO US: ".
         05 ws-stt-tax-amount  pic $z(11)9.99.
      *
       01 ws-calcs.
         05 ws-tax-owed-to-us  pic 9(5)V99.
      *
       01 ws-constants.
         05 ws-store-names     pic x(12) value "010203040512".
         05 ws-store-literals  redefines ws-store-names pic xx occurs 6
                               times indexed by ws-store-index.
      *
       77 ws-current-page      pic 999 value 1.
       77 ws-line-count        pic 99 value 0.
       77 ws-lines-per-page    pic 99 value 20.
      *
       77 ws-eof-flag          pic x value 'n'.
       77 ws-is-true           pic x value 'y'.
      *
       77 ws-1                 pic 9 value 1.
       77 ws-2                 pic 9 value 2.
       77 ws-6                 pic 9 value 6.
      *
       77 ws-tax-amount        pic V99 value 0.13.
       77 ws-total-tax-owed    pic 9(6)V99 value 0.
       77 ws-ret-rec-total     pic 9(8) value 0.
       77 ws-ret-total-amt     pic 9(12)V99.
      *
       77 ws-ret-store-amounts pic 9(12)V99 value 0 occurs 6 times
                               indexed by ret-store-amt-index.
       77 ws-ret-store-totals  pic 9(8) value 0 occurs 6 times indexed
                               by ret-store-total-index.
      *
       77 ws-array-pointer     pic 99 value 1.
      *
       procedure division.
       0000-Main.
      *
           perform 1000-open-files.
           perform 2000-print-report-headings.
           perform 3000-read-file.
           perform 4000-process-pages
             until ws-eof-flag equals ws-is-true.
           perform 5000-prepare-summary.
           perform 6000-print-report-footers.
           perform 7000-close-files.
           goback.
      *
       1000-open-files.
      *
           open input file-06-returns-data.
           open output file-09-r-report.
      *
       2000-print-report-headings.
      *
           write report-print-line from spaces.
           write report-print-line from ws-report-name-line.
      *
       3000-read-file.
      *
           read file-06-returns-data
               at end
                   move ws-is-true to ws-eof-flag.
      *
       4000-process-pages.
      *
           perform 4100-print-page-headings.
           perform 4200-process-lines
             varying ws-line-count from ws-1 by ws-1
             until (ws-line-count > ws-lines-per-page or
             ws-eof-flag = ws-is-true).
      *
       4100-print-page-headings.
      *
           write report-print-line from spaces.
      *
           move ws-current-page to ws-title-page-num.
      *
           if ws-current-page equals ws-1 then
               write report-print-line from spaces
               write report-print-line from ws-report-title-line
           else
               write report-print-line from ws-report-title-line
                 after advancing page
           end-if.
      *
           add ws-1 to ws-current-page.
           write report-print-line from spaces.
           write report-print-line from ws-page-heading.
      *
       4200-process-lines.
      *
           perform 4210-reset-calcs.
           perform 4220-calculate-tax.
           perform 4230-write-detail-line.
           perform 4240-determine-returns.
      *
           perform 3000-read-file.
      *
       4210-reset-calcs.
      *
           move zeroes to ws-calcs.
      *
       4220-calculate-tax.
      *
           compute ws-tax-owed-to-us rounded = in-trans-amt *
             ws-tax-amount.
           add ws-tax-owed-to-us   to ws-total-tax-owed.
      *
       4230-write-detail-line.
      *
           write report-print-line from spaces.
      *
           move in-trans-code      to ws-dl-trans-code.
           move in-trans-amt       to ws-dl-trans-amt.
           move in-pay-type        to ws-dl-pay-type.
           move in-store-num       to ws-dl-store-num.
           move in-invoice-num     to ws-dl-invoice-num.
           move in-sku-code        to ws-dl-sku-code.
           move ws-tax-owed-to-us  to ws-dl-tax-owed.
      *
           write report-print-line from ws-detail-line.
      *
       4240-determine-returns.
      *
           set ws-store-index to ws-1.
           search ws-store-literals varying ws-store-index
               when ws-store-literals(ws-store-index) = in-store-num
                   set ret-store-amt-index to ws-store-index
                   add in-trans-amt to ws-ret-store-amounts(
                       ret-store-amt-index)
                   set ret-store-total-index to ws-store-index
                   add ws-1 to ws-ret-store-totals(
                       ret-store-total-index).
      *
           add ws-1 to ws-ret-rec-total.
           add in-trans-amt to ws-ret-total-amt.
      *
       5000-prepare-summary.
      *
           move ws-total-tax-owed to ws-stt-tax-amount.
      *
           move ws-ret-rec-total to ws-rl-return-total.
           move ws-ret-total-amt to ws-rl-return-amount.
      *
       6000-print-report-footers.
      *
           write report-print-line from spaces after advancing ws-2
             lines.
      *
           move ws-1 to ws-array-pointer.
           perform
             varying ws-array-pointer
             from ws-1 by ws-1
             until (ws-array-pointer > ws-6)
               move ws-store-literals(ws-array-pointer) to
                 ws-rfes-r-store(ws-array-pointer)
               move ws-ret-store-amounts(ws-array-pointer) to
                 ws-rfes-r-amount(ws-array-pointer)
               move ws-ret-store-totals(ws-array-pointer) to
                 ws-rfes-r-total(ws-array-pointer)
      *
               write report-print-line from
                 ws-summary-return-for-each-store(ws-array-pointer)
           end-perform.
      *
           write report-print-line from spaces.
      *
           write report-print-line from ws-summary-return-line.
      *
           write report-print-line from spaces.
      *
           write report-print-line from ws-summary-total-tax-owed-line.
      *
       7000-close-files.
      *
           close file-06-returns-data, file-09-r-report.
      *
       end program ReturnsProcessing.