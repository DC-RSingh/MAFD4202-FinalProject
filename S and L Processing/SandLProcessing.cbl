       identification division.
       program-id. SandLProcessing.
       author. Raje Singh.
       date-written. 2021-04-14.
      * Program Description:
      * Performs analysis on the Sales and layaway records produced by
      * the Data Split and Count program. Calculates tax for each S&L
      * record then outputs a report.
      *
       environment division.
       input-output section.
       file-control.
      *
          select file-05-sl-data
               assign
                   to '../../../../datafiles/data/file-05-sl-data.dat'
               organization is line sequential.
      *
           select file-08-sl-report
               assign
                   to '../../../../datafiles/data/file-08-sl-report.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd file-05-sl-data
           data record is in-sale-data.
      *
       01 in-sale-data.
         05 in-trans-code      pic x.
           88 in-trans-code-sales value 'S'.
           88 in-trans-code-lay value 'L'.
         05 in-trans-amt       pic 9(5)V99.
         05 in-pay-type        pic XX.
         05 in-store-num       pic XX.
         05 in-invoice-num     pic X(9).
         05 in-sku-code        pic X(15).
      *
       fd file-08-sl-report
           data record is report-print-line.
      *
       01 report-print-line    pic x(120).
      *
       working-storage section.
      *
       01 ws-report-name-line.
         05 filler             pic x(26) value
                               "Raje Singh, Final Project".
      *
       01 ws-report-title-line.
         05 filler             pic x(10) value spaces.
         05 filler             pic x(24) value
                               "SALES AND LAYAWAY REPORT".
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
         05 filler             pic x(9) value "TAX OWING".
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
         05 ws-dl-tax-owing    pic $z(4)9.99.
      *
      *
       01 ws-summary-sl-line.
         05 filler             pic x(29) value
                               "TOTAL NUMBER OF S&L RECORDS: ".
         05 filler             pic xxxx value spaces.
         05 ws-sll-sl-total    pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-sll-sl-amount   pic $z(12)9.99.
      *
       01 ws-summary-sale-line.
         05 filler             pic x(30) value
                               "TOTAL NUMBER OF SALE RECORDS: ".
         05 filler             pic xxx value spaces.
         05 ws-sal-sale-total  pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-sal-sale-amount pic $z(12)9.99.
      *
       01 ws-summary-layaway-line.
         05 filler             pic x(33) value
                               "TOTAL NUMBER OF LAYAWAY RECORDS: ".
         05 ws-ll-layaway-total pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-ll-layaway-amount pic $z(11)9.99.
      *
       01 ws-summary-pnt-trans-for-each-pay-type occurs 3 times.
         05 filler             pic x(16) value "% NUM TRANS FOR ".
         05 ws-tpt-sl-pay-type pic xx.
         05 filler             pic x(12) value " PAY TYPE: ".
         05 ws-tpt-sl-percent  pic zz9.99.
         05 filler             pic x value '%'.
      *
       01 ws-summary-total-tax-owing-line.
         05 filler             pic x(17) value "TOTAL TAX OWING: ".
         05 ws-stt-tax-amount  pic $z(11)9.99.
      *
       01 ws-highest-store-number-line.
         05 filler             pic x(37) value
                               "STORE WITH HIGHEST S&L TRANS AMOUNT: ".
         05 ws-hsn-store-num   pic xx.
      *
       01 ws-lowest-store-number-line.
         05 filler             pic x(37) value
                               "STORE WITH LOWEST S&L TRANS AMOUNT:  ".
         05 ws-lsn-store-num   pic xx.
      *
       01 ws-calcs.
         05 ws-tax-owing       pic 9(5)V99.
      *
       77 ws-current-page      pic 999 value 1.
       77 ws-line-count        pic 99 value 0.
       77 ws-lines-per-page    pic 99 value 20.
      *
       77 ws-eof-flag          pic x value 'n'.
       77 ws-is-true           pic x value 'y'.
      *
       77 ws-1                 pic 9 value 1.
       77 ws-3                 pic 9 value 3.
       77 ws-6                 pic 9 value 6.
       77 ws-100               pic 999 value 100.
       77 ws-tax-amount        pic V99 value 0.13.
       77 ws-total-tax-owing   pic 9(6)V99 value 0.
      *
       77 ws-highest-trans-amt pic 9(5)V99 value 0.
       77 ws-lowest-trans-amt  pic 9(5)V99 value 99999.99.
      *
       01 ws-constants.
         05 ws-store-names     pic x(12) value "010203040512".
         05 ws-store-literals  redefines ws-store-names pic xx occurs 6
                               times indexed by ws-store-index.
         05 ws-payment-types   pic x(6) value "CACRDB".
         05 ws-pay-literals    redefines ws-payment-types pic xx occurs
                               3 times indexed by ws-pay-index.
      *
       77 ws-total-sl-records  pic 9(8) value 0.
       77 ws-total-trans-amt   pic 9(12)V99.
       77 ws-lay-rec-total     pic 9(8) value 0.
       77 ws-lay-total-amt     pic 9(12)V99.
       77 ws-sale-rec-total    pic 9(8) value 0.
       77 ws-sale-total-amt    pic 9(12)V99.
       77 ws-sl-pay-totals     pic 9(8) value 0 occurs 3 times indexed
                               by pay-total-index.
       77 ws-sl-store-amounts  pic 9(12)V99 value 0 occurs 6 times
                               indexed by sl-store-amt-index.
       77 ws-trans-percent     pic 999V99.
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
           open input file-05-sl-data.
           open output file-08-sl-report.
      *
       2000-print-report-headings.
      *
           write report-print-line from spaces.
           write report-print-line from ws-report-name-line.
      *
       3000-read-file.
      *
           read file-05-sl-data
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
           perform 4240-determine-sales-layaway.
           perform 3000-read-file.
      *
       4210-reset-calcs.
      *
           move zeroes to ws-calcs.
      *
       4220-calculate-tax.
      *
           compute ws-tax-owing rounded = in-trans-amt * ws-tax-amount.
           add ws-tax-owing to ws-total-tax-owing.
      *
       4230-write-detail-line.
      *
           write report-print-line from spaces.
      *
           move in-trans-code to ws-dl-trans-code.
           move in-trans-amt to ws-dl-trans-amt.
           move in-pay-type to ws-dl-pay-type.
           move in-store-num to ws-dl-store-num.
           move in-invoice-num to ws-dl-invoice-num.
           move in-sku-code to ws-dl-sku-code.
           move ws-tax-owing to ws-dl-tax-owing.
      *
           write report-print-line from ws-detail-line.
      *
       4240-determine-sales-layaway.
      *
           set ws-store-index to ws-1.
           search ws-store-literals varying ws-store-index
               when ws-store-literals(ws-store-index) = in-store-num
                   set sl-store-amt-index to ws-store-index
                   add in-trans-amt to ws-sl-store-amounts(
                       sl-store-amt-index).
      *
           if in-trans-code-sales then
               add ws-1 to ws-sale-rec-total
               add in-trans-amt to ws-sale-total-amt
           end-if.
      *
           if in-trans-code-lay then
               add ws-1 to ws-lay-rec-total
               add in-trans-amt to ws-lay-total-amt
           end-if.
      *
           add in-trans-amt to ws-total-trans-amt.
           add ws-1 to ws-total-sl-records.
      *
           set ws-pay-index to ws-1.
           search ws-pay-literals varying ws-pay-index
               when ws-pay-literals(ws-pay-index) = in-pay-type
                   set pay-total-index to ws-pay-index
                   add ws-1 to ws-sl-pay-totals(pay-total-index).
      *      
       5000-prepare-summary.
      *
           move ws-total-tax-owing to ws-stt-tax-amount.
      *
           move ws-total-sl-records to ws-sll-sl-total.
           move ws-total-trans-amt to ws-sll-sl-amount.
      *
           move ws-sale-rec-total to ws-sal-sale-total.
           move ws-sale-total-amt to ws-sal-sale-amount.
      *
           move ws-lay-rec-total to ws-ll-layaway-total.
           move ws-lay-total-amt to ws-ll-layaway-amount.
      *
           move ws-1 to ws-array-pointer.
           perform
             varying ws-array-pointer
             from ws-1 by ws-1
             until (ws-array-pointer > ws-6)
               if ws-sl-store-amounts(ws-array-pointer) >=
                 ws-highest-trans-amt then
                   move ws-sl-store-amounts(ws-array-pointer) to
                     ws-highest-trans-amt
                   move ws-store-literals(ws-array-pointer) to
                     ws-hsn-store-num
               end-if
      *
               if ws-sl-store-amounts(ws-array-pointer) <=
                 ws-lowest-trans-amt then
                   move ws-sl-store-amounts(ws-array-pointer) to
                     ws-lowest-trans-amt
                   move ws-store-literals(ws-array-pointer) to
                     ws-lsn-store-num
               end-if
           end-perform.
      *
       6000-print-report-footers.
      *
           write report-print-line from ws-summary-sl-line after
             advancing ws-3 lines.
           write report-print-line from spaces.
      *
           write report-print-line from ws-summary-sale-line.
           write report-print-line from ws-summary-layaway-line.
      *
           write report-print-line from spaces.
      *
           move ws-1 to ws-array-pointer.
           perform
             varying ws-array-pointer
             from ws-1 by ws-1
             until (ws-array-pointer > ws-3)
      *
               move ws-pay-literals(ws-array-pointer) to
                 ws-tpt-sl-pay-type(ws-array-pointer)
               compute ws-trans-percent rounded = (ws-sl-pay-totals(
                   ws-array-pointer) / ws-total-sl-records) * ws-100
               move ws-trans-percent to ws-tpt-sl-percent(
                   ws-array-pointer)
      *
               write report-print-line from
                 ws-summary-pnt-trans-for-each-pay-type(
                   ws-array-pointer)
      *
           end-perform.
      *
           write report-print-line from spaces.
      *
           write report-print-line from ws-summary-total-tax-owing-line.
      *
           write report-print-line from spaces.
      *
           write report-print-line from ws-highest-store-number-line.
           write report-print-line from ws-lowest-store-number-line.
      *
       7000-close-files.
      *
           close file-05-sl-data, file-08-sl-report.
      *
       end program SandLProcessing.