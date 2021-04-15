       identification division.
       program-id. DataSplitAndCount.
       author. Raje Singh.
       date-written. 2021-04-14.
      * Program Description:
      * Separates valid records based on their transaction code (sale
      * or layaway) and return, outputting them to two different data
      * files for further processing.
      *
      * Also produces a report with data for each transaction type.
      *
       environment division.
       input-output section.
       file-control.
      *
           select file-03-valid-data
               assign
                  to
                   '../../../../datafiles/data/file-03-valid-data.dat'
               organization is line sequential.
      *
          select file-05-sl-data
               assign
                   to '../../../../datafiles/data/file-05-sl-data.dat'
               organization is line sequential.
      *
           select file-06-returns-data
               assign
                   to
                   '../../../../datafiles/data/file-06-returns-data.dat'
               organization is line sequential.
      *
           select file-07-split-report
               assign
                   to
                   '../../../../datafiles/data/file-07-split-report.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd file-03-valid-data
           data record is in-valid-data.
      *
       01 in-valid-data.
         05 in-trans-code      pic x.
           88 in-sl-record-check value 'S', 'L'.
           88 in-sl-sale-check value 'S'.
           88 in-sl-layaway-check value 'L'.
           88 in-returns-check value 'R'.
         05 in-trans-amt       pic 9(5)V99.
         05 in-pay-type        pic XX.
         05 in-store-num       pic XX.
         05 in-invoice-num     pic X(9).
         05 in-sku-code        pic X(15).
      *
       fd file-05-sl-data
           data record is sl-out-data.
      *
       01 sl-out-data.
         05 sl-trans-code      pic x.
         05 sl-trans-amt       pic 9(5)V99.
         05 sl-pay-type        pic XX.
         05 sl-store-num       pic XX.
         05 sl-invoice-num     pic X(9).
         05 sl-sku-code        pic X(15).
      *
       fd file-06-returns-data
           data record is r-out-data.
      *
       01 r-out-data.
         05 ro-trans-code      pic x.
         05 ro-trans-amt       pic 9(5)V99.
         05 ro-pay-type        pic XX.
         05 ro-store-num       pic XX.
         05 ro-invoice-num     pic X(9).
         05 ro-sku-code        pic X(15).
      *
       fd file-07-split-report
           data record is report-print-line.
      *
       01 report-print-line    pic x(100).
      *
       working-storage section.
      *
       01 ws-cct-report-name-line.
         05 filler             pic x(26) value
                               "Raje Singh, Final Project".
      *
       01 ws-cct-report-title-line.
         05 filler             pic x(10) value spaces.
         05 filler             pic x(25) value
                               "COUNTS AND CONTROL TOTALS".
      *
       01 ws-cct-sl-line.
         05 filler             pic x(29) value
                               "TOTAL NUMBER OF S&L RECORDS: ".
         05 filler             pic xxxx value spaces.
         05 ws-sll-sl-total    pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-sll-sl-amount   pic $z(12)9.99.
      *
       01 ws-cct-sale-line.
         05 filler             pic x(30) value
                               "TOTAL NUMBER OF SALE RECORDS: ".
         05 filler             pic xxx value spaces.
         05 ws-sal-sale-total  pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-sal-sale-amount pic $z(12)9.99.
      *
       01 ws-cct-layaway-line.
         05 filler             pic x(33) value
                               "TOTAL NUMBER OF LAYAWAY RECORDS: ".
         05 ws-ll-layaway-total pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-ll-layaway-amount pic $z(11)9.99.
      *
       01 ws-cct-sl-trans-for-each-store occurs 6 times.
         05 filler             pic x(29) value
                               "TOTAL TRANS AMOUNT FOR STORE ".
         05 ws-slfes-sl-store    pic xx.
         05 filler             pic xx value ": ".
         05 ws-slfes-sl-store-amount pic $z(11)9.99.
      *
       01 ws-cct-pnt-trans-for-each-pay-type occurs 3 times.
         05 filler             pic x(16) value "% NUM TRANS FOR ".
         05 ws-tpt-sl-pay-type pic xx.
         05 filler             pic x(12) value " PAY TYPE: ".
         05 ws-tpt-sl-percent  pic zz9.99.
         05 filler             pic x value '%'.
      *
       01 ws-cct-return-for-each-store occurs 6 times.
         05 filler             pic x(41) value
                            "TOTAL NUMBER OF RETURN RECORDS FOR STORE ".
         05 ws-rfes-r-store    pic xx.
         05 filler             pic x(2) value ": ".
         05 ws-rfes-r-total    pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-rfes-r-amount   pic $z(10)9.99.
      *
       01 ws-cct-return-line.
         05 filler             pic x(32) value
                               "TOTAL NUMBER OF RETURN RECORDS: ".
         05 ws-rl-return-total pic z(7)9.
         05 filler             pic x(11) value "   AMOUNT: ".
         05 ws-rl-return-amount pic $z(11)9.99.
      *
       01 ws-cct-grand-total-line.
         05 filler             pic x(20) value "GRAND TOTAL AMOUNT: ".
         05 ws-gtl-amount      pic $z(11)9.99.
      *
       01 ws-constants.
         05 ws-store-names     pic x(12) value "010203040512".
         05 ws-store-literals  redefines ws-store-names pic xx occurs 6
                               times indexed by ws-store-index.
      *
         05 ws-payment-types   pic x(6) value "CACRDB".
         05 ws-pay-literals    redefines ws-payment-types pic xx occurs
                               3 times indexed by ws-pay-index.
      *
       77 ws-eof-flag          pic x value 'n'.
       77 ws-is-true           pic x value 'y'.
       77 ws-1                 pic 9 value 1.
       77 ws-3                 pic 9 value 3.
       77 ws-6                 pic 9 value 6.
       77 ws-100               pic 999 value 100.
      *
       77 ws-grand-total-amt   pic 9(12)V9(2) value 0.
       77 ws-ret-rec-total     pic 9(8) value 0.
       77 ws-ret-total-amt     pic 9(12)V99.
       77 ws-trans-percnt      pic 999V99.
       77 ws-lay-rec-total     pic 9(8) value 0.
       77 ws-lay-total-amt     pic 9(12)V99.
       77 ws-sale-rec-total    pic 9(8) value 0.
       77 ws-sale-total-amt    pic 9(12)V99.
       77 ws-s-and-l-rec-total pic 9(8) value 0.
       77 ws-s-and-l-total-amt pic 9(12)V99.
      *
       77 ws-array-pointer     pic 99 value 1.
      *
       77 ws-ret-store-amounts pic 9(12)V99 value 0 occurs 6 times
                               indexed by ret-store-amt-index.
       77 ws-ret-store-totals  pic 9(8) value 0 occurs 6 times indexed
                               by ret-store-total-index.
       77 ws-sl-store-amounts  pic 9(12)V99 value 0 occurs 6 times
                               indexed by sl-store-amt-index.
       77 ws-sl-pay-totals     pic 9(8) value 0 occurs 3 times indexed
                               by pay-total-index.
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
           perform 6000-close-files.
           goback.
      *
       1000-open-files.
           open input file-03-valid-data.
           open output file-05-sl-data, file-06-returns-data,
             file-07-split-report.
      *
       2000-print-report-headings.
      *
           write report-print-line from spaces.
           write report-print-line from ws-cct-report-name-line.
           write report-print-line from spaces.
           write report-print-line from ws-cct-report-title-line.
           write report-print-line from spaces.
      *
       3000-read-file.
      *
           read file-03-valid-data
               at end
                   move ws-is-true to ws-eof-flag.
      *
       4000-process-records.
      *
           if in-returns-check then
               perform 4100-returns-record
           else
               if in-sl-record-check then
                   perform 4200-s-and-l-record
               end-if
           end-if.
      *
           perform 3000-read-file.
      *
       4100-returns-record.
      *
           write r-out-data from in-valid-data.
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
       4200-s-and-l-record.
      *
           write sl-out-data from in-valid-data.
      *
           set ws-store-index to ws-1.
           search ws-store-literals varying ws-store-index
               when ws-store-literals(ws-store-index) = in-store-num
                   set sl-store-amt-index to ws-store-index
                   add in-trans-amt to ws-sl-store-amounts(
                       sl-store-amt-index).
      *
           set ws-pay-index to ws-1.
           search ws-pay-literals varying ws-pay-index
               when ws-pay-literals(ws-pay-index) = in-pay-type
                   set pay-total-index to ws-pay-index
                   add ws-1 to ws-sl-pay-totals(pay-total-index).
      *
           if in-sl-sale-check then
               add ws-1 to ws-sale-rec-total
               add in-trans-amt to ws-sale-total-amt
           else
               if in-sl-layaway-check then
                   add ws-1 to ws-lay-rec-total
                   add in-trans-amt to ws-lay-total-amt
               end-if
           end-if.
      *
       5000-prepare-summary.
      *
           compute ws-s-and-l-rec-total = ws-sale-rec-total +
             ws-lay-rec-total.
      *
           compute ws-s-and-l-total-amt = ws-sale-total-amt +
             ws-lay-total-amt.
      *
           compute ws-grand-total-amt = ws-s-and-l-total-amt -
             ws-ret-total-amt.
      *
           move ws-s-and-l-rec-total to ws-sll-sl-total.
           move ws-s-and-l-total-amt to ws-sll-sl-amount.
           write report-print-line from ws-cct-sl-line.
           write report-print-line from spaces.
      *
           move ws-sale-rec-total to ws-sal-sale-total.
           move ws-sale-total-amt to ws-sal-sale-amount.
           write report-print-line from ws-cct-sale-line.
           write report-print-line from spaces.
      *
           move ws-lay-rec-total to ws-ll-layaway-total.
           move ws-lay-total-amt to ws-ll-layaway-amount.
           write report-print-line from ws-cct-layaway-line.
           write report-print-line from spaces.
      *
           move ws-1 to ws-array-pointer.
           perform
             varying ws-array-pointer
             from ws-1 by ws-1
             until (ws-array-pointer > ws-6)
      *
               move ws-store-literals(ws-array-pointer) to
                 ws-slfes-sl-store(ws-array-pointer)
               move ws-sl-store-amounts(ws-array-pointer) to
                 ws-slfes-sl-store-amount(ws-array-pointer)
      *
               write report-print-line from
                 ws-cct-sl-trans-for-each-store(ws-array-pointer)
      *
           end-perform.
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
               compute ws-trans-percnt rounded = (ws-sl-pay-totals(
                   ws-array-pointer) / ws-s-and-l-rec-total) * ws-100
               move ws-trans-percnt to ws-tpt-sl-percent(
                   ws-array-pointer)
      *
               write report-print-line from
                 ws-cct-pnt-trans-for-each-pay-type(ws-array-pointer)
      *
           end-perform.
      *
           write report-print-line from spaces.
      *
           move ws-1 to ws-array-pointer.
           perform
             varying ws-array-pointer
             from ws-1 by ws-1
             until (ws-array-pointer > ws-6)
      *
               move ws-store-literals(ws-array-pointer) to
                 ws-rfes-r-store(ws-array-pointer)
               move ws-ret-store-amounts(ws-array-pointer) to
                 ws-rfes-r-amount(ws-array-pointer)
               move ws-ret-store-totals(ws-array-pointer) to
                 ws-rfes-r-total(ws-array-pointer)
      *
               write report-print-line from
                 ws-cct-return-for-each-store(ws-array-pointer)
      *
           end-perform.
      *
           write report-print-line from spaces.
      *
           move ws-ret-rec-total to ws-rl-return-total.
           move ws-ret-total-amt to ws-rl-return-amount.
           write report-print-line from ws-cct-return-line.
           write report-print-line from spaces.
      *
           move ws-grand-total-amt to ws-gtl-amount.
           write report-print-line from ws-cct-grand-total-line.
      *
      *
       6000-close-files.
      *
           close file-03-valid-data, file-05-sl-data,
             file-06-returns-data, file-07-split-report.
      *
       end program DataSplitAndCount.