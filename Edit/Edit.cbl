       identification division.
       program-id. Edit.
       author. Raje Singh.
       date-written. 2021-04-14.
      * Program Description: 
      *
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
           88 pr-valid-trans-code value 'S', 'R', 'L'.
         05 pr-trans-amt       pic 9(5)V99.
         05 pr-pay-type        pic XX.
         05 pr-store-num       pic XX.
         05 pr-invoice-num     pic X(9).
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
       01 report-print-line    pic x(100).
      *
       working-storage section.

       procedure division.
      *
       000-Main.
      *
           goback.
      *

       end program Edit.