# MAFD4202 Final Project (Winter 2021)

## System Description

The system is to process sales records from the Point-of-Sale (POS) devices at our stores.  

There are 3 types of transactions (Sales, Returns and Layaways), and the store accepts Cash, Credit and Debit for payment types. 

## Project Breakdown

The project contains 4 programs:

 - [Edit](https://github.com/DC-RSingh/MAFD4202-FinalProject/blob/master/Edit/Edit.cbl): Validates the records in the input file representing records from a Point-of-Sale device. Splitting up valid and invalid records into different files.
 
 - [Data Split and Count](https://github.com/DC-RSingh/MAFD4202-FinalProject/blob/master/Data%20Split%20and%20Count/DataSplitAndCount.cbl): Splits the valid data produced by the [Edit Program](https://github.com/DC-RSingh/MAFD4202-FinalProject/blob/master/Edit/Edit.cbl) into a Sales and Layaways file and Returns file. Also generates summary data.

 - [S and L Processing](https://github.com/DC-RSingh/MAFD4202-FinalProject/blob/master/S%20and%20L%20Processing/SandLProcessing.cbl): Produces a report on all Sales and Layaway records from the data file produced by the [Data Split and Count Program](https://github.com/DC-RSingh/MAFD4202-FinalProject/blob/master/Data%20Split%20and%20Count/DataSplitAndCount.cbl). Generates summary data at the end of the report.
 
 - [Returns Processing](https://github.com/DC-RSingh/MAFD4202-FinalProject/blob/master/Returns%20Processing/ReturnsProcessing.cbl): Produces a report on all Returns records from the data file produced by the [Data Split and Count Program](https://github.com/DC-RSingh/MAFD4202-FinalProject/blob/master/Data%20Split%20and%20Count/DataSplitAndCount.cbl). Generates summary data at the end of the report.
 
 ## Project Requirements
 
 In order to debug and run the programs in this solution, you will need the [Micro Focus Visual Cobol Extension for Visual Studio](https://www.microfocus.com/en-us/products/visual-cobol/overview). This project was built on Visual Studio Community 2019. You could, of course, take the COBOL files and compile and run them on whatever other platform suits you.
 
