# Nota Bene Grader

## First Time Setup:  

### Mandatory first time setup:  
* Download class roster and update file path in the script (read script comments
  for more details)
* Update the instructor
* Enter your NB username and password in the appropriate locations

### Optional first time set-up:  
* Set grading mode and partial credit function (defaults to no partial credit)
* Uncomment and insert path to a working directory if desired (defaults to the
  same folder as the script)

## Usage:  
* Run the script using `Rscript /path/to/nbCutoff.txt`, replacing `/path/to`
  with the actual path to your nbCutoff.txt file (e.g.,
  `'C:/Users/eric/Desktop/NBGrader/nbCutoff.txt'` on Windows,
  `'/Users/eric/Desktop/NBGrader/nbCutoff.txt'` on Mac, or
  `'/home/eric/NBGrader/nbCutoff.txt'` on Linux)
  and follow prompts
* Alternatively, in the Console box in RStudio, type
  `source('/path/to/nbCutoff.txt')`, replacing  `/path/to` with the actual path
  to your nbCutoff.txt file 
* Inspect the spot check output file for errors
    * `Orig.Word` means the total number of words across all comments the
      student posted
    * `Word.Count` means the total number of words across all comments the
      student posted on time
    * Same thing for `Orig.Char` and `Character.Count`
    * `On.Time` is the total number of comments counted as on time
    * `Total.Comments` is the total number of comments posted, even if late, at
      the time of grading
    * `Score` is the score the student received, based on `Word.Count` and
      `On.Time`
    * If partial credit is given for late comments, `Time.Partial.Cred.Frac` is
      the total number of comments used for calculating the student's score. For
      example, if the student makes 4 comments, but the last comment is late by
      enough time that the partial credit function gives the student half
      credit, the student is considered to have made 3.5 comments and
      `Time.Partial.Cred.Frac` would show 3.5 for said student.

## Dependencies:  
`httr` and `xlsx`

To install `xlsx`, you will first need to install the latest version of Java, which can
be found at http://www.java.com/en/download/ or
http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html

Then, to install the `xlsx` package, open an R console and run
`install.packages('xlsx')`

To install the `httr` package, open an R console and run
`install.packages('httr')`

You can also try uncommenting the respective lines at the top of the script, but
this procedure seems to be the most foolproof way of doing it.

