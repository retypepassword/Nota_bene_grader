# Nota Bene Grader

## First Time Setup:  

### Mandatory first time setup:  
* Copy grade_cutoffs_template.txt to grade_cutoffs.txt

In grade_cutoffs.txt:  
* Download class roster and update file path
* Set settings for Canvas
    * Get an authentication token from your [Canvas profile](https://canvas.ucdavis.edu/profile/settings)
        * This only has to be done once. The token can be reused for different
          configuration files
        * Toward the bottom, under Approved Integrations, click *New Access Token*
        * Under *Purpose*, write a descriptive name that will remind you of what this
          app is (e.g., NB Grader)
        * From the new window that pops up, copy the text under *Token* and paste into
          grade_cutoffs.txt
    * Get the Canvas course id
        * This needs to be done for each course.
        * Go to the main page for the course (e.g., https://canvas.ucdavis.edu/courses/12345)
        * Copy the number at the end (in this example, it's 12345) and paste into
          grade_cutoffs.txt
    * Get a template Grades CSV file
        * Go to the Grades page for the course and export the grades into a csv file.
        * Rename the Grades file by removing the date part of the name (e.g.
          Jan_31_07_42_) so it's just Grades-[course name].csv
        * Put the Grades file in the same place as the configuration file
        * Update the name of the Grades file with the appropriate name in the
          configuration file
  * Set settings for SmartSite
        * Update the instructor
  * Enter your NB username and password in the appropriate locations
  * Update the grading settings/score cutoffs
        * Follow instructions in the file. You can have as many or as few scores as
          desired.
        * The highest score listed is taken to be the highest possible score for the
          assignment
        * Keep the indentations
            * Scores should indented be four spaces
            * Comment/Word cutoffs should be indented an additional four spaces beyond
              that.

## Usage:  
* In the Console box in RStudio, type `source('/path/to/nbCutoff.txt')`,
  replacing  `/path/to` with the actual path to your nbCutoff.txt file 
* *Note: This version has not been tested with Rscript.* Run the script using
  `Rscript /path/to/nbCutoff.txt`, replacing `/path/to` with the actual path to
  your nbCutoff.txt file (e.g., `'C:/Users/eric/Desktop/NBGrader/nbCutoff.txt'` on
  Windows, `'/Users/eric/Desktop/NBGrader/nbCutoff.txt'` on Mac, or
  `'/home/eric/NBGrader/nbCutoff.txt'` on Linux) and follow prompts
* The script updates itself each time it's run, so don't change the script;
  changes will be overwritten
* Inspect the spot check output file for errors
    * `Orig.Total.Words` and `Total.Word.Count`
      is the total number of words in the original comment, before applying any
      workaround prevention measures.
    * `Orig.Credited.Words` and `Credited.Word.Count`
      is 0 if all the words in the comment are the same length and equal to
      `Orig.Total.Words` if they're not.
    * `Orig.Comment.Credited.Words` and `Comment.Credit.Word.Count`
      is 0 if there are insufficient numbers of words in the comment or equal to
      `Orig.Credited.Words` if not.
    * `Orig.Char` and `Character.Count`
      is the total number of characters in the comment
    * `Partial.Cred.Frac`
      is the total number of comments that were submitted on time, plus fractional
      credit given for comments that were submitted late. If GRADE_MODE is 0, this
      should always be the same as On.Time.Count.
    * `Credited.Partial.Cred.Frac`
      is the total number of comments in Partial.Cred.Frac that were sufficiently
      long
    * `Credited.On.Time`
      is the total number of sufficiently-long comments that were submitted on
      time, unless GRADE_MODE is not 0, in which case it's the total number of
      comments rounded from `Credited.Partial.Cred.Frac`
    * `Total.On.Time`
      is the total number of comments that were submitted on time, unless
      GRADE_MODE is not 0, in which case it's the total number of comments rounded
      from `Partial.Cred.Frac`
    * `Total.Comments`
      is the total number of comments that were submitted.
    * `Score` is the score the student received, based on `Word.Count` and
      `On.Time`
    * If partial credit is given for late comments, `Partial.Cred.Frac` is
      the total number of comments used for calculating the student's score. For
      example, if the student makes 4 comments, but the last comment is late by
      enough time that the partial credit function gives the student half
      credit, the student is considered to have made 3.5 comments and
      `Partial.Cred.Frac` would show 3.5 for said student.

## Dependencies:  
`httr`, `xlsx`, and `yaml`

To install `xlsx`, you will first need to install the latest version of Java, which can
be found at http://www.java.com/en/download/ or
http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html

Then, to install the `xlsx` package, open an R console and run
`install.packages('xlsx')`

To install the `httr` package, open an R console and run
`install.packages('httr')`

To install the `yaml` package, open an R console and run
`install.packages('yaml')`

You can also try uncommenting the respective lines at the top of the script, but
this procedure seems to be the most foolproof way of doing it.

