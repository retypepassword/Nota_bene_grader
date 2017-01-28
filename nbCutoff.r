##remove the below '#' to install the package before opening xlsx or httr
#install.packages('xlsx')
suppressPackageStartupMessages(library(xlsx))
#install.packages('httr')
library(httr)

################################################################################
#                              User Settings                                   #
################################################################################

### Set Parameters for Analysis
# Make a folder to hold all NB files- paste its path below. Defaults to the R
# script's directory. Put path here if you want a different directory:
# working.directory <- "/Users/jeffreymiller/Desktop/NB Grades"

### Download the latest roster and put the path to this file. If you put the
# roster in folder specified above (or the same folder as the R script if no
# folder is specified), you can just put the filename.
# Otherwise, you'll need the whole path:
roster <- "BIS_002A_B01_B29_WQ_2017_1_26_17.xls"

### Change only once
instruct <- "Facciotti"

### Put login info for Nota Bene here:
EMAIL <- ""
PASSWORD <- ""

# Grading mode (choice of 0: No credit if late, 1: Partial credit if late 1
# minute or less, 2: Partial credit if late 2 minutes or less, etc.)
#
# Partial credit is applied using the function:
# f(x) = (-e^((2/a)*e*x-(2*e-ln(5)))+5)/5
# where e is the constant, x is minutes after due date, and a is minutes past
# the due date after which no partial credit is applied. f(x) is the percentage
# of full comment credit given to a comment expressed as a decimal.
GRADE_MODE <- 0

PARTIAL_CREDIT <- function(x) {
    (-exp((2/GRADE_MODE) * exp(1) * x - (2 * exp(1) - log(5))) + 5) / 5
}

# Don't count comments with fewer than:
MIN_WORDS <- 0 # words (to prevent comments like "a" to make it seem like there are five comments) and an average of
AVG_CHARS_PER <- 0 # characters per word (to prevent comments like "a a a a a" to get around the word limit)
WORDS_NOT_BE_SAME_LENGTH <- TRUE # make sure the words are different words (to prevent comments like "something
                              # something something something something" to get around the word limit and
                              # character average limit)

score <- function(stats) {
    # Replace Comment.Credit.Word.Count and Credited.On.Time with the more
    # readable Word.Count and On.Time, respectively.
    names(stats) <- replace(names(stats), c(8, 14), c('Word.Count', 'On.Time'))

####Evaluate and assign score ###CHANGE VALUES HERE
    # Students with at least 4 sufficiently-long comments (or at least 10 short
    # comments) and a total of at least 35 words across the 4 comments get 5 points
    sel <- (stats$On.Time >= 4 & stats$Word.Count >= 35)
    stats$Score[sel & stats$Score < 0] <- 5
    sel <- (stats$On.Time >= 3 & stats$Word.Count >= 35)
    stats$Score[sel & stats$Score < 0] <- 4
    sel <- (stats$On.Time >= 3 & stats$Word.Count >= 25)
    stats$Score[sel & stats$Score < 0] <- 3
    sel <- (stats$On.Time >= 2 & stats$Word.Count >= 18)
    stats$Score[sel & stats$Score < 0] <- 2
    sel <- (stats$On.Time >= 1 & stats$Word.Count >= 9)
    stats$Score[sel & stats$Score < 0] <- 1
    # Students who didn't answer at all or had fewer than 9 words in their answers
    stats$Score[stats$Score < 0] <- 0

    # Undo name changes made in beginning of function
    names(stats) <- replace(names(stats), c(8, 14), c('Comment.Credit.Word.Count', 'Credited.On.Time'))

    stats
}

# Don't need to change anything below this line ################################

################################################################################
#                            Configuration Functions                           #
################################################################################

# Lifted from http://stackoverflow.com/a/15373917
rScriptParms <- function() {
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmdArgs)
    list(cmdArgs = cmdArgs, needle = needle, match = match)
}

usingRScript <- function(rsParms) {
    if (missing(rsParms)) {
        rsParms <- rScriptParms()
    }
    length(rsParms$match > 0)
}

thisFile <- function() {
    rsParms <- rScriptParms()

    if (usingRScript(rsParms)) {
        # Rscript
        normalizePath(sub(rsParms$needle, "", rsParms$cmdArgs[rsParms$match]))
    }
    else {
        # source'd
        normalizePath(sys.frames()[[1]]$ofile)
    }
}

if (exists("working.directory")) {
    setwd(working.directory)
} else {
    this.dir <- dirname(thisFile())
    setwd(this.dir)
}

################################################################################
#                                Data Retrieval                                #
################################################################################

STDIN <- file("stdin")

# Set User Agent string so it doesn't look like we're scraping data with R.
# Change to something else (e.g., Chrome) if desired. Should have no effect
# on anything.
# Firefox 44.0 on Linux
ua <- add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64; rv:44.0) Gecko/20100101 Firefox/44.0")
# Chrome 45.0 on Windows 10
# ua <- add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.93 Safari/537.36")
# Safari 601.1.56 on OS X
# ua <- add_headers("User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11) AppleWebKit/601.1.56 (KHTML, like Gecko) Version/9.0 Safari/601.1.56")

# Get ckey for all subsequent requests.
l <- POST(url = "http://nb.mit.edu/pdf4/rpc?guest=1",
    body = list(a = paste('{"email":"', EMAIL , '","password":"', PASSWORD, '"}', sep = ""),
        cid = 0,
        f = 'login_user'), 
    encode = "form",
    config = c(ua))
login_data <- content(l, type = "application/json")
ckey <- login_data$payload$ckey

# Set ckey cookie
ckey_cookie <- set_cookies(ckey = ckey)

# Set rest of cookies (server returns the rest of the cookies when you request
# the page with a ckey). Still need to use ckey_cookie in future requests,
# though.
invisible(GET(url = "http://nb.mit.edu", ua, ckey_cookie))
 
# Get ids for nota bene assignments
nota_bene_assigs <- POST(url = paste("http://nb.mit.edu/pdf4/rpc?guest=1&ckey=", ckey, sep = ""),
    body = list(a = '{"types":["ensembles", "folders", "files", "sections"]}',
        cid = 0,
        f = 'getObjects'),
    encode = "form",
    config = c(ckey_cookie, ua))
 
# Display nota bene ids (internal to the nota bene website) so user can select
# the one for which to retrieve data
nb_assig_data <- content(nota_bene_assigs, type = "application/json")

# Figure out which class to get nota benes for
class_ids <- data.frame(classes = mapply(function(x) nb_assig_data$payload$ensembles[[x]]$name, names(nb_assig_data$payload$ensembles)))
print(class_ids)

class_id_is_valid <- function(class_id) ! is.null(nb_assig_data$payload$ensembles[[as.character(class_id)]])
repeat {
    if (usingRScript()) {
        cat("Enter the id of the class (Ctrl + D to quit): ")
        class_id <- as.numeric(readLines(STDIN, 1))
    } else {
        class_id <- as.numeric(readline("Enter the id of the class: "))
    }

    if (class_id_is_valid(class_id)) break
    # Give error message for invalid ids:
    cat("Invalid Class ID entered. Please try again.\n")
}

# Figure out which nota bene to retrieve
titles_ids <- data.frame(titles = mapply(function(x) nb_assig_data$payload$files[[x]]$title, Filter(function(x) nb_assig_data$payload$files[[x]]$id_ensemble == class_id, names(nb_assig_data$payload$files))))
print(titles_ids)

nb_id_is_valid <- function(nb_id) ! is.null(nb_assig_data$payload$files[[as.character(nb_id)]])
repeat {
    if (usingRScript()) {
        cat("Enter the id of the Nota Bene you wish to retrieve (Ctrl + D to quit): ")
        nb_id <- as.numeric(readLines(STDIN, 1))
    } else {
        nb_id <- as.numeric(readline("Enter the id of the Nota Bene you wish to retrieve: "))
    }

    if (nb_id_is_valid(nb_id)) break
    # Give error message for invalid ids:
    cat("Invalid ID entered. Please try again.\n")
}

# Needed for Smartsite-friendly output
if (usingRScript()) {
    cat("Enter the assignment number (1 - 10): ")
    assign_num <- as.numeric(readLines(STDIN, 1))
} else {
    assign_num <- as.numeric(readline("Enter the assignment number (1 - 10): "))
}

# Get the due date for the retrieved nota bene (assuming the displayed time
# is written in Pacific Time). Convert to UTC because comment submission times
# are stored in UTC. Store as Pacific Time (no tz defaults to local time) even
# though it's actually UTC so less code has to be written later.
due_date <- as.POSIXct(
    format(
        as.POSIXct(nb_assig_data$payload$files[[as.character(nb_id)]]$due,
        format = "%FT%T",
        tz = "America/Los_Angeles"),
     tz = "UTC")
)

# Should be exactly the same as the class id entered above
ensemble_id <- as.numeric(nb_assig_data$payload$files[[as.character(nb_id)]]$id_ensemble)

cat("Getting data. Please wait...\n")

# Get the data for all students
students <- POST(url = paste("http://nb.mit.edu/pdf4/rpc?ckey=", ckey, sep = ""),
    body = list(a = paste('{"id_ensemble":', ensemble_id, '}', sep = ""),
        cid = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
        f = 'getMembers'),
    encode = "form",
    config = c(ckey_cookie, ua))
student_data <- content(students, type = "application/json")
cat("Keep waiting...\n")

# Get the data for the requested nota bene
nota_bene <- POST(url = paste("http://nb.mit.edu/pdf4/rpc?ckey=", ckey, sep = ""),
    body = list(a = paste('{"file":', nb_id, '}', sep = ""),
        cid = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
        f = 'getNotes'),
    encode = "form",
    config = c(ckey_cookie, ua))
nb_data <- content(nota_bene, type = "application/json")
cat("Keep waiting...\n")

# Get just the full name, email, creation time, and comments
# Get just the full name, email, creation time, and comments
get_data <- function(comment) {
    student_info <- student_data$payload[[as.character(comment$id_author)]]
    if (is.null(student_info)) {
        c(gsub(" [^ ]+$\\s*", "", comment$fullname), gsub("^[^ ]+\\s*", "", comment$fullname), NA, comment$ctime, cleanHTML(comment$body))
    }
    else {
        c(student_info$firstname, student_info$lastname, student_info$email, comment$ctime, cleanHTML(comment$body))
    }
}

# Clean up HTML tags
cleanHTML <- function(comment_body) {
    gsub("<.*?>", "", comment_body)
}

data <- t(sapply(nb_data$payload$comments, get_data))
rownames(data) <- data[,3]
colnames(data) <- c("First Name", "Last Name", "Email", "Time", "Comment")

################################################################################
#                               Score Calculation                              #
################################################################################

cat("Calculating scores...\n")

#set workfile name from assignment name
nb_file <- nb_assig_data$payload$files[[as.character(nb_id)]]$title
outfilename <- substr(basename(nb_file), 1, nchar(basename(nb_file)) - 4)

###Split out worksheet into in objects
rost <- read.xlsx(roster, 1)

# As of 3/3/16, on the Nota Bene website, the character count, comment count,
# and word count statistics for the XLS file (when one clicks Download as XLS)
# are calculated within a database query. See Line 312 in
# github:nbproject/nbproject/apps/base/annotations.py in commit c6bb13a (Jan 10,
# 2016) for the relevant query.
# URL: https://github.com/nbproject/nbproject/blob/c6bb13ae30d992d03d9fb4ecad75e91af5208df4/apps/base/annotations.py#L312
#
# Comment count is calculated using: count(v.id)
# Word count is calculated using whitespace: sum(array_length(regexp_split_to_array(c.body, E'\\\\s+'), 1))
# Character count is calculated using: sum(length(c.body))
#
# These data can easily be reproduced in R from the JSON data:

# Calculates word count for each comment
count_words <- function(comment) {
    # If the comment's blank, it's blank
    if (nchar(gsub("\\s+", "", comment)) == 0)
        return(c(0, 0))

    # Count number of characters per word. Need gsub to remove leading spaces
    # because otherwise, the leading spaces are counted as a word of 0 length.
    word_char_nums <- sapply(strsplit(gsub("^\\s+", "", comment), "\\s+"), FUN = nchar)

    # Count number of words with the same length as the first word
    same_length_words <- sum(word_char_nums == word_char_nums[1])

    # Count number of words
    total_word_count <- length(word_char_nums)

    # if same_length_words is equal to the total word count, there be trouble.
    if (WORDS_NOT_BE_SAME_LENGTH && same_length_words == total_word_count)
        return(c(0, total_word_count))

    c(total_word_count, total_word_count)
}

# Calculates character count for each comment
count_characters <- function(comment) {
    nchar(comment)
}

# Convenience function to string the word count and character count into a list
count_words_and_characters <- function(comment) {
    counted <- count_words(comment)
    total_words <- counted[2]
    words <- counted[1]
    credited_words <- words
    characters <- count_characters(comment)
    
    if (words < MIN_WORDS) credited_words <- 0
    if (words == 0 || (characters / words) < AVG_CHARS_PER) credited_words <- 0

    c(total_words, words, credited_words, characters)
}

# Returns (1, 1) for on time comments and (0, 1) for late comments or something
# in between when partial credit is allowed. The lists can be summed up later to
# obtain, for each student, the amount of credit given for each comment and the
# total number of comments
on_time_credit <- function(x) {
    comment_date <- as.POSIXct(x, format='%FT%T')
    # When summed, becomes
                # On.Time                        Total.Comments
    if (GRADE_MODE <= 0) {
        c(ifelse(comment_date < due_date, 1, 0), 1)
    }
    else {
        c(partial_credit_func(comment_date, due_date), 1)
    }
}

# Sets word/character counts for each comment to 0 (or a fraction of the count)
# if the comment was not on time so that the comment's word/character counts are
# not included in the student's sum.
modify_late <- function(word_character_counts, on_time_data) {
    # Set to 0 if late at all
    # word_character_counts * floor(on_time_data[,1])
    # Set to a fraction if only a little late; 0 otherwise
    cbind(word_character_counts, word_character_counts * on_time_data[,1])
}

# Sets a comment to 0 (instead of 1) if it doesn't have the sufficient number of
# words (i.e., the comment essentially doesn't count if there aren't enough
# words). Uses the credited_words column from word_character_counts
modify_min <- function(word_character_counts, comment_count) {
    sufficiently_long_comment_count <- comment_count[,1]
    sufficiently_long_comment_count[word_character_counts[,3] == 0] <- 0
    # When summed, becomes
          # Credited                       # On.Time, Total.Comments
    cbind(sufficiently_long_comment_count, comment_count)
}

# Used to evaluate each comment for degree of lateness and applies partial
# credit accordingly
partial_credit_func <- function(date, due_date) {
    # Give full credit if the comment is not late at all
    minutes_late <- as.double(date - due_date, units = "mins")
    if (minutes_late <= 0) {
        return(1)
    }

    # Apply partial credit as a fraction according to the partial credit
    # function if the comment is late. Don't give negative credit.
    percentage <- PARTIAL_CREDIT(minutes_late)
    ifelse(percentage < 0, 0, percentage)
}

# Figure out which comments are on time and sum the number of on-time comments
# (this part: sapply(FUN = on_time_credit, data[, 'Time']))
# for each e-mail address
# (the outer part: aggregate by list(data[, 'Email']) using sum)
on_time_counts <- t(sapply(FUN = on_time_credit, data[, 'Time']))

# Sum word counts and character counts for each e-mail address
individual_comment_statistics <- t(sapply(data[, "Comment"],
    FUN = count_words_and_characters))

comment_statistics <- aggregate(
    modify_late(individual_comment_statistics, on_time_counts),
    by = list(data[, "Email"]), FUN = sum)

on_time_and_long_enough_count <- modify_min(individual_comment_statistics, on_time_counts)

# Number of on-time comments
on_time_count <- aggregate(on_time_and_long_enough_count,
    by = list(data[, 'Email']), FUN = sum)

# Clarify what output columns are for word statistics
# * Orig.Total.Words and Total.Word.Count
#   is the total number of words in the original comment, before applying any
#   workaround prevention measures.
# * Orig.Credited.Words and Credited.Word.Count
#   is 0 if all the words in the comment are the same length and equal to
#   Orig.Total.Words if they're not.
# * Orig.Comment.Credited.Words and Comment.Credit.Word.Count
#   is 0 if there are insufficient numbers of words in the comment or equal to
#   Orig.Credited.Words if not.
# * Orig.Char and Character.Count
#   is the total number of characters in the comment
# The second set of 4 columns is the same as the first set of 4 columns, but
# after being multiplied by 1 or 0, depending on whether or not the comment was
# on time.
names(comment_statistics) <- c("Email.Address", "Orig.Total.Words", "Orig.Credited.Words", "Orig.Comment.Credited.Words", "Orig.Char", "Total.Word.Count", "Credited.Word.Count", "Comment.Credit.Word.Count", "Character.Count")

# Round off number of comments so something like 3.99 comments (due to partial
# credit calculation / being less than a minute late) doesn't lose points.
on_time_count <- cbind(on_time_count, round(on_time_count[,3]), round(on_time_count[,2]))

# Reorder columns so they're in a more logical order
on_time_count <- on_time_count[, c(1, 4, 3, 2, 5, 6)]

# TODO: Total word count not giving the correct values. One-word answers are not
# included in the total word count even though the strict settings are both 0.

# Clarify what output columns are for comment statistics
# * Partial.Cred.Frac
#   is the total number of comments that were submitted on time, plus fractional
#   credit given for comments that were submitted late. If GRADE_MODE is 0, this
#   should always be the same as On.Time.Count.
# * Credited.Partial.Cred.Frac
#   is the total number of comments in Partial.Cred.Frac that were sufficiently
#   long
# * Credited.On.Time
#   is the total number of sufficiently-long comments that were submitted on
#   time, unless GRADE_MODE is not 0, in which case it's the total number of
#   comments rounded from Credited.Partial.Cred.Frac
# * Total.On.Time
#   is the total number of comments that were submitted on time, unless
#   GRADE_MODE is not 0, in which case it's the total number of comments rounded
#   from Partial.Cred.Frac
# * Total.Comments
#   is the total number of comments that were submitted.
names(on_time_count) <- c("Email.Address", "Total.Comments", "Partial.Cred.Frac", "Credited.Partial.Cred.Frac", "Total.On.Time", "Credited.On.Time")

# Merge the two statistics data.frames
stats <- data.frame(merge(comment_statistics, on_time_count, by = "Email.Address"), Score = -1)
stats <- score(stats)

# Merge scores with roster via email address
mg <- merge(rost, stats, "Email.Address", all.x = TRUE) ## contains email and stu id

# Replace NAs from merge with 0s (looks nicer)
mg[is.na(mg)] <- 0

# Find duplicate student ids and remove the one(s) with the lower score (if
# there are duplicates and if there's one with a lower score)
duplicates <- mg[mg$User.ID %in% mg$User.ID[duplicated(mg$User.ID)], ]

# Get highest score of duplicates
dupScores <- duplicates[order(duplicates$Score, decreasing=TRUE), ]
highDupScores <- dupScores[unique(match(duplicates$User.ID, dupScores$User.ID)),]
lowDupScores <- dupScores[!rownames(dupScores) %in% rownames(highDupScores),]

# Write New Workbook for sorting and spot checks (contains all data used for
# grading and then some)
output1 <- createWorkbook()
sheet <- createSheet(output1, sheetName="results")
addDataFrame(mg, sheet)

# Highlight deleted (anything with something other than the highest score)
# duplicate student id rows in sort sheet
dupRows <- as.numeric(rownames(lowDupScores))
highlight <- CellStyle(output1) + Fill(backgroundColor = "red1")
invisible(lapply(getCells(getRows(sheet, rowIndex = dupRows + 1)),
    FUN = function(x) { setCellStyle(x, highlight) }))

saveWorkbook(output1, paste(outfilename, "_processed_45_25_", instruct, ".xlsx", sep=""))

####Write new notebook for uploading assignment to smartsite
# No-structure gradebook (only grades). Grade and student data. Use this
# approach if you have already set up your gradebook and you want to add
# scores/comments for new grade items, or you want to add or overwrite
# scores/comments for existing items.

###Create New Data Frame For Output
data2 <- data.frame(Student.Id=mg$User.ID, New.Item=mg$Score)

# Filter out low-score duplicates
data2 <- data2[!rownames(data2) %in% rownames(lowDupScores), ]

#Rename Columns to SmartSite-Friendly Format
names(data2) = c("Student Id", paste("NB", assign_num, "[5]", sep=""))

####Write new file
output2 <- createWorkbook()
sheet <- createSheet(output2, sheetName="sheet0")
addDataFrame(data2, sheet, row.names=FALSE)
saveWorkbook(output2, paste(outfilename, "_processed_45_25_", instruct, "_upload.xlsx", sep=""))
###
##### Import the above file into smartsite. Appears in the working directory (NB folder) ###

