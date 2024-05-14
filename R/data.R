#' Methods to load and prepare data

####################################################################################
# Everyday Discrimination scale (EDS)
# 
# 11-items no reverse coding, no subscales
####################################################################################
COLUMN_RENAMES <- list(
    "Participant ID:"="ID",
    "You are treated with less courtesy than other people."="Q1",
    "You are treated with less respect than other people."="Q2",
    "You receive poorer service than other people at restaurants or stores."="Q3",
    "People act as if they think you are not smart."="Q4",
    "People act as if they are afraid of you."="Q5",
    "People act as if they think you are dishonest."="Q6",
    "People act as if they're better than you."="Q7",
    "You are called names or insulted."="Q8",
    "You are threatened or harassed."="Q9",
    "You are followed around in stores."="Q10",
    #"You are discriminated against.", ##### REMOVE
    "What do you think is the main reason for these experiences?"="Q11"
)
RAW_NAMES = names(COLUMN_RENAMES)
NEW_NAMES = unlist(COLUMN_RENAMES)

#' Load the item response data
#'
#' Before using this function, download the spreadsheet at
#'    https://docs.google.com/spreadsheets/d/18DEOWuUOuWg4zhFTpcX0g0cfjSHpzYAT
#' into your ~/Downloads directory.
#'
#' @param base.path Path to where the 
load_data = function(data_path="~/Downloads/CGHP_21_62.xlsx"){
    # Load data
    library("readxl")
    df <-readxl::read_excel("~/Downloads/CGHP_21_62.xlsx")
    
    # Select and rename the columns of interest
    df <- data.frame(df[, RAW_NAMES, drop = FALSE])
    names(df) <- NEW_NAMES
    return(df)
}
####################################################################################
# Collective Self-Esteem (CSE)
# https://www.asc.ohio-state.edu/psychology/crockerlab/cse.php
#
#  reverse-score answers to items 2, 4, 5, 7, 10, 12, 13, and 15,
#  such that (1 = 7), (2 = 6), (3 = 5), (4 = 4), (5 = 3), (6 = 2), (7 = 1).
#
# Items 1, 5, 9 and 13 = Membership self-esteem.
# Items 2, 6, 10 and 14 = Private collective self-esteem.
# Items 3, 7, 11, and 15 = Public collective self-esteem.
# Items 4, 8, 12, and 16 = Importance to Identity.
####################################################################################
COLUMN_RENAMES_CSE <- list(
    "Participant ID:",
    "I am a worthy member of my race/ethnic group."="Q1", 
    "I often regret that I belong to my racial/ethnic group."="Q2", 
    "Overall, my racial/ethnic group is considered good by others."="Q3", 
    "Overall, my race/ethnicity has very little to do with how I feel about myself."="Q4", # nolint
    "I feel I don't have much to offer to my racial/ethnic group."="Q5", # nolint # nolint: line_length_linter.
    "In general, I'm glad to be a member of my racial/ethnic group."="Q6", # nolint
    "Most people consider my racial/ethnic group, on the average, to be more ineffective than other groups."="Q7", # nolint
    "The racial/ethnic group I belong to is an important reflection of who I am."="Q8",
    "I am a cooperative participant in the activities of my racial/ethnic group."="Q9",
    "Overall, I often feel that my racial/ethnic group is not worthwhile."="Q10",
    "In general, others respect my race/ethnicity."="Q11",
    "My race/ethnicity is unimportant to my sense of what kind of person I am."="Q12",
    "I often feel I'm a useless member of my racial/ethnic group."="Q13",
    "I feel good about the race/ethnicity I belong to."="Q14",
    "In general, others think that my racial/ethnic group is unworthy."="Q15",
    "In general, belonging to my race/ethnicity is an important part of my self image."="Q16"
                     ) # nolint: indentation_linter.


