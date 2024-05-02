#' Methods to load and prepare data

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
