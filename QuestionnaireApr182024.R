rm(list=ls())

library("dplyr")
library("ggplot2")
#main_path <- "/Users/akiko/OneDrive - University of Pittsburgh/SeedProject/Analysis_Apr2024/" #Office
#main_path <- "/Volumes/CEREBRO/Studies/NEMO2.0/Public/Analysis/misc/"
main_path <- "/Users/akikomizuno/OneDrive - University of Pittsburgh/SeedProject/Analysis_Apr2024/" #home
setwd(main_path)

library("readxl")
df <-read_excel("CGHP_21_62.xlsx")
names(df)

##########################################
# Extract each questionnaires based on questions
# Everyday Discrimination (EDS)
# (no subscales)
##########################################

# Assuming df is your dataframe containing your data
# Replace 'df' with the name of your dataframe if it's different

# List of column names to subset
column_names <- c("Participant ID:",
                  "You are treated with less courtesy than other people.",
                  "You are treated with less respect than other people.",
                  "You receive poorer service than other people at restaurants or stores.",
                  "People act as if they think you are not smart.",
                  "People act as if they are afraid of you.",
                  "People act as if they think you are dishonest.",
                  "People act as if they're better than you.",
                  "You are called names or insulted.",
                  "You are threatened or harassed.",
                  "You are followed around in stores.",
                  #"You are discriminated against.", ##### REMOVE
                  "What do you think is the main reason for these experiences?")

# Subset the dataframe using the list of column names
df_EDS <- data.frame(df[, column_names, drop = FALSE])
names(df_EDS) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11")

# all_values <- c(EDQ_df$Q1, EDQ_df$Q2, EDQ_df$Q3, EDQ_df$Q4, EDQ_df$Q5, EDQ_df$Q6, EDQ_df$Q7, EDQ_df$Q8, EDQ_df$Q9, EDQ_df$Q10, EDQ_df$Q11)
# unique_values <- unique(all_values)
# print(unique_values)

LEVELS = c(
  "Almost everyday", 
  "At least once a week",
  "A few times a month",
  "A few times a year", 
  "Less than once a year", 
  "Never"
)

df_n = data.frame(matrix(ncol = 11, nrow = 42))

for(i in 2:12){
  df_n[,i] = as.numeric(factor(df_EDS[, i+1], levels =rev(LEVELS)))
}

df_n[1] <- df_EDS$ID
df_n[11] <- df_EDS$Q11
names(df_n) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")

row_sums <- numeric(nrow(df_n))

for(i in 1:nrow(df_n)){
  row_sums[i] = sum(df_n[i, 2:10])
}
df_f <- cbind(df_n, row_sums)

# Create a histogram of EDS scores (total scores)
mean(df_f$row_sums)
sd(df_f$row_sums)

data_df <- data.frame(df_f = c(0, seq(10, 50, by = 10), 60))
# Calculate frequencies for each bin
data_df$freq <- sapply(data_df$df_f, function(bin) sum(row_sums >= bin & row_sums < bin + 10))
# Plot the histogram
plot_EDS <- ggplot(data = data_df, aes(x = df_f, y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = c(seq(0, 60, by = 10)), labels = c(0, seq(10, 60, by = 10))) +
  labs(
    title = "Everyday Discrimination Scale (EDS)",
    subtitle = "Range 0-60",
    x = "mean = 19.67 (sd = 6.93)",
    y = "Frequency"
  )
plot_EDS

## Chronicity-based coding
# Load the required library
library(dplyr)

z <- df_f %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars(Q1:Q9), funs(recode(.,`1` = 0, `2`= 0.5, `3` = 3, `4` = 36, `5` = 104, `6` = 260)))
z

row_sums_C <- 0
for(i in 1:nrow(z)){
  row_sums_C[i] = sum(z[i, 2:10])
}
z <- cbind(z, row_sums_C)
z <- z[,-13]

mean(z$row_sums_C)
sd(z$row_sums_C)
plot_EDS_chro <- ggplot(data = data.frame(z = row_sums_C), aes(x = z)) +
  geom_histogram(binwidth = 25, fill = "steelblue", color = "white") +
  # scale_x_continuous(breaks = c(0, 5, 30, 360, 1040, 2600)) +
  # scale_x_continuous(breaks = c(0, 24, 448, 2600)) +
  
  labs(
    title = "Everyday Discrimination Scale (EDS) ",
    subtitle = "Range 0-2600",
    x = "mean = 92.70 (sd = 193.13) ",
    y = "Frequency"
  )
print(plot_EDS_chro)

mean(df_f$row_sums)
sd(df_f$row_sums)

##
# z --> make it to one data frame for later
df_all <- z[,c(1,12,13)]
names(df_all) <- c("ID","EDS_total", "EDS_ChroTotal")

#Q12chr variables --> redefine as a factor
# histograms  
unique(EDS_df$Q12)

Q12_f <-c("Your Gender Identity",
          "Your Race",
          "Your shade of skin color",
          "Your Sexual Orientation",
          "Your Religion",
          "Your Age",
          "A physical disability",
          "Your Height",
          "Your Weight",
          "Some other Aspect of Your Physical Appearance",
          "Your Education or Income Level",
          "Your Ancestry or National Origins"
)
Q12_f <- factor(Q12_f)
CateScale <- c(1:12)
Q12Temp <- data.frame(Q12_f,CateScale,0 )
colnames(Q12Temp) = c("response",  "CateScale", "score")

countsQ12 = count(EDS_df, Q12,.drop = FALSE)
names(countsQ12) <- c("response", "frequency")

qualiQ12 <- left_join(Q12Temp, countsQ12, by = "response")
qualiQ12 <- qualiQ12[, c(1, 2,4)]

qualiQ12$response <- factor(qualiQ12$response, levels = qualiQ12$response)
qualiQ12[is.na(qualiQ12)] <- 0
Q12_freq <- qualiQ12[,c(1,3)]

Q12_freq$response <- factor(Q12_freq$response)

# Plot the frequencies vertically
p <- ggplot(Q12_freq, aes(x=response, y=frequency)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(
    title = "Everyday Discrimination: Q11",
    subtitle = "What do you think is the main reason for these experiences?",
    y = "Frequency"
  )

# Display the plot
p



####################################################################################
# Extract each questionnaires based on questions
# 
# Social Well-Being (SWBQ)
# 
# Reverse coding for "Q1", "Q2","Q7", "Q8", "Q9", "Q10", "Q13", "Q15"
# Subscales:
#  Social Integration =   Q2, Q6, Q11
#  Social Acceptance =    Q3, Q10, Q14
#  Social Contribution =  Q4, Q7, Q15
#  Social Actualization = Q5, Q9, Q13
#  Social Coherence =     Q1, Q8, Q12
####################################################################################
# Find column names containing the specified characters
desired_columns <- grep("The following questions are about your thoughts about how you feel in your community and society. |ID", colnames(df), value = TRUE)

# Subset the dataframe using the desired columns
WellBeing_df <- data.frame(df[, desired_columns, drop = FALSE])
names(WellBeing_df)

#check unique responses
# Subset the dataframe using the list of column names
names(WellBeing_df) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11","Q12", "Q13", "Q14", "Q15")

all_values <- c(WellBeing_df$Q2, WellBeing_df$Q3, WellBeing_df$Q4, WellBeing_df$Q5, WellBeing_df$Q6, WellBeing_df$Q7, 
                WellBeing_df$Q8, WellBeing_df$Q9, WellBeing_df$Q10, WellBeing_df$Q11,WellBeing_df$Q12, WellBeing_df$Q13, WellBeing_df$Q14, 
                WellBeing_df$Q15)
unique_values <- unique(all_values)
print(unique_values)

WellBeing_dfPosi <- WellBeing_df[, c("ID","Q3","Q4", "Q5", "Q6", "Q11","Q12", "Q14")]
WellBeing_dfNega <- WellBeing_df[,c("Q1", "Q2","Q7", "Q8", "Q9", "Q10", "Q13", "Q15")]

#scoring
LEVELS = c("Strongly Disagree",
           "Moderately Disagree",
           "Slightly Disagree",
           "Neither Agree nor Disagree",
           "Slightly Agree",
           "Moderately Agree","Strongly Agree")
#[1] "Strongly Disagree"          "Moderately Disagree"        "Slightly Disagree"          "Neither Agree nor Disagree" "Slightly Agree"             "Moderately Agree"           "Strongly Agree" 

df_n = data.frame(matrix(ncol = 16, nrow = 42))

for(i in 1:8){
  df_n[,i] = as.numeric(factor(WellBeing_dfPosi[, i], levels =LEVELS)) #positive LEVEL
}
df_n[1] <- WellBeing_df$ID
names(df_n) <- c("ID","Q3","Q4", "Q5", "Q6", "Q11","Q12", "Q14" )

for(i in 9:16){
  df_n[,i] = as.numeric(factor(WellBeing_dfNega[, i-8], levels = rev(LEVELS))) #Reverse LEVEL
}
names(df_n) <- c("ID","Q3","Q4", "Q5", "Q6", "Q11","Q12", "Q14","Q1", "Q2","Q7", "Q8", "Q9", "Q10", "Q13", "Q15")

#Calculate Sum (total)
WellBeingSum <- 0
for(i in 1:nrow(df_n)){
  WellBeingSum[i] = sum(df_n[i, 2:16])
}

df_WB <- cbind(df_n, WellBeingSum)

# Create a histogram of EDS scores (total scores)
mean(df_WB$WellBeingSum,na.rm = TRUE)
sd(df_WB$WellBeingSum, na.rm = TRUE)
#68.78049, 9.663106

plot_WB <- ggplot(data = data.frame(df_WB = WellBeingSum), aes(x = df_WB)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  # scale_x_continuous(breaks = c(0, 5, 30, 360, 1040, 2600)) +
  # scale_x_continuous(breaks = c(0, 24, 448, 2600)) +
  
  labs(
    title = "Social Well-Being Scale (SWBS)",
    subtitle = "Range 15-105",
    x = "mean = 68.78 (sd = 9.66) ",
    y = "Frequency"
  )
print(plot_WB)

df_all <- df_all[, 1:3]
df_all <- cbind(df_all, df_WB[17])

# Calculate subscales
#  Social Integration =   Q2, Q6, Q11
#  Social Acceptance =    Q3, Q10, Q14
#  Social Contribution =  Q4, Q7, Q15
#  Social Actualization = Q5, Q9, Q13
#  Social Coherence =     Q1, Q8, Q12
WellBeing_dfInteg <- df_WB[, c("Q2","Q6", "Q11")]
WellBeing_dfAcce <- df_WB[, c("Q3","Q10", "Q14")]
WellBeing_dfContri <- df_WB[, c("Q4","Q7", "Q15")]
WellBeing_dfActua <- df_WB[, c("Q5","Q9", "Q13")]
WellBeing_dfCoher <- df_WB[, c("Q1","Q8", "Q12")]

WB_Integ <- 0
for(i in 1:nrow(WellBeing_dfInteg)){
  WB_Integ[i] = sum(WellBeing_dfInteg[i, 1:3])
}

WB_Acce <- 0
for(i in 1:nrow(WellBeing_dfAcce)){
  WB_Acce[i] = sum(WellBeing_dfAcce[i, 1:3])
}

WB_Contri <- 0
for(i in 1:nrow(WellBeing_dfContri)){
  WB_Contri[i] = sum(WellBeing_dfContri[i, 1:3])
}

WB_Actua <- 0
for(i in 1:nrow(WellBeing_dfActua)){
  WB_Actua[i] = sum(WellBeing_dfActua[i, 1:3])
}

WB_Coher <- 0
for(i in 1:nrow(WellBeing_dfCoher)){
  WB_Coher[i] = sum(WellBeing_dfCoher[i, 1:3])
}

df_all <- cbind(df_all, WB_Integ,WB_Acce,WB_Contri,WB_Actua,WB_Coher)

########################################################################################################################################################################
# Race in Context (RiC)
#
#
#
########################################################################################################################################################################
desired_columns <- c("Participant ID:",
                    "How many good friends do you have?",
                    "How many of these friends identify as the same race as you?",
                    "In the past 6 months, how often did you interact with (eg. comment on their page/update; have them comment on your posts) people of races other than your own?",
                    "What is the quality of this interaction?")

# identify the indices of the columns for RiC questions
matching_indices <- which(colnames(df) %in% desired_columns)
print(matching_indices)
# 2 54 55 66

# Subset the dataframe using the desired columns
RiC_df <- data.frame(df[, c(2,54:67), drop = FALSE])
names(RiC_df)

#check unique responses
# Subset the dataframe using the list of column names
names(RiC_df) <- c("ID","Q1", "Q2", "Q3", "Q3a","Q4","Q4a","Q5","Q5a","Q6","Q6a","Q7","Q7a","Q8","Q8a")

all_values <- c(RiC_df$Q1,RiC_df$Q2, RiC_df$Q3,RiC_df$Q3a, RiC_df$Q4,RiC_df$Q4a,
                RiC_df$Q5,RiC_df$Q5a,RiC_df$Q6,RiC_df$Q6a, RiC_df$Q7, RiC_df$Q7a,RiC_df$Q8,RiC_df$Q8a)
unique_values <- unique(all_values)
print(unique_values)

# replace the misspelled values
RiC_df <- data.frame(lapply(RiC_df, function(x) gsub("Fiar", "Fair", x)))
RiC_df <- data.frame(lapply(RiC_df, function(x) gsub("2-5 Days a week", "2-5 days a week", x)))

# convert as numeric (# of friends)
RiC_df$Q1 <- as.numeric(RiC_df$Q1)
RiC_df$Q2 <- as.numeric(RiC_df$Q2)

# Q1:How many good friends do you have?
mean(RiC_df$Q1) #6.809524
sd(RiC_df$Q1) #4.575854
range(RiC_df$Q1) #2,20
plot_RiC <- ggplot(RiC_df, aes(x = Q1)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6,8,10)) +
  labs(
    title = "RiC: Q1 How many good friends do you have?",
    subtitle = "Min = 2, Max = 20",
    x = "mean = 6.81 (sd = 4.58) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 10)
print(plot_RiC)

mean(RiC_df$Q2) #3.666667
sd(RiC_df$Q2) #3.771955
range(RiC_df$Q2) #0, 15

plot_RiC <- ggplot(RiC_df, aes(x = Q2)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6,8,10)) +
  labs(
    title = "RiC: Q2 How many of these friends identify as the same race as you?",
    subtitle = "Min = 0, Max = 15",
    x = "mean = 3.67 (sd = 3.78) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 10)

print(plot_RiC)

RiC_dfFreq <- RiC_df[, c("Q3","Q4","Q5","Q6","Q7","Q8")]
RiC_dfQuali <- RiC_df[,c("Q3a","Q4a","Q5a","Q6a","Q7a","Q8a")]

#scoring
LEVELS_freq = c(
  "Never",
  "Almost Never", 
  "Less than once a month",
  "Once a month",
  "Once a week",
  "2-5 days a week",
  "Daily",
  "Several times a day",
  "Most or all of the day"
)

LEVELS_quali = c(
  "Poor",
  "Fair", 
  "Good",
  "Very Good",
  "Excellent"
)

df_n = data.frame(matrix(ncol = 6, nrow = 42))
for(i in 1:6){
  df_n[,i] = as.numeric(factor(RiC_dfFreq[, i], levels =LEVELS_freq))
}

df_n2 = data.frame(matrix(ncol = 6, nrow = 42))
for(i in 1:6){
  df_n2[,i] = as.numeric(factor(RiC_dfQuali[, i], levels =LEVELS_quali))
}

RiC_df2 <- cbind(RiC_df[1:3],df_n, df_n2)
names(RiC_df2) <- c("ID","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q3a","Q4a","Q5a","Q6a","Q7a","Q8a")

#Calculate Sum (total)
AveSameRace_freq <- 0
AveDiffRace_freq <- 0
AveSameRace_quali <- 0
AveDiffRace_quali <- 0

for(i in 1:nrow(RiC_df2)){
  AveSameRace_freq[i] = sum(RiC_df2[i, c("Q3","Q5","Q7")])/3
}
for(i in 1:nrow(RiC_df2)){
  AveDiffRace_freq[i] = sum(RiC_df2[i, c("Q4","Q6","Q8")])/3
}
for(i in 1:nrow(RiC_df2)){
  AveSameRace_quali[i] = sum(RiC_df2[i, c("Q3a","Q5a","Q7a")])/3
}
for(i in 1:nrow(RiC_df2)){
  AveDiffRace_quali[i] = sum(RiC_df2[i, c("Q4a","Q6a","Q8a")])/3
}

RiC_df2 <- cbind(RiC_df2, AveSameRace_freq,AveDiffRace_freq,AveSameRace_quali,AveDiffRace_quali)

# Create a histogram for each average
mean(AveSameRace_freq,na.rm = TRUE)
sd(AveSameRace_freq, na.rm = TRUE)
#5.34127, 1.703636

plot_RiCSameRace_freq <- ggplot(RiC_df2, aes(x = AveSameRace_freq)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
#  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
#  scale_y_continuous(breaks = c(0, 2, 4, 6,8,10)) +
  labs(
    title = "In the past 6 months, how often did you___with people of your same race?",
    subtitle = "Range 1: Never, 9: Most or all of the day",
    x = "mean = 5.34 (sd = 1.70) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20) +
  xlim(min = 0, max = 10)
print(plot_RiCSameRace_freq)

mean(AveDiffRace_freq,na.rm = TRUE)
sd(AveDiffRace_freq, na.rm = TRUE)
#6.103175, 1.174453

plot_DiffRace_freq <- ggplot(RiC_df2, aes(x = AveDiffRace_freq)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
#  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6,8,10)) +
  labs(
    title = "In the past 6 months, how often did you___with people of races other than your own?",
    subtitle = "Range 1: Never, 9: Most or all of the day",
    x = "mean = 6.10 (sd = 1.17) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 0, max = 10)
print(plot_DiffRace_freq)

mean(AveSameRace_quali,na.rm = TRUE)
sd(AveSameRace_quali, na.rm = TRUE)
#3.873016, 0.6541607

plot_RiC_SameRace_quali <- ggplot(RiC_df2, aes(x = AveSameRace_quali)) +
  geom_histogram(breaks = seq(1, 5, by = 0.5), fill = "steelblue", color = "white") +
  labs(
    title = "What is the quality of the interaction? (Same Race)",
    subtitle = "Range 1: Poor -- 5: Excellent",
    x = "mean = 3.87 (sd = 0.65) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 1, max = 5)
print(plot_RiC_SameRace_quali)

mean(AveDiffRace_quali,na.rm = TRUE)
sd(AveDiffRace_quali, na.rm = TRUE)
#3.747967, 0.6573536

plot_RiC_DiffRace_quali <- ggplot(RiC_df2, aes(x = AveDiffRace_quali)) +
  geom_histogram(breaks = seq(1, 5, by = 0.5), fill = "steelblue", color = "white") +
  labs(
    title = "What is the quality of the interaction? (Different Race)",
    subtitle = "Range 1: Poor -- 5: Excellent",
    x = "mean = 3.75 (sd = 0.66) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 1, max = 5)
print(plot_RiC_DiffRace_quali)

# Plot histgrams for frequency Same Race [Leisure, Work/Schoo, Online]
mean(RiC_df2$Q3,na.rm = TRUE)
sd(RiC_df2$Q3, na.rm = TRUE)
#5.52381, 2.233209

plot_Q3 <- ggplot(RiC_df2, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
#  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  labs(
    title = "Q3: Same Race, Frequency, Leisure or Free Time",
    subtitle = "Range 1: Never, 9: Most or all of the day",
    x = "mean = 5.52 (sd = 2.23) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 0, max = 10)
print(plot_Q3)

mean(RiC_df2$Q5,na.rm = TRUE)
sd(RiC_df2$Q5, na.rm = TRUE)
#5.5, 2.539925
plot_Q5 <- ggplot(RiC_df2, aes(x = Q5)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
#  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  labs(
    title = "Q5: Same Race, Frequency, Work or School",
    subtitle = "Range 1: Never, 9: Most or all of the day",
    x = "mean = 5.5 (sd = 2.5) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 0, max = 10)
print(plot_Q5)

mean(RiC_df2$Q7,na.rm = TRUE)
sd(RiC_df2$Q7, na.rm = TRUE)
#5.0, 1.7943
plot_Q7 <- ggplot(RiC_df2, aes(x = Q7)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
#  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  labs(
    title = "Q7: Same Race, Frequency, Social Media or Online",
    subtitle = "Range 1: Never, 9: Most or all of the day",
    x = "mean = 5.0 (sd = 1.7) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 0, max = 10)
print(plot_Q7)

# Plot histgrams for quantity Different Race [Leisure, Work/Schoo, Online]
mean(RiC_df2$Q4,na.rm = TRUE)
sd(AveDiffRace_quali, na.rm = TRUE)
#3.747967, 0.6573536
plot_RiC_DiffRace_quali <- ggplot(RiC_df2, aes(x = AveDiffRace_quali)) +
  geom_histogram(breaks = seq(1, 5, by = 0.5), fill = "steelblue", color = "white") +
  labs(
    title = "What is the quality of the interaction? (Different Race)",
    subtitle = "Range 1: Poor -- 5: Excellent",
    x = "mean = 3.75 (sd = 0.66) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 1, max = 5)
print(plot_RiC_DiffRace_quali)
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
df_all <- cbind(df_all, CSESum)

# Calculate subscales
# Items 1, 5, 9 and 13 = Membership self-esteem.
# Items 2, 6, 10 and 14 = Private collective self-esteem.
# Items 3, 7, 11, and 15 = Public collective self-esteem.
# Items 4, 8, 12, and 16 = Importance to Identity.
CSE_member <- df_CSE[, c("Q1","Q5", "Q9", "Q13")]
CSE_private <- df_CSE[, c("Q2","Q6", "Q10", "Q14")]
CSE_public <- df_CSE[, c("Q3","Q7", "Q11", "Q15")]
CSE_identity <- df_CSE[, c("Q4","Q8", "Q12", "Q16")]

member_CSE <- 0
for(i in 1:nrow(CSE_member)){
  member_CSE[i] = sum(CSE_member[i, 1:4])/4
}

private_CSE  <- 0
for(i in 1:nrow(CSE_private)){
  private_CSE[i] = sum(CSE_private[i, 1:4])/4
}

public_CSE  <- 0
for(i in 1:nrow(CSE_public)){
  public_CSE[i] = sum(CSE_public[i, 1:4])/4
}

identity_CSE  <- 0
for(i in 1:nrow(CSE_identity)){
  identity_CSE[i] = sum(CSE_identity[i, 1:4])/4
}

df_all <- cbind(df_all, member_CSE,private_CSE,public_CSE,identity_CSE)
####################################################################################
# Satisfaction with Life Scale (SWLS)
#
# 5-item (no reverse scoring)
# 1-7 likert scale
####################################################################################
desired_columns <- grep("Below are five statements that you may agree or disagree with. |ID", colnames(df), value = TRUE)

# Subset the dataframe using the list of column names
df_SWLS <- data.frame(df[, desired_columns, drop = FALSE])
names(df_SWLS) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5")

all_values <- c(df_SWLS$Q1,df_SWLS$Q2,df_SWLS$Q3,df_SWLS$Q4,df_SWLS$Q5)
unique_values <- unique(all_values)
print(unique_values)

#scoring
LEVELS = c("Strongly Disagree",
           "Disagree",
           "Slightly Disagree",
           "Neither Agree nor Disagree",
           "Slightly Agree",
           "Agree",
           "Strongly Agree")

df_n = data.frame(matrix(ncol = 6, nrow = 42))

for(i in 1:6){
  df_n[,i] = as.numeric(factor(df_SWLS[, i], levels =LEVELS)) #positive LEVEL
}
df_n[1] <- df_SWLS$ID
names(df_n) <- c("ID","Q1","Q2", "Q3", "Q4", "Q5")

#Calculate Sum (total)
SWLSSum <- 0
for(i in 1:nrow(df_n)){
  SWLSSum[i] = sum(df_n[i, 2:6])
}

df_SWLS <- cbind(df_n, SWLSSum)

# Create a histogram of EDS scores (total scores)
mean(df_SWLS$SWLSSum,na.rm = TRUE)
sd(df_SWLS$SWLSSum, na.rm = TRUE)
#22.95122, 6.870048

plot_SWLS <- ggplot(data = data.frame(df_SWLS = SWLSSum), aes(x = df_SWLS)) +
  geom_histogram(breaks = seq(0, 40, by = 5),binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Satisfaction with Life Scale (SWLS) ",
    subtitle = "Range 5-35",
    x = "mean = 22.95 (sd = 6.87) ",
    y = "Frequency"
  )
print(plot_SWLS)

#df_all <- cbind(df_all, df_SWLS[77])

####################################################################################
# Multidimensional Scale of Perceived Social Support (MSPSS)
# 
# 12-item, 7-point Likert scale (1=very strongly disagree, 7=svery trongly agree)
# Subscales
#       1. Significant Other: Q1, 2, 5, 10  (sum then divide by 4)
#       2. Family:            Q3, 4, 8, 11  (sum then divide by 4)
#       3. Friends:           Q6, 7, 9, 12  (sum then divide by 4)
####################################################################################

desired_columns <- grep("Instructions: We are interested in how you feel about the following statements |ID", colnames(df), value = TRUE)

# Subset the dataframe using the list of column names
df_SWLS <- data.frame(df[, desired_columns, drop = FALSE])
names(df_SWLS) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5")

all_values <- c(df_SWLS$Q1,df_SWLS$Q2,df_SWLS$Q3,df_SWLS$Q4,df_SWLS$Q5)
unique_values <- unique(all_values)
print(unique_values)

#scoring
LEVELS = c("Strongly Disagree",
           "Disagree",
           "Slightly Disagree",
           "Neither Agree nor Disagree",
           "Slightly Agree",
           "Agree",
           "Strongly Agree")

df_n = data.frame(matrix(ncol = 6, nrow = 42))

for(i in 1:6){
  df_n[,i] = as.numeric(factor(df_SWLS[, i], levels =LEVELS)) #positive LEVEL
}
df_n[1] <- df_SWLS$ID
names(df_n) <- c("ID","Q1","Q2", "Q3", "Q4", "Q5")

#Calculate Sum (total)
SWLSSum <- 0
for(i in 1:nrow(df_n)){
  SWLSSum[i] = sum(df_n[i, 2:6])
}

df_SWLS <- cbind(df_n, SWLSSum)

# Create a histogram of EDS scores (total scores)
mean(df_SWLS$SWLSSum,na.rm = TRUE)
sd(df_SWLS$SWLSSum, na.rm = TRUE)
#22.95122, 6.870048

plot_SWLS <- ggplot(data = data.frame(df_SWLS = SWLSSum), aes(x = df_SWLS)) +
  geom_histogram(breaks = seq(0, 40, by = 5),binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Satisfaction with Life Scale (SWLS) ",
    subtitle = "Range 5-35",
    x = "mean = 22.95 (sd = 6.87) ",
    y = "Frequency"
  )
print(plot_SWLS)

#df_all <- cbind(df_all, df_SWLS[77])


##########################################
# Chronic Strains





##########################################