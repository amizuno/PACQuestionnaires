# Set up. Your working directory needs to be the top level of this repository.
rm(list=ls())
REPO_NAME = "PACQuestionnaires"
cwd_name = basename(getwd())
if (cwd_name != REPO_NAME) {
    stop(paste0("Current working directory is ", cwd_name, " but needs to be ", REPO_NAME))
}
source("R/data.R")


df = load_data()


library("dplyr")
library("ggplot2")
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
df_all <- z[,c(1,11:13)]
names(df_all) <- c("ID","EDS_Reason","EDS_total", "EDS_ChroTotal")

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

mean(WB_Integ,na.rm = TRUE)
sd(WB_Integ, na.rm = TRUE)
#14.80488, 14.80488
plot_WB <- ggplot(df_all, aes(x = WB_Integ)) +
  geom_histogram(binwidth = 2,fill = "steelblue", color = "white") +
  labs(
    title = "Social Well-Being Scale (SWBS)",
    subtitle = "Social Integration",
    x = "mean = 14.80 (sd = 4.15) ",
    y = "Frequency"
  ) +
  ylim(min=0, 15)+
  xlim(min=0, max=30 )
print(plot_WB)

mean(WB_Acce,na.rm = TRUE)
sd(WB_Acce, na.rm = TRUE)
plot_WB <- ggplot(df_all, aes(x = WB_Acce)) +
  geom_histogram(binwidth = 2,fill = "steelblue", color = "white") +
  labs(
    title = "Social Well-Being Scale (SWBS)",
    subtitle = "Social Acceptance",
    x = "mean = 13.00 (sd = 2.24) ",
    y = "Frequency"
  ) +
  ylim(min=0, 15)+
  xlim(min=0, max=30 )
print(plot_WB)

mean(WB_Contri,na.rm = TRUE)
sd(WB_Contri, na.rm = TRUE)
plot_WB <- ggplot(df_all, aes(x = WB_Contri)) +
  geom_histogram(binwidth = 2,fill = "steelblue", color = "white") +
  labs(
    title = "Social Well-Being Scale (SWBS)",
    subtitle = "Social Contribution",
    x = "mean = 16.83 (sd = 2.89) ",
    y = "Frequency"
  ) +
  ylim(min=0, 15)+
  xlim(min=0, max=30 )
print(plot_WB)

mean(WB_Actua,na.rm = TRUE)
sd(WB_Actua, na.rm = TRUE)
plot_WB <- ggplot(df_all, aes(x = WB_Actua)) +
  geom_histogram(binwidth = 2,fill = "steelblue", color = "white") +
  labs(
    title = "Social Well-Being Scale (SWBS)",
    subtitle = "Social Actualization",
    x = "mean = 12.39 (sd = 3.06) ",
    y = "Frequency"
  ) +
  ylim(min=0, 15)+
  xlim(min=0, max=30 )
print(plot_WB)

mean(WB_Coher,na.rm = TRUE)
sd(WB_Coher, na.rm = TRUE)
plot_WB <- ggplot(df_all, aes(x = WB_Coher)) +
  geom_histogram(binwidth = 2,fill = "steelblue", color = "white") +
  labs(
    title = "Social Well-Being Scale (SWBS)",
    subtitle = "Social Coherence",
    x = "mean = 11.76 (sd = 3.15) ",
    y = "Frequency"
  ) +
  ylim(min=0, 15)+
  xlim(min=0, max=30 )
print(plot_WB)
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
  AveSameRace_freq[i] = round(sum(RiC_df2[i, c("Q3","Q5","Q7")])/3,2)
}
for(i in 1:nrow(RiC_df2)){
  AveDiffRace_freq[i] = round(sum(RiC_df2[i, c("Q4","Q6","Q8")])/3,2)
}
for(i in 1:nrow(RiC_df2)){
  AveSameRace_quali[i] = round(sum(RiC_df2[i, c("Q3a","Q5a","Q7a")])/3,2)
}
for(i in 1:nrow(RiC_df2)){
  AveDiffRace_quali[i] = round(sum(RiC_df2[i, c("Q4a","Q6a","Q8a")])/3,2)
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
sd(RiC_df2$Q4, na.rm = TRUE)
#6.357143, 1.778207
plot_RiCQ4 <- ggplot(RiC_df2, aes(x = Q4)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "How often did you spend free time with people of races other than your own? ",
    subtitle = "Leisure or Free time",
    x = "mean = 6.36 (sd = 1.78) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 0, max = 10)
print(plot_RiCQ4)

mean(RiC_df2$Q6,na.rm = TRUE)
sd(RiC_df2$Q6, na.rm = TRUE)
#6.97619, 1.505892
plot_RiCQ6 <- ggplot(RiC_df2, aes(x = Q6)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "How often did you work closely with people of races other than your own? ",
    subtitle = "Work or School",
    x = "mean = 6.98 (sd = 1.51) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 0, max = 10)
print(plot_RiCQ6)

mean(RiC_df2$Q8,na.rm = TRUE)
sd(RiC_df2$Q8, na.rm = TRUE)
#4.97619, 1.814415
plot_RiCQ8 <- ggplot(RiC_df2, aes(x = Q8)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "How often did you ___ with people of races other than your own? ",
    subtitle = "social media or online spaces",
    x = "mean = 4.98 (sd = 1.81) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 0, max = 10)
print(plot_RiCQ8)

### quality
mean(RiC_df2$Q3a,na.rm = TRUE)
sd(RiC_df2$Q3a, na.rm = TRUE)
#4, 0.8553989

plot_Q3a <- ggplot(RiC_df2, aes(x = Q3a)) +
  geom_histogram(breaks = seq(1, 5, by = 0.5),  fill = "steelblue", color = "white") +
  #  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  labs(
    title = "Q3a: What is the quality of the interaction?",
    subtitle = "Same Race, leisure or free time",
    x = "mean = 4.00 (sd = 0.86) ",
    y = "Frequency"
  ) +
   ylim(min = 0, max = 20)+
   xlim(min = 1, max = 5)
print(plot_Q3a)

plot_Q5a <- ggplot(RiC_df2, aes(x = Q5a)) +
  geom_histogram(breaks = seq(1, 5, by = 0.5),  fill = "steelblue", color = "white") +
  #  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  labs(
    title = "Q3a: What is the quality of the interaction?",
    subtitle = "Same Race, school or work",
#    x = "mean = 4.00 (sd = 0.86) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 1, max = 5)
print(plot_Q5a)

plot_Q7a <- ggplot(RiC_df2, aes(x = Q7a)) +
  geom_histogram(breaks = seq(1, 5, by = 0.5),  fill = "steelblue", color = "white") +
  #  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  labs(
    title = "Q3a: What is the quality of the interaction?",
    subtitle = "Same Race, social media or online",
    #    x = "mean = 4.00 (sd = 0.86) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 1, max = 5)
print(plot_Q7a)

plot_Q8a <- ggplot(RiC_df2, aes(x = Q8a)) +
  geom_histogram(breaks = seq(1, 5, by = 0.5),  fill = "steelblue", color = "white") +
  #  scale_x_continuous(breaks = c(0, 5, 10, 20)) +
  labs(
    title = "What is the quality of the interaction?",
    subtitle = "Different Race, social media or online",
    #    x = "mean = 4.00 (sd = 0.86) ",
    y = "Frequency"
  ) +
  ylim(min = 0, max = 20)+
  xlim(min = 1, max = 5)
print(plot_Q8a)

# Compute the % f sane race good friends Q2/Q1

RiC_df2$Q2_over_Q1 <- round((RiC_df2$Q2 / RiC_df2$Q1),2)
names(RiC_df2) <- c("ID","Q1","Q2","RiC_Q3","RiC_Q4","RiC_Q5","RiC_Q6","RiC_Q7","RiC_Q8","RiC_Q3a","RiC_Q4a",
                    "RiC_Q5a","RiC_Q6a","RiC_Q7a","RiC_Q8a", 
                    "RiC_Same_freq","RiC_Diff_freq" ,"RiC_Same_quali","RiC_Diff_quali","RiC_Q2_over_Q1")

df_all <- cbind(df_all, RiC_df2[,c(4:20)])

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
desired_columns <- c("Participant ID:",
                    "I am a worthy member of my race/ethnic group.",
                    "I often regret that I belong to my racial/ethnic group.",
                    "Overall, my racial/ethnic group is considered good by others.",
                    "Overall, my race/ethnicity has very little to do with how I feel about myself.",
                    "I feel I don't have much to offer to my racial/ethnic group.",
                    "In general, I'm glad to be a member of my racial/ethnic group.",
                    "Most people consider my racial/ethnic group, on the average, to be more ineffective than other groups.",
                    "The racial/ethnic group I belong to is an important reflection of who I am.",
                    "I am a cooperative participant in the activities of my racial/ethnic group.",
                    "Overall, I often feel that my racial/ethnic group is not worthwhile.",
                    "In general, others respect my race/ethnicity.",
                    "My race/ethnicity is unimportant to my sense of what kind of person I am.",
                    "I often feel I'm a useless member of my racial/ethnic group.",
                    "I feel good about the race/ethnicity I belong to.",
                    "In general, others think that my racial/ethnic group is unworthy.",
                    "In general, belonging to my race/ethnicity is an important part of my self image.")


# Subset the dataframe using the list of column names
df_CSE <- data.frame(df[, desired_columns, drop = FALSE])
names(df_CSE) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5","Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12",
                    "Q13", "Q14", "Q15","Q16")

all_values <- c(df_CSE$Q1,df_CSE$Q2,df_CSE$Q3,df_CSE$Q4,df_CSE$Q5,df_CSE$Q6,df_CSE$Q7,df_CSE$Q8,df_CSE$Q9,df_CSE$Q10,
                df_CSE$Q11,df_CSE$Q12,df_CSE$Q13,df_CSE$Q14,df_CSE$Q15,df_CSE$Q16)
unique_values <- unique(all_values)
print(unique_values)

#scoring
LEVELS = c("1 - Strongly Disagree",
           "2 - Disagree",
           "3 - Disagree Somewhat",
           "4 - Neutral",
           "5 - Agree Somewhat",
           "6 - Agree",
           "7 - Strongly Agree")

df_n = data.frame(matrix(ncol = 17, nrow = 42))

for(i in 2:17){
  df_n[,i] = as.numeric(factor(df_CSE[, i], levels =LEVELS)) #positive LEVEL
}
df_n[1] <- df_CSE$ID
names(df_n) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5","Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12",
                 "Q13", "Q14", "Q15","Q16")

#Calculate Sum (total)
SWLSSum <- 0
for(i in 1:nrow(df_n)){
  SWLSSum[i] = sum(df_n[i, 2:6])
}

df_SWLS <- cbind(df_n, SWLSSum)


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
# total score = sum then divide by 12
# Subscales
#       1. Significant Other: Q1, 2, 5, 10  (sum then divide by 4)
#       2. Family:            Q3, 4, 8, 11  (sum then divide by 4)
#       3. Friends:           Q6, 7, 9, 12  (sum then divide by 4)
####################################################################################
desired_columns <- grep("Instructions: We are interested in how you feel about |ID", colnames(df), value = TRUE)

# Subset the dataframe using the list of column names
df_MSPSS <- data.frame(df[, desired_columns, drop = FALSE])
names(df_MSPSS) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5","Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")

all_values <- c(df_MSPSS$Q1,df_MSPSS$Q2,df_MSPSS$Q3,df_MSPSS$Q4,df_MSPSS$Q5,
                df_MSPSS$Q6,df_MSPSS$Q7,df_MSPSS$Q8,df_MSPSS$Q9,df_MSPSS$Q10,
                df_MSPSS$Q11,df_MSPSS$Q12)
unique_values <- unique(all_values)
print(unique_values)

#scoring
LEVELS = c("Very Strongly Disagree",
           "Strongly Disagree",
           "Mildly Disagree",
           "Neutral",
           "Mildly Agree",
           "Strongly Agree",
           "Very Strongly Agree")

df_n = data.frame(matrix(ncol = 13, nrow = 42))

for(i in 1:13){
  df_n[,i] = as.numeric(factor(df_MSPSS[, i], levels =LEVELS)) #positive LEVEL
}
df_n[1] <- df_MSPSS$ID
names(df_n) <- c("ID","Q1","Q2", "Q3", "Q4", "Q5","Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")

#Calculate Sum (total)
MSPSave <- 0
for(i in 1:nrow(df_n)){
  MSPSave[i] = sum(df_n[i, 2:13])/12
}

df_MSPSS <- cbind(df_n, round(MSPSave,2))
names(df_MSPSS)[14] <- "MSPSStotal_ave"

# Create a histogram of MSPSS scores (total scores)
mean(df_MSPSS$MSPSStotal_ave,na.rm = TRUE)
sd(df_MSPSS$MSPSStotal_ave, na.rm = TRUE)
#22.95122, 6.870048

plot_MSPSS <- ggplot(df_MSPSS, aes(x = MSPSStotal_ave)) +
  geom_histogram(breaks = seq(0, 10, by = 0.55), binwidth = 0.5, fill = "steelblue", color = "white", width=1) +
  labs(
    title = "Multidimensional Scale of Perceived Social Support (MSPSS) ",
    subtitle = "TOTAL (Range 1-7)",
    x = "mean = 5.68 (sd = 1.10) ",
    y = "Frequency"
  ) +
  ylim(min=0, max=20)
print(plot_MSPSS)

# Calculate subscales
#   1. Significant Other: Q1, 2, 5, 10  (sum then divide by 4)
#   2. Family:            Q3, 4, 8, 11  (sum then divide by 4)
#   3. Friends:           Q6, 7, 9, 12  (sum then divide by 4)
df_MSPSS_SigOther <- df_MSPSS[, c("Q1","Q2", "Q5", "Q10")]
df_MSPSS_Family <- df_MSPSS[, c("Q3","Q4", "Q8", "Q11")]
df_MSPSS_Friends <- df_MSPSS[, c("Q6","Q7", "Q9", "Q12")]

MSPSS_SigOther <- 0
for(i in 1:nrow(df_MSPSS_SigOther)){
  MSPSS_SigOther[i] = sum(df_MSPSS_SigOther[i, 1:4])/4
}

MSPSS_Family  <- 0
for(i in 1:nrow(df_MSPSS_Family)){
  MSPSS_Family[i] = sum(df_MSPSS_Family[i, 1:4])/4
}

MSPSS_Friend  <- 0
for(i in 1:nrow(df_MSPSS_Friends)){
  MSPSS_Friend[i] = sum(df_MSPSS_Friends[i, 1:4])/4
}

df_MSPSS <- cbind(df_MSPSS, MSPSS_SigOther,MSPSS_Family,MSPSS_Friend)

#df_all <- cbind(df_all, df_SWLS[77])

mean(df_MSPSS$MSPSS_SigOther,na.rm = TRUE)
sd(MSPSS_SigOther,na.rm = TRUE)
# 5.658537, 1.579909
plot_MSPSS <- ggplot(df_MSPSS, aes(x = MSPSS_SigOther)) +
  geom_histogram(breaks = seq(0, 10, by = 0.55), binwidth = 0.5, fill = "steelblue", color = "white", width=1) +
  labs(
    title = "Multidimensional Scale of Perceived Social Support (MSPSS) ",
    subtitle = "Significant Other",
    x = "mean = 5.66 (sd = 1.58) ",
    y = "Frequency"
  ) +
  ylim(min=0, max=20)
print(plot_MSPSS)

mean(df_MSPSS$MSPSS_Family,na.rm = TRUE)
sd(MSPSS_Family,na.rm = TRUE)
#  5.390244,1.455645
plot_MSPSS <- ggplot(df_MSPSS, aes(x = MSPSS_Family)) +
  geom_histogram(breaks = seq(0, 10, by = 0.55), binwidth = 0.5, fill = "steelblue", color = "white", width=1) +
  labs(
    title = "Multidimensional Scale of Perceived Social Support (MSPSS) ",
    subtitle = "Family",
    x = "mean = 5.39 (sd = 1.46) ",
    y = "Frequency"
  ) +
  ylim(min=0, max=20)
print(plot_MSPSS)

mean(df_MSPSS$MSPSS_Friend,na.rm = TRUE)
sd(MSPSS_Friend,na.rm = TRUE)
#  5.993902,1.194372
plot_MSPSS <- ggplot(df_MSPSS, aes(x = MSPSS_Friend)) +
  geom_histogram(breaks = seq(0, 10, by = 0.55), binwidth = 0.5, fill = "steelblue", color = "white", width=1) +
  labs(
    title = "Multidimensional Scale of Perceived Social Support (MSPSS) ",
    subtitle = "Friend",
    x = "mean = 5.99 (sd = 1.19) ",
    y = "Frequency"
  ) +
  ylim(min=0, max=20)
print(plot_MSPSS)



####################################################################################
# Chronic Strains
#
# 12 item
# 3-Likert scale: Not true, Shomewhat true, very true, Does not apply to me
####################################################################################

desired_columns <- grep("Thinking about your life currently, are the statements below not true|ID", colnames(df), value = TRUE)

# Subset the dataframe using the list of column names
df_ChroStrain <- data.frame(df[, desired_columns, drop = FALSE])
names(df_ChroStrain)
names(df_ChroStrain) <- c("ID","Q1", "Q2", "Q3", "Q4", "Q5","Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")

all_values <- c(df_ChroStrain$Q1,df_ChroStrain$Q2,df_ChroStrain$Q3,df_ChroStrain$Q4,df_ChroStrain$Q5,
                df_ChroStrain$Q6,df_ChroStrain$Q7,df_ChroStrain$Q8,df_ChroStrain$Q9,df_ChroStrain$Q10,
                df_ChroStrain$Q11,df_ChroStrain$Q12)
unique_values <- unique(all_values)
print(unique_values)

#scoring
LEVELS = c("Not true",
           "Somewhat true",
           "Very true")

df_n = data.frame(matrix(ncol = 13, nrow = 42))

for(i in 1:13){
  df_n[,i] = as.numeric(factor(df_ChroStrain[, i], levels =LEVELS)) #positive LEVEL
}
df_n[1] <- df_ChroStrain$ID
names(df_n) <- c("ID","Q1","Q2", "Q3", "Q4", "Q5","Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")

#Calculate Sum (total)
ChroStrain_total <- 0
for(i in 1:nrow(df_n)){
  ChroStrain_total[i] = sum(df_n[i, 2:13], na.rm=TRUE)
}

ChroStrain_total[1] <- "NA"
ChroStrain_total <- as.numeric(ChroStrain_total)
df_ChroStrain <- cbind(df_n, ChroStrain_total)

# Create a histogram of MSPSS scores (total scores)
mean(df_ChroStrain$ChroStrain_total,na.rm = TRUE)
sd(df_ChroStrain$ChroStrain_total, na.rm = TRUE)
#15.95122, 3.361184

plot_ChroStrain <- ggplot(df_ChroStrain, aes(x = ChroStrain_total)) +
  geom_histogram(fill = "steelblue", color = "white",binwidth = 2.5) +
  labs(
    title = "Chronic Strains",
    x = "mean = 15.95 (sd = 3.36) ",
    y = "Frequency"
  ) 
print(plot_ChroStrain)




##########################################