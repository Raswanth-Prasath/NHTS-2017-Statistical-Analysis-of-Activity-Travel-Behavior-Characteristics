###############################################################################################
########################## PART 0: Prepare the Analysis Data Set ##############################
###############################################################################################

# clear the global environment
rm(list = ls()) 

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to current file location

# read data frames
df_household <- read.csv("nhts_2017/hhpub.csv")
df_person <- read.csv("nhts_2017/perpub.csv")
df_trip <- read.csv("nhts_2017/trippub.csv")

# frequencies of a variable (see nhts codebook: https://nhts.ornl.gov/tables09/CodebookBrowser.aspx)
table(df_trip$URBAN)
table(df_trip$HHSTATE)

# subset data frame based on a condition (the condition is all trips made by individuals from the states of AZ, NM, CO, UT)
dfh <- subset(df_household, HHSTATE=="AZ" | HHSTATE=="NM" | HHSTATE=="CO" | HHSTATE=="UT") 
dfp <- subset(df_person,    HHSTATE=="AZ" | HHSTATE=="NM" | HHSTATE=="CO" | HHSTATE=="UT") 
dft <- subset(df_trip,      HHSTATE=="AZ" | HHSTATE=="NM" | HHSTATE=="CO" | HHSTATE=="UT") 

# subset data frame such that it is limited to adults with weekday trips 
dfh <- subset(dfh, TRAVDAY>=2 & TRAVDAY<="6")               # weekdays extraction
dfp <- subset(dfp, TRAVDAY>="2" & TRAVDAY<="6" & R_AGE>=18)  # weekdays and adults extraction
dft <- subset(dft, TRAVDAY>="2" & TRAVDAY<="6" & R_AGE>=18)   # weekdays and adults extraction

# remove (delete) redundant data frames
rm(df_household, df_person, df_trip)

# show top and bottom rows in a data frame
head(dfp) # this shows row numbers in original data frame (before subseting)
tail(dfp) # this shows row numbers in original data frame (before subseting)
rownames(dfp) <- NULL # reset row numbers
head(dfp) 
tail(dfp) 

############################################################################################
###################### PART 1: DATA VISUALIZATION AND T-TESTS ##############################
############################################################################################

# see distribution for DWELTIME and WHYTRP1S: see https://nhts.ornl.gov/tables09/CodebookBrowser.aspx
sum(is.na(dft$WHYTRP1S))            # count NAs
table(dft$WHYTRP1S)                 # check for missing records (NAs migth be coded as another value)

sum(is.na(dft$DWELTIME))            # count NAs
table(dft$DWELTIME)                 # check for missing records (NAs coded as something else, -9)
dft[ , c('HOUSEID', 'PERSONID', 'TDTRPNUM', 'DWELTIME')] # Dweltime is coded as -9 for the final trips on the survey day

# explore DWELTIME and WHYTRP1S in more detail
subset(dft[ , c('HOUSEID', 'PERSONID', 'TDTRPNUM', 'DWELTIME', 'WHYTRP1S')], WHYTRP1S == 1) # trips to home
subset(dft[ , c('HOUSEID', 'PERSONID', 'TDTRPNUM', 'DWELTIME')], DWELTIME == -9) # Dweltime is coded as -9 for the final trips on the survey day
subset(dft, HOUSEID == 30001438)

# subset to exclude final trips, as their destinations are home
dft <-subset(dft, WHYTRP1S != 1)     # exclude trips with their destinations being home
dft <-subset(dft, DWELTIME > 0)      # exclude trips with their dwell time unknown (missing)
dft$act_freq <- 1                   # create a variable assigning one to each activity 


aggregated_dft <- aggregate(list(dwel_time = dft$DWELTIME, act_freq = dft$act_freq), 
                            by=list(HOUSEID = dft$HOUSEID, PERSONID = dft$PERSONID), 
                            FUN=sum) # alternative way to aggregate and assign column names at the same time

# merge two data frames based on select key variables
merged_dfp <- merge(dfp, aggregated_dft, by=c("HOUSEID", "PERSONID"), all=TRUE)
head(merged_dfp)

# check and clean for missing records
sum(is.na(merged_dfp$dwel_time))
sum(is.na(merged_dfp$act_freq))

merged_dfp$dwel_time[is.na(merged_dfp$dwel_time)]<- 0 # assign 0 to all missing values N/A
merged_dfp$act_freq[is.na(merged_dfp$act_freq)]<- 0 # assign 0 to all missing values N/A

# plot dweltime on a histogram 
hist(merged_dfp$dwel_time, main='Dwelling Time Distribution', xlab= "Time (min)", ylab="Frequency", 
     border="green", col="red", xlim=c(0,1200), ylim=c(0,2000), las=1, breaks=10)

# plot using PLOTLY library
# install.packages('plotly')
library(plotly)
plot_ly(x = merged_dfp$dwel_time, type = "histogram", nbinsx = 20, color='red') # more details at https://plotly.com/r/histograms/

# alternative plotting with DataExplorer package
# install.packages('DataExplorer')
library(DataExplorer)
plot_histogram(merged_dfp$dwel_time)

# install dplyr
# install.packages('dplyr')
library(dplyr)
merged_dfp %>%
  select(dwel_time, act_freq) %>%
  plot_histogram()

merged_dfp %>%
  select(dwel_time, act_freq) %>%
  plot_density()

# install other EDA libraries
# install.packages('summarytools')
library(summarytools)
dfSummary(merged_dfp[, c('dwel_time', 'R_SEX')])

# check and remove records with missing gender information
table(merged_dfp$R_SEX)
merged_dfp <- subset(merged_dfp, R_SEX >= 0)

# get average dwell_time for each gender category
tapply(merged_dfp$dwel_time, merged_dfp$R_SEX, mean)
round(tapply(merged_dfp$dwel_time, merged_dfp$R_SEX, mean), digits = 1) # round to one digit

# get descriptive (summary) statistics
library(plyr)
ddply(merged_dfp, .(R_SEX), summarise, mean=mean(act_freq), Std.Dev=sqrt(var(act_freq)))
count(merged_dfp$R_SEX)
table(merged_dfp$R_SEX)

# define auto availability variable: 1 = deficient, 2 = sufficient 
dfSummary(merged_dfp[, c('HHVEHCNT', 'DRVRCNT')])
merged_dfp$auto_avail= ifelse(merged_dfp$HHVEHCNT < merged_dfp$DRVRCNT, 1, 
                              ifelse (merged_dfp$HHVEHCNT >= merged_dfp$DRVRCNT, 2, 0 ))

# get summary stats
ddply(merged_dfp, .(auto_avail), summarise, mean=mean(dwel_time), Std.Dev=sqrt(var(dwel_time)))
count(merged_dfp$auto_avail)

# perform independent sample t-test
# t-test between gender groups
count(merged_dfp$R_SEX) # check whether there are only two groups
t.test(act_freq ~ R_SEX, data=merged_dfp, var.equal=TRUE)  # assumed equal variances between populations

# t-test between auto availability groups
count(merged_dfp$auto_avail) # check whether there are only two groups
t.test(dwel_time  ~ auto_avail, data=merged_dfp, var.equal=TRUE)  # assumed equal variances between populations

library(ggplot2)

# For t-test of activity frequency by gender
gender_plot <- ggplot(merged_dfp, aes(x = factor(R_SEX), y = act_freq, fill = factor(R_SEX))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_manual(
    values = c("1" = "#8884d8", "2" = "#82ca9d"),
    name = "Gender",
    labels = c("Male", "Female")
  ) +
  labs(
    title = "Average Number of Activities by Gender",
    subtitle = "p-value = 0.2399 (not significant)",
    x = "Gender",
    y = "Average Number of Activities"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# For t-test of dwell time by auto availability
auto_plot <- ggplot(merged_dfp, aes(x = factor(auto_avail), y = dwel_time, fill = factor(auto_avail))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_manual(
    values = c("1" = "#8884d8", "2" = "#82ca9d"),
    name = "Auto Availability",
    labels = c("Deficient", "Sufficient")
  ) +
  labs(
    title = "Average Dwell Time by Auto Availability",
    subtitle = "p-value < 0.001 (significant)",
    x = "Auto Availability",
    y = "Average Dwell Time (minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Add error bars
gender_plot <- gender_plot + 
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)

auto_plot <- auto_plot + 
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)

# Display plots
print(gender_plot)
print(auto_plot)

# Add data validation checks
print(table(merged_dfp$R_SEX, useNA="always"))  # Check for NA values in gender
print(table(merged_dfp$auto_avail, useNA="always"))  # Check for NA values in auto availability

# Add descriptive statistics before t-tests
aggregate(act_freq ~ R_SEX, data=merged_dfp, FUN=function(x) c(mean=mean(x), sd=sd(x), n=length(x)))
aggregate(dwel_time ~ auto_avail, data=merged_dfp, FUN=function(x) c(mean=mean(x), sd=sd(x), n=length(x)))

# Add confidence intervals to output
t.test(act_freq ~ R_SEX, data=merged_dfp, var.equal=TRUE, conf.level=0.95)
t.test(dwel_time ~ auto_avail, data=merged_dfp, var.equal=TRUE, conf.level=0.95)

# Save plots if needed
# ggsave("gender_comparison.png", gender_plot)
# ggsave("auto_comparison.png", auto_plot)



############################################################################################################################
################################# PART 2: TRIP PURPOSE DISTRIBUTION AND CHI-SQUARE TESTS ###################################
############################################################################################################################

# clear the global environment
rm(list = ls()) 

# set working directory to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# read data frames
dft <- read.csv("nhts_2017/trippub.csv")

# extracting data sets
dft <- subset(dft, HHSTATE == "AZ" | HHSTATE == "NM" | HHSTATE == "CO" | HHSTATE == "UT") 
dft <- subset(dft, TRAVDAY >= 2 & TRAVDAY <= 6 & R_AGE >= 18)

# get univariate distributions
library(summarytools)
dfSummary(dft[, c('WHYTRP1S', 'WORKER')])

# get summary stats (percentage distribution)
table_1 <- table(dft$WHYTRP1S, dft$WORKER) # counts cross variables
prop.table(table_1) # percentages
round(prop.table(table_1, 2), 2) 
round(100*prop.table(table_1, 2), 2) 
addmargins(round(prop.table(table_1, 2), 2),1) 

# alternatively use SmartEDA
# install.packages('SmartEDA')
library(SmartEDA)
library(dplyr)

dft <- dft %>%
  mutate(
    trip_purpose = case_when(
      WHYTRP1S == 1  ~ "Home",
      WHYTRP1S == 10 ~ "Work",
      WHYTRP1S == 20 ~ "School/Daycare/Religious",
      WHYTRP1S == 30 ~ "Medical/Dental",
      WHYTRP1S == 40 ~ "Shopping/Errands",
      WHYTRP1S == 50 ~ "Social/Recreational",
      WHYTRP1S == 70 ~ "Transport someone",
      WHYTRP1S == 80 ~ "Meals",
      WHYTRP1S == 97 ~ "Other",
      TRUE ~ NA_character_ # Catch-all for unexpected values
    )
  )

dft <- dft %>%
  mutate(
    employment = case_when(
      WORKER == 1  ~ "Worker",
      WORKER == 2 ~ "Non-worker",
      TRUE ~ NA_character_ # Catch-all for unexpected values
    )
  )

ExpCatViz(
  dft %>%
    select(trip_purpose, employment),
  target='employment'
)

# chi-square test (see here for more details: https://en.wikipedia.org/wiki/Pearson%27s_chi-squared_test and https://en.wikipedia.org/wiki/Chi-squared_test)
# h_0 = The trip purpose is independent of employment status
table_1
chisq.test(table_1)

table_1_percent <- round(100*prop.table(table_1,2), 2)
table_1_percent
chisq.test(table_1_percent)

# age 65p vs. rest of the sample
dft <-subset(dft,R_AGE >= 5)
summary(dft)
dft$age_65p= ifelse(dft$R_AGE>=65,1,0)

# getting descriptive stats (percentage distribution)
table_2 <- table(dft$WHYTRP1S, dft$age_65p)
table_2
chisq.test(table_2)
addmargins(table_2)
prop.table(table_2,2) 
round(prop.table(table_2,2), 2) 
round(100*prop.table(table_2,2), 2) 
addmargins(round(100*prop.table(table_2, 2), 2),1) 


# chi-square test (r documentation: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/chisq.test)
table_2_percent <- round(100*prop.table(table_2,2), 2)
chisq.test(table_2_percent) 

# Create visualizations from the actual table values
library(ggplot2)

# Convert table_1 to a data frame for plotting
worker_data <- as.data.frame(table_1)
colnames(worker_data) <- c("Purpose", "Status", "Count")
worker_data$Percentage <- round(100 * worker_data$Count/colSums(table_1)[worker_data$Status], 2)

# Convert numeric purpose codes to descriptive labels
purpose_labels <- c(
  "1" = "Home",
  "10" = "Work", 
  "20" = "School/Religious",
  "30" = "Medical",
  "40" = "Shopping",
  "50" = "Social/Recreational",
  "70" = "Transport",
  "80" = "Meals",
  "97" = "Other"
)
worker_data$Purpose <- factor(purpose_labels[as.character(worker_data$Purpose)], 
                              levels = purpose_labels)

# Create Employment Status plot
worker_plot <- ggplot(worker_data, aes(x = Purpose, y = Percentage, fill = factor(Status))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("#8884d8", "#82ca9d"),
    name = "Employment Status",
    labels = c("Worker", "Non-worker")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(
    title = "Trip Purpose Distribution by Employment Status",
    subtitle = paste("Chi-square test p-value", format.pval(chisq.test(table_1)$p.value, digits = 2)),
    x = "Trip Purpose",
    y = "Percentage (%)"
  )

# Similarly for age groups
age_data <- as.data.frame(table_2)
colnames(age_data) <- c("Purpose", "Age", "Count")
age_data$Percentage <- round(100 * age_data$Count/colSums(table_2)[age_data$Age], 2)
age_data$Purpose <- factor(purpose_labels[as.character(age_data$Purpose)], 
                           levels = purpose_labels)

# Create Age Groups plot
age_plot <- ggplot(age_data, aes(x = Purpose, y = Percentage, fill = factor(Age))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("#8884d8", "#82ca9d"),
    name = "Age Group",
    labels = c("Under 65", "65 and older")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(
    title = "Trip Purpose Distribution by Age Group",
    subtitle = paste("Chi-square test p-value", format.pval(chisq.test(table_2)$p.value, digits = 2)),
    x = "Trip Purpose",
    y = "Percentage (%)"
  )

# Display plots
print(worker_plot)
print(age_plot)

# Worker Status plot with frequencies
worker_plot <- ggplot(worker_data, aes(x = Purpose, y = Count, fill = factor(Status))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("#8884d8", "#82ca9d"),
    name = "Employment Status",
    labels = c("Worker", "Non-worker")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(
    title = "Trip Purpose Distribution by Employment Status",
    subtitle = paste("Chi-square test p-value", format.pval(chisq.test(table_1)$p.value, digits = 2)),
    x = "Trip Purpose",
    y = "Number of Trips"
  )

# Age Groups plot with frequencies
age_plot <- ggplot(age_data, aes(x = Purpose, y = Count, fill = factor(Age))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("#8884d8", "#82ca9d"),
    name = "Age Group",
    labels = c("Under 65", "65 and older")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(
    title = "Trip Purpose Distribution by Age Group",
    subtitle = paste("Chi-square test p-value", format.pval(chisq.test(table_2)$p.value, digits = 2)),
    x = "Trip Purpose",
    y = "Number of Trips"
  )

# Display plots
print(worker_plot)
print(age_plot)


###########################################################################################
################################## PART 3: MODAL SPLIT FOR TRIP TYPES #####################
###########################################################################################


################################################
### part_3a: modal split for home based work ###
################################################

# clear the global environment
rm(list = ls()) 

# setwd("C:/Users/ibatur/Dropbox (ASU)/Irfan Batur/ASU-Courses/fall_2022/abm/project_1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to current file location

# read data frames
dft <- read.csv("nhts_2017/trippub.csv")

# extracting data sets
dft <- subset(dft, HHSTATE == "AZ" | HHSTATE == "NM" | HHSTATE == "CO" | HHSTATE == "UT") 
dft <- subset(dft, TRAVDAY >= 2 & TRAVDAY <= 6 & R_AGE >= 18)

# explore trptrans and numontrp variabls
table(dft$TRPTRANS)
table(dft$NUMONTRP)

# create dummy variables for selected modes
dft$sov       = ifelse((dft$TRPTRANS >= 3 &  dft$TRPTRANS <= 6) & dft$NUMONTRP == 1, 1, 0)  # SOV
dft$hov_2     = ifelse((dft$TRPTRANS >= 3 &  dft$TRPTRANS <= 6) & dft$NUMONTRP == 2, 1, 0)  # HOV2
dft$hov_3p    = ifelse((dft$TRPTRANS >= 3 &  dft$TRPTRANS <= 6) & dft$NUMONTRP >= 3, 1, 0)  # HOV3+ 
dft$bus       = ifelse((dft$TRPTRANS == 10 | dft$TRPTRANS == 11 | dft$TRPTRANS == 13 | dft$TRPTRANS == 14), 1, 0)
dft$rail      = ifelse((dft$TRPTRANS == 15 | dft$TRPTRANS == 16), 1, 0)
dft$bike      = ifelse((dft$TRPTRANS == 2), 1, 0)
dft$walk      = ifelse((dft$TRPTRANS == 1), 1, 0)
dft$other     = ifelse((dft$sov != 1 & dft$hov_2 != 1 & dft$hov_3p != 1 & dft$bus != 1 & dft$rail != 1 & dft$bike != 1 & dft$walk  != 1 ), 1, 0)

# create a variable by merging mode dummies
dft$new_mode = 1*dft$sov + 2*dft$hov_2 + 3*dft$hov_3p + 4*dft$bus + 5*dft$rail + 6*dft$bike + 7*dft$walk + 8*dft$other

# check if new variable is consistent
table(dft$new_mode, dft$TRPTRANS)

# check if new variable is consistent
table(dft$new_mode, dft$TRPTRANS)

# Check if WHYTRP1S exists and how it's stored
names(dft) # Verify column names again

# get home-based work trips
library(dplyr)
# dft %>% count(TRIPPURP)
dft <- subset(dft, TRIPPURP == "HBW")
#dft %>% count(TRIPPURP)

# define auto availability variable: 1 = deficient, 2 = sufficient 
dft$auto_avail= ifelse(dft$HHVEHCNT < dft$DRVRCNT, 1, 
                       ifelse (dft$HHVEHCNT >= dft$DRVRCNT, 2, 0 ))

# distribution of hbw trips by auto availability
table_1 <- table(dft$new_mode, dft$auto_avail)
table_1
prop.table(table_1,2) 
round(prop.table(table_1,2), 2) 
round(100*prop.table(table_1,2), 2) 
freq_table = addmargins(round(100*prop.table(table_1,2), 2) ,1) 
freq_table

dimnames(freq_table) <- list(mode              = c('sov', 'hov2', 'hov3+', 'bus', 'rail', 'bike', 'walk', 'other', 'Total'),
                             auto_availability = c("deficient", "suficient"))

freq_table

# apply t-test for sov among market segments
# check group means
library(plyr)
ddply(dft, .(auto_avail), summarise, mean=mean(sov), Std.Dev=sqrt(var(sov)))
count(dft$auto_avail) 

# two-sample t-test
t.test(sov ~ auto_avail, data=dft, var.equal=TRUE) 

library(ggplot2)

# Create data frame for plotting from frequency table 
plot_data <- as.data.frame(freq_table)
plot_data <- plot_data[plot_data$mode != "Total", ] # Remove total row

# Create clustered bar chart
clustered_plot <- ggplot(plot_data, aes(x = mode, y = Freq, fill = auto_availability)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#8884d8", "#82ca9d"),
                    name = "Auto Availability",
                    labels = c("Auto Deficient", "Auto Sufficient")) +
  labs(title = "Modal Split by Auto Availability",
       subtitle = "Home-based Work Trips",
       x = "Mode",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = sprintf("%.1f", Freq)),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 3)

# Print plot
print(clustered_plot)

# Save plot if needed
# ggsave("clustered_modal_split.png", clustered_plot, width = 10, height = 6)

###############################################
### part_3b: all trips (low vs high income) ###
###############################################

# clear the global environment
rm(list = ls()) 

# setwd("C:/Users/ibatur/Dropbox (ASU)/Irfan Batur/ASU-Courses/fall_2022/abm/project_1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to current file location

# read data frames
dft <- read.csv("nhts_2017/trippub.csv")

# extracting data sets
dft <- subset(dft, HHSTATE == "AZ" | HHSTATE == "NM" | HHSTATE == "CO" | HHSTATE == "UT") 
dft <- subset(dft, TRAVDAY >= 2 & TRAVDAY <= 6 & R_AGE >= 18)

# explore trptrans and numontrp variabls
table(dft$TRPTRANS)
table(dft$NUMONTRP)

# let's create new_mode variable using mutate frunction from dplyr 
dft <- dft %>%
  mutate(
    new_mode = case_when(
      (TRPTRANS >= 3 & TRPTRANS <= 6) & NUMONTRP == 1 ~ 'sov',  # SOV
      (TRPTRANS >= 3 & TRPTRANS <= 6) & NUMONTRP == 2 ~ 'hov2',  # HOV2
      (TRPTRANS >= 3 & TRPTRANS <= 6) & NUMONTRP >= 3 ~ 'hov3p',  # HOV3+
      TRPTRANS %in% c(10, 11, 13, 14) ~ 'bus',  # Bus
      TRPTRANS %in% c(15, 16) ~ 'rail',  # Rail
      TRPTRANS == 2 ~ 'bike',  # Bike
      TRPTRANS == 1 ~ 'walk',  # Walk
      TRUE ~ 'other'  # Other
    )
  )

# check if new variable is consistent
table(dft$new_mode, dft$TRPTRANS)

# explore income variable
table(dft$HHFAMINC)

# subset for missing income records and create an new income variable indicating low and high income
dft <- subset(dft,  HHFAMINC >= 0)
dft <- dft %>%
  mutate(
    income = case_when(
      HHFAMINC <= 3  ~ "Low",
      HHFAMINC >= 4 & HHFAMINC <= 6  ~ "Mid",
      HHFAMINC >= 7  ~ "High",
      TRUE ~ NA_character_ # Catch-all for unexpected values
    )
  )

dft <- subset(dft,  income  != 'Mid') # remove records for mid income

# check for NAs in trip purpose variable
sum(is.na(dft$TRIPPURP))
count(dft$TRIPPURP)

# distribution of all trips by income (<$30k vs >$75k)
table_1 <- table(dft$new_mode, dft$income)
table_1
prop.table(table_1,2) 
round(prop.table(table_1,2), 2) 
round(100*prop.table(table_1,2), 2) 
freq_table = addmargins(round(100*prop.table(table_1,2), 2) ,1) 
freq_table

# create sov dummy variable
dft$sov       = ifelse((dft$new_mode == 'sov'), 1, 0)  # SOV

# apply t-test for sov among market segments
t.test(sov ~ income, data=dft, var.equal=TRUE) 

# Convert frequency table to data frame for plotting
plot_data <- as.data.frame(table_1)  # Use raw counts for the plot
colnames(plot_data) <- c("mode", "income", "count") # Rename columns for clarity

# Calculate percentages for the plot
plot_data <- plot_data %>%
  group_by(income) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

# Create clustered bar chart
clustered_plot <- ggplot(plot_data, aes(x = mode, y = percentage, fill = income)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#8884d8", "#82ca9d"),
                    name = "Income Level",
                    labels = c("Low (<= $29,999)", "High (>= $75,000)")) +
  labs(title = "Modal Split by Income Level",
       subtitle = "All Trips",
       x = "Mode",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = sprintf("%.1f", percentage)),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 3)

# Print the plot
print(clustered_plot)


###############################################################################################################################################################################
############################################################# PART 4: TEMPORAL DISTRIBUTION ###################################################################################
###############################################################################################################################################################################

# clear the global environment
rm(list = ls()) 

# setwd("C:/Users/ibatur/Dropbox (ASU)/Irfan Batur/ASU-Courses/fall_2022/abm/project_1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to current file location

# read data frames
dft <- read.csv("nhts_2017/trippub.csv")

# subset for desired conditions
dft <- subset(dft, HHSTATE == "AZ" | HHSTATE == "NM" | HHSTATE == "CO" | HHSTATE == "UT") 
dft <- subset(dft, TRAVDAY >= 2 & TRAVDAY <= 6 & R_AGE >= 18)

# subset for desired conditions
table(dft$TRIPPURP)
dft <- subset(dft,TRIPPURP != -9) # remove -9 in TRIPPURP variable

# view STRTIME and STR_MIN variables to check if they are consistent
dft[order(dft$STRTTIME),][,c("STRTTIME","STR_MIN")]

# create trip start time variable
unique(dft$STRTTIME)
dft$STR_MIN <- as.character(dft$STRTTIME)

# add leading zeros to start time variable
library(stringr)
dft$STR_MIN <- str_pad(dft$STR_MIN, 4, pad = "0")

# split into hour and min
dft$hour <- substr(dft$STR_MIN, start = 1, stop = 2)
unique(dft$hour)

dft$min <- substr(dft$STR_MIN, start = 3, stop = 4)
unique(dft$min)

dft$hour <- as.numeric(dft$hour)
dft$min <- as.numeric(dft$min)

# merge hour and min as start time variable in min
dft$STR_MIN <- dft$hour*60 + dft$min 

# view STRTIME and STR_MIN variables to check if they are consistent
dft[order(dft$STRTTIME),][,c("STRTTIME","STR_MIN")]

# subset for desired trip purpose types
hbo         <- subset(dft, TRIPPURP == "HBO")
hbw         <- subset(dft, TRIPPURP == "HBW")
hbshop      <- subset(dft, TRIPPURP=="HBSHOP")

# plotting the graphs
library(maptools)
library(sp)
library(ggplot2)

# home based work (temporal distribution)
ggplot(hbw, aes(hbw$STR_MIN)) + geom_histogram(aes(y=..count../(1))) + 
  scale_x_continuous(breaks = seq(0, 1440, by = 30)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("HBW: Temporal Distribution") +
  labs(y= "Frequency", x = "Start Time in Min")


# home based work (temporal probability distribution)
ggplot(hbw, aes(hbw$STR_MIN)) + geom_histogram(aes(y=..count../sum(..count..))) + 
  scale_x_continuous(breaks = seq(0, 1440, by = 30)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("HBW: Time of Day Proportional Distribution") +
  labs(y= "Proportion", x = "Start Time in Min")

# home based shopping (temporal distribution)
ggplot(hbshop, aes(hbshop$STR_MIN)) + geom_histogram(aes(y=..count../(1.))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks = seq(0, 1440, by = 30)) + ggtitle("HBSHOP: Time of Day Frequency Distribution")+
  labs(y= "Proportion", x = "Start Time in Min")

# home based shopping (temporal probability distribution)
ggplot(hbshop, aes(hbshop$STR_MIN)) + geom_histogram(aes(y=..count../sum(..count..))) + 
  scale_x_continuous(breaks = seq(0, 1440, by = 30)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("HBSHOP: Time of Day Proportional Distribution") +
  labs(y= "Proportion", x = "Start Time in Min")

# AM peak and PM peak home-based work and work based-other
# exclude all the trips having either home as origin (whyfrom) or destination (whyto)
df_am <- subset(dft, STRTTIME >="600" & STRTTIME <="859" ) # get am trips
df_am['am_pm'] <- 'AM'
df_pm <- subset(dft, STRTTIME >="1600" & STRTTIME <="1859") # get pm trips
df_pm$am_pm <- 'PM'
df_am_pm <- rbind(df_am, df_pm) # bind/merge two data frames vertically
df_am_pm['work_trip'] <- ifelse(df_am_pm$WHYTO == 3 | df_am_pm$WHYTO == 4 | df_am_pm$WHYFROM == 3 | df_am_pm$WHYFROM == 4, 1, 0) #get work-related-trips (needs to be clarified in the class)
table(df_am_pm$work_trip, df_am_pm$am_pm) #cross-comparison of the number of trips by work_trip and am_pm trips
prop.table(table(df_am_pm$work_trip, df_am_pm$am_pm))*100 # percent wise comparison

# First create the AM peak period data
df_am <- subset(dft, STRTTIME >= "600" & STRTTIME <= "859")
df_am['am_pm'] <- 'AM'
df_am['work_trip'] <- ifelse(df_am$WHYTO == 3 | df_am$WHYTO == 4 | 
                               df_am$WHYFROM == 3 | df_am$WHYFROM == 4, 1, 0)

# Then create the PM peak period data  
df_pm <- subset(dft, STRTTIME >= "1600" & STRTTIME <= "1859")
df_pm['am_pm'] <- 'PM'
df_pm['work_trip'] <- ifelse(df_pm$WHYTO == 3 | df_pm$WHYTO == 4 | 
                               df_pm$WHYFROM == 3 | df_pm$WHYFROM == 4, 1, 0)

# Create the off-peak period data
df_offpeak <- subset(dft, (STRTTIME < "600" | 
                             (STRTTIME > "859" & STRTTIME < "1600") |
                             STRTTIME > "1859"))
df_offpeak['am_pm'] <- 'OFF'
df_offpeak['work_trip'] <- ifelse(df_offpeak$WHYTO == 3 | df_offpeak$WHYTO == 4 | 
                                    df_offpeak$WHYFROM == 3 | df_offpeak$WHYFROM == 4, 1, 0)

# Now create clean versions with just the columns we need
df_am_clean <- df_am[, c('work_trip', 'am_pm')]
df_pm_clean <- df_pm[, c('work_trip', 'am_pm')] 
df_offpeak_clean <- df_offpeak[, c('work_trip', 'am_pm')]

# Combine them
df_all_periods <- rbind(df_am_clean, df_pm_clean, df_offpeak_clean)

# Get the distributions
print("Raw counts:")
print(table(df_all_periods$work_trip, df_all_periods$am_pm))

print("\nPercentages:")
print(prop.table(table(df_all_periods$work_trip, df_all_periods$am_pm))*100)

library(ggplot2)

# Enhanced HBW Temporal Distribution
hbw_dist <- ggplot(hbw, aes(x = STR_MIN)) + 
  geom_histogram(fill = "#82ca9d", color = "#8884d8", bins = 48) + 
  scale_x_continuous(
    breaks = seq(0, 1440, by = 60),
    labels = function(x) sprintf("%02d:00", x %/% 60)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Home-Based Work Trip Distribution",
    subtitle = "By Time of Day",
    y = "Frequency",
    x = "Time of Day"
  )

# Enhanced HBW Proportional Distribution
hbw_prop <- ggplot(hbw, aes(x = STR_MIN)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), 
                 fill = "#8884d8", color = "#82ca9d", bins = 48) + 
  scale_x_continuous(
    breaks = seq(0, 1440, by = 60),
    labels = function(x) sprintf("%02d:00", x %/% 60)
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Home-Based Work Trip Proportional Distribution",
    subtitle = "By Time of Day",
    y = "Proportion",
    x = "Time of Day"
  )

# Enhanced HBSHOP Temporal Distribution
hbshop_dist <- ggplot(hbshop, aes(x = STR_MIN)) + 
  geom_histogram(fill = "#8884d8", color = "#82ca9d", bins = 48) + 
  scale_x_continuous(
    breaks = seq(0, 1440, by = 60),
    labels = function(x) sprintf("%02d:00", x %/% 60)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Home-Based Shopping Trip Distribution",
    subtitle = "By Time of Day",
    y = "Frequency",
    x = "Time of Day"
  )

# Enhanced HBSHOP Proportional Distribution
hbshop_prop <- ggplot(hbshop, aes(x = STR_MIN)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), 
                 fill = "#8884d8", color = "#82ca9d", bins = 48) + 
  scale_x_continuous(
    breaks = seq(0, 1440, by = 60),
    labels = function(x) sprintf("%02d:00", x %/% 60)
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Home-Based Shopping Trip Proportional Distribution",
    subtitle = "By Time of Day",
    y = "Proportion",
    x = "Time of Day"
  )

# Print all plots
print(hbw_dist)
print(hbw_prop)
print(hbshop_dist)
print(hbshop_prop)

# Add bar plot for AM/PM peak comparison
peak_comparison <- ggplot(df_am_pm, aes(x = am_pm, fill = factor(work_trip))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("#8884d8", "#82ca9d"),
    name = "Trip Type",
    labels = c("Other", "Work-Related")
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(
    title = "Work vs Non-Work Trips Distribution",
    subtitle = "AM Peak (6-8:59 AM) vs PM Peak (4-6:59 PM)",
    x = "Time Period",
    y = "Proportion"
  )

print(peak_comparison)


##################################################################################################
################################### PART 5: DAILY ACTIVITY DURATION ##############################
##################################################################################################

# clear the global environment
rm(list = ls()) 

# setwd("C:/Users/ibatur/Dropbox (ASU)/Irfan Batur/ASU-Courses/fall_2022/abm/project_1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to current file location

# load data frames and subset for desired conditions
dfp <- read.csv("nhts_2017/perpub.csv")
dft <- read.csv("nhts_2017/trippub.csv")
dfp <- subset(dfp, HHSTATE == "AZ" | HHSTATE == "NM" | HHSTATE == "CO" | HHSTATE == "UT")
dft <- subset(dft, HHSTATE == "AZ" | HHSTATE == "NM" | HHSTATE == "CO" | HHSTATE == "UT") 

dfp <- subset(dfp, TRAVDAY >= 2 & TRAVDAY <= 6 & R_AGE >= 18)
dft <- subset(dft, TRAVDAY >= 2 & TRAVDAY <= 6 & R_AGE >= 18) 

# work trips
# get work trips
dft_work <- subset(dft, WHYTRP1S == 10 & DWELTIME > 0)

# aggregate to the person level
aggregated_dft_work <- aggregate(list(dwel_time = dft_work$DWELTIME), 
                                 by=list(HOUSEID = dft_work$HOUSEID, PERSONID=dft_work$PERSONID), sum)
dfp_work   <- merge(dfp, aggregated_dft_work, by=c("HOUSEID", "PERSONID"), all=TRUE)

summary(dfp_work$dwel_time)

# sum NAs
sum(is.na(dfp_work$dwel_time))


# get univariate distributions
library(summarytools)
dfSummary(dfp_work[, c('dwel_time')])

# plot without zeros
library(ggplot2)
ggplot(dfp_work, aes(dwel_time)) + geom_histogram() + ggtitle("Work or Work-Related Activities") +
  labs(y= "Frequency (person)", x = "Dweling Time (min)")

# plot with zeros
dfp_work$dwel_time[is.na(dfp_work$dwel_time)] <- 0

summary(dfp_work$dwel_time)

ggplot(dfp_work, aes(dwel_time)) + geom_histogram() + ggtitle("Work or Work-Related Activities") +
  labs(y= "Frequency (person)", x = "Dweling Time (min)")


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to process activity durations
process_activity <- function(activity_code, activity_name) {
  # Filter trips for specific activity
  dft_activity <- subset(dft, WHYTRP1S == activity_code & DWELTIME > 0)
  
  # Aggregate to person level
  aggregated_dft <- aggregate(list(dwel_time = dft_activity$DWELTIME), 
                              by = list(HOUSEID = dft_activity$HOUSEID, PERSONID = dft_activity$PERSONID), 
                              sum)
  
  # Merge with person data
  dfp_activity <- merge(dfp, aggregated_dft, by = c("HOUSEID", "PERSONID"), all = TRUE)
  
  # Calculate average before replacing NA
  avg_duration <- round(mean(dfp_activity$dwel_time, na.rm = TRUE), 1)
  
  # Replace NA with 0 for plotting
  dfp_activity$dwel_time[is.na(dfp_activity$dwel_time)] <- 0
  
  # Create histogram
  p <- ggplot(dfp_activity, aes(dwel_time)) + 
    geom_histogram(fill = "#8884d8", color = "#82ca9d", bins = 30) +
    ggtitle(paste(activity_name, "Activity Duration")) +
    labs(y = "Frequency (persons)", x = "Duration (minutes)") +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, 
             label = paste("Average:", avg_duration, "mins"), 
             hjust = 1.1, vjust = 1.1, size = 5, color = "#333333")
  
  print(p)
  
  return(avg_duration)
}

# Process different activities
work_avg <- process_activity(10, "Work")
shopping_avg <- process_activity(40, "Shopping/Errands")  # Verify WHYTRP1S code
social_avg <- process_activity(50, "Social Recreation")    # Verify WHYTRP1S code

# Print averages
cat("Average daily activity durations:\n")
cat("- Work:", work_avg, "minutes\n")
cat("- Shopping/Errands:", shopping_avg, "minutes\n")
cat("- Social Recreation:", social_avg, "minutes\n")



# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to process activity durations with improved statistics
process_activity <- function(activity_code, activity_name) {
  # Filter trips for specific activity and positive duration
  dft_activity <- subset(dft, WHYTRP1S == activity_code & DWELTIME > 0)
  
  # Aggregate to person level
  aggregated_dft <- aggregate(list(dwel_time = dft_activity$DWELTIME), 
                              by = list(HOUSEID = dft_activity$HOUSEID, 
                                        PERSONID = dft_activity$PERSONID), 
                              sum)
  
  # Merge with person data
  dfp_activity <- merge(dfp, aggregated_dft, by = c("HOUSEID", "PERSONID"), all = TRUE)
  
  # Calculate statistics for non-zero durations
  non_zero_stats <- summary(dfp_activity$dwel_time[!is.na(dfp_activity$dwel_time)])
  participants <- sum(!is.na(dfp_activity$dwel_time))
  total_people <- nrow(dfp_activity)
  participation_rate <- round((participants/total_people) * 100, 1)
  
  # Create histogram for non-zero durations
  p <- ggplot(subset(dfp_activity, !is.na(dwel_time)), aes(dwel_time)) + 
    geom_histogram(fill = "#8884d8", color = "#82ca9d", bins = 30) +
    ggtitle(paste(activity_name, "Activity Duration Distribution")) +
    labs(y = "Frequency (persons)", 
         x = "Duration (minutes)",
         subtitle = paste0("Participation Rate: ", participation_rate, "%")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  print(p)
  
  # Return summary statistics
  return(list(
    non_zero_stats = non_zero_stats,
    participation_rate = participation_rate,
    total_participants = participants,
    total_sample = total_people
  ))
}

# Process different activities
work_stats <- process_activity(10, "Work")
shopping_stats <- process_activity(40, "Shopping/Errands")
social_stats <- process_activity(50, "Social/Recreational")

# Print comprehensive statistics
activities <- c("Work", "Shopping/Errands", "Social/Recreational")
for(i in 1:3) {
  stats <- list(work_stats, shopping_stats, social_stats)[[i]]
  cat("\n", activities[i], "Activity Statistics:\n")
  cat("Participation Rate:", stats$participation_rate, "%\n")
  cat("Number of Participants:", stats$total_participants, 
      "out of", stats$total_sample, "people\n")
  cat("Duration Statistics (minutes):\n")
  print(stats$non_zero_stats)
  cat("----------------------------------------\n")
}
