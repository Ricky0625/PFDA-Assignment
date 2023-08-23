# ------------------------------------------------------------------------------------
# PROGRAMMING FOR DATA ANALYSIS - INDIVIDUAL ASSIGNMENT
# Module Code : CT127-3-2-PFDA
# Module Name : PROGRAMMING FOR DATA ANALYSIS
# Title       : Employee Attrition Analysis
# Intake      : APU2F2109SE
# Student Name: RICKY WONG TIONG SONG
# Student ID  : TP054622
# Lecturer    : LIEW YEE JING
# ------------------------------------------------------------------------------------

# ================================== Load libraries ==================================
library(tidyverse)
library(scales)

# =================================== Import Data ====================================
# Set the working directory
setwd("D:/Desktop/Degree-Y2/SEM-1/PFDA/Assignment/PFDA-Assignment")
# clean dataset
data <- read.csv("employee_attrition.csv")

# =============================== Data Pre-processing ================================
str(data)
summary(data)

# Convert char type data into factor, for date data convert from char to date.
emp <- data %>%
  mutate(
    orighiredate_key = as.Date(orighiredate_key, format = "%m/%d/%Y"),
    # 1/1/1900 means still working so set those as NA
    terminationdate_key = ifelse(terminationdate_key == "1/1/1900", NA, terminationdate_key),
    terminationdate_key = as.Date(terminationdate_key, format = "%m/%d/%Y"),
    city_name = as.factor(city_name),
    department_name = as.factor(department_name),
    job_title = as.factor(job_title),
    store_name = as.factor(store_name),
    gender_short = as.factor(gender_short),
    # typo. "Resignaton" -> "Resignation"
    termreason_desc = ifelse(termreason_desc == "Resignaton", "Resignation", termreason_desc),
    termreason_desc = as.factor(termreason_desc),
    termtype_desc = as.factor(termtype_desc),
    STATUS_YEAR = as.factor(STATUS_YEAR),
    STATUS_YEAR = ordered(STATUS_YEAR, c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)),
    STATUS = as.factor(STATUS),
    BUSINESS_UNIT = as.factor(BUSINESS_UNIT)
  ) %>% 
  # Remove meaningless attributes
  select(everything(), -c(gender_full, recorddate_key, birthdate_key))

# check the data and its structure
View(emp)
str(emp)
summary(emp)

emp %>%
  summarise(
    count = n_distinct(EmployeeID)
  )


# -------------------------------- Data Exploration ----------------------------------
# Try to get a general idea of the dataset
# -------------------------------- Treasure Hunt 1 -----------------------------------
# Have idea on the trend
emp %>% 
  filter(STATUS == "TERMINATED") %>% 
  group_by(STATUS_YEAR) %>% 
  ggplot(aes(STATUS_YEAR, fill = termreason_desc)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~gender_short) +
  coord_flip() +
  labs(title = "Overall trend of employee termination over 10 years", fill = "Reason") +
  xlab("Year") +
  ylab("Percentage (%)") +
  scale_fill_manual(values = c("Layoff" = "#008080",
                               "Resignation" = "#dba879",
                               "Retirement" = "#e2725b"))

# Some findings:
# Layoff only happened in 2014 and 2015
# Starting from 2011, no male employee retire from their job anymore
# many employee leave the company in 2014 and most common reason is layoff regardless the gender
# retirement most likely happened on female employees

# Questions that I wanna find out
# 1. What age did people retire at?
# 2. What are the reasons of layoffs?

# -------------------------------- Treasure Hunt 2 -----------------------------------
# another exploration
emp %>% 
  filter(
    STATUS == "TERMINATED",
    ) %>% 
  group_by(gender_short) %>% 
  ggplot(aes(gender_short, fill = gender_short)) +
  geom_bar() +
  facet_grid(termreason_desc~STATUS_YEAR) +
  labs(title = "Number of terminated employee of each year based on gender and termination reason", fill = "Gender") +
  xlab("Gender") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#F289AF",
                               "M" = "#68C1EB"))

# shows that layoff happen in 2014 and 2015 once again
# uneven age distribution because female employees mainly contribute to retirement.
# It means the company has more old female workers compared to male workers in the past 10 years.
# the number of resignations increased in 2011 to 2012 and decrease again




# =================================== Question 1 =====================================
# What age did people retire at?
# ====================================================================================
# In canada, the minimum age to get CCP is 60. Standard age is 65 but they can get it as early as 60, as late as 70

# Older employees are going to retire. Well, it's a good news for them and younger colleages
# who will take their place.
# But, this also means the company are losing experience-based knowledge. Experts are retiring.
# When a knowledgeable and long-serving colleagues retires, it can almost feel like they
# are taking the company with them. So, the company need to be well-prepared to handle
# the retirement wave so that the transition could be smooth.

# ----------------------------------- Analysis 1 -------------------------------------
# Find the relationship between age and termination reason
age_terminated <- emp %>% 
  drop_na(termreason_desc) %>% 
  filter(STATUS == "TERMINATED") %>% 
  group_by(age)

## boxplot with jitter
ggplot(data = age_terminated, mapping = aes(x = termreason_desc, y = age)) +
  geom_boxplot(mapping = aes(fill = termreason_desc)) +
  labs(title = "The reason of termination based on age (With Jitter)", fill = "Termination Reason") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  xlab("Termination Reason") +
  ylab("Age") +
  scale_fill_manual(values = c("Layoff" = "#008080",
                               "Resignation" = "#dba879",
                               "Retirement" = "#e2725b"))

# There is  bimodal distribution on "Retirement". Seems like employees will retire at the age of 60 or 65.

# ----------------------------------- Analysis 2 -------------------------------------
# Find the number of active employees that will going to retire soon
retire_soon <- emp %>% 
  filter(
    STATUS_YEAR == 2015,
    STATUS == "ACTIVE"
  ) %>% 
  group_by(job_title) %>% 
  mutate(
    might_retired_soon = ifelse((age >= 55 & age <= 65), TRUE, FALSE)
  )

ggplot(retire_soon, mapping = aes(x = fct_rev(fct_infreq(job_title)), fill = might_retired_soon)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of employees that soon to be retired based on business unit (55 to 65 years old in 2015)", fill = "Soon to be retired") +
  xlab("Job Title") +
  ylab("Percentage(%)") +
  coord_flip() +
  facet_wrap(~BUSINESS_UNIT, scales = "free")

ggplot(retire_soon, mapping = aes(x = fct_rev(fct_infreq(job_title)), fill = might_retired_soon)) +
  geom_bar() +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(title = "Number of employees that soon to be retired based on business unit (55 to 65 years old in 2015)", fill = "Soon to be retired") +
  xlab("Job Title") +
  ylab("Percentage(%)") +
  coord_flip() +
  facet_wrap(~BUSINESS_UNIT, scales = "free")

# Findings for this analysis:
## There will be a huge retirement wave in next 5 to 10 years
## There will be a huge number of meat cutter that will retire soon
## top management employees (HEADOFFICE) also will face a huge impact due to retirement
## important roles like CEO, CIO, VP Finance ... will also going to retire


# ----------------------------------- Analysis 5 -------------------------------------
# Find the number of middle management level employees that going to retire
store_manager_level = c("Store Manager", "Produce Manager", "Processed Foods Manager", "Meats Manager", "Customer Service Manager", "Bakery Manager")

stores_retire_soon <- emp %>% 
  filter(
    STATUS == "ACTIVE",
    BUSINESS_UNIT == "STORES",
    STATUS_YEAR == 2015
  ) %>% 
  mutate(
    level = ifelse(job_title %in% store_manager_level, "Manager", "Employee"),
    might_retired = ifelse((age >=55 & age <= 65), TRUE, FALSE)
  ) %>% 
  group_by(job_title)

ggplot(stores_retire_soon, aes(job_title, fill = might_retired)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~level, scales = "free") +
  labs(
    title = "Percentage of STORES employee that will going to retire in 5 to 10 years (Based on level)",
    fill = "Might Retire (in 5 to 10 years)") +
  xlab("Job Title") +
  ylab("Percentage (%)") +
  coord_flip()

stores_retire_soon %>% 
  group_by(level) %>% 
  summarise(
    count = n(),
    retire_rate = sum(might_retired == TRUE),
    percentage = paste(round((retire_rate/count) * 100, 0),"%")
  ) %>% 
  View()

# Within 5 to 10 years, the number of meat cutter will decrease significantly. (serious case)
# Within 5 to 10 years, these jobs might going to disappear in the company completely
## Produce Manager
## Processed Foods Manager
## Customer Service Manager
## Bakery Manager






#Changed By Nabin Bhatta(LBEF Campus - APU, Nepal). 1st analysis 5 should be executed, then only analysis 3 and 4 executes otherwise it will throw an error. 









# ----------------------------------- Analysis 3 -------------------------------------
## Find which city and store with be affected the most due to the meat cutter's huge retirement wave
stores_retire_soon %>% 
  filter(
    job_title == "Meat Cutter"
  ) %>%
  ggplot(aes(fct_rev(fct_infreq(city_name)))) +
  geom_bar(aes(fill = might_retired)) +
  labs(
    title = "Number of meat cutters that are going to retire in each city (2015 data)",
    fill = "Might Retire (in 5 to 10 years)") +
  xlab("City name") +
  ylab("Count") +
  coord_flip()

# Findings for this analysis:
## except for Vancouver, Abbotsford, and Nelson,
## the number of meat cutters will decrease significantly and reach an extent
## that some of the city no longer has any meat cutters

## the graph also shows the uneven ratio between younger meat cutters and older meat cutters

## company should hire new meat cutter
## company should let the older meat cutters to train the younger meat cutter to avoid knowledge gap

# ----------------------------------- Analysis 4 -------------------------------------
## Find which store will be affected the most due to the meat cutter's huge retirement wave
# latest data in 2015
emp %>% 
  filter(
    STATUS_YEAR == 2015,
    job_title == "Meat Cutter",
    STATUS == "ACTIVE"
  ) %>% 
  group_by(city_name) %>% 
  summarise(
    num_of_store = length(unique(store_name))
  ) %>% 
  arrange(desc(num_of_store)) %>% 
  View()

# Facts
## There a lots of meat cutter that might retire in 5 to 10 years.
## Some store even will not have any meat cutter any more.
## Other than the stores in Abbotsford, Nelson and Vancouver, the meat cutters in remaining
## stores has more than 50% of employees that will going to retire in 5 to 10 years.
## This also proves that the ratio of young employee and old employee for meat cutter is imbalance.

# the stores in each city (with meat cutters)
store_mc_retire_50 <- stores_retire_soon %>% 
  filter(job_title == "Meat Cutter") %>% 
  group_by(store_name) %>% 
  mutate(
    total = n(),
    count = sum(might_retired == TRUE),
    percentage = round((count/total) * 100, 0)
  )

ggplot(store_mc_retire_50, aes(city_name, store_name, color = (percentage >= 50))) +
  geom_label(aes(label = percentage), nudge_x = 0.5, nudge_y = 0.25) +
  geom_point(size = 5) +
  labs(
    title = "Stores that have more than 50% meat cutters going to retire soon",
    color = "More than 50%") +
  xlab("City") +
  ylab("Store Name") +
  coord_flip()

# Findings for this analysis:
## the company had 30 stores that have meats department in 2015
## Vancouver has 4 stores with a meats department
## other cities on the list only have 1 store
## Vancouver store (41 & 42) will not have any meat cutter left due to the retirement wave

# ----------------------------- Conclusion (Q1) -------------------------------------

# It seems that there will be a huge retirement wave in the next 5 to 10 years.
# the company should be well-prepared to overcome this
# there are somethings that needs to be done to prevent worst case to be happened
## Start hiring new employees especially for meat cutters
## Plan and start the top management level employee selection process. The process will be time-consuming
## Same goes to the middle management level employees, need to start the selection process

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Q1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 




# =================================== Question 2 =====================================
# What are the reasons for resignation?
# Resignation: an act of giving up a job or position formally or officially
# Reasons that caused resignation:
## 1.	  Not having a good relationship with the boss
## 2.	  Bored by the work itself
## 3.	  Relationships with coworkers are awful
## 4.	  Fewer opportunities to use their skills and abilities
## 5.	  The contribution of their work to the organization's business goals
## 6.	  They need to work independently most of the time
## 7.	  Meaningless job
## 8.	  Low recognition of employee's performance
## 9.	  The organization's financial status is not stable
## 10.	The culture of the organization

# by knowing why employee resign, the company can know how they should retain the employees

# ====================================================================================

# ----------------------------------- Analysis 1 -------------------------------------
# Find the relationship between age and resignation
age_resign <- emp %>% 
  filter(
    termreason_desc == "Resignation"
  ) %>% 
  group_by(as.factor(age))

ggplot(age_resign, aes(age)) +
  geom_histogram(aes(y = ..density.., fill = gender_short), bins = 30) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.6) +
  facet_wrap(~gender_short) +
  labs(title = "Density plot and Histogram of the relationship between age and number of resignation", fill = "Gender") +
  xlab("Age") +
  ylab("Density") +
  scale_fill_manual(values = c("F" = "#F289AF",
                               "M" = "#68C1EB"))

# Findings for this analysis:
## bimodal distribution in female employees where the highest peak is somewhere in the bin of 30 years old
## second-highest peak is somewhere between 20 - 25 years old
## Male employees' peak is somewhere in between 20 - 25 years old

## Problems:
### Job-hopping
### Students that just wanted to gain some experience before graduated
### Employees that just entered workforce
#### 1.	They don't feel like they're learning 
#### 2.	New opportunity arises. 
#### 3.	They take jobs that are the wrong fit. 
#### 4.	Unsure about their career path. 

# ----------------------------------- Analysis 2 -------------------------------------
# Find the relationship between age of first marriage and resignation

# use the graph in analysis 2-1

# Findings for this analysis:
## In canada, average age of first marriage will is around 30 years old
## So employees that around 30 years old in the graph can also represent female that just got into married life
## A report indicates that 43% of women quit their jobs after getting into married life to raise their families

## Reasons why married women leave workforce
### 1.	The company is not friendly toward married women. 
### 2.	The needs of taking care of the family. 
### 3.	Their family does not support them. 
### 4.	Their husband can financially support the family.

# ----------------------------------- Analysis 3 -------------------------------------
# Find the relationship between department and resignation
emp %>% 
  filter(
    termreason_desc == "Resignation"
  ) %>% 
  group_by(BUSINESS_UNIT) %>% 
  summarise(
    total_resignation = n()
  ) %>% 
  View()
# only one in headoffice, can ignore

store_resignation <- emp %>% 
  filter(
    termreason_desc == "Resignation",
    BUSINESS_UNIT == "STORES"
  ) %>% 
  group_by(department_name)
  
ggplot(store_resignation, aes(department_name)) +
  geom_bar(fill = "#dba879") +
  labs(title = "Total number of resignation in each department in STORES over the years") +
  xlab("Department Name") +
  ylab("Count")

# customer service has the highest resignation
# what are the jobs in customer service department?
emp %>% 
  filter(
    department_name == "Customer Service",
    termreason_desc != "Not Applicable"
  ) %>% 
  group_by(STATUS_YEAR) %>% 
  ggplot(aes(job_title, fill = termreason_desc)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Jobs in Customer Service Department and the Termination Reasons", fill = "Termination Reasons") +
  xlab("Job Title") +
  ylab("Percentage (%)")

# Findings for this analysis:
## Customer Service has the highest resignation rate
## Only cashier contribute the number
## According to a report done by Agile Craft, cashiers are at 97% risk of being impacted by automation 
## Job security is one of the crucial things when it comes to our job
## their job security has been threatened
## Fear of being replace, so they leave the company
## Big companies like Giant, Walmart, and others have started to install self-checkout lines in their store
## resign to look for another opportunity

# ----------------------------------- Analysis 4 -------------------------------------
# Find the relationship between the number of cashiers and resignation
emp %>% 
  filter(
    job_title == "Cashier"
  ) %>% 
  group_by(STATUS_YEAR) %>% 
  ggplot(aes(STATUS_YEAR, fill = gender_short)) +
  geom_bar(position = "dodge") +
  labs(title = "Numbers of cashiers by years", fill = "Gender") +
  xlab("Year") +
  ylab("Count")

# Findings for this analysis:
## the number of cashiers is increasing every year
## the number of cashiers only started to decrease in 2014
## high competition let the cashier to resign from the job

# ----------------------------------- Analysis 5 -------------------------------------
# Find the relationship between city type and resignation

# the city_name column is too tedious, so trying to categorize them into city classification
# The Degree of Urbanization identifies three types of settlements:
## Cities, which have a population of at least 50,000 inhabitants in contiguous dense grid cells (>1,500 inhabitants per km2);
## Towns and semi-dense areas, which have a population of at least 5,000 inhabitants in contiguous grid cells with a density of at least 300 inhabitants per km2; and
## Rural areas, which consist mostly of low-density grid cells (2).

# city population
emp_with_pop <- emp %>% 
  mutate(
    population = case_when(
      city_name == "Vancouver" ~ 600000,
      city_name == "Terrace" ~ 19443,
      city_name == "Nanaimo" ~ 84905,
      city_name == "Nelson" ~ 9813,
      city_name == "Kelowna" ~ 125109,
      city_name == "Victoria" ~ 289625,
      city_name == "Kamloops" ~ 68714,
      city_name == "Fort St John" ~ 17402,
      city_name == "Surrey" ~ 394976,
      city_name == "Vernon" ~ 47274,
      city_name == "Quesnel" ~ 13788,
      city_name == "Chilliwack" ~ 77000,
      city_name == "Dawson Creek" ~ 10802,
      city_name == "Squamish" ~ 58549,
      city_name == "New Westminster" ~ 85590,
      city_name == "Port Coquitlam" ~ 63508,
      city_name == "Cortes Island" ~	1050,
      city_name == "Burnaby" ~ 202799,
      city_name == "Bella Bella" ~ 1106,
      city_name == "Cranbrook" ~ 18610,
      city_name == "Williams Lake" ~ 14168,
      city_name == "Trail" ~ 9707,
      city_name == "Prince George" ~ 65558,
      city_name == "Richmond" ~ 182000,
      city_name == "Grand Forks" ~ 59166,
      city_name == "West Vancouver" ~ 42694,
      city_name == "Abbotsford" ~ 151683,
      city_name == "Aldergrove" ~ 12363,
      city_name == "Langley" ~ 23606,
      city_name == "North Vancouver" ~ 48000,
      city_name == "White Rock" ~ 66450,
      city_name == "New Westminister" ~ 58549,
      city_name == "Fort Nelson" ~ 3366,
      city_name == "Haney" ~ 21041,
      city_name == "Valemount" ~ 1021,
      city_name == "Ocean Falls" ~ 50,
      city_name == "Princeton" ~ 30681,
      city_name == "Dease Lake" ~ 400,
      city_name == "Pitt Meadows" ~ 17410,
      city_name == "Blue River" ~ 877
    ),
    city_class = case_when(
      population < 5000 ~ "rural",
      population >= 5000 & population < 50000 ~ "town",
      population >= 50000 ~ "city"
    ),
    city_class = as.factor(city_class),
    city_class = ordered(city_class, c("city", "town", "rural"))
  ) %>% 
  select(everything(), -c(population))

emp_with_pop %>% 
  filter(termreason_desc == "Resignation") %>% 
  group_by(city_class) %>% 
  ggplot(aes(city_class, fill = city_class)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  labs(title = "Total number of resignations based on the city classification", fill = "City Class") +
  xlab("City Class") +
  ylab("Count") +
  scale_fill_manual(values = c("city" = "#81b29a",
                               "town" = "#f2cc8f",
                               "rural" = "#e07a5f"))


# Findings for this analysis:
## urban area normally has more job opportunities, high-paying job openings are in urban area
## always will another chance
## job-hopping issue
  
## the other areas has comparably lesser job opportunities
## the only jobs are there, no much choice

# ----------------------------- Conclusion (Q2) -------------------------------------

# Job-hopping seems to be one of the factor that cause employee around 20-25 years old resign
# Female employees that got into marriage life is another group of employee that contribute a lot in resignation
# Among all the department, Customer Service department has the highest resignation rate among the others
## Cashier is the only job in customer service that has resignation case
# Employees that lived in city has higher resignation. City has more job opportunity so job-hopping is more likely to happen
# towns and rural areas has lesser job opportunity, so they only have few choices
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Q2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 




# =================================== Question 3 =====================================
# What are the reason of layoffs?
# ====================================================================================
# Being laid off refers to a temporary or permanent termination of work contract by an employee
# because of reasons relating to the business. A company may suspend just one worker or a group
# of workers at the same time.

# Reason of being laidoff:
## 1. cost reduction
## 2. staffing redundancies
## 3. relocation
## 4. merger or buyout - some role become redundant
## 5. downsizing
## 7. Outsourcing
## 8. Automation
## 9. Going out for business

# Common layoffs discrimination
## 1.	Age discrimination
## 2.	Gender discrimination
## 3.	Religion discrimination
## 4.	Nationality discrimination
## 5.	Race discrimination
## 6.	Disability discrimination


# data exploration already showed that only 2014 and 2015 have layoffs
# Exploration
emp %>% 
  group_by(STATUS_YEAR) %>% 
  filter(
    termreason_desc == "Layoff",
  ) %>% 
  ggplot(aes(STATUS_YEAR, fill = BUSINESS_UNIT))+
  geom_bar()

# Only occurs in STORES!

# ----------------------------------- Analysis 1 -------------------------------------
# Find which job has the highest layoff
emp %>% 
  group_by(STATUS_YEAR) %>% 
  filter(
    termreason_desc == "Layoff",
  ) %>% 
  ggplot(aes(fct_rev(fct_infreq(job_title)), fill = gender_short))+
  geom_bar(position = "dodge") +
  labs(title = "The number of layoffs based on job title", fill = "Gender") +
  xlab("Job Title") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#F289AF",
                               "M" = "#68C1EB")) +
  coord_flip()

# Findings for this analysis:
## cashier has the highest number once again but this time is layoff
## dairy person is the second highest
## meat cutter is the third highest

## robotics and automation is slowly replacing human's job
## Agile Craft stated that cashiers are at 97% risk of being impacted by automation
## economists say that cashiers will disappear in the next decade due to automation

## for dairy person and meat cutter,
## technology is also taking their place.

# Valid reasons: 
## staffing redundancies
## Automation

# ----------------------------------- Analysis 2 -------------------------------------
# Find the relationship between age and layoffs
age_layoff <- emp %>% 
  filter(
    termreason_desc == "Layoff"
  ) %>% 
  group_by(as.factor(age))

ggplot(age_layoff, aes(age)) +
  geom_histogram(aes(y = ..density.., fill = gender_short), bins = 30) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.6) +
  facet_wrap(~gender_short) +
  labs(
    title = "Density plot and Histogram of the relationship between age and number of layoffs",
    fill = "Gender") +
  xlab("Age") +
  ylab("Density") +
  scale_fill_manual(values = c("F" = "#F289AF",
                               "M" = "#68C1EB"))

# Findings for this analysis:
## young employees around 30± years old seem to have a high layoff rate regardless of gender
## both male and female employees that are 65 years old have the highest number of layoffs in both facets
## This could be a trick that the company may use to cover up older employees' large-scale layoffs
## https://www.hg.org/legal-articles/how-companies-may-use-layoffs-to-hide-age-discrimination-50105

# ----------------------------------- Analysis 3 -------------------------------------
# Are there any pregnancy discrimination layoffs that happened in the company?

# use the graph from analysis 3-2

# Findings for this analysis:
## the average age of first marriage for female in Canada is 30.8 years old
## the graph also shows that employees around that age has high layoff rate
## might have pregnancy discrimination
## usually, company will temporary layoff pregnant or afterbirth female to have long
## holiday to rest. After they fully recover, they can return back to the workforce
## But, sometimes some company will layoff those employee permanent as they think
## pregnant and afterbirth women will lower the productivity and their focus will not
## be on the work, so employers tend to layoff them. This is pregnancy discrimination

## But... in this dataset does not have any info that stated that those layoffs
## are temporary or permanent, so we only can suspect the company has pregnancy discrimination

# ----------------------------------- Analysis 4 -------------------------------------
# Are there any gender discrimination layoffs that happened in the company?
emp %>% 
  filter(
    termreason_desc == "Layoff"
  ) %>% 
  group_by(gender_short) %>% 
  ggplot(aes(gender_short, fill = gender_short)) +
  geom_bar() +
  facet_wrap(~STATUS_YEAR) +
  labs(title = "Number of layoffs in 2014 and 2015 based on gender", fill = "Gender") +
  xlab("Year") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#F289AF",
                             "M" = "#68C1EB"))

# Findings for this analysis:
## female seems to have higher layoff rate compared to male
## but, they only have little difference so this is not that abnormal
## so, no gender discrimination in layoff

# ----------------------------- Conclusion (Q3) -------------------------------------

# Automation seems to be the major problems that caused jobs like Cashier, Dairy Person and Meat cutter being laid off
# When trying to discover the layoffs discrimination that exists in the company, these are the result
## 1. Age discrimination - Might have. The company is trying to cover up the huge number of older
##                         employees being layoff by lay off some younger employees
## 2. Pregnancy discrimination - Don't have info about the types of layoffs so couldn't prove
## 3. Gender discrimination - Didn't exist

## HR should discuss with the Cashier, Dairy Person and Meat cutter before laid them off due to automation
## Shouldn't try to cover up the age discrimination in layoff

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Q3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
