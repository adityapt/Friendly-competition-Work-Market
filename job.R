                  #################################################
                  ### I am participating in Friendly competition### 
                  ###The Questions are answered using R############
                  #################################################

setwd("~/Resume/Job")
Sys.setlocale("LC_ALL", "English")
install.packages("readr")
library(readr)
job <- read_csv("nyc_jobs.CSV")
str(job)
nrow(job)


# since they havent specified which column to consider, i am considering Agency column
# To determine the Agency that has more openings

a = data.frame(aggregate(job$`#OfPositions`, list(job$Agency), sum))
a
ncol(a)

a[which.max(a$x),]

#        Group.1                    x
#23 DEPT OF HEALTH/MENTAL HYGIENE 2800

names(job)
a = data.frame(names(job))
a
names(a)= "job"

# removing the space inside the column names

library(sqldf)
a = sqldf("select replace(job,' ','') from a")
names(job) = unlist(a)
names(job)[13] = "Division"
names(job)
library(sqldf) 
nrow(job)
a = job

# To determine the department/division with minimum salary

agg = as.data.frame(aggregate(job$SalaryRangeTo, list(job$SalaryFrequency),min))
names(agg)  = c("Type","min")
names(agg)
agg[3,1]


unique(job[which(job$SalaryFrequency == agg[1,1] & job$SalaryRangeTo == agg[1,2]),c(13,11,10)])
unique(job[which(job$SalaryFrequency == agg[2,1] & job$SalaryRangeTo == agg[2,2]),c(13,11,10)])
unique(job[which(job$SalaryFrequency == agg[3,1] & job$SalaryRangeTo == agg[3,2]),c(13,11,10)])

  
######### The salary frequency is different for certain jobs. We need to find the minimum salary for each
# type and match it back with division

# The divisions with least salaries are mentioned in the below output

#           Division       SalaryFrequency SalaryRangeTo
#3252 ECB - Queens Office          Annual         29000
#1631 Asset Mgmt/Elevator Svcs     Daily           232
#53   Administration              Hourly           10


# to determine the department/division with Maximum salary
aggmax = as.data.frame(aggregate(job$SalaryRangeFrom, list(job$SalaryFrequency),max))
aggmax = as.data.frame(aggregate(cbind(job$SalaryRangeFrom,job$SalaryRangeTo), list(job$SalaryFrequency),max))
names(aggmax)  = c("Type","maxfrom","maxto")
names(aggmax)
aggmax

unique(job[which(job$SalaryFrequency == aggmax[1,1] & (job$SalaryRangeTo == aggmax[1,2] | job$SalaryRangeFrom == aggmax[1,3])),c(13,11,9,10)])
unique(job[which(job$SalaryFrequency == aggmax[2,1] & (job$SalaryRangeTo == aggmax[2,2] | job$SalaryRangeFrom == aggmax[2,3])),c(13,11,9,10)])
unique(job[which(job$SalaryFrequency == aggmax[3,1] & (job$SalaryRangeTo == aggmax[3,2] | job$SalaryRangeFrom == aggmax[3,3])),c(13,11,9,10)])

# The divisions with maximum salaries are mentioned in the below output
# considering that people can be till rangeto , i have considered all the departments that
# have a maximum from and to range of salaries. This can be observed below:

# Divisions with same maximum salaryrangeto but different salaryrangefrom are shown 

#         Division                  SalaryFrequency SalaryRangeFrom SalaryRangeTo
#588  Administration                       Annual          198518        198518
#590 Counsels Office                       Annual          188000        198518
#2439 Asset Mgmt/Mech. Maintenance         Daily             394           394
#58        SH Reproductive Health          Hourly              66            71
#393                   SH Medical          Hourly              66            71
#689 Applicant Investigation Unit          Hourly              69            71


##############################################################################################
########################### Jobs that are hardest to fill ####################################
##############################################################################################

str(job)
summary(job$`#OfPositions`)
which.min(job$`#OfPositions`)
levels(factor((job$BusinessTitle)))
#job$`#OfPositions`[which(job$BusinessTitle == "Supervisor Mechanics (Mechanical Equipment)")]

df = data.frame(aggregate(job$`#OfPositions`,list(job$BusinessTitle),sum))
top10= tail(sort(df$x),10)
top10df = as.data.frame(list(0))

for ( i in 1: 10) (for (j in 1:2) (top10df[i,j] = df[which(df$x == top5[i]),j]))
names(top10df) = c("position","count")
sqldf("select distinct SalaryRangeFrom, SalaryRangeTo,SalaryFrequency, BusinessTitle from job where BusinessTitle in 
      (select position from top10df)")

# looking at the slalary ranges of the positions that have more vacancies
# The funda being NY is a hub with so many people. If there are more vacancies then
# there are no applicants for those jobs

#       SalaryRangeFrom SalaryRangeTo SalaryFrequency                      BusinessTitle

#1               37            37          Hourly             Nurse, Bureau of School Health
#2            59743         86523          Annual                          Resident Engineer
#3            60000         60000          Annual                         Plumbing Inspector
#4               32            37          Hourly             Nurse, Bureau of School Health
#5            61237         88686          Annual                          Resident Engineer
#6               38            38          Hourly Junior Public Health Nurse (School Health)
#7            54347         74187          Annual            School Mental Health Consultant
#8            52000         52000          Annual                         Plumbing Inspector
#9            37907         37907          Annual                   Parks Enforcement Patrol
#10           52505         80538          Annual             Investigate Consultant Level I
#11           51586         77404          Annual                          Resident Engineer
#12           69000         70422          Annual                          Resident Engineer
#13              12            12          Hourly          City Seasonal Aide/Security Guard
#14           46466         56270          Annual                              Design Intern
#15           46466         56270          Annual                        Construction Intern
#16           48410         60000          Annual                         Plumbing Inspector

# there are a lot of openings for the aforementioned business titles and the salary is very less
# many people are moving towards soft collar jobs and are searching for better jobs 
# So, I feel that the jobs that are very hard to fill in NY are 

#[1] "Design Intern"                              "Construction Intern"                       
#[3] "Junior Public Health Nurse (School Health)" "Parks Enforcement Patrol"                  
#[5] "Plumbing Inspector"                         "Nurse, Bureau of School Health"             
#[7] "City Seasonal Aide/Security Guard"         
