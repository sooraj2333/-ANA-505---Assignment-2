# Workforce Scheduling for Anonymous Bank Call Center (R)

##Loading all the packages from 3 to 16.
library(lubridate)  # date functions
##loading package to parse and manipulate the date.

library(grid)  # graphics utilities needed for split-plotting
##loading package that adds grid to graphical plots.

library(ggplot2)  # graphics package with ribbon plot
##loading package used to plot graphs.

library(queueing)  # queueing functions, including Erlang C
##loading package for Queuing model for determining the scheduling for shifts.

library(lpSolve)  # linear programming package
##loading high level linear programming package.

# ensure that two binary files are in the working directory
# these come from running R code from R_Utilities_Appendix
# source("R_utility_program_3.R") provides split-plotting utilities
load("mtpa_split_plotting_utilities.Rdata")
# source("R_utility_program_4.R") provides wait-time ribbon plots
load("mtpa_wait_time_ribbon_utility.Rdata")
##Code line 22 and 24 - Loading the saved special functions from the output of the 2 script files for split and wait time ribbon plots, R_utility_program_3.R and R_utility_program_4.R, respectively.

put.title.on.plots <- TRUE  # put title on wait-time ribbon plots
##code line 27 - assigning a logical value to a variable "put.title.on.plots". this will be later be called to add title on plotted graphs and to make universal changes when all graphs dont need a title.

##Comments below from  line 31 to 68 is the description and explanation of the variables in the anonymous bank dataset. 
# The call center data from "Anonymous Bank" in Israel were provided 
# by Avi Mandelbaum, with the help of Ilan Guedj.
# data source: http://ie.technion.ac.il/serveng/callcenterdata/index.html
# variable names and definitions from documentation 
# VRU  Voice Response Unit automated service
# vru.line  6 digits Each entering phone-call is first routed through a VRU: 
#           There are 6 VRUs labeled AA01 to AA06. Each VRU has several lines
#           labeled 1-16. There are a total of 65 lines. Each call is assigned 
#           a VRU number and a line number.
# call.id  unique call identifier
# customer.id  unique identifier for existing customer, zero for non-customer
# priority  0 or 1 for unidentified or regular customers
#           2 for priority customers who receive advanced position in queue
# type  type of service
#       PS  regular activity (coded 'PS' for 'Peilut Shotefet')
#       PE  regular activity in English (coded 'PE' for 'Peilut English')
#       IN  internet consulting (coded 'IN' for 'Internet')
#       NE  stock exchange activity (coded 'NE' for 'Niarot Erech') 
#       NW  potential customer getting information
#       TT  customers who left a message asking the bank to return their call 
#           but, while the system returned their call, the calling-agent became 
#           busy hence the customers were put on hold in the queue.
# date  year-month-day
# vru_entry  time that the phone-call enters the call-center or VRU
# vru_exit  time of exit from VRU directly to service or to queue
# vru_time  time in seconds spent in the VRU 
#           (calculated by exit_time – entry_time)
# q_start  time of joining the queue (00:00:00 for customers who abandon VRU
#          or do not enter the queue) 
# q_exit  time in seconds of exiting queue to receive service or abandonment
# q_time  time spent in queue (calculated by q_exit – q_start)
# outcome  AGENT = service
#          HANG = hang up
#          PHANTOM = a virtual call to be ignored
# ser_start  time of beginning of service by agent
# ser_exit  time of end of service by agent
# ser_time  service duration in seconds (calculated by ser_exit – ser_start)
# server  name of agent, NO_SERVER if no service provided


# focus upon February 1999
call.center.input.data <- read.table("data_anonymous_bank_february.txt", 
  header = TRUE, colClasses = c("character","integer","numeric",
  "integer","character","character","character","character","integer",
  "character","character","integer","factor","character","character",
  "integer","character"))
##Code line 72 to 76 - read.table function call reads the "data_anonymous_bank_february.txt" file in a table format and creates a data frame from it. arguments defined:
##Header is included in the file. the column classes are defined for all the variables using teh argument colClasses.
##The output of the read.table is assigned to the object "call.center.input.data".

##Code line 83 - checking and printing the "call.center.input.data" object and seeing the summary and simple descriptive statistics for better understanding of teh data present. 
# check data frame object and variable values
print(summary(call.center.input.data))

##Code line 87 - using subset function call : "deleting" or rather choosing a subset removing the 278 Phantom call(virtual calls) rows from the dataset(variable = outcome). the output is assigned to object "call.center.data".
# delete PHANTOM calls
call.center.data <- subset(call.center.input.data, subset = (outcome != "PHANTOM"))

##Code line 91 - using subset function call : "deleting" or rather choosing a subset removing the rows with negative values of vru_time from the dataset because teh time spent in VRU having a negative value does not make sense.the output is rewrites "call.center.data" object.
# negative VRU times make no sense... drop these rows from data frame
call.center.data <- subset(call.center.data, subset = (vru_time >= 0))

##Code line 95 to 96 - Adding a new variable wait time by adding the values in vru_time and q_time to get the total wait time. this addition in column creates 18 columns/varibles in "call.center.data" data frame object.
# calculate wait time as sum of vru_time and q_time
call.center.data$wait_time <- 
  call.center.data$vru_time + call.center.data$q_time

##Code line 102 - concatenating 19 to the date example - 990201(yymmdd) -> 19990201(yyyymmdd)
##code line 103 - uses function call ymd to change the character to date class ; example - 19990201 -> 1999-02-01
# define four-digit year so year is not read as 2099
# convert date string to date variable 
call.center.data$date <- paste("19", call.center.data$date, sep ="")
call.center.data$date <- ymd(call.center.data$date)

##Code line 108 - creates a new vairiable day_of_week in the dataframe call.center.data, where the function call wday gives the day of the week (1 to 7).
##Code line 109 - labeling all the days of the week to their corresponding day of the week using the factor function call and defining the level argument for defining the values the variable might take and labeling them correspondingly using the label argument.
# identify day of the week 1 = Sunday ... 7 = Saturday
call.center.data$day_of_week <- wday(call.center.data$date)
call.center.data$day_of_week <- factor(call.center.data$day_of_week,
  levels = c(1:7), labels = c("Sunday","Monday","Tuesday",
  "Wednesday","Thursday","Friday","Saturday"))

##Code line 115 - tabulating the day_of_week variable to see the count each day
# examine frequency of calls by day of week
print(table(call.center.data$day_of_week))

##Code Line 119 to 123 - separating the time stamps into lists and then referencing the 1st entry of each of the list to extract the hour of entry and creating a new variable call_hour in the object call.center.data.
# identify the hour of entry into the system
time.list <- strsplit(call.center.data$vru_entry,":")
call.hour <- numeric(nrow(call.center.data))
for (index.for.call in 1:nrow(call.center.data)) 
  call.hour[index.for.call] <- as.numeric(time.list[[index.for.call]][1])
call.center.data$call_hour <- call.hour

##Code line 127 - tabulating the day of the week against the call hour.
# check frequency of calls in February by hour and day of week
print(with(call.center.data, table(day_of_week, call_hour)))

##Code line 129 and 135 - creating subset of call.center.data, named selected.week which has the first week data and plotting tabular representation to see the number of calls every hour each day.
# select first week of February 1999 for data visualization and analysis
# that week began on Monday, February 1 and ended on Sunday, February 7
selected.week <- subset(call.center.data, subset = (date < ymd("19990208")))

# check frequency of calls in week by hour and day of week
print(with(selected.week, table(day_of_week, call_hour)))


##Code line 140 - creating a list for the day of the week and assigning too an object that can be called upon later.
# loop for day of week ignoring Saturdays in Isreal
day.of.week.list <- c("Monday","Tuesday",
  "Wednesday","Thursday","Friday","Sunday")

##Code line 149 to 168 - Wait time ribbon plots for each of the six days of the day.of.week.list(list) loop and plot them in a pdf. call upon the special functions created using the scripts - R_utility_program_4.R and R_utility_program_3.R
# wait-time ribbon plots for the six selected days
# call upon utility function wait.time.ribbon
# the utility makes use of grid split-plotting 
# place ribbon plot and text table/plot on each file
# each plot goes to its own external pdf file
for(index.day in seq(along=day.of.week.list)) {
  this.day.of.week <- day.of.week.list[index.day]
  pdf(file = paste("fig_operations_management_ribbon_",
  tolower(this.day.of.week),".pdf",sep=""), width = 11, height = 8.5)  
  if(put.title.on.plots) {
    ribbon.plot.title <- paste(this.day.of.week,"Call Center Operations")
    }
    else {
    ribbon.plot.title <- "" 
    }
  selected.day <- subset(selected.week, 
    subset = (day_of_week == this.day.of.week),
    select = c("call_hour","wait_time","ser_time","server"))
  colnames(selected.day) <- c("hour","wait","service","server")
  wait.time.ribbon(wait.service.data = selected.day, 
    title = ribbon.plot.title,
    use.text.tagging = TRUE, wait.time.goal = 30, wait.time.max = 90,
    plotting.min = 0, plotting.max = 250)    
  dev.off()  
  }

##Code line 172 - creating an object called wednesdays that has a subset of only wednesdays of feruary 1999.
# select Wednesdays in February for the queueing model
wednesdays <- subset(call.center.data, subset = (day_of_week == "Wednesday"))

##Code Line 177 to 185 - calculating the calls arrived from hour 1 to 24 on wednesdays of Feb 1999. the call_hour variable is indexed 0 to 23 which is taken into consideration by coded.index.for.hour object. 
# compute arrival rate of calls as calls for hour  
# we do not use table() here because some hours could have zero calls
calls.for.hour <- numeric(24)
for(index.for.hour in 1:24) { 
# 24-hour clock has first hour coded as zero in input data file
  coded.index.for.hour <- index.for.hour - 1  
  this.hour.calls <- 
    subset(wednesdays, subset = (call_hour == coded.index.for.hour))  
  if(nrow(this.hour.calls) > 0) 
    calls.for.hour[index.for.hour] <- nrow(this.hour.calls)  
  }

##Code line 189 - computing the arrival rate for each hour on each wednesday. data in numeric object call.for.hour is for all four wednesdays and therefore average is taken by dividing by 4.
# compute arrival rate as average number of calls into VRU per hour
hourly.arrival.rate <- calls.for.hour/4  # four Wednesdays in February

##Code line 195 - creating a subset of wednesday where services were provided(therefore excluding all values that have "NO_SERVER")
# service times can vary hour-by-hour due to differences 
# in service requests and individuals calling hour-by-hour
# begin by selecting calls that receive service
wednesdays.served <- subset(wednesdays, subset = (server != "NO_SERVER"))

##Code line 198 to 209 - calculating the number of calls serviced at each hour and assigning it to served.for.hour and calculating the mean duration of service in seconds and assigning it to hourly.mean.service.time. the subset now for this.hour.calls is reduced to wednesdays, servided entries.
hourly.mean.service.time <- numeric(24)
served.for.hour <- numeric(24)
for(index.for.hour in 1:24) { 
# 24-hour clock has first hour coded as zero in input data file
  coded.index.for.hour <- index.for.hour - 1  
  this.hour.calls <- 
    subset(wednesdays.served, subset = (call_hour == coded.index.for.hour))
  if(nrow(this.hour.calls) > 0) {
    served.for.hour[index.for.hour] <- nrow(this.hour.calls)
    hourly.mean.service.time[index.for.hour] <- mean(this.hour.calls$ser_time)
    }
  } 

##Code line 213 - computing the serverd rate for each hour on each wednesday. data in numeric object served.for.hour is for all four wednesdays and therefore average is taken by dividing by 4.
# hourly service rate given the current numbers of service operators
hourly.served.rate <- served.for.hour/4  # four Wednesdays in February

#Code line 217 to 224 - creating dataframes for hourly arrival rate and hourly served date with the hour indexing and the notation of "arrived" or "served" in every row. after binding the two dataframes to  single data frame arrival.service.data.frame.
# build data frame for plotting arrival and service rates
hour <- 1:24  # hour for horizontal axix of line chart
type <- rep("Arrived", length = 24)
value <- hourly.arrival.rate
arrival.data.frame <- data.frame(hour, value, type) 
type <- rep("Served", length = 24)
value <- hourly.served.rate
service.data.frame <- data.frame(hour, value, type) 
arrival.service.data.frame <- rbind(arrival.data.frame, service.data.frame)

##Code Line 227 to 244 - creating a graphical plot of the arrival and service rates on wednesdays of Feb 1999 using ggplot and difining axis and aesthetics as well. this is shown in 24 hour X-axis and arrival in yellow and served in green.
pdf(file = "fig_operations_management_wednesdays_arrived_served.pdf", 
  width = 11, height = 8.5)
plotting.object <- ggplot(data = arrival.service.data.frame, 
  aes(x = hour, y = value, fill = type)) + 
  geom_line() +
  geom_point(size = 4, shape = 21) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25),
    labels = 
      c("00","02","04","06","08","10","12","14","16","18","20","22","24")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  labs(x = "Hour of Day (24-Hour Clock)", y = "Average Calls per Hour") +
  scale_fill_manual(values = c("yellow","dark green"), 
    guide = guide_legend(title = NULL))  +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  theme(legend.text = element_text(size=15)) +
  coord_fixed(ratio = 1/10)    
print(plotting.object)
dev.off()

##Code line 249 to 263 - replacing 0 entries with the mean of the ser_time(service time) variable in the object subset wednesday.served. converting the cleaned object hourly.mean.service.time to the number of calls that is serviced in an hour in each of the 24 hours -> hourly.serviced.rate. then we take the mean of this and assign to an object "service.rate" which will be used in the queueing model as the service capacity.
# examine service times per service operator
# for hours with no service time information use the mean as value
hourly.mean.service.time <- 
  ifelse((hourly.mean.service.time == 0),
    mean(wednesdays.served$ser_time),
    hourly.mean.service.time) 
# compute service rate noting that there are 3600 seconds in an hour
# adding 60 seconds to each mean service time for time between calls
# this 60 seconds is the wrap up time or time a service agent remains 
# unavailable to answer a new call after a call has been completed
hourly.service.rate <- 3600/(hourly.mean.service.time + 60)

# we observe that mean service times do not vary that much hour-by-hour
# so we use the mean hourly service rate in queueing calculations
# mean(hourly.service.rate) is 14.86443
# so we use 15 calls per hour as the rate for one service operator
SERVICE.RATE <- 15

##Code line 274 to 282 - using the Queueing model to run in loop to find the number of servers needed each hour of wdenesdays.
# C_erlang function from the queueing package
# inputs c = number of servers
#        r = ratio of rate of arrivals and rate of service
# returns the propability of waiting in queue because all servers are busy
# let us set a target for the probability of waiting in queue to be 0.50
# using while-loop iteration we determine the number of servers needed 
# we do this for each hour of the day knowing the hourly arrival rate

PROBABILITY.GOAL <- 0.50
servers.needed <- integer(24)  # initialize to zero
for(index.for.hour in 1:24) {
  if (hourly.arrival.rate[index.for.hour] > 0) {
    erlang.probability <- 1.00  # intialization prior to entering while-loop
    while (erlang.probability > PROBABILITY.GOAL) {
      servers.needed[index.for.hour] <- servers.needed[index.for.hour] + 1
      erlang.probability <- C_erlang(c = servers.needed[index.for.hour], 
          r = hourly.arrival.rate[index.for.hour]/SERVICE.RATE)
      }  # end while-loop for defining servers needed given probability goal 
    }  # end if-block for hours with calls
  }  # end for-loop for the hour

# the result for servers.needed is obtained as
# 1  1  1  0  1  1  1  4  8  9 10  9  8 16 10 10  6  7  8  8  6  6  5  4
# we will assume the bank call center will be closed hours 00 through 05
# but use the other values as the bank's needed numbers of servers
servers.needed[1:6] <- 0 ## the servers needed for hour 1 to 6 (00 to 05) is minimal and it is not cost effective to start the shift as early as hour 1 or00. therefore reassigning teh first 6 values of the servers needed object as 0.

cat("\n","----- Hourly Operator Requirements -----","\n") ##Concatenating and printing header to show the heading "Hourly Operator Requirements" before printing (Code line 294) the servers.needed data below in the console of R output.
print(servers.needed)

## code line 298 and 301 - reading the CSV data of teh bank shifts which has binary input of the hours of the day for each shift and then assigning it to the dataframe bank.shifts.data.frame, then examining the structure of the data frame which can also be seen in global environment of R.studio.
# read in case data for the structure of call center worker shifts
bank.shifts.data.frame <- read.csv("data_anonymous_bank_shifts.csv")

# examine the structure of the case data frame
print(str(bank.shifts.data.frame))

constraint.matrix <- as.matrix(bank.shifts.data.frame[,3:10]) ##Creates a matrix of all the shift variables/columns with their respective values and assigned to object constraint.matrix.
cat("\n","----- Call Center Shift Constraint Matrix -----","\n") ##Concatenating and printing header to show the heading "Call Center Shift Constraint Matrix" before printing (Code line 305) the contraint.matrix data below in the console of R output.
print(constraint.matrix)

##Code line 312 to 319 - creating a cost vector (cost.vector) as the goal is to minimize the total cost. then using this to run an integer program to minimize the cost per shift.
# six-hour shift salaries in Israeli sheqels 
# 1 ILS = 3.61 USD in June 2013
# these go into the objective function for integer programing
# with the objective of minimizing total costs
cost.vector <- c(252,288,180,180,180,288,288,288) 

call.center.schedule <- lp(const.mat=constraint.matrix,
const.rhs = servers.needed, ##right handed constraints as the servers.needed object values.
const.dir = rep(">=",times=8), ##providing the direction of the constraint replicating 8 times.
int.vec = 1:8,
objective = cost.vector,
direction = "min") ## solving for minimizing the cost.

##Code line 323 to 337 - Preparing the summary of all the results of the linear program for a tabular summary of the data frame object created call.center.summary.
# prepare summary of the results for the call center problem
ShiftID <- 1:8 ##serialization of each of teh 8 shifts.
StartTime <- c(0,6,8,10,12,2,4,6) ## shift starts every 2 hours from 6 AM.
# c("Midnight","6 AM","8 AM","10 AM","Noon","2 PM","4 PM","6 PM")
ShiftDuration <- rep(6,times=8) ##replicating 6 for each shift as each shift is 6 hours.
HourlyShiftSalary <- c(42,48,30,30,30,48,48,48) ##cost.vector values divided by 6 hours in each shift to give the hourly cost.
HourlyShiftCost <- call.center.schedule$objective # six x hourly shift salary
Solution <- call.center.schedule$solution ##providing the optimal solution from the linear program.gives the number of servers to be employed in that shift. 
ShiftCost <- call.center.schedule$solution * call.center.schedule$objective ## cost of the total servers in each shift.

call.center.summary <- 
  data.frame(ShiftID,StartTime,ShiftDuration,HourlyShiftSalary,
  HourlyShiftCost,Solution,ShiftCost) ##creating a data frame of all the columns created from code line 323 to 330.
  
cat("\n\n","Call Center Summary","\n\n") ##Concatenating and printing header to show the heading "Call Center Summary" before printing (Code line 337) the call.center.summary data frame below in the console of R output.
print(call.center.summary)

# the solution is obtained by print(call.center.schedule) 
# or by summing across the hourly solution times the cost objective
 print(call.center.schedule) ##gives the total of the solution of the linear program, i.e $6336 for all shifts.
cat("\n\n","Call Center Summary Minimum Cost Solution:",sum(ShiftCost),"\n\n") ##Concatenating and printing header to show the heading "Call Center Summary Minimum Cost Solution:" before printing the sum of teh shift cost the console of R output.

##Coding line 346 to 354 - creating the solution data frame to compare the hourly need to the optimal solution and then binding it for a plotting data frame.
# build data frame for plotting the solution compared with need
hour <- 1:24  # hour for horizontal axix of line chart
type <- rep("Hourly Need", length = 24)
value <- servers.needed
needs.data.frame <- data.frame(hour, value, type) 
type <- rep("Optimal Solution", length = 24)
value <- schedule.fit.to.need <- 
  constraint.matrix %*% call.center.schedule$solution
solution.data.frame <- data.frame(hour, value, type) 
plotting.data.frame <- rbind(needs.data.frame, solution.data.frame)

##Code line 357 to 374 - creating a graphical plot of the servers needed and the optimal solution per hour on wednesdays of Feb 1999 using ggplot and defining axis and aesthetics as well. this is shown in 24 hour X-axis and hourly needs in white and optimal solution in blue.
# plot the solution... solution match to the workforce need
pdf(file = "fig_operations_management_solution.pdf", width = 11, height = 8.5)
plotting.object <- ggplot(data = plotting.data.frame, 
  aes(x = hour, y = value, fill = type)) + 
  geom_line() +
  geom_point(size = 4, shape = 21) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25),
    labels = 
      c("00","02","04","06","08","10","12","14","16","18","20","22","24")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  labs(x = "Hour of Day (24-Hour Clock)", y = "Number of Service Operators") +
  scale_fill_manual(values = c("white","blue"), 
    guide = guide_legend(title = NULL)) +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  theme(legend.text = element_text(size=15)) +
  coord_fixed(ratio = 2/2.25)    
print(plotting.object)
dev.off()

# Suggestion for the student:
# Try running a sensitivity test, varying the workforce requirements
# and noting the effect upon the optimal assignment of workers to shifts.


