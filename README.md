
# starting points
x_ini = 0;  y_ini=0


# target points
m = 11 # x-axis
n = 7  # y-axis


# find path
path = NULL
x = 0; y = 0

a = 1
while (a == 1){
  if (x < m & y < n){  # when x and y smaller than m and n
    moveDIR <- sample(1:2, 1) #1=x-direction, 2=y-direction
    if (moveDIR == 1){
      x = x + 1
      y = y
    } else {
      x = x
      y = y + 1
    }
  } else if (x == m & y < n){  # when x reaches m but not y
    moveDIR <- 2
    y = y + 1
  } else if (x < m & y == n){  # when y reaches n but not x
    moveDIR <- 1
    x = x + 1
  } else {
    break
  }
  path <- rbind(path, data.frame(x, y))
}


D <- pmax((path$x/m)-(path$y/n), (path$y/n)-(path$x/m)) # find maximum by each row between two vectors

options(digits=10)
mean(D)
sd(D)



# TRAFFIC STOP

MT <- read.csv ("MT_cleaned.csv", stringsAsFactors = FALSE)
VT <- read.csv ("VT_cleaned.csv", stringsAsFactors = FALSE)

options(digits=10)

# calculate male traffic stop
gender <- as.data.frame(table(MT$driver_gender)) # check for NA or blank values
gender$Freq[3] / sum(gender$Freq[2:3]) # ignore blank values

# calculate out of state traffic stop
outSTATE <- as.data.frame(table(MT$out_of_state)) # check for NA or blank values
outSTATE$Freq[2] / nrow(MT)

# Chi-Square test for proportion of arrest
table(MT$stop_outcome)
table(VT$stop_outcome)

# speeding violation
table(grepl("Speeding", MT$violation))
table(grepl("Speeding", MT$violation))[2] / nrow(MT)

# DUI violation
table(grepl("DUI", MT$violation))[2] / nrow(MT)
table(grepl("DUI", VT$violation))

# average vehicle year in 2020
year <- data.frame(stopYEAR = as.integer(substr(MT$stop_date, 1, 4)), vehYEAR = as.integer(MT$vehicle_year))
year <- na.omit(year) # remove NA
yrAGGR <- aggregate(year, by=(list(year$stopYEAR)), FUN=mean) # aggregate year of vehicle for each calendar year

plot(yrAGGR$stopYEAR, yrAGGR$vehYEAR)
fitYEAR <- lm(yrAGGR$vehYEAR ~ yrAGGR$stopYEAR); summary(fitYEAR)
abline(fitYEAR)
vehYRpred <- fitYEAR$coefficients[2]*2020 + fitYEAR$coefficients[1] #y=AX + B

# p-value
summary(fitYEAR)
fitYEAR$model

# diff in total number of stops by hour of day
MTVT <- c(MT$stop_time, VT$stop_time)
MTVThour <- as.integer(substr(MTVT, 1, 2))
hourAGGR <- aggregate(MTVThour, by=(list(MTVThour)), FUN=length)
max(hourAGGR$x) - min(hourAGGR$x)

# estimate area of counties
plot(MT$lon, MT$lat) # visual check for extreme values
test <- MT[MT$lat<60 & MT$lat>40 & MT$lon<=-50 & MT$lon>=-150,]
plot(test$lon, test$lat) # visual check for extreme values

counties <- unique(MT$county_name)
