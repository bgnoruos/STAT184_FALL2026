csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"
#Problem 2 part A
measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))
head(measurements)
str(measurements)
dim(measurements)
names(measurements)
colSums(is.na(measurements))
NA == NA
#Part B
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32
measurements$ph_below_7 <- measurements$ph < 7
subset_b <- measurements[complete.cases(measurements) & 
                           measurements$site %in% c("North", "South") & 
                           measurements$status == "ok", ]
mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)
#PART C
A <- matrix(1:4, nrow = 2)
A*B
dim(A*B)
A%%B
dim(A%%B)
