setwd("/home/fou/Downloads/data/")
data <- read.csv("MIE.csv")
library(magrittr)
source("/home/fou/Desktop/Online/RCoursera/MasteringSoftwareDevelopmentR/AdvanceR/AdvanceR/oop_code2.R")
#############
x <- make_LD(data)
print(class(x))
print(x)
out <- subject(x, 10)
print(out)
out <- subject(x, 14)
print(out)
out <- subject(x, 54) %>% summary
print(out)
out <- subject(x, 14) %>% summary
print(out)
out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)


## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)
out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
