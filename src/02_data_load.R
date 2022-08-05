#import dataset
kbobattingdata <- read_csv("data/kbobattingdata.csv")
View(kbobattingdata)

#run some functions to see what kind of data we're working with
head(kbobattingdata)
skim(kbobattingdata)
summary(kbobattingdata)
str(kbobattingdata) 