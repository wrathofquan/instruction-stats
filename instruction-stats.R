library(tidyverse)
library(googlesheets)
library(ggthemes)

gs_ls()

raw <- gs_url("https://docs.google.com/spreadsheets/d/1yg5lNLXUm7Upiz76a8QgyASyWJrUdRxcm9R_JwmyGA0/")

raw <- raw %>%
  gs_read(ws = "Form Responses 1")
  
  
raw <- rename(raw, timestamp = `Timestamp`,
              lastname = `Your Last Name`,
              type = `Type of interaction`,
              date = `Date`,
              course = `Course Information`,
              faculty = `Faculty Name`,
              patron = `Patron Type`,
              attendees = `Number of Attendees*`,
              location = `Location`,
              delivery = `Delivery Mode`,
              time = `Time Spent`,
              notes = `Notes (can include learning goals, location of instructional materials, co-presenters etc.)`,
              copresenters = `Copresenters`)

raw1 <- filter(raw, lastname == "Quan")
    

## interactions over time

#convert date variable from character class to date

raw1$date <- as.Date(raw1$date, "%m/%d/%Y")

p1 <- ggplot(raw1, aes(date)) +
  geom_histogram(binwidth = 10, stat = "bin", alpha = .2) +
  geom_density(stat = "bin", binwidth = 10)+
  theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  ggtitle("Entries over time") + scale_y_discrete(name="")


## type of research consultation

p2 <- ggplot(raw1, aes(type)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Interaction type")+ scale_x_discrete(name="")

## courses
#split the super long course title into an easier to read one
raw1 <- raw1 %>% separate(course, into = c("course","course2"),  sep = "\\-", extra = "merge")

#reorder the factors, so that they appear as ascending in chart
raw1$course <- factor(raw1$course, levels=names(sort(table(raw1$course), increasing=TRUE)))

p3 <- ggplot(data= subset(raw1, !is.na(course)), aes(x=course)) +
  geom_bar(stat= "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Course titles by count")+ scale_x_discrete(name="")

## type of interaction

raw1$type <- factor(raw1$type, levels=names(sort(table(raw1$type), increasing=TRUE)))


p3.1 <- ggplot(data= subset(raw1, !is.na(course)), aes(x=type)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE)
  ggtitle("")+ scale_x_discrete(name="")


## faculty

#this works to reorder by count of faculty
raw1$faculty <- factor(raw1$faculty, levels=names(sort(table(raw1$faculty), increasing=TRUE)))

p4 <- ggplot(data= subset(raw1, !is.na(faculty)), aes(x=faculty)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Faculty names")+ scale_x_discrete(name="")

## patrons

raw1$patron <- factor(raw1$patron, levels=names(sort(table(raw1$patron), increasing=TRUE)))

p5 <- ggplot(data= raw1, aes(x= patron)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Patron type")+ scale_x_discrete(name="")

## location
raw1$location <- factor(raw1$location, levels=names(sort(table(raw1$location), increasing=TRUE)))

p6 <- ggplot(data= raw1, aes(x=location)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Location")+ scale_x_discrete(name="")

## delivery mode
raw1$delivery <- factor(raw1$delivery, levels=names(sort(table(raw1$delivery), increasing=TRUE)))

p7 <- ggplot(data= raw1, aes(x=delivery)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Delivery mode")+ scale_x_discrete(name="")

## time

raw1$time <- factor(raw1$time, levels=names(sort(table(raw1$time), increasing=TRUE)))

p8 <- ggplot(data= raw1, aes(x=time)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Time")+ scale_x_discrete(name="")

## grid

grid.arrange(p1, p2, p3, p4,p5, p6, p7, p8, nrow = 2)


