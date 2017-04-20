library(tidyverse)
library(googlesheets)
library(ggthemes)
library(gridExtra)

#read in instruction stats sheet
gs_ls()

raw <- gs_url("https://docs.google.com/spreadsheets/d/1yg5lNLXUm7Upiz76a8QgyASyWJrUdRxcm9R_JwmyGA0/")

raw <- raw %>%
  gs_read(ws = "Form Responses 1")

#renames variables for easier typing

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

# to look at individuals
raw1 <- filter(raw, lastname == "Quan" )

#to look at entire group
raw1 <- raw

## interactions over time

#convert date variable from character class to date

raw1$date <- as.Date(raw1$date, "%m/%d/%Y")

p1 <- ggplot(raw1, aes(date)) +
  geom_histogram(binwidth = 10, stat = "bin", alpha = .2) +
  geom_density(stat = "bin", binwidth = 10)+
  theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  ggtitle("Entries over time") 


## type of research consultation
raw1$type <- factor(raw1$type, levels=names(sort(table(raw1$type), increasing=TRUE)))

p2 <- ggplot(raw1, aes(type)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Interaction type")+ scale_x_discrete(name="")

## courses
#split the super long course title into an easier to read one
# raw2 splits by white space to separate just department code
raw2 <- raw1 %>% separate(course, into = c("dept","course2", "course3"),  sep = " ", extra = "drop")

#create new data.frame with just department and count
raw3 <- count(raw2, dept) %>% filter(!is.na(dept))
raw3 <- arrange(raw3,desc(n))

#plot of most worked with departments
p3 <- ggplot(data = raw3, aes(reorder(dept, n), n)) + geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + scale_x_discrete(name="department")

#create new data.frame to look at department + course number

raw4 <- raw1 %>% separate(course, into = c("course","course2"),  sep = "\\-", extra = "merge")

course_sum <- as.data.frame(table(raw4$course)) 


#splitting and combining some unique courses with different separators

course_sum <- separate(course_sum, Var1, into = c("a","b", "c"), sep = " ", extra = "merge")

course_sum <- unite(course_sum, course, a, b, sep = " ")

course_sum <- course_sum %>% 
  rename(count = Freq) %>%
  arrange(desc(count))  %>%
  top_n(20)

#reorder the factors, so that they appear as ascending in chart
#course_sum <- factor(course_sum$course, levels=names(sort(table(course_sum$course), increasing=TRUE)))

p3 <- ggplot(data=course_sum, aes(x=reorder(course, count), y = count)) +
  geom_bar(stat= "identity") +
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

faculty <- as.data.frame(table(raw1$faculty)) 

faculty <- faculty %>% 
  rename(faculty = Var1, count = Freq) %>% 
  arrange(desc(count))  %>%
  top_n(20)

p4 <- ggplot(data= faculty, aes(x= reorder(faculty,count), y= count)) +
  geom_bar(stat= "identity") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Instructor names")+ scale_x_discrete(name="")

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

##wordcluoud

w<-count(raw, notes)
wordcloud(words = w$notes, freq=w$n)

1 <- count(raw1)

## grid

a <- grid.arrange(p1, p2, p5, p6,  nrow = 4)

b <- p3

c <- p4

d <- grid.arrange(p7, p8,  nrow = 1)

