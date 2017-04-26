library(tidyverse)
library(googlesheets)
library(ggthemes)
library(gridExtra)
library(tidytext)

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

#convert date from character
raw$date <- as.Date(raw$date, "%m/%d/%Y")

#split date into month, year variables
raw <- raw %>% separate(date, into = c("year","month", "day"),  sep = "\\-", remove = FALSE) 
  
raw$month <- as.numeric(raw$month)


raw <- mutate(raw, semester = ifelse(month %in% 01:05, "Spring",
                                     ifelse(month %in% 06:08, "Summer",
                                            ifelse(month %in% 09:12, "Fall", "NA")))) 

raw$semester <- as.factor(raw$semester)



# to look at individuals
raw1 <- filter(raw, lastname == "Quan" )

#to look at entire group
raw1 <- raw

## interactions over time

trend <- ggplot(raw1, aes(date)) +
  geom_histogram(binwidth = 10, stat = "bin", alpha = .2) +
  geom_density(stat = "bin", binwidth = 10)+
  theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  ggtitle("Interactions over time") 


## type of research consultation
raw1$type <- factor(raw1$type, levels=names(sort(table(raw1$type), increasing=TRUE)))

interaction <- ggplot(raw1, aes(type)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Interaction type")+ scale_x_discrete(name="") 


## courses
#split the super long course title into an easier to read one
# raw2 splits by white space to separate just department code
raw2 <- raw1 %>% separate(course, into = c("dept","course2", "course3"),  sep = " ", extra = "drop")

#create new data.frame with just department and count
raw3 <- count(raw2, dept, semester) %>% filter(!is.na(dept))
raw3 <- arrange(raw3,desc(n))

#plot of most worked with departments
departments <- ggplot(data = raw3, aes(reorder(dept, n), n, fill = semester)) + geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + scale_x_discrete(name="department") +
  ggtitle("Departments by Count x Semester")

#create new data.frame to look at department + course number

raw4 <- raw1 %>% separate(course, into = c("course","course2"),  sep = "\\-", extra = "merge")

course_sum <- as.data.frame(table(raw4$course)) 


#splitting and combining some unique courses with different separators

course_sum <- separate(course_sum, Var1, into = c("a","b", "c"), sep = " ", extra = "merge")

course_sum <- unite(course_sum, course, a, b, sep = " ")

course_sum <- course_sum %>% 
  rename(count = Freq) %>%
  arrange(desc(count))  #%>%
  #top_n(20)

#reorder the factors, so that they appear as ascending in chart

course <- ggplot(data=course_sum, aes(x=reorder(course, count), y = count)) +
  geom_bar(stat= "identity") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Course titles by count")+ scale_x_discrete(name="")

## type of interaction

raw1$type <- factor(raw1$type, levels=names(sort(table(raw1$type), increasing=TRUE)))

interaction1 <- ggplot(data= subset(raw1, !is.na(course)), aes(x=type)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE)
  ggtitle("")+ scale_x_discrete(name = "") 
  


## faculty

faculty <- as.data.frame(table(raw1$faculty)) 

faculty <- faculty %>% 
  rename(faculty = Var1, count = Freq) %>% 
  arrange(desc(count))  %>%
  top_n(40)

faculty <- ggplot(data= faculty, aes(x= reorder(faculty,count), y= count)) +
  geom_bar(stat= "identity") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Instructor by Count")+ scale_x_discrete(name="")

## patrons

raw1$patron <- factor(raw1$patron, levels=names(sort(table(raw1$patron), increasing=TRUE)))

patron <- ggplot(data= raw1, aes(x= patron)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Patron type")+ scale_x_discrete(name="") 


## location
raw1$location <- factor(raw1$location, levels=names(sort(table(raw1$location), increasing=TRUE)))

location <- ggplot(data= raw1, aes(x=location)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Location")+ scale_x_discrete(name="") 


## delivery mode

delivery <- ggplot(data= raw1, aes(x=delivery)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Delivery mode")+ scale_x_discrete(name="") 


## time

raw1$time <- factor(raw1$time, levels=names(sort(table(raw1$time), increasing=TRUE)))

time <- ggplot(data= raw1, aes(x=time)) +
  geom_bar(stat = "count") +
  coord_flip()+
  theme_tufte(base_size = 11, base_family = "serif", ticks = FALSE) + 
  ggtitle("Time")+ scale_x_discrete(name="") 


##wordcluoud.. not super useful..

w<-count(raw, notes)
w <- w %>%
  na.omit() %>%
  unnest_tokens(notes1, notes) %>%
  count(notes1, sort = TRUE) %>%
  anti_join(stop_words) %>%
  ungroup()

w <- count(w, notes)
wordcloud(words = w$notes, freq=w$n)


## output onto grid. needs some work...

a <- grid.arrange(trend, time, interaction, patron, location, delivery,  nrow = 2)

b <- grid.arrange(departments, course, nrow = 1)

c <- faculty


