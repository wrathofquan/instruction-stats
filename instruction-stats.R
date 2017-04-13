library(tidyverse)
library(googlesheets)

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

quan <- filter(raw, lastname == "Quan")
    

## interactions over time

p1 <- ggplot(quan, aes(date)) +
  geom_histogram(binwidth = 500, stat = "count")


## type of research consultation

p2 <- ggplot(quan, aes(type)) +
  geom_bar(stat = "count") +
  coord_flip()

## courses

p3 <- ggplot(data= subset(quan, !is.na(course)), aes(x=course)) +
  geom_bar(stat = "count") +
  coord_flip()


## faculty


p4 <- ggplot(data= subset(quan, !is.na(faculty)), aes(x=faculty)) +
  geom_bar(stat = "count") +
  coord_flip()

## patrons

p5 <- ggplot(data= quan, aes(x=patron)) +
  geom_bar(stat = "count") +
  coord_flip()

## location

p6 <- ggplot(data= quan, aes(x=location)) +
  geom_bar(stat = "count") +
  coord_flip()

## delivery mode

p7 <- ggplot(data= quan, aes(x=delivery)) +
  geom_bar(stat = "count") +
  coord_flip()

## time

p8 <- ggplot(data= quan, aes(x=time)) +
  geom_bar(stat = "count") +
  coord_flip()


## grid
grid.arrange(p1, p2, p3, p4,p5, p6, p7, p8, nrow = 2)


