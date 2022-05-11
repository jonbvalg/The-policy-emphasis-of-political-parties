library(scales)
library(tidyverse)
library(readxl)
library(openxlsx)
library(moments)

setwd("C:/Users/Eier/OneDrive/Dokumenter/CAP")

#### The parties' vote share ####
Party  <- "AP"
year <- c(1981,1985,1989,1993,1997,2001,2005,2009,2013,2017,2021)
data <- cbind.data.frame(Party,year)

Party <- "FRP"
frp <- cbind.data.frame(Party,year)

data <- rbind(data,frp)

Party <- "V"
v <- cbind.data.frame(Party,year)

Party <- "H"
h <- cbind.data.frame(Party,year)

year <- c(1989,1993,1997,2001,2005,2009,2013,2017,2021)
Party <- "MDG"
mdg <- cbind.data.frame(Party,year)

data <- rbind(data,mdg)
data <- rbind(data,v)

year <- c(2001,2005,2009,2013,2017,2021)
Party <- "KP"
kp <- cbind.data.frame(Party,year)

data <- rbind(data,kp)
data <- rbind(data,h)

data$Vote <- c(0.371,0.408,0.343,0.369,0.350,0.243,0.327,
               0.354,0.308,0.274,0.263,0.045,0.037,0.130,
               0.063,0.153,0.146,0.221,0.229,0.163,0.152,
               0.116,0.004,0.001,0.002,0.002,0.001,0.003,
               0.028,0.032,0.039,0.039,0.031,0.032,0.036,
               0.045,0.039,0.059,0.039,0.052,0.044,0.046,
               0.017,0.008,0.002,0.001,0.001,0.000,0.304,
               0.317,0.222,0.170,0.143,0.212,0.141,0.172,
               0.268,0.250,0.204)

## Creating a graph showing the parties' vote
## share

ggplot(data, aes(year, Vote, group = Party, colour = Party)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black",
                                 "H" = "deepskyblue2")) +
  geom_hline(yintercept = 0.04, size = 0.1) +
  geom_line() +
  labs(y = "Share of the Votes",
       x = "Election year", 
       title = "Graph 4.1. Election results",
       caption = "The vertical line shows the election threshold introduced in 1989") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 0)) +
  scale_x_continuous(breaks = seq(1981,2021,4)) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

#### Downloading ap manifestos ####

ap1985 <- read_xlsx("AP_1985.xlsx")
ap1989 <- read_xlsx("AP_1989.xlsx")
ap1993 <- read_xlsx("AP_1993.xlsx")
ap1997 <- read_xlsx("AP_1997.xlsx")
ap2001 <- read_xlsx("AP_2001.xlsx")
ap2005 <- read_xlsx("AP_2005.xlsx")
ap2009 <- read_xlsx("AP_2009.xlsx")
ap2013 <- read_xlsx("AP_2013.xlsx")
ap2017 <- read_xlsx("AP_2017.xlsx")
ap2021 <- read_xlsx("AP 2021.xlsx")

#### Downloading V manifestos ####

v1985 <- read_xlsx("V_1985.xlsx")
v1989 <- read_xlsx("V_1989.xlsx")
v1993 <- read_xlsx("V_1993.xlsx")
v1997 <- read_xlsx("V_1997.xlsx")
v2001 <- read_xlsx("V_2001.xlsx")
v2005 <- read_xlsx("V_2005.xlsx")
v2009 <- read_xlsx("V_2009.xlsx")
v2013 <- read_xlsx("V_2013.xlsx")
v2017 <- read_xlsx("V_2017.xlsx")
v2021 <- read_xlsx("V_2021.xlsx")

### Downloading mdg manifestos ####

mdg1993 <- read_xlsx("MDG_1993.xlsx")
mdg1997 <- read_xlsx("MDG_1997.xlsx")
mdg2001 <- read_xlsx("MDG_2001.xlsx")
mdg2005 <- read_xlsx("MDG_2005.xlsx")
mdg2009 <- read_xlsx("MDG_2009.xlsx")
mdg2013 <- read_xlsx("MDG_2013.xlsx")
mdg2017 <- read_xlsx("MDG_2017.xlsx")
mdg2021 <- read_xlsx("MDG_2021.xlsx")

#### Downloading frp manifestos ####

frp1985 <- read_xlsx("FRP_1985.xlsx")
frp1989 <- read_xlsx("FRP_1989.xlsx")
frp1993 <- read_xlsx("FRP_1993.xlsx")
frp1997 <- read_xlsx("FRP_1997.xlsx")
frp2001 <- read_xlsx("FRP_2001.xlsx")
frp2005 <- read_xlsx("FRP_2005.xlsx")
frp2009 <- read_xlsx("FRP_2009.xlsx")
frp2013 <- read_xlsx("FRP_2013.xlsx")
frp2017 <- read_xlsx("FRP_2017.xlsx")
frp2021 <- read_xlsx("FRP_2021.xlsx")

#### Downloading kp manifestos ####

kp2001 <- read_xlsx("KP_2001.xlsx")
kp2005 <- read_xlsx("KP_2005.xlsx")
kp2009 <- read_xlsx("KP_2009.xlsx")
kp2013 <- read_xlsx("KP_2013.xlsx")
kp2017 <- read_xlsx("KP_2017.xlsx")
kp2021 <- read_xlsx("KP_2021.xlsx")

#### Downloading H manifestos ####

h1997 <- read_xlsx("H_1997.xlsx")
h2001 <- read_xlsx("H_2001.xlsx")
h2005 <- read_xlsx("H_2005.xlsx")
h2009 <- read_xlsx("H_2009.xlsx")
h2013 <- read_xlsx("H_2013.xlsx")
h2017 <- read_xlsx("H_2017.xlsx")
h2021 <- read_xlsx("H_2021.xlsx")

#### Creating the major topics for AP ####

ap1985 <- ap1985 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


ap1989 <- ap1989 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap1993 <- ap1993 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap1997 <- ap1997 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap2001 <- ap2001 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap2005 <- ap2005 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap2009 <- ap2009 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap2013 <- ap2013 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap2017 <- ap2017 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))
ap2021 <- ap2021 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,23,29))))))))))))))))))))))

#### Creating the major topics for frp ####

frp1985 <- frp1985 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))



frp1989 <- frp1989 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


frp1993 <- frp1993 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))



frp1997 <- frp1997 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))



frp2001 <- frp2001 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))



frp2005 <- frp2005 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


frp2009 <- frp2009 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


frp2013 <- frp2013 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


frp2017 <- frp2017 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


frp2021 <- frp2021 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

#### Creating the major topics for mdg ####

mdg1993 <- mdg1993 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


mdg1997 <- mdg1997 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


mdg2001 <- mdg2001 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


mdg2005 <- mdg2005 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


mdg2009 <- mdg2009 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


mdg2013 <- mdg2013 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


mdg2017 <- mdg2017 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


mdg2021 <- mdg2021 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

#### Creating the major topics for venstre ####

v1985 <- v1985 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v1989 <- v1989 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v1993 <- v1993 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v1997 <- v1997 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v2001 <- v2001 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v2005 <- v2005 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v2009 <- v2009 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v2013 <- v2013 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v2017 <- v2017 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


v2021 <- v2021 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

#### Creating the major topics for kp ####

kp2001 <- kp2001 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

kp2005 <- kp2005 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

kp2009 <- kp2009 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

kp2013 <- kp2013 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


kp2017 <- kp2017 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

kp2021 <- kp2021 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

#### Creating the major topics for H ####

h1997 <- h1997 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


h2001 <- h2001 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

h2005 <- h2005 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

h2009 <- h2009 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

h2013 <- h2013 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


h2017 <- h2017 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))

h2021 <- h2021 %>%
  mutate(Major_Topic = ifelse(Norway_Code <200,1, ifelse(Norway_Code <300, 2, 
                                                         ifelse(Norway_Code < 400,3,
                                                                ifelse(Norway_Code < 500, 4, ifelse(Norway_Code < 600,
                                                                                                    5,
                                                                                                    ifelse(Norway_Code < 700, 6, 
                                                                                                           ifelse(Norway_Code < 800,
                                                                                                                  7, 
                                                                                                                  ifelse(Norway_Code < 900,
                                                                                                                         8,
                                                                                                                         ifelse(Norway_Code < 1000,
                                                                                                                                9,
                                                                                                                                ifelse(Norway_Code < 1100,
                                                                                                                                       10,
                                                                                                                                       ifelse(Norway_Code < 1300,
                                                                                                                                              12,
                                                                                                                                              ifelse(Norway_Code < 1400,
                                                                                                                                                     13, ifelse(Norway_Code < 1500,
                                                                                                                                                                14,
                                                                                                                                                                ifelse(Norway_Code < 1600,
                                                                                                                                                                       15,
                                                                                                                                                                       ifelse(Norway_Code < 1700,
                                                                                                                                                                              16, ifelse(Norway_Code < 1800,
                                                                                                                                                                                         17,
                                                                                                                                                                                         ifelse(Norway_Code < 1900,
                                                                                                                                                                                                18,
                                                                                                                                                                                                ifelse(Norway_Code < 2000,
                                                                                                                                                                                                       19,
                                                                                                                                                                                                       ifelse(Norway_Code < 2100,
                                                                                                                                                                                                              20,
                                                                                                                                                                                                              ifelse(Norway_Code < 2200,
                                                                                                                                                                                                                     21, ifelse(Norway_Code < 2400,
                                                                                                                                                                                                                                23,29))))))))))))))))))))))


##### Recoding irrelevant sentences to NA ####

ap1985$Major_Topic[ap1985$Major_Topic == 29] <- NA
ap1989$Major_Topic[ap1989$Major_Topic == 29] <- NA
ap1993$Major_Topic[ap1993$Major_Topic == 29] <- NA
ap1997$Major_Topic[ap1997$Major_Topic == 29] <- NA
ap2001$Major_Topic[ap2001$Major_Topic == 29] <- NA
ap2005$Major_Topic[ap2005$Major_Topic == 29] <- NA
ap2009$Major_Topic[ap2009$Major_Topic == 29] <- NA
ap2013$Major_Topic[ap2013$Major_Topic == 29] <- NA
ap2017$Major_Topic[ap2017$Major_Topic == 29] <- NA
ap2021$Major_Topic[ap2021$Major_Topic == 29] <- NA

frp1985$Major_Topic[frp1985$Major_Topic == 29] <- NA
frp1989$Major_Topic[frp1989$Major_Topic == 29] <- NA
frp1993$Major_Topic[frp1993$Major_Topic == 29] <- NA
frp1997$Major_Topic[frp1997$Major_Topic == 29] <- NA
frp2001$Major_Topic[frp2001$Major_Topic == 29] <- NA
frp2005$Major_Topic[frp2005$Major_Topic == 29] <- NA
frp2009$Major_Topic[frp2009$Major_Topic == 29] <- NA
frp2013$Major_Topic[frp2013$Major_Topic == 29] <- NA
frp2017$Major_Topic[frp2017$Major_Topic == 29] <- NA
frp2021$Major_Topic[frp2021$Major_Topic == 29] <- NA

mdg1993$Major_Topic[mdg1993$Major_Topic == 29] <- NA
mdg1997$Major_Topic[mdg1997$Major_Topic == 29] <- NA
mdg2001$Major_Topic[mdg2001$Major_Topic == 29] <- NA
mdg2005$Major_Topic[mdg2005$Major_Topic == 29] <- NA
mdg2009$Major_Topic[mdg2009$Major_Topic == 29] <- NA
mdg2013$Major_Topic[mdg2013$Major_Topic == 29] <- NA
mdg2017$Major_Topic[mdg2017$Major_Topic == 29] <- NA
mdg2021$Major_Topic[mdg2021$Major_Topic == 29] <- NA

v1985$Major_Topic[v1985$Major_Topic == 29] <- NA
v1989$Major_Topic[v1989$Major_Topic == 29] <- NA
v1993$Major_Topic[v1993$Major_Topic == 29] <- NA
v1997$Major_Topic[v1997$Major_Topic == 29] <- NA
v2001$Major_Topic[v2001$Major_Topic == 29] <- NA
v2005$Major_Topic[v2005$Major_Topic == 29] <- NA
v2009$Major_Topic[v2009$Major_Topic == 29] <- NA
v2013$Major_Topic[v2013$Major_Topic == 29] <- NA
v2017$Major_Topic[v2017$Major_Topic == 29] <- NA
v2021$Major_Topic[v2021$Major_Topic == 29] <- NA

kp2001$Major_Topic[kp2001$Major_Topic == 29] <- NA
kp2005$Major_Topic[kp2005$Major_Topic == 29] <- NA
kp2009$Major_Topic[kp2009$Major_Topic == 29] <- NA
kp2013$Major_Topic[kp2013$Major_Topic == 29] <- NA
kp2017$Major_Topic[kp2017$Major_Topic == 29] <- NA
kp2021$Major_Topic[kp2021$Major_Topic == 29] <- NA

h1997$Major_Topic[h1997$Major_Topic == 29] <- NA
h2001$Major_Topic[h2001$Major_Topic == 29] <- NA
h2005$Major_Topic[h2005$Major_Topic == 29] <- NA
h2009$Major_Topic[h2009$Major_Topic == 29] <- NA
h2013$Major_Topic[h2013$Major_Topic == 29] <- NA
h2017$Major_Topic[h2017$Major_Topic == 29] <- NA
h2021$Major_Topic[h2021$Major_Topic == 29] <- NA

#### Creating dataframes for the major topics ####

ap85 <- data.frame(table(ap1985$Major_Topic)) %>%
  subset(select = Freq)
ap85 <- data.frame(t(ap85))

ap89 <- data.frame(table(ap1989$Major_Topic)) %>%
  subset(select = Freq)
ap89 <- data.frame(t(ap89))

ap93 <- data.frame(table(ap1993$Major_Topic)) %>%
  subset(select = Freq)
ap93 <- data.frame(t(ap93))

ap97 <- data.frame(table(ap1997$Major_Topic)) %>%
  subset(select = Freq)
ap97 <- data.frame(t(ap97))

ap01 <- data.frame(table(ap2001$Major_Topic)) %>%
  subset(select = Freq)
ap01 <- data.frame(t(ap01))

ap05 <- data.frame(table(ap2005$Major_Topic)) %>%
  subset(select = Freq)
ap05 <- data.frame(t(ap05))

ap09 <- data.frame(table(ap2009$Major_Topic)) %>%
  subset(select = Freq)
ap09 <- data.frame(t(ap09))

ap13 <- data.frame(table(ap2013$Major_Topic)) %>%
  subset(select = Freq)
ap13 <- data.frame(t(ap13))

ap17 <- data.frame(table(ap2017$Major_Topic)) %>%
  subset(select = Freq)
ap17 <- data.frame(t(ap17))

ap21 <- data.frame(table(ap2021$Major_Topic)) %>%
  subset(select = Freq)
ap21 <- data.frame(t(ap21))

frp85 <- data.frame(table(frp1985$Major_Topic)) %>%
  subset(select = Freq)
frp85 <- data.frame(t(frp85))

frp89 <- data.frame(table(frp1989$Major_Topic)) %>%
  subset(select = Freq)
frp89 <- data.frame(t(frp89))

frp93 <- data.frame(table(frp1993$Major_Topic)) %>%
  subset(select = Freq)
frp93 <- data.frame(t(frp93))

frp97 <- data.frame(table(frp1997$Major_Topic)) %>%
  subset(select = Freq)
frp97 <- data.frame(t(frp97))

frp01 <- data.frame(table(frp2001$Major_Topic)) %>%
  subset(select = Freq)
frp01 <- data.frame(t(frp01))

frp05 <- data.frame(table(frp2005$Major_Topic)) %>%
  subset(select = Freq)
frp05 <- data.frame(t(frp05))

frp09 <- data.frame(table(frp2009$Major_Topic)) %>%
  subset(select = Freq)
frp09 <- data.frame(t(frp09))

frp13 <- data.frame(table(frp2013$Major_Topic)) %>%
  subset(select = Freq)
frp13 <- data.frame(t(frp13))

frp17 <- data.frame(table(frp2017$Major_Topic)) %>%
  subset(select = Freq)
frp17 <- data.frame(t(frp17))

frp21 <- data.frame(table(frp2021$Major_Topic)) %>%
  subset(select = Freq)
frp21 <- data.frame(t(frp21))
table(mdg1993$Major_Topic)
mdg93 <- data.frame(table(mdg1993$Major_Topic)) %>%
  subset(select = Freq)
mdg93 <- data.frame(t(mdg93))
mdg93 <- rename(mdg93, X18 = X16, X20 = X17
)
mdg93$X16 <- 0
mdg93$X17 <- 0
mdg93$X19 <- 0
mdg93$X21 <- 0

mdg97 <- data.frame(table(mdg1997$Major_Topic)) %>%
  subset(select = Freq)
mdg97 <- data.frame(t(mdg97))

mdg01 <- data.frame(table(mdg2001$Major_Topic)) %>%
  subset(select = Freq)
mdg01 <- data.frame(t(mdg01))

mdg05 <- data.frame(table(mdg2005$Major_Topic)) %>%
  subset(select = Freq)
mdg05 <- data.frame(t(mdg05))
table(mdg2009$Major_Topic)
mdg09 <- data.frame(table(mdg2009$Major_Topic)) %>%
  subset(select = Freq)
mdg09 <- data.frame(t(mdg09))
mdg09 <- rename(mdg09, X10 = X9, X11 = X10,
                X12 = X11, X13 = X12, X14 = X13, 
                X16 = X14, X17 = X15, X18 = X16,
                X19 = X17, X21 = X18)
mdg09$X9 <- 0
mdg09$X15 <- 0
mdg09$X20 <- 0

mdg13 <- data.frame(table(mdg2013$Major_Topic)) %>%
  subset(select = Freq)
mdg13 <- data.frame(t(mdg13))

mdg17 <- data.frame(table(mdg2017$Major_Topic)) %>%
  subset(select = Freq)
mdg17 <- data.frame(t(mdg17))

mdg21 <- data.frame(table(mdg2021$Major_Topic)) %>%
  subset(select = Freq)
mdg21 <- data.frame(t(mdg21))

v85 <- data.frame(table(v1985$Major_Topic)) %>%
  subset(select = Freq)
v85 <- data.frame(t(v85))

v89 <- data.frame(table(v1989$Major_Topic)) %>%
  subset(select = Freq)
v89 <- data.frame(t(v89))

v93 <- data.frame(table(v1993$Major_Topic)) %>%
  subset(select = Freq)
v93 <- data.frame(t(v93))

v97 <- data.frame(table(v1997$Major_Topic)) %>%
  subset(select = Freq)
v97 <- data.frame(t(v97))

v01 <- data.frame(table(v2001$Major_Topic)) %>%
  subset(select = Freq)
v01 <- data.frame(t(v01))

v05 <- data.frame(table(v2005$Major_Topic)) %>%
  subset(select = Freq)
v05 <- data.frame(t(v05))

v09 <- data.frame(table(v2009$Major_Topic)) %>%
  subset(select = Freq)
v09 <- data.frame(t(v09))

v13 <- data.frame(table(v2013$Major_Topic)) %>%
  subset(select = Freq)
v13 <- data.frame(t(v13))

v17 <- data.frame(table(v2017$Major_Topic)) %>%
  subset(select = Freq)
v17 <- data.frame(t(v17))

v21 <- data.frame(table(v2021$Major_Topic)) %>%
  subset(select = Freq)
v21 <- data.frame(t(v21))

kp01 <- data.frame(table(kp2001$Major_Topic)) %>%
  subset(select = Freq)
kp01 <- data.frame(t(kp01))

kp05 <- data.frame(table(kp2005$Major_Topic)) %>%
  subset(select = Freq)
kp05 <- data.frame(t(kp05))
kp09 <- data.frame(table(kp2009$Major_Topic)) %>%
  subset(select = Freq)
kp09 <- data.frame(t(kp09))
kp13 <- data.frame(table(kp2013$Major_Topic)) %>%
  subset(select = Freq)
kp13 <- data.frame(t(kp13))
kp17 <- data.frame(table(kp2017$Major_Topic)) %>%
  subset(select = Freq)
kp17 <- data.frame(t(kp17))
kp21 <- data.frame(table(kp2021$Major_Topic)) %>%
  subset(select = Freq)
kp21 <- data.frame(t(kp21))

h97 <- data.frame(table(h1997$Major_Topic)) %>%
  subset(select = Freq)
h97 <- data.frame(t(h97))

h01 <- data.frame(table(h2001$Major_Topic)) %>%
  subset(select = Freq)
h01 <- data.frame(t(h01))

h05 <- data.frame(table(h2005$Major_Topic)) %>%
  subset(select = Freq)
h05 <- data.frame(t(h05))

h09 <- data.frame(table(h2009$Major_Topic)) %>%
  subset(select = Freq)
h09 <- data.frame(t(h09))

h13 <- data.frame(table(h2013$Major_Topic)) %>%
  subset(select = Freq)
h13 <- data.frame(t(h13))

h17 <- data.frame(table(h2017$Major_Topic)) %>%
  subset(select = Freq)
h17 <- data.frame(t(h17))

h21 <- data.frame(table(h2021$Major_Topic)) %>%
  subset(select = Freq)
h21 <- data.frame(t(h21))

## Merging the dataframes

apAtU <- full_join(ap85,ap89)
apAtU <- full_join(apAtU,ap93)
apAtU <- full_join(apAtU,ap97)
apAtU <- full_join(apAtU,ap01)
apAtU <- full_join(apAtU,ap05)
apAtU <- full_join(apAtU,ap09)
apAtU <- full_join(apAtU,ap13)
apAtU <- full_join(apAtU,ap17)
apAtU <- full_join(apAtU,ap21)

frpAtu <- full_join(frp85,frp89)
frpAtu <- full_join(frpAtu,frp93)
frpAtu <- full_join(frpAtu,frp97)
frpAtu <- full_join(frpAtu,frp01)
frpAtu <- full_join(frpAtu,frp05)
frpAtu <- full_join(frpAtu,frp09)
frpAtu <- full_join(frpAtu,frp13)
frpAtu <- full_join(frpAtu,frp17)
frpAtu <- full_join(frpAtu,frp21)

mdgAtu <- full_join(mdg97,mdg93)
mdgAtu <- full_join(mdgAtu,mdg01)
mdgAtu <- full_join(mdgAtu,mdg05)
mdgAtu <- full_join(mdgAtu,mdg09)
mdgAtu <- full_join(mdgAtu,mdg13)
mdgAtu <- full_join(mdgAtu,mdg17)
mdgAtu <- full_join(mdgAtu,mdg21)

vAtu <- full_join(v85,v89)
vAtu <- full_join(vAtu,v93)
vAtu <- full_join(vAtu,v97)
vAtu <- full_join(vAtu,v01)
vAtu <- full_join(vAtu,v05)
vAtu <- full_join(vAtu,v09)
vAtu <- full_join(vAtu,v13)
vAtu <- full_join(vAtu,v17)
vAtu <- full_join(vAtu,v21)

kpAtu <- full_join(kp01, kp05)
kpAtu <- full_join(kpAtu, kp09)
kpAtu <- full_join(kpAtu, kp13)
kpAtu <- full_join(kpAtu, kp17)
kpAtu <- full_join(kpAtu, kp21)

hAtu <- full_join(h97,h01)
hAtu <- full_join(hAtu,h05)
hAtu <- full_join(hAtu,h09)
hAtu <- full_join(hAtu,h13)
hAtu <- full_join(hAtu,h17)
hAtu <- full_join(hAtu,h21)

## Renaming

apAtU <- rename(apAtU, Macroeconomics = X1, 'Civil rights' = X2,
                Health = X3, 'Agriculture and fishing' = X4, Labour = X5,
                Education = X6, Environment = X7, 
                Energy = X8, Immigration = X9, Traffic = X10,
                'Legal affairs' = X11, 'Social policy' = X12, Housing = X13,
                'Commercial policy' = X14, Defence = X15,
                Technology = X16,
                'Foreign trade' = X17, 'Foreign policy' = X18, Government = X19,
                'Public lands' = X20, Culture = X21)

frpAtu <- rename(frpAtu, Macroeconomics = X1, 'Civil rights' = X2,
                 Health = X3, 'Agriculture and fishing' = X4, Labour = X5,
                 Education = X6, Environment = X7, 
                 Energy = X8, Immigration = X9, Traffic = X10,
                 'Legal affairs' = X11, 'Social policy' = X12, Housing = X13,
                 'Commercial policy' = X14,
                 Defence = X15, Technology = X16,
                 'Foreign trade' = X17, 'Foreign policy' = X18, Government = X19,
                 'Public lands' = X20, Culture = X21)

mdgAtu <- rename(mdgAtu, Macroeconomics = X1, 'Civil rights' = X2,
                 Health = X3, 'Agriculture and fishing' = X4, Labour = X5,
                 Education = X6, Environment = X7, 
                 Energy = X8, Immigration = X9, Traffic = X10,
                 'Legal affairs' = X11, 'Social policy' = X12, Housing = X13,
                 'Commercial policy' = X14, Defence = X15,
                 Technology = X16,
                 'Foreign trade' = X17, 'Foreign policy' = X18, Government = X19,
                 'Public lands' = X20, Culture = X21)

vAtu <- rename(vAtu, Macroeconomics = X1, 'Civil rights' = X2,
               Health = X3, 'Agriculture and fishing' = X4, Labour = X5,
               Education = X6, Environment = X7, 
               Energy = X8, Immigration = X9, Traffic = X10,
               'Legal affairs' = X11, 'Social policy' = X12, Housing = X13,
               'Commercial policy' = X14, Defence = X15,
               Technology = X16,
               'Foreign trade' = X17, 'Foreign policy' = X18, Government = X19,
               'Public lands' = X20, Culture = X21)

kpAtu <- rename(kpAtu, Macroeconomics = X1, 'Civil rights' = X2,
                Health = X3, 'Agriculture and fishing' = X4, Labour = X5,
                Education = X6, Environment = X7, 
                Energy = X8, Immigration = X9, Traffic = X10,
                'Legal affairs' = X11, 'Social policy' = X12, Housing = X13,
                'Commercial policy' = X14, Defence = X15,
                Technology = X16,
                'Foreign trade' = X17, 'Foreign policy' = X18, Government = X19,
                'Public lands' = X20, Culture = X21)

hAtu <- rename(hAtu, Macroeconomics = X1, 'Civil rights' = X2,
                Health = X3, 'Agriculture and fishing' = X4, Labour = X5,
                Education = X6, Environment = X7, 
                Energy = X8, Immigration = X9, Traffic = X10,
                'Legal affairs' = X11, 'Social policy' = X12, Housing = X13,
                'Commercial policy' = X14, Defence = X15,
                Technology = X16,
                'Foreign trade' = X17, 'Foreign policy' = X18, Government = X19,
                'Public lands' = X20, Culture = X21)

## Adding a variable for election year

year <- c(1985,1989,1993,1997,2001,2005,2009,
          2013,2017,2021)

apAtU <- cbind(year,apAtU)
frpAtu <- cbind(year,frpAtu)
vAtu <- cbind(year,vAtu)

year <- c(1997,1993,2001,2005,2009,2013,2017,2021)
mdgAtu <- cbind(year,mdgAtu)
mdgAtu <- mdgAtu[order(year), ]

year <- c(2001,2005,2009,2013,2017,2021)
kpAtu <- cbind(year, kpAtu)

year <- c(1997,2001,2005,2009,2013,2017,2021)
hAtu <- cbind(year,hAtu)

## Adding party names as a variable

party <- c("AP","AP","AP","AP","AP","AP","AP","AP","AP","AP")
apAtU <- cbind(party,apAtU)

party <- c("FRP","FRP","FRP","FRP","FRP","FRP","FRP","FRP",
           "FRP","FRP")
frpAtu <- cbind(party,frpAtu)

party <- c("MDG","MDG","MDG","MDG","MDG","MDG","MDG","MDG")
mdgAtu <- cbind(party,mdgAtu)

party <- c("V","V","V","V","V","V","V","V","V","V")
vAtu <- cbind(party,vAtu)

party <- c("KP","KP","KP","KP","KP","KP")
kpAtu <- cbind(party, kpAtu)

party <- "H"
hAtu <- cbind(party,hAtu)

Atu <- full_join(apAtU,frpAtu)
Atu <- full_join(Atu,mdgAtu)
Atu <- full_join(Atu,vAtu)
Atu <- full_join(Atu, kpAtu)
Atu <- full_join(Atu,hAtu)

#### Creating dataframes of the proportional values ####

apfreq85 <- as.data.frame(prop.table(table(ap1985$Major_Topic))) %>%
  subset(select = Freq)
apfreq85 <-as.data.frame(t(apfreq85))
apfreq89 <- as.data.frame(prop.table(table(ap1989$Major_Topic))) %>%
  subset(select = Freq)
apfreq89 <-as.data.frame(t(apfreq89))
apfreq93 <- as.data.frame(prop.table(table(ap1993$Major_Topic))) %>%
  subset(select = Freq)
apfreq93 <-as.data.frame(t(apfreq93))
apfreq97 <- as.data.frame(prop.table(table(ap1997$Major_Topic))) %>%
  subset(select = Freq)
apfreq97 <-as.data.frame(t(apfreq97))
apfreq01 <- as.data.frame(prop.table(table(ap2001$Major_Topic))) %>%
  subset(select = Freq)
apfreq01 <-as.data.frame(t(apfreq01))
apfreq05 <- as.data.frame(prop.table(table(ap2005$Major_Topic))) %>%
  subset(select = Freq)
apfreq05 <-as.data.frame(t(apfreq05))
apfreq09 <- as.data.frame(prop.table(table(ap2009$Major_Topic))) %>%
  subset(select = Freq)
apfreq09 <-as.data.frame(t(apfreq09))
apfreq13 <- as.data.frame(prop.table(table(ap2013$Major_Topic))) %>%
  subset(select = Freq)
apfreq13 <-as.data.frame(t(apfreq13))
apfreq17 <- as.data.frame(prop.table(table(ap2017$Major_Topic))) %>%
  subset(select = Freq)
apfreq17 <-as.data.frame(t(apfreq17))
apfreq21 <- as.data.frame(prop.table(table(ap2021$Major_Topic))) %>%
  subset(select = Freq)
apfreq21 <-as.data.frame(t(apfreq21))

apfreq <- full_join(apfreq85,apfreq89)
apfreq <- full_join(apfreq,apfreq93)
apfreq <- full_join(apfreq,apfreq97)
apfreq <- full_join(apfreq,apfreq01)
apfreq <- full_join(apfreq,apfreq05)
apfreq <- full_join(apfreq,apfreq09)
apfreq <- full_join(apfreq,apfreq13)
apfreq <- full_join(apfreq,apfreq17)
apfreq <- full_join(apfreq,apfreq21)

apfrequ <- rename(apfreq, Macroeconomics = `1`, 'Civil rights' = `2`,
                  Health = `3`, 'Agriculture and fishing'  = `4`, Labour = `5`,
                  Education = `6`, Environment = `7`, 
                  Energy = `8`, Immigration = `9`, Traffic = `10`,
                  'Legal affairs' = `11`, 'Social policy' = `12`, Housing = `13`,
                  'Commercial policy' = `14`, Defence = `15`,
                  Technology = `16`,
                  'Foreign trade' = `17`, 'Foreign policy' = `18`, Government = `19`,
                  'Public lands' = `20`, Culture = `21`)

frpfreq85 <- as.data.frame(prop.table(table(frp1985$Major_Topic))) %>%
  subset(select = Freq)
frpfreq85 <-as.data.frame(t(frpfreq85))
frpfreq89 <- as.data.frame(prop.table(table(frp1989$Major_Topic))) %>%
  subset(select = Freq)
frpfreq89 <-as.data.frame(t(frpfreq89))
frpfreq93 <- as.data.frame(prop.table(table(frp1993$Major_Topic))) %>%
  subset(select = Freq)
frpfreq93 <-as.data.frame(t(frpfreq93))
frpfreq97 <- as.data.frame(prop.table(table(frp1997$Major_Topic))) %>%
  subset(select = Freq)
frpfreq97 <-as.data.frame(t(frpfreq97))
frpfreq01 <- as.data.frame(prop.table(table(frp2001$Major_Topic))) %>%
  subset(select = Freq)
frpfreq01 <-as.data.frame(t(frpfreq01))
frpfreq05 <- as.data.frame(prop.table(table(frp2005$Major_Topic))) %>%
  subset(select = Freq)
frpfreq05 <-as.data.frame(t(frpfreq05))
frpfreq09 <- as.data.frame(prop.table(table(frp2009$Major_Topic))) %>%
  subset(select = Freq)
frpfreq09 <-as.data.frame(t(frpfreq09))
frpfreq13 <- as.data.frame(prop.table(table(frp2013$Major_Topic))) %>%
  subset(select = Freq)
frpfreq13 <-as.data.frame(t(frpfreq13))
frpfreq17 <- as.data.frame(prop.table(table(frp2017$Major_Topic))) %>%
  subset(select = Freq)
frpfreq17 <-as.data.frame(t(frpfreq17))
frpfreq21 <- as.data.frame(prop.table(table(frp2021$Major_Topic))) %>%
  subset(select = Freq)
frpfreq21 <-as.data.frame(t(frpfreq21))

frpfreq <- full_join(frpfreq85,frpfreq89)
frpfreq <- full_join(frpfreq,frpfreq93)
frpfreq <- full_join(frpfreq,frpfreq97)
frpfreq <- full_join(frpfreq,frpfreq01)
frpfreq <- full_join(frpfreq,frpfreq05)
frpfreq <- full_join(frpfreq,frpfreq09)
frpfreq <- full_join(frpfreq,frpfreq13)
frpfreq <- full_join(frpfreq,frpfreq17)
frpfreq <- full_join(frpfreq,frpfreq21)

frpfrequ <- rename(frpfreq, Macroeconomics = `1`, 'Civil rights' = `2`,
                   Health = `3`, 'Agriculture and fishing' = `4`, Labour = `5`,
                   Education = `6`, Environment = `7`, 
                   Energy = `8`, Immigration = `9`, Traffic = `10`,
                   'Legal affairs' = `11`, 'Social policy' = `12`, Housing = `13`,
                   'Commercial policy' = `14`, Defence = `15`,
                   Technology = `16`,
                   'Foreign trade' = `17`, 'Foreign policy' = `18`, Government = `19`,
                   'Public lands' = `20`, Culture = `21`)

mdgfreq93 <- as.data.frame(prop.table(table(mdg1993$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq93 <-as.data.frame(t(mdgfreq93))
mdgfreq93 <- rename(mdgfreq93, `18` = `16`, `20` = `17`)
mdgfreq93$`16` <- 0
mdgfreq93$`17` <- 0
mdgfreq93$`19` <- 0
mdgfreq93$`21` <- 0
mdgfreq97 <- as.data.frame(prop.table(table(mdg1997$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq97 <-as.data.frame(t(mdgfreq97))
mdgfreq01 <- as.data.frame(prop.table(table(mdg2001$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq01 <-as.data.frame(t(mdgfreq01))
mdgfreq05 <- as.data.frame(prop.table(table(mdg2005$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq05 <-as.data.frame(t(mdgfreq05))
mdgfreq09 <- as.data.frame(prop.table(table(mdg2009$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq09 <-as.data.frame(t(mdgfreq09))
mdgfreq09 <- rename(mdgfreq09, `10` = `9`, `11` = `10`,
                    `12` = `11`, `13` = `12`, `14` = `13`, 
                    `16` = `14`, `17` = `15`, `18` = `16`,
                    `19` = `17`, `21` = `18`)
mdgfreq09$`9` <- 0
mdgfreq09$`15` <- 0
mdgfreq09$`20` <- 0
mdgfreq13 <- as.data.frame(prop.table(table(mdg2013$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq13 <-as.data.frame(t(mdgfreq13))
mdgfreq17 <- as.data.frame(prop.table(table(mdg2017$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq17 <-as.data.frame(t(mdgfreq17))
mdgfreq21 <- as.data.frame(prop.table(table(mdg2021$Major_Topic))) %>%
  subset(select = Freq)
mdgfreq21 <-as.data.frame(t(mdgfreq21))

table(mdg1993$Major_Topic)

mdgfreq <- full_join(mdgfreq97,mdgfreq93)
mdgfreq <- full_join(mdgfreq,mdgfreq01)
mdgfreq <- full_join(mdgfreq,mdgfreq05)
mdgfreq <- full_join(mdgfreq,mdgfreq09)
mdgfreq <- full_join(mdgfreq,mdgfreq13)
mdgfreq <- full_join(mdgfreq,mdgfreq17)
mdgfreq <- full_join(mdgfreq,mdgfreq21)

#### Rename mdg major topics ####

mdgfrequ <- rename(mdgfreq, Macroeconomics = `1`, 'Civil rights' = `2`,
                   Health = `3`, 'Agriculture and fishing' = `4`, Labour = `5`,
                   Education = `6`, Environment = `7`, 
                   Energy = `8`, Immigration = `9`, Traffic = `10`,
                   'Legal affairs' = `11`, 'Social policy' = `12`, Housing = `13`,
                   'Commercial policy' = `14`, Defence = `15`,
                   Technology = `16`,
                   'Foreign trade' = `17`, 'Foreign policy' = `18`, Government = `19`,
                   'Public lands' = `20`, Culture = `21`)

year <- c(1997,1993,2001,2005,2009,2013,2017,2021)
mdgfrequ <- as.data.frame(cbind(year,mdgfrequ))
mdgfrequ <- mdgfrequ[order(year), ]

vfreq85 <- as.data.frame(prop.table(table(v1985$Major_Topic))) %>%
  subset(select = Freq)
vfreq85 <-as.data.frame(t(vfreq85))
vfreq89 <- as.data.frame(prop.table(table(v1989$Major_Topic))) %>%
  subset(select = Freq)
vfreq89 <-as.data.frame(t(vfreq89))
vfreq93 <- as.data.frame(prop.table(table(v1993$Major_Topic))) %>%
  subset(select = Freq)
vfreq93 <-as.data.frame(t(vfreq93))
vfreq97 <- as.data.frame(prop.table(table(v1997$Major_Topic))) %>%
  subset(select = Freq)
vfreq97 <-as.data.frame(t(vfreq97))
vfreq01 <- as.data.frame(prop.table(table(v2001$Major_Topic))) %>%
  subset(select = Freq)
vfreq01 <-as.data.frame(t(vfreq01))
vfreq05 <- as.data.frame(prop.table(table(v2005$Major_Topic))) %>%
  subset(select = Freq)
vfreq05 <-as.data.frame(t(vfreq05))
vfreq09 <- as.data.frame(prop.table(table(v2009$Major_Topic))) %>%
  subset(select = Freq)
vfreq09 <-as.data.frame(t(vfreq09))
vfreq13 <- as.data.frame(prop.table(table(v2013$Major_Topic))) %>%
  subset(select = Freq)
vfreq13 <-as.data.frame(t(vfreq13))
vfreq17 <- as.data.frame(prop.table(table(v2017$Major_Topic))) %>%
  subset(select = Freq)
vfreq17 <-as.data.frame(t(vfreq17))
vfreq21 <- as.data.frame(prop.table(table(v2021$Major_Topic))) %>%
  subset(select = Freq)
vfreq21 <-as.data.frame(t(vfreq21))

vfreq <- full_join(vfreq85,vfreq89)
vfreq <- full_join(vfreq,vfreq93)
vfreq <- full_join(vfreq,vfreq97)
vfreq <- full_join(vfreq,vfreq01)
vfreq <- full_join(vfreq,vfreq05)
vfreq <- full_join(vfreq,vfreq09)
vfreq <- full_join(vfreq,vfreq13)
vfreq <- full_join(vfreq,vfreq17)
vfreq <- full_join(vfreq,vfreq21)

#### Creating a dataframe ####

setwd("C:/Users/Eier/OneDrive/Dokumenter/Notater_til_Master")
write.xlsx(all_frequ, "C:/Users/Eier/OneDrive/Dokumenter/Notater_til_Master/Proportional data.xlsx")

#### Rename major topics venstre ####

vfrequ <- rename(vfreq, Macroeconomics = `1`, 'Civil rights' = `2`,
                 Health = `3`, 'Agriculture and fishing' = `4`, Labour = `5`,
                 Education = `6`, Environment = `7`, 
                 Energy = `8`, Immigration = `9`, Traffic = `10`,
                 'Legal affairs' = `11`, 'Social policy' = `12`, Housing = `13`,
                 'Commercial policy' = `14`, Defence = `15`,
                 Technology = `16`,
                 'Foreign trade' = `17`, 'Foreign policy' = `18`, Government = `19`,
                 'Public lands' = `20`, Culture = `21`)

kpfreq01 <- as.data.frame(prop.table(table(kp2001$Major_Topic))) %>%
  subset(select = Freq)
kpfreq01 <-as.data.frame(t(kpfreq01))
kpfreq05 <- as.data.frame(prop.table(table(kp2005$Major_Topic))) %>%
  subset(select = Freq)
kpfreq05 <-as.data.frame(t(kpfreq05))
kpfreq09 <- as.data.frame(prop.table(table(kp2009$Major_Topic))) %>%
  subset(select = Freq)
kpfreq09 <-as.data.frame(t(kpfreq09))
kpfreq13 <- as.data.frame(prop.table(table(kp2013$Major_Topic))) %>%
  subset(select = Freq)
kpfreq13 <-as.data.frame(t(kpfreq13))
kpfreq17 <- as.data.frame(prop.table(table(kp2017$Major_Topic))) %>%
  subset(select = Freq)
kpfreq17 <-as.data.frame(t(kpfreq17))
kpfreq21 <- as.data.frame(prop.table(table(kp2021$Major_Topic))) %>%
  subset(select = Freq)
kpfreq21 <-as.data.frame(t(kpfreq21))

kpfreq <- full_join(kpfreq01,kpfreq05)
kpfreq <- full_join(kpfreq,kpfreq09)
kpfreq <- full_join(kpfreq,kpfreq13)
kpfreq <- full_join(kpfreq,kpfreq17)
kpfreq <- full_join(kpfreq,kpfreq21)

kpfrequ <- rename(kpfreq, Macroeconomics = `1`, 'Civil rights' = `2`,
                  Health = `3`, 'Agriculture and fishing' = `4`, Labour = `5`,
                  Education = `6`, Environment = `7`, 
                  Energy = `8`, Immigration = `9`, Traffic = `10`,
                  'Legal affairs' = `11`, 'Social policy' = `12`, Housing = `13`,
                  'Commercial policy' = `14`, Defence = `15`,
                  Technology = `16`,
                  'Foreign trade' = `17`, 'Foreign policy' = `18`, Government = `19`,
                  'Public lands' = `20`, Culture = `21`)

year <- c(2001,2005,2009,2013,2017,2021)
kpfrequ <- cbind(year,kpfrequ)

hfreq97 <- as.data.frame(prop.table(table(h1997$Major_Topic))) %>%
  subset(select = Freq)
hfreq97 <-as.data.frame(t(hfreq97))

hfreq01 <- as.data.frame(prop.table(table(h2001$Major_Topic))) %>%
  subset(select = Freq)
hfreq01 <-as.data.frame(t(hfreq01))

hfreq05 <- as.data.frame(prop.table(table(h2005$Major_Topic))) %>%
  subset(select = Freq)
hfreq05 <-as.data.frame(t(hfreq05))

hfreq09 <- as.data.frame(prop.table(table(h2009$Major_Topic))) %>%
  subset(select = Freq)
hfreq09 <-as.data.frame(t(hfreq09))

hfreq13 <- as.data.frame(prop.table(table(h2013$Major_Topic))) %>%
  subset(select = Freq)
hfreq13 <-as.data.frame(t(hfreq13))

hfreq17 <- as.data.frame(prop.table(table(h2017$Major_Topic))) %>%
  subset(select = Freq)
hfreq17 <-as.data.frame(t(hfreq17))

hfreq21 <- as.data.frame(prop.table(table(h2021$Major_Topic))) %>%
  subset(select = Freq)
hfreq21 <-as.data.frame(t(hfreq21))

hfreq <- full_join(hfreq97,hfreq01)
hfreq <- full_join(hfreq,hfreq05)
hfreq <- full_join(hfreq,hfreq09)
hfreq <- full_join(hfreq,hfreq13)
hfreq <- full_join(hfreq,hfreq17)
hfreq <- full_join(hfreq,hfreq21)

hfrequ <- rename(hfreq, Macroeconomics = `1`, 'Civil rights' = `2`,
                  Health = `3`, 'Agriculture and fishing' = `4`, Labour = `5`,
                  Education = `6`, Environment = `7`, 
                  Energy = `8`, Immigration = `9`, Traffic = `10`,
                  'Legal affairs' = `11`, 'Social policy' = `12`, Housing = `13`,
                  'Commercial policy' = `14`, Defence = `15`,
                  Technology = `16`,
                  'Foreign trade' = `17`, 'Foreign policy' = `18`, Government = `19`,
                  'Public lands' = `20`, Culture = `21`)

year <- c(1997,2001,2005,2009,2013,2017,2021)
hfrequ <- cbind(year,hfrequ)

year <- c(1985,1989,1993,1997,2001,2005,2009,
          2013,2017,2021)
vfrequ <- as.data.frame(cbind(year,vfrequ))

apfrequ <- cbind(year,apfrequ)
frpfrequ <- cbind(year,frpfrequ)

party <- c("AP","AP","AP","AP","AP","AP","AP","AP","AP","AP")
apfrequ <- cbind(party,apfrequ)

party <- c("FRP","FRP","FRP","FRP","FRP","FRP","FRP","FRP",
           "FRP","FRP")
frpfrequ <- cbind(party,frpfrequ)

party <- c("MDG","MDG","MDG","MDG","MDG","MDG","MDG","MDG")
mdgfrequ <- cbind(party,mdgfrequ)

party <- c("V","V","V","V","V","V","V","V","V","V")
vfrequ <- cbind(party,vfrequ)

party <- c("KP","KP","KP","KP","KP","KP")
kpfrequ <- cbind(party,kpfrequ)

party <- "H"
hfrequ <- cbind(party,hfrequ)

all_frequ <- full_join(apfrequ,frpfrequ)
all_frequ <- full_join(all_frequ,mdgfrequ)
all_frequ <- full_join(all_frequ,vfrequ)
all_frequ <- full_join(all_frequ,kpfrequ)
all_frequ <- full_join(all_frequ,hfrequ)

#### Examining the descriptive statistics

summary(apfrequ)
summary(hfrequ)
summary(frpfrequ)
summary(mdgfrequ)
summary(vfrequ)
summary(kpfrequ)
#### Examining the development of issue attention ####

ggplot(Atu, aes(year, Macroeconomics, colour = party)) +
  geom_point(aes()) +
  geom_line() +
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "H" = "royalblue2"))

ggplot(all_frequ, aes(year, Macroeconomics, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election years",
       title = "Graph 9.3.1. The proportion of macroeconomics in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0.02,0.12),
                     breaks = seq(0.02,0.12,0.02)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_frequ, aes(year, `Civil rights`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "9.3.2. The proportion of civil rights in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  theme(plot.title = element_text(hjust = 0.5) )

ggplot(all_frequ, aes(year, Health, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "9.3.3. The proportion of health in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_frequ, aes(year, `Agriculture and fishing`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 9.3.4. The proportion of agriculture and fishing in manifestos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0.02,0.18),
                     breaks = seq(0.02,0.18,0.04))

ggplot(all_frequ, aes(year, Labour, colour = party)) +
  geom_point() +
  theme_classic()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "royalblue2")) +
  geom_line()

ggplot(all_frequ, aes(year, Education, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "blue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 9.3.5. The proportion of education in manifestos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0,0.15))

ggplot(all_frequ, aes(year, Environment, colour = party)) +
  geom_point() +
  theme_classic()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  scale_y_continuous(breaks = seq(0,0.3,0.05))

ggplot(all_frequ, aes(year, Energy, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 9.3.6. The proportion of energy in manifestos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1985,2021,4))

ggplot(all_frequ, aes(year, Immigration, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.7. The proportion of immigration in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4))

ggplot(all_frequ, aes(year, Traffic
                      , colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.8. The proportion of traffic in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4))

ggplot(all_frequ, aes(year, `Legal affairs`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 9.3.9, The proportion of legal affairs in manifestos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0,0.1))

ggplot(all_frequ, aes(year, `Social policy`, colour = party)) +
  geom_point() +
  theme_classic()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line()

ggplot(all_frequ, aes(year, Housing, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 9.3.10. The proportion of housing in manifestos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0,0.075),
                     breaks = seq(0,0.075,0.025))

ggplot(all_frequ, aes(year, `Commercial policy`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.11. The proportion of commercial policy in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0.01,0.09),
                     breaks = seq(0.01,0.09,0.02))

ggplot(all_frequ, aes(year, Defence, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.12. The proportion of defence in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0,0.075),
                     breaks = seq(0,0.075,0.025))

ggplot(all_frequ, aes(year, Technology, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 9.3.13. The proportion of technology in manifestos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(breaks = seq(0,0.05,0.025))

ggplot(all_frequ, aes(year, `Foreign trade`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.14. The proportion of foreign trade in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(breaks = seq(0,0.04,0.01),
                     limits = c(0,0.04))

ggplot(all_frequ, aes(year, `Foreign policy`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.15. The proportion of foreign policy in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0,0.2),
                     breaks = seq(0,0.2,0.05))

ggplot(all_frequ, aes(year, Government, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.16. The proportion of government affairs in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4))

ggplot(all_frequ, aes(year, `Public lands`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.17. The proportion of public lands in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4))

ggplot(all_frequ, aes(year, Culture, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       title = "Graph 9.3.18. The proportion of culture in manifestos") +
  scale_x_continuous(breaks = seq(1985,2021,4)) +
  scale_y_continuous(limits = c(0,0.06),
                     breaks = seq(0,0.06,0.02))

#### Measuring the length of manifestos ####

all_frequ$count <- as.numeric(c(count(ap1985),
                                count(ap1989),
                                count(ap1993),
                                count(ap1997),
                                count(ap2001),
                                count(ap2005),
                                count(ap2009),
                                count(ap2013),
                                count(ap2017),
                                count(ap2021),
                                count(frp1985),
                                count(frp1989),
                                count(frp1993),
                                count(frp1997),
                                count(frp2001),
                                count(frp2005),
                                count(frp2009),
                                count(frp2013),
                                count(frp2017),
                                count(frp2021),
                                count(mdg1993),
                                count(mdg1997),
                                count(mdg2001),
                                count(mdg2005),
                                count(mdg2009),
                                count(mdg2013),
                                count(mdg2017),
                                count(mdg2021),
                                count(v1985),
                                count(v1989),
                                count(v1993),
                                count(v1997),
                                count(v2001),
                                count(v2005),
                                count(v2009),
                                count(v2013),
                                count(v2017),
                                count(v2021),
                                count(kp2001),
                                count(kp2005),
                                count(kp2009),
                                count(kp2013),
                                count(kp2017),
                                count(kp2021),
                                count(h1997),
                                count(h2001),
                                count(h2005),
                                count(h2009),
                                count(h2013),
                                count(h2017),
                                count(h2021)))

## Creating a graph showing the number of 
## sentences in each manifestos

ggplot(all_frequ, aes(year, count, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       y = "Number of sentences",
       title = "Graph 5.1. The number of sentences in manifestos") +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(plot.title = element_text(hjust = 0.5)) 

## Examining the number of sentences in 
## manifestos further

mean(all_frequ$count)
sum(all_frequ$count)

## Measuring the number of irrelevant sentences                  

ap <- full_join(ap1985,ap1989)
ap <- full_join(ap,ap1993)
ap <- full_join(ap,ap1997)
ap <- full_join(ap,ap2001)
ap <- full_join(ap,ap2005)
ap <- full_join(ap,ap2009)
ap <- full_join(ap,ap2013)
ap <- full_join(ap,ap2017)
ap <- full_join(ap,ap2021)

frp <- full_join(frp1985,frp1989)
frp <- full_join(frp,frp1993)
frp <- full_join(frp,frp1997)
frp <- full_join(frp,frp2001)
frp <- full_join(frp,frp2005)
frp <- full_join(frp,frp2009)
frp <- full_join(frp,frp2013)
frp <- full_join(frp,frp2017)
frp <- full_join(frp,frp2021)

mdg <- full_join(mdg1993,mdg1997)
mdg <- full_join(mdg,mdg2001)
mdg <- full_join(mdg,mdg2005)
mdg <- full_join(mdg,mdg2009)
mdg <- full_join(mdg,mdg2013)
mdg <- full_join(mdg,mdg2017)
mdg <- full_join(mdg,mdg2021)

v <- full_join(v1985,v1989)
v <- full_join(v,v1993)
v <- full_join(v,v1997)
v <- full_join(v,v2001)
v <- full_join(v,v2005)
v <- full_join(v,v2009)
v <- full_join(v,v2013)
v <- full_join(v,v2017)
v <- full_join(v,v2021)

kp <- full_join(kp2001,kp2005)
kp <- full_join(kp,kp2009)
kp <- full_join(kp,kp2013)
kp <- full_join(kp,kp2017)
kp <- full_join(kp,kp2021)

h <- full_join(h1997,h2001)
h <- full_join(h,h2005)
h <- full_join(h,h2009)
h <- full_join(h,h2013)
h <- full_join(h,h2017)
h <- full_join(h,h2021)

all <- full_join(ap,frp)
all <- full_join(all,mdg)
all <- full_join(all,v)
all <- full_join(all,kp)
all <- full_join(all,h)

table(all$Norway_Code[all$Norway_Code == 2999])
95708 - 3503

write.xlsx(all, "C:/Users/Eier/OneDrive/Dokumenter/Notater_til_Master/Norwegian policy data.xlsx")
#### Graphs used in the thesis ####

## Creating a graph showing the development of
## the attention to labour

ggplot(all_frequ, aes(year, Labour, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 6.2. The proportion of labour policy in manifestos") +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(plot.title = element_text(hjust = 0.5)) 

## Creating a graph showing the development of
## the attention to social policy

ggplot(all_frequ, aes(year, `Social policy`, colour = party)) +
  geom_point() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  geom_line() +
  labs(x = "Election year",
       title = "Graph 6.3. The proportion of social policies in manifestos") +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(plot.title = element_text(hjust = 0.5))

## Examining the descriptive statistics of the
## attention to labour

summary(all_frequ$Labour[all_frequ$party == "AP"])
summary(all_frequ$Labour[all_frequ$party == "FRP"])
summary(all_frequ$Labour[all_frequ$party == "MDG"])
summary(all_frequ$Labour[all_frequ$party == "V"])
summary(all_frequ$Labour[all_frequ$party == "KP"])
 
## Descriptive statistics of frp's attention to
## immigration
min(frpfrequ$Immigration)
mean(all_frequ$Immigration[all_frequ$year == 1985])

## Creating a graph showing the development of
## FrP's attention to immigration and legal 
## affairs

ggplot(frpfrequ, aes(year)) +
  geom_line(aes(y = Immigration, colour = "Immigration")) +
  geom_line(aes(y = `Legal affairs`, colour = "Legal affairs")) +
  theme_bw() +
  labs(x = "Election year", 
       y = "Proportion of attention",
       colour = "Major topic",
       title = "Graph 6.6. The Progress party's attention to immigration and social affairs over time") +
  geom_point(aes(y = Immigration, colour = "Immigration")) +
  geom_point(aes(y = `Legal affairs`, colour = "Legal affairs")) +
  scale_fill_discrete(labels = c("Immigration", "Legal affairs")) +
  scale_y_continuous(breaks = c(0.01,0.02,0.03,0.04,0.05,0.06,
                                0.07,0.08,0.09,0.1)) +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021))

## Creating a graph showing the development of 
## the attention to the environment

ggplot(all_frequ, aes(year, Environment, colour = party)) +
  geom_point() +
  geom_line() +
  theme_bw()+
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  labs(x = "Election year",
       title = "Graph 6.9. The proportion of environmental policy in manifestos") +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,0.3,0.05),
                     limits = c(0.,0.3))
  
#### Creating area plots ####

## Turning the data into long format

freq_long <- all_frequ %>%
pivot_longer(!1:2 & !24, names_to = "Topics", 
               values_to = "Attention")

freq_long$Topics <- factor(freq_long$Topics,
                           levels = c("Macroeconomics",
                                      "Civil rights",
                                      "Health",
                                      "Agriculture and fishing",
                                      "Labour", "Education",
                                      "Environment", "Energy",
                                      "Immigration", "Traffic",
                                      "Legal affairs",
                                      "Social policy",
                                      "Housing",
                                      "Commercial policy",
                                      "Defence",
                                      "Technology",
                                      "Foreign trade",
                                      "Foreign policy", "Government",
                                      "Public lands", "Culture"))

## Examining the distribution of all parties

ggplot(freq_long, aes(year, Attention,
                      fill = Topics)) +
  geom_area(alpha = .6, size = 1, 
            colour = "black") +
  scale_fill_manual(values = scales::hue_pal()(21), breaks = c("Macroeconomics",
                                                               "Civil rights",
                                                               "Health", "Agriculture and fishing",
                                                               "Labour", "Education",
                                                               "Environment", "Energy",
                                                               "Immigration", "Traffic",
                                                               "Legal affairs", "Social policy",
                                                               "Housing", "Commercial policy",
                                                               "Defence",
                                                               "Technology",
                                                               "Foreign trade",
                                                               "Foreign policy", "Government",
                                                               "Public lands", "Culture")
                    
  ) +
  facet_wrap(vars(party)) +
  theme_bw()

## Creating an area plot showing the distribution
## of AP

freq_long %>% filter(party == "AP") %>%
  ggplot(aes(year, Attention,
             fill = Topics)) +
  geom_area(alpha = 0.6, size = 0.5, 
            colour = "black") +
  scale_fill_manual(values = scales::hue_pal()(21), breaks = c("Macroeconomics",
                                                               "Civil rights",
                                                               "Health", "Agriculture and fishing",
                                                               "Labour", "Education",
                                                               "Environment", "Energy",
                                                               "Immigration", "Traffic",
                                                               "Legal affairs", "Social policy",
                                                               "Housing", "Commercial policy",
                                                               "Defence",
                                                               "Technology",
                                                               "Foreign trade",
                                                               "Foreign policy", "Government",
                                                               "Public lands", "Culture")) +
  theme_bw() +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       y = "Issue attention",
       title = "Graph 6.1. The issue emphasis of the Labour party")

## Examining AP furhter 

apfrequ$core <- apfrequ$Labour + apfrequ$`Social policy`

ggplot(apfrequ, aes(year, core)) +
  geom_line()
summary(apfrequ)

## Creating an area plot showing the distribution
## of FrP

freq_long %>% filter(party == "FRP") %>%
  ggplot(aes(year, Attention,
             fill = Topics)) +
  geom_area(alpha = .6, size = 0.5, 
            colour = "black") +
  scale_fill_manual(values = scales::hue_pal()(21), breaks = c("Macroeconomics",
                                                               "Civil rights",
                                                               "Health", "Agriculture and fishing",
                                                               "Labour", "Education",
                                                               "Environment", "Energy",
                                                               "Immigration", "Traffic",
                                                               "Legal affairs", "Social policy",
                                                               "Housing", "Commercial policy",
                                                               "Defence",
                                                               "Technology",
                                                               "Foreign trade",
                                                               "Foreign policy", "Government",
                                                               "Public lands", "Culture")) +
  theme_bw() +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       y = "Issue attention",
       title = "Graph 6.5. The issue emphasis of the Progress party")

## Examining FrP and immigration further

summary(frpfrequ)
summary(apfrequ$Immigration)
summary(frpfrequ$Immigration)
summary(mdgfrequ$Immigration)
summary(vfrequ$Immigration)
summary(kpfrequ$Immigration)
summary(hfrequ$Immigration)

## Creating an area plot showing the distribution
## of Mdg

freq_long %>% filter(party == "MDG") %>%
  ggplot(aes(year, Attention,
             fill = Topics)) +
  geom_area(alpha = .6, size = 0.5, 
            colour = "black") +
  scale_fill_manual(values = scales::hue_pal()(21), breaks = c("Macroeconomics",
                                                               "Civil rights",
                                                               "Health", "Agriculture and fishing",
                                                               "Labour", "Education",
                                                               "Environment", "Energy",
                                                               "Immigration", "Traffic",
                                                               "Legal affairs", "Social policy",
                                                               "Housing", "Commercial policy",
                                                               "Defence",
                                                               "Technology",
                                                               "Foreign trade",
                                                               "Foreign policy", "Government",
                                                               "Public lands", "Culture")) +
  theme_bw() +
  scale_x_continuous(breaks = c(1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       y = "Issue attention",
       title = "Graph 6.8. The issue emphasis of the Green party")

## Creating an area plot showing the distribution
## of V

freq_long %>% filter(party == "V") %>%
  ggplot(aes(year, Attention,
             fill = Topics)) +
  geom_area(alpha = .6, size = 0.5, 
            colour = "black") +
  scale_fill_manual(values = scales::hue_pal()(21), breaks = c("Macroeconomics",
                                                               "Civil rights",
                                                               "Health", "Agriculture and fishing",
                                                               "Labour", "Education",
                                                               "Environment", "Energy",
                                                               "Immigration", "Traffic",
                                                               "Legal affairs", "Social policy",
                                                               "Housing", "Commercial policy",
                                                               "Defence",
                                                               "Technology",
                                                               "Foreign trade",
                                                               "Foreign policy", "Government",
                                                               "Public lands", "Culture")) +
  theme_bw() +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       y = "Issue attention",
       title = "Graph 6.10. The issue emphasis of the Liberal party")

## Examining V and some core issues futher

summary(vfrequ$Education)
summary(vfrequ)
summary(mdgfrequ)
mean(mdgfrequ$Environment) - mean(mdgfrequ$`Agriculture and fishing`)
mean(vfrequ$Environment) - mean(vfrequ$Education)

summary(all_frequ$Environment[all_frequ$party == "AP"])
summary(all_frequ$Environment[all_frequ$party == "FRP"])
summary(all_frequ$Environment[all_frequ$party == "MDG"])
summary(all_frequ$Environment[all_frequ$party == "V"])
summary(all_frequ$Environment[all_frequ$party == "KP"])
ggplot(all_frequ, aes(year, Environment, colour = party)) +
  geom_point() +
  geom_line()

table(all_frequ$Environment[all_frequ$ year == 2021])

## Creating an area plot showing the distribution
## of KP

freq_long %>% filter(party == "KP") %>%
  ggplot(aes(year, Attention,
             fill = Topics)) +
  geom_area(alpha = .6, size = 0.5, 
            colour = "black") +
  scale_fill_manual(values = scales::hue_pal()(21), breaks = c("Macroeconomics",
                                                               "Civil rights",
                                                               "Health", "Agriculture and fishing",
                                                               "Labour", "Education",
                                                               "Environment", "Energy",
                                                               "Immigration", "Traffic",
                                                               "Legal affairs", "Social policy",
                                                               "Housing", "Commercial policy",
                                                               "Defence",
                                                               "Technology",
                                                               "Foreign trade",
                                                               "Foreign policy", "Government",
                                                               "Public lands", "Culture")) +
  theme_bw() +
  scale_x_continuous(breaks = c(2001,2005,2009,
                                2013,2017,2021)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       y = "Issue attention",
       title = "Graph 6.11. The issue emphasis of the Coastal party")

## Creating an area plot showing the distribution
## of H

freq_long %>% filter(party == "H") %>%
  ggplot(aes(year, Attention,
             fill = Topics)) +
  geom_area(alpha = .6, size = 0.5, 
            colour = "black") +
  scale_fill_manual(values = scales::hue_pal()(21), breaks = c("Macroeconomics",
                                                               "Civil rights",
                                                               "Health", "Agriculture and fishing",
                                                               "Labour", "Education",
                                                               "Environment", "Energy",
                                                               "Immigration", "Traffic",
                                                               "Legal affairs", "Social policy",
                                                               "Housing", "Commercial policy",
                                                               "Defence",
                                                               "Technology",
                                                               "Foreign trade",
                                                               "Foreign policy", "Government",
                                                               "Public lands", "Culture")) +
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election year",
       y = "Issue attention",
       title = "Graph 6.4. The issue attention of the Conservative party")

## Examining some of H's core issues and possible
## adaptations to coalition partners

summary(hfrequ)
ggplot(hfrequ, aes(x = year)) +
  geom_line(aes(y = Health)) +
  geom_line(aes(y = Education)) +
  geom_line(aes(y = `Legal affairs`)) +
  geom_line(aes(y = `Social policy`)) +
  geom_line(aes(y = Government))

ggplot(all_frequ, aes(year, Immigration, colour = party)) +
  geom_line() +
  geom_point()

summary(apfrequ$Immigration[apfrequ$year >2008])
summary(frpfrequ$Immigration[frpfrequ$year >2008])
summary(mdgfrequ$Immigration[mdgfrequ$year >2008])
summary(vfrequ$Immigration[vfrequ$year >2008])
summary(hfrequ$Immigration[hfrequ$year >2008])

ggplot(hfrequ, aes(year, `Government`)) +
  geom_point() +
  geom_line()

#### Looking at descriptive statistics ####

summary(apfrequ)
summary(frpfrequ)
summary(mdgfrequ)
summary(vfrequ)
summary(kpfrequ)
summary(hfrequ)

#### Testing the punctuated equilibrium theory ####

## Creating dataframes measuring the change of
## attention for AP

ap_frequ2 <- apfrequ %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Civil_change = (`Civil rights` - lag(`Civil rights`))/lag(`Civil rights`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Health_change = (Health - lag(Health)/lag(Health)))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Agriculture_change = (`Agriculture and fishing` - lag(`Agriculture and fishing`))/lag(`Agriculture and fishing`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

ap_frequ2 <- ap_frequ2 %>% 
  mutate(Education_change = (Education - lag(Education))/lag(Education))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration)/lag(Immigration)))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/(lag(Traffic)))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Legal_change = (`Legal affairs` - lag(`Legal affairs`))/lag(`Legal affairs`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Social_change = (`Social policy` - lag(`Social policy`))/lag(`Social policy`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Commercial_change = (`Commercial policy` - lag(`Commercial policy`))/lag(`Commercial policy`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Technology_change = (Technology - lag(Technology))/lag(Technology))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Trade_change = (`Foreign trade` - lag(`Foreign trade`))/lag(`Foreign trade`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Foreign_change = (`Foreign policy` - lag(`Foreign policy`))/lag(`Foreign policy`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Government_change = (Government - lag(Government))/lag(Government))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Lands_change = (`Public lands` - lag(`Public lands`))/lag(`Public lands`))

ap_frequ2 <- ap_frequ2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))

## Creating dataframes measuring the change of
## attention for frp

frp_frequ2 <- frpfrequ %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Civil_change = (`Civil rights` - lag(`Civil rights`))/lag(`Civil rights`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Health_change = (Health - lag(Health)/lag(Health)))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Agriculture_change = (`Agriculture and fishing` - lag(`Agriculture and fishing`))/lag(`Agriculture and fishing`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

frp_frequ2 <- frp_frequ2 %>% 
  mutate(Education_change = (Education - lag(Education))/lag(Education))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration)/lag(Immigration)))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/(lag(Traffic)))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Legal_change = (`Legal affairs` - lag(`Legal affairs`))/lag(`Legal affairs`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Social_change = (`Social policy` - lag(`Social policy`))/lag(`Social policy`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Commercial_change = (`Commercial policy` - lag(`Commercial policy`))/lag(`Commercial policy`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Technology_change = (Technology - lag(Technology))/lag(Technology))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Trade_change = (`Foreign trade` - lag(`Foreign trade`))/lag(`Foreign trade`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Foreign_change = (`Foreign policy` - lag(`Foreign policy`))/lag(`Foreign policy`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Government_change = (Government - lag(Government))/lag(Government))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Lands_change = (`Public lands` - lag(`Public lands`))/lag(`Public lands`))

frp_frequ2 <- frp_frequ2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))

## Creating dataframes measuring change of 
## attention for mdg

mdg_frequ2 <- mdgfrequ %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Civil_change = (`Civil rights` - lag(`Civil rights`))/lag(`Civil rights`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Health_change = (Health - lag(Health)/lag(Health)))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Agriculture_change = (`Agriculture and fishing` - lag(`Agriculture and fishing`))/lag(`Agriculture and fishing`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

mdg_frequ2 <- mdg_frequ2 %>% 
  mutate(Education_change = (Education - lag(Education))/lag(Education))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration)/lag(Immigration)))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/(lag(Traffic)))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Legal_change = (`Legal affairs` - lag(`Legal affairs`))/lag(`Legal affairs`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Social_change = (`Social policy` - lag(`Social policy`))/lag(`Social policy`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Commercial_change = (`Commercial policy` - lag(`Commercial policy`))/lag(`Commercial policy`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Technology_change = (Technology - lag(Technology))/lag(Technology))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Trade_change = (`Foreign trade` - lag(`Foreign trade`))/lag(`Foreign trade`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Foreign_change = (`Foreign policy` - lag(`Foreign policy`))/lag(`Foreign policy`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Government_change = (Government - lag(Government))/lag(Government))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Lands_change = (`Public lands` - lag(`Public lands`))/lag(`Public lands`))

mdg_frequ2 <- mdg_frequ2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))

## Creating dataframes measuring the change of
## attention for V

v_frequ2 <- vfrequ %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

v_frequ2 <- v_frequ2 %>%
  mutate(Civil_change = (`Civil rights` - lag(`Civil rights`))/lag(`Civil rights`))

v_frequ2 <- v_frequ2 %>%
  mutate(Health_change = (Health - lag(Health)/lag(Health)))

v_frequ2 <- v_frequ2 %>%
  mutate(Agriculture_change = (`Agriculture and fishing` - lag(`Agriculture and fishing`))/lag(`Agriculture and fishing`))

v_frequ2 <- v_frequ2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

v_frequ2 <- v_frequ2 %>% 
  mutate(Education_change = (Education - lag(Education))/lag(Education))

v_frequ2 <- v_frequ2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

v_frequ2 <- v_frequ2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

v_frequ2 <- v_frequ2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration)/lag(Immigration)))

v_frequ2 <- v_frequ2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/(lag(Traffic)))

v_frequ2 <- v_frequ2 %>%
  mutate(Legal_change = (`Legal affairs` - lag(`Legal affairs`))/lag(`Legal affairs`))

v_frequ2 <- v_frequ2 %>%
  mutate(Social_change = (`Social policy` - lag(`Social policy`))/lag(`Social policy`))

v_frequ2 <- v_frequ2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

v_frequ2 <- v_frequ2 %>%
  mutate(Commercial_change = (`Commercial policy` - lag(`Commercial policy`))/lag(`Commercial policy`))

v_frequ2 <- v_frequ2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

v_frequ2 <- v_frequ2 %>%
  mutate(Technology_change = (Technology - lag(Technology))/lag(Technology))

v_frequ2 <- v_frequ2 %>%
  mutate(Trade_change = (`Foreign trade` - lag(`Foreign trade`))/lag(`Foreign trade`))

v_frequ2 <- v_frequ2 %>%
  mutate(Foreign_change = (`Foreign policy` - lag(`Foreign policy`))/lag(`Foreign policy`))

v_frequ2 <- v_frequ2 %>%
  mutate(Government_change = (Government - lag(Government))/lag(Government))

v_frequ2 <- v_frequ2 %>%
  mutate(Lands_change = (`Public lands` - lag(`Public lands`))/lag(`Public lands`))

v_frequ2 <- v_frequ2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))

## Creating dataframes measuring change of 
## attention for KP

kp_frequ2 <- kpfrequ %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Civil_change = (`Civil rights` - lag(`Civil rights`))/lag(`Civil rights`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Health_change = (Health - lag(Health)/lag(Health)))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Agriculture_change = (`Agriculture and fishing` - lag(`Agriculture and fishing`))/lag(`Agriculture and fishing`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

kp_frequ2 <- kp_frequ2 %>% 
  mutate(Education_change = (Education - lag(Education))/lag(Education))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration)/lag(Immigration)))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/(lag(Traffic)))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Legal_change = (`Legal affairs` - lag(`Legal affairs`))/lag(`Legal affairs`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Social_change = (`Social policy` - lag(`Social policy`))/lag(`Social policy`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Commercial_change = (`Commercial policy` - lag(`Commercial policy`))/lag(`Commercial policy`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Technology_change = (Technology - lag(Technology))/lag(Technology))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Trade_change = (`Foreign trade` - lag(`Foreign trade`))/lag(`Foreign trade`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Foreign_change = (`Foreign policy` - lag(`Foreign policy`))/lag(`Foreign policy`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Government_change = (Government - lag(Government))/lag(Government))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Lands_change = (`Public lands` - lag(`Public lands`))/lag(`Public lands`))

kp_frequ2 <- kp_frequ2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))

## Creating dataframes measuring change of 
## attention for H

h_frequ2 <- hfrequ %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

h_frequ2 <- h_frequ2 %>%
  mutate(Civil_change = (`Civil rights` - lag(`Civil rights`))/lag(`Civil rights`))

h_frequ2 <- h_frequ2 %>%
  mutate(Health_change = (Health - lag(Health)/lag(Health)))

h_frequ2 <- h_frequ2 %>%
  mutate(Agriculture_change = (`Agriculture and fishing` - lag(`Agriculture and fishing`))/lag(`Agriculture and fishing`))

h_frequ2 <- h_frequ2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

h_frequ2 <- h_frequ2 %>% 
  mutate(Education_change = (Education - lag(Education))/lag(Education))

h_frequ2 <- h_frequ2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

h_frequ2 <- h_frequ2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

h_frequ2 <- h_frequ2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration)/lag(Immigration)))

h_frequ2 <- h_frequ2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/(lag(Traffic)))

h_frequ2 <- h_frequ2 %>%
  mutate(Legal_change = (`Legal affairs` - lag(`Legal affairs`))/lag(`Legal affairs`))

h_frequ2 <- h_frequ2 %>%
  mutate(Social_change = (`Social policy` - lag(`Social policy`))/lag(`Social policy`))

h_frequ2 <- h_frequ2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

h_frequ2 <- h_frequ2 %>%
  mutate(Commercial_change = (`Commercial policy` - lag(`Commercial policy`))/lag(`Commercial policy`))

h_frequ2 <- h_frequ2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

h_frequ2 <- h_frequ2 %>%
  mutate(Technology_change = (Technology - lag(Technology))/lag(Technology))

h_frequ2 <- h_frequ2 %>%
  mutate(Trade_change = (`Foreign trade` - lag(`Foreign trade`))/lag(`Foreign trade`))

h_frequ2 <- h_frequ2 %>%
  mutate(Foreign_change = (`Foreign policy` - lag(`Foreign policy`))/lag(`Foreign policy`))

h_frequ2 <- h_frequ2 %>%
  mutate(Government_change = (Government - lag(Government))/lag(Government))

h_frequ2 <- h_frequ2 %>%
  mutate(Lands_change = (`Public lands` - lag(`Public lands`))/lag(`Public lands`))

h_frequ2 <- h_frequ2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))

## Merging and fixing the data

all_frequ2 <- full_join(ap_frequ2,frp_frequ2)
all_frequ2 <- full_join(all_frequ2,mdg_frequ2)
all_frequ2 <- full_join(all_frequ2,v_frequ2)
all_frequ2 <- full_join(all_frequ2,kp_frequ2)
all_frequ2 <- full_join(all_frequ2,h_frequ2)

all_frequ2 <- all_frequ2 %>%
  select(ends_with(c("party","year", "change"))) %>%
  pivot_longer(cols = ends_with("change"))

all_frequ2 <- all_frequ2 %>%
  mutate(percent_change = value*100)

all_frequ2$value <- ifelse(is.infinite(all_frequ2$value)  == T,NA,
                           all_frequ2$value)

all_frequ2$percent_change <- ifelse(is.infinite(all_frequ2$percent_change) == T,
                                    NA,
                                    all_frequ2$percent_change)

## ## Prepping datasets of change for each party

ap_frequ2 <- ap_frequ2 %>%
  select(ends_with(c("party","year", "change"))) %>%
  pivot_longer(cols = ends_with("change"))
ap_frequ2 <- ap_frequ2 %>%
  mutate(percent_change = value*100)
ap_frequ2$percent_change <- ifelse(is.infinite(ap_frequ2$percent_change) == T,
                                   NA,
                                   ap_frequ2$percent_change)

frp_frequ2 <- frp_frequ2 %>%
  select(ends_with(c("party","year", "change"))) %>%
  pivot_longer(cols = ends_with("change"))
frp_frequ2 <- frp_frequ2 %>%
  mutate(percent_change = value*100)
frp_frequ2$percent_change <- ifelse(is.infinite(frp_frequ2$percent_change) == T,
                                    NA,
                                    frp_frequ2$percent_change)

mdg_frequ2 <- mdg_frequ2 %>%
  select(ends_with(c("party","year", "change"))) %>%
  pivot_longer(cols = ends_with("change"))
mdg_frequ2 <- mdg_frequ2 %>%
  mutate(percent_change = value*100)
mdg_frequ2$percent_change <- ifelse(is.infinite(mdg_frequ2$percent_change) == T,
                                    NA,
                                    mdg_frequ2$percent_change)

v_frequ2 <- v_frequ2 %>%
  select(ends_with(c("party","year", "change"))) %>%
  pivot_longer(cols = ends_with("change"))
v_frequ2 <- v_frequ2 %>%
  mutate(percent_change = value*100)
v_frequ2$percent_change <- ifelse(is.infinite(v_frequ2$percent_change) == T,
                                  NA,
                                  v_frequ2$percent_change)
kp_frequ2 <- kp_frequ2 %>%
  select(ends_with(c("party","year", "change"))) %>%
  pivot_longer(cols = ends_with("change"))
kp_frequ2 <- kp_frequ2 %>%
  mutate(percent_change = value*100)
kp_frequ2$percent_change <- ifelse(is.infinite(kp_frequ2$percent_change) == T,
                                   NA,
                                   kp_frequ2$percent_change)

h_frequ2 <- h_frequ2 %>%
  select(ends_with(c("party","year", "change"))) %>%
  pivot_longer(cols = ends_with("change"))
h_frequ2 <- h_frequ2 %>%
  mutate(percent_change = value*100)
h_frequ2$percent_change <- ifelse(is.infinite(h_frequ2$percent_change) == T,
                                  NA,
                                  h_frequ2$percent_change)

## Examining kurtosis

class(all_frequ2$percent_change)
kurtosis(all_frequ2$percent_change, na.rm = T)
kurtosis(ap_frequ2$percent_change, na.rm = T)
kurtosis(frp_frequ2$percent_change, na.rm = T)
kurtosis(mdg_frequ2$percent_change, na.rm = T)
kurtosis(v_frequ2$percent_change, na.rm = T)
kurtosis(kp_frequ2$percent_change, na.rm = T)
kurtosis(h_frequ2$percent_change, na.rm = T)

## Measuring AP's L-kurtosis

ap_frequ3 <- subset(ap_frequ2, select = percent_change) 
ap_frequ3 <- na.omit(ap_frequ3)
ap_frequ3 <- t(ap_frequ3)
n <- length(ap_frequ3)
y <- sort(ap_frequ3)

L1ap <- (1/choose(n,1))*sum(sort(ap_frequ3))

sum.t.L2 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L2 <- (choose((i-1), 1)- choose((n-i), 1))*y[i]
  
  sum.t.L2[i] <- t.L2
  
}

sum(sum.t.L2)
L2ap <- 0.5*(1/choose(n,2))*sum(sum.t.L2)



sum.t.L3 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L3 <- (choose((i-1), 2)-(2*choose((i-1),1)*choose((n-i),1))+choose((n-i), 2))*y[i]
  
  
  
  sum.t.L3[i] <- t.L3
  
}

sum(sum.t.L3)
L3ap <- (1/3)*(1/choose(n,3))*sum(sum.t.L3)


sum.t.L4 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L4 <- (choose((i-1), 3)-(3*choose((i-1),2)*choose((n-i),1))+(3*choose((i-1),1)*choose((n-i),2))-choose((n-i), 3))*y[i]
  sum.t.L4[i] <- t.L4
  
}

sum(sum.t.L4)
L4ap <- (1/4)*(1/choose(n,4))*sum(sum.t.L4)

L4ap/L2ap

## Measuring FrP's L-kurtosis

frp_frequ3 <- subset(frp_frequ2, select = percent_change) 
frp_frequ3 <- na.omit(frp_frequ3)
frp_frequ3 <- t(frp_frequ3)
n <- length(frp_frequ3)
y <- sort(frp_frequ3)

L1frp <- (1/choose(n,1))*sum(sort(frp_frequ3))

sum.t.L2 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L2 <- (choose((i-1), 1)- choose((n-i), 1))*y[i]
  
  sum.t.L2[i] <- t.L2
  
}

sum(sum.t.L2)
L2frp <- 0.5*(1/choose(n,2))*sum(sum.t.L2)



sum.t.L3 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L3 <- (choose((i-1), 2)-(2*choose((i-1),1)*choose((n-i),1))+choose((n-i), 2))*y[i]
  
  
  
  sum.t.L3[i] <- t.L3
  
}

sum(sum.t.L3)
L3frp <- (1/3)*(1/choose(n,3))*sum(sum.t.L3)


sum.t.L4 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L4 <- (choose((i-1), 3)-(3*choose((i-1),2)*choose((n-i),1))+(3*choose((i-1),1)*choose((n-i),2))-choose((n-i), 3))*y[i]
  sum.t.L4[i] <- t.L4
  
}

sum(sum.t.L4)
L4frp <- (1/4)*(1/choose(n,4))*sum(sum.t.L4)

L4frp/L2frp

## Measuring mdg's L-kurtosis

mdg_frequ3 <- subset(mdg_frequ2, select = percent_change) 
mdg_frequ3 <- na.omit(mdg_frequ3)
mdg_frequ3 <- t(mdg_frequ3)
n <- length(mdg_frequ3)
y <- sort(mdg_frequ3)

L1mdg <- (1/choose(n,1))*sum(sort(mdg_frequ3))

sum.t.L2 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L2 <- (choose((i-1), 1)- choose((n-i), 1))*y[i]
  
  sum.t.L2[i] <- t.L2
  
}

sum(sum.t.L2)
L2mdg <- 0.5*(1/choose(n,2))*sum(sum.t.L2)



sum.t.L3 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L3 <- (choose((i-1), 2)-(2*choose((i-1),1)*choose((n-i),1))+choose((n-i), 2))*y[i]
  
  
  
  sum.t.L3[i] <- t.L3
  
}

sum(sum.t.L3)
L3mdg <- (1/3)*(1/choose(n,3))*sum(sum.t.L3)


sum.t.L4 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L4 <- (choose((i-1), 3)-(3*choose((i-1),2)*choose((n-i),1))+(3*choose((i-1),1)*choose((n-i),2))-choose((n-i), 3))*y[i]
  sum.t.L4[i] <- t.L4
  
}

sum(sum.t.L4)
L4mdg <- (1/4)*(1/choose(n,4))*sum(sum.t.L4)

L4mdg/L2mdg

## Measuring V's L-kurtosis

v_frequ3 <- subset(v_frequ2, select = percent_change) 
v_frequ3 <- na.omit(v_frequ3)
v_frequ3 <- t(v_frequ3)
n <- length(v_frequ3)
y <- sort(v_frequ3)

L1v <- (1/choose(n,1))*sum(sort(v_frequ3))

sum.t.L2 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L2 <- (choose((i-1), 1)- choose((n-i), 1))*y[i]
  
  sum.t.L2[i] <- t.L2
  
}

sum(sum.t.L2)
L2v <- 0.5*(1/choose(n,2))*sum(sum.t.L2)



sum.t.L3 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L3 <- (choose((i-1), 2)-(2*choose((i-1),1)*choose((n-i),1))+choose((n-i), 2))*y[i]
  
  
  
  sum.t.L3[i] <- t.L3
  
}

sum(sum.t.L3)
L3v <- (1/3)*(1/choose(n,3))*sum(sum.t.L3)


sum.t.L4 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L4 <- (choose((i-1), 3)-(3*choose((i-1),2)*choose((n-i),1))+(3*choose((i-1),1)*choose((n-i),2))-choose((n-i), 3))*y[i]
  sum.t.L4[i] <- t.L4
  
}

sum(sum.t.L4)
L4v <- (1/4)*(1/choose(n,4))*sum(sum.t.L4)

L4v/L2v

## Measuring KP's L-kurtosis

kp_frequ3 <- subset(kp_frequ2, select = percent_change) 
kp_frequ3 <- na.omit(kp_frequ3)
kp_frequ3 <- t(kp_frequ3)
n <- length(kp_frequ3)
y <- sort(kp_frequ3)

L1kp <- (1/choose(n,1))*sum(sort(kp_frequ3))

sum.t.L2 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L2 <- (choose((i-1), 1)- choose((n-i), 1))*y[i]
  
  sum.t.L2[i] <- t.L2
  
}

sum(sum.t.L2)
L2kp <- 0.5*(1/choose(n,2))*sum(sum.t.L2)



sum.t.L3 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L3 <- (choose((i-1), 2)-(2*choose((i-1),1)*choose((n-i),1))+choose((n-i), 2))*y[i]
  
  
  
  sum.t.L3[i] <- t.L3
  
}

sum(sum.t.L3)
L3kp <- (1/3)*(1/choose(n,3))*sum(sum.t.L3)


sum.t.L4 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L4 <- (choose((i-1), 3)-(3*choose((i-1),2)*choose((n-i),1))+(3*choose((i-1),1)*choose((n-i),2))-choose((n-i), 3))*y[i]
  sum.t.L4[i] <- t.L4
  
}

sum(sum.t.L4)
L4kp <- (1/4)*(1/choose(n,4))*sum(sum.t.L4)

L4kp/L2kp

## Measuring H's L-kurtosis

h_frequ3 <- subset(h_frequ2, select = percent_change) 
h_frequ3 <- na.omit(h_frequ3)
h_frequ3 <- t(h_frequ3)
n <- length(h_frequ3)
y <- sort(h_frequ3)

L1h <- (1/choose(n,1))*sum(sort(h_frequ3))

sum.t.L2 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L2 <- (choose((i-1), 1)- choose((n-i), 1))*y[i]
  
  sum.t.L2[i] <- t.L2
  
}

sum(sum.t.L2)
L2h <- 0.5*(1/choose(n,2))*sum(sum.t.L2)



sum.t.L3 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L3 <- (choose((i-1), 2)-(2*choose((i-1),1)*choose((n-i),1))+choose((n-i), 2))*y[i]
  
  
  
  sum.t.L3[i] <- t.L3
  
}

sum(sum.t.L3)
L3h <- (1/3)*(1/choose(n,3))*sum(sum.t.L3)


sum.t.L4 <- 0
bb <- n
for (i in 1:n)
  
{
  
  t.L4 <- (choose((i-1), 3)-(3*choose((i-1),2)*choose((n-i),1))+(3*choose((i-1),1)*choose((n-i),2))-choose((n-i), 3))*y[i]
  sum.t.L4[i] <- t.L4
  
}

sum(sum.t.L4)
L4h <- (1/4)*(1/choose(n,4))*sum(sum.t.L4)

L4h/L2h

#### Measuring the number of subtopics ####

## Removing irrelevant sentences

ap1985u <- ap1985 %>%
  filter(Norway_Code != 2999)
ap1989u <- ap1989 %>%
  filter(Norway_Code != 2999)
ap1993u <- ap1993 %>%
  filter(Norway_Code != 2999)
ap1997u <- ap1997 %>%
  filter(Norway_Code != 2999)
ap2001u <- ap2001 %>%
  filter(Norway_Code != 2999)
ap2005u <- ap2005 %>%
  filter(Norway_Code != 2999)
ap2009u <- ap2009 %>%
  filter(Norway_Code != 2999)
ap2013u <- ap2013 %>%
  filter(Norway_Code != 2999)
ap2017u <- ap2017 %>%
  filter(Norway_Code != 2999)
ap2021u <- ap2021 %>%
  filter(Norway_Code != 2999)
frp1985u <- frp1985 %>%
  filter(Norway_Code != 2999)
frp1989u <- frp1989 %>%
  filter(Norway_Code != 2999)
frp1993u <- frp1993 %>%
  filter(Norway_Code != 2999)
frp1997u <- frp1997 %>%
  filter(Norway_Code != 2999)
frp2001u <- frp2001 %>%
  filter(Norway_Code != 2999)
frp2005u <- frp2005 %>%
  filter(Norway_Code != 2999)
frp2009u <- frp2009 %>%
  filter(Norway_Code != 2999)
frp2013u <- frp2013 %>%
  filter(Norway_Code != 2999)
frp2017u <- frp2017 %>%
  filter(Norway_Code != 2999)
frp2021u <- frp2021 %>%
  filter(Norway_Code != 2999)
mdg1993u <- mdg1993 %>%
  filter(Norway_Code != 2999)
mdg1997u <- mdg1997 %>%
  filter(Norway_Code != 2999)
mdg2001u <- mdg2001 %>%
  filter(Norway_Code != 2999)
mdg2005u <- mdg2005 %>%
  filter(Norway_Code != 2999)
mdg2009u <- mdg2009 %>%
  filter(Norway_Code != 2999)
mdg2013u <- mdg2013 %>%
  filter(Norway_Code != 2999)
mdg2017u <- mdg2017 %>%
  filter(Norway_Code != 2999)
mdg2021u <- mdg2021 %>%
  filter(Norway_Code != 2999)
v1985u <- v1985 %>%
  filter(Norway_Code != 2999)
v1989u <- v1989 %>%
  filter(Norway_Code != 2999)
v1993u <- v1993 %>%
  filter(Norway_Code != 2999)
v1997u <- v1997 %>%
  filter(Norway_Code != 2999)
v2001u <- v2001 %>%
  filter(Norway_Code != 2999)
v2005u <- v2005 %>%
  filter(Norway_Code != 2999)
v2009u <- v2009 %>%
  filter(Norway_Code != 2999)
v2013u <- v2013 %>%
  filter(Norway_Code != 2999)
v2017u <- v2017 %>%
  filter(Norway_Code != 2999)
v2021u <- v2021 %>%
  filter(Norway_Code != 2999)
kp2001u <- kp2001 %>%
  filter(Norway_Code != 2999)
kp2005u <- kp2005 %>%
  filter(Norway_Code != 2999)
kp2009u <- kp2009 %>%
  filter(Norway_Code != 2999)
kp2013u <- kp2013 %>%
  filter(Norway_Code != 2999)
kp2017u <- kp2017 %>%
  filter(Norway_Code != 2999)
kp2021u <- kp2021 %>%
  filter(Norway_Code != 2999)
h1997u <- h1997 %>%
  filter(Norway_Code!= 2999)
h2001u <- h2001 %>%
  filter(Norway_Code!= 2999)
h2005u <- h2005 %>%
  filter(Norway_Code!= 2999)
h2009u <- h2009 %>%
  filter(Norway_Code!= 2999)
h2013u <- h2013 %>%
  filter(Norway_Code!= 2999)
h2017u <- h2017 %>%
  filter(Norway_Code!= 2999)
h2021u <- h2021 %>%
  filter(Norway_Code!= 2999)

## Merging the datasets

apu <- full_join(ap1985u,ap1989)
apu <- full_join(apu,ap1993u)
apu <- full_join(apu,ap1997u)
apu <- full_join(apu,ap2001u)
apu <- full_join(apu,ap2005u)
apu <- full_join(apu,ap2009u)
apu <- full_join(apu,ap2013u)
apu <- full_join(apu,ap2017u)
apu <- full_join(apu,ap2021u)

frpu <- full_join(frp1985u,frp1989u)
frpu <- full_join(frpu,frp1993u)
frpu <- full_join(frpu,frp1997u)
frpu <- full_join(frpu,frp2001u)
frpu <- full_join(frpu,frp2005u)
frpu <- full_join(frpu,frp2009u)
frpu <- full_join(frpu,frp2013u)
frpu <- full_join(frpu,frp2017u)
frpu <- full_join(frpu,frp2021u)

mdgu <- full_join(mdg1993u,mdg1997u)
mdgu <- full_join(mdgu,mdg2001u)
mdgu <- full_join(mdgu,mdg2005u)
mdgu <- full_join(mdgu,mdg2009u)
mdgu <- full_join(mdgu,mdg2013u)
mdgu <- full_join(mdgu,mdg2017u)
mdgu <- full_join(mdgu,mdg2021u)

vu <- full_join(v1985u,v1989u)
vu <- full_join(vu,v1993u)
vu <- full_join(vu,v1997u)
vu <- full_join(vu,v2001u)
vu <- full_join(vu,v2005u)
vu <- full_join(vu,v2009u)
vu <- full_join(vu,v2013u)
vu <- full_join(vu,v2017u)
vu <- full_join(vu,v2021u)

kpu <- full_join(kp2001u,kp2005u)
kpu <- full_join(kpu, kp2009u)
kpu <- full_join(kpu, kp2013u)
kpu <- full_join(kpu, kp2017u)
kpu <- full_join(kpu, kp2021u)

hu <- full_join(h1997u,h2001u)
hu <- full_join(hu,h2005u)
hu <- full_join(hu,h2009u)
hu <- full_join(hu,h2013u)
hu <- full_join(hu,h2017u)
hu <- full_join(hu,h2021u)

all_u <- full_join(apu,frpu)
all_u <- full_join(all_u,mdgu)
all_u <- full_join(all_u,vu)
all_u <- full_join(all_u,kpu)
all_u <- full_join(all_u,hu)

## Creating a variable for the number of 
## subtopics included in each manifesto

all_frequ$total <- c(length(table(ap1985u$Norway_Code)),
                     length(table(ap1989u$Norway_Code)),
                     length(table(ap1993u$Norway_Code)),
                     length(table(ap1997u$Norway_Code)),
                     length(table(ap2001u$Norway_Code)),
                     length(table(ap2005u$Norway_Code)),
                     length(table(ap2009u$Norway_Code)),
                     length(table(ap2013u$Norway_Code)),
                     length(table(ap2017u$Norway_Code)),
                     length(table(ap2021u$Norway_Code)),
                     length(table(frp1985u$Norway_Code)),
                     length(table(frp1989u$Norway_Code)),
                     length(table(frp1993u$Norway_Code)),
                     length(table(frp1997u$Norway_Code)),
                     length(table(frp2001u$Norway_Code)),
                     length(table(frp2005u$Norway_Code)),
                     length(table(frp2009u$Norway_Code)),
                     length(table(frp2013u$Norway_Code)),
                     length(table(frp2017u$Norway_Code)),
                     length(table(frp2021u$Norway_Code)),
                     length(table(mdg1993u$Norway_Code)),
                     length(table(mdg1997u$Norway_Code)),
                     length(table(mdg2001u$Norway_Code)),
                     length(table(mdg2005u$Norway_Code)),
                     length(table(mdg2009u$Norway_Code)),
                     length(table(mdg2013u$Norway_Code)),
                     length(table(mdg2017u$Norway_Code)),
                     length(table(mdg2021u$Norway_Code)),
                     length(table(v1985u$Norway_Code)),
                     length(table(v1989u$Norway_Code)),
                     length(table(v1993u$Norway_Code)),
                     length(table(v1997u$Norway_Code)),
                     length(table(v2001u$Norway_Code)),
                     length(table(v2005u$Norway_Code)),
                     length(table(v2009u$Norway_Code)),
                     length(table(v2013u$Norway_Code)),
                     length(table(v2017u$Norway_Code)),
                     length(table(v2021u$Norway_Code)),
                     length(table(kp2001u$Norway_Code)),
                     length(table(kp2005u$Norway_Code)),
                     length(table(kp2009u$Norway_Code)),
                     length(table(kp2013u$Norway_Code)),
                     length(table(kp2017u$Norway_Code)),
                     length(table(kp2021u$Norway_Code)),
                     length(table(h1997u$Norway_Code)),
                     length(table(h2001u$Norway_Code)),
                     length(table(h2005u$Norway_Code)),
                     length(table(h2009u$Norway_Code)),
                     length(table(h2013u$Norway_Code)),
                     length(table(h2017u$Norway_Code)),
                     length(table(h2021u$Norway_Code)))

show(all_frequ$total)
## Creating a graph showing the development of
## subtopics included in manifestos

ggplot(all_frequ, aes(year, total, colour = party)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2"))+
  theme_bw()  +
  labs(x = "Election year",
       y = "Number of subtopics",
       title = "Graph 6.13. The number of subtopics out of 235 used in manifestos") +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(plot.title = element_text(hjust = 0.5))

## Measuring the average number of subtopics 
## included

mean(all_frequ$total)
mean(all_frequ$total[all_frequ$party == "AP"])
mean(all_frequ$total[all_frequ$party == "FRP"])
mean(all_frequ$total[all_frequ$party == "MDG"])
mean(all_frequ$total[all_frequ$party == "V"])
mean(all_frequ$total[all_frequ$party == "KP"])
mean(all_frequ$total[all_frequ$party == "H"])

#### Creating a variable for entropy ####

entropy <- (all_frequ$Macroeconomics)*log(all_frequ$Macroeconomics)+
  (all_frequ$`Civil rights`)*log(all_frequ$`Civil rights`) +
  (all_frequ$Health)*log(all_frequ$Health) +
  all_frequ$`Agriculture and fishing`*log(all_frequ$`Agriculture and fishing`)+
  all_frequ$Labour*log(all_frequ$Labour) +
  all_frequ$Education*log(all_frequ$Education) +
  all_frequ$Environment*log(all_frequ$Environment) +
  all_frequ$Energy*log(all_frequ$Energy) +
  all_frequ$Immigration*log(all_frequ$Immigration) +
  all_frequ$Traffic*log(all_frequ$Traffic) +
  all_frequ$`Legal affairs`*log(all_frequ$`Legal affairs`) +
  all_frequ$`Social policy`*log(all_frequ$`Social policy`) +
  all_frequ$Housing*log(all_frequ$Housing) +
  all_frequ$`Commercial policy`*log(all_frequ$`Commercial policy`) +
  all_frequ$Defence*log(all_frequ$Defence) +
  all_frequ$Technology*log(all_frequ$Technology)+
  all_frequ$`Foreign trade`*log(all_frequ$`Foreign trade`)+
  all_frequ$`Foreign policy`*log(all_frequ$`Foreign policy`) +
  all_frequ$Government*log(all_frequ$Government) +
  all_frequ$`Public lands`*log(all_frequ$`Public lands`) +
  all_frequ$Culture*log(all_frequ$Culture)

entropy09 <- (all_frequ$Macroeconomics)*log(all_frequ$Macroeconomics)+
  (all_frequ$`Civil rights`)*log(all_frequ$`Civil rights`) +
  (all_frequ$Health)*log(all_frequ$Health) +
  all_frequ$`Agriculture and fishing`*log(all_frequ$`Agriculture and fishing`)+
  all_frequ$Labour*log(all_frequ$Labour) +
  all_frequ$Education*log(all_frequ$Education) +
  all_frequ$Environment*log(all_frequ$Environment) +
  all_frequ$Energy*log(all_frequ$Energy) +
  all_frequ$Immigration*(all_frequ$Immigration) +
  all_frequ$Traffic*log(all_frequ$Traffic) +
  all_frequ$`Legal affairs`*log(all_frequ$`Legal affairs`) +
  all_frequ$`Social policy`*log(all_frequ$`Social policy`) +
  all_frequ$Housing*log(all_frequ$Housing) +
  all_frequ$`Commercial policy`*log(all_frequ$`Commercial policy`) +
  all_frequ$Defence*(all_frequ$Defence) +
  all_frequ$Technology*log(all_frequ$Technology)+
  all_frequ$`Foreign trade`*log(all_frequ$`Foreign trade`)+
  all_frequ$`Foreign policy`*log(all_frequ$`Foreign policy`) +
  all_frequ$Government*log(all_frequ$Government) +
  all_frequ$`Public lands`*(all_frequ$`Public lands`) +
  all_frequ$Culture*log(all_frequ$Culture)
entropy09[25]

entropy93 <- (all_frequ$Macroeconomics)*log(all_frequ$Macroeconomics)+
  (all_frequ$`Civil rights`)*log(all_frequ$`Civil rights`) +
  (all_frequ$Health)*log(all_frequ$Health) +
  all_frequ$`Agriculture and fishing`*log(all_frequ$`Agriculture and fishing`)+
  all_frequ$Labour*log(all_frequ$Labour) +
  all_frequ$Education*log(all_frequ$Education) +
  all_frequ$Environment*log(all_frequ$Environment) +
  all_frequ$Energy*log(all_frequ$Energy) +
  all_frequ$Immigration*log(all_frequ$Immigration) +
  all_frequ$Traffic*log(all_frequ$Traffic) +
  all_frequ$`Legal affairs`*log(all_frequ$`Legal affairs`) +
  all_frequ$`Social policy`*log(all_frequ$`Social policy`) +
  all_frequ$Housing*log(all_frequ$Housing) +
  all_frequ$`Commercial policy`*log(all_frequ$`Commercial policy`) +
  all_frequ$Defence*log(all_frequ$Defence) +
  all_frequ$Technology*(all_frequ$Technology)+
  all_frequ$`Foreign trade`*(all_frequ$`Foreign trade`)+
  all_frequ$`Foreign policy`*log(all_frequ$`Foreign policy`) +
  all_frequ$Government*(all_frequ$Government) +
  all_frequ$`Public lands`*log(all_frequ$`Public lands`) +
  all_frequ$Culture*(all_frequ$Culture)

## Adding the entropy scores for the uncomplete
##manifestos

entropy[21] <- entropy93[21]
entropy[25] <- entropy09[25]

## Creating the variable

Entropi <- entropy*-1
Entropi
all_frequ$Entropi <- Entropi

## Examining the entropy scores

all_frequ$Entropi

mean(all_frequ$Entropi[all_frequ$party == "AP"])
mean(all_frequ$Entropi[all_frequ$party == "FRP"])
mean(all_frequ$Entropi[all_frequ$party == "MDG"])
mean(all_frequ$Entropi[all_frequ$party == "V"])
mean(all_frequ$Entropi[all_frequ$party == "KP"])
mean(all_frequ$Entropi[all_frequ$party == "H"])

## Creating a graph showing the development of 
## entropy scores

ggplot(all_frequ, aes(year,Entropi,colour = party)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2"))+
  theme_bw()  +
  labs(x = "Election year",
       y = "Entropy",
       title = "Graph 6.14. The development of entropy scores in manifestos") +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  theme(plot.title = element_text(hjust = 0.5))

show(all_frequ$Entropi)

#### Including subtopics ####

ap1985$Norway_Code[ap1985$Norway_Code == 2999] <- NA
ap1989$Norway_Code[ap1989$Norway_Code == 2999] <- NA
ap1993$Norway_Code[ap1993$Norway_Code == 2999] <- NA
ap1997$Norway_Code[ap1997$Norway_Code == 2999] <- NA
ap2001$Norway_Code[ap2001$Norway_Code == 2999] <- NA
ap2005$Norway_Code[ap2005$Norway_Code == 2999] <- NA
ap2009$Norway_Code[ap2009$Norway_Code == 2999] <- NA
ap2013$Norway_Code[ap2013$Norway_Code == 2999] <- NA
ap2017$Norway_Code[ap2017$Norway_Code == 2999] <- NA
ap2021$Norway_Code[ap2021$Norway_Code == 2999] <- NA

frp1985$Norway_Code[frp1985$Norway_Code == 2999] <- NA
frp1989$Norway_Code[frp1989$Norway_Code == 2999] <- NA
frp1993$Norway_Code[frp1993$Norway_Code == 2999] <- NA
frp1997$Norway_Code[frp1997$Norway_Code == 2999] <- NA
frp2001$Norway_Code[frp2001$Norway_Code == 2999] <- NA
frp2005$Norway_Code[frp2005$Norway_Code == 2999] <- NA
frp2009$Norway_Code[frp2009$Norway_Code == 2999] <- NA
frp2013$Norway_Code[frp2013$Norway_Code == 2999] <- NA
frp2017$Norway_Code[frp2017$Norway_Code == 2999] <- NA
frp2021$Norway_Code[frp2021$Norway_Code == 2999] <- NA

mdg1993$Norway_Code[mdg1993$Norway_Code == 2999] <- NA
mdg1997$Norway_Code[mdg1997$Norway_Code == 2999] <- NA
mdg2001$Norway_Code[mdg2001$Norway_Code == 2999] <- NA
mdg2005$Norway_Code[mdg2005$Norway_Code == 2999] <- NA
mdg2009$Norway_Code[mdg2009$Norway_Code == 2999] <- NA
mdg2013$Norway_Code[mdg2013$Norway_Code == 2999] <- NA
mdg2017$Norway_Code[mdg2017$Norway_Code == 2999] <- NA
mdg2021$Norway_Code[mdg2021$Norway_Code == 2999] <- NA

v1985$Norway_Code[v1985$Norway_Code == 2999] <- NA
v1989$Norway_Code[v1989$Norway_Code == 2999] <- NA
v1993$Norway_Code[v1993$Norway_Code == 2999] <- NA
v1997$Norway_Code[v1997$Norway_Code == 2999] <- NA
v2001$Norway_Code[v2001$Norway_Code == 2999] <- NA
v2005$Norway_Code[v2005$Norway_Code == 2999] <- NA
v2009$Norway_Code[v2009$Norway_Code == 2999] <- NA
v2013$Norway_Code[v2013$Norway_Code == 2999] <- NA
v2017$Norway_Code[v2017$Norway_Code == 2999] <- NA
v2021$Norway_Code[v2021$Norway_Code == 2999] <- NA

kp2001$Norway_Code[kp2001$Norway_Code == 2999] <- NA
kp2005$Norway_Code[kp2005$Norway_Code == 2999] <- NA
kp2009$Norway_Code[kp2009$Norway_Code == 2999] <- NA
kp2013$Norway_Code[kp2013$Norway_Code == 2999] <- NA
kp2017$Norway_Code[kp2017$Norway_Code == 2999] <- NA
kp2021$Norway_Code[kp2021$Norway_Code == 2999] <- NA

h2001$Norway_Code[h2001$Norway_Code == 2999] <- NA
h2005$Norway_Code[h2005$Norway_Code == 2999] <- NA
h2009$Norway_Code[h2009$Norway_Code == 2999] <- NA
h2013$Norway_Code[h2013$Norway_Code == 2999] <- NA
h2017$Norway_Code[h2017$Norway_Code == 2999] <- NA
h2021$Norway_Code[h2021$Norway_Code == 2999] <- NA

Var1 <- as.factor(c(100,101,103,104,105,107,108,110,111,112,
                    199,200,201,202,204,205,206,207,208,209,
                    210,211,299,300,301,302,321,322,323,324,
                    325,331,332,333,334,335,341,342,398,399,
                    400,401,402,403,404,405,406,407,408,409,
                    498,499,500,501,502,503,504,505,506,507,
                    508,529,599,600,601,602,603,604,606,607,
                    698,699,700,701,703,704,705,707,708,709,
                    710,711,712,798,799,800,801,802,803,804,
                    805,806,807,898,899,900,1000,1001,1002,
                    1003,1005,1007,1008,1010,1098,1099,1200,
                    1201,1202,1203,1204,1205,1206,1207,1208,
                    1210,1211,1227,1299,1300,1301,1302,1303,1304,
                    1305,1308,1399,1400,1401,1403,1404,1405,
                    1406,1408,1409,1411,1498,1499,1500,1501,
                    1502,1504,1505,1507,1520,1521,1522,1523,
                    1524,1525,1526,1599,1600,1602,1603,1604,
                    1605,1606,1608,1610,1611,1612,1614,1615,
                    1616,1617,1619,1620,1698,1699,1700,1701,
                    1704,1705,1706,1707,1708,1709,1798,1799,
                    1800,1802,1803,1804,1806,1807,1808,1899,
                    1900,1901,1902,1905,1906,1907,1908,1909,
                    1910,1911,1912,1913,1914,1915,1916,1919,
                    1920,1925,1926,1927,1929,1999,2000,2001,
                    2002,2003,2004,2005,2006,2007,2008,2009,
                    2010,2011,2012,2015,2016,2030,2099,2100,
                    2101,2102,2103,2104,2105,2199,2300))

data <- as.data.frame(Var1)

apfreq85N <- as.data.frame(prop.table(table(ap1985$Norway_Code)))
apfreq85N <- full_join(data,apfreq85N)
apfreq85N[is.na(apfreq85N)] <- 0
apfreq85N <- subset(apfreq85N,select = Freq)
apfreq85N <-as.data.frame(t(apfreq85N))

apfreq89N <- as.data.frame(prop.table(table(ap1989$Norway_Code)))
apfreq89N <- full_join(data,apfreq89N)
apfreq89N[is.na(apfreq89N)] <- 0
apfreq89N <- subset(apfreq89N,select = Freq)
apfreq89N <-as.data.frame(t(apfreq89N))

apfreq93N <- as.data.frame(prop.table(table(ap1993$Norway_Code)))
apfreq93N <- full_join(data,apfreq93N)
apfreq93N[is.na(apfreq93N)] <- 0
apfreq93N <- subset(apfreq93N,select = Freq)
apfreq93N <-as.data.frame(t(apfreq93N))

apfreq97N <- as.data.frame(prop.table(table(ap1997$Norway_Code)))
apfreq97N <- full_join(data,apfreq97N)
apfreq97N[is.na(apfreq97N)] <- 0
apfreq97N <- subset(apfreq97N,select = Freq)
apfreq97N <-as.data.frame(t(apfreq97N))

apfreq01N <- as.data.frame(prop.table(table(ap2001$Norway_Code)))
apfreq01N <- full_join(data,apfreq01N)
apfreq01N[is.na(apfreq01N)] <- 0
apfreq01N <- subset(apfreq01N,select = Freq)
apfreq01N <-as.data.frame(t(apfreq01N))

apfreq05N <- as.data.frame(prop.table(table(ap2005$Norway_Code)))
apfreq05N <- full_join(data,apfreq05N)
apfreq05N[is.na(apfreq05N)] <- 0
apfreq05N <- subset(apfreq05N,select = Freq)
apfreq05N <-as.data.frame(t(apfreq05N))

apfreq09N <- as.data.frame(prop.table(table(ap2009$Norway_Code)))
apfreq09N <- full_join(data,apfreq09N)
apfreq09N[is.na(apfreq09N)] <- 0
apfreq09N <- subset(apfreq09N,select = Freq)
apfreq09N <-as.data.frame(t(apfreq09N))

apfreq13N <- as.data.frame(prop.table(table(ap2013$Norway_Code)))
apfreq13N <- full_join(data,apfreq13N)
apfreq13N[is.na(apfreq13N)] <- 0
apfreq13N <- subset(apfreq13N,select = Freq)
apfreq13N <-as.data.frame(t(apfreq13N))

apfreq17N <- as.data.frame(prop.table(table(ap2017$Norway_Code)))
apfreq17N <- full_join(data,apfreq17N)
apfreq17N[is.na(apfreq17N)] <- 0
apfreq17N <- subset(apfreq17N,select = Freq)
apfreq17N <-as.data.frame(t(apfreq17N))

apfreq21N <- as.data.frame(prop.table(table(ap2021$Norway_Code)))
apfreq21N <- full_join(data,apfreq21N)
apfreq21N[is.na(apfreq21N)] <- 0
apfreq21N <- subset(apfreq21N,select = Freq)
apfreq21N <-as.data.frame(t(apfreq21N))

freqNap <- full_join(apfreq85N,apfreq89N)
freqNap <- full_join(freqNap,apfreq93N)
freqNap <- full_join(freqNap,apfreq97N)
freqNap <- full_join(freqNap,apfreq01N)
freqNap <- full_join(freqNap,apfreq05N)
freqNap <- full_join(freqNap,apfreq09N)
freqNap <- full_join(freqNap,apfreq13N)
freqNap <- full_join(freqNap,apfreq17N)
freqNap <- full_join(freqNap,apfreq21N)

frpfreq85N <- as.data.frame(prop.table(table(frp1985$Norway_Code)))
frpfreq85N <- full_join(data,frpfreq85N)
frpfreq85N[is.na(frpfreq85N)] <- 0
frpfreq85N <- subset(frpfreq85N,select = Freq)
frpfreq85N <-as.data.frame(t(frpfreq85N))

frpfreq89N <- as.data.frame(prop.table(table(frp1989$Norway_Code)))
frpfreq89N <- full_join(data,frpfreq89N)
frpfreq89N[is.na(frpfreq89N)] <- 0
frpfreq89N <- subset(frpfreq89N,select = Freq)
frpfreq89N <-as.data.frame(t(frpfreq89N))

frpfreq93N <- as.data.frame(prop.table(table(frp1993$Norway_Code)))
frpfreq93N <- full_join(data,frpfreq93N)
frpfreq93N[is.na(frpfreq93N)] <- 0
frpfreq93N <- subset(frpfreq93N,select = Freq)
frpfreq93N <-as.data.frame(t(frpfreq93N))

frpfreq97N <- as.data.frame(prop.table(table(frp1997$Norway_Code)))
frpfreq97N <- full_join(data,frpfreq97N)
frpfreq97N[is.na(frpfreq97N)] <- 0
frpfreq97N <- subset(frpfreq97N,select = Freq)
frpfreq97N <-as.data.frame(t(frpfreq97N))

frpfreq01N <- as.data.frame(prop.table(table(frp2001$Norway_Code)))
frpfreq01N <- full_join(data,frpfreq01N)
frpfreq01N[is.na(frpfreq01N)] <- 0
frpfreq01N <- subset(frpfreq01N,select = Freq)
frpfreq01N <-as.data.frame(t(frpfreq01N))

frpfreq05N <- as.data.frame(prop.table(table(frp2005$Norway_Code)))
frpfreq05N <- full_join(data,frpfreq05N)
frpfreq05N[is.na(frpfreq05N)] <- 0
frpfreq05N <- subset(frpfreq05N,select = Freq)
frpfreq05N <-as.data.frame(t(frpfreq05N))

frpfreq09N <- as.data.frame(prop.table(table(frp2009$Norway_Code)))
frpfreq09N <- full_join(data,frpfreq09N)
frpfreq09N[is.na(frpfreq09N)] <- 0
frpfreq09N <- subset(frpfreq09N,select = Freq)
frpfreq09N <-as.data.frame(t(frpfreq09N))

frpfreq13N <- as.data.frame(prop.table(table(frp2013$Norway_Code)))
frpfreq13N <- full_join(data,frpfreq13N)
frpfreq13N[is.na(frpfreq13N)] <- 0
frpfreq13N <- subset(frpfreq13N,select = Freq)
frpfreq13N <-as.data.frame(t(frpfreq13N))

frpfreq17N <- as.data.frame(prop.table(table(frp2017$Norway_Code)))
frpfreq17N <- full_join(data,frpfreq17N)
frpfreq17N[is.na(frpfreq17N)] <- 0
frpfreq17N <- subset(frpfreq17N,select = Freq)
frpfreq17N <-as.data.frame(t(frpfreq17N))

frpfreq21N <- as.data.frame(prop.table(table(frp2021$Norway_Code)))
frpfreq21N <- full_join(data,frpfreq21N)
frpfreq21N[is.na(frpfreq21N)] <- 0
frpfreq21N <- subset(frpfreq21N,select = Freq)
frpfreq21N <-as.data.frame(t(frpfreq21N))

freqNfrp <- full_join(frpfreq85N,frpfreq89N)
freqNfrp <- full_join(freqNfrp,frpfreq93N)
freqNfrp <- full_join(freqNfrp,frpfreq97N)
freqNfrp <- full_join(freqNfrp,frpfreq01N)
freqNfrp <- full_join(freqNfrp,frpfreq05N)
freqNfrp <- full_join(freqNfrp,frpfreq09N)
freqNfrp <- full_join(freqNfrp,frpfreq13N)
freqNfrp <- full_join(freqNfrp,frpfreq17N)
freqNfrp <- full_join(freqNfrp,frpfreq21N)

mdgfreq93N <- as.data.frame(prop.table(table(mdg1993$Norway_Code)))
mdgfreq93N <- full_join(data,mdgfreq93N)
mdgfreq93N[is.na(mdgfreq93N)] <- 0
mdgfreq93N <- subset(mdgfreq93N,select = Freq)
mdgfreq93N <-as.data.frame(t(mdgfreq93N))

mdgfreq97N <- as.data.frame(prop.table(table(mdg1997$Norway_Code)))
mdgfreq97N <- full_join(data,mdgfreq97N)
mdgfreq97N[is.na(mdgfreq97N)] <- 0
mdgfreq97N <- subset(mdgfreq97N,select = Freq)
mdgfreq97N <-as.data.frame(t(mdgfreq97N))

mdgfreq01N <- as.data.frame(prop.table(table(mdg2001$Norway_Code)))
mdgfreq01N <- full_join(data,mdgfreq01N)
mdgfreq01N[is.na(mdgfreq01N)] <- 0
mdgfreq01N <- subset(mdgfreq01N,select = Freq)
mdgfreq01N <-as.data.frame(t(mdgfreq01N))

mdgfreq05N <- as.data.frame(prop.table(table(mdg2005$Norway_Code)))
mdgfreq05N <- full_join(data,mdgfreq05N)
mdgfreq05N[is.na(mdgfreq05N)] <- 0
mdgfreq05N <- subset(mdgfreq05N,select = Freq)
mdgfreq05N <-as.data.frame(t(mdgfreq05N))

mdgfreq09N <- as.data.frame(prop.table(table(mdg2009$Norway_Code)))
mdgfreq09N <- full_join(data,mdgfreq09N)
mdgfreq09N[is.na(mdgfreq09N)] <- 0
mdgfreq09N <- subset(mdgfreq09N,select = Freq)
mdgfreq09N <-as.data.frame(t(mdgfreq09N))

mdgfreq13N <- as.data.frame(prop.table(table(mdg2013$Norway_Code)))
mdgfreq13N <- full_join(data,mdgfreq13N)
mdgfreq13N[is.na(mdgfreq13N)] <- 0
mdgfreq13N <- subset(mdgfreq13N,select = Freq)
mdgfreq13N <-as.data.frame(t(mdgfreq13N))

mdgfreq17N <- as.data.frame(prop.table(table(mdg2017$Norway_Code)))
mdgfreq17N <- full_join(data,mdgfreq17N)
mdgfreq17N[is.na(mdgfreq17N)] <- 0
mdgfreq17N <- subset(mdgfreq17N,select = Freq)
mdgfreq17N <-as.data.frame(t(mdgfreq17N))

mdgfreq21N <- as.data.frame(prop.table(table(mdg2021$Norway_Code)))
mdgfreq21N <- full_join(data,mdgfreq21N)
mdgfreq21N[is.na(mdgfreq21N)] <- 0
mdgfreq21N <- subset(mdgfreq21N,select = Freq)
mdgfreq21N <-as.data.frame(t(mdgfreq21N))

freqNmdg <- full_join(mdgfreq93N,mdgfreq97N)
freqNmdg <- full_join(freqNmdg,mdgfreq01N)
freqNmdg <- full_join(freqNmdg,mdgfreq05N)
freqNmdg <- full_join(freqNmdg,mdgfreq09N)
freqNmdg <- full_join(freqNmdg,mdgfreq13N)
freqNmdg <- full_join(freqNmdg,mdgfreq17N)
freqNmdg <- full_join(freqNmdg,mdgfreq21N)

vfreq85N <- as.data.frame(prop.table(table(v1985$Norway_Code)))
vfreq85N <- full_join(data,vfreq85N)
vfreq85N[is.na(vfreq85N)] <- 0
vfreq85N <- subset(vfreq85N,select = Freq)
vfreq85N <-as.data.frame(t(vfreq85N))

vfreq89N <- as.data.frame(prop.table(table(v1989$Norway_Code)))
vfreq89N <- full_join(data,vfreq89N)
vfreq89N[is.na(vfreq89N)] <- 0
vfreq89N <- subset(vfreq89N,select = Freq)
vfreq89N <-as.data.frame(t(vfreq89N))

vfreq93N <- as.data.frame(prop.table(table(v1993$Norway_Code)))
vfreq93N <- full_join(data,vfreq93N)
vfreq93N[is.na(vfreq93N)] <- 0
vfreq93N <- subset(vfreq93N,select = Freq)
vfreq93N <-as.data.frame(t(vfreq93N))

vfreq97N <- as.data.frame(prop.table(table(v1997$Norway_Code)))
vfreq97N <- full_join(data,vfreq97N)
vfreq97N[is.na(vfreq97N)] <- 0
vfreq97N <- subset(vfreq97N,select = Freq)
vfreq97N <-as.data.frame(t(vfreq97N))

vfreq01N <- as.data.frame(prop.table(table(v2001$Norway_Code)))
vfreq01N <- full_join(data,vfreq01N)
vfreq01N[is.na(vfreq01N)] <- 0
vfreq01N <- subset(vfreq01N,select = Freq)
vfreq01N <-as.data.frame(t(vfreq01N))

vfreq05N <- as.data.frame(prop.table(table(v2005$Norway_Code)))
vfreq05N <- full_join(data,vfreq05N)
vfreq05N[is.na(vfreq05N)] <- 0
vfreq05N <- subset(vfreq05N,select = Freq)
vfreq05N <-as.data.frame(t(vfreq05N))

vfreq09N <- as.data.frame(prop.table(table(v2009$Norway_Code)))
vfreq09N <- full_join(data,vfreq09N)
vfreq09N[is.na(vfreq09N)] <- 0
vfreq09N <- subset(vfreq09N,select = Freq)
vfreq09N <-as.data.frame(t(vfreq09N))

vfreq13N <- as.data.frame(prop.table(table(v2013$Norway_Code)))
vfreq13N <- full_join(data,vfreq13N)
vfreq13N[is.na(vfreq13N)] <- 0
vfreq13N <- subset(vfreq13N,select = Freq)
vfreq13N <-as.data.frame(t(vfreq13N))

vfreq17N <- as.data.frame(prop.table(table(v2017$Norway_Code)))
vfreq17N <- full_join(data,vfreq17N)
vfreq17N[is.na(vfreq17N)] <- 0
vfreq17N <- subset(vfreq17N,select = Freq)
vfreq17N <-as.data.frame(t(vfreq17N))

vfreq21N <- as.data.frame(prop.table(table(v2021$Norway_Code)))
vfreq21N <- full_join(data,vfreq21N)
vfreq21N[is.na(vfreq21N)] <- 0
vfreq21N <- subset(vfreq21N,select = Freq)
vfreq21N <-as.data.frame(t(vfreq21N))

freqNv <- full_join(vfreq85N,vfreq89N)
freqNv <- full_join(freqNv,vfreq93N)
freqNv <- full_join(freqNv,vfreq97N)
freqNv <- full_join(freqNv,vfreq01N)
freqNv <- full_join(freqNv,vfreq05N)
freqNv <- full_join(freqNv,vfreq09N)
freqNv <- full_join(freqNv,vfreq13N)
freqNv <- full_join(freqNv,vfreq17N)
freqNv <- full_join(freqNv,vfreq21N)

kpfreq01N <- as.data.frame(prop.table(table(kp2001$Norway_Code)))
kpfreq01N <- full_join(data,kpfreq01N)
kpfreq01N[is.na(kpfreq01N)] <- 0
kpfreq01N <- subset(kpfreq01N,select = Freq)
kpfreq01N <-as.data.frame(t(kpfreq01N))

kpfreq05N <- as.data.frame(prop.table(table(kp2005$Norway_Code)))
kpfreq05N <- full_join(data,kpfreq05N)
kpfreq05N[is.na(kpfreq05N)] <- 0
kpfreq05N <- subset(kpfreq05N,select = Freq)
kpfreq05N <-as.data.frame(t(kpfreq05N))

kpfreq09N <- as.data.frame(prop.table(table(kp2009$Norway_Code)))
kpfreq09N <- full_join(data,kpfreq09N)
kpfreq09N[is.na(kpfreq09N)] <- 0
kpfreq09N <- subset(kpfreq09N,select = Freq)
kpfreq09N <-as.data.frame(t(kpfreq09N))

kpfreq13N <- as.data.frame(prop.table(table(kp2013$Norway_Code)))
kpfreq13N <- full_join(data,kpfreq13N)
kpfreq13N[is.na(kpfreq13N)] <- 0
kpfreq13N <- subset(kpfreq13N,select = Freq)
kpfreq13N <-as.data.frame(t(kpfreq13N))

kpfreq17N <- as.data.frame(prop.table(table(kp2017$Norway_Code)))
kpfreq17N <- full_join(data,kpfreq17N)
kpfreq17N[is.na(kpfreq17N)] <- 0
kpfreq17N <- subset(kpfreq17N,select = Freq)
kpfreq17N <-as.data.frame(t(kpfreq17N))

kpfreq21N <- as.data.frame(prop.table(table(kp2021$Norway_Code)))
kpfreq21N <- full_join(data,kpfreq21N)
kpfreq21N[is.na(kpfreq21N)] <- 0
kpfreq21N <- subset(kpfreq21N,select = Freq)
kpfreq21N <-as.data.frame(t(kpfreq21N))

freqNkp <- full_join(kpfreq01N,kpfreq05N)
freqNkp <- full_join(freqNkp,kpfreq09N)
freqNkp <- full_join(freqNkp,kpfreq13N)
freqNkp <- full_join(freqNkp,kpfreq17N)
freqNkp <- full_join(freqNkp,kpfreq21N)

hfreq01N <- as.data.frame(prop.table(table(h2001$Norway_Code)))
hfreq01N <- full_join(data,hfreq01N)
hfreq01N[is.na(hfreq01N)] <- 0
hfreq01N <- subset(hfreq01N,select = Freq)
hfreq01N <-as.data.frame(t(hfreq01N))

hfreq05N <- as.data.frame(prop.table(table(h2005$Norway_Code)))
hfreq05N <- full_join(data,hfreq05N)
hfreq05N[is.na(hfreq05N)] <- 0
hfreq05N <- subset(hfreq05N,select = Freq)
hfreq05N <-as.data.frame(t(hfreq05N))

hfreq09N <- as.data.frame(prop.table(table(h2009$Norway_Code)))
hfreq09N <- full_join(data,hfreq09N)
hfreq09N[is.na(hfreq09N)] <- 0
hfreq09N <- subset(hfreq09N,select = Freq)
hfreq09N <-as.data.frame(t(hfreq09N))

hfreq13N <- as.data.frame(prop.table(table(h2013$Norway_Code)))
hfreq13N <- full_join(data,hfreq13N)
hfreq13N[is.na(hfreq13N)] <- 0
hfreq13N <- subset(hfreq13N,select = Freq)
hfreq13N <-as.data.frame(t(hfreq13N))

hfreq17N <- as.data.frame(prop.table(table(h2017$Norway_Code)))
hfreq17N <- full_join(data,hfreq17N)
hfreq17N[is.na(hfreq17N)] <- 0
hfreq17N <- subset(hfreq17N,select = Freq)
hfreq17N <-as.data.frame(t(hfreq17N))

hfreq21N <- as.data.frame(prop.table(table(h2021$Norway_Code)))
hfreq21N <- full_join(data,hfreq21N)
hfreq21N[is.na(hfreq21N)] <- 0
hfreq21N <- subset(hfreq21N,select = Freq)
hfreq21N <-as.data.frame(t(hfreq21N))

freqNh <- full_join(hfreq01N,hfreq05N)
freqNh <- full_join(freqNh,hfreq09N)
freqNh <- full_join(freqNh,hfreq13N)
freqNh <- full_join(freqNh,hfreq17N)
freqNh <- full_join(freqNh,hfreq21N)

year <- c(1985,1989,1993,1997,2001,2005,2009,
          2013,2017,2021)

freqNap <- cbind(year,freqNap)
freqNfrp <- cbind(year,freqNfrp)
freqNv <- cbind(year,freqNv)

year <- c(1993,1997,2001,2005,2009,2013,2017,2021)
freqNmdg <- cbind(year,freqNmdg)

year <- c(2001,2005,2009,2013,2017,2021)
freqNkp <- cbind(year,freqNkp)
freqNh <- cbind(year,freqNh)

party <- "AP"
freqNap <- cbind(party,freqNap)
party <- "FRP"
freqNfrp <- cbind(party,freqNfrp)
party <- "MDG"
freqNmdg <- cbind(party,freqNmdg)
party <- "V"
freqNv <- cbind(party,freqNv)
party <- "KP"
freqNkp <- cbind(party,freqNkp)
party <- "H"
freqNh <- cbind(party,freqNh)

freqN <- full_join(freqNap,freqNfrp)
freqN <- full_join(freqN, freqNmdg)
freqN <- full_join(freqN, freqNv)
freqN <- full_join(freqN,freqNkp)
freqN <- full_join(freqN, freqNh)

freqN <- rename(freqN, `General macroeconomics` = `1`, Inflation =`2`,
                Unemployment = `3`, `Monetary policy` = `4`,`National budget` = `5`,
                Taxes = `6`, Industry = `7`,`Price control` = `8`,
                `Petroleum industry` = `9`,`The Government pension fund` = `10`,
                `Other macroeconomics` =  `11`,`General civil rights` = `12`,
                `Ethnic and racial discrimination` = `13`,
                `Gender and sexual discrimination` = `14`,
                `Age discrimination` = `15`,`Handicap discrimination` = `16`,
                `Voting rights` = `17`,`Freedom of speech,religion and assembly` = `18`,
                `Protection of personal data` = `19`,`Anti-government activities` = `20`,
                `The church of Norway` =  `21`,`Sami and national minorities issues` = `22`,
                `Other civil rights` = `23`,`General Health` = `24`,
                `Healthcare reform` = `25`,`Public healthcare coverage` = `26`,
                `Regulation of drug industry and treatment` = `27`,
                `Healthcare facilities` = `28`,`Agreements between the public health insurrance and private health suppliers` = `29`,
                `Medical malpractice` = `30`,`Health manpower` = `31`,
                `Prevention` = `32`,`Children and infants` = `33`,
                `Mental illness` = `34`,`Long-term care and treatment` = `35`,
                `Prescription drug coverage` = `36`,`Tobacco` = `37`,
                `Alcohol and controlled illegal drugs` = `38`,
                `Research and development within healthcare` = `39`,
                `Other health` = `40`,`General agriculture` = `41`,
                `Agricultural exports and imports` = `42`,
                `Subsidies and regulation on agriculture` = `43`,
                `Food policy` = `44`,`Agricultural marketing and promotion` = `45`,
                `Animal and crop diseases` = `46`,`Welfare of livestock` = `47`,
                `Environmental problems related to agriculture` = `48`,
                `Fisheries policy` = `49`,`Aquaculture industry` = `50`,
                `Agricultural research and development` = `51`,`Other agriculture` = `52`, 
                `General labour` = `53`,`Working environment` = `54`,
                `Active labour market policy` = `55`,`Personal employee benefits` = `56`,
                `General labour market questions on trade unions` = `57`,
                `Specific private market collective bargain questions` = `58`,
                `Child labour` = `59`,`Market related transfer payments` = `60`,
                `Questions on the employment situation within specific industries` = `61`,
                `Migrant and seasonal workers` = `62`,`Other labour` = `63`,
                `General education` = `64`,`Higher education` = `65`,`Primary school and vocational upper secondary education` = `66`,
                `Education of underprivileged students` = `67`,
                `Vocational post-secondary education` = `68`,
                `Special education` = `69`,`Libraries and improvement of the level of education`=  `70`,
                `Research and development on education` = `71`,
                `Other education` = `72`,`General environment` = `73`,
                `Drinking water` = `74`,`Waste disposal` = `75`,
                `Hazardous substance` = `76`,`Climate change` = `77`,
                `Recycling` = `78`,`Indoor environmental hazards` = `79`,
                `Animal and plant life protection` = `80`,`Marine environment` = `81`,
                `Freshwater environment` = `82`,`Spatial planning` = `83`,
                `Environmental research and development` = `84`,
                `Other environment` = `85`,`General energy` = `86`,`Nuclear energy` = `87`,
                `Electricity and hydroelectricity` = `88`,`Oil and natural gas` = `89`,
                `Heat supply` = `90`,`Coal` = `91`,`Alternative energy` = `92`,
                `Energy conservation` = `93`,`Research and development on energy` = `94`,
                `Other energy` = `95`,`All questions on immigration` = `96`,
                `General traffic` = `97`,`Mass transport` = `98`,
                `Road construction and motor vehicles` = `99`,
                `Airports and air traffic` = `100`,`Railroads` = `101`,
                `Maritime issues` = `102`,`Shipbuilding industry` = `103`,
                `Public works and transport facillities` = `104`,
                `Research and development on traffic` = `105`,`Other traffic` = `106`,
                `General legal affairs` = `107`,
                `Authorities dealing with crime and gun control` = `108`,
                `Financial and oranized crime` = `109`,`Drug-related crime` = `110`,
                `Court system` = `111`,`Prison system` = `112`,
                `Juvenile crime` = `113`,`Child abuse` = `114`,
                `Family issues` = `115`,`Criminal code issues` = `116`,
                `Crime prevention` = `117`,`Domestic security responses to terrorism` = `118`,
                `Other legal affairs` = `119`,`General social policy` = `120`, `Food assistance` = `121`,
                `Social security benefit` = `122`,`Elderly issues` = `123`,
                `Assistance to the disabled and handicapped` = `124`,
                `Volunteer associations` = `125`,`Parental leave and childcare` = `126`,
                `Other social policy` = `127`,`General urban and housing` = `128`,
                `Urban housing issues` = `129`,`Urban development` = `130`,
                `Rural housing issues` = `131`,`Rural development` = `132`,
                `Low-income housing issues` = `133`,
                `Elderly and handicapped housing` = `134`,`Homeless issues` = `135`,
                `Housing marked issues for homeowners` = `136`, `Reseach and development on building techiques` = `137`,
                `Other housing` = `138`,`General industrial and comercial` = `139`,
                `Banking` = `140`,`Securities and investments` = `141`,
                `Mortgages and credit cards` = `142`,`Insurrance issues` = `143`,
                `Bankruptcy` = `144`,`Antitrust legislation` = `145`,
                `Small and medium-sized businesses` = `146`,
                `Copyright and patents` = `147`,`Domestic disaster relief` = `148`,
                `Tourism` = `149`,`Consumer policy` = `150`,
                `Sports and gambling regulation` = `151`,
                `Other industial and comercial` = `152`,`General defence` = `153`,
                `Security policy and defence alliances` = `154`,
                `Military intelligence and espionage` = `155`,
                `Military readiness` = `156`,`Arms control` = `157`,
                `Military aid and weapons sales` = `158`,
                `Military personell and families` = `159`,
                `Military procurment and weapons system aquisitions` = `160`,
                `Military installations` = `161`,`The Home Guard` = `162`,
                `Environmental problems caused by military activities` = `163`,
                `Civil defence` = `164`,`Civilian personnel in the Armed Forces and consequences of military issues for civilians` =  `165`,
                `Oversight of defence contractors` = `166`,
                `Issues directly related to war` = `167`,
                `Claims against the Norwegian Armed Forces` = `168`,
                `Research and development on defence` = `169`,
                `Other defence` = `170`,`General research, technology and communications` = `171`,
                `Space travel` =  `172`,`Commercial use of space and satellites` = `173`,
                `Scientific cooperation` = `174`,`Telecommunication and telephone services` = `175`,
                `Media` = `176`,`Weather forecasting` = `177`,
                `Computer industry and computer security` = `178`,
                `Research and research policy` = `179`,
                `Other research, techonology and communications` = `180`,
                `General foreign trade` = `181`,`Free trade agreements` = `182`,
                `Export promotion and regulation` = `183`,
                `Foreign investments in Norway and Norwegian company investments abroad` = `184`,
                `Competitiveness and the balance of payment` = `185`,
                `Tariffs, imports and import regulation` = `186`,
                `Echange rates` = `187`,`Other foreign trade` = `188`,
                `General foreign policy` = `189`,`Foreign aid` = `190`,
                `Global environmental problems` = `191`,
                `Developing countries` = `192`,`International finance` = `193`,
                `China` = `194`,`Russia, USSR and former Soviet republics` = `195`,
                `Eastern Europe` = `196`,`EU/EEA` = `197`,`Africa` = `198`,
                `South Africa` = `199`,`Western Europe and Scandinavia` = `200`,
                `Latin and Central America` = `201`,`Canal issues` = `202`,
                `Northern America` = `203`,`Asia and Occeania` = `204`,
                `Middle East` = `205`,`Human rights` = `206`,
                `International organisations` = `207`,
                `International terrorism` = `208`,
                `Norwegians abroad and border control` = `209`,
                `Other international policy` = `210`,
                `General governmental operations` = `211`,
                `Relations between state, municipalities and counties` = `212`,
                `Government efficiency` = `213`,`Postal services` = `214`,
                `Public servants` = `215`,`Nominations and appointments` = `216`,
                `Medals, orders and currency` = `217`,
                `Government procurment, outsourcing and contracts` = `218`,
                `Public buildings and property and general privatization issues` = `219`,
                `The tax administration` = `220`,
                `Prime Minister impeachment` = `221`,
                `Relations between parliament and other ministers` = `222`,
                `Regulation of elections and election campaigns` = `223`,
                `Claims against the government` = `224`,
                `Regulation and control over municipalities and counties` = `225`,
                `National holidays` = `226`,`Other governmental issues` = `227`,
                `General public lands and water management` = `228`,
                `National parks` = `229`,`Indigenous affairs` = `230`,
                `Use of public natural resources` = `231`,
                `Water and sea resources including harbours` = `232`,
                `Svalbard and other territories` = `233`,
                `Other public lands issues` = `234`,`General culture` = `235`)

write.xlsx(freqN, "C:/Users/Eier/OneDrive/Dokumenter/Notater_til_Master/Proportional data with subtopics.xlsx")

#### Creating a graph showing FrP's attention to taxes

freqN %>%
  filter(party == "FRP") %>% ggplot(aes(year, Taxes)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Election year",
       title = "Graph 6.7. The Progress party's attention to tax policy over time",
       y = "Tax policy") +
  scale_x_continuous(breaks = c(1985,1989,1993,1997,2001,2005,2009,
                                2013,2017,2021)) +
  scale_y_continuous(limits = c(0.019,0.08))

#### Showing the proportion of coastal policy ####

freqN$Coast <- freqN$`Fisheries policy` + freqN$`Aquaculture industry` +
  freqN$`Marine environment` + freqN$`Maritime issues` + freqN$`Shipbuilding industry` +
  freqN$`Water and sea resources including harbours`

ggplot(freqN, aes(year,Coast, colour = party)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Election year",
       y = "Coastal policy",
       title = "Graph 6.12. The proportion of coastal policies in manifestos") +
  scale_colour_manual(values = c("AP" = "red3", "FRP" = "mediumblue",
                                 "MDG" = "chartreuse", "V" = "seagreen4",
                                 "KP" = "black", "H" = "deepskyblue2")) +
  scale_x_continuous(breaks = c(2001,2005,2009,2013,2017,2021),
                     limits = c(2001,2021)) +
  theme(plot.title = element_text(hjust = 0.5))
