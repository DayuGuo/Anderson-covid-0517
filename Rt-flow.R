library(jsonlite)
library(rjson)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
rm(list = ls())

setwd("~/Github-database/Anderson-covid-0517")

#load("Rtout.Rdata")
#load("COVID-19.RData")
#output=read.csv("/Users/Anderson/Desktop/PhDwork/Py-Github/Scarping/weijianwei/output523.csv",header = T) %>% 
#  select(-1) %>% mutate(time=as.Date(time))


## non-omicron
#df11=output %>% filter(time<as.Date("2022-01-18")) %>% as_tibble() %>% 
#  mutate(Strength_covid=ifelse(Strength_covid==0,0.001,Strength_covid),
#         x=log(Strength_flu),
#         y=(Rt_rate))



load("~/Github-database/Anderson-covid-0517/Rt-flow.RData")
## plot
#write.csv(df11,"df11.csv")
df11 <- read_csv("df11.csv")

test <- df11 %>%
  mutate(
    `Groups` = case_when(
      `Groups` == "OC" ~ "Out of control",
      `Groups` == "UC" ~ "Under control",
      TRUE ~ `Groups` # All the other remain the same
    )
  )
library("ggsci")
library("ggplot2")
library("gridExtra")

p11=ggplot(df11,aes(x=x,y=y,label=name))+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  geom_point(
color = "#68D5D5", fill="#68D5D5",
    size = 2.5, alpha = 0.5, 
    shape = 21
  )+
  geom_label_repel(aes(label = name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+ 
  geom_smooth(aes(x,y),method = lm)+
  labs(x="Population flow index control",
       y=quote("COVID-19 outbreak control with"~italic(R)[t]))  +
  theme_bw()

ggsave("RT-A.pdf",width = 12,height = 6,dpi = 500)



## non-omicron
#df22=output %>% filter(time>as.Date("2022-01-17")) %>% as_tibble() %>% 
#  mutate(Strength_covid=ifelse(Strength_covid==0,0.001,Strength_covid),
#         x=log(Strength_flu),
#         y=(Rt_rate))



# write.csv(df22,"df22.csv")

df22 <- read_csv("df22.csv")

## plot
test <- df22 %>%
  mutate(
    `Groups` = case_when(
      `Groups` == "UC" ~ "Under control",
      `Groups` == "OC" ~ "Out of control",
      TRUE ~ `Groups` # All the other remain the same
    )
  )

p22=ggplot(df22,aes(x=x,y=y,label=name))+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  geom_point(
    aes(color = `Groups`, fill = `Groups`),
    size = 2.5, alpha = 0.5, 
    shape = 21
  )+
  geom_label_repel(aes(label = name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+ 
  geom_smooth(aes(x,y),method = lm)+
  labs(x="Population flow index control",
       y=quote("COVID-19 outbreak control with"~italic(R)[t]))  +
  theme_bw()

ggsave("Rt-B.pdf",width = 12,height = 6)



fit11=glm(y~x,data = df11%>% filter(!y==Inf))
summary(fit11)

fit22=glm(y~x,data = df22)
summary(fit22)

