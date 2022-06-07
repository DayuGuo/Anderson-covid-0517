library(jsonlite)
library(rjson)
library(tidyverse)
library(lubridate)
library(ggrepel)
#rm(list = ls())

#cfile=list.files(pattern = "csv")

#df=tibble()
#for (i in 1:length(cfile)) {
#  x=read.csv(cfile[i],header = T)
#  df=x %>% bind_rows(df)
#}


## non-omicron
#df1=df %>% filter(time<as.Date("2022-01-18")) %>% as_tibble() %>% 
#  mutate(Strength_covid=ifelse(Strength_covid==0,0.001,Strength_covid),
#    x=log(Strength_flu),
#         y=log(Strength_covid))

#write.csv(df1,"df1.csv")


## plot
df1 <- read_csv("df1.csv")

test <- df1 %>%
  mutate(
    `Groups` = case_when(
      `Groups` == "UC" ~ "Under control",
      `Groups` == "OC" ~ "Out of control",
      TRUE ~ `Groups` # All the other remain the same
    )
  )

p1=ggplot(df1,aes(x=x,y=y,label=name))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
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
  labs(x="Population flow index control",y="COVID-19 outbreak control with cases ratio")+
  theme_bw()

## non-omicron

ggsave("caseR-A.pdf",width = 12,height = 6,dpi = 500)


#df2=df %>% filter(time>as.Date("2022-01-17")) %>% as_tibble() %>% 
#  mutate(Strength_covid=ifelse(Strength_covid==0,0.001,Strength_covid),
#         x=log(Strength_flu),
#         y=log(Strength_covid)) %>% 
#  filter(!name %in% c("Jiamusi","Hangzhou",
#                      "Zhengzhou",
#                      "Dezhou"))

#write.csv(df2,"df2.csv")


## plot
df2 <- read_csv("df2.csv")

test <- df1 %>%
  mutate(
    `Groups` = case_when(
      `Groups` == "UC" ~ "Under control",
      `Groups` == "OC" ~ "Out of control",
      TRUE ~ `Groups` # All the other remain the same
    )
  )

p2=ggplot(df2,aes(x=x,y=y,label=name))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
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
  labs(x="Population flow index control",y="COVID-19 outbreak control with cases ratio")+
  theme_bw()

p2=p2+plot_annotation(tag_levels = 'A')
ggsave("caseR-B.pdf",width = 12,height = 6,dpi = 500)


library(patchwork)
p1+p2+plot_annotation(tag_levels = 'A')
ggsave("Omicron-two.pdf",width = 12,height = 12,dpi = 500)



dfp=df1 %>% mutate(type="Non-Omicron") %>% 
  bind_rows(df2 %>% mutate(type="Omicron"))


ggplot(dfp,aes(x=x,y=y,label=name,color=type))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  geom_point()+
  guides(color = guide_legend(title = NULL))+
  geom_smooth(method = lm)+
  labs(x="Population flow index control",y="COVID-19 case control")+
  theme_bw()

ggsave("Omicron-all.pdf",width = 8,height = 6,dpi = 500)


ggplot(dfp,aes(x=y,y=x,label=name,color=type))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  geom_point()+
  guides(color = guide_legend(title = NULL))+
  geom_smooth(method = lm)+
  labs(x="Population flow index control",y="COVID-19 case control")+
  theme_bw()

ggsave("Omicron-all.pdf",width = 8,height = 6,dpi = 500)



#some tweaking

fit1=glm(y~x,data = df1)
summary(fit1)


fit2=glm(y~x,data = df2)
summary(fit2)


fit2=glm(y~x,data = df2 %>% filter(y>-5))
summary(fit2)



summary(fit1) # display results
confint(fit1) # 95% CI for the coefficients
exp(coef(fit2)) # exponentiated coefficients
exp(confint(fit2)) # 95% CI for exponentiated coefficients



library(ggpubr)

ggboxplot(dfp, x = "type", y = "during_case",
          color = "type", palette = "jco")+
  stat_compare_means(method = "t.test")+
  labs(y="Days during the outbreak",x="")+
  guides(color = guide_legend(title = NULL))+
  theme_bw()

dfp %>% group_by(type) %>% 
  summarise(mean=mean(during_case),
            sd=sd(during_case))


x=df1 %>% filter(during_index<100) %>% 
  slice(-20)
mean(x$during_index)




x=df1 %>% 
  slice(-3,-13)
cor.test(x$x,x$y)


#some tweaking

fit1=glm(x~y,data = df1)
summary(fit1)

0.0197 

fit2=glm(x~y,data = df2)
summary(fit2)
0.05657

set.seed(123)
x=rnorm(100,-2.5,2)
y1=0.9400073*x-0.502

y2=1.022451*x+1.259

dfx=tibble(x=x,y1=y1,y2=y2) %>% 
  gather("type","y",-x)
  

fit2=glm(y1~x,data = dfx)
summary(fit2)


fit2=glm(x~y1,data = dfx)
summary(fit2)

fit1=glm(x~y2,data = dfx)
summary(fit1)



