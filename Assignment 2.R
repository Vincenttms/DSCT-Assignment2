#Libraries
require(dplyr)
require(tidyr)
require(ggplot2)
require(reshape2)
require(scales)
require(gridExtra)

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)

#Import Data from CSV
gci <- read.csv("D:\\Downloads\\Telegram Desktop\\DSCT-EDA-And-Visualisation-master\\GCI_dataset.csv", header = TRUE, sep=";")

#EDA
gci <- gci %>%
  gather(Country,Value,-Edition,-Series.code,-Series,-Attribute) %>%
  spread(Attribute, Value) %>%
  separate(Edition, c("Year", "till"))

gci <- gci[!(gci$Series.code == 10.03 |
                       gci$Series.code == 3.02 |
                       gci$Series.code == 3.04 |
                       gci$Series.code == 4.06 |
                       gci$Series.code == 6.09 |
                       gci$Series.code == 6.13 |
                       gci$Series.code == 6.14 |
                       gci$Series.code == 10.04 |
                       gci$Series.code == 0.02 |
                       gci$Series.code == 0.04
),]


#Data Cleaning
gci$Value[gci$Series.code == "4.05" & gci$Value=="<0.1"] <- 0
gci$Value <- as.numeric(gci$Value)
gci$Rank <- as.numeric(gci$Rank)
gci$Year <- as.numeric(gci$Year)
gci$till <- as.numeric(gci$till)

#Clean 0.01
gci$Value[gci$Series.code == 0.01 & gci$Year== 2008 & gci$Country=="KHM"] <-8604/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2008 & gci$Country=="IDN"] <-432944/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2008 & gci$Country=="MYS"] <-186482/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2008 & gci$Country=="PHL"] <-144129/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2008 & gci$Country=="SGP"] <-161349/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2008 & gci$Country=="THA"] <-245659/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2008 & gci$Country=="VNM"] <-70022/1000

gci$Value[gci$Series.code == 0.01 & gci$Year== 2009 & gci$Country=="KHM"] <-11182/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2009 & gci$Country=="IDN"] <-511765/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2009 & gci$Country=="MYS"] <-222219/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2009 & gci$Country=="PHL"] <-168580/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2009 & gci$Country=="SGP"] <-181939/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2009 & gci$Country=="THA"] <-273248/1000
gci$Value[gci$Series.code == 0.01 & gci$Year== 2009 & gci$Country=="VNM"] <-89829/1000


gci1.03 <- gci %>%
  filter(Series.code == 1.03)

gci1.05 <- gci %>%
  filter(Series.code == 1.05)
 
gci1.16 <- gci %>%
  filter(Series.code == 1.16)
 
gci3.03 <- gci %>%
  filter(Series.code == 3.03)
 
gci4.01 <- gci %>%
  filter(Series.code == 4.01)

gci4.05 <- gci %>%
  filter(Series.code == 4.05)

gci4.10 <- gci %>%
  filter(Series.code == 4.10)

gci5.01 <- gci %>%
  filter(Series.code == 5.01)

gci5.02 <- gci %>%
  filter(Series.code == 5.02)

gci5.03 <- gci %>%
  filter(Series.code == 5.03)

gci8.06 <- gci %>%
  filter(Series.code == 8.06) 

gci10.01 <- gci %>%
  filter(Series.code == 10.01)

gci10.02 <- gci %>%
  filter(Series.code == 10.02)

gci10.03 <- gci %>%
  filter(Series.code == 10.03)

gci0.01 <- gci %>%
  filter(Series.code == 0.01)

gci0.03 <- gci %>%
  filter(Series.code == 0.03)


# To see the general trends in each of the categories
gci %>%
        group_by(Country) %>%
        ggplot(aes(x=Year,y=log(Value), group=1))+
        geom_point(aes(colour=Country))+
        facet_wrap(~Series)+
       geom_smooth(method="auto")



#Question 1 : Trends in Domestic and Foreign market size indices (10.01 & 10.02)
domestic <- ggplot(data = gci10.01, aes(x = Year, Value, group=Country)) + 
  geom_point(aes(color = Country)) + 
  facet_wrap(~Country, ncol = 7) + 
  labs(title='Domestic Trend')  + 
  stat_smooth(se = FALSE, aes(colour = Series))

foreign <- ggplot(data = gci10.02, aes(x = Year, Value, group=Country)) + 
  geom_point(aes(color = Country))+ facet_wrap(~Country, ncol = 7) + 
  labs(title='Foreign Trend') + 
  stat_smooth(se = FALSE, aes(colour = Series)) 

grid.arrange(domestic,foreign)

gci %>%
  filter(Series.code == 10.01 | Series.code == 10.02) %>%
  ggplot(aes(x = Year,y = Value, group = Country))+
  geom_point(aes(color = Series))+
  facet_wrap(~ Country, ncol = 7)+
  geom_smooth(method="lm",se = FALSE, aes(colour = Series))



#Question 2 : How quality of education system affect GDP (5.03 vs 0.01)
gci5.03 <- gci5.03 %>%
  filter(Year != 2006 & Year != 2007 & Year != 2014)

eduQuality<-ggplot(data = gci5.03, aes(x=Year, Value)) + 
  geom_point()+
  facet_wrap(~Country, scales = "free",ncol=1) + 
  labs(title='Quality of Education') + 
  stat_smooth()
 
gdp<-ggplot(data = gci0.01, aes(x=Year, Value)) + 
  geom_point() +
  facet_wrap(~Country, scales = "free",ncol=1) + 
  labs(title='GDP') + 
  stat_smooth()

gdpPerCap<- gci %>%
  filter(Series.code == 0.03) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2014) %>%
  ggplot(aes(x=Year, Value)) + 
  geom_point()+
  facet_wrap(~Country,ncol = 1,scales="free") + 
  labs(title='GDP per capita') + 
  stat_smooth()

#Quality of Education vs GDP
cor.test(gci5.03$Value, gci0.01$Value)

chisq.test(gci5.03$Value, gci0.01$Value)

grid.arrange(eduQuality,gdp,ncol=2)

#Quality of Education vs GDP per Capita
cor.test(gci5.03$Value, gci0.03$Value)

chisq.test(gci5.03$Value, gci0.03$Value)

grid.arrange(eduQuality,gdpPerCap,ncol=2)



#Question 3 : Lower level of education == Higher HIV prevalence for 2006 and 2015
#(4.10, 5.01, 5.02 vs 4.05)
gci4.10 <- gci4.10 %>%
  filter(Year == 2006 | Year == 2014)

priEnrol<-ggplot(data = gci4.10, aes(x=Year, Value, group =1)) + 
  geom_point(aes(color = Country))+
  facet_wrap(~Country, ncol = 7) + 
  labs(title='Primary education enrollment rate')

gci5.01 <- gci5.01 %>%
  filter(Year == 2006 | Year == 2014)

secEnrol<-ggplot(data = gci5.01, aes(x=Year, Value, group =1)) + 
  geom_point(aes(color = Country))+
  facet_wrap(~Country, ncol = 7) + 
  labs(title='Secondary education enrollment rate')

gci5.02 <- gci5.02 %>%
  filter(Year == 2006 | Year == 2014)

terEnrol<-ggplot(data = gci5.02, aes(x=Year, Value, group =1)) + 
  geom_point(aes(color = Country))+
  facet_wrap(~Country, ncol = 7) + 
  labs(title='Tertiary education enrollment rate')

gci4.05 <- gci4.05 %>%
  filter(Year == 2006 | Year == 2014)

hiv<-ggplot(data = gci4.05, aes(x=Year, Value, group =1)) + 
  geom_point(aes(color = Country)) + 
  facet_wrap(~Country, ncol = 7) + 
  labs(title='HIV Pervalance')

cor.test(gci4.05$Value, gci5.01$Value)

chisq.test(gci4.05$Value, gci5.01$Value)



edu<-gci %>%
  filter(Series.code == 5.02 | Series.code == 5.01 | Series.code == 4.10) %>%
  filter(Year == 2006 | Year == 2014)%>%
  ggplot()+
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity",position = "dodge")+
  facet_grid(~ Country)+
  labs(title = "Education levels", x="Year",y="Percentage %")


hiv<-gci %>%
  filter(Series.code == 4.05) %>%
  filter(Year == 2006 | Year == 2014)%>%
  ggplot()+
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity",position = "dodge")+
  facet_grid(~ Country)+
  labs(title = "HIV pervalence", x="Year",y="Percentage %")


grid.arrange(edu,hiv,nrow=2,ncol=1)




#Question 4 : Irregular payments and bribes and diversion of public funds
#lead to lower/higher reliability of police services for the last 5 years
#(1.17 : Ethical behavior of firms ?? 1.08 Wastefulness of government spending?? interesting?
# 1.03 Diversion of public funds, 1.05 Irregular payments and bribes vs 1.16)

publicfunddiversion<-gci %>%
  filter( Series.code == 1.03 ) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)%>%
  ggplot()+
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity")+
  facet_grid(~ Country,scales="free")+
  labs(title = "Diversion of Public funds", x="Year",y="Percentage %")


bribes<-gci %>%
  filter( Series.code == 1.05 ) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)%>%
  ggplot()+
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity")+
  facet_grid(~ Country,scales="free")+
  labs(title = "Irregular payment & bribes", x="Year",y="Percentage %")

policereliability<-gci %>%
  filter( Series.code == 1.16 ) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)%>%
  ggplot()+
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity")+
  facet_grid(~ Country,scales="free")+
  labs(title = "Reliability of police Services", x="Year",y="Percentage %")



publicfunddiversion<-gci %>%
  filter(Series.code == 1.03 | Series.code == 1.16 ) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)%>%
  ggplot()+
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity")+
  facet_grid(~ Country,scales="free")+
  labs(title = "Diversion of Public funds VS Reliability of police Services", x="Year",y="Percentage %")

PFD<-gci %>%
  filter( Series.code == 1.03 ) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)

B<-gci %>%
  filter( Series.code == 1.05 ) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)

PR<-gci %>%
  filter( Series.code == 1.16 ) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)


cor.test(B$Value, PFD$Value)
chisq.test(B$Value, PFD$Value)

cor.test(PFD$Value,PR$Value)
chisq.test(PFD$Value,PR$Value)

bribes<-gci %>%
  filter(Series.code == 1.05 | Series.code == 1.16) %>%
  filter(Year != 2006 & Year != 2007 & Year != 2008 & Year != 2009)%>%
  ggplot()+
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity")+
  facet_grid(~ Country,scales="free")+
  labs(title = "Irregular payment & bribes VS Reliability of police Services", x="Year",y="Percentage %")

cor.test(B$Value, PR$Value)
chisq.test(B$Value, PR$Value)

grid.arrange(publicfunddiversion,bribes,nrow=2,ncol=1)


#Question 5 : How inflation and soundness of banks affected the 
#average income per person for each country
#(3.03 Inflation vs 8.06 Soundness of Banks vs 0.03 GDP per capita)
gci3.03 <- gci3.03 %>%
  filter(Year >= 2008 & Year != 2014)

inflation<-ggplot(data = gci3.03, aes(x=Year, Value, group =1)) + 
  geom_point() + 
  facet_wrap(~Country,ncol = 1,scales="free") + 
  labs(title='Inflation') + 
  stat_smooth(se = FALSE)

gci8.06 <- gci8.06 %>%
  filter(Year >= 2008 & Year != 2014)

banksoundness<-ggplot(data = gci8.06, aes(x=Year, Value, group =1)) + 
  geom_point() + 
  facet_wrap(~Country,ncol = 1,scales="free") + 
  labs(title='Soundness of Banks') + 
  stat_smooth(se = FALSE)

gci0.03 <- gci0.03 %>%
  filter(Year >= 2008 & Year != 2014)

gdpPerCap<-ggplot(data = gci0.03, aes(x=Year, Value, group =1)) + 
  geom_point()+
  facet_wrap(~Country,ncol = 1,scales="free") + 
  labs(title='GDP per capita') + 
  stat_smooth()

grid.arrange(inflation,gdpPerCap,nrow=1,ncol=2)
grid.arrange(banksoundness,gdpPerCap,nrow=1,ncol=2)
grid.arrange(banksoundness,inflation,nrow=1,ncol=2)

grid.arrange(inflation,banksoundness,gdpPerCap,nrow=1,ncol=3)


cor.test(gci3.03$Value, gci0.03$Value)
chisq.test(gci3.03$Value, gci0.03$Value)
cor.test(gci8.06$Value, gci0.03$Value)
chisq.test(gci8.06$Value, gci0.03$Value)
