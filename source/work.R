setwd("/Users/school/INFO-201code/projects/a3-JohnnyCarroll22")
incarceration_trends_jail_jurisdiction <- read.csv("~/INFO-201code/projects/a3-JohnnyCarroll22/incarceration-trends/incarceration_trends_jail_jurisdiction.csv")
incarceration_trends <- read.csv("~/INFO-201code/projects/a3-JohnnyCarroll22/incarceration-trends/incarceration_trends.csv", header=TRUE)
library("tidyverse")
library("plotly")
library("dplyr")
library("leaflet")
library("knitr")

#isolating California

ca_incar<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000'| year=='2001'| year=='2002'| year=='2003'| year=='2004'| year=='2005'|year=='2006'| year=='2007'| year=='2008'| year=='2009'| year=='2010')
#View(ca_incar)

#identifying change over time  

ca_incar2000<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000')

ca_incar2010<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2010')

ten_yr_dif<-ca_incar2010-ca_incar2000

blackdif<-sum(ten_yr_dif$black_jail_pop,na.rm = TRUE)
whitedif<-sum(ten_yr_dif$white_jail_pop,na.rm = TRUE)

#Urban vs rural
urban<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000'| year=='2001'| year=='2002'| year=='2003'| year=='2004'| year=='2005'|year=='2006'| year=='2007'| year=='2008'| year=='2009'| year=='2010') %>%
  filter(urbanicity=='urban')

urban_prop_black=(sum(urban$black_jail_pop)/sum(urban$total_jail_pop))*100
urban_prop_white=(sum(urban$white_jail_pop)/sum(urban$total_jail_pop))*100



rural<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000'| year=='2001'| year=='2002'| year=='2003'| year=='2004'| year=='2005'|year=='2006'| year=='2007'| year=='2008'| year=='2009'| year=='2010') %>%
  filter(urbanicity=='rural')

rural[is.na(rural)] = 0

#View(rural)


rural_prop_black=(sum(rural$black_jail_pop)/sum(rural$total_jail_pop,na.rm=TRUE))
rural_prop_white=(sum(rural$white_jail_pop)/sum(rural$total_jail_pop,na.rm=TRUE))

#PLOTS
ca_incar2<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000'| year=='2001'| year=='2002'| year=='2003'| year=='2004'| year=='2005'|year=='2006'| year=='2007'| year=='2008'| year=='2009'| year=='2010')%>%
  filter(county_name=='San Mateo County'|county_name=='San Diego County')
#View(ca_incar2)

#plot 1
fig <- plot_ly(
  data = ca_incar2,     
  x = ~year, 
  y = ~black_jail_pop, 
  color = ~county_name, 
  type = "scatter", 
  mode = "lines" 
)%>%
  layout(
    title = "Incarceration of African Americans In Two Major CA Counties",
    xaxis = list(title = "Year"), 
    yaxis = list(title = "Number of Black Inmates") 
  )


#plot 2
ca_incar3<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000')

fig2<- plot_ly(
  data = ca_incar3 ,    
  x = ~white_jail_pop , 
  y = ~white_pop_15to64 ,
  color = ~urbanicity ,
  type = "scatter"
  )%>%
  layout(
    title = "White Population vs White Inmates: CA 2000",
    xaxis = list(title = "Number of White Inmates"), 
    yaxis = list(title = "Number of White People in a Population") 
  )

fig3<- plot_ly(
  data = ca_incar3 ,    
  x = ~black_jail_pop , 
  y = ~black_pop_15to64 ,
  color = ~urbanicity ,
  type = "scatter"
)%>%
  layout(
    title = "Black Population vs Black Inmates: CA 2000",
    xaxis = list(title = "Number of Black Inmates"), 
    yaxis = list(title = "Number of Black People in a Population") 
  )
#MAP
prop_black<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000')%>%
  filter(county_name=='Los Angeles County'|county_name=='San Mateo County'|county_name=='San Diego County'|county_name=='Orange County')%>%
  mutate(prop_black=((black_jail_pop/total_jail_pop)*100))
#View(prop_black)
    
ca_incar3<-incarceration_trends %>%
  filter(state=="CA")%>%
  filter(year=='2000')%>%
  filter(county_name=='Los Angeles County'|county_name=='San Mateo County'|county_name=='San Diego County'|county_name=='Orange County')
  #View(ca_incar3)
  
lat<-c(34.0522,33.7175,32.7157,37.4337)
prop_black$lat=lat
long<-c(-118.2437,-117.8311,-117.1611,-122.4014)
prop_black$long=long

map <- leaflet(data = prop_black) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = prop_black$lat ,
    lng = prop_black$long ,
    popup = paste(prop_black$county_name) ,
    radius = prop_black$prop_black)



  
  