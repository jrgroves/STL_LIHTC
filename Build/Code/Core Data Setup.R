#Connect the LIHTC data to the spaital parcel data
#Jeremy R. Groves
#Created: July 14, 2021

rm(list=ls())

library(sf)
library(tidyverse)
library(foreign)

#FUNCTIONS
diff1<-function(x){
  temp<-x %>%
    select(PARID, HUDID, YRPINS)
  temp<-merge(temp, sales,by="PARID", all.x=TRUE)
  
  temp<-temp %>%
    select(-SALETYPE, -SALEVAL, -geometry) %>%
    mutate(SALEYR = format(SALEDT, format="%Y")) %>%
    mutate(pre = case_when(
      SALEYR < YRPINS ~ 1,
      TRUE ~ 0)) %>%
    select(PARID, HUDID, YRPINS, SALEDT, r_PRICE, pre) 
  
  temp$geometry<-NULL
  
  temp<-temp %>%
    arrange(PARID, SALEDT)  %>%
    group_by(PARID, HUDID) %>%
    mutate(l_PRICE = lag(r_PRICE),
           l_pre = lag(pre),
           l_SALEDT = lag(SALEDT),
           d_PRICE = r_PRICE - l_PRICE,
           d_pre = l_pre - pre) %>%
    subset(!is.na(l_PRICE))
  return(temp)
}  

#Create Maps
stl<-st_read("./Build/Input/Parcels.shp")

stl2<-stl %>%
    select(LOCATOR, OWNER_NAME, PROP_ADRNU, PROP_ADD, PROP_ZIP,OWN_ADD, OWN_CITY,
           OWN_STATE, OWN_ZIP, SCHSUB, MUNYCODE, PROPCLASS, LUC, LUCODE, TENURE, 
           LIVUNIT, YEARBLT, RESQFT, CENSUS_TRA, CENSUS_BLO, ZONING, MUNI_ZONIN,
           geometry) %>%
    mutate(PARID = LOCATOR) 
stl2<-st_make_valid(stl2)

STL<-stl2 %>%
  summarize(ID = "STL")

#Read in sales data from repeat sales
load("./Build/Input/Master Sales.RData")

    #Removes very low dollar amounts and takes the most recent of repeated sales observations
    sales<-sales %>%
      subset(PRICE>19999)%>%
      arrange(PARID,SALEDT,PRICE) %>%
      group_by(PARID,SALEDT) %>%
      filter(!is.na(SALETYPE)) %>%
      mutate(ticker=row_number()) %>%
      mutate(num = max(ticker)) %>%
      filter(ticker==num) %>%
      select(PARID, SALEDT, PRICE, SALETYPE, SALEVAL) %>%
      mutate(DATE = format(SALEDT, format="%Y-%m-01"))
    
    #Adjust prices to 2021
      cpi<-read.csv("./Build/Input/CPIAUCSL.csv", as.is=TRUE, header=TRUE)
         cpi$adj<-270.981 / cpi$CPIAUCSL  #Sets prices to June 2021
      sales<-merge(sales, cpi, by="DATE", all.x=TRUE)
      sales <- sales %>%
        mutate(r_PRICE = PRICE * adj) %>%
          select(-CPIAUCSL, -adj, -DATE)
    
    #Pulls out PARIDs with more than one sale present in data
    rep<-sales %>%
      group_by(PARID) %>%
      count(PARID) %>%
      subset(n>1)  
    
    #Merges with GIS data
    rep.sale<-merge(stl2, rep, by="PARID")
    
    rep.sale<-rep.sale %>%
      select(PARID,geometry) 
        
#reew
      
#Read in the LIHTC data
LIH<-read.csv("./Build/Input/LIHTC.csv", as.is=TRUE, header=TRUE)

lih <- LIH %>%
    select(PARID, HUDID, YRPINS) %>%
    mutate(LIHTC = 1) %>%
    subset(PARID!="")

#Intersect LHITC with GIS data
stl2<-merge(stl2, lih, by="PARID", all=TRUE)
  stl2$LIHTC[is.na(stl2$LIHTC)]<-0
  
#Create Buffers around LHITC properties
lih.map<-stl2 %>%
    subset(LIHTC==1) %>%
      select(PARID, LIHTC, geometry)%>%
        rename(PARID.LIHTC=PARID)

buffer.1000ft<-st_buffer(lih.map, 1000)
buffer.half<-st_buffer(lih.map, 2640)
buffer.one<-st_buffer(lih.map, 5280)
buffer.onehalf<-st_buffer(lih.map, 7920)
buffer.two<-st_buffer(lih.map, 10560)

#Intersect the buffers with the repeated sales data

inter.1000ft<-st_intersection(buffer.1000ft, rep.sale)
inter.half<-st_intersection(buffer.half, rep.sale)
inter.one<-st_intersection(buffer.one, rep.sale)
inter.onehalf<-st_intersection(buffer.onehalf, rep.sale)
inter.two<-st_intersection(buffer.two, rep.sale)

#Merge the ring IDs with the LIHTC data
inter.1000ft<-merge(inter.1000ft, lih, by.x="PARID.LIHTC", by.y="PARID", all.x=TRUE)
inter.half<-merge(inter.half, lih, by.x="PARID.LIHTC", by.y="PARID", all.x=TRUE)
inter.one<-merge(inter.one, lih, by.x="PARID.LIHTC", by.y="PARID", all.x=TRUE)
inter.onehalf<-merge(inter.onehalf, lih, by.x="PARID.LIHTC", by.y="PARID", all.x=TRUE)
inter.two<-merge(inter.two, lih, by.x="PARID.LIHTC", by.y="PARID", all.x=TRUE)  

#Pull in sales data and find the differences in sale prices along with which are sold 
#Before and After an LIHTC property

ring1<-diff1(inter.1000ft)
ring2<-diff1(inter.half)
ring3<-diff1(inter.one)               
ring4<-diff1(inter.onehalf) 
ring5<-diff1(inter.two)  

