# Functions for Data Cleaning

library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(foreign)

################
#   FUNCTIONS
################
assign_nums<-function(data)
{
  rows = nrow(data)
  iter = 1
  nums = numeric(rows)
  weeks = numeric(rows)
  # pb = txtProgressBar(min = 1, max = rows, style = 3)
  for(i in 1:rows)
  {
    row = data[i,]
    if (row$diff != 1)
      iter = iter + 1
    nums[i] = iter
    weeks[i] = row$week
  }
  return(list(weeks,nums))
}

#helper function
process_split<-function(df)
{
  store = df$store
  upc = df$upc
  weeks = df$num[[1]][[1]]
  nums = df$num[[1]][[2]]
  
  output = tryCatch(
    tibble(store = store,
           upc = upc,
           week = weeks,
           nums = nums),
    error = function(e){
      tibble(store = store,
             upc = upc,
             weeks = -1,
             nums = -1)
    })
  return(output)
}

#assign promonum
make_df<-function(df)
{
  df = group_by(df, store, upc) %>%
    arrange(week) %>%
    do(num = assign_nums(.))
  splits = group_by(df, store, upc) %>%
    group_split()
  output = lapply(splits, process_split) %>%
    bind_rows()
  return(output)
}

check_head<-function(df) #for each df in pct list
{
  df=arrange(df,week)
  pctdiscount0=df$pctdiscount[1] #first pct in first week
  promonumcurr = df$promonum[1]
  durprice = df$unitprice[1] #price during promo
  if(is.na(pctdiscount0))
  {
    ppdf=filter(df, promonum == -1*promonumcurr)
    postprice = ppdf[1,]$unitprice #price of antipromo
    pctdiscount = ifelse(postprice==durprice,
                         0,
                         (postprice-durprice)/durprice)
    
    df$pctdiscount[1] = pctdiscount
  }
  return(df)
}

#function takes df and returns a df with response variable (split ran on store and upc)
calc_dsales <- function(df)
{
  #check if begins with promo or not
  promosdf=filter(df,promonum>0)
  nonpromosdf=filter(df,promonum<0)
  
  firstpromo=df[which(df$promonum == min(promosdf$promonum)),]
  firstnonpromo=df[which(df$promonum == max(nonpromosdf$promonum)),]
  df = df %>%
    arrange(week)
  if(min(firstpromo$week)<min(firstnonpromo$week))
  { #UPC begins with a promotion in earliest week
    #check end
    if(df[nrow(df),]$anysale==1)
    { #ends on a promotion 
      lastpromo = max(df$promonum)
      df = filter(df,promonum != lastpromo)
    }
  }
  else # UPC begins in a non promo
  {
    if(df[nrow(df),]$anysale==0)
    { # begins in non promo ends with nonpromo
      nonpromodf=filter(df,promonum<0)
      firstnonpromo=max(nonpromodf$promonum)
      df = filter(df,promonum != firstnonpromo)
      df = mutate(df, promonum = ifelse(promonum < 0, promonum + 1, promonum))
    }
    else
    {
      #begins with nonpromo and ends with promo
      nonpromodf=filter(df,promonum<0)
      firstnonpromo=max(nonpromodf$promonum)
      lastpromo = max(df$promonum)
      df = filter(df, promonum!=firstnonpromo,promonum!=lastpromo) 
      df = mutate(df, promonum = ifelse(promonum < 0, promonum + 1, promonum))
    }
  }
  df=group_by(df,store,upc,promonum) %>%
    summarise(promo_amt = mean(promo_amt, na.rm = TRUE),
              nonpromo_amt = mean(nonpromo_amt, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(av_promonum = abs(promonum)) %>%
    group_by(store, upc, av_promonum) %>%
    arrange(promonum) %>%
    mutate(pct_diff = (promo_amt-lag(nonpromo_amt))/promo_amt)
  
  return(df)
}

#df=excel file from Kilts
#perishable: {True, False}
final_df=function(df,perishable){
  
  df<-df[which(df$WEEK<53),]
  df<-df[,c(1,3,2,4:7)]
  names(df)<-c("store","week","upc","move","qty","price","sale")
  df$anysale<-0
  df$anysale[which(df$sale=="S" | df$sale=="B" | df$sale=="C")]<-1
  
  if(perishable==T){
    df$perishable<-1
  }else{
    df$perishable=0
  }
  
  df = group_by(df, store, upc, anysale) %>%
    arrange(week) %>%
    mutate(diff = week - lag(week),
           diff = ifelse(is.na(diff), 1, diff)) %>%
    ungroup()
  df_sale = filter(df, anysale == 1)
  df_nosale = filter(df, anysale == 0)
  
  df_sale2 = make_df(df_sale)
  df_nosale2 = make_df(df_nosale)
  
  dffin=rbind(df_sale2,df_nosale2)
  dffin=merge(df,dffin,by=c("store","week","upc"))
  dffin = select(dffin, -diff)
  #making promonum
  dffin=dffin %>%
    rename(promonum=nums) %>%
    mutate(promonum=ifelse(anysale==1,promonum,-1*promonum))
  
  #average duration of promos for category by store
  durations = group_by(dffin, store, upc, promonum) %>%
    filter(anysale == 1) %>%
    summarise(duration = n()) %>%
    ungroup() %>%
    group_by(store) %>%
    summarise(avgduration=sum(duration)/n())
  dffin = merge(dffin, durations, by = c("store"))
  
  #average frequency/year of promos for category by store 
  freq = group_by(dffin,store,upc,anysale) %>% 
    summarise(upcstorefreq=max(promonum)) %>%  
    filter(anysale == 1) %>%
    ungroup() %>%
    group_by(store) %>%
    summarise(storefreq=sum(upcstorefreq)/n())
  
  #unit price
  dffin<-mutate(dffin, unitprice=price/qty)
  
  #pct discount by UPC 
  pct=dffin %>%
    group_by(store,upc) %>%
    filter(price!=0 & qty !=0) %>% ##filtered out things that would mess w pct calc
    arrange(week) %>%
    mutate(pctdiscount = ifelse(anysale==1, #general calc
                                ifelse(lag(unitprice)==unitprice,
                                       0,
                                       (lag(unitprice)-unitprice)/lag(unitprice)),
                                0)) %>%
    mutate(pctdiscount = ifelse((week==1 & is.na(pctdiscount)),
                                (lead(unitprice)-unitprice)/lead(unitprice),
                                pctdiscount)) %>%
    ungroup() %>%
    group_by(store,upc) %>%
    group_split() #returns list of small dfs
  
  pct<-lapply(X=pct,FUN=check_head) %>% 
    bind_rows()
  
  dffin=merge(pct,freq,by=c("store"))
  
  #avg sale per UPC during promo by store
  promo <- dffin %>%
    filter(anysale==1) %>%
    group_by(store,promonum,upc) %>%
    mutate(promo_amt = mean(move))
  
  #avg sales during NONpromo
  nonpromo<-dffin %>%
    filter(anysale==0) %>%
    group_by(store,promonum,upc) %>%
    mutate(nonpromo_amt = mean(move))
  
  promo <- merge(x=dffin,y=promo,bt=c("store","upc","promonum"))
  promo$nonpromo_amt<-NA
  nonpromo <- merge(x=dffin,y=nonpromo,bt=c("store","upc","promonum"))
  nonpromo$promo_amt<-NA
  
  dffin<-rbind(promo,nonpromo)
  
  pct<-dffin %>%
    select(store,week,upc,promonum,pctdiscount) %>%
    group_by(store) %>%
    filter(promonum>0,pctdiscount!=0) %>%
    summarise(catdiscount = mean(pctdiscount,na.rm=TRUE))
  
  salessplit<-dffin %>%
    group_by(store,upc) %>%
    group_split()
  
  dffin2 = lapply(X = salessplit,
                      FUN = calc_dsales)
  dffin3 = bind_rows(dffin2)
  dffin3 = dffin3 %>%
    ungroup() %>%
    filter(promonum>0) %>%
    select(store,upc,promonum,pct_diff)
  
  #MERGING X vars
  dffinal=merge(dffin3,freq,by=c("store"))
  dffinal=merge(dffinal,durations,by=c("store"))
  dffinal=merge(dffinal,pct,by=c("store"))
  
  return(dffinal)
}
