library(tidyverse)
library(tidyquant)
library(reshape2)
library(corrplot)
library(readxl)
library(ggplot2)
library(ggpubr)
library(plotly)
library(reshape2)
theme_agon <- theme(legend.key = element_rect(fill="black"),
                    legend.background = element_rect(color="white", fill="#263238"),
                    plot.subtitle = element_text(size=6, color="white"),
                    panel.background = element_rect(fill="black"),
                    panel.border = element_rect(fill=NA),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(color="black", linetype=0),
                    panel.grid.minor.y = element_blank(),
                    plot.background = element_rect(fill="black"),
                    text = element_text(color="white"),
                    axis.text = element_text(color="white")
                    
)


#read file
channel_NET <- read_xls("data_input/Channel_Revenue.xls")
asset_NET <- read_xls("data_input/Asset_Revenue.xls")
bydate_NET <- read_xls("data_input/ByDate_revenue.xls")
region_NET <- read_xls("data_input/region_revenue.xls")
#source for Overview tab


rev_plot <- function(start = '2018-01-01', end= '2018-03-01'){
  #create total revenue plot
sourcedata <- bydate_NET %>%
  mutate("Date"=as.Date(Date),"Revenue" = `YouTube ad revenue (USD)`) %>%
  select(c(Date, Revenue, Views)) %>%
  filter((Date > as.Date(start) & Date < as.Date(end)))
 return(sourcedata)
} 

views_plot <- function(start = '2018-01-01', end= '2018-03-01'){
viewsvid <- bydate_NET %>%
  mutate("Date"=as.Date(Date)) %>%
  select(c(Date, Views)) %>%
  mutate(Date = as.Date(Date)) %>%
  filter((Date > as.Date(start) & Date < as.Date(end)))
return(viewsvid)
}
  

#Youtube Red Revenue Plot
red_plot <- function(start = "2018-01-01", end='2018-03-01'){
redsource <- bydate_NET %>%
  mutate("Date"=as.Date(Date), "Revenue"=`YouTube Red partner revenue (USD)`,"Views" = `YouTube Red views`) %>%
  select(c(Date, Revenue,Views)) %>%
  filter((Date > as.Date(start) & Date < as.Date(end)))
return(redsource)
}

redviews_plot <- function(start = '2018-01-01', end= '2018-03-01'){
red_vi <- bydate_NET %>%
  mutate("Date"=as.Date(Date), "red_views"=`YouTube Red views`) %>%  
  select(c(Date, red_views)) %>%
  filter((Date > as.Date(start) & Date < as.Date(end)))
return(red_vi)  
}


# lets create revenue by country plot

library(dplyr)
library(tidyverse)
region_NET <- region_NET %>% 
  select("country" = Geography, "revenue" = `YouTube ad revenue (USD)`) 

region_NET<-aggregate(revenue~country,data=region_NET,FUN=sum)
region_NET_filt <- region_NET %>% top_n(10)

region_NET_filt$country <- factor(region_NET_filt$country) %>%
  fct_reorder(region_NET_filt$revenue)

# rename united kingdom and united states

region_NET_filt$country <- recode(region_NET_filt$country
                                  ,'United States' = 'USA'
                                  ,'United Kingdom' = 'UK'
)


##create world map 
library(rworldmap)
map.world <- map_data("world")
map.world <- map.world %>% select(long, lat, group,region)

## combine regon_net data and map.world
map.world_joined <- left_join(map.world, region_NET_filt, by = c(region = "country"))

map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(revenue),F,T))

## because singapore is a small country we have plot more clearly in map
library(ggmap)
#df.country_points <- data.frame(country = c("Singapore"),stringsAsFactors = F)
#geocode.country_points <- geocode(df.country_points$country)
#df.country_points <- cbind(df.country_points,geocode.country_points)


##create our map plot
library(ggplot2)

map_plot <- ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
  #geom_point(data = country_points, aes(x = lon, y = lat), color = "#e60000") +
  scale_fill_manual(values = c("#CCCCCC","#85bb65")) +

  
  theme(text = element_text(family="Times", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#000000")
        ,plot.background = element_rect(fill = "#000000")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )


# create top 30 channel by revenue plot from Youtube data
## Data preparation

channel_NET <- channel_NET %>% 
  select("channel" = `Channel display name`, "revenue" = `YouTube ad revenue`, 
         "redView_revenue" = `YouTube Red partner revenue`, "impression" = `Ad impressions`,
         "CPM" = `CPM`, "views" = Views, "watchtime_min" = `Watch time_minutes`, "subscribers" = Subscribers, 
         "red_views" = `YouTube Red views`, "redwatchtime_min" = `YouTube Red watch time_minutes`, "likes" = Likes, 
         "dislikes" = Dislikes, "comments" = Comments, "shares" = Shares) 

library(tidyverse)
top30channel_NET <- channel_NET%>% top_n(30)
top30channel_NET$channel <- factor(top30channel_NET$channel) %>%
  fct_reorder(top30channel_NET$revenue)

## create top 30 channel plot

channel_colp <- ggplot(top30channel_NET, aes(x=channel, y=revenue))+
  geom_col(fill="goldenrod3")+
  coord_flip()+
  theme_agon


# reate top 30 video asset by revenue plot from Youtube data


asset_NET <- asset_NET %>% 
  select("asset" = Asset, "type" = Asset_type, 
         "revenue" = Estimated_partner_revenue, "ad_revenue" = Estimated_partner_ad_revenue,
         "adsense_revenue" = Estimated_partner_AdSense_revenue, "doubleclick_revenue" = Estimated_partner_DoubleClick_revenue,     "watchtime_min" = Watch_time_min, "shares" = Shares, 
         "views" = Views, "av_viewduration_min" = Average_view_duration_min, 
         "av_viewpercentage_view" = Average_percentage_viewed, "likes" = Likes, 
         "dislikes" = Dislikes, "comments" = Comments) 

asset_NET<-aggregate(revenue~asset,data=asset_NET,FUN=sum)
top30asset_NET <- asset_NET%>% top_n(30)
top30asset_NET$asset <- factor(top30asset_NET$asset) %>%
  fct_reorder(top30asset_NET$revenue)

## create top 30 video asset plot
asset_colp <- ggplot(top30asset_NET, aes(x=asset, y=revenue))+
  geom_col(fill="goldenrod3")+
  coord_flip()+
  theme_agon

# create Revenue vs CPM plot
library(tidyverse)
library(reshape)
library(zoo)
compdata <- bydate_NET %>%
  mutate("Date"=as.Date(Date),"Revenue" = `YouTube ad revenue (USD)`, "CPM" = `CPM (USD)`) %>%
  select(Date, Revenue, CPM, `Estimated monetized playbacks`, Views)



p <-
  ggplot(compdata, aes(x = Date))  
comp <-
  p + geom_line(aes(y=Views, colour="Views"))

comp <-
  comp + geom_line(aes(y=`Estimated monetized playbacks`, colour="Estimated monetized playback"))


comp <- comp + scale_y_continuous(sec.axis = sec_axis(~.*1000000000, name = "CPM")) +theme_agon


# create plot in content tab

top30channel_NET$sentiment <- top30channel_NET$likes/top30channel_NET$dislikes
top30channel_NET$likesratio <- top30channel_NET$likes/top30channel_NET$views
top30channel_NET$dislikesratio <- top30channel_NET$dislikes/top30channel_NET$views
top30channel_NET$subsratio <- top30channel_NET$subscribers/top30channel_NET$views

library(ggrepel)
foxp <- ggplot(top30channel_NET, aes(x=sentiment, y=watchtime_min, color=channel))+
  geom_point(aes(size=views), show.legend = F)+
  geom_label(aes(label=channel), size=1.4, nudge_y = 800)+
   theme_agon


pexp <- ggplot(top30channel_NET, aes(x=likesratio, y=dislikesratio, color=channel))+
  geom_point( aes(size=shares),show.legend = F)+
  labs(title="Likes vs Dislikes in Top 30 hannel")+
  theme_agon


channel.favor <- top30channel_NET %>% 
  group_by(channel) %>%
  summarise(likeratio = mean(likes/views), 
            dlikeratio = mean(dislikes/views)
  ) %>%
  mutate(favor = likeratio/dlikeratio)

channel.favor$channel <- factor(channel.favor$channel) %>%
  fct_reorder(channel.favor$favor)

colp2 <- ggplot(channel.favor, aes(x=channel, y=favor))+
  geom_col(fill="dodgerblue4")+
  coord_flip()+
  labs(title="Favorability Index by channel, 2018")+
  theme_agon
