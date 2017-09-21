
require(bigrquery)

# p+ geom_hline(aes(xintercept=mean(D3ROI)),
#               color="blue", linetype="dashed", size=1)

project <- '610510520745'
args <- commandArgs(trailingOnly = TRUE)
set_service_token(args[1])

require(plyr)
require(dplyr)
library(ggplot2)
require(scales)
require(ggrepel)
require(ggthemes)
library(tibble)
library(formattable)
library(magrittr)

# getwd()
# setwd("/Users/andrewtynan/Desktop")
# list.files()

source('d3_ROI_sql.R')

# check the data pulled from d3_ROI_sql.R
  # cohort_date 
cohort_date_d3_ROI_data %>% head()
cohort_date_d3_ROI_data  %>% colnames()
cohort_date_d3_ROI_data$cohort_date  %>% min()
cohort_date_d3_ROI_data$cohort_date %>% class()

  # cohort_date and channel  
cohort_date_partner_d3_ROI_data %>% head()
cohort_date_partner_d3_ROI_data  %>% colnames()
cohort_date_partner_d3_ROI_data$channel %>% unique()
cohort_date_partner_d3_ROI_data$cohort_date  %>% min()

#convert cohort_date from string to date
cohort_date_d3_ROI_data$cohort_date <- as.Date(cohort_date_d3_ROI_data$cohort_date)
cohort_date_partner_d3_ROI_data$cohort_date <- as.Date(cohort_date_partner_d3_ROI_data$cohort_date)


## checks 
  # some of the channels have no spend, so using ARPI for assess quality 
cohort_date_partner_d3_ROI_data$channel %>% unique()
cohort_date_partner_d3_ROI_data %>%
  filter(!channel %in% c('applovin')) %>%   
  select(cohort_date,channel,D3_ROI,installs) %>%
  group_by(channel) %>% 
  summarise(avg_D3_ROI = mean(D3_ROI, na.rm=TRUE)) %>% 
  head(20)


# get the mean and std dev
d3_ROI_avg <- 
cohort_date_d3_ROI_data %>% 
  summarize( 
    avg_D3_ROI = mean(D3_ROI, na.rm = TRUE),
    std_dev_D3_ROI = sd(D3_ROI, na.rm = TRUE)
    ) 

cbind(cohort_date_d3_ROI_data, d3_ROI_avg) %>% 
  select(cohort_date, D3_ROI, avg_D3_ROI, std_dev_D3_ROI) %>% 
  mutate(warning = case_when(D3_ROI < (D3_ROI-(std_dev_D3_ROI*2)) ~ "low outlier",
                            D3_ROI > (D3_ROI+(std_dev_D3_ROI*2)) ~ "high outlier"
                            ),
        pos = (D3_ROI-(std_dev_D3_ROI*2)),
        neg = (D3_ROI+(std_dev_D3_ROI*2))
    ) %>% 
  head()


#D3 ROI overall 
cohort_date_d3_ROI_data %>% 
  ggplot() +
  aes(x = cohort_date, y = D3_ROI,
      colour = D3_ROI) +
  geom_line() + 
  labs(title = "D3 ROI by Cohort Date",
        subtitle = "Daily Trend",
        caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  


#D3 ROI overall density plot 
cohort_date_d3_ROI_data %>% 
  ggplot() +
  aes(x = D3_ROI) +
  geom_density() + 
  labs(title = "D3 ROI Density",
       subtitle = "Daily Trend",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  


#D3 ROI density by channel
cohort_date_partner_d3_ROI_data %>% 
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%   
  ggplot() +
  aes(x = D3_ROI
    ,colour = D3_ROI
    ,fill = channel
    ,alpha = 0.2
    ) +
  geom_density() + 
  facet_wrap(~channel, scales ='free') +
  labs(title = "D3 ROI by Cohort Date",
        subtitle = "Daily Trend",
        caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  


#D3 overall ROI histogram of partner D3 ROI
cohort_date_partner_d3_ROI_data %>% 
  # filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
  #                       , 'tapjoy', 'unity ads', 'vungle')) %>%   
  ggplot() +
  aes(x = D3_ROI
      ,fill = channel
  ) +
  geom_histogram(binwidth=0.005, position="stack") +
  facet_wrap(~channel, scales='free') + 
  labs(title = "D3 ROI Histogram",
       subtitle = "By Partner",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  


#D3 overall ROI histogram of partner eCPI
cohort_date_partner_d3_ROI_data %>% 
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%   
  ggplot() +
  aes(x = eCPI
     ,fill = channel
  ) +
  geom_histogram(binwidth=0.5, position="stack") +
  facet_wrap(~channel, scales='free') + 
  labs(title = "eCPI Histogram",
       subtitle = "For Top Partners",
       caption = "Note: Data is last 60 days"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  


#D3 ROI histogram of partner D3 ROI
cohort_date_partner_d3_ROI_data %>% 
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%   
  ggplot() +
  aes(x = D3_ROI
      ,fill = channel
      ) +
  geom_histogram(binwidth=0.005, position="identity") +
  facet_wrap(~channel, scales='free')
  labs(title = "D3 ROI Density",
       subtitle = "Daily Trend",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  


#D3 ROI by top partners & cohort_date faceted by partner 
cohort_date_partner_d3_ROI_data %>% 
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>% 
  ggplot() +
  aes(x = cohort_date, y = D3_ROI
              ,group = channel
              ,color = channel) + 
  geom_line() +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  facet_wrap(~channel, scales ='free') +   
  labs(title = "D3 ROI by Cohort Date & Partner",
        subtitle = "Split-out By Partner",
        caption = "NOTE: Y Axis Scales differ by plot"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom')  


#loess fit for D3 ROI for top partners 
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(cohort_date,D3_ROI) %>% 
  group_by(cohort_date) %>%
  summarise(avg_D3_ROI = mean(D3_ROI, na.rm = T),
            std_dev_D3_ROI = sd(D3_ROI, na.rm = T)
            ) %>% # print(n=15) #check before plotting 
  ggplot() +
  aes(x = cohort_date, y = avg_D3_ROI
      # ,label = avg_D3_ROI
      ,label = sprintf("%1.0f%%", 100*avg_D3_ROI)  # formatting in geom_label_repel
      ) + 
  geom_line() +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Avg. Daily D3 ROI for Top Partners", 
        subtitle = "Split-out By Partner",
        caption = "NOTE: Y Axis Scales differ by plot"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom')  + 
  geom_text(hjust=0, vjust=0)   


#avg w/ error bars D3 ROI for top partners 
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(cohort_date,D3_ROI) %>% 
  group_by(cohort_date) %>%
  summarise(avg_D3_ROI = mean(D3_ROI, na.rm = T),
            std_dev_D3_ROI = sd(D3_ROI, na.rm = T),
            iqr_D3_ROI = IQR(D3_ROI, na.rm = T)
            ) %>% # print(n=15) #check before plotting 
  ggplot() +
  aes(x = cohort_date, y = avg_D3_ROI
      # ,label = round(avg_D3_ROI,2)
      ) + 
  geom_line() +
  geom_point(color = "tomato4") +
  geom_errorbar(aes(ymin = avg_D3_ROI - iqr_D3_ROI,
                    ymax = avg_D3_ROI + iqr_D3_ROI),
                color = "grey60") +    
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Avg. Daily D3 ROI for Top Partners", 
        subtitle = "with Inter-Quartile Error Bars",
        caption = "NOTE: Bounds of error bars are 1st and 3rd quartiles."
  ) +
  # geom_text(hjust=0, vjust=0)   +   
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom')  


#boxplot D3 ROI using the top partners' data
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(D3_ROI) %>%
  ggplot() +
  aes(x = '', y = D3_ROI   # need to provide an empty placeholder for x 
      ,color = D3_ROI
  ) + 
  geom_boxplot(outlier.colour = "red", alpha = 0.3) +
  geom_jitter(width = 0.2) +
  scale_y_continuous(labels = scales::percent) +  
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "D3 ROI", 
       subtitle = "Trailing 60 Days",
       caption = "Note: the blue points are jitters to show volume of observations, 
                  \ and red points are outliers.\n"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'right') 


#boxplot D3 ROI using the top partners' data FILTERED TO EXCLUDE OUTLIERS
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%   
  filter(D3_ROI < sd(D3_ROI, na.rm = TRUE)*2) %>%     
  select(D3_ROI) %>%
  ggplot() +
  aes(x = '', y = D3_ROI   # need to provide an empty placeholder for x
      ,color = D3_ROI
  ) + 
  geom_boxplot(outlier.colour = "red", alpha = 0.3 ) +
  geom_jitter(width = 0.2) +
  scale_y_continuous(labels = scales::percent) +  
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "D3 ROI", 
       subtitle = "Note: data is filtered to within 2 standard deviations",
       caption = "Note: the blue points are jitters to show volume of observations, 
       \ and red points are outliers.\n"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'right') 


#boxplot channel D3 ROI for top partners 
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(channel,D3_ROI) %>%
  ggplot() +
  aes(channel, D3_ROI
      ,color = channel
      ,alpha = 0.3 
  ) + 
  geom_boxplot(outlier.colour = "red") +
  geom_jitter(width = 0.2) +
  scale_y_continuous(labels = scales::percent) +  
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "D3 ROI for Top Partners", 
       subtitle = "Trailing 60 Days",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom') 


#boxplot channel D3 ROI for top partners FILTERED TO EXCLUDE OUTLIERS
cohort_date_partner_d3_ROI_data %>% 
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  filter(D3_ROI < sd(D3_ROI, na.rm = TRUE)*2) %>%   
  select(channel,D3_ROI) %>%
  ggplot() +
  aes(channel, D3_ROI
      ,color = channel
      ,alpha = 0.3 
      ) + 
  geom_boxplot() +  #outlier.colour = "red"
  geom_jitter(width = 0.2) +
  scale_y_continuous(labels = scales::percent) +  
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "D3 ROI for Top Partners", 
        subtitle = "Filtered to within 2 standard deviations.",
        caption = "Note: Trailing 60 Days"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom') 


#boxplot cohort_date D3 ROI for top partners 
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(cohort_date,channel,D3_ROI) %>%
  filter(cohort_date > Sys.Date() - 30) %>%
  ggplot() +
  aes(as.factor(cohort_date), D3_ROI
      ,color = D3_ROI 
      ,alpha = 0.3
  ) + 
  geom_boxplot(outlier.colour = "red") +
  geom_jitter(width = 0.2) +
  scale_y_continuous(labels = scales::percent) +  
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "D3 ROI for Top Partners", 
        subtitle = "Trailing 60 Days",
        caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
        ) 


#D3 ROI for trailing 30 cohort dates faceted by top partners 
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(cohort_date,channel,D3_ROI) %>%
  filter(cohort_date > Sys.Date() - 30) %>%
  ggplot() +
  aes(cohort_date <- as.factor(cohort_date), D3_ROI
      ,color = D3_ROI 
      ,alpha = 0.3
  ) + 
  facet_wrap(~channel) +  # removed scales ='free' to give one row of x axis labels 
  geom_point() +
  scale_y_continuous(labels = scales::percent) +  
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "D3 ROI for Top Partners", 
        subtitle = "Trailing 30 Days",
        caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
  ) 


#channel faceted by cohort_date D3 ROI for top partners
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(cohort_date,channel,D3_ROI) %>%
  filter((between(cohort_date, Sys.Date() - 10,  Sys.Date()-1))) %>%  # FILTER THE DATES BETWEEN
  ggplot() +
  aes(channel, D3_ROI
      ,color = channel 
  ) + 
  facet_wrap(~as.factor(cohort_date)) + # removed scales ='free' to give one row of x axis labels 
  geom_point() + 
  scale_y_continuous(labels = scales::percent) +  
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "D3 ROI for Top Partners", 
        subtitle = "Trailing 60 Days",
        caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
  ) 


# D3_ARPI by top partners & cohort_date faceted by partner 
cohort_date_partner_d3_ROI_data %>% 
  # filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
  #                       , 'tapjoy', 'unity ads', 'vungle')) %>% 
  ggplot() +
  aes(x = cohort_date, y = D3_ARPI
      ,group = channel
      ,color = channel) + 
  geom_line() +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  facet_wrap(~channel, scales ='free') +   
  labs(title = "D3_ARPI by Cohort Date & Partner",
       subtitle = "Split-out By Partner",
       caption = "NOTE: Y Axis Scales differ by plot"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom')  

