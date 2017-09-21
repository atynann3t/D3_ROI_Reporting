

# overall daily ARPI 
cohort_date_d3_ROI_data %>% 
  ggplot() +
  aes(x = cohort_date, y = D3_ARPI,
      colour = D3_ARPI) +
  geom_line() + 
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "D3 ARPI by Cohort Date",
       subtitle = "Daily Trend",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 


# D3_ARPI overall density  
cohort_date_d3_ROI_data %>% 
  ggplot() +
  aes(x = D3_ARPI) +
  geom_density() + 
  labs(title = "D3 ARPI Density",
       subtitle = "Daily Trend",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  


# D3_ARPI density by channel 
cohort_date_partner_d3_ROI_data %>% 
  ggplot() +
  aes(x = D3_ARPI
      ,colour = channel
      ,fill = channel
  ) +
  geom_density(alpha = 0.2) + 
  facet_wrap(~channel, scales ='free') +
  labs(title = "D3 ARPI by Cohort Date",
       subtitle = "NOTE: axis ranges differ by channel",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom')  + 
  guides(col = guide_legend(nrow=3), byrow=TRUE)


# D3_ARPI histogram by channel
cohort_date_partner_d3_ROI_data %>% 
  # filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
  #                       , 'tapjoy', 'unity ads', 'vungle')) %>%   
  ggplot() +
  aes(x = D3_ARPI
      ,fill = channel
  ) +
  geom_histogram(binwidth=0.05, position="stack") +
  facet_wrap(~channel, scales='free') + 
  labs(title = "D3 ARPI Histogram",
       subtitle = "By Partner",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom')  + 
  guides(col = guide_legend(nrow=3), byrow=TRUE)


