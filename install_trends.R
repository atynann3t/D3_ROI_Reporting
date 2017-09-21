#Installs by top partners & cohort_date faceted by partner TREND LINE (LOESS)
cohort_date_partner_d3_ROI_data %>% 
  # filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
  #                       , 'tapjoy', 'unity ads', 'vungle')) %>% 
  ggplot() +
  aes(x = cohort_date, y = installs
      ,group = channel
      ,color = channel) + 
  geom_line() +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  facet_wrap(~channel, scales ='free') +   
  labs(title = "Installs by Cohort Date & Partner",
       subtitle = "Split-out By Partner",
       caption = "NOTE: Y Axis Scales differ by plot"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom') + 
  guides(col = guide_legend(nrow=3), byrow=TRUE)


#Installs by top partners & cohort_date faceted by partner STACKED TREND LINE 
cohort_date_partner_d3_ROI_data %>% 
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%
  ggplot() +
  aes(x = cohort_date, y = installs
      ,group = channel
      ,color = channel) + 
  geom_area(aes(fill = channel)) + 
  labs(title = "Installs by Cohort Date & Partner",
       subtitle = "Split-out By Partner",
       caption = "NOTE: Y Axis Scales differ by plot"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom') + 
  guides(col = guide_legend(nrow=3), byrow=TRUE)


#Installs by top partners & cohort_date faceted by partner 100% STACKED TREND LINE 
cohort_date_partner_d3_ROI_data %>% 
  select(cohort_date, channel, installs) %>% 
  # filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
  #                       , 'tapjoy', 'unity ads', 'vungle')) %>%
  group_by(cohort_date) %>% 
  mutate(installs_daily_total  = sum(installs)) %>% 
  mutate(installs_percent = installs / installs_daily_total) %>% # not * 100 to work with scale_y_continuous
  ggplot() + 
  aes(x = cohort_date, y = installs_percent
      ,group = channel
      ,color = channel) + 
  geom_area(aes(fill = channel, group = channel), position = 'stack') + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Installs by Cohort Date & Partner",
       subtitle = "Split-out By Partner",
       caption = "NOTE: Y Axis Scales differ by plot"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom') + 
  guides(col = guide_legend(nrow=3), byrow=TRUE)

s