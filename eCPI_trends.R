


#2d density of eCPI and D3_ROI using the top partners data RASTER style
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(cohort_date,D3_ARPI,eCPI,D3_ROI) %>%
  filter((between(cohort_date, Sys.Date() - 10,  Sys.Date()-1))) %>%  # FILTER THE DATES BETWEEN
  ggplot() +  
  aes(eCPI, D3_ROI
      # ,color = channel 
  ) +   
  stat_density2d(geom = 'raster', aes(fill = ..density..), contour = FALSE) + 
  labs(title = "D3 ROI & eCPI Joint Density", 
       subtitle = "Trailing 60 Days",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
  ) 


#2d density of eCPI and D3_ROI using the top partners data 
cohort_date_partner_d3_ROI_data %>%
  filter(channel %in% c('applovin', 'crossinstall', 'facebook', 'google', 'ios search ads', 'ironsource'
                        , 'tapjoy', 'unity ads', 'vungle')) %>%     
  select(cohort_date,D3_ARPI,eCPI,D3_ROI) %>%
  filter((between(cohort_date, Sys.Date() - 10,  Sys.Date()-1))) %>%  # FILTER THE DATES BETWEEN
  ggplot() +  
  aes(eCPI, D3_ROI
      # ,color = channel 
  ) +   
  geom_point() + 
  stat_density2d() + 
  labs(title = "D3 ROI & eCPI Joint Density Points", 
       subtitle = "Trailing 60 Days",
       caption = ""
  ) + 
  theme(plot.title = element_text(hjust = 0.5),  
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
  ) 