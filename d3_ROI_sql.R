
# get d3 ROI (and related metrics) at cohort_date level 
cohort_date_d3_ROI_sql <- 
"#standardSQL
SELECT 
      cohort_date,
      sum(installs) as installs, 
      case when Sum(d30_NPU) > 0 then SUM(spend) / Sum(d30_NPU) end CPD30NPU,
      case when Sum(cohort_payers) > 0 then SUM(spend) / Sum(cohort_payers) end CPP,      
      case when Sum(d1_NPU) > 0 then SUM(spend) / Sum(d1_NPU) end CPD1NPU,        
      case when Sum(spend) > 0 then SUM(d1_revenue_net) / SUM(spend) end D1_ROI,         
      case when Sum(Installs) > 0 then sum(d3_rr)/sum(Installs) end D3_Retention,                
      sum(d3_revenue_net) / sum(spend) as D3_ROI,
      FORMAT_DATE('%A', cast(cohort_date as date)) AS weekday,
      SUM(cohort_revenue_net) / Sum(spend) as YTD_percent,
      SUM(spend) / Sum(Installs) as eCPI,
      sum(cohort_revenue_net) / sum(Installs) as YTD_ARPI,
      sum(d3_revenue_net) / sum(Installs) as D3_ARPI      
   FROM `n3twork-marketing-analytics.Metrics.nebo_temp_table` a
  Where cast(cohort_date as date) >= DATE_ADD(cast(cohort_date as date), INTERVAL -60 DAY)
  AND d3_revenue_net is not null
group by 1
Order by 1"   

cohort_date_d3_ROI_data <- query_exec(cohort_date_d3_ROI_sql, project = project, use_legacy_sql=FALSE)

# get d3 ROI (and related metrics) at cohort_date and partner level 
cohort_date_partner_d3_ROI_sql <- 
"#standardSQL
Select 
      cohort_date,
      CASE WHEN channel = 'applifier' THEN 'unity ads'
            WHEN channel = 'apple_search' THEN 'ios search ads'
            WHEN channel = 'supersonic' THEN 'ironsource'
            WHEN channel = 'pinsight_media' THEN 'pinsight media'
            WHEN channel = 'adaction' then 'adaction interactive'
            ELSE channel
      END AS channel,
      SUM(d3_revenue_net) / sum(installs) as D3_ARPI,       
      case when Sum(d30_NPU) > 0 then SUM(spend) / Sum(d30_NPU) end CPD30NPU,
      case when Sum(cohort_payers) > 0 then SUM(spend) / Sum(cohort_payers) end CPP,      
      case when Sum(d1_NPU) > 0 then SUM(spend) / Sum(d1_NPU) end CPD1NPU,        
      case when Sum(spend) > 0 then SUM(d1_revenue_net) / SUM(spend) end D1_ROI,         
      case when Sum(Installs) > 0 then sum(d3_rr)/sum(Installs) end D3_Retention,                
      sum(d3_revenue_net) / sum(spend) as D3_ROI,
      FORMAT_DATE('%A', cast(cohort_date as date)) AS weekday,
      SUM(cohort_revenue_net) / Sum(spend) as YTD_percent,
      SUM(spend) / Sum(Installs) as eCPI,
      sum(cohort_revenue_net) / sum(Installs) as YTD_ARPI
    FROM `n3twork-marketing-analytics.Metrics.nebo_temp_table` a
  Where cast(cohort_date as date) >= DATE_ADD(cast(cohort_date as date), INTERVAL -60 DAY)
  AND d3_revenue_net is not null
  AND installs is not null
group by 1,2
Order by 1,3 desc"

cohort_date_partner_d3_ROI_data <- query_exec(cohort_date_partner_d3_ROI_sql, project = project, use_legacy_sql=FALSE)



