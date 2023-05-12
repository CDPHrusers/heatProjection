library(data.table)
library(tidyverse)
library(tidycensus)
library(ggthemes)
library(patchwork)
`%notin%` <- Negate(`%in%`)
library(patchwork)
library(httr)
set_config(use_proxy(url='http://l1proxy.frb.org:8080'))
Sys.setenv(https_proxy="http://l1proxy.frb.org:8080")


foo <- fread("results_combined_4ms.csv") %>%
  filter(!is.na(region) & !is.na(hpiquartile)) %>%
  mutate(ccs = factor(ccs, levels = c("6CCS","MENTAL","allED")))

########## Counting ZCTAs 
foo %>% 
  filter(ccs == "6CCS") %>%
  select(zip) %>%
  unique() %>%
  nrow() # 1326

foo %>% 
  filter(ccs == "MENTAL") %>%
  select(zip) %>%
  unique() %>%
  nrow() # 1327

foo %>% 
  filter(ccs == "allED") %>%
  select(zip) %>%
  unique() %>%
  nrow() # 1327

####### TABLE 1 ############
foo %>% 
  # filter(metric == "estimate_AN") %>% 
  group_by(ccs) %>%
  summarise(geo = "Statewide", EDvisits = sum(total_ED)) %>%
  spread(key = ccs, value = EDvisits)
  

foo %>% 
  filter(metric == "estimate_AN") %>% 
  group_by(region, ccs) %>%
  summarise(EDvisits = sum(total_ED)) %>%
  spread(key = ccs, value = EDvisits)

foo %>% 
  filter(metric == "estimate_AN") %>% 
  group_by(hpiquartile, ccs) %>%
  summarise(EDvisits = sum(total_ED), pop = sum(pop)) %>%
  spread(key = ccs, value = EDvisits) 



####### TABLE 2 ################ 
foo %>% 
  # filter(metric == "estimate_AN") %>% 
  group_by(ccs, metric) %>%
  summarise(region = "Statewide", 
            AN = sum(heat_AN), 
            AF = sum(heat_AN)/sum(total_ED)) %>% 
  gather(stat, value, AN, AF) %>%
  spread(metric, value)


foo %>% 
  # filter( ccs == "6CCS") %>% 
  group_by(region, ccs, metric) %>%
  summarise(AN = sum(heat_AN), 
            AF = sum(heat_AN)/sum(total_ED))%>% 
  gather(stat, value, AN, AF) %>%
  spread(metric, value)

foo %>% 
  # filter(metric == "estimate_AN") %>% 
  group_by(hpiquartile, ccs, metric) %>%
  summarise(AN = sum(heat_AN), 
            AF = sum(heat_AN)/sum(total_ED))%>% 
  gather(stat, value, AN, AF) %>%
  spread(metric, value)


######## TABLE 3 RATES ########

rates_state <- foo %>%
  filter(metric == "estimate_AN") %>%
  group_by(ccs, zip) %>%
  summarise(rate1 = (heat_AN/13)/pop*100000) %>%
  group_by(ccs) %>%
  summarise(rate = mean(rate1, na.rm= T), 
            lower = mean(rate1, na.rm= T) - (sd(rate1, na.rm= T)/sqrt(length(rate1))), 
            upper = mean(rate1, na.rm = T) + (sd(rate1, na.rm= T)/sqrt(length(rate1)))
  ) %>%
  mutate(table = paste0(round(rate, 0), " (", round(lower, 0),", ",round(upper,0),")"))

 foo %>%
  filter(metric == "estimate_AN") %>%
  group_by(ccs, region, zip) %>%
  summarise(rate1 = (heat_AN/13)/pop*100000) %>%
  group_by(ccs, region) %>%
  summarise(rate = mean(rate1, na.rm= T), 
            lower = mean(rate1, na.rm= T) - (sd(rate1, na.rm= T)/sqrt(length(rate1))), 
            upper = mean(rate1, na.rm = T) + (sd(rate1, na.rm= T)/sqrt(length(rate1)))) %>%
  mutate(table = paste0(round(rate, 0), " (", round(lower, 0),", ",round(upper,0),")")) %>%
  merge({filter(.,region == "Association of Bay Area Health Officers (ABAHO)") %>% select(ccs, ref = rate, ref_l = lower, ref_u = upper)}) %>%
   mutate(RR = paste0(round(rate/ref, 1), " (", round(lower/ref_l, 1),", ",round(upper/ref_u,1),")"))

foo %>%
  filter(metric == "estimate_AN") %>%
  group_by(ccs, hpiquartile, zip) %>%
  summarise(rate1 = (heat_AN/13)/pop*100000) %>%
  group_by(ccs, hpiquartile) %>%
  summarise(rate = mean(rate1, na.rm= T), 
            lower = mean(rate1, na.rm= T) - (sd(rate1, na.rm= T)/sqrt(length(rate1))), 
            upper = mean(rate1, na.rm = T) + (sd(rate1, na.rm= T)/sqrt(length(rate1)))
  )%>%
  mutate(table = paste0(round(rate, 0), " (", round(lower, 0),", ",round(upper,0),")")) %>%
  merge({filter(.,hpiquartile == 4) %>% select(ccs, ref = rate, ref_l = lower, ref_u = upper)}) %>%
  mutate(RR = round(rate/ref, 1))
  # mutate(RR = paste0(round(rate/ref, 1), " (", round(lower/ref_l, 1),", ",round(upper/ref_u,1),")"))


###### Figure 2 ##################

hpi <- fread("../data/equity_indices/HPI_zcta/hpi2score.csv") %>%
  select(zip = GEO_ID, percentile = pctile_st) %>%
  filter(!is.na(percentile))

df <- foo %>%
  merge(hpi, all.x = T) %>%
  filter(metric == "estimate_AN") %>%
  mutate(rate1 = (heat_AN/13)/pop*100000, percentile = percentile *100) 
  
df1 <- filter(df, ccs == "6CCS") 
mod1 <- lm(df1$rate1 ~ df1$percentile, weights = df1$pop ) 
text1 <-  paste0("y = ",format(coef(mod1)[1], digits = 4), " + ",format(coef(mod1)[2], digits = 4), "x;  r2 = ",format(summary(mod1)$r.squared, digits = 3))

p1 <- ggplot(df1)+ 
  geom_point(aes(x = percentile, y = rate1, color = factor(hpiquartile), size = pop), alpha = 0.3) +
  geom_smooth(aes(x = percentile, y = rate1, weight = pop), method = "lm", se = T) + 
  geom_text(x =75, y = 500, label = text1) +
  scale_color_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"), 
                     labels = c("Fewest Opportunities","Q2","Q3","Most Opportunities")) +
  # facet_grid(ccs ~ ., scales = "free") +
  labs(y = "Annual ED Visits per 100k (physical)", x = "") + 
  theme_minimal() + 
  guides(color = FALSE, size = FALSE)

df2 <- filter(df, ccs == "MENTAL") 
mod2 <- lm(df2$rate1 ~ df2$percentile, weights = df2$pop) 
text2 <-  paste0("y = ",format(coef(mod2)[1], digits = 4), " + ",format(coef(mod2)[2], digits = 4), "x;  r2 = ",format(summary(mod2)$r.squared, digits = 3))

p2 <- ggplot(df2)+ 
  geom_point(aes(x = percentile, y = rate1, color = factor(hpiquartile), size = pop), alpha = 0.3) +
  geom_smooth(aes(x = percentile, y = rate1, weight = pop), method = "lm", se = T) + 
  geom_text(x =75, y = 900, label = text2) +
  scale_color_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"), 
                     labels = c("Fewest Opportunities","Q2","Q3","Most Opportunities")) +
  # facet_grid(ccs ~ ., scales = "free") +
  labs(y = "Annual ED Visits per 100k (mental)", x = "") + 
  theme_minimal() + 
  guides(color = FALSE, size = FALSE)

df3 <- filter(df, ccs == "allED") 
mod3 <- lm(df3$rate1 ~ df3$percentile, weights = df3$pop)
text3 <-  paste0("y = ",format(coef(mod3)[1], digits = 4), " + ",format(coef(mod3)[2], digits = 4), "x;  r2 = ",format(summary(mod3)$r.squared, digits = 3))

p3 <- ggplot(df3)+ 
  geom_point(aes(x = percentile, y = rate1, color = factor(hpiquartile), size = pop), alpha = 0.3) +
  geom_smooth(aes(x = percentile, y = rate1, weight = pop), method = "lm", se = T) + 
  geom_text(x =75, y = 2000, label = text3) +
  scale_color_manual(name = "HPI Quartile", values = c("royalblue","lightblue","lightgreen","darkgreen"), 
                     labels = c("Fewest Opportunities","Q2","Q3","Most Opportunities")) +
  # facet_grid(ccs ~ ., scales = "free") +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  labs(y = "Annual ED Visits per 100k (all causes)", x = "HPI percentile (fewest to most opportunities)", caption = "linear trends weighted by ZCTA population")

p1 / p2 / p3


###########################
# examining correlation between ccs groups 
library(GGally)

bind_rows(df1, df2, df3) %>% 
  dcast.data.table(zip + region + pop~ ccs, value.var = "rate1") %>% # fwrite("4tabl.csv")
  select(-zip, -region ) %>% 
  plot( pch=20 , cex=1.5 , col="#69b3a2")
  ggpairs(columns = 3:5, ggplot2::aes(colour=region))
  
###### Figure 3 ##################
# method 1 using SE around average rate of zip codes in each group
foo %>%
  filter(metric == "estimate_AN") %>%
  group_by(ccs, hpiquartile, region, zip) %>%
  summarise(rate1 = (heat_AN/13)/pop*100000) %>%
  group_by(ccs, region, hpiquartile) %>%
  summarise(rate = mean(rate1, na.rm= T), 
            lower = mean(rate1, na.rm= T) - (sd(rate1, na.rm= T)/sqrt(length(rate1))), 
            upper = mean(rate1, na.rm = T) + (sd(rate1, na.rm= T)/sqrt(length(rate1)))) %>%
  merge({select(rates_state, ccs, ref = rate)}) %>%
  ggplot()+ 
  # geom_bar(aes(x = hpiquartile, y = rate, fill = factor(hpiquartile)), stat = "identity", position = "dodge" , alpha = 0.7) +
  geom_errorbar(aes(x = hpiquartile, ymax = upper, ymin= lower), color = "gray50") + 
  geom_point(aes(x = hpiquartile, y = rate, color = factor(hpiquartile)), size = 3, alpha = 0.7) +
  geom_hline(aes(yintercept = ref), linetype = "dashed", color = "gray50") + 
  scale_fill_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"))+
  scale_color_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"), 
                     labels = c("Fewest Opportunities","Q2","Q3","Most Opportunities"))+
  facet_grid(ccs ~ region, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(caption = "statewide rate show by dashed line")
 

# method 2 looking at errors from the models
average <- foo %>%
  filter(metric == "estimate_AN") %>%
  group_by(ccs, hpiquartile, region, zip) %>%
  summarise(rate1 = (heat_AN/13)/pop*100000) %>%
  group_by(ccs, region, hpiquartile) %>%
  summarise(rate = mean(rate1, na.rm= T))
upper <- foo %>%
  filter(metric == "UL95_AN") %>%
  group_by(ccs, hpiquartile, region, zip) %>%
  summarise(rate1 = (heat_AN/13)/pop*100000) %>%
  group_by(ccs, region, hpiquartile) %>%
  summarise(upper = mean(rate1, na.rm= T))
lower <- foo %>%
  filter(metric == "LL95_AN") %>%
  group_by(ccs, hpiquartile, region, zip) %>%
  summarise(rate1 = (heat_AN/13)/pop*100000) %>%
  group_by(ccs, region, hpiquartile) %>%
  summarise(lower = mean(rate1, na.rm= T))

merge(average, lower) %>% merge(upper) %>%
  ggplot()+ 
  geom_bar(aes(x = hpiquartile, y = rate, fill = factor(hpiquartile)), stat = "identity", position = "dodge" , alpha = 0.7) +
  geom_errorbar(aes(x = hpiquartile, ymax = upper, ymin= lower)) + 
  scale_fill_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"))+
  scale_color_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"))+
  facet_grid(ccs ~ region) +
  theme_minimal()






#############   MINT #####################

mints <- fread("results_mint_4ms.csv")


ggplot(mints[!is.na(hpiquartile)]) + 
  geom_jitter(aes(y = mint, x = factor(hpiquartile), color = factor(hpiquartile)), alpha = 0.3) + 
  scale_color_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"))+
  facet_grid(ccs ~ region)+ 
  theme_minimal()

ggplot(mints[!is.na(hpiquartile) & ccs != "allED"]) + 
  geom_boxplot(aes(y = mint, x = factor(hpiquartile), color = factor(hpiquartile)), alpha = 0.3) + 
  scale_color_manual(name = "HPI", values = c("royalblue","lightblue","lightgreen","darkgreen"))+
  facet_wrap(~ ccs)+ 
  theme_minimal()

mints[!is.na(hpiquartile) & ccs == "6CCS"] %>% lm(formula = mint ~ hpiquartile + region) %>% summary

ggplot(mints[!is.na(hpiquartile)]) +
  geom_point(aes(
    y = mint,
    x = factor(hpiquartile),
    color = factor(hpiquartile)
  ), alpha = 0.3) +
  geom_smooth(aes(y = mint, x = factor(hpiquartile)), method = "lm") +
  scale_color_manual(
    name = "HPI",
    values = c("royalblue", "lightblue", "lightgreen", "darkgreen")
  ) +
  facet_grid(ccs ~ region) +
  theme_minimal()




hpi <- fread("../data/equity_indices/HPI_zcta/hpi2score.csv") %>%
  select(zip = GEO_ID, percentile = pctile_st) %>%
  filter(!is.na(percentile))

df <- mints %>%
  merge(hpi, all.x = T)


ggplot(df[!is.na(hpiquartile) & ccs == "6CCS" & pop >2000]) +
  geom_point(aes(
    y = mint,
    x = percentile,
    color = factor(hpiquartile)
  ), alpha = 0.6) +
  geom_smooth(aes(y = mint, x = percentile), method = "lm") +
  scale_color_manual(
    name = "HPI",
    values = c("royalblue", "lightblue", "lightgreen", "darkgreen")
  ) +
  facet_grid(~ccs) +
  theme_minimal()


# fwrite(df, "mints_4TABLEAU.csv")





############### RR CURVES #################################### 

hpi <- fread("../data/equity_indices/HPI_zcta/hpi2score.csv") %>%
  select(zip = GEO_ID, county)

df <- mints %>%
  merge(hpi, all.x = T)


top_zips <- df[df[, .I[which.max(pop)], by=.(region, hpiquartile)]$V1] %>%
  select(zip, region, hpiquartile, pop)



fread("results/temp_RR_table_6CCSheat.csv") %>%
  .[zip %in% top_zips$zip] %>%
  merge(top_zips) %>%
  filter(!is.na(hpiquartile)) %>%
  ggplot() +
  geom_line(aes(x = temp*(9/5)+32, y = estimate_RR , group = factor(hpiquartile), color = factor(hpiquartile)), size = 1.5) + 
  # geom_ribbon(aes(x = temp*(9/5)+32, ymin = LL95_RR, ymax = UL95_RR ,  fill = factor(hpiquartile)), alpha = 0.1) +
  scale_color_manual(
    name = "HPI",
    values = c("royalblue", "lightblue", "lightgreen", "darkgreen"), 
    labels = c("Fewest Opportunities","Q2","Q3","Most Opportunities")
  ) +
    scale_fill_manual(
      name = "HPI",
      values = c("royalblue", "lightblue", "lightgreen", "darkgreen")
    ) +
  facet_grid(~region) + 
  theme_minimal() + 
  labs(x = "Temperaure (F)", y = "Relative Risk", caption = "Curves represent most populous zip code in each region and HPI Quartile ") + 
  theme(legend.position = "bottom")




rr <- fread("results/temp_RR_table_6CCSheat.csv") %>%
  .[zip %in% top_zips$zip] %>%
  merge(top_zips) %>%
  filter(!is.na(hpiquartile))  

label = paste0(unique(rr$zip), " - ", unique(rr$region), " - HPI Quartile ", unique(rr$hpiquartile))


rr[, region := factor(region, levels = c("Rural Association of Northern California Health Officers (RANCHO)", 
                                         "Greater Sacramento Region Health Officers",
                                         "Association of Bay Area Health Officers (ABAHO)",
                                         "San Joaquin Valley Consortium (SJVC)",
                                         "Southern California Health Officers"
                                         ))]


ggplot(rr) +
  geom_line(aes(x = temp*(9/5)+32, y = estimate_RR , group = factor(hpiquartile), color = factor(hpiquartile)), size = 1.5) + 
  # geom_ribbon(aes(x = temp*(9/5)+32, ymin = LL95_RR, ymax = UL95_RR ,  fill = factor(hpiquartile)), alpha = 0.1) +
  scale_color_manual(
    name = "HPI",
    values = c("royalblue", "lightblue", "lightgreen", "darkgreen"),
    labels = c("Fewest Opportunities","Q2","Q3","Most Opportunities")
  ) +
  # scale_fill_manual(
  #   name = "HPI",
  #   values = c("royalblue", "lightblue", "lightgreen", "darkgreen")
  # ) +
  facet_grid(region~.) + 
  theme_minimal() + 
  labs(x = "Temperaure (F)", 
       y = "Relative Risk") +
  theme(legend.position = "bottom")




