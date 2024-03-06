
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 3 Answers
## Author:        Ian McCarthy
## Date Created:  3/16/2020
## Date Edited:   3/6/2024
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               modelsummary, fixest, kableExtra)

# Read data and set workspace for knitr -------------------------------
cig.data <- readRDS('data/output/TaxBurden_Data.rds')

cig.data <- cig.data %>% group_by(state) %>% arrange(state, Year) %>%
  mutate(tax_change = tax_state - lag(tax_state),
         tax_change_d = ifelse(tax_change==0,0,1),
         price_cpi_2012 = cost_per_pack*(cpi_2012/index),
         total_tax_cpi_2012=tax_dollar*(cpi_2012/index),
         ln_tax_2012=log(total_tax_cpi_2012),
         ln_sales=log(sales_per_capita),
         ln_price_2012=log(price_cpi_2012))


# Create objects for markdown ---------------------------------------------


## tax changes
tax.change.plot <- cig.data %>% group_by(Year) %>% filter(Year<1986, Year>1970) %>%
  summarize(mean_change=mean(tax_change_d)) %>%
  ggplot(aes(x=as.factor(Year), y=mean_change)) +
  geom_bar(stat="identity") +
  labs(
    x="Year",
    y="Share of States"
  ) + ylim(0,.5) +
  theme_bw()


## tax and price amounts
tax.price.data <- cig.data %>% select(Year, state, total_tax_cpi_2012, price_cpi_2012) %>%
  group_by(Year) %>% summarize(mean_tax=mean(total_tax_cpi_2012, na.rm=TRUE),
                               mean_price=mean(price_cpi_2012, na.rm=TRUE)) %>%
  pivot_longer(cols=c("mean_tax","mean_price"),
               names_to="var", values_to="dollars")

tax.price.plot <- tax.price.data %>%
  ggplot(aes(x=Year,y=dollars,color=var)) + 
  geom_line() + 
  labs(
    x="Year",
    y="Dollars per Pack (2012 $)"
  ) + ylim(0,10) +
  geom_text(data = tax.price.data %>% filter(Year == 2012), 
            aes(label = c("Mean Tax","Mean Price"),
                x = Year,
                y = dollars-.3)) +
  scale_color_manual(values=c("black","black")) +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_continuous(breaks=seq(1970, 2020, 5))


## Price changes
cig.data.change <- cig.data %>% ungroup() %>%
  filter(Year==1970) %>% select(state, price_1970=price_cpi_2012) %>%
  left_join(cig.data %>% filter(Year==2018) %>% select(state, price_2018=price_cpi_2012),
            by=c("state")) %>%
  mutate(price_change = price_2018-price_1970)

high.change <- cig.data.change %>% slice_max(price_change, n=5) %>% mutate(change_group="high")
low.change <- cig.data.change %>% slice_min(price_change, n=5) %>% mutate(change_group="low")
change.group <- rbind(high.change, low.change)

top.bottom.price <- cig.data %>% ungroup() %>%
  inner_join(change.group %>% select(state, change_group),
            by=c("state"))

## Figure for high price changes
high.price.plot <- top.bottom.price %>% filter(change_group=="high") %>%
  ggplot(aes(x=Year,y=sales_per_capita, color=state)) + 
  stat_summary(fun="mean",geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))


## Figure for low price changes
low.price.plot <- top.bottom.price %>% filter(change_group=="low") %>%
  ggplot(aes(x=Year,y=sales_per_capita, color=state)) + 
  stat_summary(fun="mean",geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))


## Figure for high and low price changes
combined.price.plot <- top.bottom.price %>% 
  ggplot(aes(x=Year,y=sales_per_capita, linetype=change_group)) + 
  stat_summary(fun="mean",geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="Level of Price Increase"
  ) + 
  geom_text(data = top.bottom.price %>% group_by(Year, change_group) %>%
              summarize(mean_sales=mean(sales_per_capita,  na.rm=TRUE)) %>%
              filter(Year == 2016), 
            aes(label = c("High Price Change","Low Price Change"),
                x = Year-3,
                y = mean_sales-5)) +
  scale_color_manual(values=c("black","black")) +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_continuous(breaks=seq(1970, 2020, 5))


## Regression results
ols1 <- feols(ln_sales~ln_price_2012, data=cig.data %>% filter(Year<1991))
iv1 <- feols(ln_sales ~ 1 | ln_price_2012 ~ ln_tax_2012, data=cig.data %>% filter(Year<1991))
first.stage <- feols(ln_price_2012~ln_tax_2012, data=cig.data %>% filter(Year<1991))
reduced.form <- feols(ln_sales~ln_tax_2012, data=cig.data %>% filter(Year<1991))

ols2 <- feols(ln_sales~ln_price_2012, data=cig.data %>% filter(Year>=1991 & Year<=2015))
iv2 <- feols(ln_sales ~ 1 | ln_price_2012 ~ ln_tax_2012, data=cig.data %>% filter(Year>=1991 & Year<=2015))
first.stage2 <- feols(ln_price_2012~ln_tax_2012, data=cig.data %>% filter(Year>=1991 & Year<=2015))
reduced.form2 <- feols(ln_sales~ln_tax_2012, data=cig.data %>% filter(Year>=1991 & Year<=2015))

rm(list=c("cig.data", "cig.data.change", "top.bottom.price"))
save.image("analysis/Hwk3_workspace.Rdata")


