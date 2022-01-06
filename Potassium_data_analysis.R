## Packages ----
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(stringr)
library(tidyr)
library(viridis)

## Read data ----
path <- "W:/Pathology/CLINICAL CHEMISTRY/Clin_Lab_Reports/AutoLab Reports/"
files <- dir(path)

df_all <- lapply(files, function(x) {
  
  # Conditional on file name
  if (!grepl("SM|WM|\\$", x)) {
    
    # Update console for tracking purposes
    message(paste0("Processing: ", x))
    
    # Read sheet "XAC" (Alinity data from CXH) and mutate
    df <- read_excel(paste0(path, x), sheet = "XAC") %>%
      filter(`Test Code` == "K") %>%
      filter(!grepl("[:lower:]", `Location Code`)) %>% # filter for GP locations
      filter(!grepl("CXH|CX|SM|HH|WM", `Location Code`)) %>%
      mutate(
        Result_new = case_when(
          grepl("<", Result) ~ NA_real_
          ,grepl("Insufficient", Result) ~ NA_real_
          ,grepl(">", Result) ~ NA_real_
          ,TRUE ~ as.numeric(Result)
        )
        ,Date = date(parse_date_time(ResultDT, c("dmy HMS")))
        ,Month = month(Date)
        ,Month_floor = floor_date(Date, "month")
        ,Week = week(Date)
        ,Week_floor = floor_date(Date, "week")
        ,`QA Veri` = as.character(`QA Veri`)
      )
    
    return(df)
    
  } else {
    
    message(paste0("Skipping: ", x))
    return(NULL)
    
  }
  
}) %>% bind_rows()

## Summarise data by proportion above certain thresholds ----
df_summary <- df_all %>%
  group_by(Week_floor) %>%
  summarise(
    n = n()
    ,geq6.5 = sum(Result_new >= 6.5, na.rm = TRUE)
    ,geq6.0 = sum(Result_new >= 6.0, na.rm = TRUE)
    ,geq5.3 = sum(Result_new >= 5.3, na.rm = TRUE)
    ,prop_geq6.5 = geq6.5 / n
    ,prop_geq6.0 = geq6.0 / n
    ,prop_geq5.3 = geq5.3 / n
    ,se_geq6.5 = 1.96 * sqrt(prop_geq6.5 * (1 - prop_geq6.5) / n)
    ,se_geq6.0 = 1.96 * sqrt(prop_geq6.0 * (1 - prop_geq6.0) / n)
    ,se_geq5.3 = 1.96 * sqrt(prop_geq5.3 * (1 - prop_geq5.3) / n)
  ) %>% 
  select(Week_floor, starts_with("prop"), starts_with("se"))

df_sum <- pivot_longer(df_summary, -Week_floor, names_to = c("measure", "threshold"), names_sep = "_") %>%
  pivot_wider(names_from = "measure", values_from = "value")

## Plot theme ----
plot_theme <- theme(
  panel.background = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.border = element_rect(fill = NA, colour = "black")
  ,axis.ticks = element_line(colour = "black")
  ,axis.text = element_text(colour = "black")
  ,axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
  ,axis.title = element_text(colour = "black")
  ,axis.title.x = element_blank()
  ,strip.background = element_rect(fill = "grey80", colour = "black")
  ,legend.background = element_rect(fill = "white", colour = "black")
)

## Static heatmap by date ----
p1 <- ggplot(df_all, aes(x = Date, y = Result_new, group = Date))+ 
  geom_bin2d(binwidth = c(7, 0.1), col = "black")+ # bins the data by week
  geom_hline(yintercept = 4.4, col = "black", size = 1, alpha = 0.5)+
  stat_summary(
    fun = "median" # summarises the data as the median per day
    ,geom = "line"
    ,col = "red2"
    ,aes(group = 1)
    ,size = 1.5
    ,alpha = 0.75
    ,fun.args = list(na.rm = TRUE)
  )+
  plot_theme+
  ylab("Potassium (mmol/L)")+
  labs(fill = "Number of samples")+
  scale_y_continuous(breaks = seq(1.5, 8.5, 1))+
  scale_fill_viridis(option = "viridis", direction = 1)+
  scale_x_date(date_breaks = "2 weeks")

## Static plot of proportions above thresholds by week ----
p2 <- ggplot(df_sum, aes(x = Week_floor, colour = threshold))+
  geom_point(aes(group = threshold, y = prop * 100), size = 2, alpha = 0.75)+
  geom_line(aes(group = threshold, y = prop * 100), size = 1, alpha = 0.75)+
  geom_errorbar(aes(ymin = (prop - se) * 100, ymax = (prop + se) * 100), width = 0, size = 1)+
  scale_colour_manual(values = c("orange1", "orangered1", "darkred"), labels = c(">= 5.3", ">= 6.0", ">= 6.5"))+
  plot_theme+
  ylab("Proportion of samples (%)")+
  labs(colour = "Threshold (mmol/L)")+
  scale_x_date(date_breaks = "2 weeks")
  
cowplot::plot_grid(p1, p2, labels = c("A", "B"), align = "hv", axis = "l", nrow = 2)

## Interactive plot of data grouped by location ----
# Summarise data before plotting
df_practice <- df_all %>%
  group_by(Week_floor, `Location Code`) %>%
  summarise(
    median = round(median(Result_new, na.rm = TRUE), 2)
    ,lwr = round(quantile(Result_new, 0.025, na.rm = TRUE), 2)
    ,upr = round(quantile(Result_new, 0.975, na.rm = TRUE), 2)
    ,n = n()
  ) %>%
  rename(Location = `Location Code`)

highlight_key(df_practice %>% arrange(Location), ~Location) %>%
  plot_ly(x = ~Week_floor, y = ~median, color = I("black")) %>%
  plotly::group_by(Location) %>%
  add_trace(
    type = "scatter"
    ,mode = "lines+markers"
    ,text = ~Location
    ,alpha = 0.25
    ,hovertemplate = paste0(
      "<b>Date</b>: %{x}<br>"
      ,"<b>Location</b>: %{text}<br>"
      ,"<b>Median</b>: %{y} mmol/L"
    ) 
  ) %>%
  layout(
    xaxis = list(title = "Date")
    ,yaxis = list(title = "Median potassium (mmol/L)")
    ,showlegend = FALSE
    ,margin = list(b = 150)
  )%>%
  highlight(on = "plotly_selected", dynamic = TRUE, selectize = TRUE)
