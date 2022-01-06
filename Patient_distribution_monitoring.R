## Packages ----
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(readxl)
library(stringr)

## Read data ----
# Main tumour markers
analytes <- c("CA125", "CA153", "CA199", "PSA", "CEA", "AFP", "HCG", "PSARAT", "SCC")
path <- "W:/Pathology/CLINICAL CHEMISTRY/Clin_Lab_Reports/Oncology Mthly Reports/"

# 12th floor markers
analytes <- c("HCGE", "HCGTM", "HCGU", "IL6", "INHIB", "NSE", "PLAP")
path <- "W:/Pathology/CLINICAL CHEMISTRY/Clin_Lab_Reports/Oncology2 Mthly Reports/"

files <- dir(path)
df_all <- lapply(files, function(x) {
  message("Processing: ", x)
  df <- read_excel(paste0(path, x)) %>%
    mutate(
      Result_new = case_when(
        grepl("<", Result) ~ NA_real_
        ,grepl("Insufficient", Result) ~ NA_real_
        ,grepl(">", Result) ~ NA_real_
        ,TRUE ~ as.numeric(Result)
      )
      ,Date = parse_date_time(ResultDT, c("dmy HMS"))
      ,Month = month(Date)
      ,Month_floor = floor_date(Date, "month")
      ,Week = week(Date)
      ,Week_floor = floor_date(Date, "week")
    ) %>%
    filter(`Test Code` %in% analytes)
  return(df)
}) %>% bind_rows()

## plot of data by week ----
ggplot(df_all, aes(x = Week_floor, y = Result_new, group = Week_floor))+
  # geom_jitter(alpha = 0.1, width = 0.2)+
  geom_violin(draw_quantiles = c(0.055, 0.5, 0.945), fill = NA, adjust = 2)+ # 89% interval
  stat_summary(fun = "median", geom = "line", colour = "red2", aes(group = 1), size = 1, alpha = 0.5)+
  stat_summary(fun = "quantile", geom = "line", colour = "mediumorchid", aes(group = 1), size = 1, alpha = 0.5, fun.args = list(probs = 0.055))+
  stat_summary(fun = "quantile", geom = "line", colour = "mediumorchid", aes(group = 1), size = 1, alpha = 0.5, fun.args = list(probs = 0.945))+
  facet_wrap(~`Test Code`, scale = "free_y")+
  theme(
    panel.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.border = element_rect(fill = NA, colour = "black")
    ,axis.ticks = element_line(colour = "black")
    ,axis.text = element_text(colour = "black")
    ,axis.title = element_text(colour = "black")
    ,strip.background = element_rect(fill = "grey80", colour = "black")
  )+
  scale_y_log10()+
  expand_limits(y = 0)+
  xlab("Month")+
  ylab("Analyte concentration")
