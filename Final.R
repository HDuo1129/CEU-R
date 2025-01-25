# Option 1: tidytuesdayR package 
# install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2024-09-24')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 39)

country_results_df <- tuesdata$country_results_df
individual_results_df <- tuesdata$individual_results_df
timeline_df <- tuesdata$timeline_df

# Option 2: Read directly from GitHub

country_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/country_results_df.csv')
individual_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/individual_results_df.csv')
timeline_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/timeline_df.csv')


# Scraping IMO results data

library(tidyverse)
library(rvest)
library(janitor)
library(httr2)

timeline_df <- read_html("https://www.imo-official.org/organizers.aspx") %>%
  html_table() %>%
  .[[1]] %>%
  clean_names() %>%
  rename(
    "all_contestant" = contestants,
    "male_contestant" = contestants_2,
    "female_contestant" = contestants_3,
    "edition" = number
  ) %>%
  filter(edition != "#") %>%
  mutate(
    start_date = paste0(gsub("(.*)(-)(.*)", "\\1", date),year),
    end_date = paste0(gsub("(.*)(-)(.*)", "\\3", date),year),
    across(
      c(start_date, end_date),
      ~as.Date(.x, format = "%d.%m.%Y")
    ),
    across(
      c(edition, year, countries, all_contestant, male_contestant, female_contestant),
      as.integer
    )
  ) %>%
  select(-date) %>%
  # only keeping records till current year
  filter(year < 2025)

# circulate through country results link and rbind tables
scrape_country <- function(year) {
  paste0("https://www.imo-official.org/year_country_r.aspx?year=", year) %>%
    read_html() %>%
    html_table() %>%
    .[[1]] %>%
    clean_names() %>%
    filter(country != "Country") %>%
    mutate(year = year, .before = "country") 
}

country_results_df <- map_df(timeline_df$year, scrape_country) %>%
  select(
    year,
    country,
    team_size_all = team_size,
    team_size_male = team_size_2,
    team_size_female = team_size_3,
    starts_with("p"),
    awards_gold = awards,
    awards_silver = awards_2,
    awards_bronze = awards_3,
    awards_honorable_mentions = awards_4,
    leader,
    deputy_leader
  ) %>% 
  mutate(
    across(
      c(team_size_all:awards_honorable_mentions),
      as.integer
    )
  )


# circulate through individual results link and rbind tables
scrape_individual <- function(year) {
  # These can time out, so we'll use httr2 to retry.
  paste0("https://www.imo-official.org/year_individual_r.aspx?year=", year) %>%
    httr2::request() %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform() %>%
    httr2::resp_body_html() %>%
    html_table() %>%
    .[[1]] %>%
    clean_names() %>%
    mutate(year = year, .before = "contestant") 
}

individual_results_df <- map_df(timeline_df$year, scrape_individual) %>%
  select(
    year:p6, p7, total,
    individual_rank = number_rank,
    award
  ) %>%
  mutate(
    across(
      c(year, p1:individual_rank),
      as.integer
    )
  )


library(data.table)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

country <- country_results_df
individual <- individual_results_df
timeline <- timeline_df
rm(country_results_df, individual_results_df, timeline_df)

head(country)
head(individual)
head(timeline)

country <- data.table(country)
individual <- data.table(individual)
timeline <- data.table(timeline)

# 计算各国奖牌总数
country[, awards_total := awards_gold + awards_silver + awards_bronze]

# 筛选出2024年的数据
country_2024 <- country[year == 2024]

# 将数据从宽格式转换为长格式
country_long <- melt(country_2024, id.vars = c("year", "country"), 
                     variable.name = "medal_type", value.name = "count",
                     measure.vars = c("awards_gold", "awards_silver", "awards_bronze", "awards_total"))

# 绘制图表
ggplot(country_long, aes(x = country, y = count, fill = medal_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # 使用条形图并且"dodge"使条形并排显示
  facet_wrap(~medal_type, scales = "free_y", ncol = 1) +  # 分面显示，每种奖牌类型一个面板
  labs(title = "2024 Awards by Country and Medal Type", x = "Country", y = "Number of Awards") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # 改善x轴标签的可读性
        legend.position = "none")  # 移除图例


# 性别比例可视化
ggplot(timeline, aes(x = year)) +
  geom_density(aes(y = male_contestant, fill = "Male"), stat="identity") +
  geom_density(aes(y = female_contestant, fill = "Female"), stat="identity", alpha=0.7) +
  labs(title = "参赛者性别比例变化", x = "年份", y = "参赛人数") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()


# 6题箱线图
country[, p7 := NULL]

# 计算每一题的全球平均分
scores_long <- melt(country, measure.vars = patterns("^p[1-6]$"), variable.name = "problem", value.name = "score")

# 绘制每一题的全球平均分数分布的箱线图
ggplot(scores_long, aes(x = problem, y = score, fill = problem)) +
  geom_boxplot() +
  labs(title = "IMO全球每题平均分数分布（题目1-6）", x = "题目", y = "分数") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")  # 使用 Pastel1 色板提供颜色


# 分析团队大小与金牌数的关系
country[, female_share := team_size_female / team_size_all]
team_size_vs_gold <- country[, .(
  female_share,
  awards_gold
), by = country]

# 绘制散点图
ggplot(country, aes(x = female_share, y = awards_gold)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "团队规模与金牌数量的关系", x = "平均团队规模", y = "金牌数量") +
  theme_minimal() +
  scale_color_brewer(palette = "Set3")

# 中国研究
china_data <- country[country == "People's Republic of China"]

china_data_long <- melt(china_data, id.vars = "year", measure.vars = c("awards_gold", "awards_silver", "awards_bronze"),
                        variable.name = "medal", value.name = "count")

ggplot(china_data_long, aes(x = year, y = count, color = medal)) +
  geom_line(size=1.2) +  # Use size for line thickness in geom_line
  geom_point(size=3) +  # Use size for point size in geom_point
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "中国历年金银铜奖牌数量", x = "年份", y = "奖牌数量") +
  scale_color_manual(values = c("awards_gold" = "gold", "awards_silver" = "lightgrey", "awards_bronze" = "darkorange")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top")




# 教练员匹配
accumulate_award_types <- function(name) {
  if (name %in% individual$contestant) {
    # 将所有找到的奖牌类型合并为一个字符串
    awards_list <- individual[contestant == name, paste(award, collapse=", ")]
    return(awards_list)
  } else {
    return("")
  }
}
# 应用函数获取每个领导和副领导的奖牌类型
country[, leader_awards := sapply(leader, accumulate_award_types)]
country[, deputy_leader_awards := sapply(deputy_leader, accumulate_award_types)]
print(head(country, 3))



calculate_total_awards <- function(name) {
  if (name %in% individual$contestant) {
    # 转换奖项为数值
    points <- individual[contestant == name, .(award)][, award := fifelse(award == "Honourable mention", 1,
                                                                          fifelse(award == "Bronze medal", 2,
                                                                                  fifelse(award == "Silver medal", 3,
                                                                                          fifelse(award == "Gold medal", 4, NA))))]
    # 计算总分
    total_points <- sum(points$award, na.rm = TRUE)
    return(total_points)
  } else {
    return(0)
  }
}

# 应用该函数到country数据集
country[, leader_total_awards := sapply(leader, calculate_total_awards)]
country[, deputy_leader_total_awards := sapply(deputy_leader, calculate_total_awards)]

# 将队长和副队长的数据合并到一个数据框中
leader_data <- country[, .(role = "Leader", total_awards = awards_total, total_points = leader_total_awards)]
deputy_leader_data <- country[, .(role = "Deputy Leader", total_awards = awards_total, total_points = deputy_leader_total_awards)]
# 合并这两个数据框
combined_data <- rbind(leader_data, deputy_leader_data)

ggplot(combined_data, aes(x = total_points, y = total_awards, color = total_points)) +
  geom_point(size=4, alpha=0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~role, scales = "free") +
  labs(title = "领导和副领导的总奖项数对团队总奖牌数的影响",
       x = "个人历史总奖项数",
       y = "团队总奖牌数") +
  scale_color_continuous(name = "个人历史总奖项数") +
  theme_minimal() +
  theme(legend.position = "bottom")





# 定义一个函数，用于找出并保存最高奖项
find_highest_award <- function(name) {
  if (name %in% individual$contestant) {
    # 提取与当前名字匹配的奖项，并将它们转换为数值
    highest_award <- individual[contestant == name, .(award)][
      , award := fifelse(award == "Honourable mention", 1,
                         fifelse(award == "Bronze medal", 2,
                                 fifelse(award == "Silver medal", 3,
                                         fifelse(award == "Gold medal", 4, NA))))]
    # 忽略NA并找出最大值
    max_award <- max(highest_award$award, na.rm = TRUE)  # 使用 na.rm = TRUE 来忽略NA值
    
    # 将最大数值奖项转换回其对应的字符串表示
    if (!is.na(max_award)) {  # 检查max_award是否为NA
      return(fifelse(max_award == 1, "Honourable mention",
                     fifelse(max_award == 2, "Bronze medal",
                             fifelse(max_award == 3, "Silver medal",
                                     fifelse(max_award == 4, "Gold medal", "No Award")))))
    } else {
      return("No Awards")
    }
  } else {
    return("No Contestant Records")
  }
}


# 应用该函数到country数据集
country[, leader_highest_award := sapply(leader, find_highest_award)]
country[, deputy_leader_highest_award := sapply(deputy_leader, find_highest_award)]

# 对最高奖项进行因子化处理，以保证顺序和完整性
country$leader_highest_award <- factor(country$leader_highest_award, levels = c("Honourable mention", "Bronze medal", "Silver medal", "Gold medal"))

ggplot(country, aes(x = leader_highest_award)) +
  geom_bar(aes(fill = leader_highest_award), show.legend = FALSE) +
  labs(title = "领导获得的最高奖项分布", x = "最高奖项", y = "频数") +
  theme_minimal()