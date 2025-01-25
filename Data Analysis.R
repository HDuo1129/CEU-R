library(data.table)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

country <- country_results_df
individual <- individual_results_df
timeline <- timeline_df
country <- data.table(country)
individual <- data.table(individual)
timeline <- data.table(timeline)

# Q1: Distribution of National Awards in 2024
# 计算各国奖牌总数
country[, awards_total := awards_gold + awards_silver + awards_bronze]
# 筛选出2024年的数据
country_2024 <- country[year == 2024]
# 将数据从宽格式转换为长格式
country_2024_long <- melt(country_2024, id.vars = c("year", "country"), 
                     variable.name = "medal_type", value.name = "count",
                     measure.vars = c("awards_gold", "awards_silver", "awards_bronze", "awards_total"))
# 绘制图表
ggplot(country_2024_long, aes(x = country, y = count, fill = medal_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # 使用条形图并且"dodge"使条形并排显示
  facet_wrap(~medal_type, scales = "free_y", ncol = 1) +  # 分面显示，每种奖牌类型一个面板
  labs(title = "2024 Awards by Country and Medal Type", x = "Country", y = "Number of Awards") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # 改善x轴标签的可读性
        legend.position = "none")  # 移除图例

# Q2: Changes in the Gender Ratio of Participants
ggplot(timeline, aes(x = year)) +
  geom_density(aes(y = male_contestant, fill = "Male"), stat="identity") +
  geom_density(aes(y = female_contestant, fill = "Female"), stat="identity", alpha=0.7) +
  labs(title = "Gender Ratio of Participants", x = "Year", y = "Entry") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 改善x轴标签的可读性

# Q3: IMO Global Average Score Distribution per Question
## Since the vast majority of the Question 7 data were NA, they were not analysed.
scores_long <- melt(country, measure.vars = patterns("^p[1-6]$"), variable.name = "problem", value.name = "score")
ggplot(scores_long, aes(x = problem, y = score, fill = problem)) +
  geom_boxplot() +
  labs(title = "IMO Global Average Score Distribution per Question", x = "Question", y = "Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")  # 使用 Pastel1 色板提供颜色

# Q4: Share of Female Players and Number of Gold Medals
country[, female_share := team_size_female / team_size_all]
female_share <- country[, .(female_share, awards_gold), by = country]

ggplot(female_share, aes(x = female_share, y = awards_gold)) +
  geom_point(size=3, aes(color = female_share, size = awards_gold), alpha=0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  labs(title = "Share of Female Players and Number of Gold Medals", x = "Share of Female Players", y = "Gold Medals") +
  scale_size_continuous(range = c(1, 5)) +  # 调整点的大小并隐藏图例
  scale_color_gradient(low = "blue", high = "red") +  # 添加颜色渐变
  theme_minimal() +
  theme(legend.position = "none")  # 如果不需要图例可以关闭

# Q5: Trends in China's Medal Count
china_data <- country[country == "People's Republic of China"]
china_data_long <- melt(china_data, id.vars = "year", measure.vars = c("awards_gold", "awards_silver", "awards_bronze"),
                        variable.name = "medal", value.name = "count")

ggplot(china_data_long, aes(x = year, y = count, color = medal)) +
  geom_line(size=1) +  # Use size for line thickness in geom_line
  geom_point(size=2) +  # Use size for point size in geom_point
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Trends in China's Medal Count", x = "Year", y = "Medal Count") +
  scale_color_manual(values = c("awards_gold" = "gold", "awards_silver" = "lightgrey", "awards_bronze" = "darkorange")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top")

# Q6: Team Leader Awards and Team Medals
# 首先，将奖项转换为数值并找出每个参赛者的最高奖项
individual[, award_value := fifelse(award == "Honourable mention", 1,
                                    fifelse(award == "Bronze medal", 2,
                                            fifelse(award == "Silver medal", 3,
                                                    fifelse(award == "Gold medal", 4, NA))))]
individual_max_awards <- individual[, .(max_award = max(award_value, na.rm = TRUE)), by = contestant]
# 将最大奖项数值转换回字符串形式
individual_max_awards[, max_award := fifelse(max_award == 1, "Honourable mention",
                                             fifelse(max_award == 2, "Bronze medal",
                                                     fifelse(max_award == 3, "Silver medal",
                                                             fifelse(max_award == 4, "Gold medal", "No Award"))))]
# 然后，将这个数据表与 country 数据表合并
# 合并操作，并确保过滤 NA 值
country_leader <- merge(country, individual_max_awards, by.x = "leader", by.y = "contestant", all.x = TRUE, suffixes = c("", "_leader"))
country_leader <- country_leader[!is.na(country_leader$max_award), ]  # 注意确保列名正确
country_leader$role <- "Leader"
names(country_leader)[names(country_leader) == "max_award"] <- "leader_max_award"

country_deputy_leader <- merge(country, individual_max_awards, by.x = "deputy_leader", by.y = "contestant", all.x = TRUE, suffixes = c("", "_deputy_leader"))
country_deputy_leader <- country_deputy_leader[!is.na(country_deputy_leader$max_award), ]  # 注意确保列名正确
country_deputy_leader$role <- "Deputy Leader"
names(country_deputy_leader)[names(country_deputy_leader) == "max_award"] <- "deputy_leader_max_award"

# 转换 country_leader 数据为长格式
leader_long <- melt(country_leader, id.vars = c("leader_max_award"),
                    measure.vars = c("awards_gold", "awards_silver", "awards_bronze", "awards_honorable_mentions"),
                    variable.name = "Team_Award", value.name = "Count")
# 因子化奖项以确保顺序
leader_long$Team_Award <- factor(leader_long$Team_Award,
                                 levels = c("awards_gold", "awards_silver", "awards_bronze", "awards_honorable_mentions"),
                                 labels = c("Gold", "Silver", "Bronze", "Honorable Mentions"))
leader_long$leader_max_award <- factor(leader_long$leader_max_award,
                                       levels = c("Honourable mention", "Bronze medal", "Silver medal", "Gold medal"),
                                       labels = c("Honourable Mention", "Bronze", "Silver", "Gold"))

# 绘制堆叠柱状图，显示领导下团队的奖项分布
ggplot(leader_long, aes(x = leader_max_award, y = Count, fill = Team_Award)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Team Awards Distribution by Leader's Max Award Type",
       x = "Leader's Maximum Award",
       y = "Count of Team Awards") +
  scale_fill_manual(values = c("Gold" = "gold", 
                               "Silver" = "lightgrey", 
                               "Bronze" = "darkorange",
                               "Honorable Mentions" = "purple"), name = "Team Award Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 转换 country_deputy_leader 数据为长格式
deputy_leader_long <- melt(country_deputy_leader, id.vars = c("deputy_leader_max_award"),
                           measure.vars = c("awards_gold", "awards_silver", "awards_bronze", "awards_honorable_mentions"),
                           variable.name = "Team_Award", value.name = "Count")
# 因子化奖项以确保顺序和颜色填充一致
deputy_leader_long$Team_Award <- factor(deputy_leader_long$Team_Award,
                                        levels = c("awards_gold", "awards_silver", "awards_bronze", "awards_honorable_mentions"),
                                        labels = c("Gold", "Silver", "Bronze", "Honorable Mentions"))
deputy_leader_long$deputy_leader_max_award <- factor(deputy_leader_long$deputy_leader_max_award,
                                                     levels = c("Honourable mention", "Bronze medal", "Silver medal", "Gold medal"),
                                                     labels = c("Honourable Mention", "Bronze", "Silver", "Gold"))
# 绘制堆叠柱状图，显示副领导下团队的奖项分布
ggplot(deputy_leader_long, aes(x = deputy_leader_max_award, y = Count, fill = Team_Award)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Team Awards Distribution by Deputy Leader's Max Award Type",
       x = "Deputy Leader's Maximum Award",
       y = "Count of Team Awards") +
  scale_fill_manual(values = c("Gold" = "gold", 
                               "Silver" = "lightgrey", 
                               "Bronze" = "darkorange",
                               "Honorable Mentions" = "purple"), name = "Team Award Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
