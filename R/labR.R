library(ggplot2)
library(dplyr)

data <- read.csv("1_2006-2018 EPL stats.csv")

head(data)
str(data)

missing_values <- colSums(is.na(data))
print(missing_values)

summary(data$goals)

goals_hist <- ggplot(data, aes(x = goals)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Розподіл забитих голів", x = "Голи", y = "Частота") +
  theme_minimal()


ggsave("goals_hist.png", plot = goals_hist, width = 8, height = 6, dpi = 300)


top_teams <- data %>%
  arrange(desc(total_yel_card)) %>%
  head(10)

top_teams_plot <- ggplot(top_teams, aes(x = reorder(team, -total_yel_card))) +
  geom_bar(aes(y = total_yel_card), stat = "identity", fill = "gold", alpha = 0.8) +
  geom_bar(aes(y = total_red_card), stat = "identity", fill = "red", alpha = 0.5) +
  labs(title = "Топ-10 команд за кількістю карток", x = "Команда", y = "Кількість карток") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("top_teams_cards.png", plot = top_teams_plot, width = 8, height = 6, dpi = 300)


season_trend <- data %>%
  group_by(season) %>%
  summarise(avg_goals = mean(goals, na.rm = TRUE))


season_trend_plot <- ggplot(season_trend, aes(x = season, y = avg_goals)) +
  geom_line(group = 1, color = "blue") +
  geom_point(size = 2, color = "darkblue") +
  labs(title = "Середня кількість голів за сезонами", x = "Сезон", y = "Середня кількість голів") +
  theme_minimal()


ggsave("season_trend_plot.png", plot = season_trend_plot, width = 8, height = 6, dpi = 300)


wins_hist <- ggplot(data, aes(x = wins)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Розподіл перемог", x = "Перемоги", y = "Частота") +
  theme_minimal()


ggsave("wins_hist.png", plot = wins_hist, width = 8, height = 6, dpi = 300)
