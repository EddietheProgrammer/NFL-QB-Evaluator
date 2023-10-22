library(ggplot2)
library(utils)
NFL_ELO <- read.csv('/Users/calvi/Downloads/Weighted_Values_Valuation_With_Salary.csv')
model <- lm(AAV ~ QB_Valuation , data = NFL_ELO)
data <- NFL_ELO


library(ggplot2)
library(ggdist)

library(ggplot2)

# Calculate mean values for QB_Valuation and AAV
mean_QB_Valuation <- mean(NFL_ELO$QB_Valuation)
mean_AAV <- mean(NFL_ELO$AAV)

ggplot(data = NFL_ELO, aes(x = QB_Valuation, y = AAV)) +
  geom_point(aes(color = factor(QB_Valuation)), size = 4) +
  geom_vline(xintercept = mean_QB_Valuation, color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = mean_AAV, color = "blue", linetype = "dashed", linewidth = 0.8) +
  geom_text_repel(aes(label = player_name),
                  box.padding = 0.45,
                  size = 3,
                  family = "Roboto",
                  fontface = "bold") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma()) +
  labs(x = "Non-Play Action Yards",
       y = "Play Action Yards",
       title = "Cumulative Passing Yards",
       subtitle = "Non-Play Action vs. Play Action") +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(family = "Roboto",
                              size = 10, 
                              color = "black"),
    axis.text = element_text(family = "Roboto",
                             face = "bold",
                             size = 10,
                             color = "black"),
    plot.title.position = "plot",
    plot.title = element_text(family = "Roboto",
                              size = 16,
                              face = "bold",
                              color = "#E31837",
                              vjust = .02,
                              hjust = 0.5),
    plot.subtitle = element_text(family = "Roboto",
                                 size = 12,
                                 color = "black",
                                 hjust = 0.5),
    plot.caption = element_text(family = "Roboto",
                                size = 8,
                                face = "italic",
                                color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.border = element_blank()
  )




