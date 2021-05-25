# Moral, Oltra-Cucarella, Diaz-Orueta
# Scripts for reproducing the figures in the paper

library(tidyverse)
library(ggplot2)
library(ggpubr)

load("moral_et_al_data_for_plots.RData")

## F tests
fplot <- summary_signif %>%
  pivot_longer(3:4, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .5) +
  geom_line() +
  facet_wrap(~ coef) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Percentage of p-value < 0.05") +
  ggtitle("Significance of F-test")

png("f_tests.png", res = 800, units = "in", w = 6, h = 4)
fplot
dev.off()

## TPR and TNR and ACC
tpr <- summary_rci_TPR %>%
  pivot_longer(1:2, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Mean TPR (%)") +
  facet_wrap(~ threshold) +
  ggtitle("True Positive Rate") +
  ylim(60,100)

tnr <- summary_rci_TNR %>%
  pivot_longer(1:2, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Mean TNR (%)") +
  facet_wrap(~ threshold) +
  ggtitle("True Negative Rate") +
  ylim(60,100)

acc <- summary_rci_ACC %>%
  pivot_longer(1:2, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Mean accuracy (%)") +
  facet_wrap(~ threshold) +
  ggtitle("Accuracy") +
  ylim(60,100)

png("tpr.png", res = 800, units = "in", w = 6, h = 8)
ggarrange(tpr, tnr, acc, ncol = 1, common.legend = TRUE, legend = "bottom")
dev.off()

## HNP results
hnp1 <- summary_hnp %>%
  pivot_longer(1:2, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  geom_hline(yintercept = 5, lty = 2) +
  xlab("Sample size") +
  ylab("Mean % of points outside envelope") +
  ylim(0,100) +
  ggtitle("Model goodness-of-fit")

hnp2 <- summary_rci_hnp %>%
  pivot_longer(1:2, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Mean % of points outside envelope") +
  ggtitle("Normality of RCI based on half-normal plot") +
  ylim(0, 100) +
  geom_hline(yintercept = 5, lty = 2)

png("hnp.png", res = 800, units = "in", w = 10, h = 5)
ggarrange(hnp1, hnp2, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

## Coverage of CI
ci90 <- summary_cover90 %>%
  pivot_longer(3:4, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  facet_wrap(~ coef) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Coverage rate (%)") +
  ggtitle("Coverage of 90% CI") +
  ylim(0,100) +
  geom_hline(yintercept = 90, lty = 2)

ci95 <- summary_cover95 %>%
  pivot_longer(3:4, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  facet_wrap(~ coef) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Coverage rate (%)") +
  ggtitle("Coverage of 95% CI") +
  ylim(0,100) +
  geom_hline(yintercept = 95, lty = 2)

ci99 <- summary_cover99 %>%
  pivot_longer(3:4, names_to = "model", values_to = "percentage") %>%
  ggplot(aes(x = sample_size2, y = percentage, col = model)) +
  theme_bw() +
  geom_point(alpha = .8) +
  geom_line(alpha = .8) +
  facet_wrap(~ coef) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = sample_sizes) +
  xlab("Sample size") +
  ylab("Coverage rate (%)") +
  ggtitle("Coverage of 99% CI") +
  ylim(0,100) +
  geom_hline(yintercept = 99, lty = 2)

png("coverage.png", res = 800, units = "in", w = 18, h = 5)
ggarrange(ci90, ci95, ci99, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()