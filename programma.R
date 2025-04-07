library(dplyr)
library(ggplot2)
1#
kordat <- read.table("variants4.txt", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1)
2#
cols_to_factor <- names(kordat)[9:ncol(kordat)]
kordat[cols_to_factor] <- lapply(kordat[cols_to_factor], as.factor)
3#
sink("results.txt")
4#
cat("kopsavilkums par faktoru līmeņiem:\n")
for (col in cols_to_factor) {
  cat("\nFaktors:", col, "\n")
  print(table(kordat[[col]]))
}
5#
sl.by.b <- split(kordat$Slope, kordat$b)
cat("\n'Slope' vērtības sadalītas pēc b faktora:\n")
print(sl.by.b)
6#
kordat$Slope <- as.numeric(kordat$Slope)
kordat$Intercept <- as.numeric(kordat$Intercept)
kordat$adj.r.squared <- as.numeric(kordat$adj.r.squared)
kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)
7#
cat("\nStandartnovirze pa f faktora līmeņiem:\n")
for (col in names(kordat)[sapply(kordat, is.numeric)]) {
  sd_data <- aggregate(kordat[[col]], by = list(kordat$f), FUN = sd)
  colnames(sd_data) <- c("f", paste0("SD_", col))
  print(sd_data)
}
8#
if (any(kordat$adj.r.squared > 0, na.rm = TRUE)) {
  prockordat <- kordat[kordat$adj.r.squared > 0.7, ]
} else {
  prockordat <- kordat[kordat$adj.r.squared > -0.3, ]
}
9#
prockordat$Slope <- 1 - 1 / prockordat$Slope
10#
cat("\nProckordat (filtrēts un pārveidots):\n")
print(prockordat)

sink()
11#
ggplot(kordat, aes(x = MAD, y = Average)) +
  geom_point(color = "blue") +
  labs(title = "Izkliedes grafiks", x = "MAD", y = "Average") +
  theme_minimal()
ggsave("scatter.svg")
12#
ggplot(kordat, aes(x = f, y = Intercept, fill = f)) +
  geom_boxplot() +
  labs(title = "Kastīšu grafiks pēc f faktora", x = "f faktors", y = "Intercept") +
  theme_minimal()
ggsave("boxplot.svg")