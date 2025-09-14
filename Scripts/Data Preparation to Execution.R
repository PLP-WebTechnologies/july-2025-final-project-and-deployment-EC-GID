# Creating the dataset based on the table
Plant <- rep(1:6, each = 7, times = 2)  # 6 plants for each group, each measured at 7 CO2 levels
Treatment <- rep(c("Control", "Chilled"), each = 21)  # 3 plants per treatment, repeated for both groups
Group <- rep(c("Quebec", "Mississippi"), each = 42)  # Two groups: Quebec and Mississippi
CO2_Concentration <- rep(c(95, 175, 250, 350, 500, 675, 1000), times = 12)  # 7 CO2 levels

# CO2 Uptake Rates from the table
CO2_Uptake <- c(
  # Quebec Control
  16.0, 30.4, 34.8, 37.2, 35.3, 39.2, 39.7,
  13.6, 27.3, 37.1, 41.8, 40.6, 41.4, 44.3,
  16.2, 30.4, 42.1, 43.6, 42.1, 43.9, 45.5,
  # Quebec Chilled
  14.2, 27.4, 30.3, 32.5, 35.4, 38.7, 38.7,
  9.3, 27.3, 27.4, 30.0, 35.1, 37.5, 42.4,
  15.1, 21.0, 38.1, 34.0, 38.9, 39.6, 41.4,
  # Mississippi Control
  10.6, 21.9, 26.2, 30.0, 30.9, 32.4, 35.5,
  12.0, 22.0, 30.6, 31.8, 31.1, 31.1, 31.5,
  11.3, 19.4, 29.4, 32.5, 28.5, 28.1, 27.7,
  # Mississippi Chilled
  10.5, 14.2, 14.7, 15.6, 19.5, 22.2, 21.9,
  7.7, 11.4, 14.9, 14.9, 17.8, 19.0, 20.4,
  8.6, 14.5, 15.0, 17.6, 19.4, 19.9, 21.4
)
Data <- data.frame(Plant, Treatment, Group, CO2_Concentration, CO2_Uptake)
Immah <- subset(Data, Treatment %in% c("Control", "Chilled"))
Immah$conc_factor <- factor(Immah$CO2_Concentration)
fit <- aov(CO2_Uptake ~ Group * Treatment * conc_factor + Error(Plant/conc_factor), 
data = Immah)
summary(fit)
Immah$Group <- interaction(Immah$Group, Immah$Treatment)
col_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
interaction.plot(x.factor = Immah$conc_factor,
                 trace.factor = Immah$Group,response = Immah$CO2_Uptake,
                 type = "b",col = col_palette,pch = 3:6,lty = 1:4,
                 xlab = "CO2 Concentration (ppm)",ylab = expression("CO"*" Uptake (µmol/m² sec)"),
                 main = "Interaction Effects: Plant Type & Treatment",legend = FALSE) # We'll add custom legend
legend("topleft", legend = levels(Immah$Group),title = "Plant Type & Treatment",
       col = col_palette,pch = 16:19,lty = 1:4, bty = "n", cex = 0.8)
boxplot(CO2_Uptake ~ Group * conc_factor, data = Immah, col = c("gold", "green3"),
        main = "CO2 Uptake by Plant Type and Concentration",
        ylab = expression("CO"*" Uptake (µmol/m² sec)"),xlab = "", axes = FALSE)
axis(2)
text(x = 1:length(unique(Immah$conc_factor)) * 2 - 0.5, y = par("usr") - 1, 
     labels = levels(Immah$conc_factor),srt = 45, adj = 1, xpd = NA)
mtext("CO2 Concentration (ppm)", side = 1, line = 6)
grid(nx = NA, ny = NULL)
par(las = 0)
par(mar = c(5, 4, 4, 2))