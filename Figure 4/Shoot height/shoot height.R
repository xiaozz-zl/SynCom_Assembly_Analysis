
# === Load data ===
# 指向子文件夹里的文件
df <- read.csv("Figure 4/Shoot height/shoot height.csv")

# === Load required packages ===
library(ggplot2)
library(dplyr)
library(ggpubr)

# === Preprocess data ===
df$richness <- factor(df$richness, levels = unique(df$richness))

# === Define fill colors ===
fill_colors <- c(
  "CK" = "#E4EBD7",
  "1"  = "#CAD9B0",
  "2"  = "#AAC18A",
  "3"  = "#86A762",
  "4"  = "#6B8C4D"
)

# === Plot ===
y_max <- max(df$Shoot_height, na.rm = TRUE) + 5

p <- ggplot(df, aes(x = richness, y = Shoot_height, fill = richness)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(size = 3, alpha = 0.7, color = "black", width = 0.3) +
  scale_fill_manual(values = fill_colors) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26, face = "plain"),
    axis.text = element_text(size = 22),
    panel.border = element_rect(color = "black", linewidth = 1.2, fill = NA),
    axis.ticks = element_line(linewidth = 1.2),
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = expression("Shoot height (cm " * plant^{-1} * ")")) +
  ylim(75, y_max)

# === Save PDF ===
# 检查根目录下是否有 output 文件夹，没有则创建一个
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
}

# 保存 PDF 到 根目录/output/ 文件夹中
ggsave(
  filename = here("output", "shoot height.pdf"), 
  plot = p, 
  device = "pdf", 
  width = 8, 
  height = 6, 
  units = "in"
)

# 打印一条成功消息
message("图片已成功保存至: ", here("output", "shoot height.pdf"))