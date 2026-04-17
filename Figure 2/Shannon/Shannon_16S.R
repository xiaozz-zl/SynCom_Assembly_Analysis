# === Load required packages ===
library(here)
library(ggplot2)
library(dplyr)
library(ggpubr)

# === Import data ===
df <- read.csv("Figure 2/Shannon/Shannon_16S.csv")

# Preserve the order of factor levels
df$richness <- factor(df$richness, levels = unique(df$richness))

# # Color palette option 1: F7BABA tones
my_colors <- c("#ffffff", "#fdeeee", "#fbdcdc", "#f9cbcb", "#f7baba")


# === Plot boxplot without significance annotations ===
p <- ggplot(df, aes(x = richness, y = Shannon)) +
  geom_boxplot(aes(fill = richness), width = 1, outlier.shape = NA, color = "black") +
  geom_jitter(size = 3, alpha = 0.7, color = "black", width = 0.3) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, face = "plain"),
    axis.text = element_text(size = 18),
    panel.border = element_rect(color = "black", linewidth = 1.2, fill = NA),
    axis.ticks = element_line(linewidth = 1.2),
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Shannon diversity") +
  ylim(4, 7)

# === Save figure as PDF ===
# 检查根目录下是否有 output 文件夹，没有则创建一个
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
}

# 保存 PDF 到 根目录/output/ 文件夹中
ggsave(
  filename = here("output", "Shannon_16S.pdf"), 
  plot = p, 
  device = "pdf", 
  width = 8, 
  height = 6, 
  units = "in"
)

# 打印一条成功消息
message("图片已成功保存至: ", here("output", "Shannon_16S.pdf"))