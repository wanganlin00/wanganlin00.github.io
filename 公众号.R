library(ggplot2)
library(dplyr)

text <- "RadiOncology"
n_char <- nchar(text)
colors <- colorRampPalette(c("red", "green"))(n_char)

# 创建文字数据
text_data <- tibble(
    x = 1 + (seq_len(n_char) - 1) * 0.1,
    y = 1,
    char = strsplit(text, "")[[1]],
    color = colors
)

# 生成DNA双螺旋数据
y_min <- 0.2   # 垂直范围下限
y_max <- 1.8   # 垂直范围上限
x_center <- 1 + (n_char - 1)*0.1/2  # 水平居中
n_points <- 100
frequency <- 4
amplitude <- 0.15

# 生成主链数据
y_values <- seq(y_min, y_max, length.out = n_points)
x_left <- x_center + amplitude * sin(y_values * 2 * pi * frequency)
x_right <- x_center - amplitude * sin(y_values * 2 * pi * frequency)

dna_main <- data.frame(
    x = c(x_left, x_right),
    y = rep(y_values, 2),
    group = rep(c("left", "right"), each = n_points)
)

# 生成连接线数据
n_connects <- 15
y_connects <- seq(y_min, y_max, length.out = n_connects)
x_left_con <- x_center + amplitude * sin(y_connects * 2 * pi * frequency)
x_right_con <- x_center - amplitude * sin(y_connects * 2 * pi * frequency)

connects <- data.frame(
    x = x_left_con,
    xend = x_right_con,
    y = y_connects,
    yend = y_connects
)

# 创建颜色渐变
color_func <- colorRamp(c("red", "green"))
dna_main$color <- rgb(color_func((dna_main$y - y_min)/(y_max - y_min)), maxColorValue = 255)
connects$color <- rgb(color_func((connects$y - y_min)/(y_max - y_min)), maxColorValue = 255)

# 绘制图形
ggplot() +
    # 绘制DNA主链
    geom_path(data = dna_main, aes(x, y, group = group, color = color), 
              linewidth = 0.8, alpha = 0.8) +
    # 绘制连接线
    geom_segment(data = connects, aes(x = x, xend = xend, y = y, yend = yend, color = color),
                 linewidth = 0.5, alpha = 0.6) +
    # 添加文字
    geom_text(data = text_data, aes(x, y, label = char, color = color),
              size = 66, show.legend = FALSE) +
    # 颜色设置
    scale_color_identity() +
    # 坐标设置
    # coord_cartesian(xlim = c(0.5, 2.5), ylim = c(y_min, y_max), clip = "off") +
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
ggsave("公众号.png",dpi = 900)
