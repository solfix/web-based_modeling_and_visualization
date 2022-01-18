# ggplot2可视化图形选项

# 初始化可视化全局选项
init_vis_global <- function() {
  list(
    theme = "gray",            # 主题
    font_family = "",          # 字体家族
    font_size = 16,            # 字体大小
    palette_family_d = "base", # 离散调色板家族
    palette_d = "none",        # 离散调色板
    palette_rev_d = F,         # 离散调色板颜色反序
    palette_family_c = "base", # 连续调色板家族
    palette_c = "none",        # 连续调色板
    palette_rev_c = F          # 连续调色板颜色反序
  )
}

# 初始化图形基本选项
init_vis_options <- function() {
  list(
    title    = NULL,          # 主标题
    subtitle = NULL,          # 副标题
    xlab     = NULL,          # x轴标签
    ylab     = NULL,          # y轴标签

    show_legend = TRUE,       # 显示图例
    color_title = NULL,       # 色彩图例标题
    fill_title  = NULL,       # 填充图例标题
    char_size_title = NULL,   # 符号大小图例标题

    char_shape = 16,          # 符号形状
    char_size  = 2,           # 符号大小
    char_color = "black",     # 符号颜色
    char_fill  = "gray50",    # 符号填充色
    char_alpha = 0.5,         # 符号透明度

    line_type  = 1,           # 线条形状
    line_size  = 1,           # 线条大小
    line_color = "black",     # 线条颜色
    line_alpha = 0.5,         # 线条透明度

    area_linetype  = 1,       # 区域边界线形状
    area_linesize  = 0.5,     # 区域边界线宽度
    area_linecolor = "black", # 区域边界线颜色
    area_fill = "lightgray",  # 区域填充色
    area_alpha = 0.5,         # 区域透明度

    # 添加图层的图形选项
    char2_shape = 16,
    char2_size  = 2,
    char2_color = "black",
    char2_fill  = "gray50",
    char2_alpha = 0.5,

    line2_type  = 1,
    line2_size  = 1,
    line2_color = "black",
    line2_alpha = 0.5,

    area2_linetype  = 1,
    area2_linesize  = 0.5,
    area2_linecolor = "black",
    area2_fill = "lightgray",
    area2_alpha = 0.5
  )
}

# (1) 单变量可视化选项
init_vis1_options <- function() {
  list(
    # (1-0) 散点图：无特殊选项

    # (1-1) 点图：
    binwidth_r = 30,
    dotsize = 1,
    stackratio = 1,

    # (1-2) 直方图
    bins = 6,                 # 分箱数
    freq = F,                 # 频率密度
    add_density = F,          # 添加估计的核密度曲线
    add_rug = F,              # 添加地毯

    # (1-3) 核密度图：具有add_rug

    # (1-4) 箱线图
    width = .7,               # 箱的宽度
    notch = F,                # 凹槽
    horizontal = F,           # 水平方向
    add_jitter = F,           # 添加抖动散点
    jitter_width = .3,        # 抖动散点宽度=2x

    # (1-5) 小提琴图具有箱线图除notch外所有其他选项
    add_boxplot = F,          # 添加箱线图

    # (1-6) 条形图：具有width, horizontal

    # (1-7) 饼图
    start = 0,                # 初始角度
    clockwise = F             # 顺时针
  )
}

# (2) 双变量可视化选项

init_vis2_options <- function() {
  list(
    # (2-0) 散点图：
    scatter_margin_type = "none", # 边缘图形类型
    scatter_margins = "both",     # 边缘
    add_rug = F,                  # 添加地毯

    # (2-1) 平滑散点图：
    add_scatter = T,                   # 添加散点
    user_palette = T,                  # 自定义调色板
    user_palette_low = "white",        # 自定义调色板低阶色
    user_palette_high = "dodgerblue4", # 自定义调色板高阶色

    # (2-2) 二维密度图：add_scatter
    # (2-3) 散列图：无特殊选项
    # (2-4) 克利夫兰点图：无特殊选项

    # (2-5) 密度图
    position = "stack",       # 位置

    # (2-6) 山峦图：无特殊选项

    # (2-7) 箱线图
    width = .7,               # 箱的宽度
    notch = F,                # 凹槽
    horizontal = F,           # 水平方向
    box_fill = F,             # 填充不同颜色
    add_jitter = F,           # 添加抖动散点
    jitter_width = .3,        # 抖动散点宽度=2x

    # (2-8) 小提琴图具有箱线图除notch外所有其他选项
    add_boxplot = F          # 添加箱线图
  )
}

