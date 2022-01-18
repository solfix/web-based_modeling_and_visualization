# 单变量可视化

# 数值型变量可视化图形
numeric_vis1_types <- c("散点图"   = "scatter_t",
                        "点阵图"   = "dotplot_t",
                        "直方图"   = "histogram_t",
                        "密度图"   = "density_t",
                        "箱线图"   = "boxplot_t",
                        "小提琴图" = "violin_t")

# 因子型变量可视化图形
factor_vis1_types <- c("条形图"   = "barplot_t",
                       "饼图"     = "pie_t")

# 散点图
vis1_scatter <- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts

  p <- df %>%
    dplyr::mutate(i__=row_number()) %>%
    ggplot(aes(x=i__, y=.data[[var]])) +
    geom_point(shape=opts$char_shape,
               size =opts$char_size,
               color=opts$char_color,
               fill =opts$char_fill,
               alpha=opts$char_alpha) +
    labs(x=get_label(opts$xlab, "索引"),
         y=get_label(opts$ylab, var),
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 点图
vis1_dotplot<- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts1 <- vis_params$opts1

  bw <- (max(df[[var]])-min(df[[var]])) / opts1$binwidth_r

  p <- df %>%
    ggplot(aes(x=.data[[var]])) +
    geom_dotplot(binwidth=bw,
                 dotsize=opts1$dotsize,
                 stackratio=opts1$stackratio,
                 color=opts$char_color,
                 fill =opts$char_fill,
                 alpha=opts$char_alpha) +
    labs(x=get_label(opts$xlab, var),
         y=get_label(opts$ylab, "计数"),
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 直方图
vis1_histogram <- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts1 <- vis_params$opts1

  p <- df %>% ggplot(aes(x=.data[[var]]))

  p <- if (opts1$freq) {
    p + geom_histogram(aes(y=..density..),
                       bins=opts1$bins,
                       linetype=opts$area_linetype,
                       size=opts$area_linesize,
                       color=opts$area_linecolor,
                       fill=opts$area_fill,
                       alpha=opts$area_alpha) +
      labs(y=get_label(opts$ylab, "频率密度"))
  } else {
    p + geom_histogram(aes(y=..count..),
                       bins=opts1$bins,
                       linetype=opts$area_linetype,
                       size=opts$area_linesize,
                       color=opts$area_linecolor,
                       fill=opts$area_fill,
                       alpha=opts$area_alpha) +
      labs(y=get_label(opts$ylab, "频数"))
  }

  p <- p +
    labs(x=get_label(opts$xlab, var),
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  # 添加核密度估计曲线
  if (opts1$add_density) {
    p <- p +
      geom_density(linetype=opts$line_type,
                   size=opts$line_size,
                   color=opts$line_color,
                   alpha=opts$line_alpha)
  }

  # 添加地毯
  if (opts1$add_rug) {
    p <- p + geom_rug(linetype=opts$line_type,
                      size=opts$line_size,
                      color=opts$line_color,
                      alpha=opts$line_alpha)
  }

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 核密度估计曲线
vis1_density <- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts1 <- vis_params$opts1

  p <- df %>%
    ggplot(aes(x=.data[[var]])) +
    geom_density(linetype=opts$area_linetype,
                 size=opts$area_linesize,
                 color=opts$area_linecolor,
                 fill=opts$area_fill,
                 alpha=opts$area_alpha) +
    labs(x=get_label(opts$xlab, var),
         y=get_label(opts$xlab, "密度"),
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  # 添加地毯
  if (opts1$add_rug) {
    p <- p + geom_rug(linetype=opts$line_type,
                      size=opts$line_size,
                      color=opts$line_color,
                      alpha=opts$line_alpha)
  }

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 箱线图
vis1_boxplot <- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts1 <- vis_params$opts1

  p <- df %>% ggplot(aes(x=factor(0), y=.data[[var]]))

  p <- if (opts1$add_jitter) {
    p + geom_boxplot(notch=opts1$notch,
                     width=opts1$width,
                     linetype=opts$area_linetype,
                     size=opts$area_linesize,
                     color=opts$area_linecolor,
                     fill=opts$area_fill,
                     alpha=opts$area_alpha,
                     outlier.shape=NA) +
      geom_jitter(shape=opts$char_shape,
                  size=opts$char_size,
                  color=opts$char_color,
                  fill=opts$char_fill,
                  alpha=opts$char_alpha,
                  width=opts1$jitter_width * opts1$width)
  } else {
    p + geom_boxplot(notch=opts1$notch,
                     width=opts1$width,
                     linetype=opts$area_linetype,
                     size=opts$area_linesize,
                     color=opts$area_linecolor,
                     fill=opts$area_fill,
                     alpha=opts$area_alpha,
                     outlier.shape=opts$char_shape,
                     outlier.size=opts$char_size,
                     outlier.color=opts$char_color,
                     outlier.fill=opts$char_fill,
                     outlier.alpha=opts$char_alpha)
  }

  p <- p + labs(title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  p <- add_theme(p, global$theme, global$font_size, global$font_family)

  if(opts1$horizontal) {
    p + coord_flip() +
      labs(x=get_label(opts$ylab, var),
           y=get_label(opts$xlab, NULL)) +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  } else {
    p +
      labs(x=get_label(opts$xlab, NULL),
           y=get_label(opts$ylab, var)) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
}

# 小提琴图
vis1_violin <- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts1 <- vis_params$opts1

  p <- df %>% ggplot(aes(x=factor(0), y=.data[[var]])) +
    geom_violin(width=opts1$width,
                linetype=opts$area_linetype,
                size=opts$area_linesize,
                color=opts$area_linecolor,
                fill=opts$area_fill,
                alpha=opts$area_alpha)

  # 添加箱线图
  if (opts1$add_boxplot) {
    p <- p + geom_boxplot(width=.25*opts1$width,
                          linetype=opts$area2_linetype,
                          size=opts$area2_linesize,
                          color=opts$area2_linecolor,
                          fill=opts$area2_fill,
                          alpha=opts$area2_alpha,
                          outlier.shape=NA)
  }

  # 添加抖动散点
  if (opts1$add_jitter) {
    p <- p  +
      geom_jitter(shape=opts$char_shape,
                  size=opts$char_size,
                  color=opts$char_color,
                  fill=opts$char_fill,
                  alpha=opts$char_alpha,
                  width=opts1$jitter_width * opts1$width)
  }

  p <- p + labs(title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  p <- add_theme(p, global$theme, global$font_size, global$font_family)

  if(opts1$horizontal) {
    p + coord_flip() +
      labs(x=get_label(opts$ylab, var),
           y=get_label(opts$xlab, NULL)) +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  } else {
    p +
      labs(x=get_label(opts$xlab, NULL),
           y=get_label(opts$ylab, var)) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
}

# 条形图
vis1_barplot <- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts1 <- vis_params$opts1

  p <- df %>%
    ggplot(aes(x=.data[[var]])) +
    geom_bar(width=opts1$width,
             linetype=opts$area_linetype,
             size=opts$area_linesize,
             color=opts$area_linecolor,
             fill=opts$area_fill,
             alpha=opts$area_alpha) +
    labs(title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  p <- if(opts1$horizontal) {
    p + coord_flip() +
      labs(x=get_label(opts$ylab, "频数"),
           y=get_label(opts$xlab, var))
  } else {
    p +
      labs(x=get_label(opts$xlab, var),
           y=get_label(opts$ylab, "频数"))
  }

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 饼图
vis1_pie <- function(df, var, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts1 <- vis_params$opts1

  p <- df %>%
    group_by(.data[[var]]) %>%
    summarize(n__=n()) %>%
    ggplot(aes(x='', y=n__, fill=.data[[var]])) +
    geom_bar(stat='identity', width=1,
             linetype=opts$area_linetype,
             size=opts$area_linesize,
             color=opts$area_linecolor,
             alpha=opts$area_alpha,
             show.legend=opts$show_legend) +
    labs(fill=var,
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL)) +
    coord_polar(theta='y',
                start=opts1$start*pi / 180,
                direction=ifelse(opts1$clockwise, -1, 1))

  if (global$palette_d!="none") {
    n_colors <- n_distinct(df[[var]])
    p <- add_fill_discrete(p,
                           global$palette_family_d,
                           global$palette_d,
                           n_colors,
                           reverse=global$palette_rev_d)
  }

  if (opts$show_legend & !is.null(opts$fill_title)) {
    if (!is_empty(opts$fill_title)) {
      p <- p + labs(fill=opts$fill_title)
    }
  }

 # if (vis_params$add_theme) {
 #   add_theme(p, vis_params$theme, vis_params$font_size, vis_params$font_family)
 # } else {
  p + theme_void()
 # }
}

vis1_plot <- function(df, var, vis1_plot_type, vis1_params) {
  switch (vis1_plot_type,
          scatter_t   = vis1_scatter   (df, var, vis1_params),
          dotplot_t   = vis1_dotplot   (df, var, vis1_params),
          histogram_t = vis1_histogram (df, var, vis1_params),
          density_t   = vis1_density   (df, var, vis1_params),
          boxplot_t   = vis1_boxplot   (df, var, vis1_params),
          violin_t    = vis1_violin    (df, var, vis1_params),
          barplot_t   = vis1_barplot   (df, var, vis1_params),
          pie_t       = vis1_pie       (df, var, vis1_params),
  )
}
