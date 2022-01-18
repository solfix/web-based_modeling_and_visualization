# 双变量可视化

library(ggpubr)
library(ggExtra)
library(ggridges)
library(ggmosaic)

vis2_types <- list(

  TT = c("散点图"         = "scatter_t",
         "光滑散点图"     = "smooth_scatter_t",
         "二维密度图" = "density2d_t"),

  TF = c("散列图"         = "strip_t",
         "克利夫兰点图"   = "cleveland_t",
         "密度图"         = "density_t",
         "山峦图"         = "ridge_t"),

  FT = c("箱线图"        = "boxplot_t",
         "小提琴图"      = "violin_t"),

  FF = c("条形图"        = "barplot_t",
         "气泡图"        = "bubble_t",
         "马赛克图"      = "mosaic_t")
)

get_vis2_type <- function(x, y) {

  c(is.numeric(x), is.numeric(y)) %>%
    factor(levels = c(FALSE, TRUE), labels = c("F", "T")) %>%
    str_c(collapse = "")
}

vis2_plot <- function(df, var1, var2, plot_type, vis_params) {
  switch (plot_type,
          scatter_t = vis2_scatter(df, var1, var2, vis_params),
          smooth_scatter_t = vis2_smooth_scatter(df, var1, var2, vis_params),
          density2d_t = vis2_density2d(df, var1, var2, vis_params),
          strip_t = vis2_strip(df, var1, var2, vis_params),
          cleveland_t = vis2_cleveland(df, var1, var2, vis_params),
          density_t = vis2_density(df, var1, var2, vis_params),
          ridge_t = vis2_ridge(df, var1, var2, vis_params),
          boxplot_t = vis2_boxplot(df, var1, var2, vis_params),
          violin_t = vis2_violin(df, var1, var2, vis_params),
          barplot_t = vis2_barplot(df, var1, var2, vis_params),
          bubble_t = vis2_bubble(df, var1, var2, vis_params),
          mosaic_t = vis2_mosaic(df, var1, var2, vis_params),
  )
}

# (1) 散点图
#
# 添加边缘图形：
## ggExtra::ggMarginal: <https://zhuanlan.zhihu.com/p/373656061>
## ggpubr::ggscatterhist: https://www.sci666.com.cn/61232.html
##     ggscatterhist(df, x=var1, y=var2, margin.plot=opts2$scatter_margin, print=F)$sp

scatter_margin <- c("无"="none", "密度图"="density", "直方图"="histogram", "箱线图"="boxplot", "小提琴图"="violin")

vis2_scatter <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts2 <- vis_params$opts2

  p <- df %>%
    ggplot(aes(x=.data[[var1]], y=.data[[var2]])) +
    geom_point(shape=opts$char_shape,
               size =opts$char_size,
               color=opts$char_color,
               fill =opts$char_fill,
               alpha=opts$char_alpha) +
    labs(x=get_label(opts$xlab, var1),
         y=get_label(opts$ylab, var2),
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  if (opts2$add_rug) {
    p <- p + geom_rug(linetype=opts$line_type,
                      size=opts$line_size,
                      color=opts$line_color,
                      alpha=opts$line_alpha,
                      position="jitter")
  }

  p <- add_theme(p, global$theme, global$font_size, global$font_family)

  if (opts2$scatter_margin_type!="none") {
    ggMarginal(p, type=opts2$scatter_margin_type, margins=opts2$scatter_margins,
               xparams=list(linetype=opts$area_linetype,
                            size=opts$area_linesize,
                            color=opts$area_linecolor,
                            fill=opts$area_fill,
                            alpha=opts$area_alpha),
               yparams=list(linetype=opts$area_linetype,
                            size=opts$area_linesize,
                            color=opts$area_linecolor,
                            fill=opts$area_fill,
                            alpha=opts$area_alpha))
  } else {
    p
  }
}

# (2) 平滑散点图
# 方法来源：https://wahani.github.io/2015/12/smoothScatter-with-ggplot2/

vis2_smooth_scatter <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts2 <- vis_params$opts2

  p <- df %>%
    ggplot(aes(x=.data[[var1]], y=.data[[var2]])) +
    stat_density2d(aes(fill=..density..^0.25), geom="tile", contour=FALSE, n=200, show.legend=opts$show_legend)

  if (opts2$user_palette) {
    p <- p + scale_fill_continuous(low=opts2$user_palette_low, high=opts2$user_palette_high)
  } else {
    if (global$palette_c!="none") {
      p <- add_fill_continuous(p,
                          global$palette_family_c,
                          global$palette_c,
                          n=100,
                          reverse=global$palette_rev_c)
    }
  }

  p <- p + labs(x=get_label(opts$xlab, var1),
                y=get_label(opts$ylab, var2),
                title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  if (opts$show_legend & !is.null(opts$fill_title)) {
    if (!is_empty(opts$fill_title)) {
      p <- p + labs(fill=opts$fill_title)
    }
  }

  if(opts2$add_scatter) {
    p <- p + geom_point(shape=opts$char_shape,
                        size =opts$char_size,
                        color=opts$char_color,
                        fill =opts$char_fill,
                        alpha=opts$char_alpha)
  }

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# (3) 二维密度图
vis2_density2d <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts2 <- vis_params$opts2

  p <- df %>%
    ggplot(aes(x=.data[[var1]], y=.data[[var2]])) +
    geom_density2d(linetype=opts$line_type,
                   size=opts$line_size,
                   color=opts$line_color,
                   alpha=opts$line_alpha)

  if(opts2$add_scatter) {
    p <- p + geom_point(shape=opts$char_shape,
                        size =opts$char_size,
                        color=opts$char_color,
                        fill =opts$char_fill,
                        alpha=opts$char_alpha)
  }

  p <- p + labs(x=get_label(opts$xlab, var1),
                y=get_label(opts$ylab, var2),
                title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# (4) 散列图
vis2_strip <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts

  p <- df %>%
    ggplot(aes(x=.data[[var1]], y=.data[[var2]])) +
    geom_point(shape=opts$char_shape,
               size =opts$char_size,
               color=opts$char_color,
               fill =opts$char_fill,
               alpha=opts$char_alpha) +
    labs(x=get_label(opts$xlab, var1),
         y=get_label(opts$ylab, var2),
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# (5) 克利夫兰点图
vis2_cleveland <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts

  p <- df %>%
    mutate(i__=row_number()) %>%
    ggpubr::ggdotchart(x="i__", y=var1,
                       group=var2, color=var2,
                       shape=opts$char_shape,
                       size =opts$char_size,
                       alpha=opts$char_alpha,
                       show.legend=opts$show_legend,
                       rotate=TRUE,
                       sorting="descending")

  if (global$palette_d!="none") {
    n_colors <- n_distinct(df[[var2]])
    p <- add_color_discrete(p,
                            global$palette_family_d,
                            global$palette_d,
                            n_colors,
                            reverse=global$palette_rev_d)
  }

  p <- p + labs(x=get_label(opts$ylab, NULL),
       y=get_label(opts$xlab, var1),
       title=get_label(opts$title, NULL),
       subtitle=get_label(opts$subtitle, NULL))

  if (opts$show_legend & !is.null(opts$color_title)) {
    if (!is_empty(opts$color_title)) {
      p <- p + labs(color=opts$color_title)
    }
  }

  add_theme(p, global$theme, global$font_size, global$font_family) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
}

# （6）密度图
vis2_density <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts2 <- vis_params$opts2

  p <- df %>%
    ggplot(aes(x=.data[[var1]], fill=.data[[var2]])) +
    geom_density(position=opts2$position,
                 linetype=opts$line_type,
                 size=opts$line_size,
                 color=opts$line_color,
                 alpha=opts$line_alpha,
                 show.legend=opts$show_legend)


  if (global$palette_d!="none") {
    n_colors <- n_distinct(df[[var2]])
    p <- add_fill_discrete(p,
                           global$palette_family_d,
                           global$palette_d,
                           n_colors,
                           reverse=global$palette_rev_d)
  }

  p <- p + labs(x=get_label(opts$xlab, var1),
                y=get_label(opts$ylab, "密度"),
                title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  if (opts$show_legend & !is.null(opts$fill_title)) {
    if (!is_empty(opts$fill_title)) {
      p <- p + labs(fill=opts$fill_title)
    }
  }

  add_theme(p, global$theme, global$font_size, global$font_family)
}

#（7）山峦图
vis2_ridge <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts

  p <- df %>%
    ggplot(aes(x=.data[[var1]], y=.data[[var2]], fill=..density..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, show.legend=opts$show_legend) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))

  if (global$palette_c!="none") {
    p <- add_fill_continuous(p,
                           global$palette_family_c,
                           global$palette_c,
                           n=100,
                           reverse=global$palette_rev_c)
  }

  p <- p + labs(x=get_label(opts$xlab, var1),
                y=get_label(opts$ylab, var2),
                title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  if (opts$show_legend & !is.null(opts$fill_title)) {
    if (!is_empty(opts$fill_title)) {
      p <- p + labs(fill=opts$fill_title)
    }
  }

  p + theme_ridges(font_size=global$font_size, grid = TRUE)
}

vis2_boxplot <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts2 <- vis_params$opts2

  p <- if (opts2$box_fill) {
    df %>% ggplot(aes(x=.data[[var1]], y=.data[[var2]], fill=.data[[var1]]))
  } else {
    df %>% ggplot(aes(x=.data[[var1]], y=.data[[var2]]), fill=opts$area_fill)
  }

  p <- if (opts2$add_jitter) {
    p + geom_boxplot(notch=opts2$notch,
                     width=opts2$width,
                     linetype=opts$area_linetype,
                     size=opts$area_linesize,
                     color=opts$area_linecolor,
                     alpha=opts$area_alpha,
                     outlier.shape=NA) +
      geom_jitter(shape=opts$char_shape,
                  size=opts$char_size,
                  color=opts$char_color,
                  fill=opts$char_fill,
                  alpha=opts$char_alpha,
                  width=opts2$jitter_width * opts2$width)
  } else {
    p + geom_boxplot(notch=opts2$notch,
                     width=opts2$width,
                     linetype=opts$area_linetype,
                     size=opts$area_linesize,
                     color=opts$area_linecolor,
                     alpha=opts$area_alpha,
                     outlier.shape=opts$char_shape,
                     outlier.size=opts$char_size,
                     outlier.color=opts$char_color,
                     outlier.fill=opts$char_fill,
                     outlier.alpha=opts$char_alpha)
  }

  if (opts2$box_fill & global$palette_d!="none") {
    n_colors <- n_distinct(df[[var1]])
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

  p <- if(opts2$horizontal) {
    p + coord_flip() +
      labs(x=get_label(opts$ylab, var1),
           y=get_label(opts$xlab, var2))
  } else {
    p + labs(x=get_label(opts$xlab, var1),
             y=get_label(opts$ylab, var2))
  }

  p <- p + labs(title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  p <- add_theme(p, global$theme, global$font_size, global$font_family)

  if (!opts$show_legend) {
    p <- p + theme(legend.position = "none")
  }

  p
}

# 小提琴图
vis2_violin <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts2 <- vis_params$opts2

  p <- if (opts2$box_fill) {
    df %>% ggplot(aes(x=.data[[var1]], y=.data[[var2]], fill=.data[[var1]])) +
      geom_violin(width=opts2$width,
                  linetype=opts$area_linetype,
                  size=opts$area_linesize,
                  color=opts$area_linecolor,
                  alpha=opts$area_alpha,
                  show.legend=opts$show_legend)
  } else {
    df %>% ggplot(aes(x=.data[[var1]], y=.data[[var2]])) +
      geom_violin(width=opts2$width,
                  linetype=opts$area_linetype,
                  size=opts$area_linesize,
                  color=opts$area_linecolor,
                  fill=opts$area_fill,
                  alpha=opts$area_alpha)
  }

  add_theme(p, global$theme, global$font_size, global$font_family)

  # 调色板(填充色)
  if (opts2$box_fill & global$palette_d!="none") {
    n_colors <- n_distinct(df[[var1]])
    p <- add_fill_discrete(p,
                           global$palette_family_d,
                           global$palette_d,
                           n_colors,
                           reverse=global$palette_rev_d)
  }

  # 添加箱线图
  if (opts2$add_boxplot) {
    p <- p + geom_boxplot(width=.25*opts2$width,
                          linetype=opts$area2_linetype,
                          size=opts$area2_linesize,
                          color=opts$area2_linecolor,
                          alpha=opts$area2_alpha,
                          outlier.shape=NA)
  }

  # 添加抖动散点
  if (opts2$add_jitter) {
    p <- p  +
      geom_jitter(shape=opts$char_shape,
                  size=opts$char_size,
                  color=opts$char_color,
                  fill=opts$char_fill,
                  alpha=opts$char_alpha,
                  width=opts2$jitter_width * opts2$width)
  }

  if (opts$show_legend & !is.null(opts$fill_title)) {
    if (!is_empty(opts$fill_title)) {
      p <- p + labs(fill=opts$fill_title)
    }
  }

  p <- if(opts2$horizontal) {
    p + coord_flip() +
      labs(x=get_label(opts$ylab, var1),
           y=get_label(opts$xlab, var2))
  } else {
    p + labs(x=get_label(opts$xlab, var1),
             y=get_label(opts$ylab, var2))
  }

  p <- p + labs(title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 条形图
vis2_barplot <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts
  opts2 <- vis_params$opts2

  p <- df %>%
    ggplot(aes(x=.data[[var1]], fill=.data[[var2]])) +
    geom_bar(position=opts2$position,
             width=opts2$width,
             linetype=opts$area_linetype,
             size=opts$area_linesize,
             color=opts$area_linecolor,
             alpha=opts$area_alpha,
             show.legend=opts$show_legend)

  # 填充色调色板
  if (global$palette_d!="none") {
    n_colors <- n_distinct(df[[var2]])
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

  p <- if(opts2$horizontal) {
    p + coord_flip() +
      labs(x=get_label(opts$ylab, var1),
           y=get_label(opts$xlab, var2))
  } else {
    p + labs(x=get_label(opts$xlab, var1),
             y=get_label(opts$ylab, var2))
  }

  p <- p + labs(title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 气泡图
vis2_bubble <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts

  p <- df %>%
    count(.data[[var1]], .data[[var2]]) %>%
    ggplot(aes(x=.data[[var1]], y=.data[[var2]], size=n)) +
    geom_point(show.legend=opts$show_legend)

  p <- p +
    labs(x=get_label(opts$xlab, var1),
         y=get_label(opts$ylab, var2),
         title=get_label(opts$title, NULL),
         subtitle=get_label(opts$subtitle, NULL))

  if (opts$show_legend & !is.null(opts$char_size_title)) {
    if (!is_empty(opts$char_size_title)) {
      p <- p + labs(size=opts$char_size_title)
    }
  }

  add_theme(p, global$theme, global$font_size, global$font_family)
}

# 马赛克图
vis2_mosaic <- function(df, var1, var2, vis_params) {

  global <- vis_params$global
  opts <- vis_params$opts

  p <- df %>%
    mutate(x__=.data[[var1]], y__=.data[[var2]]) %>%
    ggplot() +
    geom_mosaic(aes(x=product(y__, x__), fill=y__), show.legend=opts$show_legend)

  if (global$palette_d!="none") {
    n_colors <- n_distinct(df[[var2]])
    p <- add_fill_discrete(p,
                           global$palette_family_d,
                           global$palette_d,
                           n_colors,
                           reverse=global$palette_rev_d)
  }

  fill_title <- var2
  if (opts$show_legend & !is.null(opts$fill_title)) {
    if (!is_empty(opts$fill_title)) {
      fill_title <- opts$fill_title
    }
  }

  p <- p + labs(fill=fill_title)

  p <- p + labs(x=get_label2(opts$xlab, var1),
                y=get_label2(opts$ylab, var2),
                title=get_label(opts$title, NULL),
                subtitle=get_label(opts$subtitle, NULL))

  add_theme(p, global$theme, global$font_size, global$font_family)
}
