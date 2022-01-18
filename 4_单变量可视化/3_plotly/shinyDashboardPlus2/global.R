library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(rlang)
library(showtext)
library(Cairo)
library(ggthemes)
library(plotly)

showtext_auto(enable=T)

is_empty <- function (x, empty = "\\s*")
{
  length(x) == 0 || (length(x) == 1 && is.na(x)) ||
    (length(x) == 1 && grepl(paste0("^", empty, "$"), x))
}

# 从文件中读取数据
read_data <- function(datafile_name, datafile_path) {
  ext <- tools::file_ext(datafile_name)
  switch(ext,
         csv = vroom::vroom(datafile_path, delim = ","),
         tsv = vroom::vroom(datafile_path, delim = "\t"),
         xls = readxl::read_xls(datafile_path),
         xlsx = readxl::read_xlsx(datafile_path),
         sav = tibble::as_tibble(foreign::read.spss(datafile_path)),
         json = jsonlite::read_json(datafile_path, simplifyVector = TRUE),
         validate("Invalid file; 请上传 .csv、.tsv、xls、xlsx, sav 格式的文件")
  )
}

ggplot2_plot <- function(df, var,
                         vis1_var_type, vis1_num_plot_type, vis1_factor_plot_type,
                         vis1_params) {

  p <- df %>%
    dplyr::select(.data[[var]]) %>%
    mutate(index=row_number()) %>%
    ggplot()

  p <- if (vis1_var_type=="numeric") {
    switch (vis1_num_plot_type,
            scatter_t = p +
              geom_point(aes(x=index, y=.data[[var]]),
                         shape=vis1_params$shape, size=vis1_params$size,
                         color=vis1_params$color, fill=vis1_params$fill,
                         alpha=vis1_params$alpha) +
              labs(x=ifelse(is.null(vis1_params$xlab), "索引", vis1_params$xlab),
                   y=ifelse(is.null(vis1_params$ylab), var, vis1_params$ylab),
                   title=vis1_params$title, subtitle=vis1_params$subtitle),

            histogram_t = p +
              (if(vis1_params$freq) geom_histogram(aes(x=.data[[var]], y=..density..), bins=vis1_params$bins, color=vis1_params$color, fill=vis1_params$fill, alpha=vis1_params$alpha)
               else geom_histogram(aes(x=.data[[var]], y=..count..  ), bins=vis1_params$bins, color=vis1_params$color, fill=vis1_params$fill, alpha=vis1_params$alpha)) +
              labs(x=ifelse(is.null(vis1_params$xlab), var, vis1_params$xlab),
                   y=ifelse(is.null(vis1_params$ylab), "频数", vis1_params$ylab),
                   title=vis1_params$title, subtitle=vis1_params$subtitle),

            boxplot_t = p +
              geom_boxplot(aes(x=factor(0), y=.data[[var]]),
                           color=vis1_params$color, fill=vis1_params$fill, alpha=vis1_params$alpha) +
              labs(
                x=if(is.null(vis1_params$xlab) & vis1_params$horizontal==F) var else vis1_params$xlab,
                y=if(is.null(vis1_params$ylab) & vis1_params$horizontal==T) var else vis1_params$ylab,
                title=vis1_params$title,
                subtitle=vis1_params$subtitle),
    )

  } else {
    switch (vis1_factor_plot_type,
            barplot_t = p + geom_bar(aes(x=.data[[var]]),
                                     width=vis1_params$width,
                                     color=vis1_params$color, fill=vis1_params$fill, alpha=vis1_params$alpha) +
              labs(
                x=if(is.null(vis1_params$xlab) & vis1_params$horiz==T) var else vis1_params$xlab,
                y=if(is.null(vis1_params$ylab) & vis1_params$horiz==F) var else vis1_params$ylab,
                title=vis1_params$title,
                subtitle=vis1_params$subtitle),
    )
  }

  if (vis1_var_type=="numeric" & vis1_num_plot_type=="histogram_t" & vis1_params$freq & vis1_params$density) {
    p <- p + geom_density(aes(x=.data[[var]]))
  }

  if (vis1_var_type=='factor' & vis1_factor_plot_type=="barplot_t" & vis1_params$horiz) {
    p <- p + coord_flip()
  }

  p
}

# 加载内置数据集
m_datasets <- readRDS(file="../../../data/m_datasets.rds")

# 内置数据集名称
built_in_dataset_names <- names(m_datasets)

# 数据变换的类型
m_transforms <- c("对数变换"="log_transform", "标准化"="std_transform")

# 单变量绘图类型
num_vis1_types <- c("散点图"="scatter_t", "直方图"="histogram_t", "箱线图"="boxplot_t")
factor_vis1_types <- c("条形"="barplot_t", "饼图"="pie_t")

## ggplot2和ggthemes中的主题
ggplot2_themes <- c(
  "theme_gray",
  "theme_bw",
  "theme_linedraw",
  "theme_light",
  "theme_dark",
  "theme_minimal",
  "theme_classic",
  "theme_void",
  "theme_base",
  "theme_calc",
  "theme_clean",
  "theme_economist",
  "theme_economist_white",
  "theme_excel",
  "theme_excel_new",
  "theme_few",
  "theme_fivethirtyeight",
  "theme_foundation",
  "theme_gdocs",
  "theme_hc",
  "theme_igray",
  "theme_map",
  "theme_pander",
  "theme_par",
  "theme_solarized",
  "theme_solarized_2",
  "theme_solid",
  "theme_stata",
  "theme_tufte",
  "theme_wsj"
)
