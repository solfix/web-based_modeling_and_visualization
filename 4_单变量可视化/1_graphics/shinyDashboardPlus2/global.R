library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(rlang)
library(showtext)
library(Cairo)

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

classic_plot <- function(x, var,
                         vis1_var_type, vis1_num_plot_type, vis1_factor_plot_type,
                         vis1_params) {

  if (vis1_var_type=="numeric") {
    switch (vis1_num_plot_type,
            scatter_t = plot(x,
                             pch = vis1_params$pch,
                             col = vis1_params$col,
                             bg  = vis1_params$bg,
                             cex = vis1_params$cex,
                             main= vis1_params$main,
                             sub = vis1_params$sub,
                             xlab= ifelse(is.null(vis1_params$xlab), "索引", vis1_params$xlab),
                             ylab= ifelse(is.null(vis1_params$ylab), var, vis1_params$ylab)),
            histogram_t = hist(x,
                               breaks = ifelse(is.null(vis1_params$breaks), "Sturges", vis1_params$breaks),
                               freq = vis1_params$freq,
                               col = vis1_params$bg,
                               main= vis1_params$main,
                               sub = vis1_params$sub,
                               xlab= ifelse(is.null(vis1_params$xlab), var, vis1_params$xlab),
                               ylab= ifelse(is.null(vis1_params$ylab), "频数", vis1_params$ylab)),
            boxplot_t = boxplot(x,
                                col = vis1_params$bg,
                                notch = vis1_params$notch,
                                horizontal=vis1_params$horizontal,
                                main= vis1_params$main,
                                sub = vis1_params$sub,
                                xlab=if(is.null(vis1_params$xlab) & (vis1_params$horizontal==T)) var else vis1_params$xlab,
                                ylab=if(is.null(vis1_params$ylab) & (vis1_params$horizontal==F)) var else vis1_params$ylab),
    )
  } else {
    switch (vis1_factor_plot_type,
            barplot_t = barplot(table(x),
                                horiz = vis1_params$horiz,
                                col = vis1_params$bg,
                                main= vis1_params$main,
                                sub = vis1_params$sub,
                                xlab=if(is.null(vis1_params$xlab) & (vis1_params$horiz==F)) var else vis1_params$xlab,
                                ylab=if(is.null(vis1_params$ylab) & (vis1_params$horiz==T)) var else vis1_params$ylab),
            pie_t = pie(table(x),
                        radius = vis1_params$radius,
                        init.angle = vis1_params$angle,
                        clockwise = vis1_params$clockwise,
                        main= vis1_params$main,
                        sub = vis1_params$sub),
    )
  }
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
