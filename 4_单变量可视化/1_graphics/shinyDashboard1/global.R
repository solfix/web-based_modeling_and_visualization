library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(tidyverse)
library(rlang)

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

# 加载内置数据集
m_datasets <- readRDS(file="../../../data/m_datasets.rds")

# 内置数据集名称
built_in_dataset_names <- names(m_datasets)

# 数据变换的类型
m_transforms <- c("对数变换"="log_transform", "标准化"="std_transform")

# 单变量绘图类型
num_vis1_types <- c("散点图"="scatter_t", "直方图"="histogram_t", "核密度图"="density_t", "箱线图"="boxplot_t")
factor_vis1_types <- c("条形"="barplot_t", "饼图"="pie_t")
