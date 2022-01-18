library(shiny)
library(tidyverse)
library(rlang)
library(datamods)

# 中文本地化
options("datamods.i18n" = "datamods_i18n_cn.csv")

is_empty <- function (x, empty = "\\s*")
{
  length(x) == 0 || (length(x) == 1 && is.na(x)) ||
    (length(x) == 1 && grepl(paste0("^", empty, "$"), x))
}

# 加载内置数据集
m_datasets <- readRDS(file="../../../data/m_datasets.rds")

# 内置数据集名称
built_in_dataset_names <- names(m_datasets)

# 数据变换的类型
m_transforms <- c("对数变换"="log_transform", "标准化"="std_transform")

