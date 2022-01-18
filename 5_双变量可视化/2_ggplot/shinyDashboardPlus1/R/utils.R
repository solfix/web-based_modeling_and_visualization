# 检查输入是否为空串
is_empty <- function (x, empty = "\\s*")
{
  length(x) == 0 || (length(x) == 1 && is.na(x)) ||
    (length(x) == 1 && grepl(paste0("^", empty, "$"), x))
}

# 如果输入为NULL，则返回缺省字符串；如果输入非空但是空串，返回NULL值；否则返回输入字符串
get_label <- function(input_str, default_str) {
  if (is.null(input_str)) {
    default_str
  } else {
    if (is_empty(input_str)) NULL else input_str
  }
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
