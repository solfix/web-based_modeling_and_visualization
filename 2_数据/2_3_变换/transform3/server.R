# 定义服务器逻辑
shinyServer(function(input, output, session) {

  # 原始数据
  dataset <- reactive(m_datasets[[input$dataset]])

  # 数据集发生切换后，更新变量（列）的选择项
  observeEvent(dataset(), {

    # “冻结”输入：确保任何使用该输入的反应值或输出都不会被更新。
    freezeReactiveValue(input, "columns")

    # 以新数据集的变量名更新变量（列）的选择项
    updateSelectInput(inputId = "columns", choices = names(dataset()), selected = names(dataset()))
  })

  # 变量选择后的数据
  selected_data <- reactive({
    if (input$show_select) dataset()[, input$columns] else dataset()
  })

  # 合并数据 = 变量选择后的数据 + 变换数据
  merge_data <- reactive({
    df <- selected_data()
    if (input$show_transform & !is.null(transformed_data())) {
      df <- cbind(df, transformed_data()) }
    return (df)
  })

  # 字符串筛选条件
  filter_str <- reactiveValues(data=NULL)

  # 保存筛选条件
  observeEvent(input$data_filter_ok, {
    filter_str$data <- input$data_filter
  })

  # 清除筛选条件
  observeEvent(input$data_filter_clear, {
    updateTextAreaInput(inputId = "data_filter", value = "")
    filter_str$data <- NULL
  })

  # 对合并数据进行筛选得到的数据
  filter_data <- reactive({
    tryCatch({
      df <- merge_data()
      if (!is_empty(filter_str$data)) {
        df <- filter(df, eval_tidy(parse_expr(filter_str$data)))
      }
      shinyFeedback::feedbackWarning("data_filter", F, "")
      return (df)
    }, error = function(e) {
      shinyFeedback::feedbackWarning("data_filter", T, e$message)
      return (merge_data())
    })
  })

  trans_vars <- reactive({
    df <- dataset() %>% dplyr::select_if(is.numeric)
    vars <- names(df)
    if (input$transform=="log_transform") {
      vars <- names(df)[purrr::map_lgl(df, ~min(.)>0)]
    }
    vars
  })

  observeEvent(trans_vars(), {
    freezeReactiveValue(input, "transform_vars")
    updateSelectInput(inputId="transform_vars", choices=trans_vars(), selected=NULL)
  })

  # 数据变换
  transformed_data <- reactive({
    df <- NULL
    if (input$transform=="log_transform" & !is_empty(input$transform_vars)) {
      df <- dataset()[,input$transform_vars] %>% map(log)
      names(df) <- paste0("LOG ", names(df))
    } else if (input$transform=="std_transform" & !is_empty(input$transform_vars)) {
      df <- dataset()[,input$transform_vars] %>% map(scale)
      names(df) <- paste0("STD ", names(df))
    }
    return (df)
  })

  # 处理后的最终数据
  processed_data <- reactive({
    if(input$show_filter) filter_data() else merge_data()
  })

  # DT::renderDataTable分页显示数据集中已选取的变量，缺省一页显示5行
  output$table <- DT::renderDataTable({
    processed_data()
  }, options = list(
    # 表格中显示的中文标签
    language = list(
      info = "显示第 _START_ 至 _END_ 项结果，共 _TOTAL_ 项",
      search = "搜索：",
      paginate = list(previous = "上一页", `next` = "下一页"),
      lengthMenu = "显示 _MENU_ 项结果"
    ),
    pageLength = 5,
    scrollX = TRUE # 水平方向滑动条
  ))
})
