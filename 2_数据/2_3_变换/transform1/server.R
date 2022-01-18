# 定义服务器逻辑
shinyServer(function(input, output, session) {

  # 以反应值形式保存选取的数据
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

  # 数据筛选
  filter_data <- reactive({
    tryCatch({
      df <- if(is_empty(filter_str$data)) selected_data() else
        filter(selected_data(), eval_tidy(parse_expr(filter_str$data)))
      shinyFeedback::feedbackWarning("data_filter", F, "")
      return (df)
    }, error = function(e) {
      shinyFeedback::feedbackWarning("data_filter", T, e$message)
      return (selected_data())
    })
  })

  # 处理后的最终数据
  processed_data <- reactive({
    if(input$show_filter) filter_data() else selected_data()
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
