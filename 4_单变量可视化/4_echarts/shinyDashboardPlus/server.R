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

  # 反应表达式存储的用于选择的变量选项
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

  observeEvent(input$add_dataset, {

    is_empty_name <- is_empty(input$dataset_name)
    shinyFeedback::feedbackWarning("dataset_name", is_empty_name, "数据名不能为空！")
    req(!is_empty(input$dataset_name))

    is_built_in <- (input$dataset_name %in% built_in_dataset_names)
    shinyFeedback::feedbackWarning("dataset_name", is_built_in, "不能与内置数据集同名！")
    req(!is_built_in)

    m_datasets[[input$dataset_name]] <<- processed_data()

    freezeReactiveValue(input, "dataset")
    updateSelectInput(inputId = "dataset", choices = names(m_datasets), selected = input$dataset_name)
    updateSelectInput(inputId = "del_dataset_names", choices = setdiff(names(m_datasets), built_in_dataset_names), selected = NULL)
  })

  observeEvent(input$del_dataset, {

    req(!is_empty(input$del_dataset_names))

    freezeReactiveValue(input, "dataset")
    for (dat_name in input$del_dataset_names) {
      m_datasets[[dat_name]] <<- NULL
    }

    updateSelectInput(inputId = "dataset", choices = names(m_datasets), selected = "SurgicalUnit")
    updateSelectInput(inputId = "del_dataset_names", choices = setdiff(names(m_datasets), built_in_dataset_names), selected = NULL)
  })

  observeEvent(input$import_dataset, {
    req(input$upload)

    is_empty_name <- is_empty(input$upload_dataset_name)
    shinyFeedback::feedbackWarning("upload_dataset_name", is_empty_name, "数据名不能为空！")
    req(!is_empty(input$upload_dataset_name))

    is_built_in <- (input$upload_dataset_name %in% built_in_dataset_names)
    shinyFeedback::feedbackWarning("upload_dataset_name", is_built_in, "不能与内置数据集同名！")
    req(!is_built_in)

    df <- read_data(input$upload$name, input$upload$datapath)

    m_datasets[[input$upload_dataset_name]] <<- df
    freezeReactiveValue(input, "dataset")
    updateSelectInput(inputId = "dataset", choices = names(m_datasets), selected = input$upload_dataset_name)
    updateSelectInput(inputId = "del_dataset_names", choices = setdiff(names(m_datasets), built_in_dataset_names), selected = NULL)

  })

  output$download_dataset <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      vroom::vroom_write(processed_data(), file)
    }
  )

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

  # 将所有变量（数值和因子）的名字保存在反应表达式中，用于单变量可视化
  vis1_vars <- reactive({
    if (input$vis1_var_type=="numeric") {
      names(dataset())[purrr::map_lgl(dataset(), ~is.numeric(.))]
    } else {
      names(dataset())[purrr::map_lgl(dataset(), ~is.factor(.))]
    }
  })

  # 更新单变量可视化的变量选项
  observeEvent(vis1_vars(), {
    freezeReactiveValue(input, "vis1_var")
    updateSelectInput(inputId = "vis1_var", choices = vis1_vars())
  })

  output$echarts_vis1 <- renderEcharts4r({
    df <- dataset() %>%
      mutate(x__=.data[[input$vis1_var]], index=row_number())
    if (input$vis1_var_type=="numeric") {
      p <- switch (input$vis1_num_plot_type,
              scatter_t = df |>
                e_charts(x=index) |>
                e_scatter(serie=x__, name=input$vis1_var),

              histogram_t = df |>
                e_charts() |>
                e_histogram(serie=x__, name=input$vis1_var),

              density_t = df |>
                e_charts() |>
                e_density(serie=x__, name=input$vis1_var),

              boxplot_t = df |>
                mutate(g__=input$vis1_var) %>%
                group_by(g__) %>%
                e_charts() |>
                e_boxplot(serie=x__),
      )
    } else {
      df <- count(df, x__)
      p <- switch (input$vis1_factor_plot_type,
              barplot_t = df |>
                e_charts(x=x__) |>
                e_bar(serie=n, name=input$vis1_var),
              pie_t = df |>
                e_charts(x=x__) |>
                e_pie(serie=n, name=input$vis1_var),
      )
    }
    p |> e_theme(input$vis1_theme)
  })
})
