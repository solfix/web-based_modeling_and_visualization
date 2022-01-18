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

  # 单变量可视化参数
  vis1_params <- reactiveValues(
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    pch = 1,
    col = "black",
    bg = "lightgray",
    cex = 1,
    breaks = NULL,
    freq = F,
    density = F,
    notch = F,
    horizontal = T,
    horiz = F,
    radius = .8,
    angle = 0,
    clockwise = F
  )

  # 保存可视化参数
  observeEvent(input$vis1_params_ok, {

    vis1_params$main <- input$vis1_main
    vis1_params$sub  <- input$vis1_sub

    if(input$vis1_var_type != 'factor' | input$vis1_factor_plot_type != 'pie_t') {
      vis1_params$xlab <- input$vis1_xlab
      vis1_params$ylab <- input$vis1_ylab
      vis1_params$bg   <- input$vis1_bg
    }

    if (input$vis1_var_type == "numeric") {
      if (input$vis1_num_plot_type == 'scatter_t') {
        vis1_params$pch <- input$vis1_pch
        vis1_params$col <- input$vis1_col
        vis1_params$cex <- input$vis1_cex
      } else if (input$vis1_num_plot_type == 'histogram_t') {
        vis1_params$breaks <- input$vis1_breaks
        vis1_params$freq   <- input$vis1_freq
      } else {
        vis1_params$notch <- input$vis1_notch
        vis1_params$horizontal <- input$vis1_horizontal
      }
    } else {
      if (input$vis1_num_plot_type == 'barplot_t') {
        vis1_params$horiz <- input$vis1_horiz
      } else {
        vis1_params$radius <- input$vis1_radius
        vis1_params$angle <- input$vis1_angle
        vis1_params$clockwise <- input$vis1_clockwise
      }
    }
  })

  # 清除可视化参数
  observeEvent(input$vis1_params_clear, {
    vis1_params$main = NULL
    vis1_params$sub = NULL
    vis1_params$xlab = NULL
    vis1_params$ylab = NULL
    vis1_params$pch = 1
    vis1_params$col = "black"
    vis1_params$bg = "lightgray"
    vis1_params$cex = 1
    vis1_params$breaks = NULL
    vis1_params$freq = F
    #vis1_params$density = F
    vis1_params$notch = F
    vis1_params$horizontal = T
    vis1_params$horiz = F
    vis1_params$radius = .8
    vis1_params$angle = 0
    vis1_params$clockwise = F
  })

  vis1_fig <- reactive({
    req(input$vis1_var, input$vis1_var_type)

    var <- input$vis1_var
    x <- dataset()[[var]]

    classic_plot(x, var,
                 input$vis1_var_type,
                 input$vis1_num_plot_type,
                 input$vis1_factor_plot_type,
                 vis1_params)

  })

  output$vis1_download = downloadHandler(

    filename=function() {
      paste0("Plot.", input$vis1_fig_type)
    },

    content=function(file){

      req(input$vis1_var, input$vis1_var_type)
      var <- input$vis1_var
      x <- dataset()[[var]]

      if (input$vis1_fig_type=='png') {
        png(file, units="in", width=input$vis1_fig_wd, height=input$vis1_fig_ht, res=72)
      } else if (input$vis1_fig_type=='pdf') {
        pdf(file, width=input$vis1_fig_wd, height=input$vis1_fig_ht)
      } else {
        svg = svg(file, width=input$vis1_fig_wd, height=input$vis1_fig_ht)
      }

      classic_plot(x, var,
                   input$vis1_var_type,
                   input$vis1_num_plot_type,
                   input$vis1_factor_plot_type,
                   vis1_params)
      dev.off()

    }
  )

  #output$message <- renderPrint({ print(vis1_params$clockwise) })

  output$classic_vis1 <- renderPlot({
    vis1_fig()
  })
})
