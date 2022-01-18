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
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    shape = 1,
    size = 2,
    color = "black",
    fill = "lightgray",
    alpha = 1,
    bins = NULL,
    freq = F,
    density = F,
    width = .7,
    horizontal = T,
    horiz = F
  )

  # 保存可视化参数
  observeEvent(input$vis1_params_ok, {

    vis1_params$title <- if(!is.null(input$vis1_title) & !is_empty(input$vis1_title)) input$vis1_title else NULL
    vis1_params$subtitle <- if(!is.null(input$vis1_subtitle) & !is_empty(input$vis1_subtitle)) input$vis1_subtitle else NULL

    if(input$vis1_var_type != 'factor' | input$vis1_factor_plot_type != 'pie_t') {
      vis1_params$xlab <- if(!is.null(input$vis1_xlab) & !is_empty(input$vis1_xlab)) input$vis1_xlab else NULL
      vis1_params$ylab <- if(!is.null(input$vis1_ylab) & !is_empty(input$vis1_ylab)) input$vis1_ylab else NULL
      vis1_params$color <- input$vis1_color
      vis1_params$fill <- input$vis1_fill
    }

    vis1_params$alpha <- input$vis1_alpha

    if (input$vis1_var_type == "numeric") {
      if (input$vis1_num_plot_type == 'scatter_t') {
        vis1_params$shape <- input$vis1_shape
        vis1_params$size <- input$vis1_size
      } else if (input$vis1_num_plot_type == 'histogram_t') {
        vis1_params$bins <- input$vis1_bins
        vis1_params$freq <- input$vis1_freq
        vis1_params$density <- input$vis1_density
      } else {
        vis1_params$horizontal <- input$vis1_horizontal
      }
    } else {
      if (input$vis1_factor_plot_type == 'barplot_t') {
        vis1_params$width <- input$vis1_width
        vis1_params$horiz <- input$vis1_horiz
      }
    }
  })

  # 清除可视化参数
  observeEvent(input$vis1_params_clear, {
    vis1_params$title = NULL
    vis1_params$subtitle = NULL
    vis1_params$xlab = NULL
    vis1_params$ylab = NULL
    vis1_params$color = "black"
    vis1_params$fill = "lightgray"
    vis1_params$alpha = 1
    vis1_params$shape = 1
    vis1_params$size = 2
    vis1_params$bins = NULL
    vis1_params$freq = F
    vis1_params$density = F
    vis1_params$width = .7
    vis1_params$horizontal = T
    vis1_params$horiz = F
  })

  # 无主题的ggplot2图（无饼图）
  vis1_fig_without_theme <- reactive({
    req(input$vis1_var, input$vis1_var_type)

    ggplot2_plot(dataset(), input$vis1_var,
                 input$vis1_var_type,
                 input$vis1_num_plot_type,
                 input$vis1_factor_plot_type,
                 vis1_params)

  })

  # 有主题的ggplot2图（无饼图）
  vis1_fig_with_theme <- reactive({
    p <- vis1_fig_without_theme()
    p <- switch(input$vis1_theme,
                theme_gray = p + theme_gray(base_size=input$vis1_fontsize),
                theme_bw = p + theme_bw(base_size=input$vis1_fontsize),
                theme_linedraw = p + theme_linedraw(base_size=input$vis1_fontsize),
                theme_light = p + theme_light(base_size=input$vis1_fontsize),
                theme_dark = p + theme_dark(base_size=input$vis1_fontsize),
                theme_minimal = p + theme_minimal(base_size=input$vis1_fontsize),
                theme_classic = p + theme_classic(base_size=input$vis1_fontsize),
                theme_void = p + theme_void(base_size=input$vis1_fontsize),
                theme_base = p + theme_base(base_size=input$vis1_fontsize),
                theme_calc = p + theme_calc(base_size=input$vis1_fontsize),
                theme_clean = p + theme_clean(base_size=input$vis1_fontsize),
                theme_economist = p + theme_economist(base_size=input$vis1_fontsize),
                theme_economist_white = p + theme_economist_white(base_size=input$vis1_fontsize),
                theme_excel = p + theme_excel(base_size=input$vis1_fontsize),
                theme_excel_new = p + theme_excel_new(base_size=input$vis1_fontsize),
                theme_few = p + theme_few(base_size=input$vis1_fontsize),
                theme_fivethirtyeight = p + theme_fivethirtyeight(base_size=input$vis1_fontsize),
                theme_foundation = p + theme_foundation(base_size=input$vis1_fontsize),
                theme_gdocs = p + theme_gdocs(base_size=input$vis1_fontsize),
                theme_hc = p + theme_hc(base_size=input$vis1_fontsize),
                theme_igray = p + theme_igray(base_size=input$vis1_fontsize),
                theme_map = p + theme_map(base_size=input$vis1_fontsize),
                theme_pander = p + theme_pander(base_size=input$vis1_fontsize),
                theme_par = p + theme_par(base_size=input$vis1_fontsize),
                theme_solarized = p + theme_solarized(base_size=input$vis1_fontsize),
                theme_solarized_2 = p + theme_solarized_2(base_size=input$vis1_fontsize),
                theme_solid = p + theme_solid(base_size=input$vis1_fontsize),
                theme_stata = p + theme_stata(base_size=input$vis1_fontsize),
                theme_tufte = p + theme_tufte(base_size=input$vis1_fontsize),
                theme_wsj = p + theme_wsj(base_size=input$vis1_fontsize),
    )

    if (input$vis1_var_type=="numeric" & input$vis1_num_plot_type=="boxplot_t") {
      if(vis1_params$horizontal) {
        p <- p + coord_flip() + theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
      } else {
        p <- p + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
      }
    }

    p
  })

  pie_chart <- reactive({
    req(input$vis1_var_type=="factor" & input$vis1_factor_plot_type=="pie_t")
    fig <- dataset() %>%
      rename(x__=input$vis1_var) %>%
      count(x__) %>%
      plot_ly(labels = ~x__, values = ~n) %>%
      add_pie(alpha=.1) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    if (!is.null(vis1_params$title)) {
      fig <- fig %>% layout(title = vis1_params$title)
    }

    fig
  })

  output$vis1_download = downloadHandler(

    filename=function() {
      paste0("Plot.", input$vis1_fig_type)
    },

    content=function(file){
      req(input$vis1_var, input$vis1_var_type)
      ggsave(file, vis1_fig_with_theme(), units="in", width=input$vis1_fig_wd, height=input$vis1_fig_ht)
    }
  )

  #output$message <- renderPrint({ print(input$vis1_fontsize) })

  output$plotly_vis1 <- renderPlotly({

    if (input$vis1_var_type=="factor" & input$vis1_factor_plot_type=="pie_t") {
      pie_chart() %>%
        config(toImageButtonOptions = list(format = "svg"))

    } else {
      ggplotly(vis1_fig_with_theme()) %>%
        config(toImageButtonOptions = list(format = "svg"))
    }
  })
})
