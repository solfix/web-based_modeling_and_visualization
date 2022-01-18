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
  vis_params <- reactiveValues(
    global = init_vis_global(),
    opts = init_vis_options(),
    opts1 = init_vis1_options()
  )

  observeEvent(input$vis_theme, {
    vis_params$global$theme <- input$vis_theme
  })

  observeEvent(input$vis_font_size, {
    vis_params$global$font_size = input$vis_font_size
  })

  observeEvent(input$vis_palette_family_d, {
    vis_palettes <- switch (input$vis_palette_family_d,
                            base = c("none", base_palettes),
                            RColorBrewer = brewer_palettes,
                            viridis = viridis_palettes,
                            wesanderson = wesanderson_palettes,
                            ggsci = ggsci_palettes_discrete
    )
    vis_params$global$palette_family_d = input$vis_palette_family_d
    freezeReactiveValue(input, "vis_palette_d")
    updateSelectInput(inputId="vis_palette_d", choices=vis_palettes)
  })

  observeEvent(input$vis_palette_d, {
    vis_params$global$palette_d <- input$vis_palette_d
  })

  observeEvent(input$vis_palette_rev_d, {
    vis_params$global$palette_rev_d <- input$vis_palette_rev_d
  })

  observeEvent(input$vis_palette_family_c, {
    vis_palettes <- switch (input$vis_palette_family_c,
                            base = c("none", base_palettes),
                            viridis = viridis_palettes,
                            wesanderson = wesanderson_palettes,
                            ggsci = ggsci_palettes_continuous
    )
    vis_params$global$palette_family_c = input$vis_palette_family_c
    freezeReactiveValue(input, "vis_palette_c")
    updateSelectInput(inputId="vis_palette_c", choices=vis_palettes)
  })

  observeEvent(input$vis_palette_c, {
    vis_params$global$palette_c <- input$vis_palette_c
  })

  observeEvent(input$vis_palette_rev_c, {
    vis_params$global$palette_rev_c <- input$vis_palette_rev_c
  })

  # 保存可视化参数
  observeEvent(input$vis_params_ok, {

    vis_params$opts$title <- input$vis_title
    vis_params$opts$subtitle <- input$vis_subtitle
    vis_params$opts$xlab <- input$vis_xlab
    vis_params$opts$ylab <- input$vis_ylab

    vis_params$opts$char_shape <- input$vis_char_shape
    vis_params$opts$char_size <- input$vis_char_size
    vis_params$opts$char_color <- input$vis_char_color
    vis_params$opts$char_fill <- input$vis_char_fill
    vis_params$opts$char_alpha <- input$vis_char_alpha

    vis_params$opts$line_type <- input$vis_line_type
    vis_params$opts$line_size <- input$vis_line_size
    vis_params$opts$line_color <- input$vis_line_color
    vis_params$opts$line_alpha <- input$vis_line_alpha

    vis_params$opts$area_linetype <- input$vis_area_linetype
    vis_params$opts$area_linesize <- input$vis_area_linesize
    vis_params$opts$area_linecolor <- input$vis_area_linecolor
    vis_params$opts$area_fill <- input$vis_area_fill
    vis_params$opts$area_alpha <- input$vis_area_alpha

    vis_params$opts$char2_shape <- input$vis_char2_shape
    vis_params$opts$char2_size <- input$vis_char2_size
    vis_params$opts$char2_color <- input$vis_char2_color
    vis_params$opts$char2_fill <- input$vis_char2_fill
    vis_params$opts$char2_alpha <- input$vis_char2_alpha

    vis_params$opts$line2_type <- input$vis_line2_type
    vis_params$opts$line2_size <- input$vis_line2_size
    vis_params$opts$line2_color <- input$vis_line2_color
    vis_params$opts$line2_alpha <- input$vis_line2_alpha

    vis_params$opts$area2_linetype <- input$vis_area2_linetype
    vis_params$opts$area2_linesize <- input$vis_area2_linesize
    vis_params$opts$area2_linecolor <- input$vis_area2_linecolor
    vis_params$opts$area2_fill <- input$vis_area2_fill
    vis_params$opts$area2_alpha <- input$vis_area2_alpha

    if (input$vis1_var_type == "numeric") {
      if (input$vis1_numeric_plot_type == 'dotplot_t') {
        vis_params$opts1$binwidth_r <- input$vis1_binwidth_r
        vis_params$opts1$dotsize <- input$vis1_dotsize
        vis_params$opts1$stackratio <- input$vis1_stackratio
      } else if (input$vis1_numeric_plot_type == 'histogram_t') {
        vis_params$opts1$bins <- input$vis1_bins
        vis_params$opts1$freq <- input$vis1_freq
        vis_params$opts1$add_density <- input$vis1_add_density
        vis_params$opts1$add_rug <- input$vis1_add_rug
      } else if (input$vis1_numeric_plot_type == 'density_t') {
        vis_params$opts1$add_rug <- input$vis1_add_rug
      } else if (input$vis1_numeric_plot_type == 'boxplot_t') {
        vis_params$opts1$width <- input$vis1_width
        vis_params$opts1$notch <- input$vis1_notch
        vis_params$opts1$horizontal <- input$vis1_horizontal
        vis_params$opts1$add_jitter <- input$vis1_add_jitter
        vis_params$opts1$jitter_width <- input$vis1_jitter_width
      } else {
        vis_params$opts1$width <- input$vis1_width
        vis_params$opts1$horizontal <- input$vis1_horizontal
        vis_params$opts1$add_jitter <- input$vis1_add_jitter
        vis_params$opts1$jitter_width <- input$vis1_jitter_width
        vis_params$opts1$add_boxplot <- input$vis1_add_boxplot
      }
    } else {
      if (input$vis1_factor_plot_type == 'barplot_t') {
        vis_params$opts1$width <- input$vis1_width
        vis_params$opts1$horizontal <- input$vis1_horizontal
      } else {
        vis_params$opts1$start <- input$vis1_start
        vis_params$opts1$clockwise <- input$vis1_clockwise
        #vis_params$opts1$add_theme <- input$vis1_add_theme
      }
    }
  })

  # 重置可视化参数
  observeEvent(input$vis_params_clear, {
    vis_params$opts = init_vis_options()
    vis_params$opts1 = init_vis1_options()
  })

  # 单变量可视化图像保存为反应表达式
  vis1 <- reactive({
    req(input$vis1_var, input$vis1_var_type)

    plot_type <- ifelse(input$vis1_var_type=="numeric",
                        input$vis1_numeric_plot_type,
                        input$vis1_factor_plot_type)

    vis1_plot(dataset(), input$vis1_var, plot_type, vis_params)
  })

  # 显示单变量可视化图像
  output$ggplot_vis1 <- renderPlot({
    vis1()
  })

  output$message <- renderPrint({
    print(input$vis1_numeric_plot_type)
    print(vis_params$opts1$binwidth_r)
    print(vis_params$opts1$dotsize)
    print(vis_params$opts1$stackratio)
  })

  vis2 <- reactive({
    mtcars %>%
      ggplot(aes(x=wt, y=mpg)) +
      geom_point()
  })

  output$ggplot_vis2 <- renderPlot({
    vis2()
  })

  # 保存图像
  output$vis_download = downloadHandler(

    filename=function() {
      paste0("Plot.", input$vis_fig_type)
    },

    content=function(file){

      fig <- switch (input$vis_tab,
                     Vis1 = vis1(),
                     Vis2 = vis2(),
      )

      ggsave(file, plot=fig, units="in", width=input$vis_fig_wd, height=input$vis_fig_ht)
    }
  )
})
