ggplot2_vis2_plot <- function(df, var1, var2, flag, plot_type, vis1_params) {

  p <- ggplot(df, aes(x=.data[[var1]], y=.data[[var2]]))

  switch(flag,
         TT = switch(plot_type,
                     scatter_t = p <- p +
                       geom_point(shape=vis1_params$char_shape, size=vis1_params$char_size, color=vis1_params$char_color,
                                  fill=vis1_params$fill, alpha=vis1_params$alpha) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), var2, vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),

                     # 方法来源：https://wahani.github.io/2015/12/smoothScatter-with-ggplot2/
                     smooth_scatter_t = p <- p +
                       stat_density2d(aes(fill = ..density..^0.25), geom = "tile", contour = FALSE, n = 200, show.legend=F) +
                      # ggsci::scale_fill_aaas() +
                       scale_fill_continuous(low = "white", high = "dodgerblue4") +
                       geom_point(shape=vis1_params$char_shape, size=vis1_params$char_size, color=vis1_params$char_color,
                                  fill=vis1_params$fill, alpha=vis1_params$alpha) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), var2, vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),

                     density2d_t = p <- p +
                       geom_point(shape=vis1_params$char_shape, size=vis1_params$char_size, color=vis1_params$char_color,
                                  fill=vis1_params$fill, alpha=vis1_params$alpha) +
                       geom_density2d(linetype=vis1_params$line_type, size=vis1_params$line_size, color=vis1_params$line_color) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), var2, vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),
         ),
         TF = switch(plot_type,
                     strip_t = p <- p +
                       geom_point(shape=vis1_params$char_shape, size=vis1_params$char_size, color=vis1_params$char_color,
                                  fill=vis1_params$fill, alpha=vis1_params$alpha) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), var2, vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),

                     cleveland_dot_t = p <- df %>%
                       mutate(i__=row_number()) %>%
                       ggpubr::ggdotchart(x="i__", y=var1, group=var2, color=var2,
                                          shape=vis1_params$char_shape, size=vis1_params$char_size,
                                          rotate=TRUE, sorting="descending"),

                     group_density_t = p <- ggplot(df, aes(x=.data[[var1]], fill=.data[[var2]])) +
                       geom_density(position=position_dodge("dodge", width=.3), size=vis1_params$line_size, alpha=vis1_params$alpha) +
                       #ggsci::scale_fill_aaas() +
                       ggsci::scale_fill_jama() +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), "密度", vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),

                     marginal_density_t = p <- ggplot(df, aes(x=.data[[var1]], stat(count), fill=.data[[var2]])) +
                       geom_density(position="stack", size=vis1_params$line_size, alpha=vis1_params$alpha) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), "密度", vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),

                     conditional_density_t = p <- ggplot(df, aes(x=.data[[var1]], stat(count), fill=.data[[var2]])) +
                       geom_density(position="fill", size=vis1_params$line_size, alpha=vis1_params$alpha) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), "密度", vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),
         ),
         FT = switch(plot_type,
                     boxplot_t = p <- p +
                       geom_boxplot(aes(fill=.data[[var1]]), size=vis1_params$line_size, alpha=vis1_params$alpha) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), var2, vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),
         ),
         FF = switch(plot_type,
                     barplot_t = p <- ggplot(df, aes(x=.data[[var1]], fill=.data[[var2]])) +
                       geom_bar(position="dodge", linetype=vis1_params$line_type, size=vis1_params$line_size, color=vis1_params$line_color, alpha=vis1_params$alpha) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), "频数", vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),

                     dot_t = p <- df %>%
                       count(.data[[var1]], .data[[var2]]) %>%
                       ggplot(aes(x=.data[[var1]], y=.data[[var2]], size=n)) +
                       geom_point() +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), var2, vis1_params$ylab),
                            title=vis1_params$title, subtitle=vis1_params$subtitle),

                     mosaic_t = p <- df %>%
                       mutate(x__=.data[[var1]], y__=.data[[var2]]) %>%
                       ggplot() +
                       geom_mosaic(aes(x=product(y__, x__), fill=y__)) +
                       labs(x=ifelse(is.null(vis1_params$xlab), var1, vis1_params$xlab),
                            y=ifelse(is.null(vis1_params$ylab), var2, vis1_params$ylab),
                            fill=var2, title=vis1_params$title, subtitle=vis1_params$subtitle),
         ),
  )

  p
}


ggplot2_vis1_plot <- function(df, var,
                              vis1_var_type, vis1_num_plot_type, vis1_factor_plot_type,
                              vis1_params) {
  p <- df %>%
    dplyr::select(.data[[var]]) %>%
    mutate(index=row_number()) %>%
    ggplot()

  p <- if (vis1_var_type=="numeric") {
    switch (vis1_num_plot_type,
            scatter_t = p +
              geom_point(aes(x=index, y=.data[[var]]),
                         shape=vis1_params$char_shape, size=vis1_params$char_size, color=vis1_params$char_color,
                         fill=vis1_params$fill, alpha=vis1_params$alpha) +
              labs(x=ifelse(is.null(vis1_params$xlab), "索引", vis1_params$xlab),
                   y=ifelse(is.null(vis1_params$ylab), var, vis1_params$ylab),
                   title=vis1_params$title, subtitle=vis1_params$subtitle),

            histogram_t = p +
              (if(vis1_params$freq) geom_histogram(aes(x=.data[[var]], y=..density..), bins=vis1_params$bins, color=vis1_params$char_color, fill=vis1_params$fill, alpha=vis1_params$alpha)
               else geom_histogram(aes(x=.data[[var]], y=..count..  ), bins=vis1_params$bins, color=vis1_params$char_color, fill=vis1_params$fill, alpha=vis1_params$alpha)) +
              labs(x=ifelse(is.null(vis1_params$xlab), var, vis1_params$xlab),
                   y=ifelse(is.null(vis1_params$ylab), "频数", vis1_params$ylab),
                   title=vis1_params$title, subtitle=vis1_params$subtitle),

            density_t = p +
              geom_density(aes(x=.data[[var]]), color=vis1_params$char_color, fill=vis1_params$fill, alpha=vis1_params$alpha) +
              labs(
                x=if(is.null(vis1_params$xlab) & vis1_params$horizontal==F) var else vis1_params$xlab,
                y=if(is.null(vis1_params$ylab) & vis1_params$horizontal==T) var else vis1_params$ylab,
                title=vis1_params$title,
                subtitle=vis1_params$subtitle),

            boxplot_t = p +
              geom_boxplot(aes(x=factor(0), y=.data[[var]]),
                           notch=vis1_params$notch, width=vis1_params$width,
                           color=vis1_params$char_color, fill=vis1_params$fill, alpha=vis1_params$alpha) +
              labs(
                x=vis1_params$xlab,
                y=if(is.null(vis1_params$ylab)) var else vis1_params$ylab,
                title=vis1_params$title,
                subtitle=vis1_params$subtitle),

            violin_t = p +
              geom_violin(aes(x=factor(0), y=.data[[var]]),
                          width=vis1_params$width, color=vis1_params$line_color, fill=vis1_params$fill, alpha=vis1_params$alpha) +
              labs(
                x=vis1_params$xlab,
                y=if(is.null(vis1_params$ylab)) var else vis1_params$ylab,
                title=vis1_params$title,
                subtitle=vis1_params$subtitle),
    )

  } else {
    switch (vis1_factor_plot_type,
            barplot_t = p + geom_bar(aes(x=.data[[var]]),
                                     width=vis1_params$width,
                                     color=vis1_params$char_color, fill=vis1_params$fill, alpha=vis1_params$alpha) +
              labs(
                x=if(is.null(vis1_params$xlab)) var else vis1_params$xlab,
                y=vis1_params$ylab,
                title=vis1_params$title,
                subtitle=vis1_params$subtitle),

            pie_t = df %>%
              group_by(.data[[var]]) %>%
              count() %>%
              ggplot(aes(x='', y=n, fill=.data[[var]])) +
              geom_bar(stat='identity', width=1, alpha=vis1_params$alpha, color='black') +
              coord_polar(theta='y', start=vis1_params$start*pi/180, direction = ifelse(vis1_params$clockwise,-1,1)) + theme_void() +
              labs(fill=var,
                   title=if(!is.null(vis1_params$title)) vis1_params$title else NULL,
                   subtitle=if(!is.null(vis1_params$subtitle)) vis1_params$title else NULL),
    )
  }

  if (vis1_var_type=="numeric" & vis1_num_plot_type=="histogram_t" & vis1_params$freq & vis1_params$density) {
    p <- p + geom_density(aes(x=.data[[var]]), linetype=vis1_params$line_type, size=vis1_params$line_size, color=vis1_params$line_color, fill=NA)
  }

  if (vis1_var_type=="numeric" & vis1_num_plot_type=="density_t" & vis1_params$rug) {
    p <- p + geom_rug(aes(x=.data[[var]]))
  }

  if (vis1_var_type=='factor' & vis1_factor_plot_type=="barplot_t" & vis1_params$horizontal) {
    p <- p + coord_flip()
  }

  p
}

add_theme <- function(p, vis_theme, vis_fontsize) {
  switch(vis_theme,
         theme_gray = p + theme_gray(base_family="", base_size=vis_fontsize),
         theme_bw = p + theme_bw(base_family="", base_size=vis_fontsize),
         theme_linedraw = p + theme_linedraw(base_family="", base_size=vis_fontsize),
         theme_light = p + theme_light(base_family="", base_size=vis_fontsize),
         theme_dark = p + theme_dark(base_family="", base_size=vis_fontsize),
         theme_minimal = p + theme_minimal(base_family="", base_size=vis_fontsize),
         theme_classic = p + theme_classic(base_family="", base_size=vis_fontsize),
         theme_void = p + theme_void(base_family="", base_size=vis_fontsize),
         theme_base = p + theme_base(base_family="", base_size=vis_fontsize),
         theme_calc = p + theme_calc(base_family="", base_size=vis_fontsize),
         theme_clean = p + theme_clean(base_family="", base_size=vis_fontsize),
         theme_economist = p + theme_economist(base_family="", base_size=vis_fontsize),
         theme_economist_white = p + theme_economist_white(base_family="", base_size=vis_fontsize),
         theme_excel = p + theme_excel(base_family="", base_size=vis_fontsize),
         theme_excel_new = p + theme_excel_new(base_family="", base_size=vis_fontsize),
         theme_few = p + theme_few(base_family="", base_size=vis_fontsize),
         theme_fivethirtyeight = p + theme_fivethirtyeight(base_family="", base_size=vis_fontsize),
         theme_foundation = p + theme_foundation(base_family="", base_size=vis_fontsize),
         theme_gdocs = p + theme_gdocs(base_family="", base_size=vis_fontsize),
         theme_hc = p + theme_hc(base_family="", base_size=vis_fontsize),
         theme_igray = p + theme_igray(base_family="", base_size=vis_fontsize),
         theme_map = p + theme_map(base_family="", base_size=vis_fontsize),
         theme_pander = p + theme_pander(base_family="", base_size=vis_fontsize),
         theme_par = p + theme_par(base_family="", base_size=vis_fontsize),
         theme_solarized = p + theme_solarized(base_family="", base_size=vis_fontsize),
         theme_solarized_2 = p + theme_solarized_2(base_family="", base_size=vis_fontsize),
         theme_solid = p + theme_solid(base_family="", base_size=vis_fontsize),
         theme_stata = p + theme_stata(base_family="", base_size=vis_fontsize),
         theme_tufte = p + theme_tufte(base_family="", base_size=vis_fontsize),
         theme_wsj = p + theme_wsj(base_family="", base_size=vis_fontsize),
  )
}

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
    char_shape = 16,
    char_size = 2,
    char_color = "black",
    line_type = 1,
    line_size = 1,
    line_color = "black",
    fill = "lightgray",
    alpha = .5,
    bins = NULL,
    freq = F,
    density = F,
    rug = F,
    width = .7,
    notch = F,
    horizontal = F,
    start = 0,
    clockwise = F
  )

  # 保存可视化参数
  observeEvent(input$vis1_params_ok, {

    vis1_params$title <- if(!is.null(input$vis1_title) & !is_empty(input$vis1_title)) input$vis1_title else NULL
    vis1_params$subtitle <- if(!is.null(input$vis1_subtitle) & !is_empty(input$vis1_subtitle)) input$vis1_subtitle else NULL
    vis1_params$xlab <- if(!is.null(input$vis1_xlab) & !is_empty(input$vis1_xlab)) input$vis1_xlab else NULL
    vis1_params$ylab <- if(!is.null(input$vis1_ylab) & !is_empty(input$vis1_ylab)) input$vis1_ylab else NULL

    vis1_params$char_shape <- input$vis1_char_shape
    vis1_params$char_size <- input$vis1_char_size
    vis1_params$char_color <- input$vis1_char_color
    vis1_params$line_type <- input$vis1_line_type
    vis1_params$line_size <- input$vis1_line_size
    vis1_params$line_color <- input$vis1_line_color
    vis1_params$fill <- input$vis1_fill
    vis1_params$alpha <- input$vis1_alpha

    if (input$vis1_var_type == "numeric") {
      if (input$vis1_num_plot_type == 'histogram_t') {
        vis1_params$bins <- input$vis1_bins
        vis1_params$freq <- input$vis1_freq
        vis1_params$density <- input$vis1_density
      } else if (input$vis1_num_plot_type == 'density_t') {
        vis1_params$rug <- input$vis1_rug
      } else {
        vis1_params$width <- input$vis1_width
        vis1_params$notch <- input$vis1_notch
        vis1_params$horizontal <- input$vis1_horizontal
      }
    } else {
      if (input$vis1_factor_plot_type == 'barplot_t') {
        vis1_params$width <- input$vis1_width
        vis1_params$horizontal <- input$vis1_horizontal
      } else {
        vis1_params$start <- input$vis1_start
        vis1_params$clockwise <- input$vis1_clockwise
      }
    }
  })

  # 清除可视化参数
  observeEvent(input$vis1_params_clear, {
    vis1_params$title = NULL
    vis1_params$subtitle = NULL
    vis1_params$xlab = NULL
    vis1_params$ylab = NULL

    vis1_params$char_shape = 16
    vis1_params$char_size = 2
    vis1_params$char_color = "black"
    vis1_params$line_type = 1
    vis1_params$line_size = 2
    vis1_params$line_color = "black"
    vis1_params$fill = "lightgray"
    vis1_params$alpha = .5

    vis1_params$bins = NULL
    vis1_params$freq = F
    vis1_params$density = F
    vis1_params$rug = F
    vis1_params$width = .7
    vis1_params$notch = F
    vis1_params$horizontal = F
    vis1_params$angle = 0
    vis1_params$clockwise = F
  })

  vis1_without_theme <- reactive({
    req(input$vis1_var, input$vis1_var_type)

    ggplot2_vis1_plot(dataset(), input$vis1_var,
                      input$vis1_var_type,
                      input$vis1_num_plot_type,
                      input$vis1_factor_plot_type,
                      vis1_params)
  })

  vis1_with_theme <- reactive({
    p <- vis1_without_theme()

    if (input$vis1_var_type=="numeric" | input$vis1_factor_plot_type!="pie_t") {

      p <- add_theme(p, input$vis_theme, input$vis_fontsize)

      if (input$vis1_var_type=="numeric" & (input$vis1_num_plot_type=="boxplot_t" | input$vis1_num_plot_type=="violin_t")) {
        if(vis1_params$horizontal) {
          p <- p + coord_flip() + theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
        } else {
          p <- p + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
        }
      }
    }

    p
  })

  output$ggplot_vis1 <- renderPlot({
    vis1_with_theme()
  })

  output$vis_download = downloadHandler(

    filename=function() {
      paste0("Plot.", input$vis_fig_type)
    },

    content=function(file){

      fig <- if(input$sidebar=="Vis1") vis1_with_theme() else vis2_with_theme()
      ggsave(file, plot=fig, units="in", width=input$vis_fig_wd, height=input$vis_fig_ht)
    }
  )

  # 将所有变量（数值和因子）的名字保存在反应表达式中
  vis2_vars <- reactive({
    names(dataset())[purrr::map_lgl(dataset(), ~is.numeric(.) | is.factor(.))]
  })

  observeEvent(vis2_vars(), {
    freezeReactiveValue(input, "vis2_var1")
    freezeReactiveValue(input, "vis2_var2")
    updateSelectInput(inputId = "vis2_var1", choices = vis2_vars())
    updateSelectInput(inputId = "vis2_var2", choices = vis2_vars())
  })

  old_vis2_flag <- ""

  vis2_flag <- reactive({
    req(input$vis2_var1, input$vis2_var2)

    c(is.numeric(dataset()[[input$vis2_var1]]),
      is.numeric(dataset()[[input$vis2_var2]])) %>%
      factor(levels = c(FALSE, TRUE), labels = c("F", "T")) %>%
      str_c(collapse = "")
  })

 # output$vis2_type <- renderUI({
  observeEvent(vis2_flag(), {
    if (old_vis2_flag != vis2_flag()) {
      old_vis2_flag <<- vis2_flag()
      plot_types <- switch(vis2_flag(),
                           TT = c("散点图"="scatter_t", "光滑散点图"="smooth_scatter_t", "二维经验密度图"="density2d_t"),
                           TF = c("散列图"="strip_t", "克利夫兰点图"="cleveland_dot_t", "分组密度图"="group_density_t", "边缘密度图"="marginal_density_t", "条件密度图"="conditional_density_t"),
                           FT = c("箱线图"="boxplot_t"),
                           FF = c("条形图"="barplot_t", "点图"="dot_t", "马赛克图"="mosaic_t")
      )
      freezeReactiveValue(input, "vis2_plot_type")
      updateSelectInput(inputId="vis2_plot_type", choices=plot_types)
    }
    #selectInput("vis2_plot_type", "选择绘图类型：", choices=plot_types)
  })

  vis2_without_theme <- reactive({
    req(input$vis2_var1, input$vis2_var2, vis2_flag, input$vis2_plot_type)

    ggplot2_vis2_plot(dataset(), input$vis2_var1, input$vis2_var2, vis2_flag(), input$vis2_plot_type, vis1_params)
  })

  vis2_with_theme <- reactive({
    p <- add_theme(vis2_without_theme(), input$vis_theme, input$vis_fontsize)

    if (vis2_flag()=="TF" & input$vis2_plot_type=="cleveland_dot_t") {
      p <- p + theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
    }

    p
  })

  output$ggplot_vis2 <- renderPlot({
    vis2_with_theme()
  })

  output$message <- renderPrint({
    print(input$vis2_var1)
    print(input$vis2_var2)
    print(vis2_flag())
    print(input$vis2_plot_type)
    print(old_vis2_flag)
  })

  #input$vis2_var1, input$vis2_var1,
  #input$vis1_flag, input$plot_type

})
