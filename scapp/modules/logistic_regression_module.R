# ロジスティック回帰分析モジュールUI --------
analysis_regression_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "ロジスティック回帰分析設定",
        width = 12,
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        p("注意：この機能は統計的な知識を持つ専門家向けのものです。前年度の説明変数と、今年度の目的変数を選択できます。"),
        fluidRow(
          column(width = 4,
                 selectInput(ns("outcome_var"), "目的変数（今年度データから）", choices = NULL),
                 p("is_hs変数が高ストレス者の有無を表す変数です。"),
                 # プレビュー表示エリアを追加
                 uiOutput(ns("outcome_preview_ui")),
                 # 閾値設定UIはプレビューUIの中で動的に表示
                 uiOutput(ns("outcome_threshold_ui"))
          ),
          column(width = 4,
                 selectizeInput(ns("predictor_vars"), "説明変数（前年度データから）", choices = NULL, multiple = TRUE,
                                options = list(placeholder = '分析したい要因を選択'))
          )
        ),
        actionButton(ns("run_regression_button"), "回帰分析を実行", icon = icon("calculator"), class = "btn-success")
      )
    ),
    fluidRow(
      box(
        title = "分析結果",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        tabsetPanel(
          tabPanel("モデルサマリー", verbatimTextOutput(ns("model_summary_output")) |> withSpinner()),
          tabPanel("係数・オッズ比", reactableOutput(ns("model_coefficients_table")) |> withSpinner())
        )
      )
    )
  )
}

# ロジスティック回帰分析モジュールサーバー ---
analysis_regression_module_server <- function(id,
                                              processed_current_year_data,
                                              processed_previous_year_data) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_past <- reactive({
      processed_previous_year_data() |> 
        mutate(across(matches("^q"), as.factor))
    })
    
    #過去データがない場合はエラーをModalでユーザーに警告
    observe({
      
      isnull_now  <- is.null(processed_current_year_data())
      isnull_past <- is.null(processed_previous_year_data())
      # 
      # if(isnull_now & isnull_past){
      #   showModal(modalDialog(
      #     "今年と過去のデータが存在しないため分析できません。",
      #     footer = modalButton("OK")
      #   ))
      # }else if(isnull_now & !isnull_past){
      #   showModal(modalDialog(
      #    "今年のデータが存在しないため分析できません。",
      #     footer = modalButton("OK")
      #   ))
      # }else if(!isnull_now & isnull_past){
      #   showModal(modalDialog(
      #    "昨年のデータが存在しないため分析できません。",
      #     footer = modalButton("OK")
      #   ))
      # }
      
      # if(isnull_now | isnull_past){
      #   updateActionButton(session, "run_regression_button",disabled = TRUE)
      # }
    })
    
    # UIの選択肢を動的に更新
    observe({
      req(processed_current_year_data(), processed_previous_year_data())
      data_now <- processed_current_year_data()
      data_past <- data_past()
      
      # 目的変数の候補 (数値、文字、因子)
      choices_now <- colnames(data_now)
      # 説明変数の候補 (数値)
      choices_past <- colnames(data_past)
      
      updateSelectInput(session, "outcome_var", choices = choices_now, selected = "is_hs")
      updateSelectizeInput(session, "predictor_vars", choices = choices_past)
    })
    
    # アウトカムプレビューの動的生成-------
    output$outcome_preview_ui <- renderUI({
      req(processed_current_year_data(), input$outcome_var)
      
      selected_var_name <- input$outcome_var
      data_vec <- processed_current_year_data()[[selected_var_name]]
      
      if (is.numeric(data_vec)) {
        # 連続変数の場合: ヒストグラムを表示
        tagList(
          hr(),
          h5("ヒストグラムと閾値プレビュー"),
          plotOutput(ns("outcome_histogram"), height = "200px")
        )
      } else if (is.character(data_vec) || is.factor(data_vec)) {
        # カテゴリー変数の場合: 件数とプレビューの条件をチェック
        counts_df <- as_tibble(data_vec) |>
          rename(value = 1) |>
          count(value, name = "件数", sort = TRUE)
        
        if (nrow(counts_df) > 10) {
          # カテゴリー数が10を超える場合はメッセージ表示
          tagList(
            hr(),
            p("カテゴリー数が10を超えているため、プレビューは表示されません。", style = "color: grey;")
          )
        } else {
          # 10以下の場合は度数分布表を表示
          tagList(
            hr(),
            h5("カテゴリー別件数プレビュー"),
            div(style = "max-height: 250px; overflow-y: auto;",
                reactableOutput(ns("outcome_counts_table"))
            )
          )
        }
      } else {
        NULL # その他の型の場合は何も表示しない
      }
    })
    
    # 連続変数の場合のみ閾値設定UIを表示
    output$outcome_threshold_ui <- renderUI({
      req(processed_current_year_data(), input$outcome_var)
      data_vec <- processed_current_year_data()[[input$outcome_var]]
      if (is.numeric(data_vec)) {
        numericInput(ns("outcome_threshold"), "目的変数の閾値（この値以上を1とする）", 
                     value = round(median(data_vec, na.rm = TRUE), 2), # 初期値を中央値に
                     min = min(data_vec, na.rm = TRUE),
                     max = max(data_vec, na.rm = TRUE))
      } else {
        NULL
      }
    })
    
    # ヒストグラムの描画
    output$outcome_histogram <- renderPlot({
      req(is.numeric(processed_current_year_data()[[input$outcome_var]]), !is.null(input$outcome_threshold))
      
      ggplot(processed_current_year_data(), aes_string(x = input$outcome_var)) +
        geom_histogram(bins = 30, fill = "cornflowerblue", color = "white", alpha = 0.8) +
        geom_vline(aes(xintercept = input$outcome_threshold), color = "red", linetype = "dashed", size = 1) +
        labs(title = NULL, x = input$outcome_var, y = "度数") +
        theme_minimal(base_size = 14)
    }, res = 96)
    
    # 度数分布表の描画
    output$outcome_counts_table <- renderReactable({
      req(is.character(processed_current_year_data()[[input$outcome_var]]) || is.factor(processed_current_year_data()[[input$outcome_var]]))
      
      as_tibble(processed_current_year_data()[[input$outcome_var]]) |>
        rename(カテゴリー = 1) |>
        count(カテゴリー, name = "件数", sort = TRUE) |>
        reactable(compact = TRUE, bordered = TRUE, highlight = TRUE)
    })
    
    # 分析結果を保持するリアクティブな値------
    analysis_results <- eventReactive(input$run_regression_button, {
      # req(
      #   processed_current_year_data(),
      #   processed_previous_year_data(),
      #   input$outcome_var,
      #   input$predictor_vars
      # )
      
      data_now <- processed_current_year_data()
      data_past <- data_past()
      
      if(is.null(data_now) | is.null(data_past)){
        
        showModal(modalDialog(
         "データが存在しないため分析できません。",
          footer = modalButton("OK")
        ))
        
        return()
      }else{
        # 目的変数（今年度）と empid を選択
        outcome_data <- data_now |>
          select(empid, outcome_col = all_of(input$outcome_var))
        
        # 目的変数の二値化
        # 連続変数の場合:
        if(is.numeric(outcome_data$outcome_col)){
          req(input$outcome_threshold)
          outcome_data <- outcome_data |>
            mutate(outcome_binary = if_else(outcome_col >= input$outcome_threshold, 1, 0))
        } else {
          # カテゴリー変数の場合: 最初のカテゴリーを1、それ以外を0とする
          first_category <- unique(na.omit(outcome_data$outcome_col))[1]
          showNotification(paste0("カテゴリー変数 '", first_category, "' を1、その他を0として分析します。"), type="warning")
          outcome_data <- outcome_data |>
            mutate(outcome_binary = if_else(outcome_col == first_category & !is.na(outcome_col), 1, 0))
        }
        outcome_data <- outcome_data |> select(empid, outcome_binary)
        
        all_predictors <- input$predictor_vars
        predictor_data <- data_past |>
          select(empid, all_of(all_predictors))
        model_data <- inner_join(outcome_data, predictor_data, by = "empid") |>
          na.omit() 
        
        if (nrow(model_data) < 50 || length(unique(model_data$outcome_binary)) < 2) {
          showNotification("分析対象データが不足しているか、結果が1種類しかないため分析を実行できません。", type = "error")
          return(NULL)
        }
        
        formula_str <- paste("outcome_binary ~", paste(all_predictors, collapse = " + "))
        model <- glm(as.formula(formula_str), data = model_data, family = binomial(link = "logit"))
        
        model_summary <- summary(model)
        coefficients_table <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
          rename(
            odds_ratio = estimate,
            conf_low_or = conf.low,
            conf_high_or = conf.high
          ) |>
          mutate(across(where(is.numeric), ~round(.x, 3)))
        
        return(list(summary = model_summary, coefficients = coefficients_table))
      }
      
      
    })
    
    # 結果の出力
    output$model_summary_output <- renderPrint({
      req(analysis_results())
      analysis_results()$summary
    })
    
    output$model_coefficients_table <- renderReactable({
      req(analysis_results())
      reactable(
        analysis_results()$coefficients,
        columns = list(
          term = colDef(name = "変数"),
          odds_ratio = colDef(name = "オッズ比"),
          std.error = colDef(name = "標準誤差"),
          statistic = colDef(name = "z値"),
          p.value = colDef(name = "p値", format = colFormat(digits = 3)),
          conf_low_or = colDef(name = "信頼区間(下限)"),
          conf_high_or = colDef(name = "信頼区間(上限)")
        ),
        highlight = TRUE, bordered = TRUE, compact = TRUE, wrap = FALSE
      )
    })
    
  })
}