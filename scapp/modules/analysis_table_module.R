# 表描画モジュールUI----------------------
analysis_table_module_ui <- function(id) {
  ns <- NS(id) # 名前空間を取得

  skrisk_gyousyu <- c(
    "全産業",
    "医療・福祉",
    "運輸・郵便業",
    "卸売・小売業",
    "教育・学習支援業",
    "金融・保険業",
    "建設業",
    "公務",
    "サービス業",
    "情報通信業",
    "製造業"
  )

  tagList(
    fluidRow(
      box(
        title = "表示設定",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        fluidRow(
          column(width = 4,
                 selectInput(ns("grouping_var"), "集計単位の選択",
                             choices = c("部署（大分類）" = "dept1",
                                         "部署（大分類 - 中分類）" = "dept1_dept2",
                                         "年齢区分" = "age_kubun",
                                         "性別" = "gender"))
          ),
          column(width = 4,
                 selectInput(ns("display_mode"), "表示モードの選択",
                             choices = c("偏差値(今回)" = "hensati",
                                         #"前回との差" = "diff",
                                         "偏差値(前回)" = "hensati_prev")),
                 numericInput(ns("limitnumber"),"分析対象の最低人数を設定する",value=10, min=5)
          ),
          column(width = 4,
                 selectInput(ns("gyousyu"), "総合健康リスク計算の業種の選択",
                             choices = skrisk_gyousyu),
                 selectInput(ns("long_or_cross"), "総合健康リスク計算の種類",
                             choices = c("縦断(推奨)" = "long","横断" = "cross")),
                 br(), # 見た目調整
                 actionButton(ns("update_table_button"), "表を更新", icon = icon("sync"))
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "集団分析結果",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        # dataTableOutput から reactableOutput に変更
        reactableOutput(ns("summary_table")) |> withSpinner(type = 6, color = "#0dc5c1")
      )
    )
  )
}


# 表描画モジュールサーバー -------
analysis_table_module_server <- function(id,
                                         processed_current_year_data, # reactive({ tibble })
                                         processed_previous_year_data) { # reactive({ tibble | NULL })
  
  moduleServer(id, function(input, output, session) {
    
    # --- 静的データの読み込み ---
    # アプリ起動時に一度だけ読み込む
    hensati_data <- read_csv("modules/table11.csv")
    nbjsq <- read_csv("modules/nbjsq_question_text.csv")
    nbjsqlabs <- read_csv("modules/nbjsq_label_hensati.csv")
    
    ns <- session$ns
    
    # --- リアクティブ: データの有無をチェック ---
    # データの有無をリアクティブな値として保持することで、コードの可読性を高める
    has_current_data <- reactive({ !is.null(processed_current_year_data()) })
    has_previous_data <- reactive({ !is.null(processed_previous_year_data()) })
    
    # --- UIの動的更新 ---
    # データの有無に応じてUI要素（ボタン、選択肢）を更新する
    observe({
      
      # 2. 過去データの有無で表示モードの選択肢を切り替え
      if (has_previous_data() && has_current_data()) {
        updateSelectInput(session, "display_mode",
                          choices = c("偏差値(今回)" = "hensati",
                                      "前回との差" = "diff",
                                      "偏差値(前回)" = "hensati_prev"),
                          selected = input$display_mode) # ユーザーの選択を維持
      } else {
        updateSelectInput(session, "display_mode",
                          choices = c("偏差値(今回)" = "hensati"),
                          selected = "hensati")
      }
    })
    
    # --- 表データの計算（ボタンクリック時にのみ実行） ---
    # eventReactiveのトリガーを input$update_table_button に限定し、
    # 他の入力値は isolate() でラップして依存関係から切り離す。
    display_data_and_params <- eventReactive(input$update_table_button, {
      
      # 必須データの存在を最初に確認
      req(has_current_data(), cancelOutput = TRUE)
      
      # ボタンクリック時の入力値を isolate() で一度に取得
      params <- isolate({
        list(
          display_mode = input$display_mode,
          grouping_var = input$grouping_var,
          gyousyu = input$gyousyu,
          long_or_cross = input$long_or_cross,
          limitnumber = input$limitnumber
        )
      })
      
      # 表示モードに応じて過去データの要否をチェック
      if (params$display_mode %in% c("diff", "hensati_prev")) {
        req(has_previous_data(), cancelOutput = TRUE)
      }
      
      # パラメータに基づいてグループ化変数などを決定（inputを直接参照しない）
      group_vars <- switch(params$grouping_var,
                           "dept1" = "dept1",
                           "dept1_dept2" = c("dept1", "dept2"),
                           "age_kubun" = "age_kubun",
                           "gender" = "gender",
                           "dept1")
      
      target_sheet <- switch(params$grouping_var,
                             "age_kubun" = "age_kubun",
                             "gender" = "gender",
                             "全体")
      
      # データの準備
      current_data <- processed_current_year_data()
      previous_data <- processed_previous_year_data()
      
      # 表示モードに応じて計算を実行
      
      if (params$display_mode %in% c("hensati", "hensati_prev")) {
        data_to_use <- if (params$display_mode == "hensati") current_data else previous_data
        
        hyou <- calculate_hensati_hyou(
          current_data = data_to_use,
          hensati_data = hensati_data,
          target_sheet = target_sheet, 
          group_vars = group_vars,
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = params$gyousyu, 
          target_longorcross = params$long_or_cross
        )
        
      } else { # "diff" の場合
        hyou_now <- calculate_hensati_hyou(
          current_data = current_data, 
          hensati_data = hensati_data,
          target_sheet = target_sheet, 
          group_vars = group_vars,
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = params$gyousyu, 
          target_longorcross = params$long_or_cross
        )
        
        hyou_past <- calculate_hensati_hyou(
          current_data = previous_data, 
          hensati_data = hensati_data,
          target_sheet = target_sheet, 
          group_vars = group_vars,
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = params$gyousyu, 
          target_longorcross = params$long_or_cross
        )
        
        #今回の人数情報を別の変数に保持しておく
        n_data_now <- hyou_now |> 
          select(all_of(group_vars), `受検人数`, `不完全回答人数`)
        
        # 差分計算 
        hyou_now_long <- hyou_now |> pivot_longer(cols = !all_of(group_vars), names_to = "name", values_to = "values_now")
        hyou_past_long <- hyou_past |> pivot_longer(cols = !all_of(group_vars), names_to = "name", values_to = "values_past")
        
        hyou <- hyou_now_long |>
          left_join(hyou_past_long, by = c(group_vars, "name")) |>
          mutate(diff = values_now - values_past) |>
          select(all_of(group_vars), name, diff) |>
          pivot_wider(id_cols = all_of(group_vars), names_from = name, values_from = diff) |>
          ungroup() |> 
        # 分データに、保持しておいた今回の人数情報を結合
        left_join(n_data_now, by = group_vars, suffix = c("差","今回値"))
      }
      
      # 計算結果と、その計算に使ったパラメータをリストで返す
      list(
        hyou = hyou,
        params = params,
        group_vars = group_vars
      )
      
    }, ignoreNULL = FALSE)
    
    # --- テーブルのレンダリング ---
    output$summary_table <- renderReactable({
      # validate() でユーザーに分かりやすいフィードバックを提供
      # validate(
      #   need(has_current_data(), "今年度のストレスチェックデータを読み込んでください。")
      # )
      
      # ボタンが押されると、display_data_and_params() が実行され、結果がここに渡される
      calc_results <- display_data_and_params()
      
      # 計算結果がまだない場合（初期表示時など）はここで処理を中断
      req(calc_results, cancelOutput = TRUE)
      
      # リストからデータとパラメータを安全に受け取る
      # これにより、この描画ブロックは input の値に直接依存しなくなる
      hyou <- calc_results$hyou
      params <- calc_results$params
      group_vars <- calc_results$group_vars
      
      # validate(
      #   need(nrow(hyou) > 0, "計算結果がありません。集計単位や設定を確認してください。")
      # )
      
      # 表の設定と整形 (inputを直接参照せず、受け取ったパラメータを使用)
      if (params$display_mode == "diff") {
        sets <- setting_hensati_hyou("diff", group_vars) 
      } else {
        sets <- setting_hensati_hyou("single", group_vars)
      }
      
      # 表の整形
      hyou_final <- hyou |>
        relocate(all_of(sets$column_order)) |>
        mutate(`高ストレス者割合` = 100 * `高ストレス者割合`)
      
      if(params$display_mode == "diff"){
        hyou_final <- hyou_final |> 
          mutate(
            .is_hidden = (`受検人数今回値` - `不完全回答人数今回値`) < params$limitnumber,
            across(
              .cols = !c(`受検人数差`, `不完全回答人数差`, .is_hidden, matches("dept")),
              .fns = ~ if_else(.is_hidden, NA_real_, .) # NAの型を明示
            )
          ) |>
          select(-.is_hidden)
      }else if(params$display_mode == "single"){
        hyou_final <- hyou_final |> 
          mutate(
            .is_hidden = (`受検人数` - `不完全回答人数`) < params$limitnumber,
            across(
              .cols = !c(`受検人数`, `不完全回答人数`, .is_hidden, matches("dept")),
              .fns = ~ if_else(.is_hidden, NA_real_, .) # NAの型を明示
            )
          ) |>
          select(-.is_hidden)
      }
        
      
      # Reactable描画
      reactable(
        hyou_final,
        defaultColDef = sets$default_col_def,
        columns = sets$column_setting_list,
        columnGroups = sets$col_group_list,
        filterable = FALSE,
        searchable = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        striped = TRUE,
        compact = TRUE,
        wrap = FALSE,
        defaultPageSize = 15,
        minRows = 15,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 15, 30, 60),
        theme = reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5ff",
          cellPadding = "8px 12px"
        )
      )
    })
  })
}