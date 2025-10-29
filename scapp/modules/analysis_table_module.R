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
    ),
    fluidRow(
      shiny::downloadButton(ns("download_table_button"), "表をダウンロード")
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
      rv$no_previous_data <- is.null(processed_previous_year_data())
      rv$no_current_data <- is.null(processed_current_year_data())
      if(rv$no_previous_data){
        updateSelectInput(session,"display_mode", "表示モードの選択",
                          choices = c("偏差値(今回)" = "hensati"))
        
        showModal(modalDialog(
          "過去分のストレスチェックデータが読み込まれていないため差分の結果の表示はできません。",
          title = "過去データが読み込まれていません", 
          footer = modalButton("OK")
        ))
        
      }else if(!rv$no_current_data & !rv$no_previous_data){
        updateSelectInput(session,"display_mode", "表示モードの選択",
                    choices = c("偏差値(今回)" = "hensati",
                                "前回との差" = "diff",
                                "偏差値(前回)" = "hensati_prev"))
      }else if(rv$no_current_data){
        updateActionButton(session, "update_table_button", disabled = TRUE)
        
        showModal(modalDialog(
          "今年度分のストレスチェックデータが読み込まれていないため結果の表示はできません。",
          title = "今年度分データが読み込まれていません", 
          footer = modalButton("OK")
        ))
      }

    })
    
    table_data_and_settings <- eventReactive(input$update_table_button, {
      req(processed_current_year_data())
      
      display_mode <- isolate({input$display_mode})
      
      if(display_mode == "hensati"){
        
        current_data <- processed_current_year_data()  
      }else if(display_mode == "diff"){
        req(processed_previous_year_data())
        datanow <- processed_current_year_data()  
        datapast <- processed_previous_year_data()
        
      }else if(display_mode == "hensati_prev"){
        req(processed_previous_year_data())
        current_data <- processed_previous_year_data()
      }
      
      #グループ化する変数を選択
      group_vars <- isolate({switch(input$grouping_var,
                                    "dept1" = "dept1",
                                    "dept1_dept2" = c("dept1", "dept2"),
                                    "age_kubun" = "age_kubun",
                                    "gender" = "gender",
                                    "dept1"
      )})
      
      #どの基準値を利用するか選択
      target_sheet <- isolate({switch(input$grouping_var,
                                      "dept1" = "全体",
                                      "dept1_dept2" = "全体",
                                      "age_kubun" = "age_kubun",
                                      "gender" = "gender",
                                      "全体"
      )})
      
      gyousyu <- isolate(input$gyousyu)
      long_or_cross <- isolate(input$long_or_cross)
      
      #最終表を作成する
      if(display_mode %in% c("hensati","hensati_prev")){
        #単年度
        hyou_base <- calculate_hensati_hyou(
          current_data = current_data, 
          hensati_data = hensati_data, 
          target_sheet = target_sheet, 
          group_vars = group_vars,
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = gyousyu,
          target_longorcross = long_or_cross
        )
        
      } else { # "diff" の場合
        hyou_now <- calculate_hensati_hyou(
          current_data = current_data, 
          hensati_data = hensati_data,
          target_sheet = target_sheet, 
          group_vars = group_vars,
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = gyousyu,
          target_longorcross = long_or_cross
        )
        
        hyou_past <- calculate_hensati_hyou(
          current_data = previous_data, 
          hensati_data = hensati_data,
          target_sheet = target_sheet, 
          group_vars = group_vars,
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = gyousyu,
          target_longorcross = long_or_cross
        )
        
        #今回の人数情報を別の変数に保持しておく
        n_data_now <- hyou_now |> 
          select(all_of(group_vars), `受検人数`, `不完全回答人数`)
        
        # 差分計算 
        hyou_now_long <- hyou_now |> pivot_longer(cols = !all_of(group_vars), names_to = "name", values_to = "values_now")
        hyou_past_long <- hyou_past |> pivot_longer(cols = !all_of(group_vars), names_to = "name", values_to = "values_past")
        
        hyou <- hyou_now |> 
          left_join(hyou_past, by=c(group_vars, "name")) |> 
          mutate(diff = values_now - values_past) |> 
          select(all_of(group_vars), name, diff) |> 
          pivot_wider(id_cols = group_vars, names_from = name, values_from = diff) |> 
          ungroup()
        
        hyou_base <- hyou
        
      }
      
      if(display_mode == "diff"){
        sets <- setting_hensati_hyou("diff", group_vars)
      }else if(display_mode %in% c("hensati","hensati_prev")){
        sets <- setting_hensati_hyou("single", group_vars) 
      }
      
      limitnum <- isolate(input$limitnumber)
      
      # 表の順番を整える
      hyou <- hyou_base |> relocate(all_of(sets$column_order))
      
      # 高ストレス者割合が少数なので100倍する (列が存在する場合のみ実行)
      if ("高ストレス者割合" %in% names(hyou)) {
        hyou <- hyou |> mutate(`高ストレス者割合` = 100 * `高ストレス者割合`)
      }
      
      # limitnumber以下の人数に描画を制限する (列が存在する場合のみ実行)
      if (all(c("受検人数", "不完全回答人数") %in% names(hyou))) {
        hyou <- hyou |> 
          mutate(ishide = (`受検人数` - `不完全回答人数`) < limitnum) |> 
          mutate(across(.cols = !c(`受検人数`, `不完全回答人数`, ishide, matches("dept")),
                        .fns = ~ if_else(ishide, NA, .)
          )) |> 
          select(!ishide)
      }
      
      return(list(
        processed_data = hyou,
        settings = sets
      ))
      
      
    }, ignoreNULL = FALSE)

    

    
    reactable_widget <- reactive({
      req(table_data_and_settings())
      
      result <- table_data_and_settings()
      hyou_data <- result$processed_data
      sets <- result$settings
      
      # reactable オブジェクトを作成
      reactable(
        hyou_data,
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
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 15, 30, 60),
        theme = reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5ff",
          cellPadding = "8px 12px"
        )
      )
    )})
    
    # テーブルのレンダリング (renderDataTable から renderReactable に変更)
    output$summary_table <- renderReactable({
      reactable_widget()
      
    })
    
    
    #テーブルのダウンロード
    output$download_table_button <- downloadHandler(
      filename = function() {
        
        group_name <- input$grouping_var
        mode_name <- input$display_mode
        paste0("集団分析結果_", group_name, "_", mode_name, "_", Sys.Date(), ".html")
      },
      content = function(file) {
        req(reactable_widget(), cancelOutput = TRUE, message = "表を更新ボタンを押してください。")
        widget_to_save <- reactable_widget()
        
        htmlwidgets::saveWidget(widget = widget_to_save, file = file, selfcontained = TRUE)
      },
      contentType = "text/html"
    )
      
    })
}
