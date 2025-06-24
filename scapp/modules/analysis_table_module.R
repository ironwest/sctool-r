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
                                         "前回との差" = "diff",
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
    
    rv <- reactiveValues(
      no_previous_data = FALSE,
      no_current_data = FALSE
    )
    
    hensati_data <- read_csv("modules/table11.csv")
    nbjsq <- read_csv("modules/nbjsq_question_text.csv")
    nbjsqlabs <- read_csv("modules/nbjsq_label_hensati.csv")
    
    ns <- session$ns
    
    #過去データがない場合にdiffや過去の表示はさせない
    observe({
      rv$no_previous_data <- is.null(processed_previous_year_data())
      rv$no_current_data <- is.null(processed_current_year_data())
      if(rv$no_previous_data){
        updateSelectInput(session,"display_mode", "表示モードの選択",
                          choices = c("偏差値(今回)" = "hensati"))
        
        
      }else if(!rv$no_current_data & !rv$no_previous_data){
        updateSelectInput(session,"display_mode", "表示モードの選択",
                    choices = c("偏差値(今回)" = "hensati",
                                "前回との差" = "diff",
                                "偏差値(前回)" = "hensati_prev"))
      }else if(rv$no_current_data){
        updateActionButton(session, "update_table_button", disabled = TRUE)
      }

    })

    
    display_data <- eventReactive(input$update_table_button, {
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
      
      #最終表を作成する
      if(display_mode %in% c("hensati","hensati_prev")){
        #単年度
        hyou <- calculate_hensati_hyou(
          current_data = current_data, 
          hensati_data = hensati_data, 
          target_sheet = target_sheet, 
          group_vars = group_vars, 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = input$gyousyu,
          target_longorcross = input$long_or_cross
        )
        
      }else if(display_mode == "diff"){
        #年度の比較
        hyou_now <- calculate_hensati_hyou(
          current_data = datanow, 
          hensati_data = hensati_data, 
          target_sheet = target_sheet, 
          group_vars = group_vars, 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = input$gyousyu,
          target_longorcross = input$long_or_cross
        )
        
        hyou_past <- calculate_hensati_hyou(
          current_data = datapast, 
          hensati_data = hensati_data, 
          target_sheet = target_sheet, 
          group_vars = group_vars, 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = input$gyousyu,
          target_longorcross = input$long_or_cross
        )
        
        hyou_now <- hyou_now |> 
          pivot_longer(cols = !group_vars) |> 
          rename(values_now = value)
        
        hyou_past <- hyou_past |> 
          pivot_longer(cols = !group_vars) |> 
          rename(values_past = value)
        
        hyou <- hyou_now |> 
          left_join(hyou_past, by=c(group_vars, "name")) |> 
          mutate(diff = values_now - values_past) |> 
          select(all_of(group_vars), name, diff) |> 
          pivot_wider(id_cols = group_vars, names_from = name, values_from = diff) |> 
          ungroup()
        
      }
      
    return(hyou)
      
    }, ignoreNULL = FALSE)
    
    # テーブルのレンダリング (renderDataTable から renderReactable に変更)
    output$summary_table <- renderReactable({
      req(display_data())
      
      
  
      group_vars <- isolate({switch(input$grouping_var,
             "dept1" = "dept1",
             "dept1_dept2" = c("dept1", "dept2"),
             "age_kubun" = "age_kubun",
             "gender" = "gender",
             "dept1"
      )})
      
      display_mode <- isolate({input$display_mode})
      if(display_mode == "diff"){
        sets <- setting_hensati_hyou("diff", group_vars)
      }else if(display_mode %in% c("hensati","hensati_prev")){
        sets <- setting_hensati_hyou("single", group_vars) 
      }
    
      #表の順番を整える-------------------
      hyou <- display_data() |> relocate(all_of(sets$column_order))
      
      #高ストレス者割合が少数なので100倍する
      hyou <- hyou |>  mutate(`高ストレス者割合` = 100*高ストレス者割合)
      
      #limitnumber以下の人数に描画を制限する
      limitnum <- isolate(input$limitnumber)
      
      
      hyou <- hyou |> 
        mutate(ishide = (`受検人数`-`不完全回答人数`)<limitnum) |> 
        mutate(across(.cols = !c(`受検人数`,`不完全回答人数`,ishide, matches("dept")),
                      .fns = ~ if_else(ishide,NA,.)
                      )) |> 
        select(!ishide)
      
      
      #reactableでの描画
      reactable(
        hyou,
        defaultColDef = sets$default_col_def,
        columns = sets$column_setting_list,
        columnGroups = sets$col_group_list,
        filterable = FALSE,     # 各列のフィルターを有効化 (DTのfilter='top'に相当)
        searchable = TRUE,      # 全体検索ボックスを有効化
        highlight = TRUE,       # 行をホバーした際にハイライト
        bordered = TRUE,        # 境界線を表示
        striped = TRUE,         # 縞模様
        compact = TRUE,         # コンパクト表示
        wrap = FALSE,           # 横スクロールを有効化 (DTのscrollX=TRUEに相当)
        defaultPageSize = 15,   # 1ページの表示行数 (DTのpageLengthに相当)
        minRows = 15,
        resizable = TRUE,
        showPageSizeOptions = TRUE, # ページサイズ変更オプションを表示
        pageSizeOptions = c(10, 15, 30, 60), # ページサイズの選択肢
        theme = reactableTheme( # 簡単なテーマ設定
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5ff",
          cellPadding = "8px 12px"
        )
      )
      
    })
    
  })
}