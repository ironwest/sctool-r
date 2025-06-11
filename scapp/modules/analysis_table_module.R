# --- 1. 必要ライブラリ ---
# reactable を追加
library(shiny)
library(shinydashboard)
library(dplyr)
library(reactable) # DT の代わりに reactable を使用
library(DT) # もし他の部分でDTを使っている場合は残す（このスクリプトでは不要）

# --- 2. 表描画モジュールUI (`analysis_table_module_ui`) ---
analysis_table_module_ui <- function(id) {
  ns <- NS(id) # 名前空間を取得
  
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
                                         "年齢区分・性別" = "age_gender"))
          ),
          column(width = 4,
                 selectInput(ns("display_mode"), "表示モードの選択",
                             choices = c("標準得点 (T得点)" = "t_score",
                                         "前回との差" = "diff_from_previous",
                                         "平均値" = "mean_score"))
          ),
          column(width = 4,
                 # 将来的なフィルター用のスペース
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
        reactableOutput(ns("summary_table"))
      )
    )
  )
}


# --- 3. 表描画モジュールサーバー (`analysis_table_module_server`) ---
analysis_table_module_server <- function(id,
                                         processed_current_year_data, # reactive({ tibble })
                                         processed_previous_year_data) { # reactive({ tibble | NULL })
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    nbjsq <- read_csv("nbjsq_question_text.csv")
    
    display_data <- eventReactive(input$update_table_button, {
      req(processed_current_year_data())
      current_data <- processed_current_year_data()
      
      
      # グループ化する変数
      group_vars <- switch(input$grouping_var,
                           "dept1" = "dept1",
                           "dept1_dept2" = c("dept1", "dept2"),
                           "age_gender" = c("age_group", "gender"),
                           "dept1"
      )
      
      # 集計対象の列
      nbjsq |> 
        select(syakudo_minor, syakudo_minor_eng) |> distinct()
      colnames(current_data)
      score_columns <- str_c("q",1:13)
      
      # 集計処理
      summary_result <- current_data |>
        group_by(across(all_of(group_vars))) |>
        summarise(
          n = n(),
          across(all_of(score_columns), ~mean(.x, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        mutate(across(where(is.numeric) & !matches("^n$"), ~round(.x, 2)))
      
      return(summary_result)
      
    }, ignoreNULL = FALSE)
    
    # テーブルのレンダリング (renderDataTable から renderReactable に変更)
    output$summary_table <- renderReactable({
      req(display_data())
      
      # reactable() 関数でテーブルを生成
      reactable(
        display_data(),
        filterable = TRUE,     # 各列のフィルターを有効化 (DTのfilter='top'に相当)
        searchable = TRUE,      # 全体検索ボックスを有効化
        highlight = TRUE,       # 行をホバーした際にハイライト
        bordered = TRUE,        # 境界線を表示
        striped = TRUE,         # 縞模様
        compact = TRUE,         # コンパクト表示
        wrap = FALSE,           # 横スクロールを有効化 (DTのscrollX=TRUEに相当)
        defaultPageSize = 15,   # 1ページの表示行数 (DTのpageLengthに相当)
        showPageSizeOptions = TRUE, # ページサイズ変更オプションを表示
        pageSizeOptions = c(10, 15, 25, 50), # ページサイズの選択肢
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


# --- 4. モジュールをテストするためのスタンドアロンアプリ ---
# ダミーデータの作成
dummy_current_data <- read_csv("../demodata/processed_nbjsq_dummy_data1_alpha.csv")
dummy_previous_data <- read_csv("../demodata/processed_nbjsq_dummy_data2_alpha.csv")



# ---- アプリUI ----
ui <- dashboardPage(
  dashboardHeader(title = "表描画モジュール テスト (reactable版)"),
  dashboardSidebar(),
  dashboardBody(
    analysis_table_module_ui("analysis_table")
  )
)

# ---- アプリサーバー ----
server <- function(input, output, session) {
  
  current_data_reactive <- reactive({ dummy_current_data })
  previous_data_reactive <- reactive({ dummy_previous_data })
  
  analysis_table_module_server(
    id = "analysis_table",
    processed_current_year_data = current_data_reactive,
    processed_previous_year_data = previous_data_reactive
  )
  
}

# --- 5. アプリケーションの実行 ---
shinyApp(ui, server)