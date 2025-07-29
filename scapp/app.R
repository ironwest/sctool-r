# app.R
# global.Rでライブラリやソースは読み込み済み
source("global.R")

# --- アプリケーションUIの定義 ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ストレスチェック集団分析"),
  dashboardSidebar(
    # shinyjsを有効化
    shinyjs::useShinyjs(),
    sidebarMenu(
      id = "main_tabs",
      menuItem("はじめに", tabName = "welcome", icon = icon("info-circle")),
      menuItem("データ設定", tabName = "data_setup", icon = icon("cogs"),
               menuSubItem("今年度データ設定", tabName = "current_year_setup"),
               menuSubItem("昨年度データ設定", tabName = "previous_year_setup")
      ),
      menuItem("分析", tabName = "analysis_menu", icon = icon("chart-bar"),
               menuSubItem("集計表", tabName = "analysis_table"),
               menuSubItem("部署比較分析", tabName = "dept_comparison"),
               menuSubItem("要因探索 (回帰分析)", tabName = "regression_analysis")
      )
    )
  ),
  dashboardBody(
    tabItems(
      # 各タブに対応するUIを配置
      tabItem(tabName = "welcome",
              h2("ようこそ"),
              p("このツールは、ストレスチェックデータの集団分析を行います。"),
              p("左のメニューから「データ設定」を選び、分析したいデータを準備してください。"),
              p("オンラインデモのための仮想データは、"),
              tags$a(href="https://drive.google.com/drive/folders/1j7t7owZTC-yUByONbgI8Vder67vvWxIB?usp=sharing", "こちらのリンク"),
              p("からダウンロードできます。")
      ),
      tabItem(tabName = "current_year_setup",
              h2("今年度データ設定ウィザード"),
              wizard_module_ui("current_year_wizard") # 今年度用ウィザードモジュールUI
      ),
      tabItem(tabName = "previous_year_setup",
              h2("昨年度データ設定ウィザード"),
              wizard_module_ui("previous_year_wizard") # 昨年度用ウィザードモジュールUI
      ),
      tabItem(tabName = "analysis_table",
              h2("全体集計表"),
              analysis_table_module_ui("analysis_table")
      ),
      tabItem(tabName = "dept_comparison",
              h2("部署比較分析"),
              dept_comparison_module_ui("dept_comparison_module") # 部署比較モジュールUI
      ),
      tabItem(tabName = "regression_analysis",
              h2("要因探索（ロジスティック回帰分析）"),
              analysis_regression_module_ui("regression_module") # 回帰分析モジュールUI
      )
    )
  )
)


# --- アプリケーションサーバーの定義 ---
server <- function(input, output, session) {
  
  # --- モジュールサーバーの呼び出しとデータ連携 ---
  
  # データ設定ウィザードモジュールを呼び出し
  current_year_data <- wizard_module_server("current_year_wizard", year_label = "今年度")
  previous_year_data <- wizard_module_server("previous_year_wizard", year_label = "昨年度")

  # --- UIの動的制御 ---
  # 今年度のデータが設定されるまで分析タブを無効化する
  # observe({
  #   # is_setup_completeはreactiveなので()をつけて呼び出す
  #   if (isTRUE(current_year_data$is_setup_complete())) {
  #     # menuItemのdata-valueは自動でtabNameと同じになる
  #     shinyjs::enable(selector = "a[data-value='analysis_table']")
  #     shinyjs::enable(selector = "a[data-value='dept_comparison']")
  #     shinyjs::enable(selector = "a[data-value='regression_analysis']")
  #   } else {
  #     shinyjs::disable(selector = "a[data-value='analysis_table']")
  #     shinyjs::disable(selector = "a[data-value='dept_comparison']")
  #     shinyjs::disable(selector = "a[data-value='regression_analysis']")
  #   }
  # })
  
  # 表描画モジュールの呼び出し
  analysis_table_module_server(
    id = "analysis_table",
    processed_current_year_data = current_year_data$get_processed_data,
    processed_previous_year_data = previous_year_data$get_processed_data
  )
  
  # 部署比較分析モジュールを呼び出し
  dept_comparison_module_server(
    id = "dept_comparison_module",
    processed_data_now = current_year_data$get_processed_data, # reactiveをそのまま渡す
    processed_data_past = previous_year_data$get_processed_data
  )
  
  # 回帰分析モジュールを呼び出し
  analysis_regression_module_server(
    id = "regression_module",
    processed_current_year_data = current_year_data$get_processed_data,
    processed_previous_year_data = previous_year_data$get_processed_data
  )
}


# --- アプリケーションの実行 ---
shinyApp(ui, server)