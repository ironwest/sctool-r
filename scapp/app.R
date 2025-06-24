# app.R

library(shiny)
library(shinydashboard)
library(jsonlite)
library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(reactable)
library(shinycssloaders)
library(ggplot2)
library(broom)
library(openxlsx2)
library(showtext) 

# モジュールUI/サーバーと、各種ヘルパー関数を読み込む
source("modules/wizard_module.R")
source("modules/analysis_table_module.R")
source("modules/dept_comparison_module.R")
source("modules/logistic_regression_module.R")

# _moduleで利用する関数と設定値
source("modules/calculate_scores.R")
source("modules/calculate_hensati.R")
source("modules/calculate_sougoukrisk.R")
source("modules/calculate_hensati_hyou.R")

source("modules/setting_hensati_hyou.R")
source("modules/setting_gh_analysis.R") #ghsetting
source("modules/setting_bench_mapper.R")

source("modules/make_xx_result.R")
source("modules/make_excel_report.R")

# `%||%` 演算子 (NULL の場合にデフォルト値を返すヘルパー)
`%||%` <- function(a, b) if (!is.null(a)) a else b

#値マッピング用で利用する質問番号の順番に質問の文章が含まれるベクトル
qtext <- read_csv("nbjsq_question_text.csv") |> dplyr::pull(qtext)

# --- アプリケーションUIの定義 ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ストレスチェック集団分析"),
  dashboardSidebar(
    sidebarMenu(
      id = "main_tabs",
      menuItem("はじめに", tabName = "welcome", icon = icon("info-circle")),
      menuItem("データ設定", tabName = "data_setup", icon = icon("cogs"),
               menuSubItem("今年度データ設定", tabName = "current_year_setup"),
               menuSubItem("昨年度データ設定", tabName = "previous_year_setup")
      ),
      menuItem("分析", tabName = "analysis", icon = icon("chart-bar"),
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
              p("左のメニューから「データ設定」を選び、分析したいデータを準備してください。")
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


# --- 3. アプリケーションサーバーの定義 ---
server <- function(input, output, session) {
  
  # --- モジュールサーバーの呼び出しとデータ連携 ---
  
  # データ設定ウィザードモジュールを呼び出し
  current_year_data <- wizard_module_server("current_year_wizard", year_label = "今年度")
  previous_year_data <- wizard_module_server("previous_year_wizard", year_label = "昨年度")
  
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


# --- 4. アプリケーションの実行 ---
shinyApp(ui, server)