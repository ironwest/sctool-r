# global.R

# --- 1. パッケージの読み込み ---
library(shiny)
library(shinydashboard)
library(shinyjs) 
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

# --- 2. 日本語フォントの設定 ---
# アプリケーションの実行環境に応じて適切なフォントを指定
font_add_google("Noto Sans JP", "notosans-jp")
showtext_auto()

# --- 3. モジュールとヘルパー関数の読み込み ---
source("modules/wizard_module.R")
source("modules/analysis_table_module.R")
source("modules/dept_comparison_module.R")
source("modules/logistic_regression_module.R")

source("modules/calculate_scores.R")
source("modules/calculate_hensati.R")
source("modules/calculate_sougoukrisk.R")
source("modules/calculate_hensati_hyou.R")

source("modules/setting_hensati_hyou.R")
source("modules/setting_gh_analysis.R")
source("modules/setting_bench_mapper.R")

source("modules/make_xx_result.R")
source("modules/make_excel_report.R")

# --- 4. グローバルなオブジェクトと関数の定義 ---

# NULL の場合にデフォルト値を返すヘルパー演算子
`%||%` <- function(a, b) if (!is.null(a)) a else b

# 質問テキストの読み込み (エラーハンドリングを強化)
qtext_path <- "modules/nbjsq_question_text.csv"
if (file.exists(qtext_path)) {
  qtext <- read_csv(qtext_path, show_col_types = FALSE) |> dplyr::pull(qtext)
} else {
  qtext <- character(0)
  warning(paste("質問テキストファイルが見つかりません:", qtext_path))
}