library(shiny)
library(shinydashboard)

#UIコンポーネント---------
# (ここにUI要素の定義を追加していくことができます)

#ダッシュボードの骨格-------------------

##ダッシュボードヘッダー---------
header <- dashboardHeader(title = "集団分析ツール") # タイトルを追加すると良いでしょう

##ダッシュボードサイドバー------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs", # タブのIDを設定しておくと便利です
    menuItem("今年度", tabName = "current_year_top", icon = icon("calendar-alt"),
             menuSubItem("ファイルの読み込み", tabName = "current_year_upload", icon = icon("file-arrow-up")),
             menuSubItem("値の対応設定", tabName = "current_year_mapping", icon = icon("cogs"))
    ),
    menuItem("昨年度", tabName = "previous_year_top", icon = icon("calendar-check"),
             menuSubItem("ファイルの読み込み", tabName = "previous_year_upload", icon = icon("file-arrow-up")),
             menuSubItem("値の対応設定", tabName = "previous_year_mapping", icon = icon("cogs"))
    ),
    menuItem("分析", icon = icon("chart-pie"), # 「分析」自体がページを持たないなら tabName は不要
             # もし「分析」自体も表示するページがあるなら tabName = "analysis_top" を設定
             menuItem("まとめ", icon = icon("layer-group"), # 「まとめ」を menuItem に変更
                      # 「まとめ」自体がページを持たないなら tabName は不要
                      # もし「まとめ」自体も表示するページがあるなら tabName = "analysis_summary_top" などを設定
                      menuSubItem("職場(階層1)別集団解析", tabName = "summary_workplace1", icon = icon("sitemap")),
                      menuSubItem("職場(階層2)別集団解析", tabName = "summary_workplace2", icon = icon("sitemap")),
                      menuSubItem("年齢‐性別別集団解析", tabName = "summary_age_gender", icon = icon("users"))
             ), # menuItem("まとめ") の終わり
             menuSubItem("アウトカム", tabName = "analysis_outcome", icon = icon("bullseye")),
             menuSubItem("仕事の負担", tabName = "analysis_workload", icon = icon("briefcase")),
             menuSubItem("個人の資源", tabName = "analysis_individual_resources", icon = icon("user-shield")),
             menuSubItem("部署の資源", tabName = "analysis_department_resources", icon = icon("people-roof")),
             menuSubItem("事業場の資源", tabName = "analysis_site_resources", icon = icon("industry"))
    ) # menuItem("分析") の終わり
  )
)

##ダッシュボード本体------------------------
# (body部分は変更なし、ただしtabNameの整合性を確認してください)
body <- dashboardBody(
  tabItems(
    # 今年度
    tabItem(tabName = "current_year_upload", h2("今年度 - ファイルの読み込み")),
    tabItem(tabName = "current_year_mapping", h2("今年度 - 値の対応設定")),
    # 昨年度
    tabItem(tabName = "previous_year_upload", h2("昨年度 - ファイルの読み込み")),
    tabItem(tabName = "previous_year_mapping", h2("昨年度 - 値の対応設定")),
    # 分析 - まとめ
    # もし「まとめ」自体にページを作るなら、対応するtabItemを追加
    #例: tabItem(tabName = "analysis_summary_top", h2("分析 - まとめ概要")), 
    tabItem(tabName = "summary_workplace1", h2("分析 - まとめ - 職場(階層1)別集団解析")),
    tabItem(tabName = "summary_workplace2", h2("分析 - まとめ - 職場(階層2)別集団解析")),
    tabItem(tabName = "summary_age_gender", h2("分析 - まとめ - 年齢‐性別別集団解析")),
    # 分析 - その他
    tabItem(tabName = "analysis_outcome", h2("分析 - アウトカム")),
    tabItem(tabName = "analysis_workload", h2("分析 - 仕事の負担")),
    tabItem(tabName = "analysis_individual_resources", h2("分析 - 個人の資源")),
    tabItem(tabName = "analysis_department_resources", h2("分析 - 部署の資源")),
    tabItem(tabName = "analysis_site_resources", h2("分析 - 事業場の資源"))
  )
)

#UI----------------------
dashboardPage(header, sidebar, body)