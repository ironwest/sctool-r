# 必要なライブラリを読み込む
library(readr)
library(dplyr)
library(validate)
library(rmarkdown)
library(flextable)

# 0. データのマッピングはmapperappで実施。

# 1. データの読み込み(1．と2.は、01_datavalidation.Rで将来的に処理)
data_path <- "data/processed_data.csv"
if (!file.exists(data_path)) {
  stop("データファイルが見つかりません: ", data_path)
}


source("setting/column_specification.R")
best_encoding <- guess_encoding(data_path)$encoding[1]

stress_data <- read_csv(data_path, col_types = colsettype, locale = locale(encoding=best_encoding))




# 2. データ検証
rules <- validator(
  !is.na(empid),
  nchar(name) > 0
)

confrontation <- confront(stress_data, rules)

# 検証が失敗した場合は停止
if (any(summary(confrontation)$fails > 0)) {
  print("データ検証に失敗しました。処理を中断します。")
  print(summary(confrontation))
  stop()
}
print("データ検証成功。レポート生成を開始します。")

# 3. 出力ディレクトリの作成
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

#個々人のデータから表示するものを作成する-------------------------------------
benchd <- read_csv("setting/table11.csv") |> 
  filter(qtype == "BJSQ") |> 
  filter(sheet == "全体")

d <- stress_data |> 
  mutate(age = as.character(age))
i <- 1
#対象者の表
aempid <- d$empid[i]
aname  <- d$name[i]
aage   <- d$age[i]
asex   <- d$gender[i]
adept1 <- d$dept1[i]
adept2 <- d$dept2[i]
adept  <- str_c(adept1, adept2, sep=" ")

if(FALSE){
  adept <- "スーパーセールスアドバイザー部隊 カスタマーサクセスサポートコンサルテーション部"
  aname <- "アブラヒム ジェイストコボビッチ ザ サード"
  aempid <- "00000008"
}

# header
header_data <- tribble(
  ~v1       , ~v2   , ~v3   , ~v4  , ~v5  , ~v6   ,
  "氏名"    , aname ,""     ,""   , ""    ,""     ,
  "所属"    , adept ,""     ,""   , ""    ,""     ,
  "社員番号", aempid,"年齢", aage , "性別", asex  ,
)

flextable(header_data) |> 
  delete_part(part="header") |> 
  theme_box() |> 
  flextable::merge_at(i = 1, j = 2:6) |> 
  flextable::merge_at(i = 2, j = 2:6) |> 
  flextable::width(j=1,2,"cm") |> 
  flextable::width(j=2,3,"cm") |> 
  flextable::width(j=3,1.5,"cm") |> 
  flextable::width(j=4,2,"cm") |> 
  flextable::width(j=5,1.5,"cm") |> 
  flextable::width(j=6,2,"cm")
  
# overall result
colnames(d)

d$is_hs
d$demand
d$control
d$boss_support
d$fellow_support

#57問のデータの表示

#80問ある場合の表示

# 4. パラメータ化レポートのレンダリングループ
for (i in 1:nrow(stress_data)) {
  # 現在の行のデータを取得
  params_list <- as.list(stress_data[i, ])
  
  # 出力ファイル名を生成
  output_filename <- paste0(
    "StressCheck_",
    params_list$employee_id,
    "_",
    format(Sys.Date(), "%Y%m%d"),
    ".pdf"
  )
  
  # レポートのレンダリング
  render(
    input = "report_template.Rmd",
    output_format = "pdf_document",
    output_file = output_filename,
    output_dir = output_dir,
    params = params_list,
    quiet = TRUE # レンダリング中のメッセージを抑制
  )
  
  cat(paste0(i, "/", nrow(stress_data), ": ", output_filename, " を生成しました。\n"))
}

print("すべてのレポート生成が完了しました。")