# NBJSQのダミーデータ生成関数
generate_nbjsq_dummy_data <- function(N) {
  # N: 生成するダミーデータの人数
  
  # 1. ID
  id <- 1:N
  
  # 2. 年齢 (例: 20歳から60歳までの一様分布)
  age <- sample(20:60, N, replace = TRUE)
  
  # 3. 性別 (例: 1=男性, 2=女性、ほぼ半々と仮定)
  gender_code <- sample(1:2, N, replace = TRUE, prob = c(0.5, 0.5))
  gender <- factor(gender_code, levels = 1:2, labels = c("男性", "女性"))
  
  # 4. 部署（大分類）の例
  dept_major_options <- c("営業本部", "開発本部", "管理本部", "生産本部", "企画部","マーケット戦略部")
  department_major <- sample(dept_major_options, N, replace = TRUE)
  
  # 5. 部署（中分類）の例 (大分類に紐づく形で生成)
  department_minor <- character(N)
  for (i in 1:N) {
    if (department_major[i] == "営業本部") {
      department_minor[i] <- sample(c("国内営業第一部", "国内営業第二部", "海外営業部", "営業企画部"), 1)
    } else if (department_major[i] == "開発本部") {
      department_minor[i] <- sample(c("製品開発部", "技術研究部", "デザイン部"), 1)
    } else if (department_major[i] == "管理本部") {
      department_minor[i] <- sample(c("人事部", "総務部", "経理部", "法務部"), 1)
    } else if (department_major[i] == "生産本部") {
      department_minor[i] <- sample(c("生産管理部", "品質保証部", "製造第一課", "製造第二課"), 1)
    } else if (department_major[i] == "企画部") {
      department_minor[i] <- sample(c("経営企画課", "事業開発課", "広報IR課"), 1)
    } else if (department_major[i] == "マーケット戦略部") {
      department_minor[i] <- sample(c("UX開発部", "マーケット戦略1課", "マーケット戦略2課"), 1)
    } else {
      department_minor[i] <- "その他" # 念のため
    }
  }
  department_minor <- factor(department_minor)
  
  # 6. NBJSQ 57項目 (各項目1～4のランダムな値)
  # 各質問項目をQ1, Q2, ..., Q57とします。
  # 回答尺度は例として 1:そうだ, 2:まあそうだ, 3:ややちがう, 4:ちがう と仮定します。
  # 実際の調査票の回答の向き（点数が高いほどストレスが高い/低いなど）に応じて調整してください。
  nbjsq_items <- data.frame(matrix(NA, nrow = N, ncol = 80))
  for (i in 1:80) {
    # 各項目で回答の傾向を少し変えたい場合は、prob引数で調整可能
    # 例: sample(1:4, N, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4))
    nbjsq_items[, i] <- sample(c(letters[1:4]), N, replace = TRUE)
    colnames(nbjsq_items)[i] <- paste0("質問", i)
  }
  
  # 7. 全てのデータをデータフレームに結合
  dummy_data <- data.frame(
    ID = id,
    Age = age,
    Gender = gender,
    Department_Major = factor(department_major),
    Department_Minor = department_minor
  )
  dummy_data <- cbind(dummy_data, nbjsq_items)
  
  return(dummy_data)
}

# --- 関数の使用例 ---

# 3000人分のダミーデータを生成
nbjsq_data1 <- generate_nbjsq_dummy_data(3500)
nbjsq_data2 <- generate_nbjsq_dummy_data(3700)

# 生成されたデータの最初の数行を表示
print(head(nbjsq_data1))

# 生成されたデータの要約統計量を表示
# summary(nbjsq_data)

# 生成されたデータをCSVファイルとして保存する場合
write.csv(nbjsq_data1, "nbjsq_dummy_data1.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(nbjsq_data2, "nbjsq_dummy_data2.csv", row.names = FALSE, fileEncoding = "UTF-8")
