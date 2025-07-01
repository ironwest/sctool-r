# tests/testthat/test-calculate_scores.R

# テスト対象の関数を読み込む
# testthat v3以降では、テスト実行時にプロジェクトルートがカレントディレクトリになるため、
# パスはプロジェクトルートから記述します。
source("modules/calculate_scores.R")

# `calculate_stress_factors`という関数が存在すると仮定してテストを記述
test_that("ストレス要因尺度のスコア計算が正しく行われる", {
  
  # 1. テスト用の入力データを作成
  # 実際の列名やデータ構造に合わせてください
  input_df <- tibble::tibble(
    id = 1:2,
    # 「仕事の量的負担」に関する質問項目 (Q3, Q9, Q15)
    B_Q3  = c(1, 4), # 1:そうだ, 4:ちがう
    B_Q9  = c(2, 3),
    B_Q15 = c(1, 2),
    # 「仕事のコントロール」に関する質問項目 (Q5, Q11, Q17) - 逆転項目
    B_Q5  = c(4, 1), # 4:そうだ, 1:ちがう
    B_Q11 = c(3, 2),
    B_Q17 = c(4, 1)
  )
  
  # 2. テスト対象の関数を実行
  # この関数名はプロジェクトに合わせて変更してください
  result_df <- calculate_stress_factors(input_df)
  
  # 3. 結果を検証
  
  # a) 新しいスコア列が追加されていることを確認
  expect_true("仕事の量的負担" %in% names(result_df))
  expect_true("仕事のコントロール" %in% names(result_df))
  
  # b) スコアの計算結果が正しいことを確認
  # 量的負担: 1+2+1=4, 4+3+2=9
  expect_equal(result_df$`仕事の量的負担`, c(4, 9))
  
  # コントロール(逆転項目): (5-4)+(5-3)+(5-4)=1+2+1=4, (5-1)+(5-2)+(5-1)=4+3+4=11
  expect_equal(result_df$`仕事のコントロール`, c(4, 11))
})

test_that("入力データに必要な列がない場合にエラーが発生する", {
  
  # 必須列(B_Q3)が欠けたデータ
  invalid_df <- tibble::tibble(id = 1, B_Q9 = 2)
  
  # `stop()`などでエラーが投げられることを期待
  expect_error(calculate_stress_factors(invalid_df))
})

test_that("入力データにNAが含まれる場合の挙動を確認", {
    
  input_df_na <- tibble::tibble(
    id = 1,
    B_Q3  = c(1),
    B_Q9  = c(NA), # NAを含む
    B_Q15 = c(1)
  )
  
  result_df <- calculate_stress_factors(input_df_na)
  
  # 合計スコアがNAになることを期待 (na.rm=FALSEの場合)
  expect_true(is.na(result_df$`仕事の量的負担`[1]))
})
