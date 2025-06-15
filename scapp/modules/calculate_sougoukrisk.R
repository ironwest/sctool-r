#総合健康リスク計算（北里大学24年度データを利用）
# 縦断解析
# p/1-p
# = exp (𝛽0 + 𝛽1 ∗ (仕事の量的負担) + 
#          𝛽2∗ (仕事のコントロール) + 
#          𝛽3∗ (上司の支援) + 
#          𝛽4 ∗ (同僚の支援) + 
#          𝛽5∗ (前年度高ストレス者割合) + 
#          𝛽6∗ (年代平均) + 
#          𝛽7 ∗ (女性比率）


#総合健康リスクは平均ではなく、合計点の部署ごとの計算を行うため、その計算を行う
#なお、ここでの計算にはすべて「逆転した点数の総和」を用いることに注意が必要

d <- read_csv("../demodata/processed_nbjsq_dummy_data1_alpha.csv")

sumscore <- d |> 
  select(tempid, matches("q(1|2|3|8|9|10|47|50|53|48|51|54)$")) |> 
  mutate(across(matches("q"), ~5-.)) |> 
  mutate(demand = q1+q2+q3,
         control = q8+q9+q10,
         boss_support = q47+q50+q53,
         fellow_support = q48+q51+q54) |> 
  select(tempid, demand, control, boss_support, fellow_support)

d <- d |> 
  left_join(sumscore, by="tempid")

sc_averages_for_risk <- base |> 
  left_join(sumscore, by="empid") |> 
  group_by(grp, syakudogrp) |> 
  summarise(mean_score = mean(sumscore)) |> 
  ungroup() |> 
  pivot_wider(id_cols = grp, names_from = syakudogrp, values_from = mean_score)


sc_averages_for_risk_overall <- base |> 
  left_join(sumscore, by="empid") |> 
  mutate(grp = "全体") |> 
  group_by(grp, syakudogrp) |> 
  summarise(mean_score = mean(sumscore)) |> 
  ungroup() |> 
  pivot_wider(id_cols = grp, names_from = syakudogrp, values_from = mean_score)

input_data <- sc_averages_for_risk
fixed_data <- read_csv("./riskcalc/fixed_data.csv")

#' 総合健康リスク計算の関数
#' 
#' @param input_data ストレスチェック結果のデータフレーム（sc_averages_for_risk)
#' @param fixed_data 業種別平均、係数データ（riskcalc/fixed_data.csvに収納)
#' @param industry_name 対象業種名
#' @return リスク結果計算を含むデータフレーム
calculate_all_risks <- function(input_data, fixed_data, industry_name = "全体"){
  
  input_data <- input_data |> 
    mutate(across(where(is.numeric), ~ round(.x,3)))
  
  avg_vals <- fixed_data |> filter(industry == industry_name, type=="avg")
  coef_vals <- fixed_data |> filter(industry == industry_name, type=="coef")
  
  if(nrow(avg_vals) == 0 | nrow(coef_vals)==0) stop("指定された業種がfixed_dataに見当たりません")
  
  results <- input_data |> 
    mutate(
      #Vol-Control (RiskA)
      risk_A = floor(
        pmin(
          exp(
            ((demand - avg_vals$demand) * coef_vals$demand) + ((control - avg_vals$control) * coef_vals$control)
          ) * 100, 350)),
      
      #Support (RiskB)
      risk_B = floor(
        pmin(
          exp(
            ((boss_support - avg_vals$boss) * coef_vals$boss) + ((fellow_support - avg_vals$fellow) * coef_vals$fellow)
          ) * 100, 350)),
      
      #Total Risk
      total_risk = floor(risk_A * risk_B / 100),
      
      risk_A_old = floor(pmin(exp(((demand - 8.7) * 0.076) + (control - 8)*-0.089)*100, 350)),
      risk_B_old = floor(pmin(exp(((boss_support - 7.6) * -0.097) + (fellow_support - 8.1)*-0.097)*100, 350)),
      total_risk_old = floor(risk_A_old * risk_B_old / 100)
    )
  
  
  return(results)
  
  
}

total_risk_oa_hyou　 <- calculate_all_risks(sc_averages_for_risk_overall, fixed_data) |> 
  select(grp, total_risk)

total_risk_data <- calculate_all_risks(sc_averages_for_risk, fixed_data) |> 
  select(grp, total_risk)

total_risk_hyou <- bind_rows(total_risk_oa_hyou, total_risk_data) |> 
  mutate(name1 = "総合健康リスク") |> 
  select(grp,name1,value = total_risk)

# 未受診者人数を算出-------------
nas_hyou_grp <- nbjsq |> 
  group_by(empid) |> 
  summarise(nas = all(is.na(value))) |> 
  left_join(base, by=c("empid")) |> 
  group_by(grp) |> 
  summarise(nas = sum(nas))

nas_hyou_oa <- nbjsq |> 
  group_by(empid) |> 
  summarise(nas = all(is.na(value))) |> 
  left_join(base, by=c("empid")) |> 
  mutate(grp = "全体") |> 
  group_by(grp) |> 
  summarise(nas = sum(nas))

nas_hyou <- bind_rows(nas_hyou_oa, nas_hyou_grp) |> 
  mutate(name1 = "未受検者数(人)") |> 
  rename(value = nas) |> 
  relocate(grp,name1,value)


#ここまでの結果を結合する
fin <- bind_rows(
  hs_hyou,
  total_risk_hyou,
  harasment_hyou_data,
  hensati_hyou_data,
  nas_hyou
)

return(fin)

}

