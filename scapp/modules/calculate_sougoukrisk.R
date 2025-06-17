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

# calculate_kenkourisk(
#   d = current_data,
#   grp_vars = group_vars,
#   tgtgyousyu = "全産業"
# )


# d <- read_csv("../demodata/processed_nbjsq_dummy_data1_alpha.csv")
# grp_vars <- c("dept1","dept2")
# tgtgyousyu <- "全産業"

#総合健康リスクを計算する関数
calculate_sougoukrisk <- function(d, grp_vars, tgtgyousyu,precise=FALSE){
  #設定の読み込み
  risk_calc_setting <- read_csv("../modules/risk_coefficients.csv")
  
  #指定した業種の係数と、平均値を取得する
  risk_calc_setting <- risk_calc_setting |> 
    filter(gyousyu == tgtgyousyu) |> 
    filter(!is.na(avg)) |> 
    pivot_wider(id_cols = c(gyousyu, coefname, avg), names_from = type, values_from = coef)
  
  avg <- setNames(risk_calc_setting$avg, risk_calc_setting$coefname)
  coeflong <- setNames(risk_calc_setting$long, risk_calc_setting$coefname)
  coefcross <- setNames(risk_calc_setting$cross, risk_calc_setting$coefname)
  
  #総合健康リスクを縦断、横断、旧リスクで計算する
  sougou_kenkou_risk <- d |>
    select(all_of(grp_vars),demand, control, boss_support, fellow_support) |> 
    group_by(across(all_of(grp_vars))) |>
    summarise(
      demand = mean(demand, na.rm=TRUE), 
      control = mean(control, na.rm=TRUE), 
      boss_support = mean(boss_support, na.rm=TRUE), 
      fellow_support = mean(fellow_support, na.rm=TRUE)
    ) |> 
    mutate(
      #longitudinal kenkou risk:
      #Vol-Control (RiskA)
      risk_A_long = floor(
        pmin(
          exp(
            ((demand - avg["demand"]) * coeflong["demand"]) + ((control - avg["control"]) * coeflong["control"])
          ) * 100, 350)),
      
      #Support (RiskB)
      risk_B_long = floor(
        pmin(
          exp(
            ((boss_support - avg["boss_support"]) * coeflong["boss_support"]) + ((fellow_support - avg["fellow_support"]) * coeflong["fellow_support"])
          ) * 100, 350)),
      
      #Total risk
      total_risk_long = floor(risk_A_long * risk_B_long / 100),
      
      #crosssectional kenkou risk:
      #VOl-control (RiskA)
      risk_A_cross = floor(
        pmin(
          exp(
            ((demand - avg["demand"]) * coefcross["demand"]) + ((control - avg["control"]) * coefcross["control"])
          ) * 100, 350)),
      
      #Support (RiskB)
      risk_B_cross = floor(
        pmin(
          exp(
            ((boss_support - avg["boss_support"]) * coefcross["boss_support"]) + ((fellow_support - avg["fellow_support"]) * coefcross["fellow_support"])
          ) * 100, 350)),
      
      #Total risk
      total_risk_cross = floor(risk_A_cross * risk_B_cross / 100),
      
      risk_A_old = floor(pmin(exp(((demand - 8.7) * 0.076) + (control - 8)*-0.089)*100, 350)),
      risk_B_old = floor(pmin(exp(((boss_support - 7.6) * -0.097) + (fellow_support - 8.1)*-0.097)*100, 350)),
      total_risk_old = floor(risk_A_old * risk_B_old / 100)
    )
    
  if(precise){
    sougou_kenkou_risk
  }else{
    sougou_kenkou_risk <- sougou_kenkou_risk |> select(all_of(grp_vars),matches("total"))
  }
    
  
  return(sougou_kenkou_risk)
}















