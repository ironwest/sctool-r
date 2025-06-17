source("calculate_hensati.r")
source("calculate_scores.r")
source("calculate_sougoukrisk.r")

calculate_hensati_hyou <- function(current_data, hensati_data, target_sheet, group_vars,nbjsq, nbjsqlabs, target_gyousyu, target_longorcross){
  
  #偏差値表を作成する
  hyou <- calculate_hensati(
    d = current_data, 
    hensati_data = hensati_data, 
    tgtsheet=target_sheet, 
    grp_vars = group_vars, 
    nbjsq = nbjsq, 
    nbjsqlabs=nbjsqlabs, 
    ret = "hensati"
  )
  
  hyou <- hyou |>  #<<- を最後に<-に戻すこと（デバッグ中）
    select(hensatigrp, all_of(group_vars), syakudo_japanese, hensati) |> 
    pivot_wider(id_cols = all_of(group_vars), names_from = syakudo_japanese, values_from = hensati)
  
  
  #高ストレス者の人数と割合を計算する
  hyouhs <- current_data |> 
    group_by(across(all_of(group_vars))) |> 
    summarise(
      `受検人数` = n(),
      `不完全回答人数` = sum(is.na(is_hs)),
      `高ストレス者人数` = sum(is_hs, na.rm=TRUE),
      `高ストレス者割合` = `高ストレス者人数`/`受検人数`
    )
  
  hyou <- hyou |> left_join(hyouhs, by=group_vars)
  
  #総合健康リスクの計算
  hyouskrisk <- calculate_sougoukrisk(
    d = current_data,
    grp_vars = group_vars,
    tgtgyousyu = target_gyousyu
  )
  
  hyouskrisk <- hyouskrisk |> select(all_of(group_vars), matches(target_longorcross))
  
  if(target_longorcross == "long"){
    hyouskrisk <- hyouskrisk |> rename("総合健康リスク" = total_risk_long)
  }else if(target_longorcross == "cross"){
    hyouskrisk <- hyouskrisk |> rename("総合健康リスク" = total_risk_cross)
  }
  
  hyou <- hyou |> left_join(hyouskrisk, by=group_vars)
  
  return(hyou)
}
