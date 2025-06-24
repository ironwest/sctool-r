calculate_hensati_hyou <- function(current_data, 
                                   hensati_data, 
                                   target_sheet, 
                                   group_vars,nbjsq, 
                                   nbjsqlabs, 
                                   target_gyousyu, 
                                   target_longorcross, precise = FALSE){
  
  #偏差値表を作成する
  hyou <- calculate_hensati(
    d = current_data, 
    hensati_data = hensati_data, 
    tgtsheet=target_sheet, 
    grp_vars = group_vars, 
    nbjsq = nbjsq, 
    nbjsqlabs=nbjsqlabs
  )
  
  if(precise){ #make mean and hensati column if precise is true
    hyou <- hyou |> 
      select(hensatigrp, all_of(group_vars), syakudo_japanese, hensati, average = value) |> 
      pivot_wider(id_cols = all_of(group_vars), names_from = syakudo_japanese, values_from = c(hensati, average))
      
    newcolnames <- str_remove(colnames(hyou), "hensati_" )
    hyou <- hyou |> setNames(newcolnames)
    
  }else{
    hyou <- hyou |> 
      select(hensatigrp, all_of(group_vars), syakudo_japanese, hensati) |> 
      pivot_wider(id_cols = all_of(group_vars), names_from = syakudo_japanese, values_from = hensati)
  }
  
  #高ストレス者の人数と割合を計算する
  
  anyna <- current_data |> 
    select(tempid,matches("q\\d+")) |> 
    pivot_longer(cols = !tempid) |> 
    group_by(tempid) |> 
    summarise(isanyna = sum(is.na(value)))
  
  current_data <- current_data |> left_join(anyna, by="tempid")
    
  hyouhs <- current_data |> 
    group_by(across(all_of(group_vars))) |> 
    summarise(
      `受検人数` = n(),
      `不完全回答人数` = sum(isanyna>0),
      `高ストレス者人数` = sum(is_hs, na.rm=TRUE),
      `高ストレス者割合` = `高ストレス者人数`/`受検人数`
    )
  
  hyou <- hyou |> left_join(hyouhs, by=group_vars)
  
  #総合健康リスクの計算
  hyouskrisk <- calculate_sougoukrisk(
    d = current_data,
    grp_vars = group_vars,
    tgtgyousyu = target_gyousyu,
    precise=TRUE
  )
  
  if(precise) {
    #do not select when precise is TRUE
    if(target_longorcross == "long"){
      hyouskrisk <- hyouskrisk |> mutate("総合健康リスク" = total_risk_long)
    }else if(target_longorcross == "cross"){
      hyouskrisk <- hyouskrisk |> mutate("総合健康リスク" = total_risk_cross)
    }
      
  }else{
    hyouskrisk <- hyouskrisk |> select(all_of(group_vars), matches(target_longorcross))  
    if(target_longorcross == "long"){
      hyouskrisk <- hyouskrisk |> rename("総合健康リスク" = total_risk_long)
    }else if(target_longorcross == "cross"){
      hyouskrisk <- hyouskrisk |> rename("総合健康リスク" = total_risk_cross)
    }
  }
  
  hyou <- hyou |> left_join(hyouskrisk, by=group_vars)
  
  return(hyou)
}
