#block <- list(block1 ="农村统计年鉴", block2="生产条件",
#              block3 ="农业机械",
#              block4= c("农业机械总动力","大中型拖拉机","小型拖拉机",
#                        "大中型拖拉机配套农具","小型拖拉机配套农具",
#                        "农用排灌电动机","农用排灌柴油机",
#                        "农用水泵", "联合收获机", 
#                        "机动脱粒机", "节水灌溉类机械"))


#dir_variables <- "data-proc/basic-vars-2019-8-3.xlsx"
#vars_set <- read.xlsx(dir_variables) %>%
#  mutate(variables= str_c(block1, block2, block3, block4, sep = "_"))


get.vars <- function(data,block, what){
  vars <- data %>%
    filter(chn_block1 %in% block$block1, 
           chn_block2 %in% block$block2,
           chn_block3 %in% block$block3, 
           chn_block4 %in% block$block4) %>%
    select(one_of(what)) %>%
    unlist()
  return(vars)
}