# function for count white spaces
countWhiteSpaces <- function(x) attr(gregexpr("(?<=[^#])[#]+(?=[^#])", x, perl = TRUE)[[1]], "match.length")

# function for seek the whitespace style
###install.packages("mgsub")
###library("mgsub")
str_seek <- function(strs, style=n_style,rep=n_rep){
  out <- mgsub::mgsub(strs, pattern = style, replacement =rep,perl = F)
  return(out)
}


# function for calculate vector modes
getModes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


# first occurrence [upper talbe of two part]
get_rawpage <- function(web_url,x_tbl){
  # the html source file declares gb2312
  tbl_raw <- read_html(web_url,encoding = "UTF-8") %>%
    html_nodes(xpath = x_tbl) %>%
    html_table(., fill=T, trim=T) %>%
    .[[1]] %>%
    # substitute all the white space with #
    mutate(X1=gsub("\u00A0", "#", X1)) 
  
  # detect the start and end row
  first_end <- which(str_detect(tbl_raw$X1,"北京|新疆"))
  range <- (first_end[1]-1):(first_end[2])
  
  tbl_dt <- tbl_raw %>%
    # delete rows unnecessary
    .[range,] %>%
    as_tibble()
  
  return(tbl_dt)
}

# first occurrence [upper talbe of two part]
get_rawpage_lower <- function(web_url,x_tbl=path_tbl){
  # the html source file declares gb2312
  tbl_raw <- read_html(web_url,encoding = "gb2312") %>%
    html_nodes(xpath = x_tbl) %>%
    html_table(., fill=T, trim=T) %>%
    .[[1]] %>%
    # substitute all the white space with #
    mutate(X1=gsub("\u00A0", "#", X1)) 
  
  # detect the start and end row
  first_end <- which(str_detect(tbl_raw$X1,"北京|新疆"))
  range <- (first_end[3]-1):first_end[4]
  
  tbl_dt <- tbl_raw %>%
    # delete rows unnecessary
    .[range,] %>%
    as_tibble()
  
  return(tbl_dt)
}

# first occurrence [upper talbe of two part]
get_rawpage_spc <- function(web_url,x_tbl=path_tbl, spc){
  # the html source file declares gb2312
  tbl_raw <- read_html(web_url,encoding = "utf-8") %>%
    html_nodes(xpath = x_tbl) %>%
    html_table(., fill=T, trim=T) %>%
    .[[1]] %>%
    # substitute all the white space with #
    mutate(X1=gsub("\u00A0", "#", X1)) 
  
  # detect the start and end row
  first_end <- which(str_detect(tbl_raw$X1,"地方合计|新疆"))
  n_part <- length(first_end)/2
  
  if(spc > n_part){
    print(paste0("Error: specification of ",spc, "is larger than ", n_part))
  } else {
    range <- (first_end[(2*spc -1)]  ):first_end[2*spc] #  如有"全国"，则起始要减去1
  }
  
  tbl_dt <- tbl_raw %>%
    # delete rows unnecessary
    .[range,] %>%
    as_tibble()
  
  return(tbl_dt)
}


# check styles
tbl_check <- function(dt){
  tbl_dt <- dt %>%
    # match and replace all '-' after any chinese character
    mutate(value=gsub("(?<=[\\p{Han}])#{1}", ")#", value, perl = T)) %>% 
    mutate(n=map(.x = value, .f = countWhiteSpaces)) %>%
    mutate(len = lengths(n), 
           min=map(.x=n, .f=min), 
           max=map(.x=n, .f=max), 
           mode=map(.x = n, .f = getModes)) %>%
    mutate(value=gsub(")", "",value,fixed = T))
}

# for the special case when html table contains "38.1112.3" cell
#### so we know that we should split it into "38.11" and "12.3" 
#### html case in file:///D:/github/agri-base/data/v2/01-gdp-dq/html-source01/2008.html
tbl_check_robust <- function(dt){
  tbl_dt <- dt %>%
    # robust regex match and group replace
    mutate(value=gsub("(?<=\\.)(\\d{2})(\\d{2})(?=\\.)", "\\1###\\2", value, perl = T)) %>%
    # match and replace all '-' after any chinese character
    mutate(value=gsub("(?<=[\\p{Han}])#{1}", ")#", value, perl = T)) %>% 
    mutate(n=map(.x = value, .f = countWhiteSpaces)) %>%
    mutate(len = lengths(n), 
           min=map(.x=n, .f=min), 
           max=map(.x=n, .f=max), 
           mode=map(.x = n, .f = getModes)) %>%
    mutate(value=gsub(")", "",value,fixed = T)) %>%
    mutate(value=gsub(")", "",value,fixed = T))
}


# identify numbers of columns 
get_cols <- function(dt){
  row_perfect <-  which(dt$len==max(dt$len))[1]
  n_vars <- dt %>%
    #as_tibble() %>%
    .[row_perfect,1] %>%
    mutate(value=gsub("(#){1,50}", ";", value, perl = F)) %>%
    str_split(., ";") %>%
    unlist() %>%
    length()
  vars_eng <- str_c("V", 1:n_vars)
  return(vars_eng)
}


# obtain the table output
get_split <- function(dt, style=n_style, rep =n_rep,vars_eng=names_eng){
  len_max <- dt %>% .$len %>% unlist() %>% max()
  tbl_seek <- dt %>%
    mutate(str= if_else(
      len==len_max,gsub("(#){1,50}", ";", value, perl = F), # for full len
      mgsub::mgsub(value, pattern = style, 
                   replacement =rep,perl = F) # for other
    ) ) %>%
    select(str) %>%
    mutate(str=gsub("全国总计|总计|全国合计|地方合计", "全国", str, perl = F))  %>%
    mutate(str=gsub("T|s", "", str, perl = F))  %>% # handle character liers
    mutate(str=gsub("新驰", "新疆", str, perl = F))  %>% # handle character liers
    separate(str,into = vars_eng, sep = ";")  %>%
    mutate_at(all_of(vars_eng[-1]), .funs = as.numeric, digits=2)
}