#' Create an Adjacency Matrix
#'
#' This function turns edgelists into Adjacency Matrices.
#'
#' @export

create_adj_matrix <- function (edge_data,panel) 
{
  edge <- edge_data %>% 
    mutate_all(as.numeric) %>% 
    filter(to %in% pull(panel,which(grepl("external",colnames(panel),ignore.case = T))))  %>%
    mutate(tie = 1) %>% 
    mutate_if(~any(is.character(.)), ~as.numeric(.))
  
  empty_edge <- expand_grid(x =pull(panel,which(grepl("external",colnames(panel),ignore.case = T))), 
                            y = pull(panel,which(grepl("external",colnames(panel),ignore.case = T))))
  empty_edge %<>% mutate(tie = 0)
  empty_net <- anti_join(empty_edge, edge, by = c(x = "from", 
                                                  y = "to"))
  temp <- edge %>% full_join(empty_net, by = c(from = "x", 
                                               to = "y", tie = "tie")) %>% mutate(tie = case_when(from == 
                                                                                                    to ~ NA_real_, TRUE ~ tie))
  adj <- temp %>% arrange(from, to) %>% pivot_wider(names_from = to, 
                                                    values_from = tie) %>% column_to_rownames("from")
  return(adj)
}
