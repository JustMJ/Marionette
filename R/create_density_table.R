#' Create a Density Table
#'
#' This function creates a density table
#'
#' @export
create_density_table<- function(adj,names) {


  results<- data.frame(Deps = names,
                       Internal = c(0,0,0,0,0,0),
                       External = c(0,0,0,0,0,0))


  for (i in seq_len(nrow(results))) {


    internal_ids<- panel %>%
      filter(Dept == results[i,1] )
    external_ids<- panel %>%
      filter(Dept != results[i,1] | is.na(Dept))


    internal_adj<- adj
    external_adj<- adj


    internal_adj %<>%
      filter(rownames(internal_adj) %in% internal_ids$ExternalDataReference)
    internal_adj %<>%
      select(one_of(rownames(internal_adj)))

    external_adj %<>%
      filter(rownames(external_adj) %in% external_ids$ExternalDataReference)
    external_adj %<>%
      select(one_of(rownames(internal_adj)))


    temp_internal<- internal_adj %>%
      summarise(density = sum(colSums(.,na.rm=T)))
    temp_external<- external_adj %>%
      summarise(density = sum(colSums(.,na.rm=T)))


    results$Internal[i]<-(temp_internal)
    results$External[i]<-(temp_external)
  }
  return(results)
}

