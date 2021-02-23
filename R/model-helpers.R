
#' @noRd
.generate_col <- function(prefix, suffix) {
  tidyr::crossing(
    prefix = prefix,
    suffix = suffix
  ) %>% 
    tidyr::unite('col', prefix, suffix, remove = FALSE) %>% 
    dplyr::arrange(col)
}

#' @noRd
.get_cols_lst <- function(stem = get_valid_stems()) {
  .validate_stem(stem)
  stems <- get_valid_stems()
  suffixes <- .get_valid_suffixes()
  cols_suffix <-
    .generate_col(
      prefix = c('g', 'xg', 'estimated_followers_count', sprintf('%s_count_lag1', stems)),
      suffix = suffixes # .get_valid_suffixes(),
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        prefix,
        list(
          lab = ~dplyr::case_when(
            .x == 'g' ~ 'Goals',
            .x == 'xg' ~ 'xG',
            .x == 'estimated_followers_count' ~ 'Twitter Account Followers',
            TRUE ~ sprintf('Prior # of %ss', stringr::str_remove(.toupper1(.x), '_count_lag1'))
          )
        )
      ),
      dplyr::across(
        suffix,
        list(
          lab = ~dplyr::case_when(
            .x == 'a' ~ 'Away',
            .x == 'h' ~ 'Home'
          )
        )
      ),
      # lab = sprintf('%s %s', suffix_lab, prefix_lab)
      lab = sprintf('%s, %s', prefix_lab, suffix_lab)
    ) %>% 
    dplyr::select(-dplyr::matches('_lab$'))
      
  # NOT: Found that these time columns are among the least important, so taking them out.
  cols_time <-
    .generate_col(
      prefix = c('hour', 'wday'),
      suffix = c('x1', 'y1', 'x2', 'y2')
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        prefix,
        list(
          lab = ~dplyr::case_when(
            .x == 'hour' ~ 'Hour',
            .x == 'wday' ~ 'Weekday'
          )
        )
      ),
      dplyr::across(
        suffix,
        list(
          `1` = ~stringr::str_sub(.x, 1, -2),
          `2` = ~stringr::str_sub(.x, 2, 2)
        )
      ),
      dplyr::across(
        suffix_1,
        list(
          lab = ~dplyr::case_when(
            .x == 'x' ~ 'Sin',
            .x == 'y' ~ 'Cos'
          )
        )
      ),
      dplyr::across(
        suffix_2,
        list(
          lab = ~dplyr::case_when(
            .x == '1' ~ '1st',
            .x == '2' ~ '2nd'
          )
        )
      ),
      lab = sprintf('%s Order %s Term, %s', suffix_2_lab, suffix_1_lab, prefix_lab)
    ) %>% 
    dplyr::select(-dplyr::matches('_lab$'))
  
  cols_538 <-
    .generate_col(
      prefix = c('prob'),
      suffix = suffixes
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        prefix,
        list(
          lab = ~dplyr::case_when(
            # .x == 'spi' ~ 'SPI',
            .x == 'prob' ~ 'P(Win)',
            # .x == 'proj_score' ~ 'Projected Goals',
            # .x == 'importance' ~ 'Standings Importance',
            # .x == 'nsxg' ~ 'Non-Shot xG'
          )
        )
      ),
      dplyr::across(col, ~stringr::str_replace(.x, '_([ah])$', '_538_\\1')),
      dplyr::across(
        suffix,
        list(
          lab = ~dplyr::case_when(
            .x == 'a' ~ 'Away',
            .x == 'h' ~ 'Home'
          )
        )
      ),
      dplyr::across(suffix_lab, ~sprintf('538, %s', .x)),
      # lab = sprintf('%s %s', suffix_lab, prefix_lab)
      lab = sprintf('%s, %s', prefix_lab, suffix_lab)
    ) %>% 
    dplyr::select(-dplyr::matches('_lab$')) %>% 
    dplyr::add_row(col = 'probtie_538', lab = 'P(Draw), 538')
  cols_538
  
  cols_derived <-
    dplyr::tibble(
      col = c('xgd_h2a', 'gd_h2a', 'd_h2a'), # , 'd_agree_h2a'),
      lab = c('xG Difference', 'Goal Difference', 'xG-Goal Difference') # , 'xG-G Difference Matches Result')
    )
  cols_derived
  
  cols_lst <-
    list(
      col_y = sprintf('%s_count_log', stem),
      cols_id = 'status_id',
      # cols_id = 'idx',
      col_wt = 'wt',
      col_strata = 'created_at',
      cols_extra = c(
        'idx',
        'is_fresh',
        'status_id',
        'text',
        'created_at',
        'team_a',
        'team_h',
        cols_suffix$col,
        sprintf('%s_count', c('estimated_followers', stems))
      ),
      cols_x = c('estimated_followers_count', cols_suffix$col, cols_538$col, cols_derived$col),
      cols_x_names = c('xGPhilopher\'s # of Followers', cols_suffix$lab, cols_538$lab, cols_derived$lab)
    )
  cols_lst
}

#' List of columns specifying use in model.
#' 
#' @inheritParams do_fit
#' @export
get_cols_lst <- .get_cols_lst

#' @seealso \url{https://github.com/topepo/caret/blob/master/pkg/caret/R/createDataPartition.R}
#' @noRd
.create_folds <- function (y, k = 10, list = TRUE, returnTrain = FALSE) {
  if(class(y)[1] == "Surv") y <- y[,"time"]
  if(is.numeric(y)) {
    ## Group the numeric data based on their magnitudes
    ## and sample within those groups.
    
    ## When the number of samples is low, we may have
    ## issues further slicing the numeric data into
    ## groups. The number of groups will depend on the
    ## ratio of the number of folds to the sample size.
    ## At most, we will use quantiles. If the sample
    ## is too small, we just do regular unstratified
    ## CV
    cuts <- floor(length(y)/k)
    if(cuts < 2) cuts <- 2
    if(cuts > 5) cuts <- 5
    breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
    y <- cut(y, breaks, include.lowest = TRUE)
  }
  
  if(k < length(y)) {
    ## reset levels so that the possible levels and
    ## the levels in the vector are the same
    y <- factor(as.character(y))
    numInClass <- table(y)
    foldVector <- vector(mode = "integer", length(y))
    
    ## For each class, balance the fold allocation as far
    ## as possible, then resample the remainder.
    ## The final assignment of folds is also randomized.
    for(i in 1:length(numInClass)) {
      ## create a vector of integers from 1:k as many times as possible without
      ## going over the number of samples in the class. Note that if the number
      ## of samples in a class is less than k, nothing is produced here.
      min_reps <- numInClass[i] %/% k
      if(min_reps > 0) {
        spares <- numInClass[i] %% k
        seqVector <- rep(1:k, min_reps)
        ## add enough random integers to get  length(seqVector) == numInClass[i]
        if(spares > 0) seqVector <- c(seqVector, sample(1:k, spares))
        ## shuffle the integers for fold assignment and assign to this classes's data
        foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
      } else {
        ## Here there are less records in the class than unique folds so
        ## randomly sprinkle them into folds.
        foldVector[which(y == names(numInClass)[i])] <- sample(1:k, size = numInClass[i])
      }
    }
  } else foldVector <- seq(along = y)
  
  if(list) {
    out <- split(seq(along = y), foldVector)
    names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), sep = "")
    if(returnTrain) out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
  } else out <- foldVector
  out
}
