
#' @noRd
.get_x <-
  function(...,
           f = NULL,
           file = tempfile(),
           ext = 'rds',
           dir = getwd(),
           path = NULL,
           f_import = rio::import,
           f_export = rio::export,
           append = FALSE,
           export = TRUE,
           overwrite = FALSE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    
    if(!path_exists & append) {
      .display_warning('Can\'t append to `path = "{path}` since it doesn\t exist!')
    }
    
    # if(!path_exists & overwrite) {
    #   .display_warning('Can\'t overwrite `path = "{path}` since it doesn\t exist!')
    # }
    
    if(path_exists & !overwrite & !append) {
      .display_info('Importing from `path = "{path}"`.')
      return(f_import(path))
    }
    
    if(path_exists & append) {
      if(!export) {
        .display_warning('Setting `export = TRUE` since `append = TRUE` take higher priority.')
        export <- TRUE
      }
      .display_info('Importing from `path = "{path}"` for appending.')
      res_init <- f_import(path)
    }
    
    f_safe <- purrr::safely(f, otherwise = NULL)
    res <- f_safe(...)
    if (is.null(res)) {
      .display_error('Something went wrong with function call `f`!')
    } else {
      res <- res$result
    }
    
    if(append) {
      res <- list(res_init, res) %>% map(rbind)
    }
    
    if(!export) {
      return(res)
    }
    
    dir <- dirname(path)
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    f_export(res, path)
    .display_info('Exported to `path = "{path}"`.')
    res
    
  }
