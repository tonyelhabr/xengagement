
#' Wrapper function for importing and exporting
#' 
#' Wrapper that imports and exports result of a function to a file on disk.
#' @param f function to execute
#' @param ... Extra arguments to pass to `f`
#' @param dir Directory to use to generate `path` if `path` is not explicitly provided.
#' @param file File name (without extension) to generate `path` if `path` is not explicitly provided.
#' @param ext File extension to use to generate `path` if `path` is not explicitly provided.
#' @param path Path to export to.
#' @param f_import Function to import with if file exists and `overwrite = TRUE`.
#' @param f_export Function to export with if `export = TRUE` .
#' @param append Whether to append. Supersedes `export` and `overwrite`.
#' @param export Whether to export. Supersedes `overwrite.
#' @param overwrite Whether to overwrite.
do_get <-
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
      res <- list(res_init, res) %>% purrr::map(rbind)
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
