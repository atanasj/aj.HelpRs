##' @title import some global variables
##' @import utils
utils::globalVariables(".")

##' @title html package help function
##' @param ... usually `library = "package-name"`
##' @export
aj_h <-
  function(...) {
    utils::help(package = ...,
                help_type = "html")
  }


##' @title create interactive python repl on load
##' @param aj_py launch interactive python repl
aj_py <-
  function(){
    reticulate::repl_python()
  }

##' @title create df of descriptives
##' @param df your data
##' @param ... accepts arguments as per `psych::describe`
##' @export
desc_df <-
  function(df, ...) {
    ## view outliers
    descriptives <-
      psych::describe(df, ...)
    ## get the outlier names
    variable_names <-
      tibble::as_tibble(names(df))
    ## bind names for variables
    aj_desc_df <-
      dplyr::bind_cols(variable_names, descriptives)
    ## rename first column
    dplyr::rename(aj_desc_df, var_name = 1)
    return(aj_desc_df)
  }

##' @title create df of descriptives by group
##' @importFrom magrittr %>%
##' @param df your data
##' @param grp var to group by
##' @param ... accepts arguments used as per `pscyh::describeBy`
##' @export
descBy_df <-
  function(df, grp, ...) {
    ## view outliers
    descriptives <-
      psych::describeBy(df,
                        as.factor(grp),
                        mat = TRUE,
                        digits = 2,
                        ...)
    ## get the outlier names
    variable_names <-
      tibble::as_tibble(names(df))
    variable_names <-
      tibble::rowid_to_column(variable_names, "vars")
    ## bind names for variables
    aj_descBy_df <-
      dplyr::full_join(variable_names,
                       descriptives,
                       by = "vars")
    ## rename first column
    aj_descBy_df <-
      aj_descBy_df %>%
      dplyr::select(-c(1, 3)) %>%
      dplyr::rename(var_name = 1,
                    group = 2)
    ## dplyr::select(aj_descBy_df, !3)
    return(aj_descBy_df)
  }

##' @title create numerous qq_plots of selected variables
##' @importFrom magrittr %>%
##' @importFrom rlang .data
##' @import ggplot2
##' @param df your data
##' @details use with dplyr::select or purr::keep
##' @details https://drsimonj.svbtle.com/quick-plot-of-all-variables
##' @export
qq_plots <-
  function(df) {
    multi_qq_plots <-
      df %>%
      tidyr::gather() %>%                   # Convert to key-value pairs
      ggplot2::ggplot(aes(
                 sample = .data$value)) + # Plot the values
      ggplot2::facet_wrap(                  # In separate panels
                 ~ .data$key,
                 scales = "free") +
      ggplot2::stat_qq() +                  # provide qq_plots
      ggplot2::stat_qq_line()               # provide qq_line
    return(multi_qq_plots)
  }

##' @title create prorated means and percent missing of selected variables
##' @importFrom magrittr %>%
##' @importFrom rlang := UQ .data
##' @importFrom dplyr select
##' @param data your data
##' @param new_name the prefix of the new variables as a string
##' @param ... variables to select as per dplyr::select
##' @export
pro_mean_miss <-
  function(data, new_name, ...){
    data <-
      data %>%
      dplyr::mutate(
               ## mean of rows
               UQ(paste(rlang::syms(c(new_name)), "mean", sep = "_")) :=
                 round(
                   base::rowMeans(select(., ...), na.rm = TRUE) *
                   ## scaled up to base measures
                   base::rowSums(!is.na(select(., ...))),
                   digits = 2),
               ## percent missing of rows
               UQ(paste(rlang::syms(c(new_name)), "miss", sep = "_")) :=
                 base::rowMeans(is.na(dplyr::select(., ...)))
             )
    return(data)
  }


##' @title multiple boxplots with outliers
##' @importFrom magrittr %>%
##' @importFrom stats IQR quantile
##' @importFrom rlang .data
##' @import reshape2
##' @param data your data
##' @param melt_var var to melt by e.g, the participant ID
##' @param incl requires regex string of var names to include
##' @param excl requires regex string of var names to exclude
##' @export
outlier_boxplots <-
  function(data, melt_var, incl = incl, excl = excl){
    is_outlier <- function(x, na.rm = TRUE) {
      return(x < quantile(.data$x, 0.25, na.rm = na.rm) - 1.5 *
             IQR(.data$x, na.rm = na.rm) |
             x > quantile(.data$x, 0.75, na.rm = na.rm) + 1.5 *
             IQR(.data$x, na.rm = na.rm))
    }
    outlier_boxplots <-
      data %>%
      reshape2::melt(id.var = melt_var) %>%
      dplyr::rename(id_var = 1) %>%
      dplyr::group_by(.data$variable) %>% #
      dplyr::mutate(outlier =
                      dplyr::if_else(
                               is_outlier(.data$value),
                               .data$id_var,
                               as.character(NA))) %>%
      dplyr::ungroup() %>%
      ## filer to select variables
      dplyr::filter(
               ## variables to include
               stringr::str_detect(
                          .data$variable,
                          incl
                          ## "^dep|^str|^tb|^acss|^mssi|mean$"
                        ) &
               ## variables to exclude
               !stringr::str_detect(
                           .data$variable,
                           excl
                           ## "sum|post_scrn"
                         )
             ) %>%
      ggplot2::ggplot(aes(x = .data$variable, y = .data$value)) + #
      ggplot2::facet_wrap(~ .data$variable, scales = "free") +
      ggplot2::geom_boxplot() +
      ggplot2::geom_text(
                 aes(label = .data$outlier),
                 na.rm = TRUE,
                 hjust = -0.3,
                 size = 2,
                 position = position_jitter()
               )
    return(outlier_boxplots)
  }


##' @title function to collapse duplicates entries
##' @param df this function should be run on a dataframe or matrix
##' @importFrom magrittr %>%
##' @examples
##' ## collapse duplicates and recode any with problems
##' \dontrun{
##' sp_change <-
##'     sp_change %>%
##'     group_by(uci) %>%
##'     summarise_all(coalesce_by_column) %>%
##'     ungroup()
##' }
##' @export
coalesce_by_column <-
  function(df) {
    return(dplyr::coalesce(!!! as.list(df)))
  }
