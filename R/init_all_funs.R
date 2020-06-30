##' A collection of useful functions
##'
##' This is an initial package development with my functions. The plan will be
##' to build on these and separate functions out into seperate documents.
##' Additionally, automated testing may be added at a later date.
##'
##' @author Atanas Janackovski





##' @title html package help function
##' @param ... usually `library = "package-name"`
##' @export
ajh <-
    function(...) {
        utils::help(...,
                    help_type = "html")
    }


# ##' @title create interactive python repl on load
# py <-
#     reticulate::repl_python()

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
##' @param df your data
##' @details use with dplyr::select or purr::keep
##' @details https://drsimonj.svbtle.com/quick-plot-of-all-variables
##' @export
qq_plots <-
    function(df) {
        multi_qq_plots <-
            df %>%
            tidyr::gather() %>%                    # Convert to key-value pairs
            ggplot2::ggplot(aes(sample = value)) + # Plot the values
            ggplot2::facet_wrap(                   # In separate panels
                         ~ key,
                         scales = "free") +
            ggplot2::stat_qq() +                   # provide qq_plots
            ggplot2::stat_qq_line()                # provide qq_line
        return(multi_qq_plots)
    }

##' @title create prorated means and percent missing of selected variables
##' @param data your data
##' @param new_name the prefix of the new variables as a string
##' @param ... variables to select as per dplyr::select
##' @export
pro_means_miss <-
    function(data, new_name, ...){
        data <-
            data %>%
            dplyr::mutate(
                       ## mean of rows
                       UQ(paste(rlang::syms(c(new_name)), "mean", sep = "_")) :=
                           round(
                               base::rowMeans(dplyr::select(., ...)),
                               digits = 2),
                       ## percent missing of rows
                       UQ(paste(rlang::syms(c(new_name)), "miss", sep = "_")) :=
                           base::rowMeans(is.na(dplyr::select(., ...)))
                   )
        return(data)
    }


##' @title multiple boxplots with outliers
##' @param data your data
##' @param melt_var var to melt by
##' @param incl requires regex string of var names to include
##' @param excl requires regex string of var names to exclude
##' @export
outlier_boxplots <-
    function(data, melt_var, incl = incl, excl = excl){
        is_outlier <- function(x, na.rm = TRUE) {
            return(x < quantile(x, 0.25, na.rm = na.rm) - 1.5 *
                   IQR(x, na.rm = na.rm) |
                   x > quantile(x, 0.75, na.rm = na.rm) + 1.5 *
                   IQR(x, na.rm = na.rm))
        }
        outlier_boxplots <-
            data %>%
            reshape2::melt(id.var = melt_var) %>%
            dplyr:: rename(id_var = 1) %>%
            dplyr::group_by(variable) %>% #
            dplyr::mutate(outlier =
                              dplyr::if_else(
                                  is_outlier(value),
                                  id_var,
                                  as.character(NA))) %>%
            dplyr::ungroup() %>%
            ## filer to select variables
            dplyr::filter(
                ## variables to include
                stringr::str_detect(
                    variable,
                    incl
                    ## "^dep|^str|^tb|^acss|^mssi|mean$"
                ) &
                    ## variables to exclude
                    !stringr::str_detect(
                        variable,
                        excl
                        ## "sum|post_scrn"
                    )
            ) %>%
            ggplot2::ggplot(aes(x = variable, y = value)) + #
            ggplot2::facet_wrap(~ variable, scales = "free") +
            ggplot2::geom_boxplot() +
            ggplot2::geom_text(
                aes(label = outlier),
                na.rm = TRUE,
                hjust = -0.3,
                size = 2,
                position = position_jitter()
            )
        return(outlier_boxplots)
    }
