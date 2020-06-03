##' A collection of useful functions
##'
##' This is an initial package development with my functions. The plan will be
##' to build on these and separate functions out into seperate documents.
##' Additionally, automated testing may be added at a later date.
##'
##' @title init_all_funs
##' @author Atanas Janackovski

##' @title help function for use in other consoles i.e., in vscode, radian, emacs, etc.
h <-
    function(...) {
        utils::help(..., help_type = "html")
}


##' @title create interactive python repl on load
py <-
    reticulate::repl_python

##' create df of descriptives
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
## use with dplyr::select or purr::keep
## https://drsimonj.svbtle.com/quick-plot-of-all-variables
qq_plots <-
    function(df) {
        multi_qq_plots <-
            df %>%
            tidyr::gather() %>%                  # Convert to key-value pairs
            ggplot(aes(sample = value)) +        # Plot the values
            facet_wrap(~ key, scales = "free") + # In separate panels
            stat_qq() + stat_qq_line()           # provide the qq_plots
        return(multi_qq_plots)
    }

##' @title create row means and percent missing of selected variables
##' @param data your data
##' @param new_name the prefix of the new variables as a string
##' @param ... variables to select as per dplyr::select
row_means_miss_perc <-
    function(data, new_name, ...){
        data <-
            data %>%
            dplyr::mutate(
                       ## mean of rows
                       UQ(paste(rlang::syms(c(new_name)), "mean", sep = "_")) :=
                           round(
                               base::rowMeans(select(., ...)),
                               digits = 2),
                       ## percent missing of rows
                       UQ(paste(rlang::syms(c(new_name)), "miss", sep = "_")) :=
                           base::rowMeans(is.na(select(., ...)))
                   )
        return(data)
    }
