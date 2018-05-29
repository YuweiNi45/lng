
#' create tables providing descriptive statistics
#'
#' Generic function to create tables providing descriptive statistics summary for overall and in different groups.
#'
#' @param dat   a data frame or environment (or object coercible by as.data.frame to a data frame) containing the variables to be summarized
#' @param y   a numeric vector of outcome variable of interest (default is NULL)
#' @param alternative   a character string specifying the alternative approaches, must be one of "overall" (default), "total" or "bygroup"
#' @param digits     the digits of the data values (default is 2)
#' @param ...   further arguments to be passed to or from methods
#'
#' @return  Generate descriptive statistical tables for different type of outcome variables of interest without missing value
#'
#' @references Hlavac Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.1. https://CRAN.R-project.org/package=stargazer
#'
#' @seealso \code{\link[stats]{anova}}
#'
#' \code{\link[stats]{kruskal.test}}
#'
#'
#'
#'@import NHANES
#'
#'@export
decrib <-
  function(dat,
           y = NULL,
           alternative = c("overall", "total", "bygroup"),
           digit = 2,
           ...) {
    alternative <- match.arg(alternative)
    #testing data
    #dat <- data2

    #y <- dat$TotChol

    ##get the names done for each colomn of the final table for different groups
    if (!is.null(y)) {
      y_length <- length(y)

      data_names <- colnames(dat)

      name_length <- length(data_names)

      result <- numeric(name_length)

      for (i in 1:name_length) {
        result[i] <- sum(y %in% factor(unlist(dat[data_names[i]])))
      }

      ncol_y <- which(result == y_length)

      y_group <- data_names[ncol_y]



      #data just includes the covariates

      dat_noy <- dat[,-ncol_y]

      #get the Total column done

      ###know which variable is continuous and which is categorical

      ncol_allx <- ncol(dat_noy)

      all_level <- numeric(ncol_allx)

      for (i in 1:ncol_allx) {
        if (!is.null(levels(unlist(dat_noy[, i])))) {
          all_level[i] <- length(levels(unlist(dat_noy[, i])))
        }
      }

      max_level <- max(all_level)

      ## if we only have continuous variables
      if ((sum(all_level > 0)) == 0) {
        result_tol_mec <- numeric(ncol_allx)

        result_tol_sd <- numeric(ncol_allx)

        con_names <- colnames(dat_noy)

        for (i in 1:ncol_allx) {
          result_tol_mec[i] <- mean(unlist(dat_noy[, i]))
          result_tol_sd[i] <- sd(unlist(dat_noy[, i]))
        }

        ##create the table for continuous variables' total
        out1 <- data.frame(Total = paste0(
          format(result_tol_mec, digits = digit),
          "+/-",
          format(result_tol_sd, digits = digit)
        ))

        colnames(out1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1) <- con_names

        out_totalnum <- out1

      }
      ## if we only have cateorical variables
      else if ((sum(all_level > 0)) == ncol_allx) {
        result_tol_count <-
          matrix(nrow = ncol_allx, ncol = max_level)
        result_tol_freq <-
          matrix(nrow = ncol_allx, ncol = max_level)

        cat_names <- colnames(dat_noy)

        cat_names_each <- character(0)

        for (i in 1:ncol_allx) {
          levels_cat <- levels(unlist(dat_noy[, i]))
          nlevel <- length(levels_cat)
          for (j in 1:nlevel) {
            cat_names_each <-
              c(cat_names_each,
                paste0(cat_names[i], ": ", levels_cat[j]))
            result_tol_count[i, j] <-
              length(which(unlist(dat_noy[, i]) == levels_cat[j]))
            result_tol_freq[i, j] <-
              length(which(unlist(dat_noy[, i]) == levels_cat[j])) / length(unlist(dat_noy[, i]))
          }
        }

        #get categorical varaibles ready

        result_tol_count1 <-
          result_tol_count[which(!cat_names == ""), ]

        result_tol_freq1 <-
          result_tol_freq[which(!cat_names == ""), ]

        cat_total_count <- numeric(0)

        cat_total_freq <- numeric(0)

        if (!is.null(nrow(result_tol_count1))) {
          for (i in 1:nrow(result_tol_count1)) {
            if (!is.null(ncol(result_tol_count1))) {
              for (j in 1:ncol(result_tol_count1)) {
                if (!is.na(result_tol_count1[i, j])) {
                  cat_total_count <- c(cat_total_count, result_tol_count1[i, j])
                  cat_total_freq <-
                    c(cat_total_freq, result_tol_freq1[i, j])
                }
              }
            }
            else {
              if (!is.na(result_tol_count1[i])) {
                cat_total_count <- c(cat_total_count, result_tol_count1[i])
                cat_total_freq <-
                  c(cat_total_freq, result_tol_freq1[i])
              }
            }

          }
        } else {
          if (!is.null(ncol(result_tol_count1))) {
            for (j in 1:ncol(result_tol_count1)) {
              if (!is.na(result_tol_count1[j])) {
                cat_total_count <- c(cat_total_count, result_tol_count1[j])
                cat_total_freq <-
                  c(cat_total_freq, result_tol_freq1[j])
              }
            }
          } else {
            cat_total_count <- result_tol_count1
            cat_total_freq <- result_tol_freq1
          }
        }

        ##create the table for categorical variables' total

        out1_1 <- data.frame(Total = paste0(
          format(cat_total_count, digits = digit),
          " (",
          format(cat_total_freq * 100, digits = digit),
          ")"
        ))

        colnames(out1_1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1_1) <- cat_names_each

        out_totalnum <- out1_1
      }
      ##we have both continuous and categorical variables
      else{
        result_tol_mec <- numeric(ncol_allx)

        result_tol_sd <- numeric(ncol_allx)

        result_tol_count <-
          matrix(nrow = ncol_allx, ncol = max_level)
        result_tol_freq <-
          matrix(nrow = ncol_allx, ncol = max_level)

        #as.character(unlist(dat_noy[, 1]))

        all_var_names <- colnames(dat_noy)

        con_names <- character(ncol_allx)

        cat_names <- character(ncol_allx)

        cat_names_each <- character(0)

        for (i in 1:ncol_allx) {
          if (all_level[i] == 0) {
            con_names[i] <- all_var_names[i]
            result_tol_mec[i] <- mean(unlist(dat_noy[, i]))
            result_tol_sd[i] <- sd(unlist(dat_noy[, i]))
          } else if (all_level[i] != 0) {
            levels_cat <- levels(unlist(dat_noy[, i]))
            nlevel <- length(levels_cat)
            cat_names[i] <- all_var_names[i]
            for (j in 1:nlevel) {
              cat_names_each <-
                c(cat_names_each,
                  paste0(cat_names[i], ": ", levels_cat[j]))
              result_tol_count[i, j] <-
                length(which(unlist(dat_noy[, i]) == levels_cat[j]))
              result_tol_freq[i, j] <-
                length(which(unlist(dat_noy[, i]) == levels_cat[j])) / length(unlist(dat_noy[, i]))
            }
          }

        }

        #get continuoius variables ready

        con_names1 <- con_names[-which(con_names == "")]

        result_tol_mec1 <-
          result_tol_mec[-which(result_tol_mec == 0)]

        result_tol_sd1 <- result_tol_sd[-which(result_tol_sd == 0)]

        ##create the table for continuous variables' total
        out1 <- data.frame(Total = paste0(
          format(result_tol_mec1, digits = digit),
          "+/-",
          format(result_tol_sd1, digits = digit)
        ))

        colnames(out1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1) <- con_names1


        #get categorical varaibles ready

        all_level1 <- all_level[-which(all_level == 0)]

        cat_names1 <- cat_names[-which(cat_names == "")]

        result_tol_count1 <-
          result_tol_count[which(!cat_names == ""), ]

        result_tol_freq1 <-
          result_tol_freq[which(!cat_names == ""), ]

        cat_total_count <- numeric(0)

        cat_total_freq <- numeric(0)

        if (!is.null(nrow(result_tol_count1))) {
          for (i in 1:nrow(result_tol_count1)) {
            if (!is.null(ncol(result_tol_count1))) {
              for (j in 1:ncol(result_tol_count1)) {
                if (!is.na(result_tol_count1[i, j])) {
                  cat_total_count <- c(cat_total_count, result_tol_count1[i, j])
                  cat_total_freq <-
                    c(cat_total_freq, result_tol_freq1[i, j])
                }
              }
            }
            else {
              if (!is.na(result_tol_count1[i])) {
                cat_total_count <- c(cat_total_count, result_tol_count1[i])
                cat_total_freq <-
                  c(cat_total_freq, result_tol_freq1[i])
              }
            }

          }
        } else {
          if (!is.null(ncol(result_tol_count1))) {
            for (j in 1:ncol(result_tol_count1)) {
              if (!is.na(result_tol_count1[j])) {
                cat_total_count <- c(cat_total_count, result_tol_count1[j])
                cat_total_freq <-
                  c(cat_total_freq, result_tol_freq1[j])
              }
            }
          } else {
            cat_total_count <- result_tol_count1
            cat_total_freq <- result_tol_freq1
          }
        }

        ##create the table for categorical variables' total

        out1_1 <- data.frame(Total = paste0(
          format(cat_total_count, digits = digit),
          " (",
          format(cat_total_freq * 100, digits = digit),
          ")"
        ))

        colnames(out1_1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1_1) <- cat_names_each


        ##create the total table by combining these two tables!!!! finally!!!! we made it !!!!

        out_total <- rbind(out1, out1_1)

        out_totalnum <- out_total
      }



      #===================================================================================================================#
      #===================================================================================================================#



      ##create the table for different groups
      out_all <- out_totalnum


      if (is.null(levels(y))) {
        out_all <- out_all
      } else{
        group_levels <- levels(y)
        ngroup_levels <- length(group_levels)

        for (g in 1:ngroup_levels) {
          dat1 <- dat[which(y == group_levels[g]),]

          dat_noy <- dat1[,-ncol_y]

          ncol_allx <- ncol(dat_noy)

          all_level <- numeric(ncol_allx)

          for (i in 1:ncol_allx) {
            if (!is.null(levels(unlist(dat_noy[, i])))) {
              all_level[i] <- length(levels(unlist(dat_noy[, i])))
            }
          }

          max_level <- max(all_level)



          ## if we only have continuous variables
          if ((sum(all_level > 0)) == 0) {
            result_tol_mec <- numeric(ncol_allx)

            result_tol_sd <- numeric(ncol_allx)

            con_names <- colnames(dat_noy)

            for (i in 1:ncol_allx) {
              result_tol_mec[i] <- mean(unlist(dat_noy[, i]))
              result_tol_sd[i] <- sd(unlist(dat_noy[, i]))
            }

            ##create the table for continuous variables' total
            out1 <- data.frame(Total = paste0(
              format(result_tol_mec, digits = digit),
              "+/-",
              format(result_tol_sd, digits = digit)
            ))

            colnames(out1) <-
              c("Total [Mean (SD) or N(%)]")

            rownames(out1) <- con_names

            out_total <- out1

          }
          ## if we only have cateorical variables
          else if ((sum(all_level > 0)) == ncol_allx) {
            result_tol_count <-
              matrix(nrow = ncol_allx, ncol = max_level)
            result_tol_freq <-
              matrix(nrow = ncol_allx, ncol = max_level)

            cat_names <- colnames(dat_noy)

            cat_names_each <- character(0)

            for (i in 1:ncol_allx) {
              levels_cat <- levels(unlist(dat_noy[, i]))
              nlevel <- length(levels_cat)
              cat_names[i] <- all_var_names[i]
              for (j in 1:nlevel) {
                cat_names_each <-
                  c(cat_names_each,
                    paste0(cat_names[i], ": ", levels_cat[j]))
                result_tol_count[i, j] <-
                  length(which(unlist(dat_noy[, i]) == levels_cat[j]))
                result_tol_freq[i, j] <-
                  length(which(unlist(dat_noy[, i]) == levels_cat[j])) / length(unlist(dat_noy[, i]))
              }
            }

            #get categorical varaibles ready

            result_tol_count1 <-
              result_tol_count[which(!cat_names == ""), ]

            result_tol_freq1 <-
              result_tol_freq[which(!cat_names == ""), ]

            cat_total_count <- numeric(0)

            cat_total_freq <- numeric(0)

            if (!is.null(nrow(result_tol_count1))) {
              for (i in 1:nrow(result_tol_count1)) {
                if (!is.null(ncol(result_tol_count1))) {
                  for (j in 1:ncol(result_tol_count1)) {
                    if (!is.na(result_tol_count1[i, j])) {
                      cat_total_count <- c(cat_total_count, result_tol_count1[i, j])
                      cat_total_freq <-
                        c(cat_total_freq, result_tol_freq1[i, j])
                    }
                  }
                }
                else {
                  if (!is.na(result_tol_count1[i])) {
                    cat_total_count <- c(cat_total_count, result_tol_count1[i])
                    cat_total_freq <-
                      c(cat_total_freq, result_tol_freq1[i])
                  }
                }

              }
            } else {
              if (!is.null(ncol(result_tol_count1))) {
                for (j in 1:ncol(result_tol_count1)) {
                  if (!is.na(result_tol_count1[j])) {
                    cat_total_count <- c(cat_total_count, result_tol_count1[j])
                    cat_total_freq <-
                      c(cat_total_freq, result_tol_freq1[j])
                  }
                }
              } else {
                cat_total_count <- result_tol_count1
                cat_total_freq <- result_tol_freq1
              }
            }
            ##create the table for categorical variables' total

            out1_1 <- data.frame(Total = paste0(
              format(cat_total_count, digits = digit),
              " (",
              format(cat_total_freq * 100, digits = digit),
              ")"
            ))

            colnames(out1_1) <-
              c("Total [Mean (SD) or N(%)]")

            rownames(out1_1) <- cat_names_each

            out_total <- out1_1
          }
          ##we have both continuous and categorical variables
          else{
            result_tol_mec <- numeric(ncol_allx)

            result_tol_sd <- numeric(ncol_allx)

            result_tol_count <-
              matrix(nrow = ncol_allx, ncol = max_level)
            result_tol_freq <-
              matrix(nrow = ncol_allx, ncol = max_level)

            #as.character(unlist(dat_noy[, 1]))

            all_var_names <- colnames(dat_noy)

            con_names <- character(ncol_allx)

            cat_names <- character(ncol_allx)

            cat_names_each <- character(0)

            for (i in 1:ncol_allx) {
              if (all_level[i] == 0) {
                con_names[i] <- all_var_names[i]
                result_tol_mec[i] <- mean(unlist(dat_noy[, i]))
                result_tol_sd[i] <- sd(unlist(dat_noy[, i]))
              } else if (all_level[i] != 0) {
                levels_cat <- levels(unlist(dat_noy[, i]))
                nlevel <- length(levels_cat)
                cat_names[i] <- all_var_names[i]
                for (j in 1:nlevel) {
                  cat_names_each <-
                    c(cat_names_each,
                      paste0(cat_names[i], ": ", levels_cat[j]))
                  result_tol_count[i, j] <-
                    length(which(unlist(dat_noy[, i]) == levels_cat[j]))
                  result_tol_freq[i, j] <-
                    length(which(unlist(dat_noy[, i]) == levels_cat[j])) / length(unlist(dat_noy[, i]))
                }
              }

            }

            #get continuoius variables ready

            con_names1 <- con_names[-which(con_names == "")]

            result_tol_mec1 <-
              result_tol_mec[-which(result_tol_mec == 0)]

            result_tol_sd1 <-
              result_tol_sd[-which(result_tol_sd == 0)]

            ##create the table for continuous variables' total
            out1 <- data.frame(Total = paste0(
              format(result_tol_mec1, digits = digit),
              "+/-",
              format(result_tol_sd1, digits = digit)
            ))

            colnames(out1) <-
              c("Total [Mean (SD) or N(%)]")

            rownames(out1) <- con_names1


            #get categorical varaibles ready

            all_level1 <- all_level[-which(all_level == 0)]

            cat_names1 <- cat_names[-which(cat_names == "")]

            result_tol_count1 <-
              result_tol_count[which(!cat_names == ""), ]

            result_tol_freq1 <-
              result_tol_freq[which(!cat_names == ""), ]

            cat_total_count <- numeric(0)

            cat_total_freq <- numeric(0)

            if (!is.null(nrow(result_tol_count1))) {
              for (i in 1:nrow(result_tol_count1)) {
                if (!is.null(ncol(result_tol_count1))) {
                  for (j in 1:ncol(result_tol_count1)) {
                    if (!is.na(result_tol_count1[i, j])) {
                      cat_total_count <- c(cat_total_count, result_tol_count1[i, j])
                      cat_total_freq <-
                        c(cat_total_freq, result_tol_freq1[i, j])
                    }
                  }
                }
                else {
                  if (!is.na(result_tol_count1[i])) {
                    cat_total_count <- c(cat_total_count, result_tol_count1[i])
                    cat_total_freq <-
                      c(cat_total_freq, result_tol_freq1[i])
                  }
                }

              }
            } else {
              if (!is.null(ncol(result_tol_count1))) {
                for (j in 1:ncol(result_tol_count1)) {
                  if (!is.na(result_tol_count1[j])) {
                    cat_total_count <- c(cat_total_count, result_tol_count1[j])
                    cat_total_freq <-
                      c(cat_total_freq, result_tol_freq1[j])
                  }
                }
              } else {
                cat_total_count <- result_tol_count1
                cat_total_freq <- result_tol_freq1
              }
            }

            ##create the table for categorical variables' total

            out1_1 <- data.frame(Total = paste0(
              format(cat_total_count, digits = digit),
              " (",
              format(cat_total_freq * 100, digits = digit),
              ")"
            ))

            colnames(out1_1) <-
              c("Total [Mean (SD) or N(%)]")

            rownames(out1_1) <- cat_names_each


            ##create the total table by combining these two tables!!!! finally!!!! we made it !!!!

            out_total <- rbind(out1, out1_1)
          }
          #mkaing the tablw that combines both total and subgroups
          out_all <- cbind(out_all, out_total)
        }
        if (alternative == "total") {
          out_all <- out_totalnum
        } else if (alternative == "bygroup") {
          out_all <-
            out_all[-which(colnames(out_all) == colnames(out_totalnum))]
        } else {
          out_all <- out_all
        }
      }


    }
    # if y is missing
    else {
      dat_noy <- dat

      #get the Total column done

      ###know which variable is continuous and which is categorical

      ncol_allx <- ncol(dat_noy)

      all_level <- numeric(ncol_allx)

      for (i in 1:ncol_allx) {
        if (!is.null(levels(unlist(dat_noy[, i])))) {
          all_level[i] <- length(levels(unlist(dat_noy[, i])))
        }
      }

      max_level <- max(all_level)

      ## if we only have continuous variables
      if ((sum(all_level > 0)) == 0) {
        result_tol_mec <- numeric(ncol_allx)

        result_tol_sd <- numeric(ncol_allx)

        con_names <- colnames(dat_noy)

        for (i in 1:ncol_allx) {
          result_tol_mec[i] <- mean(unlist(dat_noy[, i]))
          result_tol_sd[i] <- sd(unlist(dat_noy[, i]))
        }

        ##create the table for continuous variables' total
        out1 <- data.frame(Total = paste0(
          format(result_tol_mec, digits = digit),
          "+/-",
          format(result_tol_sd, digits = digit)
        ))

        colnames(out1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1) <- con_names

        out_all <- out1

      }
      ## if we only have cateorical variables
      else if ((sum(all_level > 0)) == ncol_allx) {
        result_tol_count <-
          matrix(nrow = ncol_allx, ncol = max_level)
        result_tol_freq <-
          matrix(nrow = ncol_allx, ncol = max_level)

        cat_names <- colnames(dat_noy)

        cat_names_each <- character(0)

        for (i in 1:ncol_allx) {
          levels_cat <- levels(unlist(dat_noy[, i]))
          nlevel <- length(levels_cat)
          cat_names[i] <- all_var_names[i]
          for (j in 1:nlevel) {
            cat_names_each <-
              c(cat_names_each,
                paste0(cat_names[i], ": ", levels_cat[j]))
            result_tol_count[i, j] <-
              length(which(unlist(dat_noy[, i]) == levels_cat[j]))
            result_tol_freq[i, j] <-
              length(which(unlist(dat_noy[, i]) == levels_cat[j])) / length(unlist(dat_noy[, i]))
          }
        }

        #get categorical varaibles ready

        result_tol_count1 <-
          result_tol_count[which(!cat_names == ""), ]

        result_tol_freq1 <-
          result_tol_freq[which(!cat_names == ""), ]

        cat_total_count <- numeric(0)

        cat_total_freq <- numeric(0)

        if (!is.null(nrow(result_tol_count1))) {
          for (i in 1:nrow(result_tol_count1)) {
            if (!is.null(ncol(result_tol_count1))) {
              for (j in 1:ncol(result_tol_count1)) {
                if (!is.na(result_tol_count1[i, j])) {
                  cat_total_count <- c(cat_total_count, result_tol_count1[i, j])
                  cat_total_freq <-
                    c(cat_total_freq, result_tol_freq1[i, j])
                }
              }
            }
            else {
              if (!is.na(result_tol_count1[i])) {
                cat_total_count <- c(cat_total_count, result_tol_count1[i])
                cat_total_freq <-
                  c(cat_total_freq, result_tol_freq1[i])
              }
            }

          }
        } else {
          if (!is.null(ncol(result_tol_count1))) {
            for (j in 1:ncol(result_tol_count1)) {
              if (!is.na(result_tol_count1[j])) {
                cat_total_count <- c(cat_total_count, result_tol_count1[j])
                cat_total_freq <-
                  c(cat_total_freq, result_tol_freq1[j])
              }
            }
          } else {
            cat_total_count <- result_tol_count1
            cat_total_freq <- result_tol_freq1
          }
        }

        ##create the table for categorical variables' total

        out1_1 <- data.frame(Total = paste0(
          format(cat_total_count, digits = digit),
          " (",
          format(cat_total_freq * 100, digits = digit),
          ")"
        ))

        colnames(out1_1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1_1) <- cat_names_each

        out_all <- out1_1
      }
      ##we have both continuous and categorical variables
      else{
        result_tol_mec <- numeric(ncol_allx)

        result_tol_sd <- numeric(ncol_allx)

        result_tol_count <-
          matrix(nrow = ncol_allx, ncol = max_level)
        result_tol_freq <-
          matrix(nrow = ncol_allx, ncol = max_level)

        #as.character(unlist(dat_noy[, 1]))

        all_var_names <- colnames(dat_noy)

        con_names <- character(ncol_allx)

        cat_names <- character(ncol_allx)

        cat_names_each <- character(0)

        for (i in 1:ncol_allx) {
          if (all_level[i] == 0) {
            con_names[i] <- all_var_names[i]
            result_tol_mec[i] <- mean(unlist(dat_noy[, i]))
            result_tol_sd[i] <- sd(unlist(dat_noy[, i]))
          } else if (all_level[i] != 0) {
            levels_cat <- levels(unlist(dat_noy[, i]))
            nlevel <- length(levels_cat)
            cat_names[i] <- all_var_names[i]
            for (j in 1:nlevel) {
              cat_names_each <-
                c(cat_names_each,
                  paste0(cat_names[i], ": ", levels_cat[j]))
              result_tol_count[i, j] <-
                length(which(unlist(dat_noy[, i]) == levels_cat[j]))
              result_tol_freq[i, j] <-
                length(which(unlist(dat_noy[, i]) == levels_cat[j])) / length(unlist(dat_noy[, i]))
            }
          }

        }

        #get continuoius variables ready

        con_names1 <- con_names[-which(con_names == "")]

        result_tol_mec1 <-
          result_tol_mec[-which(result_tol_mec == 0)]

        result_tol_sd1 <- result_tol_sd[-which(result_tol_sd == 0)]

        ##create the table for continuous variables' total
        out1 <- data.frame(Total = paste0(
          format(result_tol_mec1, digits = digit),
          "+/-",
          format(result_tol_sd1, digits = digit)
        ))

        colnames(out1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1) <- con_names1


        #get categorical varaibles ready

        all_level1 <- all_level[-which(all_level == 0)]

        cat_names1 <- cat_names[-which(cat_names == "")]

        result_tol_count1 <-
          result_tol_count[which(!cat_names == ""), ]

        result_tol_freq1 <-
          result_tol_freq[which(!cat_names == ""), ]

        cat_total_count <- numeric(0)

        cat_total_freq <- numeric(0)

        if (!is.null(nrow(result_tol_count1))) {
          for (i in 1:nrow(result_tol_count1)) {
            for (j in 1:ncol(result_tol_count1)) {
              if (!is.na(result_tol_count1[i, j])) {
                cat_total_count <- c(cat_total_count, result_tol_count1[i, j])
                cat_total_freq <-
                  c(cat_total_freq, result_tol_freq1[i, j])
              }
            }

          }
        } else {
          for (j in 1:ncol(result_tol_count1)) {
            if (!is.na(result_tol_count1[j])) {
              cat_total_count <- c(cat_total_count, result_tol_count1[j])
              cat_total_freq <-
                c(cat_total_freq, result_tol_freq1[j])
            }
          }
        }

        ##create the table for categorical variables' total

        out1_1 <- data.frame(Total = paste0(
          format(cat_total_count, digits = digit),
          " (",
          format(cat_total_freq * 100, digits = digit),
          ")"
        ))

        colnames(out1_1) <-
          c("Total [Mean (SD) or N(%)]")

        rownames(out1_1) <- cat_names_each


        ##create the total table by combining these two tables!!!! finally!!!! we made it !!!!

        out_total <- rbind(out1, out1_1)

        out_all <- out_total
      }

    }

    return(out_all)
  }

