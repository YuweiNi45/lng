#' Identify potential confounders for one predictor of interest
#'
#' Generic function to dectect potential confounders for one predictor of interest no matter the outcome varible and predictor of interest are continuous or categorical.
#'
#' @param x   a vector of data values that contains the predictor of interest
#' @param y   a vector of data values that contains outcome varible
#' @param dat  a data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the predictor of interest, outcome varible and all the covariates
#'
#' @return  It returns a list that contains the suggested potential confounders.
#'
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
#'
#' @seealso \code{\link[stats]{lm}}
#'
#' \code{\link[stats]{glm}}
#'
#'@import tidyverse
#'
#'@export


confounding <- function(x, y, dat) {
  ##get the name and column number of outcome variable

  y_length <- length(y)

  data_names <- colnames(dat)

  name_length <- length(data_names)

  result <- numeric(name_length)

  for (i in 1:name_length) {
    result[i] <- sum(y %in% factor(unlist(dat[data_names[i]])))
  }

  ncol_y <- which(result == y_length)

  y_group <- data_names[ncol_y]

  ##get name and column number of covariate of interest

  x_length <- length(x)

  result1 <- numeric(name_length)

  for (i in 1:name_length) {
    result1[i] <- sum(x %in% factor(unlist(dat[data_names[i]])))
  }

  ncol_x <- which(result1 == x_length)

  x_group <- data_names[ncol_x]

  dat1 <- dat[c(ncol_y, ncol_x)]

  dat2 <- cbind(dat[ncol_y], dat[-c(ncol_y, ncol_x)])

  dat3 <- cbind(dat[ncol_x], dat[-c(ncol_y, ncol_x)])

  con_n <- numeric(ncol(dat2))

  if (is.null(levels(y))) {
    for (i in 2:ncol(dat2)) {
      fit2 <- lm(unlist(dat2[1]) ~ unlist(dat2[i]), data = dat2)
      n_coef <- nrow(summary(fit2)$coefficients)
      if (is.null(levels(x))) {
        fit3 <- lm(unlist(dat3[1]) ~ unlist(dat3[i]), data = dat3)
        if (summary(fit2)$coefficients[2, 4] < 0.05 &
            summary(fit3)$coefficients[2, 4] < 0.05) {
          con_n[i - 1] <- i
        }
      } else {
        fit3 <- glm(unlist(dat3[1]) ~ unlist(dat3[i]),
                    data = dat3,
                    family = binomial())
        for (j in 2:n_coef) {
          if (summary(fit2)$coefficients[j, 4] < 0.05 &
              summary(fit3)$coefficients[j, 4] < 0.05) {
            con_n[i - 1] <- i
          }
        }
      }
    }
  }else{
    for (i in 2:ncol(dat2)) {
      fit2 <-
        glm(unlist(dat2[1]) ~ unlist(dat2[2]),
            data = dat2,
            family = binomial())
      n_coef <- nrow(summary(fit2)$coefficients)
      if (is.null(levels(x))) {
        fit3 <- lm(unlist(dat3[1]) ~ unlist(dat3[i]), data = dat3)
        if (summary(fit2)$coefficients[2, 4] < 0.05 &
            summary(fit3)$coefficients[2, 4] < 0.05) {
          con_n[i - 1] <- i
        }
      } else {
        fit3 <- glm(unlist(dat3[1]) ~ unlist(dat3[i]),
                    data = dat3,
                    family = binomial())
        for (j in 2:n_coef) {
          if (summary(fit2)$coefficients[j, 4] < 0.05 &
              summary(fit3)$coefficients[j, 4] < 0.05) {
            con_n[i - 1] <- i
          }
        }

      }
    }
  }

  if (sum(!con_n == 0) > 0) {
    return(paste0("Sggested confounding variables is ", colnames(dat2[con_n])))
  } else {
    return("No suggested confounding varaibles found.")
  }

}

