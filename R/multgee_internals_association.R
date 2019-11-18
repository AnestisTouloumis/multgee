datacounts <- function(response, id, repeated, ncategories) {
  response <- as.numeric(factor(response))
  data <- data.frame(cbind(response, id, repeated))
  data <- stats::reshape(data,
    v.names = "response", idvar = "id",
    timevar = "repeated", direction = "wide"
  )
  data <- data[, -1]
  data[is.na(data)] <- 0
  ntimes <- ncol(data)
  notimepairs <- choose(ntimes, 2)
  counts <- rep.int(0, notimepairs * (ncategories^2))
  x <- rep(1:ncategories, each = notimepairs * ncategories)
  y <- rep.int(rep(1:ncategories, each = notimepairs), ncategories)
  tp <- rep.int(1:notimepairs, ncategories^2)
  ind_1 <- 1
  for (categ1 in 1:ncategories) {
    for (categ2 in 1:ncategories) {
      for (ind_2 in 1:(ntimes - 1)) {
        for (ind_3 in (ind_2 + 1):ntimes) {
          counts[ind_1] <- sum(
            (data[, ind_2] == categ1) & (data[, ind_3] == categ2)
          )
          ind_1 <- ind_1 + 1
        }
      }
    }
  }
  data <- data.frame(cbind(counts, x, y, tp))
  data
}



fitmm <- function(data, marpars, homogeneous, restricted, add) {
  LORstr <- marpars$LORstr
  LORem <- marpars$LORem
  fmla <- marpars$fmla
  ncategories <- max(data$x)
  timepairs <- max(data$tp)
  if (any(data$counts == 0)) {
    data$counts <- data$counts + add
  }
  LORterm <- matrix(0, timepairs, ncategories^2)
  if (LORstr == "uniform" | LORstr == "category.exch") {
    suppressWarnings(fitted.mod <- gnm::gnm(fmla,
      family = poisson, data = data,
      verbose = FALSE, model = FALSE
    ))
    if (is.null(fitted.mod)) {
      stop("gnm did not converge algorithm")
    }
    coefint <- as.vector(coef(fitted.mod)[gnm::pickCoef(fitted.mod, "x:y")])
    if (LORem == "2way") {
      coefint <- mean(coefint)
    }
    LORterm <- matrix(coefint, timepairs, ncategories^2)
    LORterm <- t(apply(
      LORterm, 1,
      function(x) exp(x * tcrossprod(1:ncategories))
    ))
  }
  if (LORstr == "time.exch") {
    data$x <- factor(data$x)
    data$y <- factor(data$y)
    if (is.null(restricted)) {
      if (LORem == "3way") {
        if (homogeneous) {
          suppressWarnings(fitted.mod <- gnm::gnm(fmla,
            family = poisson,
            data = data, verbose = FALSE,
            model = FALSE
          ))
          if (is.null(fitted.mod)) {
            stop("gnm did not converge algorithm")
          }
          coefint <- as.vector(coef(fitted.mod)[gnm::pickCoef(
            fitted.mod,
            "MultHomog"
          )])
          coefint <- c(tcrossprod(coefint))
        } else {
          suppressWarnings(fitted.mod <- gnm::gnm(fmla,
            family = poisson,
            data = data, verbose = FALSE,
            model = FALSE
          ))
          if (is.null(fitted.mod)) {
            stop("gnm did not converge algorithm")
          }
          coefint <- as.vector(coef(fitted.mod)[gnm::pickCoef(
            fitted.mod,
            "Mult"
          )])
          coefint <- c(tcrossprod(
            coefint[-c(1:ncategories)],
            coefint[1:ncategories]
          ))
        }
        LORterm <- exp(matrix(coefint,
          nrow = timepairs,
          ncol = ncategories^2, TRUE
        ))
      } else {
        LORterm2 <- LORterm
        for (i in 1:timepairs) {
          datamar <- data[data$tp == i, ]
          suppressWarnings(fitted.mod <- gnm::gnm(fmla,
            family = poisson,
            data = datamar, verbose = FALSE,
            model = FALSE
          ))
          if (homogeneous) {
            coefint <- as.vector(coef(fitted.mod)[gnm::pickCoef(
              fitted.mod,
              "MultHomog"
            )])
            coefint <- c(tcrossprod(coefint))
          } else {
            coefint <- as.vector(coef(fitted.mod)[gnm::pickCoef(
              fitted.mod,
              "Mult"
            )])
            coefint <- c(tcrossprod(
              coefint[1:ncategories],
              coefint[-c(1:ncategories)]
            ))
          }
          LORterm2[i, ] <- coefint
        }
        LORterm2 <- colMeans(LORterm2)
        LORterm <- exp(matrix(
          LORterm2, timepairs, ncategories^2,
          TRUE
        ))
      }
    } else {
      if (LORem == "3way") {
        if (homogeneous) {
          coefint <- RRChomog(fmla, data, ncategories)
        } else {
          coefint <- RRCheter(fmla, data, ncategories)
        }
        LORterm <- exp(matrix(coefint,
          nrow = timepairs,
          ncol = ncategories^2, TRUE
        ))
      } else {
        LORterm2 <- LORterm
        for (i in 1:timepairs) {
          datamar <- data[data$tp == i, ]
          if (homogeneous) {
            coefint <- RRChomog(fmla, datamar, ncategories)
          } else {
            coefint <- RRCheter(fmla, datamar, ncategories)
          }
          LORterm2[i, ] <- coefint
        }
        LORterm2 <- colMeans(LORterm2)
        LORterm <- exp(matrix(
          LORterm2, timepairs, ncategories^2,
          TRUE
        ))
      }
    }
  }
  if (LORstr == "RC") {
    data$x <- factor(data$x)
    data$y <- factor(data$y)
    for (i in 1:timepairs) {
      datamar <- data[data$tp == i, ]
      suppressWarnings(fitted.mod <- gnm::gnm(fmla,
        family = poisson,
        data = datamar, verbose = FALSE,
        model = FALSE
      ))
      if (is.null(restricted)) {
        if (homogeneous) {
          coefint <- as.vector(coef(fitted.mod)[gnm::pickCoef(
            fitted.mod,
            "MultHomog"
          )])
          coefint <- c(tcrossprod(coefint))
        } else {
          coefint <- as.vector(coef(fitted.mod)[gnm::pickCoef(
            fitted.mod,
            "Mult"
          )])
          coefint <- c(tcrossprod(
            coefint[1:ncategories],
            coefint[-c(1:ncategories)]
          ))
        }
      } else {
        coefint <- if (homogeneous) {
          RRChomog(fmla, datamar, ncategories)
        } else {
          RRCheter(fmla, datamar, ncategories)
        }
      }
      LORterm[i, ] <- exp(coefint)
    }
  }
  LORterm <- prop.table(LORterm, 1)
  LORterm
}




mmpar <- function(LORem, LORstr, timepairs, homogeneous) {
  if (timepairs == 1) {
    LORem <- "2way"
    LORstr <- switch(LORstr, category.exch = "uniform", RC = "time.exch",
      uniform = "uniform", time.exch = "time.exch"
    )
    if (LORstr == "uniform") {
      fmla <- counts ~ factor(x) + factor(y) + x:y
    }
    if (LORstr == "time.exch") {
      fmla <- if (homogeneous) {
        counts ~ x + y + MultHomog(x, y)
      } else {
        counts ~ x + y + Mult(x, y)
      }
    }
  } else if (LORem == "2way") {
    if (LORstr == "uniform") {
      fmla <-
        counts ~ (factor(x) + factor(y)) * factor(tp) + factor(tp):x:y
    }
    if (LORstr == "time.exch" | LORstr == "RC") {
      fmla <- if (homogeneous) {
        counts ~ x + y + MultHomog(x, y)
      } else {
        counts ~ x + y + Mult(x, y)
      }
    }
  } else {
    if (LORstr == "category.exch") {
      fmla <-
        counts ~ (factor(x) + factor(y)) * factor(tp) + factor(tp):x:y
    }
    if (LORstr == "uniform") {
      fmla <- counts ~ (factor(x) + factor(y)) * factor(tp) + x:y
    }
    if (LORstr == "time.exch") {
      fmla <- if (homogeneous) {
        counts ~ (x + y) * factor(tp) + MultHomog(x, y)
      } else {
        counts ~ (x + y) * factor(tp) + Mult(x, y)
      }
    }
  }
  list(LORem = LORem, LORstr = LORstr, fmla = fmla)
}


RCconstrains <- function(ncategories, homogeneous) {
  ncategories1 <- ncategories - 1
  nodf <- helpvec <- rep.int(0, ncategories1)
  for (i in 1:(ncategories1 - 1)) {
    helpmat <- t(combn(c(1:ncategories1), i))
    for (j in seq_len(nrow(helpmat))) {
      helpvec <- rep.int(0, ncategories1)
      helpvec[helpmat[j, ]] <- 1
      nodf <- rbind(nodf, helpvec)
    }
  }
  nodf <- unique(nodf)
  n1 <- nrow(nodf)
  parscores <- matrix(1:ncategories,
    nrow = n1, ncol = ncategories,
    byrow = TRUE
  )
  for (j in 1:ncategories1) {
    parscores[nodf[, j] == 1, j + 1] <- parscores[nodf[, j] == 1, j]
  }
  parscores <- t(apply(parscores, 1, function(x) as.numeric(factor(x))))
  ans <- list(parscores = parscores, nodf = as.numeric(rowSums(nodf)))
  if (!homogeneous) {
    Homogeneous <- ans
    n1 <- length(Homogeneous$nodf)
    parscores <- cbind(
      apply(Homogeneous$parscores, 2, function(x) rep.int(x, n1)),
      apply(Homogeneous$parscores, 2, function(x) rep(x, each = n1))
    )
    nodf <- rep(Homogeneous$nodf, each = n1) + rep.int(
      Homogeneous$nodf,
      n1
    )
    orderedindices <- order(nodf)
    ans <- list(
      parscores = parscores[orderedindices, ],
      nodf = nodf[orderedindices]
    )
  }
  ans
}



RRCheter <- function(fmla, data, ncategories) {
  Consmat <- RCconstrains(ncategories, FALSE)
  dev <- stop.constrains <- Inf
  datax <- factor(data$x)
  datay <- factor(data$y)
  maxcategory <- nlevels(datax)
  noglm <- length(Consmat$nodf[Consmat$nodf < 2 * (maxcategory - 2)])
  fmla <- update(fmla, ~ . - Mult(x, y) + Mult(z1, z2))
  for (i in 1:noglm) {
    data$z1 <- datax
    data$z2 <- datay
    levels(data$z1) <- pickcoefindz1 <-
      Consmat$parscores[i, 1:maxcategory]
    levels(data$z2) <- pickcoefindz2 <-
      Consmat$parscores[i, -(1:maxcategory)]
    RRCmod <- suppressWarnings(gnm::gnm(fmla,
      data = data, family = poisson,
      verbose = FALSE, model = FALSE
    ))
    if (!is.null(RRCmod)) {
      if (deviance(RRCmod) < dev & RRCmod$conv) {
        scores <- as.numeric(coef(RRCmod)[pickCoef(RRCmod, "Mult")])
        pickcoefind <- unique(pickcoefindz1)
        scoresmu <- scores[pickcoefind][pickcoefindz1]
        mu <- normscores(scoresmu)
        if (all(diff(mu) >= 0) | all(diff(mu) <= 0)) {
          scoresnu <- scores[-pickcoefind][pickcoefindz2]
          nu <- normscores(scoresnu)
          if (all(diff(nu) >= 0) | all(diff(nu) <= 0)) {
            dev <- deviance(RRCmod)
            stop.constrains <- Consmat$nodf[i]
            LORterm <- c(tcrossprod(scoresmu, scoresnu))
          }
        }
      }
    }
    if (stop.constrains < Consmat$nodf[i + 1]) {
      break
    }
  }
  if (!is.finite(dev)) {
    fmla <- update(fmla, ~ . - Mult(z1, z2) + x1:x2)
    for (i in (noglm + 1):length(Consmat$nodf)) {
      datax1 <- datax
      datay1 <- datay
      levels(datax1) <- pickcoefindz1 <-
        Consmat$parscores[i, 1:maxcategory]
      levels(datay1) <- pickcoefindz2 <- Consmat$parscores[i, - (1:maxcategory)]
      data$x1 <- as.numeric(datax1)
      data$x2 <- as.numeric(datay1)
      RRCmod <- suppressWarnings(glm(fmla, data = data, family = poisson))
      if (deviance(RRCmod) < dev & RRCmod$conv) {
        LORterm <- c(tcrossprod(pickcoefindz1, pickcoefindz2) *
          as.numeric(RRCmod$coef["x1:x2"]))
      }
    }
  }
  if (!is.finite(dev)) {
    LORterm <- rep(0, nlevels(datax)^2)
  }
  LORterm
}




RRChomog <- function(fmla, data, ncategories) {
  Consmat <- RCconstrains(ncategories, TRUE)
  datax <- factor(data$x)
  datay <- factor(data$y)
  dev <- stop.constrains <- Inf
  noglm <- length(Consmat$nodf[Consmat$nodf < (nlevels(datax) - 2)])
  fmla <- update(fmla, ~ . - MultHomog(x, y) + MultHomog(z1, z2))
  for (i in 1:noglm) {
    data$z1 <- datax
    data$z2 <- datay
    levels(data$z1) <- levels(data$z2) <- pickcoefind <-
      Consmat$parscores[i, ]
    suppressWarnings(RRCmod <- gnm::gnm(fmla,
      family = poisson, data = data,
      verbose = FALSE, model = FALSE
    ))
    if (!is.null(RRCmod)) {
      if (deviance(RRCmod) < dev & RRCmod$conv) {
        pickcoef <- gnm::pickCoef(RRCmod, "MultHomog(.,.)")
        scores <- as.numeric(coef(RRCmod)[pickcoef])[pickcoefind]
        mu <- normscores(scores)
        if (all(diff(mu) >= 0) | all(diff(mu) <= 0)) {
          dev <- deviance(RRCmod)
          LORterm <- c(tcrossprod(scores))
          if (Consmat$nodf[i] < Consmat$nodf[i + 1]) {
            break
          }
        }
      }
    }
    if (stop.constrains < Consmat$nodf[i + 1]) {
      break
    }
  }
  if (!is.finite(dev)) {
    fmla <- update(fmla, ~ . - MultHomog(z1, z2) + x1:x2)
    data$x1 <- as.numeric(datax1)
    data$x2 <- as.numeric(datay1)
    for (i in (noglm + 1):length(Consmat$nodf)) datax1 <- datax
    datay1 <- datay
    levels(datax1) <- levels(datay1) <- pickcoefind <-
      Consmat$parscores[i, ]
    RRCmod <- suppressWarnings(glm(fmla,
      data = data, family = poisson,
      verbose = FALSE, model = FALSE
    ))
    if (deviance(RRCmod) < dev & RRCmod$conv) {
      LORterm <- c(tcrossprod(pickcoefind) *
        as.numeric(RRCmod$coef["x1:x2"]))
    }
  }
  if (!is.finite(dev)) {
    LORterm <- rep(0, nlevels(datax)^2)
  }
  LORterm
}
