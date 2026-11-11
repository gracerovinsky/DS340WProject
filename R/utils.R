suppressPackageStartupMessages(library(data.table))

# ---- flag helper: Kaggle/BTS Cancelled/Diverted may load as logical, int or text
to_flag <- function(x) {
  if (is.logical(x)) return(x)
  if (is.character(x)) return(toupper(trimws(x)) %in% c("TRUE", "T", "1", "1.0"))
  x == 1
}

# ---- find flight CSVs for the requested years (Combined_Flights_2019.csv or Flights_2019_1.csv)
find_flight_files <- function(dir, years) {
  pat <- sprintf("^(Combined_)?Flights_(%s)(_[0-9]+)?\\.csv$", paste(years, collapse = "|"))
  f <- list.files(dir, pattern = pat, full.names = TRUE)
  if (!length(f)) stop("No flight files found in ", dir, " matching ", pat)
  sort(f)
}

# ---- airport -> IANA time zone (OpenFlights). Downloaded once, then cached.
get_airport_tz <- function(data_dir) {
  path <- file.path(data_dir, "airports.dat")
  if (!file.exists(path)) {
    url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
    message("Downloading airports.dat ...")
    utils::download.file(url, path, quiet = TRUE)
  }
  a <- fread(path, header = FALSE, na.strings = c("\\N", ""), encoding = "UTF-8",
             select = c(5, 12), col.names = c("iata", "tz"))
  a <- a[!is.na(iata) & nchar(iata) == 3 & !is.na(tz) & tz %in% OlsonNames()]
  # drop airports whose code maps to >1 different tz (ambiguous)
  a <- a[, if (uniqueN(tz) == 1L) .(tz = tz[1]), by = iata]
  a
}

# ---- local wall-clock (date + HHMM) -> UTC epoch seconds, DST-safe (per time zone)
local_to_utc <- function(date, hhmm, tz) {
  n   <- length(date)
  out <- rep(NA_real_, n)
  ok  <- !is.na(date) & !is.na(hhmm) & !is.na(tz) &
         hhmm >= 0 & hhmm <= 2400 & (hhmm %% 100) < 60
  ok[is.na(ok)] <- FALSE
  if (!any(ok)) return(out)
  is2400 <- !is.na(hhmm) & hhmm == 2400L
  h <- ifelse(is2400, 0L, hhmm)
  d <- date + as.integer(is2400)            # 2400 = 00:00 of the next day
  groups <- split(which(ok), tz[ok])
  for (z in names(groups)) {
    i <- groups[[z]]
    s <- sprintf("%s %02d:%02d", format(d[i], "%Y-%m-%d"), h[i] %/% 100L, h[i] %% 100L)
    out[i] <- as.numeric(as.POSIXct(s, format = "%Y-%m-%d %H:%M", tz = z))
  }
  out
}

# ---- UTC epoch seconds -> local minutes after midnight (per time zone)
utc_to_local_minutes <- function(sec, tz) {
  n   <- length(sec)
  out <- rep(NA_real_, n)
  ok  <- !is.na(sec) & !is.na(tz)
  groups <- split(which(ok), tz[ok])
  for (z in names(groups)) {
    i  <- groups[[z]]
    tt <- as.POSIXlt(.POSIXct(sec[i], tz = z))
    out[i] <- tt$hour * 60 + tt$min
  }
  out
}

hhmm_to_minutes <- function(x) {
  m <- (x %/% 100L) * 60L + (x %% 100L)
  ifelse(!is.na(x) & x == 2400L, 0L, m)
}

# ---- metrics ----------------------------------------------------
brier   <- function(p, y) mean((p - y)^2)
logloss <- function(p, y, eps = 1e-12) { p <- pmin(pmax(p, eps), 1 - eps); -mean(y * log(p) + (1 - y) * log(1 - p)) }
auc_roc <- function(p, y) {
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
auc_pr <- function(p, y) {            # average precision
  if (sum(y == 1) == 0) return(NA_real_)
  o <- order(p, decreasing = TRUE); y <- y[o]
  tp <- cumsum(y == 1); prec <- tp / seq_along(y)
  sum(prec[y == 1]) / sum(y == 1)
}
metrics_row <- function(p, y) {
  list(n = length(y), base_rate = mean(y), brier = brier(p, y), logloss = logloss(p, y),
       auc = auc_roc(p, y), pr_auc = auc_pr(p, y))
}

# ---- model helpers shared by 04_models.R and 05_demo.R -----------------
# design matrix for xgboost: numeric features + one-hot airport
design_matrix <- function(dat, x, airports) {
  m  <- as.matrix(dat[, x, with = FALSE])
  ap <- sapply(airports, function(a) as.numeric(as.character(dat$airport) == a))
  if (is.null(dim(ap))) ap <- matrix(ap, nrow = nrow(m))
  colnames(ap) <- paste0("ap_", airports)
  cbind(m, ap)
}
predict_model <- function(model, name, dat, feats, airports) {
  dat <- copy(dat); dat[, airport := factor(as.character(airport), levels = airports)]
  if (grepl("logit", name)) as.numeric(predict(model, newdata = dat, type = "response"))
  else as.numeric(predict(model, xgboost::xgb.DMatrix(design_matrix(dat, feats[[name]], airports))))
}
