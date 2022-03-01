library(ggplot2)
library(data.table)
spam.dt <- data.table::fread("~/spam.data")
label.col.i <- ncol(spam.dt)
feature.mat <- as.matrix(spam.dt[, -label.col.i, with=FALSE])
label.vec <- spam.dt[[label.col.i]]
is.train <- rep(c(TRUE,FALSE),l=nrow(feature.mat))
fit <- glmnet::cv.glmnet(
  feature.mat[is.train,], label.vec[is.train], family="binomial")
pred.vec <- as.numeric(predict(fit, feature.mat))
test.dt <- data.table(
  set=ifelse(is.train, "train", "test"),
  pred=pred.vec,
  label=label.vec
)[set=="test"]

getLift <- function(pred.name, guess, label, weight = rep(1, length(label))){
  if (is.factor(label)) {
    label <- as.integer(label)
  }
  stopifnot(is.numeric(label))
  label.tab <- table(label)
  if (length(label.tab) == 1) {
    print(label.tab)
    stop("only one label value")
  }
  if (all(label %in% c(0, 1))) {
    label[label == 0] <- -1
  }
  if (all(label %in% c(1, 2))) {
    label[label == 1] <- -1
    label[label == 2] <- 1
  }
  stopifnot(label %in% c(-1, 1))
  stopifnot(is.numeric(guess))
  stopifnot(length(label) == length(guess))
  if (any(is.na(guess))) {
    stop("ROC curve undefined for NA guess")
  }
  stopifnot(is.numeric(weight))
  stopifnot(length(label) == length(weight))
  stopifnot(weight > 0)
  ord <- order(guess)
  y <- label[ord]
  w <- weight[ord]
  y.hat <- guess[ord]
  is.positive <- y == 1
  is.negative <- !is.positive
  w.positive <- w.negative <- w
  w.positive[is.negative] <- 0
  w.negative[is.positive] <- 0
  cum.positive <- cumsum(w.positive)
  cum.negative <- cumsum(w.negative)
  is.end <- c(diff(y.hat) != 0, TRUE)
  n <- length(y)
  end.prop.data <- seq(1, length(y))[is.end]
  prop.data <- 1-c(0, end.prop.data)/n
  threshold <- c(y.hat[is.end], Inf)
  total.positive <- cum.positive[n]
  total.negative <- cum.negative[n]
  FN <- c(0, cum.positive[is.end])
  FNR <- FN/total.positive
  TPR <- 1 - FNR
  TN <- c(0, cum.negative[is.end])
  FP <- total.negative - TN
  FPR <- FP/total.negative
  data.table(pred.name, TPR, FPR, threshold, FN, FP, prop.data)
}

roc.points <- test.dt[, rbind(
  getLift("linear.model", pred, label),
  getLift("constant", rep(0, .N), label),
  getLift("ideal", label, label))]
ggplot()+
  geom_path(aes(
    FPR, TPR, color=pred.name),
    data=roc.points)

ggplot()+
  geom_path(aes(
    prop.data, TPR, color=pred.name),
    data=roc.points)

roc.points[, min.thresh := -threshold]
roc.points[, max.thresh := c(Inf, min.thresh[-.N]), by=pred.name]
roc.points[, lift.diff := TPR-prop.data]
roc.points[, lift.ratio := TPR/prop.data]
ggplot()+
  geom_segment(aes(
    min.thresh, lift.diff,
    xend=max.thresh, yend=lift.diff,
    color=pred.name),
    data=roc.points)+
  geom_step(aes(
    min.thresh, lift.diff,
    color=pred.name),
    data=roc.points)+
  facet_grid(pred.name ~ ., labeller=label_both)

not.nan <- roc.points[0 < prop.data]
ggplot()+
  geom_segment(aes(
    min.thresh, lift.ratio,
    xend=max.thresh, yend=lift.ratio,
    color=pred.name),
    data=not.nan)+
  geom_step(aes(
    min.thresh, lift.ratio,
    color=pred.name),
    data=not.nan)+
  facet_grid(pred.name ~ ., labeller=label_both)

only.linear <- melt(
  not.nan[pred.name=="linear.model"],
  measure=c("lift.ratio", "lift.diff"))
max.dt <- only.linear[, .SD[which.max(value)], by=variable]
hilite.dt <- rbind(
  data.table(
    label="default threshold",
    only.linear[min.thresh < 0 & 0 <= max.thresh & variable=="lift.ratio"]),
  max.dt[, label := sprintf("max %s=%.2f", variable, value)])
gg <- ggplot()+
  ggtitle("Lift differences and ratios for linear model")+
  geom_vline(aes(
    xintercept=(min.thresh+max.thresh)/2),
    data=hilite.dt[, .(min.thresh, max.thresh)])+
  geom_text(aes(
    x=(min.thresh+max.thresh)/2,
    vjust=ifelse(variable=="lift.diff", 1.1, -0.2),
    y=-Inf, label=label),
    angle=90,
    hjust=0,
    data=hilite.dt)+
  geom_segment(aes(
    min.thresh, value,
    xend=max.thresh, yend=value),
    data=only.linear)+
  geom_step(aes(
    min.thresh, value),
    data=only.linear)+
  facet_grid(variable ~ ., labeller=label_both, scales="free")+
  ylab("")+
  xlab("constant added to predicted values")
png(
  "figure-lift-chart-ratio-diff-thresholds.png",
  width=5, height=4, units="in", res=200)
print(gg)
dev.off()

pred.colors <- c(
    "ideal"="blue",
    "linear.model"="black",
    "constant"="red")
gg <- ggplot()+
  ggtitle("ROC curves for linear model of spam data")+
  scale_color_manual(values=pred.colors, breaks=names(pred.colors))+
  geom_path(aes(
    FPR, TPR, color=pred.name),
    data=roc.points)+
  geom_point(aes(
    FPR, TPR, color=pred.name),
    data=hilite.dt)+
  geom_text(aes(
    FPR, TPR, color=pred.name, label=label),
    hjust=0,
    vjust=1,
    data=hilite.dt)+
  coord_equal()+
  xlab("False Positive Rate")+
  ylab("True Positive Rate")
png(
  "figure-lift-chart-roc.png",
  width=5, height=4, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  ggtitle("Lift chart for linear model of spam data")+
  scale_color_manual(values=pred.colors, breaks=names(pred.colors))+
  geom_path(aes(
    prop.data, TPR, color=pred.name),
    data=roc.points)+
  geom_point(aes(
    prop.data, TPR, color=pred.name),
    data=hilite.dt)+
  geom_text(aes(
    prop.data, TPR, color=pred.name, label=label),
    hjust=0,
    vjust=1,
    data=hilite.dt)+
  coord_equal()+
  xlab("Proportion of data")+
  ylab("True Positive Rate")
png(
  "figure-lift-chart.png",
  width=5, height=4, units="in", res=200)
print(gg)
dev.off()

