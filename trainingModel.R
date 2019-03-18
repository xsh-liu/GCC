models <- c("knn")

fit <- sapply(models, {
      train(Renewal ~ ., methods = models)
})

pred <- predict(fit, data = test)

accu <- mean(pred == validation$Renewal)