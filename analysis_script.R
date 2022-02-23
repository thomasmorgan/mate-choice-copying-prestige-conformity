library(rjags)
library(HDInterval)

dem_scores <- dem_choices <- matrix(nrow=length(chose_face_1), ncol=9)
max_score <- total_score_f1 <- total_score_f2 <- n_dems_f1 <- n_dems_f2 <- vector(length=length(chose_face_1))
for (i in 1:length(chose_face_1)) {
  dem_choices[i, 1] <- d1f1[i]
  dem_choices[i, 2] <- d2f1[i]
  dem_choices[i, 3] <- d3f1[i]
  dem_choices[i, 4] <- d4f1[i]
  dem_choices[i, 5] <- d5f1[i]
  dem_choices[i, 6] <- d6f1[i]
  dem_choices[i, 7] <- d7f1[i]
  dem_choices[i, 8] <- d8f1[i]
  dem_choices[i, 9] <- d9f1[i]
  n_dems_f1[i] <- sum(dem_choices[i,] == 1)
  n_dems_f2[i] <- sum(dem_choices[i,] == 0)
  dem_scores[i, 1] <- d1s[i]
  dem_scores[i, 2] <- d2s[i]
  dem_scores[i, 3] <- d3s[i]
  dem_scores[i, 4] <- d4s[i]
  dem_scores[i, 5] <- d5s[i]
  dem_scores[i, 6] <- d6s[i]
  dem_scores[i, 7] <- d7s[i]
  dem_scores[i, 8] <- d8s[i]
  dem_scores[i, 9] <- d9s[i]
  total_score_f1[i] <- sum(dem_scores[i,][dem_choices[i,] == 1])
  total_score_f2[i] <- sum(dem_scores[i,][dem_choices[i,] == 0])
  max_score[i] <- max(dem_scores[i,])
}
delta_n_dems <- (n_dems_f1 - n_dems_f2)/(n_dems_f1 + n_dems_f2)
delta_dem_scores <- (total_score_f1 - total_score_f2)/(total_score_f1 + total_score_f2)
consensus <- abs(((n_dems_f1 / (n_dems_f1 + n_dems_f2)) - 0.5) * 2)



data <- list(N=length(chose_face_1[sex != -666]),
             chose_face_1=chose_face_1[sex != -666],
             initially_chose_face_1=initially_chose_face_1[sex != -666],
             d1f1=d1f1[sex != -666]*2 - 1,
             d2f1=d2f1[sex != -666]*2 - 1,
             d3f1=d3f1[sex != -666]*2 - 1,
             d4f1=d4f1[sex != -666]*2 - 1,
             d5f1=d5f1[sex != -666]*2 - 1,
             d6f1=d6f1[sex != -666]*2 - 1,
             d7f1=d7f1[sex != -666]*2 - 1,
             d8f1=d8f1[sex != -666]*2 - 1,
             d9f1=d9f1[sex != -666]*2 - 1,
             d1s=d1s[sex != -666] - mean(score[sex != -666]),
             d2s=d2s[sex != -666] - mean(score[sex != -666]),
             d3s=d3s[sex != -666] - mean(score[sex != -666]),
             d4s=d4s[sex != -666] - mean(score[sex != -666]),
             d5s=d5s[sex != -666] - mean(score[sex != -666]),
             d6s=d6s[sex != -666] - mean(score[sex != -666]),
             d7s=d7s[sex != -666] - mean(score[sex != -666]),
             d8s=d8s[sex != -666] - mean(score[sex != -666]),
             d9s=d9s[sex != -666] - mean(score[sex != -666]),
             score=score[sex != -666],
             n_dems=n_dems[sex != -666],
             preference=preference[sex != -666],
             ppt_id=ppt_id[sex != -666],
             N_ppts=max(ppt_id[sex != -666]),
             gender=gender[sex != -666] + 1,
             orientation=orientation[sex != -666] + 1,
             delta_n_dems=delta_n_dems[sex != -666],
             delta_dem_scores=delta_dem_scores[sex != -666],
             consensus=consensus[sex != -666],
             n_dems_f1=n_dems_f1[sex != -666])

model_string_ct = "
model {
  for (i in 1:N) {
    chose_face_1[i] ~ dbern(p[i])
    logit(p[i]) <- x[i]
    x[i] <- baseline[initially_chose_face_1[i] + 1] + 
            social_information[i] * ppt_effect[ppt_id[i]] * (1 + (consensus[i]-0.5)*ct_factor[gender[i], orientation[i]])

    social_information[i] <-
      d1f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d1s[i]) * ifelse(n_dems[i] > 0, 1, 0) + 
      d2f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d2s[i]) * ifelse(n_dems[i] > 1, 1, 0) +
      d3f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d3s[i]) * ifelse(n_dems[i] > 2, 1, 0) +
      d4f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d4s[i]) * ifelse(n_dems[i] > 3, 1, 0) +
      d5f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d5s[i]) * ifelse(n_dems[i] > 4, 1, 0) +
      d6f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d6s[i]) * ifelse(n_dems[i] > 5, 1, 0) +
      d7f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d7s[i]) * ifelse(n_dems[i] > 6, 1, 0) +
      d8f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d8s[i]) * ifelse(n_dems[i] > 7, 1, 0) +
      d9f1[i] * (weight[gender[i], orientation[i]] + score_effect[gender[i], orientation[i]]*d9s[i]) * ifelse(n_dems[i] > 8, 1, 0)
  }
  for (i in 1:2) {
    baseline[i] ~ dnorm(0, 1/10^2)
  }
  for (j in 1:3) {
    for (k in 1:2) {
      weight[j, k] ~ dnorm(mean_weight, 1/(sd_weight^2))
      score_effect[j, k] ~ dnorm(mean_score, 1/(sd_score^2))
      ct_factor[j, k] ~ dnorm(mean_ct_factor, 1/(sd_ct_factor^2))
    }
  }
  
  mean_weight ~ dnorm(0, 1/(5^2))
  sd_weight ~ dexp(0.5)
  mean_score ~ dnorm(0, 1/(1^2))
  sd_score ~ dexp(5)
  mean_ct_factor ~ dnorm(0, 1/1^2)
  sd_ct_factor ~ dexp(5)
  for (i in 1:N_ppts) {
    ppt_effect[i] ~ dnorm(1, 1/(sd_ppt^2))
  }
  sd_ppt ~ dexp(1)
}
"

model <- jags.model(textConnection(model_string_ct), data=data, n.chains=3)
nodes <- c("baseline", "weight", "score_effect", "sd_ppt", "mean_weight", "sd_weight", "mean_score", "sd_score", "ct_factor", "mean_ct_factor", "sd_ct_factor")
samples <- coda.samples(model,nodes,5000,1)
effectiveSize(samples)
gelman.diag(samples)
crosscorr.plot(samples)
plot(samples)

summary(samples)
save(samples, file="samples.txt")
