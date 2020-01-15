
# calculate predicted values
# expand group, item
# spread_draws for items, thetas, sigma_g
# compute using indexing?

# "true" from the model
probs <- draws %>%
  gather_draws(pprob[stan_group_item]) %>%
  ungroup() %>%
  select(stan_group_item, .draw, pprob = .value) %>%
  print()



# check to see if estimates == true
prob_sums <- probs %>%
  group_by(stan_group_item) %>%
  summarize(
    pprob_mean = mean(pprob),
    pprob_median = median(pprob)
  ) %>%
  print()

ggplot(prob_sums) +
  aes(x = pprob_mean) +
  geom_histogram()

ggplot(irt_sums) +
  aes(x = p_calc_mean) +
  geom_histogram()

left_join(irt_sums, prob_sums) %>%
  ggplot() +
  aes(x = (p_calc_mean), y = (pprob_mean)) +
  geom_abline() +
  geom_line(col = "red")


