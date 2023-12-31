context("simulate point and junctions")

test_that("compare use", {
  vx <- simRestore::simulate_policy(initial_population_size = 100,
                        K = 400,
                        num_generations = 10,
                        pull = 0,
                        put = 0,
                        num_replicates = 100,
                        starting_freq = 0.2,
                        seed = 42,
                        genetic_model = "junctions",
                        verbose = FALSE)

  vy <- simRestore::simulate_policy(initial_population_size = 100,
                        K = 400,
                        num_generations = 10,
                        pull = 0,
                        put = 0,
                        num_replicates = 100,
                        starting_freq = 0.2,
                        seed = 42,
                        genetic_model = "point",
                        verbose = FALSE)

  for (tt in unique(vx$results$t)) {
    if (tt > 1) {
      a <- subset(vx$results, vx$results$t == tt)
      b <- subset(vy$results, vy$results$t == tt)
      vv <- t.test(a$num_individuals, b$num_individuals)
      vv2 <- t.test(a$freq_focal_ancestry, b$freq_focal_ancestry)
      testthat::expect_true(vv2$p.value > 0.001)
      testthat::expect_true(vv$p.value > 0.001)
    }
  }
})

test_that("check introduction frequency", {
  # using simple model:
  for (anc_put in c(0.0, 0.5, 1.0)) {
    vx <- simulate_policy(initial_population_size = 30,
                          K = 400,
                          num_generations = 20,
                          pull = 0,
                          put = 100,
                          num_replicates = 1,
                          starting_freq = 0.2,
                          seed = 42,
                          genetic_model = "point",
                          ancestry_put = 1,
                          verbose = FALSE)
    a1 <- tail(vx$results$freq_focal_ancestry, 1)
    testthat::expect_equal(a1, 1, tolerance = 0.01)
  }
  # using junctions:
  for (anc_put in c(0.0, 0.5, 1.0)) {
    vx <- simulate_policy(initial_population_size = 300,
                          K = 400,
                          num_generations = 20,
                          pull = 0,
                          put = 100,
                          num_replicates = 1,
                          starting_freq = 0.2,
                          seed = 42,
                          genetic_model = "junctions",
                          ancestry_put = 1,
                          verbose = FALSE)
    a1 <- tail(vx$results$freq_focal_ancestry, 1)
    testthat::expect_equal(a1, 1, tolerance = 0.05)
  }
})

test_that("genetics output", {
  vx <- simRestore::simulate_policy(initial_population_size = 100,
                                    K = 400,
                                    num_generations = 10,
                                    pull = 0,
                                    put = 0,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    genetic_model = "junctions",
                                    verbose = FALSE,
                                    return_genetics = TRUE)

  testthat::expect_equal(length(unique(vx$genetics$chromosome)), 2)
  num_indiv <- tail(vx$results$num_individuals, 1)
  testthat::expect_equal(length(unique(vx$genetics$individual)), num_indiv)

  num_males <- tail(vx$results$num_males, 1)
  num_females <- tail(vx$results$num_females, 1)
  count_sex <- c(0, 0)
  for (x in unique(vx$genetics$individual)) {
    a1 <- subset(vx$genetics, vx$genetics$individual == x)
    focal_sex <- a1$sex[1]
    count_sex[focal_sex + 1] <- count_sex[focal_sex + 1] + 1
  }
  testthat::expect_equal(count_sex[1], num_males)
  testthat::expect_equal(count_sex[2], num_females)

  testthat::expect_gt(length(unique(vx$genetics$position)), 2)

  vx <- simRestore::simulate_policy(initial_population_size = 10,
                                    K = 400,
                                    num_generations = 3,
                                    pull = 0,
                                    put = 0,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    genetic_model = "point",
                                    verbose = FALSE,
                                    return_genetics = TRUE)

  testthat::expect_equal(length(unique(vx$genetics$chromosome)), 2)
  num_indiv <- tail(vx$results$num_individuals, 1)
  testthat::expect_equal(length(unique(vx$genetics$individual)), num_indiv)

  num_males <- tail(vx$results$num_males, 1)
  num_females <- tail(vx$results$num_females, 1)
  count_sex <- c(0, 0)
  for (x in unique(vx$genetics$individual)) {
    a1 <- subset(vx$genetics, vx$genetics$individual == x)
    focal_sex <- a1$sex[1]
    count_sex[focal_sex + 1] <- count_sex[focal_sex + 1] + 1
  }
  testthat::expect_equal(count_sex[1], num_males)
  testthat::expect_equal(count_sex[2], num_females)

  focal_anc <- tail(vx$results$freq_focal_ancestry, 1)
  testthat::expect_equal(focal_anc, mean(vx$genetics$ancestry))
})


test_that("multiple chromosomes", {
    vx <- simRestore::simulate_policy(initial_population_size = 200,
                                      K = 500,
                                      num_generations = 3,
                                      pull = 0,
                                      put = 0,
                                      morgan = c(1, 2, 3),
                                      num_replicates = 1,
                                      starting_freq = 0.3,
                                      seed = 42,
                                      genetic_model = "junctions",
                                      verbose = FALSE,
                                      return_genetics = TRUE)

    testthat::expect_equal(length(unique(vx$genetics$linkage_group)), 3)

    vx <- simRestore::simulate_policy(initial_population_size = 300,
                                      K = 500,
                                      num_generations = 3,
                                      pull = 0,
                                      put = 0,
                                      morgan = c(1, 2, 3),
                                      num_replicates = 1,
                                      starting_freq = 0.3,
                                      seed = 42,
                                      genetic_model = "point",
                                      verbose = FALSE,
                                      return_genetics = TRUE)

    testthat::expect_equal(length(unique(vx$genetics$linkage_group)), 3)
})

test_that("random mating", {
  vx <- simRestore::simulate_policy(initial_population_size = 10,
                                    K = 100,
                                    num_generations = 10,
                                    pull = 0,
                                    put = 10,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    random_mating = FALSE,
                                    verbose = FALSE)

  vy <- simRestore::simulate_policy(initial_population_size = 10,
                                    K = 100,
                                    num_generations = 10,
                                    pull = 0,
                                    put = 10,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    random_mating = TRUE,
                                    verbose = FALSE)

  # these two simulations should differ.
  a1 <- vx$results$freq_focal_ancestry
  a2 <- vy$results$freq_focal_ancestry
  testthat::expect_true(sum(a1 - a2) != 0)
})

test_that("epc", {
  vx <- simRestore::simulate_policy(initial_population_size = 10,
                                    K = 100,
                                    num_generations = 10,
                                    pull = 0,
                                    put = 10,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    verbose = FALSE)

  vy <- simRestore::simulate_policy(initial_population_size = 10,
                                    K = 100,
                                    num_generations = 10,
                                    pull = 0,
                                    put = 10,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    extra_pair_copulation = 0.1,
                                    verbose = FALSE)

  # these two simulations should differ.
  a1 <- vx$results$freq_focal_ancestry
  a2 <- vy$results$freq_focal_ancestry
  testthat::expect_true(sum(a1 - a2) != 0)
})


test_that("pull_ancestry", {
  vx <- simRestore::simulate_policy(initial_population_size = 10,
                                    K = 100,
                                    num_generations = 10,
                                    pull = 0,
                                    put = 10,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    verbose = FALSE)

  vy <- simRestore::simulate_policy(initial_population_size = 10,
                                    K = 100,
                                    num_generations = 10,
                                    pull = 10,
                                    put = 10,
                                    num_replicates = 1,
                                    starting_freq = 0.2,
                                    seed = 42,
                                    ancestry_pull = 0.1,
                                    verbose = FALSE)

  # these two simulations should differ.
  a1 <- vx$results$freq_focal_ancestry
  a2 <- vy$results$freq_focal_ancestry
  testthat::expect_true(sum(a1 - a2) != 0)
})
