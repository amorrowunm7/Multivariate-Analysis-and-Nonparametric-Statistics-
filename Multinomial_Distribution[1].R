# Multinomial distribution

# Instead of favorite colors, this question is about favorite programming language for data analytics. My Thursday night class has n = 3 students, all from the statistics department at Texas A&M University where it is well known that 50% of students prefer Python, 30% prefer SAS, and 20% prefer R. I decide to ask my class which of the three languages they prefer. Find the probability that no one selects R.

# Outcome vector is {Python, SAS, R} so K = 3.  Size = 3 students.  Prob = c(.5, .3, .2).
> # Find all outcomes where no one chooses R.
  > # Use dmultinom get prob of each.  Add them all together.
  > pvec  <- c(.5, .3, .2)
  # four outcomes have R = 0
  kvec1 <- c(3,0,0)
  > kvec2 <- c(2,1,0)
  > kvec3 <- c(1,2,0)
  > kvec4 <- c(0,3,0)
  > answer <- dmultinom(kvec1, 3, pvec) + dmultinom(kvec2, 3, pvec) + dmultinom(kvec3, 3, pvec) + dmultinom(kvec4, 3,
                                                                                                            answer                                                          #[1] 0.512