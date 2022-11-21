goal_seq <- c(0,1,2,3,4,5,6,7,8,9,10)
lambda_a <- 2.76
lambda_b <- 3.22

pois_test_a <- dpois(goal_seq, lambda_a)
pois_test_b <- dpois(goal_seq, lambda_b)

pois_matrix <- round(pois_test_a %*% t(pois_test_b),3)
colnames(pois_matrix) <- goal_seq
rownames(pois_matrix) <- goal_seq

pois_matrix <- as.matrix(pois_matrix)
pois_matrix

team_a <- rowSums(pois_matrix * lower.tri(pois_matrix, diag=FALSE))
team_b <- rowSums(pois_matrix * upper.tri(pois_matrix, diag=FALSE))
tie <- rowSums(pois_matrix * diag(pois_matrix))

team_a <- sum(team_a)
team_b <- sum(team_b)
tie <- 1-(team_a + team_b)

team_a
team_b
tie
