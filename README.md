# GameRanking
A Bayesian ranking algorithm for multiplayer games with player and faction effects

My brother is insanely into board games and he got a new game called Twilight Imperium. It is a 3 to 6 player game and each time you play each player chooses a faction. There are 17 factions. He wanted a way to rank everyone he has played with as well as ranking the factions by how much they contribute to the final score.

Almost all the inspiration for this model is taken from the LARC project (also on Github) where we developed a model to rank teams in pairwise comparisons. This uses very similar methodology, except that we had to deal with multiplayer games where we're not dealing with "win/loss" but point tallies.

First we had to decide the relative merits of coming in first or second or third. Twilight Imperium is a race to ten points. We decided your "final score" should be your final point tally (between 0 and 10) plus the number of players that you beat (eg. one if you came second to last). This rewards scoring points but also rewards the winner and helps spread out the scores a bit.

The model attempts to estimate a strength parameter for every faction and player. Your relative strength for a single game is: (player strength + faction strength)/sum(all strengths in game). We then use the relative strengths as the probabilities in a multinomial likelihood where each x is each players score.

We introduce a parameter, lambda, to control the relative importance of player effects to faction effects. Lambda is a beta RV that describes the percent attributable to the player effect.

The MCMC was performed in JAGS.

The only research done for this was the Wikipedia article on the multinomial distribution (https://en.wikipedia.org/wiki/Multinomial_distribution), so there are quite possibly much better ways out there!
