# A solution to the Monty Hall problem (as shown by Gary King in his class)

sims <- 10000
WinNoSwitch <- 0
WinSwitch <- 0
doors <- c(1, 2, 3)

for (i in 1:sims) {
        WinDoor <- sample(doors, 1)
        choice <- sample(doors, 1)
        if (WinDoor == choice) # no switch
        WinNoSwitch <- WinNoSwitch + 1
        doorsLeft <- doors[doors != choice] # switch
        if (any(doorsLeft == WinDoor))
        WinSwitch <- WinSwitch + 1
}

cat("Prob(Car | no switch) = ", WinNoSwitch/sims, "\n")
cat("Prob(Car | switch) = ", WinSwitch/sims, "\n")
