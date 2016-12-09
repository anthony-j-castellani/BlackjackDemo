# Introduces the program.
init_fun <- function(COUNT=1) {
    print("Welcome to the table, we're playing Blackjack today.")
    rules_choice(COUNT)
}

# Offers to print out the rules or continues
rules_choice <- function(COUNT) {
    print("Do you want to see the rules?")
    CHOICE <- readline("[yes / no] > ")
    if(CHOICE == "yes") {
        rules_fun(COUNT)
    } else if(CHOICE == "no") {
        chip_buy(COUNT)
    } else {
        rules_choice(COUNT)
    }
}

# Tool to purchase chips.
chip_buy <- function(COUNT) {
    print("How many chips do you want to buy?")
    options(warn=-1)
    CHIPS <- as.numeric(readline("$"))
    options(warn=0)
    if(is.na(CHIPS)) {
        chip_buy(COUNT)
    } else {
        print(paste("You now have $", CHIPS, " in chips. Let's begin.", sep = ""))
        deck_shuffler(CHIPS,COUNT)
    }
}

# Shuffles the deck, calls the wager function.
deck_shuffler <- function(CHIPS,COUNT) {
    print("Shuffling the 6-deck shoe...")
    SHUFFLED.DECK <- DECK[sample(1:312),]
    wager_fun(CHIPS,COUNT,SHUFFLED.DECK)
}

# Takes bets. A filter stops the player from betting with insufficient funds,
# calling the chip purchasing function.
wager_fun <- function(CHIPS,COUNT,SHUFFLED.DECK) {
    if(CHIPS < 2) {
        print("You have insufficient funds to continue.")
        buy_quit(CHIPS,COUNT,SHUFFLED.DECK)
    } else {
        print("How much do you want to wager? (The minimum bet is $2)")
        options(warn=-1)
        BET <- as.numeric(readline("$"))
        while(is.na(BET)) {
            print("How much do you want to wager? (The minimum bet is $2)")
            BET <- as.numeric(readline("$"))
        }
        options(warn=0)
        if(BET >= 2 & BET > CHIPS) {
            print("You have insufficient funds for this wager.")
            buy_quit(CHIPS,COUNT,SHUFFLED.DECK)
        } else if(BET >= 2 & BET <= CHIPS) {
            init_deal(BET,CHIPS,COUNT,SHUFFLED.DECK)
        } else {
            wager_fun(CHIPS,COUNT,SHUFFLED.DECK)
        }
    }
}

# the function that allows the player to purchase chips or quit.
buy_quit <- function(CHIPS,COUNT,SHUFFLED.DECK) {
    print("Do you want to buy more chips or quit?")
    CHOICE <- readline("[buy / quit] > ")
    if(CHOICE == "buy") {
        print("How many chips do you want to buy?")
        options(warn=-1)
        PURCHASE <- as.numeric(readline("$"))
        while(is.na(PURCHASE)) {
            print("How many chips do you want to buy?")
            PURCHASE <- as.numeric(readline("$"))
        }
        options(warn=0)
        CHIPS <- CHIPS + PURCHASE
        print(paste("You now have $", CHIPS, " in chips.", sep = ""))
        wager_fun(CHIPS,COUNT,SHUFFLED.DECK)
    } else if(CHOICE == "quit") {
        print(paste("Thank you for playing. You cash out at $", CHIPS, ".", sep = ""))
    } else {
        buy_quit(CHIPS,COUNT,SHUFFLED.DECK)
    }
}

# The function that deals the first two cards to the player and dealer.
# Initializes the dealer scoring function and calls the next function.
init_deal <- function(BET,CHIPS,COUNT,SHUFFLED.DECK) {
    PLAYER <- SHUFFLED.DECK[c((COUNT),(COUNT+2)),]
    DEALER <- SHUFFLED.DECK[c((COUNT+1),(COUNT+3)),]
    COUNT <- COUNT + 4
    print(paste("The dealer's face-up card is showing ",
                DEALER[1,1], ".", sep = ""))
    print(paste("Your cards are ", PLAYER[1,1], " and ",
                PLAYER[2,1], ".", sep = ""))
    if(sum(DEALER$VALUE2) == 21) {
        D.SCORE <- sum(DEALER$VALUE2)
    } else if(sum(DEALER$VALUE2) > 21) {
        D.SCORE <- 2
    } else if(sum(DEALER$VALUE2) > 16 & sum(DEALER$VALUE2) < 21) {
        D.SCORE <- sum(DEALER$VALUE2)
    } else {
        D.SCORE <- sum(DEALER$VALUE1)
    }
    insurance_filter(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
}

# If the dealer's face-up card is an ace, offer the player insurance.
insurance_filter <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK) {
    if(DEALER$VALUE1[1] == 1) {
        print("You have the option of placing an insurance bet.")
        print("Do you want to place an insurance bet?")
        CHOICE <- readline("[yes / no] > ")
        if(CHOICE == "no") {
            print("You have declined to place an insurance bet. Continuing with normal play.")
            init_decision(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
        } else if(CHOICE == "yes") {
            print("You have chosen to place an insurance bet.")
            print(paste("You may bet up to one-half of your original bet. ($", BET/2, ")", sep = ""))
            insurance_bet(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
        } else {
            insurance_filter(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
        }
    } else {
        init_decision(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
    }
}

# The function that plays out the insurance side bet.
insurance_bet <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK) {
    print("How much do you want to bet?")
    options(warn=-1)
    SIDE.BET <- as.numeric(readline("$"))
    while(is.na(SIDE.BET)) {
        print("How much do you want to bet?")
        SIDE.BET <- as.numeric(readline("$"))
    }
    options(warn=0)
    if(SIDE.BET <= (BET/2)) {
        if(sum(DEALER$VALUE2) == 21) {
            print("Dealer looks at the face-down card.")
            print("Dealer has 21. Player wins the insurance bet.")
            print("Player is paid double the insurance bet.")
            CHIPS <- CHIPS + (SIDE.BET*2)
            print(paste("Your chip count is $", CHIPS, ".", sep = ""))
            init_decision(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
        } else {
            print("Dealer looks at the face-down card.")
            print("Dealer does not have 21. Player loses the insurance bet.")
            CHIPS <- CHIPS - SIDE.BET
            print(paste("Your chip count is $", CHIPS, ".", sep = ""))
            init_decision(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
        }
    } else {
        print(paste("Insurance bets may be no larger than one-half of the original bet. ($", BET/2, ")", sep = ""))
        insurance_bet(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
    }
}

# A filter function. If someone wins with the initial two cards, ends the hand and
# calls the replay function. Otherwise continues the action.
init_decision <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK) {
    if(sum(DEALER$VALUE2) != 21 & sum(PLAYER$VALUE2) == 21) {
        print(paste("Player's ", PLAYER$CARD[1], " and ", PLAYER$CARD[2],
                    " make 21. You've won $", (BET*1.5), ".", sep = ""))
        print(paste("Your chip count is $", CHIPS + (BET*1.5), ".", sep = ""))
        CHIPS <- CHIPS + (BET*1.5)
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(sum(DEALER$VALUE2) == 21 & sum(PLAYER$VALUE2) != 21) {
        print(paste("Seeing a visible ", DEALER$CARD[1],
                    ", Dealer flips the face-down card, revealing a ", DEALER$CARD[2],
                    ".", sep = ""))
        print(paste("Dealer's 21 beats your hand of ", PLAYER$CARD[1], " and ",
                    PLAYER$CARD[2], ". You lose your bet.", sep = ""))
        print(paste("Your chip count is $", CHIPS - BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(sum(DEALER$VALUE2) == 21 & sum(PLAYER$VALUE2) == 21) {
        print(paste("Player's ", PLAYER$CARD[1], " and ", PLAYER$CARD[2],
                    " make 21.", sep = ""))
        print(paste("Seeing a visible ", DEALER$CARD[1],
                    "Dealer flips the face-down card, revealing a ", DEALER$CARD[2],
                    ".", sep = ""))
        print("Player and Dealer both have 21. The round is a draw.")
        print(paste("Your chip count is $", CHIPS, ".", sep = ""))
        play_quit(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK)
    } else {
        PL <- PLAYER[1,]
        PR <- PLAYER[2,]
        hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM=1,PL,PR,SPLIT.HAND=0)
    }
}

# A function to allow the player to choose to play again or quit.
# If play again is chosen and the deck is near the end, the deck is shuffled.
play_quit <- function(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK) {
    print("Do you want to play again or quit?")
    CHOICE <- readline("[play / quit] > ")
    if(CHOICE == "play" & COUNT < 238) {
        wager_fun(CHIPS,COUNT,SHUFFLED.DECK)
    } else if(CHOICE == "play" & COUNT > 237) {
        deck_shuffler(CHIPS,COUNT)
    } else if(CHOICE == "quit") {
        print(paste("Thank you for playing. You cash out at $", CHIPS, ".", sep = ""))
    } else {
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    }
}

# A function that allows the player to choose between hitting, standing, splitting pairs, or doubling down.
hit_stand <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND) {
    if((sum(PLAYER$VALUE1[1:2]) > 8) & (sum(PLAYER$VALUE1[1:2]) < 12) & (BET*2 <= CHIPS) & (HIT.NUM == 1)) {
        if((PLAYER$CARD[1] == 5) & (PLAYER$CARD[2] == 5)) {
        print("Do you want to double down, hit, split pairs, or stand?")
        CHOICE <- readline("[double / hit / split / stand] > ")
        } else {
        print("Do you want to double down, hit, or stand?")
        CHOICE <- readline("[double / hit / stand] > ")
        }
        if(CHOICE == "hit") {
            hit_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else if(CHOICE == "double") {
            print("You have chosen to double down. The dealer deals you one card face-down.")
            BET <- BET * 2
            print(paste("Your original bet has been doubled to $", BET, ".", sep = ""))
            print("It is now the dealer's turn. The dealer now turns over the dealer's face-down card.")
            PLAYER <- rbind(PLAYER,SHUFFLED.DECK[COUNT,])
            COUNT <- COUNT + 1
            stand_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else if(CHOICE == "stand") {
            print("The dealer now turns over the face-down card.")
            stand_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else if(CHOICE == "split") {
            if((PLAYER$CARD[1] != 5) | (PLAYER$CARD[2] != 5)) {
                hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
            } else {
                print("You have chosen to split your hand.")
                print("You now have two bets outstanding, one on each hand, each equal to your original bet.")
                print("You will now play each hand in turn.")
                split_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND=1)
            }
        } else {
            hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        }
    } else if((PLAYER$CARD[1] == PLAYER$CARD[2]) & (BET*2 <= CHIPS) & (HIT.NUM == 1)) {
        print("Do you want to double hit, split pairs, or stand?")
        CHOICE <- readline("[hit / stand / split] > ")
        if(CHOICE == "hit") {
            hit_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else if(CHOICE == "stand") {
            print("The dealer now turns over the face-down card.")
            stand_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else if(CHOICE == "split") {
            print("You have chosen to split your hand.")
            print("You now have two bets outstanding, one on each hand, each equal to your original bet.")
            print("You will now play each hand in turn.")
            split_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND=1)
        } else {
            hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        }
    } else {
        print("Do you want to hit or stand?")
        CHOICE <- readline("[hit / stand] > ")
        if(CHOICE == "hit") {
            hit_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else if(CHOICE == "stand") {
            stand_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else {
            hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        }
    }
}

split_fun <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND) {
    HIT.NUM <- HIT.NUM + 1
    if(PLAYER$CARD[1] == "A") {
        print("With split aces, you are given one card per ace and may not draw again.")
        rbind(PL,SHUFFLED.DECK[COUNT,])
        COUNT <- COUNT + 1
        rbind(PR,SHUFFLED.DECK[COUNT,])
        COUNT <- COUNT + 1
        print(paste("Your left hand is ", PL$CARD[1], " and ", PL$CARD[2], ".", sep = ""))
        print(paste("Your right hand is ", PR$CARD[1], " and ", PR$CARD[3], ".", sep = ""))
        print("It is now the dealer's turn.")
        stand_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
    } else if(PLAYER$CARD[1] != "A" & SPLIT.HAND == 1) {
        print("Now playing your left hand. Dealer issues player one card.")
        hit_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
    } else {
        print("Now playing your right hand. Dealer issues player one card.")
        hit_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
    }
    
}

# What happens when the player chooses to hit.
hit_fun <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND) {
    HIT.NUM <- HIT.NUM + 1
    if(SPLIT.HAND == 0) {
        PLAYER <- rbind(PLAYER,SHUFFLED.DECK[COUNT,])
        COUNT <- COUNT + 1
        print(paste("Your cards are ",
                    paste(PLAYER$CARD[1:(nrow(PLAYER)-1)], collapse = ", "),
                    ", and ", PLAYER$CARD[nrow(PLAYER)], ".", sep = ""))
        if(sum(PLAYER$VALUE1) > 21) {
            print("Player has busted; you lose your bet.")
            CHIPS <- CHIPS - BET
            print(paste("Your chip count is $", CHIPS, ".", sep = ""))
            play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
        } else {
            hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        }
    } else if(SPLIT.HAND == 1) {
        PL <- rbind(PL,SHUFFLED.DECK[COUNT,])
        COUNT <- COUNT + 1
        print(paste("Your cards are ",
                    paste(PL$CARD[1:(nrow(PL)-1)], collapse = ", "),
                    ", and ", PL$CARD[nrow(PL)], ".", sep = ""))
        if(sum(PL$VALUE1) > 21) {
            print("Player has busted left hand.")
            print("Moving on to player's right hand.")
            split_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND=2)
        } else {
            hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        }
    } else {
        PR <- rbind(PR,SHUFFLED.DECK[COUNT,])
        COUNT <- COUNT + 1
        print(paste("Your cards are ",
                    paste(PR$CARD[1:(nrow(PR)-1)], collapse = ", "),
                    ", and ", PR$CARD[nrow(PR)], ".", sep = ""))
        if(sum(PL$VALUE1) > 21) {
            print("Player has busted right hand.")
            print("It is now the dealer's turn.")
            print("The dealer now turns over the face-down card.")
            stand_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else {
            hit_stand(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        }
    }
}

# What happens when the player chooses to stand.
stand_fun <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND) {
    HIT.NUM <- HIT.NUM + 1
    if(SPLIT.HAND == 1 & PLAYER$CARD[1] != "A") {
        split_fun(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND=2)
    } else {
        print(paste("The dealer's cards are ", DEALER$CARD[1], " and ", DEALER$CARD[2], ".", sep = ""))
        dealer_play(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
    }
}

# The dealer play engine.
dealer_play <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND) {
    if(D.SCORE > 16) {
        print("The dealer stands.")
        win_lose(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
    } else {
        NEW.CARD <- SHUFFLED.DECK[COUNT,]
        COUNT <- COUNT + 1
        DEALER <- rbind(DEALER,NEW.CARD)
        print("The dealer hits.")
        print(paste("The dealer's cards are ",
                    paste(DEALER$CARD[1:(nrow(DEALER)-1)], collapse = ", "),
                    ", and ", DEALER$CARD[nrow(DEALER)], ".", sep = ""))
        if(D.SCORE + NEW.CARD$VALUE2 > 16 & D.SCORE + NEW.CARD$VALUE2 <= 21) {
            D.SCORE <- D.SCORE + NEW.CARD$VALUE2
            print("The dealer stands.")
            win_lose(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else if(D.SCORE + NEW.CARD$VALUE1 > 21) {
            D.SCORE <- D.SCORE + NEW.CARD$VALUE1
            print("The dealer stands.")
            win_lose(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        } else {
            D.SCORE <- D.SCORE + NEW.CARD$VALUE1
            dealer_play(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND)
        }
    }
}

# After both the dealer and the player have played their turns, this function
# announces the winner and the loser, then gives the player the option to play again.
win_lose <- function(BET,CHIPS,COUNT,DEALER,D.SCORE,PLAYER,SHUFFLED.DECK,HIT.NUM,PL,PR,SPLIT.HAND) {

    if(PLAYER$CARD[1] != PLAYER$CARD[2]) {

    print(paste("The dealer's cards are ",
                paste(DEALER$CARD[1:(nrow(DEALER)-1)], collapse = ", "),
                ", and ", DEALER$CARD[nrow(DEALER)], ".", sep = ""))
    print(paste("Your cards are ",
                paste(PLAYER$CARD[1:(nrow(PLAYER)-1)], collapse = ", "),
                ", and ", PLAYER$CARD[nrow(PLAYER)], ".", sep = ""))
    P.SCORE.BASE <- sum(PLAYER$VALUE1)
    if(sum(PLAYER$VALUE1) == sum(PLAYER$VALUE2)) {
        P.SCORE <- P.SCORE.BASE
    } else if(sum(PLAYER$VALUE1) != sum(PLAYER$VALUE2) & sum(PLAYER$VALUE1) < 12) {
        P.SCORE <- P.SCORE.BASE + 10
    } else {
        P.SCORE <- P.SCORE.BASE
    }
    if(D.SCORE > 21 & P.SCORE < 22) {
        print("Dealer busts. Player wins this hand.")
        print(paste("Your chip count is $", CHIPS+BET, ".", sep = ""))
        CHIPS <- CHIPS + BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(P.SCORE > 21 & D.SCORE < 22) {
        print("Player busts, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(D.SCORE > 21 & P.SCORE > 21) {
        print("Dealer and Player both bust, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(D.SCORE > P.SCORE) {
        print("Dealer wins this hand, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(P.SCORE > D.SCORE) {
        print("Player wins this hand.")
        print(paste("Your chip count is $", CHIPS+BET, ".", sep = ""))
        CHIPS <- CHIPS + BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else {
        print("The round is a draw.")
        print(paste("Your chip count is $", CHIPS, ".", sep = ""))
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    }

    } else {  # PLAYER$CARD[1] != PLAYER$CARD[2]

    print(paste("The dealer's cards are ",
                paste(DEALER$CARD[1:(nrow(DEALER)-1)], collapse = ", "),
                ", and ", DEALER$CARD[nrow(DEALER)], ".", sep = ""))
    print(paste("Your left hand is ",
                paste(PL$CARD[1:(nrow(PL)-1)], collapse = ", "),
                ", and ", PL$CARD[nrow(PL)], ".", sep = ""))
    print(paste("Your right hand is ",
                paste(PR$CARD[1:(nrow(PR)-1)], collapse = ", "),
                ", and ", PR$CARD[nrow(PR)], ".", sep = ""))
    PL.SCORE.BASE <- sum(PL$VALUE1)
    PR.SCORE.BASE <- sum(PR$VALUE1)
    if(sum(PL$VALUE1) == sum(PL$VALUE2)) {
        PL.SCORE <- PL.SCORE.BASE
    } else if(sum(PL$VALUE1) != sum(PL$VALUE2) & sum(PL$VALUE1) < 12) {
        PL.SCORE <- PL.SCORE.BASE + 10
    } else {
        PL.SCORE <- PL.SCORE.BASE
    }
    if(sum(PR$VALUE1) == sum(PR$VALUE2)) {
        PR.SCORE <- PR.SCORE.BASE
    } else if(sum(PR$VALUE1) != sum(PR$VALUE2) & sum(PR$VALUE1) < 12) {
        PR.SCORE <- PR.SCORE.BASE + 10
    } else {
        PR.SCORE <- PR.SCORE.BASE
    }

    if(D.SCORE > 21 & PL.SCORE < 22) {
        print("Dealer busts. Player's left hand wins this round.")
        print(paste("Your chip count is $", CHIPS+BET, ".", sep = ""))
        CHIPS <- CHIPS + BET
    } else if(PL.SCORE > 21 & D.SCORE < 22) {
        print("Player's left hand busts, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
    } else if(D.SCORE > 21 & PL.SCORE > 21) {
        print("Player's left hand and Dealer both bust, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
    } else if(D.SCORE > PL.SCORE) {
        print("Dealer beats Player's left hand, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
    } else if(PL.SCORE > D.SCORE) {
        print("Player's left hand beats Dealer.")
        print(paste("Your chip count is $", CHIPS+BET, ".", sep = ""))
        CHIPS <- CHIPS + BET
    } else {
        print("The Player's left hand ties with the Dealer.")
        print(paste("Your chip count is $", CHIPS, ".", sep = ""))
    }

    if(D.SCORE > 21 & PR.SCORE < 22) {
        print("Dealer busts. Player's right hand wins this round.")
        print(paste("Your chip count is $", CHIPS+BET, ".", sep = ""))
        CHIPS <- CHIPS + BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(PR.SCORE > 21 & D.SCORE < 22) {
        print("Player's right hand busts, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(D.SCORE > 21 & PR.SCORE > 21) {
        print("Player's right hand and Dealer both bust, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(D.SCORE > PR.SCORE) {
        print("Dealer beats Player's right hand, you lose your bet.")
        print(paste("Your chip count is $", CHIPS-BET, ".", sep = ""))
        CHIPS <- CHIPS - BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else if(PR.SCORE > D.SCORE) {
        print("Player's right hand beats Dealer.")
        print(paste("Your chip count is $", CHIPS+BET, ".", sep = ""))
        CHIPS <- CHIPS + BET
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    } else {
        print("The Player's right hand ties with the Dealer.")
        print(paste("Your chip count is $", CHIPS, ".", sep = ""))
        play_quit(BET,CHIPS,COUNT,DEALER,PLAYER,SHUFFLED.DECK)
    }


    }  # PLAYER$CARD[1] != PLAYER$CARD[2]

}

# Prints out the rules of the game.
rules_fun <- function(COUNT) {
    print("Blackjack")
    print("")
    print("The Pack")
    print("The standard 52-card pack is used, but six decks of cards (312 cards)")
    print("are shuffled together. In addition, the dealer uses marker to indicate")
    print("when it will be time for the cards to be reshuffled.")
    print("")
    print("Object of the Game")
    print("Each participant attempts to beat the dealer by getting a count as")
    print("close to 21 as possible, without going over 21.")
    print("")
    print("Card Values/Scoring")
    print("It is up to each individual player if an ace is worth 1 or 11. Face")
    print("cards are 10 and any other card is its pip value.")
    print("")
    print("The Shuffle")
    print("The dealer shuffles the pack. A marker is placed so that the last 75")
    print("cards or so will not be used. (Not dealing to the bottom of all the")
    print("cards makes it more difficult for professional card counters to")
    print("operate effectively.)")
    print("")
    print("Betting")
    print("Before the deal begins, each player places a bet, in chips, in front")
    print("of him in the designated area. The minimum limit is established at $2.")
    print("")
    print("The Deal")
    print("When the player has wagered, the dealer gives one card face up to the")
    print("player, and then one card face up to himself. Another card is then")
    print("dealt face up to the player, but the dealer takes his second card face")
    print("down. Thus, the player receives two cards face up, and the dealer")
    print("receives one card face up and one card face down.")
    print("")
    print("Naturals")
    print("If a player's first two cards are an ace and a 'ten-card' (a picture")
    print("card or 10), giving him a count of 21 in two cards, this is a natural")
    print("or 'blackjack.' If any player has a natural and the dealer does not,")
    print("the dealer immediately pays that player one and a half times the")
    print("amount of his bet. If the dealer has a natural, he immediately")
    print("collects the bets of all players who do not have naturals, (but no")
    print("additional amount). If the dealer and another player both have")
    print("naturals, the bet of that player is a stand-off (a tie), and the")
    print("player takes back his chips. If the dealer's face-up card is a ten-")
    print("card or an ace, he looks at his face-down card to see if the two cards")
    print("make a natural. If the face-up card is not a ten-card or an ace, he")
    print("does not look at the face-down card until it is the dealer's turn to")
    print("play.")
    print("")
    print("The Play")
    print("The player goes first and must decide whether to 'stand' (not ask for")
    print("another card) or 'hit' (ask for another card in an attempt to get")
    print("closer to a count of 21, or even hit 21 exactly). Thus, a player may")
    print("stand on the two cards originally dealt him, or he may ask the dealer")
    print("for additional cards, one at a time, until he either decides to stand")
    print("on the total (if it is 21 or under), or goes 'bust' (if it is over 21).")
    print("In the latter case, the player loses and the dealer collects the bet")
    print("wagered. The combination of an ace with a card other than a ten-card")
    print("is known as a 'soft hand,' because the player can count the ace as a 1")
    print("or 11, and either draw cards or not. For example with a 'soft 17' (an")
    print("ace and a 6), the total is 7 or 17. While a count of 17 is a good hand,")
    print("the player may wish to draw for a higher total. If the draw creates a")
    print("bust hand by counting the ace as an 11, the player simply counts the")
    print("ace as a 1 and continues playing by standing or 'hitting' (asking the")
    print("dealer for additional cards, one at a time).")
    print("")
    print("The Dealer's Play")
    print("When the dealer has served every player, his face-down card is turned")
    print("up. If the total is 17 or more, he must stand. If the total is 16 or")
    print("under, he must take a card. He must continue to take cards until the")
    print("total is 17 or more, at which point the dealer must stand. If the")
    print("dealer has an ace, and counting it as 11 would bring his total to 17")
    print("or more (but not over 21), he must count the ace as 11 and stand. The")
    print("dealer's decisions, then, are automatic on all plays, whereas the")
    print("player always has the option of taking one or more cards.")
    print("")
    print("Signaling Intentions")
    print("When a player's turn comes, he can enter 'hit'. When the player")
    print("decides to stand, he can enter 'stand'.")
    print("")
    print("Splitting Pairs")
    print("If a player's first two cards are of the same denomination, such as")
    print("two jacks or two sixes, he may choose to treat them as two separate")
    print("hands when his turn comes around. The amount of his original bet then")
    print("goes on one of the cards, and an equal amount must be placed as a bet")
    print("on the other card. The player first plays the hand to his left by")
    print("standing or hitting one or more times; only then is the hand to the")
    print("right played. The two hands are thus treated separately, and the")
    print("dealer settles with each on its own merits. With a pair of aces, the")
    print("player is given one card for each ace and may not draw again. Also, if")
    print("a ten-card is dealt to one of these aces, the payoff is equal to the")
    print("bet (not one and one-half to one, as with a blackjack at any other")
    print("time).")
    print("")
    print("Doubling Down")
    print("Another option open to the player is doubling his bet when the")
    print("original two cards dealt total 9, 10, or 11. When the player's turn")
    print("comes, he places a bet equal to the original bet, and the dealer gives")
    print("him just one card, which is placed face down and is not turned up")
    print("until the bets are settled at the end of the hand. With two fives, the")
    print("player may split a pair, double down, or just play the hand in the")
    print("regular way. Note that the dealer does not have the option of")
    print("splitting or doubling down.")
    print("")
    print("Insurance")
    print("When the dealer's face-up card is an ace, any of the players may make")
    print("a side bet of up to half the original bet that the dealer's face-down")
    print("card is a ten-card, and thus a blackjack for the house. Once all such")
    print("side bets are placed, the dealer looks at his hole card. If it is a")
    print("ten-card, it is turned up, and those players who have made the")
    print("insurance bet win and are paid double the amount of their half-bet - a")
    print("2 to 1 payoff. When a blackjack occurs for the dealer, of course, the")
    print("hand is over, and the players' main bets are collected - unless a")
    print("player also has blackjack, in which case it is a stand-off. Insurance")
    print("is invariably not a good proposition for the player, unless he is")
    print("quite sure that there are an unusually high number of ten-cards still")
    print("left undealt.")
    print("")
    print("Settlement")
    print("A bet once paid and collected is never returned. Thus, one key")
    print("advantage to the dealer is that the player goes first. If the player")
    print("goes bust, he has already lost his wager, even if the dealer goes bust")
    print("as well. If the dealer goes over 21, he pays each player who has stood")
    print("the amount of that player's bet. If the dealer stands at 21 or less,")
    print("he pays the bet of any player having a higher total (not exceeding 21)")
    print("and collects the bet of any player having a lower total. If there is a")
    print("stand-off (a player having the same total as the dealer), no chips are")
    print("paid out or collected.")
    print("")
    print("Reshuffling")
    print("When each player's bet is settled, the dealer gathers in that player's")
    print("cards and sets them aside. The dealer continues to deal from the shoe")
    print("until he comes to the marker, which indicates that it is time to")
    print("reshuffle. Once that round of play is over, the dealer shuffles all")
    print("the cards, places the cards in the shoe, and the game continues.")
    print("")
    print("Note")
    print("These rules have been taken from www.bicyclecards.com/how-to-play/blackjack/")
    print("and have been modified to reflect the specifics of this version of blackjack.")
    chip_buy(COUNT)
}

# Build the basic deck of cards
DECK <- data.frame(
    CARD = c(unlist(lapply(2:10, function(i) rep(i,4))),
             rep("J",4),rep("Q",4),rep("K",4),rep("A",4)),
    SUIT = rep(c("C","D","H","S"),13),
    VALUE1 = c(unlist(lapply(2:10, function(i) rep(i,4))),
               rep(10,12),rep(1,4)),
    VALUE2 = c(unlist(lapply(2:10, function(i) rep(i,4))),
               rep(10,12),rep(11,4))
)

# Create a 6-deck shoe of cards from the basic deck of cards
DECK <- rbind(DECK,DECK,DECK,DECK,DECK,DECK)

# Start the program
init_fun()