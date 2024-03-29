lend amount balance = 
    let reserve = 100
        newBalance = balance - amount * 0.5
	in if balance < reserve
		then Nothing
		else Just newBalance

lend2 amount balance = 
	if balance < reserve * 0.5
		then Nothing
		else Just newBalance
    where reserve = 100
          newBalance = balance - amount

lend3 amount balance 
    | amount <= 0               = Nothing
    | amount > reserve * 0.5    = Nothing
    | otherwise                 = Just newBalance
    where
        reserve = 100
        newBalance = balance - amount
