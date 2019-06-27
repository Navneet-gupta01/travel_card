# Oyster Application

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/63cecc982ab740059f221cf84c2d0114)](https://app.codacy.com/app/Navneet-gupta01/travel_card?utm_source=github.com&utm_medium=referral&utm_content=Navneet-gupta01/travel_card&utm_campaign=Badge_Grade_Dashboard)

## To Start the console Application
```
sbt run
```
and follow the instruction.


## Further Improvement:
1.	Error handling can be improved.
2. 	Refactor Some code to make it more generic.
3. 	Some Edge cases missing , for Ex: User touching multiple time at same station within short span of time while system still processing, they should not be charged more than once.  
4.	For now not storing journey history as not required. Can be incooperated further into the solution.     


## Testing The Application

```
sbt test
```

* Any Illegal Station Entry will deduct maximum fare for (CheckIn/Checkout Pair).
* Edge cases Not Handled  
	-- User touching multiple time at same station within short span of time while system still processing, they should not be charged more than once.  
	-- Checking IN and Checking OUT at same station within some stipulated timeout, User should not be charged, ore should be refunded. Ex: User by mistake checkin and then tried to avoid the trip by checkout immediately.  
	-- Checking for illegal station codes.  Illegal station codes checkin/checkout will be charged at max rate.  
	
### Test case for main application is not passing as its hard to check for randomness. We can introduce random typeclass and use differnet interpreters for testing and production environement. -- Incoperate in next phase.   