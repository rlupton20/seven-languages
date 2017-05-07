number := Random value(1,100) floor
i := 10;
guess := 1000
previous := 1000

stdin := File standardInput

while( i > 0 and guess != number,
        previous = guess;
        guess = stdin readLine asNumber;
        if(guess == number, "Success!" println,
                if((number - guess)**2 < (number - previous)**2,
                        "Warmer" println,
                        "Colder" println));)

stdin close