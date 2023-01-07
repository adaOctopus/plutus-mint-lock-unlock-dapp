### StateMachine attemp

Here we will attempt to build a StateMachine (very simple one) and implement some more sophisticated type of testing: Propert Based Testing

This is my first attempt ever doing it, so don't judge.

### Use Case: Gambling StateMachine Contract


In our case, we will assume a good old gambling scenario.

1. A user will create a pool of funds, by Locking ADA to the GamblingStateMachine.
2. The user will lock those funds by using a simple password attached to it.
3. The initiation of the contract will be accompanied with a Token which will identify the machine.
4. Another user will attempt to unlock the funds by providing a password and an ADA bet.
5. For the user to bet, he/she needs to own the identification token in order to participate.
6. If the user wins the bet, unlocks and gets the stake, and the token gets returned.
7. If not, the amount remains locked with the additional bet.

This is a simpler version of the GameStateMachine.
It is simpler because we dont update the password and keep betting. If you win the bet, is done, go crazy :P


### State Transitions

1. Initialize the Gambling Machine
2. Lock the funds with the password
3. Finish the gambling machine.

### Gambling Inputs

1. Mint the identifier token
2. Take a bet
