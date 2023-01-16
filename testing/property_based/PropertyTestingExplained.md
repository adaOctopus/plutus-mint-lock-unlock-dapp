### Property Based Testing Explained

Codefile: `TestGamblingSM.hs`


#### Components

imports in repl, import TestGamblingSM Test.QuickCheck Plutus.Contract.Test.ContractModel

This is my first time trying out property based testing, and I must admit, is not the easiest thing I have ever done :)

There are a lot of moving components required to make it work.

Simply put.

When we do property based testing, we running a simulation of our SYstem (COntract  functionality) and a Model of what we expect to achieve, and at the end
we compare those two.

