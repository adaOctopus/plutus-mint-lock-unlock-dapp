### Details of how to use this repo locally.


The scope of this repository is to help you practice, and review a more realistic end-to-end case scenario of Plutus contracts and how to interact with them.
The main goal is adoption. And let's be real for a second, with the current state of the learning curve for Plutus, adoption of Cardano is just wishful thinking.

Maybe this repo is also not the greatest tool, but if you learn at least one new thing that will help you comprehend Cardano and Plutus better, this repo fulfilled its purpose.

So, let's begin...


### Build the project

PS: This guide assumes you have nix & cabal installed (with iohk cache properly setup.)

In order to play with it, you need to build it first with `cabal`.

1. You need to have plutus-apps repo cloned locally (if you don't have that..common..what you been doing?)
Joke's aside...
run: `git clone https://github.com/input-output-hk/plutus-apps`
2. I am using james-iohk/plutus-scripts when reviewing contracts so im using this tag: 87b647b05902a7cef37340fda9acb175f962f354
3. once you clone it, checkout that tag inside the root directory of plutus apps: run inside /plutus-apps `git checkout 87b647b05902a7cef37340fda9acb175f962f354`
4. Once you do, enter nix-shell (make sure you have flakes and everything setup as mentioned in the PS earlier.)
5. Now that you are inside nix shell, run `cd ..` to move out of the directory.
-- WE REMAIN INSIDE NIX SHELL.
6. Now clone this WONDERFUL repo (jk jk) in case you haven't: run `git clone https://github.com/tas2017/plutus-mint-lock-unlock-dapp.git`
7. enter inside the directory : `cd plutus-mint-lock-unlock-dapp`
8. NOw run `cabal build` (you are inside a nix shell)
9. EVerything should be done and compiled properly.
10. If not search the error or message in the PlutusWizards discord https://discord.gg/hvGtC7Xh