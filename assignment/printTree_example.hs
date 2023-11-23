-- https://matthewmcgonagle.github.io/blog/2017/10/10/PrintingABinaryTreeInHaskell

data Tree a = Empty | Node a (Tree a) (Tree a)