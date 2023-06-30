import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ do
            applyPoly (P [6, 4, 3, 2, 1]) 0 `shouldBe` 6
            applyPoly (P [1, 2, 3]) 5 `shouldBe` (1 + 2 * (5^1) + 3* (5^2))
        it "(==)" $ do
            (P [1, 1, 1]) == (P [1, 1, 1, 0, 0]) `shouldBe` True
            (P [1, 0, 1]) == (P [1, 1, 1, 0, 0]) `shouldBe` False
        it "show" $ do
            show (P [1, 2, 3]) `shouldBe` "3 * x^2 + 2 * x + 1"
            show (P [0, 1, 2, 3, 0]) `shouldBe` "3 * x^3 + 2 * x^2 + x"
        it "plus" $ do
            plus (P [1, 2, 3]) (P [1, 2, 3]) `shouldBe` (P [2, 4, 6])
            plus (P [1, 0, 1]) (P [0, 1, 0]) `shouldBe` (P [1, 1, 1])
        it "times" $ do
            times (P [1, 6]) (P [3, -5, 2]) `shouldBe` (P [3, 13, -28, 12])
            times (P [1, 1]) (P [1, 0, 1]) `shouldBe` (P [1, 1, 1, 1])
        it "nderiv" $ do
            nderiv 3 (P [3, 13, -28, 12]) `shouldBe` (P [72])
            nderiv 1 (P [1, 1, 1, 1]) `shouldBe` P [1, 2, 3]
    describe "simpleLang" $ do
        it "run" $ do
            runSimpler empty (DAssign "A" (Val 10)) "A" `shouldBe` 10
            run empty (Incr "A") "A" `shouldBe` 1
        it "state" $ do
            empty "x" `shouldBe` 0
        it "eval" $ do
            eval empty (Val 5) `shouldBe` 5
            eval (extend empty "a" 1) (Op (Val 1) Eql (Var "a")) `shouldBe` 1
        it "desugar" $ do 
            desugar (Incr "A") `shouldBe` DAssign "A" (Op (Var "A") Plus (Val 1))
