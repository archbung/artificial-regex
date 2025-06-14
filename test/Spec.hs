import Test.Hspec
import Test.QuickCheck
import Regex

main :: IO ()
main = hspec $ do
  describe "Regex.eval" $ do
    it "correctly evaluates simple regexes" $ do
      eval (parse "a") `shouldBe` ["a"]
      eval (parse "[ab]") `shouldBe` ["a","b"]
      eval (parse "(ab)") `shouldBe` ["ab"]
    it "correctly evaluates nested regexes" $ do
      eval (parse "[(ab)]") `shouldBe` ["ab"]
      eval (parse "[(ab)c]") `shouldBe` ["ab","c"]
      eval (parse "([ab]c)") `shouldBe` ["ac","bc"]
      eval (parse "[a[[bb]b[[b]]](c)(d)]") `shouldBe` ["a","b","c","d"]

  describe "Regex.simplify" $ do
    it "correctly simplifies regexes" $ do
      show (simplify (parse "[[ab]]")) `shouldBe` "[ab]"
      show (simplify (parse "((a)b)")) `shouldBe` "(ab)"
      show (simplify (parse "[a[[bb]b[[b]]](c)(d)]")) `shouldBe` "[abcd]"

    it "preserves the meaning" $ do
      property $ \r -> eval r == eval (simplify r)

    it "simplifies as far as possible" $ do
      property $ \r -> simplify (simplify r) == simplify r

  describe "Regex.solve" $ do
    it "correctly solves simple problems" $ do
      solve "[[a]b]\n(a(b))\n[a[[bb]b[[b]]](c)(d)]"
        `shouldBe` [(1,"(a(b))","(ab)"),(2,"[[a]b]","[ab]"),(4,"[a[[bb]b[[b]]](c)(d)]","[abcd]")]
