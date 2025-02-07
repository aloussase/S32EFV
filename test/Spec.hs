import           Data.Text  (Text)
import           S32EFV
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  classifySpec
  aggregateSpec

classifySpec :: Spec
classifySpec = do
  describe "classify" $ do
    context "Banco Guayaquil" $ do
      let ph = mkBancoGuayaquilParseHandle

      it "correctly classifies single line as needs" $
        case classify ph "1\t02/02/2025\t03/02/2025\tNOTA DEBITO\t117271\tIVA SERVICIO DIGITAL AH\tMATRIZ\t$0.31\t$689.40\t$689.40\t\t\t" of
          Just (Needs m) -> do
            mDate m `shouldBe` "02/02/2025"
            mTipo m `shouldBe` "nota debito"
            mMonto m `shouldBe` "$0.31"
            mRef m `shouldBe` ""
          _ -> error "expected Needs but got Wants"

      it "correctly classifies single line as wants" $ do
        case classify ph "23\t28/01/2025\t28/01/2025\tNOTA DE DEBITO\t8315\tCOMPRA MAESTRO LOCAL\tMATRIZ\t$12.71\t$1,124.12\t$1,124.12\tLITTLE ITALY MALL DEL  GU009MDS26IPI5\t514440XXXXXX1000\tLITTLE ITALY MALL DEL  GU009MDS26IPI5" of
          Just (Wants m) -> do
            mDate m `shouldBe` "28/01/2025"
            mTipo m `shouldBe` "nota de debito"
            mMonto m `shouldBe` "$12.71"
            mRef m `shouldBe` "little italy mall del  gu009mds26ipi5"
          _ -> error "expected Wants but got Needs"

      it "returns Nothing when given a movement that is not an expense" $ example $
        case classify ph "6\t01/02/2025\t03/02/2025\tNOTA CREDITO\t503781\tIVA SERVICIO DIGITAL AH\tMATRIZ\t$0.37\t$764.51\t$764.51\tREVERSO\t" of
          Nothing -> return ()
          _       -> error "expected Nothing but got a something"

      it "returns Nothing when given an invalid line" $ example $
        case classify ph "6\t01/02/2025\t03/02/2025\tNOTA CREDITO\t503781\tIVA SERVICIO DIGITAL AH\tMATRIZ\t$0.37\t$764.51\t$764.51" of
          Nothing -> return ()
          _ -> error "expected invalid line not to be successfully parsed, but it somehow was :o"


aggregateSpec :: Spec
aggregateSpec = describe "aggregate" $ do
  it "correctly aggregates two needs" $ do
    let result = aggregate [mkNeed "$2.0", mkNeed "$1.50"]
    tNeeds result `shouldBe` 3.50
    tWants result `shouldBe` 0.0
    tSpent result `shouldBe` 3.50

  it "correctly aggregates two wants" $ do
    let result = aggregate [mkWant "$2.0", mkWant "$1.50"]
    tNeeds result `shouldBe` 0.0
    tWants result `shouldBe` 3.5
    tSpent result `shouldBe` 3.50

  it "correctly aggregates a want and a need" $ do
    let result = aggregate [mkWant "$2.0", mkNeed "$1.50"]
    tNeeds result `shouldBe` 1.50
    tWants result `shouldBe` 2.0
    tSpent result `shouldBe` 3.50

  it "correctly aggregates two wants and a two needs" $ do
    let result = aggregate [mkWant "$2.0", mkWant "$3.69", mkNeed "$1.50", mkNeed "$4.50"]
    tNeeds result `shouldBe` 6.0
    tWants result `shouldSatisfy` (\n -> n > 5.68 && n <= 5.69)

  it "correctly handles invalid amount strings" $ do
    let result = aggregate [mkWant "5.6", mkWant "$1.0"]
    tNeeds result `shouldBe` 0.0
    tWants result `shouldBe` 1.0
    tSpent result `shouldBe` 1.0

  it "correctly handles commas in thousands" $ do
    let result = aggregate [mkWant "$1,340.0"]
    tNeeds result `shouldBe` 0.0
    tWants result `shouldBe` 1340.0
    tSpent result `shouldBe` 1340.0


-- * Helpers

mkNeed :: Text -> Classification
mkNeed monto = Needs $ MkMovement "" "nota de debito" monto ""

mkWant :: Text -> Classification
mkWant monto = Wants $ MkMovement "" "nota de debito" monto ""
