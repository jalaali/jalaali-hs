import Test.Hspec
import qualified Data.Time.Calendar.JalaaliSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Data.Time.Calendar.Jalaali" Data.Time.Calendar.JalaaliSpec.spec
