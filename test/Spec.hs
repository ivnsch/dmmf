import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import PlaceOrderWorkflow
import Types.CheckedAddress
import qualified Data.List.NonEmpty as NE
import qualified Types.OrderLineId
import qualified Types.OrderId as OrderId
import qualified Types.ProductCode as ProductCode
import qualified Types.UnitQuantity as UnitQuantity
import qualified Types.OrderQuantity as OrderQuantity
import qualified Types.CustomerInfo as CustomerInfo
import qualified Types.PersonalName as PersonalName
import SharedTypes
import qualified Types.Address as Address
import qualified Types.ValidatedOrder as ValidatedOrder
import Types.UnvalidatedCustomerInfo
import Types.OrderLine
import UnvalidatedOrder
import UnvalidatedOrderLine
import Types.String50

main :: IO ()
main = hspec $
  describe "Order validation" $ do
    let orderIdStr = "1"
    let orderLineIdStr = "1"
    let orderId = OrderId.create orderIdStr
    let orderLineId = OrderLineId.create orderLineIdStr
    let unvalidatedCustomerInfo = UnvalidatedCustomerInfo "a" "a" "a"
    let shippingUnvalidatedAddress = UnvalidatedAddress "a"
    let unvalidatedOrderline = UnvalidatedOrderLine orderLineIdStr orderIdStr "1" 1
    let unvalidatedOrderLines = NE.fromList [unvalidatedOrderline]

    let checkAddressExists _ = CheckedAddress.CheckedAddress "a" "a" "a" "a" "a" "1"
    let unvalidatedOrder = UnvalidatedOrder orderIdStr unvalidatedCustomerInfo shippingUnvalidatedAddress unvalidatedOrderLines

    let customerInfo = CustomerInfo.CustomerInfo (PersonalName.PersonalName (String50.string50 "a") (String50.string50 "a")) (EmailAddress "a")
    let dummyStr = String50.string50 "a" :: String50
    let shippingAddress = Address.Address dummyStr dummyStr dummyStr dummyStr (City dummyStr) (ZipCode "1")
    let billingAddress = shippingAddress
    let orderLine = OrderLine orderLineId orderId (ProductCode.Widget "1") (OrderQuantity.UnitQuantity $ UnitQuantity.create 1)
    let orderLines = NE.fromList [orderLine]

    it "If product exists, validation succeeds" $ do
      let checkProductCodeExists _ = True
      let validatedOrder = validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder

      validatedOrder `shouldBe` ValidatedOrder.ValidatedOrder orderId customerInfo shippingAddress billingAddress orderLines

      -- TODO fails with "did not get expected exception: ErrorCall", though in logs we see ErrorCall Invalid: 1 if we run without evaluate (last commented line)
    -- it "If product doesn't exist, validation fails" $ do
    --   let checkProductCodeExists _ = False
    --   let validatedOrder = validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder

    --   evaluate validatedOrder `shouldThrow` errorCall "Invalid: 1"
    --   -- evaluate (head []) `shouldThrow` errorCall "Prelude.head: empty list"
    --   -- validatedOrder `shouldBe` ValidatedOrder.ValidatedOrder orderId customerInfo shippingAddress billingAddress orderLines
