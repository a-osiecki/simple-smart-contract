module Contract where

import           Ledger
import qualified Ledger.Ada                    as Ada
import           Ledger.Value

import           Ledger.Constraints             ( TxConstraints )
import qualified PlutusTx
import           PlutusTx.Builtins
import           PlutusTx.Numeric  as Num
import           PlutusTx.Prelude

-- | Contract Types

newtype MyRedeemer = Redeemer ()

newtype MyDatum = Datum ()

newtype MyParameter = Parameter { stateNFT :: AssetClass }

data ObservableState = ObservableState { osParameter :: !MyParameter
                                       , osTxId :: !TxId
                                       }
    deriving (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance OpenApi.ToSchema ObservableState

PlutusTx.makeLift ''MyParameter
PlutusTx.makeLift ''ObservableState
PlutusTx.unstableMakeIsData ''ObservableState
PlutusTx.unstableMakeIsData ''MyDatum
PlutusTx.unstableMakeIsData ''MyRedeemer

-- | OffChain logic
type MySchema = Endpoint "create" ()
            .\/ Endpoint "consume" ()

run ::  Contract (Last ObservableState) MySchema Text ()
run sett txRef = do
    obsState <- start
    logInfo @String $ "Contract working!"
    Contract.tell $ Last $ Just obsState
    endpoints $ osParameter obsState
