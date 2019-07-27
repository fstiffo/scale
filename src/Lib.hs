module Lib
  ( someFunc
  ) where

import           Data.Decimal
import           Data.Time

someFunc :: IO ()
someFunc = putStrLn $ show $ cassa movimenti

data Condomino
  = Michela
  | Gerardo
  | Elena
  | Giulia
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Operazione
  = VersamentoQuote Condomino
                    Decimal
  | PagamentoScale
  | AltraSpesa String
               Decimal
  | AltroVersamento String
                    Decimal
  | Prestito Decimal
  | Restituzione Decimal

type Movimento = (Day, Operazione)

data Param = Param
  { costoScale     :: Decimal
  , numPulizieMese :: Int
  , quotaMensile   :: Decimal
  }

type Attuale = (Day, Param)

tempoZero = fromGregorian 2019 7 1

attuale =
  (tempoZero, Param {costoScale = 20, numPulizieMese = 2, quotaMensile = 12})

condomini = [Michela, Gerardo, Elena, Giulia]

movimenti =
  [ (tempoZero, AltroVersamento "Appianamento" 333)
  , (tempoZero, VersamentoQuote Michela 74)
  , (tempoZero, VersamentoQuote Gerardo 78)
  , (tempoZero, VersamentoQuote Elena 48)
  , (fromGregorian 2019 7 22, Prestito 500)
  , (fromGregorian 2019 7 11, PagamentoScale)
  ]

contabile :: Operazione -> Decimal
contabile (VersamentoQuote _ d) = d
contabile PagamentoScale        = negate $ costoScale $ snd (attuale)
contabile (AltraSpesa _ d)      = -d
contabile (AltroVersamento _ d) = d
contabile (Prestito d)          = -d
contabile (Restituzione d)      = d

cassa :: [Movimento] -> Decimal
cassa movs = sum $ map (\(_, o) -> contabile o) movimenti

tesoretto :: Day -> [Movimento] -> Decimal
tesoretto oggi movs = avanzo + altri
  where
    avanzo = 0
    altri =
      sum $
      map
        (\(_, o) ->
           case o of
             AltraSpesa _ d      -> -d
             AltroVersamento _ d -> d)
        movs
