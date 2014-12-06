module Munje.Logic ( declare
                   , expToDeclaration
                   ) where

import Munje.Parser

type Declaration = [String]

declare :: [Expression] -> [Declaration]
declare [] = []
declare (h : t) =
  addDeclaration h (declare t)

addDeclaration :: Expression -> [Declaration] -> [Declaration]
addDeclaration e acc =
  if any (\x -> x == (expToDeclaration e)) acc then acc else (expToDeclaration e) : acc

expToDeclaration :: Expression -> Declaration
expToDeclaration (Left (Bridi selbri sumtis)) =
  selbri : (sumtiToDeclaration sumtis)

sumtiToDeclaration :: [Sumti] -> Declaration
sumtiToDeclaration [] = []
sumtiToDeclaration (h : t) =
  h : (sumtiToDeclaration t)

