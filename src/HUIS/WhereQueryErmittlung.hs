module HUIS.WhereQueryErmittlung(whereQueryZu) where

import Text.ParserCombinators.Parsec

type SQL = String
type TextfeldInhalt = String

whereQueryZu:: TextfeldInhalt-> SQL
whereQueryZu textfeldInhalt =
    case parse whereQuerySuche "" textfeldInhalt of
        Left fehlerMeldung-> "Passt nicht: " ++ show fehlerMeldung
        Right whereQuery-> whereQuery

whereQuerySuche:: Parser SQL
whereQuerySuche = do
    vn <-vorname
    nn <-try nachname <|> return ""
    return $ if vn == "" then ""
           else "WHERE " ++ (if nn=="" then "pgd_vn= '" ++ vn ++ "'"
                            else "pgd_vn='" ++ vn ++ "', pgd_nn='" ++ nn ++ "'")

vorname:: Parser String
vorname = do
    try $ skipMany (oneOf " 1234567890")
    many1 letter

nachname:: Parser String
nachname = do
    string " "
    many1 letter

