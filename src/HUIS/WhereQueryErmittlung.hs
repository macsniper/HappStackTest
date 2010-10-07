module HUIS.WhereQueryErmittlung(whereQueryZu) where

import Text.ParserCombinators.Parsec

type SQL = String
type TextfeldInhalt = String

whereQueryZu:: TextfeldInhalt→  SQL
whereQueryZu textfeldInhalt =
    case parse whereQuerySuche "" textfeldInhalt of
        Left fehlerMeldung→  "Passt nicht: " ⊕ show fehlerMeldung
        Right whereQuery→  whereQuery

whereQuerySuche:: Parser SQL
whereQuerySuche = do
    vn ←  parseFirstname
    nn ←  try parseLastname <|> return ""
    return $ if vn ≡ "" then ""
           else "WHERE " ⊕ (if nn=="" then "pgd_nn= '" ⊕ vn ⊕ "'"
                            else "pgd_vn='" ⊕ vn ⊕ "', pgd_nn='" ⊕ nn ⊕ "'") ⊕ ";"

parseFirstname:: Parser String
parseFirstname = do
    try $ skipMany (oneOf " 1234567890")
    many1 letter

parseLastname:: Parser String
parseLastname = do
    string " "
    many1 letter



