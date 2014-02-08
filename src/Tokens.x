
{
   {-# OPTIONS_GHC -w #-}

   module Tokens(Token(..), scanTokens) where
}

%wrapper "posn"


$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  \=                            { \p _ -> TkCharSym p '=' }
  \+                            { \p _ -> TkCharSym p '+' }
  \-                            { \p _ -> TkCharSym p '-' }
  \*                            { \p _ -> TkCharSym p '*' }
  \/                            { \p _ -> TkCharSym p '/' }
  \(                            { \p _ -> TkCharSym p '(' }
  \)                            { \p _ -> TkCharSym p ')' }
  \{                            { \p _ -> TkCharSym p '{' }                           
  \}                            { \p _ -> TkCharSym p '}' }
  \;                            { \p _ -> TkCharSym p ';' }
  \\                            { \p _ -> TkCharSym p '\\' }
  \.                            { \p _ -> TkCharSym p '.' }
  \<                            { \p _ -> TkCharSym p '<' }
  >                             { \p _ -> TkCharSym p '>' }
  let                           { \p _ -> TkStrSym p "let" }
  letrec                        { \p _ -> TkStrSym p "letrec" }
  in                            { \p _ -> TkStrSym p "in" }
  case                          { \p _ -> TkStrSym p "case" }
  of                            { \p _ -> TkStrSym p "of" }
  "->"                          { \p _ -> TkStrSym p "->" }
  "neg"                         { \p _ -> TkStrSym p "neg" }
  $alpha [$alpha $digit \_ \']* { \p s -> TokenId p s }
  $digit+                       { \p s -> TokenInt p (read s) }

{

tok f p s = f p s

data Token 
   = TokenInt !AlexPosn !Int
   | TokenId !AlexPosn !String
   | TkCharSym !AlexPosn !Char
   | TkStrSym !AlexPosn !String
   deriving (Eq, Show)

scanTokens = alexScanTokens

}