
{

{-# OPTIONS_GHC -w #-}

module Grammar(parse) where

import Tokens
import Language

}

%name parseGrammar
%tokentype { Token }
%error { parseError }


%token
    int     { TokenInt _ $$ }
    id      { TokenId _ $$ }
    let     { TkStrSym _ "let" }
    letrec  { TkStrSym _ "letrec" }
    in      { TkStrSym _ "in" }
    case    { TkStrSym _ "case" }
    of      { TkStrSym _ "of" }
    neg     { TkStrSym _ "neg" }
    "->"    { TkStrSym _ "->" }
    '='     { TkCharSym _ '=' }
    ';'     { TkCharSym _ ';' }
    '+'     { TkCharSym _ '+' }
    '-'     { TkCharSym _ '-' }
    '*'     { TkCharSym _ '*' }
    '/'     { TkCharSym _ '/' }
    '('     { TkCharSym _ '(' }
    ')'     { TkCharSym _ ')' }
    '{'     { TkCharSym _ '{'}
    '}'     { TkCharSym _ '}'}
    '\\'    { TkCharSym _ '\\' }
    '.'     { TkCharSym _ '.' }
    '<'     {  TkCharSym _ '<' }
    '>'     {  TkCharSym _ '>' }


%right in
%left '+' '-'
%left '*' '/'
%left neg


%%

Program :: { CoreProgram }
Program : SCs { $1 }


SCs 
   : SC      { [$1] }
   | SCs SC  { $2 : $1 }


SC :: { ScDefn Name }
SC : 
   id ArgNames '=' Expr ';' { ScDefn $1 (reverse $2) $4 }


ArgNames 
   :             { [] }
   | ArgNames id { $2 : $1 }


Expr 
   : Expr '+' Expr { EAp (EAp (EVar "+") $1) $3 }
   | Expr '-' Expr { EAp (EAp (EVar "-") $1) $3 }
   | Expr '*' Expr { EAp (EAp (EVar "*") $1) $3 }
   | Expr '/' Expr { EAp (EAp (EVar "/") $1) $3 }
   | neg Expr { EAp (EVar "neg") $2 } 
   | Expr10n { $1 }


Expr10n 
   : let Defs in Expr { ELet False $2 $4 }
   | letrec Defs in Expr  { ELet True $2 $4 }
   | case Expr of '{' Alts '}' { ECase $2 $5 }
   | '\\' ArgNames '.' Expr ';' { ELam $2 $4 }
   | FExpr { $1 } 
   
   
FExpr 
   : AExpr { $1 }
   | FExpr AExpr { EAp $1 $2 }
   
 
 
AExpr : int { ENum $1 }
      | id { EVar $1 }
      | '(' Expr ')' { $2 }


Defs
   : Def { [$1] }
   | Defs Def { $2 : $1 }


Def 
   : id '=' Expr ';' { ($1, $3) }


Alts :: { [CoreAlter] }
   : Alt      { [$1] }    
   | Alts Alt { $2 : $1 }
     

Alt :: { CoreAlter }
   : '<' int '>' ArgNames "->" Expr { ($2, $4, $6) }


{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ (show tokens) 


parse = parseGrammar . scanTokens

    
}
