--
--       hsExpand.hs
--       
--       Copyright 2014 Mark Kolloros <uvthenfuv@gmail.com>
--       
--       This program is free software; you can redistribute it and/or modify
--       it under the terms of the GNU General Public License as published by
--       the Free Software Foundation; either version 2 of the License, or
--       (at your option) any later version.
--       
--       This program is distributed in the hope that it will be useful,
--       but WITHOUT ANY WARRANTY; without even the implied warranty of
--       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--       GNU General Public License for more details.
--       
--       You should have received a copy of the GNU General Public License
--       along with this program; if not, write to the Free Software
--       Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
--       MA 02110-1301, USA.
--       


-- TODO: More thorough documentation.
-- TODO: Make it work well on any kind of Haskell98 code.


module Main where
import System.Environment ( getArgs )
import Language.Haskell.Parser ( parseModuleWithMode, ParseMode(..), ParseResult(..) )
import Language.Haskell.Syntax -- for the types
import Language.Haskell.Pretty ( prettyPrint )
import Data.Maybe ( isNothing, fromJust )
import Data.List ( find )
import System.Console.GetOpt

import Debug.Trace ( trace )

import BracketPrint ( bracketIndent )


-- The elements are, in order: filename, # of lines to print.
data Params = Params String Integer


defaultLinesToPrint :: Integer
defaultLinesToPrint = 5


usage :: String
usage = "Usage: TODO"


optionDecriptions :: [OptDescr Integer]
optionDecriptions = [
    Option "ln" ["lines"]           (ReqArg read "0") "Number of lines to print.",
    Option "hv" ["help", "version"] (NoArg 0)     "Display a usage message instead of doing anything."]


main = do
    args <- getArgs
    let (options, filenames, err) = (getOpt Permute optionDecriptions args)
    if err /= [] then putStrLn (concat err) else do
        if 0 `elem` options || filenames == [] then putStrLn usage else do
            let filename = head filenames
            text <- readFile filename
            putStrLn $ evaluate (makeParam filename options) $ parseModuleWithMode (ParseMode filename) text


makeParam :: String -> [Integer] -> Params
makeParam f []  = Params f defaultLinesToPrint
makeParam f [n] = Params f n
makeParam f _   = error "Unexpected multiple parameters."


evaluate :: Params -> ParseResult HsModule -> String
evaluate _      (ParseFailed loc error) = parseError loc error
evaluate params (ParseOk (HsModule srcloc mod _ _ decls))
 | isNothing maybeMain = "Error: No main was found!"
 | otherwise           = expandingLines (fromJust maybeMain) decls (linesToPrint params)
 where
 maybeMain = find (isPatBind "main") decls
 linesToPrint :: Params -> Integer
 linesToPrint (Params _ n) = n


parseError :: SrcLoc -> String -> String
parseError (SrcLoc filename line column) error = filename++":"++(show line)++":"++(show column)++": "++error


isPatBind :: String -> HsDecl -> Bool
isPatBind name (HsPatBind _ pat _ _) = pat == (HsPVar (HsIdent name))
isPatBind _    _                     = False


expandingLines :: HsDecl -> [HsDecl] -> Integer -> String
expandingLines mainDecl context 1 = (prettyPrint mainDecl)
expandingLines mainDecl context n = (prettyPrint mainDecl)++"\n"++rest
 where
 rest     = expandingLines (expandMain mainDecl context) context (pred n)


expandMain :: HsDecl -> [HsDecl] -> HsDecl
expandMain (HsPatBind sl lhs (HsGuardedRhss  _  ) decls) context = error "Placeholder: expanding guarded right-hand side."
expandMain (HsPatBind sl lhs (HsUnGuardedRhs exp) decls) context = (HsPatBind sl lhs rhs newDecls)
 where
 rhs = (HsUnGuardedRhs expanded)
 newDecls = decls
 expanded = expandExpr (decls++context) exp


-- Expanding expressions in general. (Function applications, infix applications, etc.)
-- TODO: Treat applications and infix applications the same way by
--       converting back and forth between infix and non-infix and
--       calling a separate function that does basic function
--       expansion.
-- TODO: Make the automatic parentheses adding code result in
--       nice-looking code.
expandExpr :: [HsDecl] -> HsExp -> HsExp
-- expandExpr decls exp | trace ("expandExpr exp: " ++ bracketIndent (show exp)) False = undefined
expandExpr decls (HsApp fn@(HsVar (UnQual (HsIdent f))) value)
 | varExpansion = (HsApp fn (expandExpr decls value))
 | otherwise    = HsParen (expandFn matches value)
 where
 varExpansion = isNothing maybeMatches || (requiresLit matches && not (isLit value))
 matches :: [HsMatch]
 matches = fromJust maybeMatches
 maybeMatches :: Maybe [HsMatch]
 maybeMatches = find (isFnDef f) decls >>= return . getMatches
 getMatches :: HsDecl -> [HsMatch]
 getMatches (HsFunBind matches) = matches
expandExpr decls (HsInfixApp exp1 op exp2) -- this is a hack, we should treat infix and non-infix applications the same way
 -- "Builtins."
 | op == (HsQVarOp (UnQual (HsSymbol "+"))) && isLit exp1 && isLit exp2 = (expIntFn (+)) exp1 exp2
 | op == (HsQVarOp (UnQual (HsSymbol "-"))) && isLit exp1 && isLit exp2 = (expIntFn (-)) exp1 exp2
 | op == (HsQVarOp (UnQual (HsSymbol "*"))) && isLit exp1 && isLit exp2 = (expIntFn (*)) exp1 exp2
 | otherwise = HsInfixApp (expandExpr decls exp1) op (expandExpr decls exp2)
expandExpr decls (HsParen inner@(HsParen _)) = (expandExpr decls inner)
expandExpr decls (HsParen exp)
 | isLit expanded = expanded
 | otherwise      = (HsParen expanded)
 where expanded = (expandExpr decls exp)
expandExpr _     (HsLit a)     = (HsLit a)
expandExpr _ exp = error $ "Placeholder: Can't expand \"" ++ show exp ++ "\""


-- Decides whether the given name is the name of the function the given declaration defines.
isFnDef :: String -> HsDecl -> Bool
isFnDef f (HsFunBind ((HsMatch _ (HsIdent name) _ _ _):matches)) = f == name
isFnDef _    _                                                   = False


-- Decides whether a function definition requires the argument(s) to be literals.
requiresLit :: [HsMatch] -> Bool
requiresLit matches = or (map isLitPat patterns)
 where
 patterns = concat $ map patternsOf matches
 patternsOf :: HsMatch -> [HsPat]
 patternsOf (HsMatch _ _ pats _ _) = pats


-- Expand a function application, replacing the application with its definition.
-- TODO: Rewrite so the error msg includes the name of the function and the value that wasn't matched.
expandFn :: [HsMatch] -> HsExp -> HsExp
-- expandFn patterns value | trace ("expandFn value: " ++ show value) False = undefined
expandFn []     _ = error "expandFn: None of the patterns matched."
expandFn ((HsMatch _ _ patterns rhs _):ms) value
 | length patterns /= 1    = error ("Placeholder: number of patterns in function match is " ++ show (length patterns) ++ ", not one.")
 | isNothing expanded = expandFn ms value
 | otherwise          = fromJust expanded
 where
 expanded :: Maybe HsExp
 expanded = trySubstitute value (head patterns) rhs


-- See if the given expression matches the given pattern, and if so, substitute the expression into the given right-hand side.
trySubstitute :: HsExp -> HsPat -> HsRhs -> Maybe HsExp
-- trySubstitute exp pat rhs | trace ("trySubstitute (" ++ show exp ++ "), (" ++ show pat ++ "), (" ++ show rhs ++ ")") False = undefined
trySubstitute (HsLit lit) (HsPLit plit) (HsUnGuardedRhs exp)
 | lit == plit = Just exp
 | otherwise   = Nothing
trySubstitute value (HsPVar (HsIdent varname)) (HsUnGuardedRhs exp) = Just (replaceAll varname value exp)
trySubstitute value (HsPLit plit) rhs = Nothing
trySubstitute _ pat rhs = error "Placeholder: trying to substitute for undefined pattern-type or right-hand side"


-- Given the name of the variable we want to replace, the expression
-- we want to replace the variable with ("value"), and an expression
-- that might contain that variable, replace all occurences of that
-- variable in the expression with the value.
-- TODO: Maybe I should do this with generics?
replaceAll :: String -> HsExp -> HsExp -> HsExp
-- replaceAll name value exp | trace ("replaceAll " ++ name ++ " (" ++ show value ++ ") (" ++ show exp ++ ")") False = undefined
replaceAll name value exp@(HsVar (UnQual (HsIdent varname)))
 | name == varname = value
 | otherwise       = exp
replaceAll name value (HsInfixApp exp1 op exp2) = HsInfixApp (replaceAll name value exp1) op (replaceAll name value exp2)
replaceAll name value (HsParen exp)             = HsParen (replaceAll name value exp)
replaceAll name value (HsApp exp1 exp2)         = HsApp (replaceAll name value exp1) (replaceAll name value exp2)
replaceAll name value (HsLit a)                 = HsLit a
replaceAll _    _     exp                       = error $ "Placeholder: trying to replaceAll for undefined expression: " ++ show exp


-- General helper functions.
expIntFn :: (Integer -> Integer -> Integer) -> (HsExp -> HsExp -> HsExp)
expIntFn fn = inner
 where
 inner :: HsExp -> HsExp -> HsExp
 inner (HsLit (HsInt a)) (HsLit (HsInt b)) = (HsLit (HsInt (fn a b)))


isLit :: HsExp -> Bool
isLit (HsLit _) = True
isLit _         = False


isLitPat :: HsPat -> Bool
isLitPat (HsPLit _) = True
isLitPat _          = False





