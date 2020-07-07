import System.Console.Haskeline
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map.Strict as Map

data Formula = And Formula Formula | 
               Or Formula Formula | 
			   Impl Formula Formula |
			   Eqv Formula Formula |
			   Not Formula | 
			   Atom [Char]  
			   deriving (Eq)

instance Show Formula where
	show (And f1 f2) = "(" ++ show f1 ++ " /\\ " ++ show f2 ++ ")"
	show (Or f1 f2) = "(" ++ show f1 ++ " \\/ " ++ show f2 ++ ")"
	show (Impl f1 f2) = "(" ++ show f1 ++ " => " ++ show f2 ++ ")"
	show (Eqv f1 f2) = "(" ++ show f1 ++ " <=> " ++ show f2 ++ ")"
	show (Not f) = "( ~" ++ show f ++ ")"
	show (Atom c) = c

-- 将隐含转换为一般形式&将非提取到最内层
remove_impl :: Formula -> Formula
remove_impl (And f1 f2) = And (remove_impl f1) (remove_impl f2)
remove_impl (Or f1 f2) = Or (remove_impl f1) (remove_impl f2)
remove_impl (Eqv f1 f2) = Or (And r_f1 r_f2) (And (Not r_f1) (Not r_f2))
	where 
		r_f1 = remove_impl f1 
		r_f2 = remove_impl f2
remove_impl (Impl f1 f2) = Or (Not r_f1) f2
	where 
		r_f1 = remove_impl f1
		r_f2 = remove_impl f2
remove_impl (Not f) = Not (remove_impl f)
remove_impl formula = formula
to_nnf :: Formula -> Formula
to_nnf (Not (And f1 f2)) = Or r_f1 r_f2
	where 
		r_f1 = to_nnf (Not f1)
		r_f2 = to_nnf (Not f2)
to_nnf (Not (Or f1 f2)) = And r_f1 r_f2
	where 
		r_f1 = to_nnf (Not f1)
		r_f2 = to_nnf (Not f2)
to_nnf (Not (Not f)) = r_f
	where 
		r_f = to_nnf f  
to_nnf (And f1 f2) = And (to_nnf f1) (to_nnf f2) 
to_nnf (Or f1 f2) = Or (to_nnf f1) (to_nnf f2) 
to_nnf (Not f) = Not (to_nnf f) 
to_nnf formula = formula 

nnf :: Formula -> Formula
nnf formula =  to_nnf $ remove_impl formula	



-- 使用tableau递归检测是否闭合，只考虑tableau树的正公式推导，因为所有负号被提取到原子层。&可满足性
is_closed :: Map.Map [Char] Bool -> [Formula] -> Bool
is_closed branch [] = False
is_closed branch ((And f1 f2):xs) = is_closed branch (f1:f2:xs) 
is_closed branch ((Or f1 f2):xs) = (is_closed branch (f1:xs)) && (is_closed branch (f2:xs))

is_closed branch ((Not (Atom c)):xs) = case Map.lookup c branch of 
								  Nothing -> is_closed (Map.insert c False branch) xs
								  Just False -> is_closed branch xs
								  Just True -> True

is_closed branch ((Atom c):xs) = case Map.lookup c branch of 
								  Nothing -> is_closed (Map.insert c True branch) xs
								  Just True -> is_closed branch xs
								  Just False -> True

is_satisfiable :: Formula -> Bool
is_satisfiable formula = not $ is_closed Map.empty [nnf formula]



-- 格式化输入
data Statement =  Satisfiable Formula 
					deriving (Eq)
instance Show Statement where 
	show (Satisfiable formula) = "可满足性: " ++ show formula ++ " : " ++ show (is_satisfiable formula)


languageDef = emptyDef {
							Token.identStart=letter
						,   Token.identLetter = alphaNum
						,   Token.reservedOpNames=[ "~", "&", "|", "=>" , "<=>" ]
						,   Token.caseSensitive=False
					   }
lexer = Token.makeTokenParser languageDef	
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens
whiteSpace = Token.whiteSpace lexer
whileParser :: Parser Statement
whileParser = whiteSpace >> satisfiableStatement
satisfiableStatement = 
					do 
					   formula <- formulaExpression
					   return $ Satisfiable formula
formulaExpression :: Parser Formula
formulaExpression = buildExpressionParser operators terms
operators = [
				  [ Prefix (reservedOp "~" >> return (Not )) ]
				, [
					Infix (reservedOp "&" >> return (And )) AssocLeft 
				  , Infix (reservedOp "|" >> return (Or )) AssocLeft  
				  ]
				, [ Infix (reservedOp "=>" >> return (Impl )) AssocLeft ]
				, [ Infix (reservedOp "<=>" >> return (Eqv )) AssocLeft ]
			]

terms = parens lexer formulaExpression
	<|> (Atom <$> identifier)

parseString :: String -> String
parseString str =
	case parse whileParser "" str of
		Left e  -> "Parsing error: " ++ show e
		Right r -> show r
		
main :: IO ()
main = do runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine ">>> "
           case minput of
               Just input -> do outputStrLn $ parseString input
                                loop  			
		
