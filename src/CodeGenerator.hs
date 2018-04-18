module CodeGenerator (
    ASMCode
    , genCode
    , lineComment
    , gLabel
    , pushV
    , pushI
    , store
    , add
    , sub
    , neg
    , mul
    , idiv
    , lt
    , different
    , lte
    , gt        
    , gte
    , eq
    , aElse
    , goto
    , gosub
    , aReturn
    , printString
    , printInt
    , printNewLine
    , input
    , aMain
    , endMain
) where

import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

type Var = Char

indent :: String
indent = "        "

data ASMCode = ASMCode
    (Set.Set Var)           -- ^ Vars
    (Map.Map String String) -- ^ Strings                -- ^ String name, String value
    (Set.Set Int)           -- ^ Labels
    String                  -- ^ Instructions
    (Set.Set String)        -- ^ Relocations
    (Set.Set String)        -- ^ External functions
    deriving (Eq, Show)


instance Monoid ASMCode where
    mempty = ASMCode Set.empty Map.empty Set.empty "" Set.empty Set.empty
    (ASMCode v1 s1 l1 i1 r1 ef1) `mappend` (ASMCode v2 s2 l2 i2 r2 ef2) =
        ASMCode (v1 `Set.union` v2) (s1 `Map.union` s2) (l1 `Set.union` l2) (i1 ++ i2) (r1 `Set.union` r2) (ef1 `Set.union` ef2)


-- ####################
-- 'Smart' constructors

var :: Var -> ASMCode
var v = ASMCode (Set.singleton v) Map.empty Set.empty "" (Set.singleton [v]) Set.empty

string ::   String          -- ^ String Name 
            -> String       -- ^ String value
            -> ASMCode
string sn sv = ASMCode Set.empty (Map.singleton sn sv) Set.empty "" (Set.singleton sn) Set.empty

label :: String -> ASMCode
label s = ASMCode Set.empty Map.empty Set.empty (s ++ ":\n") Set.empty Set.empty

comment :: String -> ASMCode
comment s = ASMCode Set.empty Map.empty Set.empty ("# " ++ s ++ "\n") Set.empty Set.empty

emptyLine :: ASMCode
emptyLine = ASMCode Set.empty Map.empty Set.empty "\n" Set.empty Set.empty

instr :: String -> ASMCode
instr s = ASMCode Set.empty Map.empty Set.empty (indent ++ s ++ "\n") Set.empty Set.empty

externalFunction :: String -> ASMCode
externalFunction s = ASMCode Set.empty Map.empty Set.empty "" Set.empty (Set.singleton s)

-- #####################
-- Final code generation

genVariables :: ASMCode -> String
genVariables (ASMCode vs _ _ _ _ _) =
    Set.foldr (\v s -> s ++ [v] ++ ": .word 0\n") "" vs

genStrings :: ASMCode -> String
genStrings (ASMCode _ ss _ _ _ _) = 
    Map.foldrWithKey (\k v s -> s ++ k ++ ": .asciz " ++ show v ++ "\n") "" ss

genDataSection :: ASMCode -> String
genDataSection p = 
    ".data\n.balign 4\n"
    ++ genVariables p
    ++ genStrings p

genGotoSection :: ASMCode -> String
genGotoSection (ASMCode _ _ ls _ _ _) =
    case Set.null ls of
        True -> ""
        False ->
            "gosub:\n"
            ++ indent ++ "push {lr}\n"
            ++ "goto :\n"
            ++ Set.foldr (\v s -> s ++ indent ++ "cmp r0, #" ++ show v ++ "\n" ++ indent ++ "beq label_" ++ show v ++ "\n") "" ls

genRelocation :: ASMCode -> String
genRelocation (ASMCode _ _ _ _ r _) =
    Set.foldr (\v s -> s ++ "address_of_" ++ v ++ ": .word " ++ v ++ "\n") "" r

genExternalFunctions :: ASMCode -> String
genExternalFunctions (ASMCode _ _ _ _ _ ef) =
    Set.foldr (\v s -> s ++ ".global " ++ v ++ "\n") "" ef

genTextSection :: ASMCode -> String
genTextSection p@(ASMCode _ _ _ c _ _) = 
    ".text\n.global main\n"
    ++ genGotoSection p
    ++ c
    ++ genRelocation p
    ++ genExternalFunctions p

genCode :: ASMCode -> String
genCode p = 
    genDataSection p
    ++ genTextSection p


-- ###########################
-- Intermediate representation

lineComment :: String -> ASMCode
lineComment s = emptyLine <> comment s

gLabel :: Int -> ASMCode
gLabel i = ASMCode Set.empty Map.empty (Set.singleton i) ("label_" ++ show i ++ ":\n") Set.empty Set.empty

pushV :: Var -> ASMCode
pushV v = 
    var v 
    <> instr ("ldr r0, address_of_" ++ [v])
    <> instr "ldr r0, [r0]"
    <> instr "push {r0}"

pushI :: Int -> ASMCode
pushI i =
    instr ("mov r0, #" ++ show i)
    <> instr "push {r0}"

store :: Var -> ASMCode
store v =
    var v
    <> instr "pop {r0}"
    <> instr ("ldr r1, address_of_" ++ [v])
    <> instr "str r0, [r1]"

add :: ASMCode
add = 
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "add r0, r0, r1"
    <> instr "push {r0}"

sub :: ASMCode
sub = 
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "sub r0, r0, r1"
    <> instr "push {r0}"

neg :: ASMCode
neg = 
    instr "pop {r1}"
    <> instr "neg r0, r1"
    <> instr "push {r0}"

mul :: ASMCode
mul = 
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "mul r0, r0, r1"
    <> instr "push {r0}"

idiv :: ASMCode
idiv = 
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "div r0, r0, r1"
    <> instr "push {r0}"

lt :: Int -> ASMCode
lt i =
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "cmp r0, r1"
    <> instr ("bge else" ++ show i)

different :: Int -> ASMCode
different i =
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "cmp r0, r1"
    <> instr ("beq else" ++ show i)

lte :: Int -> ASMCode
lte i =
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "cmp r0, r1"
    <> instr ("bgt else" ++ show i)

gt :: Int -> ASMCode
gt i =
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "cmp r0, r1"
    <> instr ("ble else" ++ show i)

gte :: Int -> ASMCode
gte i =
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "cmp r0, r1"
    <> instr ("blt else" ++ show i)

eq :: Int -> ASMCode
eq i =
    instr "pop {r1}"
    <> instr "pop {r0}"
    <> instr "cmp r0, r1"
    <> instr ("bne else" ++ show i)

aElse :: Int -> ASMCode
aElse i = label ("else" ++ show i)

goto :: ASMCode
goto =
    instr "pop {r0}"
    <> instr "b goto"

gosub :: ASMCode
gosub =
    instr "pop {r0}"
    <> instr "bl gosub"

aReturn :: ASMCode
aReturn = 
    instr "pop {lr}"
    <> instr "bx lr"

printString ::  String      -- ^ String name 
                -> String   -- ^ String Value
                -> ASMCode
printString sn sv =
    string sn sv
    <> externalFunction "printf"
    <> instr ("ldr r0, address_of_" ++ sn)
    <> instr "bl printf"

printInt :: ASMCode
printInt =
    externalFunction "printf"
    <> string "pattern" "%d"
    <> instr "ldr r0, address_of_pattern"
    <> instr "pop {r1}"
    <> instr "bl printf"

printNewLine :: ASMCode
printNewLine = 
    externalFunction "puts"
    <> string "empty_string" ""
    <> instr "ldr r0, address_of_empty_string"
    <> instr "bl puts"

input :: Var -> ASMCode
input v = 
    var v
    <> string "pattern" "%d"
    <> externalFunction "scanf"
    <> instr "ldr r0, address_of_pattern"
    <> instr ("ldr r1, address_of_" ++ [v])
    <> instr "bl scanf"

aMain :: ASMCode
aMain = 
    label "main"
    <> instr "push {lr}"

endMain =
    instr "pop {lr}"
    <> instr "mov r0, #0"
    <> instr "bx lr"

-- ##################################################
-- Factorial example - just for documentation purpose

factorial :: ASMCode
factorial =
    aMain
    <> pushI 20
    <> goto
    <> gLabel 10 
    <> pushV 'R'
    <> pushV 'N'
    <> mul
    <> store 'R'
    <> pushV 'N'
    <> pushI 1
    <> sub
    <> store 'N'
    <> pushI 0
    <> pushV 'N'
    <> eq 1
    <> aReturn
    <> aElse 1
    <> pushV 'N'
    <> pushI 0
    <> gt 2
    <> pushI 10
    <> gosub
    <> aElse 2
    <> aReturn
    <> gLabel 20
    <> input 'N'
    <> pushI 1
    <> store 'R'
    <> pushI 10
    <> gosub
    <> printString "string1" "N! = "
    <> pushV 'R'
    <> printInt
    <> printNewLine
    <> endMain  
    
