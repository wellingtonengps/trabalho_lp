import System.IO
import System.Random

type Die = Int
type Dice = [Die]

-- Função para gerar um número pseudo-aleatório entre 1 e 6
rollDie :: IO Die
rollDie = getStdRandom (randomR (1, 6)) :: IO Die

-- Função para sortear a configuração inicial dos dados
initialDice :: Int -> IO Dice
initialDice n = sequence $ replicate n rollDie

-- Função para exibir o estado atual dos dados
showDice :: Dice -> String
showDice dice = unwords (map show dice)

-- Função para remover um dado
removeDie :: Dice -> Int -> Dice
removeDie dice index = take index dice ++ drop (index + 1) dice

-- Função para rotacionar um dado
rotateDie :: Die -> Die
rotateDie die
  | die > 1 = die - 1
  | otherwise = die -- Não deve acontecer, pois apenas rotacionamos dados > 1

-- Função para o computador fazer uma jogada no nível fácil
computerMoveEasy :: Dice -> IO Dice
computerMoveEasy dice = do
  index <- rollDie
  let dieIndex = (index `mod` length dice)
  let die = dice !! dieIndex
  if die == 1
    then return (removeDie dice dieIndex)
    else return (take dieIndex dice ++ [rotateDie die] ++ drop (dieIndex + 1) dice)

-- Função para verificar se uma configuração é vencedora
isWinningConfiguration :: Dice -> Bool
isWinningConfiguration dice
  | length dice == 1 = head dice /= 2 && head dice /= 5
  | otherwise = any (\die -> die /= 2 && die /= 5) dice

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  putStrLn "Digite a quantidade de dados:"
  n <- readLn
  dice <- initialDice n
  gameLoop dice

gameLoop :: Dice -> IO ()
gameLoop dice = do
  putStrLn $ "Estado atual dos dados: " ++ showDice dice
  putStrLn "Sua vez de jogar. Escolha uma ação:"
  putStrLn "1. Remover um dado"
  putStrLn "2. Rotacionar um dado"
  action <- readLn
  dice' <- playerMove dice action
  if null dice'
    then putStrLn "Você venceu!"
    else do
      dice'' <- computerMoveEasy dice'
      if null dice''
        then putStrLn "O computador venceu!"
        else gameLoop dice''

playerMove :: Dice -> Int -> IO Dice
playerMove dice action = do
  putStrLn "Escolha o índice do dado (começando de 0):"
  index <- readLn
  let die = dice !! index
  case action of
    1 -> return (removeDie dice index)
    2 -> return (take index dice ++ [rotateDie die] ++ drop (index + 1) dice)
    _ -> do
      putStrLn "Ação inválida. Tente novamente."
      playerMove dice action
