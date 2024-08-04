-- Celso Zacarias da Silva Junior - 202076003 
-- Wellington Pereira Silva - 201935041

-- import System.IO
import System.Random

type Dado = [Int]

type Mesa = [Dado]

-- Função para gerar um número pseudo-aleatório entre 1 e 6
faceAleatoria = do
  num <- randomRIO (1, 6) :: IO Int
  return num

-- Função para gerar os lados de um dado cuja face superior é n
geraDado :: Int -> Dado
geraDado 1 = [1]
geraDado n = n : geraDado (n - 1)

-- Função para rodar um dado
rodaDado :: Int -> Dado
rodaDado n = (filter (\x -> n + x /= 7) (geraDado n)) :: Dado

-- Função para rodar um dado aleatório
rodaDadoAleatorio = do
  n <- faceAleatoria
  return (rodaDado n)

-- Função para retornar a face de cima de um dado
faceCimaDado :: Dado -> Int
faceCimaDado dado = head dado

-- Função para retornar os lados disponíveis para rotação de um dado
ladosDado dado = do
  d <- dado
  let primeiroValor = tail d
  return primeiroValor

-- verifica na lista de lados do dado d se a face n é valida
eRotacaoValida :: Dado -> Int -> Bool
eRotacaoValida dado n = elem n dado

-- Rotacionar um dado para a face n
rotacionaDado n = rodaDado n

-- Função para verificar se é possível remover o dado d
podeRemover [1] = True
podeRemover _ = False

-- Remover um dado da mesa
removeDadoDaMesa :: Mesa -> Int -> Mesa
removeDadoDaMesa mesa indice =
  take indice mesa ++ drop (indice + 1) mesa

-- Função para rotacionar o dado da mesa
rotacionaDadoDaMesa :: Mesa -> Int -> Int -> Mesa
rotacionaDadoDaMesa mesa indice face =
  let (antes, dadoAtual : depois) = splitAt indice mesa
   in antes ++ [rotacionaDado face] ++ depois

-- Função para criar a mesa
criaMesa :: Int -> IO Mesa
criaMesa n = sequence $ replicate n rodaDadoAleatorio

-- Função para imprimir o dado
showDado :: [Int] -> String
showDado (x : xs) =
  let caudaStr = unwords (map show xs)
   in "Cima: " ++ show x ++ " Lados Possíveis: " ++ caudaStr

-- Função para imprimir a mesa
showMesa :: Mesa -> String
showMesa mesa = unlines (map showDado mesa)

-- Verificar mesa vazia
eMesaVazia :: Mesa -> Bool
eMesaVazia [] = True
eMesaVazia _ = False

-- Main
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!\n"
  putStrLn "Digite a dificuldade (1 = Fácil, 2 = Difícil): "
  dificuldade <- readLn
  if (dificuldade > 2 || dificuldade < 1)
    then do
      putStrLn "Dificuldade Inválida!\n"
      main
    else do
      putStrLn "Digite a quantidade de dados:\n"
      n <- readLn
      mesa <- criaMesa n
      case dificuldade of
        1 -> gameLoop mesa jogadaComputadorFacil
        _ -> gameLoop mesa jogadaComputadorDificil

gameLoop :: Mesa -> (Mesa -> IO Mesa) -> IO ()
gameLoop mesa jogadaComputador = do
  putStrLn $ "Estado atual dos dados: \n" ++ showMesa mesa
  putStrLn "Sua vez de jogar. Escolha uma ação:\n"
  putStrLn "1. Remover um dado\n"
  putStrLn "2. Rotacionar um dado\n"
  action <- readLn
  if (action > 2 || action < 1)
    then gameLoop mesa jogadaComputador
    else do
      novaMesa <- jogadaJogador action mesa
      if eMesaVazia novaMesa
        then putStrLn "Parabéns, você venceu!"
        else do
          novaMesa <- jogadaComputadorDificil novaMesa -- Tem que ser computador
          if eMesaVazia novaMesa
            then putStrLn "O computador venceu! :("
            else gameLoop novaMesa jogadaComputador

-- Função que cria a jogada Jogador
jogadaJogador :: Int -> Mesa -> IO Mesa
jogadaJogador action mesa = do
  putStrLn "Selecione um dado: \n"
  dado <- readLn
  case action of
    1 -> do
      let m = mesa
      if podeRemover (m !! dado)
        then return (removeDadoDaMesa mesa dado)
        else do
          putStrLn "Dado inválido"
          jogadaJogador action mesa
    _ -> do
      putStrLn "Selecione a face: \n"
      let m = mesa
      face <- readLn
      if eRotacaoValida (m !! dado) face
        then do
          return (rotacionaDadoDaMesa m dado face)
        else do
          putStrLn "Face inválida"
          jogadaJogador action mesa

-- Função para escolher uma face aleatória dos lados disponíveis
escolherFaceAleatoria :: Dado -> IO Int
escolherFaceAleatoria facesDisponiveis = randomRIO (1, length facesDisponiveis - 1)

-- Função para escolher um dado aleatório de uma mesa
escolherDadoAleatorio :: Mesa -> IO Int
escolherDadoAleatorio mesa = randomRIO (0, length mesa - 1)

-- Função para escolher uma ação aleatória
escolherAcaoAleatoria :: IO Int
escolherAcaoAleatoria = randomRIO (1, 2) -- 1 para remover e 2 para rotacionar

-- Função para a jogada do computador fácil
jogadaComputadorFacil :: Mesa -> IO Mesa
jogadaComputadorFacil mesa = do
  let numDados = length mesa
  if numDados == 0
    then return mesa
    else do
      action <- escolherAcaoAleatoria
      dadoIndex <- escolherDadoAleatorio mesa
      case action of
        1 -> do
          let dado = mesa !! dadoIndex
          if podeRemover dado
            then do
              putStrLn $ "O computador removeu o dado " ++ show dadoIndex ++ "\n"
              return (removeDadoDaMesa mesa dadoIndex)
            else jogadaComputadorFacil mesa
        _ -> do
          face <- escolherFaceAleatoria (mesa !! dadoIndex)
          if eRotacaoValida (mesa !! dadoIndex) face
            then do
              putStrLn $ "O computador rotacionou o dado " ++ show dadoIndex ++ " para a face " ++ show face ++ "\n"
              return (rotacionaDadoDaMesa mesa dadoIndex face)
            else jogadaComputadorFacil mesa

-- Função para a jogada do computador Dificil

-- Verifica se uma configuração com um dado é vencedora ou perdedora
eConfiguracaoVencedora1Dado :: Dado -> Bool
eConfiguracaoVencedora1Dado dado = case faceCimaDado dado of
  1 -> True
  2 -> False
  3 -> True
  4 -> True
  5 -> False
  6 -> True

-- Verifica se uma configuração com dois dados é vencedora ou perdedora
eConfiguracaoVencedora2Dados :: Dado -> Dado -> Bool
eConfiguracaoVencedora2Dados dado1 dado2
  | faceCimaDado dado1 == faceCimaDado dado2 = False
  | (faceCimaDado dado1 + faceCimaDado dado2) == 7 = False
  | otherwise = True

-- Verifica se todos os dados são apenas 2 ou 5 (Faces perdedoras)
todosDados2ou5 :: Mesa -> Bool
todosDados2ou5 mesa = all (\dado -> faceCimaDado dado == 2 || faceCimaDado dado == 5) mesa

podeFormarParesPerdedores :: Mesa -> Bool
podeFormarParesPerdedores dados = all eConfiguracaoPerdedoraParaPar pares
  where
    pares = [(x, y) | x <- dados, y <- dados, x /= y]
    eConfiguracaoPerdedoraParaPar (d1, d2) = not $ eConfiguracaoVencedora2Dados d1 d2

-- Verifica se uma configuração é perdedora
eConfiguracaoPerdedora :: Mesa -> Bool
eConfiguracaoPerdedora mesa
  | length mesa == 1 = not $ eConfiguracaoVencedora1Dado (head mesa)
  | length mesa == 2 = not $ eConfiguracaoVencedora2Dados (mesa !! 0) (mesa !! 1)
  | todosDados2ou5 mesa = True
  | otherwise = not $ podeFormarParesPerdedores mesa

-- Função para a jogada do computador difícil
jogadaComputadorDificil :: Mesa -> IO Mesa
jogadaComputadorDificil mesa = do
  let numDados = length mesa
  if numDados == 0
    then return mesa
    else do
      -- Tenta remover um dado que garanta uma configuração perdedora
      let removiveis = filter podeRemover mesa
      if not (null removiveis)
        then do
          let dadoIndex = head [i | (i, dado) <- zip [0 ..] mesa, podeRemover dado]
          putStrLn $ "O computador removeu o dado " ++ show dadoIndex ++ "\n"
          return (removeDadoDaMesa mesa dadoIndex)
        else do
          -- Tenta rotacionar um dado para garantir uma configuração perdedora
          let rotacoesValidas = [(i, f) | (i, dado) <- zip [0 ..] mesa, f <- [1 .. 6], eRotacaoValida dado f, eConfiguracaoPerdedora (rotacionaDadoDaMesa mesa i f)]
          if not (null rotacoesValidas)
            then do
              let (dadoIndex, face) = head rotacoesValidas
              putStrLn $ "O computador rotacionou o dado " ++ show dadoIndex ++ " para a face " ++ show face ++ "\n"
              return (rotacionaDadoDaMesa mesa dadoIndex face)
            else jogadaComputadorDificil mesa -- Se não encontrar uma jogada válida, repete a função
