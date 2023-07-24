-- retorna o nome de todos os livros (emprestados ou disponíveis)
-- sem repetição
-- pode usar a função nub de Data.List que remove repetição
-- livros bancoDados0 
-- [“Java”, “CSP”, “UML”, “Haskell”, “Concorrência”]
livros :: Biblioteca -> [Nome]
livros (pessoas, disponiveis, emprestimos) = nub $ map (\(L nome _) -> nome) (disponiveis ++ map snd emprestimos)

-- quantidade total (disponível e emprestado) de um livro
-- a contagem considera todas as edições
–- retorna 0 se livro não existe
-- qtdExemplares bancoDados0 "Java"
-- 2
qtdExemplares :: Biblioteca -> Nome -> Int
qtdExemplares (pessoas, disponiveis, emprestimos) nome = length $ filter (\(L n _) -> n == nome) (disponiveis ++ map snd emprestimos)

-- retorna a quantidade disponível de um livro para
-- empréstimo
-- livroDisponivel bancoDados0 "Java"
-- 1
livroDisponivel :: Biblioteca -> Nome -> Int 
livroDisponivel (pessoas, disponiveis, emprestimos) nome = length $ filter (\(L n _) -> n == nome) disponiveis

-- retorna true se o usuário está apto a pegar um livro emprestado
-- usuarioApto bancoDados0 12345678
-- True
usuarioApto :: Biblioteca -> Telefone -> Bool
usuarioApto (pessoas, disponiveis, emprestimos) telefone = any (\(P _ t) -> t == telefone) pessoas

-- livros que estao com uma pessoa cujo telefone é fornecido
-- lista retornada é vazia quando usuário não possui o livro
-- ou quando o telefone não está cadastrado
-- livrosPessoa bancoDados0 96874343 
-- [L "UML" 4,L "Haskell" 4]
livrosPessoa :: Biblioteca -> Telefone -> [Livro] 
livrosPessoa (pessoas, disponiveis, emprestimos) telefone = map snd $ filter (\(P _ t, _) -> t == telefone) emprestimos

-- retorna uma lista de pares com o livro no primeiro elemento
-- e a lista de pessoas que estão com o livro emprestado
-- distingue a edição do livro
emprestimos :: Biblioteca -> [(Livro,[Pessoa])] 
emprestimos (pessoas, disponiveis, emprestimos) = map (\l@(L _ _) -> (l, map fst $ filter (\(_, lv) -> lv == l) emprestimos)) $ nub $ map snd emprestimos

-- se livro está disponível, e se pessoa está cadastrada, 
-- livro sai da lista de disponível e entra na lista de
-- empréstimo
–- caso contrário, a biblioteca é retornada inalterada
emprestar :: Biblioteca -> Pessoa -> Livro -> Biblioteca 
emprestar bib@(pessoas, disponiveis, emprestimos) pessoa@(P _ t1) livro@(L _ _) =
  if usuarioApto bib t1 && livroDisponivel bib (nomeLivro livro) > 0 then
    let novosDisponiveis = filter (/=livro) disponiveis in
    let novosEmprestimos = (pessoa,livro):emprestimos in
    (pessoas,novosDisponiveis,novosEmprestimos)
  else bib where nomeLivro (L n _) = n

-- se livro está emprestado, devolve o mesmo
devolver :: Biblioteca -> Telefone -> Nome -> Edicao -> Biblioteca 
devolver bib@(pessoas, disponiveis, emprestimos) telefone nome edicao =
  if any (\(P _ t,l@(L n e)) -> t == telefone && n == nome && e == edicao) emprestimos then
    let novosEmprestimos = filter (\(P _ t,l@(L n e)) -> not(t == telefone && n == nome && e == edicao)) emprestimos in
    let novosDisponiveis = L nome edicao : disponiveis in
    (pessoas,novosDisponiveis,novosEmprestimos)
  else bib 
