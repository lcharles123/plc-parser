(* PlcChecker *)
(*Este módulo é responsável pela checagem de tipos. 
Ele deve prover uma função teval : expr -> plcType env -> plcType que,
data uma expressão 'e' em sintaxe abstrata e um ambiente de tipos para 
as variáveis livres em 'e' (pode não haver nenhuma), produz o tipo de
'e' naquele ambiente se 'e' é bem tipada e falha (produzindo uma das 
exceções já presentes no arquivo) caso contrário. A implementação de
teval deve seguir as regras de tipagem especificadas no Apêndice A*)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

(*1- A sequência de entrada não contém nenhum elemento
2- É usada nas situações onde nenhuma das específicadas se encaixa.
3- Se os tipos usados numa comparação são diferentes.
4- O tipo de retorno da função não condiz com o corpo da mesma.
5- Os tipos da expressões dos possíveis caminhos de um If divergem
6- A condição do if não é booleana
7- Não há resultados para a expressão match
8- O tipo de algum dos casos em match difere dos demais
9- O tipo das opções de match difere do tipo da expressão passada para Match
10- Você está passando pra uma chamada de função um tipo diferente do qual ela suporta
11- Você está tentando chamar algo que não é uma função.
12- Tentativa de acessar um elemento fora dos limites da lista
13- Tentativa de acessar um elemento em uma expressão que não é uma lista.*)

(* implementa as regras de tipagem, descritas no final da especificacao, provavelmente usara lookup *)
(* teval : expr -> plcType env -> plcType *)
(* teval deve ser capaz de lidar com o que foi definido em expr (Abysn.sml)
 Deve-se mapear expr para algum tipo defnido em plcType*)

fun teval (ConI _) (environ: plcType env) = IntT (* env definido em Environ.sml *)
  | teval (ConB _) (environ: plcType env) = BoolT
  | teval (Var var) (environ: plcType env) = ( (lookup environ var) handle SymbolNotFound => raise SymbolNotFound )
  | teval (ESeq seq) (environ: plcType env) =
    let
        fun f (SeqT seqt) = (SeqT seqt)
          | f _ = raise EmptySeq
    in 
        f seq
    end
  | teval (Let(var, e1, e2)) (environ: plcType env) =
    let
        val tipoE1 = teval e1 environ
        val novoEnv = (var, tipoE1) :: environ
    in
        teval e2 novoEnv
    end  
  | teval (Letrec(nome, tipoArg, arg, tipoFun, e1, e2)) (environ:plcType env) =
    let
        val tipoE1 = teval e1 ((nome, FunT (tipoArg, tipoFun)) :: (arg, tipoArg) :: environ)
        val tipoE2  = teval e2 ((nome, FunT (tipoArg, tipoFun)) :: environ)
    in
        if tipoE1 = tipoFun then tipoE2 else raise WrongRetType
    end
  | teval (Prim1(operador, e)) (environ: plcType env) =
    let
        val tipoE = teval e environ
    in
        case operador of
            "hd" => (
                case tipoE of
                    SeqT tipo => tipo (*Pois a op hd retorna apenas o primeiro elemento da lista, logo basta retornar seu tipo*)
                    | _ => raise UnknownType)
            
            | "tl" => ( 
                case tipoE of
                    SeqT tipo => SeqT tipo (*Pois tl retorna uma LISTA com o tail da passada como param, 
                    logo devemos retornar o tipo como lista do msm tipo da recebida como param. *)
                    | _ => raise UnknownType )           
            | "ise" => (
                case tipoE of
                    SeqT tipo => BoolT (* ise sempre é do tipo bool, no caso BoolT*)
                    | _ => raise UnknownType )
            | "print" => ( ListT [] )
            | "!" => ( if tipoE = BoolT then BoolT else raise UnknownType )
            | "-" => ( if tipoE = IntT then IntT else raise UnknownType )
            | _ => raise UnknownType  
    end
  | teval (Prim2(operador, e1, e2)) (environ: plcType env) =
    (*Avalia e1 e e2*)
    let
        val tipoE1 = teval e1 environ 
        val tipoE2 = teval e2 environ
    in
        case operador of
            (*Operações de +, -, *, / são feitas apenas para IntT*)
            "+" => if tipoE1 = IntT andalso tipoE2 = tipoE1 then IntT else raise UnknownType
            | "-" => if tipoE1 = IntT andalso tipoE2 = tipoE1 then IntT else raise UnknownType
            | "*" => if tipoE1 = IntT andalso tipoE2 = tipoE1 then IntT else raise UnknownType
            | "/" => if tipoE1 = IntT andalso tipoE2 = tipoE1 then IntT else raise UnknownType
            (* Para "="" e "!=" é necessário verificar se o tipo de cada um é válido e se são do mesmo tipo*)
            | "=" => if (tipoE1 = IntT orelse tipoE1 = BoolT) then
                        if (tipoE2 = tipoE1) then BoolT else raise NotEqTypes
                    else raise UnknownType
            | "!=" => if (tipoE1 = IntT orelse tipoE1 = BoolT) then
                        if (tipoE2 = tipoE1) then BoolT else raise NotEqTypes
                    else raise UnknownType
            | "<" => if tipoE1 = IntT andalso tipoE2 = tipoE1 then BoolT else raise UnknownType
            | "<=" => if tipoE1 = IntT andalso tipoE2 = tipoE1 then BoolT else raise UnknownType
            (*Operador && requer operandos do tipo BoolT*)
            | "&&" => if tipoE1 = BoolT andalso tipoE2 = tipoE1 then BoolT else raise UnknownType
            | "::" => (
                    case (tipoE1, tipoE2) of
                      (BoolT, ListT []) => SeqT BoolT (*Está se criando uma seq do tipo de e1 - nesse caso BoolT*)
                    | (IntT, ListT []) => SeqT IntT (*Está se criando uma seq do tipo de e1 - nesse caso IntT*)
                    | (ListT tipoList, ListT []) => SeqT (ListT tipoList) (*Está se adicionando uma lista dentro de outra vazia - logo retorna-se tipo seq da lista do tipo x*)
                    (*Nos casos abaixo verifica-se se o elemento é do tipo da Seq, se sim - retorna tipo Seq do tipo do elemento, senão - raise*)
                    | (BoolT, SeqT tipoS) => if tipoS = BoolT then SeqT BoolT else raise NotEqTypes 
                    | (IntT, SeqT tipoS) => if tipoS = IntT then SeqT IntT else raise NotEqTypes
                    (*No caso abaixo está se adicionando uma lista numa seq de lista, então verificar se o tipo da lista é compativel com as listas de seq*)
                    | (ListT tipoList, SeqT tipoS) => if ListT tipoList = tipoS then SeqT tipoS else raise NotEqTypes 
                    | _ => raise UnknownType )
            | ";" => tipoE2
            | _ => raise UnknownType
    end
  | teval (If(cond, e1, e2)) (environ: plcType env) =
    (*Avalia-se cada uma das expr, incluindo cond*)
    let
        val tipoE1 = teval e1 environ
        val tipoE2 = teval e2 environ
        val tipoCond = teval cond environ
    in
        if tipoCond = BoolT then 
            if tipoE1 = tipoE2 then tipoE1 else raise DiffBrTypes (*DiffBrTypes - Tipos diferentes dos caminhos de if*)
        else raise IfCondNotBool (*Condição do if não é bool*)
    end
  | teval (Match(e1, matchList)) (environ: plcType env) =
    let
        val tipoHdElement = teval (#2 (hd matchList)) environ
        val tipoE1 = teval e1 environ
        (*Vai passando pela matchlist e assegurando que a expressão a ser matched é igual às condições *)
        fun buscaMatchList (Match(e1, head::[])) (environ: plcType env) =
            (case head of
                (SOME teste1, teste2) => 
                    if ((teval teste2 environ) = tipoHdElement) andalso (tipoE1 = (teval teste1 environ)) then
                        teval teste2 environ  
                    else 
                        if ((teval teste2 environ) = tipoHdElement) then
                            raise MatchCondTypesDiff
                        else 
                            raise MatchResTypeDiff
                | (NONE, teste2) => 
                    if (teval teste2 environ) = tipoHdElement then 
                        tipoHdElement 
                    else 
                        raise MatchResTypeDiff )
          | buscaMatchList (Match(e1, head::tail)) (environ: plcType env) =        
            (case head of
                (SOME teste1, teste2) => 
                    if ((teval teste2 environ) = tipoHdElement) andalso (tipoE1 = (teval teste1 environ)) then
                        buscaMatchList (Match(e1, tail)) environ 
                    else 
                        if ((teval teste2 environ) = tipoHdElement) then
                            raise MatchCondTypesDiff
                        else 
                            raise MatchResTypeDiff
                | _ => raise UnknownType )(* Como ja foi tratado no caso anterior, agora não existe a opção NONE ...*)
        | buscaMatchList (Match(e1, _)) (environ: plcType env) = raise NoMatchResults
        | buscaMatchList _ _ = raise UnknownType (*Qualquer outro caso em que se chamar a fun de busca é invalido*) 
    in
        buscaMatchList(Match(e1, matchList)) environ
    end  
  | teval (Call(e1, e2)) (environ: plcType env) =
        (case (teval e1 environ) of
            FunT (tipoParam, tipoRet) => 
                if (teval e2 environ) = tipoParam then tipoRet else raise CallTypeMisM (*passando pra uma chamada de função um tipo diferente do qual ela suporta*)
           | _ => raise NotFunc ) (*chamando algo que não é uma função*)
  | teval (List list) (environ: plcType env) =
    let
        fun verificaLista (head::[]) = (teval head environ)::[] (*percorre toda a lista*)
          | verificaLista (head::tail) = (teval head environ)::(verificaLista tail)
          | verificaLista _ = []
    in
        ListT (verificaLista list)
    end 
  | teval (Item (indice, e)) (environ: plcType env) =
    let
        fun capturaElemento (index, []) = raise ListOutOfRange
          | capturaElemento (index, (head::[])) = if index = 1 then head else raise ListOutOfRange
          | capturaElemento (index, (head::tail)) = if index = 1 then head else capturaElemento (index - 1, tail)
    in
        case (teval e environ) of
            ListT list => capturaElemento(indice, list)
            | _ => raise OpNonList (*Tentativa de acessar um elemento em uma expressão que não é uma lista*)
    end
  | teval (Anon(tipo, param, e)) (environ: plcType env) = 
    let
        val tipoE = teval e ((param, tipo)::environ)
    in
        FunT (tipo, tipoE)
    end


