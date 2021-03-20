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

(* implementa as regras de tipagem, descritas no final da especificacao, provavelmente usara lookup *)
(* teval : expr -> plcType env -> plcType *)
(* teval deve ser capaz de lidar com o que foi definido em expr (Abysn.sml)
 Deve-se mapear expr para algum tipo defnido em plcType*)

fun teval (ConI _) (environ: plcType env) = IntT (* env definido *)
  | teval (ConB _) (environ: plcType env) = BoolT
  | teval (Var var) (environ: plcType env) = 
    let
        fun useLookup env v = 
            lookup env v
    in 
        useLookup environ var
        handle SymbolNotFound => raise SymbolNotFound 
    end
  | teval (ESeq seq) (environ: plcType env) = 
    let
        fun f seq = case seq of
            SeqT seqt => SeqT seqt
            | f _ => raise EmptySeq
    in 
        f seq
    end
  | teval (Let(var, e1, e2)) (environ:plcType env) =
    let
        val tipoe1 = teval e1 environ
        val novoEnv = (var, tipoe1) :: environ
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
  | teval (Prim1(operador, e)) (environ:plcType env) =
    let
      val tipoE = teval e environ
    in
      case operador of
          "hd" => let in
            case tipoE of
                SeqT tipo => tipo (*Pois a op hd retorna apenas o primeiro elemento da lista, logo basta retornar seu tipo*)
              | _ => raise UnknownType
          end
        | "tl" => let in
            case tipoE of
                SeqT tipo => SeqT tipo (*Pois tl retorna uma LISTA com o tail da passada como param, 
                logo devemos retornar o tipo como lista do msm tipo da recebida como param. *)
              | _ => raise UnknownType
          end
        | "ise" => let in
            case tipoE of
                SeqT tipo => BoolT (* ise sempre é do tipo bool, no caso BoolT*)
              | _ => raise UnknownType
          end
        | "print" => ListT []
        | "!" => case tipoE of BoolT => BoolT 
                    | _ => raise UnknownType
        | "-" => case tipoE of IntT => IntT 
                    | _ => raise UnknownType
        | _ => raise UnknownType (*Operador inválido*)
    end 

 
(*val para test*)
val teste = IntT;
fun teval exp plctype envi = teste; (*TODO*)

