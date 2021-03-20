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

fun teval (ConI _) (environment: plcType env) = IntT (* env definido *)
  | teval (ConB _) (environment: plcType env) = BoolT
  | teval (Var v) (environment: plcType env) = let
                                                    fun useLookup env v = 
                                                        lookup environment v
                                                        handle SymbolNotFound
                                               in 
                                                    useLookup env var
                                               end
  | teval ()  
 
(*val para test*)
val teste = IntT;
fun teval exp plctype envi = teste; (*TODO*)

