(* PlcChecker *)

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

(*val para test*)
val teste = IntT;
fun teval exp plctype envi = teste; (*TODO*)

