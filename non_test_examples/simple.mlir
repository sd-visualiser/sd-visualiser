// mlir-opt -mlir-print-op-generic input.mlir -o output.mlir
// func.func @simple(i64, i1) -> i64 {
// ^bb0(%a: i64, %cond: i1): // Code dominated by ^bb0 may refer to %a
//   cf.cond_br %cond, ^bb1, ^bb2
//
// ^bb1:
//   cf.br ^bb3(%a: i64)    // Branch passes %a as the argument
//
// ^bb2:
//   %b = arith.addi %a, %a : i64
//   cf.br ^bb3(%b: i64)    // Branch passes %b as the argument
//
// // ^bb3 receives an argument, named %c, from predecessors
// // and passes it on to bb4 along with %a. %a is referenced
// // directly from its defining operation and is not passed through
// // an argument of ^bb3.
// ^bb3(%c: i64):
//   cf.br ^bb4(%c, %a : i64, i64)
//
// ^bb4(%d : i64, %e : i64):
//   %0 = arith.addi %d, %e : i64
//   return %0 : i64   // Return is also a terminator.
// }

"builtin.module"() ({
  "func.func"() ({
  ^bb0(%arg0: i64, %arg1: i1):
    "cf.cond_br"(%arg1)[^bb1, ^bb2] {operandSegmentSizes = array<i32: 1, 0, 0>} : (i1) -> ()
  ^bb1:  // pred: ^bb0
    "cf.br"(%arg0)[^bb3] : (i64) -> ()
  ^bb2:  // pred: ^bb0
    %0 = "arith.addi"(%arg0, %arg0) : (i64, i64) -> i64
    "cf.br"(%0)[^bb3] : (i64) -> ()
  ^bb3(%1: i64):  // 2 preds: ^bb1, ^bb2
    "cf.br"(%1, %arg0)[^bb4] : (i64, i64) -> ()
  ^bb4(%2: i64, %3: i64):  // pred: ^bb3
    %4 = "arith.addi"(%2, %3) : (i64, i64) -> i64
    "func.return"(%4) : (i64) -> ()
  }) {function_type = (i64, i1) -> i64, sym_name = "simple"} : () -> ()
}) : () -> ()
