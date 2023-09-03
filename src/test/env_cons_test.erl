-module(env_cons_test).

-include("env_test.hrl").

cons_define_test() ->
    Define = "(define x (cons 1 2))
              (define y (cons 3 4))
              (define z (cons x y))",
    Run = env_run:run_fun([format, true]),

    Run(Define),
    ?assertEqual("1", Run("(car (car z))")),
    ?assertEqual("3", Run("(car (cdr z))")),
    ?assertEqual("((1 . 2) 3 . 4)", Run("z")),

    Run(stop).

cons_rat_test() ->
    Run = env_run:run_fun(),

    Run({file, ?PATH ++ "rat.scm"}),
    ?assertEqual({pair_val, [{num_val,1} | {num_val,2}]}, Run("one-half")),
    ?assertEqual({pair_val, [{num_val,5} | {num_val,6}]}, Run("(add-rat one-half one-third)")),
    ?assertEqual({pair_val, [{num_val,1} | {num_val,6}]}, Run("(mul-rat one-half one-third)")),
    ?assertEqual({pair_val, [{num_val,2} | {num_val,3}]}, Run("(add-rat one-third one-third)")),

    Run(stop).

list_test() ->
    Run = env_run:run_fun([format, true]),

    Run({file, ?PATH ++ "list.scm"}),
    ?assertEqual("true", Run("(= one-through-four (cons 1 (cons 2 (cons 3 (cons 4 nil)))))")),
    ?assertEqual("16", Run("(list-ref squares 3)")),
    ?assertEqual("4", Run("(length odds)")),
    ?assertEqual("(1 3 5 7 1 4 9 16 25)", Run("(append odds squares)")),

    Run(stop).

tree_test() ->
    Define = "(define (count-leaves x)
                  (cond ((null? x) 0)
                        ((not (pair? x)) 1)
                        (else (+ (count-leaves (car x))
                                (count-leaves (cdr x))))))
              (define x (cons (list 1 2) (list 3 4)))",
    Run = env_run:run_fun([format, true]),

    Run(Define),
    ?assertEqual("((1 2) 3 4)", Run("x")),
    ?assertEqual("(1 (2 (3 4)))", Run("(list 1 (list 2 (list 3 4)))")),
    ?assertEqual("4", Run("(count-leaves x)")),
    ?assertEqual("8", Run("(count-leaves (list x x))")),

    Run(stop).
