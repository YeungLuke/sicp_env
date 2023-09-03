-module(env_run_test).

-include("env_test.hrl").

add_test() ->
    ?assertEqual({num_val,146}, env_run:run_code("(+ 23 123)")).

define_test() ->
    ?assertEqual({num_val,10}, env_run:run_code("(define c 10) c")).

lambda_test() ->
    ?assertEqual({num_val,3}, env_run:run_code("(define c (lambda () (+ 1 2))) (c)")).

assign_test() ->
    Code = "(define make-counter
                (lambda (n)
                    (lambda ()
                        (set! n (+ 1 n))
                        n)))
            (define c1 (make-counter 0))
            (c1)",
    ?assertEqual({num_val,1}, env_run:run_code(Code)).

fact_decl_test() ->
    ?assertEqual({num_val,120}, env_run:run_file(?PATH ++ "fact_decl.scm")).

fact_imper_test() ->
    ?assertEqual({num_val,120}, env_run:run_file(?PATH ++ "fact_imper.scm")).

cond_code(N) ->
    "(define (f x)
        (cond ((> x 0) x)
                ((= x 0) 0)
                (else (- 0 x))))
     (f "++ integer_to_list(N) ++")".

cond_test() ->
    ?assertEqual({num_val,1}, env_run:run_code(cond_code(1))),
    ?assertEqual({num_val,0}, env_run:run_code(cond_code(0))),
    ?assertEqual({num_val,1}, env_run:run_code(cond_code(-1))).

and_or_test() ->
    ?assertEqual({bool_val,false}, env_run:run_code("(and true false)")),
    ?assertEqual({bool_val,true}, env_run:run_code("(and true true)")),
    ?assertEqual({bool_val,false}, env_run:run_code("(and false (/ 1 0))")),
    ?assertEqual({bool_val,true}, env_run:run_code("(or true false)")),
    ?assertEqual({bool_val,true}, env_run:run_code("(or false true)")),
    ?assertEqual({bool_val,false}, env_run:run_code("(or false false)")),
    ?assertEqual({bool_val,true}, env_run:run_code("(or true (/ 1 0))")),
    ?assertEqual({bool_val,true}, env_run:run_code("(or false (and true (> 1 0)))")).

let_test() ->
    Code = "(let ((x 3)
                  (y (+ 4 2)))
                 (* x y))",
    ?assertEqual({num_val,18}, env_run:run_code(Code)).

let_star_test() ->
    ?assertEqual({num_val,39}, env_run:run_file(?PATH ++ "let_star.scm")).

let_bind_code(N) ->
    "(define (fib n)
        (let fib-iter ((a 1)
                       (b 0)
                       (count n))
            (if (= count 0)
                b
                (fib-iter (+ a b) a (- count 1)))))
     (fib "++ integer_to_list(N) ++")".

let_bind_test() ->
    ?assertEqual({num_val,3}, env_run:run_code(let_bind_code(4))),
    ?assertEqual({num_val,55}, env_run:run_code(let_bind_code(10))).

nested_define_code(N) ->
    "(define (f x)
        (define (even? n)
            (if (= n 0)
                true
                (odd? (- n 1))))
        (define (odd? n)
            (if (= n 0)
                false
                (even? (- n 1))))
        (odd? x))
    (f "++ integer_to_list(N) ++")".

nested_define_test() ->
    ?assertEqual({bool_val, true}, env_run:run_code(nested_define_code(1))),
    ?assertEqual({bool_val, false}, env_run:run_code(nested_define_code(10))).

% 4.19 todo
wrong_nested_test() ->
    ?assertEqual({num_val, 16}, env_run:run_file(?PATH ++ "wrong_nested.scm")).

% 4.21
lambda_fib_code(N)->
    "((lambda (n)
        ((lambda (h)
            (h h n))
        (lambda (f k)
            (cond ((= k 0) 0)
                    ((= k 1) 1)
                    (else (+ (f f (- k 2)) (f f (- k 1))))))))
        "++ integer_to_list(N) ++")".

lambda_fib_test() ->
    ?assertEqual({num_val, 3}, env_run:run_code(lambda_fib_code(4))),
    ?assertEqual({num_val, 55}, env_run:run_code(lambda_fib_code(10))).

% 4.21
fact_lambda_test() ->
    ?assertEqual({num_val, 120}, env_run:run_file(?PATH ++ "fact_lambda.scm")).

if_none_alter_test() ->
    ?assertEqual({bool_val, false}, env_run:run_file(?PATH ++ "if_none_alter.scm")).

not_test() ->
    ?assertEqual({bool_val, false}, env_run:run_file(?PATH ++ "not.scm")).

quote_test() ->
    ?assertEqual("a", env_run:run_code("'a", [{format, true}])),
    ?assertEqual("()", env_run:run_code("'()", [{format, true}])),
    ?assertEqual("(quote ())", env_run:run_code("''()", [{format, true}])),
    ?assertEqual("(a true \"a\")", env_run:run_code("'(a true \"a\")", [{format, true}])),
    ?assertEqual("(true (quote b))", env_run:run_code("'(true 'b)", [{format, true}])),
    ?assertEqual("((quote (true)) (quote b))", env_run:run_code("'('(true) 'b)", [{format, true}])),
    ?assertEqual("(quote (nil list 1 \"if\"))", env_run:run_code("''(nil list 1 \"if\")", [{format, true}])),
    ?assertEqual("(quote ((list) (quote avc)))", env_run:run_code("''((list) 'avc)", [{format, true}])).
