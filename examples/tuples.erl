[ % usually a tuple represents a function call, 
  % that's why additional function is required to input a tuple
    {pool, [{size, 1},
            {worker_type, dummy_worker}],
        [
            {test_proplist, [{t, test, true}]},
            {print, {sprintf, "~tp", [{t, elem1, elem2}]}}
        ]} % function input is {elem1, elem2}
].
