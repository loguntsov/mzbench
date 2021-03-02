[ % "poisson" worker start
    {pool, [{size, 60},
            % Syntax: {poisson, <L>}
            %  <L> - parameter λ, which is the expected number of "events" or "arrivals" that occur per second
            {worker_start, {poisson, {{numvar, "rate", 60}, rpm}}},
            {worker_type, dummy_worker}],
        [{print, {sprintf, "My number is: ~tp", [{round_robin, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}]}}]}
].
