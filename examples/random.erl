[ % random element example
    {pool, [{size, 1},
            {worker_type, dummy_worker}],
        [{print, {sprintf, "binary: ~tp,list: ~tp,number: ~tp", [{random_binary, 3}, {random_list, 3}, {random_number, 33}]}}]}
].
% should produce something like this:
% 18:19:41.058 [info] <0.177.0> [ director ] Started all pools
% 18:19:41.067 [info] <0.190.0> Appending "binary: <<3,106,60>>,list: [57,230,132],number: 2", Meta: [{function,print},{line,4},{pool_name,"pool1"},{worker_id,1}]
% 18:19:41.067 [info] <0.186.0> [ "pool1" ] All workers have finished
